import snoop
from enum import Enum, unique, auto
from collections import namedtuple
import regex

Span = namedtuple("Span", ["start", "after"])

Token = namedtuple("Token", ["type", "src", "value", "span", "newlines"])

RegExp = namedtuple("RegExp", ["body", "flags"])

Template = namedtuple("Template", ["tv", "trv"])


def utf_16_encode(str):
    def enc(ch):
        if ord(ch) <= 0xFFFF:
            return ch
        cpx = ord(ch) - 0x10000
        return chr((cpx >> 10) + 0xD800) + chr((cpx & 0x3FF) + 0xDC00)

    return "".join(enc(c) for c in str)


def utf_16_decode(str):
    def dec(str):
        lead = 0
        for c in str:
            if lead == 0:
                if ord(c) < 0xD800 or ord(c) > 0xDBFF:
                    yield c
                else:
                    lead = ord(c)
            else:
                if ord(c) < 0xDC00 or ord(c) > 0xDFFF:
                    raise SyntaxError("Invalid or Unexpected Token")
                yield chr(((lead - 0xD800) << 10) + ord(c) - 0xDC00 + 0x10000)
                lead = 0
        if lead:
            # Ended the string with an uncompleted surrogate.
            raise SyntaxError("Invalid or Unexpected Token")

    return "".join(c for c in dec(str))


escape_match = regex.compile(r"\\u(?:([0-9A-Fa-f]{4})|(?:\{([0-9a-fA-F]+)\}))")


def identifier_name_string_value(src):
    # Return the String value consisting of the sequence of code units corresponding to IdentifierName. In determining
    # the sequence any occurrences of \ UnicodeEscapeSequence are first replaced with the code point represented by the
    # UnicodeEscapeSequence and then the code points of the entire IdentifierName are converted to code units by
    # UTF16Encoding each code point.
    result = ""
    scan_pos = 0
    while 1:
        escape = escape_match.search(src, pos=scan_pos)
        if escape is None:
            result += src[scan_pos:]
            break
        span = escape.span()
        chval = int(escape.group(1) or escape.group(2), 16)
        if chval > 0x10FFFF:
            # Not a character. Throw a syntax error.
            raise SyntaxError("Undefined Unicode code-point")
        assert chval >= 0  # Negative numbers should have been avoided via the regex match
        result += src[scan_pos : span[0]] + chr(chval)
        scan_pos = span[1]
    return utf_16_encode(result)


def identifier_name_early_errors(id_token):
    # 11.6.1.1 Static Semantics: Early Errors
    # IdentifierStart :: \ UnicodeEscapeSequence
    #     It is a Syntax Error if SV(UnicodeEscapeSequence) is none of "$", or "_", or the UTF16Encoding of a code
    #     point matched by the UnicodeIDStart lexical grammar production.
    # IdentifierPart :: \ UnicodeEscapeSequence
    #     It is a Syntax Error if SV(UnicodeEscapeSequence) is none of "$", or "_", or the UTF16Encoding of either
    #     <ZWNJ> or <ZWJ>, or the UTF16Encoding of a Unicode code point that would be matched by the UnicodeIDContinue
    #     lexical grammar production.

    # Since the string value in the token has already had the unicode escapes processed, we just validate according to
    # the rules in 11.6.
    name = utf_16_decode(id_token.value)
    id_validate_match = regex.compile(r"[\p{ID_Start}_$][\p{ID_Continue}$\N{ZWJ}\N{ZWNJ}]*$")
    if not id_validate_match.match(name):
        raise SyntaxError("Invalid or Unexpected Token")


class LexerCore:
    @unique
    class Goal(Enum):
        """
        The "Goal" of a lexer is that symbol which is the topmost element of the lexing tree. Generally, it identifes
        the class of tokens that are valid in a particular circumstance. For ECMAScript, there are four different
        potential goals of the lexer:

            InputElementDiv
            InputElementRegExp
            InputElementRegExpOrTemplateTail
            InputElementTemplateTail

        They have the following meanings:

        InputElementRegExpOrTemplateTail : This is the goal whenever a RegularExpressionLiteral, TemplateMiddle, or
            TemplateTail is permitted. (This is the typical target when lexing a template string.)
        InputElementTemplateTail : This is the goal whenever a TemplateMiddle or TemplateTail is permitted (but not a
            RegularExpresionLiteral). (This happens during the lexing of a template string during something that
            allows the divsion operator.)
        InputElementRegExp : This is the goal whenever a RegularExpressionLiteral is allowed, but not a TemplateMiddle
            or TemplateTail. In other words, most of the time.
        InputElementDiv : This is the goal when neither template production, nor the regexp production is allowed.
            This happens when you're not in a template string, and division is allowed.
        """

        InputElementDiv = auto()
        InputElementRegExp = auto()
        InputElementRegExpOrTemplateTail = auto()
        InputElementTemplateTail = auto()

    InputElementDiv = Goal.InputElementDiv
    InputElementRegExp = Goal.InputElementRegExp
    InputElementRegExpOrTemplateTail = Goal.InputElementRegExpOrTemplateTail
    InputElementTemplateTail = Goal.InputElementTemplateTail

    def __init__(self, source_text):
        """
        Make a new Lexer. The only info that needs providing is the source text. It should be encoded in utf-16.
        """
        self.src = source_text
        self.cache = {}

    @staticmethod
    def _string_value(chars):
        assert (chars[0] == "'" and chars[-1] == "'") or (chars[0] == '"' and chars[-1] == '"')
        # Remove the quotes
        chars = chars[1 : len(chars) - 1]
        # Translate any escape sequences
        if chars.find("\\") < 0:
            # No escapes. We're done
            return utf_16_encode(chars)
        decoded = ""
        index = 0
        while index < len(chars):
            slash = chars.find("\\", index)
            if slash < 0:  # No more!
                decoded += chars[index:]
                break
            decoded += chars[index:slash]
            index = slash
            if chars[index + 1] == "x":
                decoded += chr(int(chars[index + 2 : index + 4], 16))
                index += 4
            elif chars[index + 1] == "u":
                if chars[index + 2] != "{":
                    decoded += chr(int(chars[index + 2 : index + 6], 16))
                    index += 6
                else:
                    closing = chars.find("}", index + 3)
                    chval = int(chars[index + 3 : closing], 16)
                    if chval > 0x10FFFF:
                        # Not a valid Unicode code point. Throw a syntax error.
                        raise SyntaxError("Undefined Unicode code-point")
                    assert chval >= 0  # Negative values here should have been alredy rejected via regex
                    decoded += chr(chval)
                    index = closing + 1
            elif chars[index + 1] == "0":
                decoded += "\u0000"
                index += 2
            elif chars[index + 1] == "b":
                decoded += "\u0008"
                index += 2
            elif chars[index + 1] == "t":
                decoded += "\u0009"
                index += 2
            elif chars[index + 1] == "n":
                decoded += "\u000a"
                index += 2
            elif chars[index + 1] == "v":
                decoded += "\u000b"
                index += 2
            elif chars[index + 1] == "f":
                decoded += "\u000c"
                index += 2
            elif chars[index + 1] == "r":
                decoded += "\u000d"
                index += 2
            elif chars[index + 1] in "\n\r\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR}":
                if chars[index + 1] == "\r" and chars[index + 2] == "\n":
                    index += 3
                else:
                    index += 2
            else:
                decoded += chars[index + 1]
                index += 2
        return utf_16_encode(decoded)

    identifiername_match = regex.compile(
        r"([\p{ID_Start}_$]|(\\u([0-9a-fA-F]{4}|({[0-9a-fA-F]*}))))([\p{ID_Continue}$\N{ZWJ}\N{ZWNJ}]|(\\u([0-9a-fA-F]{4}|({[0-9a-fA-F]*}))))*"
    )
    WhiteSpace = r"[\p{Zs}\N{ZWNBSP}\N{TAB}\N{VT}\N{FF}\N{SP}]"
    whitespace_match = regex.compile(f"({WhiteSpace})+")
    LineTerminator = r"[\N{LF}\N{CR}\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR}]"
    linebreak_match = regex.compile(r"[\N{LF}\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR}]|\N{CR}\N{LF}?")
    single_line_comment_match = regex.compile(r"//[^\N{LF}\N{CR}\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR}]*")
    multi_line_comment_match = regex.compile(r"/\*([^*]|(\*[^/]))*\*?\*/")
    punctuator_match = regex.compile(
        r">>>=|\.\.\.|<<=|>>=|>>>|===|!==|\*\*=|<=|>=|==|!=|\*\*|\+\+|--|<<|>>|&&|\|\||\+=|-=|\*=|%=|&=|\|=|^=|=>|[-{()\[\].;,><+*%&|^!~?:=]"
    )
    divpunctuator_match = regex.compile(r"/=?")
    rightbracepunctuator_match = regex.compile("}")
    binaryintegerliteral_match = regex.compile(
        r"0[bB][01]+(?!([\p{ID_Start}_$0-9]|(\\u([0-9a-fA-F]{4}|({[0-9a-fA-F]*})))))"
    )
    octalintegerliteral_match = regex.compile(
        r"0[oO][0-7]+(?!([\p{ID_Start}_$0-9]|(\\u([0-9a-fA-F]{4}|({[0-9a-fA-F]*})))))"
    )
    hexintegerliteral_match = regex.compile(
        r"0[xX][0-9a-fA-F]+(?!([\p{ID_Start}_$0-9]|(\\u([0-9a-fA-F]{4}|({[0-9a-fA-F]*})))))"
    )
    decimalliteral_match = regex.compile(
        r"((0|[1-9][0-9]*)(\.[0-9]*)?|(\.[0-9]+))([eE][-+]?[0-9]+)?(?!([\p{ID_Start}_$]|(\\u([0-9a-fA-F]{4}|({[0-9a-fA-F]*})))))"
    )
    doublestringliteral_match = regex.compile(
        r'"([^"\\\N{LF}\N{CR}\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR}]|\\([\N{LF}\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR}]|\N{CR}\N{LF}?)|\\([^ux0-9\r\n\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR}]|0(?![0-9])|x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|u{[0-9a-fA-F]+}))*"'
    )
    singlestringliteral_match = regex.compile(
        r"'([^'\\\N{LF}\N{CR}\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR}]|\\([\N{LF}\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR}]|\N{CR}\N{LF}?)|\\([^ux0-9\r\n\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR}]|0(?![0-9])|x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|u{[0-9a-fA-F]+}))*'"
    )

    _RegularExpressionFlags = r"(([\p{ID_Continue}$\N{ZWJ}\N{ZWNJ}]|(\\u([0-9a-fA-F]{4}|({[0-9a-fA-F]*}))))*)"
    _RegularExpressionNonTerminator = r"([^\N{LF}\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR\N{CR}])"
    _RegularExpressionBackslashSequence = r"(\\" + _RegularExpressionNonTerminator + r")"
    _RegularExpressionClassChar = (
        r"([^\N{LF}\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR\N{CR}\]\\]|" + _RegularExpressionBackslashSequence + ")"
    )
    _RegularExpressionClassChars = "(" + _RegularExpressionClassChar + "*)"
    _RegularExpressionClass = r"(\[" + _RegularExpressionClassChars + r"\])"
    _RegularExpressionChar = (
        r"([^\N{LF}\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR\N{CR}[/\\]|"
        + _RegularExpressionBackslashSequence
        + "|"
        + _RegularExpressionClass
        + ")"
    )
    _RegularExpressionFirstChar = (
        r"([^\N{LF}\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR\N{CR}*/[\]]|"
        + _RegularExpressionBackslashSequence
        + "|"
        + _RegularExpressionClass
        + ")"
    )
    _RegularExpressionChars = "(" + _RegularExpressionChar + "*)"
    _RegularExpressionBody = "(" + _RegularExpressionFirstChar + _RegularExpressionChars + ")"
    _RegularExpressionLiteral = "(/(?P<body>" + _RegularExpressionBody + ")/(?P<flags>" + _RegularExpressionFlags + "))"
    regularexpressionliteral_match = regex.compile(_RegularExpressionLiteral)

    _UnicodeEscapeSequence = r"(u([0-9a-fA-F]{4}|\{[0-9a-fA-F]{1,6}\}))"
    _HexEscapeSequence = r"(x[0-9a-fA-F]{2})"
    _CharacterEscapeSequence = r"([^0-9xu\r\n\N{PARAGRAPH SEPARATOR}\N{LINE SEPARATOR}])"
    _EscapeSequence = (
        "(" + _CharacterEscapeSequence + "|0(?![0-9])|" + _HexEscapeSequence + "|" + _UnicodeEscapeSequence + ")"
    )
    _NotEscapeSequence = r"(0[0-9]|[1-9]|x(?![0-9a-fA-F])|x[0-9a-fA-F](?![0-9a-fA-F])|u(?![0-9a-fA-F{])|u[0-9a-fA-F]{1,3}(?![0-9a-fA-F])|u\{(?![0-9a-fA-F])|u\{[0-9a-fA-F]{7,}(?![0-9a-fA-F])|u\{[0-9a-fA-F]{1,6}(?![0-9a-fA-F}]))"
    _LineTerminatorSequence = r"(\n|\r\n?|\N{LINE SEPARATOR}|\N{PARAGRAPH SEPARATOR})"
    _LineContinuation = r"(\\" + _LineTerminatorSequence + ")"
    _TemplateCharacter = (
        r"(\$(?!\{)|\\"
        + _EscapeSequence
        + r"|\\"
        + _NotEscapeSequence
        + "|"
        + _LineContinuation
        + "|"
        + _LineTerminatorSequence
        + r"|[^`\\$\r\n\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR}])"
    )
    _NoSubstitutionTemplate = "(`(?<tchars>" + _TemplateCharacter + "*)`)"
    nosubstitutiontemplate_match = regex.compile(_NoSubstitutionTemplate)
    templatehead_match = regex.compile("(`(?<tchars>" + _TemplateCharacter + r"*)\$\{)")
    templatemiddle_match = regex.compile(r"(\}(?<tchars>" + _TemplateCharacter + r"*)\$\{)")
    templatetail_match = regex.compile(r"(\}(?<tchars>" + _TemplateCharacter + "*)`)")

    skippable = namedtuple("skippable", ("after", "newlines"))

    def process_skippable(self, pos):
        """
        Given a position within the source text, move the position past the skippable stuff (whitespace and comments).

        The return value is a 2-element named tuple:
           - after: The position of the first character after the skippable stuff
           - newlines: This is a list of spans indicating all the newlines that the lexer saw between the starting
             position and the after position.
        """
        newlines = []
        done = False
        while not done:
            # Strip any leading whitespace
            ws = self.whitespace_match.match(self.src, pos=pos)
            if ws:
                # The span of a match from regex is (startidx, after_idx), so our new position is just the second
                # value in that tuple.
                pos = ws.span()[1]

            # Check for a line break
            lb = self.linebreak_match.match(self.src, pos=pos)
            if lb:
                # Record the line break in our newlines list.
                lb_span = lb.span()
                newlines.append(Span(start=lb_span[0], after=lb_span[1]))
                pos = lb_span[1]
                # And then jump back to the top of the token scanner.
                continue

            # Check for single line comments
            slc = self.single_line_comment_match.match(self.src, pos=pos)
            if slc:
                # We just skip over single line comments (because they cannot include line breaks)
                pos = slc.span()[1]
                continue

            # Check for multi-line comments
            mlc = self.multi_line_comment_match.match(self.src, pos=pos)
            if mlc:
                span = mlc.span()
                # We need to collect any newlines embedded in the comment.
                scan_pos = span[0]
                while 1:
                    embedded = self.linebreak_match.search(self.src, pos=scan_pos, endpos=span[1])
                    if embedded is None:
                        break
                    lb_span = embedded.span()
                    newlines.append(Span(start=lb_span[0], after=lb_span[1]))
                    scan_pos = lb_span[1]
                pos = span[1]
                continue

            # None of those means we're at something we shouldn't ignore.
            done = True
        return self.skippable(after=pos, newlines=newlines)

    def token(self, pos, goal):
        """
        Given a position within the source text, and the lexical goal, this returns the next token.

        The return value is a 5-element tuple:
           - token type: This is something like NUMERIC or STRING or IDENTIFIER for those
             things that the grammar treats generically. Punctuation has a type that matches exactly the punctuation
             itself. (It's a string.)
           - src: This is a copy of the source text that produced the token.
           - value: This is the value of the token, which kind of depends on the token type. Identifiers have their
             unicode escapes decoded; numbers are translated from strings to actual numeric values, and strings have
             their quotes removed and all of their escapes parsed. Punctuation is just a copy of the type (and the
             source).
           - span: This is a "Span" pair, indicating the start and end of the token, as source document indices. The
             "next" token should start scanning from the "after" element of this span.
           - newlines: This is a list of spans indicating all the newlines that the lexer saw between that starting
             position and the start of the token. (Note that no tokens can contain source newlines except for strings.)
             When the ECMAScript specification says "[no newline here]" what that means is that the token following
             that expression should have an emtpy list in the "newlines" spot.

        Scanning for a token: whitespace is ignored; comments are ignored (beyond counting newlines). Newlines are
        collected. The return value's start position is on or above the starting position given as an argument to
        this routine.

        If the scanner finds text it can't understand, it returns None. (Since there are multiple goals possible,
        raising a syntax error is a bad idea.)

        If the scanner runs out of input (without previously throwing an exception), it returns None. (That's your
        end-of-file marker.)

        Some Early Error checks might be run (the unicode-escaped chars in identifier namaes come to mind). If they
        fail, SyntaxError exceptions are raised.

        The lexical goal turns on and off some of the matches; if you're really curious about that whole procedure,
        you should really read section 11 of the ECMAScript specification.

        Open issue: Strings and Templates are the only token type that can span multiple lines of source. How should
        they inform the caller about that? We could add them to the newlines list, but that breaks the handling of "[no
        newline here]", which is important for things like "return 'my multiline string'" because newlines are not
        allowed between return keywords the returned expression.
        """
        cached = self.cache.get((pos, goal), None)
        if cached:
            return cached
        val = self._token(pos, goal)
        self.cache[(pos, goal)] = val
        return val

    def _token(self, pos, goal):
        newlines = []
        while 1:
            pos, nls = self.process_skippable(pos)
            newlines.extend(nls)

            # Check for common tokens (IdentifierName, Punctuator, NumericLiteral, StringLiteral, Template)

            # Identifier
            ident = self.identifiername_match.match(self.src, pos=pos)
            if ident:
                span = ident.span()
                identifier_src = self.src[span[0] : span[1]]
                sv = identifier_name_string_value(identifier_src)
                id_token = Token(type="IDENTIFIER", src=self.src, value=sv, span=Span(*span), newlines=newlines)
                # Check Early Errors (section 11.6.1.1)
                identifier_name_early_errors(id_token)
                return id_token

            # Punctuator
            punct = self.punctuator_match.match(self.src, pos=pos)
            if punct:
                span = punct.span()
                return Token(
                    type=punct.group(0), value=punct.group(0), src=self.src, span=Span(*span), newlines=newlines
                )

            # NumericLiteral
            intconvert = lambda base: lambda span: int(self.src[span[0] + 2 : span[1]], base)
            for matcher, converter in (
                (self.binaryintegerliteral_match, intconvert(2)),
                (self.octalintegerliteral_match, intconvert(8)),
                (self.hexintegerliteral_match, intconvert(16)),
                (self.decimalliteral_match, lambda span: float(self.src[span[0] : span[1]])),
            ):
                nl = matcher.match(self.src, pos=pos)
                if nl:
                    span = nl.span()
                    return Token(
                        type="NUMERIC", src=self.src, value=converter(span), span=Span(*span), newlines=newlines
                    )

            # StringLiteral
            for matcher in (self.doublestringliteral_match, self.singlestringliteral_match):
                sl = matcher.match(self.src, pos=pos)
                if sl:
                    span = sl.span()
                    return Token(
                        type="STRING",
                        src=self.src,
                        value=self._string_value(self.src[span[0] : span[1]]),
                        span=Span(*span),
                        newlines=newlines,
                    )

            # DivPunctuator is available for the InputElementDiv and InputElementTemplateTail goals.
            if goal in (self.InputElementDiv, self.InputElementTemplateTail):
                dp = self.divpunctuator_match.match(self.src, pos=pos)
                if dp:
                    span = dp.span()
                    return Token(type=dp.group(0), value=dp.group(0), src=self.src, span=Span(*span), newlines=newlines)

            # RightBracePunctuator is available for InputElementDiv or InputElementRegExp
            if goal in (self.InputElementDiv, self.InputElementRegExp):
                dbp = self.rightbracepunctuator_match.match(self.src, pos=pos)
                if dbp:
                    span = dbp.span()
                    return Token(
                        type=dbp.group(0), value=dbp.group(0), src=self.src, span=Span(*span), newlines=newlines
                    )

            # Regular Expressions available only with InputElementRegExp and InputElementRegExpOrTemplateTail
            if goal in (self.InputElementRegExp, self.InputElementRegExpOrTemplateTail):
                regex_literal = self.regularexpressionliteral_match.match(self.src, pos=pos)
                if regex_literal:
                    span = regex_literal.span()
                    return Token(
                        type="REGEXP",
                        value=RegExp(
                            utf_16_encode(regex_literal.group("body")), utf_16_encode(regex_literal.group("flags"))
                        ),
                        src=self.src,
                        span=Span(*span),
                        newlines=newlines,
                    )

            # All productions get NoSubstitutionTemplate and TemplateHead
            # But only the "TemplateTail" goals get TemplateMiddle or TemplateTail
            for valid_goals, matcher, tokentype in (
                (
                    (
                        self.InputElementDiv,
                        self.InputElementRegExp,
                        self.InputElementRegExpOrTemplateTail,
                        self.InputElementTemplateTail,
                    ),
                    self.nosubstitutiontemplate_match,
                    "NOSUBSTITUTIONTEMPLATE",
                ),
                (
                    (
                        self.InputElementDiv,
                        self.InputElementRegExp,
                        self.InputElementRegExpOrTemplateTail,
                        self.InputElementTemplateTail,
                    ),
                    self.templatehead_match,
                    "TEMPLATEHEAD",
                ),
                (
                    (self.InputElementRegExpOrTemplateTail, self.InputElementTemplateTail),
                    self.templatemiddle_match,
                    "TEMPLATEMIDDLE",
                ),
                (
                    (self.InputElementRegExpOrTemplateTail, self.InputElementTemplateTail),
                    self.templatetail_match,
                    "TEMPLATETAIL",
                ),
            ):
                if goal in valid_goals:
                    tmpl = matcher.match(self.src, pos=pos)
                    if tmpl:
                        span = tmpl.span()
                        return Token(
                            type=tokentype,
                            value=Template(
                                tv=self._TemplateValue(tmpl.group("tchars")),
                                trv=self._TemplateRawValue(tmpl.group("tchars")),
                            ),
                            src=self.src,
                            span=Span(*span),
                            newlines=newlines,
                        )

            # The end. If we still have input and we haven't returned, then this is an unrecognized token.
            # You might think this means we should raise a syntax error, but because there are alternate
            # lexical goals that turns out to be a really bad idea.
            return None

    def is_done(self, pos):
        # Given a position in the source text, return TRUE if only skippable content (whitespace and comments) remains.
        pos, _ = self.process_skippable(pos)
        return pos >= len(self.src)

    class _TemplateFailure(Exception):
        pass

    @classmethod
    def _parse_template_escape(cls, sequence):
        assert len(sequence) > 0
        if len(sequence) == 1:
            return {"0": "\0", "b": "\b", "t": "\t", "n": "\n", "v": "\v", "f": "\f", "r": "\r"}.get(
                sequence[0], sequence[0]
            )
        if sequence[0] == "x":
            assert len(sequence) == 3
            return chr(int(sequence[1:], 16))
        if sequence[0:2] == "u{":
            assert sequence[-1] == "}"
            val = int(sequence[2:-1], 16)
            if val > 0x10FFFF:
                raise cls._TemplateFailure
            return chr(val)
        assert sequence[0] == "u" and len(sequence) == 5
        return chr(int(sequence[1:], 16))

    @staticmethod
    def _TemplateRawValue(tchars):
        return utf_16_encode(regex.sub(r"\r\n?", "\n", tchars))

    @classmethod
    def _TemplateValue(cls, tchars):
        def process(src):
            pos = 0
            while pos < len(src):
                slash = src.find("\\", pos)
                if slash < 0:
                    slash = len(src)
                while pos < slash:
                    # Check for unescaped CR or CRLF. Both get translated to LF.
                    cr = regex.search(r"\r\n?", src, pos=pos, endpos=slash)
                    if cr:
                        span = cr.span()
                        if span[0] > pos:
                            yield src[pos : span[0]]
                        yield "\n"
                        pos = span[1]
                        continue
                    yield src[pos:slash]
                    pos = slash
                if pos >= len(src):
                    break
                escapesequence = regex.match(cls._EscapeSequence, src, pos=pos + 1)
                if escapesequence:
                    yield cls._parse_template_escape(escapesequence.group(0))
                    pos = escapesequence.span()[1]
                    continue
                nonescape = regex.match(cls._NotEscapeSequence, src, pos=pos + 1)
                if nonescape:
                    raise cls._TemplateFailure
                linecont = regex.match(cls._LineTerminatorSequence, src, pos=pos + 1)
                assert linecont, "Bad logic in _TemplateValue"
                pos = linecont.span()[1]

        try:
            return utf_16_encode("".join(process(tchars)))
        except cls._TemplateFailure:
            return None

    reserved_words = (
        "await",
        "break",
        "case",
        "catch",
        "class",
        "const",
        "continue",
        "debugger",
        "default",
        "delete",
        "do",
        "else",
        "export",
        "extends",
        "finally",
        "for",
        "function",
        "if",
        "import",
        "in",
        "instanceof",
        "new",
        "return",
        "super",
        "switch",
        "this",
        "throw",
        "try",
        "typeof",
        "var",
        "void",
        "while",
        "with",
        "yield",
        "enum",
        "true",
        "false",
        "null",
    )


class Lexer(LexerCore):
    # Adds a bit more state for a more stateful API.
    def __init__(self, source_text):
        super().__init__(source_text)
        self.pos = 0

    def __repr__(self):
        code = f"{self.src[self.pos:self.pos+25]!r}"
        return f"Lexer[pos={self.pos}: {code}]"

    def current_position(self):
        return self.pos

    def reset_position(self, where):
        self.pos = where

    def peek_token(self, count=1, goal=LexerCore.InputElementRegExp):
        def pt(count, pos):
            while count > 0:
                t = self.token(pos, goal)
                if t:
                    pos = t.span.after
                yield t
                count -= 1

        if count == 1:
            return next(pt(1, self.pos))
        return list(pt(count, self.pos))

    def next_token(self, goal=LexerCore.InputElementRegExp):
        t = self.token(self.pos, goal)
        if t:
            self.pos = t.span.after
        return t

    def next_token_if(self, tok_type, prior_newline_allowed=True, goal=LexerCore.InputElementRegExp):
        tok = self.peek_token(1, goal)
        if tok and tok.type == tok_type and (prior_newline_allowed or len(tok.newlines) == 0):
            self.pos = tok.span.after
            return tok
        return None

    def next_id_if(self, id_value, prior_newline_allowed=True, goal=LexerCore.InputElementRegExp):
        tok = self.peek_token(1, goal)
        if (
            tok
            and tok.type == "IDENTIFIER"
            and tok.value == id_value
            and (prior_newline_allowed or len(tok.newlines) == 0)
        ):
            self.pos = tok.span.after
            return tok
        return None

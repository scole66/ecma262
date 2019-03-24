from enum import Enum, auto, unique
from itertools import chain
import unicodedata


class LexerError(Exception):
    def __init__(self, message):
        self.message = message


class Lexer(object):

    @unique
    class Goal(Enum):
        InputElementDiv = auto()
        InputElementRegExp = auto()
        InputElementRegExpOrTemplateTail = auto()
        InputElementTemplateTail = auto()

    @unique
    class Type(Enum):
        Token = auto()
        Whitespace = auto()
        Comment = auto()
        LineTerminator = auto()

    whitespace_a = [
        '\u0009',  # <TAB> CHARACTER TABULATION
        '\u000b',  # <VT> LINE TABULATION
        '\u000c',  # <FF> FORM FEED
        '\u0020',  # <SP> SPACE
        '\u00a0',  # <NBSP> NO-BREAK SPACE
        '\ufeff'  # <ZWNBSP> ZERO WIDTH NO-BREAK SPACE
    ]
    line_terminators = [
        '\u000a',  # <LF> LINE FEED
        '\u000d',  # <CR> CARRIAGE RETURN
        '\u2028',  # <LS> LINE SEPARATOR
        '\u2029'  # <PS> PARAGRAPH SEPARATOR
    ]

    def __init__(self, stream):
        super().__init__()
        self.stream = stream
        self.linenum = 1
        self.gathered_chars = ''

    def _initial(self, ch, lookahead):
        # Empty string means we're done
        if ch == '':
            return (self._done, [])
        # Initial State for InputElementDiv:
        # We're looking at the first char for the production
        # InputElementDiv ::
        #    WhiteSpace
        #    LineTerminator
        #    Comment
        #    CommonToken
        #    DivPunctuator
        #    RightBracePunctuator
        if ch in self.whitespace_a or unicodedata.category(ch) == 'Zs':
            # WhiteSpace
            return (self._initial, [{'type': self.Type.Whitespace, 'value': ch}])
        elif ch in self.line_terminators:
            # LineTerminator
            if ch != '\u000d' or lookahead != '\u000a':
                self.linenum += 1
            return (self._initial, [{'type': self.Type.LineTerminator, 'value': ch}])
        elif ch == '/':
            # Might be Comment::SingleLineComment, Comment::MultiLineComment, DivPunctuator::/, or DivPunctuator::/=
            self.gathered_chars = ch
            return (self._comment_or_div, [])
        elif ch in '(),:;?[]{}~':
            # These are CommonToken::Punctuator or RightBracePunctuator that are uniquely one character in size
            return (self._initial, [{'type': self.Type.Token, 'value': ch}])
        elif ch == '!':
            return (self._bang, [])
        elif ch == '%':
            return (self._percent, [])
        elif ch == '&':
            return (self._ampersand, [])
        elif ch == '*':
            return (self._asterisk, [])
        elif ch == '+':
            return (self._plus, [])
        elif ch == '-':
            return (self._minus, [])
        elif ch == '.':
            return (self._period, [])
        elif ch == '<':
            return (self._less_than, [])
        elif ch == '=':
            return (self._equals, [])
        elif ch == '>':
            return (self._greater_than, [])
        elif ch == '^':
            return (self._caret, [])
        elif ch == '|':
            return (self._pipe, [])

        # NumericLiterals start with the digits 0 through 9, or the period. (Period is handled in self._period.)
        elif ch in '0123456789':
            self.gathered_chars = ch
            return (self._numeric_start, [])

        # IdentifierName also manages to include reserved words (this function also captures the start of a unicode
        # escape sequence).
        elif self.is_identifier_start(ch):
            self.gathered_chars = ch
            if ch == '\\':
                return (self._ident_start_escape, [])
            return (self._ident_capture, [])

        # More to add still...
        return (self._initial, [])

    def _done(self, ch, lookahead):
        pass  # should never get here, honestly.

    def _bang(self, ch, lookahead):
        # We already have !. Might be !, !=, or !==.
        if ch == '=':
            return (self._bang_equals, [])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '!'}], results))

    def _bang_equals(self, ch, lookahead):
        # We already have !=. Might also be !==.
        if ch == '=':
            return (self._initial, [{'type': self.Type.Token, 'value': '!=='}])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '!='}], results))

    def _percent(self, ch, lookahead):
        # We already have %. Might also be %=.
        if ch == '=':
            return (self._initial, [{'type': self.Type.Token, 'value': '%='}])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '%'}], results))

    def _ampersand(self, ch, lookahead):
        # We already have &. Might also be && or &=.
        if ch == '&' or ch == '=':
            return (self._initial, [{'type': self.Type.Token, 'value': '&'+ch}])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '&'}], results))

    def _asterisk(self, ch, lookahead):
        # We already have *. Might also be **, **=, or *=
        if ch == '=':
            return (self._initial, [{'type': self.Type.Token, 'value': '*='}])
        if ch == '*':
            return (self._star_star, [])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '*'}], results))

    def _star_star(self, ch, lookahead):
        # We already have **. Might also be **=.
        if ch == '=':
            return (self._initial, [{'type': self.Type.Token, 'value': '**='}])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '**'}], results))

    def _plus(self, ch, lookahead):
        # We already have +. Might also be ++ or +=.
        if ch == '+' or ch == '=':
            return (self._initial, [{'type': self.Type.Token, 'value': '+'+ch}])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '+'}], results))

    def _minus(self, ch, lookahead):
        # We already have -. Might also be -- or -=.
        if ch == '-' or ch == '=':
            return (self._initial, [{'type': self.Type.Token, 'value': '-'+ch}])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '-'}], results))

    def _period(self, ch, lookahead):
        # We already have '.'. Might also be '...'.
        if ch == '.':
            return (self._dot_dot, [])
        if ch and ch in '0123456789':
            # Hey, look, it's a number.
            self.gathered_chars = '.' + ch
            return (self._after_decimal, [])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '.'}], results))

    def _dot_dot(self, ch, lookahead):
        # We already have '..'. Might also be '...'.
        if ch == '.':
            return (self._initial, [{'type': self.Type.Token, 'value': '...'}])
        # (But there's no '..', so that tokenizes into two individual dots.)
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '.'}, {'type': self.Type.Token, 'value': '.'}], results))

    def _less_than(self, ch, lookahead):
        # We already have '<'. Might also be '<<', '<<=', or '<='.
        if ch == '=':
            return (self._initial, [{'type': self.Type.Token, 'value': '<='}])
        if ch == '<':
            return (self._less_less, [])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '<'}], results))

    def _less_less(self, ch, lookahead):
        # We have <<. Might also be <<=.
        if ch == '=':
            return (self._initial, [{'type': self.Type.Token, 'value': '<<='}])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '<<'}], results))

    def _equals(self, ch, lookahead):
        # We have =. Might also be ==, ===, or =>.
        if ch == '>':
            return (self._initial, [{'type': self.Type.Token, 'value': '=>'}])
        if ch == '=':
            return (self._equal_equal, [])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '='}], results))

    def _equal_equal(self, ch, lookahead):
        # We have ==. Might also be ===.
        if ch == '=':
            return (self._initial, [{'type': self.Type.Token, 'value': '==='}])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '=='}], results))

    def _greater_than(self, ch, lookahead):
        # We have >. Might also be >=, >>, >>=, >>>, or >>>=.
        if ch == '=':
            return (self._initial, [{'type': self.Type.Token, 'value': '>='}])
        if ch == '>':
            return (self._greater_greater, [])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '>'}], results))

    def _greater_greater(self, ch, lookahead):
        # We have >>. Might also be >>=, >>>, or >>>=.
        if ch == '=':
            return (self._initial, [{'type': self.Type.Token, 'value': '>>='}])
        if ch == '>':
            return (self._greater_x3, [])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '>>'}], results))

    def _greater_x3(self, ch, lookahead):
        # We have >>>. Might also be >>>=.
        if ch == '=':
            return (self._initial, [{'type': self.Type.Token, 'value': '>>>='}])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '>>>'}], results))

    def _caret(self, ch, lookahead):
        # We have ^. Might also be ^=.
        if ch == '=':
            return (self._initial, [{'type': self.Type.Token, 'value': '^='}])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '^'}], results))

    def _pipe(self, ch, lookahead):
        # We have |. Might also be |= or ||.
        if ch and ch in '=|':
            return (self._initial, [{'type': self.Type.Token, 'value': '|'+ch}])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '|'}], results))

    def _comment_or_div(self, ch, lookahead):
        # We have one slash. All kinds of things this might be.
        if ch == '=':
            return (self._initial, [{'type': self.Type.Token, 'value': '/='}])
        if ch == '/':
            # A SingleLineComment
            self.gathered_chars = '//'
            return (self._single_line_comment, [])
        if ch == '*':
            # A MultiLineComment
            self.gathered_chars = '/*'
            return (self._multi_line_comment, [])
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': '/'}], results))

    def _single_line_comment(self, ch, lookahead):
        if ch != '' and ch not in self.line_terminators:
            self.gathered_chars += ch
            return (self._single_line_comment, [])
        # capture the gathered chars, else the call to _initial might wipe it out
        comment_value = self.gathered_chars
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Comment, 'value': comment_value}], results))

    def _multi_line_comment(self, ch, lookahead):
        if ch == '':
            # Hit EOF! That's an unterminated comment error.
            raise LexerError('Unterminated multi-line comment')
        self.gathered_chars += ch
        if ch == '/' and len(self.gathered_chars) >= 4 and self.gathered_chars[-2] == '*':
            return (self._initial, [{'type': self.Type.Comment, 'value': self.gathered_chars}])
        return (self._multi_line_comment, [])

    def _numeric_start(self, ch, lookahead):
        # We have the first char of a number (though, not a period)
        # The new char might indicate a different base. Check that.
        if self.gathered_chars == '0':
            if ch and ch in 'bB':
                self.gathered_chars += ch
                return (self._binary_digits, [])
            if ch and ch in 'oO':
                self.gathered_chars += ch
                return (self._octal_digits, [])
            if ch and ch in 'xX':
                self.gathered_chars += ch
                return (self._hex_digits, [])
            if ch and ch == '.':
                self.gathered_chars += ch
                return (self._after_decimal, [])
            if ch and ch in 'eE':
                self.gathered_chars += ch
                return (self._after_e, [])
        elif ch and ch in '0123456789':
            self.gathered_chars += ch
            return (self._integer_part, [])
        elif ch and ch in 'eE':
            self.gathered_chars += ch
            return (self._after_e, [])
        elif ch == '.':
            self.gathered_chars += ch
            return (self._after_decimal, [])

        if ch and (ch in '0123456789' or self.is_identifier_start(ch)):
            raise LexerError('Invalid chars after Numeric Literal')

        # capture the gathered chars, else the call to _initial might wipe it out
        captured_value = self.gathered_chars
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': captured_value}], results))

    def _binary_digits(self, ch, lookahead):
        if ch and ch in '01':
            self.gathered_chars += ch
            return (self._binary_digits, [])
        if ch and (ch in '23456789' or self.is_identifier_start(ch)):
            raise LexerError('Invalid chars after Numeric Literal')
        if self.gathered_chars[-1] not in '01':
            raise LexerError('Invalid numeric literal')
        # capture the gathered chars, else the call to _initial might wipe it out
        captured_value = self.gathered_chars
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': captured_value}], results))

    def _octal_digits(self, ch, lookahead):
        if ch and ch in '01234567':
            self.gathered_chars += ch
            return (self._octal_digits, [])
        if ch and (ch in '89' or self.is_identifier_start(ch)):
            raise LexerError('Invalid chars after Numeric Literal')
        if self.gathered_chars[-1] not in '01234567':
            raise LexerError('Invalid numeric literal')
        # capture the gathered chars, else the call to _initial might wipe it out
        captured_value = self.gathered_chars
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': captured_value}], results))

    def _hex_digits(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            self.gathered_chars += ch
            return (self._hex_digits, [])
        if ch and self.is_identifier_start(ch):
            raise LexerError('Invalid chars after Numeric Literal')
        if self.gathered_chars[-1] not in '0123456789abcdefABCDEF':
            raise LexerError('Invalid numeric literal')
        # capture the gathered chars, else the call to _initial might wipe it out
        captured_value = self.gathered_chars
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': captured_value}], results))

    def _integer_part(self, ch, lookahead):
        if ch and ch in '0123456789':
            self.gathered_chars += ch
            return (self._integer_part, [])
        if ch == '.':
            self.gathered_chars += ch
            return (self._after_decimal, [])
        if ch and ch in 'eE':
            self.gathered_chars += ch
            return (self._after_e, [])

        if ch and self.is_identifier_start(ch):
            raise LexerError('Invalid chars after Numeric Literal')
        # capture the gathered chars, else the call to _initial might wipe it out
        captured_value = self.gathered_chars
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': captured_value}], results))

    def _after_decimal(self, ch, lookahead):
        if ch and ch in '0123456789':
            self.gathered_chars += ch
            return (self._after_decimal, [])
        if ch and ch in 'eE':
            self.gathered_chars += ch
            return (self._after_e, [])

        if ch and self.is_identifier_start(ch):
            raise LexerError('Invalid chars after Numeric Literal')
        # capture the gathered chars, else the call to _initial might wipe it out
        captured_value = self.gathered_chars
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': captured_value}], results))

    def _after_e(self, ch, lookahead):
        if ch and ch in '-+0123456789':
            self.gathered_chars += ch
            return (self._exponent_digits, [])
        # If we saw an 'e' but no number following, we wind up with a syntax error, thanks to the rule: "The
        # SourceCharacter immediately following a NumericLiteral must not be an IdentifierStart or DecimalDigit."
        #
        # I.e.: the lexical rules actually say that "3e" is the NumericLiteral "3" followed by the Identifier "e", but
        # the rule makes that a syntax error. (Note that "3e1" is a NumericLiteral on its own. Splitting it into two
        # tokens in the prior case makes things very confusing.) Anyway: we don't have to get tricky here, we just
        # raise an exception.
        raise LexerError('Invalid numeric literal')

    def _exponent_digits(self, ch, lookahead):
        if ch and ch in '0123456789':
            self.gathered_chars += ch
            return (self._exponent_digits, [])
        if ch and self.is_identifier_start(ch):
            raise LexerError('Invalid chars after Numeric Literal')
        # capture the gathered chars, else the call to _initial might wipe it out
        captured_value = self.gathered_chars
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': captured_value}], results))

    @staticmethod
    def is_unicode_id_start(ch):
        # ID_Start characters are derived from the Unicode General_Category of uppercase letters, lowercase letters,
        # titlecase letters, modifier letters, other letters, letter numbers, plus Other_ID_Start, minus Pattern_Syntax and
        # Pattern_White_Space code points.

        # The General_Category parts of that are: Lu Ll Lt Lm Lo and Nl.
        # Other_ID_Start is U+1885, U+1886, U+2118, U+212E, U+309B, and U+309C (which have categories Mn Sm So and Sk).
        # Pattern_Syntax is a long list, but doesn't have anything from Other_ID_Start, and only 2E2F (vertical tilde) from
        # the category matches.
        # Pattern_White_Space is short, but doesn't have anything from the prior categories, so I'm not sure why it's even
        # listed.

        cat = unicodedata.category(ch)
        return ((cat in ['Lu', 'Ll', 'Lt', 'Lm', 'Lo', 'Nl'] and ch != '\u2e2f') or
                ch in '\u1885\u1886\u2118\u212e\u309b\u309c')

    @classmethod
    def is_unicode_id_continue(cls, ch):
        # ID_Continue characters include ID_Start characters, plus characters having the Unicode General_Category of
        # nonspacing marks, spacing combining marks, decimal number, connector punctuation, plus Other_ID_Continue , minus
        # Pattern_Syntax and Pattern_White_Space code points.

        # In set notation:
        # [\p{ID_Start}\p{Mn}\p{Mc}\p{Nd}\p{Pc}\p{Other_ID_Continue}-\p{Pattern_Syntax}-\p{Pattern_White_Space}]

        # ================================================
        #
        # 00B7          ; Other_ID_Continue # Po       MIDDLE DOT
        # 0387          ; Other_ID_Continue # Po       GREEK ANO TELEIA
        # 1369..1371    ; Other_ID_Continue # No   [9] ETHIOPIC DIGIT ONE..ETHIOPIC DIGIT NINE
        # 19DA          ; Other_ID_Continue # No       NEW TAI LUE THAM DIGIT ONE
        #
        # Total code points: 12
        other_id_continue = '\u00b7\u0387\u1369\u136a\u136b\u136c\u136d\u136e\u136f\u1370\u1371\u19da'

        # Again, the Pattern_Syntax and Pattern_White_Space don't actually take anything out -- those chars aren't in the
        # valid list to begin with.
        return (cls.is_unicode_id_start(ch) or
                (unicodedata.category(ch) in [ 'Mn', 'Mc', 'Nd', 'Pc' ] or ch in other_id_continue))

    @classmethod
    def is_identifier_start(cls, ch):
        return cls.is_unicode_id_start(ch) or ch in '$_\\'

    def _ident_capture(self, ch, lookahead):
        if ch and (self.is_unicode_id_continue(ch) or ch in '$\u200c\u200d'):
            self.gathered_chars += ch
            return (self._ident_capture, [])
        if ch == '\\':
            self.gathered_chars += ch
            return (self._identpart_escape_1, [])
        captured_value = self.gathered_chars
        state, result = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': captured_value}], result))

    def _identpart_escape_1(self, ch, lookahead):
        if ch != 'u':
            raise LexerError('Invalid IdentifierName escape sequence')
        self.gathered_chars += 'u'
        return (self._identpart_escape_2, [])

    def _ident_start_escape(self, ch, lookahead):
        if ch != 'u':
            raise LexerError('Invalid IdentifierName escape sequence')
        self.gathered_chars += 'u'
        return (self._identstart_escape_2, [])

    def _identpart_escape_2(self, ch, lookahead):
        # We have '\u'. Next is either exactly 4 hex digits, or '{' <many digits> '}'.
        if ch and ch in '0123456789abcdefABCDEF':
            self.gathered_chars += ch
            return (self._3_digits_left, [])
        if ch == '{':
            self.gathered_chars += '{'
            return (self._n_digits_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _identstart_escape_2(self, ch, lookahead):
        # We have '\u'. Next is either exactly 4 hex digits, or '{' <many digits> '}'.
        if ch and ch in '0123456789abcdefABCDEF':
            self.gathered_chars += ch
            return (self._istart_3_digits_left, [])
        if ch == '{':
            self.gathered_chars += '{'
            return (self._istart_n_digits_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _3_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            self.gathered_chars += ch
            return (self._2_digits_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _istart_3_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            self.gathered_chars += ch
            return (self._istart_2_digits_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _2_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            self.gathered_chars += ch
            return (self._1_digit_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _istart_2_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            self.gathered_chars += ch
            return (self._istart_1_digit_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _1_digit_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            self.gathered_chars += ch
            escaped_char = chr(int(self.gathered_chars[-4:], 16))
            if self.is_unicode_id_continue(escaped_char) or escaped_char in '$\u200c\u200d':
                return (self._ident_capture, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _istart_1_digit_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            self.gathered_chars += ch
            escaped_char = chr(int(self.gathered_chars[-4:], 16))
            if self.is_unicode_id_start(escaped_char) or escaped_char in '$_':
                return (self._ident_capture, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _n_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            self.gathered_chars += ch
            return (self._n_digits_left, [])
        if ch == '}':
            self.gathered_chars += ch
            charcode = int(self.gathered_chars[self.gathered_chars.rfind('{')+1:-1], 16)
            if charcode <= 0x10FFFF:
                escaped_char = chr(charcode)
                if self.is_unicode_id_continue(escaped_char) or escaped_char in '$\u200c\u200d':
                    return (self._ident_capture, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _istart_n_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            self.gathered_chars += ch
            return (self._istart_n_digits_left, [])
        if ch == '}':
            self.gathered_chars += ch
            charcode = int(self.gathered_chars[self.gathered_chars.rfind('{')+1:-1], 16)
            if charcode <= 0x10FFFF:
                escaped_char = chr(charcode)
                if self.is_unicode_id_start(escaped_char) or escaped_char in '$_':
                    return (self._ident_capture, [])
        raise LexerError('Invalid IdentifierName escape sequence')



    def lex(self, goal=Goal.InputElementDiv):
        state = self._initial

        ch = self.stream.read(1)
        lookahead = self.stream.read(1)

        while state != self._done:
            state, results = state(ch, lookahead)
            for rval in results:
                yield rval
            ch = lookahead
            lookahead = self.stream.read(1)

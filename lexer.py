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

    class Token(object):
        def __init__(self, lexer, type, start, length):
            self.lexer = lexer
            self.type = type
            self.start = start
            self.length = length
        def value(self):
            return self.lexer.source[self.start:self.start+self.length]

    whitespace_a = (
        '\u0009'  # <TAB> CHARACTER TABULATION
        '\u000b'  # <VT> LINE TABULATION
        '\u000c'  # <FF> FORM FEED
        '\u0020'  # <SP> SPACE
        '\u00a0'  # <NBSP> NO-BREAK SPACE
        '\ufeff'  # <ZWNBSP> ZERO WIDTH NO-BREAK SPACE
    )
    line_terminators = (
        '\u000a'  # <LF> LINE FEED
        '\u000d'  # <CR> CARRIAGE RETURN
        '\u2028'  # <LS> LINE SEPARATOR
        '\u2029'  # <PS> PARAGRAPH SEPARATOR
    )

    def __init__(self, source_text):
        super().__init__()
        self.source = source_text
        self.linenum = 1
        self.start = 0
        self.pos = 0

    def _make_token(self, type, end_prior):
        where = self.pos
        length = where - self.start
        if end_prior:
            length -= 1
        tok = self.Token(self, type, self.start, length)
        self.start += length
        return tok

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
            return (self._initial, [self._make_token(self.Type.Whitespace, False)])
        elif ch in self.line_terminators:
            # LineTerminator
            if ch != '\u000d' or lookahead != '\u000a':
                self.linenum += 1
            return (self._initial, [self._make_token(self.Type.LineTerminator, False)])
        elif ch == '/':
            # Might be Comment::SingleLineComment, Comment::MultiLineComment, DivPunctuator::/, or DivPunctuator::/=
            return (self._comment_or_div, [])
        elif ch in '(),:;?[]{}~':
            # These are CommonToken::Punctuator or RightBracePunctuator that are uniquely one character in size
            return (self._initial, [self._make_token(self.Type.Token, False)])
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
            return (self._numeric_start, [])

        # IdentifierName also manages to include reserved words (this function also captures the start of a unicode
        # escape sequence).
        elif self.is_identifier_start(ch):
            if ch == '\\':
                return (self._ident_start_escape, [])
            return (self._ident_capture, [])

        elif ch == "'":
            return (self._single_string_capture, [])
        elif ch == '"':
            return (self._double_string_capture, [])

        # More to add still...
        return (self._initial, [])

    def _done(self, ch, lookahead):
        pass  # should never get here, honestly.

    def _bang(self, ch, lookahead):
        # We already have !. Might be !, !=, or !==.
        if ch == '=':
            return (self._bang_equals, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _bang_equals(self, ch, lookahead):
        # We already have !=. Might also be !==.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _percent(self, ch, lookahead):
        # We already have %. Might also be %=.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _ampersand(self, ch, lookahead):
        # We already have &. Might also be && or &=.
        if ch == '&' or ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _asterisk(self, ch, lookahead):
        # We already have *. Might also be **, **=, or *=
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        if ch == '*':
            return (self._star_star, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _star_star(self, ch, lookahead):
        # We already have **. Might also be **=.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _plus(self, ch, lookahead):
        # We already have +. Might also be ++ or +=.
        if ch == '+' or ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _minus(self, ch, lookahead):
        # We already have -. Might also be -- or -=.
        if ch == '-' or ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _period(self, ch, lookahead):
        # We already have '.'. Might also be '...'.
        if ch == '.':
            return (self._dot_dot, [])
        if ch and ch in '0123456789':
            # Hey, look, it's a number.
            return (self._after_decimal, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _dot_dot(self, ch, lookahead):
        # We already have '..'. Might also be '...'.
        if ch == '.':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        # (But there's no '..', so that tokenizes into two individual dots.)
        tok = self._make_token(self.Type.Token, True) # This is a '..' token...
        tok.length -= 1 # Convert it to '.' (the first dot)
        self.start -= 1 # back up start to point to the 2nd dot
        tok2 = self._make_token(self.Type.Token, True) # This just got the 2nd dot
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok, tok2], results))

    def _less_than(self, ch, lookahead):
        # We already have '<'. Might also be '<<', '<<=', or '<='.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        if ch == '<':
            return (self._less_less, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _less_less(self, ch, lookahead):
        # We have <<. Might also be <<=.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _equals(self, ch, lookahead):
        # We have =. Might also be ==, ===, or =>.
        if ch == '>':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        if ch == '=':
            return (self._equal_equal, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _equal_equal(self, ch, lookahead):
        # We have ==. Might also be ===.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _greater_than(self, ch, lookahead):
        # We have >. Might also be >=, >>, >>=, >>>, or >>>=.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        if ch == '>':
            return (self._greater_greater, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _greater_greater(self, ch, lookahead):
        # We have >>. Might also be >>=, >>>, or >>>=.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        if ch == '>':
            return (self._greater_x3, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _greater_x3(self, ch, lookahead):
        # We have >>>. Might also be >>>=.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _caret(self, ch, lookahead):
        # We have ^. Might also be ^=.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _pipe(self, ch, lookahead):
        # We have |. Might also be |= or ||.
        if ch and ch in '=|':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _comment_or_div(self, ch, lookahead):
        # We have one slash. All kinds of things this might be.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        if ch == '/':
            # A SingleLineComment
            return (self._single_line_comment, [])
        if ch == '*':
            # A MultiLineComment
            return (self._multi_line_comment, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _single_line_comment(self, ch, lookahead):
        if ch and ch not in self.line_terminators:
            return (self._single_line_comment, [])
        tok = self._make_token(self.Type.Comment, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _multi_line_comment(self, ch, lookahead):
        if ch == '':
            # Hit EOF! That's an unterminated comment error.
            raise LexerError('Unterminated multi-line comment')
        pos = self.pos
        if ch == '/' and (pos - self.start) >= 4 and self.source[pos-2] == '*':
            return (self._initial, [self._make_token(self.Type.Comment, False)])
        return (self._multi_line_comment, [])

    def _numeric_start(self, ch, lookahead):
        # We have the first char of a number (though, not a period)
        # The new char might indicate a different base. Check that.
        if self.source[self.start] == '0':
            if ch and ch in 'bB':
                return (self._binary_digits, [])
            if ch and ch in 'oO':
                return (self._octal_digits, [])
            if ch and ch in 'xX':
                return (self._hex_digits, [])
            if ch and ch == '.':
                return (self._after_decimal, [])
            if ch and ch in 'eE':
                return (self._after_e, [])
        elif ch and ch in '0123456789':
            return (self._integer_part, [])
        elif ch and ch in 'eE':
            return (self._after_e, [])
        elif ch == '.':
            return (self._after_decimal, [])

        if ch and (ch in '0123456789' or self.is_identifier_start(ch)):
            raise LexerError('Invalid chars after Numeric Literal')

        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _binary_digits(self, ch, lookahead):
        if ch and ch in '01':
            return (self._binary_digits, [])
        if ch and (ch in '23456789' or self.is_identifier_start(ch)):
            raise LexerError('Invalid chars after Numeric Literal')
        if self.source[self.pos-2] not in '01':
            raise LexerError('Invalid numeric literal')
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _octal_digits(self, ch, lookahead):
        if ch and ch in '01234567':
            return (self._octal_digits, [])
        if ch and (ch in '89' or self.is_identifier_start(ch)):
            raise LexerError('Invalid chars after Numeric Literal')
        if self.source[self.pos-2] not in '01234567':
            raise LexerError('Invalid numeric literal')
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _hex_digits(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._hex_digits, [])
        if ch and self.is_identifier_start(ch):
            raise LexerError('Invalid chars after Numeric Literal')
        if self.source[self.pos-2] not in '0123456789abcdefABCDEF':
            raise LexerError('Invalid numeric literal')
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _integer_part(self, ch, lookahead):
        if ch and ch in '0123456789':
            return (self._integer_part, [])
        if ch == '.':
            return (self._after_decimal, [])
        if ch and ch in 'eE':
            return (self._after_e, [])

        if ch and self.is_identifier_start(ch):
            raise LexerError('Invalid chars after Numeric Literal')
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _after_decimal(self, ch, lookahead):
        if ch and ch in '0123456789':
            return (self._after_decimal, [])
        if ch and ch in 'eE':
            return (self._after_e, [])

        if ch and self.is_identifier_start(ch):
            raise LexerError('Invalid chars after Numeric Literal')
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _after_e(self, ch, lookahead):
        if ch and ch in '-+0123456789':
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
            return (self._exponent_digits, [])
        if ch and self.is_identifier_start(ch):
            raise LexerError('Invalid chars after Numeric Literal')
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

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
            return (self._ident_capture, [])
        if ch == '\\':
            return (self._identpart_escape_1, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _identpart_escape_1(self, ch, lookahead):
        if ch != 'u':
            raise LexerError('Invalid IdentifierName escape sequence')
        return (self._identpart_escape_2, [])

    def _ident_start_escape(self, ch, lookahead):
        if ch != 'u':
            raise LexerError('Invalid IdentifierName escape sequence')
        return (self._identstart_escape_2, [])

    def _identpart_escape_2(self, ch, lookahead):
        # We have '\u'. Next is either exactly 4 hex digits, or '{' <many digits> '}'.
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._3_digits_left, [])
        if ch == '{':
            return (self._n_digits_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _identstart_escape_2(self, ch, lookahead):
        # We have '\u'. Next is either exactly 4 hex digits, or '{' <many digits> '}'.
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._istart_3_digits_left, [])
        if ch == '{':
            return (self._istart_n_digits_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _3_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._2_digits_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _istart_3_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._istart_2_digits_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _2_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._1_digit_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _istart_2_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._istart_1_digit_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _1_digit_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            pos = self.pos
            escaped_char = chr(int(self.source[pos-4:pos], 16))
            if self.is_unicode_id_continue(escaped_char) or escaped_char in '$\u200c\u200d':
                return (self._ident_capture, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _istart_1_digit_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            pos = self.pos
            escaped_char = chr(int(self.source[pos-4:pos], 16))
            if self.is_unicode_id_start(escaped_char) or escaped_char in '$_':
                return (self._ident_capture, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _n_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._n_digits_left, [])
        pos = self.pos
        if ch == '}':
            if self.source[pos-2] == '{':
                # No digits!
                raise LexerError('Invalid IdentifierName escape sequence')
            bracket = self.source.rfind('{', self.start, pos)
            charcode = int(self.source[bracket+1:pos-1], 16)
            if charcode <= 0x10FFFF:
                escaped_char = chr(charcode)
                if self.is_unicode_id_continue(escaped_char) or escaped_char in '$\u200c\u200d':
                    return (self._ident_capture, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _istart_n_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._istart_n_digits_left, [])
        pos = self.pos
        if ch == '}':
            if self.source[pos-2] == '{':
                # No digits!
                raise LexerError('Invalid IdentifierName escape sequence')
            bracket = self.source.rfind('{', self.start, pos)
            charcode = int(self.source[bracket+1:pos-1], 16)
            if charcode <= 0x10FFFF:
                escaped_char = chr(charcode)
                if self.is_unicode_id_start(escaped_char) or escaped_char in '$_':
                    return (self._ident_capture, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _single_string_capture(self, ch, lookahead):
        if ch == "'":
            return (self._initial, [self._make_token(self.Type.Token, False)])
        if ch == '\\':
            return (self._string_escape, [])
        if ch == '' or ch in self.line_terminators:
            raise LexerError('Unterminated String')
        return (self._single_string_capture, [])

    def _string_escape(self, ch, lookahead):
        # We've already consumed the leading slash of one of:
        #  *  SingleStringCharacter :: \ EscapeSequence
        #  *  SingleStringCharacter :: LineContinuation :: \ LineTerminatorSequence
        if ch == '':
            raise LexerError('Syntax Error: Unterminated string escape')
        if ch in self.line_terminators:
            # This is the LineContinuation bit
            if ch != '\r' or lookahead != '\n':
                return (self._single_string_capture, [])
            return (self._single_string_lfonly, [])
        if ch == '0' and (lookahead == '' or lookahead not in '0123456789'):
            # EscapeSequence :: 0 (lookahead not in DecimalDigit)
            return (self._single_string_capture, [])
        if ch == 'x':
            return (self._single_string_hex_escape, [])
        if ch == 'u':
            return (self._single_string_unicode_escape, [])
        if ch in '0123456789':
            raise LexerError('Syntax Error in string escape')
        return (self._single_string_capture, [])

    def _single_string_lfonly(self, ch, lookahead):
        # we already know ch is \n, so just capture and move on
        return (self._single_string_capture, [])

    def _single_string_hex_escape(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            if self.source[self.pos-2] == 'x':
                return (self._single_string_hex_escape, [])
            return (self._single_string_capture, [])
        raise LexerError('Syntax Error in Hex Value for String Escape')

    def _single_string_unicode_escape(self, ch, lookahead):
        # Either 4 hex digits, or '{' <many hex digits> '}'
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._single_string_unicode_4digits, [])
        if ch == '{':
            return (self._single_string_unicode_brackets, [])
        raise LexerError('Syntax Error in Unicode String Escape')

    def _single_string_unicode_4digits(self, ch, lookahead):
        # We've gotten \u<digit> .. with maybe more digits.
        if ch and ch in '0123456789abcdefABCDEF':
            upos = self.source.rfind('u', self.start, self.pos)
            if self.pos - upos == 5:
                return (self._single_string_capture, [])
            return (self._single_string_unicode_4digits, [])
        raise LexerError('Syntax Error in Unicode String Escape')

    def _single_string_unicode_brackets(self, ch, lookahead):
        # We've gotten \u{ and maybe digits
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._single_string_unicode_brackets, [])
        if ch == '}' and self.source[self.pos-2] != '{':
            bracket = self.source.rfind('{', self.start, self.pos)
            charcode = int(self.source[bracket+1:self.pos-1], 16)
            if charcode <= 0x10FFFF:
                return (self._single_string_capture, [])
        raise LexerError('Syntax Error in Unicode String Escape')

    def lex(self, goal=Goal.InputElementDiv):
        state = self._initial

        ch = self.source[self.pos]
        try:
            lookahead = self.source[self.pos+1]
        except IndexError:
            lookahead = ''
        self.pos += 1

        while state != self._done:
            state, results = state(ch, lookahead)
            for rval in results:
                yield rval
            ch = lookahead
            try:
                lookahead = self.source[self.pos+1]
            except IndexError:
                lookahead = ''
            self.pos += 1

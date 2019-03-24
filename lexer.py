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
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Comment, 'value': self.gathered_chars}], results))

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

        if ch and ch in '0123456789':
            # Also needs IdentifierStart in that.
            raise LexerError('Invalid chars after Numeric Literal')

        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': self.gathered_chars}], results))

    def _binary_digits(self, ch, lookahead):
        if ch and ch in '01':
            self.gathered_chars += ch
            return (self._binary_digits, [])
        if ch and ch in '23456789':
            # Also needs IdentifierStart in that.
            raise LexerError('Invalid chars after Numeric Literal')
        if self.gathered_chars[-1] not in '01':
            raise LexerError('Invalid numeric literal')
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': self.gathered_chars}], results))

    def _octal_digits(self, ch, lookahead):
        if ch and ch in '01234567':
            self.gathered_chars += ch
            return (self._octal_digits, [])
        if ch and ch in '89':
            # Also needs IdentifierStart in that.
            raise LexerError('Invalid chars after Numeric Literal')
        if self.gathered_chars[-1] not in '01234567':
            raise LexerError('Invalid numeric literal')
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': self.gathered_chars}], results))

    def _hex_digits(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            self.gathered_chars += ch
            return (self._hex_digits, [])
#        if ch in ??:
            # Also needs IdentifierStart in that.
#            raise LexerError('Invalid chars after Numeric Literal')
        if self.gathered_chars[-1] not in '0123456789abcdefABCDEF':
            raise LexerError('Invalid numeric literal')
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': self.gathered_chars}], results))

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

        # if ch in IndentifierStart, abort
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': self.gathered_chars}], results))

    def _after_decimal(self, ch, lookahead):
        if ch and ch in '0123456789':
            self.gathered_chars += ch
            return (self._after_decimal, [])
        if ch and ch in 'eE':
            self.gathered_chars += ch
            return (self._after_e, [])

        # if ch in IndentierStart, abort
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': self.gathered_chars}], results))

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
        # if ch in IdentifierStart, abort
        state, results = self._initial(ch, lookahead)
        return (state, chain([{'type': self.Type.Token, 'value': self.gathered_chars}], results))

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

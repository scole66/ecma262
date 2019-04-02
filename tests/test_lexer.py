import pytest

from ecmascript import *


def test_lex_simple_sample():
    input_text = "goat = pig + beard - curly tail"
    l = Lexer(input_text)
    result = [{'type': token.type, 'value': token.value()} for token in l.lex()]

    expected = [
        {'type': Lexer.Type.Token, 'value': 'goat'},
        {'type': Lexer.Type.Whitespace, 'value': ' '},
        {'type': Lexer.Type.Token, 'value': '='},
        {'type': Lexer.Type.Whitespace, 'value': ' '},
        {'type': Lexer.Type.Token, 'value': 'pig'},
        {'type': Lexer.Type.Whitespace, 'value': ' '},
        {'type': Lexer.Type.Token, 'value': '+'},
        {'type': Lexer.Type.Whitespace, 'value': ' '},
        {'type': Lexer.Type.Token, 'value': 'beard'},
        {'type': Lexer.Type.Whitespace, 'value': ' '},
        {'type': Lexer.Type.Token, 'value': '-'},
        {'type': Lexer.Type.Whitespace, 'value': ' '},
        {'type': Lexer.Type.Token, 'value': 'curly'},
        {'type': Lexer.Type.Whitespace, 'value': ' '},
        {'type': Lexer.Type.Token, 'value': 'tail'},
    ]

    assert result == expected


@pytest.mark.parametrize('test_input,expected',
                         [
                             ('%', [{'type': Lexer.Type.Token, 'value': '%'}]),
                             ('!', [{'type': Lexer.Type.Token, 'value': '!'}]),
                             ('!=', [{'type': Lexer.Type.Token, 'value': '!='}]),
                             ('!==', [{'type': Lexer.Type.Token, 'value': '!=='}]),
                             ('%=', [{'type': Lexer.Type.Token, 'value': '%='}]),
                             ('&', [{'type': Lexer.Type.Token, 'value': '&'}]),
                             ('&&', [{'type': Lexer.Type.Token, 'value': '&&'}]),
                             ('&=', [{'type': Lexer.Type.Token, 'value': '&='}]),
                             ('(', [{'type': Lexer.Type.Token, 'value': '('}]),
                             (')', [{'type': Lexer.Type.Token, 'value': ')'}]),
                             ('*', [{'type': Lexer.Type.Token, 'value': '*'}]),
                             ('**', [{'type': Lexer.Type.Token, 'value': '**'}]),
                             ('**=', [{'type': Lexer.Type.Token, 'value': '**='}]),
                             ('*=', [{'type': Lexer.Type.Token, 'value': '*='}]),
                             ('+', [{'type': Lexer.Type.Token, 'value': '+'}]),
                             ('++', [{'type': Lexer.Type.Token, 'value': '++'}]),
                             ('+=', [{'type': Lexer.Type.Token, 'value': '+='}]),
                             (',', [{'type': Lexer.Type.Token, 'value': ','}]),
                             ('-', [{'type': Lexer.Type.Token, 'value': '-'}]),
                             ('--', [{'type': Lexer.Type.Token, 'value': '--'}]),
                             ('-=', [{'type': Lexer.Type.Token, 'value': '-='}]),
                             ('.', [{'type': Lexer.Type.Token, 'value': '.'}]),
                             ('...', [{'type': Lexer.Type.Token, 'value': '...'}]),
                             (':', [{'type': Lexer.Type.Token, 'value': ':'}]),
                             (';', [{'type': Lexer.Type.Token, 'value': ';'}]),
                             ('<', [{'type': Lexer.Type.Token, 'value': '<'}]),
                             ('<<', [{'type': Lexer.Type.Token, 'value': '<<'}]),
                             ('<<=', [{'type': Lexer.Type.Token, 'value': '<<='}]),
                             ('<=', [{'type': Lexer.Type.Token, 'value': '<='}]),
                             ('=', [{'type': Lexer.Type.Token, 'value': '='}]),
                             ('==', [{'type': Lexer.Type.Token, 'value': '=='}]),
                             ('===', [{'type': Lexer.Type.Token, 'value': '==='}]),
                             ('=>', [{'type': Lexer.Type.Token, 'value': '=>'}]),
                             ('>', [{'type': Lexer.Type.Token, 'value': '>'}]),
                             ('>=', [{'type': Lexer.Type.Token, 'value': '>='}]),
                             ('>>', [{'type': Lexer.Type.Token, 'value': '>>'}]),
                             ('>>=', [{'type': Lexer.Type.Token, 'value': '>>='}]),
                             ('>>>', [{'type': Lexer.Type.Token, 'value': '>>>'}]),
                             ('>>>=', [
                              {'type': Lexer.Type.Token, 'value': '>>>='}]),
                             ('?', [{'type': Lexer.Type.Token, 'value': '?'}]),
                             ('[', [{'type': Lexer.Type.Token, 'value': '['}]),
                             (']', [{'type': Lexer.Type.Token, 'value': ']'}]),
                             ('^', [{'type': Lexer.Type.Token, 'value': '^'}]),
                             ('^=', [{'type': Lexer.Type.Token, 'value': '^='}]),
                             ('{', [{'type': Lexer.Type.Token, 'value': '{'}]),
                             ('}', [{'type': Lexer.Type.Token, 'value': '}'}]),
                             ('|', [{'type': Lexer.Type.Token, 'value': '|'}]),
                             ('|=', [{'type': Lexer.Type.Token, 'value': '|='}]),
                             ('||', [{'type': Lexer.Type.Token, 'value': '||'}]),
                             ('/', [{'type': Lexer.Type.Token, 'value': '/'}]),
                             ('/=', [{'type': Lexer.Type.Token, 'value': '/='}]),
                             ('~', [{'type': Lexer.Type.Token, 'value': '~'}]),
                             ('..', [{'type': Lexer.Type.Token, 'value': '.'},
                                     {'type': Lexer.Type.Token, 'value': '.'}]),
                             ('**!==+&!-| ', [{'type': Lexer.Type.Token, 'value': '**'}, {'type': Lexer.Type.Token, 'value': '!=='},
                                              {'type': Lexer.Type.Token, 'value': '+'}, {
                                 'type': Lexer.Type.Token, 'value': '&'}, {'type': Lexer.Type.Token, 'value': '!'},
                                 {'type': Lexer.Type.Token, 'value': '-'},
                                 {'type': Lexer.Type.Token, 'value': '|'},
                                 {'type': Lexer.Type.Whitespace, 'value': ' '}]),
                             (' \u2007', [{'type': Lexer.Type.Whitespace, 'value': ' '}, {
                                 'type': Lexer.Type.Whitespace, 'value': '\u2007'}]),
                             ('\r\n\u2028\u2029', [{'type': Lexer.Type.LineTerminator, 'value': '\r'},
                                                   {'type': Lexer.Type.LineTerminator,
                                                       'value': '\n'},
                                                   {'type': Lexer.Type.LineTerminator,
                                                       'value': '\u2028'},
                                                   {'type': Lexer.Type.LineTerminator, 'value': '\u2029'}]),
                            ('!// This is a comment', [{'type': Lexer.Type.Token, 'value': '!'},
                                {'type': Lexer.Type.Comment, 'value': '// This is a comment'}]),
                            ('''%/* comment that
can even be
multiple lines!
*/''', [{'type': Lexer.Type.Token, 'value': '%'}, {'type': Lexer.Type.Comment, 'value': '''/* comment that
can even be
multiple lines!
*/'''}]),
                            ('//comment\n', [{'type': Lexer.Type.Comment, 'value': '//comment'},
                                             {'type': Lexer.Type.LineTerminator, 'value': '\n'}]),
                            ('/* comment 1 *//* comment 2 */', [{'type': Lexer.Type.Comment, 'value': '/* comment 1 */'},
                                                                {'type': Lexer.Type.Comment, 'value': '/* comment 2 */'}]),
                            ('0', [{'type': Lexer.Type.Token, 'value': '0'}]),
                            ('1', [{'type': Lexer.Type.Token, 'value': '1'}]),
                            ('10', [{'type': Lexer.Type.Token, 'value': '10'}]),
                            ('0.', [{'type': Lexer.Type.Token, 'value': '0.'}]),
                            ('0.3', [{'type': Lexer.Type.Token, 'value': '0.3'}]),
                            ('1.3', [{'type': Lexer.Type.Token, 'value': '1.3'}]),
                            ('0e10', [{'type': Lexer.Type.Token, 'value': '0e10'}]),
                            ('2e22', [{'type': Lexer.Type.Token, 'value': '2e22'}]),
                            ('0.e+3', [{'type': Lexer.Type.Token, 'value': '0.e+3'}]),
                            ('.37', [{'type': Lexer.Type.Token, 'value': '.37'}]),
                            ('12345', [{'type': Lexer.Type.Token, 'value': '12345'}]),
                            ('12345.678e-91', [{'type': Lexer.Type.Token, 'value': '12345.678e-91'}]),
                            ('12345e-91', [{'type': Lexer.Type.Token, 'value': '12345e-91'}]),
                            ('0b010', [{'type': Lexer.Type.Token, 'value': '0b010'}]),
                            ('0O765', [{'type': Lexer.Type.Token, 'value': '0O765'}]),
                            ('0xabc8', [{'type': Lexer.Type.Token, 'value': '0xabc8'}]),
                            ('69/90', [{'type': Lexer.Type.Token, 'value': '69'}, {'type': Lexer.Type.Token, 'value': '/'}, {'type': Lexer.Type.Token, 'value': '90'}]),
                            ('id', [{'type': Lexer.Type.Token, 'value': 'id'}]),
                            ('_test\\u{10330}it', [{'type': Lexer.Type.Token, 'value': '_test\\u{10330}it'}]),
                            ('\\ufd69thing', [{'type': Lexer.Type.Token, 'value': '\\ufd69thing'}]),
                            ('_\\u0068pop', [{'type': Lexer.Type.Token, 'value': '_\\u0068pop'}]),
                            ('\\u{10330}it', [{'type': Lexer.Type.Token, 'value': '\\u{10330}it'}]),
                            #('""', [{'type': Lexer.Type.Token, 'value': '""'}]),
                            ("''", [{'type': Lexer.Type.Token, 'value': "''"}]),
                            ("'\\b'", [{'type': Lexer.Type.Token, 'value': "'\\b'"}]),
                            ("'abcd'", [{'type': Lexer.Type.Token, 'value': "'abcd'"}]),
                            ("'ab\\\ncd'", [{'type': Lexer.Type.Token, 'value': "'ab\\\ncd'"}]),
                            ("'ab\\\rcd'", [{'type': Lexer.Type.Token, 'value': "'ab\\\rcd'"}]),
                            ("'ab\\\r\ncd'", [{'type': Lexer.Type.Token, 'value': "'ab\\\r\ncd'"}]),
                            ("'ab\\0cd'", [{'type': Lexer.Type.Token, 'value': "'ab\\0cd'"}]),
                            ("'ab\\x09cd'", [{'type': Lexer.Type.Token, 'value': "'ab\\x09cd'"}]),
                            ("'ab\\u09ffcd'", [{'type': Lexer.Type.Token, 'value': "'ab\\u09ffcd'"}]),
                            ("'ab\\u{10330}cd'", [{'type': Lexer.Type.Token, 'value': "'ab\\u{10330}cd'"}]),
                         ])
def test_lex(test_input, expected):
    l = Lexer(test_input)
    result = list(l.lex())
    assert [{'type': token.type, 'value': token.value()} for token in result] == expected

@pytest.mark.parametrize('test_input',
                         [ '/* unterminated', '0000', '0b1015', '0o3129', '0x', '0b', '0o', '3e', '0xab$', '0b0100r', '0o765_', '67.22e+21\u2118', '432A', '7.Z',
                           'id_\\tab', '\\tab', '_\\utab', '\\utab', '_\\u0m', '\\u0m',
                           '_\\u00m', '\\u00m', '_\\u000m', '\\u000m', '_\\u{718Q', '\\u{718Q',
                           '_\\u{738190789503417839105170}', '\\u{}p', '_\\u{}p', 'Embedded\\u0000Null',
                           "'\\", "'", "'ab\ncd'", "'a\\09b'", "'ab\\xAX'", "'ab\\uX0000zzz'",
                           "'ab\\u67XX'", "'ab\\u{}XX'", "'ab\\u{789401578910}'", "'ab\\u{###}'"])
def test_syntax_error(test_input):
    l = Lexer(test_input)
    with pytest.raises(LexerError):
        result = list(l.lex())

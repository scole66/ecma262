import pytest
from itertools import chain

from ecmascript import *

InputElementDiv = Lexer.Goal.InputElementDiv
InputElementRegExp = Lexer.Goal.InputElementRegExp
InputElementRegExpOrTemplateTail = Lexer.Goal.InputElementRegExpOrTemplateTail
InputElementTemplateTail = Lexer.Goal.InputElementTemplateTail


def test_lex_simple_sample():
    input_text = "goat = pig + beard - curly tail"
    l = Lexer(input_text)
    result = [{"type": token.type, "value": token.value.value} for token in l.lex()]

    expected = [
        {"type": "GOAL_SCRIPT", "value": None},
        {"type": "IDENTIFIER", "value": "goat"},
        {"type": "EQUALS", "value": "="},
        {"type": "IDENTIFIER", "value": "pig"},
        {"type": "PLUS", "value": "+"},
        {"type": "IDENTIFIER", "value": "beard"},
        {"type": "MINUS", "value": "-"},
        {"type": "IDENTIFIER", "value": "curly"},
        {"type": "IDENTIFIER", "value": "tail"},
    ]

    assert result == expected


@pytest.mark.parametrize(
    "test_input,expected",
    [
        ("", []),
        ("%", [{"type": "PERCENT", "value": "%"}]),
        ("!", [{"type": "BANG", "value": "!"}]),
        ("!=", [{"type": "BANGEQ", "value": "!="}]),
        ("!==", [{"type": "BANGEQEQ", "value": "!=="}]),
        ("%=", [{"type": "PERCENTEQ", "value": "%="}]),
        ("&", [{"type": "AMP", "value": "&"}]),
        ("&&", [{"type": "AMPAMP", "value": "&&"}]),
        ("&=", [{"type": "AMPEQ", "value": "&="}]),
        ("(", [{"type": "LPAREN_", "value": "("}]),
        ("(let", [{"type": "LPAREN_LET", "value": "("}, {"type": "LET", "value": "let"}]),
        ("([", [{"type": "LPAREN_LBRACKET", "value": "("}, {"type": "LBRACKET", "value": "["}]),
        (")", [{"type": "RPAREN", "value": ")"}]),
        ("*", [{"type": "STAR", "value": "*"}]),
        ("**", [{"type": "STARSTAR", "value": "**"}]),
        ("**=", [{"type": "STARSTAREQ", "value": "**="}]),
        ("*=", [{"type": "STAREQ", "value": "*="}]),
        ("+", [{"type": "PLUS", "value": "+"}]),
        ("++", [{"type": "PLUSPLUS", "value": "++"}]),
        ("+=", [{"type": "PLUSEQ", "value": "+="}]),
        (",", [{"type": "COMMA", "value": ","}]),
        ("-", [{"type": "MINUS", "value": "-"}]),
        ("--", [{"type": "MINUSMINUS", "value": "--"}]),
        ("-=", [{"type": "MINUSEQ", "value": "-="}]),
        (".", [{"type": "PERIOD", "value": "."}]),
        ("...", [{"type": "DOTDOTDOT", "value": "..."}]),
        (":", [{"type": "COLON", "value": ":"}]),
        (";", [{"type": "SEMICOLON", "value": ";"}]),
        ("<", [{"type": "LT", "value": "<"}]),
        ("<<", [{"type": "LTLT", "value": "<<"}]),
        ("<<=", [{"type": "LTLE", "value": "<<="}]),
        ("<=", [{"type": "LE", "value": "<="}]),
        ("=", [{"type": "EQUALS", "value": "="}]),
        ("==", [{"type": "EQEQ", "value": "=="}]),
        ("===", [{"type": "EQEQEQ", "value": "==="}]),
        ("=>", [{"type": "EQGT", "value": "=>"}]),
        (">", [{"type": "GT", "value": ">"}]),
        (">=", [{"type": "GE", "value": ">="}]),
        (">>", [{"type": "GTGT", "value": ">>"}]),
        (">>=", [{"type": "GTGE", "value": ">>="}]),
        (">>>", [{"type": "GTGTGT", "value": ">>>"}]),
        (">>>=", [{"type": "GTGTGE", "value": ">>>="}]),
        ("?", [{"type": "QUESTION", "value": "?"}]),
        ("[", [{"type": "LBRACKET", "value": "["}]),
        ("]", [{"type": "RBRACKET", "value": "]"}]),
        ("^", [{"type": "XOR", "value": "^"}]),
        ("^=", [{"type": "XOREQ", "value": "^="}]),
        ("{", [{"type": "LCURLY", "value": "{"}]),
        ("}", [{"type": "RCURLY", "value": "}"}]),
        ("|", [{"type": "PIPE", "value": "|"}]),
        ("|=", [{"type": "PIPEEQ", "value": "|="}]),
        ("||", [{"type": "PIPEPIPE", "value": "||"}]),
        ("/", [{"type": "DIV", "value": "/"}]),
        ("/=", [{"type": "DIVEQ", "value": "/="}]),
        ("~", [{"type": "TILDE", "value": "~"}]),
        ("..", [{"type": "PERIOD", "value": "."}, {"type": "PERIOD", "value": "."}]),
        (
            "**!==+&!-| ",
            [
                {"type": "STARSTAR", "value": "**"},
                {"type": "BANGEQEQ", "value": "!=="},
                {"type": "PLUS", "value": "+"},
                {"type": "AMP", "value": "&"},
                {"type": "BANG", "value": "!"},
                {"type": "MINUS", "value": "-"},
                {"type": "PIPE", "value": "|"},
            ],
        ),
        (" \u2007", []),
        ("\r\n\u2028\u2029", []),
        ("!// This is a comment", [{"type": "BANG", "value": "!"}]),
        (
            """%/* comment that
can even\r\nbe
multiple lines!
*/""",
            [{"type": "PERCENT", "value": "%"}],
        ),
        ("//comment\n", []),
        ("/* comment 1 *//* comment 2 */", []),
        ("0", [{"type": "NUMERIC", "value": 0}]),
        ("1", [{"type": "NUMERIC", "value": 1}]),
        ("10", [{"type": "NUMERIC", "value": 10}]),
        ("0.", [{"type": "NUMERIC", "value": 0}]),
        ("0.3", [{"type": "NUMERIC", "value": 0.3}]),
        ("1.3", [{"type": "NUMERIC", "value": 1.3}]),
        ("0e10", [{"type": "NUMERIC", "value": 0}]),
        ("2e22", [{"type": "NUMERIC", "value": 2e22}]),
        ("0.e+3", [{"type": "NUMERIC", "value": 0}]),
        (".37", [{"type": "NUMERIC", "value": 0.37}]),
        ("12345", [{"type": "NUMERIC", "value": 12345}]),
        ("12345.678e-91", [{"type": "NUMERIC", "value": 12345.678e-91}]),
        ("12345e-91", [{"type": "NUMERIC", "value": 12345e-91}]),
        ("0b010", [{"type": "NUMERIC", "value": 0b010}]),
        ("0O765", [{"type": "NUMERIC", "value": 0o765}]),
        ("0xabc8", [{"type": "NUMERIC", "value": 0xABC8}]),
        ("69/90", [{"type": "NUMERIC", "value": 69}, {"type": "DIV", "value": "/"}, {"type": "NUMERIC", "value": 90}]),
        ("id", [{"type": "IDENTIFIER", "value": "id"}]),
        ("_test\\u{10330}it", [{"type": "IDENTIFIER", "value": "_test" + chr(0x10330) + "it"}]),
        ("\\ufd69thing", [{"type": "IDENTIFIER", "value": "\ufd69thing"}]),
        ("_\\u0068pop", [{"type": "IDENTIFIER", "value": "_hpop"}]),
        ("\\u{10330}it", [{"type": "IDENTIFIER", "value": chr(0x10330) + "it"}]),
        ('""', [{"type": "STRING", "value": ""}]),
        ("''", [{"type": "STRING", "value": ""}]),
        ("'\\b'", [{"type": "STRING", "value": "\x08"}]),
        ("'abcd'", [{"type": "STRING", "value": "abcd"}]),
        ("'ab\\\ncd'", [{"type": "STRING", "value": "abcd"}]),
        ("'ab\\\rcd'", [{"type": "STRING", "value": "abcd"}]),
        ("'ab\\\r\ncd'", [{"type": "STRING", "value": "abcd"}]),
        ("'ab\\0cd'", [{"type": "STRING", "value": "ab\0cd"}]),
        ("'ab\\x09cd'", [{"type": "STRING", "value": "ab\x09cd"}]),
        ("'ab\\u09ffcd'", [{"type": "STRING", "value": "ab\u09ffcd"}]),
        ("'ab\\u{10330}cd'", [{"type": "STRING", "value": "ab" + chr(0x10330) + "cd"}]),
        ("'\\b\\f\\n\\r\\t\\v\\0\\'\\\"'", [{"type": "STRING", "value": "\b\f\n\r\t\v\0'\""}]),
        ("await", [{"type": "AWAIT", "value": "await"}]),
        ("break", [{"type": "BREAK", "value": "break"}]),
        ("case", [{"type": "CASE", "value": "case"}]),
        ("catch", [{"type": "CATCH", "value": "catch"}]),
        ("class", [{"type": "CLASS", "value": "class"}]),
        ("const", [{"type": "CONST", "value": "const"}]),
        ("continue", [{"type": "CONTINUE", "value": "continue"}]),
        ("debugger", [{"type": "DEBUGGER", "value": "debugger"}]),
        ("default", [{"type": "DEFAULT", "value": "default"}]),
        ("delete", [{"type": "DELETE", "value": "delete"}]),
        ("do", [{"type": "DO", "value": "do"}]),
        ("else", [{"type": "ELSE", "value": "else"}]),
        ("export", [{"type": "EXPORT", "value": "export"}]),
        ("extends", [{"type": "EXTENDS", "value": "extends"}]),
        ("finally", [{"type": "FINALLY", "value": "finally"}]),
        ("for", [{"type": "FOR", "value": "for"}]),
        ("function", [{"type": "FUNCTION", "value": "function"}]),
        ("if", [{"type": "IF", "value": "if"}]),
        ("import", [{"type": "IMPORT", "value": "import"}]),
        ("in", [{"type": "IN", "value": "in"}]),
        ("instanceof", [{"type": "INSTANCEOF", "value": "instanceof"}]),
        ("new", [{"type": "NEW", "value": "new"}]),
        ("return", [{"type": "RETURN", "value": "return"}]),
        ("super", [{"type": "SUPER", "value": "super"}]),
        ("switch", [{"type": "SWITCH", "value": "switch"}]),
        ("this", [{"type": "THIS", "value": "this"}]),
        ("throw", [{"type": "THROW", "value": "throw"}]),
        ("try", [{"type": "TRY", "value": "try"}]),
        ("typeof", [{"type": "TYPEOF", "value": "typeof"}]),
        ("var", [{"type": "VAR", "value": "var"}]),
        ("void", [{"type": "VOID", "value": "void"}]),
        ("while", [{"type": "WHILE", "value": "while"}]),
        ("with", [{"type": "WITH", "value": "with"}]),
        ("yield", [{"type": "YIELD", "value": "yield"}]),
        ("enum", [{"type": "ENUM", "value": "enum"}]),
        ("null", [{"type": "NULL", "value": "null"}]),
        ("true", [{"type": "TRUE", "value": "true"}]),
        ("false", [{"type": "FALSE", "value": "false"}]),
        (
            "true;\nfalse;\nnull;\nmath;\nNaN;\n",
            [
                {"type": "TRUE", "value": "true"},
                {"type": "SEMICOLON", "value": ";"},
                {"type": "FALSE", "value": "false"},
                {"type": "SEMICOLON", "value": ";"},
                {"type": "NULL", "value": "null"},
                {"type": "SEMICOLON", "value": ";"},
                {"type": "IDENTIFIER", "value": "math"},
                {"type": "SEMICOLON", "value": ";"},
                {"type": "IDENTIFIER", "value": "NaN"},
                {"type": "SEMICOLON", "value": ";"},
            ],
        ),
        ("`no_subs`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": ("no_subs", "no_subs")}]),
        ("``", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": ("", "")}]),
        ("`${", [{"type": "TEMPLATEHEAD", "value": ("", "")}]),
        ("`abcd\\${}..${", [{"type": "TEMPLATEHEAD", "value": ("abcd${}..", "abcd\\${}..")}]),
        ("`ab\\xty${", [{"type": "TEMPLATEHEAD", "value": (None, "ab\\xty")}]),
        (r"`ab\x6ty${", [{"type": "TEMPLATEHEAD", "value": (None, r"ab\x6ty")}]),
        (r"`ab\u{1234}cd${", [{"type": "TEMPLATEHEAD", "value": ("ab\u1234cd", r"ab\u{1234}cd")}]),
        (r"`ab\u{12349aabcdde3}cd${", [{"type": "TEMPLATEHEAD", "value": (None, r"ab\u{12349aabcdde3}cd")}]),
        (r"`ab\u000a\u000d`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": ("ab\n\r", r"ab\u000a\u000d")}]),
        (r"`ab\u45tonya`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": (None, r"ab\u45tonya")}]),
        (r"`ab\u1zball`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": (None, r"ab\u1zball")}]),
        (r"`ab\xball`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": ("ab\xball", r"ab\xball")}]),
        (r"`ab\09ty`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": (None, r"ab\09ty")}]),
        (r"`ab\0xy`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": ("ab\0xy", r"ab\0xy")}]),
        ("`ab\\\r\ncd`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": ("abcd", "ab\\\ncd")}]),
        ("`ab\\\rcd`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": ("abcd", "ab\\\ncd")}]),
        (
            "`ab\\\u2028cd\\\u2029ef\\\ngh`",
            [{"type": "NOSUBSTITUTIONTEMPLATE", "value": ("abcdefgh", "ab\\\u2028cd\\\u2029ef\\\ngh")}],
        ),
        (
            "`ab\ncd\u2028ef\u2029gh\rij\r\nkl`",
            [
                {
                    "type": "NOSUBSTITUTIONTEMPLATE",
                    "value": ("ab\ncd\u2028ef\u2029gh\nij\nkl", "ab\ncd\u2028ef\u2029gh\nij\nkl"),
                }
            ],
        ),
        ("`ab\\xpop\n`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": (None, "ab\\xpop\n")}]),
        ("`ab\\xpop\r`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": (None, "ab\\xpop\n")}]),
        ("`ab\\xpop\\0poop`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": (None, "ab\\xpop\\0poop")}]),
        ("`a\\xp\\n`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": (None, "a\\xp\\n")}]),
        ("`a\\xp\\x3b`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": (None, "a\\xp\\x3b")}]),
        ("`a\\xp\\u003b`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": (None, "a\\xp\\u003b")}]),
        ("`a\\xp\\u{3b}`", [{"type": "NOSUBSTITUTIONTEMPLATE", "value": (None, "a\\xp\\u{3b}")}]),
    ],
)
def test_lex(test_input, expected):
    l = Lexer(test_input)
    result = list(l.lex())[1:]
    assert [{"type": token.type, "value": token.value.value} for token in result] == expected


@pytest.mark.parametrize(
    "test_input",
    [
        "/* unterminated",
        "0000",
        "0b1015",
        "0o3129",
        "0x",
        "0b",
        "0o",
        "3e",
        "0xab$",
        "0b0100r",
        "0o765_",
        "67.22e+21\u2118",
        "432A",
        "7.Z",
        "id_\\tab",
        "\\tab",
        "_\\utab",
        "\\utab",
        "_\\u0m",
        "\\u0m",
        "id_\\u005cu0040",
        "_\\u00m",
        "\\u00m",
        "_\\u000m",
        "\\u000m",
        "_\\u{718Q",
        "\\u{718Q",
        "\\u{3a}bob",
        "\\u003abob",
        "a\\u{3a}bob",
        "a\\u003abob",
        "\\u{47381907950801}",
        "_\\u{738190789503417839105170}",
        "\\u{}p",
        "_\\u{}p",
        "Embedded\\u0000Null",
        "'\\",
        "'",
        "'ab\ncd'",
        "'a\\09b'",
        "'ab\\xAX'",
        "'ab\\uX0000zzz'",
        "'ab\\u67XX'",
        "'ab\\u{}XX'",
        "'ab\\u{789401578910}'",
        "'ab\\u{###}'",
        r"`ab\u45",
        "`ab\\",
        "\0",
    ],
)
def test_lex_syntax_error(test_input):
    l = Lexer(test_input)
    with pytest.raises(LexerError):
        result = list(l.lex())


@pytest.mark.parametrize(
    "test_input, test_goals, expected",
    [
        ("/abcd/", [InputElementRegExp], [{"type": "REGEXP", "value": ("abcd", "")}]),
        ("/ab[0-9]cd/g", [InputElementRegExp], [{"type": "REGEXP", "value": ("ab[0-9]cd", "g")}]),
        ("/ab\\ncd/g", [InputElementRegExp], [{"type": "REGEXP", "value": ("ab\\ncd", "g")}]),
        (r"/ab[\t\r\n]/g", [InputElementRegExp], [{"type": "REGEXP", "value": (r"ab[\t\r\n]", "g")}]),
        (r"/ab/\u0061\u0069", [InputElementRegExp], [{"type": "REGEXP", "value": ("ab", r"\u0061\u0069")}]),
        (r"/ab/\u{1234}", [InputElementRegExp], [{"type": "REGEXP", "value": ("ab", r"\u{1234}")}]),
        ("// This is a comment //", [InputElementRegExp], [None]),
        ("/* comment */ blue", [InputElementRegExp], [{"type": "IDENTIFIER", "value": "blue"}]),
        ("/\\t/", [InputElementRegExp], [{"type": "REGEXP", "value": ("\\t", "")}]),
        ("/[0-9]/", [InputElementRegExp], [{"type": "REGEXP", "value": ("[0-9]", "")}]),
        ("}${", [InputElementTemplateTail], [{"type": "TEMPLATEMIDDLE", "value": ("", "")}]),
        ("}`", [InputElementTemplateTail], [{"type": "TEMPLATETAIL", "value": ("", "")}]),
        ("}bob${", [InputElementTemplateTail], [{"type": "TEMPLATEMIDDLE", "value": ("bob", "bob")}]),
    ],
)
def test_lex_goalchange(test_input, test_goals, expected):
    l = Lexer(test_input)
    result = [l.lex_next_token(goal) for goal in chain((InputElementDiv,), test_goals)][1:]
    assert [{"type": token.type, "value": token.value.value} if token else token for token in result] == expected


@pytest.mark.parametrize(
    "test_input, test_goals",
    [
        ("/abcd\n", [InputElementRegExp]),
        ("/ab\\", [InputElementRegExp]),
        ("/ab[", [InputElementRegExp]),
        ("/ab[9\\", [InputElementRegExp]),
        (r"/ab/\d", [InputElementRegExp]),
        (r"/ab/\u{6", [InputElementRegExp]),
        (r"/ab/\up", [InputElementRegExp]),
        (r"/ab/\u1p", [InputElementRegExp]),
        (r"/ab/\u12p", [InputElementRegExp]),
        (r"/ab/\u123p", [InputElementRegExp]),
        (r"/ab/\u{123p", [InputElementRegExp]),
        (r"/ab/\u{}p", [InputElementRegExp]),
        ("/", [InputElementRegExp]),
        ("/ab/\\u003ag", [InputElementRegExp]),
        ("/ab/\\u{987654321}", [InputElementRegExp]),
        ("/ab/\\u{3a}", [InputElementRegExp]),
        ("}", [InputElementTemplateTail]),
    ],
)
def test_lex_goalchange_errs(test_input, test_goals):
    l = Lexer(test_input)
    with pytest.raises(LexerError):
        result = [l.lex_next_token(goal) for goal in chain((InputElementDiv,), test_goals)]


def test_lex_tokenvalue_repr():
    tv = Lexer.TokenValue()
    tv.value = 100
    tv.lt_follows = True
    tv.name = "TOKEN"

    val = repr(tv)
    assert "value=100" in val
    assert "lt_follows=True" in val
    assert "name=TOKEN" in val

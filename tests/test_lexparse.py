import pytest

import ecmascript.lexer2 as lexer2
import ecmascript.ecmascript as parse2

# Lexer/Parser integration tests : Make soure we get parse nodes from source text.
# Note that we don't check the whole parse tree. That's a PITA. We just check the top node.


def run_test(src, expected_type, parse_fcn, *parse_args):
    lex = lexer2.Lexer(src, SyntaxError)
    rv = parse_fcn(parse2.Parse2Context(src), lex, *parse_args)
    assert isinstance(rv, expected_type)


class Test_Identifier:
    @pytest.mark.parametrize(
        "src, expected_type",
        [("identifier", parse2.P2_Identifier)]
        + [
            (word, type(None))
            for word in (
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
        ],
    )
    def test_01(self, src, expected_type):
        run_test(src, expected_type, parse2.parse_Identifier)


class Test_IdentifierReference:
    @pytest.mark.parametrize(
        "src, parse_args, expected_type",
        [
            ("identifier", (Yield, Await), parse2.P2_IdentifierReference_Identifier)
            for Yield in (True, False)
            for Await in (True, False)
        ]
        + [("yield", (False, Await), parse2.P2_IdentifierReference_YIELD) for Await in (True, False)]
        + [("yield", (True, Await), type(None)) for Await in (True, False)]
        + [("await", (Yield, False), parse2.P2_IdentifierReference_AWAIT) for Yield in (True, False)]
        + [("await", (Yield, True), type(None)) for Yield in (True, False)],
    )
    def test_01(self, src, parse_args, expected_type):
        run_test(src, expected_type, parse2.parse_IdentifierReference, *parse_args)


class Test_BindingIdentifier:
    @pytest.mark.parametrize(
        "src, parse_args, expected_type",
        [
            (identifier, (Yield, Await), expected)
            for Yield in (False, True)
            for Await in (False, True)
            for identifier, expected in (
                ("id", parse2.P2_BindingIdentifier_Identifier),
                ("yield", parse2.P2_BindingIdentifier_YIELD),
                ("await", parse2.P2_BindingIdentifier_AWAIT),
            )
        ],
    )
    def test_01(self, src, parse_args, expected_type):
        run_test(src, expected_type, parse2.parse_BindingIdentifier, *parse_args)


class Test_BreakStatement:
    @pytest.mark.parametrize(
        "src, parse_args, expected_type",
        [
            (srcstream, (Yield, Await), parse2.P2_BreakStatement_BREAK)
            for Yield in (False, True)
            for Await in (False, True)
            for srcstream in ("break", "break;", "break\nmumble;")
        ]
        + [
            (srcstream, (Yield, Await), parse2.P2_BreakStatement_BREAK_LabelIdentifier)
            for Yield in (False, True)
            for Await in (False, True)
            for srcstream in ("break mumble;", "break mumble")
        ]
        + [
            (errstream, (Yield, Await), type(None))
            for Yield in (False, True)
            for Await in (False, True)
            for errstream in ("break =", "", "break mumble =")
        ],
    )
    def test_01(self, src, parse_args, expected_type):
        run_test(src, expected_type, parse2.parse_BreakStatement, *parse_args)


class Test_ReturnStatement:
    @pytest.mark.parametrize(
        "src, parse_args, expected_type",
        [
            (srcstream, (Yield, Await), parse2.P2_ReturnStatement_RETURN)
            for Yield in (False, True)
            for Await in (False, True)
            for srcstream in ("return;", "return", "return\nmumble;")
        ]
        + [
            (srcstream, (Yield, Await), parse2.P2_ReturnStatement_RETURN_Expression)
            for Yield in (False, True)
            for Await in (False, True)
            for srcstream in ("return mumble;", "return mumble")
        ]
        + [
            (errstream, (Yield, Await), type(None))
            for Yield in (False, True)
            for Await in (False, True)
            for errstream in ("return =", "", "return mumble =")
        ],
    )
    def test_01(self, src, parse_args, expected_type):
        run_test(src, expected_type, parse2.parse_ReturnStatement, *parse_args)


class Test_WithStatement:
    @pytest.mark.parametrize(
        "src, parse_args, expected_type",
        [
            (
                "with (bob=78+alice) { charlie=bob+99; }",
                (Yield, Await, Return),
                parse2.P2_WithStatement_WITH_Expression_Statement,
            )
            for Yield in (False, True)
            for Await in (False, True)
            for Return in (False, True)
        ]
        + [
            (errstream, (Yield, Await, Return), type(None))
            for Yield in (False, True)
            for Await in (False, True)
            for Return in (False, True)
            for errstream in ("", "with", "with poop", "with(*);", "with(u=3) =;")
        ],
    )
    def test_01(self, src, parse_args, expected_type):
        run_test(src, expected_type, parse2.parse_WithStatement, *parse_args)


def test_callexpression_in_lhs():
    run_test(
        "test()", parse2.P2_LeftHandSideExpression_CallExpression, parse2.parse_LeftHandSideExpression, False, False
    )

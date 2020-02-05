import pytest
from itertools import chain
import snoop

import ecmascript.ecmascript as parse2
import ecmascript.ecmascript as basics
from .helpers import *

import ecmascript.ecmascript
import ecmascript.lexer2 as lexer2

#### Literal ###############################################
#
# 888      d8b 888                              888
# 888      Y8P 888                              888
# 888          888                              888
# 888      888 888888  .d88b.  888d888  8888b.  888
# 888      888 888    d8P  Y8b 888P"       "88b 888
# 888      888 888    88888888 888     .d888888 888
# 888      888 Y88b.  Y8b.     888     888  888 888
# 88888888 888  "Y888  "Y8888  888     "Y888888 888
#
#
#
############################################################
def test_Literal_init(context):
    lit = ecmascript.ecmascript.P2_Literal(context, "StrictArg", ["child"])

    assert lit.name == "Literal"
    assert lit.context == context
    assert lit.children == ["child"]
    assert lit.strict == "StrictArg"


def test_Literal_NullLiteral_init(context):
    lit = ecmascript.ecmascript.P2_Literal_NullLiteral(context, "StrictArg", ["child"])

    assert lit.name == "Literal"
    assert lit.context == context
    assert lit.children == ["child"]
    assert lit.NullLiteral == "child"


def test_Literal_BooleanLiteral_init(context):
    lit = ecmascript.ecmascript.P2_Literal_BooleanLiteral(context, "StrictArg", ["child"])

    assert lit.name == "Literal"
    assert lit.context == context
    assert lit.children == ["child"]
    assert lit.BooleanLiteral == "child"


def test_Literal_NumericLiteral_init(context):
    lit = ecmascript.ecmascript.P2_Literal_NumericLiteral(context, "StrictArg", ["child"])

    assert lit.name == "Literal"
    assert lit.context == context
    assert lit.children == ["child"]
    assert lit.NumericLiteral == "child"


def test_Literal_StringLiteral_init(context):
    lit = ecmascript.ecmascript.P2_Literal_StringLiteral(context, "StrictArg", ["child"])

    assert lit.name == "Literal"
    assert lit.context == context
    assert lit.children == ["child"]
    assert lit.StringLiteral == "child"


class Test_parse_Literal(parse_test):
    # Syntax
    #   Literal:
    #       NullLiteral
    #       BooleanLiteral
    #       NumericLiteral
    #       StringLiteral
    target = staticmethod(ecmascript.ecmascript.parse_Literal)
    target_argnames = ()
    productions = (
        (("null",), ecmascript.ecmascript.P2_Literal_NullLiteral),
        (("true",), ecmascript.ecmascript.P2_Literal_BooleanLiteral),
        (("NUMERIC¡788",), ecmascript.ecmascript.P2_Literal_NumericLiteral),
        (("STRING¡bob",), ecmascript.ecmascript.P2_Literal_StringLiteral),
    )

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


def test_PrimaryExpression_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression(context, "StrictArg", ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]


def test_PrimaryExpression_THIS_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_THIS(context, "StrictArg", ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]


def test_PrimaryExpression_IdentifierReference_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_IdentifierReference(context, "StrictArg", ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.IdentifierReference == "child"


def test_PrimaryExpression_Literal_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_Literal(context, "StrictArg", ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.Literal == "child"


def test_PrimaryExpression_ArrayLiteral_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_ArrayLiteral(context, "StrictArg", ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.ArrayLiteral == "child"


def test_PrimaryExpression_ObjectLiteral_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_ObjectLiteral(context, "StrictArg", ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.ObjectLiteral == "child"


def test_PrimaryExpression_FunctionExpression_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_FunctionExpression(context, "StrictArg", ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.FunctionExpression == "child"


def test_PrimaryExpression_ClassExpression_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_ClassExpression(context, "StrictArg", ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.ClassExpression == "child"


def test_PrimaryExpression_GeneratorExpression_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_GeneratorExpression(context, "StrictArg", ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.GeneratorExpression == "child"


def test_PrimaryExpression_AsyncFunctionExpression_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_AsyncFunctionExpression(context, "StrictArg", ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.AsyncFunctionExpression == "child"


def test_PrimaryExpression_AsyncGeneratorExpression_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_AsyncGeneratorExpression(context, "StrictArg", ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.AsyncGeneratorExpression == "child"


def test_PrimaryExpression_RegularExpressionLiteral_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_RegularExpressionLiteral(context, "StrictArg", ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.RegularExpressionLiteral == "child"


def test_PrimaryExpression_TemplateLiteral_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_TemplateLiteral(context, "StrictArg", ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.TemplateLiteral == "child"


def test_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList(
        context, "StrictArg", ["child"]
    )

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.CoverParenthesizedExpressionAndArrowParameterList == "child"


class Test_parse_PrimaryExpression(parse_test):
    # Syntax
    #   PrimaryExpression[Yield, Await] :
    #       this
    #       IdentifierReference[?Yield, ?Await]
    #       Literal
    #       ArrayLiteral[?Yield, ?Await]
    #       ObjectLiteral[?Yield, ?Await]
    #       FunctionExpression
    #       ClassExpression[?Yield, ?Await]
    #       GeneratorExpression
    #       AsyncFunctionExpression
    #       AsyncGeneratorExpression
    #       RegularExpressionLiteral
    #       TemplateLiteral[?Yield, ?Await, ~Tagged]
    #       CoverParenthesizedExpressionAndArrowParameterList[?Yield, ?Await]
    target = staticmethod(ecmascript.ecmascript.parse_PrimaryExpression)
    target_argnames = ("Yield", "Await")
    productions = (
        (("this",), ecmascript.ecmascript.P2_PrimaryExpression_THIS),
        (("IdentifierReference",), ecmascript.ecmascript.P2_PrimaryExpression_IdentifierReference),
        (("Literal",), ecmascript.ecmascript.P2_PrimaryExpression_Literal),
        (("ArrayLiteral",), ecmascript.ecmascript.P2_PrimaryExpression_ArrayLiteral),
        (("ObjectLiteral",), ecmascript.ecmascript.P2_PrimaryExpression_ObjectLiteral),
        (("FunctionExpression",), ecmascript.ecmascript.P2_PrimaryExpression_FunctionExpression),
        (("ClassExpression",), ecmascript.ecmascript.P2_PrimaryExpression_ClassExpression),
        (("GeneratorExpression",), ecmascript.ecmascript.P2_PrimaryExpression_GeneratorExpression),
        (("AsyncFunctionExpression",), ecmascript.ecmascript.P2_PrimaryExpression_AsyncFunctionExpression),
        (("AsyncGeneratorExpression",), ecmascript.ecmascript.P2_PrimaryExpression_AsyncGeneratorExpression),
        (("REGEXP¡/bob/",), ecmascript.ecmascript.P2_PrimaryExpression_RegularExpressionLiteral),
        (("TemplateLiteral",), ecmascript.ecmascript.P2_PrimaryExpression_TemplateLiteral),
        (
            ("CoverParenthesizedExpressionAndArrowParameterList",),
            ecmascript.ecmascript.P2_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList,
        ),
    )
    called_argnames = {
        "IdentifierReference": ("?Yield", "?Await"),
        "ArrayLiteral": ("?Yield", "?Await"),
        "ObjectLiteral": ("?Yield", "?Await"),
        "ClassExpression": ("?Yield", "?Await"),
        "TemplateLiteral": ("?Yield", "?Await", "~Tagged"),
        "CoverParenthesizedExpressionAndArrowParameterList": ("?Yield", "?Await"),
        "Literal": (),
        "FunctionExpression": (),
        "GeneratorExpression": (),
        "AsyncFunctionExpression": (),
        "AsyncGeneratorExpression": (),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


class Test_PrimaryExpression_IsIdentifierRef:
    THIS = ecmascript.ecmascript.P2_PrimaryExpression_THIS
    Literal = ecmascript.ecmascript.P2_PrimaryExpression_Literal
    ArrayLiteral = ecmascript.ecmascript.P2_PrimaryExpression_ArrayLiteral
    ObjectLiteral = ecmascript.ecmascript.P2_PrimaryExpression_ObjectLiteral
    FunctionExpression = ecmascript.ecmascript.P2_PrimaryExpression_FunctionExpression
    ClassExpression = ecmascript.ecmascript.P2_PrimaryExpression_ClassExpression
    GeneratorExpression = ecmascript.ecmascript.P2_PrimaryExpression_GeneratorExpression
    AsyncFunctionExpression = ecmascript.ecmascript.P2_PrimaryExpression_AsyncFunctionExpression
    AsyncGeneratorExpression = ecmascript.ecmascript.P2_PrimaryExpression_AsyncGeneratorExpression
    RegularExpressionLiteral = ecmascript.ecmascript.P2_PrimaryExpression_RegularExpressionLiteral
    TemplateLiteral = ecmascript.ecmascript.P2_PrimaryExpression_TemplateLiteral
    CPEAAPL = ecmascript.ecmascript.P2_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList
    IdentifierReference = ecmascript.ecmascript.P2_PrimaryExpression_IdentifierReference

    # 12.2.1.4 Static Semantics: IsIdentifierRef
    # PrimaryExpression : IdentifierReference
    #   1. Return true.
    # PrimaryExpression : this
    # PrimaryExpression : Literal
    # PrimaryExpression : ArrayLiteral
    # PrimaryExpression : ObjectLiteral
    # PrimaryExpression : FunctionExpression
    # PrimaryExpression : ClassExpression
    # PrimaryExpression : GeneratorExpression
    # PrimaryExpression : AsyncFunctionExpression
    # PrimaryExpression : AsyncGeneratorExpression
    # PrimaryExpression : RegularExpressionLiteral
    # PrimaryExpression : TemplateLiteral
    # PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
    #   1. Return false.
    @pytest.mark.parametrize(
        "ctor, expected",
        (
            pytest.param(THIS, False, id="this"),
            pytest.param(Literal, False, id="Literal"),
            pytest.param(ArrayLiteral, False, id="ArrayLiteral"),
            pytest.param(ObjectLiteral, False, id="ObjectLiteral"),
            pytest.param(FunctionExpression, False, id="FunctionExpression"),
            pytest.param(ClassExpression, False, id="ClassExpression"),
            pytest.param(GeneratorExpression, False, id="GeneratorExpression"),
            pytest.param(AsyncFunctionExpression, False, id="AsyncFunctionExpression",),
            pytest.param(AsyncGeneratorExpression, False, id="AsyncGeneratorExpression",),
            pytest.param(RegularExpressionLiteral, False, id="RegularExpressionLiteral",),
            pytest.param(TemplateLiteral, False, id="TemplateLiteral"),
            pytest.param(CPEAAPL, False, id="CoverParenthesizedExpressionAndArrowParameterList",),
            pytest.param(IdentifierReference, True, id="IdentifierReference"),
        ),
    )
    @strict_params
    def test_normal(self, context, mocker, ctor, expected, strict):
        pe = ctor(context, strict, [mocker.Mock()])
        assert pe.IsIdentifierRef() is expected


class Test_PrimaryExpression_AssignmentTargetType:
    THIS = ecmascript.ecmascript.P2_PrimaryExpression_THIS
    Literal = ecmascript.ecmascript.P2_PrimaryExpression_Literal
    ArrayLiteral = ecmascript.ecmascript.P2_PrimaryExpression_ArrayLiteral
    ObjectLiteral = ecmascript.ecmascript.P2_PrimaryExpression_ObjectLiteral
    FunctionExpression = ecmascript.ecmascript.P2_PrimaryExpression_FunctionExpression
    ClassExpression = ecmascript.ecmascript.P2_PrimaryExpression_ClassExpression
    GeneratorExpression = ecmascript.ecmascript.P2_PrimaryExpression_GeneratorExpression
    AsyncFunctionExpression = ecmascript.ecmascript.P2_PrimaryExpression_AsyncFunctionExpression
    AsyncGeneratorExpression = ecmascript.ecmascript.P2_PrimaryExpression_AsyncGeneratorExpression
    RegularExpressionLiteral = ecmascript.ecmascript.P2_PrimaryExpression_RegularExpressionLiteral
    TemplateLiteral = ecmascript.ecmascript.P2_PrimaryExpression_TemplateLiteral
    CPEAAPL = ecmascript.ecmascript.P2_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList
    IdentifierReference = ecmascript.ecmascript.P2_PrimaryExpression_IdentifierReference

    # 12.2.1.5 Static Semantics: AssignmentTargetType
    # PrimaryExpression : this
    # PrimaryExpression : Literal
    # PrimaryExpression : ArrayLiteral
    # PrimaryExpression : ObjectLiteral
    # PrimaryExpression : FunctionExpression
    # PrimaryExpression : ClassExpression
    # PrimaryExpression : GeneratorExpression
    # PrimaryExpression : AsyncFunctionExpression
    # PrimaryExpression : AsyncGeneratorExpression
    # PrimaryExpression : RegularExpressionLiteral
    # PrimaryExpression : TemplateLiteral
    #   1. Return invalid.
    @pytest.mark.parametrize(
        "ctor",
        (
            pytest.param(THIS, id="this"),
            pytest.param(Literal, id="Literal"),
            pytest.param(ArrayLiteral, id="ArrayLiteral"),
            pytest.param(ObjectLiteral, id="ObjectLiteral"),
            pytest.param(FunctionExpression, id="FunctionExpression"),
            pytest.param(ClassExpression, id="ClassExpression"),
            pytest.param(GeneratorExpression, id="GeneratorExpression"),
            pytest.param(AsyncFunctionExpression, id="AsyncFunctionExpression",),
            pytest.param(AsyncGeneratorExpression, id="AsyncGeneratorExpression",),
            pytest.param(RegularExpressionLiteral, id="RegularExpressionLiteral",),
            pytest.param(TemplateLiteral, id="TemplateLiteral"),
        ),
    )
    @strict_params
    def test_normal(self, context, mocker, strict, ctor):
        pe = ctor(context, strict, [mocker.Mock()])
        assert pe.AssignmentTargetType == ecmascript.ecmascript.INVALID

    # 12.2.1.5 Static Semantics: AssignmentTargetType
    # PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
    #   1. Let expr be CoveredParenthesizedExpression of CoverParenthesizedExpressionAndArrowParameterList.
    #   2. Return AssignmentTargetType of expr.
    @strict_params
    def test_CPEAAPL(self, context, mocker, strict):
        expr = mocker.Mock(**{"AssignmentTargetType.return_value": mocker.sentinel.assignment_target_type})
        cpeaapl = mocker.Mock(**{"CoveredParenthesizedExpression": expr})
        pe = ecmascript.ecmascript.P2_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList(
            context, strict, [cpeaapl]
        )

        rv = pe.AssignmentTargetType()

        assert rv == mocker.sentinel.assignment_target_type


# 12.2.2.1 Runtime Semantics: Evaluation
# PrimaryExpression : this
#   1. Return ? ResolveThisBinding().
@strict_params
def test_PrimaryExpression_Evaluation_THIS(context, mocker, strict):
    mocker.patch("ecmascript.ecmascript.ResolveThisBinding", return_value=mocker.sentinel.resolve_this_binding)
    pe = ecmascript.ecmascript.P2_PrimaryExpression_THIS(context, strict, [mocker.Mock()])

    rv = pe.evaluate()
    assert rv == mocker.sentinel.resolve_this_binding


# 12.2.4.1 Runtime Semantics: Evaluation
# Literal : NullLiteral
#   1. Return null.
# Literal : BooleanLiteral
#   1. If BooleanLiteral is the token false, return false.
#   2. If BooleanLiteral is the token true, return true.
# Literal : NumericLiteral
#   1. Return the number whose value is MV of NumericLiteral as defined in 11.8.3.
# Literal : StringLiteral
#   1. Return the StringValue of StringLiteral as defined in 11.8.4.1.
@pytest.mark.parametrize(
    "ctor, value, expected",
    (
        (ecmascript.ecmascript.P2_Literal_NullLiteral, None, ecmascript.ecmascript.JSNull.NULL),
        (ecmascript.ecmascript.P2_Literal_BooleanLiteral, "true", True),
        (ecmascript.ecmascript.P2_Literal_BooleanLiteral, "false", False),
        (ecmascript.ecmascript.P2_Literal_NumericLiteral, 67.25, 67.25),
        (ecmascript.ecmascript.P2_Literal_StringLiteral, "mystring", "mystring"),
    ),
)
@strict_params
def test_Literal_evaluation(context, mocker, strict, ctor, value, expected):
    token = mocker.Mock(**{"value": value})
    lit = ctor(context, strict, [token])
    rv = lit.evaluate()
    assert rv == expected


#### CoverParenthesizedExpressionAndArrowParameterList ##################################################################################################################################################################################################################################################################################################################################################################################################
#
#  .d8888b.                                     8888888b.                                     888    888                        d8b                        888 8888888888                                                      d8b                          d8888               888        d8888                                        8888888b.                                                   888                     888      d8b          888
# d88P  Y88b                                    888   Y88b                                    888    888                        Y8P                        888 888                                                             Y8P                         d88888               888       d88888                                        888   Y88b                                                  888                     888      Y8P          888
# 888    888                                    888    888                                    888    888                                                   888 888                                                                                        d88P888               888      d88P888                                        888    888                                                  888                     888                   888
# 888         .d88b.  888  888  .d88b.  888d888 888   d88P  8888b.  888d888  .d88b.  88888b.  888888 88888b.   .d88b.  .d8888b  888 88888888  .d88b.   .d88888 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.      d88P 888 88888b.   .d88888     d88P 888 888d888 888d888  .d88b.  888  888  888 888   d88P  8888b.  888d888  8888b.  88888b.d88b.   .d88b.  888888  .d88b.  888d888 888      888 .d8888b  888888
# 888        d88""88b 888  888 d8P  Y8b 888P"   8888888P"      "88b 888P"   d8P  Y8b 888 "88b 888    888 "88b d8P  Y8b 88K      888    d88P  d8P  Y8b d88" 888 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b    d88P  888 888 "88b d88" 888    d88P  888 888P"   888P"   d88""88b 888  888  888 8888888P"      "88b 888P"       "88b 888 "888 "88b d8P  Y8b 888    d8P  Y8b 888P"   888      888 88K      888
# 888    888 888  888 Y88  88P 88888888 888     888        .d888888 888     88888888 888  888 888    888  888 88888888 "Y8888b. 888   d88P   88888888 888  888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888   d88P   888 888  888 888  888   d88P   888 888     888     888  888 888  888  888 888        .d888888 888     .d888888 888  888  888 88888888 888    88888888 888     888      888 "Y8888b. 888
# Y88b  d88P Y88..88P  Y8bd8P  Y8b.     888     888        888  888 888     Y8b.     888  888 Y88b.  888  888 Y8b.          X88 888  d88P    Y8b.     Y88b 888 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888  d8888888888 888  888 Y88b 888  d8888888888 888     888     Y88..88P Y88b 888 d88P 888        888  888 888     888  888 888  888  888 Y8b.     Y88b.  Y8b.     888     888      888      X88 Y88b.
#  "Y8888P"   "Y88P"    Y88P    "Y8888  888     888        "Y888888 888      "Y8888  888  888  "Y888 888  888  "Y8888   88888P' 888 88888888  "Y8888   "Y88888 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888 d88P     888 888  888  "Y88888 d88P     888 888     888      "Y88P"   "Y8888888P"  888        "Y888888 888     "Y888888 888  888  888  "Y8888   "Y888  "Y8888  888     88888888 888  88888P'  "Y888
#                                                                                                                                                                                  888
#                                                                                                                                                                                  888
#                                                                                                                                                                                  888
#
#########################################################################################################################################################################################################################################################################################################################################################################################################################################################
def test_P2_CoverParenthesizedExpressionAndArrowParameterList_init(context):
    cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList(
        context, "StrictArg", ["child"], "YieldArg", "AwaitArg"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"
    assert cpeaapl.context == context
    assert cpeaapl.children == ["child"]
    assert cpeaapl.Yield == "YieldArg"
    assert cpeaapl.Await == "AwaitArg"


def test_P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_RPAREN_init(context):
    cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_RPAREN(
        context, "StrictArg", ["(", "Expression", ")"], "Y", "A"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"
    assert cpeaapl.Expression == "Expression"


def test_P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_RPAREN_init(context):
    cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_RPAREN(
        context, "StrictArg", ["(", "Expression", ")", ","], "Y", "A"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"
    assert cpeaapl.Expression == "Expression"


def test_P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_RPAREN_init(context):
    cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_RPAREN(
        context, "StrictArg", ["(", ")"], "Y", "A"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"


def test_P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_DOTDOTDOT_BindingIdentifier_RPAREN_init(
    context,
):
    cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_DOTDOTDOT_BindingIdentifier_RPAREN(
        context, "StrictArg", ["(", "...", "BindingIdentifier", ")"], "Y", "A"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"
    assert cpeaapl.BindingIdentifier == "BindingIdentifier"


def test_P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_DOTDOTDOT_BindingPattern_RPAREN_init(context):
    cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_DOTDOTDOT_BindingPattern_RPAREN(
        context, "StrictArg", ["(", "...", "BindingPattern", ")"], "Y", "A"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"
    assert cpeaapl.BindingPattern == "BindingPattern"


def test_P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_DOTDOTDOT_BindingIdentifier_RPAREN_init(
    context,
):
    cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_DOTDOTDOT_BindingIdentifier_RPAREN(
        context, "StrictArg", ["(", "Expression", ",", "...", "BindingIdentifier", ")"], "Y", "A"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"
    assert cpeaapl.Expression == "Expression"
    assert cpeaapl.BindingIdentifier == "BindingIdentifier"


def test_P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_DOTDOTDOT_BindingPattern_RPAREN_init(
    context,
):
    cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_DOTDOTDOT_BindingPattern_RPAREN(
        context, "StrictArg", ["(", "Expression", ",", "...", "BindingPattern", ")"], "Y", "A"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"
    assert cpeaapl.Expression == "Expression"
    assert cpeaapl.BindingPattern == "BindingPattern"


class Test_parse_CoverParenthesizedExpressionAndArrowParameterList(parse_test):
    # Syntax
    #   CoverParenthesizedExpressionAndArrowParameterList[Yield, Await] :
    #       ( Expression[+In, ?Yield, ?Await] )
    #       ( Expression[+In, ?Yield, ?Await] , )
    #       ( )
    #       ( ... BindingIdentifier[?Yield, ?Await] )
    #       ( ... BindingPattern[?Yield, ?Await] )
    #       ( Expression[+In, ?Yield, ?Await] , ... BindingIdentifier[?Yield, ?Await] )
    #       ( Expression[+In, ?Yield, ?Await] , ... BindingPattern[?Yield, ?Await] )
    target = staticmethod(ecmascript.ecmascript.parse_CoverParenthesizedExpressionAndArrowParameterList)
    target_argnames = ("Yield", "Await")
    LP_EXP_RP = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_RPAREN
    LP_EXP_COM_RP = (
        ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_RPAREN
    )
    LP_RP = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_RPAREN
    LP_DOTS_BI_RP = (
        ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_DOTDOTDOT_BindingIdentifier_RPAREN
    )
    LP_DOTS_BP_RP = (
        ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_DOTDOTDOT_BindingPattern_RPAREN
    )
    LP_EXP_BI_RP = (
        ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_DOTDOTDOT_BindingIdentifier_RPAREN
    )
    LP_EXP_BP_RP = (
        ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_DOTDOTDOT_BindingPattern_RPAREN
    )
    productions = (
        (("(", "Expression", ")"), LP_EXP_RP),
        (("(", "Expression", ",", ")"), LP_EXP_COM_RP),
        (("(", ")"), LP_RP),
        (("(", "...", "BindingIdentifier", ")"), LP_DOTS_BI_RP),
        (("(", "...", "BindingPattern", ")"), LP_DOTS_BP_RP),
        (("(", "Expression", ",", "...", "BindingIdentifier", ")"), LP_EXP_BI_RP),
        (("(", "Expression", ",", "...", "BindingPattern", ")"), LP_EXP_BP_RP),
    )
    called_argnames = {
        "Expression": ("+In", "?Yield", "?Await"),
        "BindingIdentifier": ("?Yield", "?Await"),
        "BindingPattern": ("?Yield", "?Await"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


#### ParenthesizedExpression ##############################################################################################################################################################################
#
#     8888888b.                                     888    888                        d8b                        888 8888888888                                                      d8b
#     888   Y88b                                    888    888                        Y8P                        888 888                                                             Y8P
#     888    888                                    888    888                                                   888 888
#     888   d88P  8888b.  888d888  .d88b.  88888b.  888888 88888b.   .d88b.  .d8888b  888 88888888  .d88b.   .d88888 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
#     8888888P"      "88b 888P"   d8P  Y8b 888 "88b 888    888 "88b d8P  Y8b 88K      888    d88P  d8P  Y8b d88" 888 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
#     888        .d888888 888     88888888 888  888 888    888  888 88888888 "Y8888b. 888   d88P   88888888 888  888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
#     888        888  888 888     Y8b.     888  888 Y88b.  888  888 Y8b.          X88 888  d88P    Y8b.     Y88b 888 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
#     888        "Y888888 888      "Y8888  888  888  "Y888 888  888  "Y8888   88888P' 888 88888888  "Y8888   "Y88888 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                                                                                                        888
#                                                                                                                                        888
#                                                                                                                                        888
#
###########################################################################################################################################################################################################
def test_P2_ParenthesizedExpression_init(context):
    pe = ecmascript.ecmascript.P2_ParenthesizedExpression(context, "StrictArg", ["child"])
    assert pe.name == "ParenthesizedExpression"
    assert pe.context == context
    assert pe.children == ["child"]


def test_P2_ParenthesizedExpression_LPAREN_Expression_RPAREN_init(context):
    pe = ecmascript.ecmascript.P2_ParenthesizedExpression_LPAREN_Expression_RPAREN(
        context, "StrictArg", ["(", "Expression", ")"]
    )
    assert pe.name == "ParenthesizedExpression"
    assert pe.Expression == "Expression"


class Test_parse_ParenthesizedExpression(parse_test):
    # Syntax
    #   ParenthesizedExpression[Yield, Await] :
    #       ( Expression[+In, ?Yield, ?Await] )
    target = staticmethod(ecmascript.ecmascript.parse_ParenthesizedExpression)
    target_argnames = ("Yield", "Await")
    productions = (
        (("(", "Expression", ")"), ecmascript.ecmascript.P2_ParenthesizedExpression_LPAREN_Expression_RPAREN),
    )
    called_argnames = {
        "Expression": ("+In", "?Yield", "?Await"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


#### Elision ########################################################
#
#     8888888888 888 d8b          d8b
#     888        888 Y8P          Y8P
#     888        888
#     8888888    888 888 .d8888b  888  .d88b.  88888b.
#     888        888 888 88K      888 d88""88b 888 "88b
#     888        888 888 "Y8888b. 888 888  888 888  888
#     888        888 888      X88 888 Y88..88P 888  888
#     8888888888 888 888  88888P' 888  "Y88P"  888  888
#
#
#
#####################################################################
def test_Elision_init(context):
    elision = ecmascript.ecmascript.P2_Elision(context, "StrictArg", ["child"])
    assert elision.name == "Elision"
    assert elision.context == context
    assert elision.children == ["child"]


def test_Elision_COMMA_init(context):
    elision = ecmascript.ecmascript.P2_Elision_COMMA(context, "StrictArg", [","])
    assert elision.name == "Elision"
    assert elision.children == [","]


def test_Elision_Elision_COMMA_init(context):
    elision = ecmascript.ecmascript.P2_Elision_Elision_COMMA(context, "StrictArg", ["child_a", "child_b"])
    assert elision.name == "Elision"
    assert elision.children == ["child_a", "child_b"]
    assert elision.Elision == "child_a"


class Test_parse_Elision(parse_test):
    # Syntax
    #   Elision :
    #       ,
    #       Elision ,
    target = staticmethod(ecmascript.ecmascript.parse_Elision)
    target_argnames = ()
    productions = (
        ((",",), ecmascript.ecmascript.P2_Elision_COMMA),
        ((",", ","), ecmascript.ecmascript.P2_Elision_Elision_COMMA),
    )
    called_argnames = {}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


class Test_ArrayInitializer_ElisionWidth:
    # 12.2.5.1 Static Semantics: ElisionWidth
    # Elision : ,
    #   1. Return the numeric value 1.
    # Elision : Elision ,
    #   1. Let preceding be the ElisionWidth of Elision.
    #   2. Return preceding + 1.
    @strict_params
    def test_one_comma(self, context, mocker, strict):
        elision = ecmascript.ecmascript.P2_Elision_COMMA(context, strict, [mocker.Mock()])
        assert elision.ElisionWidth() == 1

    @strict_params
    def test_many_commas(self, context, mocker, strict):
        previous = mocker.Mock(**{"ElisionWidth.return_value": 50})
        elision = ecmascript.ecmascript.P2_Elision_Elision_COMMA(context, strict, [previous, mocker.Mock()])
        assert elision.ElisionWidth() == 51


#### ArrayLiteral #################################################################################
#
#        d8888                                   888      d8b 888                              888
#       d88888                                   888      Y8P 888                              888
#      d88P888                                   888          888                              888
#     d88P 888 888d888 888d888  8888b.  888  888 888      888 888888  .d88b.  888d888  8888b.  888
#    d88P  888 888P"   888P"       "88b 888  888 888      888 888    d8P  Y8b 888P"       "88b 888
#   d88P   888 888     888     .d888888 888  888 888      888 888    88888888 888     .d888888 888
#  d8888888888 888     888     888  888 Y88b 888 888      888 Y88b.  Y8b.     888     888  888 888
# d88P     888 888     888     "Y888888  "Y88888 88888888 888  "Y888  "Y8888  888     "Y888888 888
#                                            888
#                                       Y8b d88P
#                                        "Y88P"
#
###################################################################################################
def test_ArrayLiteral_init(context):
    al = ecmascript.ecmascript.P2_ArrayLiteral(context, "StrictArg", ["child"])
    assert al.name == "ArrayLiteral"
    assert al.context == context
    assert al.children == ["child"]


def test_ArrayLiteral_LBRACKET_RBRACKET_init(context):
    al = ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_RBRACKET(context, "StrictArg", ["[", "]"])
    assert al.name == "ArrayLiteral"
    assert al.children == ["[", "]"]


def test_ArrayLiteral_LBRACKET_Elision_RBRACKET_init(context):
    al = ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_Elision_RBRACKET(
        context, "StrictArg", ["lbracket", "elision", "rbracket"]
    )
    assert al.name == "ArrayLiteral"
    assert al.Elision == "elision"


def test_ArrayLiteral_LBRACKET_ElementList_RBRACKET_init(context):
    al = ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_ElementList_RBRACKET(
        context, "StrictArg", ["lbracket", "elementlist", "rbracket"]
    )
    assert al.name == "ArrayLiteral"
    assert al.ElementList == "elementlist"


def test_ArrayLiteral_LBRACKET_ElementList_COMMA_RBRACKET_init(context):
    al = ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_ElementList_COMMA_RBRACKET(
        context, "StrictArg", ["lbra", "elementlist", "comma", "rbra"]
    )
    assert al.name == "ArrayLiteral"
    assert al.ElementList == "elementlist"


def test_ArrayLiteral_elementlist_elision_init(context):
    al = ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_ElementList_COMMA_Elision_RBRACKET(
        context, "StrictArg", ["lbra", "elementlist", "comma", "elision", "rbra"]
    )
    assert al.name == "ArrayLiteral"
    assert al.ElementList == "elementlist"
    assert al.Elision == "elision"


class Test_parse_ArrayLiteral(parse_test):
    # Syntax
    #   ArrayLiteral[Yield, Await] :
    #       [ ]
    #       [ Elision ]
    #       [ ElementList[?Yield, ?Await] ]
    #       [ ElementList[?Yield, ?Await] , ]
    #       [ ElementList[?Yield, ?Await] , Elision ]
    target = staticmethod(ecmascript.ecmascript.parse_ArrayLiteral)
    target_argnames = ("Yield", "Await")
    productions = (
        (("[", "]"), ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_RBRACKET),
        (("[", "Elision", "]"), ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_Elision_RBRACKET),
        (("[", "ElementList", "]"), ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_ElementList_RBRACKET),
        (("[", "ElementList", ",", "]"), ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_ElementList_COMMA_RBRACKET),
        (
            ("[", "ElementList", ",", "Elision", "]"),
            ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_ElementList_COMMA_Elision_RBRACKET,
        ),
    )
    called_argnames = {
        "Elision": (),
        "ElementList": ("?Yield", "?Await"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


class Test_ArrayInitializer_Evaluation:
    # 12.2.5.3 Runtime Semantics: Evaluation

    # ArrayLiteral : [ Elision ]
    #   1. Let array be ! ArrayCreate(0).
    #   2. Let pad be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
    #   3. Perform Set(array, "length", ToUint32(pad), false).
    #   4. NOTE: The above Set cannot fail because of the nature of the object returned by ArrayCreate.
    #   5. Return array.
    @strict_params
    def test_ArrayLiteral_Empty(self, context, mocker, strict):
        al = ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_RBRACKET(context, strict, [mocker.Mock(), mocker.Mock()])
        ac = mocker.patch("ecmascript.ecmascript.ArrayCreate", return_value=mocker.sentinel.array)
        s = mocker.patch("ecmascript.ecmascript.Set", return_value=None)
        rv = al.evaluate()
        assert rv == mocker.sentinel.array
        ac.assert_called_with(0)
        s.assert_called_with(mocker.sentinel.array, "length", 0, False)

    @strict_params
    def test_ArrayLiteral_Elision(self, context, mocker, strict):
        elision = mocker.Mock(**{"ElisionWidth.return_value": 10})
        al = ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_Elision_RBRACKET(
            context, strict, [mocker.Mock(), elision, mocker.Mock()]
        )
        ac = mocker.patch("ecmascript.ecmascript.ArrayCreate", return_value=mocker.sentinel.array)
        s = mocker.patch("ecmascript.ecmascript.Set", return_value=None)
        rv = al.evaluate()
        assert rv == mocker.sentinel.array
        ac.assert_called_with(0)
        s.assert_called_with(mocker.sentinel.array, "length", 10, False)
        elision.ElisionWidth.assert_called_with()

    # ArrayLiteral : [ ElementList ]
    #   1. Let array be ! ArrayCreate(0).
    #   2. Let len be the result of performing ArrayAccumulation for ElementList with arguments array and 0.
    #   3. ReturnIfAbrupt(len).
    #   4. Perform Set(array, "length", ToUint32(len), false).
    #   5. NOTE: The above Set cannot fail because of the nature of the object returned by ArrayCreate.
    #   6. Return array.
    @strict_params
    def test_ArrayLiteral_ElementList(self, context, mocker, strict):
        el = mocker.Mock(**{"ArrayAccumulation.return_value": 100})
        al = ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_ElementList_RBRACKET(
            context, strict, [mocker.Mock(), el, mocker.Mock()]
        )
        ac = mocker.patch("ecmascript.ecmascript.ArrayCreate", return_value=mocker.sentinel.array)
        s = mocker.patch("ecmascript.ecmascript.Set", return_value=None)
        rv = al.evaluate()
        assert rv == mocker.sentinel.array
        ac.assert_called_with(0)
        s.assert_called_with(mocker.sentinel.array, "length", 100, False)
        el.ArrayAccumulation.assert_called_with(mocker.sentinel.array, 0)

    # ArrayLiteral : [ ElementList , Elision ]
    #   1. Let array be ! ArrayCreate(0).
    #   2. Let len be the result of performing ArrayAccumulation for ElementList with arguments array and 0.
    #   3. ReturnIfAbrupt(len).
    #   4. Let padding be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
    #   5. Perform Set(array, "length", ToUint32(padding + len), false).
    #   6. NOTE: The above Set cannot fail because of the nature of the object returned by ArrayCreate.
    #   7. Return array.
    @strict_params
    def test_ArrayLiteral_ElementList_Elision(self, context, mocker, strict):
        elision = mocker.Mock(**{"ElisionWidth.return_value": 10})
        el = mocker.Mock(**{"ArrayAccumulation.return_value": 100})
        al = ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_ElementList_COMMA_Elision_RBRACKET(
            context, strict, [mocker.Mock(), el, mocker.Mock(), elision, mocker.Mock()]
        )
        ac = mocker.patch("ecmascript.ecmascript.ArrayCreate", return_value=mocker.sentinel.array)
        s = mocker.patch("ecmascript.ecmascript.Set", return_value=None)
        rv = al.evaluate()
        assert rv == mocker.sentinel.array
        ac.assert_called_with(0)
        s.assert_called_with(mocker.sentinel.array, "length", 110, False)
        el.ArrayAccumulation.assert_called_with(mocker.sentinel.array, 0)
        elision.ElisionWidth.assert_called_with()

    @strict_params
    def test_ArrayLiteral_ElementList_COMMA(self, context, mocker, strict):
        el = mocker.Mock(**{"ArrayAccumulation.return_value": 100})
        al = ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_ElementList_COMMA_RBRACKET(
            context, strict, [mocker.Mock(), el, mocker.Mock(), mocker.Mock()]
        )
        ac = mocker.patch("ecmascript.ecmascript.ArrayCreate", return_value=mocker.sentinel.array)
        s = mocker.patch("ecmascript.ecmascript.Set", return_value=None)
        rv = al.evaluate()
        assert rv == mocker.sentinel.array
        ac.assert_called_with(0)
        s.assert_called_with(mocker.sentinel.array, "length", 100, False)
        el.ArrayAccumulation.assert_called_with(mocker.sentinel.array, 0)


#### ElementList #################################################################################
#
#     8888888888 888                                          888    888      d8b          888
#     888        888                                          888    888      Y8P          888
#     888        888                                          888    888                   888
#     8888888    888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888 888      888 .d8888b  888888
#     888        888 d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888    888      888 88K      888
#     888        888 88888888 888  888  888 88888888 888  888 888    888      888 "Y8888b. 888
#     888        888 Y8b.     888  888  888 Y8b.     888  888 Y88b.  888      888      X88 Y88b.
#     8888888888 888  "Y8888  888  888  888  "Y8888  888  888  "Y888 88888888 888  88888P'  "Y888
#
#
#
#
##################################################################################################
def test_P2_ElementList_init(context):
    el = ecmascript.ecmascript.P2_ElementList(context, "StrictArg", ["child"])
    assert el.name == "ElementList"
    assert el.context == context
    assert el.children == ["child"]


def test_P2_ElementList_AssignmentExpression_init(context):
    el = ecmascript.ecmascript.P2_ElementList_AssignmentExpression(context, "StrictArg", ["AssignmentExpression"])
    assert el.name == "ElementList"
    assert el.AssignmentExpression == "AssignmentExpression"


def test_P2_ElementList_Elision_AssignmentExpression_init(context):
    el = ecmascript.ecmascript.P2_ElementList_Elision_AssignmentExpression(
        context, "StrictArg", ["Elision", "AssignmentExpression"]
    )
    assert el.name == "ElementList"
    assert el.Elision == "Elision"
    assert el.AssignmentExpression == "AssignmentExpression"


def test_P2_ElementList_SpreadElement_init(context):
    el = ecmascript.ecmascript.P2_ElementList_SpreadElement(context, "StrictArg", ["SpreadElement"])
    assert el.name == "ElementList"
    assert el.SpreadElement == "SpreadElement"


def test_P2_ElementList_Elision_SpreadElement_init(context):
    el = ecmascript.ecmascript.P2_ElementList_Elision_SpreadElement(
        context, "StrictArg", ["Elision", "SpreadElement"]
    )
    assert el.name == "ElementList"
    assert el.Elision == "Elision"
    assert el.SpreadElement == "SpreadElement"


def test_P2_ElementList_ElementList_COMMA_AssignmentExpression_init(context):
    el = ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_AssignmentExpression(
        context, "StrictArg", ["ElementList", ",", "AssignmentExpression"]
    )
    assert el.name == "ElementList"
    assert el.ElementList == "ElementList"
    assert el.AssignmentExpression == "AssignmentExpression"


def test_P2_ElementList_ElementList_COMMA_Elision_AssignmentExpression_init(context):
    el = ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_Elision_AssignmentExpression(
        context, "StrictArg", ["ElementList", ",", "Elision", "AssignmentExpression"]
    )
    assert el.name == "ElementList"
    assert el.ElementList == "ElementList"
    assert el.Elision == "Elision"
    assert el.AssignmentExpression == "AssignmentExpression"


def test_P2_ElementList_ElementList_COMMA_SpreadElement_init(context):
    el = ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_SpreadElement(
        context, "StrictArg", ["ElementList", ",", "SpreadElement"]
    )
    assert el.name == "ElementList"
    assert el.ElementList == "ElementList"
    assert el.SpreadElement == "SpreadElement"


def test_P2_ElementList_ElementList_COMMA_Elision_SpreadElement_init(context):
    el = ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_Elision_SpreadElement(
        context, "StrictArg", ["ElementList", ",", "Elision", "SpreadElement"]
    )
    assert el.name == "ElementList"
    assert el.ElementList == "ElementList"
    assert el.Elision == "Elision"
    assert el.SpreadElement == "SpreadElement"


class Test_parse_ElementList(parse_test):
    # Syntax
    #   ElementList[Yield, Await] :
    #       AssignmentExpression[+In, ?Yield, ?Await]
    #       Elision AssignmentExpression[+In, ?Yield, ?Await]
    #       SpreadElement[?Yield, ?Await]
    #       Elision SpreadElement[?Yield, ?Await]
    #       ElementList[?Yield, ?Await] , AssignmentExpression[+In, ?Yield, ?Await]
    #       ElementList[?Yield, ?Await] , SpreadElement[?Yield, ?Await]
    #       ElementList[?Yield, ?Await] , Elision AssignmentExpression[+In, ?Yield, ?Await]
    #       ElementList[?Yield, ?Await] , Elision SpreadElement[?Yield, ?Await]
    target = staticmethod(ecmascript.ecmascript.parse_ElementList)
    target_argnames = ("Yield", "Await")
    productions = (
        (("AssignmentExpression",), ecmascript.ecmascript.P2_ElementList_AssignmentExpression),
        (("Elision", "AssignmentExpression"), ecmascript.ecmascript.P2_ElementList_Elision_AssignmentExpression),
        (("SpreadElement",), ecmascript.ecmascript.P2_ElementList_SpreadElement),
        (("Elision", "SpreadElement"), ecmascript.ecmascript.P2_ElementList_Elision_SpreadElement),
        (
            ("AssignmentExpression", ",", "AssignmentExpression"),
            ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_AssignmentExpression,
        ),
        (
            ("AssignmentExpression", ",", "SpreadElement"),
            ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_SpreadElement,
        ),
        (
            ("AssignmentExpression", ",", "Elision", "AssignmentExpression"),
            ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_Elision_AssignmentExpression,
        ),
        (
            ("AssignmentExpression", ",", "Elision", "SpreadElement"),
            ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_Elision_SpreadElement,
        ),
    )
    called_argnames = {
        "AssignmentExpression": ("+In", "?Yield", "?Await"),
        "Elision": (),
        "SpreadElement": ("?Yield", "?Await"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


class Test_ArrayInitializer_ArrayAccumulation:
    # 12.2.5.2 Runtime Semantics: ArrayAccumulation
    #   With parameters array and nextIndex.

    # ElementList : Elision AssignmentExpression
    #   1. Let padding be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
    #   2. Let initResult be the result of evaluating AssignmentExpression.
    #   3. Let initValue be ? GetValue(initResult).
    #   4. Let created be CreateDataProperty(array, ToString(ToUint32(nextIndex + padding)), initValue).
    #   5. Assert: created is true.
    #   6. Return nextIndex + padding + 1.
    @strict_params
    def test_ElementList_Elision_AssignmentExpression(self, context, mocker, strict):
        elision = mocker.Mock(**{"ElisionWidth.return_value": 10})
        init_value = mocker.Mock()
        init_result = mocker.Mock()
        gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value=init_value)
        ae = mocker.Mock(**{"evaluate.return_value": init_result})
        array = mocker.Mock()
        cdp = mocker.patch("ecmascript.ecmascript.CreateDataProperty", return_value=True)
        el = ecmascript.ecmascript.P2_ElementList_Elision_AssignmentExpression(context, strict, [elision, ae])

        rv = el.ArrayAccumulation(array, 15)
        assert rv == 26
        cdp.assert_called_with(array, "25", init_value)
        gv.assert_called_with(init_result)

    @strict_params
    def test_ElementList_AssignmentExpression(self, context, mocker, strict):
        init_value = mocker.Mock()
        init_result = mocker.Mock()
        gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value=init_value)
        ae = mocker.Mock(**{"evaluate.return_value": init_result})
        array = mocker.Mock()
        cdp = mocker.patch("ecmascript.ecmascript.CreateDataProperty", return_value=True)
        el = ecmascript.ecmascript.P2_ElementList_AssignmentExpression(context, strict, [ae])

        rv = el.ArrayAccumulation(array, 15)
        assert rv == 16
        cdp.assert_called_with(array, "15", init_value)
        gv.assert_called_with(init_result)

    # ElementList : Elision SpreadElement
    #   1. Let padding be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
    #   2. Return the result of performing ArrayAccumulation for SpreadElement with arguments array and
    #      nextIndex + padding.
    @strict_params
    def test_ElementList_Elision_SpreadElement(self, context, mocker, strict):
        elision = mocker.Mock(**{"ElisionWidth.return_value": 10})
        se = mocker.Mock(**{"ArrayAccumulation.return_value": mocker.sentinel.aa_se})
        el = ecmascript.ecmascript.P2_ElementList_Elision_SpreadElement(context, strict, [elision, se])
        array = mocker.Mock()
        rv = el.ArrayAccumulation(array, 15)
        assert rv == mocker.sentinel.aa_se
        se.ArrayAccumulation.assert_called_with(array, 25)

    @strict_params
    def test_ElementList_SpreadElement(self, context, mocker, strict):
        se = mocker.Mock(**{"ArrayAccumulation.return_value": mocker.sentinel.aa_se})
        el = ecmascript.ecmascript.P2_ElementList_SpreadElement(context, strict, [se])
        rv = el.ArrayAccumulation(mocker.sentinel.array, 15)
        assert rv == mocker.sentinel.aa_se
        se.ArrayAccumulation.assert_called_with(mocker.sentinel.array, 15)

    # ElementList : ElementList , Elision AssignmentExpression
    #   1. Let postIndex be the result of performing ArrayAccumulation for ElementList with arguments array and nextIndex.
    #   2. ReturnIfAbrupt(postIndex).
    #   3. Let padding be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
    #   4. Let initResult be the result of evaluating AssignmentExpression.
    #   5. Let initValue be ? GetValue(initResult).
    #   6. Let created be CreateDataProperty(array, ToString(ToUint32(postIndex + padding)), initValue).
    #   7. Assert: created is true.
    #   8. Return postIndex + padding + 1.
    @strict_params
    def test_ElementList_ElementList_Elision_AssignmentExpression(self, context, mocker, strict):
        elision = mocker.Mock(**{"ElisionWidth.return_value": 10})
        el_child = mocker.Mock(**{"ArrayAccumulation.return_value": 100})
        ae = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.init_result})
        gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value=mocker.sentinel.init_value)
        cdp = mocker.patch("ecmascript.ecmascript.CreateDataProperty", return_value=True)
        el = ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_Elision_AssignmentExpression(
            context, strict, [el_child, mocker.Mock(), elision, ae]
        )

        rv = el.ArrayAccumulation(mocker.sentinel.array, 15)
        el_child.ArrayAccumulation.assert_called_with(mocker.sentinel.array, 15)
        gv.assert_called_with(mocker.sentinel.init_result)
        cdp.assert_called_with(mocker.sentinel.array, "110", mocker.sentinel.init_value)
        assert rv == 111

    @strict_params
    def test_ElementList_ElementList_AssignmentExpression(self, context, mocker, strict):
        el_child = mocker.Mock(**{"ArrayAccumulation.return_value": 100})
        ae = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.init_result})
        gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value=mocker.sentinel.init_value)
        cdp = mocker.patch("ecmascript.ecmascript.CreateDataProperty", return_value=True)
        el = ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_AssignmentExpression(
            context, strict, [el_child, mocker.Mock(), ae]
        )

        rv = el.ArrayAccumulation(mocker.sentinel.array, 15)
        el_child.ArrayAccumulation.assert_called_with(mocker.sentinel.array, 15)
        gv.assert_called_with(mocker.sentinel.init_result)
        cdp.assert_called_with(mocker.sentinel.array, "100", mocker.sentinel.init_value)
        assert rv == 101

    # ElementList : ElementList , Elision SpreadElement
    #   1. Let postIndex be the result of performing ArrayAccumulation for ElementList with arguments array and nextIndex.
    #   2. ReturnIfAbrupt(postIndex).
    #   3. Let padding be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
    #   4. Return the result of performing ArrayAccumulation for SpreadElement with arguments array and
    #      postIndex + padding.
    @strict_params
    def test_ElementList_ElementList_Elision_SpreadElement(self, context, mocker, strict):
        el_child = mocker.Mock(**{"ArrayAccumulation.return_value": 100})
        elision = mocker.Mock(**{"ElisionWidth.return_value": 10})
        se = mocker.Mock(**{"ArrayAccumulation.return_value": mocker.sentinel.aa_se})
        el = ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_Elision_SpreadElement(
            context, strict, [el_child, mocker.Mock(), elision, se]
        )
        rv = el.ArrayAccumulation(mocker.sentinel.array, 15)
        el_child.ArrayAccumulation.assert_called_with(mocker.sentinel.array, 15)
        se.ArrayAccumulation.assert_called_with(mocker.sentinel.array, 110)
        assert rv == mocker.sentinel.aa_se

    @strict_params
    def test_ElementList_ElementList_SpreadElement(self, context, mocker, strict):
        el_child = mocker.Mock(**{"ArrayAccumulation.return_value": 100})
        se = mocker.Mock(**{"ArrayAccumulation.return_value": mocker.sentinel.aa_se})
        el = ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_SpreadElement(
            context, strict, [el_child, mocker.Mock(), se]
        )
        rv = el.ArrayAccumulation(mocker.sentinel.array, 15)
        el_child.ArrayAccumulation.assert_called_with(mocker.sentinel.array, 15)
        se.ArrayAccumulation.assert_called_with(mocker.sentinel.array, 100)
        assert rv == mocker.sentinel.aa_se

    # SpreadElement : ... AssignmentExpression
    #   1. Let spreadRef be the result of evaluating AssignmentExpression.
    #   2. Let spreadObj be ? GetValue(spreadRef).
    #   3. Let iteratorRecord be ? GetIterator(spreadObj).
    #   4. Repeat,
    #       a. Let next be ? IteratorStep(iteratorRecord).
    #       b. If next is false, return nextIndex.
    #       c. Let nextValue be ? IteratorValue(next).
    #       d. Let status be CreateDataProperty(array, ToString(ToUint32(nextIndex)), nextValue).
    #       e. Assert: status is true.
    #       f. Increase nextIndex by 1.
    @pytest.mark.parametrize("size", (0, 1, 3))
    @strict_params
    def test_SpreadElement_AssignmentExpression(self, context, mocker, strict, size):
        ae = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.spread_ref})
        gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value=mocker.sentinel.spread_obj)
        gi = mocker.patch("ecmascript.ecmascript.GetIterator", return_value=mocker.sentinel.iterator_record)
        gen = ((z,) for z in range(size))
        mystep = lambda iteratorRecord: next(gen, False)
        its = mocker.patch("ecmascript.ecmascript.IteratorStep", side_effect=mystep)
        myvalue = lambda nextresult: str(nextresult[0])
        itv = mocker.patch("ecmascript.ecmascript.IteratorValue", side_effect=myvalue)
        cdp = mocker.patch("ecmascript.ecmascript.CreateDataProperty", return_value=True)
        se = ecmascript.ecmascript.P2_SpreadElement_DOTDOTDOT_AssignmentExpression(
            context, strict, [mocker.Mock(), ae]
        )

        rv = se.ArrayAccumulation(mocker.sentinel.array, 15)

        se.AssignmentExpression.evaluate.assert_called_with()
        gv.assert_called_with(mocker.sentinel.spread_ref)
        gi.assert_called_with(mocker.sentinel.spread_obj)
        assert its.call_args_list == [mocker.call(mocker.sentinel.iterator_record)] * (size + 1)
        assert itv.call_args_list == [mocker.call((x,)) for x in range(size)]
        assert cdp.call_args_list == [mocker.call(mocker.sentinel.array, str(x + 15), str(x)) for x in range(size)]
        assert rv == size + 15


#### SpreadElement #########################################################################################################
#
#      .d8888b.                                          888 8888888888 888                                          888
#     d88P  Y88b                                         888 888        888                                          888
#     Y88b.                                              888 888        888                                          888
#      "Y888b.   88888b.  888d888  .d88b.   8888b.   .d88888 8888888    888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
#         "Y88b. 888 "88b 888P"   d8P  Y8b     "88b d88" 888 888        888 d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
#           "888 888  888 888     88888888 .d888888 888  888 888        888 88888888 888  888  888 88888888 888  888 888
#     Y88b  d88P 888 d88P 888     Y8b.     888  888 Y88b 888 888        888 Y8b.     888  888  888 Y8b.     888  888 Y88b.
#      "Y8888P"  88888P"  888      "Y8888  "Y888888  "Y88888 8888888888 888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#                888
#                888
#                888
#
############################################################################################################################
def test_P2_SpreadElement_init(context):
    se = ecmascript.ecmascript.P2_SpreadElement(context, "StrictArg", ["child"])
    assert se.name == "SpreadElement"
    assert se.context == context
    assert se.children == ["child"]


def test_P2_SpreadElement_DOTDOTDOT_AssignmentExpression_init(context):
    se = ecmascript.ecmascript.P2_SpreadElement_DOTDOTDOT_AssignmentExpression(
        context, "StrictArg", ["...", "AssignmentExpression"]
    )
    assert se.name == "SpreadElement"
    assert se.AssignmentExpression == "AssignmentExpression"


class Test_parse_SpreadElement(parse_test):
    # Syntax
    #   SpreadElement[Yield, Await]:
    #       ... AssignmentExpression[+In, ?Yield, ?Await]
    target = staticmethod(ecmascript.ecmascript.parse_SpreadElement)
    target_argnames = ("Yield", "Await")
    productions = (
        (("...", "AssignmentExpression"), ecmascript.ecmascript.P2_SpreadElement_DOTDOTDOT_AssignmentExpression),
    )
    called_argnames = {
        "AssignmentExpression": ("+In", "?Yield", "?Await"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


#### ObjectLiteral ######################################################################################
#
#  .d88888b.  888         d8b                   888    888      d8b 888                              888
# d88P" "Y88b 888         Y8P                   888    888      Y8P 888                              888
# 888     888 888                               888    888          888                              888
# 888     888 88888b.    8888  .d88b.   .d8888b 888888 888      888 888888  .d88b.  888d888  8888b.  888
# 888     888 888 "88b   "888 d8P  Y8b d88P"    888    888      888 888    d8P  Y8b 888P"       "88b 888
# 888     888 888  888    888 88888888 888      888    888      888 888    88888888 888     .d888888 888
# Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.  888      888 Y88b.  Y8b.     888     888  888 888
#  "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888 88888888 888  "Y888  "Y8888  888     "Y888888 888
#                         888
#                        d88P
#                      888P"
#
#########################################################################################################
def test_P2_ObjectLiteral_init(context):
    ol = ecmascript.ecmascript.P2_ObjectLiteral(context, "StrictArg", ["child"])
    assert ol.name == "ObjectLiteral"
    assert ol.context == context
    assert ol.children == ["child"]


def test_P2_ObjectLiteral_LCURLY_RCURLY_init(context):
    ol = ecmascript.ecmascript.P2_ObjectLiteral_LCURLY_RCURLY(context, "StrictArg", ["{", "}"])
    assert ol.name == "ObjectLiteral"


def test_P2_ObjectLiteral_LCURLY_PropertyDefinitionList_RCURLY_init(context):
    ol = ecmascript.ecmascript.P2_ObjectLiteral_LCURLY_PropertyDefinitionList_RCURLY(
        context, "StrictArg", ["{", "PropertyDefinitionList", "}"]
    )
    assert ol.name == "ObjectLiteral"
    assert ol.PropertyDefinitionList == "PropertyDefinitionList"


def test_P2_ObjectLiteral_LCURLY_PropertyDefinitionList_COMMA_RCURLY_init(context):
    ol = ecmascript.ecmascript.P2_ObjectLiteral_LCURLY_PropertyDefinitionList_COMMA_RCURLY(
        context, "StrictArg", ["{", "PropertyDefinitionList", ",", "}"]
    )
    assert ol.name == "ObjectLiteral"
    assert ol.PropertyDefinitionList == "PropertyDefinitionList"


class Test_parse_ObjectLiteral(parse_test):
    # Syntax
    #   ObjectLiteral[Yield, Await] :
    #       { }
    #       { PropertyDefinitionList[?Yield, ?Await] }
    #       { PropertyDefinitionList[?Yield, ?Await] , }
    target = staticmethod(ecmascript.ecmascript.parse_ObjectLiteral)
    target_argnames = ("Yield", "Await")
    productions = (
        (("{", "}"), ecmascript.ecmascript.P2_ObjectLiteral_LCURLY_RCURLY),
        (
            ("{", "PropertyDefinitionList", "}"),
            ecmascript.ecmascript.P2_ObjectLiteral_LCURLY_PropertyDefinitionList_RCURLY,
        ),
        (
            ("{", "PropertyDefinitionList", ",", "}"),
            ecmascript.ecmascript.P2_ObjectLiteral_LCURLY_PropertyDefinitionList_COMMA_RCURLY,
        ),
    )
    called_argnames = {"PropertyDefinitionList": ("?Yield", "?Await")}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


#### PropertyDefinitionList ######################################################################################################################################################
#
#     8888888b.                                             888             8888888b.            .d888 d8b          d8b 888    d8b                   888      d8b          888
#     888   Y88b                                            888             888  "Y88b          d88P"  Y8P          Y8P 888    Y8P                   888      Y8P          888
#     888    888                                            888             888    888          888                     888                          888                   888
#     888   d88P 888d888  .d88b.  88888b.   .d88b.  888d888 888888 888  888 888    888  .d88b.  888888 888 88888b.  888 888888 888  .d88b.  88888b.  888      888 .d8888b  888888
#     8888888P"  888P"   d88""88b 888 "88b d8P  Y8b 888P"   888    888  888 888    888 d8P  Y8b 888    888 888 "88b 888 888    888 d88""88b 888 "88b 888      888 88K      888
#     888        888     888  888 888  888 88888888 888     888    888  888 888    888 88888888 888    888 888  888 888 888    888 888  888 888  888 888      888 "Y8888b. 888
#     888        888     Y88..88P 888 d88P Y8b.     888     Y88b.  Y88b 888 888  .d88P Y8b.     888    888 888  888 888 Y88b.  888 Y88..88P 888  888 888      888      X88 Y88b.
#     888        888      "Y88P"  88888P"   "Y8888  888      "Y888  "Y88888 8888888P"   "Y8888  888    888 888  888 888  "Y888 888  "Y88P"  888  888 88888888 888  88888P'  "Y888
#                                 888                                   888
#                                 888                              Y8b d88P
#                                 888                               "Y88P"
#
##################################################################################################################################################################################
def test_P2_PropertyDefinitionList_init(context):
    pdl = ecmascript.ecmascript.P2_PropertyDefinitionList(context, "StrictArg", ["child"])
    assert pdl.name == "PropertyDefinitionList"
    assert pdl.context == context
    assert pdl.children == ["child"]


def test_P2_PropertyDefinitionList_PropertyDefinition_init(context):
    pdl = ecmascript.ecmascript.P2_PropertyDefinitionList_PropertyDefinition(
        context, "StrictArg", ["PropertyDefinition"]
    )
    assert pdl.name == "PropertyDefinitionList"
    assert pdl.PropertyDefinition == "PropertyDefinition"


def test_P2_PropertyDefinitionList_PropertyDefinitionList_COMMA_PropertyDefinition_init(context):
    pdl = ecmascript.ecmascript.P2_PropertyDefinitionList_PropertyDefinitionList_COMMA_PropertyDefinition(
        context, "StrictArg", ["PropertyDefinitionList", ",", "PropertyDefinition"]
    )
    assert pdl.name == "PropertyDefinitionList"
    assert pdl.PropertyDefinitionList == "PropertyDefinitionList"
    assert pdl.PropertyDefinition == "PropertyDefinition"


class Test_parse_PropertyDefinitionList(parse_test):
    # Syntax
    #   PropertyDefinitionList[Yield, Await] :
    #       PropertyDefinition[?Yield, ?Await]
    #       PropertyDefinitionList[?Yield, ?Await] , PropertyDefinition[?Yield, ?Await]
    target = staticmethod(ecmascript.ecmascript.parse_PropertyDefinitionList)
    target_argnames = ("Yield", "Await")
    productions = (
        (("PropertyDefinition",), ecmascript.ecmascript.P2_PropertyDefinitionList_PropertyDefinition),
        (
            ("PropertyDefinition", ",", "PropertyDefinition"),
            ecmascript.ecmascript.P2_PropertyDefinitionList_PropertyDefinitionList_COMMA_PropertyDefinition,
        ),
    )
    called_argnames = {"PropertyDefinition": ("?Yield", "?Await")}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


#### PropertyDefinition #############################################################################################################################
#
#     8888888b.                                             888             8888888b.            .d888 d8b          d8b 888    d8b
#     888   Y88b                                            888             888  "Y88b          d88P"  Y8P          Y8P 888    Y8P
#     888    888                                            888             888    888          888                     888
#     888   d88P 888d888  .d88b.  88888b.   .d88b.  888d888 888888 888  888 888    888  .d88b.  888888 888 88888b.  888 888888 888  .d88b.  88888b.
#     8888888P"  888P"   d88""88b 888 "88b d8P  Y8b 888P"   888    888  888 888    888 d8P  Y8b 888    888 888 "88b 888 888    888 d88""88b 888 "88b
#     888        888     888  888 888  888 88888888 888     888    888  888 888    888 88888888 888    888 888  888 888 888    888 888  888 888  888
#     888        888     Y88..88P 888 d88P Y8b.     888     Y88b.  Y88b 888 888  .d88P Y8b.     888    888 888  888 888 Y88b.  888 Y88..88P 888  888
#     888        888      "Y88P"  88888P"   "Y8888  888      "Y888  "Y88888 8888888P"   "Y8888  888    888 888  888 888  "Y888 888  "Y88P"  888  888
#                                 888                                   888
#                                 888                              Y8b d88P
#                                 888                               "Y88P"
#
#####################################################################################################################################################
def test_P2_PropertyDefinition_init(context):
    pd = ecmascript.ecmascript.P2_PropertyDefinition(context, "StrictArg", ["child"])
    assert pd.name == "PropertyDefinition"
    assert pd.context == context
    assert pd.children == ["child"]


def test_P2_PropertyDefinition_IdentifierReference_init(context):
    pd = ecmascript.ecmascript.P2_PropertyDefinition_IdentifierReference(
        context, "StrictArg", ["IdentifierReference"]
    )
    assert pd.name == "PropertyDefinition"
    assert pd.IdentifierReference == "IdentifierReference"


def test_P2_PropertyDefinition_CoverInitializedName_init(context):
    pd = ecmascript.ecmascript.P2_PropertyDefinition_CoverInitializedName(
        context, "StrictArg", ["CoverInitializedName"]
    )
    assert pd.name == "PropertyDefinition"
    assert pd.CoverInitializedName == "CoverInitializedName"


def test_P2_PropertyDefinition_PropertyName_COLON_AssignmentExpression_init(context):
    pd = ecmascript.ecmascript.P2_PropertyDefinition_PropertyName_COLON_AssignmentExpression(
        context, "StrictArg", ["PropertyName", ":", "AssignmentExpression"]
    )
    assert pd.name == "PropertyDefinition"
    assert pd.PropertyName == "PropertyName"
    assert pd.AssignmentExpression == "AssignmentExpression"


def test_P2_PropertyDefinition_MethodDefinition_init(context):
    pd = ecmascript.ecmascript.P2_PropertyDefinition_MethodDefinition(context, "StrictArg", ["MethodDefinition"])
    assert pd.name == "PropertyDefinition"
    assert pd.MethodDefinition == "MethodDefinition"


def test_P2_PropertyDefinition_DOTDOTDOT_AssignmentExpression_init(context):
    pd = ecmascript.ecmascript.P2_PropertyDefinition_DOTDOTDOT_AssignmentExpression(
        context, "StrictArg", ["...", "AssignmentExpression"]
    )
    assert pd.name == "PropertyDefinition"
    assert pd.AssignmentExpression == "AssignmentExpression"


class Test_parse_PropertyDefinition(parse_test):
    # Syntax
    #   PropertyDefinition[Yield, Await] :
    #       IdentifierReference[?Yield, ?Await]
    #       CoverInitializedName[?Yield, ?Await]
    #       PropertyName[?Yield, ?Await] : AssignmentExpression[+In, ?Yield, ?Await]
    #       MethodDefinition[?Yield, ?Await]
    #       ... AssignmentExpression[+In, ?Yield, ?Await]
    target = staticmethod(ecmascript.ecmascript.parse_PropertyDefinition)
    target_argnames = ("Yield", "Await")
    productions = (
        (("IdentifierReference",), ecmascript.ecmascript.P2_PropertyDefinition_IdentifierReference),
        (("CoverInitializedName",), ecmascript.ecmascript.P2_PropertyDefinition_CoverInitializedName),
        (
            ("PropertyName", ":", "AssignmentExpression"),
            ecmascript.ecmascript.P2_PropertyDefinition_PropertyName_COLON_AssignmentExpression,
        ),
        (("MethodDefinition",), ecmascript.ecmascript.P2_PropertyDefinition_MethodDefinition),
        (
            ("...", "AssignmentExpression"),
            ecmascript.ecmascript.P2_PropertyDefinition_DOTDOTDOT_AssignmentExpression,
        ),
    )
    called_argnames = {
        "IdentifierReference": ("?Yield", "?Await"),
        "CoverInitializedName": ("?Yield", "?Await"),
        "PropertyName": ("?Yield", "?Await"),
        "AssignmentExpression": ("+In", "?Yield", "?Await"),
        "MethodDefinition": ("?Yield", "?Await"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


#### PropertyName ######################################################################################################
#
#     8888888b.                                             888             888b    888
#     888   Y88b                                            888             8888b   888
#     888    888                                            888             88888b  888
#     888   d88P 888d888  .d88b.  88888b.   .d88b.  888d888 888888 888  888 888Y88b 888  8888b.  88888b.d88b.   .d88b.
#     8888888P"  888P"   d88""88b 888 "88b d8P  Y8b 888P"   888    888  888 888 Y88b888     "88b 888 "888 "88b d8P  Y8b
#     888        888     888  888 888  888 88888888 888     888    888  888 888  Y88888 .d888888 888  888  888 88888888
#     888        888     Y88..88P 888 d88P Y8b.     888     Y88b.  Y88b 888 888   Y8888 888  888 888  888  888 Y8b.
#     888        888      "Y88P"  88888P"   "Y8888  888      "Y888  "Y88888 888    Y888 "Y888888 888  888  888  "Y8888
#                                 888                                   888
#                                 888                              Y8b d88P
#                                 888                               "Y88P"
#
########################################################################################################################
def test_P2_PropertyName_init(context):
    pn = ecmascript.ecmascript.P2_PropertyName(context, "StrictArg", ["child"])
    assert pn.name == "PropertyName"
    assert pn.context == context
    assert pn.children == ["child"]


def test_P2_PropertyName_LiteralPropertyName_init(context):
    pn = ecmascript.ecmascript.P2_PropertyName_LiteralPropertyName(context, "StrictArg", ["LiteralPropertyName"])
    assert pn.name == "PropertyName"
    assert pn.LiteralPropertyName == "LiteralPropertyName"


def test_P2_PropertyName_ComputedPropertyName_init(context):
    pn = ecmascript.ecmascript.P2_PropertyName_ComputedPropertyName(context, "StrictArg", ["ComputedPropertyName"])
    assert pn.name == "PropertyName"
    assert pn.ComputedPropertyName == "ComputedPropertyName"


class Test_parse_PropertyName(parse_test):
    # Syntax
    #   PropertyName[Yield, Await] :
    #       LiteralPropertyName
    #       ComputedPropertyName[?Yield, ?Await]
    target = staticmethod(ecmascript.ecmascript.parse_PropertyName)
    target_argnames = ("Yield", "Await")
    productions = (
        (("LiteralPropertyName",), ecmascript.ecmascript.P2_PropertyName_LiteralPropertyName),
        (("ComputedPropertyName",), ecmascript.ecmascript.P2_PropertyName_ComputedPropertyName),
    )
    called_argnames = {"LiteralPropertyName": (), "ComputedPropertyName": ("?Yield", "?Await")}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


#### LiteralPropertyName #################################################################################################################################################
#
#     888      d8b 888                              888 8888888b.                                             888             888b    888
#     888      Y8P 888                              888 888   Y88b                                            888             8888b   888
#     888          888                              888 888    888                                            888             88888b  888
#     888      888 888888  .d88b.  888d888  8888b.  888 888   d88P 888d888  .d88b.  88888b.   .d88b.  888d888 888888 888  888 888Y88b 888  8888b.  88888b.d88b.   .d88b.
#     888      888 888    d8P  Y8b 888P"       "88b 888 8888888P"  888P"   d88""88b 888 "88b d8P  Y8b 888P"   888    888  888 888 Y88b888     "88b 888 "888 "88b d8P  Y8b
#     888      888 888    88888888 888     .d888888 888 888        888     888  888 888  888 88888888 888     888    888  888 888  Y88888 .d888888 888  888  888 88888888
#     888      888 Y88b.  Y8b.     888     888  888 888 888        888     Y88..88P 888 d88P Y8b.     888     Y88b.  Y88b 888 888   Y8888 888  888 888  888  888 Y8b.
#     88888888 888  "Y888  "Y8888  888     "Y888888 888 888        888      "Y88P"  88888P"   "Y8888  888      "Y888  "Y88888 888    Y888 "Y888888 888  888  888  "Y8888
#                                                                                   888                                   888
#                                                                                   888                              Y8b d88P
#                                                                                   888                               "Y88P"
#
##########################################################################################################################################################################
def test_P2_LiteralPropertyName_init(context):
    lpn = ecmascript.ecmascript.P2_LiteralPropertyName(context, "StrictArg", ["child"])
    assert lpn.name == "LiteralPropertyName"
    assert lpn.context == context
    assert lpn.children == ["child"]


def test_P2_LiteralPropertyName_IdentifierName_init(context):
    lpn = ecmascript.ecmascript.P2_LiteralPropertyName_IdentifierName(context, "StrictArg", ["IdentifierName"])
    assert lpn.name == "LiteralPropertyName"
    assert lpn.IdentifierName == "IdentifierName"


def test_P2_LiteralPropertyName_StringLiteral_init(context):
    lpn = ecmascript.ecmascript.P2_LiteralPropertyName_StringLiteral(context, "StrictArg", ["StringLiteral"])
    assert lpn.name == "LiteralPropertyName"
    assert lpn.StringLiteral == "StringLiteral"


def test_P2_LiteralPropertyName_NumericLiteral_init(context):
    lpn = ecmascript.ecmascript.P2_LiteralPropertyName_NumericLiteral(context, "StrictArg", ["NumericLiteral"])
    assert lpn.name == "LiteralPropertyName"
    assert lpn.NumericLiteral == "NumericLiteral"


class Test_parse_LiteralPropertyName(parse_test):
    # Syntax
    #   LiteralPropertyName:
    #       IdentifierName
    #       StringLiteral
    #       NumericLiteral
    target = staticmethod(ecmascript.ecmascript.parse_LiteralPropertyName)
    target_argnames = ()
    productions = (
        (("IDENTIFIER¡bob",), ecmascript.ecmascript.P2_LiteralPropertyName_IdentifierName),
        (("STRING¡bob",), ecmascript.ecmascript.P2_LiteralPropertyName_StringLiteral),
        (("NUMERIC¡772",), ecmascript.ecmascript.P2_LiteralPropertyName_NumericLiteral),
    )
    called_argnames = {}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


#### ComputedPropertyName #######################################################################################################################################################################
#
#  .d8888b.                                           888                  888 8888888b.                                             888             888b    888
# d88P  Y88b                                          888                  888 888   Y88b                                            888             8888b   888
# 888    888                                          888                  888 888    888                                            888             88888b  888
# 888         .d88b.  88888b.d88b.  88888b.  888  888 888888  .d88b.   .d88888 888   d88P 888d888  .d88b.  88888b.   .d88b.  888d888 888888 888  888 888Y88b 888  8888b.  88888b.d88b.   .d88b.
# 888        d88""88b 888 "888 "88b 888 "88b 888  888 888    d8P  Y8b d88" 888 8888888P"  888P"   d88""88b 888 "88b d8P  Y8b 888P"   888    888  888 888 Y88b888     "88b 888 "888 "88b d8P  Y8b
# 888    888 888  888 888  888  888 888  888 888  888 888    88888888 888  888 888        888     888  888 888  888 88888888 888     888    888  888 888  Y88888 .d888888 888  888  888 88888888
# Y88b  d88P Y88..88P 888  888  888 888 d88P Y88b 888 Y88b.  Y8b.     Y88b 888 888        888     Y88..88P 888 d88P Y8b.     888     Y88b.  Y88b 888 888   Y8888 888  888 888  888  888 Y8b.
#  "Y8888P"   "Y88P"  888  888  888 88888P"   "Y88888  "Y888  "Y8888   "Y88888 888        888      "Y88P"  88888P"   "Y8888  888      "Y888  "Y88888 888    Y888 "Y888888 888  888  888  "Y8888
#                                   888                                                                    888                                   888
#                                   888                                                                    888                              Y8b d88P
#                                   888                                                                    888                               "Y88P"
#
#################################################################################################################################################################################################
def test_P2_ComputedPropertyName_init(context):
    cpn = ecmascript.ecmascript.P2_ComputedPropertyName(context, "StrictArg", ["child"])
    assert cpn.name == "ComputedPropertyName"
    assert cpn.context == context
    assert cpn.children == ["child"]


def test_P2_ComputedPropertyName_LBRACKET_AssignmentExpression_RBRACKET_init(context):
    cpn = ecmascript.ecmascript.P2_ComputedPropertyName_LBRACKET_AssignmentExpression_RBRACKET(
        context, "StrictArg", ["[", "AssignmentExpression", "]"]
    )
    assert cpn.name == "ComputedPropertyName"
    assert cpn.AssignmentExpression == "AssignmentExpression"


class Test_parse_ComputedPropertyName(parse_test):
    # Syntax
    #   ComputedPropertyName[Yield, Await] :
    #       [ AssignmentExpression[+In, ?Yield, ?Await] ]
    target = staticmethod(ecmascript.ecmascript.parse_ComputedPropertyName)
    target_argnames = ("Yield", "Await")
    productions = (
        (
            ("[", "AssignmentExpression", "]"),
            ecmascript.ecmascript.P2_ComputedPropertyName_LBRACKET_AssignmentExpression_RBRACKET,
        ),
    )
    called_argnames = {"AssignmentExpression": ("+In", "?Yield", "?Await")}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


#### CoverInitializedName ##############################################################################################################################################
#
#  .d8888b.                                     8888888          d8b 888    d8b          888 d8b                        888 888b    888
# d88P  Y88b                                      888            Y8P 888    Y8P          888 Y8P                        888 8888b   888
# 888    888                                      888                888                 888                            888 88888b  888
# 888         .d88b.  888  888  .d88b.  888d888   888   88888b.  888 888888 888  8888b.  888 888 88888888  .d88b.   .d88888 888Y88b 888  8888b.  88888b.d88b.   .d88b.
# 888        d88""88b 888  888 d8P  Y8b 888P"     888   888 "88b 888 888    888     "88b 888 888    d88P  d8P  Y8b d88" 888 888 Y88b888     "88b 888 "888 "88b d8P  Y8b
# 888    888 888  888 Y88  88P 88888888 888       888   888  888 888 888    888 .d888888 888 888   d88P   88888888 888  888 888  Y88888 .d888888 888  888  888 88888888
# Y88b  d88P Y88..88P  Y8bd8P  Y8b.     888       888   888  888 888 Y88b.  888 888  888 888 888  d88P    Y8b.     Y88b 888 888   Y8888 888  888 888  888  888 Y8b.
#  "Y8888P"   "Y88P"    Y88P    "Y8888  888     8888888 888  888 888  "Y888 888 "Y888888 888 888 88888888  "Y8888   "Y88888 888    Y888 "Y888888 888  888  888  "Y8888
#
#
#
#
########################################################################################################################################################################
def test_P2_CoverInitializedName_init(context):
    cin = ecmascript.ecmascript.P2_CoverInitializedName(context, "StrictArg", ["child"])
    assert cin.name == "CoverInitializedName"
    assert cin.context == context
    assert cin.children == ["child"]


def test_P2_CoverInitializedName_IdentifierReference_Initializer_init(context):
    cin = ecmascript.ecmascript.P2_CoverInitializedName_IdentifierReference_Initializer(
        context, "StrictArg", ["IdentifierReference", "Initializer"]
    )
    assert cin.name == "CoverInitializedName"
    assert cin.IdentifierReference == "IdentifierReference"
    assert cin.Initializer == "Initializer"


class Test_parse_CoverInitializedName(parse_test):
    # Syntax
    #   CoverInitializedName[Yield, Await] :
    #       IdentifierReference[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]
    target = staticmethod(ecmascript.ecmascript.parse_CoverInitializedName)
    target_argnames = ("Yield", "Await")
    productions = (
        (
            ("IdentifierReference", "Initializer"),
            ecmascript.ecmascript.P2_CoverInitializedName_IdentifierReference_Initializer,
        ),
    )
    called_argnames = {"IdentifierReference": ("?Yield", "?Await"), "Initializer": ("+In", "?Yield", "?Await")}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


#### Initializer ############################################################
#
# 8888888          d8b 888    d8b          888 d8b
#   888            Y8P 888    Y8P          888 Y8P
#   888                888                 888
#   888   88888b.  888 888888 888  8888b.  888 888 88888888  .d88b.  888d888
#   888   888 "88b 888 888    888     "88b 888 888    d88P  d8P  Y8b 888P"
#   888   888  888 888 888    888 .d888888 888 888   d88P   88888888 888
#   888   888  888 888 Y88b.  888 888  888 888 888  d88P    Y8b.     888
# 8888888 888  888 888  "Y888 888 "Y888888 888 888 88888888  "Y8888  888
#
#
#
#
#############################################################################
def test_P2_Initializer_init(context):
    init = ecmascript.ecmascript.P2_Initializer(context, "StrictArg", ["child"])
    assert init.name == "Initializer"
    assert init.context == context
    assert init.children == ["child"]


def test_P2_Initializer_EQUALS_AssignmentExpression_init(context):
    init = ecmascript.ecmascript.P2_Initializer_EQUALS_AssignmentExpression(
        context, "StrictArg", ["=", "AssignmentExpression"]
    )
    assert init.name == "Initializer"
    assert init.AssignmentExpression == "AssignmentExpression"


class Test_parse_Initializer(parse_test):
    # Syntax
    #   Initializer[In, Yield, Await]:
    #       = AssignmentExpression[?In, ?Yield, ?Await]
    target = staticmethod(ecmascript.ecmascript.parse_Initializer)
    target_argnames = ("In", "Yield", "Await")
    productions = (
        (("=", "AssignmentExpression"), ecmascript.ecmascript.P2_Initializer_EQUALS_AssignmentExpression,),
    )
    called_argnames = {
        "AssignmentExpression": ("?In", "?Yield", "?Await"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


#### TemplateLiteral ########################################################################################################
#
# 88888888888                                 888          888             888      d8b 888                              888
#     888                                     888          888             888      Y8P 888                              888
#     888                                     888          888             888          888                              888
#     888      .d88b.  88888b.d88b.  88888b.  888  8888b.  888888  .d88b.  888      888 888888  .d88b.  888d888  8888b.  888
#     888     d8P  Y8b 888 "888 "88b 888 "88b 888     "88b 888    d8P  Y8b 888      888 888    d8P  Y8b 888P"       "88b 888
#     888     88888888 888  888  888 888  888 888 .d888888 888    88888888 888      888 888    88888888 888     .d888888 888
#     888     Y8b.     888  888  888 888 d88P 888 888  888 Y88b.  Y8b.     888      888 Y88b.  Y8b.     888     888  888 888
#     888      "Y8888  888  888  888 88888P"  888 "Y888888  "Y888  "Y8888  88888888 888  "Y888  "Y8888  888     "Y888888 888
#                                    888
#                                    888
#                                    888
#
#############################################################################################################################
def test_P2_TemplateLiteral_init(context):
    tl = ecmascript.ecmascript.P2_TemplateLiteral(context, "StrictArg", ["child"], "TaggedArg")
    assert tl.name == "TemplateLiteral"
    assert tl.context == context
    assert tl.children == ["child"]
    assert tl.Tagged == "TaggedArg"


def test_P2_TemplateLiteral_NoSubstitutionTemplate_init(context):
    tl = ecmascript.ecmascript.P2_TemplateLiteral_NoSubstitutionTemplate(context, "StrictArg", ["NST"], "T")
    assert tl.name == "TemplateLiteral"
    assert tl.NoSubstitutionTemplate == "NST"


def test_P2_TemplateLiteral_SubstitutionTemplate_init(context):
    tl = ecmascript.ecmascript.P2_TemplateLiteral_SubstitutionTemplate(context, "StrictArg", ["SubTemp"], "T")
    assert tl.name == "TemplateLiteral"
    assert tl.SubstitutionTemplate == "SubTemp"


class Test_parse_TemplateLiteral(parse_test):
    # Syntax
    #   TemplateLiteral[Yield, Await, Tagged] :
    #       NoSubstitutionTemplate
    #       SubstitutionTemplate[?Yield, ?Await, ?Tagged]
    target = staticmethod(ecmascript.ecmascript.parse_TemplateLiteral)
    target_argnames = ("Yield", "Await", "Tagged")
    productions = (
        (("NOSUBSTITUTIONTEMPLATE¡bob",), ecmascript.ecmascript.P2_TemplateLiteral_NoSubstitutionTemplate),
        (("SubstitutionTemplate",), ecmascript.ecmascript.P2_TemplateLiteral_SubstitutionTemplate),
    )
    called_argnames = {
        "NoSubstitutionTempate": (),
        "SubstitutionTemplate": ("?Yield", "?Await", "?Tagged"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


#### SubstitutionTemplate ###############################################################################################################################################
#
#  .d8888b.           888               888    d8b 888             888    d8b                   88888888888                                 888          888
# d88P  Y88b          888               888    Y8P 888             888    Y8P                       888                                     888          888
# Y88b.               888               888        888             888                              888                                     888          888
#  "Y888b.   888  888 88888b.  .d8888b  888888 888 888888 888  888 888888 888  .d88b.  88888b.      888      .d88b.  88888b.d88b.  88888b.  888  8888b.  888888  .d88b.
#     "Y88b. 888  888 888 "88b 88K      888    888 888    888  888 888    888 d88""88b 888 "88b     888     d8P  Y8b 888 "888 "88b 888 "88b 888     "88b 888    d8P  Y8b
#       "888 888  888 888  888 "Y8888b. 888    888 888    888  888 888    888 888  888 888  888     888     88888888 888  888  888 888  888 888 .d888888 888    88888888
# Y88b  d88P Y88b 888 888 d88P      X88 Y88b.  888 Y88b.  Y88b 888 Y88b.  888 Y88..88P 888  888     888     Y8b.     888  888  888 888 d88P 888 888  888 Y88b.  Y8b.
#  "Y8888P"   "Y88888 88888P"   88888P'  "Y888 888  "Y888  "Y88888  "Y888 888  "Y88P"  888  888     888      "Y8888  888  888  888 88888P"  888 "Y888888  "Y888  "Y8888
#                                                                                                                                  888
#                                                                                                                                  888
#                                                                                                                                  888
#
#########################################################################################################################################################################
def test_P2_SubstitutionTemplate_init(context):
    st = ecmascript.ecmascript.P2_SubstitutionTemplate(context, "StrictArg", ["child"], "TaggedArg")
    assert st.name == "SubstitutionTemplate"
    assert st.context == context
    assert st.children == ["child"]
    assert st.Tagged == "TaggedArg"


def test_P2_SubstitutionTemplate_TemplateHead_Expression_TemplateSpans_init(context):
    st = ecmascript.ecmascript.P2_SubstitutionTemplate_TemplateHead_Expression_TemplateSpans(
        context, "StrictArg", ["TemplateHead", "Expression", "TemplateSpans"], "T"
    )
    assert st.name == "SubstitutionTemplate"
    assert st.TemplateHead == "TemplateHead"
    assert st.Expression == "Expression"
    assert st.TemplateSpans == "TemplateSpans"


class Test_parse_SubstitutionTemplate(parse_test):
    # Syntax
    #   SubstitutionTemplate[Yield, Await, Tagged] :
    #       TemplateHead Expression[+In, ?Yield, ?Await] TemplateSpans[?Yield, ?Await, ?Tagged]
    target = staticmethod(ecmascript.ecmascript.parse_SubstitutionTemplate)
    target_argnames = ("Yield", "Await", "Tagged")
    productions = (
        (
            ("TEMPLATEHEAD¡bob", "Expression", "TemplateSpans"),
            ecmascript.ecmascript.P2_SubstitutionTemplate_TemplateHead_Expression_TemplateSpans,
        ),
    )
    called_argnames = {
        "Expression": ("+In", "?Yield", "?Await"),
        "TemplateSpans": ("?Yield", "?Await", "?Tagged"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


#### TemplateSpans #######################################################################################################
#
# 88888888888                                 888          888              .d8888b.
#     888                                     888          888             d88P  Y88b
#     888                                     888          888             Y88b.
#     888      .d88b.  88888b.d88b.  88888b.  888  8888b.  888888  .d88b.   "Y888b.   88888b.   8888b.  88888b.  .d8888b
#     888     d8P  Y8b 888 "888 "88b 888 "88b 888     "88b 888    d8P  Y8b     "Y88b. 888 "88b     "88b 888 "88b 88K
#     888     88888888 888  888  888 888  888 888 .d888888 888    88888888       "888 888  888 .d888888 888  888 "Y8888b.
#     888     Y8b.     888  888  888 888 d88P 888 888  888 Y88b.  Y8b.     Y88b  d88P 888 d88P 888  888 888  888      X88
#     888      "Y8888  888  888  888 88888P"  888 "Y888888  "Y888  "Y8888   "Y8888P"  88888P"  "Y888888 888  888  88888P'
#                                    888                                              888
#                                    888                                              888
#                                    888                                              888
#
##########################################################################################################################
def test_P2_TemplateSpans_init(context):
    ts = ecmascript.ecmascript.P2_TemplateSpans(context, "StrictArg", ["child"], "TaggedArg")
    assert ts.name == "TemplateSpans"
    assert ts.context == context
    assert ts.children == ["child"]
    assert ts.Tagged == "TaggedArg"


def test_P2_TemplateSpans_TemplateTail_init(context):
    ts = ecmascript.ecmascript.P2_TemplateSpans_TemplateTail(context, "StrictArg", ["Tail"], "T")
    assert ts.name == "TemplateSpans"
    assert ts.TemplateTail == "Tail"


def test_P2_TemplateSpans_TemplateMiddleList_TemplateTail_init(context):
    ts = ecmascript.ecmascript.P2_TemplateSpans_TemplateMiddleList_TemplateTail(
        context, "StrictArg", ["MiddleList", "Tail"], "T"
    )
    assert ts.name == "TemplateSpans"
    assert ts.TemplateMiddleList == "MiddleList"
    assert ts.TemplateTail == "Tail"


class Test_parse_TemplateSpans(parse_test):
    # Syntax
    #   TemplateSpans[Yield, Await, Tagged] :
    #       TemplateTail
    #       TemplateMiddleList[?Yield, ?Await, ?Tagged] TemplateTail
    target = staticmethod(ecmascript.ecmascript.parse_TemplateSpans)
    target_argnames = ("Yield", "Await", "Tagged")
    productions = (
        (
            ("TemplateMiddleList", "TEMPLATETAIL¡bob"),
            ecmascript.ecmascript.P2_TemplateSpans_TemplateMiddleList_TemplateTail,
        ),
        (("TEMPLATETAIL¡bob",), ecmascript.ecmascript.P2_TemplateSpans_TemplateTail),
    )
    called_argnames = {
        "TemplateMiddleList": ("?Yield", "?Await", "?Tagged"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


#### TemplateMiddleList #################################################################################################################################
#
# 88888888888                                 888          888             888b     d888 d8b      888      888 888          888      d8b          888
#     888                                     888          888             8888b   d8888 Y8P      888      888 888          888      Y8P          888
#     888                                     888          888             88888b.d88888          888      888 888          888                   888
#     888      .d88b.  88888b.d88b.  88888b.  888  8888b.  888888  .d88b.  888Y88888P888 888  .d88888  .d88888 888  .d88b.  888      888 .d8888b  888888
#     888     d8P  Y8b 888 "888 "88b 888 "88b 888     "88b 888    d8P  Y8b 888 Y888P 888 888 d88" 888 d88" 888 888 d8P  Y8b 888      888 88K      888
#     888     88888888 888  888  888 888  888 888 .d888888 888    88888888 888  Y8P  888 888 888  888 888  888 888 88888888 888      888 "Y8888b. 888
#     888     Y8b.     888  888  888 888 d88P 888 888  888 Y88b.  Y8b.     888   "   888 888 Y88b 888 Y88b 888 888 Y8b.     888      888      X88 Y88b.
#     888      "Y8888  888  888  888 88888P"  888 "Y888888  "Y888  "Y8888  888       888 888  "Y88888  "Y88888 888  "Y8888  88888888 888  88888P'  "Y888
#                                    888
#                                    888
#                                    888
#
#########################################################################################################################################################
def test_P2_TemplateMiddleList_init(context):
    tml = ecmascript.ecmascript.P2_TemplateMiddleList(context, "StrictArg", ["child"], "TaggedArg")
    assert tml.name == "TemplateMiddleList"
    assert tml.context == context
    assert tml.children == ["child"]
    assert tml.Tagged == "TaggedArg"


def test_P2_TemplateMiddleList_TemplateMiddle_Expression_init(context):
    tml = ecmascript.ecmascript.P2_TemplateMiddleList_TemplateMiddle_Expression(
        context, "StrictArg", ["TemplateMiddle", "Expression"], "T"
    )
    assert tml.name == "TemplateMiddleList"
    assert tml.TemplateMiddle == "TemplateMiddle"
    assert tml.Expression == "Expression"


def test_P2_TemplateMiddleList_TemplateMiddleList_TemplateMiddle_Expression_init(context):
    tml = ecmascript.ecmascript.P2_TemplateMiddleList_TemplateMiddleList_TemplateMiddle_Expression(
        context, "StrictArg", ["TemplateMiddleList", "TemplateMiddle", "Expression"], "T"
    )
    assert tml.name == "TemplateMiddleList"
    assert tml.TemplateMiddleList == "TemplateMiddleList"
    assert tml.TemplateMiddle == "TemplateMiddle"
    assert tml.Expression == "Expression"


class Test_parse_TemplateMiddleList(parse_test):
    # Syntax
    #   TemplateMiddleList[Yield, Await, Tagged] :
    #       TemplateMiddle Expression[+In, ?Yield, ?Await]
    #       TemplateMiddleList[?Yield, ?Await, ?Tagged] TemplateMiddle Expression[+In, ?Yield, ?Await]
    target = staticmethod(ecmascript.ecmascript.parse_TemplateMiddleList)
    target_argnames = ("Yield", "Await", "Tagged")
    productions = (
        (
            ("TEMPLATEMIDDLE¡bob", "Expression"),
            ecmascript.ecmascript.P2_TemplateMiddleList_TemplateMiddle_Expression,
        ),
        (
            ("TEMPLATEMIDDLE¡bob", "Expression", "TEMPLATEMIDDLE¡alice", "Expression"),
            ecmascript.ecmascript.P2_TemplateMiddleList_TemplateMiddleList_TemplateMiddle_Expression,
        ),
    )
    called_argnames = {
        "Expression": ("+In", "?Yield", "?Await"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos)


####################################################################################
#
#  d888    .d8888b.       .d8888b.       d888        d888
# d8888   d88P  Y88b     d88P  Y88b     d8888       d8888
#   888          888            888       888         888
#   888        .d88P          .d88P       888         888
#   888    .od888P"       .od888P"        888         888
#   888   d88P"          d88P"            888         888
#   888   888"       d8b 888"       d8b   888   d8b   888
# 8888888 888888888  Y8P 888888888  Y8P 8888888 Y8P 8888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | Semantics
# 12.2.1.1 | Static Semantics: CoveredParenthesizedExpression
####################################################################################
class Test_Semantics_CoveredParenthesizedExpression:
    # CoverParenthesizedExpressionAndArrowParameterList : ( Expression )
    #   1. Return the ParenthesizedExpression that is covered by CoverParenthesizedExpressionAndArrowParameterList.
    @pytest.mark.parametrize("Await", (False, True))
    @pytest.mark.parametrize("Yield", (False, True))
    @strict_params
    def test_normal(self, context, mocker, strict, Yield, Await):
        cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_RPAREN(
            context, strict, [mocker.Mock(), mocker.Mock(), mocker.Mock()], Yield, Await
        )
        cpeaapl.covering = mocker.Mock(return_value=mocker.sentinel.return_value)
        rv = cpeaapl.CoveredParenthesizedExpression
        assert rv == mocker.sentinel.return_value
        cpeaapl.covering.assert_called_with(
            ecmascript.ecmascript.parse_ParenthesizedExpression, strict, Yield, Await
        )


####################################################################################
#
#  d888    .d8888b.       .d8888b.       d888        .d8888b.
# d8888   d88P  Y88b     d88P  Y88b     d8888       d88P  Y88b
#   888          888            888       888              888
#   888        .d88P          .d88P       888            .d88P
#   888    .od888P"       .od888P"        888        .od888P"
#   888   d88P"          d88P"            888       d88P"
#   888   888"       d8b 888"       d8b   888   d8b 888"
# 8888888 888888888  Y8P 888888888  Y8P 8888888 Y8P 888888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | Semantics
# 12.2.1.2 | Static Semantics: HasName
####################################################################################
class Test_Semantics_HasName:
    # PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
    #   1. Let expr be CoveredParenthesizedExpression of CoverParenthesizedExpressionAndArrowParameterList.
    #   2. If IsFunctionDefinition of expr is false, return false.
    #   3. Return HasName of expr.
    @pytest.mark.parametrize("ifd", (False, True))
    @pytest.mark.parametrize("hasname", (False, True))
    @strict_params
    def test_PrimaryExpression_HasName_01(self, context, mocker, ifd, hasname, strict):
        expr = mocker.Mock(**{"IsFunctionDefinition.return_value": ifd, "HasName.return_value": hasname})
        cpeaapl = mocker.Mock(**{"CoveredParenthesizedExpression": expr})
        pe = ecmascript.ecmascript.P2_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList(
            context, strict, [cpeaapl]
        )

        rv = pe.HasName()
        assert rv == (ifd and hasname)


####################################################################################
#
#  d888    .d8888b.       .d8888b.       d888        .d8888b.
# d8888   d88P  Y88b     d88P  Y88b     d8888       d88P  Y88b
#   888          888            888       888            .d88P
#   888        .d88P          .d88P       888           8888"
#   888    .od888P"       .od888P"        888            "Y8b.
#   888   d88P"          d88P"            888       888    888
#   888   888"       d8b 888"       d8b   888   d8b Y88b  d88P
# 8888888 888888888  Y8P 888888888  Y8P 8888888 Y8P  "Y8888P"
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | Semantics
# 12.2.1.3 | Static Semantics: IsFunctionDefinition
####################################################################################
class Test_Semantics_IsFunctionDefinition:
    # PrimaryExpression : this
    # PrimaryExpression : IdentifierReference
    # PrimaryExpression : Literal
    # PrimaryExpression : ArrayLiteral
    # PrimaryExpression : ObjectLiteral
    # PrimaryExpression : RegularExpressionLiteral
    # PrimaryExpression : TemplateLiteral
    #   1. Return false.
    @pytest.mark.parametrize(
        "ctor",
        (
            ecmascript.ecmascript.P2_PrimaryExpression_THIS,
            ecmascript.ecmascript.P2_PrimaryExpression_IdentifierReference,
            ecmascript.ecmascript.P2_PrimaryExpression_Literal,
            ecmascript.ecmascript.P2_PrimaryExpression_ArrayLiteral,
            ecmascript.ecmascript.P2_PrimaryExpression_ObjectLiteral,
            ecmascript.ecmascript.P2_PrimaryExpression_RegularExpressionLiteral,
            ecmascript.ecmascript.P2_PrimaryExpression_TemplateLiteral,
        ),
    )
    @strict_params
    def test_not_functions(self, context, mocker, strict, ctor):
        pe = ctor(context, strict, [mocker.Mock()])
        assert not pe.IsFunctionDefinition()

    # PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
    #   1. Let expr be CoveredParenthesizedExpression of CoverParenthesizedExpressionAndArrowParameterList.
    #   2. Return IsFunctionDefinition of expr.
    @strict_params
    def test_parenthesized_expression(self, context, mocker, strict):
        expr = mocker.Mock(**{"IsFunctionDefinition.return_value": mocker.sentinel.is_function_definition})
        cpeaapl = mocker.Mock(**{"CoveredParenthesizedExpression": expr})
        pe = ecmascript.ecmascript.P2_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList(
            context, strict, [cpeaapl]
        )

        rv = pe.IsFunctionDefinition()

        assert rv == mocker.sentinel.is_function_definition


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.       d888
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b     d8888
#   888          888            888     888              888
#   888        .d88P          .d88P     888d888b.        888
#   888    .od888P"       .od888P"      888P "Y88b       888
#   888   d88P"          d88P"          888    888       888
#   888   888"       d8b 888"       d8b Y88b  d88P d8b   888
# 8888888 888888888  Y8P 888888888  Y8P  "Y8888P"  Y8P 8888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | Object Initializer
# 12.2.6.1 | Static Semantics: Early Errors
####################################################################################
class Test_ObjectInitializer_EarlyErrors:
    # 12.2.6.1 Static Semantics: Early Errors

    # PropertyDefinition : MethodDefinition
    #   * It is a Syntax Error if HasDirectSuper of MethodDefinition is true.
    @pytest.mark.parametrize("hds, count", ((True, 1), (False, 0)))
    @strict_params
    def test_PropertyDefinition_MethodDefinition(self, context, mocker, strict, hds, count):
        md = mocker.Mock(**{"HasDirectSuper.return_value": hds})
        pd = ecmascript.ecmascript.P2_PropertyDefinition_MethodDefinition(context, strict, [md])

        rv = pd.EarlyErrors()
        expected = [Expected_Exception] * count
        actual = [type(err) for err in rv]
        assert actual == expected

    # PropertyDefinition : CoverInitializedName
    #   * Always throw a Syntax Error if code matches this production.
    @strict_params
    def test_PropertyDefinition_CoverInitializedName(self, context, mocker, strict):
        cin = mocker.Mock()
        pd = ecmascript.ecmascript.P2_PropertyDefinition_CoverInitializedName(context, strict, [cin])

        rv = pd.EarlyErrors()
        actual = [type(err) for err in rv]
        assert actual == [Expected_Exception]


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.       .d8888b.
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b     d88P  Y88b
#   888          888            888     888                   888
#   888        .d88P          .d88P     888d888b.           .d88P
#   888    .od888P"       .od888P"      888P "Y88b      .od888P"
#   888   d88P"          d88P"          888    888     d88P"
#   888   888"       d8b 888"       d8b Y88b  d88P d8b 888"
# 8888888 888888888  Y8P 888888888  Y8P  "Y8888P"  Y8P 888888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | Object Initializer
# 12.2.6.2 | Static Semantics: ComputedPropertyContains
####################################################################################
class Test_ObjectLiteral_ComputedPropertyContains:
    # 12.2.6.2 Static Semantics: ComputedPropertyContains
    #   With parameter symbol.

    # PropertyName : LiteralPropertyName
    #   1. Return false.
    @strict_params
    def test_PropertyName_LiteralPropertyName(self, context, mocker, strict):
        pn = ecmascript.ecmascript.P2_PropertyName_LiteralPropertyName(context, strict, [mocker.Mock()])
        rv = pn.ComputedPropertyContains("Nonsense")
        assert rv is False

    # PropertyName : ComputedPropertyName
    #   1. Return the result of ComputedPropertyName Contains symbol.
    @strict_params
    def test_PropertyName_ComputedPropertyName(self, context, mocker, strict):
        symbol = "Nonsense"
        cpn = mocker.Mock(**{"Contains.return_value": mocker.sentinel.return_value})
        pn = ecmascript.ecmascript.P2_PropertyName_ComputedPropertyName(context, strict, [cpn])
        rv = pn.ComputedPropertyContains(symbol)
        assert rv == mocker.sentinel.return_value
        cpn.Contains.assert_called_with(symbol)


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.       .d8888b.
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b     d88P  Y88b
#   888          888            888     888                 .d88P
#   888        .d88P          .d88P     888d888b.          8888"
#   888    .od888P"       .od888P"      888P "Y88b          "Y8b.
#   888   d88P"          d88P"          888    888     888    888
#   888   888"       d8b 888"       d8b Y88b  d88P d8b Y88b  d88P
# 8888888 888888888  Y8P 888888888  Y8P  "Y8888P"  Y8P  "Y8888P"
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | Object Initializer
# 12.2.6.3 | Static Semantics: Contains
####################################################################################
class Test_ObjectInitializer_Contains:
    # 12.2.6.3 Static Semantics: Contains
    #   With parameter symbol.

    # PropertyDefinition : MethodDefinition
    #   1. If symbol is MethodDefinition, return true.
    #   2. Return the result of ComputedPropertyContains for MethodDefinition with argument symbol.
    @strict_params
    def test_PropertyDefinition_MethodDefinition_01(self, context, mocker, strict):
        md = mocker.Mock(**{"ComputedPropertyContains.return_value": mocker.sentinel.return_value})
        pd = ecmascript.ecmascript.P2_PropertyDefinition_MethodDefinition(context, strict, [md])
        rv = pd.Contains("MethodDefinition")
        assert rv is True
        md.ComputedPropertyContains.assert_not_called()

    @strict_params
    def test_PropertyDefinition_MethodDefinition_02(self, context, mocker, strict):
        symbol = "OtherSymbol"
        md = mocker.Mock(**{"ComputedPropertyContains.return_value": mocker.sentinel.return_value})
        pd = ecmascript.ecmascript.P2_PropertyDefinition_MethodDefinition(context, strict, [md])
        rv = pd.Contains(symbol)
        assert rv == mocker.sentinel.return_value
        md.ComputedPropertyContains.assert_called_with(symbol)

    # LiteralPropertyName : IdentifierName
    #   1. If symbol is a ReservedWord, return false.
    #   2. If symbol is an Identifier and StringValue of symbol is the same value as the StringValue of IdentifierName,
    #      return true.
    #   3. Return false.
    @pytest.mark.parametrize(
        "symbol, expected",
        [pytest.param(rw, False, id=rw) for rw in lexer2.Lexer.reserved_words]
        + [("IdentifierName", True), ("bob", False),],
    )
    @strict_params
    def test_LiteralPropertyName_IdentifierName_Contains(self, context, mocker, strict, symbol, expected):
        idname = mocker.Mock(value="IdentifierName")
        lpn = ecmascript.ecmascript.P2_LiteralPropertyName_IdentifierName(context, strict, [idname])

        rv = lpn.Contains(symbol)
        assert rv == expected


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.          d8888
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b        d8P888
#   888          888            888     888              d8P 888
#   888        .d88P          .d88P     888d888b.       d8P  888
#   888    .od888P"       .od888P"      888P "Y88b     d88   888
#   888   d88P"          d88P"          888    888     8888888888
#   888   888"       d8b 888"       d8b Y88b  d88P d8b       888
# 8888888 888888888  Y8P 888888888  Y8P  "Y8888P"  Y8P       888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | Object Initializer
# 12.2.6.4 | Static Semantics: IsComputedPropertyKey
####################################################################################
class Test_ObjectInitializer_IsComputedPropertyKey:
    # 12.2.6.4 Static Semantics: IsComputedPropertyKey
    # PropertyName : LiteralPropertyName
    #   1. Return false.
    @strict_params
    def test_LiteralPropertyName(self, context, mocker, strict):
        node = ecmascript.ecmascript.P2_PropertyName_LiteralPropertyName(context, strict, [mocker.Mock()])
        assert node.IsComputedPropertyKey() is False

    # PropertyName : ComputedPropertyName
    #   1. Return true.
    @strict_params
    def test_ComputedPropertyName(self, context, mocker, strict):
        node = ecmascript.ecmascript.P2_PropertyName_ComputedPropertyName(context, strict, [mocker.Mock()])
        assert node.IsComputedPropertyKey() is True


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.      888888888
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b     888
#   888          888            888     888            888
#   888        .d88P          .d88P     888d888b.      8888888b.
#   888    .od888P"       .od888P"      888P "Y88b          "Y88b
#   888   d88P"          d88P"          888    888            888
#   888   888"       d8b 888"       d8b Y88b  d88P d8b Y88b  d88P
# 8888888 888888888  Y8P 888888888  Y8P  "Y8888P"  Y8P  "Y8888P"
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | Object Initializer
# 12.2.6.5 | Static Semantics: PropName
####################################################################################
class Test_ObjectInitializer_PropName:
    # PropertyDefinition : IdentifierReference
    #   1. Return StringValue of IdentifierReference.
    @strict_params
    def test_PropertyDefinition_IdentifierReference(self, context, mocker, strict):
        ir = mocker.Mock(StringValue=mocker.sentinel.string_value)
        pn = ecmascript.ecmascript.P2_PropertyDefinition_IdentifierReference(context, strict, [ir])
        assert pn.PropName() == mocker.sentinel.string_value

    # PropertyDefinition : ... AssignmentExpression
    #   1. Return empty.
    @strict_params
    def test_PropertyDefinition_DOTDOTDOT_AssignmentExpression(self, context, mocker, strict):
        pn = ecmascript.ecmascript.P2_PropertyDefinition_DOTDOTDOT_AssignmentExpression(
            context, strict, [mocker.Mock(), mocker.Mock()]
        )
        assert pn.PropName() == ecmascript.ecmascript.EMPTY

    # PropertyDefinition : PropertyName : AssignmentExpression
    #   1. Return PropName of PropertyName.
    @strict_params
    def test_PropertyDefinition_PropertyName_AssignmentExpression(self, context, mocker, strict):
        propname = mocker.Mock(**{"PropName.return_value": mocker.sentinel.propname})
        pn = ecmascript.ecmascript.P2_PropertyDefinition_PropertyName_COLON_AssignmentExpression(
            context, strict, [propname, mocker.Mock(), mocker.Mock()]
        )
        assert pn.PropName() == mocker.sentinel.propname

    # LiteralPropertyName : IdentifierName
    #   1. Return StringValue of IdentifierName.
    @strict_params
    def test_LiteralPropertyName_IdentifierName(self, context, mocker, strict):
        idname = mocker.Mock(value=mocker.sentinel.identifier_name)
        pn = ecmascript.ecmascript.P2_LiteralPropertyName_IdentifierName(context, strict, [idname])
        assert pn.PropName() == mocker.sentinel.identifier_name

    # LiteralPropertyName : StringLiteral
    #   1. Return the String value whose code units are the SV of the StringLiteral.
    @strict_params
    def test_LiteralPropertyName_StringLiteral(self, context, mocker, strict):
        idname = mocker.Mock(value=mocker.sentinel.identifier_name)
        pn = ecmascript.ecmascript.P2_LiteralPropertyName_StringLiteral(context, strict, [idname])
        assert pn.PropName() == mocker.sentinel.identifier_name

    # LiteralPropertyName : NumericLiteral
    #   1. Let nbr be the result of forming the value of the NumericLiteral.
    #   2. Return ! ToString(nbr).
    @strict_params
    def test_LiteralPropertyName_NumericLiteral(self, context, mocker, strict):
        numeric = mocker.Mock(value=989)
        pn = ecmascript.ecmascript.P2_LiteralPropertyName_NumericLiteral(context, strict, [numeric])
        assert pn.PropName() == "989"

    # ComputedPropertyName : [ AssignmentExpression ]
    #   1. Return empty.
    @strict_params
    def test_ComputedPropertyName_AssignmentExpression(context, mocker, strict):
        cpn = ecmascript.ecmascript.P2_ComputedPropertyName_LBRACKET_AssignmentExpression_RBRACKET(
            context, strict, [mocker.Mock(), mocker.Mock(), mocker.Mock()]
        )
        assert cpn.PropName() == ecmascript.ecmascript.EMPTY


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.       .d8888b.
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b     d88P  Y88b
#   888          888            888     888            888
#   888        .d88P          .d88P     888d888b.      888d888b.
#   888    .od888P"       .od888P"      888P "Y88b     888P "Y88b
#   888   d88P"          d88P"          888    888     888    888
#   888   888"       d8b 888"       d8b Y88b  d88P d8b Y88b  d88P
# 8888888 888888888  Y8P 888888888  Y8P  "Y8888P"  Y8P  "Y8888P"
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | Object Initializer
# 12.2.6.6 | Static Semantics: PropertyNameList
####################################################################################
class Test_ObjectInitializer_PropertyNameList:
    # PropertyDefinitionList : PropertyDefinition
    #   1. If PropName of PropertyDefinition is empty, return a new empty List.
    #   2. Return a new List containing PropName of PropertyDefinition.
    @strict_params
    def test_empty_definition(self, context, mocker, strict):
        pd = mocker.Mock(**{"PropName.return_value": ecmascript.ecmascript.EMPTY})
        pn = ecmascript.ecmascript.P2_PropertyDefinitionList_PropertyDefinition(context, strict, [pd])
        assert pn.PropertyNameList() == []

    @strict_params
    def test_single_definition(self, context, mocker, strict):
        pd = mocker.Mock(**{"PropName.return_value": mocker.sentinel.propname})
        pn = ecmascript.ecmascript.P2_PropertyDefinitionList_PropertyDefinition(context, strict, [pd])
        assert pn.PropertyNameList() == [mocker.sentinel.propname]

    # PropertyDefinitionList : PropertyDefinitionList , PropertyDefinition
    #   1. Let list be PropertyNameList of PropertyDefinitionList.
    #   2. If PropName of PropertyDefinition is empty, return list.
    #   3. Append PropName of PropertyDefinition to the end of list.
    #   4. Return list.
    @strict_params
    def test_list_plus_empty(self, context, mocker, strict):
        pdl = mocker.Mock(**{"PropertyNameList.return_value": [mocker.sentinel.previous]})
        pd = mocker.Mock(**{"PropName.return_value": ecmascript.ecmascript.EMPTY})
        pn = ecmascript.ecmascript.P2_PropertyDefinitionList_PropertyDefinitionList_COMMA_PropertyDefinition(
            context, strict, [pdl, mocker.Mock(), pd]
        )
        assert pn.PropertyNameList() == [mocker.sentinel.previous]

    @strict_params
    def test_list_plus_something(self, context, mocker, strict):
        pdl = mocker.Mock(**{"PropertyNameList.return_value": [mocker.sentinel.previous]})
        pd = mocker.Mock(**{"PropName.return_value": mocker.sentinel.added})
        pn = ecmascript.ecmascript.P2_PropertyDefinitionList_PropertyDefinitionList_COMMA_PropertyDefinition(
            context, strict, [pdl, mocker.Mock(), pd]
        )
        assert pn.PropertyNameList() == [mocker.sentinel.previous, mocker.sentinel.added]


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.      8888888888
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b           d88P
#   888          888            888     888                 d88P
#   888        .d88P          .d88P     888d888b.          d88P
#   888    .od888P"       .od888P"      888P "Y88b      88888888
#   888   d88P"          d88P"          888    888       d88P
#   888   888"       d8b 888"       d8b Y88b  d88P d8b  d88P
# 8888888 888888888  Y8P 888888888  Y8P  "Y8888P"  Y8P d88P
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | Object Initializer
# 12.2.6.7 | Runtime Semantics: Evaluation
####################################################################################
class Test_ObjectInitializer_Evaluation:
    # ObjectLiteral : { }
    #   1. Return ObjectCreate(%ObjectPrototype%).
    @strict_params
    def test_ObjectLiteral_empty(self, context, mocker, realm, strict):
        oc = mocker.patch("ecmascript.ecmascript.ObjectCreate", return_value=mocker.sentinel.object)
        ol = ecmascript.ecmascript.P2_ObjectLiteral_LCURLY_RCURLY(context, strict, [mocker.Mock(), mocker.Mock()])
        rv = ol.evaluate()
        assert rv == mocker.sentinel.object
        oc.assert_called_with(realm.intrinsics["%ObjectPrototype%"])

    # ObjectLiteral : { PropertyDefinitionList }
    # ObjectLiteral : { PropertyDefinitionList , }
    #   1. Let obj be ObjectCreate(%ObjectPrototype%).
    #   2. Perform ? PropertyDefinitionEvaluation of PropertyDefinitionList with arguments obj and true.
    #   3. Return obj.
    @pytest.mark.parametrize(
        "ctor",
        (
            ecmascript.ecmascript.P2_ObjectLiteral_LCURLY_PropertyDefinitionList_RCURLY,
            ecmascript.ecmascript.P2_ObjectLiteral_LCURLY_PropertyDefinitionList_COMMA_RCURLY,
        ),
    )
    @strict_params
    def test_ObjectLiteral_PDL(self, context, mocker, realm, strict, ctor):
        pdl = mocker.Mock(**{"PropertyDefinitionEvaluation.return_value": mocker.sentinel.pd_eval})
        oc = mocker.patch("ecmascript.ecmascript.ObjectCreate", return_value=mocker.sentinel.object)
        ol = ctor(context, strict, [mocker.Mock(), pdl, mocker.Mock()])
        rv = ol.evaluate()
        assert rv == mocker.sentinel.object
        oc.assert_called_with(realm.intrinsics["%ObjectPrototype%"])
        pdl.PropertyDefinitionEvaluation.assert_called_with(mocker.sentinel.object, True)

    # LiteralPropertyName : IdentifierName
    #   1. Return StringValue of IdentifierName.
    @strict_params
    def test_IdentifierName(self, context, mocker, strict):
        idname = mocker.Mock(value=mocker.sentinel.identifier_name)
        lpn = ecmascript.ecmascript.P2_LiteralPropertyName_IdentifierName(context, strict, [idname])
        assert lpn.evaluate() == mocker.sentinel.identifier_name

    # LiteralPropertyName : StringLiteral
    #   1. Return the String value whose code units are the SV of the StringLiteral.
    @strict_params
    def test_StringLiteral(self, context, mocker, strict):
        literal = mocker.Mock(value=mocker.sentinel.string_literal)
        lpn = ecmascript.ecmascript.P2_LiteralPropertyName_StringLiteral(context, strict, [literal])
        assert lpn.evaluate() == mocker.sentinel.string_literal

    # LiteralPropertyName : NumericLiteral
    #   1. Let nbr be the result of forming the value of the NumericLiteral.
    #   2. Return ! ToString(nbr).
    @strict_params
    def test_NumericLiteral(self, context, mocker, strict):
        literal = mocker.Mock(value=989)
        lpn = ecmascript.ecmascript.P2_LiteralPropertyName_NumericLiteral(context, strict, [literal])
        assert lpn.evaluate() == "989"

    # ComputedPropertyName : [ AssignmentExpression ]
    #   1. Let exprValue be the result of evaluating AssignmentExpression.
    #   2. Let propName be ? GetValue(exprValue).
    #   3. Return ? ToPropertyKey(propName).
    @strict_params
    def test_ComputedPropertyName(self, context, mocker, strict):
        ae = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.expr_value})
        gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value=mocker.sentinel.prop_name)
        tpk = mocker.patch("ecmascript.ecmascript.ToPropertyKey", return_value=mocker.sentinel.return_value)
        cpn = ecmascript.ecmascript.P2_ComputedPropertyName_LBRACKET_AssignmentExpression_RBRACKET(
            context, strict, [mocker.Mock(), ae, mocker.Mock()]
        )
        assert cpn.evaluate() == mocker.sentinel.return_value
        ae.evaluate.assert_called_with()
        gv.assert_called_with(mocker.sentinel.expr_value)
        tpk.assert_called_with(mocker.sentinel.prop_name)


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.       .d8888b.
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b     d88P  Y88b
#   888          888            888     888            Y88b. d88P
#   888        .d88P          .d88P     888d888b.       "Y88888"
#   888    .od888P"       .od888P"      888P "Y88b     .d8P""Y8b.
#   888   d88P"          d88P"          888    888     888    888
#   888   888"       d8b 888"       d8b Y88b  d88P d8b Y88b  d88P
# 8888888 888888888  Y8P 888888888  Y8P  "Y8888P"  Y8P  "Y8888P"
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | Object Initializer
# 12.2.6.8 | Runtime Semantics: PropertyDefinitionEvaluation
####################################################################################
class Test_ObjectInitializer_PropertyDefinitionEvaluation:
    # 12.2.6.8 Runtime Semantics: PropertyDefinitionEvaluation
    # With parameters object and enumerable.

    # PropertyDefinitionList : PropertyDefinitionList , PropertyDefinition
    #   1. Perform ? PropertyDefinitionEvaluation of PropertyDefinitionList with arguments object and enumerable.
    #   2. Return the result of performing PropertyDefinitionEvaluation of PropertyDefinition with arguments object
    #      and enumerable.
    @strict_params
    def test_pdl_com_pd(self, context, mocker, strict):
        child_pdl = mocker.Mock(**{"PropertyDefinitionEvaluation.return_value": None})
        pd = mocker.Mock(**{"PropertyDefinitionEvaluation.return_value": mocker.sentinel.return_value})
        pdl = ecmascript.ecmascript.P2_PropertyDefinitionList_PropertyDefinitionList_COMMA_PropertyDefinition(
            context, strict, [child_pdl, mocker.Mock(), pd]
        )
        assert (
            pdl.PropertyDefinitionEvaluation(mocker.sentinel.object, mocker.sentinel.enumerable)
            == mocker.sentinel.return_value
        )
        child_pdl.PropertyDefinitionEvaluation.assert_called_with(mocker.sentinel.object, mocker.sentinel.enumerable)
        pd.PropertyDefinitionEvaluation.assert_called_with(mocker.sentinel.object, mocker.sentinel.enumerable)

    # PropertyDefinition : ... AssignmentExpression
    #   1. Let exprValue be the result of evaluating AssignmentExpression.
    #   2. Let fromValue be ? GetValue(exprValue).
    #   3. Let excludedNames be a new empty List.
    #   4. Return ? CopyDataProperties(object, fromValue, excludedNames).
    @strict_params
    def test_dots_ae(self, context, mocker, strict):
        ae = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.expr_value})
        gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value=mocker.sentinel.from_value)
        cdp = mocker.patch("ecmascript.ecmascript.CopyDataProperties", return_value=mocker.sentinel.return_value)
        pd = ecmascript.ecmascript.P2_PropertyDefinition_DOTDOTDOT_AssignmentExpression(
            context, strict, [mocker.Mock(), ae, mocker.Mock()]
        )
        assert (
            pd.PropertyDefinitionEvaluation(mocker.sentinel.object, mocker.sentinel.enumerable)
            == mocker.sentinel.return_value
        )
        ae.evaluate.assert_called_with()
        gv.assert_called_with(mocker.sentinel.expr_value)
        cdp.assert_called_with(mocker.sentinel.object, mocker.sentinel.from_value, [])

    # PropertyDefinition : IdentifierReference
    #   1. Let propName be StringValue of IdentifierReference.
    #   2. Let exprValue be the result of evaluating IdentifierReference.
    #   3. Let propValue be ? GetValue(exprValue).
    #   4. Assert: enumerable is true.
    #   5. Assert: object is an ordinary, extensible object with no non-configurable properties.
    #   6. Return ! CreateDataPropertyOrThrow(object, propName, propValue).
    @strict_params
    def test_IdentifierReference(self, context, mocker, strict):
        ir = mocker.Mock(
            **{"StringValue": mocker.sentinel.prop_name, "evaluate.return_value": mocker.sentinel.expr_value}
        )
        gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value=mocker.sentinel.prop_value)
        cdp = mocker.patch(
            "ecmascript.ecmascript.CreateDataPropertyOrThrow", return_value=mocker.sentinel.return_value
        )
        pd = ecmascript.ecmascript.P2_PropertyDefinition_IdentifierReference(context, strict, [ir])
        assert pd.PropertyDefinitionEvaluation(mocker.sentinel.object, True) == mocker.sentinel.return_value
        ir.evaluate.assert_called_with()
        gv.assert_called_with(mocker.sentinel.expr_value)
        cdp.assert_called_with(mocker.sentinel.object, mocker.sentinel.prop_name, mocker.sentinel.prop_value)

    # PropertyDefinition : PropertyName : AssignmentExpression
    #   1. Let propKey be the result of evaluating PropertyName.
    #   2. ReturnIfAbrupt(propKey).
    #   3. If IsAnonymousFunctionDefinition(AssignmentExpression) is true, then
    #       a. Let propValue be the result of performing NamedEvaluation for AssignmentExpression with argument propKey.
    #   4. Else,
    #       a. Let exprValueRef be the result of evaluating AssignmentExpression.
    #       b. Let propValue be ? GetValue(exprValueRef).
    #   5. Assert: enumerable is true.
    #   6. Assert: object is an ordinary, extensible object with no non-configurable properties.
    #   7. Return ! CreateDataPropertyOrThrow(object, propKey, propValue).
    @strict_params
    def test_propname_ae_anonymous(self, context, mocker, strict):
        pn = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.prop_key})
        afd = mocker.patch("ecmascript.ecmascript.IsAnonymousFunctionDefinition", return_value=True)
        ae = mocker.Mock(**{"NamedEvaluation.return_value": mocker.sentinel.prop_value})
        cdp = mocker.patch(
            "ecmascript.ecmascript.CreateDataPropertyOrThrow", return_value=mocker.sentinel.return_value
        )
        pd = ecmascript.ecmascript.P2_PropertyDefinition_PropertyName_COLON_AssignmentExpression(
            context, strict, [pn, mocker.Mock(), ae]
        )
        assert pd.PropertyDefinitionEvaluation(mocker.sentinel.object, True) == mocker.sentinel.return_value
        pn.evaluate.assert_called_with()
        afd.assert_called_with(ae)
        ae.NamedEvaluation.assert_called_with(mocker.sentinel.prop_key)
        cdp.assert_called_with(mocker.sentinel.object, mocker.sentinel.prop_key, mocker.sentinel.prop_value)

    @strict_params
    def test_propname_ae_named(self, context, mocker, strict):
        pn = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.prop_key})
        afd = mocker.patch("ecmascript.ecmascript.IsAnonymousFunctionDefinition", return_value=False)
        ae = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.expr_value_ref})
        gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value=mocker.sentinel.prop_value)
        cdp = mocker.patch(
            "ecmascript.ecmascript.CreateDataPropertyOrThrow", return_value=mocker.sentinel.return_value
        )
        pd = ecmascript.ecmascript.P2_PropertyDefinition_PropertyName_COLON_AssignmentExpression(
            context, strict, [pn, mocker.Mock(), ae]
        )
        assert pd.PropertyDefinitionEvaluation(mocker.sentinel.object, True) == mocker.sentinel.return_value
        pn.evaluate.assert_called_with()
        afd.assert_called_with(ae)
        ae.evaluate.assert_called_with()
        gv.assert_called_with(mocker.sentinel.expr_value_ref)
        cdp.assert_called_with(mocker.sentinel.object, mocker.sentinel.prop_key, mocker.sentinel.prop_value)


#####################################################################################################################
#
# 8888888           .d8888b.  888            d8b                   888      d8b 888                              888
#   888            d88P  Y88b 888            Y8P                   888      Y8P 888                              888
#   888            Y88b.      888                                  888          888                              888
#   888   .d8888b   "Y888b.   888888 888d888 888 88888b.   .d88b.  888      888 888888  .d88b.  888d888  8888b.  888
#   888   88K          "Y88b. 888    888P"   888 888 "88b d88P"88b 888      888 888    d8P  Y8b 888P"       "88b 888
#   888   "Y8888b.       "888 888    888     888 888  888 888  888 888      888 888    88888888 888     .d888888 888
#   888        X88 Y88b  d88P Y88b.  888     888 888  888 Y88b 888 888      888 Y88b.  Y8b.     888     888  888 888
# 8888888  88888P'  "Y8888P"   "Y888 888     888 888  888  "Y88888 88888888 888  "Y888  "Y8888  888     "Y888888 888
#                                                              888
#                                                         Y8b d88P
#                                                          "Y88P"
#
#####################################################################################################################
class Test_PrimaryExpression_IsStringLiteral:
    @strict_params
    def test_PE_Literal(self, context, mocker, strict):
        literal = mocker.Mock(IsStringLiteral=mocker.sentinel.is_string_literal)
        pe = ecmascript.ecmascript.P2_PrimaryExpression_Literal(context, strict, [literal])
        assert pe.IsStringLiteral == mocker.sentinel.is_string_literal


#####################################################################################################################
#
# 888    888                   888     888                    .d8888b.  888            d8b          888
# 888    888                   888     888                   d88P  Y88b 888            Y8P          888
# 888    888                   888     888                   Y88b.      888                         888
# 8888888888  8888b.  .d8888b  888     888 .d8888b   .d88b.   "Y888b.   888888 888d888 888  .d8888b 888888
# 888    888     "88b 88K      888     888 88K      d8P  Y8b     "Y88b. 888    888P"   888 d88P"    888
# 888    888 .d888888 "Y8888b. 888     888 "Y8888b. 88888888       "888 888    888     888 888      888
# 888    888 888  888      X88 Y88b. .d88P      X88 Y8b.     Y88b  d88P Y88b.  888     888 Y88b.    Y88b.
# 888    888 "Y888888  88888P'  "Y88888P"   88888P'  "Y8888   "Y8888P"   "Y888 888     888  "Y8888P  "Y888
#
#
#
#
#####################################################################################################################
class Test_HasUseStrict:
    @strict_params
    def test_PrimaryExpression_Literal(self, context, mocker, strict):
        literal = mocker.Mock(HasUseStrict=mocker.sentinel.has_use_strict)
        pe = ecmascript.ecmascript.P2_PrimaryExpression_Literal(context, strict, [literal])
        assert pe.HasUseStrict == mocker.sentinel.has_use_strict

    @pytest.mark.parametrize(
        "test, expected",
        (('"bob"', False), ('""', False), ('"use strict"', True), ("'use strict'", True), ("'use strict\"", False)),
    )
    @strict_params
    def test_Literal_StringLiteral(self, context, mocker, strict, test, expected):
        sl = mocker.Mock(src=test, span=(0, len(test)))
        lit = ecmascript.ecmascript.P2_Literal_StringLiteral(context, strict, [sl])
        assert lit.HasUseStrict is expected


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.       d888
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b     d8888
#   888          888            888     Y88b. d88P       888
#   888        .d88P          .d88P      "Y88888"        888
#   888    .od888P"       .od888P"      .d8P""Y8b.       888
#   888   d88P"          d88P"          888    888       888
#   888   888"       d8b 888"       d8b Y88b  d88P d8b   888
# 8888888 888888888  Y8P 888888888  Y8P  "Y8888P"  Y8P 8888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | Regular Expression Literals
# 12.2.8.1 | Static Semantics: Early Errors
####################################################################################
class Test_RegularExpressionLiterals_EarlyErrors:
    # PrimaryExpression : RegularExpressionLiteral
    #   * It is a Syntax Error if BodyText of RegularExpressionLiteral cannot be recognized using the goal symbol
    #     Pattern of the ECMAScript RegExp grammar specified in 21.2.1.
    #   * It is a Syntax Error if FlagText of RegularExpressionLiteral contains any code points other than "g", "i",
    #     "m", "s", "u", or "y", or if it contains the same code point more than once.
    @strict_params
    def test_bad_regex(self, context, mocker, strict):
        matcher = mocker.patch("ecmascript.e262_regexp.parse_Pattern", return_value=None)
        rel = mocker.Mock(value=mocker.Mock(body=mocker.sentinel.body, flags=""))
        pe = ecmascript.ecmascript.P2_PrimaryExpression_RegularExpressionLiteral(context, strict, [rel])
        rv = pe.EarlyErrors()
        assert [type(err) for err in rv] == [Expected_Exception]
        assert rv[0].args[0] == "Bad Regular Expression"
        matcher.assert_called_with(mocker.sentinel.body, 0, False, False)

    @pytest.mark.parametrize("badflags", ("mss", "8"))
    @strict_params
    def test_bad_flags(self, context, mocker, strict, badflags):
        matcher = mocker.patch("ecmascript.e262_regexp.parse_Pattern", return_value=mocker.sentinel.match)
        rel = mocker.Mock(value=mocker.Mock(body=mocker.sentinel.body, flags=badflags))
        pe = ecmascript.ecmascript.P2_PrimaryExpression_RegularExpressionLiteral(context, strict, [rel])
        rv = pe.EarlyErrors()
        assert [type(err) for err in rv] == [Expected_Exception]
        assert rv[0].args[0] == "Bad Regular Expression Flags"
        matcher.assert_called_with(mocker.sentinel.body, 0, False, False)

    @strict_params
    def test_is_ok(self, context, mocker, strict):
        matcher = mocker.patch("ecmascript.e262_regexp.parse_Pattern", return_value=mocker.sentinel.match)
        rel = mocker.Mock(value=mocker.Mock(body=mocker.sentinel.body, flags="gm"))
        pe = ecmascript.ecmascript.P2_PrimaryExpression_RegularExpressionLiteral(context, strict, [rel])
        rv = pe.EarlyErrors()
        assert rv == []
        matcher.assert_called_with(mocker.sentinel.body, 0, False, False)


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.       .d8888b.
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b     d88P  Y88b
#   888          888            888     Y88b. d88P            888
#   888        .d88P          .d88P      "Y88888"           .d88P
#   888    .od888P"       .od888P"      .d8P""Y8b.      .od888P"
#   888   d88P"          d88P"          888    888     d88P"
#   888   888"       d8b 888"       d8b Y88b  d88P d8b 888"
# 8888888 888888888  Y8P 888888888  Y8P  "Y8888P"  Y8P 888888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | Regular Expression Literals
# 12.2.8.2 | Runtime Semantics: Evaluation
####################################################################################
class Test_RegularExpressionLiterals_Evaluation:
    # PrimaryExpression : RegularExpressionLiteral
    #   1. Let pattern be the String value consisting of the UTF16Encoding of each code point of BodyText of
    #      RegularExpressionLiteral.
    #   2. Let flags be the String value consisting of the UTF16Encoding of each code point of FlagText of
    #      RegularExpressionLiteral.
    #   3. Return RegExpCreate(pattern, flags).
    @strict_params
    def test_normal(self, context, mocker, strict):
        rec = mocker.patch("ecmascript.ecmascript.RegExpCreate", return_value=mocker.sentinel.return_value)
        rel = mocker.Mock(value=mocker.Mock(body=mocker.sentinel.body, flags=mocker.sentinel.flags))
        pe = ecmascript.ecmascript.P2_PrimaryExpression_RegularExpressionLiteral(context, strict, [rel])
        assert pe.evaluate() == mocker.sentinel.return_value
        rec.assert_called_with(mocker.sentinel.body, mocker.sentinel.flags)


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.       d888
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b     d8888
#   888          888            888     888    888       888
#   888        .d88P          .d88P     Y88b. d888       888
#   888    .od888P"       .od888P"       "Y888P888       888
#   888   d88P"          d88P"                 888       888
#   888   888"       d8b 888"       d8b Y88b  d88P d8b   888
# 8888888 888888888  Y8P 888888888  Y8P  "Y8888P"  Y8P 8888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | Template Literals
# 12.2.9.1 | Static Semantics: Early Errors
####################################################################################
class Test_TemplateLiterals_EarlyErrors:
    # TemplateLiteral : NoSubstitutionTemplate
    #   * It is a Syntax Error if the [Tagged] parameter was not set and NoSubstitutionTemplate Contains
    #     NotEscapeSequence.
    @pytest.mark.parametrize("Tagged", (False, True))
    @pytest.mark.parametrize("nes_present", (False, True))
    @strict_params
    def test_TemplateLiteral_NoSubstitutionTemplate(self, context, mocker, strict, Tagged, nes_present):
        # So, a NotEscapeSequence results in an empty .tv field of the template structure after lexing. That's all
        # the error detector code actually looks at.
        template = mocker.Mock(
            value=mocker.Mock(tv=None if nes_present else mocker.sentinel.tv, trv=mocker.sentinel.trv)
        )
        tl = ecmascript.ecmascript.P2_TemplateLiteral_NoSubstitutionTemplate(context, strict, [template], Tagged)
        errs = tl.EarlyErrors()
        expected_count = 1 if nes_present and not Tagged else 0
        assert len(errs) == expected_count

    # TemplateLiteral : SubstitutionTemplate
    #   * It is a Syntax Error if the number of elements in the result of TemplateStrings of TemplateLiteral with
    #     argument false is greater than 2^32-1.
    @pytest.mark.parametrize("numelem", (1, 2 ** 32 - 1, 2 ** 32))
    @pytest.mark.parametrize("Tagged", (False, True))
    @strict_params
    def test_TemplateLiteral_SubstitutionTemplate(self, context, mocker, strict, numelem, Tagged):
        tl = ecmascript.ecmascript.P2_TemplateLiteral_SubstitutionTemplate(context, strict, [mocker.Mock()], Tagged)
        tl.TemplateStrings = mocker.Mock(return_value=mocker.Mock(__len__=mocker.Mock(return_value=numelem)))
        errs = tl.EarlyErrors()
        expected_count = 1 if numelem > 2 ** 32 - 1 else 0
        assert len(errs) == expected_count
        tl.TemplateStrings.assert_called_with(False)

    # SubstitutionTemplate : TemplateHead Expression TemplateSpans
    #   * It is a Syntax Error if the [Tagged] parameter was not set and TemplateHead Contains NotEscapeSequence.
    @pytest.mark.parametrize("Tagged", (False, True))
    @pytest.mark.parametrize("nes_present", (False, True))
    @strict_params
    def test_SubstitutionTemplate_TH_Exp_TS(self, context, mocker, strict, Tagged, nes_present):
        th = mocker.Mock(value=mocker.Mock(tv=None if nes_present else mocker.sentinel.tv, trv=mocker.sentinel.trv))
        st = ecmascript.ecmascript.P2_SubstitutionTemplate_TemplateHead_Expression_TemplateSpans(
            context, strict, [th, mocker.Mock(), mocker.Mock()], Tagged
        )
        errs = st.EarlyErrors()
        expected_count = 1 if nes_present and not Tagged else 0
        assert len(errs) == expected_count

    # TemplateSpans : TemplateTail
    #   * It is a Syntax Error if the [Tagged] parameter was not set and TemplateTail Contains NotEscapeSequence.
    @pytest.mark.parametrize("Tagged", (False, True))
    @pytest.mark.parametrize("nes_present", (False, True))
    @strict_params
    def test_TemplateSpans_TT(self, context, mocker, strict, Tagged, nes_present):
        tt = mocker.Mock(value=mocker.Mock(tv=None if nes_present else mocker.sentinel.tv, trv=mocker.sentinel.trv))
        ts = ecmascript.ecmascript.P2_TemplateSpans_TemplateTail(context, strict, [tt], Tagged)
        errs = ts.EarlyErrors()
        expected_count = 1 if nes_present and not Tagged else 0
        assert len(errs) == expected_count

    # TemplateMiddleList : TemplateMiddle Expression
    # TemplateMiddleList : TemplateMiddleList TemplateMiddle Expression
    #   * It is a Syntax Error if the [Tagged] parameter was not set and TemplateMiddle Contains NotEscapeSequence.
    @pytest.mark.parametrize("Tagged", (False, True))
    @pytest.mark.parametrize("nes_present", (False, True))
    @strict_params
    def test_TemplateMiddleList_TM_Exp(self, context, mocker, strict, Tagged, nes_present):
        tm = mocker.Mock(value=mocker.Mock(tv=None if nes_present else mocker.sentinel.tv, trv=mocker.sentinel.trv))
        tml = ecmascript.ecmascript.P2_TemplateMiddleList_TemplateMiddle_Expression(
            context, strict, [tm, mocker.Mock()], Tagged
        )
        errs = tml.EarlyErrors()
        expected_count = 1 if nes_present and not Tagged else 0
        assert len(errs) == expected_count

    @pytest.mark.parametrize("Tagged", (False, True))
    @pytest.mark.parametrize("nes_present", (False, True))
    @strict_params
    def test_TemplateMiddleList_TML_TM_Exp(self, context, mocker, strict, Tagged, nes_present):
        tm = mocker.Mock(value=mocker.Mock(tv=None if nes_present else mocker.sentinel.tv, trv=mocker.sentinel.trv))
        tml = ecmascript.ecmascript.P2_TemplateMiddleList_TemplateMiddleList_TemplateMiddle_Expression(
            context, strict, [mocker.Mock(), tm, mocker.Mock()], Tagged
        )
        errs = tml.EarlyErrors()
        expected_count = 1 if nes_present and not Tagged else 0
        assert len(errs) == expected_count


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.       .d8888b.
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b     d88P  Y88b
#   888          888            888     888    888            888
#   888        .d88P          .d88P     Y88b. d888          .d88P
#   888    .od888P"       .od888P"       "Y888P888      .od888P"
#   888   d88P"          d88P"                 888     d88P"
#   888   888"       d8b 888"       d8b Y88b  d88P d8b 888"
# 8888888 888888888  Y8P 888888888  Y8P  "Y8888P"  Y8P 888888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | Template Literals
# 12.2.9.2 | Static Semantics: TemplateStrings
####################################################################################
class Test_TemplateLiterals_TemplateStrings:
    # 12.2.9.2 Static Semantics: TemplateStrings
    #   With parameter raw.

    # TemplateLiteral : NoSubstitutionTemplate
    #   1. If raw is false, then
    #       a. Let string be the TV of NoSubstitutionTemplate.
    #   2. Else,
    #       a. Let string be the TRV of NoSubstitutionTemplate.
    #   3. Return a List containing the single element, string.
    @pytest.mark.parametrize("raw", (False, True))
    @pytest.mark.parametrize("Tagged", (False, True))
    @strict_params
    def test_NoSubstitutionTemplate(self, context, mocker, strict, raw, Tagged):
        nst = mocker.Mock(value=mocker.Mock(tv=mocker.sentinel.tv, trv=mocker.sentinel.trv))
        tl = ecmascript.ecmascript.P2_TemplateLiteral_NoSubstitutionTemplate(context, strict, [nst], Tagged)
        ts = tl.TemplateStrings(raw)
        expected = [mocker.sentinel.trv if raw else mocker.sentinel.tv]
        assert ts == expected

    # SubstitutionTemplate : TemplateHead Expression TemplateSpans
    #   1. If raw is false, then
    #       a. Let head be the TV of TemplateHead.
    #   2. Else,
    #       a. Let head be the TRV of TemplateHead.
    #   3. Let tail be TemplateStrings of TemplateSpans with argument raw.
    #   4. Return a List containing head followed by the elements, in order, of tail.
    @pytest.mark.parametrize("raw", (False, True))
    @pytest.mark.parametrize("Tagged", (False, True))
    @strict_params
    def test_SubstitutionTemplate(self, context, mocker, strict, raw, Tagged):
        th = mocker.Mock(value=mocker.Mock(tv=mocker.sentinel.tv, trv=mocker.sentinel.trv))
        exp = mocker.Mock()
        spans = mocker.Mock(**{"TemplateStrings.return_value": [mocker.sentinel.spans]})
        st = ecmascript.ecmascript.P2_SubstitutionTemplate_TemplateHead_Expression_TemplateSpans(
            context, strict, [th, exp, spans], Tagged
        )
        expected = [mocker.sentinel.trv if raw else mocker.sentinel.tv, mocker.sentinel.spans]
        ts = st.TemplateStrings(raw)
        assert ts == expected
        spans.TemplateStrings.assert_called_with(raw)

    # TemplateSpans : TemplateTail
    #   1. If raw is false, then
    #       a. Let tail be the TV of TemplateTail.
    #   2. Else,
    #       a. Let tail be the TRV of TemplateTail.
    #   3. Return a List containing the single element, tail.
    @pytest.mark.parametrize("raw", (False, True))
    @pytest.mark.parametrize("Tagged", (False, True))
    @strict_params
    def test_TemplateTail(self, context, mocker, strict, raw, Tagged):
        tt = mocker.Mock(value=mocker.Mock(tv=mocker.sentinel.tv, trv=mocker.sentinel.trv))
        spans = ecmascript.ecmascript.P2_TemplateSpans_TemplateTail(context, strict, [tt], Tagged)
        expected = [mocker.sentinel.trv if raw else mocker.sentinel.tv]
        ts = spans.TemplateStrings(raw)
        assert ts == expected

    # TemplateSpans : TemplateMiddleList TemplateTail
    #   1. Let middle be TemplateStrings of TemplateMiddleList with argument raw.
    #   2. If raw is false, then
    #       a. Let tail be the TV of TemplateTail.
    #   3. Else,
    #       a. Let tail be the TRV of TemplateTail.
    #   4. Return a List containing the elements, in order, of middle followed by tail.
    @pytest.mark.parametrize("raw", (False, True))
    @pytest.mark.parametrize("Tagged", (False, True))
    @strict_params
    def test_Spans_MiddleList_Tail(self, context, mocker, strict, raw, Tagged):
        tml = mocker.Mock(**{"TemplateStrings.return_value": [mocker.sentinel.middle]})
        tt = mocker.Mock(value=mocker.Mock(tv=mocker.sentinel.tv, trv=mocker.sentinel.trv))
        spans = ecmascript.ecmascript.P2_TemplateSpans_TemplateMiddleList_TemplateTail(
            context, strict, [tml, tt], Tagged
        )
        expected = [mocker.sentinel.middle, mocker.sentinel.trv if raw else mocker.sentinel.tv]
        ts = spans.TemplateStrings(raw)
        assert ts == expected
        tml.TemplateStrings.assert_called_with(raw)

    # TemplateMiddleList : TemplateMiddle Expression
    #   1. If raw is false, then
    #       a. Let string be the TV of TemplateMiddle.
    #   2. Else,
    #       a. Let string be the TRV of TemplateMiddle.
    #   3. Return a List containing the single element, string.
    @pytest.mark.parametrize("raw", (False, True))
    @pytest.mark.parametrize("Tagged", (False, True))
    @strict_params
    def test_MiddleList_Middle_Exp(self, context, mocker, strict, raw, Tagged):
        exp = mocker.Mock()
        tmid = mocker.Mock(value=mocker.Mock(tv=mocker.sentinel.tv, trv=mocker.sentinel.trv))
        tml = ecmascript.ecmascript.P2_TemplateMiddleList_TemplateMiddle_Expression(
            context, strict, [tmid, exp], Tagged
        )
        expected = [mocker.sentinel.trv if raw else mocker.sentinel.tv]
        ts = tml.TemplateStrings(raw)
        assert ts == expected

    # TemplateMiddleList : TemplateMiddleList TemplateMiddle Expression
    #   1. Let front be TemplateStrings of TemplateMiddleList with argument raw.
    #   2. If raw is false, then
    #       a. Let last be the TV of TemplateMiddle.
    #   3. Else,
    #       a. Let last be the TRV of TemplateMiddle.
    #   4. Append last as the last element of the List front.
    #   5. Return front.
    @pytest.mark.parametrize("raw", (False, True))
    @pytest.mark.parametrize("Tagged", (False, True))
    @strict_params
    def test_MiddleList_MiddleList_Middle(self, context, mocker, strict, raw, Tagged):
        tml_child = mocker.Mock(**{"TemplateStrings.return_value": [mocker.sentinel.front]})
        tmid = mocker.Mock(value=mocker.Mock(tv=mocker.sentinel.tv, trv=mocker.sentinel.trv))
        tml = ecmascript.ecmascript.P2_TemplateMiddleList_TemplateMiddleList_TemplateMiddle_Expression(
            context, strict, [tml_child, tmid, mocker.Mock()], Tagged
        )
        expected = [mocker.sentinel.front, mocker.sentinel.trv if raw else mocker.sentinel.tv]
        ts = tml.TemplateStrings(raw)
        assert ts == expected
        tml_child.TemplateStrings.assert_called_with(raw)


####################################################################################
#
#  d888    .d8888b.       .d8888b.       d888    .d8888b.       d888
# d8888   d88P  Y88b     d88P  Y88b     d8888   d88P  Y88b     d8888
#   888          888            888       888   888    888       888
#   888        .d88P          .d88P       888   888    888       888
#   888    .od888P"       .od888P"        888   888    888       888
#   888   d88P"          d88P"            888   888    888       888
#   888   888"       d8b 888"       d8b   888   Y88b  d88P d8b   888
# 8888888 888888888  Y8P 888888888  Y8P 8888888  "Y8888P"  Y8P 8888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | The Grouping Operator
# 12.2.10.1 | Static Semantics: Early Errors
####################################################################################
class Test_GroupingOperator_EarlyErrors:
    # 12.2.10.1 Static Semantics: Early Errors
    # PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
    #   * It is a Syntax Error if CoverParenthesizedExpressionAndArrowParameterList is not covering a
    #     ParenthesizedExpression.
    #   * All Early Error rules for ParenthesizedExpression and its derived productions also apply to
    #     CoveredParenthesizedExpression of CoverParenthesizedExpressionAndArrowParameterList.
    @pytest.mark.parametrize("covered", (False, True))
    @strict_params
    def test_PE_CPEAAPL(self, context, mocker, strict, covered):
        cpeaapl = mocker.Mock(CoveredParenthesizedExpression=mocker.sentinel.covered if covered else None)
        pe = ecmascript.ecmascript.P2_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList(
            context, strict, [cpeaapl]
        )

        errs = pe.EarlyErrors()
        expected_count = 1 if not covered else 0
        assert len(errs) == expected_count


####################################################################################
#
#  d888    .d8888b.       .d8888b.       d888    .d8888b.          d8888
# d8888   d88P  Y88b     d88P  Y88b     d8888   d88P  Y88b        d8P888
#   888          888            888       888   888    888       d8P 888
#   888        .d88P          .d88P       888   888    888      d8P  888
#   888    .od888P"       .od888P"        888   888    888     d88   888
#   888   d88P"          d88P"            888   888    888     8888888888
#   888   888"       d8b 888"       d8b   888   Y88b  d88P d8b       888
# 8888888 888888888  Y8P 888888888  Y8P 8888888  "Y8888P"  Y8P       888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | The Grouping Operator
# 12.2.10.4 | Runtime Semantics: NamedEvaluation
####################################################################################
class Test_GroupingOperator_NamedEvaluation:
    # 12.2.10.4 Runtime Semantics: NamedEvaluation
    #   With parameter name.

    # PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
    #   1. Let expr be CoveredParenthesizedExpression of CoverParenthesizedExpressionAndArrowParameterList.
    #   2. Return the result of performing NamedEvaluation for expr with argument name.
    @strict_params
    def test_cpeaapl(self, context, mocker, strict):
        expr = mocker.Mock(**{"NamedEvaluation.return_value": mocker.sentinel.return_value})
        cpeaapl = mocker.Mock(CoveredParenthesizedExpression=expr)
        pe = ecmascript.ecmascript.P2_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList(
            context, strict, [cpeaapl]
        )

        rv = pe.NamedEvaluation(mocker.sentinel.name)
        assert rv == mocker.sentinel.return_value
        expr.NamedEvaluation.assert_called_with(mocker.sentinel.name)

    # ParenthesizedExpression : ( Expression )
    #   1. Assert: IsAnonymousFunctionDefinition(Expression) is true.
    #   2. Return the result of performing NamedEvaluation for Expression with argument name.
    @strict_params
    def test_parenthesizedexpression(self, context, mocker, strict):
        expr = mocker.Mock(**{"NamedEvaluation.return_value": mocker.sentinel.return_value})
        mocker.patch("ecmascript.ecmascript.IsAnonymousFunctionDefinition", return_value=True)
        pe = ecmascript.ecmascript.P2_ParenthesizedExpression_LPAREN_Expression_RPAREN(
            context, strict, [mocker.Mock(), expr, mocker.Mock()]
        )
        rv = pe.NamedEvaluation(mocker.sentinel.name)
        assert rv == mocker.sentinel.return_value
        expr.NamedEvaluation.assert_called_with(mocker.sentinel.name)


####################################################################################
#
#  d888    .d8888b.       .d8888b.       d888    .d8888b.      888888888
# d8888   d88P  Y88b     d88P  Y88b     d8888   d88P  Y88b     888
#   888          888            888       888   888    888     888
#   888        .d88P          .d88P       888   888    888     8888888b.
#   888    .od888P"       .od888P"        888   888    888          "Y88b
#   888   d88P"          d88P"            888   888    888            888
#   888   888"       d8b 888"       d8b   888   Y88b  d88P d8b Y88b  d88P
# 8888888 888888888  Y8P 888888888  Y8P 8888888  "Y8888P"  Y8P  "Y8888P"
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Primary Expression | The Grouping Operator
# 12.2.10.5 | Static Semantics: Evaluation
####################################################################################
class Test_GroupingOperator_Evaluation:
    # PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
    #   1. Let expr be CoveredParenthesizedExpression of CoverParenthesizedExpressionAndArrowParameterList.
    #   2. Return the result of evaluating expr.
    @strict_params
    def test_cpeeaapl(self, context, mocker, strict):
        expr = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.return_value})
        cpeaapl = mocker.Mock(CoveredParenthesizedExpression=expr)
        pe = ecmascript.ecmascript.P2_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList(
            context, strict, [cpeaapl]
        )
        rv = pe.evaluate()
        assert rv == mocker.sentinel.return_value
        expr.evaluate.assert_called_with()


####################################################################################
#
#  d888    .d8888b.       d888   888888888      888888888      888888888
# d8888   d88P  Y88b     d8888   888            888            888
#   888          888       888   888            888            888
#   888        .d88P       888   8888888b.      8888888b.      8888888b.
#   888    .od888P"        888        "Y88b          "Y88b          "Y88b
#   888   d88P"            888          888            888            888
#   888   888"       d8b   888   Y88b  d88P d8b Y88b  d88P d8b Y88b  d88P
# 8888888 888888888  Y8P 8888888  "Y8888P"  Y8P  "Y8888P"  Y8P  "Y8888P"
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Assignment Operators | Destructuring Assignment
# 12.15.5.5 | Runtime Semantics: IteratorDestructuringAssignmentEvaluation
####################################################################################
class Test_DestructuringAssignment_IteratorDestructuringAssignmentEvaluation:
    # With parameter iteratorRecord.

    # Elision : ,
    #   1. If iteratorRecord.[[Done]] is false, then
    #       a. Let next be IteratorStep(iteratorRecord).
    #       b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
    #       c. ReturnIfAbrupt(next).
    #       d. If next is false, set iteratorRecord.[[Done]] to true.
    #   2. Return NormalCompletion(empty).
    # Test Cases:
    #   * IteratorRecord.Done == True
    #   - IteratorRecord.Done == False
    #     * IteratorStep(IteratorRecord) returns False
    #     * IteratorStep(IteratorRecord) returns something not False
    #     * IteratorStep(IteratorRecord) raises
    @strict_params
    def test_Elision_Comma_01(self, context, mocker, strict):
        # IteratorRecord.Done == True
        iterator_record = mocker.Mock(Done=True)
        elision = ecmascript.ecmascript.P2_Elision_COMMA(context, strict, [mocker.Mock()])
        rv = elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        assert rv == ecmascript.ecmascript.EMPTY
        assert iterator_record.Done is True

    @strict_params
    def test_Elision_Comma_02(self, context, mocker, strict):
        # IteratorRecord.Done == False; IteratorStep(IteratorRecord) returns something not False
        iterator_record = mocker.Mock(Done=False)
        istep = mocker.patch("ecmascript.ecmascript.IteratorStep", return_value=mocker.sentinel.iterobj)
        elision = ecmascript.ecmascript.P2_Elision_COMMA(context, strict, [mocker.Mock()])
        rv = elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        assert rv == ecmascript.ecmascript.EMPTY
        assert iterator_record.Done is False
        istep.assert_called_with(iterator_record)

    @strict_params
    def test_Elision_Comma_03(self, context, mocker, strict):
        # IteratorRecord.Done == False; IteratorStep(IteratorRecord) returns False
        iterator_record = mocker.Mock(Done=False)
        istep = mocker.patch("ecmascript.ecmascript.IteratorStep", return_value=False)
        elision = ecmascript.ecmascript.P2_Elision_COMMA(context, strict, [mocker.Mock()])
        rv = elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        assert rv == ecmascript.ecmascript.EMPTY
        assert iterator_record.Done is True
        istep.assert_called_with(iterator_record)

    @strict_params
    def test_Elision_Comma_04(self, context, mocker, strict):
        # IteratorRecord.Done == False; IteratorStep(IteratorRecord) raises
        iterator_record = mocker.Mock(Done=False)
        istep = mocker.patch(
            "ecmascript.ecmascript.IteratorStep", side_effect=ecmascript.ecmascript.ESTypeError("Test Error")
        )
        elision = ecmascript.ecmascript.P2_Elision_COMMA(context, strict, [mocker.Mock()])
        with pytest.raises(ecmascript.ecmascript.ESTypeError, match="Test Error"):
            elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        assert iterator_record.Done is True
        istep.assert_called_with(iterator_record)

    # Elision : Elision ,
    #   1. Perform ? IteratorDestructuringAssignmentEvaluation of Elision with iteratorRecord as the argument.
    #   2. If iteratorRecord.[[Done]] is false, then
    #       a. Let next be IteratorStep(iteratorRecord).
    #       b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
    #       c. ReturnIfAbrupt(next).
    #       d. If next is false, set iteratorRecord.[[Done]] to true.
    #   3. Return NormalCompletion(empty).
    # Test Cases:
    #   * After step 1, iteratorRecord.[[Done]] is true
    #   - After step 1, iteratorRecord.[[Done]] is false
    #       * IteratorStep(iteratorRecord) raises
    #       * IteratorStep(iteratorRecord) returns false
    #       * IteratorStep(iteratorRecord) returns anything but false
    @strict_params
    def test_Elision_Elision_Comma_01(self, context, mocker, strict):
        # After step 1, iteratorRecord.[[Done]] is true
        elision_child = mocker.Mock(
            **{"IteratorDestructuringAssignmentEvaluation.return_value": mocker.sentinel.child}
        )
        elision = ecmascript.ecmascript.P2_Elision_Elision_COMMA(context, strict, [elision_child, mocker.Mock()])
        iterator_record = mocker.Mock(Done=True)
        rv = elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        elision_child.IteratorDestructuringAssignmentEvaluation.assert_called_with(iterator_record)
        assert rv == ecmascript.ecmascript.EMPTY
        assert iterator_record.Done is True
        elision_child.IteratorDestructuringAssignmentEvaluation.assert_called_with(iterator_record)

    @strict_params
    def test_Elision_Elision_Comma_02(self, context, mocker, strict):
        # After step 1, iteratorRecord.[[Done]] is false; IteratorStep(iteratorRecord) raises
        elision_child = mocker.Mock(
            **{"IteratorDestructuringAssignmentEvaluation.return_value": mocker.sentinel.child}
        )
        elision = ecmascript.ecmascript.P2_Elision_Elision_COMMA(context, strict, [elision_child, mocker.Mock()])
        iterator_record = mocker.Mock(Done=False)
        istep = mocker.patch(
            "ecmascript.ecmascript.IteratorStep", side_effect=ecmascript.ecmascript.ESTypeError("Test Error")
        )
        with pytest.raises(ecmascript.ecmascript.ESTypeError, match="Test Error"):
            elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        assert iterator_record.Done is True
        istep.assert_called_with(iterator_record)
        elision_child.IteratorDestructuringAssignmentEvaluation.assert_called_with(iterator_record)

    @strict_params
    def test_Elision_Elision_Comma_03(self, context, mocker, strict):
        # After step 1, iteratorRecord.[[Done]] is false; IteratorStep(iteratorRecord) returns false
        elision_child = mocker.Mock(
            **{"IteratorDestructuringAssignmentEvaluation.return_value": mocker.sentinel.child}
        )
        elision = ecmascript.ecmascript.P2_Elision_Elision_COMMA(context, strict, [elision_child, mocker.Mock()])
        iterator_record = mocker.Mock(Done=False)
        istep = mocker.patch("ecmascript.ecmascript.IteratorStep", return_value=False)
        rv = elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        assert rv == ecmascript.ecmascript.EMPTY
        assert iterator_record.Done is True
        istep.assert_called_with(iterator_record)
        elision_child.IteratorDestructuringAssignmentEvaluation.assert_called_with(iterator_record)

    @strict_params
    def test_Elision_Elision_Comma_04(self, context, mocker, strict):
        # After step 1, iteratorRecord.[[Done]] is false; IteratorStep(iteratorRecord) returns anything but false
        elision_child = mocker.Mock(
            **{"IteratorDestructuringAssignmentEvaluation.return_value": mocker.sentinel.child}
        )
        elision = ecmascript.ecmascript.P2_Elision_Elision_COMMA(context, strict, [elision_child, mocker.Mock()])
        iterator_record = mocker.Mock(Done=False)
        istep = mocker.patch("ecmascript.ecmascript.IteratorStep", return_value=mocker.sentinel.iterobj)
        rv = elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        assert rv == ecmascript.ecmascript.EMPTY
        assert iterator_record.Done is False
        istep.assert_called_with(iterator_record)
        elision_child.IteratorDestructuringAssignmentEvaluation.assert_called_with(iterator_record)

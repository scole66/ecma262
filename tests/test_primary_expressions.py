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
    lit = ecmascript.ecmascript.P2_Literal(context, ["child"])

    assert lit.name == "Literal"
    assert lit.context == context
    assert lit.children == ["child"]


def test_Literal_NullLiteral_init(context):
    lit = ecmascript.ecmascript.P2_Literal_NullLiteral(context, ["child"])

    assert lit.name == "Literal"
    assert lit.context == context
    assert lit.children == ["child"]
    assert lit.NullLiteral == "child"


def test_Literal_BooleanLiteral_init(context):
    lit = ecmascript.ecmascript.P2_Literal_BooleanLiteral(context, ["child"])

    assert lit.name == "Literal"
    assert lit.context == context
    assert lit.children == ["child"]
    assert lit.BooleanLiteral == "child"


def test_Literal_NumericLiteral_init(context):
    lit = ecmascript.ecmascript.P2_Literal_NumericLiteral(context, ["child"])

    assert lit.name == "Literal"
    assert lit.context == context
    assert lit.children == ["child"]
    assert lit.NumericLiteral == "child"


def test_Literal_StringLiteral_init(context):
    lit = ecmascript.ecmascript.P2_Literal_StringLiteral(context, ["child"])

    assert lit.name == "Literal"
    assert lit.context == context
    assert lit.children == ["child"]
    assert lit.StringLiteral == "child"


@pytest.mark.parametrize(
    "token_stream, result_type",
    [
        ([NULL], ecmascript.ecmascript.P2_Literal_NullLiteral),
        ([TRUE], ecmascript.ecmascript.P2_Literal_BooleanLiteral),
        ([FALSE], ecmascript.ecmascript.P2_Literal_BooleanLiteral),
        ([THREE], ecmascript.ecmascript.P2_Literal_NumericLiteral),
        ([SALICE], ecmascript.ecmascript.P2_Literal_StringLiteral),
        ([YIELD], type(None)),
        ([], type(None)),
    ],
)
def test_parse_Literal(context, token_stream, result_type):
    lexer = Lexer(token_stream)
    lit = ecmascript.ecmascript.parse_Literal(context, lexer)

    assert isinstance(lit, result_type)
    if lit is not None:
        assert lit.children[0] == token_stream[0]
        assert lexer.pos == 1
    else:
        assert lexer.pos == 0


def test_PrimaryExpression_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression(context, ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]


def test_PrimaryExpression_THIS_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_THIS(context, ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]


def test_PrimaryExpression_IdentifierReference_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_IdentifierReference(context, ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.IdentifierReference == "child"


def test_PrimaryExpression_Literal_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_Literal(context, ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.Literal == "child"


def test_PrimaryExpression_ArrayLiteral_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_ArrayLiteral(context, ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.ArrayLiteral == "child"


def test_PrimaryExpression_ObjectLiteral_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_ObjectLiteral(context, ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.ObjectLiteral == "child"


def test_PrimaryExpression_FunctionExpression_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_FunctionExpression(context, ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.FunctionExpression == "child"


def test_PrimaryExpression_ClassExpression_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_ClassExpression(context, ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.ClassExpression == "child"


def test_PrimaryExpression_GeneratorExpression_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_GeneratorExpression(context, ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.GeneratorExpression == "child"


def test_PrimaryExpression_AsyncFunctionExpression_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_AsyncFunctionExpression(context, ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.AsyncFunctionExpression == "child"


def test_PrimaryExpression_AsyncGeneratorExpression_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_AsyncGeneratorExpression(context, ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.AsyncGeneratorExpression == "child"


def test_PrimaryExpression_RegularExpressionLiteral_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_RegularExpressionLiteral(context, ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.RegularExpressionLiteral == "child"


def test_PrimaryExpression_TemplateLiteral_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_TemplateLiteral(context, ["child"])

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.TemplateLiteral == "child"


def test_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList_init(context):
    pe = ecmascript.ecmascript.P2_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList(
        context, ["child"]
    )

    assert pe.name == "PrimaryExpression"
    assert pe.context == context
    assert pe.children == ["child"]
    assert pe.CoverParenthesizedExpressionAndArrowParameterList == "child"


def trees_compare(pn, expected):
    if isinstance(expected, Token):
        return isinstance(pn, Token) and pn.type == expected.type and pn.value == expected.value
    else:
        parent, children = expected
        return (
            isinstance(pn, parent)
            and len(pn.children) == len(children)
            and all(trees_compare(child, expected) for child, expected in zip(pn.children, children))
        )


@pytest.mark.parametrize(
    "token_stream, expected",
    [([THIS], ecmascript.ecmascript.P2_PrimaryExpression_THIS), ([MATCHES_NONE], None), ([], None)],
)
def test_parse_PrimaryExpression_01(context, token_stream, expected):
    lexer = Lexer(token_stream)
    pe = ecmascript.ecmascript.parse_PrimaryExpression(context, lexer, False, True)

    if expected is None:
        assert pe is None
        assert lexer.pos == 0
    else:
        assert isinstance(pe, expected)
        assert pe.children[0] == token_stream[0]
        assert lexer.pos == len(token_stream)


@pytest.mark.parametrize(
    "name, ya, tagged, expected_class",
    [
        ("IdentifierReference", True, False, ecmascript.ecmascript.P2_PrimaryExpression_IdentifierReference),
        ("Literal", False, False, ecmascript.ecmascript.P2_PrimaryExpression_Literal),
        ("ArrayLiteral", True, False, ecmascript.ecmascript.P2_PrimaryExpression_ArrayLiteral),
        ("ObjectLiteral", True, False, ecmascript.ecmascript.P2_PrimaryExpression_ObjectLiteral),
        ("FunctionExpression", False, False, ecmascript.ecmascript.P2_PrimaryExpression_FunctionExpression),
        ("ClassExpression", True, False, ecmascript.ecmascript.P2_PrimaryExpression_ClassExpression),
        ("GeneratorExpression", False, False, ecmascript.ecmascript.P2_PrimaryExpression_GeneratorExpression),
        ("AsyncFunctionExpression", False, False, ecmascript.ecmascript.P2_PrimaryExpression_AsyncFunctionExpression),
        ("AsyncGeneratorExpression", False, False, ecmascript.ecmascript.P2_PrimaryExpression_AsyncGeneratorExpression),
        ("TemplateLiteral", True, True, ecmascript.ecmascript.P2_PrimaryExpression_TemplateLiteral),
        (
            "CoverParenthesizedExpressionAndArrowParameterList",
            True,
            False,
            ecmascript.ecmascript.P2_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList,
        ),
    ],
)
def test_parse_PrimaryExpression_02(mocker, context, name, ya, tagged, expected_class):
    tokens_consumed = 1

    def side_effect(ctx, lexer, *args, **kwargs):
        lexer.pos += tokens_consumed
        return name

    mocked = mocker.patch(f"ecmascript.ecmascript.parse_{name}", side_effect=side_effect)
    lexer = Lexer([MATCHES_NONE])
    pe = ecmascript.ecmascript.parse_PrimaryExpression(context, lexer, "YieldArg", "AwaitArg")

    assert isinstance(pe, expected_class)
    if ya and not tagged:
        mocked.assert_called_with(context, lexer, "YieldArg", "AwaitArg")
    elif ya and tagged:
        mocked.assert_called_with(context, lexer, "YieldArg", "AwaitArg", False)
    else:
        mocked.assert_called_with(context, lexer)
    assert getattr(pe, name) == name
    assert lexer.pos == tokens_consumed


def PrimaryExpression_mocks(mocker):
    return {
        "IdentifierReference": mocker.patch(
            "ecmascript.ecmascript.parse_IdentifierReference", side_effect=ir_sideeffect
        ),
        "Literal": mocker.patch("ecmascript.ecmascript.parse_Literal", side_effect=literal_sideeffect),
        "ArrayLiteral": mocker.patch("ecmascript.ecmascript.parse_ArrayLiteral", side_effect=arrayliteral_sideeffect),
        "ObjectLiteral": mocker.patch(
            "ecmascript.ecmascript.parse_ObjectLiteral", side_effect=objectliteral_sideeffect
        ),
        "TemplateLiteral": mocker.patch(
            "ecmascript.ecmascript.parse_TemplateLiteral", side_effect=templateliteral_sideeffect
        ),
        "CoverParenthesizedExpressionAndArrowParameterList": mocker.patch(
            "ecmascript.ecmascript.parse_CoverParenthesizedExpressionAndArrowParameterList",
            side_effect=coverparenthesizedexpressionandarrowparameterlist_sideeffect,
        ),
        "FunctionExpression": mocker.patch(
            "ecmascript.ecmascript.parse_FunctionExpression", side_effect=functionexpression_sideeffect
        ),
        "ClassExpression": mocker.patch(
            "ecmascript.ecmascript.parse_ClassExpression", side_effect=classexpression_sideeffect
        ),
        "GeneratorExpression": mocker.patch(
            "ecmascript.ecmascript.parse_GeneratorExpression", side_effect=generatorexpression_sideeffect
        ),
        "AsyncFunctionExpression": mocker.patch(
            "ecmascript.ecmascript.parse_AsyncFunctionExpression", side_effect=asyncfunctionexpression_sideeffect
        ),
        "AsyncGeneratorExpression": mocker.patch(
            "ecmascript.ecmascript.parse_AsyncGeneratorExpression", side_effect=asyncgeneratorexpression_sideeffect
        ),
    }


# 12.2.1.1 Static Semantics: CoveredParenthesizedExpression
# CoverParenthesizedExpressionAndArrowParameterList : ( Expression )
#   1. Return the ParenthesizedExpression that is covered by CoverParenthesizedExpressionAndArrowParameterList.
def test_PrimaryExpression_CoveredParenthesizedExpression_01(context, mocker):
    # This one is _so_ much easier with a real lexer.
    lexer = lexer2.Lexer("(a)")
    cpeaap = ecmascript.ecmascript.parse_CoverParenthesizedExpressionAndArrowParameterList(context, lexer, False, False)

    rv = cpeaap.CoveredParenthesizedExpression
    assert rv and isinstance(rv, parse2.P2_ParenthesizedExpression_LPAREN_Expression_RPAREN)
    assert [t.value for t in rv.terminals()] == ["(", "a", ")"]


# 12.2.1.2 Static Semantics: HasName
# PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
#   1. Let expr be CoveredParenthesizedExpression of CoverParenthesizedExpressionAndArrowParameterList.
#   2. If IsFunctionDefinition of expr is false, return false.
#   3. Return HasName of expr.
@pytest.mark.parametrize(
    "fundef, hasname, expected",
    [
        pytest.param(hasname, fundef, hasname and fundef, id=f"IsFunDef:{fundef}, HasName:{hasname}")
        for fundef in (False, True)
        for hasname in (False, True)
    ],
)
def test_PrimaryExpression_HasName_01(context, mocker, fundef, hasname, expected):
    lexer = Lexer([COVERPARENTHESIZEDEXPRESSIONANDARROWPARAMETERLIST])

    expr = mocker.Mock()
    expr.IsFunctionDefinition = mocker.Mock(return_value=fundef)
    expr.HasName = mocker.Mock(return_value=hasname)
    cpeaapl = mocker.Mock()
    cpeaapl.CoveredParenthesizedExpression = expr
    mocker.patch("ecmascript.ecmascript.parse_CoverParenthesizedExpressionAndArrowParameterList", return_value=cpeaapl)
    pe = ecmascript.ecmascript.parse_PrimaryExpression(context, lexer, False, False)

    rv = pe.HasName()
    assert rv == expected


# 12.2.1.3 Static Semantics: IsFunctionDefinition
# PrimaryExpression : this
# PrimaryExpression : IdentifierReference
# PrimaryExpression : Literal
# PrimaryExpression : ArrayLiteral
# PrimaryExpression : ObjectLiteral
# PrimaryExpression : RegularExpressionLiteral
# PrimaryExpression : TemplateLiteral
#   1. Return false.
@pytest.mark.parametrize(
    "token_stream",
    [
        pytest.param([THIS], id="this"),
        pytest.param([IR_REPLACEMENT], id="IdentifierReference"),
        pytest.param([LITERAL], id="Literal"),
        pytest.param([ARRAYLITERAL], id="ArrayLiteral"),
        pytest.param([OBJECTLITERAL], id="ObjectLiteral"),
        pytest.param([REGULAREXPRESSIONLITERAL], id="RegularExpressionLiteral"),
        pytest.param([TEMPLATELITERAL], id="TemplateLiteral"),
    ],
)
def test_PrimaryExpression_IsFunctionDefinition_01(context, mocker, token_stream):
    lexer = Lexer(token_stream)
    PrimaryExpression_mocks(mocker)
    pe = ecmascript.ecmascript.parse_PrimaryExpression(context, lexer, False, False)
    assert not pe.IsFunctionDefinition()


# 12.2.1.3 Static Semantics: IsFunctionDefinition
# PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
#   1. Let expr be CoveredParenthesizedExpression of CoverParenthesizedExpressionAndArrowParameterList.
#   2. Return IsFunctionDefinition of expr.
def test_PrimaryExpression_IsFunctionDefinition_02(context, mocker):
    lexer = Lexer([COVERPARENTHESIZEDEXPRESSIONANDARROWPARAMETERLIST])

    expr = mocker.Mock()
    expr.IsFunctionDefinition = mocker.Mock(return_value="IsFunDef")
    cpeaapl = mocker.Mock()
    cpeaapl.CoveredParenthesizedExpression = expr
    mocker.patch("ecmascript.ecmascript.parse_CoverParenthesizedExpressionAndArrowParameterList", return_value=cpeaapl)
    pe = ecmascript.ecmascript.parse_PrimaryExpression(context, lexer, False, False)

    rv = pe.IsFunctionDefinition()

    expr.IsFunctionDefinition.assert_called()
    assert rv == "IsFunDef"


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
    "token, expected",
    [
        pytest.param(THIS, False, id="this"),
        pytest.param(LITERAL, False, id="Literal"),
        pytest.param(ARRAYLITERAL, False, id="ArrayLiteral"),
        pytest.param(OBJECTLITERAL, False, id="ObjectLiteral"),
        pytest.param(FUNCTIONEXPRESSION, False, id="FunctionExpression"),
        pytest.param(CLASSEXPRESSION, False, id="ClassExpression"),
        pytest.param(GENERATOREXPRESSION, False, id="GeneratorExpression"),
        pytest.param(ASYNCFUNCTIONEXPRESSION, False, id="AsyncFunctionExpression"),
        pytest.param(ASYNCGENERATOREXPRESSION, False, id="AsyncGeneratorExpression"),
        pytest.param(REGULAREXPRESSIONLITERAL, False, id="RegularExpressionLiteral"),
        pytest.param(TEMPLATELITERAL, False, id="TemplateLiteral"),
        pytest.param(
            COVERPARENTHESIZEDEXPRESSIONANDARROWPARAMETERLIST,
            False,
            id="CoverParenthesizedExpressionAndArrowParameterList",
        ),
        pytest.param(IR_REPLACEMENT, True, id="IdentifierReference"),
    ],
)
def test_PrimaryExpression_IsIdentifierRef_01(context, mocker, token, expected):
    lexer = Lexer([token])
    PrimaryExpression_mocks(mocker)
    pe = ecmascript.ecmascript.parse_PrimaryExpression(context, lexer, False, False)
    assert pe.IsIdentifierRef() == expected


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
    "token",
    [
        pytest.param(THIS, id="this"),
        pytest.param(LITERAL, id="Literal"),
        pytest.param(ARRAYLITERAL, id="ArrayLiteral"),
        pytest.param(OBJECTLITERAL, id="ObjectLiteral"),
        pytest.param(FUNCTIONEXPRESSION, id="FunctionExpression"),
        pytest.param(CLASSEXPRESSION, id="ClassExpression"),
        pytest.param(GENERATOREXPRESSION, id="GeneratorExpression"),
        pytest.param(ASYNCFUNCTIONEXPRESSION, id="AsyncFunctionExpression"),
        pytest.param(ASYNCGENERATOREXPRESSION, id="AsyncGeneratorExpression"),
        pytest.param(REGULAREXPRESSIONLITERAL, id="RegularExpressionLiteral"),
        pytest.param(TEMPLATELITERAL, id="TemplateLiteral"),
    ],
)
def test_PrimaryExpression_AssignmentTargetType_01(context, mocker, token):
    lexer = Lexer([token])
    PrimaryExpression_mocks(mocker)
    pe = ecmascript.ecmascript.parse_PrimaryExpression(context, lexer, False, False)
    assert pe.AssignmentTargetType == parse2.INVALID


# 12.2.1.5 Static Semantics: AssignmentTargetType
# PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
#   1. Let expr be CoveredParenthesizedExpression of CoverParenthesizedExpressionAndArrowParameterList.
#   2. Return AssignmentTargetType of expr.
def test_PrimaryExpression_AssignmentTargetType_CPEAAPL(context, mocker):
    lexer = Lexer([COVERPARENTHESIZEDEXPRESSIONANDARROWPARAMETERLIST])

    expr = mocker.Mock()
    expr.AssignmentTargetType = "ATT"
    cpeaapl = mocker.Mock()
    cpeaapl.CoveredParenthesizedExpression = expr
    mocker.patch("ecmascript.ecmascript.parse_CoverParenthesizedExpressionAndArrowParameterList", return_value=cpeaapl)
    pe = ecmascript.ecmascript.parse_PrimaryExpression(context, lexer, False, False)

    rv = pe.AssignmentTargetType
    assert rv == "ATT"


# 12.2.2.1 Runtime Semantics: Evaluation
# PrimaryExpression : this
#   1. Return ? ResolveThisBinding().
def test_PrimaryExpression_Evaluation_THIS(context, mocker):
    rtb = mocker.patch("ecmascript.ecmascript.ResolveThisBinding", return_value="RTB")
    lexer = lexer2.Lexer("this")
    pe = ecmascript.ecmascript.parse_PrimaryExpression(context, lexer, False, False)

    rv = pe.evaluate()
    assert rv == "RTB"
    rtb.assert_called_with()


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
    "src, expected",
    [
        ("null", ecmascript.ecmascript.JSNull.NULL),
        ("false", False),
        ("true", True),
        ("67.34", 67.34),
        ('"mystring"', "mystring"),
    ],
)
def test_PrimaryExpression_Evaluation_Literal(context, mocker, src, expected):
    lexer = lexer2.Lexer(src)
    pe = ecmascript.ecmascript.parse_PrimaryExpression(context, lexer, False, False)

    rv = pe.evaluate()
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
        context, ["child"], "YieldArg", "AwaitArg"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"
    assert cpeaapl.context == context
    assert cpeaapl.children == ["child"]
    assert cpeaapl.Yield == "YieldArg"
    assert cpeaapl.Await == "AwaitArg"


def test_P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_RPAREN_init(context):
    cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_RPAREN(
        context, ["(", "Expression", ")"], "Y", "A"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"
    assert cpeaapl.Expression == "Expression"


def test_P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_RPAREN_init(context):
    cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_RPAREN(
        context, ["(", "Expression", ")", ","], "Y", "A"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"
    assert cpeaapl.Expression == "Expression"


def test_P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_RPAREN_init(context):
    cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_RPAREN(
        context, ["(", ")"], "Y", "A"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"


def test_P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_DOTDOTDOT_BindingIdentifier_RPAREN_init(context):
    cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_DOTDOTDOT_BindingIdentifier_RPAREN(
        context, ["(", "...", "BindingIdentifier", ")"], "Y", "A"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"
    assert cpeaapl.BindingIdentifier == "BindingIdentifier"


def test_P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_DOTDOTDOT_BindingPattern_RPAREN_init(context):
    cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_DOTDOTDOT_BindingPattern_RPAREN(
        context, ["(", "...", "BindingPattern", ")"], "Y", "A"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"
    assert cpeaapl.BindingPattern == "BindingPattern"


def test_P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_DOTDOTDOT_BindingIdentifier_RPAREN_init(
    context,
):
    cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_DOTDOTDOT_BindingIdentifier_RPAREN(
        context, ["(", "Expression", ",", "...", "BindingIdentifier", ")"], "Y", "A"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"
    assert cpeaapl.Expression == "Expression"
    assert cpeaapl.BindingIdentifier == "BindingIdentifier"


def test_P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_DOTDOTDOT_BindingPattern_RPAREN_init(
    context,
):
    cpeaapl = ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_DOTDOTDOT_BindingPattern_RPAREN(
        context, ["(", "Expression", ",", "...", "BindingPattern", ")"], "Y", "A"
    )
    assert cpeaapl.name == "CoverParenthesizedExpressionAndArrowParameterList"
    assert cpeaapl.Expression == "Expression"
    assert cpeaapl.BindingPattern == "BindingPattern"


def CoverParenthesizedExpressionAndArrowParameterList_mocks(mocker):
    return {
        "exp": mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect),
        "bi": mocker.patch("ecmascript.ecmascript.parse_BindingIdentifier", side_effect=bindingidentifier_sideeffect),
        "bp": mocker.patch("ecmascript.ecmascript.parse_BindingPattern", side_effect=bindingpattern_sideeffect),
    }


@pytest.mark.parametrize(
    "token_stream, expected_class",
    [
        (
            [LPAREN, EXPRESSION, RPAREN],
            ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_RPAREN,
        ),
        (
            [LPAREN, EXPRESSION, COMMA, RPAREN],
            ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_RPAREN,
        ),
    ],
)
def test_parse_CoverParenthesizedExpressionAndArrowParameterList_01(mocker, context, token_stream, expected_class):
    # cpeaapl : ( Expression )
    # cpeaapl : ( Expression , )
    lexer = Lexer(token_stream)
    mocks = CoverParenthesizedExpressionAndArrowParameterList_mocks(mocker)

    cpeaapl = ecmascript.ecmascript.parse_CoverParenthesizedExpressionAndArrowParameterList(
        context, lexer, "YieldArg", "AwaitArg"
    )
    assert isinstance(cpeaapl, expected_class)
    assert cpeaapl.Expression == "Expression"
    assert lexer.pos == len(token_stream)
    mocks["exp"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")


def test_parse_CoverParenthesizedExpressionAndArrowParameterList_02(mocker, context):
    # cpeaapl : ( )
    lexer = Lexer([LPAREN, RPAREN])
    CoverParenthesizedExpressionAndArrowParameterList_mocks(mocker)

    cpeaapl = ecmascript.ecmascript.parse_CoverParenthesizedExpressionAndArrowParameterList(
        context, lexer, "YieldArg", "AwaitArg"
    )
    assert isinstance(cpeaapl, ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_RPAREN)
    assert lexer.pos == 2


def test_parse_CoverParenthesizedExpressionAndArrowParameterList_03(mocker, context):
    # cpeaapl : ( ... BindingIdentifier )
    lexer = Lexer([LPAREN, DOTDOTDOT, BINDINGIDENTIFIER, RPAREN])
    mocks = CoverParenthesizedExpressionAndArrowParameterList_mocks(mocker)

    cpeaapl = ecmascript.ecmascript.parse_CoverParenthesizedExpressionAndArrowParameterList(
        context, lexer, "YieldArg", "AwaitArg"
    )
    assert isinstance(
        cpeaapl,
        ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_DOTDOTDOT_BindingIdentifier_RPAREN,
    )
    assert lexer.pos == 4
    assert cpeaapl.BindingIdentifier == "BindingIdentifier"
    mocks["bi"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")


def test_parse_CoverParenthesizedExpressionAndArrowParameterList_04(mocker, context):
    # cpeaapl : ( ... BindingPattern )
    lexer = Lexer([LPAREN, DOTDOTDOT, BINDINGPATTERN, RPAREN])
    mocks = CoverParenthesizedExpressionAndArrowParameterList_mocks(mocker)

    cpeaapl = ecmascript.ecmascript.parse_CoverParenthesizedExpressionAndArrowParameterList(
        context, lexer, "YieldArg", "AwaitArg"
    )
    assert isinstance(
        cpeaapl,
        ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_DOTDOTDOT_BindingPattern_RPAREN,
    )
    assert lexer.pos == 4
    assert cpeaapl.BindingPattern == "BindingPattern"
    mocks["bp"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")


def test_parse_CoverParenthesizedExpressionAndArrowParameterList_05(mocker, context):
    # cpeaapl : ( Expression , ... BindingIdentifier )
    lexer = Lexer([LPAREN, EXPRESSION, COMMA, DOTDOTDOT, BINDINGIDENTIFIER, RPAREN])
    mocks = CoverParenthesizedExpressionAndArrowParameterList_mocks(mocker)

    cpeaapl = ecmascript.ecmascript.parse_CoverParenthesizedExpressionAndArrowParameterList(
        context, lexer, "YieldArg", "AwaitArg"
    )
    assert isinstance(
        cpeaapl,
        ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_DOTDOTDOT_BindingIdentifier_RPAREN,
    )
    assert cpeaapl.Expression == "Expression"
    assert cpeaapl.BindingIdentifier == "BindingIdentifier"
    assert lexer.pos == 6
    mocks["exp"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")
    mocks["bi"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")


def test_parse_CoverParenthesizedExpressionAndArrowParameterList_06(mocker, context):
    # cpeaapl : ( Expression , ... BindingPattern )
    lexer = Lexer([LPAREN, EXPRESSION, COMMA, DOTDOTDOT, BINDINGPATTERN, RPAREN])
    mocks = CoverParenthesizedExpressionAndArrowParameterList_mocks(mocker)

    cpeaapl = ecmascript.ecmascript.parse_CoverParenthesizedExpressionAndArrowParameterList(
        context, lexer, "YieldArg", "AwaitArg"
    )
    assert isinstance(
        cpeaapl,
        ecmascript.ecmascript.P2_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_DOTDOTDOT_BindingPattern_RPAREN,
    )
    assert cpeaapl.Expression == "Expression"
    assert cpeaapl.BindingPattern == "BindingPattern"
    assert lexer.pos == 6
    mocks["exp"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")
    mocks["bp"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "token_stream",
    [
        [MATCHES_NONE],
        [LPAREN, MATCHES_NONE],
        [LPAREN, DOTDOTDOT, MATCHES_NONE],
        [LPAREN, EXPRESSION, MATCHES_NONE],
        [LPAREN, EXPRESSION, COMMA, MATCHES_NONE],
        [LPAREN, DOTDOTDOT, BINDINGIDENTIFIER, MATCHES_NONE],
        [LPAREN, DOTDOTDOT, BINDINGPATTERN, MATCHES_NONE],
        [LPAREN, EXPRESSION, COMMA, MATCHES_NONE],
        [LPAREN, EXPRESSION, COMMA, DOTDOTDOT, MATCHES_NONE],
        [LPAREN, EXPRESSION, COMMA, DOTDOTDOT, BINDINGIDENTIFIER, MATCHES_NONE],
        [LPAREN, EXPRESSION, COMMA, DOTDOTDOT, BINDINGPATTERN, MATCHES_NONE],
    ],
)
def test_parse_CoverParenthesizedExpressionAndArrowParameterList_07(mocker, context, token_stream):
    # all the errors
    lexer = Lexer(token_stream)
    CoverParenthesizedExpressionAndArrowParameterList_mocks(mocker)

    cpeaapl = ecmascript.ecmascript.parse_CoverParenthesizedExpressionAndArrowParameterList(
        context, lexer, "YieldArg", "AwaitArg"
    )
    assert cpeaapl is None
    assert lexer.pos == 0


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
    pe = ecmascript.ecmascript.P2_ParenthesizedExpression(context, ["child"])
    assert pe.name == "ParenthesizedExpression"
    assert pe.context == context
    assert pe.children == ["child"]


def test_P2_ParenthesizedExpression_LPAREN_Expression_RPAREN_init(context):
    pe = ecmascript.ecmascript.P2_ParenthesizedExpression_LPAREN_Expression_RPAREN(context, ["(", "Expression", ")"])
    assert pe.name == "ParenthesizedExpression"
    assert pe.Expression == "Expression"


def ParenthesizedExpression_mocks(mocker):
    return {"exp": mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect)}


def test_parse_ParenthesizedExpression_01(mocker, context):
    # ParenthesizedExpression : ( Expression )
    lexer = Lexer([LPAREN, EXPRESSION, RPAREN])
    mocks = ParenthesizedExpression_mocks(mocker)

    pe = ecmascript.ecmascript.parse_ParenthesizedExpression(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(pe, ecmascript.ecmascript.P2_ParenthesizedExpression_LPAREN_Expression_RPAREN)
    assert pe.Expression == "Expression"
    assert lexer.pos == 3
    mocks["exp"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "token_stream, error",
    [
        ([MATCHES_NONE], ""),
        ([LPAREN, MATCHES_NONE], "Poorly formed parenthesized expression"),
        ([LPAREN, EXPRESSION, MATCHES_NONE], "Poorly formed parenthesized expression"),
    ],
)
def test_parse_ParenthesizedExpression_02(mocker, context, token_stream, error):
    # Syntax errors
    lexer = Lexer(token_stream)
    ParenthesizedExpression_mocks(mocker)

    pe = ecmascript.ecmascript.parse_ParenthesizedExpression(context, lexer, "YieldArg", "AwaitArg")
    assert pe is None
    assert lexer.pos == 0


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
    elision = ecmascript.ecmascript.P2_Elision(context, ["child"])
    assert elision.name == "Elision"
    assert elision.context == context
    assert elision.children == ["child"]


def test_Elision_COMMA_init(context):
    elision = ecmascript.ecmascript.P2_Elision_COMMA(context, [","])
    assert elision.name == "Elision"
    assert elision.children == [","]


def test_Elision_Elision_COMMA_init(context):
    elision = ecmascript.ecmascript.P2_Elision_Elision_COMMA(context, ["child_a", "child_b"])
    assert elision.name == "Elision"
    assert elision.children == ["child_a", "child_b"]
    assert elision.Elision == "child_a"


@pytest.mark.parametrize(
    "token_stream, expected",
    [
        ([Token(",", ",")], (ecmascript.ecmascript.P2_Elision_COMMA, [Token(",", ",")])),
        (
            [Token(",", ",")] * 3,
            (
                ecmascript.ecmascript.P2_Elision_Elision_COMMA,
                [
                    (
                        ecmascript.ecmascript.P2_Elision_Elision_COMMA,
                        [(ecmascript.ecmascript.P2_Elision_COMMA, [Token(",", ",")]), Token(",", ",")],
                    ),
                    Token(",", ","),
                ],
            ),
        ),
        ([Token("IDENTIFIER", "bob")], None),
        ([], None),
    ],
)
def test_parse_Elision(context, token_stream, expected):
    lexer = Lexer(token_stream)
    elision = ecmascript.ecmascript.parse_Elision(context, lexer)

    if expected is None:
        assert elision is None
        assert lexer.pos == 0
    else:
        assert trees_compare(elision, expected)
        assert lexer.pos == len(token_stream)


@pytest.mark.parametrize("src, expected", [(",", 1), (",,,,", 4)])
def test_Elision_ElisionWidth(context, mocker, src, expected):
    lexer = lexer2.Lexer(src)
    elision = ecmascript.ecmascript.parse_Elision(context, lexer)

    rv = elision.ElisionWidth()
    assert rv == expected


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
    al = ecmascript.ecmascript.P2_ArrayLiteral(context, ["child"])
    assert al.name == "ArrayLiteral"
    assert al.context == context
    assert al.children == ["child"]


def test_ArrayLiteral_LBRACKET_RBRACKET_init(context):
    al = ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_RBRACKET(context, ["[", "]"])
    assert al.name == "ArrayLiteral"
    assert al.children == ["[", "]"]


def test_ArrayLiteral_LBRACKET_Elision_RBRACKET_init(context):
    al = ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_Elision_RBRACKET(context, ["lbracket", "elision", "rbracket"])
    assert al.name == "ArrayLiteral"
    assert al.Elision == "elision"


def test_ArrayLiteral_LBRACKET_ElementList_RBRACKET_init(context):
    al = ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_ElementList_RBRACKET(
        context, ["lbracket", "elementlist", "rbracket"]
    )
    assert al.name == "ArrayLiteral"
    assert al.ElementList == "elementlist"


def test_ArrayLiteral_LBRACKET_ElementList_COMMA_RBRACKET_init(context):
    al = ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_ElementList_COMMA_RBRACKET(
        context, ["lbra", "elementlist", "comma", "rbra"]
    )
    assert al.name == "ArrayLiteral"
    assert al.ElementList == "elementlist"


def test_ArrayLiteral_elementlist_elision_init(context):
    al = ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_ElementList_COMMA_Elision_RBRACKET(
        context, ["lbra", "elementlist", "comma", "elision", "rbra"]
    )
    assert al.name == "ArrayLiteral"
    assert al.ElementList == "elementlist"
    assert al.Elision == "elision"


def ArrayLiteral_mocks(mocker):
    return {
        "elision": mocker.patch("ecmascript.ecmascript.parse_Elision", side_effect=elision_sideeffect),
        "el": mocker.patch("ecmascript.ecmascript.parse_ElementList", side_effect=el_sideeffect),
    }


def test_parse_ArrayLiteral_01(mocker, context):
    # ArrayLiteral : [ ]
    lexer = Lexer([LBRACKET, RBRACKET])
    ArrayLiteral_mocks(mocker)

    al = ecmascript.ecmascript.parse_ArrayLiteral(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(al, ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_RBRACKET)
    assert lexer.pos == 2


def test_parse_ArrayLiteral_02(mocker, context):
    # ArrayLiteral [ Elision ]
    lexer = Lexer([LBRACKET, ELISION_REPLACEMENT, RBRACKET])
    mocks = ArrayLiteral_mocks(mocker)

    al = ecmascript.ecmascript.parse_ArrayLiteral(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(al, ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_Elision_RBRACKET)
    assert al.Elision == "Elision"
    assert lexer.pos == 3
    mocks["elision"].assert_called_with(context, lexer)


@pytest.mark.parametrize(
    "token_stream, expected_class",
    [
        ([LBRACKET, EL_REPLACEMENT, RBRACKET], ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_ElementList_RBRACKET),
        (
            [LBRACKET, EL_REPLACEMENT, COMMA, RBRACKET],
            ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_ElementList_COMMA_RBRACKET,
        ),
    ],
)
def test_parse_ArrayLiteral_03(mocker, context, token_stream, expected_class):
    # ArrayLiteral : [ ElementList ]
    # ArrayLiteral : [ ElementList , ]
    lexer = Lexer(token_stream)
    mocks = ArrayLiteral_mocks(mocker)

    al = ecmascript.ecmascript.parse_ArrayLiteral(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(al, expected_class)
    assert al.ElementList == "ElementList"
    assert lexer.pos == len(token_stream)
    mocks["el"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")


def test_parse_ArrayLiteral_04(mocker, context):
    # ArrayLiteral : [ ElementList , Elision ]
    lexer = Lexer([LBRACKET, EL_REPLACEMENT, COMMA, ELISION_REPLACEMENT, RBRACKET])
    mocks = ArrayLiteral_mocks(mocker)

    al = ecmascript.ecmascript.parse_ArrayLiteral(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(al, ecmascript.ecmascript.P2_ArrayLiteral_LBRACKET_ElementList_COMMA_Elision_RBRACKET)
    assert al.ElementList == "ElementList"
    assert al.Elision == "Elision"
    assert lexer.pos == 5
    mocks["el"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")
    mocks["elision"].assert_called_with(context, lexer)


@pytest.mark.parametrize(
    "token_stream",
    [
        [MATCHES_NONE],
        [LBRACKET, MATCHES_NONE],
        [LBRACKET, ELISION_REPLACEMENT, MATCHES_NONE],
        [LBRACKET, EL_REPLACEMENT, MATCHES_NONE],
        [LBRACKET, EL_REPLACEMENT, COMMA, MATCHES_NONE],
        [LBRACKET, EL_REPLACEMENT, COMMA, ELISION_REPLACEMENT, MATCHES_NONE],
    ],
)
def test_parse_ArrayLiteral_05(mocker, context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)
    ArrayLiteral_mocks(mocker)

    al = ecmascript.ecmascript.parse_ArrayLiteral(context, lexer, "YieldArg", "AwaitArg")
    assert al is None
    assert lexer.pos == 0


# 12.2.5.3 Runtime Semantics: Evaluation
# ArrayLiteral : [ Elision ]
#   1. Let array be ! ArrayCreate(0).
#   2. Let pad be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
#   3. Perform Set(array, "length", ToUint32(pad), false).
#   4. NOTE: The above Set cannot fail because of the nature of the object returned by ArrayCreate.
#   5. Return array.
@pytest.mark.parametrize("src, pad", [("[]", 0), ("[,,,]", 3)])
def test_ArrayLiteral_Elision_evaluate(context, mocker, src, pad):
    lexer = lexer2.Lexer(src)
    al = ecmascript.ecmascript.parse_ArrayLiteral(context, lexer, False, False)
    ac = mocker.patch("ecmascript.ecmascript.ArrayCreate", return_value="array")
    st = mocker.patch("ecmascript.ecmascript.Set", return_value=None)

    rv = al.evaluate()
    ac.assert_called_with(0)
    st.assert_called_with("array", "length", pad, False)
    assert rv == "array"


# 12.2.5.3 Runtime Semantics: Evaluation
# ArrayLiteral : [ ElementList ]
#   1. Let array be ! ArrayCreate(0).
#   2. Let len be the result of performing ArrayAccumulation for ElementList with arguments array and 0.
#   3. ReturnIfAbrupt(len).
#   4. Perform Set(array, "length", ToUint32(len), false).
#   5. NOTE: The above Set cannot fail because of the nature of the object returned by ArrayCreate.
#   6. Return array.
def test_ArrayLiteral_ElementList_evaluate(context, mocker):
    lexer = lexer2.Lexer("[5]")
    al = ecmascript.ecmascript.parse_ArrayLiteral(context, lexer, False, False)
    ac = mocker.patch("ecmascript.ecmascript.ArrayCreate", return_value="array")
    al.ElementList.ArrayAccumulation = mocker.Mock(return_value=100)
    st = mocker.patch("ecmascript.ecmascript.Set", return_value=None)

    rv = al.evaluate()
    ac.assert_called_with(0)
    al.ElementList.ArrayAccumulation.assert_called_with("array", 0)
    st.assert_called_with("array", "length", 100, False)
    assert rv == "array"


# 12.2.5.3 Runtime Semantics: Evaluation
# ArrayLiteral : [ ElementList , Elision ]
#   1. Let array be ! ArrayCreate(0).
#   2. Let len be the result of performing ArrayAccumulation for ElementList with arguments array and 0.
#   3. ReturnIfAbrupt(len).
#   4. Let padding be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
#   5. Perform Set(array, "length", ToUint32(padding + len), false).
#   6. NOTE: The above Set cannot fail because of the nature of the object returned by ArrayCreate.
#   7. Return array.
@pytest.mark.parametrize("src, padding", [("[6,]", 0), ("[7,,,]", 2)])
def test_ArrayLiteral_ElementList_Elision_evaluate(context, mocker, src, padding):
    lexer = lexer2.Lexer(src)
    al = ecmascript.ecmascript.parse_ArrayLiteral(context, lexer, False, False)
    ac = mocker.patch("ecmascript.ecmascript.ArrayCreate", return_value="array")
    al.ElementList.ArrayAccumulation = mocker.Mock(return_value=1)
    st = mocker.patch("ecmascript.ecmascript.Set", return_value=None)

    rv = al.evaluate()
    ac.assert_called_with(0)
    al.ElementList.ArrayAccumulation.assert_called_with("array", 0)
    st.assert_called_with("array", "length", 1 + padding, False)
    assert rv == "array"


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
    el = ecmascript.ecmascript.P2_ElementList(context, ["child"])
    assert el.name == "ElementList"
    assert el.context == context
    assert el.children == ["child"]


def test_P2_ElementList_AssignmentExpression_init(context):
    el = ecmascript.ecmascript.P2_ElementList_AssignmentExpression(context, ["AssignmentExpression"])
    assert el.name == "ElementList"
    assert el.AssignmentExpression == "AssignmentExpression"


def test_P2_ElementList_Elision_AssignmentExpression_init(context):
    el = ecmascript.ecmascript.P2_ElementList_Elision_AssignmentExpression(context, ["Elision", "AssignmentExpression"])
    assert el.name == "ElementList"
    assert el.Elision == "Elision"
    assert el.AssignmentExpression == "AssignmentExpression"


def test_P2_ElementList_SpreadElement_init(context):
    el = ecmascript.ecmascript.P2_ElementList_SpreadElement(context, ["SpreadElement"])
    assert el.name == "ElementList"
    assert el.SpreadElement == "SpreadElement"


def test_P2_ElementList_Elision_SpreadElement_init(context):
    el = ecmascript.ecmascript.P2_ElementList_Elision_SpreadElement(context, ["Elision", "SpreadElement"])
    assert el.name == "ElementList"
    assert el.Elision == "Elision"
    assert el.SpreadElement == "SpreadElement"


def test_P2_ElementList_ElementList_COMMA_AssignmentExpression_init(context):
    el = ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_AssignmentExpression(
        context, ["ElementList", ",", "AssignmentExpression"]
    )
    assert el.name == "ElementList"
    assert el.ElementList == "ElementList"
    assert el.AssignmentExpression == "AssignmentExpression"


def test_P2_ElementList_ElementList_COMMA_Elision_AssignmentExpression_init(context):
    el = ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_Elision_AssignmentExpression(
        context, ["ElementList", ",", "Elision", "AssignmentExpression"]
    )
    assert el.name == "ElementList"
    assert el.ElementList == "ElementList"
    assert el.Elision == "Elision"
    assert el.AssignmentExpression == "AssignmentExpression"


def test_P2_ElementList_ElementList_COMMA_SpreadElement_init(context):
    el = ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_SpreadElement(
        context, ["ElementList", ",", "SpreadElement"]
    )
    assert el.name == "ElementList"
    assert el.ElementList == "ElementList"
    assert el.SpreadElement == "SpreadElement"


def test_P2_ElementList_ElementList_COMMA_Elision_SpreadElement_init(context):
    el = ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_Elision_SpreadElement(
        context, ["ElementList", ",", "Elision", "SpreadElement"]
    )
    assert el.name == "ElementList"
    assert el.ElementList == "ElementList"
    assert el.Elision == "Elision"
    assert el.SpreadElement == "SpreadElement"


def ElementList_mocks(mocker):
    return {
        "elision": mocker.patch("ecmascript.ecmascript.parse_Elision", side_effect=elision_sideeffect),
        "ae": mocker.patch("ecmascript.ecmascript.parse_AssignmentExpression", side_effect=ae_sideeffect),
        "se": mocker.patch("ecmascript.ecmascript.parse_SpreadElement", side_effect=se_sideeffect),
    }


def test_parse_ElementList_01(mocker, context):
    # ElementList : AssignmentExpression
    lexer = Lexer([AE_REPLACEMENT])
    mocks = ElementList_mocks(mocker)

    el = ecmascript.ecmascript.parse_ElementList(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(el, ecmascript.ecmascript.P2_ElementList_AssignmentExpression)
    assert el.AssignmentExpression == "AssignmentExpression"
    assert lexer.pos == 1
    mocks["ae"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")


def test_parse_ElementList_02(mocker, context):
    # ElementList : Elision AssignmentExpression
    lexer = Lexer([ELISION_REPLACEMENT, AE_REPLACEMENT])
    mocks = ElementList_mocks(mocker)

    el = ecmascript.ecmascript.parse_ElementList(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(el, ecmascript.ecmascript.P2_ElementList_Elision_AssignmentExpression)
    assert el.Elision == "Elision"
    assert el.AssignmentExpression == "AssignmentExpression"
    assert lexer.pos == 2
    mocks["ae"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")
    mocks["elision"].assert_called_with(context, lexer)


def test_parse_ElementList_03(mocker, context):
    # ElementList : SpreadElement
    lexer = Lexer([SE_REPLACEMENT])
    mocks = ElementList_mocks(mocker)

    el = ecmascript.ecmascript.parse_ElementList(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(el, ecmascript.ecmascript.P2_ElementList_SpreadElement)
    assert el.SpreadElement == "SpreadElement"
    assert lexer.pos == 1
    mocks["se"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")


def test_parse_ElementList_04(mocker, context):
    # ElementList : Elision SpreadElement
    lexer = Lexer([ELISION_REPLACEMENT, SE_REPLACEMENT])
    mocks = ElementList_mocks(mocker)

    el = ecmascript.ecmascript.parse_ElementList(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(el, ecmascript.ecmascript.P2_ElementList_Elision_SpreadElement)
    assert el.Elision == "Elision"
    assert el.SpreadElement == "SpreadElement"
    assert lexer.pos == 2
    mocks["se"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")
    mocks["elision"].assert_called_with(context, lexer)


def test_parse_ElementList_05(mocker, context):
    # ElementList : ElementList , AssignmentExpression
    lexer = Lexer([SE_REPLACEMENT, COMMA, AE_REPLACEMENT])
    mocks = ElementList_mocks(mocker)

    el = ecmascript.ecmascript.parse_ElementList(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(el, ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_AssignmentExpression)
    assert el.AssignmentExpression == "AssignmentExpression"
    el2 = el.ElementList
    assert isinstance(el2, ecmascript.ecmascript.P2_ElementList_SpreadElement)
    assert lexer.pos == 3
    mocks["ae"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")


def test_parse_ElementList_06(mocker, context):
    # ElementList : ElementList , Elision AssignmentExpression
    lexer = Lexer([SE_REPLACEMENT, COMMA, ELISION_REPLACEMENT, AE_REPLACEMENT])
    mocks = ElementList_mocks(mocker)

    el = ecmascript.ecmascript.parse_ElementList(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(el, ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_Elision_AssignmentExpression)
    assert el.AssignmentExpression == "AssignmentExpression"
    assert el.Elision == "Elision"
    el2 = el.ElementList
    assert isinstance(el2, ecmascript.ecmascript.P2_ElementList_SpreadElement)
    assert lexer.pos == 4
    mocks["ae"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")
    mocks["elision"].assert_called_with(context, lexer)


def test_parse_ElementList_07(mocker, context):
    # ElementList : ElementList , SpreadElement
    lexer = Lexer([AE_REPLACEMENT, COMMA, SE_REPLACEMENT])
    mocks = ElementList_mocks(mocker)

    el = ecmascript.ecmascript.parse_ElementList(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(el, ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_SpreadElement)
    assert el.SpreadElement == "SpreadElement"
    el2 = el.ElementList
    assert isinstance(el2, ecmascript.ecmascript.P2_ElementList_AssignmentExpression)
    assert lexer.pos == 3
    mocks["se"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")


def test_parse_ElementList_08(mocker, context):
    # ElementList : ElementList , Elision SpreadElement
    lexer = Lexer([AE_REPLACEMENT, COMMA, ELISION_REPLACEMENT, SE_REPLACEMENT])
    mocks = ElementList_mocks(mocker)

    el = ecmascript.ecmascript.parse_ElementList(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(el, ecmascript.ecmascript.P2_ElementList_ElementList_COMMA_Elision_SpreadElement)
    assert el.SpreadElement == "SpreadElement"
    assert el.Elision == "Elision"
    el2 = el.ElementList
    assert isinstance(el2, ecmascript.ecmascript.P2_ElementList_AssignmentExpression)
    assert lexer.pos == 4
    mocks["se"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")
    mocks["elision"].assert_called_with(context, lexer)


@pytest.mark.parametrize("token_stream", [[MATCHES_NONE], [ELISION_REPLACEMENT, MATCHES_NONE]])
def test_parse_ElementList_09(mocker, context, token_stream):
    # Non-recursive syntax errors
    lexer = Lexer(token_stream)
    ElementList_mocks(mocker)

    el = ecmascript.ecmascript.parse_ElementList(context, lexer, "YieldArg", "AwaitArg")
    assert el is None
    assert lexer.pos == 0


@pytest.mark.parametrize(
    "token_stream", [[AE_REPLACEMENT, COMMA, MATCHES_NONE], [AE_REPLACEMENT, COMMA, ELISION_REPLACEMENT, MATCHES_NONE]]
)
def test_parse_ElementList_10(mocker, context, token_stream):
    # Recursive syntax errors
    lexer = Lexer(token_stream)
    ElementList_mocks(mocker)

    el = ecmascript.ecmascript.parse_ElementList(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(el, ecmascript.ecmascript.P2_ElementList_AssignmentExpression)
    assert lexer.pos == 1


# 12.2.5.2 Runtime Semantics: ArrayAccumulation
#   With parameters array and nextIndex.
# ElementList : Elision AssignmentExpression
#   1. Let padding be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
#   2. Let initResult be the result of evaluating AssignmentExpression.
#   3. Let initValue be ? GetValue(initResult).
#   4. Let created be CreateDataProperty(array, ToString(ToUint32(nextIndex + padding)), initValue).
#   5. Assert: created is true.
#   6. Return nextIndex + padding + 1.
@pytest.mark.parametrize("src, expected_delta, expected_propname", [(",,, 3", 4, "13"), ("78", 1, "10")])
def test_ElementList_ArrayAccumulation_Elision_AssignmentExpression(
    context, mocker, src, expected_delta, expected_propname
):
    lexer = lexer2.Lexer(src)
    el = ecmascript.ecmascript.parse_ElementList(context, lexer, False, False)
    el.AssignmentExpression.evaluate = mocker.Mock(return_value="initResult")
    cdp = mocker.patch("ecmascript.ecmascript.CreateDataProperty", return_value=True)
    gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value="initValue")

    rv = el.ArrayAccumulation("array", 10)
    assert rv == 10 + expected_delta
    cdp.assert_called_with("array", expected_propname, "initValue")
    gv.assert_called_with("initResult")


# 12.2.5.2 Runtime Semantics: ArrayAccumulation
#   With parameters array and nextIndex.
# ElementList : Elision SpreadElement
#   1. Let padding be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
#   2. Return the result of performing ArrayAccumulation for SpreadElement with arguments array and
#      nextIndex + padding.
@pytest.mark.parametrize("src, expected_index", [(",,, ...3", 13), ("...4", 10)])
def test_ElementList_ArrayAccumulation_Elision_SpreadElement(context, mocker, src, expected_index):
    lexer = lexer2.Lexer(src)
    el = ecmascript.ecmascript.parse_ElementList(context, lexer, False, False)
    el.SpreadElement.ArrayAccumulation = mocker.Mock(return_value="AA")

    rv = el.ArrayAccumulation("array", 10)
    assert rv == "AA"
    el.SpreadElement.ArrayAccumulation.assert_called_with("array", expected_index)


# 12.2.5.2 Runtime Semantics: ArrayAccumulation
#   With parameters array and nextIndex.
# ElementList : ElementList , Elision AssignmentExpression
#   1. Let postIndex be the result of performing ArrayAccumulation for ElementList with arguments array and nextIndex.
#   2. ReturnIfAbrupt(postIndex).
#   3. Let padding be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
#   4. Let initResult be the result of evaluating AssignmentExpression.
#   5. Let initValue be ? GetValue(initResult).
#   6. Let created be CreateDataProperty(array, ToString(ToUint32(postIndex + padding)), initValue).
#   7. Assert: created is true.
#   8. Return postIndex + padding + 1.
@pytest.mark.parametrize("src, expected", [("3,4", 12), ("2,,6", 13)])
def test_ElementList_ArrayAccumulation_ElementList_Elision_AssignmentExpression(context, mocker, src, expected):
    lexer = lexer2.Lexer(src)
    el = ecmascript.ecmascript.parse_ElementList(context, lexer, False, False)
    el.ElementList.ArrayAccumulation = mocker.Mock(side_effect=lambda array, nextIndex: nextIndex + 1)
    el.AssignmentExpression.evaluate = mocker.Mock(return_value="initResult")
    gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value="initValue")
    cdp = mocker.patch("ecmascript.ecmascript.CreateDataProperty", return_value=True)

    rv = el.ArrayAccumulation("array", 10)
    assert rv == expected
    el.ElementList.ArrayAccumulation.assert_called_with("array", 10)
    el.AssignmentExpression.evaluate.assert_called_with()
    gv.assert_called_with("initResult")
    cdp.assert_called_with("array", str(expected - 1), "initValue")


# 12.2.5.2 Runtime Semantics: ArrayAccumulation
#   With parameters array and nextIndex.
# ElementList : ElementList , Elision SpreadElement
#   1. Let postIndex be the result of performing ArrayAccumulation for ElementList with arguments array and nextIndex.
#   2. ReturnIfAbrupt(postIndex).
#   3. Let padding be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
#   4. Return the result of performing ArrayAccumulation for SpreadElement with arguments array and
#      postIndex + padding.
@pytest.mark.parametrize("src, delta", [("3,,,...8", 3), ("4,...9", 1)])
def test_ElementList_ArrayAccumulation_ElementList_Elision_SpreadElement(context, mocker, src, delta):
    lexer = lexer2.Lexer(src)
    el = ecmascript.ecmascript.parse_ElementList(context, lexer, False, False)
    el.ElementList.ArrayAccumulation = mocker.Mock(side_effect=lambda array, nextIndex: nextIndex + 1)
    el.SpreadElement.ArrayAccumulation = mocker.Mock(return_value="AA")

    rv = el.ArrayAccumulation("array", 10)
    assert rv == "AA"
    el.ElementList.ArrayAccumulation.assert_called_with("array", 10)
    el.SpreadElement.ArrayAccumulation.assert_called_with("array", 10 + delta)


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
    se = ecmascript.ecmascript.P2_SpreadElement(context, ["child"])
    assert se.name == "SpreadElement"
    assert se.context == context
    assert se.children == ["child"]


def test_P2_SpreadElement_DOTDOTDOT_AssignmentExpression_init(context):
    se = ecmascript.ecmascript.P2_SpreadElement_DOTDOTDOT_AssignmentExpression(context, ["...", "AssignmentExpression"])
    assert se.name == "SpreadElement"
    assert se.AssignmentExpression == "AssignmentExpression"


def SpreadElement_mocks(mocker):
    return {"ae": mocker.patch("ecmascript.ecmascript.parse_AssignmentExpression", side_effect=ae_sideeffect)}


def test_parse_SpreadElement_01(mocker, context):
    # SpreadElement : ... AssignmentExpression
    lexer = Lexer([DOTDOTDOT, AE_REPLACEMENT])
    mocks = SpreadElement_mocks(mocker)

    se = ecmascript.ecmascript.parse_SpreadElement(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(se, ecmascript.ecmascript.P2_SpreadElement_DOTDOTDOT_AssignmentExpression)
    assert se.AssignmentExpression == "AssignmentExpression"
    assert lexer.pos == 2
    mocks["ae"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[MATCHES_NONE], [DOTDOTDOT, MATCHES_NONE]])
def test_parse_SpreadElement_02(mocker, context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)
    SpreadElement_mocks(mocker)

    se = ecmascript.ecmascript.parse_SpreadElement(context, lexer, "YieldArg", "AwaitArg")
    assert se is None
    assert lexer.pos == 0


# 12.2.5.2 Runtime Semantics: ArrayAccumulation
#   With parameters array and nextIndex.
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
@pytest.mark.parametrize("size", [0, 1, 3])
def test_SpreadElement_ArrayAccumulation(context, mocker, size):
    lexer = lexer2.Lexer("...8")
    se = ecmascript.ecmascript.parse_SpreadElement(context, lexer, False, False)

    se.AssignmentExpression.evaluate = mocker.Mock(return_value="spreadRef")
    gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value="spreadObj")
    gi = mocker.patch("ecmascript.ecmascript.GetIterator", return_value="iteratorRecord")
    gen = ((z,) for z in range(size))
    mystep = lambda iteratorRecord: next(gen, False)
    its = mocker.patch("ecmascript.ecmascript.IteratorStep", side_effect=mystep)
    myvalue = lambda nextresult: str(nextresult[0])
    itv = mocker.patch("ecmascript.ecmascript.IteratorValue", side_effect=myvalue)
    cdp = mocker.patch("ecmascript.ecmascript.CreateDataProperty", return_value=True)

    rv = se.ArrayAccumulation("array", 10)
    se.AssignmentExpression.evaluate.assert_called_with()
    gv.assert_called_with("spreadRef")
    gi.assert_called_with("spreadObj")
    assert its.call_args_list == [mocker.call("iteratorRecord")] * (size + 1)
    assert itv.call_args_list == [mocker.call((x,)) for x in range(size)]
    assert cdp.call_args_list == [mocker.call("array", str(x + 10), str(x)) for x in range(size)]
    assert rv == size + 10


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
    ol = ecmascript.ecmascript.P2_ObjectLiteral(context, ["child"])
    assert ol.name == "ObjectLiteral"
    assert ol.context == context
    assert ol.children == ["child"]


def test_P2_ObjectLiteral_LCURLY_RCURLY_init(context):
    ol = ecmascript.ecmascript.P2_ObjectLiteral_LCURLY_RCURLY(context, ["{", "}"])
    assert ol.name == "ObjectLiteral"


def test_P2_ObjectLiteral_LCURLY_PropertyDefinitionList_RCURLY_init(context):
    ol = ecmascript.ecmascript.P2_ObjectLiteral_LCURLY_PropertyDefinitionList_RCURLY(
        context, ["{", "PropertyDefinitionList", "}"]
    )
    assert ol.name == "ObjectLiteral"
    assert ol.PropertyDefinitionList == "PropertyDefinitionList"


def test_P2_ObjectLiteral_LCURLY_PropertyDefinitionList_COMMA_RCURLY_init(context):
    ol = ecmascript.ecmascript.P2_ObjectLiteral_LCURLY_PropertyDefinitionList_COMMA_RCURLY(
        context, ["{", "PropertyDefinitionList", ",", "}"]
    )
    assert ol.name == "ObjectLiteral"
    assert ol.PropertyDefinitionList == "PropertyDefinitionList"


def ObjectLiteral_mocks(mocker):
    return {"pdl": mocker.patch("ecmascript.ecmascript.parse_PropertyDefinitionList", side_effect=pdl_sideeffect)}


def test_parse_ObjectLiteral_01(mocker, context):
    # ObjectLiteral : { }
    lexer = Lexer([LCURLY, RCURLY])
    ObjectLiteral_mocks(mocker)

    ol = ecmascript.ecmascript.parse_ObjectLiteral(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(ol, ecmascript.ecmascript.P2_ObjectLiteral_LCURLY_RCURLY)
    assert lexer.pos == 2


@pytest.mark.parametrize(
    "token_stream, expected_class",
    [
        (
            [LCURLY, PDL_REPLACEMENT, RCURLY],
            ecmascript.ecmascript.P2_ObjectLiteral_LCURLY_PropertyDefinitionList_RCURLY,
        ),
        (
            [LCURLY, PDL_REPLACEMENT, COMMA, RCURLY],
            ecmascript.ecmascript.P2_ObjectLiteral_LCURLY_PropertyDefinitionList_COMMA_RCURLY,
        ),
    ],
)
def test_parse_ObjectLiteral_02(mocker, context, token_stream, expected_class):
    # ObjectLiteral : { PropertyDefinitionList }
    lexer = Lexer(token_stream)
    mocks = ObjectLiteral_mocks(mocker)

    ol = ecmascript.ecmascript.parse_ObjectLiteral(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(ol, expected_class)
    assert ol.PropertyDefinitionList == "PropertyDefinitionList"
    assert lexer.pos == len(token_stream)
    mocks["pdl"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "token_stream",
    [
        [MATCHES_NONE],
        [LCURLY, MATCHES_NONE],
        [LCURLY, PDL_REPLACEMENT, MATCHES_NONE],
        [LCURLY, PDL_REPLACEMENT, COMMA, MATCHES_NONE],
    ],
)
def test_parse_ObjectLiteral_03(mocker, context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)
    ObjectLiteral_mocks(mocker)

    ol = ecmascript.ecmascript.parse_ObjectLiteral(context, lexer, "YieldArg", "AwaitArg")
    assert ol is None
    assert lexer.pos == 0


# 12.2.6.7 Runtime Semantics: Evaluation
# ObjectLiteral : { }
#   1. Return ObjectCreate(%ObjectPrototype%).
def test_ObjectLiteral_LCURLY_RCURLY_evaluate(realm, context, mocker):
    lexer = lexer2.Lexer("{}")
    ol = ecmascript.ecmascript.parse_ObjectLiteral(context, lexer, False, False)

    oc = mocker.patch("ecmascript.ecmascript.ObjectCreate", return_value="object_create")
    proto = realm.intrinsics["%ObjectPrototype%"]
    rv = ol.evaluate()
    oc.assert_called_with(proto)
    assert rv == "object_create"


# 12.2.6.7 Runtime Semantics: Evaluation
# ObjectLiteral : { PropertyDefinitionList }
# ObjectLiteral : { PropertyDefinitionList , }
#   1. Let obj be ObjectCreate(%ObjectPrototype%).
#   2. Perform ? PropertyDefinitionEvaluation of PropertyDefinitionList with arguments obj and true.
#   3. Return obj.
@pytest.mark.parametrize("src", ["{a}", "{a,}"])
def test_ObjectLiteral_PropertyDefinitionList_evaluate(realm, context, mocker, src):
    lexer = lexer2.Lexer(src)
    ol = ecmascript.ecmascript.parse_ObjectLiteral(context, lexer, False, False)
    oc = mocker.patch("ecmascript.ecmascript.ObjectCreate", return_value="object_create")
    ol.PropertyDefinitionList.PropertyDefinitionEvaluation = mocker.Mock(return_value=None)

    rv = ol.evaluate()
    assert rv == "object_create"
    oc.assert_called_with(realm.intrinsics["%ObjectPrototype%"])
    ol.PropertyDefinitionList.PropertyDefinitionEvaluation.assert_called_with("object_create", True)


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
    pdl = ecmascript.ecmascript.P2_PropertyDefinitionList(context, ["child"])
    assert pdl.name == "PropertyDefinitionList"
    assert pdl.context == context
    assert pdl.children == ["child"]


def test_P2_PropertyDefinitionList_PropertyDefinition_init(context):
    pdl = ecmascript.ecmascript.P2_PropertyDefinitionList_PropertyDefinition(context, ["PropertyDefinition"])
    assert pdl.name == "PropertyDefinitionList"
    assert pdl.PropertyDefinition == "PropertyDefinition"


def test_P2_PropertyDefinitionList_PropertyDefinitionList_COMMA_PropertyDefinition_init(context):
    pdl = ecmascript.ecmascript.P2_PropertyDefinitionList_PropertyDefinitionList_COMMA_PropertyDefinition(
        context, ["PropertyDefinitionList", ",", "PropertyDefinition"]
    )
    assert pdl.name == "PropertyDefinitionList"
    assert pdl.PropertyDefinitionList == "PropertyDefinitionList"
    assert pdl.PropertyDefinition == "PropertyDefinition"


def PropertyDefinitionList_mocks(mocker):
    return {"pd": mocker.patch("ecmascript.ecmascript.parse_PropertyDefinition", side_effect=pd_sideeffect)}


def test_parse_PropertyDefinitionList_01(mocker, context):
    # PropertyDefinitionList : PropertyDefinition
    lexer = Lexer([PD_REPLACEMENT])
    mocks = PropertyDefinitionList_mocks(mocker)

    pdl = ecmascript.ecmascript.parse_PropertyDefinitionList(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(pdl, ecmascript.ecmascript.P2_PropertyDefinitionList_PropertyDefinition)
    assert pdl.PropertyDefinition == "PropertyDefinition"
    assert lexer.pos == 1
    mocks["pd"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")


def test_parse_PropertyDefinitionList_02(mocker, context):
    # PropertyDefinitionList : PropertyDefinitionList , PropertyDefinition
    lexer = Lexer([PD_REPLACEMENT, COMMA, PD_REPLACEMENT])
    mocks = PropertyDefinitionList_mocks(mocker)

    pdl = ecmascript.ecmascript.parse_PropertyDefinitionList(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(
        pdl, ecmascript.ecmascript.P2_PropertyDefinitionList_PropertyDefinitionList_COMMA_PropertyDefinition
    )
    assert pdl.PropertyDefinition == "PropertyDefinition"
    assert lexer.pos == 3
    pdl2 = pdl.PropertyDefinitionList
    assert isinstance(pdl2, ecmascript.ecmascript.P2_PropertyDefinitionList_PropertyDefinition)
    mocks["pd"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")


def test_parse_PropertyDefinitionList_03(mocker, context):
    # Non-recursive syntax error
    lexer = Lexer([MATCHES_NONE])
    PropertyDefinitionList_mocks(mocker)

    pdl = ecmascript.ecmascript.parse_PropertyDefinitionList(context, lexer, "YieldArg", "AwaitArg")
    assert pdl is None
    assert lexer.pos == 0


def test_parse_PropertyDefinitionList_04(mocker, context):
    # Recursive syntax error
    lexer = Lexer([PD_REPLACEMENT, COMMA, MATCHES_NONE])
    PropertyDefinitionList_mocks(mocker)

    pdl = ecmascript.ecmascript.parse_PropertyDefinitionList(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(pdl, ecmascript.ecmascript.P2_PropertyDefinitionList_PropertyDefinition)
    assert lexer.pos == 1


# 12.2.6.6 Static Semantics: PropertyNameList
# PropertyDefinitionList : PropertyDefinition
#   1. If PropName of PropertyDefinition is empty, return a new empty List.
#   2. Return a new List containing PropName of PropertyDefinition.
# PropertyDefinitionList : PropertyDefinitionList , PropertyDefinition
#   1. Let list be PropertyNameList of PropertyDefinitionList.
#   2. If PropName of PropertyDefinition is empty, return list.
#   3. Append PropName of PropertyDefinition to the end of list.
#   4. Return list.
@pytest.mark.parametrize(
    "src, expected",
    [("...3", []), ("id", ["id"]), ("blue, ...4", ["blue"]), ("blue, red", ["blue", "red"]), ("...4, ...88", [])],
)
def test_PropertyDefinitionList_PropertyNameList(context, src, expected):
    lexer = lexer2.Lexer(src)
    pdl = ecmascript.ecmascript.parse_PropertyDefinitionList(context, lexer, False, False)
    assert pdl.PropertyNameList() == expected


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
    pd = ecmascript.ecmascript.P2_PropertyDefinition(context, ["child"])
    assert pd.name == "PropertyDefinition"
    assert pd.context == context
    assert pd.children == ["child"]


def test_P2_PropertyDefinition_IdentifierReference_init(context):
    pd = ecmascript.ecmascript.P2_PropertyDefinition_IdentifierReference(context, ["IdentifierReference"])
    assert pd.name == "PropertyDefinition"
    assert pd.IdentifierReference == "IdentifierReference"


def test_P2_PropertyDefinition_CoverInitializedName_init(context):
    pd = ecmascript.ecmascript.P2_PropertyDefinition_CoverInitializedName(context, ["CoverInitializedName"])
    assert pd.name == "PropertyDefinition"
    assert pd.CoverInitializedName == "CoverInitializedName"


def test_P2_PropertyDefinition_PropertyName_COLON_AssignmentExpression_init(context):
    pd = ecmascript.ecmascript.P2_PropertyDefinition_PropertyName_COLON_AssignmentExpression(
        context, ["PropertyName", ":", "AssignmentExpression"]
    )
    assert pd.name == "PropertyDefinition"
    assert pd.PropertyName == "PropertyName"
    assert pd.AssignmentExpression == "AssignmentExpression"


def test_P2_PropertyDefinition_MethodDefinition_init(context):
    pd = ecmascript.ecmascript.P2_PropertyDefinition_MethodDefinition(context, ["MethodDefinition"])
    assert pd.name == "PropertyDefinition"
    assert pd.MethodDefinition == "MethodDefinition"


def test_P2_PropertyDefinition_DOTDOTDOT_AssignmentExpression_init(context):
    pd = ecmascript.ecmascript.P2_PropertyDefinition_DOTDOTDOT_AssignmentExpression(
        context, ["...", "AssignmentExpression"]
    )
    assert pd.name == "PropertyDefinition"
    assert pd.AssignmentExpression == "AssignmentExpression"


def PropertyDefinition_mocks(mocker):
    return {
        "ir": mocker.patch("ecmascript.ecmascript.parse_IdentifierReference", side_effect=ir_sideeffect),
        "cin": mocker.patch("ecmascript.ecmascript.parse_CoverInitializedName", side_effect=cin_sideeffect),
        "pn": mocker.patch("ecmascript.ecmascript.parse_PropertyName", side_effect=pn_sideeffect),
        "ae": mocker.patch("ecmascript.ecmascript.parse_AssignmentExpression", side_effect=ae_sideeffect),
        "md": mocker.patch("ecmascript.ecmascript.parse_MethodDefinition", side_effect=md_sideeffect),
    }


def test_parse_PropertyDefinition_01(mocker, context):
    # PropertyDefinition : IdentifierReference
    lexer = Lexer([IR_REPLACEMENT])
    mocks = PropertyDefinition_mocks(mocker)

    pd = ecmascript.ecmascript.parse_PropertyDefinition(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(pd, ecmascript.ecmascript.P2_PropertyDefinition_IdentifierReference)
    assert pd.IdentifierReference == "IdentifierReference"
    assert lexer.pos == 1
    mocks["ir"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")


def test_parse_PropertyDefinition_02(mocker, context):
    # PropertyDefinition : CoverInitializedName
    lexer = Lexer([CIN_REPLACEMENT])
    mocks = PropertyDefinition_mocks(mocker)

    pd = ecmascript.ecmascript.parse_PropertyDefinition(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(pd, ecmascript.ecmascript.P2_PropertyDefinition_CoverInitializedName)
    assert pd.CoverInitializedName == "CoverInitializedName"
    assert lexer.pos == 1
    mocks["cin"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")


def test_parse_PropertyDefinition_03(mocker, context):
    # PropertyDefinition : PropertyName : AssignmentExpression
    lexer = Lexer([PN_REPLACEMENT, COLON, AE_REPLACEMENT])
    mocks = PropertyDefinition_mocks(mocker)

    pd = ecmascript.ecmascript.parse_PropertyDefinition(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(pd, ecmascript.ecmascript.P2_PropertyDefinition_PropertyName_COLON_AssignmentExpression)
    assert pd.PropertyName == "PropertyName"
    assert pd.AssignmentExpression == "AssignmentExpression"
    assert lexer.pos == 3
    mocks["pn"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")
    mocks["ae"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")


def test_parse_PropertyDefinition_04(mocker, context):
    # PropertyDefinition : MethodDefinition
    lexer = Lexer([MD_REPLACEMENT])
    mocks = PropertyDefinition_mocks(mocker)

    pd = ecmascript.ecmascript.parse_PropertyDefinition(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(pd, ecmascript.ecmascript.P2_PropertyDefinition_MethodDefinition)
    assert pd.MethodDefinition == "MethodDefinition"
    assert lexer.pos == 1
    mocks["md"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")


def test_parse_PropertyDefinition_05(mocker, context):
    # PropertyDefinition : ... AssignmentExpression
    lexer = Lexer([DOTDOTDOT, AE_REPLACEMENT])
    mocks = PropertyDefinition_mocks(mocker)

    pd = ecmascript.ecmascript.parse_PropertyDefinition(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(pd, ecmascript.ecmascript.P2_PropertyDefinition_DOTDOTDOT_AssignmentExpression)
    assert pd.AssignmentExpression == "AssignmentExpression"
    assert lexer.pos == 2
    mocks["ae"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "token_stream",
    [[MATCHES_NONE], [PN_REPLACEMENT, MATCHES_NONE], [PN_REPLACEMENT, COLON, MATCHES_NONE], [DOTDOTDOT, MATCHES_NONE]],
)
def test_parse_PropertyDefinition_06(mocker, context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)
    PropertyDefinition_mocks(mocker)

    pd = ecmascript.ecmascript.parse_PropertyDefinition(context, lexer, "YieldArg", "AwaitArg")
    assert pd is None
    assert lexer.pos == 0


# 12.2.6.1 Static Semantics: Early Errors
# PropertyDefinition : MethodDefinition
#   * It is a Syntax Error if HasDirectSuper of MethodDefinition is true.
@pytest.mark.parametrize("has_super, count", [(False, 0), (True, 1)])
def test_PropertyDefinition_MethodDefinition_EarlyErrors(context, mocker, has_super, count):
    lexer = lexer2.Lexer("a(){}")
    pd = ecmascript.ecmascript.parse_PropertyDefinition(context, lexer, False, False)
    pd.MethodDefinition.HasDirectSuper = mocker.Mock(return_value=has_super)

    rv = pd.EarlyErrors()
    expected = [Expected_Exception] * count
    actual = [type(err) for err in rv]
    assert actual == expected


# 12.2.6.1 Static Semantics: Early Errors
# PropertyDefinition : CoverInitializedName
#   * Always throw a Syntax Error if code matches this production.
def test_PropertyDefinition_CoverInitializedName(context):
    lexer = lexer2.Lexer("a=1")
    pd = ecmascript.ecmascript.parse_PropertyDefinition(context, lexer, False, False)

    rv = pd.EarlyErrors()
    actual = [type(err) for err in rv]
    assert actual == [Expected_Exception]


# 12.2.6.3 Static Semantics: Contains
#   With parameter symbol.
# PropertyDefinition : MethodDefinition
#   1. If symbol is MethodDefinition, return true.
#   2. Return the result of ComputedPropertyContains for MethodDefinition with argument symbol.
@pytest.mark.parametrize("symbol, expected", [("MethodDefinition", True), ("OtherSymbol", "cpc")])
def test_PropertyDefinition_MethodDefinition_Contains(context, mocker, symbol, expected):
    lexer = lexer2.Lexer("a(){}")
    pd = ecmascript.ecmascript.parse_PropertyDefinition(context, lexer, False, False)
    pd.MethodDefinition.ComputedPropertyContains = mocker.Mock(return_value="cpc")

    rv = pd.Contains(symbol)
    assert rv == expected
    if rv == "cpc":
        pd.MethodDefinition.ComputedPropertyContains.assert_called_with(symbol)


# 12.2.6.5 Static Semantics: PropName
# PropertyDefinition : IdentifierReference
#   1. Return StringValue of IdentifierReference.
# PropertyDefinition : ... AssignmentExpression
#   1. Return empty.
# PropertyDefinition : PropertyName : AssignmentExpression
#   1. Return PropName of PropertyName.
@pytest.mark.parametrize(
    "src, expected",
    [("identifier_ref", "identifier_ref"), ("... 67", ecmascript.ecmascript.EMPTY), ('bluegill: "5 lbs."', "bluegill")],
)
def test_PropertyDefinition_PropName(context, src, expected):
    lexer = lexer2.Lexer(src)
    pd = ecmascript.ecmascript.parse_PropertyDefinition(context, lexer, False, False)
    rv = pd.PropName()
    assert rv == expected


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
    pn = ecmascript.ecmascript.P2_PropertyName(context, ["child"])
    assert pn.name == "PropertyName"
    assert pn.context == context
    assert pn.children == ["child"]


def test_P2_PropertyName_LiteralPropertyName_init(context):
    pn = ecmascript.ecmascript.P2_PropertyName_LiteralPropertyName(context, ["LiteralPropertyName"])
    assert pn.name == "PropertyName"
    assert pn.LiteralPropertyName == "LiteralPropertyName"


def test_P2_PropertyName_ComputedPropertyName_init(context):
    pn = ecmascript.ecmascript.P2_PropertyName_ComputedPropertyName(context, ["ComputedPropertyName"])
    assert pn.name == "PropertyName"
    assert pn.ComputedPropertyName == "ComputedPropertyName"


def PropertyName_mocks(mocker):
    return {
        "lpn": mocker.patch("ecmascript.ecmascript.parse_LiteralPropertyName", side_effect=lpn_sideeffect),
        "cpn": mocker.patch("ecmascript.ecmascript.parse_ComputedPropertyName", side_effect=cpn_sideeffect),
    }


def test_parse_PropertyName_01(mocker, context):
    # PropertyName : LiteralPropertyName
    lexer = Lexer([LPN_REPLACEMENT])
    mocks = PropertyName_mocks(mocker)

    pn = ecmascript.ecmascript.parse_PropertyName(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(pn, ecmascript.ecmascript.P2_PropertyName_LiteralPropertyName)
    assert pn.LiteralPropertyName == "LiteralPropertyName"
    assert lexer.pos == 1
    mocks["lpn"].assert_called_with(context, lexer)


def test_parse_PropertyName_02(mocker, context):
    # PropertyName : ComputedPropertyName
    lexer = Lexer([CPN_REPLACEMENT])
    mocks = PropertyName_mocks(mocker)

    pn = ecmascript.ecmascript.parse_PropertyName(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(pn, ecmascript.ecmascript.P2_PropertyName_ComputedPropertyName)
    assert pn.ComputedPropertyName == "ComputedPropertyName"
    assert lexer.pos == 1
    mocks["cpn"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")


def test_parse_PropertyName_03(mocker, context):
    # Syntax Error
    lexer = Lexer([MATCHES_NONE])
    PropertyName_mocks(mocker)

    pn = ecmascript.ecmascript.parse_PropertyName(context, lexer, "YieldArg", "AwaitArg")
    assert pn is None
    assert lexer.pos == 0


# 12.2.6.2 Static Semantics: ComputedPropertyContains
#   With parameter symbol.
# PropertyName : LiteralPropertyName
#   1. Return false.
def test_PropertyName_LiteralPropertyName_ComputedPropertyContains(context):
    lexer = lexer2.Lexer("1")
    pn = ecmascript.ecmascript.parse_PropertyName(context, lexer, False, False)
    rv = pn.ComputedPropertyContains("Nonsense")
    assert rv == False


# 12.2.6.2 Static Semantics: ComputedPropertyContains
#   With parameter symbol.
# PropertyName : ComputedPropertyName
#   1. Return the result of ComputedPropertyName Contains symbol.
def test_PropertyName_ComputedPropertyName_ComputedPropertyContains(context, mocker):
    lexer = lexer2.Lexer("[1]")
    pn = ecmascript.ecmascript.parse_PropertyName(context, lexer, False, False)
    pn.ComputedPropertyName.Contains = mocker.Mock(return_value="contained")
    rv = pn.ComputedPropertyContains("Nonsense")
    assert rv == "contained"
    pn.ComputedPropertyName.Contains.assert_called_with("Nonsense")


# 12.2.6.4 Static Semantics: IsComputedPropertyKey
# PropertyName : LiteralPropertyName
#   1. Return false.
# PropertyName : ComputedPropertyName
#   1. Return true.
@pytest.mark.parametrize("src, expected", [("a", False), ("[1]", True)])
def test_PropertyName_IsComputedPropertyKey(context, src, expected):
    lexer = lexer2.Lexer(src)
    pn = ecmascript.ecmascript.parse_PropertyName(context, lexer, False, False)
    rv = pn.IsComputedPropertyKey()
    assert rv == expected


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
    lpn = ecmascript.ecmascript.P2_LiteralPropertyName(context, ["child"])
    assert lpn.name == "LiteralPropertyName"
    assert lpn.context == context
    assert lpn.children == ["child"]


def test_P2_LiteralPropertyName_IdentifierName_init(context):
    lpn = ecmascript.ecmascript.P2_LiteralPropertyName_IdentifierName(context, ["IdentifierName"])
    assert lpn.name == "LiteralPropertyName"
    assert lpn.IdentifierName == "IdentifierName"


def test_P2_LiteralPropertyName_StringLiteral_init(context):
    lpn = ecmascript.ecmascript.P2_LiteralPropertyName_StringLiteral(context, ["StringLiteral"])
    assert lpn.name == "LiteralPropertyName"
    assert lpn.StringLiteral == "StringLiteral"


def test_P2_LiteralPropertyName_NumericLiteral_init(context):
    lpn = ecmascript.ecmascript.P2_LiteralPropertyName_NumericLiteral(context, ["NumericLiteral"])
    assert lpn.name == "LiteralPropertyName"
    assert lpn.NumericLiteral == "NumericLiteral"


def test_parse_LiteralPropertyName_01(context):
    # LiteralPropertyName : IdentifierName
    lexer = Lexer([IBOB])

    lpn = ecmascript.ecmascript.parse_LiteralPropertyName(context, lexer)
    assert isinstance(lpn, ecmascript.ecmascript.P2_LiteralPropertyName_IdentifierName)
    assert lpn.IdentifierName == IBOB
    assert lexer.pos == 1


def test_parse_LiteralPropertyName_02(context):
    # LiteralPropertyName : StringLiteral
    lexer = Lexer([SALICE])

    lpn = ecmascript.ecmascript.parse_LiteralPropertyName(context, lexer)
    assert isinstance(lpn, ecmascript.ecmascript.P2_LiteralPropertyName_StringLiteral)
    assert lpn.StringLiteral == SALICE
    assert lexer.pos == 1


def test_parse_LiteralPropertyName_03(context):
    # LiteralPropertyName : NumericLiteral
    lexer = Lexer([THREE])

    lpn = ecmascript.ecmascript.parse_LiteralPropertyName(context, lexer)
    assert isinstance(lpn, ecmascript.ecmascript.P2_LiteralPropertyName_NumericLiteral)
    assert lpn.NumericLiteral == THREE
    assert lexer.pos == 1


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_LiteralPropertyName_04(context, token_stream):
    lexer = Lexer(token_stream)

    lpn = ecmascript.ecmascript.parse_LiteralPropertyName(context, lexer)
    assert lpn is None
    assert lexer.pos == 0


# 12.2.6.3 Static Semantics: Contains
#   With parameter symbol.
# LiteralPropertyName : IdentifierName
#   1. If symbol is a ReservedWord, return false.
#   2. If symbol is an Identifier and StringValue of symbol is the same value as the StringValue of IdentifierName,
#      return true.
#   3. Return false.
@pytest.mark.parametrize(
    "src, symbol, expected",
    [pytest.param(rw, rw, False, id=rw) for rw in lexer2.Lexer.reserved_words]
    + [
        ("IdentifierName", "IdentifierName", True),
        ("IdentifierName", "bob", False),
        ("Identifier\\u004eame", "IdentifierName", True),
    ],
)
def test_LiteralPropertyName_IdentifierName_Contains(context, src, symbol, expected):
    lexer = lexer2.Lexer(src)
    lpn = ecmascript.ecmascript.parse_LiteralPropertyName(context, lexer)

    rv = lpn.Contains(symbol)
    assert rv == expected


# 12.2.6.5 Static Semantics: PropName
# LiteralPropertyName : IdentifierName
#   1. Return StringValue of IdentifierName.
# LiteralPropertyName : StringLiteral
#   1. Return the String value whose code units are the SV of the StringLiteral.
# LiteralPropertyName : NumericLiteral
#   1. Let nbr be the result of forming the value of the NumericLiteral.
#   2. Return ! ToString(nbr).
@pytest.mark.parametrize("src, expected", [("idn", "idn"), ('"whitetail"', "whitetail"), ("1000", "1000")])
def test_LiteralPropertyName_PropName(context, src, expected):
    lexer = lexer2.Lexer(src)
    lpn = ecmascript.ecmascript.parse_LiteralPropertyName(context, lexer)
    rv = lpn.PropName()
    assert rv == expected


# 12.2.6.7 Runtime Semantics: Evaluation
# LiteralPropertyName : IdentifierName
#   1. Return StringValue of IdentifierName.
# LiteralPropertyName : StringLiteral
#   1. Return the String value whose code units are the SV of the StringLiteral.
# LiteralPropertyName : NumericLiteral
#   1. Let nbr be the result of forming the value of the NumericLiteral.
#   2. Return ! ToString(nbr).
@pytest.mark.parametrize("src, expected", [("idn", "idn"), ('"whitetail"', "whitetail"), ("1000", "1000")])
def test_LiteralPropertyName_evaluate(context, src, expected):
    lexer = lexer2.Lexer(src)
    lpn = ecmascript.ecmascript.parse_LiteralPropertyName(context, lexer)
    rv = lpn.evaluate()
    assert rv == expected


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
    cpn = ecmascript.ecmascript.P2_ComputedPropertyName(context, ["child"])
    assert cpn.name == "ComputedPropertyName"
    assert cpn.context == context
    assert cpn.children == ["child"]


def test_P2_ComputedPropertyName_LBRACKET_AssignmentExpression_RBRACKET_init(context):
    cpn = ecmascript.ecmascript.P2_ComputedPropertyName_LBRACKET_AssignmentExpression_RBRACKET(
        context, ["[", "AssignmentExpression", "]"]
    )
    assert cpn.name == "ComputedPropertyName"
    assert cpn.AssignmentExpression == "AssignmentExpression"


def ComputedPropertyName_mocks(mocker):
    return {"ae": mocker.patch("ecmascript.ecmascript.parse_AssignmentExpression", side_effect=ae_sideeffect)}


def test_parse_ComputedPropertyName_01(mocker, context):
    # ComputedPropertyName : [ AssignmentExpression ]
    lexer = Lexer([LBRACKET, AE_REPLACEMENT, RBRACKET])
    mocks = ComputedPropertyName_mocks(mocker)

    cpn = ecmascript.ecmascript.parse_ComputedPropertyName(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(cpn, ecmascript.ecmascript.P2_ComputedPropertyName_LBRACKET_AssignmentExpression_RBRACKET)
    assert cpn.AssignmentExpression == "AssignmentExpression"
    assert lexer.pos == 3
    mocks["ae"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "token_stream", [[], [MATCHES_NONE], [LBRACKET, MATCHES_NONE], [LBRACKET, AE_REPLACEMENT, MATCHES_NONE]]
)
def test_parse_ComputedPropertyName_02(mocker, context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)
    ComputedPropertyName_mocks(mocker)

    cpn = ecmascript.ecmascript.parse_ComputedPropertyName(context, lexer, "YieldArg", "AwaitArg")
    assert cpn is None
    assert lexer.pos == 0


# 12.2.6.5 Static Semantics: PropName
# ComputedPropertyName : [ AssignmentExpression ]
#   1. Return empty.
def test_ComputedPropertyName_PropName(context):
    lexer = lexer2.Lexer("[1]")
    cpn = ecmascript.ecmascript.parse_ComputedPropertyName(context, lexer, False, False)
    assert cpn.PropName() == ecmascript.ecmascript.EMPTY


# 12.2.6.7 Runtime Semantics: Evaluation
# ComputedPropertyName : [ AssignmentExpression ]
#   1. Let exprValue be the result of evaluating AssignmentExpression.
#   2. Let propName be ? GetValue(exprValue).
#   3. Return ? ToPropertyKey(propName).
def test_ComputedPropertyName_evaluate(realm, context, mocker):
    lexer = lexer2.Lexer("[1]")
    cpn = ecmascript.ecmascript.parse_ComputedPropertyName(context, lexer, False, False)
    cpn.AssignmentExpression.evaluate = mocker.Mock(return_value="exprValue")
    gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value="propName")
    tpk = mocker.patch("ecmascript.ecmascript.ToPropertyKey", return_value="evaluate")

    rv = cpn.evaluate()
    cpn.AssignmentExpression.evaluate.assert_called_with()
    gv.assert_called_with("exprValue")
    tpk.assert_called_with("propName")
    assert rv == "evaluate"


@pytest.mark.parametrize("src, expected", [("[1]", "1"), ("[67*4-12]", "256"), ("[`prop${8*12}z`]", "prop96z")])
def test_ComputedPropertyName_evaluate_nomocks(realm, context, src, expected):
    lexer = lexer2.Lexer(src)
    cpn = ecmascript.ecmascript.parse_ComputedPropertyName(context, lexer, False, False)

    rv = cpn.evaluate()
    assert rv == expected


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
    cin = ecmascript.ecmascript.P2_CoverInitializedName(context, ["child"])
    assert cin.name == "CoverInitializedName"
    assert cin.context == context
    assert cin.children == ["child"]


def test_P2_CoverInitializedName_IdentifierReference_Initializer_init(context):
    cin = ecmascript.ecmascript.P2_CoverInitializedName_IdentifierReference_Initializer(
        context, ["IdentifierReference", "Initializer"]
    )
    assert cin.name == "CoverInitializedName"
    assert cin.IdentifierReference == "IdentifierReference"
    assert cin.Initializer == "Initializer"


def CoverInitializedName_mocks(mocker):
    return {
        "ir": mocker.patch("ecmascript.ecmascript.parse_IdentifierReference", side_effect=ir_sideeffect),
        "init": mocker.patch("ecmascript.ecmascript.parse_Initializer", side_effect=init_sideeffect),
    }


def test_parse_CoverInitializedName_01(mocker, context):
    # CoverInitializedName : IdentiferReference Initializer
    lexer = Lexer([IR_REPLACEMENT, INIT_REPLACEMENT])
    mocks = CoverInitializedName_mocks(mocker)

    cin = ecmascript.ecmascript.parse_CoverInitializedName(context, lexer, "YieldArg", "AwaitArg")
    assert isinstance(cin, ecmascript.ecmascript.P2_CoverInitializedName_IdentifierReference_Initializer)
    assert cin.IdentifierReference == "IdentifierReference"
    assert cin.Initializer == "Initializer"
    assert lexer.pos == 2
    mocks["ir"].assert_called_with(context, lexer, "YieldArg", "AwaitArg")
    mocks["init"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE], [IR_REPLACEMENT, MATCHES_NONE]])
def test_parse_CoverInitializedName_02(mocker, context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)
    CoverInitializedName_mocks(mocker)

    cin = ecmascript.ecmascript.parse_CoverInitializedName(context, lexer, "YieldArg", "AwaitArg")
    assert cin is None
    assert lexer.pos == 0


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
    init = ecmascript.ecmascript.P2_Initializer(context, ["child"])
    assert init.name == "Initializer"
    assert init.context == context
    assert init.children == ["child"]


def test_P2_Initializer_EQUALS_AssignmentExpression_init(context):
    init = ecmascript.ecmascript.P2_Initializer_EQUALS_AssignmentExpression(context, ["=", "AssignmentExpression"])
    assert init.name == "Initializer"
    assert init.AssignmentExpression == "AssignmentExpression"


def Initializer_mocks(mocker):
    return {"ae": mocker.patch("ecmascript.ecmascript.parse_AssignmentExpression", side_effect=ae_sideeffect)}


def test_parse_Initializer_01(mocker, context):
    # Initalizer : = AssignmentExpression
    lexer = Lexer([EQ, AE_REPLACEMENT])
    mocks = Initializer_mocks(mocker)

    init = ecmascript.ecmascript.parse_Initializer(context, lexer, "InArg", "YieldArg", "AwaitArg")
    assert isinstance(init, ecmascript.ecmascript.P2_Initializer_EQUALS_AssignmentExpression)
    assert init.AssignmentExpression == "AssignmentExpression"
    assert lexer.pos == 2
    mocks["ae"].assert_called_with(context, lexer, "InArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE], [EQ, MATCHES_NONE]])
def test_parse_Initializer_02(mocker, context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)
    Initializer_mocks(mocker)

    init = ecmascript.ecmascript.parse_Initializer(context, lexer, "InArg", "YieldArg", "AwaitArg")
    assert init is None
    assert lexer.pos == 0


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
    tl = ecmascript.ecmascript.P2_TemplateLiteral(context, ["child"], "TaggedArg")
    assert tl.name == "TemplateLiteral"
    assert tl.context == context
    assert tl.children == ["child"]
    assert tl.Tagged == "TaggedArg"


def test_P2_TemplateLiteral_NoSubstitutionTemplate_init(context):
    tl = ecmascript.ecmascript.P2_TemplateLiteral_NoSubstitutionTemplate(context, ["NST"], "T")
    assert tl.name == "TemplateLiteral"
    assert tl.NoSubstitutionTemplate == "NST"


def test_P2_TemplateLiteral_SubstitutionTemplate_init(context):
    tl = ecmascript.ecmascript.P2_TemplateLiteral_SubstitutionTemplate(context, ["SubTemp"], "T")
    assert tl.name == "TemplateLiteral"
    assert tl.SubstitutionTemplate == "SubTemp"


def TemplateLiteral_mocks(mocker):
    return {"st": mocker.patch("ecmascript.ecmascript.parse_SubstitutionTemplate", side_effect=st_sideeffect)}


def test_parse_TemplateLiteral_01(mocker, context):
    # TemplateLiteral : NoSubstitutionTemplate
    lexer = Lexer([NST])
    TemplateLiteral_mocks(mocker)

    tl = ecmascript.ecmascript.parse_TemplateLiteral(context, lexer, "YieldArg", "AwaitArg", "TaggedArg")
    assert isinstance(tl, ecmascript.ecmascript.P2_TemplateLiteral_NoSubstitutionTemplate)
    assert tl.NoSubstitutionTemplate == NST
    assert lexer.pos == 1


def test_parse_TemplateLiteral_02(mocker, context):
    # TemplateLiteral : SubstitutionTemplate
    lexer = Lexer([ST_REPLACEMENT])
    mocks = TemplateLiteral_mocks(mocker)

    tl = ecmascript.ecmascript.parse_TemplateLiteral(context, lexer, "YieldArg", "AwaitArg", "TaggedArg")
    assert isinstance(tl, ecmascript.ecmascript.P2_TemplateLiteral_SubstitutionTemplate)
    assert tl.SubstitutionTemplate == "SubstitutionTemplate"
    assert lexer.pos == 1
    mocks["st"].assert_called_with(context, lexer, "YieldArg", "AwaitArg", "TaggedArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_TemplateLiteral_03(mocker, context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)
    TemplateLiteral_mocks(mocker)

    tl = ecmascript.ecmascript.parse_TemplateLiteral(context, lexer, "YieldArg", "AwaitArg", "TaggedArg")
    assert tl is None
    assert lexer.pos == 0


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
    st = ecmascript.ecmascript.P2_SubstitutionTemplate(context, ["child"], "TaggedArg")
    assert st.name == "SubstitutionTemplate"
    assert st.context == context
    assert st.children == ["child"]
    assert st.Tagged == "TaggedArg"


def test_P2_SubstitutionTemplate_TemplateHead_Expression_TemplateSpans_init(context):
    st = ecmascript.ecmascript.P2_SubstitutionTemplate_TemplateHead_Expression_TemplateSpans(
        context, ["TemplateHead", "Expression", "TemplateSpans"], "T"
    )
    assert st.name == "SubstitutionTemplate"
    assert st.TemplateHead == "TemplateHead"
    assert st.Expression == "Expression"
    assert st.TemplateSpans == "TemplateSpans"


def SubstitutionTemplate_mocks(mocker):
    return {
        "ts": mocker.patch("ecmascript.ecmascript.parse_TemplateSpans", side_effect=ts_sideeffect),
        "exp": mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect),
    }


def test_parse_SubstitutionTemplate_01(mocker, context):
    # SubstitutionTemplate : TemplateHead Expression TemplateSpans
    lexer = Lexer([TEMPLATEHEAD, EXPRESSION, TS_REPLACEMENT])
    mocks = SubstitutionTemplate_mocks(mocker)

    st = ecmascript.ecmascript.parse_SubstitutionTemplate(context, lexer, "YieldArg", "AwaitArg", "TaggedArg")
    assert isinstance(st, ecmascript.ecmascript.P2_SubstitutionTemplate_TemplateHead_Expression_TemplateSpans)
    assert st.TemplateHead == TEMPLATEHEAD
    assert st.Expression == "Expression"
    assert st.TemplateSpans == "TemplateSpans"
    assert lexer.pos == 3
    mocks["exp"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")
    mocks["ts"].assert_called_with(context, lexer, "YieldArg", "AwaitArg", "TaggedArg")


@pytest.mark.parametrize(
    "token_stream", [[], [MATCHES_NONE], [TEMPLATEHEAD, MATCHES_NONE], [TEMPLATEHEAD, EXPRESSION, MATCHES_NONE]]
)
def test_parse_SubstitutionTemplate_02(mocker, context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)
    SubstitutionTemplate_mocks(mocker)

    st = ecmascript.ecmascript.parse_SubstitutionTemplate(context, lexer, "YieldArg", "AwaitArg", "TaggedArg")
    assert st is None
    assert lexer.pos == 0


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
    ts = ecmascript.ecmascript.P2_TemplateSpans(context, ["child"], "TaggedArg")
    assert ts.name == "TemplateSpans"
    assert ts.context == context
    assert ts.children == ["child"]
    assert ts.Tagged == "TaggedArg"


def test_P2_TemplateSpans_TemplateTail_init(context):
    ts = ecmascript.ecmascript.P2_TemplateSpans_TemplateTail(context, ["Tail"], "T")
    assert ts.name == "TemplateSpans"
    assert ts.TemplateTail == "Tail"


def test_P2_TemplateSpans_TemplateMiddleList_TemplateTail_init(context):
    ts = ecmascript.ecmascript.P2_TemplateSpans_TemplateMiddleList_TemplateTail(context, ["MiddleList", "Tail"], "T")
    assert ts.name == "TemplateSpans"
    assert ts.TemplateMiddleList == "MiddleList"
    assert ts.TemplateTail == "Tail"


def TemplateSpans_mocks(mocker):
    return {"tml": mocker.patch("ecmascript.ecmascript.parse_TemplateMiddleList", side_effect=tml_sideeffect)}


def test_parse_TemplateSpans_01(mocker, context):
    # TemplateSpans : TemplateTail
    lexer = Lexer([TEMPLATETAIL])
    TemplateSpans_mocks(mocker)

    ts = ecmascript.ecmascript.parse_TemplateSpans(context, lexer, "YieldArg", "AwaitArg", "TaggedArg")
    assert isinstance(ts, ecmascript.ecmascript.P2_TemplateSpans_TemplateTail)
    assert ts.TemplateTail == TEMPLATETAIL
    assert lexer.pos == 1


def test_parse_TemplateSpans_02(mocker, context):
    # TemplateSpans : TemplateMiddleList TemplateTail
    lexer = Lexer([TML_REPLACEMENT, TEMPLATETAIL])
    mocks = TemplateSpans_mocks(mocker)

    ts = ecmascript.ecmascript.parse_TemplateSpans(context, lexer, "YieldArg", "AwaitArg", "TaggedArg")
    assert isinstance(ts, ecmascript.ecmascript.P2_TemplateSpans_TemplateMiddleList_TemplateTail)
    assert ts.TemplateMiddleList == "TemplateMiddleList"
    assert ts.TemplateTail == TEMPLATETAIL
    assert lexer.pos == 2
    mocks["tml"].assert_called_with(context, lexer, "YieldArg", "AwaitArg", "TaggedArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE], [TML_REPLACEMENT, MATCHES_NONE]])
def test_parse_TemplateSpans_03(mocker, context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)
    TemplateSpans_mocks(mocker)

    ts = ecmascript.ecmascript.parse_TemplateSpans(context, lexer, "YieldArg", "AwaitArg", "TaggedArg")
    assert ts is None
    assert lexer.pos == 0


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
    tml = ecmascript.ecmascript.P2_TemplateMiddleList(context, ["child"], "TaggedArg")
    assert tml.name == "TemplateMiddleList"
    assert tml.context == context
    assert tml.children == ["child"]
    assert tml.Tagged == "TaggedArg"


def test_P2_TemplateMiddleList_TemplateMiddle_Expression_init(context):
    tml = ecmascript.ecmascript.P2_TemplateMiddleList_TemplateMiddle_Expression(
        context, ["TemplateMiddle", "Expression"], "T"
    )
    assert tml.name == "TemplateMiddleList"
    assert tml.TemplateMiddle == "TemplateMiddle"
    assert tml.Expression == "Expression"


def test_P2_TemplateMiddleList_TemplateMiddleList_TemplateMiddle_Expression_init(context):
    tml = ecmascript.ecmascript.P2_TemplateMiddleList_TemplateMiddleList_TemplateMiddle_Expression(
        context, ["TemplateMiddleList", "TemplateMiddle", "Expression"], "T"
    )
    assert tml.name == "TemplateMiddleList"
    assert tml.TemplateMiddleList == "TemplateMiddleList"
    assert tml.TemplateMiddle == "TemplateMiddle"
    assert tml.Expression == "Expression"


def TemplateMiddleList_mocks(mocker):
    return {"exp": mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect)}


def test_parse_TemplateMiddleList_01(mocker, context):
    # TemplateMiddleList : TemplateMiddle Expression
    lexer = Lexer([TEMPLATEMIDDLE, EXPRESSION])
    mocks = TemplateMiddleList_mocks(mocker)

    tml = ecmascript.ecmascript.parse_TemplateMiddleList(context, lexer, "YieldArg", "AwaitArg", "TaggedArg")
    assert isinstance(tml, ecmascript.ecmascript.P2_TemplateMiddleList_TemplateMiddle_Expression)
    assert tml.TemplateMiddle == TEMPLATEMIDDLE
    assert tml.Expression == "Expression"
    assert lexer.pos == 2
    mocks["exp"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")


def test_parse_TemplateMiddleList_02(mocker, context):
    # TemplateMiddleList : TemplateMiddleList TemplateMiddle Expression
    lexer = Lexer([TEMPLATEMIDDLE, EXPRESSION, TEMPLATEMIDDLE, EXPRESSION])
    mocks = TemplateMiddleList_mocks(mocker)

    tml = ecmascript.ecmascript.parse_TemplateMiddleList(context, lexer, "YieldArg", "AwaitArg", "TaggedArg")
    assert isinstance(tml, ecmascript.ecmascript.P2_TemplateMiddleList_TemplateMiddleList_TemplateMiddle_Expression)
    assert tml.TemplateMiddle == TEMPLATEMIDDLE
    assert tml.Expression == "Expression"
    tml2 = tml.TemplateMiddleList
    assert isinstance(tml2, ecmascript.ecmascript.P2_TemplateMiddleList_TemplateMiddle_Expression)
    mocks["exp"].assert_called_with(context, lexer, True, "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE], [TEMPLATEMIDDLE, MATCHES_NONE]])
def test_parse_TemplateMiddleList_03(mocker, context, token_stream):
    # Syntax Errors (non-recursive)
    lexer = Lexer(token_stream)
    TemplateMiddleList_mocks(mocker)

    tml = ecmascript.ecmascript.parse_TemplateMiddleList(context, lexer, "YieldArg", "AwaitArg", "TaggedArg")
    assert tml is None
    assert lexer.pos == 0


@pytest.mark.parametrize(
    "token_stream",
    [[TEMPLATEMIDDLE, EXPRESSION, MATCHES_NONE], [TEMPLATEMIDDLE, EXPRESSION, TEMPLATEMIDDLE, MATCHES_NONE]],
)
def test_parse_TemplateMiddleList_04(mocker, context, token_stream):
    # Syntax Errors (recursive)
    lexer = Lexer(token_stream)
    TemplateMiddleList_mocks(mocker)

    tml = ecmascript.ecmascript.parse_TemplateMiddleList(context, lexer, "YieldArg", "AwaitArg", "TaggedArg")
    assert isinstance(tml, ecmascript.ecmascript.P2_TemplateMiddleList_TemplateMiddle_Expression)
    assert lexer.pos == 2

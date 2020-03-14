# 12.6 Exponentiation Operator

import pytest
from itertools import chain
import math
import snoop

from .helpers import *

import ecmascript.ecmascript as e
import ecmascript.lexer2 as lexer2

#### ExponentiationExpression ############################################################################################################################################################################
#
# 8888888888                                                       888    d8b          888    d8b                   8888888888                                                      d8b
# 888                                                              888    Y8P          888    Y8P                   888                                                             Y8P
# 888                                                              888                 888                          888
# 8888888    888  888 88888b.   .d88b.  88888b.   .d88b.  88888b.  888888 888  8888b.  888888 888  .d88b.  88888b.  8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888        `Y8bd8P' 888 "88b d88""88b 888 "88b d8P  Y8b 888 "88b 888    888     "88b 888    888 d88""88b 888 "88b 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888          X88K   888  888 888  888 888  888 88888888 888  888 888    888 .d888888 888    888 888  888 888  888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# 888        .d8""8b. 888 d88P Y88..88P 888  888 Y8b.     888  888 Y88b.  888 888  888 Y88b.  888 Y88..88P 888  888 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# 8888888888 888  888 88888P"   "Y88P"  888  888  "Y8888  888  888  "Y888 888 "Y888888  "Y888 888  "Y88P"  888  888 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                     888                                                                                                               888
#                     888                                                                                                               888
#                     888                                                                                                               888
#
##########################################################################################################################################################################################################
def test_P2_ExponentiationExpression_init(context):
    ee = e.P2_ExponentiationExpression(context, "StrictArg", FakeTokens("child"))
    assert ee.name == "ExponentiationExpression"
    assert ee.context == context
    assert [tok.value for tok in ee.children] == ["child"]


def test_P2_ExponentiationExpression_UnaryExpression_init(context):
    ee = e.P2_ExponentiationExpression_UnaryExpression(context, "StrictArg", FakeTokens("child"))
    assert ee.name == "ExponentiationExpression"
    assert ee.UnaryExpression.value == "child"


def test_P2_ExponentiationExpression_UpdateExpression_STARSTAR_ExponentiationExpression_init(context):
    ee = e.P2_ExponentiationExpression_UpdateExpression_STARSTAR_ExponentiationExpression(
        context, "StrictArg", FakeTokens("upe", "**", "ee")
    )
    assert ee.name == "ExponentiationExpression"
    assert ee.UpdateExpression.value == "upe"
    assert ee.ExponentiationExpression.value == "ee"


class Test_parse_ExponentiationExpression(parse_test):
    # Syntax
    #   ExponentiationExpression[Yield, Await] :
    #       UnaryExpression[?Yield, ?Await]
    #       UpdateExpression[?Yield, ?Await] ** ExponentiationExpression[?Yield, ?Await]
    target = staticmethod(e.parse_ExponentiationExpression)
    target_argnames = ("Yield", "Await")
    productions = (
        (("UnaryExpression",), e.P2_ExponentiationExpression_UnaryExpression),
        (
            ("UpdateExpression", "**", "UnaryExpression"),
            e.P2_ExponentiationExpression_UpdateExpression_STARSTAR_ExponentiationExpression,
        ),
    )
    called_argnames = {
        "UnaryExpression": ("?Yield", "?Await"),
        "UpdateExpression": ("?Yield", "?Await"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


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
class Test_ExponentiationOperator_IsStringLiteral:
    @strict_params
    def test_ExponentiationExpression_UnaryExpression(self, context, mocker, strict):
        child = mocker.Mock(IsStringLiteral=mocker.sentinel.is_string_literal)
        pn = e.P2_ExponentiationExpression_UnaryExpression(context, strict, [child])
        assert pn.IsStringLiteral == mocker.sentinel.is_string_literal


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
class Test_ExponentiationOperator_HasUseStrict:
    @strict_params
    def test_ExponentiationExpression_UnaryExpression(self, context, mocker, strict):
        child = mocker.Mock(HasUseStrict=mocker.sentinel.has_use_strict)
        pn = e.P2_ExponentiationExpression_UnaryExpression(context, strict, [child])
        assert pn.HasUseStrict == mocker.sentinel.has_use_strict


####################################################################################
#
#  d888    .d8888b.       .d8888b.       d888
# d8888   d88P  Y88b     d88P  Y88b     d8888
#   888          888     888              888
#   888        .d88P     888d888b.        888
#   888    .od888P"      888P "Y88b       888
#   888   d88P"          888    888       888
#   888   888"       d8b Y88b  d88P d8b   888
# 8888888 888888888  Y8P  "Y8888P"  Y8P 8888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Exponentiation Operator
# 12.6.1 | Static Semantics: IsFunctionDefinition
####################################################################################
class Test_ExponentiationOperator_IsFunctionDefinition:
    # ExponentiationExpression : UpdateExpression ** ExponentiationExpression
    #   1. Return false.
    @strict_params
    def test_normal(self, context, mocker, strict):
        ee = e.P2_ExponentiationExpression_UpdateExpression_STARSTAR_ExponentiationExpression(
            context, strict, [mocker.Mock(), mocker.Mock(), mocker.Mock()]
        )
        assert ee.IsFunctionDefinition() is False


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b
#   888          888     888                   888
#   888        .d88P     888d888b.           .d88P
#   888    .od888P"      888P "Y88b      .od888P"
#   888   d88P"          888    888     d88P"
#   888   888"       d8b Y88b  d88P d8b 888"
# 8888888 888888888  Y8P  "Y8888P"  Y8P 888888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Exponentiation Operator
# 12.6.2 | Static Semantics: AssignmentTargetType
####################################################################################
class Test_ExponentiationOperator_AssignmentTargetType:
    # ExponentiationExpression : UpdateExpression ** ExponentiationExpression
    #   1. Return invalid.
    @strict_params
    def test_normal(self, context, mocker, strict):
        ee = e.P2_ExponentiationExpression_UpdateExpression_STARSTAR_ExponentiationExpression(
            context, strict, [mocker.Mock(), mocker.Mock(), mocker.Mock()]
        )
        assert ee.AssignmentTargetType == e.INVALID


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b
#   888          888     888                 .d88P
#   888        .d88P     888d888b.          8888"
#   888    .od888P"      888P "Y88b          "Y8b.
#   888   d88P"          888    888     888    888
#   888   888"       d8b Y88b  d88P d8b Y88b  d88P
# 8888888 888888888  Y8P  "Y8888P"  Y8P  "Y8888P"
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Exponentiation Operator
# 12.6.3 | Runtime Semantics: Evaluation
####################################################################################
class Test_ExponentiationOperator_Evaluation:
    # ExponentiationExpression : UpdateExpression ** ExponentiationExpression
    #   1. Let left be the result of evaluating UpdateExpression.
    #   2. Let leftValue be ? GetValue(left).
    #   3. Let right be the result of evaluating ExponentiationExpression.
    #   4. Let rightValue be ? GetValue(right).
    #   5. Let base be ? ToNumber(leftValue).
    #   6. Let exponent be ? ToNumber(rightValue).
    #   7. Return the result of Applying the ** operator with base and exponent as specified in 12.6.4.
    @strict_params
    def test_normal(self, context, mocker, strict):
        order = {"seq": ""}

        def ue_evaluate():
            order["seq"] += "1"
            return mocker.sentinel.left

        def gv_repl(ref):
            if ref == mocker.sentinel.left:
                order["seq"] += "2"
                return mocker.sentinel.leftValue
            order["seq"] += "4"
            return mocker.sentinel.rightValue

        def ee_evaluate():
            order["seq"] += "3"
            return mocker.sentinel.right

        ue = mocker.Mock(**{"evaluate.side_effect": ue_evaluate})
        ee = mocker.Mock(**{"evaluate.side_effect": ee_evaluate})
        gv = mocker.patch("ecmascript.ecmascript.GetValue", side_effect=gv_repl)
        eo = mocker.patch("ecmascript.ecmascript.ExponentiationOperation", return_value=mocker.sentinel.return_value)

        pn = e.P2_ExponentiationExpression_UpdateExpression_STARSTAR_ExponentiationExpression(
            context, strict, [ue, mocker.Mock(), ee]
        )

        rv = pn.evaluate()
        assert rv == mocker.sentinel.return_value
        assert order["seq"] == "1234"
        ue.evaluate.assert_called_with()
        ee.evaluate.assert_called_with()
        assert gv.call_args_list == [mocker.call(mocker.sentinel.left), mocker.call(mocker.sentinel.right)]
        eo.assert_called_with(mocker.sentinel.leftValue, mocker.sentinel.rightValue)


####################################################################################
#
#  d888    .d8888b.       .d8888b.          d8888
# d8888   d88P  Y88b     d88P  Y88b        d8P888
#   888          888     888              d8P 888
#   888        .d88P     888d888b.       d8P  888
#   888    .od888P"      888P "Y88b     d88   888
#   888   d88P"          888    888     8888888888
#   888   888"       d8b Y88b  d88P d8b       888
# 8888888 888888888  Y8P  "Y8888P"  Y8P       888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Exponentiation Operator
# 12.6.4 | Applying the ** Operator
####################################################################################
class Test_ExponentiationOperator_Application:
    # Returns an implementation-dependent approximation of the result of raising base to the power exponent.
    #
    #   * If exponent is NaN, the result is NaN.
    #   * If exponent is +0, the result is 1, even if base is NaN.
    #   * If exponent is -0, the result is 1, even if base is NaN.
    #   * If base is NaN and exponent is nonzero, the result is NaN.
    #   * If abs(base) > 1 and exponent is +∞, the result is +∞.
    #   * If abs(base) > 1 and exponent is -∞, the result is +0.
    #   * If abs(base) is 1 and exponent is +∞, the result is NaN.
    #   * If abs(base) is 1 and exponent is -∞, the result is NaN.
    #   * If abs(base) < 1 and exponent is +∞, the result is +0.
    #   * If abs(base) < 1 and exponent is -∞, the result is +∞.
    #   * If base is +∞ and exponent > 0, the result is +∞.
    #   * If base is +∞ and exponent < 0, the result is +0.
    #   * If base is -∞ and exponent > 0 and exponent is an odd integer, the result is -∞.
    #   * If base is -∞ and exponent > 0 and exponent is not an odd integer, the result is +∞.
    #   * If base is -∞ and exponent < 0 and exponent is an odd integer, the result is -0.
    #   * If base is -∞ and exponent < 0 and exponent is not an odd integer, the result is +0.
    #   * If base is +0 and exponent > 0, the result is +0.
    #   * If base is +0 and exponent < 0, the result is +∞.
    #   * If base is -0 and exponent > 0 and exponent is an odd integer, the result is -0.
    #   * If base is -0 and exponent > 0 and exponent is not an odd integer, the result is +0.
    #   * If base is -0 and exponent < 0 and exponent is an odd integer, the result is -∞.
    #   * If base is -0 and exponent < 0 and exponent is not an odd integer, the result is +∞.
    #   * If base < 0 and base is finite and exponent is finite and exponent is not an integer, the result is NaN.
    @pytest.mark.parametrize(
        "base, exponent",
        (
            (0, math.nan),
            (math.nan, 10),
            (1, math.inf),
            (-1, math.inf),
            (1, -math.inf),
            (-1, -math.inf),
            (-20, 0.25),
        ),
    )
    def test_nans(self, base, exponent):
        assert math.isnan(e.ExponentiationOperation(base, exponent))

    @pytest.mark.parametrize(
        "base, exponent, sign",
        (
            (2, -math.inf, 1),
            (-2, -math.inf, 1),
            (0.5, math.inf, 1),
            (-0.5, math.inf, 1),
            (math.inf, -99, 1),
            (-math.inf, -3, -1),
            (-math.inf, -6, 1),
            (0.0, 10.0, 1),
            (-0.0, 3, -1),
            (-0.0, 6, 1),
        ),
    )
    def test_zeros(self, base, exponent, sign):
        rv = e.ExponentiationOperation(base, exponent)
        assert rv == 0.0 and math.copysign(1.0, rv) == sign

    @pytest.mark.parametrize(
        "base, exponent, expected",
        (
            (math.nan, 0.0, 1.0),
            (math.nan, -0.0, 1.0),
            (10, math.inf, math.inf),
            (-10, math.inf, math.inf),
            (0.5, -math.inf, math.inf),
            (-0.5, -math.inf, math.inf),
            (math.inf, 10, math.inf),
            (-math.inf, 3, -math.inf),
            (-math.inf, 8, math.inf),
            (0, -1, math.inf),
            (-0.0, -3, -math.inf),
            (-0.0, -4, math.inf),
            (-0.0, -math.inf, math.inf),
            (4.0, 0.5, 2.0),
            (10.0, 3.0, 1000.0),
            (3.5, 888, math.inf),
            (-3.0, 888, math.inf),
            (-3.0, 887, -math.inf),
        ),
    )
    def test_other(self, base, exponent, expected):
        assert e.ExponentiationOperation(base, exponent) == expected

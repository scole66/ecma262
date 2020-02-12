# 12.16 Comma Operator (,)

import pytest
from itertools import chain
import snoop

from .helpers import *

import ecmascript.ecmascript as e
import ecmascript.lexer2 as lexer2

#### Expression ########################################################################
#
# 8888888888                                                      d8b
# 888                                                             Y8P
# 888
# 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                     888
#                     888
#                     888
#
########################################################################################
def test_P2_Expression_init(context):
    exp = e.P2_Expression(context, "StrictArg", ["child"])
    assert exp.name == "Expression"
    assert exp.context == context
    assert exp.children == ["child"]
    assert exp.strict == "StrictArg"


def test_P2_Expression_AssignmentExpression_init(context):
    exp = e.P2_Expression_AssignmentExpression(context, "StrictArg", ["AssignmentExpression"])
    assert exp.name == "Expression"
    assert exp.AssignmentExpression == "AssignmentExpression"


def test_P2_Expression_Expression_COMMA_AssignmentExpression_init(context):
    exp = e.P2_Expression_Expression_COMMA_AssignmentExpression(
        context, "StrictArg", ["Expression", ",", "AssignmentExpression"]
    )
    assert exp.name == "Expression"
    assert exp.Expression == "Expression"
    assert exp.AssignmentExpression == "AssignmentExpression"


class Test_parse_Expression(parse_test):
    # Syntax
    #   Expression[In, Yield, Await]:
    #       AssignmentExpression[?In, ?Yield, ?Await]
    #       Expression[?In, ?Yield, ?Await] , AssignmentExpression[?In, ?Yield, ?Await]
    target = staticmethod(e.parse_Expression)
    target_argnames = ("In", "Yield", "Await")
    productions = (
        (("AssignmentExpression",), e.P2_Expression_AssignmentExpression),
        (
            ("AssignmentExpression", ",", "AssignmentExpression"),
            e.P2_Expression_Expression_COMMA_AssignmentExpression,
        ),
    )
    called_argnames = {"AssignmentExpression": ("?In", "?Yield", "?Await")}

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
class Test_Expression_IsStringLiteral:
    @strict_params
    def test_Expression_AssignmentExpression(self, context, mocker, strict):
        ae = mocker.Mock(IsStringLiteral=mocker.sentinel.is_string_literal)
        pn = e.P2_Expression_AssignmentExpression(context, strict, [ae])
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
class Test_Expression_HasUseStrict:
    @strict_params
    def test_Expression_AssignmentExpression(self, context, mocker, strict):
        ae = mocker.Mock(HasUseStrict=mocker.sentinel.has_use_strict)
        pn = e.P2_Expression_AssignmentExpression(context, strict, [ae])
        assert pn.HasUseStrict == mocker.sentinel.has_use_strict


####################################################################################
#
#  d888    .d8888b.       d888    .d8888b.       d888
# d8888   d88P  Y88b     d8888   d88P  Y88b     d8888
#   888          888       888   888              888
#   888        .d88P       888   888d888b.        888
#   888    .od888P"        888   888P "Y88b       888
#   888   d88P"            888   888    888       888
#   888   888"       d8b   888   Y88b  d88P d8b   888
# 8888888 888888888  Y8P 8888888  "Y8888P"  Y8P 8888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Comma Operator ( , )
# 12.16.1 | Static Semantics: IsFunctionDefinition
####################################################################################
class Test_CommaOperator_IsFunctionDefinition:
    # Expression : Expression , AssignmentExpression
    #   1. Return false.
    @strict_params
    def test_normal(self, context, mocker, strict):
        exp = mocker.Mock()
        ae = mocker.Mock()
        node = e.P2_Expression_Expression_COMMA_AssignmentExpression(context, strict, [exp, mocker.Mock(), ae])
        rv = node.IsFunctionDefinition()
        assert rv is False


####################################################################################
#
#  d888    .d8888b.       d888    .d8888b.       .d8888b.
# d8888   d88P  Y88b     d8888   d88P  Y88b     d88P  Y88b
#   888          888       888   888                   888
#   888        .d88P       888   888d888b.           .d88P
#   888    .od888P"        888   888P "Y88b      .od888P"
#   888   d88P"            888   888    888     d88P"
#   888   888"       d8b   888   Y88b  d88P d8b 888"
# 8888888 888888888  Y8P 8888888  "Y8888P"  Y8P 888888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Comma Operator ( , )
# 12.16.2 | Static Semantics: AssignmentTargetType
####################################################################################
class Test_CommaOperator_AssignmentTargetType:
    # Expression : Expression , AssignmentExpression
    #   1. Return invalid.
    @strict_params
    def test_normal(self, context, mocker, strict):
        exp = mocker.Mock()
        ae = mocker.Mock()
        node = e.P2_Expression_Expression_COMMA_AssignmentExpression(context, strict, [exp, mocker.Mock(), ae])
        rv = node.AssignmentTargetType
        assert rv is e.INVALID


####################################################################################
#
#  d888    .d8888b.       d888    .d8888b.       .d8888b.
# d8888   d88P  Y88b     d8888   d88P  Y88b     d88P  Y88b
#   888          888       888   888                 .d88P
#   888        .d88P       888   888d888b.          8888"
#   888    .od888P"        888   888P "Y88b          "Y8b.
#   888   d88P"            888   888    888     888    888
#   888   888"       d8b   888   Y88b  d88P d8b Y88b  d88P
# 8888888 888888888  Y8P 8888888  "Y8888P"  Y8P  "Y8888P"
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Comma Operator ( , )
# 12.16.3 | Runtime Semantics: Evaluation
####################################################################################
class Test_CommaOperator_Evaluation:
    # Expression : Expression , AssignmentExpression
    #   1. Let lref be the result of evaluating Expression.
    #   2. Perform ? GetValue(lref).
    #   3. Let rref be the result of evaluating AssignmentExpression.
    #   4. Return ? GetValue(rref).
    @strict_params
    def test_normal(self, context, mocker, strict):
        exp = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.lref})
        ae = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.rref})
        node = e.P2_Expression_Expression_COMMA_AssignmentExpression(context, strict, [exp, mocker.Mock(), ae])
        gv = mocker.patch(
            "ecmascript.ecmascript.GetValue", side_effect=[mocker.sentinel.gv_left, mocker.sentinel.return_value]
        )
        rv = node.evaluate()
        assert rv == mocker.sentinel.return_value
        exp.evaluate.assert_called_with()
        ae.evaluate.assert_called_with()
        assert gv.call_args_list == [mocker.call(mocker.sentinel.lref), mocker.call(mocker.sentinel.rref)]

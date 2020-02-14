# 12.3 Left-Hand-Side Expressions

import pytest
from itertools import chain
import snoop

from .helpers import *

import ecmascript.ecmascript as e
import ecmascript.lexer2 as lexer2

#### MemberExpression #################################################################################################################################
#
# 888b     d888                        888                       8888888888                                                      d8b
# 8888b   d8888                        888                       888                                                             Y8P
# 88888b.d88888                        888                       888
# 888Y88888P888  .d88b.  88888b.d88b.  88888b.   .d88b.  888d888 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888 Y888P 888 d8P  Y8b 888 "888 "88b 888 "88b d8P  Y8b 888P"   888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888  Y8P  888 88888888 888  888  888 888  888 88888888 888     888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# 888   "   888 Y8b.     888  888  888 888 d88P Y8b.     888     888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# 888       888  "Y8888  888  888  888 88888P"   "Y8888  888     8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                                                    888
#                                                                                    888
#                                                                                    888
#
#######################################################################################################################################################
def test_P2_MemberExpression_init(context):
    me = e.P2_MemberExpression(context, "strict", ["child"])
    assert me.name == "MemberExpression"
    assert me.context == context
    assert me.children == ["child"]
    assert me.strict == "strict"


def test_P2_MemberExpression_MemberExpression_PERIOD_IdentifierName_init(context):
    me = e.P2_MemberExpression_MemberExpression_PERIOD_IdentifierName(
        context, False, ["MemberExpression_alpha", ".", "IdentifierName_beta"]
    )
    assert me.name == "MemberExpression"
    assert me.MemberExpression == "MemberExpression_alpha"
    assert me.IdentifierName == "IdentifierName_beta"


def test_P2_MemberExpression_MemberExpression_TemplateLiteral_init(context):
    me = e.P2_MemberExpression_MemberExpression_TemplateLiteral(
        context, False, ["MemberExpression_alpha", "TemplateLiteral_beta"]
    )
    assert me.name == "MemberExpression"
    assert me.MemberExpression == "MemberExpression_alpha"
    assert me.TemplateLiteral == "TemplateLiteral_beta"


def test_P2_MemberExpression_SuperProperty_init(context):
    me = e.P2_MemberExpression_SuperProperty(context, False, ["SuperProp"])
    assert me.name == "MemberExpression"
    assert me.SuperProperty == "SuperProp"


def test_P2_MemberExpression_MetaProperty_init(context):
    me = e.P2_MemberExpression_MetaProperty(context, False, ["MetaProp"])
    assert me.name == "MemberExpression"
    assert me.MetaProperty == "MetaProp"


def test_P2_MemberExpression_NEW_MemberExpression_Arguments_init(context):
    me = e.P2_MemberExpression_NEW_MemberExpression_Arguments(context, False, ["new", "ME_alpha", "args"])
    assert me.name == "MemberExpression"
    assert me.MemberExpression == "ME_alpha"
    assert me.Arguments == "args"


class Test_P2_MemberExpression_PrimaryExpression:
    @strict_params
    def test_init(self, context, strict):
        me = e.P2_MemberExpression_PrimaryExpression(context, strict, ["PrimaryExpression"])
        assert me.name == "MemberExpression"
        assert me.PrimaryExpression == "PrimaryExpression"

    @pytest.mark.parametrize("probe, expected", ((True, True), (False, False)))
    @strict_params
    def test_IsStringLiteral(self, mocker, context, strict, probe, expected):
        primary_expression = mocker.Mock(**{"IsStringLiteral": probe})
        me = e.P2_MemberExpression_PrimaryExpression(context, strict, [primary_expression])

        assert me.IsStringLiteral is expected

    @pytest.mark.parametrize("probe, expected", ((True, True), (False, False)))
    @strict_params
    def test_HasUseStrict(self, mocker, context, strict, probe, expected):
        primary_expression = mocker.Mock(**{"HasUseStrict": probe})
        me = e.P2_MemberExpression_PrimaryExpression(context, strict, [primary_expression])

        assert me.HasUseStrict is expected


class Test_P2_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET:
    @strict_params
    def test_init(self, context, strict):
        me = e.P2_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET(
            context, strict, ["MemberExpression", "[", "Expression", "]"]
        )
        assert me.name == "MemberExpression"
        assert me.MemberExpression == "MemberExpression"
        assert me.Expression == "Expression"


class Test_parse_MemberExpression(parse_test):
    # Syntax
    #   MemberExpression[Yield, Await] :
    #       PrimaryExpression[?Yield, ?Await]
    #       MemberExpression[?Yield, ?Await] [ Expression[+In, ?Yield, ?Await] ]
    #       MemberExpression[?Yield, ?Await] . IdentifierName
    #       MemberExpression[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
    #       SuperProperty[?Yield, ?Await]
    #       MetaProperty
    #       new MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
    ME_PE = e.P2_MemberExpression_PrimaryExpression
    ME_ME_Exp = e.P2_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET
    ME_ME_Ident = e.P2_MemberExpression_MemberExpression_PERIOD_IdentifierName
    ME_ME_TL = e.P2_MemberExpression_MemberExpression_TemplateLiteral
    ME_SP = e.P2_MemberExpression_SuperProperty
    ME_MP = e.P2_MemberExpression_MetaProperty
    ME_new_ME_Args = e.P2_MemberExpression_NEW_MemberExpression_Arguments
    productions = (
        # Note that for recursive definitions, we replace the recursive part with a one-element alternate, just to
        # make sure the mocking all works. (Thus, mentions of "MemberExpression" in the syntax above are replaced
        # here with "PrimaryExpression".)
        (("PrimaryExpression",), ME_PE),
        (("PrimaryExpression", "[", "Expression", "]"), ME_ME_Exp),
        (("PrimaryExpression", ".", "IDENTIFIER¡bob"), ME_ME_Ident),
        (("PrimaryExpression", "TemplateLiteral"), ME_ME_TL),
        (("SuperProperty",), ME_SP),
        (("MetaProperty",), ME_MP),
        (("new", "PrimaryExpression", "Arguments"), ME_new_ME_Args),
    )
    target = staticmethod(e.parse_MemberExpression)
    target_argnames = ("Yield", "Await")
    called_argnames = {
        "PrimaryExpression": ("?Yield", "?Await"),
        "Expression": ("+In", "?Yield", "?Await"),
        "TemplateLiteral": ("?Yield", "?Await", "+Tagged"),
        "SuperProperty": ("?Yield", "?Await"),
        "MetaProperty": (),
        "Arguments": ("?Yield", "?Await"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### SuperProperty ###################################################################################################
#
#  .d8888b.                                     8888888b.                                             888
# d88P  Y88b                                    888   Y88b                                            888
# Y88b.                                         888    888                                            888
#  "Y888b.   888  888 88888b.   .d88b.  888d888 888   d88P 888d888  .d88b.  88888b.   .d88b.  888d888 888888 888  888
#     "Y88b. 888  888 888 "88b d8P  Y8b 888P"   8888888P"  888P"   d88""88b 888 "88b d8P  Y8b 888P"   888    888  888
#       "888 888  888 888  888 88888888 888     888        888     888  888 888  888 88888888 888     888    888  888
# Y88b  d88P Y88b 888 888 d88P Y8b.     888     888        888     Y88..88P 888 d88P Y8b.     888     Y88b.  Y88b 888
#  "Y8888P"   "Y88888 88888P"   "Y8888  888     888        888      "Y88P"  88888P"   "Y8888  888      "Y888  "Y88888
#                     888                                                   888                                   888
#                     888                                                   888                              Y8b d88P
#                     888                                                   888                               "Y88P"
#
######################################################################################################################
def test_P2_SuperProperty_init(context):
    sp = e.P2_SuperProperty(context, "StrictArg", ["child"])
    assert sp.name == "SuperProperty"
    assert sp.context == context
    assert sp.children == ["child"]
    assert sp.strict == "StrictArg"


def test_P2_SuperProperty_SUPER_LBRACKET_Expression_RBRACKET_init(context):
    sp = e.P2_SuperProperty_SUPER_LBRACKET_Expression_RBRACKET(
        context, "StrictArg", ["super", "[", "Expression", "]"]
    )
    assert sp.name == "SuperProperty"
    assert sp.Expression == "Expression"


def test_P2_SuperProperty_SUPER_PERIOD_IdentifierName_init(context):
    sp = e.P2_SuperProperty_SUPER_PERIOD_IdentifierName(context, "StrictArg", ["super", ".", "IdentifierName"])
    assert sp.name == "SuperProperty"
    assert sp.IdentifierName == "IdentifierName"


class Test_parse_SuperProperty(parse_test):
    # Syntax
    #   SuperProperty[Yield, Await] :
    #       super [ Expression[+In, ?Yield, ?Await] ]
    #       super . IdentifierName
    target = staticmethod(e.parse_SuperProperty)
    target_argnames = ("Yield", "Await")
    productions = (
        (("super", "[", "Expression", "]"), e.P2_SuperProperty_SUPER_LBRACKET_Expression_RBRACKET),
        (("super", ".", "IDENTIFIER¡bob"), e.P2_SuperProperty_SUPER_PERIOD_IdentifierName),
    )
    called_argnames = {"Expression": ("+In", "?Yield", "?Await")}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### MetaProperty #############################################################################################
#
# 888b     d888          888             8888888b.                                             888
# 8888b   d8888          888             888   Y88b                                            888
# 88888b.d88888          888             888    888                                            888
# 888Y88888P888  .d88b.  888888  8888b.  888   d88P 888d888  .d88b.  88888b.   .d88b.  888d888 888888 888  888
# 888 Y888P 888 d8P  Y8b 888        "88b 8888888P"  888P"   d88""88b 888 "88b d8P  Y8b 888P"   888    888  888
# 888  Y8P  888 88888888 888    .d888888 888        888     888  888 888  888 88888888 888     888    888  888
# 888   "   888 Y8b.     Y88b.  888  888 888        888     Y88..88P 888 d88P Y8b.     888     Y88b.  Y88b 888
# 888       888  "Y8888   "Y888 "Y888888 888        888      "Y88P"  88888P"   "Y8888  888      "Y888  "Y88888
#                                                                    888                                   888
#                                                                    888                              Y8b d88P
#                                                                    888                               "Y88P"
#
###############################################################################################################
def test_P2_MetaProperty_init(context):
    mp = e.P2_MetaProperty(context, "StrictArg", ["child"])
    assert mp.name == "MetaProperty"
    assert mp.context == context
    assert mp.children == ["child"]
    assert mp.strict == "StrictArg"


def test_P2_MetaProperty_NewTarget_init(context):
    mp = e.P2_MetaProperty_NewTarget(context, "StrictArg", ["NewTarget"])
    assert mp.name == "MetaProperty"
    assert mp.NewTarget == "NewTarget"


class Test_parse_MetaProperty(parse_test):
    # Syntax
    #   MetaProperty:
    #       NewTarget
    target = staticmethod(e.parse_MetaProperty)
    target_argnames = ()
    productions = ((("NewTarget",), e.P2_MetaProperty_NewTarget),)
    called_argnames = {"NewTarget": ()}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### NewTarget ############################################################################
#
# 888b    888                        88888888888                                    888
# 8888b   888                            888                                        888
# 88888b  888                            888                                        888
# 888Y88b 888  .d88b.  888  888  888     888      8888b.  888d888  .d88b.   .d88b.  888888
# 888 Y88b888 d8P  Y8b 888  888  888     888         "88b 888P"   d88P"88b d8P  Y8b 888
# 888  Y88888 88888888 888  888  888     888     .d888888 888     888  888 88888888 888
# 888   Y8888 Y8b.     Y88b 888 d88P     888     888  888 888     Y88b 888 Y8b.     Y88b.
# 888    Y888  "Y8888   "Y8888888P"      888     "Y888888 888      "Y88888  "Y8888   "Y888
#                                                                      888
#                                                                 Y8b d88P
#                                                                  "Y88P"
#
###########################################################################################
def test_P2_NewTarget_init(context):
    sp = e.P2_NewTarget(context, "StrictArg", ["child"])
    assert sp.name == "NewTarget"
    assert sp.context == context
    assert sp.children == ["child"]
    assert sp.strict == "StrictArg"


def test_P2_NewTarget_NEW_PERIOD_TARGET_init(context):
    sp = e.P2_NewTarget_NEW_PERIOD_TARGET(context, "StrictArg", ["new", ".", "target"])
    assert sp.name == "NewTarget"


class Test_parse_NewTarget(parse_test):
    # Syntax
    #   NewTarget :
    #       new . target
    target = staticmethod(e.parse_NewTarget)
    target_argnames = ()
    productions = ((("new", ".", "target"), e.P2_NewTarget_NEW_PERIOD_TARGET),)
    called_argnames = {}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### NewExpression ########################################################################################################
#
# 888b    888                        8888888888                                                      d8b
# 8888b   888                        888                                                             Y8P
# 88888b  888                        888
# 888Y88b 888  .d88b.  888  888  888 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888 Y88b888 d8P  Y8b 888  888  888 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888  Y88888 88888888 888  888  888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# 888   Y8888 Y8b.     Y88b 888 d88P 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# 888    Y888  "Y8888   "Y8888888P"  8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                        888
#                                                        888
#                                                        888
#
###########################################################################################################################
def test_P2_NewExpression_init(context):
    ne = e.P2_NewExpression(context, "StrictArg", ["child"])
    assert ne.name == "NewExpression"
    assert ne.context == context
    assert ne.children == ["child"]
    assert ne.strict == "StrictArg"


def test_P2_NewExpression_MemberExpression_init(context):
    ne = e.P2_NewExpression_MemberExpression(context, "StrictArg", ["MemberExpression"])
    assert ne.name == "NewExpression"
    assert ne.MemberExpression == "MemberExpression"


def test_P2_NewExpression_NEW_NewExpression_init(context):
    ne = e.P2_NewExpression_NEW_NewExpression(context, "StrictArg", ["new", "NewExpression"])
    assert ne.name == "NewExpression"
    assert ne.NewExpression == "NewExpression"


class Test_parse_NewExpression(parse_test):
    # Syntax
    #   NewExpression[Yield, Await] :
    #       MemberExpression[?Yield, ?Await]
    #       new NewExpression[?Yield, ?Await]
    target = staticmethod(e.parse_NewExpression)
    target_argnames = ("Yield", "Await")
    productions = (
        (("MemberExpression",), e.P2_NewExpression_MemberExpression),
        (("new", "MemberExpression"), e.P2_NewExpression_NEW_NewExpression),
    )
    called_argnames = {"MemberExpression": ("?Yield", "?Await")}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### CallExpression ################################################################################################
#
#  .d8888b.           888 888 8888888888                                                      d8b
# d88P  Y88b          888 888 888                                                             Y8P
# 888    888          888 888 888
# 888         8888b.  888 888 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888            "88b 888 888 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888    888 .d888888 888 888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# Y88b  d88P 888  888 888 888 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
#  "Y8888P"  "Y888888 888 888 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                 888
#                                                 888
#                                                 888
#
####################################################################################################################
def test_P2_CallExpression_init(context):
    ce = e.P2_CallExpression(context, "StrictArg", ["child"])
    assert ce.name == "CallExpression"
    assert ce.context == context
    assert ce.children == ["child"]
    assert ce.strict == "StrictArg"


def test_P2_CallExpression_CoverCallExpressionAndAsyncArrowHead_init(context):
    ce = e.P2_CallExpression_CoverCallExpressionAndAsyncArrowHead(
        context, "StrictArg", ["CoverCallExpressionAndAsyncArrowHead"], "YieldArg", "AwaitArg"
    )
    assert ce.name == "CallExpression"
    assert ce.CoverCallExpressionAndAsyncArrowHead == "CoverCallExpressionAndAsyncArrowHead"
    assert ce.Yield == "YieldArg"
    assert ce.Await == "AwaitArg"


def test_P2_CallExpression_SuperCall_init(context):
    ce = e.P2_CallExpression_SuperCall(context, "StrictArg", ["SuperCall"])
    assert ce.name == "CallExpression"
    assert ce.SuperCall == "SuperCall"


def test_P2_CallExpression_CallExpression_Arguments_init(context):
    ce = e.P2_CallExpression_CallExpression_Arguments(context, "StrictArg", ["CallExpression", "Arguments"])
    assert ce.name == "CallExpression"
    assert ce.CallExpression == "CallExpression"
    assert ce.Arguments == "Arguments"


def test_P2_CallExpression_CallExpression_LBRACKET_Expression_RBRACKET_init(context):
    ce = e.P2_CallExpression_CallExpression_LBRACKET_Expression_RBRACKET(
        context, "StrictArg", ["CallExpression", "[", "Expression", "]"]
    )
    assert ce.name == "CallExpression"
    assert ce.CallExpression == "CallExpression"
    assert ce.Expression == "Expression"


def test_P2_CallExpression_CallExpression_PERIOD_IdentifierName_init(context):
    ce = e.P2_CallExpression_CallExpression_PERIOD_IdentifierName(
        context, "StrictArg", ["CallExpression", ".", "IdentifierName"]
    )
    assert ce.name == "CallExpression"
    assert ce.CallExpression == "CallExpression"
    assert ce.IdentifierName == "IdentifierName"


def test_P2_CallExpression_CallExpression_TemplateLiteral_init(context):
    ce = e.P2_CallExpression_CallExpression_TemplateLiteral(
        context, "StrictArg", ["CallExpression", "TemplateLiteral"]
    )
    assert ce.name == "CallExpression"
    assert ce.CallExpression == "CallExpression"
    assert ce.TemplateLiteral == "TemplateLiteral"


class Test_parse_CallExpression(parse_test):
    # Syntax
    #   CallExpression[Yield, Await] :
    #       CoverCallExpressionAndAsyncArrowHead[?Yield, ?Await]
    #       SuperCall[?Yield, ?Await]
    #       CallExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
    #       CallExpression[?Yield, ?Await] [ Expression[+In, ?Yield, ?Await] ]
    #       CallExpression[?Yield, ?Await] . IdentifierName
    #       CallExpression[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
    target = staticmethod(e.parse_CallExpression)
    target_argnames = ("Yield", "Await")
    CE_CCEAAAH = e.P2_CallExpression_CoverCallExpressionAndAsyncArrowHead
    CE_SC = e.P2_CallExpression_SuperCall
    CE_CE_Args = e.P2_CallExpression_CallExpression_Arguments
    CE_CE_Expression = e.P2_CallExpression_CallExpression_LBRACKET_Expression_RBRACKET
    CE_CE_Ident = e.P2_CallExpression_CallExpression_PERIOD_IdentifierName
    CE_CE_TL = e.P2_CallExpression_CallExpression_TemplateLiteral
    productions = (
        (("CoverCallExpressionAndAsyncArrowHead",), CE_CCEAAAH),
        (("SuperCall",), CE_SC),
        (("CoverCallExpressionAndAsyncArrowHead", "Arguments"), CE_CE_Args),
        (("CoverCallExpressionAndAsyncArrowHead", "[", "Expression", "]"), CE_CE_Expression),
        (("CoverCallExpressionAndAsyncArrowHead", ".", "IDENTIFIER¡bob"), CE_CE_Ident),
        (("CoverCallExpressionAndAsyncArrowHead", "TemplateLiteral"), CE_CE_TL),
    )
    called_argnames = {
        "CoverCallExpressionAndAsyncArrowHead": ("?Yield", "?Await"),
        "SuperCall": ("?Yield", "?Await"),
        "Arguments": ("?Yield", "?Await"),
        "Expression": ("+In", "?Yield", "?Await"),
        "TemplateLiteral": ("?Yield", "?Await", "+Tagged"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### SuperCall #########################################################################
#
#  .d8888b.                                      .d8888b.           888 888
# d88P  Y88b                                    d88P  Y88b          888 888
# Y88b.                                         888    888          888 888
#  "Y888b.   888  888 88888b.   .d88b.  888d888 888         8888b.  888 888
#     "Y88b. 888  888 888 "88b d8P  Y8b 888P"   888            "88b 888 888
#       "888 888  888 888  888 88888888 888     888    888 .d888888 888 888
# Y88b  d88P Y88b 888 888 d88P Y8b.     888     Y88b  d88P 888  888 888 888
#  "Y8888P"   "Y88888 88888P"   "Y8888  888      "Y8888P"  "Y888888 888 888
#                     888
#                     888
#                     888
#
########################################################################################
def test_P2_SuperCall_init(context):
    sc = e.P2_SuperCall(context, "StrictArg", ["child"])
    assert sc.name == "SuperCall"
    assert sc.context == context
    assert sc.children == ["child"]
    assert sc.strict == "StrictArg"


def test_P2_SueprCall_SUPER_Arguments_init(context):
    sc = e.P2_SuperCall_SUPER_Arguments(context, "StrictArg", ["super", "Arguments"])
    assert sc.name == "SuperCall"
    assert sc.Arguments == "Arguments"


class Test_parse_SuperCall(parse_test):
    # Syntax
    #   SuperCall[Yield, Await] :
    #       super Arguments[?Yield, ?Await]
    target = staticmethod(e.parse_SuperCall)
    target_argnames = ("Yield", "Await")
    productions = ((("super", "Arguments"), e.P2_SuperCall_SUPER_Arguments),)
    called_argnames = {"Arguments": ("?Yield", "?Await")}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### Arguments ##########################################################################
#
#        d8888                                                           888
#       d88888                                                           888
#      d88P888                                                           888
#     d88P 888 888d888  .d88b.  888  888 88888b.d88b.   .d88b.  88888b.  888888 .d8888b
#    d88P  888 888P"   d88P"88b 888  888 888 "888 "88b d8P  Y8b 888 "88b 888    88K
#   d88P   888 888     888  888 888  888 888  888  888 88888888 888  888 888    "Y8888b.
#  d8888888888 888     Y88b 888 Y88b 888 888  888  888 Y8b.     888  888 Y88b.       X88
# d88P     888 888      "Y88888  "Y88888 888  888  888  "Y8888  888  888  "Y888  88888P'
#                           888
#                      Y8b d88P
#                       "Y88P"
#
#########################################################################################
def test_P2_Arguments_init(context):
    args = e.P2_Arguments(context, "StrictArg", ["child"])
    assert args.name == "Arguments"
    assert args.context == context
    assert args.children == ["child"]


def test_P2_Arguments_LPAREN_RPAREN_init(context):
    args = e.P2_Arguments_LPAREN_RPAREN(context, "StrictArg", ["(", ")"])
    assert args.name == "Arguments"


def test_P2_Arguments_LPAREN_ArgumentList_RPAREN_init(context):
    args = e.P2_Arguments_LPAREN_ArgumentList_RPAREN(context, "StrictArg", ["(", "ArgumentList", ")"])
    assert args.name == "Arguments"
    assert args.ArgumentList == "ArgumentList"


def test_P2_Arguments_LPAREN_ArgumentList_COMMA_RPAREN_init(context):
    args = e.P2_Arguments_LPAREN_ArgumentList_COMMA_RPAREN(context, "StrictArg", ["(", "ArgumentList", ",", ")"])
    assert args.name == "Arguments"
    assert args.ArgumentList == "ArgumentList"


class Test_parse_Arguments(parse_test):
    # Syntax
    #   Arguments[Yield, Await] :
    #       ( )
    #       ( ArgumentList[?Yield, ?Await] )
    #       ( ArgumentList[?Yield, ?Await] , )
    target = staticmethod(e.parse_Arguments)
    target_argnames = ("Yield", "Await")
    productions = (
        (("(", ")"), e.P2_Arguments_LPAREN_RPAREN),
        (("(", "ArgumentList", ")"), e.P2_Arguments_LPAREN_ArgumentList_RPAREN),
        (("(", "ArgumentList", ",", ")"), e.P2_Arguments_LPAREN_ArgumentList_COMMA_RPAREN),
    )
    called_argnames = {"ArgumentList": ("?Yield", "?Await")}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### ArgumentList ###########################################################################################
#
#        d8888                                                           888    888      d8b          888
#       d88888                                                           888    888      Y8P          888
#      d88P888                                                           888    888                   888
#     d88P 888 888d888  .d88b.  888  888 88888b.d88b.   .d88b.  88888b.  888888 888      888 .d8888b  888888
#    d88P  888 888P"   d88P"88b 888  888 888 "888 "88b d8P  Y8b 888 "88b 888    888      888 88K      888
#   d88P   888 888     888  888 888  888 888  888  888 88888888 888  888 888    888      888 "Y8888b. 888
#  d8888888888 888     Y88b 888 Y88b 888 888  888  888 Y8b.     888  888 Y88b.  888      888      X88 Y88b.
# d88P     888 888      "Y88888  "Y88888 888  888  888  "Y8888  888  888  "Y888 88888888 888  88888P'  "Y888
#                           888
#                      Y8b d88P
#                       "Y88P"
#
#############################################################################################################
def test_P2_ArgumentList_init(context):
    al = e.P2_ArgumentList(context, "StrictArg", ["child"])
    assert al.name == "ArgumentList"
    assert al.context == context
    assert al.children == ["child"]


def test_P2_ArgumentList_AssignmentExpression_init(context):
    al = e.P2_ArgumentList_AssignmentExpression(context, "StrictArg", ["AssignmentExpression"])
    assert al.name == "ArgumentList"
    assert al.AssignmentExpression == "AssignmentExpression"


def test_P2_ArgumentList_DOTDOTDOT_AssignmentExpression_init(context):
    al = e.P2_ArgumentList_DOTDOTDOT_AssignmentExpression(context, "StrictArg", ["...", "AssignmentExpression"])
    assert al.name == "ArgumentList"
    assert al.AssignmentExpression == "AssignmentExpression"


def test_P2_ArgumentList_ArgumentList_COMMA_AssignmentExpression_init(context):
    al = e.P2_ArgumentList_ArgumentList_COMMA_AssignmentExpression(
        context, "StrictArg", ["ArgumentList", ",", "AssignmentExpression"]
    )
    assert al.name == "ArgumentList"
    assert al.ArgumentList == "ArgumentList"
    assert al.AssignmentExpression == "AssignmentExpression"


def test_P2_ArgumentList_ArgumentList_COMMA_DOTDOTDOT_AssignmentExpression_init(context):
    al = e.P2_ArgumentList_ArgumentList_COMMA_DOTDOTDOT_AssignmentExpression(
        context, "StrictArg", ["ArgumentList", ",", "...", "AssignmentExpression"]
    )
    assert al.name == "ArgumentList"
    assert al.ArgumentList == "ArgumentList"
    assert al.AssignmentExpression == "AssignmentExpression"


class Test_parse_ArgumentList(parse_test):
    # Syntax
    #   ArgumentList[Yield, Await]:
    #       AssignmentExpression[+In, ?Yield, ?Await]
    #       ... AssignmentExpression[+In, ?Yield, ?Await]
    #       ArgumentList[?Yield, ?Await] , AssignmentExpression[+In, ?Yield, ?Await]
    #       ArgumentList[?Yield, ?Await] , ... AssignmentExpression[+In, ?Yield, ?Await]
    target = staticmethod(e.parse_ArgumentList)
    target_argnames = ("Yield", "Await")
    AL_AE = e.P2_ArgumentList_AssignmentExpression
    AL_Rest = e.P2_ArgumentList_DOTDOTDOT_AssignmentExpression
    AL_AL_AE = e.P2_ArgumentList_ArgumentList_COMMA_AssignmentExpression
    AL_AL_Rest = e.P2_ArgumentList_ArgumentList_COMMA_DOTDOTDOT_AssignmentExpression
    productions = (
        (("AssignmentExpression",), AL_AE),
        (("...", "AssignmentExpression"), AL_Rest),
        (("AssignmentExpression", ",", "AssignmentExpression"), AL_AL_AE),
        (("AssignmentExpression", ",", "...", "AssignmentExpression"), AL_AL_Rest),
    )
    called_argnames = {"AssignmentExpression": ("+In", "?Yield", "?Await")}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### LefHandSideExpression #############################################################################################################################################################
#
# 888                .d888 888    888                        888  .d8888b.  d8b      888          8888888888                                                      d8b
# 888               d88P"  888    888                        888 d88P  Y88b Y8P      888          888                                                             Y8P
# 888               888    888    888                        888 Y88b.               888          888
# 888       .d88b.  888888 8888888888  8888b.  88888b.   .d88888  "Y888b.   888  .d88888  .d88b.  8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888      d8P  Y8b 888    888    888     "88b 888 "88b d88" 888     "Y88b. 888 d88" 888 d8P  Y8b 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888      88888888 888    888    888 .d888888 888  888 888  888       "888 888 888  888 88888888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# 888      Y8b.     888    888    888 888  888 888  888 Y88b 888 Y88b  d88P 888 Y88b 888 Y8b.     888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# 88888888  "Y8888  888    888    888 "Y888888 888  888  "Y88888  "Y8888P"  888  "Y88888  "Y8888  8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                                                                                     888
#                                                                                                                     888
#                                                                                                                     888
#
########################################################################################################################################################################################
def test_P2_LeftHandSideExpression_init(context):
    lhs = e.P2_LeftHandSideExpression(context, "StrictArg", ["child"], "YieldArg", "AwaitArg")
    assert lhs.name == "LeftHandSideExpression"
    assert lhs.context == context
    assert lhs.children == ["child"]
    assert lhs.Yield == "YieldArg"
    assert lhs.Await == "AwaitArg"


def test_P2_LeftHandSideExpression_NewExpression_init(context):
    lhs = e.P2_LeftHandSideExpression_NewExpression(context, "StrictArg", ["NewExpression"], "YieldArg", "AwaitArg")
    assert lhs.name == "LeftHandSideExpression"
    assert lhs.NewExpression == "NewExpression"


def test_P2_LeftHandSideExpression_CallExpression_init(context):
    lhs = e.P2_LeftHandSideExpression_CallExpression(
        context, "StrictArg", ["CallExpression"], "YieldArg", "AwaitArg"
    )
    assert lhs.name == "LeftHandSideExpression"
    assert lhs.CallExpression == "CallExpression"


class Test_parse_LeftHandSideExpression(parse_test):
    # Syntax
    #   LeftHandSideExpression[Yield, Await] :
    #       NewExpression[?Yield, ?Await]
    #       CallExpression[?Yield, ?Await]
    target = staticmethod(e.parse_LeftHandSideExpression)
    target_argnames = ("Yield", "Await")
    productions = (
        (("NewExpression",), e.P2_LeftHandSideExpression_NewExpression),
        (("CallExpression",), e.P2_LeftHandSideExpression_CallExpression),
    )
    called_argnames = {"NewExpression": ("?Yield", "?Await"), "CallExpression": ("?Yield", "?Await")}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### CallMemberExpression #########################################################################################################################################################
#
#  .d8888b.           888 888 888b     d888                        888                       8888888888                                                      d8b
# d88P  Y88b          888 888 8888b   d8888                        888                       888                                                             Y8P
# 888    888          888 888 88888b.d88888                        888                       888
# 888         8888b.  888 888 888Y88888P888  .d88b.  88888b.d88b.  88888b.   .d88b.  888d888 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888            "88b 888 888 888 Y888P 888 d8P  Y8b 888 "888 "88b 888 "88b d8P  Y8b 888P"   888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888    888 .d888888 888 888 888  Y8P  888 88888888 888  888  888 888  888 88888888 888     888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# Y88b  d88P 888  888 888 888 888   "   888 Y8b.     888  888  888 888 d88P Y8b.     888     888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
#  "Y8888P"  "Y888888 888 888 888       888  "Y8888  888  888  888 88888P"   "Y8888  888     8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                                                                                888
#                                                                                                                888
#                                                                                                                888
#
###################################################################################################################################################################################
def test_P2_CallMemberExpression_init(context):
    cme = e.P2_CallMemberExpression(context, "StrictArg", ["child"])
    assert cme.name == "CallMemberExpression"
    assert cme.context == context
    assert cme.children == ["child"]


def test_P2_CallMemberExpression_MemberExpression_Arguments_init(context):
    cme = e.P2_CallMemberExpression_MemberExpression_Arguments(
        context, "StrictArg", ["MemberExpression", "Arguments"]
    )
    assert cme.name == "CallMemberExpression"
    assert cme.MemberExpression == "MemberExpression"
    assert cme.Arguments == "Arguments"


class Test_parse_CallMemberExpression(parse_test):
    # Syntax
    #   CallMemberExpression[Yield, Await] :
    #       MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
    target = staticmethod(e.parse_CallMemberExpression)
    target_argnames = ("Yield", "Await")
    productions = ((("MemberExpression", "Arguments"), e.P2_CallMemberExpression_MemberExpression_Arguments),)
    called_argnames = {"MemberExpression": ("?Yield", "?Await"), "Arguments": ("?Yield", "?Await")}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


####################################################################################
#
#  d888    .d8888b.       .d8888b.       d888        d888
# d8888   d88P  Y88b     d88P  Y88b     d8888       d8888
#   888          888          .d88P       888         888
#   888        .d88P         8888"        888         888
#   888    .od888P"           "Y8b.       888         888
#   888   d88P"          888    888       888         888
#   888   888"       d8b Y88b  d88P d8b   888   d8b   888
# 8888888 888888888  Y8P  "Y8888P"  Y8P 8888888 Y8P 8888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Left-Hand-Side Expressions | Static Semantics
# 12.3.1.1 | Static Semantics: CoveredCallExpression
####################################################################################
class Test_StaticSemantics_CoveredCallExpression:
    # CallExpression : CoverCallExpressionAndAsyncArrowHead
    #   1. Return the CallMemberExpression that is covered by CoverCallExpressionAndAsyncArrowHead.
    @pytest.mark.parametrize("Yield", (False, True))
    @pytest.mark.parametrize("Await", (False, True))
    @strict_params
    def test_CallExpression(self, context, mocker, strict, Yield, Await):
        cceaaah = mocker.Mock(**{"covering.return_value": mocker.sentinel.cme})
        ce = e.P2_CallExpression_CoverCallExpressionAndAsyncArrowHead(context, strict, [cceaaah], Yield, Await)

        rv = ce.CoveredCallExpression()
        assert rv == mocker.sentinel.cme
        cceaaah.covering.assert_called_with(e.parse_CallMemberExpression, strict, Yield, Await)


####################################################################################
#
#  d888    .d8888b.       .d8888b.       d888        .d8888b.
# d8888   d88P  Y88b     d88P  Y88b     d8888       d88P  Y88b
#   888          888          .d88P       888              888
#   888        .d88P         8888"        888            .d88P
#   888    .od888P"           "Y8b.       888        .od888P"
#   888   d88P"          888    888       888       d88P"
#   888   888"       d8b Y88b  d88P d8b   888   d8b 888"
# 8888888 888888888  Y8P  "Y8888P"  Y8P 8888888 Y8P 888888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Left-Hand-Side Expressions | Static Semantics
# 12.3.1.2 | Static Semantics: Contains
####################################################################################
class Test_StaticSemantics_Contains:
    #   With parameter symbol.

    # MemberExpression : MemberExpression . IdentifierName
    #   1. If MemberExpression Contains symbol is true, return true.
    #   2. If symbol is a ReservedWord, return false.
    #   3. If symbol is an Identifier and StringValue of symbol is the same value as the StringValue of
    #      IdentifierName, return true.
    #   4. Return false.
    # CallExpression : CallExpression . IdentifierName
    #   1. If CallExpression Contains symbol is true, return true.
    #   2. If symbol is a ReservedWord, return false.
    #   3. If symbol is an Identifier and StringValue of symbol is the same value as the StringValue of
    #      IdentifierName, return true.
    #   4. Return false.
    @pytest.mark.parametrize(
        "ctor",
        (
            e.P2_CallExpression_CallExpression_PERIOD_IdentifierName,
            e.P2_MemberExpression_MemberExpression_PERIOD_IdentifierName,
        ),
    )
    @strict_params
    def test_dotname_child_true(self, context, mocker, strict, ctor):
        # child returns true
        me_child = mocker.Mock(**{"Contains.return_value": True})
        identname = mocker.Mock(value="identname")
        me = ctor(context, strict, [me_child, mocker.Mock(), identname])
        assert me.Contains("symbol") is True
        me_child.Contains.assert_called_with("symbol")

    @pytest.mark.parametrize(
        "ctor",
        (
            e.P2_CallExpression_CallExpression_PERIOD_IdentifierName,
            e.P2_MemberExpression_MemberExpression_PERIOD_IdentifierName,
        ),
    )
    @strict_params
    def test_dotname_reserved_word(self, context, mocker, strict, ctor):
        # Asked for a reserved word
        me_child = mocker.Mock(**{"Contains.return_value": False})
        identname = mocker.Mock(value="identname")
        me = ctor(context, strict, [me_child, mocker.Mock(), identname])
        assert me.Contains("for") is False
        me_child.Contains.assert_called_with("for")

    @pytest.mark.parametrize(
        "ctor",
        (
            e.P2_CallExpression_CallExpression_PERIOD_IdentifierName,
            e.P2_MemberExpression_MemberExpression_PERIOD_IdentifierName,
        ),
    )
    @pytest.mark.parametrize("probe, expectation", (("alice", False), ("identname", True)))
    @strict_params
    def test_dotname_idname(self, context, mocker, strict, probe, expectation, ctor):
        # Normal words
        me_child = mocker.Mock(**{"Contains.return_value": False})
        identname = mocker.Mock(value="identname")
        me = ctor(context, strict, [me_child, mocker.Mock(), identname])
        assert me.Contains(probe) is expectation
        me_child.Contains.assert_called_with(probe)

    # SuperProperty : super . IdentifierName
    #   1. If symbol is the ReservedWord super, return true.
    #   2. If symbol is a ReservedWord, return false.
    #   3. If symbol is an Identifier and StringValue of symbol is the same value as the StringValue of IdentifierName, return true.
    #   4. Return false.
    @pytest.mark.parametrize(
        "probe, expected", (("super", True), ("for", False), ("identname", True), ("alice", False))
    )
    @strict_params
    def test_super(self, context, mocker, strict, probe, expected):
        identname = mocker.Mock(value="identname")
        sp = e.P2_SuperProperty_SUPER_PERIOD_IdentifierName(
            context, strict, [mocker.Mock(), mocker.Mock(), identname]
        )
        assert sp.Contains(probe) is expected


####################################################################################
#
#  d888    .d8888b.       .d8888b.       d888        .d8888b.
# d8888   d88P  Y88b     d88P  Y88b     d8888       d88P  Y88b
#   888          888          .d88P       888            .d88P
#   888        .d88P         8888"        888           8888"
#   888    .od888P"           "Y8b.       888            "Y8b.
#   888   d88P"          888    888       888       888    888
#   888   888"       d8b Y88b  d88P d8b   888   d8b Y88b  d88P
# 8888888 888888888  Y8P  "Y8888P"  Y8P 8888888 Y8P  "Y8888P"
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Left-Hand-Side Expressions | Static Semantics
# 12.3.1.3 | Static Semantics: IsFunctionDefinition
####################################################################################
class Test_StaticSemantics_IsFunctionDefinition:

    # MemberExpression :
    #   MemberExpression [ Expression ]
    #   MemberExpression . IdentifierName
    #   MemberExpression TemplateLiteral
    #   SuperProperty
    #   MetaProperty
    #   new MemberExpression Arguments
    # NewExpression :
    #   new NewExpression
    # LeftHandSideExpression :
    #   CallExpression
    #
    #   1. Return false.
    @pytest.mark.parametrize(
        "ctor",
        (
            e.P2_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET,
            e.P2_MemberExpression_MemberExpression_PERIOD_IdentifierName,
            e.P2_MemberExpression_MemberExpression_TemplateLiteral,
            e.P2_MemberExpression_SuperProperty,
            e.P2_MemberExpression_MetaProperty,
            e.P2_MemberExpression_NEW_MemberExpression_Arguments,
            e.P2_NewExpression_NEW_NewExpression,
        ),
    )
    @strict_params
    def test_most(self, mocker, context, strict, ctor):
        pn = ctor(context, strict, [mocker.Mock(), mocker.Mock(), mocker.Mock(), mocker.Mock()])
        assert pn.IsFunctionDefinition() is False

    @pytest.mark.parametrize("Yield", (False, True))
    @pytest.mark.parametrize("Await", (False, True))
    @strict_params
    def test_LHS_Call(self, context, mocker, strict, Yield, Await):
        lhs = e.P2_LeftHandSideExpression_CallExpression(context, strict, [mocker.Mock()], Yield, Await)
        assert lhs.IsFunctionDefinition() is False


####################################################################################
#
#  d888    .d8888b.       .d8888b.       d888           d8888
# d8888   d88P  Y88b     d88P  Y88b     d8888          d8P888
#   888          888          .d88P       888         d8P 888
#   888        .d88P         8888"        888        d8P  888
#   888    .od888P"           "Y8b.       888       d88   888
#   888   d88P"          888    888       888       8888888888
#   888   888"       d8b Y88b  d88P d8b   888   d8b       888
# 8888888 888888888  Y8P  "Y8888P"  Y8P 8888888 Y8P       888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Left-Hand-Side Expressions | Static Semantics
# 12.3.1.4 | Static Semantics: IsDestructuring
####################################################################################
class Test_StaticSemantics_IsDestructuring:
    # MemberExpression :
    #   MemberExpression [ Expression ]
    #   MemberExpression . IdentifierName
    #   MemberExpression TemplateLiteral
    #   SuperProperty
    #   MetaProperty
    #   new MemberExpression Arguments
    # NewExpression :
    #   new NewExpression
    # LeftHandSideExpression :
    #   CallExpression
    #
    #   1. Return false.
    @pytest.mark.parametrize(
        "ctor",
        (
            e.P2_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET,
            e.P2_MemberExpression_MemberExpression_PERIOD_IdentifierName,
            e.P2_MemberExpression_MemberExpression_TemplateLiteral,
            e.P2_MemberExpression_SuperProperty,
            e.P2_MemberExpression_MetaProperty,
            e.P2_MemberExpression_NEW_MemberExpression_Arguments,
            e.P2_NewExpression_NEW_NewExpression,
        ),
    )
    @strict_params
    def test_most(self, mocker, context, strict, ctor):
        pn = ctor(context, strict, [mocker.Mock(), mocker.Mock(), mocker.Mock(), mocker.Mock()])
        assert pn.IsDestructuring() is False

    @pytest.mark.parametrize("Yield", (False, True))
    @pytest.mark.parametrize("Await", (False, True))
    @strict_params
    def test_LHS(self, context, mocker, strict, Yield, Await):
        pn = e.P2_LeftHandSideExpression_CallExpression(context, strict, [mocker.Mock()], Yield, Await)
        assert pn.IsDestructuring() is False

    # 12.3.1.4 Static Semantics: IsDestructuring
    # MemberExpression : PrimaryExpression
    #   1. If PrimaryExpression is either an ObjectLiteral or an ArrayLiteral, return true.
    #   2. Return false.
    @pytest.mark.parametrize("kind, expected", (("ObjectLiteral", True), ("ArrayLiteral", True), ("Other", False)))
    @strict_params
    def test_PrimaryExpression(self, mocker, context, kind, expected, strict):
        primary_expression = mocker.Mock(**{"Is.side_effect": lambda arg: arg == kind})
        me = e.P2_MemberExpression_PrimaryExpression(context, strict, [primary_expression])

        assert me.IsDestructuring() is expected


####################################################################################
#
#  d888    .d8888b.       .d8888b.       d888       888888888
# d8888   d88P  Y88b     d88P  Y88b     d8888       888
#   888          888          .d88P       888       888
#   888        .d88P         8888"        888       8888888b.
#   888    .od888P"           "Y8b.       888            "Y88b
#   888   d88P"          888    888       888              888
#   888   888"       d8b Y88b  d88P d8b   888   d8b Y88b  d88P
# 8888888 888888888  Y8P  "Y8888P"  Y8P 8888888 Y8P  "Y8888P"
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Left-Hand-Side Expressions | Static Semantics
# 12.3.1.5 | Static Semantics: IsIdentifierRef
####################################################################################
class Test_StaticSemantics_IsIdentifierRef:
    # MemberExpression :
    #   MemberExpression [ Expression ]
    #   MemberExpression . IdentifierName
    #   MemberExpression TemplateLiteral
    #   SuperProperty
    #   MetaProperty
    #   new MemberExpression Arguments
    # NewExpression :
    #   new NewExpression
    # LeftHandSideExpression :
    #   CallExpression
    #
    #   1. Return false.
    @pytest.mark.parametrize(
        "ctor",
        (
            e.P2_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET,
            e.P2_MemberExpression_MemberExpression_PERIOD_IdentifierName,
            e.P2_MemberExpression_MemberExpression_TemplateLiteral,
            e.P2_MemberExpression_SuperProperty,
            e.P2_MemberExpression_MetaProperty,
            e.P2_MemberExpression_NEW_MemberExpression_Arguments,
            e.P2_NewExpression_NEW_NewExpression,
        ),
    )
    @strict_params
    def test_most(self, mocker, context, strict, ctor):
        pn = ctor(context, strict, [mocker.Mock(), mocker.Mock(), mocker.Mock(), mocker.Mock()])
        assert pn.IsIdentifierRef() is False

    @pytest.mark.parametrize("Yield", (False, True))
    @pytest.mark.parametrize("Await", (False, True))
    @strict_params
    def test_LHS(self, context, mocker, strict, Yield, Await):
        lhs = e.P2_LeftHandSideExpression_CallExpression(context, strict, [mocker.Mock()], Yield, Await)
        assert lhs.IsIdentifierRef() is False


####################################################################################
#
#  d888    .d8888b.       .d8888b.       d888        .d8888b.
# d8888   d88P  Y88b     d88P  Y88b     d8888       d88P  Y88b
#   888          888          .d88P       888       888
#   888        .d88P         8888"        888       888d888b.
#   888    .od888P"           "Y8b.       888       888P "Y88b
#   888   d88P"          888    888       888       888    888
#   888   888"       d8b Y88b  d88P d8b   888   d8b Y88b  d88P
# 8888888 888888888  Y8P  "Y8888P"  Y8P 8888888 Y8P  "Y8888P"
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Left-Hand-Side Expressions | Static Semantics
# 12.3.1.6 | Static Semantics: AssignmentTargetType
####################################################################################
class Test_StaticSemantics_AssignmentTargetType:
    # CallExpression :
    #   CallExpression [ Expression ]
    #   CallExpression . IdentifierName
    # MemberExpression :
    #   MemberExpression [ Expression ]
    #   MemberExpression . IdentifierName
    #   SuperProperty
    #
    #   1. Return simple.
    @pytest.mark.parametrize(
        "ctor",
        (
            e.P2_CallExpression_CallExpression_LBRACKET_Expression_RBRACKET,
            e.P2_CallExpression_CallExpression_PERIOD_IdentifierName,
            e.P2_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET,
            e.P2_MemberExpression_MemberExpression_PERIOD_IdentifierName,
            e.P2_MemberExpression_SuperProperty,
        ),
    )
    @strict_params
    def test_simple(self, mocker, context, strict, ctor):
        pn = ctor(context, strict, [mocker.Mock(), mocker.Mock(), mocker.Mock(), mocker.Mock()])
        assert pn.AssignmentTargetType == e.SIMPLE

    # CallExpression:
    #   CoverCallExpressionAndAsyncArrowHead
    #   SuperCall
    #   CallExpression Arguments
    #   CallExpression TemplateLiteral
    # NewExpression:
    #   new NewExpression
    # MemberExpression:
    #   MemberExpression TemplateLiteral
    #   new MemberExpression Arguments
    # NewTarget:
    #   new . target
    #
    #   1. Return invalid.
    @pytest.mark.parametrize(
        "ctor",
        (
            e.P2_CallExpression_SuperCall,
            e.P2_CallExpression_CallExpression_Arguments,
            e.P2_CallExpression_CallExpression_TemplateLiteral,
            e.P2_NewExpression_NEW_NewExpression,
            e.P2_MemberExpression_MemberExpression_TemplateLiteral,
            e.P2_MemberExpression_NEW_MemberExpression_Arguments,
            e.P2_NewTarget_NEW_PERIOD_TARGET,
        ),
    )
    @strict_params
    def test_invalid(self, mocker, context, strict, ctor):
        pn = ctor(context, strict, [mocker.Mock(), mocker.Mock(), mocker.Mock()])
        assert pn.AssignmentTargetType == e.INVALID

    @pytest.mark.parametrize("ctor", (e.P2_CallExpression_CoverCallExpressionAndAsyncArrowHead,))
    @pytest.mark.parametrize("Yield", (False, True))
    @pytest.mark.parametrize("Await", (False, True))
    @strict_params
    def test_invalid2(self, mocker, context, strict, ctor, Yield, Await):
        pn = ctor(context, strict, [mocker.Mock(), mocker.Mock(), mocker.Mock()], Yield, Await)
        assert pn.AssignmentTargetType == e.INVALID


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.       d888
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b     d8888
#   888          888          .d88P            888       888
#   888        .d88P         8888"           .d88P       888
#   888    .od888P"           "Y8b.      .od888P"        888
#   888   d88P"          888    888     d88P"            888
#   888   888"       d8b Y88b  d88P d8b 888"       d8b   888
# 8888888 888888888  Y8P  "Y8888P"  Y8P 888888888  Y8P 8888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Left-Hand-Side Expressions | Property Accessors
# 12.3.2.1 | Runtime Semantics: Evaluation
####################################################################################
class Test_PropertyAccessors_Evaluation:
    # MemberExpression : MemberExpression [ Expression ]
    #   1. Let baseReference be the result of evaluating MemberExpression.
    #   2. Let baseValue be ? GetValue(baseReference).
    #   3. Let propertyNameReference be the result of evaluating Expression.
    #   4. Let propertyNameValue be ? GetValue(propertyNameReference).
    #   5. Let bv be ? RequireObjectCoercible(baseValue).
    #   6. Let propertyKey be ? ToPropertyKey(propertyNameValue).
    #   7. If the code matched by this MemberExpression is strict mode code, let strict be true, else let strict be
    #      false.
    #   8. Return a value of type Reference whose base value component is bv, whose referenced name component is
    #      propertyKey, and whose strict reference flag is strict.
    # CallExpression : CallExpression [ Expression ]
    #   Is evaluated in exactly the same manner as MemberExpression : MemberExpression [ Expression ] except that
    #   the contained CallExpression is evaluated in step 1.
    @pytest.mark.parametrize(
        "ctor",
        (
            e.P2_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET,
            e.P2_CallExpression_CallExpression_LBRACKET_Expression_RBRACKET,
        ),
    )
    @strict_params
    def test_evaluate(self, mocker, context, strict, ctor):
        first_expression = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.base_reference})
        gv = mocker.patch(
            "ecmascript.ecmascript.GetValue",
            side_effect=(mocker.sentinel.base_value, mocker.sentinel.property_name_value),
        )
        expression = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.property_name_reference})
        roc = mocker.patch("ecmascript.ecmascript.RequireObjectCoercible", return_value=mocker.sentinel.bv)
        tpk = mocker.patch("ecmascript.ecmascript.ToPropertyKey", return_value=mocker.sentinel.property_key)
        target = ctor(context, strict, [first_expression, mocker.Mock(), expression, mocker.Mock()])
        rv = target.evaluate()
        assert all(hasattr(rv, field) for field in ("base", "name", "strict"))
        assert rv.base == mocker.sentinel.bv
        assert rv.name == mocker.sentinel.property_key
        assert rv.strict == strict
        first_expression.evaluate.assert_called_with()
        assert gv.call_args_list == [
            mocker.call(mocker.sentinel.base_reference),
            mocker.call(mocker.sentinel.property_name_reference),
        ]
        expression.evaluate.assert_called_with()
        roc.assert_called_with(mocker.sentinel.base_value)
        tpk.assert_called_with(mocker.sentinel.property_name_value)

    # MemberExpression : MemberExpression . IdentifierName
    #   1. Let baseReference be the result of evaluating MemberExpression.
    #   2. Let baseValue be ? GetValue(baseReference).
    #   3. Let bv be ? RequireObjectCoercible(baseValue).
    #   4. Let propertyNameString be StringValue of IdentifierName.
    #   5. If the code matched by this MemberExpression is strict mode code, let strict be true, else let strict be
    #      false.
    #   6. Return a value of type Reference whose base value component is bv, whose referenced name component is
    #      propertyNameString, and whose strict reference flag is strict.
    # CallExpression : CallExpression . IdentifierName
    #   Is evaluated in exactly the same manner as MemberExpression : MemberExpression . IdentifierName except that
    #   the contained CallExpression is evaluated in step 1.
    @pytest.mark.parametrize(
        "ctor",
        (
            e.P2_MemberExpression_MemberExpression_PERIOD_IdentifierName,
            e.P2_CallExpression_CallExpression_PERIOD_IdentifierName,
        ),
    )
    @strict_params
    def test_dotname(self, context, mocker, strict, ctor):
        me_child = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.base_reference})
        gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value=mocker.sentinel.base_value)
        roc = mocker.patch("ecmascript.ecmascript.RequireObjectCoercible", return_value=mocker.sentinel.bv)
        ident = mocker.Mock(value="identname")
        me = ctor(context, strict, [me_child, mocker.Mock(), ident])
        rv = me.evaluate()
        assert isinstance(rv, e.Reference)
        assert rv.base == mocker.sentinel.bv
        assert rv.name == "identname"
        assert rv.strict == strict
        me_child.evaluate.assert_called_with()
        gv.assert_called_with(mocker.sentinel.base_reference)
        roc.assert_called_with(mocker.sentinel.base_value)


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.       d888
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b     d8888
#   888          888          .d88P          .d88P       888
#   888        .d88P         8888"          8888"        888
#   888    .od888P"           "Y8b.          "Y8b.       888
#   888   d88P"          888    888     888    888       888
#   888   888"       d8b Y88b  d88P d8b Y88b  d88P d8b   888
# 8888888 888888888  Y8P  "Y8888P"  Y8P  "Y8888P"  Y8P 8888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Left-Hand-Side Expressions | The new Operator
# 12.3.3.1 | Runtime Semantics: Evaluation
####################################################################################
class Test_NewOperator_Evaluation:
    # NewExpression : new NewExpression
    #   1. Return ? EvaluateNew(NewExpression, empty).
    @strict_params
    def test_new_expression(self, context, mocker, strict):
        newexp_child = mocker.sentinel.new_expression
        newexp = e.P2_NewExpression_NEW_NewExpression(context, strict, [mocker.Mock(), newexp_child])
        en = mocker.patch("ecmascript.ecmascript.EvaluateNew", return_value=mocker.sentinel.return_value)
        rv = newexp.evaluate()
        assert rv == mocker.sentinel.return_value
        en.assert_called_with(newexp_child, e.EMPTY)

    # MemberExpression : new MemberExpression Arguments
    #   1. Return ? EvaluateNew(MemberExpression, Arguments).
    @strict_params
    def test_new_args(self, context, mocker, strict):
        memb_child = mocker.sentinel.member_expression
        args = mocker.sentinel.arguments
        me = e.P2_MemberExpression_NEW_MemberExpression_Arguments(context, strict, [mocker.Mock(), memb_child, args])
        en = mocker.patch("ecmascript.ecmascript.EvaluateNew", return_value=mocker.sentinel.return_value)
        rv = me.evaluate()
        assert rv == mocker.sentinel.return_value
        en.assert_called_with(memb_child, args)

    # 12.3.3.1.1: Runtime Semantics: EvaluateNew ( constructExpr, arguments )
    #   1. Assert: constructExpr is either a NewExpression or a MemberExpression.
    #   2. Assert: arguments is either empty or an Arguments.
    #   3. Let ref be the result of evaluating constructExpr.
    #   4. Let constructor be ? GetValue(ref).
    #   5. If arguments is empty, let argList be a new empty List.
    #   6. Else,
    #      a. Let argList be ArgumentListEvaluation of arguments.
    #      b. ReturnIfAbrupt(argList).
    #   7. If IsConstructor(constructor) is false, throw a TypeError exception.
    #   8. Return ? Construct(constructor, argList).
    def test_EvaluateNew_01(self, context, mocker):
        # Arguments object _and_ IsConstructor
        ce = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.ref})
        ce.configure_mock(name="NewExpression")
        args = mocker.Mock(**{"ArgumentListEvaluation.return_value": [mocker.sentinel.argList]})
        args.configure_mock(name="Arguments")
        gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value=mocker.sentinel.constructor)
        ic = mocker.patch("ecmascript.ecmascript.IsConstructor", return_value=True)
        construct = mocker.patch("ecmascript.ecmascript.Construct", return_value=mocker.sentinel.return_value)

        rv = e.EvaluateNew(ce, args)
        assert rv == mocker.sentinel.return_value
        ce.evaluate.assert_called_with()
        gv.assert_called_with(mocker.sentinel.ref)
        args.ArgumentListEvaluation.assert_called_with()
        ic.assert_called_with(mocker.sentinel.constructor)
        construct.assert_called_with(mocker.sentinel.constructor, [mocker.sentinel.argList])

    def test_EvaluateNew_02(self, context, mocker):
        # No Arguments object
        ce = mocker.Mock(**{"evaluate.return_value": mocker.sentinel.ref})
        ce.configure_mock(name="NewExpression")
        gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value=mocker.sentinel.constructor)
        ic = mocker.patch("ecmascript.ecmascript.IsConstructor", return_value=True)
        construct = mocker.patch("ecmascript.ecmascript.Construct", return_value=mocker.sentinel.return_value)

        rv = e.EvaluateNew(ce, e.EMPTY)
        assert rv == mocker.sentinel.return_value
        ce.evaluate.assert_called_with()
        gv.assert_called_with(mocker.sentinel.ref)
        ic.assert_called_with(mocker.sentinel.constructor)
        construct.assert_called_with(mocker.sentinel.constructor, [])

    def test_EvaluateNew_03(self, context, mocker):
        # Isn't actually a constructor
        ce = mocker.Mock(
            **{"evaluate.return_value": mocker.sentinel.ref, "matched_source.return_value": "an invalid constructor"}
        )
        ce.configure_mock(name="NewExpression")
        gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value=mocker.sentinel.constructor)
        ic = mocker.patch("ecmascript.ecmascript.IsConstructor", return_value=False)

        with pytest.raises(e.ESTypeError):
            e.EvaluateNew(ce, e.EMPTY)

        ce.evaluate.assert_called_with()
        gv.assert_called_with(mocker.sentinel.ref)
        ic.assert_called_with(mocker.sentinel.constructor)


####################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.       d888
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b     d8888
#   888          888          .d88P          .d88P       888
#   888        .d88P         8888"          8888"        888
#   888    .od888P"           "Y8b.          "Y8b.       888
#   888   d88P"          888    888     888    888       888
#   888   888"       d8b Y88b  d88P d8b Y88b  d88P d8b   888
# 8888888 888888888  Y8P  "Y8888P"  Y8P  "Y8888P"  Y8P 8888888
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Left-Hand-Side Expressions | Function Calls
# 12.4.3.1 | Runtime Semantics: Evaluation
####################################################################################
class Test_FunctionCalls_Evaluation:
    # CallExpression : CoverCallExpressionAndAsyncArrowHead
    #   1. Let expr be CoveredCallExpression of CoverCallExpressionAndAsyncArrowHead.
    #   2. Let memberExpr be the MemberExpression of expr.
    #   3. Let arguments be the Arguments of expr.
    #   4. Let ref be the result of evaluating memberExpr.
    #   5. Let func be ? GetValue(ref).
    #   6. If Type(ref) is Reference and IsPropertyReference(ref) is false and GetReferencedName(ref) is "eval",
    #      then
    #       a. If SameValue(func, %eval%) is true, then
    #           i. Let argList be ? ArgumentListEvaluation of arguments.
    #           ii. If argList has no elements, return undefined.
    #           iii. Let evalText be the first element of argList.
    #           iv. If the source code matching this CallExpression is strict mode code, let strictCaller be true.
    #               Otherwise let strictCaller be false.
    #           v. Let evalRealm be the current Realm Record.
    #           vi. Perform ? HostEnsureCanCompileStrings(evalRealm, evalRealm).
    #           vii. Return ? PerformEval(evalText, evalRealm, strictCaller, true).
    #   7. Let thisCall be this CallExpression.
    #   8. Let tailCall be IsInTailPosition(thisCall).
    #   9. Return ? EvaluateCall(func, ref, arguments, tailCall).
    @pytest.mark.parametrize("Yield", (False, True))
    @pytest.mark.parametrize("Await", (False, True))
    @strict_params
    def test_covercallexp_noteval(self, context, mocker, strict, Yield, Await):
        # Not Eval (IsPropertyReference is TRUE)
        ref = mocker.Mock(spec=e.Reference)
        memberExpr = mocker.Mock(**{"evaluate.return_value": ref})
        expr = mocker.Mock(MemberExpression=memberExpr, Arguments=mocker.sentinel.arguments)
        cceaaah = mocker.sentinel.cceaaah
        gv = mocker.patch("ecmascript.ecmascript.GetValue", return_value=mocker.sentinel.func)
        ipr = mocker.patch("ecmascript.ecmascript.IsPropertyReference", return_value=True)
        iitp = mocker.patch("ecmascript.ecmascript.IsInTailPosition", return_value=mocker.sentinel.tailCall)
        ec = mocker.patch("ecmascript.ecmascript.EvaluateCall", return_value=mocker.sentinel.return_value)
        ce = e.P2_CallExpression_CoverCallExpressionAndAsyncArrowHead(context, strict, [cceaaah], Yield, Await)
        ce.CoveredCallExpression = mocker.Mock(return_value=expr)

        rv = ce.evaluate()
        assert rv == mocker.sentinel.return_value
        ce.CoveredCallExpression.assert_called_with()
        memberExpr.evaluate.assert_called_with()
        gv.assert_called_with(ref)
        ipr.assert_called_with(ref)
        iitp.assert_called_with(ce)
        ec.assert_called_with(mocker.sentinel.func, ref, mocker.sentinel.arguments, mocker.sentinel.tailCall)

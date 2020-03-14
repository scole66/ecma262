import pytest
import snoop
from enum import Enum, unique, auto
import types
from itertools import chain, product
from typing import Iterable, Tuple

from .helpers import (
    lexer_mock,
    prod_streams,
    synerror2_streams,
    prod_mocks,
    first,
    prod_args,
    ordinary_test_params,
    syntax_error_test_params,
    parse_test,
    strict_params,
    FakeTokens,
)

import ecmascript.ecmascript as e
from ecmascript.lexer2 import Span

from pprint import pprint


class Token:
    def __init__(self, tokentype, value, newlines=[(0, 1)]):
        self.type = tokentype
        self.value = value
        self.newlines = tuple(newlines)
        self.src = value
        self.span = Span(0, 10)

    def __repr__(self):
        return f"Token[{self.type}({self.value})]"


class Lexer:
    def __init__(self, token_sequence):
        self.pos = 0
        self.sequence = token_sequence

    def peek_token(self, count=1, goal="unused"):
        if self.pos < len(self.sequence):
            if count == 1:
                return self.sequence[self.pos]
            num_to_copy = min(len(self.sequence) - self.pos, count)
            num_to_pad = count - num_to_copy
            return self.sequence[self.pos : self.pos + num_to_copy] + [None] * num_to_pad
        if count == 1:
            return None
        return [None] * count

    def next_token(self, goal=0):
        if self.pos < len(self.sequence):
            rv = self.sequence[self.pos]
            self.pos += 1
            return rv
        return None

    def next_token_if(self, tok_type, prior_newline_allowed=True, goal="usused"):
        peek = self.peek_token()
        if peek and peek.type == tok_type and (prior_newline_allowed or not peek.newlines):
            return self.next_token()
        return None

    def next_token_asi(self, do_while=False, goal="unused"):
        peek = self.peek_token()
        if not peek:
            return Token(";", ";", [])
        else:
            if peek.type == ";":
                return self.next_token()
            if peek.newlines or peek.type == "}" or do_while:
                return Token(";", ";", [])
        return None

    def next_id_if(self, id_value, prior_newline_allowed=True):
        peek = self.peek_token()
        if (
            peek
            and peek.type == "IDENTIFIER"
            and peek.value == id_value
            and (prior_newline_allowed or not peek.newlines)
        ):
            return self.next_token()
        return None

    def current_position(self):
        return self.pos

    def reset_position(self, place):
        self.pos = place

    reserved_words = (
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
    )

    @unique
    class Goal(Enum):
        InputElementDiv = auto()
        InputElementRegExp = auto()
        InputElementRegExpOrTemplateTail = auto()
        InputElementTemplateTail = auto()

    InputElementDiv = Goal.InputElementDiv
    InputElementRegExp = Goal.InputElementRegExp
    InputElementRegExpOrTemplateTail = Goal.InputElementRegExpOrTemplateTail
    InputElementTemplateTail = Goal.InputElementTemplateTail


# Useful pre-generated tokens
THIS = Token("IDENTIFIER", "this")
IBOB = Token("IDENTIFIER", "bob")
FALSE = Token("IDENTIFIER", "false")
TRUE = Token("IDENTIFIER", "true")
NULL = Token("IDENTIFIER", "null")
SALICE = Token("STRING", "alice")
FOR = Token("IDENTIFIER", "for")
IF = Token("IDENTIFIER", "if")
ELSE = Token("IDENTIFIER", "else")
LBRACKET = Token("[", "[")
RBRACKET = Token("]", "]")
THREE = Token("NUMERIC", 3)
PERIOD = Token(".", ".")
DOTDOTDOT = Token("...", "...")
ICHARLIE = Token("IDENTIFIER", "charlie")
NST = Token("NOSUBSTITUTIONTEMPLATE", ("a", "a"))
TEMPLATEHEAD = Token("TEMPLATEHEAD", ("zz", "zz"))
TEMPLATEMIDDLE = Token("TEMPLATEMIDDLE", ("gg", "gg"))
TEMPLATETAIL = Token("TEMPLATETAIL", ("mm", "mm"))
SUPER = Token("IDENTIFIER", "super")
NEW = Token("IDENTIFIER", "new")
TARGET = Token("IDENTIFIER", "target")
LPAREN = Token("(", "(")
RPAREN = Token(")", ")")
FUNCTION = Token("IDENTIFIER", "function")
FUNCTION_NONL = Token("IDENTIFIER", "function", newlines=[])
LCURLY = Token("{", "{")
RCURLY = Token("}", "}")
SEMICOLON = Token(";", ";")
PERCENT = Token("%", "%")
STAR = Token("*", "*")
SLASH = Token("/", "/")
COMMA = Token(",", ",")
COLON = Token(":", ":")
PLUSPLUS = Token("++", "++")
MINUSMINUS = Token("--", "--")
PLUSPLUS_NONL = Token("++", "++", newlines=[])
MINUSMINUS_NONL = Token("--", "--", newlines=[])
DELETE = Token("IDENTIFIER", "delete")
VOID = Token("IDENTIFIER", "void")
TYPEOF = Token("IDENTIFIER", "typeof")
PLUS = Token("+", "+")
MINUS = Token("-", "-")
TILDE = Token("~", "~")
BANG = Token("!", "!")
LTLT = Token("<<", "<<")
GTGT = Token(">>", ">>")
GTGTGT = Token(">>>", ">>>")
YIELD = Token("IDENTIFIER", "yield")
YIELD_ESCAPED = Token("IDENTIFIER", "yield")
YIELD_ESCAPED.src = "\\u0079ield"
AWAIT = Token("IDENTIFIER", "await")
ASYNC = Token("IDENTIFIER", "async")
CLASS = Token("IDENTIFIER", "class")
LET = Token("IDENTIFIER", "let")
CONST = Token("IDENTIFIER", "const")
STARSTAR = Token("**", "**")
LT = Token("<", "<")
GT = Token(">", ">")
LE = Token("<=", "<=")
GE = Token(">=", ">=")
INSTANCEOF = Token("IDENTIFIER", "instanceof")
IN = Token("IDENTIFIER", "in")
EQEQ = Token("==", "==")
EQEQEQ = Token("===", "===")
BANGEQ = Token("!=", "!=")
BANGEQEQ = Token("!==", "!==")
AMP = Token("&", "&")
EQ = Token("=", "=")
CARET = Token("^", "^")
PIPE = Token("|", "|")
AMPAMP = Token("&&", "&&")
PIPEPIPE = Token("||", "||")
QUESTION = Token("?", "?")
STAREQ = Token("*=", "*=")
SLASHEQ = Token("/=", "/=")
PERCENTEQ = Token("%=", "%=")
PLUSEQ = Token("+=", "+=")
MINUSEQ = Token("-=", "-=")
LTLTEQ = Token("<<=", "<<=")
GTGTEQ = Token(">>=", ">>=")
GTGTGTEQ = Token(">>>=", ">>>=")
AMPEQ = Token("&=", "&=")
CARETEQ = Token("^=", "^=")
PIPEEQ = Token("|=", "|=")
STARSTAREQ = Token("**=", "**=")
VAR = Token("IDENTIFIER", "var")
DO = Token("IDENTIFIER", "do")
WHILE = Token("IDENTIFIER", "while")
OF = Token("IDENTIFIER", "of")
CONTINUE = Token("IDENTIFIER", "continue")
BREAK = Token("IDENTIFIER", "break")
RETURN = Token("IDENTIFIER", "return")
WITH = Token("IDENTIFIER", "with")
SWITCH = Token("IDENTIFIER", "switch")
CASE = Token("IDENTIFIER", "case")
DEFAULT = Token("IDENTIFIER", "default")
TRY = Token("IDENTIFIER", "try")
CATCH = Token("IDENTIFIER", "catch")
FINALLY = Token("IDENTIFIER", "finally")
THROW = Token("IDENTIFIER", "throw")
DEBUGGER = Token("IDENTIFIER", "debugger")
EQGT = Token("=>", "=>")
EQGT_NONL = Token("=>", "=>", newlines=[])

MATCHES_NONE = Token("MATCHES_NONE", "nope", [])


def sideeffect(symbol, obj):
    def side_effect(context, lexer, *args):
        peek = lexer.peek_token()
        if peek and peek.value == symbol:
            if peek.type == "REPLACEMENT":
                lexer.pos += 1
                return peek.value
            if peek.type == "REPLOBJ":
                lexer.pos += 1
                return obj
        return None

    return side_effect


def parser_mock(symbol, obj=None):
    toktype = "REPLACEMENT" if not obj else "REPLOBJ"
    return (Token(toktype, symbol), sideeffect(symbol, obj))


class NotStringLiteral:
    IsStringLiteral = False
    HasUseStrict = False
    strict = False

    def __init__(self, name):
        self.name = name


PRIMARYEXPRESSION, primaryexpression_sideeffect = parser_mock("PrimaryExpression")
ME_REPLACEMENT, me_sideeffect = parser_mock("MemberExpression")
CCE_REPLACEMENT, cce_sideeffect = parser_mock("CoverCallExpressionAndAsyncArrowHead")
EXPRESSION, expression_sideeffect = parser_mock("Expression")
EXPRESSION_NONL = Token("REPLACEMENT", "Expression", newlines=[])
ARGS_REPLACEMENT, args_sideeffect = parser_mock("Arguments")
AL_REPLACEMENT, al_sideeffect = parser_mock("ArgumentList")
TL_REPLACEMENT, tl_sideeffect = parser_mock("TemplateLiteral")
SP_REPLACEMENT, sp_sideeffect = parser_mock("SuperProperty")
MP_REPLACEMENT, mp_sideeffect = parser_mock("MetaProperty")
NT_REPLACEMENT, nt_sideeffect = parser_mock("NewTarget")
SC_REPLACEMENT, sc_sideeffect = parser_mock("SuperCall")
AE_REPLACEMENT, ae_sideeffect = parser_mock("AssignmentExpression")
NE_REPLACEMENT, ne_sideeffect = parser_mock("NewExpression")
CE_REPLACEMENT, ce_sideeffect = parser_mock("CallExpression")
LHS_REPLACEMENT, lhs_sideeffect = parser_mock("LeftHandSideExpression")
UNE_REPLACEMENT, une_sideeffect = parser_mock("UnaryExpression")
UPE_REPLACEMENT, upe_sideeffect = parser_mock("UpdateExpression")
EE_REPLACEMENT, ee_sideeffect = parser_mock("ExponentiationExpression")
MO_REPLACEMENT, mo_sideeffect = parser_mock("MultiplicativeOperator")
AWE_REPLACEMENT, awe_sideeffect = parser_mock("AwaitExpression")
BINDINGIDENTIFIER, bindingidentifier_sideeffect = parser_mock("BindingIdentifier")
BINDINGPATTERN, bindingpattern_sideeffect = parser_mock("BindingPattern")
ELISION_REPLACEMENT, elision_sideeffect = parser_mock("Elision")
EL_REPLACEMENT, el_sideeffect = parser_mock("ElementList")
SE_REPLACEMENT, se_sideeffect = parser_mock("SpreadElement")
PDL_REPLACEMENT, pdl_sideeffect = parser_mock("PropertyDefinitionList")
PD_REPLACEMENT, pd_sideeffect = parser_mock("PropertyDefinition")
IR_REPLACEMENT, ir_sideeffect = parser_mock("IdentifierReference")
CIN_REPLACEMENT, cin_sideeffect = parser_mock("CoverInitializedName")
PN_REPLACEMENT, pn_sideeffect = parser_mock("PropertyName")
MD_REPLACEMENT, md_sideeffect = parser_mock("MethodDefinition")
LPN_REPLACEMENT, lpn_sideeffect = parser_mock("LiteralPropertyName")
CPN_REPLACEMENT, cpn_sideeffect = parser_mock("ComputedPropertyName")
INIT_REPLACEMENT, init_sideeffect = parser_mock("Initializer")
ST_REPLACEMENT, st_sideeffect = parser_mock("SubstitutionTemplate")
TS_REPLACEMENT, ts_sideeffect = parser_mock("TemplateSpans")
TML_REPLACEMENT, tml_sideeffect = parser_mock("TemplateMiddleList")
BLOCK_STMT_REPLACEMENT, block_statement_sideeffect = parser_mock("BlockStatement")
VARIABLE_STMT_REPLACEMENT, variable_statement_sideeffect = parser_mock("VariableStatement")
EMPTY_STMT_REPLACEMENT, empty_statement_sideeffect = parser_mock("EmptyStatement")
EXPRESSION_STMT_REPLACEMENT, expression_statement_sideeffect = parser_mock("ExpressionStatement")
IF_STMT_REPLACEMENT, if_statement_sideeffect = parser_mock("IfStatement")
BREAKABLE_STMT_REPLACEMENT, breakable_statement_sideeffect = parser_mock("BreakableStatement")
CONTINUE_STMT_REPLACEMENT, continue_statement_sideeffect = parser_mock("ContinueStatement")
BREAK_STMT_REPLACEMENT, break_statement_sideeffect = parser_mock("BreakStatement")
RETURN_STMT_REPLACEMENT, return_statement_sideeffect = parser_mock("ReturnStatement")
WITH_STMT_REPLACEMENT, with_statement_sideeffect = parser_mock("WithStatement")
LABELLED_STMT_REPLACEMENT, labelled_statement_sideeffect = parser_mock("LabelledStatement")
THROW_STMT_REPLACEMENT, throw_statement_sideeffect = parser_mock("ThrowStatement")
TRY_STMT_REPLACEMENT, try_statement_sideeffect = parser_mock("TryStatement")
DEBUGGER_STMT_REPLACEMENT, debugger_statement_sideeffect = parser_mock("DebuggerStatement")
SLI_REPLACEMENT, sli_sideeffect = parser_mock("StatementListItem", NotStringLiteral("StatementListItem"))
STATEMENT, statement_sideeffect = parser_mock("Statement")
DECL_REPLACEMENT, decl_sideeffect = parser_mock("Declaration")
SCRIPTBODY_REPLACEMENT, scriptbody_sideeffect = parser_mock("ScriptBody")
STATEMENTLIST, statementlist_sideeffect = parser_mock("StatementList")
MULT_REPLACEMENT, mult_sideeffect = parser_mock("MultiplicativeExpression")
ADDS_REPLACEMENT, adds_sideeffect = parser_mock("AdditiveExpression")
SHIFTEXPRESSION, shiftexpression_sideeffect = parser_mock("ShiftExpression")
RELEXPRESSION, relexpression_sideeffect = parser_mock("RelationalExpression")
EQUALEXPRESSION, equalexpression_sideeffect = parser_mock("EqualityExpression")
BAE_REPLACEMENT, bae_sideeffect = parser_mock("BitwiseANDExpression")
BXE_REPLACEMENT, bxe_sideeffect = parser_mock("BitwiseXORExpression")
BOE_REPLACEMENT, boe_sideeffect = parser_mock("BitwiseORExpression")
LAE_REPLACEMENT, lae_sideeffect = parser_mock("LogicalANDExpression")
LOE_REPLACEMENT, loe_sideeffect = parser_mock("LogicalORExpression")
COND_REPLACEMENT, cond_sideeffect = parser_mock("ConditionalExpression")
YE_REPLACEMENT, ye_sideeffect = parser_mock("YieldExpression")
ARRFCN_REPLACEMENT, arrfcn_sideeffect = parser_mock("ArrowFunction")
ASARRFCN_REPLACEMENT, asarrfcn_sideeffect = parser_mock("AsyncArrowFunction")
OAP_REPLACEMENT, oap_sideeffect = parser_mock("ObjectAssignmentPattern")
AAP_REPLACEMENT, aap_sideeffect = parser_mock("ArrayAssignmentPattern")
ARP_REPLACEMENT, arp_sideeffect = parser_mock("AssignmentRestProperty")
APL_REPLACEMENT, apl_sideeffect = parser_mock("AssignmentPropertyList")
ARE_REPLACEMENT, are_sideeffect = parser_mock("AssignmentRestElement")
AEL_REPLACEMENT, ael_sideeffect = parser_mock("AssignmentElementList")
DAT_REPLACEMENT, dat_sideeffect = parser_mock("DestructuringAssignmentTarget")
AP_REPLACEMENT, ap_sideeffect = parser_mock("AssignmentProperty")
AEE_REPLACEMENT, aee_sideeffect = parser_mock("AssignmentElisionElement")
ASSEL_REPLACEMENT, assel_sideeffect = parser_mock("AssignmentElement")
HD_REPLACEMENT, hd_sideeffect = parser_mock("HoistableDeclaration")
CD_REPLACEMENT, cd_sideeffect = parser_mock("ClassDeclaration")
LD_REPLACEMENT, ld_sideeffect = parser_mock("LexicalDeclaration")
FUNCTIONDECLARATION, functiondeclaration_sideeffect = parser_mock("FunctionDeclaration")
GD_REPLACEMENT, gd_sideeffect = parser_mock("GeneratorDeclaration")
AFD_REPLACEMENT, afd_sideeffect = parser_mock("AsyncFunctionDeclaration")
AGD_REPLACEMENT, agd_sideeffect = parser_mock("AsyncGeneratorDeclaration")
IS_REPLACEMENT, is_sideeffect = parser_mock("IterationStatement")
SS_REPLACEMENT, ss_sideeffect = parser_mock("SwitchStatement")
BLOCK, block_sideeffect = parser_mock("Block")
LETORCONST_REPLACEMENT, letorconst_sideeffect = parser_mock("LetOrConst")
BINDINGLIST_REPLACEMENT, bindinglist_sideeffect = parser_mock("BindingList")
LEXICALBINDING_REPLACEMENT, lexicalbinding_sideeffect = parser_mock("LexicalBinding")
VARIABLEDECLARATIONLIST, variabledeclarationlist_sideeffect = parser_mock("VariableDeclarationList")
VARIABLEDECLARATION, variabledeclaration_sideeffect = parser_mock("VariableDeclaration")
OBJECTBINDINGPATTERN, objectbindingpattern_sideeffect = parser_mock("ObjectBindingPattern")
ARRAYBINDINGPATTERN, arraybindingpattern_sideeffect = parser_mock("ArrayBindingPattern")
BINDINGPROPERTYLIST, bindingpropertylist_sideeffect = parser_mock("BindingPropertyList")
BINDINGRESTPROPERTY, bindingrestproperty_sideeffect = parser_mock("BindingRestProperty")
IDENTIFIER, identifier_sideeffect = parser_mock("Identifier")
BINDINGELEMENTLIST, bindingelementlist_sideeffect = parser_mock("BindingElementList")
BINDINGRESTELEMENT, bindingrestelement_sideeffect = parser_mock("BindingRestElement")
BINDINGPROPERTY, bindingproperty_sideeffect = parser_mock("BindingProperty")
BINDINGELISIONELEMENT, bindingelisionelement_sideeffect = parser_mock("BindingElisionElement")
BINDINGELEMENT, bindingelement_sideeffect = parser_mock("BindingElement")
SINGLENAMEBINDING, singlenamebinding_sideeffect = parser_mock("SingleNameBinding")
FORBINDING, forbinding_sideeffect = parser_mock("ForBinding")
FORDECLARATION, fordeclaration_sideeffect = parser_mock("ForDeclaration")
LABELIDENTIFIER, labelidentifier_sideeffect = parser_mock("LabelIdentifier")
LABELIDENTIFIER_NONL = Token("REPLACEMENT", "LabelIdentifier", newlines=[])
CASEBLOCK, caseblock_sideeffect = parser_mock("CaseBlock")
CASECLAUSES, caseclauses_sideeffect = parser_mock("CaseClauses")
DEFAULTCLAUSE, defaultclause_sideeffect = parser_mock("DefaultClause")
CASECLAUSE, caseclause_sideeffect = parser_mock("CaseClause")
LABELLEDITEM, labelleditem_sideeffect = parser_mock("LabelledItem")
CATCH_PRODUCTION, catch_sideeffect = parser_mock("Catch")
FINALLY_PRODUCTION, finally_sideeffect = parser_mock("Finally")
CATCHPARAMETER, catchparameter_sideeffect = parser_mock("CatchParameter")
FORMALPARAMETERS, formalparameters_sideeffect = parser_mock("FormalParameters")
FUNCTIONBODY, functionbody_sideeffect = parser_mock("FunctionBody", NotStringLiteral("FunctionBody"))
FORMALPARAMETERLIST, formalparameterlist_sideeffect = parser_mock("FormalParameterList")
FUNCTIONRESTPARAMETER, functionrestparameter_sideeffect = parser_mock("FunctionRestParameter")
FORMALPARAMETER, formalparameter_sideeffect = parser_mock("FormalParameter")
FUNCTIONSTATEMENTLIST, functionstatementlist_sideeffect = parser_mock(
    "FunctionStatementList", NotStringLiteral("FunctionStatementList")
)
ARROWPARAMETERS, arrowparameters_sideeffect = parser_mock("ArrowParameters")
CONCISEBODY, concisebody_sideeffect = parser_mock("ConciseBody")
(
    COVERPARENTHESIZEDEXPRESSIONANDARROWPARAMETERLIST,
    coverparenthesizedexpressionandarrowparameterlist_sideeffect,
) = parser_mock("CoverParenthesizedExpressionAndArrowParameterList")


def gen_mock_check(mocker, mocks, context, lexer):
    def mock_check(name, must_be_called, *args):
        def mc():
            calls = mocks[name].call_args_list
            expected = mocker.call(context, lexer, *args)
            for call in calls:
                assert call == expected, f"{name} called with wrong arguments"
            assert (not must_be_called) or len(calls) >= 1, f"{name} should be called"

        return mc

    return mock_check


def syntax_errify(production):
    collected = tuple()
    yield collected
    yield collected + (MATCHES_NONE,)
    for symbol in production[:-1]:
        collected += (symbol,)
        yield collected + (MATCHES_NONE,)


def errify_many(productions):
    for production in productions:
        yield from syntax_errify(production)


def synerror_streams(productions):
    def parametrize(seq):
        return pytest.param(seq, id=" ".join(x.value for x in seq))

    return [
        parametrize(item)
        for item in sorted(list(set(errify_many(productions))), key=lambda x: " ".join(term.value for term in x))
    ]


def lister(i, collected=None):
    try:
        n = next(i)
    except StopIteration:
        return collected
    return lambda: lister(i, list(filter(lambda x: x is not None, [collected, n])))


@pytest.mark.parametrize(
    "input, expected",
    [((int, "67"), 67), ((lister, iter(range(10))), [[[[[[[[[[0], 1], 2], 3], 4], 5], 6], 7], 8], 9])],
)
def test_trampoline(input, expected):
    result = e.trampoline(*input)
    assert result == expected


class Test_ParseNode2:
    def test_init(self):
        pn = e.ParseNode2("my context", "my_name", "strictitude", FakeTokens("child1", "child2"))

        assert pn.name == "my_name"
        assert pn.context == "my context"
        assert [tok.value for tok in pn.children] == ["child1", "child2"]
        assert pn.strict == "strictitude"

    def test_repr(self):
        token_higher = Token("GENERIC", "higher")
        token_lower = Token("GENERIC", "lower")
        mid_node = e.ParseNode2(None, "MidNode", False, [token_lower])
        top_node = e.ParseNode2(None, "TopNode", False, [mid_node, None, token_higher])

        r = repr(top_node)
        assert r == "ParseNode[TopNode : MidNode GENERIC] (lower higher)"

    def test_terminals(self):
        lower = e.ParseNode2(None, "lower", False, [IBOB, EQ, THREE, SEMICOLON])
        upper = e.ParseNode2(None, "upper", False, [ICHARLIE, None, LCURLY, lower, RCURLY])

        assert list(upper.terminals()) == [ICHARLIE, LCURLY, IBOB, EQ, THREE, SEMICOLON, RCURLY]

    def test_defer_target(self):
        child = e.ParseNode2(None, "child", False, [IBOB])
        parent = e.ParseNode2(None, "parent", False, [RETURN, child, SEMICOLON])

        assert parent.defer_target() == child

    @pytest.fixture
    def tree(self):
        lower = e.ParseNode2(None, "Lower", False, [BREAK, SEMICOLON])
        upper = e.ParseNode2(None, "Upper", False, [lower, FUNCTION])
        return upper

    @pytest.mark.parametrize(
        "symbol, expected",
        [("Lower", True), ("function", True), ("break", True), ("Upper", False), ("Nonsense", False)],
    )
    def test_Contains(self, tree, symbol, expected):
        assert tree.Contains(symbol) == expected

    @pytest.fixture
    def straight(self):
        bottom = e.ParseNode2(None, "Bottom", False, [RETURN, SEMICOLON])
        middle = e.ParseNode2(None, "Middle", False, [bottom])
        top = e.ParseNode2(None, "Top", False, [middle])
        return top

    @pytest.mark.parametrize(
        "symbol, expected", [("Top", True), ("Middle", True), ("Bottom", True), ("Nonsense", False)]
    )
    def test_Is_straight(self, straight, symbol, expected):
        assert straight.Is(symbol) == expected

    @pytest.mark.parametrize("symbol, expected", [("Upper", True), ("Lower", False)])
    def test_Is_tree(self, tree, symbol, expected):
        assert tree.Is(symbol) == expected

    @pytest.fixture
    def derived(self):
        class Test_Base(e.ParseNode2):
            def __init__(self, ctx, children):
                super().__init__(ctx, "Test", "StrictArg", children)

        class Test_Production(Test_Base):
            pass

        return (Test_Production(None, FakeTokens("child")), Test_Base)

    def test_Derived_self_string(self, derived):
        obj, _ = derived
        assert obj.Derived("Test") == obj

    def test_Derived_self_type(self, derived):
        obj, baseclass = derived
        assert obj.Derived(baseclass) == obj

    def test_Derived_onlychild(self, derived):
        obj, _ = derived
        parent = e.ParseNode2(None, "Parent", "StrictArg", [obj])
        assert parent.Derived("Test") == obj

    def test_Derived_false(self, derived):
        obj, _ = derived
        parent = e.ParseNode2(None, "Parent", "StrictArg", [obj])
        assert parent.Derived("Nonsense") is None

    def test_EarlyErrorsScan(self):
        class ChildClass(e.ParseNode2):
            def __init__(self, ctx, children):
                super().__init__(ctx, "ChildClass", "StrictArg", children)

            def EarlyErrors(self):
                return ["Child Syntax Error"]

        child = ChildClass(None, [SEMICOLON])
        parent = e.ParseNode2(None, "Parent", "StrictArg", [child])
        errs = parent.EarlyErrorsScan()
        assert errs == ["Child Syntax Error"]

    def test_direct_eval(self):
        p2c = e.Parse2Context(direct_eval="test string")
        pn = e.ParseNode2(p2c, "Top", "StrictArg", FakeTokens("child"))
        assert pn.direct_eval == "test string"

    def test_CreateSyntaxError(self):
        p2c = e.Parse2Context(syntax_error_ctor="syntax error constructor")
        pn = e.ParseNode2(p2c, "Top", "StrictArg", FakeTokens("child"))
        assert pn.CreateSyntaxError == "syntax error constructor"

    @pytest.mark.parametrize(
        "callname",
        [
            "IsFunctionDefinition",
            "IsValidSimpleAssignmentTarget",
            "LexicallyDeclaredNames",
            "TopLevelLexicallyDeclaredNames",
            "VarDeclaredNames",
            "TopLevelVarDeclaredNames",
            "VarScopedDeclarations",
            "TopLevelVarScopedDeclarations",
            "LexicallyScopedDeclarations",
            "TopLevelLexicallyScopedDeclarations",
            "ContainsDuplicateLabels",
            "ContainsUndefinedBreakTarget",
            "ContainsUndefinedContinueTarget",
            "BoundNames",
            "StringValue",
            "ArgumentListEvaluation",
            "IsConstantDeclaration",
            "PropertyDefinitionEvaluation",
            "IteratorDestructuringAssignmentEvaluation",
            "DestructuringAssignmentEvaluation",
            "PropertyBindingInitialization",
            "IsDestructuring",
            "IsSimpleParameterList",
            "ExpectedArgumentCount",
            "HasInitializer",
            "ContainsExpression",
            "IteratorBindingInitialization",
            "BindingInitialization",
            "TemplateStrings",
            "LeadingStrings",
            "IsStrict",
            "ContainsUseStrict",
            "AssignmentTargetType",
            "evaluate",
            "HasDirectSuper",
            "DefineMethod",
            "IsIdentifierRef",
            "PropertyDestructuringAssignmentEvaluation",
        ],
    )
    def test_deferred_calls(self, callname):
        class Child(e.ParseNode2):
            pass

        child = Child(None, "Child", "StrictArg", FakeTokens("child"))
        setattr(child, callname, types.MethodType(lambda self: callname, child))
        parent = e.ParseNode2(None, "Parent", "StrictArg", [child])
        assert getattr(parent, callname)() == callname


def test_parse2context_init():
    pc = e.Parse2Context("direct_eval", "syntax_error_ctor", "goal")

    assert pc.direct_eval == "direct_eval"
    assert pc.CreateSyntaxError == "syntax_error_ctor"
    assert pc.goal == "goal"


#### UpdateExpression #########################################################################################################################
#
# 888     888               888          888             8888888888                                                      d8b
# 888     888               888          888             888                                                             Y8P
# 888     888               888          888             888
# 888     888 88888b.   .d88888  8888b.  888888  .d88b.  8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888     888 888 "88b d88" 888     "88b 888    d8P  Y8b 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888     888 888  888 888  888 .d888888 888    88888888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# Y88b. .d88P 888 d88P Y88b 888 888  888 Y88b.  Y8b.     888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
#  "Y88888P"  88888P"   "Y88888 "Y888888  "Y888  "Y8888  8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#             888                                                            888
#             888                                                            888
#             888                                                            888
#
###############################################################################################################################################
def test_P2_UpdateExpression_init(context):
    upe = e.P2_UpdateExpression(context, "StrictArg", FakeTokens("child"))
    assert upe.name == "UpdateExpression"
    assert upe.context == context
    assert [tok.value for tok in upe.children] == ["child"]


def test_P2_UpdateExpression_LeftHandSideExpression_init(context):
    upe = e.P2_UpdateExpression_LeftHandSideExpression(context, "StrictArg", FakeTokens("lhs"))
    assert upe.name == "UpdateExpression"
    assert upe.LeftHandSideExpression.value == "lhs"


def test_P2_UpdateExpression_LeftHandSideExpression_PLUSPLUS_init(context):
    upe = e.P2_UpdateExpression_LeftHandSideExpression_PLUSPLUS(context, "StrictArg", FakeTokens("lhs", "++"))
    assert upe.name == "UpdateExpression"
    assert upe.LeftHandSideExpression.value == "lhs"


def test_P2_UpdateExpression_LeftHandSideExpression_MINUSMINUS_init(context):
    upe = e.P2_UpdateExpression_LeftHandSideExpression_MINUSMINUS(context, "StrictArg", FakeTokens("lhs", "--"))
    assert upe.name == "UpdateExpression"
    assert upe.LeftHandSideExpression.value == "lhs"


def test_P2_UpdateExpression_PLUSPLUS_UnaryExpression_init(context):
    upe = e.P2_UpdateExpression_PLUSPLUS_UnaryExpression(context, "StrictArg", FakeTokens("++", "unary"))
    assert upe.name == "UpdateExpression"
    assert upe.UnaryExpression.value == "unary"


def test_P2_UpdateExpression_MINUSMINUS_UnaryExpression_init(context):
    upe = e.P2_UpdateExpression_MINUSMINUS_UnaryExpression(context, "StrictArg", FakeTokens("--", "unary"))
    assert upe.name == "UpdateExpression"
    assert upe.UnaryExpression.value == "unary"


class Test_parse_UpdateExpression(parse_test):
    # Syntax
    #   UpdateExpression[Yield, Await]:
    #       LeftHandSideExpression[?Yield, ?Await]
    #       LeftHandSideExpression[?Yield, ?Await] [no LineTerminator here] ++
    #       LeftHandSideExpression[?Yield, ?Await] [no LineTerminator here] --
    #       ++ UnaryExpression[?Yield, ?Await]
    #       -- UnaryExpression[?Yield, ?Await]
    target = staticmethod(e.parse_UpdateExpression)
    target_argnames = ("Yield", "Await")
    productions = (
        (("LeftHandSideExpression",), e.P2_UpdateExpression_LeftHandSideExpression),
        (("LeftHandSideExpression", "NO_LT¡++"), e.P2_UpdateExpression_LeftHandSideExpression_PLUSPLUS),
        (("LeftHandSideExpression", "NO_LT¡--"), e.P2_UpdateExpression_LeftHandSideExpression_MINUSMINUS),
        (("++", "UnaryExpression"), e.P2_UpdateExpression_PLUSPLUS_UnaryExpression),
        (("--", "UnaryExpression"), e.P2_UpdateExpression_MINUSMINUS_UnaryExpression),
    )
    called_argnames = {"LeftHandSideExpression": ("?Yield", "?Await"), "UnaryExpression": ("?Yield", "?Await")}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### UnaryExpression ######################################################################################################################
#
#     888     888                                    8888888888                                                      d8b
#     888     888                                    888                                                             Y8P
#     888     888                                    888
#     888     888 88888b.   8888b.  888d888 888  888 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
#     888     888 888 "88b     "88b 888P"   888  888 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
#     888     888 888  888 .d888888 888     888  888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
#     Y88b. .d88P 888  888 888  888 888     Y88b 888 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
#      "Y88888P"  888  888 "Y888888 888      "Y88888 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                888                     888
#                                           Y8b d88P                     888
#                                            "Y88P"                      888
#
###########################################################################################################################################
def test_P2_UnaryExpression_init(context):
    une = e.P2_UnaryExpression(context, "StrictArg", FakeTokens("child"))
    assert une.name == "UnaryExpression"
    assert une.context == context
    assert [tok.value for tok in une.children] == ["child"]


def test_P2_UnaryExpression_UpdateExpression_init(context):
    une = e.P2_UnaryExpression_UpdateExpression(context, "StrictArg", FakeTokens("Update"))
    assert une.name == "UnaryExpression"
    assert une.UpdateExpression.value == "Update"


def test_P2_UnaryExpression_DELETE_UnaryExpression_init(context):
    une = e.P2_UnaryExpression_DELETE_UnaryExpression(context, "StrictArg", FakeTokens("delete", "Unary"))
    assert une.name == "UnaryExpression"
    assert une.UnaryExpression.value == "Unary"


def test_P2_UnaryExpression_VOID_UnaryExpression_init(context):
    une = e.P2_UnaryExpression_VOID_UnaryExpression(context, "StrictArg", FakeTokens("void", "Unary"))
    assert une.name == "UnaryExpression"
    assert une.UnaryExpression.value == "Unary"


def test_P2_UnaryExpression_TYPEOF_UnaryExpression_init(context):
    une = e.P2_UnaryExpression_TYPEOF_UnaryExpression(context, "StrictArg", FakeTokens("typeof", "Unary"))
    assert une.name == "UnaryExpression"
    assert une.UnaryExpression.value == "Unary"


def test_P2_UnaryExpression_PLUS_UnaryExpression_init(context):
    une = e.P2_UnaryExpression_PLUS_UnaryExpression(context, "StrictArg", FakeTokens("+", "Unary"))
    assert une.name == "UnaryExpression"
    assert une.UnaryExpression.value == "Unary"


def test_P2_UnaryExpression_MINUS_UnaryExpression_init(context):
    une = e.P2_UnaryExpression_MINUS_UnaryExpression(context, "StrictArg", FakeTokens("-", "Unary"))
    assert une.name == "UnaryExpression"
    assert une.UnaryExpression.value == "Unary"


def test_P2_UnaryExpression_TILDE_UnaryExpression_init(context):
    une = e.P2_UnaryExpression_TILDE_UnaryExpression(context, "StrictArg", FakeTokens("~", "Unary"))
    assert une.name == "UnaryExpression"
    assert une.UnaryExpression.value == "Unary"


def test_P2_UnaryExpression_BANG_UnaryExpression_init(context):
    une = e.P2_UnaryExpression_BANG_UnaryExpression(context, "StrictArg", FakeTokens("!", "Unary"))
    assert une.name == "UnaryExpression"
    assert une.UnaryExpression.value == "Unary"


def test_P2_UnaryExpression_AwaitExpression_init(context):
    une = e.P2_UnaryExpression_AwaitExpression(context, "StrictArg", FakeTokens("AwaitExpression"))
    assert une.name == "UnaryExpression"
    assert une.AwaitExpression.value == "AwaitExpression"


def UnaryExpression_mocks(mocker):
    return {
        "upe": mocker.patch("ecmascript.ecmascript.parse_UpdateExpression", side_effect=upe_sideeffect),
        "awe": mocker.patch("ecmascript.ecmascript.parse_AwaitExpression", side_effect=awe_sideeffect),
    }


class Test_parse_UnaryExpression(parse_test):
    # Syntax
    #   UnaryExpression[Yield, Await] :
    #       UpdateExpression[?Yield, ?Await]
    #       delete UnaryExpression[?Yield, ?Await]
    #       void UnaryExpression[?Yield, ?Await]
    #       typeof UnaryExpression[?Yield, ?Await]
    #       + UnaryExpression[?Yield, ?Await]
    #       - UnaryExpression[?Yield, ?Await]
    #       ~ UnaryExpression[?Yield, ?Await]
    #       ! UnaryExpression[?Yield, ?Await]
    #       [+Await]AwaitExpression[?Yield]
    target = staticmethod(e.parse_UnaryExpression)
    target_argnames = ("Yield", "Await")
    productions = (
        (("UpdateExpression",), e.P2_UnaryExpression_UpdateExpression),
        (("delete", "UpdateExpression"), e.P2_UnaryExpression_DELETE_UnaryExpression),
        (("void", "UpdateExpression"), e.P2_UnaryExpression_VOID_UnaryExpression),
        (("typeof", "UpdateExpression"), e.P2_UnaryExpression_TYPEOF_UnaryExpression),
        (("+", "UpdateExpression"), e.P2_UnaryExpression_PLUS_UnaryExpression),
        (("-", "UpdateExpression"), e.P2_UnaryExpression_MINUS_UnaryExpression),
        (("~", "UpdateExpression"), e.P2_UnaryExpression_TILDE_UnaryExpression),
        (("!", "UpdateExpression"), e.P2_UnaryExpression_BANG_UnaryExpression),
        (("[+Await]AwaitExpression",), (e.P2_UnaryExpression_AwaitExpression, type(None))),
    )
    called_argnames = {
        "UpdateExpression": ("?Yield", "?Await"),
        "AwaitExpression": ("?Yield",),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### MultiplicativeExpression ################################################################################################################################################################
#
# 888b     d888          888 888    d8b          888 d8b                   888    d8b                   8888888888                                                      d8b
# 8888b   d8888          888 888    Y8P          888 Y8P                   888    Y8P                   888                                                             Y8P
# 88888b.d88888          888 888                 888                       888                          888
# 888Y88888P888 888  888 888 888888 888 88888b.  888 888  .d8888b  8888b.  888888 888 888  888  .d88b.  8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888 Y888P 888 888  888 888 888    888 888 "88b 888 888 d88P"        "88b 888    888 888  888 d8P  Y8b 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888  Y8P  888 888  888 888 888    888 888  888 888 888 888      .d888888 888    888 Y88  88P 88888888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# 888   "   888 Y88b 888 888 Y88b.  888 888 d88P 888 888 Y88b.    888  888 Y88b.  888  Y8bd8P  Y8b.     888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# 888       888  "Y88888 888  "Y888 888 88888P"  888 888  "Y8888P "Y888888  "Y888 888   Y88P    "Y8888  8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                       888                                                                                 888
#                                       888                                                                                 888
#                                       888                                                                                 888
#
##############################################################################################################################################################################################
def test_P2_MultiplicativeExpression_init(context):
    me = e.P2_MultiplicativeExpression(context, "StrictArg", FakeTokens("child"))
    assert me.name == "MultiplicativeExpression"
    assert me.context == context
    assert [tok.value for tok in me.children] == ["child"]


def test_P2_MultiplicativeExpression_ExponentiationExpression_init(context):
    me = e.P2_MultiplicativeExpression_ExponentiationExpression(context, "StrictArg", FakeTokens("child"))
    assert me.name == "MultiplicativeExpression"
    assert me.ExponentiationExpression.value == "child"


def test_P2_MultiplicativeExpression_MultiplicativeExpression_MultiplicativeOperator_ExponentiationExpression_init(
    context,
):
    me = e.P2_MultiplicativeExpression_MultiplicativeExpression_MultiplicativeOperator_ExponentiationExpression(
        context, "StrictArg", FakeTokens("p1", "%", "p2")
    )
    assert me.name == "MultiplicativeExpression"
    assert me.MultiplicativeExpression.value == "p1"
    assert me.MultiplicativeOperator.value == "%"
    assert me.ExponentiationExpression.value == "p2"


class Test_parse_MultiplicativeExpression(parse_test):
    # Syntax
    #   MultiplicativeExpression[Yield, Await] :
    #       ExponentiationExpression[?Yield, ?Await]
    #       MultiplicativeExpression[?Yield, ?Await] MultiplicativeOperator ExponentiationExpression[?Yield, ?Await]
    target = staticmethod(e.parse_MultiplicativeExpression)
    target_argnames = ("Yield", "Await")
    productions = (
        (("ExponentiationExpression",), e.P2_MultiplicativeExpression_ExponentiationExpression),
        (
            ("ExponentiationExpression", "MultiplicativeOperator", "ExponentiationExpression"),
            e.P2_MultiplicativeExpression_MultiplicativeExpression_MultiplicativeOperator_ExponentiationExpression,
        ),
    )
    called_argnames = {
        "ExponentiationExpression": ("?Yield", "?Await"),
        "MultiplicativeOperator": (),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### MultiplicativeExpression #################################################################################################################################################
#
# 888b     d888          888 888    d8b          888 d8b                   888    d8b                    .d88888b.                                     888
# 8888b   d8888          888 888    Y8P          888 Y8P                   888    Y8P                   d88P" "Y88b                                    888
# 88888b.d88888          888 888                 888                       888                          888     888                                    888
# 888Y88888P888 888  888 888 888888 888 88888b.  888 888  .d8888b  8888b.  888888 888 888  888  .d88b.  888     888 88888b.   .d88b.  888d888  8888b.  888888  .d88b.  888d888
# 888 Y888P 888 888  888 888 888    888 888 "88b 888 888 d88P"        "88b 888    888 888  888 d8P  Y8b 888     888 888 "88b d8P  Y8b 888P"       "88b 888    d88""88b 888P"
# 888  Y8P  888 888  888 888 888    888 888  888 888 888 888      .d888888 888    888 Y88  88P 88888888 888     888 888  888 88888888 888     .d888888 888    888  888 888
# 888   "   888 Y88b 888 888 Y88b.  888 888 d88P 888 888 Y88b.    888  888 Y88b.  888  Y8bd8P  Y8b.     Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  Y88..88P 888
# 888       888  "Y88888 888  "Y888 888 88888P"  888 888  "Y8888P "Y888888  "Y888 888   Y88P    "Y8888   "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888  "Y88P"  888
#                                       888                                                                         888
#                                       888                                                                         888
#                                       888                                                                         888
#
###############################################################################################################################################################################
def test_P2_MultiplicativeOperator_init(context):
    mo = e.P2_MultiplicativeOperator(context, "StrictArg", FakeTokens("child"))
    assert mo.name == "MultiplicativeOperator"
    assert mo.context == context
    assert [tok.value for tok in mo.children] == ["child"]


def test_P2_MultiplicativeOperator_MULT_init(context):
    mo = e.P2_MultiplicativeOperator_MULT(context, "StrictArg", FakeTokens("child"))
    assert mo.name == "MultiplicativeOperator"


def test_P2_MultiplicativeOperator_DIV_init(context):
    mo = e.P2_MultiplicativeOperator_DIV(context, "StrictArg", FakeTokens("child"))
    assert mo.name == "MultiplicativeOperator"


def test_P2_MultiplicativeOperator_MOD_init(context):
    mo = e.P2_MultiplicativeOperator_MOD(context, "StrictArg", FakeTokens("child"))
    assert mo.name == "MultiplicativeOperator"


class Test_parse_MultiplicativeOperator(parse_test):
    # Syntax
    #   MultiplicativeOperator :
    #       *
    #       /
    #       %
    target = staticmethod(e.parse_MultiplicativeOperator)
    target_argnames = ()
    productions = (
        (("*",), e.P2_MultiplicativeOperator_MULT),
        (("/",), e.P2_MultiplicativeOperator_DIV),
        (("%",), e.P2_MultiplicativeOperator_MOD),
    )
    called_argnames = {}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### AdditiveExpression ################################################################################################################################
#
#        d8888      888      888 d8b 888    d8b                   8888888888                                                      d8b
#       d88888      888      888 Y8P 888    Y8P                   888                                                             Y8P
#      d88P888      888      888     888                          888
#     d88P 888  .d88888  .d88888 888 888888 888 888  888  .d88b.  8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
#    d88P  888 d88" 888 d88" 888 888 888    888 888  888 d8P  Y8b 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
#   d88P   888 888  888 888  888 888 888    888 Y88  88P 88888888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
#  d8888888888 Y88b 888 Y88b 888 888 Y88b.  888  Y8bd8P  Y8b.     888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# d88P     888  "Y88888  "Y88888 888  "Y888 888   Y88P    "Y8888  8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                                                     888
#                                                                                     888
#                                                                                     888
#
########################################################################################################################################################
def test_P2_AdditiveExpression_init(context):
    ae = e.P2_AdditiveExpression(context, "StrictArg", FakeTokens("child"))
    assert ae.name == "AdditiveExpression"
    assert ae.context == context
    assert [tok.value for tok in ae.children] == ["child"]
    assert ae.strict == "StrictArg"


def test_P2_AdditiveExpression_MultiplicativeExpression_init(context):
    ae = e.P2_AdditiveExpression_MultiplicativeExpression(context, "StrictArg", FakeTokens("child"))
    assert ae.name == "AdditiveExpression"
    assert ae.MultiplicativeExpression.value == "child"


def test_P2_AdditiveExpression_AdditiveExpression_PLUS_MultiplicativeExpression_init(context):
    ae = e.P2_AdditiveExpression_AdditiveExpression_PLUS_MultiplicativeExpression(
        context, "StrictArg", FakeTokens("child", "+", "alice")
    )
    assert ae.name == "AdditiveExpression"
    assert ae.AdditiveExpression.value == "child"
    assert ae.MultiplicativeExpression.value == "alice"


def test_P2_AdditiveExpression_AdditiveExpression_MINUS_MultiplicativeExpression_init(context):
    ae = e.P2_AdditiveExpression_AdditiveExpression_MINUS_MultiplicativeExpression(
        context, "StrictArg", FakeTokens("child", "-", "alice")
    )
    assert ae.name == "AdditiveExpression"
    assert ae.AdditiveExpression.value == "child"
    assert ae.MultiplicativeExpression.value == "alice"


class Test_parse_AdditiveExpression(parse_test):
    # Syntax
    #   AdditiveExpression[Yield, Await] :
    #       MultiplicativeExpression[?Yield, ?Await]
    #       AdditiveExpression[?Yield, ?Await] + MultiplicativeExpression[?Yield, ?Await]
    #       AdditiveExpression[?Yield, ?Await] - MultiplicativeExpression[?Yield, ?Await]
    target = staticmethod(e.parse_AdditiveExpression)
    target_argnames = ("Yield", "Await")
    productions = (
        (("MultiplicativeExpression",), e.P2_AdditiveExpression_MultiplicativeExpression),
        (
            ("MultiplicativeExpression", "+", "MultiplicativeExpression"),
            e.P2_AdditiveExpression_AdditiveExpression_PLUS_MultiplicativeExpression,
        ),
        (
            ("MultiplicativeExpression", "-", "MultiplicativeExpression"),
            e.P2_AdditiveExpression_AdditiveExpression_MINUS_MultiplicativeExpression,
        ),
    )
    called_argnames = {"MultiplicativeExpression": ("?Yield", "?Await")}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### ShiftExpression #########################################################################################################
#
#  .d8888b.  888      d8b  .d888 888    8888888888                                                      d8b
# d88P  Y88b 888      Y8P d88P"  888    888                                                             Y8P
# Y88b.      888          888    888    888
#  "Y888b.   88888b.  888 888888 888888 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
#     "Y88b. 888 "88b 888 888    888    888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
#       "888 888  888 888 888    888    888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# Y88b  d88P 888  888 888 888    Y88b.  888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
#  "Y8888P"  888  888 888 888     "Y888 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                           888
#                                                           888
#                                                           888
#
##############################################################################################################################
def test_P2_ShiftExpression_init(context):
    se = e.P2_ShiftExpression(context, "StrictArg", FakeTokens("child"))
    assert se.name == "ShiftExpression"
    assert se.context == context
    assert [tok.value for tok in se.children] == ["child"]


def test_P2_ShiftExpression_AdditiveExpression_init(context):
    se = e.P2_ShiftExpression_AdditiveExpression(context, "StrictArg", FakeTokens("child"))
    assert se.name == "ShiftExpression"
    assert se.AdditiveExpression.value == "child"


def test_P2_ShiftExpression_ShiftExpression_LTLT_AdditiveExpression_init(context):
    se = e.P2_ShiftExpression_ShiftExpression_LTLT_AdditiveExpression(
        context, "StrictArg", FakeTokens("ShiExp", "<<", "AddExp")
    )
    assert se.name == "ShiftExpression"
    assert se.ShiftExpression.value == "ShiExp"
    assert se.AdditiveExpression.value == "AddExp"


def test_P2_ShiftExpression_ShiftExpression_GTGT_AdditiveExpression_init(context):
    se = e.P2_ShiftExpression_ShiftExpression_GTGT_AdditiveExpression(
        context, "StrictArg", FakeTokens("ShiExp", ">>", "AddExp")
    )
    assert se.name == "ShiftExpression"
    assert se.ShiftExpression.value == "ShiExp"
    assert se.AdditiveExpression.value == "AddExp"


def test_P2_ShiftExpression_ShiftExpression_GTGTGT_AdditiveExpression_init(context):
    se = e.P2_ShiftExpression_ShiftExpression_GTGTGT_AdditiveExpression(
        context, "StrictArg", FakeTokens("ShiExp", ">>>", "AddExp")
    )
    assert se.name == "ShiftExpression"
    assert se.ShiftExpression.value == "ShiExp"
    assert se.AdditiveExpression.value == "AddExp"


def ShiftExpression_mocks(mocker):
    return {
        "AdditiveExpression": mocker.patch(
            "ecmascript.ecmascript.parse_AdditiveExpression", side_effect=adds_sideeffect
        )
    }


class Test_parse_ShiftExpression(parse_test):
    # Syntax
    #   ShiftExpression[Yield, Await] :
    #       AdditiveExpression[?Yield, ?Await]
    #       ShiftExpression[?Yield, ?Await] << AdditiveExpression[?Yield, ?Await]
    #       ShiftExpression[?Yield, ?Await] >> AdditiveExpression[?Yield, ?Await]
    #       ShiftExpression[?Yield, ?Await] >>> AdditiveExpression[?Yield, ?Await]
    target = staticmethod(e.parse_ShiftExpression)
    target_argnames = ("Yield", "Await")
    productions = (
        (("AdditiveExpression",), e.P2_ShiftExpression_AdditiveExpression),
        (
            ("AdditiveExpression", "<<", "AdditiveExpression"),
            e.P2_ShiftExpression_ShiftExpression_LTLT_AdditiveExpression,
        ),
        (
            ("AdditiveExpression", ">>", "AdditiveExpression"),
            e.P2_ShiftExpression_ShiftExpression_GTGT_AdditiveExpression,
        ),
        (
            ("AdditiveExpression", ">>>", "AdditiveExpression"),
            e.P2_ShiftExpression_ShiftExpression_GTGTGT_AdditiveExpression,
        ),
    )
    called_argnames = {
        "AdditiveExpression": ("?Yield", "?Await"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### RelationalExpression #########################################################################################################################################
#
# 8888888b.           888          888    d8b                            888 8888888888                                                      d8b
# 888   Y88b          888          888    Y8P                            888 888                                                             Y8P
# 888    888          888          888                                   888 888
# 888   d88P  .d88b.  888  8888b.  888888 888  .d88b.  88888b.   8888b.  888 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 8888888P"  d8P  Y8b 888     "88b 888    888 d88""88b 888 "88b     "88b 888 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888 T88b   88888888 888 .d888888 888    888 888  888 888  888 .d888888 888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# 888  T88b  Y8b.     888 888  888 Y88b.  888 Y88..88P 888  888 888  888 888 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# 888   T88b  "Y8888  888 "Y888888  "Y888 888  "Y88P"  888  888 "Y888888 888 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                                                                888
#                                                                                                888
#                                                                                                888
#
###################################################################################################################################################################
def test_P2_RelationalExpression(context):
    re = e.P2_RelationalExpression(context, "StrictArg", FakeTokens("child"))
    assert re.name == "RelationalExpression"
    assert re.context == context
    assert [tok.value for tok in re.children] == ["child"]


def test_P2_RelationalExpression_ShiftExpression(context):
    re = e.P2_RelationalExpression_ShiftExpression(context, "StrictArg", FakeTokens("child"))
    assert re.name == "RelationalExpression"
    assert re.ShiftExpression.value == "child"


def test_P2_RelationalExpression_RelationalExpression_OP_ShiftExpression(context):
    re = e.P2_RelationalExpression_RelationalExpression_OP_ShiftExpression(
        context, "StrictArg", FakeTokens("left", "<", "right")
    )
    assert re.name == "RelationalExpression"
    assert re.RelationalExpression.value == "left"
    assert re.ShiftExpression.value == "right"


def test_P2_RelationalExpression_RelationalExpression_LT_ShiftExpression(context):
    re = e.P2_RelationalExpression_RelationalExpression_LT_ShiftExpression(context, "StrictArg", FakeTokens("child"))
    assert re.name == "RelationalExpression"


def test_P2_RelationalExpression_RelationalExpression_GT_ShiftExpression(context):
    re = e.P2_RelationalExpression_RelationalExpression_GT_ShiftExpression(context, "StrictArg", FakeTokens("child"))
    assert re.name == "RelationalExpression"


def test_P2_RelationalExpression_RelationalExpression_LE_ShiftExpression(context):
    re = e.P2_RelationalExpression_RelationalExpression_LE_ShiftExpression(context, "StrictArg", FakeTokens("child"))
    assert re.name == "RelationalExpression"


def test_P2_RelationalExpression_RelationalExpression_GE_ShiftExpression(context):
    re = e.P2_RelationalExpression_RelationalExpression_GE_ShiftExpression(context, "StrictArg", FakeTokens("child"))
    assert re.name == "RelationalExpression"


def test_P2_RelationalExpression_RelationalExpression_INSTANCEOF_ShiftExpression(context):
    re = e.P2_RelationalExpression_RelationalExpression_INSTANCEOF_ShiftExpression(
        context, "StrictArg", FakeTokens("child")
    )
    assert re.name == "RelationalExpression"


def test_P2_RelationalExpression_RelationalExpression_IN_ShiftExpression(context):
    re = e.P2_RelationalExpression_RelationalExpression_IN_ShiftExpression(context, "StrictArg", FakeTokens("child"))
    assert re.name == "RelationalExpression"


class Test_parse_RelationalExpression(parse_test):
    # Syntax
    #   RelationalExpression[In, Yield, Await]:
    #       ShiftExpression[?Yield, ?Await]
    #       RelationalExpression[?In, ?Yield, ?Await] < ShiftExpression[?Yield, ?Await]
    #       RelationalExpression[?In, ?Yield, ?Await] > ShiftExpression[?Yield, ?Await]
    #       RelationalExpression[?In, ?Yield, ?Await] <= ShiftExpression[?Yield, ?Await]
    #       RelationalExpression[?In, ?Yield, ?Await] >= ShiftExpression[?Yield, ?Await]
    #       RelationalExpression[?In, ?Yield, ?Await] instanceof ShiftExpression[?Yield, ?Await]
    #       [+In]RelationalExpression[+In, ?Yield, ?Await] in ShiftExpression[?Yield, ?Await]
    target = staticmethod(e.parse_RelationalExpression)
    target_argnames = ("In", "Yield", "Await")
    productions = (
        (("ShiftExpression",), e.P2_RelationalExpression_ShiftExpression),
        (
            ("ShiftExpression", "<", "ShiftExpression"),
            e.P2_RelationalExpression_RelationalExpression_LT_ShiftExpression,
        ),
        (
            ("ShiftExpression", ">", "ShiftExpression"),
            e.P2_RelationalExpression_RelationalExpression_GT_ShiftExpression,
        ),
        (
            ("ShiftExpression", "<=", "ShiftExpression"),
            e.P2_RelationalExpression_RelationalExpression_LE_ShiftExpression,
        ),
        (
            ("ShiftExpression", ">=", "ShiftExpression"),
            e.P2_RelationalExpression_RelationalExpression_GE_ShiftExpression,
        ),
        (
            ("ShiftExpression", "instanceof", "ShiftExpression"),
            e.P2_RelationalExpression_RelationalExpression_INSTANCEOF_ShiftExpression,
        ),
        (
            ("[+In]ShiftExpression", "in", "ShiftExpression"),
            (
                e.P2_RelationalExpression_RelationalExpression_IN_ShiftExpression,
                e.P2_RelationalExpression_ShiftExpression,
            ),
        ),
    )
    called_argnames = {
        "ShiftExpression": ("?Yield", "?Await"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### EqualityExpression ##############################################################################################################################
#
# 8888888888                            888 d8b 888             8888888888                                                      d8b
# 888                                   888 Y8P 888             888                                                             Y8P
# 888                                   888     888             888
# 8888888     .d88888 888  888  8888b.  888 888 888888 888  888 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888        d88" 888 888  888     "88b 888 888 888    888  888 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888        888  888 888  888 .d888888 888 888 888    888  888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# 888        Y88b 888 Y88b 888 888  888 888 888 Y88b.  Y88b 888 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# 8888888888  "Y88888  "Y88888 "Y888888 888 888  "Y888  "Y88888 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                 888                                       888                     888
#                 888                                  Y8b d88P                     888
#                 888                                   "Y88P"                      888
#
######################################################################################################################################################
def test_P2_EqualityExpression_init(context):
    ee = e.P2_EqualityExpression(context, "StrictArg", FakeTokens("child"))
    assert ee.name == "EqualityExpression"
    assert ee.context == context
    assert [tok.value for tok in ee.children] == ["child"]


def test_P2_EqualityExpression_RelationalExpression_init(context):
    ee = e.P2_EqualityExpression_RelationalExpression(context, "StrictArg", FakeTokens("child"))
    assert ee.name == "EqualityExpression"
    assert ee.RelationalExpression.value == "child"


def test_P2_EqualityExpression_EqualityExpression_OP_RelationalExpression_init(context):
    ee = e.P2_EqualityExpression_EqualityExpression_OP_RelationalExpression(
        context, "StrictArg", FakeTokens("child", "==", "vampire")
    )
    assert ee.name == "EqualityExpression"
    assert ee.EqualityExpression.value == "child"
    assert ee.RelationalExpression.value == "vampire"


def test_P2_EqualityExpression_EqualityExpression_EQEQ_RelationalExpression_init(context):
    ee = e.P2_EqualityExpression_EqualityExpression_EQEQ_RelationalExpression(
        context, "StrictArg", FakeTokens("child")
    )
    assert ee.name == "EqualityExpression"


def test_P2_EqualityExpression_EqualityExpression_BANGEQ_RelationalExpression_init(context):
    ee = e.P2_EqualityExpression_EqualityExpression_BANGEQ_RelationalExpression(
        context, "StrictArg", FakeTokens("child")
    )
    assert ee.name == "EqualityExpression"


def test_P2_EqualityExpression_EqualityExpression_EQEQEQ_RelationalExpression_init(context):
    ee = e.P2_EqualityExpression_EqualityExpression_EQEQEQ_RelationalExpression(
        context, "StrictArg", FakeTokens("child")
    )
    assert ee.name == "EqualityExpression"


def test_P2_EqualityExpression_EqualityExpression_BANGEQEQ_RelationalExpression_init(context):
    ee = e.P2_EqualityExpression_EqualityExpression_BANGEQEQ_RelationalExpression(
        context, "StrictArg", FakeTokens("child")
    )
    assert ee.name == "EqualityExpression"


class Test_parse_EqualityExpression(parse_test):
    # Syntax
    #   EqualityExpression[In, Yield, Await] :
    #       RelationalExpression[?In, ?Yield, ?Await]
    #       EqualityExpression[?In, ?Yield, ?Await] == RelationalExpression[?In, ?Yield, ?Await]
    #       EqualityExpression[?In, ?Yield, ?Await] != RelationalExpression[?In, ?Yield, ?Await]
    #       EqualityExpression[?In, ?Yield, ?Await] === RelationalExpression[?In, ?Yield, ?Await]
    #       EqualityExpression[?In, ?Yield, ?Await] !== RelationalExpression[?In, ?Yield, ?Await]
    target = staticmethod(e.parse_EqualityExpression)
    target_argnames = ("In", "Yield", "Await")
    productions = (
        (("RelationalExpression",), e.P2_EqualityExpression_RelationalExpression),
        (
            ("RelationalExpression", "==", "RelationalExpression"),
            e.P2_EqualityExpression_EqualityExpression_EQEQ_RelationalExpression,
        ),
        (
            ("RelationalExpression", "!=", "RelationalExpression"),
            e.P2_EqualityExpression_EqualityExpression_BANGEQ_RelationalExpression,
        ),
        (
            ("RelationalExpression", "===", "RelationalExpression"),
            e.P2_EqualityExpression_EqualityExpression_EQEQEQ_RelationalExpression,
        ),
        (
            ("RelationalExpression", "!==", "RelationalExpression"),
            e.P2_EqualityExpression_EqualityExpression_BANGEQEQ_RelationalExpression,
        ),
    )
    called_argnames = {
        "RelationalExpression": ("?In", "?Yield", "?Await"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


#### BitwiseANDExpression ############################################################################################################################################################
#
# 888888b.   d8b 888                  d8b                          d8888 888b    888 8888888b.  8888888888                                                      d8b
# 888  "88b  Y8P 888                  Y8P                         d88888 8888b   888 888  "Y88b 888                                                             Y8P
# 888  .88P      888                                             d88P888 88888b  888 888    888 888
# 8888888K.  888 888888 888  888  888 888 .d8888b   .d88b.      d88P 888 888Y88b 888 888    888 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888  "Y88b 888 888    888  888  888 888 88K      d8P  Y8b    d88P  888 888 Y88b888 888    888 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888    888 888 888    888  888  888 888 "Y8888b. 88888888   d88P   888 888  Y88888 888    888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# 888   d88P 888 Y88b.  Y88b 888 d88P 888      X88 Y8b.      d8888888888 888   Y8888 888  .d88P 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# 8888888P"  888  "Y888  "Y8888888P"  888  88888P'  "Y8888  d88P     888 888    Y888 8888888P"  8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                                                                                   888
#                                                                                                                   888
#                                                                                                                   888
#
######################################################################################################################################################################################
def test_P2_BitwiseANDExpression_init(context):
    bae = e.P2_BitwiseANDExpression(context, "StrictArg", FakeTokens("child"))
    assert bae.name == "BitwiseANDExpression"
    assert bae.context == context
    assert [tok.value for tok in bae.children] == ["child"]


def test_P2_BitwiseANDExpression_EqualityExpression_init(context):
    bae = e.P2_BitwiseANDExpression_EqualityExpression(context, "StrictArg", FakeTokens("child"))
    assert bae.name == "BitwiseANDExpression"
    assert bae.EqualityExpression.value == "child"


def test_P2_BitwiseANDExpression_BitwiseANDExpression_AMP_EqualityExpression_init(context):
    bae = e.P2_BitwiseANDExpression_BitwiseANDExpression_AMP_EqualityExpression(
        context, "StrictArg", FakeTokens("child", "&", "right")
    )
    assert bae.name == "BitwiseANDExpression"
    assert bae.BitwiseANDExpression.value == "child"
    assert bae.EqualityExpression.value == "right"


#### BitwiseXORExpression ###########################################################################################################################################################
#
# 888888b.   d8b 888                  d8b                   Y88b   d88P  .d88888b.  8888888b.  8888888888                                                      d8b
# 888  "88b  Y8P 888                  Y8P                    Y88b d88P  d88P" "Y88b 888   Y88b 888                                                             Y8P
# 888  .88P      888                                          Y88o88P   888     888 888    888 888
# 8888888K.  888 888888 888  888  888 888 .d8888b   .d88b.     Y888P    888     888 888   d88P 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888  "Y88b 888 888    888  888  888 888 88K      d8P  Y8b    d888b    888     888 8888888P"  888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888    888 888 888    888  888  888 888 "Y8888b. 88888888   d88888b   888     888 888 T88b   888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# 888   d88P 888 Y88b.  Y88b 888 d88P 888      X88 Y8b.      d88P Y88b  Y88b. .d88P 888  T88b  888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# 8888888P"  888  "Y888  "Y8888888P"  888  88888P'  "Y8888  d88P   Y88b  "Y88888P"  888   T88b 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                                                                                  888
#                                                                                                                  888
#                                                                                                                  888
#
#####################################################################################################################################################################################
def test_P2_BitwiseXORExpression_init(context):
    bxe = e.P2_BitwiseXORExpression(context, "StrictArg", FakeTokens("child"))
    assert bxe.name == "BitwiseXORExpression"
    assert bxe.context == context
    assert [tok.value for tok in bxe.children] == ["child"]


def test_P2_BitwiseXORExpression_BitwiseANDExpression_init(context):
    bxe = e.P2_BitwiseXORExpression_BitwiseANDExpression(context, "StrictArg", FakeTokens("child"))
    assert bxe.name == "BitwiseXORExpression"
    assert bxe.BitwiseANDExpression.value == "child"


def test_P2_BitwiseXORExpression_BitwiseXORExpression_CARET_BitwiseANDExpression_init(context):
    bxe = e.P2_BitwiseXORExpression_BitwiseXORExpression_CARET_BitwiseANDExpression(
        context, "StrictArg", FakeTokens("child", "^", "0x10")
    )
    assert bxe.name == "BitwiseXORExpression"
    assert bxe.BitwiseXORExpression.value == "child"
    assert bxe.BitwiseANDExpression.value == "0x10"


#### BitwiseORExpression ################################################################################################################################################
#
# 888888b.   d8b 888                  d8b                    .d88888b.  8888888b.  8888888888                                                      d8b
# 888  "88b  Y8P 888                  Y8P                   d88P" "Y88b 888   Y88b 888                                                             Y8P
# 888  .88P      888                                        888     888 888    888 888
# 8888888K.  888 888888 888  888  888 888 .d8888b   .d88b.  888     888 888   d88P 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888  "Y88b 888 888    888  888  888 888 88K      d8P  Y8b 888     888 8888888P"  888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888    888 888 888    888  888  888 888 "Y8888b. 88888888 888     888 888 T88b   888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# 888   d88P 888 Y88b.  Y88b 888 d88P 888      X88 Y8b.     Y88b. .d88P 888  T88b  888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# 8888888P"  888  "Y888  "Y8888888P"  888  88888P'  "Y8888   "Y88888P"  888   T88b 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                                                                      888
#                                                                                                      888
#                                                                                                      888
#
#########################################################################################################################################################################
def test_P2_BitwiseORExpression_init(context):
    boe = e.P2_BitwiseORExpression(context, "StrictArg", FakeTokens("child"))
    assert boe.name == "BitwiseORExpression"
    assert boe.context == context
    assert [tok.value for tok in boe.children] == ["child"]


def test_P2_BitwiseORExpression_BitwiseXORExpression_init(context):
    boe = e.P2_BitwiseORExpression_BitwiseXORExpression(context, "StrictArg", FakeTokens("child"))
    assert boe.name == "BitwiseORExpression"
    assert boe.BitwiseXORExpression.value == "child"


def test_P2_BitwiseORExpression_BitwiseORExpression_PIPE_BitwiseXORExpression_init(context):
    boe = e.P2_BitwiseORExpression_BitwiseORExpression_PIPE_BitwiseXORExpression(
        context, "StrictArg", FakeTokens("child", "|", "bxe")
    )
    assert boe.name == "BitwiseORExpression"
    assert boe.BitwiseORExpression.value == "child"
    assert boe.BitwiseXORExpression.value == "bxe"


#### LogicalANDExpression #######################################################################################################################################################
#
# 888                        d8b                   888        d8888 888b    888 8888888b.  8888888888                                                      d8b
# 888                        Y8P                   888       d88888 8888b   888 888  "Y88b 888                                                             Y8P
# 888                                              888      d88P888 88888b  888 888    888 888
# 888       .d88b.   .d88b.  888  .d8888b  8888b.  888     d88P 888 888Y88b 888 888    888 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888      d88""88b d88P"88b 888 d88P"        "88b 888    d88P  888 888 Y88b888 888    888 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888      888  888 888  888 888 888      .d888888 888   d88P   888 888  Y88888 888    888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# 888      Y88..88P Y88b 888 888 Y88b.    888  888 888  d8888888888 888   Y8888 888  .d88P 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# 88888888  "Y88P"   "Y88888 888  "Y8888P "Y888888 888 d88P     888 888    Y888 8888888P"  8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                        888                                                                                   888
#                   Y8b d88P                                                                                   888
#                    "Y88P"                                                                                    888
#
#################################################################################################################################################################################
def test_P2_LogicalANDExpression_init(context):
    lae = e.P2_LogicalANDExpression(context, "StrictArg", FakeTokens("child"))
    assert lae.name == "LogicalANDExpression"
    assert lae.context == context
    assert [tok.value for tok in lae.children] == ["child"]


def test_P2_LogicalANDExpression_BitwiseORExpression_init(context):
    lae = e.P2_LogicalANDExpression_BitwiseORExpression(context, "StrictArg", FakeTokens("child"))
    assert lae.name == "LogicalANDExpression"
    assert lae.BitwiseORExpression.value == "child"


def test_P2_LogicalANDExpression_LogicalANDExpression_AMPAMP_BitwiseORExpression_init(context):
    lae = e.P2_LogicalANDExpression_LogicalANDExpression_AMPAMP_BitwiseORExpression(
        context, "StrictArg", FakeTokens("child", "&&", "lemon")
    )
    assert lae.name == "LogicalANDExpression"
    assert lae.LogicalANDExpression.value == "child"
    assert lae.BitwiseORExpression.value == "lemon"


#### LogicalORExpression ###########################################################################################################################################
#
# 888                        d8b                   888  .d88888b.  8888888b.  8888888888                                                      d8b
# 888                        Y8P                   888 d88P" "Y88b 888   Y88b 888                                                             Y8P
# 888                                              888 888     888 888    888 888
# 888       .d88b.   .d88b.  888  .d8888b  8888b.  888 888     888 888   d88P 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888      d88""88b d88P"88b 888 d88P"        "88b 888 888     888 8888888P"  888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888      888  888 888  888 888 888      .d888888 888 888     888 888 T88b   888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# 888      Y88..88P Y88b 888 888 Y88b.    888  888 888 Y88b. .d88P 888  T88b  888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# 88888888  "Y88P"   "Y88888 888  "Y8888P "Y888888 888  "Y88888P"  888   T88b 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                        888                                                                      888
#                   Y8b d88P                                                                      888
#                    "Y88P"                                                                       888
#
####################################################################################################################################################################
def test_P2_LogicalORExpression_init(context):
    loe = e.P2_LogicalORExpression(context, "StrictArg", FakeTokens("child"))
    assert loe.name == "LogicalORExpression"
    assert loe.context == context
    assert [tok.value for tok in loe.children] == ["child"]


def test_P2_LogicalORExpression_LogicalANDExpression_init(context):
    loe = e.P2_LogicalORExpression_LogicalANDExpression(context, "StrictArg", FakeTokens("child"))
    assert loe.name == "LogicalORExpression"
    assert loe.LogicalANDExpression.value == "child"


def test_P2_LogicalORExpression_LogicalORExpression_PIPEPIPE_LogicalANDExpression_init(context):
    loe = e.P2_LogicalORExpression_LogicalORExpression_PIPEPIPE_LogicalANDExpression(
        context, "StrictArg", FakeTokens("child", "||", "right")
    )
    assert loe.name == "LogicalORExpression"
    assert loe.LogicalORExpression.value == "child"
    assert loe.LogicalANDExpression.value == "right"


#### ConditionalExpression #################################################################################################################################################
#
#  .d8888b.                         888 d8b 888    d8b                            888 8888888888                                                      d8b
# d88P  Y88b                        888 Y8P 888    Y8P                            888 888                                                             Y8P
# 888    888                        888     888                                   888 888
# 888         .d88b.  88888b.   .d88888 888 888888 888  .d88b.  88888b.   8888b.  888 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888        d88""88b 888 "88b d88" 888 888 888    888 d88""88b 888 "88b     "88b 888 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888    888 888  888 888  888 888  888 888 888    888 888  888 888  888 .d888888 888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# Y88b  d88P Y88..88P 888  888 Y88b 888 888 Y88b.  888 Y88..88P 888  888 888  888 888 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
#  "Y8888P"   "Y88P"  888  888  "Y88888 888  "Y888 888  "Y88P"  888  888 "Y888888 888 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                                                                         888
#                                                                                                         888
#                                                                                                         888
#
############################################################################################################################################################################
def test_P2_ConditionalExpression_init(context):
    ce = e.P2_ConditionalExpression(context, "StrictArg", FakeTokens("child"))
    assert ce.name == "ConditionalExpression"
    assert ce.context == context
    assert [tok.value for tok in ce.children] == ["child"]


def test_P2_ConditionalExpression_LogicalORExpression_init(context):
    ce = e.P2_ConditionalExpression_LogicalORExpression(context, "StrictArg", FakeTokens("child"))
    assert ce.name == "ConditionalExpression"
    assert ce.LogicalORExpression.value == "child"


def test_P2_ConditionalExpression_LogicalORExpression_QUESTION_AssignmentExpression_COLON_AssignmentExpression_init(
    context,
):
    ce = e.P2_ConditionalExpression_LogicalORExpression_QUESTION_AssignmentExpression_COLON_AssignmentExpression(
        context, "StrictArg", FakeTokens("child", "?", "true_result", ":", "false_result")
    )
    assert ce.name == "ConditionalExpression"
    assert ce.LogicalORExpression.value == "child"
    assert ce.AssignmentExpression1.value == "true_result"
    assert ce.AssignmentExpression2.value == "false_result"
    assert ce.strict == "StrictArg"


#### AssignmentExpression ##########################################################################################################################################################
#
#        d8888                   d8b                                                   888    8888888888                                                      d8b
#       d88888                   Y8P                                                   888    888                                                             Y8P
#      d88P888                                                                         888    888
#     d88P 888 .d8888b  .d8888b  888  .d88b.  88888b.  88888b.d88b.   .d88b.  88888b.  888888 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
#    d88P  888 88K      88K      888 d88P"88b 888 "88b 888 "888 "88b d8P  Y8b 888 "88b 888    888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
#   d88P   888 "Y8888b. "Y8888b. 888 888  888 888  888 888  888  888 88888888 888  888 888    888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
#  d8888888888      X88      X88 888 Y88b 888 888  888 888  888  888 Y8b.     888  888 Y88b.  888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# d88P     888  88888P'  88888P' 888  "Y88888 888  888 888  888  888  "Y8888  888  888  "Y888 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                         888                                                                     888
#                                    Y8b d88P                                                                     888
#                                     "Y88P"                                                                      888
#
####################################################################################################################################################################################
def test_P2_AssignmentExpression_init(context):
    ae = e.P2_AssignmentExpression(context, "StrictArg", FakeTokens("child"))
    assert ae.name == "AssignmentExpression"
    assert ae.context == context
    assert [tok.value for tok in ae.children] == ["child"]


def test_P2_AssignmentExpression_ConditionalExpression_init(context):
    ae = e.P2_AssignmentExpression_ConditionalExpression(context, "StrictArg", FakeTokens("child"))
    assert ae.name == "AssignmentExpression"
    assert ae.ConditionalExpression.value == "child"


def test_P2_AssignmentExpression_YieldExpression_init(context):
    ae = e.P2_AssignmentExpression_YieldExpression(context, "StrictArg", FakeTokens("child"))
    assert ae.name == "AssignmentExpression"
    assert ae.YieldExpression.value == "child"


def test_P2_AssignmentExpression_ArrowFunction_init(context):
    ae = e.P2_AssignmentExpression_ArrowFunction(context, "StrictArg", FakeTokens("child"))
    assert ae.name == "AssignmentExpression"
    assert ae.ArrowFunction.value == "child"


def test_P2_AssignmentExpression_AsyncArrowFunction_init(context):
    ae = e.P2_AssignmentExpression_AsyncArrowFunction(context, "StrictArg", FakeTokens("child"))
    assert ae.name == "AssignmentExpression"
    assert ae.AsyncArrowFunction.value == "child"


def test_P2_AssignmentExpression_LeftHandSideExpression_EQ_AssignmentExpression_init(context):
    ae = e.P2_AssignmentExpression_LeftHandSideExpression_EQ_AssignmentExpression(
        context, "StrictArg", FakeTokens("child", "=", "right")
    )
    assert ae.name == "AssignmentExpression"
    assert ae.LeftHandSideExpression.value == "child"
    assert ae.AssignmentExpression.value == "right"


def test_P2_AssignmentExpression_LeftHandSideExpression_AssignmentOperator_AssignmentExpression_init(context):
    ae = e.P2_AssignmentExpression_LeftHandSideExpression_AssignmentOperator_AssignmentExpression(
        context, "StrictArg", FakeTokens("child", "+=", "right")
    )
    assert ae.name == "AssignmentExpression"
    assert ae.LeftHandSideExpression.value == "child"
    assert ae.AssignmentOperator.value == "+="
    assert ae.AssignmentExpression.value == "right"


#### AssignmentPattern ###################################################################################################################################
#
#        d8888                   d8b                                                   888    8888888b.           888    888
#       d88888                   Y8P                                                   888    888   Y88b          888    888
#      d88P888                                                                         888    888    888          888    888
#     d88P 888 .d8888b  .d8888b  888  .d88b.  88888b.  88888b.d88b.   .d88b.  88888b.  888888 888   d88P  8888b.  888888 888888  .d88b.  888d888 88888b.
#    d88P  888 88K      88K      888 d88P"88b 888 "88b 888 "888 "88b d8P  Y8b 888 "88b 888    8888888P"      "88b 888    888    d8P  Y8b 888P"   888 "88b
#   d88P   888 "Y8888b. "Y8888b. 888 888  888 888  888 888  888  888 88888888 888  888 888    888        .d888888 888    888    88888888 888     888  888
#  d8888888888      X88      X88 888 Y88b 888 888  888 888  888  888 Y8b.     888  888 Y88b.  888        888  888 Y88b.  Y88b.  Y8b.     888     888  888
# d88P     888  88888P'  88888P' 888  "Y88888 888  888 888  888  888  "Y8888  888  888  "Y888 888        "Y888888  "Y888  "Y888  "Y8888  888     888  888
#                                         888
#                                    Y8b d88P
#                                     "Y88P"
#
##########################################################################################################################################################
def test_P2_AssignmentPattern_init(context):
    ap = e.P2_AssignmentPattern(context, "StrictArg", FakeTokens("child"))
    assert ap.name == "AssignmentPattern"
    assert ap.context == context
    assert [tok.value for tok in ap.children] == ["child"]


def test_P2_AssignmentPattern_ObjectAssignmentPattern_init(context):
    ap = e.P2_AssignmentPattern_ObjectAssignmentPattern(context, "StrictArg", FakeTokens("child"))
    assert ap.name == "AssignmentPattern"
    assert ap.ObjectAssignmentPattern.value == "child"


def test_P2_AssignmentPattern_ArrayAssignmentPattern_init(context):
    ap = e.P2_AssignmentPattern_ArrayAssignmentPattern(context, "StrictArg", FakeTokens("child"))
    assert ap.name == "AssignmentPattern"
    assert ap.ArrayAssignmentPattern.value == "child"


#### ObjectAssignmentPattern ##################################################################################################################################################################################
#
#  .d88888b.  888         d8b                   888           d8888                   d8b                                                   888    8888888b.           888    888
# d88P" "Y88b 888         Y8P                   888          d88888                   Y8P                                                   888    888   Y88b          888    888
# 888     888 888                               888         d88P888                                                                         888    888    888          888    888
# 888     888 88888b.    8888  .d88b.   .d8888b 888888     d88P 888 .d8888b  .d8888b  888  .d88b.  88888b.  88888b.d88b.   .d88b.  88888b.  888888 888   d88P  8888b.  888888 888888  .d88b.  888d888 88888b.
# 888     888 888 "88b   "888 d8P  Y8b d88P"    888       d88P  888 88K      88K      888 d88P"88b 888 "88b 888 "888 "88b d8P  Y8b 888 "88b 888    8888888P"      "88b 888    888    d8P  Y8b 888P"   888 "88b
# 888     888 888  888    888 88888888 888      888      d88P   888 "Y8888b. "Y8888b. 888 888  888 888  888 888  888  888 88888888 888  888 888    888        .d888888 888    888    88888888 888     888  888
# Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.   d8888888888      X88      X88 888 Y88b 888 888  888 888  888  888 Y8b.     888  888 Y88b.  888        888  888 Y88b.  Y88b.  Y8b.     888     888  888
#  "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888 d88P     888  88888P'  88888P' 888  "Y88888 888  888 888  888  888  "Y8888  888  888  "Y888 888        "Y888888  "Y888  "Y888  "Y8888  888     888  888
#                         888                                                                  888
#                        d88P                                                             Y8b d88P
#                      888P"                                                               "Y88P"
#
###############################################################################################################################################################################################################
def test_P2_ObjectAssignmentPattern_init(context):
    oap = e.P2_ObjectAssignmentPattern(context, "StrictArg", FakeTokens("child"))
    assert oap.name == "ObjectAssignmentPattern"
    assert oap.context == context
    assert [tok.value for tok in oap.children] == ["child"]


def test_P2_ObjectAssignmentPattern_Empty_init(context):
    oap = e.P2_ObjectAssignmentPattern_Empty(context, "StrictArg", FakeTokens("{", "}"))
    assert oap.name == "ObjectAssignmentPattern"


def test_P2_ObjectAssignmentPattern_AssignmentRestProperty_init(context):
    oap = e.P2_ObjectAssignmentPattern_AssignmentRestProperty(context, "StrictArg", FakeTokens("{", "child", "}"))
    assert oap.name == "ObjectAssignmentPattern"
    assert oap.AssignmentRestProperty.value == "child"


def test_P2_ObjectAssignmentPattern_AssignmentPropertyList_init(context):
    oap = e.P2_ObjectAssignmentPattern_AssignmentPropertyList(context, "StrictArg", FakeTokens("{", "child", "}"))
    assert oap.name == "ObjectAssignmentPattern"
    assert oap.AssignmentPropertyList.value == "child"


def test_P2_ObjectAssignmentPattern_AssignmentPropertyList_AssignmentRestProperty_init(context):
    oap = e.P2_ObjectAssignmentPattern_AssignmentPropertyList_AssignmentRestProperty(
        context, "StrictArg", FakeTokens("{", "child", ",", "right", "}")
    )
    assert oap.name == "ObjectAssignmentPattern"
    assert oap.AssignmentPropertyList.value == "child"
    assert oap.AssignmentRestProperty.value == "right"


#### ArrayAssignmentPattern #############################################################################################################################################################################
#
#        d8888                                          d8888                   d8b                                                   888    8888888b.           888    888
#       d88888                                         d88888                   Y8P                                                   888    888   Y88b          888    888
#      d88P888                                        d88P888                                                                         888    888    888          888    888
#     d88P 888 888d888 888d888  8888b.  888  888     d88P 888 .d8888b  .d8888b  888  .d88b.  88888b.  88888b.d88b.   .d88b.  88888b.  888888 888   d88P  8888b.  888888 888888  .d88b.  888d888 88888b.
#    d88P  888 888P"   888P"       "88b 888  888    d88P  888 88K      88K      888 d88P"88b 888 "88b 888 "888 "88b d8P  Y8b 888 "88b 888    8888888P"      "88b 888    888    d8P  Y8b 888P"   888 "88b
#   d88P   888 888     888     .d888888 888  888   d88P   888 "Y8888b. "Y8888b. 888 888  888 888  888 888  888  888 88888888 888  888 888    888        .d888888 888    888    88888888 888     888  888
#  d8888888888 888     888     888  888 Y88b 888  d8888888888      X88      X88 888 Y88b 888 888  888 888  888  888 Y8b.     888  888 Y88b.  888        888  888 Y88b.  Y88b.  Y8b.     888     888  888
# d88P     888 888     888     "Y888888  "Y88888 d88P     888  88888P'  88888P' 888  "Y88888 888  888 888  888  888  "Y8888  888  888  "Y888 888        "Y888888  "Y888  "Y888  "Y8888  888     888  888
#                                            888                                         888
#                                       Y8b d88P                                    Y8b d88P
#                                        "Y88P"                                      "Y88P"
#
#########################################################################################################################################################################################################
def test_P2_ArrayAssignmentPattern_init(context):
    aap = e.P2_ArrayAssignmentPattern(context, "StrictArg", FakeTokens("child"))
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.context == context
    assert [tok.value for tok in aap.children] == ["child"]
    assert aap.Elision is None
    assert aap.AssignmentRestElement is None
    assert aap.AssignmentElementList is None


def test_P2_ArrayAssignmentPattern_Empty_init(context):
    aap = e.P2_ArrayAssignmentPattern_Empty(context, "StrictArg", FakeTokens("[", "]"))
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.Elision is None
    assert aap.AssignmentRestElement is None
    assert aap.AssignmentElementList is None


def test_P2_ArrayAssignmentPattern_AssignmentRestElement_init(context):
    aap = e.P2_ArrayAssignmentPattern_AssignmentRestElement(context, "StrictArg", FakeTokens("[", "ARE", "]"))
    assert aap.Elision is None
    assert aap.AssignmentRestElement.value == "ARE"
    assert aap.AssignmentElementList is None


def test_P2_ArrayAssignmentPattern_Elision_init(context):
    aap = e.P2_ArrayAssignmentPattern_Elision(context, "StrictArg", FakeTokens("[", "ELISION", "]"))
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.Elision.value == "ELISION"
    assert aap.AssignmentRestElement is None
    assert aap.AssignmentElementList is None


def test_P2_ArrayAssignmentPattern_Elision_AssignmentRestElement_init(context):
    aap = e.P2_ArrayAssignmentPattern_Elision_AssignmentRestElement(
        context, "StrictArg", FakeTokens("[", "ELISION", "ARE", "]")
    )
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.Elision.value == "ELISION"
    assert aap.AssignmentRestElement.value == "ARE"
    assert aap.AssignmentElementList is None


def test_P2_ArrayAssignmentPattern_AssignmentElementList_init(context):
    aap = e.P2_ArrayAssignmentPattern_AssignmentElementList(context, "StrictArg", FakeTokens("[", "AEL", "]"))
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.Elision is None
    assert aap.AssignmentRestElement is None
    assert aap.AssignmentElementList.value == "AEL"


def test_P2_ArrayAssignmentPattern_AssignmentElementList_Elision_init(context):
    aap = e.P2_ArrayAssignmentPattern_AssignmentElementList_Elision(
        context, "StrictArg", FakeTokens("[", "AEL", ",", "ELISION", "]")
    )
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.Elision.value == "ELISION"
    assert aap.AssignmentRestElement is None
    assert aap.AssignmentElementList.value == "AEL"


def test_P2_ArrayAssignmentPattern_AssignmentElementList_AssignmentRestElement_init(context):
    aap = e.P2_ArrayAssignmentPattern_AssignmentElementList_AssignmentRestElement(
        context, "StrictArg", FakeTokens("[", "AEL", ",", "ARE", "]")
    )
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.Elision is None
    assert aap.AssignmentRestElement.value == "ARE"
    assert aap.AssignmentElementList.value == "AEL"


def test_P2_ArrayAssignmentPattern_AssignmentElementList_Elision_AssignmentRestElement_init(context):
    aap = e.P2_ArrayAssignmentPattern_AssignmentElementList_Elision_AssignmentRestElement(
        context, "StrictArg", FakeTokens("[", "AEL", ",", "ELISION", "ARE", "]")
    )
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.Elision.value == "ELISION"
    assert aap.AssignmentRestElement.value == "ARE"
    assert aap.AssignmentElementList.value == "AEL"


#### AssignmentRestProperty ############################################################################################################################################################################
#
#        d8888                   d8b                                                   888    8888888b.                    888    8888888b.                                             888
#       d88888                   Y8P                                                   888    888   Y88b                   888    888   Y88b                                            888
#      d88P888                                                                         888    888    888                   888    888    888                                            888
#     d88P 888 .d8888b  .d8888b  888  .d88b.  88888b.  88888b.d88b.   .d88b.  88888b.  888888 888   d88P  .d88b.  .d8888b  888888 888   d88P 888d888  .d88b.  88888b.   .d88b.  888d888 888888 888  888
#    d88P  888 88K      88K      888 d88P"88b 888 "88b 888 "888 "88b d8P  Y8b 888 "88b 888    8888888P"  d8P  Y8b 88K      888    8888888P"  888P"   d88""88b 888 "88b d8P  Y8b 888P"   888    888  888
#   d88P   888 "Y8888b. "Y8888b. 888 888  888 888  888 888  888  888 88888888 888  888 888    888 T88b   88888888 "Y8888b. 888    888        888     888  888 888  888 88888888 888     888    888  888
#  d8888888888      X88      X88 888 Y88b 888 888  888 888  888  888 Y8b.     888  888 Y88b.  888  T88b  Y8b.          X88 Y88b.  888        888     Y88..88P 888 d88P Y8b.     888     Y88b.  Y88b 888
# d88P     888  88888P'  88888P' 888  "Y88888 888  888 888  888  888  "Y8888  888  888  "Y888 888   T88b  "Y8888   88888P'  "Y888 888        888      "Y88P"  88888P"   "Y8888  888      "Y888  "Y88888
#                                         888                                                                                                                 888                                   888
#                                    Y8b d88P                                                                                                                 888                              Y8b d88P
#                                     "Y88P"                                                                                                                  888                               "Y88P"
#
########################################################################################################################################################################################################
def test_P2_AssignmentRestProperty_init(context):
    arp = e.P2_AssignmentRestProperty(context, "StrictArg", FakeTokens("child"))
    assert arp.name == "AssignmentRestProperty"
    assert arp.context == context
    assert [tok.value for tok in arp.children] == ["child"]


def test_P2_AssignmentRestProperty_DestructuringAssignmentTarget_init(context):
    arp = e.P2_AssignmentRestProperty_DestructuringAssignmentTarget(context, "StrictArg", FakeTokens("...", "DAT"))
    assert arp.name == "AssignmentRestProperty"
    assert arp.DestructuringAssignmentTarget.value == "DAT"


#### AssignmentPropertyList #####################################################################################################################################################################
#
#        d8888                   d8b                                                   888    8888888b.                                             888             888      d8b          888
#       d88888                   Y8P                                                   888    888   Y88b                                            888             888      Y8P          888
#      d88P888                                                                         888    888    888                                            888             888                   888
#     d88P 888 .d8888b  .d8888b  888  .d88b.  88888b.  88888b.d88b.   .d88b.  88888b.  888888 888   d88P 888d888  .d88b.  88888b.   .d88b.  888d888 888888 888  888 888      888 .d8888b  888888
#    d88P  888 88K      88K      888 d88P"88b 888 "88b 888 "888 "88b d8P  Y8b 888 "88b 888    8888888P"  888P"   d88""88b 888 "88b d8P  Y8b 888P"   888    888  888 888      888 88K      888
#   d88P   888 "Y8888b. "Y8888b. 888 888  888 888  888 888  888  888 88888888 888  888 888    888        888     888  888 888  888 88888888 888     888    888  888 888      888 "Y8888b. 888
#  d8888888888      X88      X88 888 Y88b 888 888  888 888  888  888 Y8b.     888  888 Y88b.  888        888     Y88..88P 888 d88P Y8b.     888     Y88b.  Y88b 888 888      888      X88 Y88b.
# d88P     888  88888P'  88888P' 888  "Y88888 888  888 888  888  888  "Y8888  888  888  "Y888 888        888      "Y88P"  88888P"   "Y8888  888      "Y888  "Y88888 88888888 888  88888P'  "Y888
#                                         888                                                                             888                                   888
#                                    Y8b d88P                                                                             888                              Y8b d88P
#                                     "Y88P"                                                                              888                               "Y88P"
#
#################################################################################################################################################################################################
def test_P2_AssignmentPropertyList_init(context):
    apl = e.P2_AssignmentPropertyList(context, "StrictArg", FakeTokens("child"))
    assert apl.name == "AssignmentPropertyList"
    assert apl.context == context
    assert [tok.value for tok in apl.children] == ["child"]


def test_P2_AssignmentPropertyList_AssignmentProperty_init(context):
    apl = e.P2_AssignmentPropertyList_AssignmentProperty(context, "StrictArg", FakeTokens("child"))
    assert apl.name == "AssignmentPropertyList"
    assert apl.AssignmentProperty.value == "child"


def test_P2_AssignmentPropertyList_AssignmentPropertyList_AssignmentProperty_init(context):
    apl = e.P2_AssignmentPropertyList_AssignmentPropertyList_AssignmentProperty(
        context, "StrictArg", FakeTokens("APL", ",", "AP")
    )
    assert apl.name == "AssignmentPropertyList"
    assert apl.AssignmentPropertyList.value == "APL"
    assert apl.AssignmentProperty.value == "AP"


#### AssignmentElementList ###############################################################################################################################################################
#
#        d8888                   d8b                                                   888    8888888888 888                                          888    888      d8b          888
#       d88888                   Y8P                                                   888    888        888                                          888    888      Y8P          888
#      d88P888                                                                         888    888        888                                          888    888                   888
#     d88P 888 .d8888b  .d8888b  888  .d88b.  88888b.  88888b.d88b.   .d88b.  88888b.  888888 8888888    888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888 888      888 .d8888b  888888
#    d88P  888 88K      88K      888 d88P"88b 888 "88b 888 "888 "88b d8P  Y8b 888 "88b 888    888        888 d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888    888      888 88K      888
#   d88P   888 "Y8888b. "Y8888b. 888 888  888 888  888 888  888  888 88888888 888  888 888    888        888 88888888 888  888  888 88888888 888  888 888    888      888 "Y8888b. 888
#  d8888888888      X88      X88 888 Y88b 888 888  888 888  888  888 Y8b.     888  888 Y88b.  888        888 Y8b.     888  888  888 Y8b.     888  888 Y88b.  888      888      X88 Y88b.
# d88P     888  88888P'  88888P' 888  "Y88888 888  888 888  888  888  "Y8888  888  888  "Y888 8888888888 888  "Y8888  888  888  888  "Y8888  888  888  "Y888 88888888 888  88888P'  "Y888
#                                         888
#                                    Y8b d88P
#                                     "Y88P"
#
##########################################################################################################################################################################################
def test_P2_AssignmentElementList_init(context):
    ael = e.P2_AssignmentElementList(context, "StrictArg", FakeTokens("child"))
    assert ael.name == "AssignmentElementList"
    assert ael.context == context
    assert [tok.value for tok in ael.children] == ["child"]


def test_P2_AssignmentElementList_AssignmentElisionElement_init(context):
    ael = e.P2_AssignmentElementList_AssignmentElisionElement(context, "StrictArg", FakeTokens("child"))
    assert ael.name == "AssignmentElementList"
    assert ael.AssignmentElisionElement.value == "child"


def test_P2_AssignmentElementList_AssignmentElementList_AssignmentElisionElement_init(context):
    ael = e.P2_AssignmentElementList_AssignmentElementList_AssignmentElisionElement(
        context, "StrictArg", FakeTokens("child", ",", "sibling")
    )
    assert ael.name == "AssignmentElementList"
    assert ael.AssignmentElementList.value == "child"
    assert ael.AssignmentElisionElement.value == "sibling"


#### AssignmentElisionElement #################################################################################################################################################################################
#
#        d8888                   d8b                                                   888    8888888888 888 d8b          d8b                   8888888888 888                                          888
#       d88888                   Y8P                                                   888    888        888 Y8P          Y8P                   888        888                                          888
#      d88P888                                                                         888    888        888                                    888        888                                          888
#     d88P 888 .d8888b  .d8888b  888  .d88b.  88888b.  88888b.d88b.   .d88b.  88888b.  888888 8888888    888 888 .d8888b  888  .d88b.  88888b.  8888888    888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
#    d88P  888 88K      88K      888 d88P"88b 888 "88b 888 "888 "88b d8P  Y8b 888 "88b 888    888        888 888 88K      888 d88""88b 888 "88b 888        888 d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
#   d88P   888 "Y8888b. "Y8888b. 888 888  888 888  888 888  888  888 88888888 888  888 888    888        888 888 "Y8888b. 888 888  888 888  888 888        888 88888888 888  888  888 88888888 888  888 888
#  d8888888888      X88      X88 888 Y88b 888 888  888 888  888  888 Y8b.     888  888 Y88b.  888        888 888      X88 888 Y88..88P 888  888 888        888 Y8b.     888  888  888 Y8b.     888  888 Y88b.
# d88P     888  88888P'  88888P' 888  "Y88888 888  888 888  888  888  "Y8888  888  888  "Y888 8888888888 888 888  88888P' 888  "Y88P"  888  888 8888888888 888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#                                         888
#                                    Y8b d88P
#                                     "Y88P"
#
###############################################################################################################################################################################################################
def test_P2_AssignmentElisionElement_init(context):
    aee = e.P2_AssignmentElisionElement(context, "StrictArg", FakeTokens("child"))
    assert aee.name == "AssignmentElisionElement"
    assert aee.context == context
    assert [tok.value for tok in aee.children] == ["child"]


def test_P2_AssignmentElisionElement_Elision_AssignmentElement_init(context):
    aee = e.P2_AssignmentElisionElement_Elision_AssignmentElement(context, "StrictArg", FakeTokens("Elision", "ae"))
    assert aee.name == "AssignmentElisionElement"
    assert aee.Elision.value == "Elision"
    assert aee.AssignmentElement.value == "ae"


def test_P2_AssignmentElisionElement_AssignmentElement_init(context):
    aee = e.P2_AssignmentElisionElement_AssignmentElement(context, "StrictArg", FakeTokens("child"))
    assert aee.name == "AssignmentElisionElement"
    assert aee.AssignmentElement.value == "child"


#### AssignmentProperty ############################################################################################################################################
#
#        d8888                   d8b                                                   888    8888888b.                                             888
#       d88888                   Y8P                                                   888    888   Y88b                                            888
#      d88P888                                                                         888    888    888                                            888
#     d88P 888 .d8888b  .d8888b  888  .d88b.  88888b.  88888b.d88b.   .d88b.  88888b.  888888 888   d88P 888d888  .d88b.  88888b.   .d88b.  888d888 888888 888  888
#    d88P  888 88K      88K      888 d88P"88b 888 "88b 888 "888 "88b d8P  Y8b 888 "88b 888    8888888P"  888P"   d88""88b 888 "88b d8P  Y8b 888P"   888    888  888
#   d88P   888 "Y8888b. "Y8888b. 888 888  888 888  888 888  888  888 88888888 888  888 888    888        888     888  888 888  888 88888888 888     888    888  888
#  d8888888888      X88      X88 888 Y88b 888 888  888 888  888  888 Y8b.     888  888 Y88b.  888        888     Y88..88P 888 d88P Y8b.     888     Y88b.  Y88b 888
# d88P     888  88888P'  88888P' 888  "Y88888 888  888 888  888  888  "Y8888  888  888  "Y888 888        888      "Y88P"  88888P"   "Y8888  888      "Y888  "Y88888
#                                         888                                                                             888                                   888
#                                    Y8b d88P                                                                             888                              Y8b d88P
#                                     "Y88P"                                                                              888                               "Y88P"
#
####################################################################################################################################################################
def test_P2_AssignmentProperty_init(context):
    ap = e.P2_AssignmentProperty(context, "StrictArg", FakeTokens("child"))
    assert ap.name == "AssignmentProperty"
    assert ap.context == context
    assert [tok.value for tok in ap.children] == ["child"]


def test_P2_AssignmentProperty_IdentifierReference_init(context):
    ap = e.P2_AssignmentProperty_IdentifierReference(context, "StrictArg", FakeTokens("child"))
    assert ap.name == "AssignmentProperty"
    assert ap.IdentifierReference.value == "child"


def test_P2_AssignmentProperty_IdentifierReference_Initializer_init(context):
    ap = e.P2_AssignmentProperty_IdentifierReference_Initializer(context, "StrictArg", FakeTokens("child", "init"))
    assert ap.name == "AssignmentProperty"
    assert ap.IdentifierReference.value == "child"
    assert ap.Initializer.value == "init"


def test_P2_AssignmentProperty_PropertyName_AssignmentElement_init(context):
    ap = e.P2_AssignmentProperty_PropertyName_AssignmentElement(
        context, "StrictArg", FakeTokens("child", ":", "bob")
    )
    assert ap.name == "AssignmentProperty"
    assert ap.PropertyName.value == "child"
    assert ap.AssignmentElement.value == "bob"


#### AssignmentElement ######################################################################################################################################
#
#        d8888                   d8b                                                   888    8888888888 888                                          888
#       d88888                   Y8P                                                   888    888        888                                          888
#      d88P888                                                                         888    888        888                                          888
#     d88P 888 .d8888b  .d8888b  888  .d88b.  88888b.  88888b.d88b.   .d88b.  88888b.  888888 8888888    888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
#    d88P  888 88K      88K      888 d88P"88b 888 "88b 888 "888 "88b d8P  Y8b 888 "88b 888    888        888 d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
#   d88P   888 "Y8888b. "Y8888b. 888 888  888 888  888 888  888  888 88888888 888  888 888    888        888 88888888 888  888  888 88888888 888  888 888
#  d8888888888      X88      X88 888 Y88b 888 888  888 888  888  888 Y8b.     888  888 Y88b.  888        888 Y8b.     888  888  888 Y8b.     888  888 Y88b.
# d88P     888  88888P'  88888P' 888  "Y88888 888  888 888  888  888  "Y8888  888  888  "Y888 8888888888 888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#                                         888
#                                    Y8b d88P
#                                     "Y88P"
#
#############################################################################################################################################################
def test_P2_AssignmentElement_init(context):
    ae = e.P2_AssignmentElement(context, "StrictArg", FakeTokens("child"))
    assert ae.name == "AssignmentElement"
    assert ae.context == context
    assert [tok.value for tok in ae.children] == ["child"]


def test_P2_AssignmentElement_DestructuringAssignmentTarget_init(context):
    ae = e.P2_AssignmentElement_DestructuringAssignmentTarget(
        context, "StrictArg", FakeTokens("child"), "yieldarg", "awaitarg"
    )
    assert ae.name == "AssignmentElement"
    assert ae.DestructuringAssignmentTarget.value == "child"
    assert ae.Yield == "yieldarg"
    assert ae.Await == "awaitarg"


def test_P2_AssignmentElement_DestructuringAssignmentTarget_Initializer_init(context):
    ae = e.P2_AssignmentElement_DestructuringAssignmentTarget_Initializer(
        context, "StrictArg", FakeTokens("child", "init"), "yieldarg", "awaitarg"
    )
    assert ae.name == "AssignmentElement"
    assert ae.DestructuringAssignmentTarget.value == "child"
    assert ae.Initializer.value == "init"
    assert ae.Yield == "yieldarg"
    assert ae.Await == "awaitarg"


#### AssignmentRestElement ######################################################################################################################################################################
#
#        d8888                   d8b                                                   888    8888888b.                    888    8888888888 888                                          888
#       d88888                   Y8P                                                   888    888   Y88b                   888    888        888                                          888
#      d88P888                                                                         888    888    888                   888    888        888                                          888
#     d88P 888 .d8888b  .d8888b  888  .d88b.  88888b.  88888b.d88b.   .d88b.  88888b.  888888 888   d88P  .d88b.  .d8888b  888888 8888888    888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
#    d88P  888 88K      88K      888 d88P"88b 888 "88b 888 "888 "88b d8P  Y8b 888 "88b 888    8888888P"  d8P  Y8b 88K      888    888        888 d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
#   d88P   888 "Y8888b. "Y8888b. 888 888  888 888  888 888  888  888 88888888 888  888 888    888 T88b   88888888 "Y8888b. 888    888        888 88888888 888  888  888 88888888 888  888 888
#  d8888888888      X88      X88 888 Y88b 888 888  888 888  888  888 Y8b.     888  888 Y88b.  888  T88b  Y8b.          X88 Y88b.  888        888 Y8b.     888  888  888 Y8b.     888  888 Y88b.
# d88P     888  88888P'  88888P' 888  "Y88888 888  888 888  888  888  "Y8888  888  888  "Y888 888   T88b  "Y8888   88888P'  "Y888 8888888888 888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#                                         888
#                                    Y8b d88P
#                                     "Y88P"
#
#################################################################################################################################################################################################
def test_P2_AssignmentRestElement_init(context):
    are = e.P2_AssignmentRestElement(context, "StrictArg", FakeTokens("child"))
    assert are.name == "AssignmentRestElement"
    assert are.context == context
    assert [tok.value for tok in are.children] == ["child"]


def test_P2_AssignmentRestElement_DestructuringAssignmentTarget_init(context):
    are = e.P2_AssignmentRestElement_DestructuringAssignmentTarget(
        context, "StrictArg", FakeTokens("...", "child"), "yieldarg", "awaitarg"
    )
    assert are.name == "AssignmentRestElement"
    assert are.DestructuringAssignmentTarget.value == "child"
    assert are.Yield == "yieldarg"
    assert are.Await == "awaitarg"


##### DestructuringAssignmentTarget ############################################################################################################################################################################################################################
#
# 8888888b.                    888                              888                     d8b                          d8888                   d8b                                                   888    88888888888                                    888
# 888  "Y88b                   888                              888                     Y8P                         d88888                   Y8P                                                   888        888                                        888
# 888    888                   888                              888                                                d88P888                                                                         888        888                                        888
# 888    888  .d88b.  .d8888b  888888 888d888 888  888  .d8888b 888888 888  888 888d888 888 88888b.   .d88b.      d88P 888 .d8888b  .d8888b  888  .d88b.  88888b.  88888b.d88b.   .d88b.  88888b.  888888     888      8888b.  888d888  .d88b.   .d88b.  888888
# 888    888 d8P  Y8b 88K      888    888P"   888  888 d88P"    888    888  888 888P"   888 888 "88b d88P"88b    d88P  888 88K      88K      888 d88P"88b 888 "88b 888 "888 "88b d8P  Y8b 888 "88b 888        888         "88b 888P"   d88P"88b d8P  Y8b 888
# 888    888 88888888 "Y8888b. 888    888     888  888 888      888    888  888 888     888 888  888 888  888   d88P   888 "Y8888b. "Y8888b. 888 888  888 888  888 888  888  888 88888888 888  888 888        888     .d888888 888     888  888 88888888 888
# 888  .d88P Y8b.          X88 Y88b.  888     Y88b 888 Y88b.    Y88b.  Y88b 888 888     888 888  888 Y88b 888  d8888888888      X88      X88 888 Y88b 888 888  888 888  888  888 Y8b.     888  888 Y88b.      888     888  888 888     Y88b 888 Y8b.     Y88b.
# 8888888P"   "Y8888   88888P'  "Y888 888      "Y88888  "Y8888P  "Y888  "Y88888 888     888 888  888  "Y88888 d88P     888  88888P'  88888P' 888  "Y88888 888  888 888  888  888  "Y8888  888  888  "Y888     888     "Y888888 888      "Y88888  "Y8888   "Y888
#                                                                                                         888                                         888                                                                                   888
#                                                                                                    Y8b d88P                                    Y8b d88P                                                                              Y8b d88P
#                                                                                                     "Y88P"                                      "Y88P"                                                                                "Y88P"
#
################################################################################################################################################################################################################################################################
def test_P2_DestructuringAssignmentTarget_init(context):
    dat = e.P2_DestructuringAssignmentTarget(context, "StrictArg", FakeTokens("child"))
    assert dat.name == "DestructuringAssignmentTarget"
    assert dat.context == context
    assert [tok.value for tok in dat.children] == ["child"]


def test_P2_DestructuringAssignmentTarget_LeftHandSideExpression_init(context):
    dat = e.P2_DestructuringAssignmentTarget_LeftHandSideExpression(context, "StrictArg", FakeTokens("child"))
    assert dat.name == "DestructuringAssignmentTarget"
    assert dat.LeftHandSideExpression.value == "child"


#### Statement #####################################################################
#
#  .d8888b.  888             888                                             888
# d88P  Y88b 888             888                                             888
# Y88b.      888             888                                             888
#  "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
#     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
#       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
# Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
#  "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
#
#
#
####################################################################################
def test_P2_Statement_init(context):
    statement = e.P2_Statement(context, "StrictArg", FakeTokens("child"))
    assert statement.name == "Statement"
    assert statement.context == context
    assert [tok.value for tok in statement.children] == ["child"]


def test_P2_Statement_BlockStatement_init(context):
    statement = e.P2_Statement_BlockStatement(context, "StrictArg", FakeTokens("child"))
    assert statement.name == "Statement"
    assert statement.BlockStatement.value == "child"


def test_P2_Statement_VariableStatement_init(context):
    statement = e.P2_Statement_VariableStatement(context, "StrictArg", FakeTokens("child"))
    assert statement.name == "Statement"
    assert statement.VariableStatement.value == "child"


def test_P2_Statement_EmptyStatement_init(context):
    statement = e.P2_Statement_EmptyStatement(context, "StrictArg", FakeTokens("child"))
    assert statement.name == "Statement"
    assert statement.EmptyStatement.value == "child"


def test_P2_Statement_ExpressionStatement_init(context):
    statement = e.P2_Statement_ExpressionStatement(context, "StrictArg", FakeTokens("child"))
    assert statement.name == "Statement"
    assert statement.ExpressionStatement.value == "child"


def test_P2_Statement_IfStatement_init(context):
    statement = e.P2_Statement_IfStatement(context, "StrictArg", FakeTokens("child"))
    assert statement.name == "Statement"
    assert statement.IfStatement.value == "child"


def test_P2_Statement_BreakableStatement_init(context):
    statement = e.P2_Statement_BreakableStatement(context, "StrictArg", FakeTokens("child"))
    assert statement.name == "Statement"
    assert statement.BreakableStatement.value == "child"


def test_P2_Statement_ContinueStatement_init(context):
    statement = e.P2_Statement_ContinueStatement(context, "StrictArg", FakeTokens("child"))
    assert statement.name == "Statement"
    assert statement.ContinueStatement.value == "child"


def test_P2_Statement_BreakStatement_init(context):
    statement = e.P2_Statement_BreakStatement(context, "StrictArg", FakeTokens("child"))
    assert statement.name == "Statement"
    assert statement.BreakStatement.value == "child"


def test_P2_Statement_ReturnStatement_init(context):
    statement = e.P2_Statement_ReturnStatement(context, "StrictArg", FakeTokens("child"))
    assert statement.name == "Statement"
    assert statement.ReturnStatement.value == "child"


def test_P2_Statement_WithStatement_init(context):
    statement = e.P2_Statement_WithStatement(context, "StrictArg", FakeTokens("child"))
    assert statement.name == "Statement"
    assert statement.WithStatement.value == "child"


def test_P2_Statement_LabelledStatement_init(context):
    statement = e.P2_Statement_LabelledStatement(context, "StrictArg", FakeTokens("child"))
    assert statement.name == "Statement"
    assert statement.LabelledStatement.value == "child"


def test_P2_Statement_ThrowStatement_init(context):
    statement = e.P2_Statement_ThrowStatement(context, "StrictArg", FakeTokens("child"))
    assert statement.name == "Statement"
    assert statement.ThrowStatement.value == "child"


def test_P2_Statement_TryStatement_init(context):
    statement = e.P2_Statement_TryStatement(context, "StrictArg", FakeTokens("child"))
    assert statement.name == "Statement"
    assert statement.TryStatement.value == "child"


def test_P2_Statement_DebuggerStatement_init(context):
    statement = e.P2_Statement_DebuggerStatement(context, "StrictArg", FakeTokens("child"))
    assert statement.name == "Statement"
    assert statement.DebuggerStatement.value == "child"


#### Declaration #########################################################################
#
# 8888888b.                    888                           888    d8b
# 888  "Y88b                   888                           888    Y8P
# 888    888                   888                           888
# 888    888  .d88b.   .d8888b 888  8888b.  888d888  8888b.  888888 888  .d88b.  88888b.
# 888    888 d8P  Y8b d88P"    888     "88b 888P"       "88b 888    888 d88""88b 888 "88b
# 888    888 88888888 888      888 .d888888 888     .d888888 888    888 888  888 888  888
# 888  .d88P Y8b.     Y88b.    888 888  888 888     888  888 Y88b.  888 Y88..88P 888  888
# 8888888P"   "Y8888   "Y8888P 888 "Y888888 888     "Y888888  "Y888 888  "Y88P"  888  888
#
#
#
#
##########################################################################################
def test_P2_Declaration_init(context):
    decl = e.P2_Declaration(context, "StrictArg", FakeTokens("child"))
    assert decl.name == "Declaration"
    assert decl.context == context
    assert [tok.value for tok in decl.children] == ["child"]


def test_P2_Declaration_HoistableDeclaration_init(context):
    decl = e.P2_Declaration_HoistableDeclaration(context, "StrictArg", FakeTokens("child"))
    assert decl.name == "Declaration"
    assert decl.HoistableDeclaration.value == "child"


def test_P2_Declaration_ClassDeclaration_init(context):
    decl = e.P2_Declaration_ClassDeclaration(context, "StrictArg", FakeTokens("child"))
    assert decl.name == "Declaration"
    assert decl.ClassDeclaration.value == "child"


def test_P2_Declaration_LexicalDeclaration_init(context):
    decl = e.P2_Declaration_LexicalDeclaration(context, "StrictArg", FakeTokens("child"))
    assert decl.name == "Declaration"
    assert decl.LexicalDeclaration.value == "child"


#### HoistableDeclaration #######################################################################################################################################
#
# 888    888          d8b          888             888      888          8888888b.                    888                           888    d8b
# 888    888          Y8P          888             888      888          888  "Y88b                   888                           888    Y8P
# 888    888                       888             888      888          888    888                   888                           888
# 8888888888  .d88b.  888 .d8888b  888888  8888b.  88888b.  888  .d88b.  888    888  .d88b.   .d8888b 888  8888b.  888d888  8888b.  888888 888  .d88b.  88888b.
# 888    888 d88""88b 888 88K      888        "88b 888 "88b 888 d8P  Y8b 888    888 d8P  Y8b d88P"    888     "88b 888P"       "88b 888    888 d88""88b 888 "88b
# 888    888 888  888 888 "Y8888b. 888    .d888888 888  888 888 88888888 888    888 88888888 888      888 .d888888 888     .d888888 888    888 888  888 888  888
# 888    888 Y88..88P 888      X88 Y88b.  888  888 888 d88P 888 Y8b.     888  .d88P Y8b.     Y88b.    888 888  888 888     888  888 Y88b.  888 Y88..88P 888  888
# 888    888  "Y88P"  888  88888P'  "Y888 "Y888888 88888P"  888  "Y8888  8888888P"   "Y8888   "Y8888P 888 "Y888888 888     "Y888888  "Y888 888  "Y88P"  888  888
#
#
#
#
#################################################################################################################################################################
def test_P2_HoistableDeclaration_init(context):
    hd = e.P2_HoistableDeclaration(context, "StrictArg", FakeTokens("child"))
    assert hd.name == "HoistableDeclaration"
    assert hd.context == context
    assert [tok.value for tok in hd.children] == ["child"]


def test_P2_HoistableDeclaration_FunctionDeclaration_init(context):
    hd = e.P2_HoistableDeclaration_FunctionDeclaration(context, "StrictArg", FakeTokens("child"))
    assert hd.name == "HoistableDeclaration"
    assert hd.FunctionDeclaration.value == "child"


def test_P2_HoistableDeclaration_GeneratorDeclaration_init(context):
    hd = e.P2_HoistableDeclaration_GeneratorDeclaration(context, "StrictArg", FakeTokens("child"))
    assert hd.name == "HoistableDeclaration"
    assert hd.GeneratorDeclaration.value == "child"


def test_P2_HoistableDeclaration_AsyncFunctionDeclaration_init(context):
    hd = e.P2_HoistableDeclaration_AsyncFunctionDeclaration(context, "StrictArg", FakeTokens("child"))
    assert hd.name == "HoistableDeclaration"
    assert hd.AsyncFunctionDeclaration.value == "child"


def test_P2_HoistableDeclaration_AsyncGeneratorDeclaration_init(context):
    hd = e.P2_HoistableDeclaration_AsyncGeneratorDeclaration(context, "StrictArg", FakeTokens("child"))
    assert hd.name == "HoistableDeclaration"
    assert hd.AsyncGeneratorDeclaration.value == "child"


#### BreakableStatement #########################################################################################################################################
#
# 888888b.                             888               888      888           .d8888b.  888             888                                             888
# 888  "88b                            888               888      888          d88P  Y88b 888             888                                             888
# 888  .88P                            888               888      888          Y88b.      888             888                                             888
# 8888888K.  888d888  .d88b.   8888b.  888  888  8888b.  88888b.  888  .d88b.   "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
# 888  "Y88b 888P"   d8P  Y8b     "88b 888 .88P     "88b 888 "88b 888 d8P  Y8b     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
# 888    888 888     88888888 .d888888 888888K  .d888888 888  888 888 88888888       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
# 888   d88P 888     Y8b.     888  888 888 "88b 888  888 888 d88P 888 Y8b.     Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 8888888P"  888      "Y8888  "Y888888 888  888 "Y888888 88888P"  888  "Y8888   "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
#
#
#
#################################################################################################################################################################
def test_P2_BreakableStatement_init(context):
    bs = e.P2_BreakableStatement(context, "StrictArg", FakeTokens("child"))
    assert bs.name == "BreakableStatement"
    assert bs.context == context
    assert [tok.value for tok in bs.children] == ["child"]


def test_P2_BreakableStatement_IterationStatement_init(context):
    bs = e.P2_BreakableStatement_IterationStatement(context, "StrictArg", FakeTokens("child"))
    assert bs.name == "BreakableStatement"
    assert bs.IterationStatement.value == "child"


def test_P2_BreakableStatement_SwitchStatement_init(context):
    bs = e.P2_BreakableStatement_SwitchStatement(context, "StrictArg", FakeTokens("child"))
    assert bs.name == "BreakableStatement"
    assert bs.SwitchStatement.value == "child"


#### BlockStatement ##########################################################################################################
#
# 888888b.   888                   888       .d8888b.  888             888                                             888
# 888  "88b  888                   888      d88P  Y88b 888             888                                             888
# 888  .88P  888                   888      Y88b.      888             888                                             888
# 8888888K.  888  .d88b.   .d8888b 888  888  "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
# 888  "Y88b 888 d88""88b d88P"    888 .88P     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
# 888    888 888 888  888 888      888888K        "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
# 888   d88P 888 Y88..88P Y88b.    888 "88b Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 8888888P"  888  "Y88P"   "Y8888P 888  888  "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
#
#
#
##############################################################################################################################
def test_P2_BlockStatement_init(context):
    bs = e.P2_BlockStatement(context, "StrictArg", FakeTokens("child"))
    assert bs.name == "BlockStatement"
    assert bs.context == context
    assert [tok.value for tok in bs.children] == ["child"]


def test_P2_BlockStatement_Block_init(context):
    bs = e.P2_BlockStatement_Block(context, "StrictArg", FakeTokens("child"))
    assert bs.name == "BlockStatement"
    assert bs.Block.value == "child"


#### Block #################################
#
# 888888b.   888                   888
# 888  "88b  888                   888
# 888  .88P  888                   888
# 8888888K.  888  .d88b.   .d8888b 888  888
# 888  "Y88b 888 d88""88b d88P"    888 .88P
# 888    888 888 888  888 888      888888K
# 888   d88P 888 Y88..88P Y88b.    888 "88b
# 8888888P"  888  "Y88P"   "Y8888P 888  888
#
#
#
#
############################################
def test_P2_Block_init(context):
    block = e.P2_Block(context, "StrictArg", FakeTokens("child"))
    assert block.name == "Block"
    assert block.context == context
    assert [tok.value for tok in block.children] == ["child"]


def test_P2_Block_Empty_init(context):
    block = e.P2_Block_Empty(context, "StrictArg", FakeTokens("child"))
    assert block.name == "Block"


def test_P2_Block_StatementList_init(context):
    block = e.P2_Block_StatementList(context, "StrictArg", FakeTokens("{", "stmt", "}"))
    assert block.name == "Block"
    assert block.StatementList.value == "stmt"


#### StatementList ##############################################################################################
#
#  .d8888b.  888             888                                             888    888      d8b          888
# d88P  Y88b 888             888                                             888    888      Y8P          888
# Y88b.      888             888                                             888    888                   888
#  "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888 888      888 .d8888b  888888
#     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888    888      888 88K      888
#       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888    888      888 "Y8888b. 888
# Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.  888      888      X88 Y88b.
#  "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888 88888888 888  88888P'  "Y888
#
#
#
#
#################################################################################################################
def test_P2_StatementList_init(context):
    sl = e.P2_StatementList(context, "StrictArg", FakeTokens("child"))
    assert sl.name == "StatementList"
    assert sl.context == context
    assert [tok.value for tok in sl.children] == ["child"]


def test_P2_StatementList_StatementListItem_init(context):
    sl = e.P2_StatementList_StatementListItem(context, "StrictArg", FakeTokens("child"))
    assert sl.name == "StatementList"
    assert sl.StatementListItem.value == "child"


def test_P2_StatementList_StatementList_StatementListItem_init(context):
    sl = e.P2_StatementList_StatementList_StatementListItem(context, "StrictArg", FakeTokens("sl", "sli"))
    assert sl.name == "StatementList"
    assert sl.StatementList.value == "sl"
    assert sl.StatementListItem.value == "sli"


#### StatementListItem ################################################################################################################################
#
#  .d8888b.  888             888                                             888    888      d8b          888    8888888 888
# d88P  Y88b 888             888                                             888    888      Y8P          888      888   888
# Y88b.      888             888                                             888    888                   888      888   888
#  "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888 888      888 .d8888b  888888   888   888888  .d88b.  88888b.d88b.
#     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888    888      888 88K      888      888   888    d8P  Y8b 888 "888 "88b
#       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888    888      888 "Y8888b. 888      888   888    88888888 888  888  888
# Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.  888      888      X88 Y88b.    888   Y88b.  Y8b.     888  888  888
#  "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888 88888888 888  88888P'  "Y888 8888888  "Y888  "Y8888  888  888  888
#
#
#
#
#######################################################################################################################################################
def test_P2_StatementListItem_init(context):
    sli = e.P2_StatementListItem(context, "StrictArg", FakeTokens("child"))
    assert sli.name == "StatementListItem"
    assert sli.context == context
    assert [tok.value for tok in sli.children] == ["child"]


def test_P2_StatementListItem_Statement_init(context):
    sli = e.P2_StatementListItem_Statement(context, "StrictArg", FakeTokens("child"))
    assert sli.name == "StatementListItem"
    assert sli.Statement.value == "child"


def test_P2_StatementListItem_Declaration_init(context):
    sli = e.P2_StatementListItem_Declaration(context, "StrictArg", FakeTokens("child"))
    assert sli.name == "StatementListItem"
    assert sli.Declaration.value == "child"


#### LexicalDeclaration #######################################################################################################################
#
# 888                        d8b                   888 8888888b.                    888                           888    d8b
# 888                        Y8P                   888 888  "Y88b                   888                           888    Y8P
# 888                                              888 888    888                   888                           888
# 888       .d88b.  888  888 888  .d8888b  8888b.  888 888    888  .d88b.   .d8888b 888  8888b.  888d888  8888b.  888888 888  .d88b.  88888b.
# 888      d8P  Y8b `Y8bd8P' 888 d88P"        "88b 888 888    888 d8P  Y8b d88P"    888     "88b 888P"       "88b 888    888 d88""88b 888 "88b
# 888      88888888   X88K   888 888      .d888888 888 888    888 88888888 888      888 .d888888 888     .d888888 888    888 888  888 888  888
# 888      Y8b.     .d8""8b. 888 Y88b.    888  888 888 888  .d88P Y8b.     Y88b.    888 888  888 888     888  888 Y88b.  888 Y88..88P 888  888
# 88888888  "Y8888  888  888 888  "Y8888P "Y888888 888 8888888P"   "Y8888   "Y8888P 888 "Y888888 888     "Y888888  "Y888 888  "Y88P"  888  888
#
#
#
#
###############################################################################################################################################
def test_P2_LexicalDeclaration_init(context):
    ld = e.P2_LexicalDeclaration(context, "StrictArg", FakeTokens("child"))
    assert ld.name == "LexicalDeclaration"
    assert ld.context == context
    assert [tok.value for tok in ld.children] == ["child"]


def test_P2_LexicalDeclaration_LetOrConst_BindingList_init(context):
    ld = e.P2_LexicalDeclaration_LetOrConst_BindingList(context, "StrictArg", FakeTokens("const", "bindings", ";"))
    assert ld.name == "LexicalDeclaration"
    assert ld.LetOrConst.value == "const"
    assert ld.BindingList.value == "bindings"


#### LetOrConst ############################################################################
#
# 888               888     .d88888b.           .d8888b.                             888
# 888               888    d88P" "Y88b         d88P  Y88b                            888
# 888               888    888     888         888    888                            888
# 888       .d88b.  888888 888     888 888d888 888         .d88b.  88888b.  .d8888b  888888
# 888      d8P  Y8b 888    888     888 888P"   888        d88""88b 888 "88b 88K      888
# 888      88888888 888    888     888 888     888    888 888  888 888  888 "Y8888b. 888
# 888      Y8b.     Y88b.  Y88b. .d88P 888     Y88b  d88P Y88..88P 888  888      X88 Y88b.
# 88888888  "Y8888   "Y888  "Y88888P"  888      "Y8888P"   "Y88P"  888  888  88888P'  "Y888
#
#
#
#
############################################################################################
def test_P2_LetOrConst_init(context):
    loc = e.P2_LetOrConst(context, "StrictArg", FakeTokens("child"))
    assert loc.name == "LetOrConst"
    assert loc.context == context
    assert [tok.value for tok in loc.children] == ["child"]


def test_P2_LetOrConst_Let_init(context):
    loc = e.P2_LetOrConst_Let(context, "StrictArg", FakeTokens("child"))
    assert loc.name == "LetOrConst"


def test_P2_LetOrConst_Const_init(context):
    loc = e.P2_LetOrConst_Const(context, "StrictArg", FakeTokens("child"))
    assert loc.name == "LetOrConst"


#### BindingList #####################################################################
#
# 888888b.   d8b               888 d8b                   888      d8b          888
# 888  "88b  Y8P               888 Y8P                   888      Y8P          888
# 888  .88P                    888                       888                   888
# 8888888K.  888 88888b.   .d88888 888 88888b.   .d88b.  888      888 .d8888b  888888
# 888  "Y88b 888 888 "88b d88" 888 888 888 "88b d88P"88b 888      888 88K      888
# 888    888 888 888  888 888  888 888 888  888 888  888 888      888 "Y8888b. 888
# 888   d88P 888 888  888 Y88b 888 888 888  888 Y88b 888 888      888      X88 Y88b.
# 8888888P"  888 888  888  "Y88888 888 888  888  "Y88888 88888888 888  88888P'  "Y888
#                                                    888
#                                               Y8b d88P
#                                                "Y88P"
#
######################################################################################
def test_P2_BindingList_init(context):
    bl = e.P2_BindingList(context, "StrictArg", FakeTokens("child"))
    assert bl.name == "BindingList"
    assert bl.context == context
    assert [tok.value for tok in bl.children] == ["child"]


def test_P2_BindingList_LexicalBinding_init(context):
    bl = e.P2_BindingList_LexicalBinding(context, "StrictArg", FakeTokens("child"))
    assert bl.name == "BindingList"
    assert bl.LexicalBinding.value == "child"


def test_P2_BindingList_BindingList_LexicalBinding_init(context):
    bl = e.P2_BindingList_BindingList_LexicalBinding(context, "StrictArg", FakeTokens("child", ",", "sibling"))
    assert bl.name == "BindingList"
    assert bl.LexicalBinding.value == "sibling"
    assert bl.BindingList.value == "child"


#### LexicalBinding ##########################################################################################
#
# 888                        d8b                   888 888888b.   d8b               888 d8b
# 888                        Y8P                   888 888  "88b  Y8P               888 Y8P
# 888                                              888 888  .88P                    888
# 888       .d88b.  888  888 888  .d8888b  8888b.  888 8888888K.  888 88888b.   .d88888 888 88888b.   .d88b.
# 888      d8P  Y8b `Y8bd8P' 888 d88P"        "88b 888 888  "Y88b 888 888 "88b d88" 888 888 888 "88b d88P"88b
# 888      88888888   X88K   888 888      .d888888 888 888    888 888 888  888 888  888 888 888  888 888  888
# 888      Y8b.     .d8""8b. 888 Y88b.    888  888 888 888   d88P 888 888  888 Y88b 888 888 888  888 Y88b 888
# 88888888  "Y8888  888  888 888  "Y8888P "Y888888 888 8888888P"  888 888  888  "Y88888 888 888  888  "Y88888
#                                                                                                         888
#                                                                                                    Y8b d88P
#                                                                                                     "Y88P"
#
##############################################################################################################
def test_P2_LexicalBinding_init(context):
    lb = e.P2_LexicalBinding(context, "StrictArg", FakeTokens("child"))
    assert lb.name == "LexicalBinding"
    assert lb.context == context
    assert [tok.value for tok in lb.children] == ["child"]


def test_P2_LexicalBinding_BindingIdentifier_init(context):
    lb = e.P2_LexicalBinding_BindingIdentifier(context, "StrictArg", FakeTokens("child"))
    assert lb.name == "LexicalBinding"
    assert lb.BindingIdentifier.value == "child"


def test_P2_LexicalBinding_BindingIdentifier_Initializer_init(context):
    lb = e.P2_LexicalBinding_BindingIdentifier_Initializer(context, "StrictArg", FakeTokens("child", "init"))
    assert lb.name == "LexicalBinding"
    assert lb.BindingIdentifier.value == "child"
    assert lb.Initializer.value == "init"


def test_P2_LexicalBinding_BindingPattern_Initializer_init(context):
    lb = e.P2_LexicalBinding_BindingPattern_Initializer(context, "StrictArg", FakeTokens("child", "init"))
    assert lb.name == "LexicalBinding"
    assert lb.BindingPattern.value == "child"
    assert lb.Initializer.value == "init"


#### VariableStatement #############################################################################################################################
#
# 888     888                  d8b          888      888           .d8888b.  888             888                                             888
# 888     888                  Y8P          888      888          d88P  Y88b 888             888                                             888
# 888     888                               888      888          Y88b.      888             888                                             888
# Y88b   d88P  8888b.  888d888 888  8888b.  88888b.  888  .d88b.   "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
#  Y88b d88P      "88b 888P"   888     "88b 888 "88b 888 d8P  Y8b     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
#   Y88o88P   .d888888 888     888 .d888888 888  888 888 88888888       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
#    Y888P    888  888 888     888 888  888 888 d88P 888 Y8b.     Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
#     Y8P     "Y888888 888     888 "Y888888 88888P"  888  "Y8888   "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
#
#
#
####################################################################################################################################################
def test_P2_VariableStatement_init(context):
    vs = e.P2_VariableStatement(context, "StrictArg", FakeTokens("child"))
    assert vs.name == "VariableStatement"
    assert vs.context == context
    assert [tok.value for tok in vs.children] == ["child"]


def test_P2_VariableStatement_VariableDeclarationList_init(context):
    vs = e.P2_VariableStatement_VariableDeclarationList(context, "StrictArg", FakeTokens("var", "vdl", ";"))
    assert vs.name == "VariableStatement"
    assert vs.VariableDeclarationList.value == "vdl"


#### VariableDeclarationList ##########################################################################################################################################################
#
# 888     888                  d8b          888      888          8888888b.                    888                           888    d8b                   888      d8b          888
# 888     888                  Y8P          888      888          888  "Y88b                   888                           888    Y8P                   888      Y8P          888
# 888     888                               888      888          888    888                   888                           888                          888                   888
# Y88b   d88P  8888b.  888d888 888  8888b.  88888b.  888  .d88b.  888    888  .d88b.   .d8888b 888  8888b.  888d888  8888b.  888888 888  .d88b.  88888b.  888      888 .d8888b  888888
#  Y88b d88P      "88b 888P"   888     "88b 888 "88b 888 d8P  Y8b 888    888 d8P  Y8b d88P"    888     "88b 888P"       "88b 888    888 d88""88b 888 "88b 888      888 88K      888
#   Y88o88P   .d888888 888     888 .d888888 888  888 888 88888888 888    888 88888888 888      888 .d888888 888     .d888888 888    888 888  888 888  888 888      888 "Y8888b. 888
#    Y888P    888  888 888     888 888  888 888 d88P 888 Y8b.     888  .d88P Y8b.     Y88b.    888 888  888 888     888  888 Y88b.  888 Y88..88P 888  888 888      888      X88 Y88b.
#     Y8P     "Y888888 888     888 "Y888888 88888P"  888  "Y8888  8888888P"   "Y8888   "Y8888P 888 "Y888888 888     "Y888888  "Y888 888  "Y88P"  888  888 88888888 888  88888P'  "Y888
#
#
#
#
#######################################################################################################################################################################################
def test_P2_VariableDeclarationList_init(context):
    vdl = e.P2_VariableDeclarationList(context, "StrictArg", FakeTokens("child"))
    assert vdl.name == "VariableDeclarationList"
    assert vdl.context == context
    assert [tok.value for tok in vdl.children] == ["child"]


def test_P2_VariableDeclarationList_VariableDeclaration_init(context):
    vdl = e.P2_VariableDeclarationList_VariableDeclaration(context, "StrictArg", FakeTokens("child"))
    assert vdl.name == "VariableDeclarationList"
    assert vdl.VariableDeclaration.value == "child"


def test_P2_VariableDeclarationList_VariableDeclarationList_VariableDeclaration_init(context):
    vdl = e.P2_VariableDeclarationList_VariableDeclarationList_VariableDeclaration(
        context, "StrictArg", FakeTokens("child", ",", "sibling")
    )
    assert vdl.name == "VariableDeclarationList"
    assert vdl.VariableDeclarationList.value == "child"
    assert vdl.VariableDeclaration.value == "sibling"


#### VariableDeclaration #################################################################################################################################
#
# 888     888                  d8b          888      888          8888888b.                    888                           888    d8b
# 888     888                  Y8P          888      888          888  "Y88b                   888                           888    Y8P
# 888     888                               888      888          888    888                   888                           888
# Y88b   d88P  8888b.  888d888 888  8888b.  88888b.  888  .d88b.  888    888  .d88b.   .d8888b 888  8888b.  888d888  8888b.  888888 888  .d88b.  88888b.
#  Y88b d88P      "88b 888P"   888     "88b 888 "88b 888 d8P  Y8b 888    888 d8P  Y8b d88P"    888     "88b 888P"       "88b 888    888 d88""88b 888 "88b
#   Y88o88P   .d888888 888     888 .d888888 888  888 888 88888888 888    888 88888888 888      888 .d888888 888     .d888888 888    888 888  888 888  888
#    Y888P    888  888 888     888 888  888 888 d88P 888 Y8b.     888  .d88P Y8b.     Y88b.    888 888  888 888     888  888 Y88b.  888 Y88..88P 888  888
#     Y8P     "Y888888 888     888 "Y888888 88888P"  888  "Y8888  8888888P"   "Y8888   "Y8888P 888 "Y888888 888     "Y888888  "Y888 888  "Y88P"  888  888
#
#
#
#
##########################################################################################################################################################
def test_P2_VariableDeclaration_init(context):
    vd = e.P2_VariableDeclaration(context, "StrictArg", FakeTokens("child"))
    assert vd.name == "VariableDeclaration"
    assert vd.context == context
    assert [tok.value for tok in vd.children] == ["child"]


def test_P2_VariableDeclaration_BindingIdentifier_init(context):
    vd = e.P2_VariableDeclaration_BindingIdentifier(context, "StrictArg", FakeTokens("child"))
    assert vd.name == "VariableDeclaration"
    assert vd.BindingIdentifier.value == "child"


def test_P2_VariableDeclaration_BindingIdentifier_Initializer_init(context):
    vd = e.P2_VariableDeclaration_BindingIdentifier_Initializer(context, "StrictArg", FakeTokens("child", "init"))
    assert vd.name == "VariableDeclaration"
    assert vd.BindingIdentifier.value == "child"
    assert vd.Initializer.value == "init"


def test_P2_VariableDeclaration_BindingPattern_Initializer_init(context):
    vd = e.P2_VariableDeclaration_BindingPattern_Initializer(context, "StrictArg", FakeTokens("child", "init"))
    assert vd.name == "VariableDeclaration"
    assert vd.BindingPattern.value == "child"
    assert vd.Initializer.value == "init"


#### BindingPattern #################################################################################################
#
# 888888b.   d8b               888 d8b                   8888888b.           888    888
# 888  "88b  Y8P               888 Y8P                   888   Y88b          888    888
# 888  .88P                    888                       888    888          888    888
# 8888888K.  888 88888b.   .d88888 888 88888b.   .d88b.  888   d88P  8888b.  888888 888888  .d88b.  888d888 88888b.
# 888  "Y88b 888 888 "88b d88" 888 888 888 "88b d88P"88b 8888888P"      "88b 888    888    d8P  Y8b 888P"   888 "88b
# 888    888 888 888  888 888  888 888 888  888 888  888 888        .d888888 888    888    88888888 888     888  888
# 888   d88P 888 888  888 Y88b 888 888 888  888 Y88b 888 888        888  888 Y88b.  Y88b.  Y8b.     888     888  888
# 8888888P"  888 888  888  "Y88888 888 888  888  "Y88888 888        "Y888888  "Y888  "Y888  "Y8888  888     888  888
#                                                    888
#                                               Y8b d88P
#                                                "Y88P"
#
#####################################################################################################################
def test_P2_BindingPattern_init(context):
    bp = e.P2_BindingPattern(context, "StrictArg", FakeTokens("child"))
    assert bp.name == "BindingPattern"
    assert bp.context == context
    assert [tok.value for tok in bp.children] == ["child"]


def test_P2_BindingPattern_ObjectBindingPattern_init(context):
    bp = e.P2_BindingPattern_ObjectBindingPattern(context, "StrictArg", FakeTokens("child"))
    assert bp.name == "BindingPattern"
    assert bp.ObjectBindingPattern.value == "child"


def test_P2_BindingPattern_ArrayBindingPattern_init(context):
    bp = e.P2_BindingPattern_ArrayBindingPattern(context, "StrictArg", FakeTokens("child"))
    assert bp.name == "BindingPattern"
    assert bp.ArrayBindingPattern.value == "child"


#### ObjectBindingPattern ################################################################################################################################################
#
#  .d88888b.  888         d8b                   888    888888b.   d8b               888 d8b                   8888888b.           888    888
# d88P" "Y88b 888         Y8P                   888    888  "88b  Y8P               888 Y8P                   888   Y88b          888    888
# 888     888 888                               888    888  .88P                    888                       888    888          888    888
# 888     888 88888b.    8888  .d88b.   .d8888b 888888 8888888K.  888 88888b.   .d88888 888 88888b.   .d88b.  888   d88P  8888b.  888888 888888  .d88b.  888d888 88888b.
# 888     888 888 "88b   "888 d8P  Y8b d88P"    888    888  "Y88b 888 888 "88b d88" 888 888 888 "88b d88P"88b 8888888P"      "88b 888    888    d8P  Y8b 888P"   888 "88b
# 888     888 888  888    888 88888888 888      888    888    888 888 888  888 888  888 888 888  888 888  888 888        .d888888 888    888    88888888 888     888  888
# Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.  888   d88P 888 888  888 Y88b 888 888 888  888 Y88b 888 888        888  888 Y88b.  Y88b.  Y8b.     888     888  888
#  "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888 8888888P"  888 888  888  "Y88888 888 888  888  "Y88888 888        "Y888888  "Y888  "Y888  "Y8888  888     888  888
#                         888                                                                             888
#                        d88P                                                                        Y8b d88P
#                      888P"                                                                          "Y88P"
#
##########################################################################################################################################################################
def test_P2_ObjectBindingPattern_init(context):
    obp = e.P2_ObjectBindingPattern(context, "StrictArg", FakeTokens("child"))
    assert obp.name == "ObjectBindingPattern"
    assert obp.context == context
    assert [tok.value for tok in obp.children] == ["child"]


def test_P2_ObjectBindingPattern_Empty_init(context):
    obp = e.P2_ObjectBindingPattern_Empty(context, "StrictArg", FakeTokens("child"))
    assert obp.name == "ObjectBindingPattern"


def test_P2_ObjectBindingPattern_BindingRestProperty_init(context):
    obp = e.P2_ObjectBindingPattern_BindingRestProperty(context, "StrictArg", FakeTokens("{", "child", "}"))
    assert obp.name == "ObjectBindingPattern"
    assert obp.BindingRestProperty.value == "child"


def test_P2_ObjectBindingPattern_BindingPropertyList_init(context):
    obp = e.P2_ObjectBindingPattern_BindingPropertyList(context, "StrictArg", FakeTokens("{", "child", "}"))
    assert obp.name == "ObjectBindingPattern"
    assert obp.BindingPropertyList.value == "child"


def test_P2_ObjectBindingPattern_BindingPropertyList_BindingRestProperty_init(context):
    obp = e.P2_ObjectBindingPattern_BindingPropertyList_BindingRestProperty(
        context, "StrictArg", FakeTokens("{", "child", ",", "sibling", "}")
    )
    assert obp.name == "ObjectBindingPattern"
    assert obp.BindingPropertyList.value == "child"
    assert obp.BindingRestProperty.value == "sibling"


#### ArrayBindingPattern ###########################################################################################################################################
#
#        d8888                                   888888b.   d8b               888 d8b                   8888888b.           888    888
#       d88888                                   888  "88b  Y8P               888 Y8P                   888   Y88b          888    888
#      d88P888                                   888  .88P                    888                       888    888          888    888
#     d88P 888 888d888 888d888  8888b.  888  888 8888888K.  888 88888b.   .d88888 888 88888b.   .d88b.  888   d88P  8888b.  888888 888888  .d88b.  888d888 88888b.
#    d88P  888 888P"   888P"       "88b 888  888 888  "Y88b 888 888 "88b d88" 888 888 888 "88b d88P"88b 8888888P"      "88b 888    888    d8P  Y8b 888P"   888 "88b
#   d88P   888 888     888     .d888888 888  888 888    888 888 888  888 888  888 888 888  888 888  888 888        .d888888 888    888    88888888 888     888  888
#  d8888888888 888     888     888  888 Y88b 888 888   d88P 888 888  888 Y88b 888 888 888  888 Y88b 888 888        888  888 Y88b.  Y88b.  Y8b.     888     888  888
# d88P     888 888     888     "Y888888  "Y88888 8888888P"  888 888  888  "Y88888 888 888  888  "Y88888 888        "Y888888  "Y888  "Y888  "Y8888  888     888  888
#                                            888                                                    888
#                                       Y8b d88P                                               Y8b d88P
#                                        "Y88P"                                                 "Y88P"
#
####################################################################################################################################################################
def test_P2_ArrayBindingPattern_init(context):
    abp = e.P2_ArrayBindingPattern(context, "StrictArg", FakeTokens("child"))
    assert abp.name == "ArrayBindingPattern"
    assert abp.context == context
    assert [tok.value for tok in abp.children] == ["child"]


def test_P2_ArrayBindingPattern_Empty_init(context):
    abp = e.P2_ArrayBindingPattern_Empty(context, "StrictArg", FakeTokens("child"))
    assert abp.name == "ArrayBindingPattern"


def test_P2_ArrayBindingPattern_Elision_init(context):
    abp = e.P2_ArrayBindingPattern_Elision(context, "StrictArg", FakeTokens("[", "child", "]"))
    assert abp.name == "ArrayBindingPattern"
    assert abp.Elision.value == "child"


def test_P2_ArrayBindingPattern_BindingRestElement_init(context):
    abp = e.P2_ArrayBindingPattern_BindingRestElement(context, "StrictArg", FakeTokens("[", "child", "]"))
    assert abp.name == "ArrayBindingPattern"
    assert abp.BindingRestElement.value == "child"


def test_P2_ArrayBindingPattern_Elision_BindingRestElement_init(context):
    abp = e.P2_ArrayBindingPattern_Elision_BindingRestElement(
        context, "StrictArg", FakeTokens("[", ",,,", "child", "]")
    )
    assert abp.name == "ArrayBindingPattern"
    assert abp.Elision.value == ",,,"
    assert abp.BindingRestElement.value == "child"


def test_P2_ArrayBindingPattern_BindingElementList_init(context):
    abp = e.P2_ArrayBindingPattern_BindingElementList(context, "StrictArg", FakeTokens("[", "child", "]"))
    assert abp.name == "ArrayBindingPattern"
    assert abp.BindingElementList.value == "child"


def test_P2_ArrayBindingPattern_BindingElementList_Elision_init(context):
    abp = e.P2_ArrayBindingPattern_BindingElementList_Elision(
        context, "StrictArg", FakeTokens("[", "child", ",", ",,,", "]")
    )
    assert abp.name == "ArrayBindingPattern"
    assert abp.BindingElementList.value == "child"
    assert abp.Elision.value == ",,,"


def test_P2_ArrayBindingPattern_BindingElementList_BindingRestElement_init(context):
    abp = e.P2_ArrayBindingPattern_BindingElementList_BindingRestElement(
        context, "StrictArg", FakeTokens("[", "child", ",", "...bre", "]")
    )
    assert abp.name == "ArrayBindingPattern"
    assert abp.BindingElementList.value == "child"
    assert abp.BindingRestElement.value == "...bre"


def test_P2_ArrayBindingPattern_BindingElementList_Elision_BindingRestElement_init(context):
    abp = e.P2_ArrayBindingPattern_BindingElementList_Elision_BindingRestElement(
        context, "StrictArg", FakeTokens("[", "child", ",", ",,,", "...bre", "]")
    )
    assert abp.name == "ArrayBindingPattern"
    assert abp.BindingElementList.value == "child"
    assert abp.Elision.value == ",,,"
    assert abp.BindingRestElement.value == "...bre"


#### BindingRestProperty ##########################################################################################################################################
#
# 888888b.   d8b               888 d8b                   8888888b.                    888    8888888b.                                             888
# 888  "88b  Y8P               888 Y8P                   888   Y88b                   888    888   Y88b                                            888
# 888  .88P                    888                       888    888                   888    888    888                                            888
# 8888888K.  888 88888b.   .d88888 888 88888b.   .d88b.  888   d88P  .d88b.  .d8888b  888888 888   d88P 888d888  .d88b.  88888b.   .d88b.  888d888 888888 888  888
# 888  "Y88b 888 888 "88b d88" 888 888 888 "88b d88P"88b 8888888P"  d8P  Y8b 88K      888    8888888P"  888P"   d88""88b 888 "88b d8P  Y8b 888P"   888    888  888
# 888    888 888 888  888 888  888 888 888  888 888  888 888 T88b   88888888 "Y8888b. 888    888        888     888  888 888  888 88888888 888     888    888  888
# 888   d88P 888 888  888 Y88b 888 888 888  888 Y88b 888 888  T88b  Y8b.          X88 Y88b.  888        888     Y88..88P 888 d88P Y8b.     888     Y88b.  Y88b 888
# 8888888P"  888 888  888  "Y88888 888 888  888  "Y88888 888   T88b  "Y8888   88888P'  "Y888 888        888      "Y88P"  88888P"   "Y8888  888      "Y888  "Y88888
#                                                    888                                                                 888                                   888
#                                               Y8b d88P                                                                 888                              Y8b d88P
#                                                "Y88P"                                                                  888                               "Y88P"
#
###################################################################################################################################################################
def test_P2_BindingRestProperty_init(context):
    brp = e.P2_BindingRestProperty(context, "StrictArg", FakeTokens("child"))
    assert brp.name == "BindingRestProperty"
    assert brp.context == context
    assert [tok.value for tok in brp.children] == ["child"]


def test_P2_BindingRestProperty_BindingIdentifier_init(context):
    brp = e.P2_BindingRestProperty_BindingIdentifier(context, "StrictArg", FakeTokens("...", "child"))
    assert brp.name == "BindingRestProperty"
    assert brp.BindingIdentifier.value == "child"


#### BindingPropertyList ###################################################################################################################################
#
# 888888b.   d8b               888 d8b                   8888888b.                                             888             888      d8b          888
# 888  "88b  Y8P               888 Y8P                   888   Y88b                                            888             888      Y8P          888
# 888  .88P                    888                       888    888                                            888             888                   888
# 8888888K.  888 88888b.   .d88888 888 88888b.   .d88b.  888   d88P 888d888  .d88b.  88888b.   .d88b.  888d888 888888 888  888 888      888 .d8888b  888888
# 888  "Y88b 888 888 "88b d88" 888 888 888 "88b d88P"88b 8888888P"  888P"   d88""88b 888 "88b d8P  Y8b 888P"   888    888  888 888      888 88K      888
# 888    888 888 888  888 888  888 888 888  888 888  888 888        888     888  888 888  888 88888888 888     888    888  888 888      888 "Y8888b. 888
# 888   d88P 888 888  888 Y88b 888 888 888  888 Y88b 888 888        888     Y88..88P 888 d88P Y8b.     888     Y88b.  Y88b 888 888      888      X88 Y88b.
# 8888888P"  888 888  888  "Y88888 888 888  888  "Y88888 888        888      "Y88P"  88888P"   "Y8888  888      "Y888  "Y88888 88888888 888  88888P'  "Y888
#                                                    888                             888                                   888
#                                               Y8b d88P                             888                              Y8b d88P
#                                                "Y88P"                              888                               "Y88P"
#
############################################################################################################################################################
def test_P2_BindingPropertyList_init(context):
    bpl = e.P2_BindingPropertyList(context, "StrictArg", FakeTokens("child"))
    assert bpl.name == "BindingPropertyList"
    assert bpl.context == context
    assert [tok.value for tok in bpl.children] == ["child"]


def test_P2_BindingPropertyList_BindingProperty_init(context):
    bpl = e.P2_BindingPropertyList_BindingProperty(context, "StrictArg", FakeTokens("child"))
    assert bpl.name == "BindingPropertyList"
    assert bpl.BindingProperty.value == "child"


def test_P2_BindingPropertyList_BindingPropertyList_BindingProperty_init(context):
    bpl = e.P2_BindingPropertyList_BindingPropertyList_BindingProperty(
        context, "StrictArg", FakeTokens("list", ",", "child")
    )
    assert bpl.name == "BindingPropertyList"
    assert bpl.BindingPropertyList.value == "list"
    assert bpl.BindingProperty.value == "child"


#### BindingElementList #############################################################################################################################
#
# 888888b.   d8b               888 d8b                   8888888888 888                                          888    888      d8b          888
# 888  "88b  Y8P               888 Y8P                   888        888                                          888    888      Y8P          888
# 888  .88P                    888                       888        888                                          888    888                   888
# 8888888K.  888 88888b.   .d88888 888 88888b.   .d88b.  8888888    888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888 888      888 .d8888b  888888
# 888  "Y88b 888 888 "88b d88" 888 888 888 "88b d88P"88b 888        888 d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888    888      888 88K      888
# 888    888 888 888  888 888  888 888 888  888 888  888 888        888 88888888 888  888  888 88888888 888  888 888    888      888 "Y8888b. 888
# 888   d88P 888 888  888 Y88b 888 888 888  888 Y88b 888 888        888 Y8b.     888  888  888 Y8b.     888  888 Y88b.  888      888      X88 Y88b.
# 8888888P"  888 888  888  "Y88888 888 888  888  "Y88888 8888888888 888  "Y8888  888  888  888  "Y8888  888  888  "Y888 88888888 888  88888P'  "Y888
#                                                    888
#                                               Y8b d88P
#                                                "Y88P"
#
#####################################################################################################################################################
def test_P2_BindingElementList_init(context):
    bel = e.P2_BindingElementList(context, "StrictArg", FakeTokens("child"))
    assert bel.name == "BindingElementList"
    assert bel.context == context
    assert [tok.value for tok in bel.children] == ["child"]


def test_P2_BindingElementList_BindingElement_init(context):
    bel = e.P2_BindingElementList_BindingElisionElement(context, "StrictArg", FakeTokens("child"))
    assert bel.name == "BindingElementList"
    assert bel.BindingElisionElement.value == "child"


def test_P2_BindingElementList_BindingElementList_BindingElisionElement_init(context):
    bel = e.P2_BindingElementList_BindingElementList_BindingElisionElement(
        context, "StrictArg", FakeTokens("list", ",", "child")
    )
    assert bel.name == "BindingElementList"
    assert bel.BindingElementList.value == "list"
    assert bel.BindingElisionElement.value == "child"


#### BindingElisionElement ###############################################################################################################################################
#
# 888888b.   d8b               888 d8b                   8888888888 888 d8b          d8b                   8888888888 888                                          888
# 888  "88b  Y8P               888 Y8P                   888        888 Y8P          Y8P                   888        888                                          888
# 888  .88P                    888                       888        888                                    888        888                                          888
# 8888888K.  888 88888b.   .d88888 888 88888b.   .d88b.  8888888    888 888 .d8888b  888  .d88b.  88888b.  8888888    888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
# 888  "Y88b 888 888 "88b d88" 888 888 888 "88b d88P"88b 888        888 888 88K      888 d88""88b 888 "88b 888        888 d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
# 888    888 888 888  888 888  888 888 888  888 888  888 888        888 888 "Y8888b. 888 888  888 888  888 888        888 88888888 888  888  888 88888888 888  888 888
# 888   d88P 888 888  888 Y88b 888 888 888  888 Y88b 888 888        888 888      X88 888 Y88..88P 888  888 888        888 Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 8888888P"  888 888  888  "Y88888 888 888  888  "Y88888 8888888888 888 888  88888P' 888  "Y88P"  888  888 8888888888 888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#                                                    888
#                                               Y8b d88P
#                                                "Y88P"
#
##########################################################################################################################################################################
def test_P2_BindingElisionElement(context):
    bee = e.P2_BindingElisionElement(context, "StrictArg", FakeTokens("child"))
    assert bee.name == "BindingElisionElement"
    assert bee.context == context
    assert [tok.value for tok in bee.children] == ["child"]


def test_P2_BindingElisionElement_BindingElement(context):
    bee = e.P2_BindingElisionElement_BindingElement(context, "StrictArg", FakeTokens("child"))
    assert bee.name == "BindingElisionElement"
    assert bee.BindingElement.value == "child"


def test_P2_BindingElisionElement_Elision_BindingElement(context):
    bee = e.P2_BindingElisionElement_Elision_BindingElement(context, "StrictArg", FakeTokens(",,,", "child"))
    assert bee.name == "BindingElisionElement"
    assert bee.Elision.value == ",,,"
    assert bee.BindingElement.value == "child"


#### BindingProperty ##########################################################################################################
#
# 888888b.   d8b               888 d8b                   8888888b.                                             888
# 888  "88b  Y8P               888 Y8P                   888   Y88b                                            888
# 888  .88P                    888                       888    888                                            888
# 8888888K.  888 88888b.   .d88888 888 88888b.   .d88b.  888   d88P 888d888  .d88b.  88888b.   .d88b.  888d888 888888 888  888
# 888  "Y88b 888 888 "88b d88" 888 888 888 "88b d88P"88b 8888888P"  888P"   d88""88b 888 "88b d8P  Y8b 888P"   888    888  888
# 888    888 888 888  888 888  888 888 888  888 888  888 888        888     888  888 888  888 88888888 888     888    888  888
# 888   d88P 888 888  888 Y88b 888 888 888  888 Y88b 888 888        888     Y88..88P 888 d88P Y8b.     888     Y88b.  Y88b 888
# 8888888P"  888 888  888  "Y88888 888 888  888  "Y88888 888        888      "Y88P"  88888P"   "Y8888  888      "Y888  "Y88888
#                                                    888                             888                                   888
#                                               Y8b d88P                             888                              Y8b d88P
#                                                "Y88P"                              888                               "Y88P"
#
###############################################################################################################################
def test_P2_BindingProperty(context):
    bp = e.P2_BindingProperty(context, "StrictArg", FakeTokens("child"))
    assert bp.name == "BindingProperty"
    assert bp.context == context
    assert [tok.value for tok in bp.children] == ["child"]


def test_P2_BindingProperty_SingleNameBinding(context):
    bp = e.P2_BindingProperty_SingleNameBinding(context, "StrictArg", FakeTokens("child"))
    assert bp.name == "BindingProperty"
    assert bp.SingleNameBinding.value == "child"


def test_P2_BindingProperty_PropertyName_BindingElement(context):
    bp = e.P2_BindingProperty_PropertyName_BindingElement(context, "StrictArg", FakeTokens("prop", ":", "child"))
    assert bp.name == "BindingProperty"
    assert bp.PropertyName.value == "prop"
    assert bp.BindingElement.value == "child"


#### BindingElement ####################################################################################################
#
# 888888b.   d8b               888 d8b                   8888888888 888                                          888
# 888  "88b  Y8P               888 Y8P                   888        888                                          888
# 888  .88P                    888                       888        888                                          888
# 8888888K.  888 88888b.   .d88888 888 88888b.   .d88b.  8888888    888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
# 888  "Y88b 888 888 "88b d88" 888 888 888 "88b d88P"88b 888        888 d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
# 888    888 888 888  888 888  888 888 888  888 888  888 888        888 88888888 888  888  888 88888888 888  888 888
# 888   d88P 888 888  888 Y88b 888 888 888  888 Y88b 888 888        888 Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 8888888P"  888 888  888  "Y88888 888 888  888  "Y88888 8888888888 888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#                                                    888
#                                               Y8b d88P
#                                                "Y88P"
#
########################################################################################################################
def test_P2_BindingElement_init(context):
    be = e.P2_BindingElement(context, "StrictArg", FakeTokens("child"))
    assert be.name == "BindingElement"
    assert be.context == context
    assert [tok.value for tok in be.children] == ["child"]


def test_P2_BindingElement_SingleNameBinding_init(context):
    be = e.P2_BindingElement_SingleNameBinding(context, "StrictArg", FakeTokens("child"))
    assert be.name == "BindingElement"
    assert be.SingleNameBinding.value == "child"


def test_P2_BindingElement_BindingPattern_init(context):
    be = e.P2_BindingElement_BindingPattern(context, "StrictArg", FakeTokens("pattern"))
    assert be.name == "BindingElement"
    assert be.BindingPattern.value == "pattern"


def test_P2_BindingElement_BindingPattern_Initializer_init(context):
    be = e.P2_BindingElement_BindingPattern_Initializer(context, "StrictArg", FakeTokens("pattern", "=child"))
    assert be.name == "BindingElement"
    assert be.BindingPattern.value == "pattern"
    assert be.Initializer.value == "=child"


#### SingleNameBinding ############################################################################################################################
#
#  .d8888b.  d8b                   888          888b    888                                 888888b.   d8b               888 d8b
# d88P  Y88b Y8P                   888          8888b   888                                 888  "88b  Y8P               888 Y8P
# Y88b.                            888          88888b  888                                 888  .88P                    888
#  "Y888b.   888 88888b.   .d88b.  888  .d88b.  888Y88b 888  8888b.  88888b.d88b.   .d88b.  8888888K.  888 88888b.   .d88888 888 88888b.   .d88b.
#     "Y88b. 888 888 "88b d88P"88b 888 d8P  Y8b 888 Y88b888     "88b 888 "888 "88b d8P  Y8b 888  "Y88b 888 888 "88b d88" 888 888 888 "88b d88P"88b
#       "888 888 888  888 888  888 888 88888888 888  Y88888 .d888888 888  888  888 88888888 888    888 888 888  888 888  888 888 888  888 888  888
# Y88b  d88P 888 888  888 Y88b 888 888 Y8b.     888   Y8888 888  888 888  888  888 Y8b.     888   d88P 888 888  888 Y88b 888 888 888  888 Y88b 888
#  "Y8888P"  888 888  888  "Y88888 888  "Y8888  888    Y888 "Y888888 888  888  888  "Y8888  8888888P"  888 888  888  "Y88888 888 888  888  "Y88888
#                              888                                                                                                             888
#                         Y8b d88P                                                                                                        Y8b d88P
#                          "Y88P"                                                                                                          "Y88P"
#
###################################################################################################################################################
class Test_SingleNameBinding:
    def test_P2_SingleNameBinding_init(self, context):
        snb = e.P2_SingleNameBinding(context, "StrictArg", FakeTokens("child"))
        assert snb.name == "SingleNameBinding"
        assert snb.context == context
        assert [tok.value for tok in snb.children] == ["child"]

    def test_P2_SingleNameBinding_BindingIdentifier_init(self, context):
        snb = e.P2_SingleNameBinding_BindingIdentifier(context, "StrictArg", FakeTokens("child"))
        assert snb.name == "SingleNameBinding"
        assert snb.BindingIdentifier.value == "child"

    def test_P2_SingleNameBinding_BindingIdentifier_Initializer_init(self, context):
        snb = e.P2_SingleNameBinding_BindingIdentifier_Initializer(context, "StrictArg", FakeTokens("child", "=3"))
        assert snb.name == "SingleNameBinding"
        assert snb.BindingIdentifier.value == "child"
        assert snb.Initializer.value == "=3"


#### BindingRestElement ####################################################################################################################################
#
# 888888b.   d8b               888 d8b                   8888888b.                    888    8888888888 888                                          888
# 888  "88b  Y8P               888 Y8P                   888   Y88b                   888    888        888                                          888
# 888  .88P                    888                       888    888                   888    888        888                                          888
# 8888888K.  888 88888b.   .d88888 888 88888b.   .d88b.  888   d88P  .d88b.  .d8888b  888888 8888888    888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
# 888  "Y88b 888 888 "88b d88" 888 888 888 "88b d88P"88b 8888888P"  d8P  Y8b 88K      888    888        888 d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
# 888    888 888 888  888 888  888 888 888  888 888  888 888 T88b   88888888 "Y8888b. 888    888        888 88888888 888  888  888 88888888 888  888 888
# 888   d88P 888 888  888 Y88b 888 888 888  888 Y88b 888 888  T88b  Y8b.          X88 Y88b.  888        888 Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 8888888P"  888 888  888  "Y88888 888 888  888  "Y88888 888   T88b  "Y8888   88888P'  "Y888 8888888888 888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#                                                    888
#                                               Y8b d88P
#                                                "Y88P"
#
############################################################################################################################################################
class Test_BindingRestElement:
    @staticmethod
    def test_P2_BindingRestElement_init(context):
        bre = e.P2_BindingRestElement(context, "StrictArg", FakeTokens("child"))
        assert bre.name == "BindingRestElement"
        assert bre.context == context
        assert [tok.value for tok in bre.children] == ["child"]

    @staticmethod
    def test_P2_BindingRestElement_BindingIdentifier_init(context):
        bre = e.P2_BindingRestElement_BindingIdentifier(context, "StrictArg", FakeTokens("...", "child"))
        assert bre.name == "BindingRestElement"
        assert bre.BindingIdentifier.value == "child"

    @staticmethod
    def test_P2_BindingRestElement_BindingPattern_init(context):
        bre = e.P2_BindingRestElement_BindingPattern(context, "StrictArg", FakeTokens("...", "child"))
        assert bre.name == "BindingRestElement"
        assert bre.BindingPattern.value == "child"


#### EmptyStatement ##################################################################################################################
#
# 8888888888                        888              .d8888b.  888             888                                             888
# 888                               888             d88P  Y88b 888             888                                             888
# 888                               888             Y88b.      888             888                                             888
# 8888888    88888b.d88b.  88888b.  888888 888  888  "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
# 888        888 "888 "88b 888 "88b 888    888  888     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
# 888        888  888  888 888  888 888    888  888       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
# 888        888  888  888 888 d88P Y88b.  Y88b 888 Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 8888888888 888  888  888 88888P"   "Y888  "Y88888  "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#                          888                  888
#                          888             Y8b d88P
#                          888              "Y88P"
#
######################################################################################################################################
def test_P2_EmptyStatement_init(context):
    es = e.P2_EmptyStatement(context, "StrictArg", FakeTokens("child"))
    assert es.name == "EmptyStatement"
    assert es.context == context
    assert [tok.value for tok in es.children] == ["child"]


def test_P2_EmptyStatement_SEMICOLON_init(context):
    es = e.P2_EmptyStatement_SEMICOLON(context, "StrictArg", FakeTokens("child"))
    assert es.name == "EmptyStatement"


#### ExpressionStatement #################################################################################################################################################
#
# 8888888888                                                      d8b                    .d8888b.  888             888                                             888
# 888                                                             Y8P                   d88P  Y88b 888             888                                             888
# 888                                                                                   Y88b.      888             888                                             888
# 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.   "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
# 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
# 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
# 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888 Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888  "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#                     888
#                     888
#                     888
#
##########################################################################################################################################################################
def test_P2_ExpressionStatement_init(context):
    es = e.P2_ExpressionStatement(context, "StrictArg", FakeTokens("child"))
    assert es.name == "ExpressionStatement"
    assert es.context == context
    assert [tok.value for tok in es.children] == ["child"]


def test_P2_ExpressionStatement_Expression_SEMICOLON_init(context):
    es = e.P2_ExpressionStatement_Expression_SEMICOLON(context, "StrictArg", FakeTokens("exp_stmt", ";"))
    assert es.name == "ExpressionStatement"
    assert es.Expression.value == "exp_stmt"


#### IfStatement ##################################################################################
#
# 8888888  .d888  .d8888b.  888             888                                             888
#   888   d88P"  d88P  Y88b 888             888                                             888
#   888   888    Y88b.      888             888                                             888
#   888   888888  "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
#   888   888        "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
#   888   888          "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
#   888   888    Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 8888888 888     "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
#
#
#
###################################################################################################
class Test_IfStatement:
    def test_P2_IfStatement_init(self, context):
        ifs = e.P2_IfStatement(context, "StrictArg", FakeTokens("child"))
        assert ifs.name == "IfStatement"
        assert ifs.context == context
        assert [tok.value for tok in ifs.children] == ["child"]

    def test_P2_IfStatement_Expression_Statement_Statement_init(self, context):
        ifs = e.P2_IfStatement_Expression_Statement_Statement(context, "StrictArg", FakeTokens("child"))
        assert ifs.name == "IfStatement"

    def test_P2_IfStatement_Expression_Statement_init(self, context):
        ifs = e.P2_IfStatement_Expression_Statement(context, "StrictArg", FakeTokens("child"))
        assert ifs.name == "IfStatement"


#### IterationStatement ##################################################################################################################################
#
# 8888888 888                              888    d8b                    .d8888b.  888             888                                             888
#   888   888                              888    Y8P                   d88P  Y88b 888             888                                             888
#   888   888                              888                          Y88b.      888             888                                             888
#   888   888888  .d88b.  888d888  8888b.  888888 888  .d88b.  88888b.   "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
#   888   888    d8P  Y8b 888P"       "88b 888    888 d88""88b 888 "88b     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
#   888   888    88888888 888     .d888888 888    888 888  888 888  888       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
#   888   Y88b.  Y8b.     888     888  888 Y88b.  888 Y88..88P 888  888 Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 8888888  "Y888  "Y8888  888     "Y888888  "Y888 888  "Y88P"  888  888  "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
#
#
#
##########################################################################################################################################################
AWAIT_ONLY = 1
ALT_EXPRESSION = 2


class Test_IterationStatement:
    def test_P2_IterationStatement_init(self, context):
        istmt = e.P2_IterationStatement(context, "StrictArg", FakeTokens("child"))
        assert istmt.name == "IterationStatement"
        assert istmt.context == context
        assert [tok.value for tok in istmt.children] == ["child"]
        assert istmt.strict == "StrictArg"

    def test_P2_IterationStatement_DO_Statement_WHILE_Expression_init(self, context):
        istmt = e.P2_IterationStatement_DO_Statement_WHILE_Expression(
            context, "StrictArg", FakeTokens("do", "Statement", "while", "(", "Expression", ")", ";")
        )
        assert istmt.name == "IterationStatement"
        assert istmt.Statement.value == "Statement"
        assert istmt.Expression.value == "Expression"

    def test_P2_IterationStatement_WHILE_Expression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_WHILE_Expression_Statement(
            context, "StrictArg", FakeTokens("while", "(", "Expression", ")", "Statement")
        )
        assert istmt.name == "IterationStatement"
        assert istmt.Expression.value == "Expression"
        assert istmt.Statement.value == "Statement"

    def test_P2_IterationStatement_FOR_ExpressionInit_ExpressionTest_ExpressionInc_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_ExpressionInit_ExpressionTest_ExpressionInc_Statement(
            context, "StrictArg", FakeTokens("for", "(", "ExpInit", ";", "ExpTest", ";", "ExpInc", ")", "Statement")
        )
        assert istmt.name == "IterationStatement"
        assert istmt.ExpressionInit.value == "ExpInit"
        assert istmt.ExpressionTest.value == "ExpTest"
        assert istmt.ExpressionInc.value == "ExpInc"
        assert istmt.Statement.value == "Statement"

    def test_P2_IterationStatement_FOR_VAR_VariableDeclarationList_ExpressionTest_ExpressionInc_Statement_init(
        self, context
    ):
        istmt = e.P2_IterationStatement_FOR_VAR_VariableDeclarationList_ExpressionTest_ExpressionInc_Statement(
            context,
            "StrictArg",
            FakeTokens(
                "for",
                "(",
                "var",
                "VariableDeclarationList",
                ";",
                "Expression_Test",
                ";",
                "Expression_Increment",
                ")",
                "Statement",
            ),
        )
        assert istmt.name == "IterationStatement"
        assert istmt.VariableDeclarationList.value == "VariableDeclarationList"
        assert istmt.ExpressionTest.value == "Expression_Test"
        assert istmt.ExpressionInc.value == "Expression_Increment"
        assert istmt.Statement.value == "Statement"

    def test_P2_IterationStatement_FOR_LexicalDeclaration_ExpressionTest_ExpressionInc_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_LexicalDeclaration_ExpressionTest_ExpressionInc_Statement(
            context,
            "StrictArg",
            FakeTokens(
                "for", "(", "LexicalDeclaration", "Expression_Test", ";", "Expression_Increment", ")", "Statement"
            ),
        )
        assert istmt.name == "IterationStatement"
        assert istmt.LexicalDeclaration.value == "LexicalDeclaration"
        assert istmt.ExpressionTest.value == "Expression_Test"
        assert istmt.ExpressionInc.value == "Expression_Increment"
        assert istmt.Statement.value == "Statement"

    def test_P2_IterationStatement_FOR_LeftHandSideExpression_IN_Expression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_LeftHandSideExpression_IN_Expression_Statement(
            context,
            "StrictArg",
            FakeTokens("for", "(", "LeftHandSideExpression", "in", "Expression", ")", "Statement"),
        )
        assert istmt.name == "IterationStatement"
        assert istmt.LeftHandSideExpression.value == "LeftHandSideExpression"
        assert istmt.Expression.value == "Expression"
        assert istmt.Statement.value == "Statement"

    def test_P2_IterationStatement_FOR_VAR_ForBinding_IN_Expression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_VAR_ForBinding_IN_Expression_Statement(
            context, "StrictArg", FakeTokens("for", "(", "var", "ForBinding", "in", "Expression", ")", "Statement")
        )
        assert istmt.name == "IterationStatement"
        assert istmt.ForBinding.value == "ForBinding"
        assert istmt.Expression.value == "Expression"
        assert istmt.Statement.value == "Statement"

    def test_P2_IterationStatement_FOR_ForDeclaration_IN_Expression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_ForDeclaration_IN_Expression_Statement(
            context, "StrictArg", FakeTokens("for", "(", "ForDeclaration", "in", "Expression", ")", "Statement")
        )
        assert istmt.name == "IterationStatement"
        assert istmt.ForDeclaration.value == "ForDeclaration"
        assert istmt.Expression.value == "Expression"
        assert istmt.Statement.value == "Statement"

    def test_P2_IterationStatement_FOR_LeftHandSideExpression_OF_AssignmentExpression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_LeftHandSideExpression_OF_AssignmentExpression_Statement(
            context,
            "StrictArg",
            FakeTokens("for", "(", "LeftHandSideExpression", "of", "AssignmentExpression", ")", "Statement"),
        )
        assert istmt.name == "IterationStatement"
        assert istmt.LeftHandSideExpression.value == "LeftHandSideExpression"
        assert istmt.AssignmentExpression.value == "AssignmentExpression"
        assert istmt.Statement.value == "Statement"

    def test_P2_IterationStatement_FOR_VAR_ForBinding_OF_AssignmentExpression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_VAR_ForBinding_OF_AssignmentExpression_Statement(
            context,
            "StrictArg",
            FakeTokens("for", "(", "var", "ForBinding", "of", "AssignmentExpression", ")", "Statement"),
        )
        assert istmt.name == "IterationStatement"
        assert istmt.ForBinding.value == "ForBinding"
        assert istmt.AssignmentExpression.value == "AssignmentExpression"
        assert istmt.Statement.value == "Statement"

    def test_P2_IterationStatement_FOR_ForDeclaration_OF_AssignmentExpression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_ForDeclaration_OF_AssignmentExpression_Statement(
            context,
            "StrictArg",
            FakeTokens("for", "(", "ForDeclaration", "of", "AssignmentExpression", ")", "Statement"),
        )
        assert istmt.name == "IterationStatement"
        assert istmt.ForDeclaration.value == "ForDeclaration"
        assert istmt.AssignmentExpression.value == "AssignmentExpression"
        assert istmt.Statement.value == "Statement"

    def test_P2_IterationStatement_FOR_AWAIT_LeftHandSideExpression_OF_AssignmentExpression_Statement_init(
        self, context
    ):
        istmt = e.P2_IterationStatement_FOR_AWAIT_LeftHandSideExpression_OF_AssignmentExpression_Statement(
            context,
            "StrictArg",
            FakeTokens(
                "for", "await", "(", "LeftHandSideExpression", "of", "AssignmentExpression", ")", "Statement"
            ),
        )
        assert istmt.name == "IterationStatement"
        assert istmt.LeftHandSideExpression.value == "LeftHandSideExpression"
        assert istmt.AssignmentExpression.value == "AssignmentExpression"
        assert istmt.Statement.value == "Statement"

    def test_P2_IterationStatement_FOR_AWAIT_VAR_ForBinding_OF_AssignmentExpression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_AWAIT_VAR_ForBinding_OF_AssignmentExpression_Statement(
            context,
            "StrictArg",
            FakeTokens("for", "await", "(", "var", "ForBinding", "of", "AssignmentExpression", ")", "Statement"),
        )
        assert istmt.name == "IterationStatement"
        assert istmt.ForBinding.value == "ForBinding"
        assert istmt.AssignmentExpression.value == "AssignmentExpression"
        assert istmt.Statement.value == "Statement"

    def test_P2_IterationStatement_FOR_AWAIT_ForDeclaration_OF_AssignmentExpression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_AWAIT_ForDeclaration_OF_AssignmentExpression_Statement(
            context,
            "StrictArg",
            FakeTokens("for", "await", "(", "ForDeclaration", "of", "AssignmentExpression", ")", "Statement"),
        )
        assert istmt.name == "IterationStatement"
        assert istmt.ForDeclaration.value == "ForDeclaration"
        assert istmt.AssignmentExpression.value == "AssignmentExpression"
        assert istmt.Statement.value == "Statement"


#### ForDeclaration ##################################################################################################
#
# 8888888888                  8888888b.                    888                           888    d8b
# 888                         888  "Y88b                   888                           888    Y8P
# 888                         888    888                   888                           888
# 8888888     .d88b.  888d888 888    888  .d88b.   .d8888b 888  8888b.  888d888  8888b.  888888 888  .d88b.  88888b.
# 888        d88""88b 888P"   888    888 d8P  Y8b d88P"    888     "88b 888P"       "88b 888    888 d88""88b 888 "88b
# 888        888  888 888     888    888 88888888 888      888 .d888888 888     .d888888 888    888 888  888 888  888
# 888        Y88..88P 888     888  .d88P Y8b.     Y88b.    888 888  888 888     888  888 Y88b.  888 Y88..88P 888  888
# 888         "Y88P"  888     8888888P"   "Y8888   "Y8888P 888 "Y888888 888     "Y888888  "Y888 888  "Y88P"  888  888
#
#
#
#
######################################################################################################################
class Test_ForDeclaration:
    def test_P2_ForDeclaration_init(self, context):
        fd = e.P2_ForDeclaration(context, "StrictArg", FakeTokens("child"))
        assert fd.name == "ForDeclaration"
        assert fd.context == context
        assert [tok.value for tok in fd.children] == ["child"]

    def test_P2_ForDeclaration_LetOrConst_ForBinding_init(self, context):
        fd = e.P2_ForDeclaration_LetOrConst_ForBinding(context, "StrictArg", FakeTokens("loc", "forbinding"))
        assert fd.name == "ForDeclaration"
        assert fd.LetOrConst.value == "loc"
        assert fd.ForBinding.value == "forbinding"


#### ForBinding #####################################################################
#
# 8888888888                  888888b.   d8b               888 d8b
# 888                         888  "88b  Y8P               888 Y8P
# 888                         888  .88P                    888
# 8888888     .d88b.  888d888 8888888K.  888 88888b.   .d88888 888 88888b.   .d88b.
# 888        d88""88b 888P"   888  "Y88b 888 888 "88b d88" 888 888 888 "88b d88P"88b
# 888        888  888 888     888    888 888 888  888 888  888 888 888  888 888  888
# 888        Y88..88P 888     888   d88P 888 888  888 Y88b 888 888 888  888 Y88b 888
# 888         "Y88P"  888     8888888P"  888 888  888  "Y88888 888 888  888  "Y88888
#                                                                                888
#                                                                           Y8b d88P
#                                                                            "Y88P"
#
#####################################################################################
class Test_ForBinding:
    def test_P2_ForBinding_init(self, context):
        fb = e.P2_ForBinding(context, "StrictArg", FakeTokens("child"))
        assert fb.name == "ForBinding"
        assert fb.context == context
        assert [tok.value for tok in fb.children] == ["child"]

    def test_P2_ForBinding_BindingIdentifier_init(self, context):
        fb = e.P2_ForBinding_BindingIdentifier(context, "StrictArg", FakeTokens("child"))
        assert fb.name == "ForBinding"
        assert fb.BindingIdentifier.value == "child"

    def test_P2_ForBinding_BindingPattern_init(self, context):
        fb = e.P2_ForBinding_BindingPattern(context, "StrictArg", FakeTokens("child"))
        assert fb.name == "ForBinding"
        assert fb.BindingPattern.value == "child"


#### ContinueStatement ################################################################################################################################
#
#  .d8888b.                    888    d8b                             .d8888b.  888             888                                             888
# d88P  Y88b                   888    Y8P                            d88P  Y88b 888             888                                             888
# 888    888                   888                                   Y88b.      888             888                                             888
# 888         .d88b.  88888b.  888888 888 88888b.  888  888  .d88b.   "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
# 888        d88""88b 888 "88b 888    888 888 "88b 888  888 d8P  Y8b     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
# 888    888 888  888 888  888 888    888 888  888 888  888 88888888       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
# Y88b  d88P Y88..88P 888  888 Y88b.  888 888  888 Y88b 888 Y8b.     Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
#  "Y8888P"   "Y88P"  888  888  "Y888 888 888  888  "Y88888  "Y8888   "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
#
#
#
#######################################################################################################################################################
class Test_ContinueStatement:
    def test_P2_ContinueStatement_init(self, context):
        cs = e.P2_ContinueStatement(context, "StrictArg", FakeTokens("child"))
        assert cs.name == "ContinueStatement"
        assert cs.context == context
        assert [tok.value for tok in cs.children] == ["child"]

    def test_P2_ContinueStatement_CONTINUE_init(self, context):
        cs = e.P2_ContinueStatement_CONTINUE(context, "StrictArg", FakeTokens("continue", ";"))
        assert cs.name == "ContinueStatement"

    def test_P2_ContinueStatement_CONTINUE_LabelIdentifier_init(self, context):
        cs = e.P2_ContinueStatement_CONTINUE_LabelIdentifier(
            context, "StrictArg", FakeTokens("continue", "superduper", ";")
        )
        assert cs.name == "ContinueStatement"
        assert cs.LabelIdentifier.value == "superduper"


#### BreakStatement ##############################################################################################################
#
# 888888b.                             888       .d8888b.  888             888                                             888
# 888  "88b                            888      d88P  Y88b 888             888                                             888
# 888  .88P                            888      Y88b.      888             888                                             888
# 8888888K.  888d888  .d88b.   8888b.  888  888  "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
# 888  "Y88b 888P"   d8P  Y8b     "88b 888 .88P     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
# 888    888 888     88888888 .d888888 888888K        "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
# 888   d88P 888     Y8b.     888  888 888 "88b Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 8888888P"  888      "Y8888  "Y888888 888  888  "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
#
#
#
##################################################################################################################################
class Test_BreakStatement:
    def test_P2_BreakStatement_init(self, context):
        bs = e.P2_BreakStatement(context, "StrictArg", FakeTokens("child"))
        assert bs.name == "BreakStatement"
        assert bs.context == context
        assert [tok.value for tok in bs.children] == ["child"]

    def test_P2_BreakStatement_BREAK_LabelIdentifier_init(self, context):
        bs = e.P2_BreakStatement_BREAK_LabelIdentifier(context, "StrictArg", FakeTokens("continue", "ident", ";"))
        assert bs.name == "BreakStatement"
        assert bs.LabelIdentifier.value == "ident"

    def test_P2_BreakStatement_BREAK_init(self, context):
        bs = e.P2_BreakStatement_BREAK(context, "StrictArg", FakeTokens("break", ";"))
        assert bs.name == "BreakStatement"


#### ReturnStatement ####################################################################################################################
#
# 8888888b.           888                               .d8888b.  888             888                                             888
# 888   Y88b          888                              d88P  Y88b 888             888                                             888
# 888    888          888                              Y88b.      888             888                                             888
# 888   d88P  .d88b.  888888 888  888 888d888 88888b.   "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
# 8888888P"  d8P  Y8b 888    888  888 888P"   888 "88b     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
# 888 T88b   88888888 888    888  888 888     888  888       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
# 888  T88b  Y8b.     Y88b.  Y88b 888 888     888  888 Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 888   T88b  "Y8888   "Y888  "Y88888 888     888  888  "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
#
#
#
#########################################################################################################################################
class Test_ReturnStatement:
    def test_P2_ReturnStatement_init(self, context):
        rs = e.P2_ReturnStatement(context, "StrictArg", FakeTokens("child"))
        assert rs.name == "ReturnStatement"
        assert rs.context == context
        assert [tok.value for tok in rs.children] == ["child"]

    def test_P2_ReturnStatement_RETURN_init(self, context):
        rs = e.P2_ReturnStatement_RETURN(context, "StrictArg", FakeTokens("return", ";"))
        assert rs.name == "ReturnStatement"

    def test_P2_ReturnStatement_RETURN_Expression_init(self, context):
        rs = e.P2_ReturnStatement_RETURN_Expression(context, "StrictArg", FakeTokens("return", "Expression", ";"))
        assert rs.name == "ReturnStatement"
        assert rs.Expression.value == "Expression"


#### WithStatement ###################################################################################################
#
# 888       888 d8b 888    888       .d8888b.  888             888                                             888
# 888   o   888 Y8P 888    888      d88P  Y88b 888             888                                             888
# 888  d8b  888     888    888      Y88b.      888             888                                             888
# 888 d888b 888 888 888888 88888b.   "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
# 888d88888b888 888 888    888 "88b     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
# 88888P Y88888 888 888    888  888       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
# 8888P   Y8888 888 Y88b.  888  888 Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 888P     Y888 888  "Y888 888  888  "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
#
#
#
######################################################################################################################
class Test_WithStatement:
    def test_P2_WithStatement_init(self, context):
        ws = e.P2_WithStatement(context, "StrictArg", FakeTokens("child"))
        assert ws.name == "WithStatement"
        assert ws.context == context
        assert [tok.value for tok in ws.children] == ["child"]

    def test_P2_WithStatement_WITH_Expression_Statement_init(self, context):
        ws = e.P2_WithStatement_WITH_Expression_Statement(
            context, "StrictArg", FakeTokens("with", "(", "Expression", ")", "Statement")
        )
        assert ws.name == "WithStatement"
        assert ws.Expression.value == "Expression"
        assert ws.Statement.value == "Statement"


#### SwitchStatement #####################################################################################################################
#
#  .d8888b.                d8b 888             888       .d8888b.  888             888                                             888
# d88P  Y88b               Y8P 888             888      d88P  Y88b 888             888                                             888
# Y88b.                        888             888      Y88b.      888             888                                             888
#  "Y888b.   888  888  888 888 888888  .d8888b 88888b.   "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
#     "Y88b. 888  888  888 888 888    d88P"    888 "88b     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
#       "888 888  888  888 888 888    888      888  888       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
# Y88b  d88P Y88b 888 d88P 888 Y88b.  Y88b.    888  888 Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
#  "Y8888P"   "Y8888888P"  888  "Y888  "Y8888P 888  888  "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
#
#
#
##########################################################################################################################################
class Test_SwitchStatement:
    def test_P2_SwitchStatement_init(self, context):
        ss = e.P2_SwitchStatement(context, "StrictArg", FakeTokens("child"))
        assert ss.name == "SwitchStatement"
        assert ss.context == context
        assert [tok.value for tok in ss.children] == ["child"]

    def test_P2_SwitchStatement_SWITCH_Expression_CaseBlock_init(self, context):
        ss = e.P2_SwitchStatement_SWITCH_Expression_CaseBlock(
            context, "StrictArg", FakeTokens("switch", "(", "Expression", ")", "CaseBlock")
        )
        assert ss.name == "SwitchStatement"
        assert ss.Expression.value == "Expression"
        assert ss.CaseBlock.value == "CaseBlock"


#### CaseBlock ###################################################################
#
#  .d8888b.                             888888b.   888                   888
# d88P  Y88b                            888  "88b  888                   888
# 888    888                            888  .88P  888                   888
# 888         8888b.  .d8888b   .d88b.  8888888K.  888  .d88b.   .d8888b 888  888
# 888            "88b 88K      d8P  Y8b 888  "Y88b 888 d88""88b d88P"    888 .88P
# 888    888 .d888888 "Y8888b. 88888888 888    888 888 888  888 888      888888K
# Y88b  d88P 888  888      X88 Y8b.     888   d88P 888 Y88..88P Y88b.    888 "88b
#  "Y8888P"  "Y888888  88888P'  "Y8888  8888888P"  888  "Y88P"   "Y8888P 888  888
#
#
#
#
##################################################################################
class Test_CaseBlock:
    def test_P2_CaseBlock_init(self, context):
        cb = e.P2_CaseBlock(context, "StrictArg", FakeTokens("child"))
        assert cb.name == "CaseBlock"
        assert cb.context == context
        assert [tok.value for tok in cb.children] == ["child"]

    def test_P2_CaseBlock_EMPTY_init(self, context):
        cb = e.P2_CaseBlock_EMPTY(context, "StrictArg", FakeTokens("{", "}"))
        assert cb.name == "CaseBlock"
        assert cb.DefaultClause is None
        assert cb.CaseClausesBefore is None
        assert cb.CaseClausesAfter is None

    def test_P2_CaseBlock_DefaultClause_init(self, context):
        cb = e.P2_CaseBlock_DefaultClause(context, "StrictArg", FakeTokens("{", "default", "}"))
        assert cb.name == "CaseBlock"
        assert cb.DefaultClause.value == "default"
        assert cb.CaseClausesAfter is None
        assert cb.CaseClausesBefore is None

    def test_P2_CaseBlock_CaseClauses_init(self, context):
        cb = e.P2_CaseBlock_CaseClauses(context, "StrictArg", FakeTokens("{", "caseclauses", "}"))
        assert cb.name == "CaseBlock"
        assert cb.DefaultClause is None
        assert cb.CaseClausesBefore.value == "caseclauses"
        assert cb.CaseClausesAfter is None

    def test_P2_CaseBlock_CaseClauses_DefaultClause_init(self, context):
        cb = e.P2_CaseBlock_CaseClauses_DefaultClause(
            context, "StrictArg", FakeTokens("{", "caseclauses", "default", "}")
        )
        assert cb.name == "CaseBlock"
        assert cb.CaseClausesBefore.value == "caseclauses"
        assert cb.DefaultClause.value == "default"
        assert cb.CaseClausesAfter is None

    def test_P2_CaseBlock_DefaultClause_CaseClauses_init(self, context):
        cb = e.P2_CaseBlock_DefaultClause_CaseClauses(
            context, "StrictArg", FakeTokens("{", "default", "caseclauses", "}")
        )
        assert cb.name == "CaseBlock"
        assert cb.CaseClausesBefore is None
        assert cb.DefaultClause.value == "default"
        assert cb.CaseClausesAfter.value == "caseclauses"

    def test_P2_CaseBlock_CaseClauses_DefaultClause_CaseClauses_init(self, context):
        cb = e.P2_CaseBlock_CaseClauses_DefaultClause_CaseClauses(
            context, "StrictArg", FakeTokens("{", "before", "default", "after", "}")
        )
        assert cb.name == "CaseBlock"
        assert cb.CaseClausesBefore.value == "before"
        assert cb.DefaultClause.value == "default"
        assert cb.CaseClausesAfter.value == "after"


#### CaseClauses ###################################################################################
#
#  .d8888b.                              .d8888b.  888
# d88P  Y88b                            d88P  Y88b 888
# 888    888                            888    888 888
# 888         8888b.  .d8888b   .d88b.  888        888  8888b.  888  888 .d8888b   .d88b.  .d8888b
# 888            "88b 88K      d8P  Y8b 888        888     "88b 888  888 88K      d8P  Y8b 88K
# 888    888 .d888888 "Y8888b. 88888888 888    888 888 .d888888 888  888 "Y8888b. 88888888 "Y8888b.
# Y88b  d88P 888  888      X88 Y8b.     Y88b  d88P 888 888  888 Y88b 888      X88 Y8b.          X88
#  "Y8888P"  "Y888888  88888P'  "Y8888   "Y8888P"  888 "Y888888  "Y88888  88888P'  "Y8888   88888P'
#
#
#
#
####################################################################################################
class Test_CaseClauses:
    def test_P2_CaseClauses_init(self, context):
        cc = e.P2_CaseClauses(context, "StrictArg", FakeTokens("child"))
        assert cc.name == "CaseClauses"
        assert cc.context == context
        assert [tok.value for tok in cc.children] == ["child"]

    def test_P2_CaseClauses_CaseClause_init(self, context):
        cc = e.P2_CaseClauses_CaseClause(context, "StrictArg", FakeTokens("caseclause"))
        assert cc.name == "CaseClauses"
        assert cc.CaseClause.value == "caseclause"

    def test_P2_CaseClauses_CaseClauses_CaseClause_init(self, context):
        cc = e.P2_CaseClauses_CaseClauses_CaseClause(context, "StrictArg", FakeTokens("clauses", "clause"))
        assert cc.name == "CaseClauses"
        assert cc.CaseClauses.value == "clauses"
        assert cc.CaseClause.value == "clause"


#### CaseClause ###########################################################################
#
#  .d8888b.                              .d8888b.  888
# d88P  Y88b                            d88P  Y88b 888
# 888    888                            888    888 888
# 888         8888b.  .d8888b   .d88b.  888        888  8888b.  888  888 .d8888b   .d88b.
# 888            "88b 88K      d8P  Y8b 888        888     "88b 888  888 88K      d8P  Y8b
# 888    888 .d888888 "Y8888b. 88888888 888    888 888 .d888888 888  888 "Y8888b. 88888888
# Y88b  d88P 888  888      X88 Y8b.     Y88b  d88P 888 888  888 Y88b 888      X88 Y8b.
#  "Y8888P"  "Y888888  88888P'  "Y8888   "Y8888P"  888 "Y888888  "Y88888  88888P'  "Y8888
#
#
#
#
###########################################################################################
class Test_CaseClause:
    def test_P2_CaseClause_init(self, context):
        cc = e.P2_CaseClause(context, "StrictArg", FakeTokens("child"))
        assert cc.name == "CaseClause"
        assert cc.context == context
        assert [tok.value for tok in cc.children] == ["child"]

    def test_P2_CaseClause_CASE_Expression_StatementList_init(self, context):
        cc = e.P2_CaseClause_CASE_Expression_StatementList(
            context, "StrictArg", FakeTokens("case", "Exp", ":", "Stmts")
        )
        assert cc.name == "CaseClause"
        assert cc.Expression.value == "Exp"
        assert cc.StatementList.value == "Stmts"

    def test_P2_CaseClause_CASE_Expression_init(self, context):
        cc = e.P2_CaseClause_CASE_Expression(context, "StrictArg", FakeTokens("case", "Exp", ":"))
        assert cc.name == "CaseClause"
        assert cc.Expression.value == "Exp"


#### DefaultClause ##########################################################################################
#
# 8888888b.            .d888                   888 888     .d8888b.  888
# 888  "Y88b          d88P"                    888 888    d88P  Y88b 888
# 888    888          888                      888 888    888    888 888
# 888    888  .d88b.  888888  8888b.  888  888 888 888888 888        888  8888b.  888  888 .d8888b   .d88b.
# 888    888 d8P  Y8b 888        "88b 888  888 888 888    888        888     "88b 888  888 88K      d8P  Y8b
# 888    888 88888888 888    .d888888 888  888 888 888    888    888 888 .d888888 888  888 "Y8888b. 88888888
# 888  .d88P Y8b.     888    888  888 Y88b 888 888 Y88b.  Y88b  d88P 888 888  888 Y88b 888      X88 Y8b.
# 8888888P"   "Y8888  888    "Y888888  "Y88888 888  "Y888  "Y8888P"  888 "Y888888  "Y88888  88888P'  "Y8888
#
#
#
#
#############################################################################################################
class Test_DefaultClause:
    def test_P2_DefaultClause_init(self, context):
        dc = e.P2_DefaultClause(context, "StrictArg", FakeTokens("child"))
        assert dc.name == "DefaultClause"
        assert dc.context == context
        assert [tok.value for tok in dc.children] == ["child"]

    def test_P2_DefaultClause_DEFAULT_init(self, context):
        dc = e.P2_DefaultClause_DEFAULT(context, "StrictArg", FakeTokens("default", ":"))
        assert dc.name == "DefaultClause"
        assert dc.StatementList is None

    def test_P2_DefaultClause_DEFAULT_StatementList_init(self, context):
        dc = e.P2_DefaultClause_DEFAULT_StatementList(context, "StrictArg", FakeTokens("default", ":", "Stmts"))
        assert dc.name == "DefaultClause"
        assert dc.StatementList.value == "Stmts"


#### LabelledStatement ###########################################################################################################################
#
# 888               888               888 888               888  .d8888b.  888             888                                             888
# 888               888               888 888               888 d88P  Y88b 888             888                                             888
# 888               888               888 888               888 Y88b.      888             888                                             888
# 888       8888b.  88888b.   .d88b.  888 888  .d88b.   .d88888  "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
# 888          "88b 888 "88b d8P  Y8b 888 888 d8P  Y8b d88" 888     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
# 888      .d888888 888  888 88888888 888 888 88888888 888  888       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
# 888      888  888 888 d88P Y8b.     888 888 Y8b.     Y88b 888 Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 88888888 "Y888888 88888P"   "Y8888  888 888  "Y8888   "Y88888  "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
#
#
#
##################################################################################################################################################
class Test_LabelledStatement:
    def test_P2_LabelledStatement_init(self, context):
        ls = e.P2_LabelledStatement(context, "StrictArg", FakeTokens("child"))
        assert ls.name == "LabelledStatement"
        assert ls.context == context
        assert [tok.value for tok in ls.children] == ["child"]

    def test_P2_LabelledStatement_LabelIdentifier_LabelledItem_init(self, context):
        ls = e.P2_LabelledStatement_LabelIdentifier_LabelledItem(context, "StrictArg", FakeTokens("id", ":", "item"))
        assert ls.name == "LabelledStatement"
        assert ls.LabelIdentifier.value == "id"
        assert ls.LabelledItem.value == "item"


#### LabelledItem ####################################################################################
#
# 888               888               888 888               888 8888888 888
# 888               888               888 888               888   888   888
# 888               888               888 888               888   888   888
# 888       8888b.  88888b.   .d88b.  888 888  .d88b.   .d88888   888   888888  .d88b.  88888b.d88b.
# 888          "88b 888 "88b d8P  Y8b 888 888 d8P  Y8b d88" 888   888   888    d8P  Y8b 888 "888 "88b
# 888      .d888888 888  888 88888888 888 888 88888888 888  888   888   888    88888888 888  888  888
# 888      888  888 888 d88P Y8b.     888 888 Y8b.     Y88b 888   888   Y88b.  Y8b.     888  888  888
# 88888888 "Y888888 88888P"   "Y8888  888 888  "Y8888   "Y88888 8888888  "Y888  "Y8888  888  888  888
#
#
#
#
######################################################################################################
class Test_LabelledItem:
    def test_P2_LabelledItem_init(self, context):
        li = e.P2_LabelledItem(context, "StrictArg", FakeTokens("child"))
        assert li.name == "LabelledItem"
        assert li.context == context
        assert [tok.value for tok in li.children] == ["child"]

    def test_P2_LabelledItem_Statement_init(self, context):
        li = e.P2_LabelledItem_Statement(context, "StrictArg", FakeTokens("child"))
        assert li.name == "LabelledItem"
        assert li.Statement.value == "child"

    def test_P2_LabelledItem_FunctionDeclaration_init(self, context):
        li = e.P2_LabelledItem_FunctionDeclaration(context, "StrictArg", FakeTokens("child"))
        assert li.name == "LabelledItem"
        assert li.FunctionDeclaration.value == "child"


#### ThrowStatement ####################################################################################################################
#
# 88888888888 888                                      .d8888b.  888             888                                             888
#     888     888                                     d88P  Y88b 888             888                                             888
#     888     888                                     Y88b.      888             888                                             888
#     888     88888b.  888d888  .d88b.  888  888  888  "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
#     888     888 "88b 888P"   d88""88b 888  888  888     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
#     888     888  888 888     888  888 888  888  888       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
#     888     888  888 888     Y88..88P Y88b 888 d88P Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
#     888     888  888 888      "Y88P"   "Y8888888P"   "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
#
#
#
########################################################################################################################################
class Test_ThrowStatement:
    def P2_ThrowStatement_init(self, context):
        ts = e.P2_ThrowStatement(context, "StrictArg", FakeTokens("child"))
        assert ts.name == "ThrowStatement"
        assert ts.context == context
        assert [tok.value for tok in ts.children] == ["child"]

    def P2_ThrowStatement_THROW_Expression_init(self, context):
        ts = e.P2_ThrowStatement_THROW_Expression(context, "StrictArg", FakeTokens("throw", "exp", ";"))
        assert ts.name == "ThrowStatement"
        assert ts.Expression.value == "exp"


#### TryStatement ###############################################################################################
#
# 88888888888                   .d8888b.  888             888                                             888
#     888                      d88P  Y88b 888             888                                             888
#     888                      Y88b.      888             888                                             888
#     888     888d888 888  888  "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
#     888     888P"   888  888     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
#     888     888     888  888       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
#     888     888     Y88b 888 Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
#     888     888      "Y88888  "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#                          888
#                     Y8b d88P
#                      "Y88P"
#
#################################################################################################################


class Test_TryStatement:
    def test_P2_TryStatement_init(self, context):
        ts = e.P2_TryStatement(context, "StrictArg", FakeTokens("child"))
        assert ts.name == "TryStatement"
        assert ts.context == context
        assert [tok.value for tok in ts.children] == ["child"]

    def test_P2_TryStatement_TRY_Block_Catch_init(self, context):
        ts = e.P2_TryStatement_TRY_Block_Catch(context, "StrictArg", FakeTokens("try", "block", "catch"))
        assert ts.name == "TryStatement"
        assert ts.Block.value == "block"
        assert ts.Catch.value == "catch"
        assert ts.Finally is None

    def test_P2_TryStatement_TRY_Block_Finally_init(self, context):
        ts = e.P2_TryStatement_TRY_Block_Finally(context, "StrictArg", FakeTokens("try", "block", "finally"))
        assert ts.name == "TryStatement"
        assert ts.Block.value == "block"
        assert ts.Catch is None
        assert ts.Finally.value == "finally"

    def test_P2_TryStatement_TRY_Block_Catch_Finally_init(self, context):
        ts = e.P2_TryStatement_TRY_Block_Catch_Finally(
            context, "StrictArg", FakeTokens("try", "block", "catch", "finally")
        )
        assert ts.name == "TryStatement"
        assert ts.Block.value == "block"
        assert ts.Catch.value == "catch"
        assert ts.Finally.value == "finally"


#### Catch ####################################
#
#  .d8888b.           888             888
# d88P  Y88b          888             888
# 888    888          888             888
# 888         8888b.  888888  .d8888b 88888b.
# 888            "88b 888    d88P"    888 "88b
# 888    888 .d888888 888    888      888  888
# Y88b  d88P 888  888 Y88b.  Y88b.    888  888
#  "Y8888P"  "Y888888  "Y888  "Y8888P 888  888
#
#
#
#
###############################################
class Test_Catch:
    def test_P2_Catch_init(self, context):
        cat = e.P2_Catch(context, "StrictArg", FakeTokens("child"))
        assert cat.name == "Catch"
        assert cat.context == context
        assert [tok.value for tok in cat.children] == ["child"]

    def test_P2_Catch_CATCH_CatchParameter_Block_init(self, context):
        cat = e.P2_Catch_CATCH_CatchParameter_Block(
            context, "StrictArg", FakeTokens("catch", "(", "param", ")", "block")
        )
        assert cat.name == "Catch"
        assert cat.CatchParameter.value == "param"
        assert cat.Block.value == "block"

    def test_P2_Catch_CATCH_Block_init(self, context):
        cat = e.P2_Catch_CATCH_Block(context, "StrictArg", FakeTokens("catch", "block"))
        assert cat.name == "Catch"
        assert cat.Block.value == "block"
        assert cat.CatchParameter is None


#### Finally #######################################
#
# 8888888888 d8b                   888 888
# 888        Y8P                   888 888
# 888                              888 888
# 8888888    888 88888b.   8888b.  888 888 888  888
# 888        888 888 "88b     "88b 888 888 888  888
# 888        888 888  888 .d888888 888 888 888  888
# 888        888 888  888 888  888 888 888 Y88b 888
# 888        888 888  888 "Y888888 888 888  "Y88888
#                                               888
#                                          Y8b d88P
#                                           "Y88P"
#
####################################################
class Test_Finally:
    def test_P2_Finally_init(self, context):
        fin = e.P2_Finally(context, "StrictArg", FakeTokens("child"))
        assert fin.name == "Finally"
        assert fin.context == context
        assert [tok.value for tok in fin.children] == ["child"]

    def test_P2_Finally_FINALLY_Block_init(self, context):
        fin = e.P2_Finally_FINALLY_Block(context, "StrictArg", FakeTokens("finally", "block"))
        assert fin.name == "Finally"
        assert fin.Block.value == "block"


#### CatchParameter ###############################################################################################################
#
#  .d8888b.           888             888      8888888b.                                                   888
# d88P  Y88b          888             888      888   Y88b                                                  888
# 888    888          888             888      888    888                                                  888
# 888         8888b.  888888  .d8888b 88888b.  888   d88P  8888b.  888d888  8888b.  88888b.d88b.   .d88b.  888888  .d88b.  888d888
# 888            "88b 888    d88P"    888 "88b 8888888P"      "88b 888P"       "88b 888 "888 "88b d8P  Y8b 888    d8P  Y8b 888P"
# 888    888 .d888888 888    888      888  888 888        .d888888 888     .d888888 888  888  888 88888888 888    88888888 888
# Y88b  d88P 888  888 Y88b.  Y88b.    888  888 888        888  888 888     888  888 888  888  888 Y8b.     Y88b.  Y8b.     888
#  "Y8888P"  "Y888888  "Y888  "Y8888P 888  888 888        "Y888888 888     "Y888888 888  888  888  "Y8888   "Y888  "Y8888  888
#
#
#
#
###################################################################################################################################
class Test_CatchParameter:
    def test_P2_CatchParameter_init(self, context):
        cp = e.P2_CatchParameter(context, "StrictArg", FakeTokens("child"))
        assert cp.name == "CatchParameter"
        assert cp.context == context
        assert [tok.value for tok in cp.children] == ["child"]

    def test_P2_CatchParameter_BindingIdentifier_init(self, context):
        cp = e.P2_CatchParameter_BindingIdentifier(context, "StrictArg", FakeTokens("child"))
        assert cp.name == "CatchParameter"
        assert cp.BindingIdentifier.value == "child"

    def test_P2_CatchParameter_BindingPattern_init(self, context):
        cp = e.P2_CatchParameter_BindingPattern(context, "StrictArg", FakeTokens("child"))
        assert cp.name == "CatchParameter"
        assert cp.BindingPattern.value == "child"


#### DebuggerStatement ######################################################################################################################################
#
# 8888888b.           888                                                   .d8888b.  888             888                                             888
# 888  "Y88b          888                                                  d88P  Y88b 888             888                                             888
# 888    888          888                                                  Y88b.      888             888                                             888
# 888    888  .d88b.  88888b.  888  888  .d88b.   .d88b.   .d88b.  888d888  "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
# 888    888 d8P  Y8b 888 "88b 888  888 d88P"88b d88P"88b d8P  Y8b 888P"       "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
# 888    888 88888888 888  888 888  888 888  888 888  888 88888888 888           "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
# 888  .d88P Y8b.     888 d88P Y88b 888 Y88b 888 Y88b 888 Y8b.     888     Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 8888888P"   "Y8888  88888P"   "Y88888  "Y88888  "Y88888  "Y8888  888      "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#                                            888      888
#                                       Y8b d88P Y8b d88P
#                                        "Y88P"   "Y88P"
#
#############################################################################################################################################################
class Test_DebuggerStatement:
    def test_P2_DebuggerStatement_init(self, context):
        ds = e.P2_DebuggerStatement(context, "StrictArg", FakeTokens("child"))
        assert ds.name == "DebuggerStatement"
        assert ds.context == context
        assert [tok.value for tok in ds.children] == ["child"]

    def test_P2_DebuggerStatement_DEBUGGER_init(self, context):
        ds = e.P2_DebuggerStatement_DEBUGGER(context, "StrictArg", FakeTokens("child"))
        assert ds.name == "DebuggerStatement"


#### FunctionDeclaration ####################################################################################################################################
#
# 8888888888                            888    d8b                   8888888b.                    888                           888    d8b
# 888                                   888    Y8P                   888  "Y88b                   888                           888    Y8P
# 888                                   888                          888    888                   888                           888
# 8888888    888  888 88888b.   .d8888b 888888 888  .d88b.  88888b.  888    888  .d88b.   .d8888b 888  8888b.  888d888  8888b.  888888 888  .d88b.  88888b.
# 888        888  888 888 "88b d88P"    888    888 d88""88b 888 "88b 888    888 d8P  Y8b d88P"    888     "88b 888P"       "88b 888    888 d88""88b 888 "88b
# 888        888  888 888  888 888      888    888 888  888 888  888 888    888 88888888 888      888 .d888888 888     .d888888 888    888 888  888 888  888
# 888        Y88b 888 888  888 Y88b.    Y88b.  888 Y88..88P 888  888 888  .d88P Y8b.     Y88b.    888 888  888 888     888  888 Y88b.  888 Y88..88P 888  888
# 888         "Y88888 888  888  "Y8888P  "Y888 888  "Y88P"  888  888 8888888P"   "Y8888   "Y8888P 888 "Y888888 888     "Y888888  "Y888 888  "Y88P"  888  888
#
#
#
#
#############################################################################################################################################################
class Test_FunctionDeclaration:
    def test_P2_FunctionDeclaration_init(self, context):
        fd = e.P2_FunctionDeclaration(context, "StrictArg", FakeTokens("child"))
        assert fd.name == "FunctionDeclaration"
        assert fd.context == context
        assert [tok.value for tok in fd.children] == ["child"]

    def test_P2_FunctionDeclaration_FUNCTION_BindingIdentifier_FormalParameters_FunctionBody_init(self, context):
        fd = e.P2_FunctionDeclaration_FUNCTION_BindingIdentifier_FormalParameters_FunctionBody(
            context,
            "StrictArg",
            FakeTokens("function", "BindingIdentifier", "(", "FormalParameters", ")", "{", "FunctionBody", "}"),
        )
        assert fd.name == "FunctionDeclaration"
        assert fd.BindingIdentifier.value == "BindingIdentifier"
        assert fd.FormalParameters.value == "FormalParameters"
        assert fd.FunctionBody.value == "FunctionBody"

    def test_P2_FunctionDeclaration_FUNCTION_FormalParameters_FunctionBody_init(self, context):
        fd = e.P2_FunctionDeclaration_FUNCTION_FormalParameters_FunctionBody(
            context, "StrictArg", FakeTokens("function", "(", "FormalParameters", ")", "{", "FunctionBody", "}")
        )
        assert fd.name == "FunctionDeclaration"
        assert fd.FormalParameters.value == "FormalParameters"
        assert fd.FunctionBody.value == "FunctionBody"


#### FunctionExpression ###################################################################################################################################
#
# 8888888888                            888    d8b                   8888888888                                                      d8b
# 888                                   888    Y8P                   888                                                             Y8P
# 888                                   888                          888
# 8888888    888  888 88888b.   .d8888b 888888 888  .d88b.  88888b.  8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
# 888        888  888 888 "88b d88P"    888    888 d88""88b 888 "88b 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
# 888        888  888 888  888 888      888    888 888  888 888  888 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
# 888        Y88b 888 888  888 Y88b.    Y88b.  888 Y88..88P 888  888 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# 888         "Y88888 888  888  "Y8888P  "Y888 888  "Y88P"  888  888 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                                                        888
#                                                                                        888
#                                                                                        888
#
###########################################################################################################################################################
class Test_FunctionExpression:
    def test_P2_FunctionExpression_init(self, context):
        fe = e.P2_FunctionExpression(context, "StrictArg", FakeTokens("child"))
        assert fe.name == "FunctionExpression"
        assert fe.context == context
        assert [tok.value for tok in fe.children] == ["child"]

    def test_P2_FunctionExpression_FUNCTION_BindingIdentifier_FormalParameters_FunctionBody_init(self, context):
        fe = e.P2_FunctionExpression_FUNCTION_BindingIdentifier_FormalParameters_FunctionBody(
            context,
            "StrictArg",
            FakeTokens("function", "BindingIdentifier", "(", "FormalParameters", ")", "{", "FunctionBody", "}"),
        )
        assert fe.name == "FunctionExpression"
        assert fe.BindingIdentifier.value == "BindingIdentifier"
        assert fe.FormalParameters.value == "FormalParameters"
        assert fe.FunctionBody.value == "FunctionBody"

    def test_P2_FunctionExpression_FUNCTION_FormalParameters_FunctionBody_init(self, context):
        fe = e.P2_FunctionExpression_FUNCTION_FormalParameters_FunctionBody(
            context, "StrictArg", FakeTokens("function", "(", "FormalParameters", ")", "{", "FunctionBody", "}")
        )
        assert fe.name == "FunctionExpression"
        assert fe.FormalParameters.value == "FormalParameters"
        assert fe.FunctionBody.value == "FunctionBody"


#### UniqueFormalParameters ##############################################################################################################################################################################
#
# 888     888          d8b                            8888888888                                         888 8888888b.                                                   888
# 888     888          Y8P                            888                                                888 888   Y88b                                                  888
# 888     888                                         888                                                888 888    888                                                  888
# 888     888 88888b.  888  .d88888 888  888  .d88b.  8888888     .d88b.  888d888 88888b.d88b.   8888b.  888 888   d88P  8888b.  888d888  8888b.  88888b.d88b.   .d88b.  888888  .d88b.  888d888 .d8888b
# 888     888 888 "88b 888 d88" 888 888  888 d8P  Y8b 888        d88""88b 888P"   888 "888 "88b     "88b 888 8888888P"      "88b 888P"       "88b 888 "888 "88b d8P  Y8b 888    d8P  Y8b 888P"   88K
# 888     888 888  888 888 888  888 888  888 88888888 888        888  888 888     888  888  888 .d888888 888 888        .d888888 888     .d888888 888  888  888 88888888 888    88888888 888     "Y8888b.
# Y88b. .d88P 888  888 888 Y88b 888 Y88b 888 Y8b.     888        Y88..88P 888     888  888  888 888  888 888 888        888  888 888     888  888 888  888  888 Y8b.     Y88b.  Y8b.     888          X88
#  "Y88888P"  888  888 888  "Y88888  "Y88888  "Y8888  888         "Y88P"  888     888  888  888 "Y888888 888 888        "Y888888 888     "Y888888 888  888  888  "Y8888   "Y888  "Y8888  888      88888P'
#                               888
#                               888
#                               888
#
##########################################################################################################################################################################################################
class Test_UniqueFormalParameters:
    def test_P2_UniqueFormalParameters_init(self, context):
        ufp = e.P2_UniqueFormalParameters(context, "StrictArg", FakeTokens("child"))
        assert ufp.name == "UniqueFormalParameters"
        assert ufp.context == context
        assert [tok.value for tok in ufp.children] == ["child"]

    def test_P2_UniqueFormalParameters_FormalParameters_init(self, context):
        ufp = e.P2_UniqueFormalParameters_FormalParameters(context, "StrictArg", FakeTokens("child"))
        assert ufp.name == "UniqueFormalParameters"
        assert ufp.FormalParameters.value == "child"


#### FormalParameters ################################################################################################################################
#
# 8888888888                                         888 8888888b.                                                   888
# 888                                                888 888   Y88b                                                  888
# 888                                                888 888    888                                                  888
# 8888888     .d88b.  888d888 88888b.d88b.   8888b.  888 888   d88P  8888b.  888d888  8888b.  88888b.d88b.   .d88b.  888888  .d88b.  888d888 .d8888b
# 888        d88""88b 888P"   888 "888 "88b     "88b 888 8888888P"      "88b 888P"       "88b 888 "888 "88b d8P  Y8b 888    d8P  Y8b 888P"   88K
# 888        888  888 888     888  888  888 .d888888 888 888        .d888888 888     .d888888 888  888  888 88888888 888    88888888 888     "Y8888b.
# 888        Y88..88P 888     888  888  888 888  888 888 888        888  888 888     888  888 888  888  888 Y8b.     Y88b.  Y8b.     888          X88
# 888         "Y88P"  888     888  888  888 "Y888888 888 888        "Y888888 888     "Y888888 888  888  888  "Y8888   "Y888  "Y8888  888      88888P'
#
#
#
#
######################################################################################################################################################
class Test_FormalParameters:
    def test_P2_FormalParameters_init(self, context):
        fp = e.P2_FormalParameters(context, "StrictArg", FakeTokens("child"))
        assert fp.name == "FormalParameters"
        assert fp.context == context
        assert [tok.value for tok in fp.children] == ["child"]

    def test_P2_FormalParameters_EMPTY_init(self, context):
        fp = e.P2_FormalParameters_EMPTY(context, "StrictArg", FakeTokens("child"))
        assert fp.name == "FormalParameters"
        assert fp.FormalParameterList is None
        assert fp.FunctionRestParameter is None

    def test_P2_FormalParameters_FunctionRestParameter_init(self, context):
        fp = e.P2_FormalParameters_FunctionRestParameter(context, "StrictArg", FakeTokens("child"))
        assert fp.name == "FormalParameters"
        assert fp.FormalParameterList is None
        assert fp.FunctionRestParameter.value == "child"

    def test_P2_FormalParameters_FormalParameterList_init(self, context):
        fp = e.P2_FormalParameters_FormalParameterList(context, "StrictArg", FakeTokens("child"))
        assert fp.name == "FormalParameters"
        assert fp.FormalParameterList.value == "child"
        assert fp.FunctionRestParameter is None

    def test_P2_FormalParameters_FormalParameterList_FunctionRestParameter_init(self, context):
        fp = e.P2_FormalParameters_FormalParameterList_FunctionRestParameter(
            context, "StrictArg", FakeTokens("FormalParameterList", ",", "FunctionRestParameter")
        )
        assert fp.name == "FormalParameters"
        assert fp.FormalParameterList.value == "FormalParameterList"
        assert fp.FunctionRestParameter.value == "FunctionRestParameter"


#### FormalParameterList #################################################################################################################################################
#
# 8888888888                                         888 8888888b.                                                   888                     888      d8b          888
# 888                                                888 888   Y88b                                                  888                     888      Y8P          888
# 888                                                888 888    888                                                  888                     888                   888
# 8888888     .d88b.  888d888 88888b.d88b.   8888b.  888 888   d88P  8888b.  888d888  8888b.  88888b.d88b.   .d88b.  888888  .d88b.  888d888 888      888 .d8888b  888888
# 888        d88""88b 888P"   888 "888 "88b     "88b 888 8888888P"      "88b 888P"       "88b 888 "888 "88b d8P  Y8b 888    d8P  Y8b 888P"   888      888 88K      888
# 888        888  888 888     888  888  888 .d888888 888 888        .d888888 888     .d888888 888  888  888 88888888 888    88888888 888     888      888 "Y8888b. 888
# 888        Y88..88P 888     888  888  888 888  888 888 888        888  888 888     888  888 888  888  888 Y8b.     Y88b.  Y8b.     888     888      888      X88 Y88b.
# 888         "Y88P"  888     888  888  888 "Y888888 888 888        "Y888888 888     "Y888888 888  888  888  "Y8888   "Y888  "Y8888  888     88888888 888  88888P'  "Y888
#
#
#
#
##########################################################################################################################################################################
class Test_FormalParameterList:
    def test_P2_FormalParameterList_init(self, context):
        fpl = e.P2_FormalParameterList(context, "StrictArg", FakeTokens("child"))
        assert fpl.name == "FormalParameterList"
        assert fpl.context == context
        assert [tok.value for tok in fpl.children] == ["child"]

    def test_P2_FormalParameterList_FormalParameter_init(self, context):
        fpl = e.P2_FormalParameterList_FormalParameter(context, "StrictArg", FakeTokens("child"))
        assert fpl.name == "FormalParameterList"
        assert fpl.FormalParameter.value == "child"

    def test_P2_FormalParameterList_FormalParameterList_FormalParameter_init(self, context):
        fpl = e.P2_FormalParameterList_FormalParameterList_FormalParameter(
            context, "StrictArg", FakeTokens("FormalParameterList", ",", "FormalParameter")
        )
        assert fpl.name == "FormalParameterList"
        assert fpl.FormalParameterList.value == "FormalParameterList"
        assert fpl.FormalParameter.value == "FormalParameter"


#### FunctionRestParameter ##################################################################################################################################################################
#
# 8888888888                            888    d8b                   8888888b.                    888    8888888b.                                                   888
# 888                                   888    Y8P                   888   Y88b                   888    888   Y88b                                                  888
# 888                                   888                          888    888                   888    888    888                                                  888
# 8888888    888  888 88888b.   .d8888b 888888 888  .d88b.  88888b.  888   d88P  .d88b.  .d8888b  888888 888   d88P  8888b.  888d888  8888b.  88888b.d88b.   .d88b.  888888  .d88b.  888d888
# 888        888  888 888 "88b d88P"    888    888 d88""88b 888 "88b 8888888P"  d8P  Y8b 88K      888    8888888P"      "88b 888P"       "88b 888 "888 "88b d8P  Y8b 888    d8P  Y8b 888P"
# 888        888  888 888  888 888      888    888 888  888 888  888 888 T88b   88888888 "Y8888b. 888    888        .d888888 888     .d888888 888  888  888 88888888 888    88888888 888
# 888        Y88b 888 888  888 Y88b.    Y88b.  888 Y88..88P 888  888 888  T88b  Y8b.          X88 Y88b.  888        888  888 888     888  888 888  888  888 Y8b.     Y88b.  Y8b.     888
# 888         "Y88888 888  888  "Y8888P  "Y888 888  "Y88P"  888  888 888   T88b  "Y8888   88888P'  "Y888 888        "Y888888 888     "Y888888 888  888  888  "Y8888   "Y888  "Y8888  888
#
#
#
#
#############################################################################################################################################################################################
class Test_FunctionRestParameter:
    def test_P2_FunctionRestParameter_init(self, context):
        frp = e.P2_FunctionRestParameter(context, "StrictArg", FakeTokens("child"))
        assert frp.name == "FunctionRestParameter"
        assert frp.context == context
        assert [tok.value for tok in frp.children] == ["child"]

    def test_P2_FunctionRestParameter_BindingRestElement_init(self, context):
        frp = e.P2_FunctionRestParameter_BindingRestElement(context, "StrictArg", FakeTokens("child"))
        assert frp.name == "FunctionRestParameter"
        assert frp.BindingRestElement.value == "child"


#### FormalParameter ########################################################################################################################
#
# 8888888888                                         888 8888888b.                                                   888
# 888                                                888 888   Y88b                                                  888
# 888                                                888 888    888                                                  888
# 8888888     .d88b.  888d888 88888b.d88b.   8888b.  888 888   d88P  8888b.  888d888  8888b.  88888b.d88b.   .d88b.  888888  .d88b.  888d888
# 888        d88""88b 888P"   888 "888 "88b     "88b 888 8888888P"      "88b 888P"       "88b 888 "888 "88b d8P  Y8b 888    d8P  Y8b 888P"
# 888        888  888 888     888  888  888 .d888888 888 888        .d888888 888     .d888888 888  888  888 88888888 888    88888888 888
# 888        Y88..88P 888     888  888  888 888  888 888 888        888  888 888     888  888 888  888  888 Y8b.     Y88b.  Y8b.     888
# 888         "Y88P"  888     888  888  888 "Y888888 888 888        "Y888888 888     "Y888888 888  888  888  "Y8888   "Y888  "Y8888  888
#
#
#
#
#############################################################################################################################################
class Test_FormalParameter:
    def test_P2_FormalParameter_init(self, context):
        fp = e.P2_FormalParameter(context, "StrictArg", FakeTokens("child"))
        assert fp.name == "FormalParameter"
        assert fp.context == context
        assert [tok.value for tok in fp.children] == ["child"]

    def test_P2_FormalParameter_BindingElement_init(self, context):
        fp = e.P2_FormalParameter_BindingElement(context, "StrictArg", FakeTokens("child"))
        assert fp.name == "FormalParameter"
        assert fp.BindingElement.value == "child"


#### FunctionBody #########################################################################################
#
# 8888888888                            888    d8b                   888888b.                 888
# 888                                   888    Y8P                   888  "88b                888
# 888                                   888                          888  .88P                888
# 8888888    888  888 88888b.   .d8888b 888888 888  .d88b.  88888b.  8888888K.   .d88b.   .d88888 888  888
# 888        888  888 888 "88b d88P"    888    888 d88""88b 888 "88b 888  "Y88b d88""88b d88" 888 888  888
# 888        888  888 888  888 888      888    888 888  888 888  888 888    888 888  888 888  888 888  888
# 888        Y88b 888 888  888 Y88b.    Y88b.  888 Y88..88P 888  888 888   d88P Y88..88P Y88b 888 Y88b 888
# 888         "Y88888 888  888  "Y8888P  "Y888 888  "Y88P"  888  888 8888888P"   "Y88P"   "Y88888  "Y88888
#                                                                                                      888
#                                                                                                 Y8b d88P
#                                                                                                  "Y88P"
#
###########################################################################################################
class Test_FunctionBody:
    def test_P2_FunctionBody_init(self, context):
        fb = e.P2_FunctionBody(context, "StrictArg", FakeTokens("child"))
        assert fb.name == "FunctionBody"
        assert fb.context == context
        assert [tok.value for tok in fb.children] == ["child"]

    def test_P2_FunctionBody_FunctionStatementList_init(self, context):
        fb = e.P2_FunctionBody_FunctionStatementList(context, "StrictArg", FakeTokens("child"))
        assert fb.name == "FunctionBody"
        assert fb.FunctionStatementList.value == "child"


#### FunctionStatementList #########################################################################################################################################################
#
# 8888888888                            888    d8b                    .d8888b.  888             888                                             888    888      d8b          888
# 888                                   888    Y8P                   d88P  Y88b 888             888                                             888    888      Y8P          888
# 888                                   888                          Y88b.      888             888                                             888    888                   888
# 8888888    888  888 88888b.   .d8888b 888888 888  .d88b.  88888b.   "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888 888      888 .d8888b  888888
# 888        888  888 888 "88b d88P"    888    888 d88""88b 888 "88b     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888    888      888 88K      888
# 888        888  888 888  888 888      888    888 888  888 888  888       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888    888      888 "Y8888b. 888
# 888        Y88b 888 888  888 Y88b.    Y88b.  888 Y88..88P 888  888 Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.  888      888      X88 Y88b.
# 888         "Y88888 888  888  "Y8888P  "Y888 888  "Y88P"  888  888  "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888 88888888 888  88888P'  "Y888
#
#
#
#
####################################################################################################################################################################################
class Test_FunctionStatementList:
    def test_P2_FunctionStatementList_init(self, context):
        fsl = e.P2_FunctionStatementList(context, "StrictArg", FakeTokens("child"))
        assert fsl.name == "FunctionStatementList"
        assert fsl.context == context
        assert [tok.value for tok in fsl.children] == ["child"]

    def test_P2_FunctionStatementList_StatementList_init(self, context):
        fsl = e.P2_FunctionStatementList_StatementList(context, "StrictArg", FakeTokens("child"))
        assert fsl.name == "FunctionStatementList"
        assert fsl.StatementList.value == "child"

    def test_P2_FunctionStatementList_EMPTY_init(self, context):
        fsl = e.P2_FunctionStatementList_EMPTY(context, "StrictArg", FakeTokens("child"))
        assert fsl.name == "FunctionStatementList"
        assert fsl.StatementList is None


#### ArrowFunction ######################################################################################################
#
#        d8888                                        8888888888                            888    d8b
#       d88888                                        888                                   888    Y8P
#      d88P888                                        888                                   888
#     d88P 888 888d888 888d888  .d88b.  888  888  888 8888888    888  888 88888b.   .d8888b 888888 888  .d88b.  88888b.
#    d88P  888 888P"   888P"   d88""88b 888  888  888 888        888  888 888 "88b d88P"    888    888 d88""88b 888 "88b
#   d88P   888 888     888     888  888 888  888  888 888        888  888 888  888 888      888    888 888  888 888  888
#  d8888888888 888     888     Y88..88P Y88b 888 d88P 888        Y88b 888 888  888 Y88b.    Y88b.  888 Y88..88P 888  888
# d88P     888 888     888      "Y88P"   "Y8888888P"  888         "Y88888 888  888  "Y8888P  "Y888 888  "Y88P"  888  888
#
#
#
#
#########################################################################################################################
class Test_ArrowFunction:
    def test_P2_ArrowFunction_init(self, context):
        af = e.P2_ArrowFunction(context, "StrictArg", FakeTokens("child"))
        assert af.name == "ArrowFunction"
        assert af.context == context
        assert [tok.value for tok in af.children] == ["child"]

    def test_P2_ArrowFunction_ArrowParameters_ConciseBody_init(self, context):
        af = e.P2_ArrowFunction_ArrowParameters_ConciseBody(
            context, "StrictArg", FakeTokens("arrowparameters", "=>", "concisebody")
        )
        assert af.name == "ArrowFunction"
        assert af.ArrowParameters.value == "arrowparameters"
        assert af.ConciseBody.value == "concisebody"


#### ArrowParameters ##############################################################################################################################
#
#        d8888                                        8888888b.                                                   888
#       d88888                                        888   Y88b                                                  888
#      d88P888                                        888    888                                                  888
#     d88P 888 888d888 888d888  .d88b.  888  888  888 888   d88P  8888b.  888d888  8888b.  88888b.d88b.   .d88b.  888888  .d88b.  888d888 .d8888b
#    d88P  888 888P"   888P"   d88""88b 888  888  888 8888888P"      "88b 888P"       "88b 888 "888 "88b d8P  Y8b 888    d8P  Y8b 888P"   88K
#   d88P   888 888     888     888  888 888  888  888 888        .d888888 888     .d888888 888  888  888 88888888 888    88888888 888     "Y8888b.
#  d8888888888 888     888     Y88..88P Y88b 888 d88P 888        888  888 888     888  888 888  888  888 Y8b.     Y88b.  Y8b.     888          X88
# d88P     888 888     888      "Y88P"   "Y8888888P"  888        "Y888888 888     "Y888888 888  888  888  "Y8888   "Y888  "Y8888  888      88888P'
#
#
#
#
###################################################################################################################################################
class Test_ArrowParameters:
    def test_P2_ArrowParameters_init(self, context):
        ap = e.P2_ArrowParameters(context, "StrictArg", FakeTokens("child"))
        assert ap.name == "ArrowParameters"
        assert ap.context == context
        assert [tok.value for tok in ap.children] == ["child"]

    def test_P2_ArrowParameters_BindingIdentifier_init(self, context):
        ap = e.P2_ArrowParameters_BindingIdentifier(context, "StrictArg", FakeTokens("child"))
        assert ap.name == "ArrowParameters"
        assert ap.BindingIdentifier.value == "child"

    def test_P2_ArrowParameters_CoverParenthesizedExpressionAndArrowParameterList_init(self, context):
        ap = e.P2_ArrowParameters_CoverParenthesizedExpressionAndArrowParameterList(
            context, "StrictArg", FakeTokens("child")
        )
        assert ap.name == "ArrowParameters"
        assert ap.CoverParenthesizedExpressionAndArrowParameterList.value == "child"


#### Script ######################################
#
#  .d8888b.                   d8b          888
# d88P  Y88b                  Y8P          888
# Y88b.                                    888
#  "Y888b.    .d8888b 888d888 888 88888b.  888888
#     "Y88b. d88P"    888P"   888 888 "88b 888
#       "888 888      888     888 888  888 888
# Y88b  d88P Y88b.    888     888 888 d88P Y88b.
#  "Y8888P"   "Y8888P 888     888 88888P"   "Y888
#                                 888
#                                 888
#                                 888
#
##################################################
def test_P2_Script_init(context):
    script = e.P2_Script(context, "StrictArg", FakeTokens("child"))
    assert script.name == "Script"
    assert script.context == context
    assert [tok.value for tok in script.children] == ["child"]


def test_P2_Script_Empty_init(context):
    script = e.P2_Script_Empty(context, "StrictArg", FakeTokens("child"))
    assert script.name == "Script"
    assert script.ScriptBody is None


def test_P2_Script_ScriptBody_init(context):
    script = e.P2_Script_ScriptBody(context, "StrictArg", FakeTokens("child"))
    assert script.name == "Script"
    assert script.ScriptBody.value == "child"


#### ScriptBody ########################################################################
#
#  .d8888b.                   d8b          888    888888b.                 888
# d88P  Y88b                  Y8P          888    888  "88b                888
# Y88b.                                    888    888  .88P                888
#  "Y888b.    .d8888b 888d888 888 88888b.  888888 8888888K.   .d88b.   .d88888 888  888
#     "Y88b. d88P"    888P"   888 888 "88b 888    888  "Y88b d88""88b d88" 888 888  888
#       "888 888      888     888 888  888 888    888    888 888  888 888  888 888  888
# Y88b  d88P Y88b.    888     888 888 d88P Y88b.  888   d88P Y88..88P Y88b 888 Y88b 888
#  "Y8888P"   "Y8888P 888     888 88888P"   "Y888 8888888P"   "Y88P"   "Y88888  "Y88888
#                                 888                                               888
#                                 888                                          Y8b d88P
#                                 888                                           "Y88P"
#
########################################################################################
def test_P2_ScriptBody_init(context):
    sb = e.P2_ScriptBody(context, "StrictArg", FakeTokens("child"))
    assert sb.name == "ScriptBody"
    assert sb.context == context
    assert [tok.value for tok in sb.children] == ["child"]


def test_P2_ScriptBody_StatementList_init(context):
    sb = e.P2_ScriptBody_StatementList(context, "StrictArg", FakeTokens("child"))
    assert sb.name == "ScriptBody"
    assert sb.StatementList.value == "child"


##########################################################################################
##########################################################################################


@pytest.mark.parametrize(
    "stub",
    [
        e.parse_AsyncFunctionExpression,
        e.parse_AsyncGeneratorExpression,
        e.parse_AwaitExpression,
        e.parse_AsyncArrowFunction,
        e.parse_AsyncFunctionDeclaration,
        e.parse_AsyncGeneratorDeclaration,
    ],
)
def test_stubs(stub):
    # This really isn't testing anything. It's here to get to 100% coverage.
    # (All of these functions will go away, once implemented, and then this
    # entire test will be removed.)
    assert stub() is None

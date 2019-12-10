import pytest
import snoop
from enum import Enum, unique, auto
import types

import ecmascript.ecmascript as e


class Token:
    def __init__(self, tokentype, value, newlines=[(0, 1)]):
        self.type = tokentype
        self.value = value
        self.newlines = tuple(newlines)
        self.src = value
        self.span = None

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
FUNCTIONBODY, functionbody_sideeffect = parser_mock("FunctionBody")
FORMALPARAMETERLIST, formalparameterlist_sideeffect = parser_mock("FormalParameterList")
FUNCTIONRESTPARAMETER, functionrestparameter_sideeffect = parser_mock("FunctionRestParameter")
FORMALPARAMETER, formalparameter_sideeffect = parser_mock("FormalParameter")
FUNCTIONSTATEMENTLIST, functionstatementlist_sideeffect = parser_mock("FunctionStatementList")
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
        pn = e.ParseNode2("my context", "my_name", "strictitude", ["child1", "child2"])

        assert pn.name == "my_name"
        assert pn.context == "my context"
        assert pn.children == ["child1", "child2"]
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
                super().__init__(ctx, "Test", children)

        class Test_Production(Test_Base):
            pass

        return (Test_Production(None, ["child"]), Test_Base)

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
        pn = e.ParseNode2(p2c, "Top", "StrictArg", ["child"])
        assert pn.direct_eval == "test string"

    def test_CreateSyntaxError(self):
        p2c = e.Parse2Context(syntax_error_ctor="syntax error constructor")
        pn = e.ParseNode2(p2c, "Top", "StrictArg", ["child"])
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
        ],
    )
    def test_deferred_calls(self, callname):
        class Child(e.ParseNode2):
            pass

        child = Child(None, "Child", [])
        setattr(child, callname, types.MethodType(lambda self: callname, child))
        parent = e.ParseNode2(None, "Parent", "StrictArg", [child])
        assert getattr(parent, callname)() == callname


def test_parse2context_init():
    pc = e.Parse2Context("direct_eval", "syntax_error_ctor", "goal")

    assert pc.direct_eval == "direct_eval"
    assert pc.CreateSyntaxError == "syntax_error_ctor"
    assert pc.goal == "goal"


def test_empty_node():
    ctx = e.Parse2Context("source text")
    en = e.empty_node(ctx)

    assert isinstance(en, e.ParseNode2)
    assert en.name == "[empty]"
    assert en.context == ctx
    assert en.children == []


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


def test_P2_MemberExpression_PrimaryExpression_init(context):
    me = e.P2_MemberExpression_PrimaryExpression(context, False, ["PrimaryExpression"])
    assert me.name == "MemberExpression"
    assert me.PrimaryExpression == "PrimaryExpression"


def test_P2_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET_init(context):
    me = e.P2_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET(
        context, False, ["MemberExpression", "[", "Expression", "]"]
    )
    assert me.name == "MemberExpression"
    assert me.MemberExpression == "MemberExpression"
    assert me.Expression == "Expression"


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


def test_parse_MemberExpression_01(mocker, context):
    # MemberExpression : PrimaryExpression
    # Source text that would match this: "bob"
    lexer = Lexer([PRIMARYEXPRESSION])
    mocked = mocker.patch("ecmascript.ecmascript.parse_PrimaryExpression", side_effect=primaryexpression_sideeffect)
    me = e.parse_MemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")

    assert isinstance(me, e.P2_MemberExpression_PrimaryExpression)
    mocked.assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert me.PrimaryExpression == "PrimaryExpression"
    assert lexer.pos == 1


def test_parse_MemberExpression_02(mocker, context):
    # MemberExpression : MemberExpression [ Expression ]
    # Source text that would match this: "bob[3]"
    lexer = Lexer([PRIMARYEXPRESSION, LBRACKET, EXPRESSION, RBRACKET])
    mocker.patch("ecmascript.ecmascript.parse_PrimaryExpression", side_effect=primaryexpression_sideeffect)
    mocker.patch("ecmascript.ecmascript.parse_SuperProperty", return_value=None)
    mocker.patch("ecmascript.ecmascript.parse_MetaProperty", return_value=None)
    parse_EXP = mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect)

    me = e.parse_MemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")

    assert isinstance(me, e.P2_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET)
    parse_EXP.assert_called_with(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg")
    assert me.Expression == "Expression"
    me2 = me.MemberExpression
    assert isinstance(me2, e.P2_MemberExpression_PrimaryExpression)
    assert me2.PrimaryExpression == "PrimaryExpression"
    assert lexer.pos == 4


def test_parse_MemberExpression_03(mocker, context):
    # MemberExpression : MemberExpression . IdentifierName
    # Source text that would match this: "bob.charlie"
    lexer = Lexer([PRIMARYEXPRESSION, PERIOD, ICHARLIE])
    mocker.patch("ecmascript.ecmascript.parse_PrimaryExpression", side_effect=primaryexpression_sideeffect)

    me = e.parse_MemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(me, e.P2_MemberExpression_MemberExpression_PERIOD_IdentifierName)
    assert me.IdentifierName == ICHARLIE
    m2 = me.MemberExpression
    assert isinstance(m2, e.P2_MemberExpression_PrimaryExpression)
    assert m2.PrimaryExpression == "PrimaryExpression"
    assert lexer.pos == 3


def test_parse_MemberExpression_04(mocker, context):
    # MemberExpresion : MemberExpression TemplateLiteral
    # Source text that would match this: "bob`a`"
    lexer = Lexer([PRIMARYEXPRESSION, TL_REPLACEMENT])
    mocker.patch("ecmascript.ecmascript.parse_PrimaryExpression", side_effect=primaryexpression_sideeffect)
    tl_mock = mocker.patch("ecmascript.ecmascript.parse_TemplateLiteral", side_effect=tl_sideeffect)

    me = e.parse_MemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(me, e.P2_MemberExpression_MemberExpression_TemplateLiteral)
    assert me.TemplateLiteral == "TemplateLiteral"
    tl_mock.assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg", True)
    me2 = me.MemberExpression
    assert isinstance(me2, e.P2_MemberExpression_PrimaryExpression)
    assert me2.PrimaryExpression == "PrimaryExpression"
    assert lexer.pos == 2


def test_parse_MemberExpression_05(mocker, context):
    # MemberExpression : SuperProperty
    # Source text that would match this: "super.bob"
    lexer = Lexer([SP_REPLACEMENT])
    mocker.patch("ecmascript.ecmascript.parse_PrimaryExpression", side_effect=primaryexpression_sideeffect)
    superprop = mocker.patch("ecmascript.ecmascript.parse_SuperProperty", side_effect=sp_sideeffect)

    me = e.parse_MemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(me, e.P2_MemberExpression_SuperProperty)
    assert me.SuperProperty == "SuperProperty"
    superprop.assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert lexer.pos == 1


def test_parse_MemberExpression_06(mocker, context):
    # MemberExpression : MetaProperty
    # Source text that matches this: "new.target"
    lexer = Lexer([MP_REPLACEMENT])
    mocker.patch("ecmascript.ecmascript.parse_PrimaryExpression", side_effect=primaryexpression_sideeffect)
    mocker.patch("ecmascript.ecmascript.parse_SuperProperty", side_effect=sp_sideeffect)
    metaprop = mocker.patch("ecmascript.ecmascript.parse_MetaProperty", side_effect=mp_sideeffect)

    me = e.parse_MemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(me, e.P2_MemberExpression_MetaProperty)
    assert me.MetaProperty == "MetaProperty"
    metaprop.assert_called_with(context, lexer, "StrictArg")
    assert lexer.pos == 1


def test_parse_MemberExpression_07(mocker, context):
    # MemberExpression : new MemberExpression Arguments
    # Source text that matches this: "new bob()"
    lexer = Lexer([NEW, PRIMARYEXPRESSION, ARGS_REPLACEMENT])
    mocker.patch("ecmascript.ecmascript.parse_PrimaryExpression", side_effect=primaryexpression_sideeffect)
    mocker.patch("ecmascript.ecmascript.parse_SuperProperty", side_effect=sp_sideeffect)
    mocker.patch("ecmascript.ecmascript.parse_MetaProperty", side_effect=mp_sideeffect)
    argsmock = mocker.patch("ecmascript.ecmascript.parse_Arguments", side_effect=args_sideeffect)

    me = e.parse_MemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(me, e.P2_MemberExpression_NEW_MemberExpression_Arguments)
    assert me.Arguments == "Arguments"
    argsmock.assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    me2 = me.MemberExpression
    assert isinstance(me2, e.P2_MemberExpression_PrimaryExpression)
    assert me2.PrimaryExpression == "PrimaryExpression"
    assert lexer.pos == 3


def test_parse_MemberExpression_08(mocker, context):
    # Check that SuperProperty recursion works.
    # MemberExpression : MemberExpression [ Expression ]
    # where the RHS MemberExpression is:  MemberExpression : SuperProperty
    # Source text that matches this is: "super.bob[3]"
    lexer = Lexer([SP_REPLACEMENT, LBRACKET, EXPRESSION, RBRACKET])
    mocker.patch("ecmascript.ecmascript.parse_PrimaryExpression", side_effect=primaryexpression_sideeffect)
    mocker.patch("ecmascript.ecmascript.parse_SuperProperty", side_effect=sp_sideeffect)
    mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect)

    me = e.parse_MemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(me, e.P2_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET)
    assert me.Expression == "Expression"
    me2 = me.MemberExpression
    assert isinstance(me2, e.P2_MemberExpression_SuperProperty)
    assert me2.SuperProperty == "SuperProperty"
    assert lexer.pos == 4


def test_parse_MemberExpression_09(mocker, context):
    # Check that MetaProperty recursion works.
    # MemberExpression : MemberExpression [ Expression ]
    # where the RHS MemberExpression is:  MemberExpression : MetaProperty
    # Source text that matches this is: "new.target[3]"
    lexer = Lexer([MP_REPLACEMENT, LBRACKET, EXPRESSION, RBRACKET])
    mocker.patch("ecmascript.ecmascript.parse_PrimaryExpression", side_effect=primaryexpression_sideeffect)
    mocker.patch("ecmascript.ecmascript.parse_SuperProperty", side_effect=sp_sideeffect)
    mocker.patch("ecmascript.ecmascript.parse_MetaProperty", side_effect=mp_sideeffect)
    mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect)

    me = e.parse_MemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(me, e.P2_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET)
    assert me.Expression == "Expression"
    me2 = me.MemberExpression
    assert isinstance(me2, e.P2_MemberExpression_MetaProperty)
    assert me2.MetaProperty == "MetaProperty"
    assert lexer.pos == 4


def test_parse_MemberExpression_10(mocker, context):
    # Check that "wrong from the start" returns None and consumes no tokens.
    # Source text example: "function bob() {;}"
    lexer = Lexer([FUNCTION, IBOB, LPAREN, RPAREN, LCURLY, SEMICOLON, RCURLY])
    mocker.patch("ecmascript.ecmascript.parse_PrimaryExpression", return_value=None)
    mocker.patch("ecmascript.ecmascript.parse_SuperProperty", return_value=None)
    mocker.patch("ecmascript.ecmascript.parse_MetaProperty", return_value=None)

    me = e.parse_MemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert me is None
    assert lexer.pos == 0


def test_parse_MemberExpression_11(mocker, context):
    # Check that errors after "new" do the right thing.
    # Source text example: "new %"
    lexer = Lexer([NEW, MATCHES_NONE])
    mocker.patch("ecmascript.ecmascript.parse_PrimaryExpression", return_value=None)
    mocker.patch("ecmascript.ecmascript.parse_SuperProperty", return_value=None)
    mocker.patch("ecmascript.ecmascript.parse_MetaProperty", return_value=None)

    me = e.parse_MemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert me is None
    assert lexer.pos == 0


def test_parse_MemberExpression_12(mocker, context):
    # Check that 'new MemberExpression Garbage' works
    # Source text example: "new bob 3"
    lexer = Lexer([NEW, PRIMARYEXPRESSION, MATCHES_NONE])
    mocker.patch("ecmascript.ecmascript.parse_SuperProperty", side_effect=sp_sideeffect)
    mocker.patch("ecmascript.ecmascript.parse_MetaProperty", side_effect=mp_sideeffect)
    mocker.patch("ecmascript.ecmascript.parse_Arguments", side_effect=args_sideeffect)
    mocker.patch("ecmascript.ecmascript.parse_PrimaryExpression", side_effect=primaryexpression_sideeffect)

    me = e.parse_MemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert me is None
    assert lexer.pos == 0


def test_parse_MemberExpression_13(mocker, context):
    # Check that MemberExpression [ Garbage ] works.
    # (I.e.: We should get that first MemberExpression back, but leave the left-bracket in the token stream)
    # Source text example: "bob[%]"
    lexer = Lexer([PRIMARYEXPRESSION, LBRACKET, MATCHES_NONE, RBRACKET])
    mocker.patch("ecmascript.ecmascript.parse_PrimaryExpression", side_effect=primaryexpression_sideeffect)
    mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect)

    me = e.parse_MemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(me, e.P2_MemberExpression_PrimaryExpression)
    assert me.PrimaryExpression == "PrimaryExpression"
    assert lexer.pos == 1


def test_parse_MemberExpression_14(mocker, context):
    # Check that a failure in ']' detection works.
    # Source text example: "bob[3 3"
    lexer = Lexer([PRIMARYEXPRESSION, LBRACKET, EXPRESSION, MATCHES_NONE])
    mocker.patch("ecmascript.ecmascript.parse_PrimaryExpression", side_effect=primaryexpression_sideeffect)
    mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect)

    me = e.parse_MemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(me, e.P2_MemberExpression_PrimaryExpression)
    assert me.PrimaryExpression == "PrimaryExpression"
    assert lexer.pos == 1


def test_parse_MemberExpression_15(mocker, context):
    # Check that failure in identifier parsing works.
    # Source text example: "bob.%"
    lexer = Lexer([PRIMARYEXPRESSION, PERIOD, MATCHES_NONE])
    mocker.patch("ecmascript.ecmascript.parse_PrimaryExpression", side_effect=primaryexpression_sideeffect)

    me = e.parse_MemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(me, e.P2_MemberExpression_PrimaryExpression)
    assert me.PrimaryExpression == "PrimaryExpression"
    assert lexer.pos == 1


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


def test_parse_SuperProperty_01(mocker, context):
    # SuperProperty : super [ Expression ]
    # Source text that matches this: "super[3]"
    lexer = Lexer([SUPER, LBRACKET, EXPRESSION, RBRACKET])
    mock_exp = mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect)

    sp = e.parse_SuperProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(sp, e.P2_SuperProperty_SUPER_LBRACKET_Expression_RBRACKET)
    assert sp.Expression == "Expression"
    mock_exp.assert_called_with(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg")
    assert lexer.pos == 4


def test_parse_SuperProperty_02(context):
    # SuperProperty : super . IdentifierName
    # Source text that matches this: "super.bob"
    lexer = Lexer([SUPER, PERIOD, IBOB])

    sp = e.parse_SuperProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(sp, e.P2_SuperProperty_SUPER_PERIOD_IdentifierName)
    assert sp.IdentifierName == IBOB
    assert lexer.pos == 3


def test_parse_SuperProperty_03(context):
    # Test passing something that doesn't start with a "super" token.
    lexer = Lexer([MATCHES_NONE])

    sp = e.parse_SuperProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert sp is None
    assert lexer.pos == 0


def test_parse_SuperProperty_04(context):
    # Starts with a "super" and then neither a '.' or a '['
    # Something like: "super %"
    lexer = Lexer([SUPER, MATCHES_NONE])

    sp = e.parse_SuperProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert sp is None
    assert lexer.pos == 0


def test_parse_SuperProperty_05(mocker, context):
    # Bad Expression
    # Something like: "super[new]"
    lexer = Lexer([SUPER, LBRACKET, NEW, RBRACKET])
    mocker.patch("ecmascript.ecmascript.parse_Expression", return_value=None)

    sp = e.parse_SuperProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert sp is None
    assert lexer.pos == 0


def test_parse_SuperProperty_06(mocker, context):
    # Bad right bracket.
    # Something like: "super[3 new]"
    lexer = Lexer([SUPER, LBRACKET, EXPRESSION, NEW, RBRACKET])
    mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect)

    sp = e.parse_SuperProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert sp is None
    assert lexer.pos == 0


def test_parse_SuperProperty_07(context):
    # Bad identifiername
    # Something like: "super.&"
    lexer = Lexer([SUPER, PERIOD, MATCHES_NONE])

    sp = e.parse_SuperProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert sp is None
    assert lexer.pos == 0


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


def test_parse_MetaProperty_01(mocker, context):
    # MetaProperty : NewTarget
    # Source looks like: "new.target"
    lexer = Lexer([NT_REPLACEMENT])
    nt_mock = mocker.patch("ecmascript.ecmascript.parse_NewTarget", side_effect=nt_sideeffect)

    mp = e.parse_MetaProperty(context, lexer, "StrictArg")
    assert isinstance(mp, e.P2_MetaProperty_NewTarget)
    assert mp.NewTarget == "NewTarget"
    nt_mock.assert_called_with(context, lexer, "StrictArg")
    assert lexer.pos == 1


def test_parse_MetaProperty_02(mocker, context):
    # Check when NewTarget doesn't match
    # Potential Source: "a=3;"
    lexer = Lexer([MATCHES_NONE])
    mocker.patch("ecmascript.ecmascript.parse_NewTarget", return_value=None)

    mp = e.parse_MetaProperty(context, lexer, "StrictArg")
    assert mp is None
    assert lexer.pos == 0


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


def test_parse_NewTarget_01(context):
    # NewTarget : new . target
    lexer = Lexer([NEW, PERIOD, TARGET])
    nt = e.parse_NewTarget(context, lexer, "StrictArg")
    assert isinstance(nt, e.P2_NewTarget_NEW_PERIOD_TARGET)
    assert lexer.pos == 3


@pytest.mark.parametrize("token_stream", [[MATCHES_NONE], [NEW, MATCHES_NONE], [NEW, PERIOD, MATCHES_NONE]])
def test_parse_NewTarget_02(context, token_stream):
    # Tests all the token mismatch cases
    lexer = Lexer(token_stream)
    nt = e.parse_NewTarget(context, lexer, "StrictArg")
    assert nt is None
    assert lexer.pos == 0


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


def test_parse_NewExpression_01(mocker, context):
    # NewExpression : MemberExpression
    lexer = Lexer([ME_REPLACEMENT])
    me_mock = mocker.patch("ecmascript.ecmascript.parse_MemberExpression", side_effect=me_sideeffect)

    ne = e.parse_NewExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ne, e.P2_NewExpression_MemberExpression)
    assert ne.MemberExpression == "MemberExpression"
    assert lexer.pos == 1
    me_mock.assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_NewExpression_02(mocker, context):
    # NewExpression : new NewExpression
    lexer = Lexer([NEW, ME_REPLACEMENT])
    mocker.patch("ecmascript.ecmascript.parse_MemberExpression", side_effect=me_sideeffect)
    ne = e.parse_NewExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ne, e.P2_NewExpression_NEW_NewExpression)
    ne2 = ne.NewExpression
    assert isinstance(ne2, e.P2_NewExpression_MemberExpression)
    assert ne2.MemberExpression == "MemberExpression"
    assert lexer.pos == 2


def test_parse_NewExpression_03(mocker, context):
    # Check deeper recursion
    lexer = Lexer([NEW, NEW, NEW, ME_REPLACEMENT])
    mocker.patch("ecmascript.ecmascript.parse_MemberExpression", side_effect=me_sideeffect)
    ne = e.parse_NewExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ne, e.P2_NewExpression_NEW_NewExpression)
    ne2 = ne.NewExpression
    assert isinstance(ne2, e.P2_NewExpression_NEW_NewExpression)
    ne3 = ne2.NewExpression
    assert isinstance(ne3, e.P2_NewExpression_NEW_NewExpression)
    ne4 = ne3.NewExpression
    assert isinstance(ne4, e.P2_NewExpression_MemberExpression)
    assert ne4.MemberExpression == "MemberExpression"
    assert lexer.pos == 4


@pytest.mark.parametrize("token_stream", [[MATCHES_NONE], [NEW, MATCHES_NONE]])
def test_parse_NewExpression_04(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    mocker.patch("ecmascript.ecmascript.parse_MemberExpression", return_value=None)
    ne = e.parse_NewExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert ne is None
    assert lexer.pos == 0


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


def CallExpression_mocks(mocker):
    return {
        "cce": mocker.patch(
            "ecmascript.ecmascript.parse_CoverCallExpressionAndAsyncArrowHead", side_effect=cce_sideeffect
        ),
        "sc": mocker.patch("ecmascript.ecmascript.parse_SuperCall", side_effect=sc_sideeffect),
        "args": mocker.patch("ecmascript.ecmascript.parse_Arguments", side_effect=args_sideeffect),
        "exp": mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect),
        "tl": mocker.patch("ecmascript.ecmascript.parse_TemplateLiteral", side_effect=tl_sideeffect),
    }


def test_parse_CallExpression_01(mocker, context):
    # CallExpression : CoverCallExpressionAndAsyncArrowHead
    # Sample source text : "bob()"
    lexer = Lexer([CCE_REPLACEMENT])
    mocks = CallExpression_mocks(mocker)

    ce = e.parse_CallExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ce, e.P2_CallExpression_CoverCallExpressionAndAsyncArrowHead)
    assert ce.CoverCallExpressionAndAsyncArrowHead == "CoverCallExpressionAndAsyncArrowHead"
    assert lexer.pos == 1
    mocks["cce"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_CallExpression_02(mocker, context):
    # CallExpression : SuperCall
    # Sample source text : "super()"
    lexer = Lexer([SC_REPLACEMENT])
    mocks = CallExpression_mocks(mocker)

    ce = e.parse_CallExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ce, e.P2_CallExpression_SuperCall)
    assert ce.SuperCall == "SuperCall"
    assert lexer.pos == 1
    mocks["sc"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_CallExpression_03(mocker, context):
    # CallExpression : CallExpression Arguments
    # Sample source text: "bob()()"
    lexer = Lexer([CCE_REPLACEMENT, ARGS_REPLACEMENT])
    mocks = CallExpression_mocks(mocker)

    ce = e.parse_CallExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ce, e.P2_CallExpression_CallExpression_Arguments)
    assert ce.Arguments == "Arguments"
    ce2 = ce.CallExpression
    assert isinstance(ce2, e.P2_CallExpression_CoverCallExpressionAndAsyncArrowHead)
    assert ce2.CoverCallExpressionAndAsyncArrowHead == "CoverCallExpressionAndAsyncArrowHead"
    mocks["args"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert lexer.pos == 2


def test_parse_CallExpression_04(mocker, context):
    # CallExpression : CallExpression [ Expression ]
    # Sample source text: "bob()[3]"
    lexer = Lexer([CCE_REPLACEMENT, LBRACKET, EXPRESSION, RBRACKET])
    mocks = CallExpression_mocks(mocker)

    ce = e.parse_CallExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ce, e.P2_CallExpression_CallExpression_LBRACKET_Expression_RBRACKET)
    assert ce.Expression == "Expression"
    ce2 = ce.CallExpression
    assert isinstance(ce2, e.P2_CallExpression_CoverCallExpressionAndAsyncArrowHead)
    assert ce2.CoverCallExpressionAndAsyncArrowHead == "CoverCallExpressionAndAsyncArrowHead"
    mocks["exp"].assert_called_with(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg")
    assert lexer.pos == 4


def test_parse_CallExpression_05(mocker, context):
    # CallExpression : CallExpression . IdentifierName
    # Sample source text: "bob().charlie"
    lexer = Lexer([CCE_REPLACEMENT, PERIOD, ICHARLIE])
    CallExpression_mocks(mocker)

    ce = e.parse_CallExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ce, e.P2_CallExpression_CallExpression_PERIOD_IdentifierName)
    assert ce.IdentifierName == ICHARLIE
    ce2 = ce.CallExpression
    assert isinstance(ce2, e.P2_CallExpression_CoverCallExpressionAndAsyncArrowHead)
    assert ce2.CoverCallExpressionAndAsyncArrowHead == "CoverCallExpressionAndAsyncArrowHead"
    assert lexer.pos == 3


def test_parse_CallExpression_06(mocker, context):
    # CallExpression : CallExpression TemplateLiteral
    # Sample source text: "bob()`a`"
    lexer = Lexer([CCE_REPLACEMENT, TL_REPLACEMENT])
    mocks = CallExpression_mocks(mocker)

    ce = e.parse_CallExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ce, e.P2_CallExpression_CallExpression_TemplateLiteral)
    assert ce.TemplateLiteral == "TemplateLiteral"
    mocks["tl"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg", True)
    ce2 = ce.CallExpression
    assert isinstance(ce2, e.P2_CallExpression_CoverCallExpressionAndAsyncArrowHead)
    assert ce2.CoverCallExpressionAndAsyncArrowHead == "CoverCallExpressionAndAsyncArrowHead"
    assert lexer.pos == 2


def test_parse_CallExpression_07(mocker, context):
    # When it all goes bad from the start.
    lexer = Lexer([MATCHES_NONE])
    CallExpression_mocks(mocker)

    ce = e.parse_CallExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert ce is None
    assert lexer.pos == 0


def test_parse_CallExpression_08(mocker, context):
    # A Bad Expression
    lexer = Lexer([CCE_REPLACEMENT, LBRACKET, MATCHES_NONE, RBRACKET])
    CallExpression_mocks(mocker)

    ce = e.parse_CallExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ce, e.P2_CallExpression_CoverCallExpressionAndAsyncArrowHead)
    assert ce.CoverCallExpressionAndAsyncArrowHead == "CoverCallExpressionAndAsyncArrowHead"
    assert lexer.pos == 1


def test_parse_CallExpression_09(mocker, context):
    # A missing right bracket
    lexer = Lexer([CCE_REPLACEMENT, LBRACKET, EXPRESSION, MATCHES_NONE])
    CallExpression_mocks(mocker)

    ce = e.parse_CallExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ce, e.P2_CallExpression_CoverCallExpressionAndAsyncArrowHead)
    assert ce.CoverCallExpressionAndAsyncArrowHead == "CoverCallExpressionAndAsyncArrowHead"
    assert lexer.pos == 1


def test_parse_CallExpression_10(mocker, context):
    lexer = Lexer([CCE_REPLACEMENT, PERIOD, MATCHES_NONE])
    CallExpression_mocks(mocker)

    ce = e.parse_CallExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ce, e.P2_CallExpression_CoverCallExpressionAndAsyncArrowHead)
    assert ce.CoverCallExpressionAndAsyncArrowHead == "CoverCallExpressionAndAsyncArrowHead"
    assert lexer.pos == 1


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


def SuperCall_mocks(mocker):
    return {"args": mocker.patch("ecmascript.ecmascript.parse_Arguments", side_effect=args_sideeffect)}


def test_parse_SuperCall_01(mocker, context):
    # SuperCall : super Arguments
    # Sample source text: "super()"
    lexer = Lexer([SUPER, ARGS_REPLACEMENT])
    mocks = SuperCall_mocks(mocker)

    sc = e.parse_SuperCall(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(sc, e.P2_SuperCall_SUPER_Arguments)
    assert sc.Arguments == "Arguments"
    assert lexer.pos == 2
    mocks["args"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_SuperCall_02(mocker, context):
    # Bad from the start
    lexer = Lexer([MATCHES_NONE])
    SuperCall_mocks(mocker)

    sc = e.parse_SuperCall(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert sc is None
    assert lexer.pos == 0


def test_parse_SuperCall_03(mocker, context):
    # Bad arguments
    lexer = Lexer([SUPER, MATCHES_NONE])
    SuperCall_mocks(mocker)

    sc = e.parse_SuperCall(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert sc is None
    assert lexer.pos == 0


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


def Argument_mocks(mocker):
    return {"al": mocker.patch("ecmascript.ecmascript.parse_ArgumentList", side_effect=al_sideeffect)}


def test_parse_Arguments_01(mocker, context):
    # Arguments : ( )
    lexer = Lexer([LPAREN, RPAREN])
    Argument_mocks(mocker)

    args = e.parse_Arguments(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(args, e.P2_Arguments_LPAREN_RPAREN)
    assert lexer.pos == 2


def test_parse_Arguments_02(mocker, context):
    # Arguments : ( ArgumentList )
    lexer = Lexer([LPAREN, AL_REPLACEMENT, RPAREN])
    mocks = Argument_mocks(mocker)

    args = e.parse_Arguments(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(args, e.P2_Arguments_LPAREN_ArgumentList_RPAREN)
    assert args.ArgumentList == "ArgumentList"
    mocks["al"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert lexer.pos == 3


def test_parse_Arguments_03(mocker, context):
    # Arguments : ( ArgumentList , )
    lexer = Lexer([LPAREN, AL_REPLACEMENT, COMMA, RPAREN])
    mocks = Argument_mocks(mocker)

    args = e.parse_Arguments(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(args, e.P2_Arguments_LPAREN_ArgumentList_COMMA_RPAREN)
    assert args.ArgumentList == "ArgumentList"
    mocks["al"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert lexer.pos == 4


def test_parse_Arguments_04(mocker, context):
    # Bad from the start
    lexer = Lexer([MATCHES_NONE])
    Argument_mocks(mocker)

    args = e.parse_Arguments(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert args is None
    assert lexer.pos == 0


@pytest.mark.parametrize(
    "token_stream",
    [[LPAREN, MATCHES_NONE], [LPAREN, AL_REPLACEMENT, MATCHES_NONE], [LPAREN, AL_REPLACEMENT, COMMA, MATCHES_NONE]],
)
def test_parse_Arguments_05(mocker, context, token_stream):
    # Bad parses with error
    lexer = Lexer(token_stream)
    Argument_mocks(mocker)

    args = e.parse_Arguments(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert args is None
    assert lexer.pos == 0


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


def ArgumentList_mocks(mocker):
    return {"ae": mocker.patch("ecmascript.ecmascript.parse_AssignmentExpression", side_effect=ae_sideeffect)}


def test_parse_ArgumentList_01(mocker, context):
    # ArgumentList : AssignmentExpression
    lexer = Lexer([AE_REPLACEMENT])
    mocks = ArgumentList_mocks(mocker)

    al = e.parse_ArgumentList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(al, e.P2_ArgumentList_AssignmentExpression)
    assert al.AssignmentExpression == "AssignmentExpression"
    assert lexer.pos == 1
    mocks["ae"].assert_called_with(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg")


def test_parse_ArgumentList_02(mocker, context):
    # ArgumentList : ... AssignmentExpression
    lexer = Lexer([DOTDOTDOT, AE_REPLACEMENT])
    mocks = ArgumentList_mocks(mocker)

    al = e.parse_ArgumentList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(al, e.P2_ArgumentList_DOTDOTDOT_AssignmentExpression)
    assert al.AssignmentExpression == "AssignmentExpression"
    assert lexer.pos == 2
    mocks["ae"].assert_called_with(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg")


def test_parse_ArgumentList_03(mocker, context):
    # ArgumentList : ArgumentList , AssignmentExpression
    lexer = Lexer([AE_REPLACEMENT, COMMA, AE_REPLACEMENT])
    mocks = ArgumentList_mocks(mocker)

    al = e.parse_ArgumentList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(al, e.P2_ArgumentList_ArgumentList_COMMA_AssignmentExpression)
    assert al.AssignmentExpression == "AssignmentExpression"
    assert lexer.pos == 3
    mocks["ae"].assert_called_with(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg")
    al2 = al.ArgumentList
    assert isinstance(al2, e.P2_ArgumentList_AssignmentExpression)


def test_parse_ArgumentList_04(mocker, context):
    # ArgumentList : ArgumentList , ... AssignmentExpression
    lexer = Lexer([DOTDOTDOT, AE_REPLACEMENT, COMMA, DOTDOTDOT, AE_REPLACEMENT])
    mocks = ArgumentList_mocks(mocker)

    al = e.parse_ArgumentList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(al, e.P2_ArgumentList_ArgumentList_COMMA_DOTDOTDOT_AssignmentExpression)
    assert al.AssignmentExpression == "AssignmentExpression"
    assert lexer.pos == 5
    mocks["ae"].assert_called_with(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg")
    al2 = al.ArgumentList
    assert isinstance(al2, e.P2_ArgumentList_DOTDOTDOT_AssignmentExpression)


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE], [DOTDOTDOT, MATCHES_NONE]])
def test_parse_ArgumentList_05(mocker, context, token_stream):
    # Bad from the start, or ... bad (i.e. the errors that don't consume any tokens)
    lexer = Lexer(token_stream)
    ArgumentList_mocks(mocker)

    al = e.parse_ArgumentList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert al is None
    assert lexer.pos == 0


@pytest.mark.parametrize("token_stream", [[AE_REPLACEMENT, COMMA], [AE_REPLACEMENT, COMMA, DOTDOTDOT, MATCHES_NONE]])
def test_parse_ArgumentList_06(mocker, context, token_stream):
    # Stopping during recursion: Make sure a trailing comma stays unparsed, and handle garbage after ...
    lexer = Lexer(token_stream)
    ArgumentList_mocks(mocker)

    al = e.parse_ArgumentList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(al, e.P2_ArgumentList_AssignmentExpression)
    assert lexer.pos == 1


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


def LeftHandSideExpression_mocks(mocker):
    return {
        "ne": mocker.patch("ecmascript.ecmascript.parse_NewExpression", side_effect=ne_sideeffect),
        "ce": mocker.patch("ecmascript.ecmascript.parse_CallExpression", side_effect=ce_sideeffect),
    }


def test_parse_LeftHandSideExpression_01(mocker, context):
    # LeftHandSideExpression : NewExpression
    lexer = Lexer([NE_REPLACEMENT])
    mocks = LeftHandSideExpression_mocks(mocker)

    lhs = e.parse_LeftHandSideExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(lhs, e.P2_LeftHandSideExpression_NewExpression)
    assert lhs.NewExpression == "NewExpression"
    assert lexer.pos == 1
    mocks["ne"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_LeftHandSideExpression_02(mocker, context):
    # LeftHandSideExpression : CallExpression
    lexer = Lexer([CE_REPLACEMENT])
    mocks = LeftHandSideExpression_mocks(mocker)

    lhs = e.parse_LeftHandSideExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(lhs, e.P2_LeftHandSideExpression_CallExpression)
    assert lhs.CallExpression == "CallExpression"
    assert lexer.pos == 1
    mocks["ce"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_LeftHandSideExpression_03(mocker, context):
    # Garbage
    lexer = Lexer([MATCHES_NONE])
    LeftHandSideExpression_mocks(mocker)

    lhs = e.parse_LeftHandSideExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert lhs is None
    assert lexer.pos == 0


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


def CallMemberExpression_mocks(mocker):
    return {
        "me": mocker.patch("ecmascript.ecmascript.parse_MemberExpression", side_effect=me_sideeffect),
        "args": mocker.patch("ecmascript.ecmascript.parse_Arguments", side_effect=args_sideeffect),
    }


def test_parse_CallMemberExpression_01(mocker, context):
    # CallMemberExpression : MemberExpression Arguments
    lexer = Lexer([ME_REPLACEMENT, ARGS_REPLACEMENT])
    mocks = CallMemberExpression_mocks(mocker)

    cme = e.parse_CallMemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(cme, e.P2_CallMemberExpression_MemberExpression_Arguments)
    assert cme.MemberExpression == "MemberExpression"
    assert cme.Arguments == "Arguments"
    assert lexer.pos == 2
    mocks["me"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    mocks["args"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[MATCHES_NONE], [ME_REPLACEMENT, MATCHES_NONE]])
def test_parse_CallMemberExpression_02(mocker, context, token_stream):
    # Error cases
    lexer = Lexer(token_stream)
    CallMemberExpression_mocks(mocker)

    cme = e.parse_CallMemberExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert cme is None
    assert lexer.pos == 0


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
    upe = e.P2_UpdateExpression(context, "StrictArg", ["child"])
    assert upe.name == "UpdateExpression"
    assert upe.context == context
    assert upe.children == ["child"]


def test_P2_UpdateExpression_LeftHandSideExpression_init(context):
    upe = e.P2_UpdateExpression_LeftHandSideExpression(context, "StrictArg", ["lhs"])
    assert upe.name == "UpdateExpression"
    assert upe.LeftHandSideExpression == "lhs"


def test_P2_UpdateExpression_LeftHandSideExpression_PLUSPLUS_init(context):
    upe = e.P2_UpdateExpression_LeftHandSideExpression_PLUSPLUS(context, "StrictArg", ["lhs", "++"])
    assert upe.name == "UpdateExpression"
    assert upe.LeftHandSideExpression == "lhs"


def test_P2_UpdateExpression_LeftHandSideExpression_MINUSMINUS_init(context):
    upe = e.P2_UpdateExpression_LeftHandSideExpression_MINUSMINUS(context, "StrictArg", ["lhs", "--"])
    assert upe.name == "UpdateExpression"
    assert upe.LeftHandSideExpression == "lhs"


def test_P2_UpdateExpression_PLUSPLUS_UnaryExpression_init(context):
    upe = e.P2_UpdateExpression_PLUSPLUS_UnaryExpression(context, "StrictArg", ["++", "unary"])
    assert upe.name == "UpdateExpression"
    assert upe.UnaryExpression == "unary"


def test_P2_UpdateExpression_MINUSMINUS_UnaryExpression_init(context):
    upe = e.P2_UpdateExpression_MINUSMINUS_UnaryExpression(context, "StrictArg", ["--", "unary"])
    assert upe.name == "UpdateExpression"
    assert upe.UnaryExpression == "unary"


def UpdateExpression_mocks(mocker):
    return {
        "lhs": mocker.patch("ecmascript.ecmascript.parse_LeftHandSideExpression", side_effect=lhs_sideeffect),
        "une": mocker.patch("ecmascript.ecmascript.parse_UnaryExpression", side_effect=une_sideeffect),
    }


def test_parse_UpdateExpression_01(mocker, context):
    # UpdateExpression : LeftHandSideExpression
    lexer = Lexer([LHS_REPLACEMENT])
    mocks = UpdateExpression_mocks(mocker)

    upe = e.parse_UpdateExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(upe, e.P2_UpdateExpression_LeftHandSideExpression)
    assert upe.LeftHandSideExpression == "LeftHandSideExpression"
    assert lexer.pos == 1
    mocks["lhs"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "token_stream, expected_class",
    [
        ([LHS_REPLACEMENT, PLUSPLUS_NONL], e.P2_UpdateExpression_LeftHandSideExpression_PLUSPLUS),
        ([LHS_REPLACEMENT, MINUSMINUS_NONL], e.P2_UpdateExpression_LeftHandSideExpression_MINUSMINUS),
    ],
)
def test_parse_UpdateExpression_02(mocker, context, token_stream, expected_class):
    # UpdateExpression : LeftHandSideExpression ++
    # UpdateExpression : LeftHandSideExpression --
    lexer = Lexer(token_stream)
    mocks = UpdateExpression_mocks(mocker)

    upe = e.parse_UpdateExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(upe, expected_class)
    assert upe.LeftHandSideExpression == "LeftHandSideExpression"
    assert lexer.pos == 2
    mocks["lhs"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "token_stream, expected_class",
    [
        ([PLUSPLUS, UNE_REPLACEMENT], e.P2_UpdateExpression_PLUSPLUS_UnaryExpression),
        ([MINUSMINUS, UNE_REPLACEMENT], e.P2_UpdateExpression_MINUSMINUS_UnaryExpression),
    ],
)
def test_parse_UpdateExpression_03(mocker, context, token_stream, expected_class):
    # UpdateExpression : ++ UnaryExpression
    # UpdateExpression : -- UnaryExpression
    lexer = Lexer(token_stream)
    mocks = UpdateExpression_mocks(mocker)

    upe = e.parse_UpdateExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(upe, expected_class)
    assert upe.UnaryExpression == "UnaryExpression"
    assert lexer.pos == 2
    mocks["une"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[LHS_REPLACEMENT, PLUSPLUS], [LHS_REPLACEMENT, MINUSMINUS]])
def test_parse_UpdateExpression_04(mocker, context, token_stream):
    # Check the NewLine issues.
    lexer = Lexer(token_stream)
    UpdateExpression_mocks(mocker)

    upe = e.parse_UpdateExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(upe, e.P2_UpdateExpression_LeftHandSideExpression)
    assert upe.LeftHandSideExpression == "LeftHandSideExpression"
    assert lexer.pos == 1


@pytest.mark.parametrize("token_stream", [[MATCHES_NONE], [PLUSPLUS, MATCHES_NONE], [MINUSMINUS, MATCHES_NONE]])
def test_parse_UpdateExpression_05(mocker, context, token_stream):
    # Garbage in all the places.
    lexer = Lexer(token_stream)
    UpdateExpression_mocks(mocker)

    upe = e.parse_UpdateExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert upe is None
    assert lexer.pos == 0


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
    une = e.P2_UnaryExpression(context, "StrictArg", ["child"])
    assert une.name == "UnaryExpression"
    assert une.context == context
    assert une.children == ["child"]


def test_P2_UnaryExpression_UpdateExpression_init(context):
    une = e.P2_UnaryExpression_UpdateExpression(context, "StrictArg", ["Update"])
    assert une.name == "UnaryExpression"
    assert une.UpdateExpression == "Update"


def test_P2_UnaryExpression_DELETE_UnaryExpression_init(context):
    une = e.P2_UnaryExpression_DELETE_UnaryExpression(context, "StrictArg", ["delete", "Unary"])
    assert une.name == "UnaryExpression"
    assert une.UnaryExpression == "Unary"


def test_P2_UnaryExpression_VOID_UnaryExpression_init(context):
    une = e.P2_UnaryExpression_VOID_UnaryExpression(context, "StrictArg", ["void", "Unary"])
    assert une.name == "UnaryExpression"
    assert une.UnaryExpression == "Unary"


def test_P2_UnaryExpression_TYPEOF_UnaryExpression_init(context):
    une = e.P2_UnaryExpression_TYPEOF_UnaryExpression(context, "StrictArg", ["typeof", "Unary"])
    assert une.name == "UnaryExpression"
    assert une.UnaryExpression == "Unary"


def test_P2_UnaryExpression_PLUS_UnaryExpression_init(context):
    une = e.P2_UnaryExpression_PLUS_UnaryExpression(context, "StrictArg", ["+", "Unary"])
    assert une.name == "UnaryExpression"
    assert une.UnaryExpression == "Unary"


def test_P2_UnaryExpression_MINUS_UnaryExpression_init(context):
    une = e.P2_UnaryExpression_MINUS_UnaryExpression(context, "StrictArg", ["-", "Unary"])
    assert une.name == "UnaryExpression"
    assert une.UnaryExpression == "Unary"


def test_P2_UnaryExpression_TILDE_UnaryExpression_init(context):
    une = e.P2_UnaryExpression_TILDE_UnaryExpression(context, "StrictArg", ["~", "Unary"])
    assert une.name == "UnaryExpression"
    assert une.UnaryExpression == "Unary"


def test_P2_UnaryExpression_BANG_UnaryExpression_init(context):
    une = e.P2_UnaryExpression_BANG_UnaryExpression(context, "StrictArg", ["!", "Unary"])
    assert une.name == "UnaryExpression"
    assert une.UnaryExpression == "Unary"


def test_P2_UnaryExpression_AwaitExpression_init(context):
    une = e.P2_UnaryExpression_AwaitExpression(context, "StrictArg", ["AwaitExpression"])
    assert une.name == "UnaryExpression"
    assert une.AwaitExpression == "AwaitExpression"


def UnaryExpression_mocks(mocker):
    return {
        "upe": mocker.patch("ecmascript.ecmascript.parse_UpdateExpression", side_effect=upe_sideeffect),
        "awe": mocker.patch("ecmascript.ecmascript.parse_AwaitExpression", side_effect=awe_sideeffect),
    }


@pytest.mark.parametrize("Await", [True, False])
def test_parse_UnaryExpression_01(mocker, context, Await):
    # UnaryExpression : UpdateExpression
    lexer = Lexer([UPE_REPLACEMENT])
    mocks = UnaryExpression_mocks(mocker)

    une = e.parse_UnaryExpression(context, lexer, "StrictArg", "YieldArg", Await)
    assert isinstance(une, e.P2_UnaryExpression_UpdateExpression)
    assert une.UpdateExpression == "UpdateExpression"
    assert lexer.pos == 1
    mocks["upe"].assert_called_with(context, lexer, "StrictArg", "YieldArg", Await)


@pytest.mark.parametrize("Await", [True, False])
@pytest.mark.parametrize(
    "token_stream, expected_class",
    [
        ([DELETE, UPE_REPLACEMENT], e.P2_UnaryExpression_DELETE_UnaryExpression),
        ([VOID, UPE_REPLACEMENT], e.P2_UnaryExpression_VOID_UnaryExpression),
        ([TYPEOF, UPE_REPLACEMENT], e.P2_UnaryExpression_TYPEOF_UnaryExpression),
        ([PLUS, UPE_REPLACEMENT], e.P2_UnaryExpression_PLUS_UnaryExpression),
        ([MINUS, UPE_REPLACEMENT], e.P2_UnaryExpression_MINUS_UnaryExpression),
        ([TILDE, UPE_REPLACEMENT], e.P2_UnaryExpression_TILDE_UnaryExpression),
        ([BANG, UPE_REPLACEMENT], e.P2_UnaryExpression_BANG_UnaryExpression),
    ],
)
def test_parse_UnaryExpression_02(mocker, context, Await, token_stream, expected_class):
    # UnaryExpression : delete UnaryExpression
    # UnaryExpression : void UnaryExpression
    # UnaryExpression : typeof UnaryExpression
    # UnaryExpression : + UnaryExpression
    # UnaryExpression : - UnaryExpression
    # UnaryExpression : ~ UnaryExpression
    # UnaryExpression : ! UnaryExpression
    lexer = Lexer(token_stream)
    UnaryExpression_mocks(mocker)

    une = e.parse_UnaryExpression(context, lexer, "StrictArg", "YieldArg", Await)
    assert isinstance(une, expected_class)
    assert lexer.pos == 2
    une2 = une.UnaryExpression
    assert isinstance(une2, e.P2_UnaryExpression_UpdateExpression)


def test_parse_UnaryExpression_03(mocker, context):
    # UnaryExpression : AwaitExpression  (Await Enabled)
    lexer = Lexer([AWE_REPLACEMENT])
    mocks = UnaryExpression_mocks(mocker)

    une = e.parse_UnaryExpression(context, lexer, "StrictArg", "YieldArg", True)
    assert isinstance(une, e.P2_UnaryExpression_AwaitExpression)
    assert une.AwaitExpression == "AwaitExpression"
    assert lexer.pos == 1
    mocks["awe"].assert_called_with(context, lexer, "StrictArg", "YieldArg")


def test_parse_UnaryExpression_04(mocker, context):
    # UnaryExpression : AwaitExpression  (Await Disabled)
    lexer = Lexer([AWE_REPLACEMENT])
    UnaryExpression_mocks(mocker)

    une = e.parse_UnaryExpression(context, lexer, "StrictArg", "YieldArg", False)
    assert une is None
    assert lexer.pos == 0


@pytest.mark.parametrize("Await", [True, False])
@pytest.mark.parametrize(
    "token_stream",
    [
        [],
        [MATCHES_NONE],
        [DELETE, MATCHES_NONE],
        [VOID, MATCHES_NONE],
        [TYPEOF, MATCHES_NONE],
        [PLUS, MATCHES_NONE],
        [MINUS, MATCHES_NONE],
        [TILDE, MATCHES_NONE],
        [BANG, MATCHES_NONE],
    ],
)
def test_parse_UnaryExpression_05(mocker, context, token_stream, Await):
    # All the Evil.
    lexer = Lexer(token_stream)
    UnaryExpression_mocks(mocker)

    une = e.parse_UnaryExpression(context, lexer, "StrictArg", "YieldArg", Await)
    assert une is None
    assert lexer.pos == 0


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
    ee = e.P2_ExponentiationExpression(context, "StrictArg", ["child"])
    assert ee.name == "ExponentiationExpression"
    assert ee.context == context
    assert ee.children == ["child"]


def test_P2_ExponentiationExpression_UnaryExpression_init(context):
    ee = e.P2_ExponentiationExpression_UnaryExpression(context, "StrictArg", ["child"])
    assert ee.name == "ExponentiationExpression"
    assert ee.UnaryExpression == "child"


def test_P2_ExponentiationExpression_UpdateExpression_STARSTAR_ExponentiationExpression_init(context):
    ee = e.P2_ExponentiationExpression_UpdateExpression_STARSTAR_ExponentiationExpression(
        context, "StrictArg", ["upe", "**", "ee"]
    )
    assert ee.name == "ExponentiationExpression"
    assert ee.UpdateExpression == "upe"
    assert ee.ExponentiationExpression == "ee"


def ExponentiationExpression_mocks(mocker):
    return {
        "une": mocker.patch("ecmascript.ecmascript.parse_UnaryExpression", side_effect=une_sideeffect),
        "upe": mocker.patch("ecmascript.ecmascript.parse_UpdateExpression", side_effect=upe_sideeffect),
    }


def test_parse_ExponentiationExpression_UnaryExpression(mocker, context):
    # ExponentiationExpression : UnaryExpression
    lexer = Lexer([UNE_REPLACEMENT])
    mocks = ExponentiationExpression_mocks(mocker)

    ee = e.parse_ExponentiationExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ee, e.P2_ExponentiationExpression_UnaryExpression)
    assert ee.UnaryExpression == "UnaryExpression"
    assert lexer.pos == 1
    mocks["une"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_ExponentiationExpression_STARSTAR(mocker, context):
    # ExponentiationExpression : UpdateExpression ** ExponentiationExpression
    lexer = Lexer([UPE_REPLACEMENT, STARSTAR, UNE_REPLACEMENT])
    mocks = ExponentiationExpression_mocks(mocker)

    ee = e.parse_ExponentiationExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ee, e.P2_ExponentiationExpression_UpdateExpression_STARSTAR_ExponentiationExpression)
    assert ee.UpdateExpression == "UpdateExpression"
    assert lexer.pos == 3
    ee2 = ee.ExponentiationExpression
    assert isinstance(ee2, e.P2_ExponentiationExpression_UnaryExpression)
    mocks["upe"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "token_stream", [[], [MATCHES_NONE], [UPE_REPLACEMENT, MATCHES_NONE], [UPE_REPLACEMENT, STARSTAR, MATCHES_NONE]]
)
def test_parse_ExponentiationExpression_errs(mocker, context, token_stream):
    # Syntax errors
    lexer = Lexer(token_stream)
    ExponentiationExpression_mocks(mocker)

    ee = e.parse_ExponentiationExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert ee is None
    assert lexer.pos == 0


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
    me = e.P2_MultiplicativeExpression(context, "StrictArg", ["child"])
    assert me.name == "MultiplicativeExpression"
    assert me.context == context
    assert me.children == ["child"]


def test_P2_MultiplicativeExpression_ExponentiationExpression_init(context):
    me = e.P2_MultiplicativeExpression_ExponentiationExpression(context, "StrictArg", ["child"])
    assert me.name == "MultiplicativeExpression"
    assert me.ExponentiationExpression == "child"


def test_P2_MultiplicativeExpression_MultiplicativeExpression_MultiplicativeOperator_ExponentiationExpression_init(
    context,
):
    me = e.P2_MultiplicativeExpression_MultiplicativeExpression_MultiplicativeOperator_ExponentiationExpression(
        context, "StrictArg", ["p1", "%", "p2"]
    )
    assert me.name == "MultiplicativeExpression"
    assert me.MultiplicativeExpression == "p1"
    assert me.MultiplicativeOperator == "%"
    assert me.ExponentiationExpression == "p2"


def MultiplicativeExpression_mocks(mocker):
    return {
        "ee": mocker.patch("ecmascript.ecmascript.parse_ExponentiationExpression", side_effect=ee_sideeffect),
        "mo": mocker.patch("ecmascript.ecmascript.parse_MultiplicativeOperator", side_effect=mo_sideeffect),
    }


def test_parse_MultiplicativeExpression_01(mocker, context):
    # MultiplicativeExpression : ExponentiationExpression
    lexer = Lexer([EE_REPLACEMENT])
    mocks = MultiplicativeExpression_mocks(mocker)

    me = e.parse_MultiplicativeExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(me, e.P2_MultiplicativeExpression_ExponentiationExpression)
    assert me.ExponentiationExpression == "ExponentiationExpression"
    assert lexer.pos == 1
    mocks["ee"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_MultiplicativeExpression_02(mocker, context):
    # MultiplicativeExpression : MultiplicativeExpression MultiplicativeOperator ExponentiationExpression
    lexer = Lexer([EE_REPLACEMENT, MO_REPLACEMENT, EE_REPLACEMENT])
    mocks = MultiplicativeExpression_mocks(mocker)

    me = e.parse_MultiplicativeExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(
        me, e.P2_MultiplicativeExpression_MultiplicativeExpression_MultiplicativeOperator_ExponentiationExpression
    )
    assert me.MultiplicativeOperator == "MultiplicativeOperator"
    assert me.ExponentiationExpression == "ExponentiationExpression"
    me2 = me.MultiplicativeExpression
    assert isinstance(me2, e.P2_MultiplicativeExpression_ExponentiationExpression)
    assert lexer.pos == 3
    mocks["mo"].assert_called_with(context, lexer, "StrictArg")
    mocks["ee"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_MultiplicativeExpression_03(mocker, context, token_stream):
    # syntax errors (not-recursive)
    lexer = Lexer(token_stream)
    MultiplicativeExpression_mocks(mocker)

    me = e.parse_MultiplicativeExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert me is None
    assert lexer.pos == 0


@pytest.mark.parametrize(
    "token_stream", [[EE_REPLACEMENT, MATCHES_NONE], [EE_REPLACEMENT, MO_REPLACEMENT, MATCHES_NONE]]
)
def test_parse_MultiplicativeExpression_04(mocker, context, token_stream):
    # recursive syntax errors
    lexer = Lexer(token_stream)
    MultiplicativeExpression_mocks(mocker)

    me = e.parse_MultiplicativeExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(me, e.P2_MultiplicativeExpression_ExponentiationExpression)
    assert lexer.pos == 1


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
    mo = e.P2_MultiplicativeOperator(context, "StrictArg", ["child"])
    assert mo.name == "MultiplicativeOperator"
    assert mo.context == context
    assert mo.children == ["child"]


def test_P2_MultiplicativeOperator_MULT_init(context):
    mo = e.P2_MultiplicativeOperator_MULT(context, "StrictArg", ["child"])
    assert mo.name == "MultiplicativeOperator"


def test_P2_MultiplicativeOperator_DIV_init(context):
    mo = e.P2_MultiplicativeOperator_DIV(context, "StrictArg", ["child"])
    assert mo.name == "MultiplicativeOperator"


def test_P2_MultiplicativeOperator_MOD_init(context):
    mo = e.P2_MultiplicativeOperator_MOD(context, "StrictArg", ["child"])
    assert mo.name == "MultiplicativeOperator"


@pytest.mark.parametrize(
    "token_stream, expected_class",
    [
        ([STAR], e.P2_MultiplicativeOperator_MULT),
        ([SLASH], e.P2_MultiplicativeOperator_DIV),
        ([PERCENT], e.P2_MultiplicativeOperator_MOD),
    ],
)
def test_parse_MultiplicativeOperator_01(context, token_stream, expected_class):
    lexer = Lexer(token_stream)

    mo = e.parse_MultiplicativeOperator(context, lexer, "StrictArg")
    assert isinstance(mo, expected_class)
    assert lexer.pos == 1


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_MultiplicativeOperator_02(context, token_stream):
    lexer = Lexer(token_stream)

    mo = e.parse_MultiplicativeOperator(context, lexer, "StrictArg")
    assert mo is None
    assert lexer.pos == 0


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
    ae = e.P2_AdditiveExpression(context, "StrictArg", ["child"])
    assert ae.name == "AdditiveExpression"
    assert ae.context == context
    assert ae.children == ["child"]
    assert ae.strict == "StrictArg"


def test_P2_AdditiveExpression_MultiplicativeExpression_init(context):
    ae = e.P2_AdditiveExpression_MultiplicativeExpression(context, "StrictArg", ["child"])
    assert ae.name == "AdditiveExpression"
    assert ae.MultiplicativeExpression == "child"


def test_P2_AdditiveExpression_AdditiveExpression_PLUS_MultiplicativeExpression_init(context):
    ae = e.P2_AdditiveExpression_AdditiveExpression_PLUS_MultiplicativeExpression(
        context, "StrictArg", ["child", "+", "alice"]
    )
    assert ae.name == "AdditiveExpression"
    assert ae.AdditiveExpression == "child"
    assert ae.MultiplicativeExpression == "alice"


def test_P2_AdditiveExpression_AdditiveExpression_MINUS_MultiplicativeExpression_init(context):
    ae = e.P2_AdditiveExpression_AdditiveExpression_MINUS_MultiplicativeExpression(
        context, "StrictArg", ["child", "-", "alice"]
    )
    assert ae.name == "AdditiveExpression"
    assert ae.AdditiveExpression == "child"
    assert ae.MultiplicativeExpression == "alice"


def AdditiveExpression_mocks(mocker):
    return {
        "mult": mocker.patch("ecmascript.ecmascript.parse_MultiplicativeExpression", side_effect=mult_sideeffect)
    }


def test_parse_AdditiveExpression_01(mocker, context):
    # AdditiveExpression : MultiplicativeExpression
    lexer = Lexer([MULT_REPLACEMENT])
    mocks = AdditiveExpression_mocks(mocker)

    ae = e.parse_AdditiveExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ae, e.P2_AdditiveExpression_MultiplicativeExpression)
    assert ae.MultiplicativeExpression == "MultiplicativeExpression"
    assert lexer.pos == 1
    mocks["mult"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token", [PLUS, MINUS])
def test_parse_AdditiveExpression_02(mocker, context, token):
    # AdditiveExpression : AdditiveExpression + MultiplicativeExpression
    # AdditiveExpression : AdditiveExpression - MultiplicativeExpression
    lexer = Lexer([MULT_REPLACEMENT, token, MULT_REPLACEMENT])
    mocks = AdditiveExpression_mocks(mocker)
    expected_class = {
        "+": e.P2_AdditiveExpression_AdditiveExpression_PLUS_MultiplicativeExpression,
        "-": e.P2_AdditiveExpression_AdditiveExpression_MINUS_MultiplicativeExpression,
    }

    ae = e.parse_AdditiveExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ae, expected_class[token.type])
    assert ae.MultiplicativeExpression == "MultiplicativeExpression"
    assert lexer.pos == 3
    mocks["mult"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    ae2 = ae.AdditiveExpression
    assert isinstance(ae2, e.P2_AdditiveExpression_MultiplicativeExpression)


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_AdditiveExpression_03(mocker, context, token_stream):
    # non-recursive syntax errors
    lexer = Lexer(token_stream)
    AdditiveExpression_mocks(mocker)

    ae = e.parse_AdditiveExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert ae is None
    assert lexer.pos == 0


@pytest.mark.parametrize(
    "token_stream", [[MULT_REPLACEMENT, PLUS, MATCHES_NONE], [MULT_REPLACEMENT, MINUS, MATCHES_NONE]]
)
def test_parse_AdditiveExpression_04(mocker, context, token_stream):
    # recursive syntax errors
    lexer = Lexer(token_stream)
    AdditiveExpression_mocks(mocker)

    ae = e.parse_AdditiveExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ae, e.P2_AdditiveExpression_MultiplicativeExpression)
    assert lexer.pos == 1


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
    se = e.P2_ShiftExpression(context, "StrictArg", ["child"])
    assert se.name == "ShiftExpression"
    assert se.context == context
    assert se.children == ["child"]


def test_P2_ShiftExpression_AdditiveExpression_init(context):
    se = e.P2_ShiftExpression_AdditiveExpression(context, "StrictArg", ["child"])
    assert se.name == "ShiftExpression"
    assert se.AdditiveExpression == "child"


def test_P2_ShiftExpression_ShiftExpression_LTLT_AdditiveExpression_init(context):
    se = e.P2_ShiftExpression_ShiftExpression_LTLT_AdditiveExpression(
        context, "StrictArg", ["ShiExp", "<<", "AddExp"]
    )
    assert se.name == "ShiftExpression"
    assert se.ShiftExpression == "ShiExp"
    assert se.AdditiveExpression == "AddExp"


def test_P2_ShiftExpression_ShiftExpression_GTGT_AdditiveExpression_init(context):
    se = e.P2_ShiftExpression_ShiftExpression_GTGT_AdditiveExpression(
        context, "StrictArg", ["ShiExp", ">>", "AddExp"]
    )
    assert se.name == "ShiftExpression"
    assert se.ShiftExpression == "ShiExp"
    assert se.AdditiveExpression == "AddExp"


def test_P2_ShiftExpression_ShiftExpression_GTGTGT_AdditiveExpression_init(context):
    se = e.P2_ShiftExpression_ShiftExpression_GTGTGT_AdditiveExpression(
        context, "StrictArg", ["ShiExp", ">>>", "AddExp"]
    )
    assert se.name == "ShiftExpression"
    assert se.ShiftExpression == "ShiExp"
    assert se.AdditiveExpression == "AddExp"


def ShiftExpression_mocks(mocker):
    return {
        "AdditiveExpression": mocker.patch(
            "ecmascript.ecmascript.parse_AdditiveExpression", side_effect=adds_sideeffect
        )
    }


def test_parse_ShiftExpression_01(mocker, context):
    # ShiftExpression : AdditiveExpression
    lexer = Lexer([ADDS_REPLACEMENT])
    mocks = ShiftExpression_mocks(mocker)

    se = e.parse_ShiftExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(se, e.P2_ShiftExpression_AdditiveExpression)
    assert se.AdditiveExpression == "AdditiveExpression"
    assert lexer.pos == 1
    mocks["AdditiveExpression"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "op_token, expected_class",
    [
        (LTLT, e.P2_ShiftExpression_ShiftExpression_LTLT_AdditiveExpression),
        (GTGT, e.P2_ShiftExpression_ShiftExpression_GTGT_AdditiveExpression),
        (GTGTGT, e.P2_ShiftExpression_ShiftExpression_GTGTGT_AdditiveExpression),
    ],
)
def test_parse_ShiftExpression_02(mocker, context, op_token, expected_class):
    # ShiftExpression : ShiftExpression << AdditiveExpression
    # ShiftExpression : ShiftExpression >> AdditiveExpression
    # ShiftExpression : ShiftExpression >>> AdditiveExpression
    lexer = Lexer([ADDS_REPLACEMENT, op_token, ADDS_REPLACEMENT])
    mocks = ShiftExpression_mocks(mocker)

    se = e.parse_ShiftExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(se, expected_class)
    assert se.AdditiveExpression == "AdditiveExpression"
    assert lexer.pos == 3
    mocks["AdditiveExpression"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    se2 = se.ShiftExpression
    assert isinstance(se2, e.P2_ShiftExpression_AdditiveExpression)


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_ShiftExpression_03(mocker, context, token_stream):
    # Syntax errors (non-recursive)
    lexer = Lexer(token_stream)
    ShiftExpression_mocks(mocker)

    se = e.parse_ShiftExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert se is None
    assert lexer.pos == 0


@pytest.mark.parametrize(
    "token_stream",
    [
        [ADDS_REPLACEMENT, LTLT, MATCHES_NONE],
        [ADDS_REPLACEMENT, GTGT, MATCHES_NONE],
        [ADDS_REPLACEMENT, GTGTGT, MATCHES_NONE],
    ],
)
def test_parse_ShiftExpression_04(mocker, context, token_stream):
    # Syntax errors (recursive)
    lexer = Lexer(token_stream)
    ShiftExpression_mocks(mocker)

    se = e.parse_ShiftExpression(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(se, e.P2_ShiftExpression_AdditiveExpression)
    assert lexer.pos == 1


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
    re = e.P2_RelationalExpression(context, "StrictArg", ["child"])
    assert re.name == "RelationalExpression"
    assert re.context == context
    assert re.children == ["child"]


def test_P2_RelationalExpression_ShiftExpression(context):
    re = e.P2_RelationalExpression_ShiftExpression(context, "StrictArg", ["child"])
    assert re.name == "RelationalExpression"
    assert re.ShiftExpression == "child"


def test_P2_RelationalExpression_RelationalExpression_OP_ShiftExpression(context):
    re = e.P2_RelationalExpression_RelationalExpression_OP_ShiftExpression(
        context, "StrictArg", ["left", "<", "right"]
    )
    assert re.name == "RelationalExpression"
    assert re.RelationalExpression == "left"
    assert re.ShiftExpression == "right"


def test_P2_RelationalExpression_RelationalExpression_LT_ShiftExpression(context):
    re = e.P2_RelationalExpression_RelationalExpression_LT_ShiftExpression(context, "StrictArg", ["child"])
    assert re.name == "RelationalExpression"


def test_P2_RelationalExpression_RelationalExpression_GT_ShiftExpression(context):
    re = e.P2_RelationalExpression_RelationalExpression_GT_ShiftExpression(context, "StrictArg", ["child"])
    assert re.name == "RelationalExpression"


def test_P2_RelationalExpression_RelationalExpression_LE_ShiftExpression(context):
    re = e.P2_RelationalExpression_RelationalExpression_LE_ShiftExpression(context, "StrictArg", ["child"])
    assert re.name == "RelationalExpression"


def test_P2_RelationalExpression_RelationalExpression_GE_ShiftExpression(context):
    re = e.P2_RelationalExpression_RelationalExpression_GE_ShiftExpression(context, "StrictArg", ["child"])
    assert re.name == "RelationalExpression"


def test_P2_RelationalExpression_RelationalExpression_INSTANCEOF_ShiftExpression(context):
    re = e.P2_RelationalExpression_RelationalExpression_INSTANCEOF_ShiftExpression(context, "StrictArg", ["child"])
    assert re.name == "RelationalExpression"


def test_P2_RelationalExpression_RelationalExpression_IN_ShiftExpression(context):
    re = e.P2_RelationalExpression_RelationalExpression_IN_ShiftExpression(context, "StrictArg", ["child"])
    assert re.name == "RelationalExpression"


def RelationalExpression_mocks(mocker):
    return {
        "ShiftExpression": mocker.patch(
            "ecmascript.ecmascript.parse_ShiftExpression", side_effect=shiftexpression_sideeffect
        )
    }


@pytest.mark.parametrize("In", [True, False])
def test_parse_RelationalExpression_01(mocker, context, In):
    # RelationalExpression : ShiftExpression
    lexer = Lexer([SHIFTEXPRESSION])
    mocks = RelationalExpression_mocks(mocker)

    re = e.parse_RelationalExpression(context, lexer, "StrictArg", In, "YieldArg", "AwaitArg")
    assert isinstance(re, e.P2_RelationalExpression_ShiftExpression)
    assert re.ShiftExpression == "ShiftExpression"
    assert lexer.pos == 1
    mocks["ShiftExpression"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("In", [True, False])
@pytest.mark.parametrize(
    "op, expected_class",
    [
        (LT, e.P2_RelationalExpression_RelationalExpression_LT_ShiftExpression),
        (GT, e.P2_RelationalExpression_RelationalExpression_GT_ShiftExpression),
        (LE, e.P2_RelationalExpression_RelationalExpression_LE_ShiftExpression),
        (GE, e.P2_RelationalExpression_RelationalExpression_GE_ShiftExpression),
        (INSTANCEOF, e.P2_RelationalExpression_RelationalExpression_INSTANCEOF_ShiftExpression),
    ],
)
def test_parse_RelationalExpression_02(mocker, context, In, op, expected_class):
    # RelationalExpression : RelationalExpression < ShiftExpression
    # RelationalExpression : RelationalExpression > ShiftExpression
    # RelationalExpression : RelationalExpression <= ShiftExpression
    # RelationalExpression : RelationalExpression >= ShiftExpression
    # RelationalExpression : RelationalExpression instanceof ShiftExpression
    lexer = Lexer([SHIFTEXPRESSION, op, SHIFTEXPRESSION])
    mocks = RelationalExpression_mocks(mocker)

    re = e.parse_RelationalExpression(context, lexer, "StrictArg", In, "YieldArg", "AwaitArg")
    assert isinstance(re, expected_class)
    assert re.ShiftExpression == "ShiftExpression"
    assert lexer.pos == 3
    mocks["ShiftExpression"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    re2 = re.RelationalExpression
    assert isinstance(re2, e.P2_RelationalExpression_ShiftExpression)


def test_parse_RelationalExpression_03(mocker, context):
    # RelationalExpression : RelationalExpression in ShiftExpression (In == False)
    lexer = Lexer([SHIFTEXPRESSION, IN, SHIFTEXPRESSION])
    RelationalExpression_mocks(mocker)

    re = e.parse_RelationalExpression(context, lexer, "StrictArg", False, "YieldArg", "AwaitArg")
    assert isinstance(re, e.P2_RelationalExpression_ShiftExpression)
    assert lexer.pos == 1


def test_parse_RelationalExpression_04(mocker, context):
    # RelationalExpression : RelationalExpression in ShiftExpression (In == True)
    lexer = Lexer([SHIFTEXPRESSION, IN, SHIFTEXPRESSION])
    mocks = RelationalExpression_mocks(mocker)

    re = e.parse_RelationalExpression(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg")
    assert isinstance(re, e.P2_RelationalExpression_RelationalExpression_IN_ShiftExpression)
    assert re.ShiftExpression == "ShiftExpression"
    assert lexer.pos == 3
    mocks["ShiftExpression"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    re2 = re.RelationalExpression
    assert isinstance(re2, e.P2_RelationalExpression_ShiftExpression)


@pytest.mark.parametrize("In", [True, False])
@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_RelationalExpression_05(mocker, context, In, token_stream):
    # Non-recursive syntax errors
    lexer = Lexer(token_stream)
    RelationalExpression_mocks(mocker)

    re = e.parse_RelationalExpression(context, lexer, "StrictArg", In, "YieldArg", "AwaitArg")
    assert re is None
    assert lexer.pos == 0


@pytest.mark.parametrize("In", [True, False])
@pytest.mark.parametrize(
    "token_stream",
    [
        [SHIFTEXPRESSION, MATCHES_NONE],
        [SHIFTEXPRESSION, LT, MATCHES_NONE],
        [SHIFTEXPRESSION, GT, MATCHES_NONE],
        [SHIFTEXPRESSION, LE, MATCHES_NONE],
        [SHIFTEXPRESSION, GE, MATCHES_NONE],
        [SHIFTEXPRESSION, INSTANCEOF, MATCHES_NONE],
        [SHIFTEXPRESSION, IN, MATCHES_NONE],
    ],
)
def test_parse_RelationalExpression_06(mocker, context, In, token_stream):
    # Recursive syntax errors
    lexer = Lexer(token_stream)
    RelationalExpression_mocks(mocker)

    re = e.parse_RelationalExpression(context, lexer, "StrictArg", In, "YieldArg", "AwaitArg")
    assert isinstance(re, e.P2_RelationalExpression_ShiftExpression)
    assert lexer.pos == 1


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
    ee = e.P2_EqualityExpression(context, "StrictArg", ["child"])
    assert ee.name == "EqualityExpression"
    assert ee.context == context
    assert ee.children == ["child"]


def test_P2_EqualityExpression_RelationalExpression_init(context):
    ee = e.P2_EqualityExpression_RelationalExpression(context, "StrictArg", ["child"])
    assert ee.name == "EqualityExpression"
    assert ee.RelationalExpression == "child"


def test_P2_EqualityExpression_EqualityExpression_OP_RelationalExpression_init(context):
    ee = e.P2_EqualityExpression_EqualityExpression_OP_RelationalExpression(
        context, "StrictArg", ["child", "==", "vampire"]
    )
    assert ee.name == "EqualityExpression"
    assert ee.EqualityExpression == "child"
    assert ee.RelationalExpression == "vampire"


def test_P2_EqualityExpression_EqualityExpression_EQEQ_RelationalExpression_init(context):
    ee = e.P2_EqualityExpression_EqualityExpression_EQEQ_RelationalExpression(context, "StrictArg", ["child"])
    assert ee.name == "EqualityExpression"


def test_P2_EqualityExpression_EqualityExpression_BANGEQ_RelationalExpression_init(context):
    ee = e.P2_EqualityExpression_EqualityExpression_BANGEQ_RelationalExpression(context, "StrictArg", ["child"])
    assert ee.name == "EqualityExpression"


def test_P2_EqualityExpression_EqualityExpression_EQEQEQ_RelationalExpression_init(context):
    ee = e.P2_EqualityExpression_EqualityExpression_EQEQEQ_RelationalExpression(context, "StrictArg", ["child"])
    assert ee.name == "EqualityExpression"


def test_P2_EqualityExpression_EqualityExpression_BANGEQEQ_RelationalExpression_init(context):
    ee = e.P2_EqualityExpression_EqualityExpression_BANGEQEQ_RelationalExpression(context, "StrictArg", ["child"])
    assert ee.name == "EqualityExpression"


def EqualityExpression_mocks(mocker):
    return {
        "RelationalExpression": mocker.patch(
            "ecmascript.ecmascript.parse_RelationalExpression", side_effect=relexpression_sideeffect
        )
    }


def test_parse_EqualityExpression_01(mocker, context):
    # EqualityExpression : RelationalExpression
    lexer = Lexer([RELEXPRESSION])
    mocks = EqualityExpression_mocks(mocker)

    ee = e.parse_EqualityExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(ee, e.P2_EqualityExpression_RelationalExpression)
    assert ee.RelationalExpression == "RelationalExpression"
    assert lexer.pos == 1
    mocks["RelationalExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "op, expected_class",
    [
        (EQEQ, e.P2_EqualityExpression_EqualityExpression_EQEQ_RelationalExpression),
        (BANGEQ, e.P2_EqualityExpression_EqualityExpression_BANGEQ_RelationalExpression),
        (EQEQEQ, e.P2_EqualityExpression_EqualityExpression_EQEQEQ_RelationalExpression),
        (BANGEQEQ, e.P2_EqualityExpression_EqualityExpression_BANGEQEQ_RelationalExpression),
    ],
)
def test_parse_EqualityExpression_02(mocker, context, op, expected_class):
    # EqualityExpression : EqualityExpression == RelationalExpression
    # EqualityExpression : EqualityExpression != RelationalExpression
    # EqualityExpression : EqualityExpression === RelationalExpression
    # EqualityExpression : EqualityExpression !== RelationalExpression
    lexer = Lexer([RELEXPRESSION, op, RELEXPRESSION])
    mocks = EqualityExpression_mocks(mocker)

    ee = e.parse_EqualityExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(ee, expected_class)
    assert lexer.pos == 3
    assert ee.RelationalExpression == "RelationalExpression"
    mocks["RelationalExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    ee2 = ee.EqualityExpression
    assert isinstance(ee2, e.P2_EqualityExpression_RelationalExpression)


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_EqualityExpression_03(mocker, context, token_stream):
    # non-recursive syntax errors
    lexer = Lexer(token_stream)
    EqualityExpression_mocks(mocker)

    ee = e.parse_EqualityExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert ee is None
    assert lexer.pos == 0


@pytest.mark.parametrize(
    "token_stream",
    [
        [RELEXPRESSION, MATCHES_NONE],
        [RELEXPRESSION, EQEQ, MATCHES_NONE],
        [RELEXPRESSION, BANGEQ, MATCHES_NONE],
        [RELEXPRESSION, EQEQEQ, MATCHES_NONE],
        [RELEXPRESSION, BANGEQEQ, MATCHES_NONE],
    ],
)
def test_parse_EqualityExpression_04(mocker, context, token_stream):
    # Recursive syntax errors
    lexer = Lexer(token_stream)
    EqualityExpression_mocks(mocker)

    ee = e.parse_EqualityExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(ee, e.P2_EqualityExpression_RelationalExpression)
    assert lexer.pos == 1


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
    bae = e.P2_BitwiseANDExpression(context, "StrictArg", ["child"])
    assert bae.name == "BitwiseANDExpression"
    assert bae.context == context
    assert bae.children == ["child"]


def test_P2_BitwiseANDExpression_EqualityExpression_init(context):
    bae = e.P2_BitwiseANDExpression_EqualityExpression(context, "StrictArg", ["child"])
    assert bae.name == "BitwiseANDExpression"
    assert bae.EqualityExpression == "child"


def test_P2_BitwiseANDExpression_BitwiseANDExpression_AMP_EqualityExpression_init(context):
    bae = e.P2_BitwiseANDExpression_BitwiseANDExpression_AMP_EqualityExpression(
        context, "StrictArg", ["child", "&", "right"]
    )
    assert bae.name == "BitwiseANDExpression"
    assert bae.BitwiseANDExpression == "child"
    assert bae.EqualityExpression == "right"


def BitwiseANDExpression_mocks(mocker):
    return {
        "EqualityExpression": mocker.patch(
            "ecmascript.ecmascript.parse_EqualityExpression", side_effect=equalexpression_sideeffect
        )
    }


def test_parse_BitwiseANDExpression_01(mocker, context):
    # BitwiseANDExpression : EqualityExpression
    lexer = Lexer([EQUALEXPRESSION])
    mocks = BitwiseANDExpression_mocks(mocker)

    bae = e.parse_BitwiseANDExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(bae, e.P2_BitwiseANDExpression_EqualityExpression)
    assert bae.EqualityExpression == "EqualityExpression"
    assert lexer.pos == 1
    mocks["EqualityExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


def test_parse_BitwiseANDExpression_02(mocker, context):
    # BitwiseANDExpression : BitwiseANDExpression & EqualityExpression
    lexer = Lexer([EQUALEXPRESSION, AMP, EQUALEXPRESSION])
    mocks = BitwiseANDExpression_mocks(mocker)

    bae = e.parse_BitwiseANDExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(bae, e.P2_BitwiseANDExpression_BitwiseANDExpression_AMP_EqualityExpression)
    assert bae.EqualityExpression == "EqualityExpression"
    assert lexer.pos == 3
    bae2 = bae.BitwiseANDExpression
    assert isinstance(bae2, e.P2_BitwiseANDExpression_EqualityExpression)
    mocks["EqualityExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_BitwiseANDExpression_03(mocker, context, token_stream):
    # non-recursive syntax errors
    lexer = Lexer(token_stream)
    BitwiseANDExpression_mocks(mocker)

    bae = e.parse_BitwiseANDExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert bae is None
    assert lexer.pos == 0


@pytest.mark.parametrize("token_stream", [[EQUALEXPRESSION, MATCHES_NONE], [EQUALEXPRESSION, AMP, MATCHES_NONE]])
def test_parse_BitwiseANDExpression_04(mocker, context, token_stream):
    # recursive syntax errors
    lexer = Lexer(token_stream)
    BitwiseANDExpression_mocks(mocker)

    bae = e.parse_BitwiseANDExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(bae, e.P2_BitwiseANDExpression_EqualityExpression)
    assert lexer.pos == 1


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
    bxe = e.P2_BitwiseXORExpression(context, "StrictArg", ["child"])
    assert bxe.name == "BitwiseXORExpression"
    assert bxe.context == context
    assert bxe.children == ["child"]


def test_P2_BitwiseXORExpression_BitwiseANDExpression_init(context):
    bxe = e.P2_BitwiseXORExpression_BitwiseANDExpression(context, "StrictArg", ["child"])
    assert bxe.name == "BitwiseXORExpression"
    assert bxe.BitwiseANDExpression == "child"


def test_P2_BitwiseXORExpression_BitwiseXORExpression_CARET_BitwiseANDExpression_init(context):
    bxe = e.P2_BitwiseXORExpression_BitwiseXORExpression_CARET_BitwiseANDExpression(
        context, "StrictArg", ["child", "^", "0x10"]
    )
    assert bxe.name == "BitwiseXORExpression"
    assert bxe.BitwiseXORExpression == "child"
    assert bxe.BitwiseANDExpression == "0x10"


def BitwiseXORExpression_mocks(mocker):
    return {
        "BitwiseANDExpression": mocker.patch(
            "ecmascript.ecmascript.parse_BitwiseANDExpression", side_effect=bae_sideeffect
        )
    }


def test_parse_BitwiseXORExpression_01(mocker, context):
    # BitwiseXORExpression : BitwiseANDExpression
    lexer = Lexer([BAE_REPLACEMENT])
    mocks = BitwiseXORExpression_mocks(mocker)

    bxe = e.parse_BitwiseXORExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(bxe, e.P2_BitwiseXORExpression_BitwiseANDExpression)
    assert bxe.BitwiseANDExpression == "BitwiseANDExpression"
    assert lexer.pos == 1
    mocks["BitwiseANDExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


def test_parse_BitwiseXORExpression_02(mocker, context):
    # BitwiseXORExpression : BitwiseXORExpression ^ BitwiseANDExpression
    lexer = Lexer([BAE_REPLACEMENT, CARET, BAE_REPLACEMENT])
    mocks = BitwiseXORExpression_mocks(mocker)

    bxe = e.parse_BitwiseXORExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(bxe, e.P2_BitwiseXORExpression_BitwiseXORExpression_CARET_BitwiseANDExpression)
    assert bxe.BitwiseANDExpression == "BitwiseANDExpression"
    assert lexer.pos == 3
    bxe2 = bxe.BitwiseXORExpression
    assert isinstance(bxe2, e.P2_BitwiseXORExpression_BitwiseANDExpression)
    mocks["BitwiseANDExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_BitwiseXORExpression_03(mocker, context, token_stream):
    # non-recursive syntax errors
    lexer = Lexer(token_stream)
    BitwiseXORExpression_mocks(mocker)

    bxe = e.parse_BitwiseXORExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert bxe is None
    assert lexer.pos == 0


@pytest.mark.parametrize("token_stream", [[BAE_REPLACEMENT, MATCHES_NONE], [BAE_REPLACEMENT, CARET, MATCHES_NONE]])
def test_parse_BitwiseXORExpression_04(mocker, context, token_stream):
    # recursive syntax errors
    lexer = Lexer(token_stream)
    BitwiseXORExpression_mocks(mocker)

    bxe = e.parse_BitwiseXORExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(bxe, e.P2_BitwiseXORExpression_BitwiseANDExpression)
    assert lexer.pos == 1


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
    boe = e.P2_BitwiseORExpression(context, "StrictArg", ["child"])
    assert boe.name == "BitwiseORExpression"
    assert boe.context == context
    assert boe.children == ["child"]


def test_P2_BitwiseORExpression_BitwiseXORExpression_init(context):
    boe = e.P2_BitwiseORExpression_BitwiseXORExpression(context, "StrictArg", ["child"])
    assert boe.name == "BitwiseORExpression"
    assert boe.BitwiseXORExpression == "child"


def test_P2_BitwiseORExpression_BitwiseORExpression_PIPE_BitwiseXORExpression_init(context):
    boe = e.P2_BitwiseORExpression_BitwiseORExpression_PIPE_BitwiseXORExpression(
        context, "StrictArg", ["child", "|", "bxe"]
    )
    assert boe.name == "BitwiseORExpression"
    assert boe.BitwiseORExpression == "child"
    assert boe.BitwiseXORExpression == "bxe"


def BitwiseORExpression_mocks(mocker):
    return {
        "BitwiseXORExpression": mocker.patch(
            "ecmascript.ecmascript.parse_BitwiseXORExpression", side_effect=bxe_sideeffect
        )
    }


def test_parse_BitwiseORExpression_01(mocker, context):
    # BitwiseORExpression : BitwiseXORExpression
    lexer = Lexer([BXE_REPLACEMENT])
    mocks = BitwiseORExpression_mocks(mocker)

    boe = e.parse_BitwiseORExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(boe, e.P2_BitwiseORExpression_BitwiseXORExpression)
    assert boe.BitwiseXORExpression == "BitwiseXORExpression"
    assert lexer.pos == 1
    mocks["BitwiseXORExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


def test_parse_BitwiseORExpression_02(mocker, context):
    # BitwiseORExpression : BitwiseORExpression | BitwiseXORExpression
    lexer = Lexer([BXE_REPLACEMENT, PIPE, BXE_REPLACEMENT])
    mocks = BitwiseORExpression_mocks(mocker)

    boe = e.parse_BitwiseORExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(boe, e.P2_BitwiseORExpression_BitwiseORExpression_PIPE_BitwiseXORExpression)
    assert boe.BitwiseXORExpression == "BitwiseXORExpression"
    assert lexer.pos == 3
    mocks["BitwiseXORExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    boe2 = boe.BitwiseORExpression
    assert isinstance(boe2, e.P2_BitwiseORExpression_BitwiseXORExpression)


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_BitwiseORExpression_03(mocker, context, token_stream):
    # non-recursive syntax errors
    lexer = Lexer(token_stream)
    BitwiseORExpression_mocks(mocker)

    boe = e.parse_BitwiseORExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert boe is None
    assert lexer.pos == 0


@pytest.mark.parametrize("token_stream", [[BXE_REPLACEMENT, MATCHES_NONE], [BXE_REPLACEMENT, PIPE, MATCHES_NONE]])
def test_parse_BitwiseORExpression_04(mocker, context, token_stream):
    # recursive syntax errors
    lexer = Lexer(token_stream)
    BitwiseORExpression_mocks(mocker)

    boe = e.parse_BitwiseORExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(boe, e.P2_BitwiseORExpression_BitwiseXORExpression)
    assert lexer.pos == 1


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
    lae = e.P2_LogicalANDExpression(context, "StrictArg", ["child"])
    assert lae.name == "LogicalANDExpression"
    assert lae.context == context
    assert lae.children == ["child"]


def test_P2_LogicalANDExpression_BitwiseORExpression_init(context):
    lae = e.P2_LogicalANDExpression_BitwiseORExpression(context, "StrictArg", ["child"])
    assert lae.name == "LogicalANDExpression"
    assert lae.BitwiseORExpression == "child"


def test_P2_LogicalANDExpression_LogicalANDExpression_AMPAMP_BitwiseORExpression_init(context):
    lae = e.P2_LogicalANDExpression_LogicalANDExpression_AMPAMP_BitwiseORExpression(
        context, "StrictArg", ["child", "&&", "lemon"]
    )
    assert lae.name == "LogicalANDExpression"
    assert lae.LogicalANDExpression == "child"
    assert lae.BitwiseORExpression == "lemon"


def LogicalANDExpression_mocks(mocker):
    return {
        "BitwiseORExpression": mocker.patch(
            "ecmascript.ecmascript.parse_BitwiseORExpression", side_effect=boe_sideeffect
        )
    }


def test_parse_LogicalANDExpression_01(mocker, context):
    # LogicalANDExpression : BitwiseORExpression
    lexer = Lexer([BOE_REPLACEMENT])
    mocks = LogicalANDExpression_mocks(mocker)

    lae = e.parse_LogicalANDExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(lae, e.P2_LogicalANDExpression_BitwiseORExpression)
    assert lae.BitwiseORExpression == "BitwiseORExpression"
    assert lexer.pos == 1
    mocks["BitwiseORExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


def test_parse_LogicalANDExpression_02(mocker, context):
    # LogicalANDExpression : LogicalANDExpression && BitwiseORExpression
    lexer = Lexer([BOE_REPLACEMENT, AMPAMP, BOE_REPLACEMENT])
    mocks = LogicalANDExpression_mocks(mocker)

    lae = e.parse_LogicalANDExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(lae, e.P2_LogicalANDExpression_LogicalANDExpression_AMPAMP_BitwiseORExpression)
    assert lae.BitwiseORExpression == "BitwiseORExpression"
    assert lexer.pos == 3
    mocks["BitwiseORExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    lae2 = lae.LogicalANDExpression
    assert isinstance(lae2, e.P2_LogicalANDExpression_BitwiseORExpression)


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_LogicalANDExpression_03(mocker, context, token_stream):
    # non-recursive syntax errors
    lexer = Lexer(token_stream)
    LogicalANDExpression_mocks(mocker)

    lae = e.parse_LogicalANDExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert lae is None
    assert lexer.pos == 0


@pytest.mark.parametrize("token_stream", [[BOE_REPLACEMENT, MATCHES_NONE], [BOE_REPLACEMENT, AMPAMP, MATCHES_NONE]])
def test_parse_LogicalANDExpression_04(mocker, context, token_stream):
    # recursive syntax errors
    lexer = Lexer(token_stream)
    LogicalANDExpression_mocks(mocker)

    lae = e.parse_LogicalANDExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(lae, e.P2_LogicalANDExpression_BitwiseORExpression)
    assert lexer.pos == 1


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
    loe = e.P2_LogicalORExpression(context, "StrictArg", ["child"])
    assert loe.name == "LogicalORExpression"
    assert loe.context == context
    assert loe.children == ["child"]


def test_P2_LogicalORExpression_LogicalANDExpression_init(context):
    loe = e.P2_LogicalORExpression_LogicalANDExpression(context, "StrictArg", ["child"])
    assert loe.name == "LogicalORExpression"
    assert loe.LogicalANDExpression == "child"


def test_P2_LogicalORExpression_LogicalORExpression_PIPEPIPE_LogicalANDExpression_init(context):
    loe = e.P2_LogicalORExpression_LogicalORExpression_PIPEPIPE_LogicalANDExpression(
        context, "StrictArg", ["child", "||", "right"]
    )
    assert loe.name == "LogicalORExpression"
    assert loe.LogicalORExpression == "child"
    assert loe.LogicalANDExpression == "right"


def LogicalORExpression_mocks(mocker):
    return {
        "LogicalANDExpression": mocker.patch(
            "ecmascript.ecmascript.parse_LogicalANDExpression", side_effect=lae_sideeffect
        )
    }


def test_parse_LogicalORExpression_01(mocker, context):
    # LogicalORExpression : LogicalANDExpression
    lexer = Lexer([LAE_REPLACEMENT])
    mocks = LogicalORExpression_mocks(mocker)

    loe = e.parse_LogicalORExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(loe, e.P2_LogicalORExpression_LogicalANDExpression)
    assert loe.LogicalANDExpression == "LogicalANDExpression"
    assert lexer.pos == 1
    mocks["LogicalANDExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


def test_parse_LogicalORExpression_02(mocker, context):
    # LogicalORExpression : LogicalORExpression || LogicalANDExpression
    lexer = Lexer([LAE_REPLACEMENT, PIPEPIPE, LAE_REPLACEMENT])
    mocks = LogicalORExpression_mocks(mocker)

    loe = e.parse_LogicalORExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(loe, e.P2_LogicalORExpression_LogicalORExpression_PIPEPIPE_LogicalANDExpression)
    assert loe.LogicalANDExpression == "LogicalANDExpression"
    assert lexer.pos == 3
    mocks["LogicalANDExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    loe2 = loe.LogicalORExpression
    assert isinstance(loe2, e.P2_LogicalORExpression_LogicalANDExpression)


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_LogicalORExpression_03(mocker, context, token_stream):
    # non-recursive syntax errors
    lexer = Lexer(token_stream)
    LogicalORExpression_mocks(mocker)

    loe = e.parse_LogicalORExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert loe is None
    assert lexer.pos == 0


@pytest.mark.parametrize(
    "token_stream", [[LAE_REPLACEMENT, MATCHES_NONE], [LAE_REPLACEMENT, PIPEPIPE, MATCHES_NONE]]
)
def test_parse_LogicalORExpression_04(mocker, context, token_stream):
    # recursive syntax errors
    lexer = Lexer(token_stream)
    LogicalORExpression_mocks(mocker)

    loe = e.parse_LogicalORExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(loe, e.P2_LogicalORExpression_LogicalANDExpression)
    assert lexer.pos == 1


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
    ce = e.P2_ConditionalExpression(context, "StrictArg", ["child"])
    assert ce.name == "ConditionalExpression"
    assert ce.context == context
    assert ce.children == ["child"]


def test_P2_ConditionalExpression_LogicalORExpression_init(context):
    ce = e.P2_ConditionalExpression_LogicalORExpression(context, "StrictArg", ["child"])
    assert ce.name == "ConditionalExpression"
    assert ce.LogicalORExpression == "child"


def test_P2_ConditionalExpression_LogicalORExpression_QUESTION_AssignmentExpression_COLON_AssignmentExpression_init(
    context,
):
    ce = e.P2_ConditionalExpression_LogicalORExpression_QUESTION_AssignmentExpression_COLON_AssignmentExpression(
        context, "StrictArg", ["child", "?", "true_result", ":", "false_result"]
    )
    assert ce.name == "ConditionalExpression"
    assert ce.LogicalORExpression == "child"
    assert ce.AssignmentExpression1 == "true_result"
    assert ce.AssignmentExpression2 == "false_result"
    assert ce.strict == "StrictArg"


def ConditionalExpression_mocks(mocker):
    return {
        "LogicalORExpression": mocker.patch(
            "ecmascript.ecmascript.parse_LogicalORExpression", side_effect=loe_sideeffect
        ),
        "AssignmentExpression": mocker.patch(
            "ecmascript.ecmascript.parse_AssignmentExpression", side_effect=ae_sideeffect
        ),
    }


def test_parse_ConditionalExpression_01(mocker, context):
    # ConditionalExpression : LogicalORExpression
    lexer = Lexer([LOE_REPLACEMENT])
    mocks = ConditionalExpression_mocks(mocker)

    ce = e.parse_ConditionalExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(ce, e.P2_ConditionalExpression_LogicalORExpression)
    assert ce.LogicalORExpression == "LogicalORExpression"
    assert lexer.pos == 1
    mocks["LogicalORExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


def test_parse_ConditionalExpression_02(mocker, context):
    # ConditionalExpression : LogicalORExpression ? AssignmentExpression : AssignmentExpression
    lexer = Lexer([LOE_REPLACEMENT, QUESTION, AE_REPLACEMENT, COLON, AE_REPLACEMENT])
    mocks = ConditionalExpression_mocks(mocker)

    ce = e.parse_ConditionalExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(
        ce, e.P2_ConditionalExpression_LogicalORExpression_QUESTION_AssignmentExpression_COLON_AssignmentExpression
    )
    assert ce.LogicalORExpression == "LogicalORExpression"
    assert lexer.pos == 5
    assert ce.AssignmentExpression1 == "AssignmentExpression"
    assert ce.AssignmentExpression2 == "AssignmentExpression"
    mocks["LogicalORExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    ae_expected_calls = [
        mocker.call(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg"),
        mocker.call(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg"),
    ]
    assert mocks["AssignmentExpression"].call_args_list == ae_expected_calls


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_ConditionalExpression_03(mocker, context, token_stream):
    # Syntax errors that consume nothing
    lexer = Lexer(token_stream)
    ConditionalExpression_mocks(mocker)

    ce = e.parse_ConditionalExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert ce is None
    assert lexer.pos == 0


@pytest.mark.parametrize(
    "token_stream",
    [
        [LOE_REPLACEMENT, MATCHES_NONE],
        [LOE_REPLACEMENT, QUESTION, MATCHES_NONE],
        [LOE_REPLACEMENT, QUESTION, AE_REPLACEMENT, MATCHES_NONE],
        [LOE_REPLACEMENT, QUESTION, AE_REPLACEMENT, COLON, MATCHES_NONE],
    ],
)
def test_parse_ConditionalExpression_04(context, mocker, token_stream):
    # Syntax errors that leave a LogicalORExpression behind
    lexer = Lexer(token_stream)
    ConditionalExpression_mocks(mocker)
    ce = e.parse_ConditionalExpression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(ce, e.P2_ConditionalExpression_LogicalORExpression)
    assert lexer.pos == 1


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
    ae = e.P2_AssignmentExpression(context, "StrictArg", ["child"])
    assert ae.name == "AssignmentExpression"
    assert ae.context == context
    assert ae.children == ["child"]


def test_P2_AssignmentExpression_ConditionalExpression_init(context):
    ae = e.P2_AssignmentExpression_ConditionalExpression(context, "StrictArg", ["child"])
    assert ae.name == "AssignmentExpression"
    assert ae.ConditionalExpression == "child"


def test_P2_AssignmentExpression_YieldExpression_init(context):
    ae = e.P2_AssignmentExpression_YieldExpression(context, "StrictArg", ["child"])
    assert ae.name == "AssignmentExpression"
    assert ae.YieldExpression == "child"


def test_P2_AssignmentExpression_ArrowFunction_init(context):
    ae = e.P2_AssignmentExpression_ArrowFunction(context, "StrictArg", ["child"])
    assert ae.name == "AssignmentExpression"
    assert ae.ArrowFunction == "child"


def test_P2_AssignmentExpression_AsyncArrowFunction_init(context):
    ae = e.P2_AssignmentExpression_AsyncArrowFunction(context, "StrictArg", ["child"])
    assert ae.name == "AssignmentExpression"
    assert ae.AsyncArrowFunction == "child"


def test_P2_AssignmentExpression_LeftHandSideExpression_EQ_AssignmentExpression_init(context):
    ae = e.P2_AssignmentExpression_LeftHandSideExpression_EQ_AssignmentExpression(
        context, "StrictArg", ["child", "=", "right"]
    )
    assert ae.name == "AssignmentExpression"
    assert ae.LeftHandSideExpression == "child"
    assert ae.AssignmentExpression == "right"


def test_P2_AssignmentExpression_LeftHandSideExpression_AssignmentOperator_AssignmentExpression_init(context):
    ae = e.P2_AssignmentExpression_LeftHandSideExpression_AssignmentOperator_AssignmentExpression(
        context, "StrictArg", ["child", "+=", "right"]
    )
    assert ae.name == "AssignmentExpression"
    assert ae.LeftHandSideExpression == "child"
    assert ae.AssignmentOperator == "+="
    assert ae.AssignmentExpression == "right"


def AssignmentExpression_mocks(mocker):
    return {
        "ConditionalExpression": mocker.patch(
            "ecmascript.ecmascript.parse_ConditionalExpression", side_effect=cond_sideeffect
        ),
        "YieldExpression": mocker.patch("ecmascript.ecmascript.parse_YieldExpression", side_effect=ye_sideeffect),
        "ArrowFunction": mocker.patch("ecmascript.ecmascript.parse_ArrowFunction", side_effect=arrfcn_sideeffect),
        "AsyncArrowFunction": mocker.patch(
            "ecmascript.ecmascript.parse_AsyncArrowFunction", side_effect=asarrfcn_sideeffect
        ),
        "LeftHandSideExpression": mocker.patch(
            "ecmascript.ecmascript.parse_LeftHandSideExpression", side_effect=lhs_sideeffect
        ),
    }


@pytest.mark.parametrize("Yield", [True, False])
def test_parse_AssignmentExpression_CE(mocker, context, Yield):
    # AssignmentExpression : ConditionalExpression
    lexer = Lexer([COND_REPLACEMENT])
    mocks = AssignmentExpression_mocks(mocker)

    ae = e.parse_AssignmentExpression(context, lexer, "StrictArg", "InArg", Yield, "AwaitArg")
    assert isinstance(ae, e.P2_AssignmentExpression_ConditionalExpression)
    assert ae.ConditionalExpression == "ConditionalExpression"
    assert lexer.pos == 1
    mocks["ConditionalExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", Yield, "AwaitArg")


def test_parse_AssignmentExpression_YE_Nope(mocker, context):
    # AssignmentExpression : [+Yield]YieldExpression (Yield == False)
    lexer = Lexer([YE_REPLACEMENT])
    AssignmentExpression_mocks(mocker)

    ae = e.parse_AssignmentExpression(context, lexer, "StrictArg", "InArg", False, "AwaitArg")
    assert ae is None
    assert lexer.pos == 0


def test_parse_AssignmentExpression_YE_Yep(mocker, context):
    # AssignmentExpression : [+Yield]YieldExpression (Yield == True)
    lexer = Lexer([YE_REPLACEMENT])
    mocks = AssignmentExpression_mocks(mocker)

    ae = e.parse_AssignmentExpression(context, lexer, "StrictArg", "InArg", True, "AwaitArg")
    assert isinstance(ae, e.P2_AssignmentExpression_YieldExpression)
    assert ae.YieldExpression == "YieldExpression"
    assert lexer.pos == 1
    mocks["YieldExpression"].assert_called_with(context, lexer, "StrictArg", "InArg", "AwaitArg")


@pytest.mark.parametrize("Yield", [True, False])
def test_parse_AssignmentExpression_AF(mocker, context, Yield):
    # AssignmentExpression : ArrowFunction
    lexer = Lexer([ARRFCN_REPLACEMENT])
    mocks = AssignmentExpression_mocks(mocker)

    ae = e.parse_AssignmentExpression(context, lexer, "StrictArg", "InArg", Yield, "AwaitArg")
    assert isinstance(ae, e.P2_AssignmentExpression_ArrowFunction)
    assert ae.ArrowFunction == "ArrowFunction"
    assert lexer.pos == 1
    mocks["ArrowFunction"].assert_called_with(context, lexer, "StrictArg", "InArg", Yield, "AwaitArg")


@pytest.mark.parametrize("Yield", [True, False])
def test_parse_AssignmentExpression_AAF(mocker, context, Yield):
    # AssignmentExpression : AsyncArrowFunction
    lexer = Lexer([ASARRFCN_REPLACEMENT])
    mocks = AssignmentExpression_mocks(mocker)

    ae = e.parse_AssignmentExpression(context, lexer, "StrictArg", "InArg", Yield, "AwaitArg")
    assert isinstance(ae, e.P2_AssignmentExpression_AsyncArrowFunction)
    assert ae.AsyncArrowFunction == "AsyncArrowFunction"
    assert lexer.pos == 1
    mocks["AsyncArrowFunction"].assert_called_with(context, lexer, "StrictArg", "InArg", Yield, "AwaitArg")


@pytest.mark.parametrize("Yield", [True, False])
def test_parse_AssignmentExpression_LHSEQ(mocker, context, Yield):
    # AssignmentExpression : LeftHandSideExpression = AssignmentExpression
    lexer = Lexer([LHS_REPLACEMENT, EQ, COND_REPLACEMENT])
    mocks = AssignmentExpression_mocks(mocker)

    ae = e.parse_AssignmentExpression(context, lexer, "StrictArg", "InArg", Yield, "AwaitArg")
    assert isinstance(ae, e.P2_AssignmentExpression_LeftHandSideExpression_EQ_AssignmentExpression)
    assert ae.LeftHandSideExpression == "LeftHandSideExpression"
    assert lexer.pos == 3
    mocks["LeftHandSideExpression"].assert_called_with(context, lexer, "StrictArg", Yield, "AwaitArg")
    ae2 = ae.AssignmentExpression
    assert isinstance(ae2, e.P2_AssignmentExpression_ConditionalExpression)


@pytest.mark.parametrize("Yield", [True, False])
@pytest.mark.parametrize(
    "op", [STAREQ, SLASHEQ, PERCENTEQ, PLUSEQ, MINUSEQ, LTLTEQ, GTGTEQ, GTGTGTEQ, AMPEQ, CARETEQ, PIPEEQ, STARSTAREQ]
)
def test_parse_AssignmentExpression_LHS_OP(mocker, context, Yield, op):
    # AssignmentExpression : LeftHandSideExpression = AssignmentExpression
    lexer = Lexer([LHS_REPLACEMENT, op, COND_REPLACEMENT])
    mocks = AssignmentExpression_mocks(mocker)

    ae = e.parse_AssignmentExpression(context, lexer, "StrictArg", "InArg", Yield, "AwaitArg")
    assert isinstance(ae, e.P2_AssignmentExpression_LeftHandSideExpression_AssignmentOperator_AssignmentExpression)
    assert ae.LeftHandSideExpression == "LeftHandSideExpression"
    assert ae.AssignmentOperator == op
    assert lexer.pos == 3
    mocks["LeftHandSideExpression"].assert_called_with(context, lexer, "StrictArg", Yield, "AwaitArg")
    ae2 = ae.AssignmentExpression
    assert isinstance(ae2, e.P2_AssignmentExpression_ConditionalExpression)


@pytest.mark.parametrize("Yield", [True, False])
@pytest.mark.parametrize(
    "token_stream",
    [
        [],
        [MATCHES_NONE],
        [LHS_REPLACEMENT, MATCHES_NONE],
        [LHS_REPLACEMENT, EQ, MATCHES_NONE],
        [LHS_REPLACEMENT, STAREQ, MATCHES_NONE],
    ],
)
def test_parse_AssignmentExpression_syntaxerrors(mocker, context, Yield, token_stream):
    # Syntax Errors.
    # Note that after integration, the pattern 'LeftHandSideExpression nomatch' will match a
    # 'AssignmentExpression : ConditionalExpression' pattern, but the unit tests can't check that.
    lexer = Lexer(token_stream)
    AssignmentExpression_mocks(mocker)

    ae = e.parse_AssignmentExpression(context, lexer, "StrictArg", "InArg", Yield, "AwaitArg")
    assert ae is None
    assert lexer.pos == 0


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
    ap = e.P2_AssignmentPattern(context, "StrictArg", ["child"])
    assert ap.name == "AssignmentPattern"
    assert ap.context == context
    assert ap.children == ["child"]


def test_P2_AssignmentPattern_ObjectAssignmentPattern_init(context):
    ap = e.P2_AssignmentPattern_ObjectAssignmentPattern(context, "StrictArg", ["child"])
    assert ap.name == "AssignmentPattern"
    assert ap.ObjectAssignmentPattern == "child"


def test_P2_AssignmentPattern_ArrayAssignmentPattern_init(context):
    ap = e.P2_AssignmentPattern_ArrayAssignmentPattern(context, "StrictArg", ["child"])
    assert ap.name == "AssignmentPattern"
    assert ap.ArrayAssignmentPattern == "child"


def AssignmentPattern_mocks(mocker):
    return {
        "ObjectAssignmentPattern": mocker.patch(
            "ecmascript.ecmascript.parse_ObjectAssignmentPattern", side_effect=oap_sideeffect
        ),
        "ArrayAssignmentPattern": mocker.patch(
            "ecmascript.ecmascript.parse_ArrayAssignmentPattern", side_effect=aap_sideeffect
        ),
    }


def test_parse_AssignmentPattern_obj(mocker, context):
    # AssignmentPattern : ObjectAssignmentPattern
    lexer = Lexer([OAP_REPLACEMENT])
    mocks = AssignmentPattern_mocks(mocker)

    ap = e.parse_AssignmentPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ap, e.P2_AssignmentPattern_ObjectAssignmentPattern)
    assert ap.ObjectAssignmentPattern == "ObjectAssignmentPattern"
    assert lexer.pos == 1
    mocks["ObjectAssignmentPattern"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_AssignmentPattern_array(mocker, context):
    # AssignmentPattern : ObjectAssignmentPattern
    lexer = Lexer([AAP_REPLACEMENT])
    mocks = AssignmentPattern_mocks(mocker)

    ap = e.parse_AssignmentPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ap, e.P2_AssignmentPattern_ArrayAssignmentPattern)
    assert ap.ArrayAssignmentPattern == "ArrayAssignmentPattern"
    assert lexer.pos == 1
    mocks["ArrayAssignmentPattern"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_AssignmentPattern_errs(mocker, context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)
    AssignmentPattern_mocks(mocker)

    ap = e.parse_AssignmentPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert ap is None
    assert lexer.pos == 0


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
    oap = e.P2_ObjectAssignmentPattern(context, "StrictArg", ["child"])
    assert oap.name == "ObjectAssignmentPattern"
    assert oap.context == context
    assert oap.children == ["child"]


def test_P2_ObjectAssignmentPattern_Empty_init(context):
    oap = e.P2_ObjectAssignmentPattern_Empty(context, "StrictArg", ["{", "}"])
    assert oap.name == "ObjectAssignmentPattern"


def test_P2_ObjectAssignmentPattern_AssignmentRestProperty_init(context):
    oap = e.P2_ObjectAssignmentPattern_AssignmentRestProperty(context, "StrictArg", ["{", "child", "}"])
    assert oap.name == "ObjectAssignmentPattern"
    assert oap.AssignmentRestProperty == "child"


def test_P2_ObjectAssignmentPattern_AssignmentPropertyList_init(context):
    oap = e.P2_ObjectAssignmentPattern_AssignmentPropertyList(context, "StrictArg", ["{", "child", "}"])
    assert oap.name == "ObjectAssignmentPattern"
    assert oap.AssignmentPropertyList == "child"


def test_P2_ObjectAssignmentPattern_AssignmentPropertyList_AssignmentRestProperty_init(context):
    oap = e.P2_ObjectAssignmentPattern_AssignmentPropertyList_AssignmentRestProperty(
        context, "StrictArg", ["{", "child", ",", "right", "}"]
    )
    assert oap.name == "ObjectAssignmentPattern"
    assert oap.AssignmentPropertyList == "child"
    assert oap.AssignmentRestProperty == "right"


def ObjectAssignmentPattern_mocks(mocker):
    return {
        "AssignmentRestProperty": mocker.patch(
            "ecmascript.ecmascript.parse_AssignmentRestProperty", side_effect=arp_sideeffect
        ),
        "AssignmentPropertyList": mocker.patch(
            "ecmascript.ecmascript.parse_AssignmentPropertyList", side_effect=apl_sideeffect
        ),
    }


def test_parse_ObjectAssignmentPattern_empty(mocker, context):
    # ObjectAssignmentPattern : { }
    lexer = Lexer([LCURLY, RCURLY])
    ObjectAssignmentPattern_mocks(mocker)

    oap = e.parse_ObjectAssignmentPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(oap, e.P2_ObjectAssignmentPattern_Empty)
    assert lexer.pos == 2


def test_parse_ObjectAssignmentPattern_arp(mocker, context):
    # ObjectAssignmentPattern : { AssignmentRestProperty }
    lexer = Lexer([LCURLY, ARP_REPLACEMENT, RCURLY])
    mocks = ObjectAssignmentPattern_mocks(mocker)

    oap = e.parse_ObjectAssignmentPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(oap, e.P2_ObjectAssignmentPattern_AssignmentRestProperty)
    assert oap.AssignmentRestProperty == "AssignmentRestProperty"
    assert lexer.pos == 3
    mocks["AssignmentRestProperty"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "token_stream", [[LCURLY, APL_REPLACEMENT, RCURLY], [LCURLY, APL_REPLACEMENT, COMMA, RCURLY]]
)
def test_parse_ObjectAssignmentPattern_apl(mocker, context, token_stream):
    # ObjectAssignmentPattern : { AssignmentPropertyList }
    # ObjectAssignmentPattern : { AssignmentPropertyList , }
    lexer = Lexer(token_stream)
    mocks = ObjectAssignmentPattern_mocks(mocker)

    oap = e.parse_ObjectAssignmentPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(oap, e.P2_ObjectAssignmentPattern_AssignmentPropertyList)
    assert oap.AssignmentPropertyList == "AssignmentPropertyList"
    assert lexer.pos == len(token_stream)
    mocks["AssignmentPropertyList"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_ObjectAssignmentPattern_apl_arp(mocker, context):
    # ObjectAssignmentPattern : { AssignmentPropertyList , AssignmentRestProperty }
    lexer = Lexer([LCURLY, APL_REPLACEMENT, COMMA, ARP_REPLACEMENT, RCURLY])
    mocks = ObjectAssignmentPattern_mocks(mocker)

    oap = e.parse_ObjectAssignmentPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(oap, e.P2_ObjectAssignmentPattern_AssignmentPropertyList_AssignmentRestProperty)
    assert oap.AssignmentPropertyList == "AssignmentPropertyList"
    assert oap.AssignmentRestProperty == "AssignmentRestProperty"
    assert lexer.pos == 5
    mocks["AssignmentPropertyList"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    mocks["AssignmentRestProperty"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "token_stream",
    [
        [],
        [MATCHES_NONE],
        [LCURLY, MATCHES_NONE],
        [LCURLY, ARP_REPLACEMENT, MATCHES_NONE],
        [LCURLY, APL_REPLACEMENT, MATCHES_NONE],
        [LCURLY, APL_REPLACEMENT, COMMA, MATCHES_NONE],
        [LCURLY, APL_REPLACEMENT, COMMA, ARP_REPLACEMENT, MATCHES_NONE],
    ],
)
def test_parse_ObjectAssignmentPattern_errs(mocker, context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)
    ObjectAssignmentPattern_mocks(mocker)

    oap = e.parse_ObjectAssignmentPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert oap is None
    assert lexer.pos == 0


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
    aap = e.P2_ArrayAssignmentPattern(context, "StrictArg", ["child"])
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.context == context
    assert aap.children == ["child"]
    assert aap.Elision is None
    assert aap.AssignmentRestElement is None
    assert aap.AssignmentElementList is None


def test_P2_ArrayAssignmentPattern_Empty_init(context):
    aap = e.P2_ArrayAssignmentPattern_Empty(context, "StrictArg", ["[", "]"])
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.Elision is None
    assert aap.AssignmentRestElement is None
    assert aap.AssignmentElementList is None


def test_P2_ArrayAssignmentPattern_AssignmentRestElement_init(context):
    aap = e.P2_ArrayAssignmentPattern_AssignmentRestElement(context, "StrictArg", ["[", "ARE", "]"])
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.Elision is None
    assert aap.AssignmentRestElement == "ARE"
    assert aap.AssignmentElementList is None


def test_P2_ArrayAssignmentPattern_Elision_init(context):
    aap = e.P2_ArrayAssignmentPattern_Elision(context, "StrictArg", ["[", "ELISION", "]"])
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.Elision == "ELISION"
    assert aap.AssignmentRestElement is None
    assert aap.AssignmentElementList is None


def test_P2_ArrayAssignmentPattern_Elision_AssignmentRestElement_init(context):
    aap = e.P2_ArrayAssignmentPattern_Elision_AssignmentRestElement(
        context, "StrictArg", ["[", "ELISION", "ARE", "]"]
    )
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.Elision == "ELISION"
    assert aap.AssignmentRestElement == "ARE"
    assert aap.AssignmentElementList is None


def test_P2_ArrayAssignmentPattern_AssignmentElementList_init(context):
    aap = e.P2_ArrayAssignmentPattern_AssignmentElementList(context, "StrictArg", ["[", "AEL", "]"])
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.Elision is None
    assert aap.AssignmentRestElement is None
    assert aap.AssignmentElementList == "AEL"


def test_P2_ArrayAssignmentPattern_AssignmentElementList_Elision_init(context):
    aap = e.P2_ArrayAssignmentPattern_AssignmentElementList_Elision(
        context, "StrictArg", ["[", "AEL", ",", "ELISION", "]"]
    )
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.Elision == "ELISION"
    assert aap.AssignmentRestElement is None
    assert aap.AssignmentElementList == "AEL"


def test_P2_ArrayAssignmentPattern_AssignmentElementList_AssignmentRestElement_init(context):
    aap = e.P2_ArrayAssignmentPattern_AssignmentElementList_AssignmentRestElement(
        context, "StrictArg", ["[", "AEL", ",", "ARE", "]"]
    )
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.Elision is None
    assert aap.AssignmentRestElement == "ARE"
    assert aap.AssignmentElementList == "AEL"


def test_P2_ArrayAssignmentPattern_AssignmentElementList_Elision_AssignmentRestElement_init(context):
    aap = e.P2_ArrayAssignmentPattern_AssignmentElementList_Elision_AssignmentRestElement(
        context, "StrictArg", ["[", "AEL", ",", "ELISION", "ARE", "]"]
    )
    assert aap.name == "ArrayAssignmentPattern"
    assert aap.Elision == "ELISION"
    assert aap.AssignmentRestElement == "ARE"
    assert aap.AssignmentElementList == "AEL"


def ArrayAssignmentPattern_mocks(mocker):
    return {
        "Elision": mocker.patch("ecmascript.ecmascript.parse_Elision", side_effect=elision_sideeffect),
        "AssignmentRestElement": mocker.patch(
            "ecmascript.ecmascript.parse_AssignmentRestElement", side_effect=are_sideeffect
        ),
        "AssignmentElementList": mocker.patch(
            "ecmascript.ecmascript.parse_AssignmentElementList", side_effect=ael_sideeffect
        ),
    }


@pytest.mark.parametrize(
    "token_stream, expected_class, has_elision, has_are, has_ael",
    [
        ([LBRACKET, RBRACKET], e.P2_ArrayAssignmentPattern_Empty, False, False, False),
        ([LBRACKET, ELISION_REPLACEMENT, RBRACKET], e.P2_ArrayAssignmentPattern_Elision, True, False, False),
        (
            [LBRACKET, ARE_REPLACEMENT, RBRACKET],
            e.P2_ArrayAssignmentPattern_AssignmentRestElement,
            False,
            True,
            False,
        ),
        (
            [LBRACKET, ELISION_REPLACEMENT, ARE_REPLACEMENT, RBRACKET],
            e.P2_ArrayAssignmentPattern_Elision_AssignmentRestElement,
            True,
            True,
            False,
        ),
        (
            [LBRACKET, AEL_REPLACEMENT, RBRACKET],
            e.P2_ArrayAssignmentPattern_AssignmentElementList,
            False,
            False,
            True,
        ),
        (
            [LBRACKET, AEL_REPLACEMENT, COMMA, RBRACKET],
            e.P2_ArrayAssignmentPattern_AssignmentElementList,
            False,
            False,
            True,
        ),
        (
            [LBRACKET, AEL_REPLACEMENT, COMMA, ELISION_REPLACEMENT, RBRACKET],
            e.P2_ArrayAssignmentPattern_AssignmentElementList_Elision,
            True,
            False,
            True,
        ),
        (
            [LBRACKET, AEL_REPLACEMENT, COMMA, ARE_REPLACEMENT, RBRACKET],
            e.P2_ArrayAssignmentPattern_AssignmentElementList_AssignmentRestElement,
            False,
            True,
            True,
        ),
        (
            [LBRACKET, AEL_REPLACEMENT, COMMA, ELISION_REPLACEMENT, ARE_REPLACEMENT, RBRACKET],
            e.P2_ArrayAssignmentPattern_AssignmentElementList_Elision_AssignmentRestElement,
            True,
            True,
            True,
        ),
    ],
)
def test_parse_ArrayAssignmentPattern_01(
    mocker, context, token_stream, expected_class, has_elision, has_are, has_ael
):
    # ArrayAssignmentPattern : [ ]
    # ArrayAssignmentPattern : [ Elision ]
    # ArrayAssignmentPattern : [ AssignmentRestElement ]
    # ArrayAssignmentPattern : [ Elision AssignmentRestElement ]
    # ArrayAssignmentPattern : [ AssignmentElementList ]
    # ArrayAssignmentPattern : [ AssignmentElementList , ]
    # ArrayAssignmentPattern : [ AssignmentElementList , Elision ]
    # ArrayAssignmentPattern : [ AssignmentElementList , AssignmentRestElement ]
    # ArrayAssignmentPattern : [ AssignmentElementList , Elision AssignmentRestElement ]
    lexer = Lexer(token_stream)
    mocks = ArrayAssignmentPattern_mocks(mocker)

    def property_check(name, expect_none):
        def check_none(val):
            assert val is None

        def check_val(val):
            assert val == name

        return check_none if expect_none else check_val

    elision_property_check = property_check("Elision", not has_elision)
    are_property_check = property_check("AssignmentRestElement", not has_are)
    ael_property_check = property_check("AssignmentElementList", not has_ael)

    mock_check = gen_mock_check(mocker, mocks, context, lexer)
    elision_mock_check = mock_check("Elision", has_elision, "StrictArg")
    are_mock_check = mock_check("AssignmentRestElement", has_are, "StrictArg", "YieldArg", "AwaitArg")
    ael_mock_check = mock_check("AssignmentElementList", has_ael, "StrictArg", "YieldArg", "AwaitArg")

    aap = e.parse_ArrayAssignmentPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(aap, expected_class)
    assert lexer.pos == len(token_stream)
    elision_property_check(aap.Elision)
    are_property_check(aap.AssignmentRestElement)
    ael_property_check(aap.AssignmentElementList)
    elision_mock_check()
    are_mock_check()
    ael_mock_check()


@pytest.mark.parametrize(
    "token_stream",
    [
        [],
        [MATCHES_NONE],
        [LBRACKET, MATCHES_NONE],
        [LBRACKET, ELISION_REPLACEMENT, MATCHES_NONE],
        [LBRACKET, ARE_REPLACEMENT, MATCHES_NONE],
        [LBRACKET, ELISION_REPLACEMENT, ARE_REPLACEMENT, MATCHES_NONE],
        [LBRACKET, AEL_REPLACEMENT, MATCHES_NONE],
        [LBRACKET, AEL_REPLACEMENT, COMMA, MATCHES_NONE],
        [LBRACKET, AEL_REPLACEMENT, COMMA, ELISION_REPLACEMENT, MATCHES_NONE],
        [LBRACKET, AEL_REPLACEMENT, COMMA, ARE_REPLACEMENT, MATCHES_NONE],
        [LBRACKET, AEL_REPLACEMENT, COMMA, ELISION_REPLACEMENT, ARE_REPLACEMENT, MATCHES_NONE],
    ],
)
def test_parse_ArrayAssignmentPattern_02(mocker, context, token_stream):
    # Syntax errors
    lexer = Lexer(token_stream)
    ArrayAssignmentPattern_mocks(mocker)

    aap = e.parse_ArrayAssignmentPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert aap is None
    assert lexer.pos == 0


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
    arp = e.P2_AssignmentRestProperty(context, "StrictArg", ["child"])
    assert arp.name == "AssignmentRestProperty"
    assert arp.context == context
    assert arp.children == ["child"]


def test_P2_AssignmentRestProperty_DestructuringAssignmentTarget_init(context):
    arp = e.P2_AssignmentRestProperty_DestructuringAssignmentTarget(context, "StrictArg", ["...", "DAT"])
    assert arp.name == "AssignmentRestProperty"
    assert arp.DestructuringAssignmentTarget == "DAT"


def AssignmentRestProperty_mocks(mocker):
    return {
        "DestructuringAssignmentTarget": mocker.patch(
            "ecmascript.ecmascript.parse_DestructuringAssignmentTarget", side_effect=dat_sideeffect
        )
    }


def test_parse_AssignmentRestProperty_01(mocker, context):
    # AssignmentRestProperty : ... DestructuringAssignmentTarget
    lexer = Lexer([DOTDOTDOT, DAT_REPLACEMENT])
    mocks = AssignmentRestProperty_mocks(mocker)

    arp = e.parse_AssignmentRestProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(arp, e.P2_AssignmentRestProperty_DestructuringAssignmentTarget)
    assert arp.DestructuringAssignmentTarget == "DestructuringAssignmentTarget"
    assert lexer.pos == 2
    mocks["DestructuringAssignmentTarget"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE], [DOTDOTDOT, MATCHES_NONE]])
def test_parse_AssignmentRestProperty_02(mocker, context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)
    AssignmentRestProperty_mocks(mocker)

    arp = e.parse_AssignmentRestProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert arp is None
    assert lexer.pos == 0


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
    apl = e.P2_AssignmentPropertyList(context, "StrictArg", ["child"])
    assert apl.name == "AssignmentPropertyList"
    assert apl.context == context
    assert apl.children == ["child"]


def test_P2_AssignmentPropertyList_AssignmentProperty_init(context):
    apl = e.P2_AssignmentPropertyList_AssignmentProperty(context, "StrictArg", ["child"])
    assert apl.name == "AssignmentPropertyList"
    assert apl.AssignmentProperty == "child"


def test_P2_AssignmentPropertyList_AssignmentPropertyList_AssignmentProperty_init(context):
    apl = e.P2_AssignmentPropertyList_AssignmentPropertyList_AssignmentProperty(
        context, "StrictArg", ["APL", ",", "AP"]
    )
    assert apl.name == "AssignmentPropertyList"
    assert apl.AssignmentPropertyList == "APL"
    assert apl.AssignmentProperty == "AP"


def AssignmentPropertyList_mocks(mocker):
    return {
        "AssignmentProperty": mocker.patch(
            "ecmascript.ecmascript.parse_AssignmentProperty", side_effect=ap_sideeffect
        )
    }


def test_parse_AssignmentPropertyList_01(mocker, context):
    # AssignmentPropertyList : AssignmentProperty
    lexer = Lexer([AP_REPLACEMENT])
    mocks = AssignmentPropertyList_mocks(mocker)

    apl = e.parse_AssignmentPropertyList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(apl, e.P2_AssignmentPropertyList_AssignmentProperty)
    assert apl.AssignmentProperty == "AssignmentProperty"
    assert lexer.pos == 1
    mocks["AssignmentProperty"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_AssignmentPropertyList_02(mocker, context):
    # AssignmentPropertyList : AssignmentPropertyList , AssignmentProperty
    lexer = Lexer([AP_REPLACEMENT, COMMA, AP_REPLACEMENT])
    mocks = AssignmentPropertyList_mocks(mocker)

    apl = e.parse_AssignmentPropertyList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(apl, e.P2_AssignmentPropertyList_AssignmentPropertyList_AssignmentProperty)
    assert apl.AssignmentProperty == "AssignmentProperty"
    assert lexer.pos == 3
    mocks["AssignmentProperty"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    apl2 = apl.AssignmentPropertyList
    assert isinstance(apl2, e.P2_AssignmentPropertyList_AssignmentProperty)


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_AssignmentPropertyList_03(mocker, context, token_stream):
    # non-recursive syntax errors
    lexer = Lexer(token_stream)
    AssignmentPropertyList_mocks(mocker)

    apl = e.parse_AssignmentPropertyList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert apl is None
    assert lexer.pos == 0


@pytest.mark.parametrize("token_stream", [[AP_REPLACEMENT, MATCHES_NONE], [AP_REPLACEMENT, COMMA, MATCHES_NONE]])
def test_parse_AssignmentPropertyList_04(mocker, context, token_stream):
    # recursive syntax errors
    lexer = Lexer(token_stream)
    AssignmentPropertyList_mocks(mocker)

    apl = e.parse_AssignmentPropertyList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(apl, e.P2_AssignmentPropertyList_AssignmentProperty)
    assert lexer.pos == 1


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
    ael = e.P2_AssignmentElementList(context, "StrictArg", ["child"])
    assert ael.name == "AssignmentElementList"
    assert ael.context == context
    assert ael.children == ["child"]


def test_P2_AssignmentElementList_AssignmentElisionElement_init(context):
    ael = e.P2_AssignmentElementList_AssignmentElisionElement(context, "StrictArg", ["child"])
    assert ael.name == "AssignmentElementList"
    assert ael.AssignmentElisionElement == "child"


def test_P2_AssignmentElementList_AssignmentElementList_AssignmentElisionElement_init(context):
    ael = e.P2_AssignmentElementList_AssignmentElementList_AssignmentElisionElement(
        context, "StrictArg", ["child", ",", "sibling"]
    )
    assert ael.name == "AssignmentElementList"
    assert ael.AssignmentElementList == "child"
    assert ael.AssignmentElisionElement == "sibling"


def AssignmentElementList_mocks(mocker):
    return {
        "AssignmentElisionElement": mocker.patch(
            "ecmascript.ecmascript.parse_AssignmentElisionElement", side_effect=aee_sideeffect
        )
    }


def test_parse_AssignmentElementList_01(mocker, context):
    # AssignmentElementList : AssignmentElisionElement
    lexer = Lexer([AEE_REPLACEMENT])
    mocks = AssignmentElementList_mocks(mocker)

    ael = e.parse_AssignmentElementList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ael, e.P2_AssignmentElementList_AssignmentElisionElement)
    assert ael.AssignmentElisionElement == "AssignmentElisionElement"
    assert lexer.pos == 1
    mocks["AssignmentElisionElement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_AssignmentElementList_02(mocker, context):
    # AssignmentElementList : AssignmentElementList , AssignmentElisionElement
    lexer = Lexer([AEE_REPLACEMENT, COMMA, AEE_REPLACEMENT])
    mocks = AssignmentElementList_mocks(mocker)

    ael = e.parse_AssignmentElementList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ael, e.P2_AssignmentElementList_AssignmentElementList_AssignmentElisionElement)
    assert ael.AssignmentElisionElement == "AssignmentElisionElement"
    assert lexer.pos == 3
    mocks["AssignmentElisionElement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    ael2 = ael.AssignmentElementList
    assert isinstance(ael2, e.P2_AssignmentElementList_AssignmentElisionElement)


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_AssignmentElementList_03(mocker, context, token_stream):
    # non-recursive syntax errors
    lexer = Lexer(token_stream)
    AssignmentElementList_mocks(mocker)

    ael = e.parse_AssignmentElementList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert ael is None
    assert lexer.pos == 0


@pytest.mark.parametrize("token_stream", [[AEE_REPLACEMENT, MATCHES_NONE], [AEE_REPLACEMENT, COMMA, MATCHES_NONE]])
def test_parse_AssignmentElementList_04(mocker, context, token_stream):
    # recursive syntax errors
    lexer = Lexer(token_stream)
    AssignmentElementList_mocks(mocker)

    ael = e.parse_AssignmentElementList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ael, e.P2_AssignmentElementList_AssignmentElisionElement)
    assert lexer.pos == 1


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
    aee = e.P2_AssignmentElisionElement(context, "StrictArg", ["child"])
    assert aee.name == "AssignmentElisionElement"
    assert aee.context == context
    assert aee.children == ["child"]


def test_P2_AssignmentElisionElement_Elision_AssignmentElement_init(context):
    aee = e.P2_AssignmentElisionElement_Elision_AssignmentElement(context, "StrictArg", ["Elision", "ae"])
    assert aee.name == "AssignmentElisionElement"
    assert aee.Elision == "Elision"
    assert aee.AssignmentElement == "ae"


def test_P2_AssignmentElisionElement_AssignmentElement_init(context):
    aee = e.P2_AssignmentElisionElement_AssignmentElement(context, "StrictArg", ["child"])
    assert aee.name == "AssignmentElisionElement"
    assert aee.AssignmentElement == "child"


def AssignmentElisionElement_mocks(mocker):
    return {
        "Elision": mocker.patch("ecmascript.ecmascript.parse_Elision", side_effect=elision_sideeffect),
        "AssignmentElement": mocker.patch(
            "ecmascript.ecmascript.parse_AssignmentElement", side_effect=assel_sideeffect
        ),
    }


def test_parse_AssignmentElisionElement_01(mocker, context):
    # AssignmentElisionElement: AssignmentElement
    lexer = Lexer([ASSEL_REPLACEMENT])
    mocks = AssignmentElisionElement_mocks(mocker)

    aee = e.parse_AssignmentElisionElement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(aee, e.P2_AssignmentElisionElement_AssignmentElement)
    assert aee.AssignmentElement == "AssignmentElement"
    assert lexer.pos == 1
    mocks["AssignmentElement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_AssignmentElisionElement_02(mocker, context):
    # AssignmentElisionElement : Elision AssignmentElement
    lexer = Lexer([ELISION_REPLACEMENT, ASSEL_REPLACEMENT])
    mocks = AssignmentElisionElement_mocks(mocker)

    aee = e.parse_AssignmentElisionElement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(aee, e.P2_AssignmentElisionElement_Elision_AssignmentElement)
    assert aee.Elision == "Elision"
    assert aee.AssignmentElement == "AssignmentElement"
    assert lexer.pos == 2
    mocks["Elision"].assert_called_with(context, lexer, "StrictArg")
    mocks["AssignmentElement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE], [ELISION_REPLACEMENT, MATCHES_NONE]])
def test_parse_AssignmentElisionElement_03(mocker, context, token_stream):
    # Syntax errors
    lexer = Lexer(token_stream)
    AssignmentElisionElement_mocks(mocker)

    aee = e.parse_AssignmentElisionElement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert aee is None
    assert lexer.pos == 0


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
    ap = e.P2_AssignmentProperty(context, "StrictArg", ["child"])
    assert ap.name == "AssignmentProperty"
    assert ap.context == context
    assert ap.children == ["child"]


def test_P2_AssignmentProperty_IdentifierReference_init(context):
    ap = e.P2_AssignmentProperty_IdentifierReference(context, "StrictArg", ["child"])
    assert ap.name == "AssignmentProperty"
    assert ap.IdentifierReference == "child"


def test_P2_AssignmentProperty_IdentifierReference_Initializer_init(context):
    ap = e.P2_AssignmentProperty_IdentifierReference_Initializer(context, "StrictArg", ["child", "init"])
    assert ap.name == "AssignmentProperty"
    assert ap.IdentifierReference == "child"
    assert ap.Initializer == "init"


def test_P2_AssignmentProperty_PropertyName_AssignmentElement_init(context):
    ap = e.P2_AssignmentProperty_PropertyName_AssignmentElement(context, "StrictArg", ["child", ":", "bob"])
    assert ap.name == "AssignmentProperty"
    assert ap.PropertyName == "child"
    assert ap.AssignmentElement == "bob"


def AssignmentProperty_mocks(mocker):
    return {
        "IdentifierReference": mocker.patch(
            "ecmascript.ecmascript.parse_IdentifierReference", side_effect=ir_sideeffect
        ),
        "Initializer": mocker.patch("ecmascript.ecmascript.parse_Initializer", side_effect=init_sideeffect),
        "PropertyName": mocker.patch("ecmascript.ecmascript.parse_PropertyName", side_effect=pn_sideeffect),
        "AssignmentElement": mocker.patch(
            "ecmascript.ecmascript.parse_AssignmentElement", side_effect=assel_sideeffect
        ),
    }


def test_parse_AssignmentProperty_01(mocker, context):
    # AssignmentProperty : IdentifierReference
    lexer = Lexer([IR_REPLACEMENT])
    mocks = AssignmentProperty_mocks(mocker)

    ap = e.parse_AssignmentProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ap, e.P2_AssignmentProperty_IdentifierReference)
    assert ap.IdentifierReference == "IdentifierReference"
    mocks["IdentifierReference"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert lexer.pos == 1


def test_parse_AssignmentProperty_02(mocker, context):
    # AssignmentProperty : IdentifierReference Initializer
    lexer = Lexer([IR_REPLACEMENT, INIT_REPLACEMENT])
    mocks = AssignmentProperty_mocks(mocker)

    ap = e.parse_AssignmentProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ap, e.P2_AssignmentProperty_IdentifierReference_Initializer)
    assert ap.IdentifierReference == "IdentifierReference"
    assert ap.Initializer == "Initializer"
    mocks["IdentifierReference"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert lexer.pos == 2


def test_parse_AssignmentProperty_03(mocker, context):
    # AssignmentProperty : PropertyName : AssignmentElement
    lexer = Lexer([PN_REPLACEMENT, COLON, ASSEL_REPLACEMENT])
    mocks = AssignmentProperty_mocks(mocker)

    ap = e.parse_AssignmentProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ap, e.P2_AssignmentProperty_PropertyName_AssignmentElement)
    assert ap.PropertyName == "PropertyName"
    assert ap.AssignmentElement == "AssignmentElement"
    assert lexer.pos == 3
    mocks["PropertyName"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "token_stream", [[], [MATCHES_NONE], [PN_REPLACEMENT, MATCHES_NONE], [PN_REPLACEMENT, COLON, MATCHES_NONE]]
)
def test_parse_AssignmentProperty_04(mocker, context, token_stream):
    # syntax errs
    lexer = Lexer(token_stream)
    AssignmentProperty_mocks(mocker)

    ap = e.parse_AssignmentProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert ap is None
    assert lexer.pos == 0


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
    ae = e.P2_AssignmentElement(context, "StrictArg", ["child"])
    assert ae.name == "AssignmentElement"
    assert ae.context == context
    assert ae.children == ["child"]


def test_P2_AssignmentElement_DestructuringAssignmentTarget_init(context):
    ae = e.P2_AssignmentElement_DestructuringAssignmentTarget(context, "StrictArg", ["child"])
    assert ae.name == "AssignmentElement"
    assert ae.DestructuringAssignmentTarget == "child"


def test_P2_AssignmentElement_DestructuringAssignmentTarget_Initializer_init(context):
    ae = e.P2_AssignmentElement_DestructuringAssignmentTarget_Initializer(context, "StrictArg", ["child", "init"])
    assert ae.name == "AssignmentElement"
    assert ae.DestructuringAssignmentTarget == "child"
    assert ae.Initializer == "init"


def AssignmentElement_mocks(mocker):
    return {
        "DestructuringAssignmentTarget": mocker.patch(
            "ecmascript.ecmascript.parse_DestructuringAssignmentTarget", side_effect=dat_sideeffect
        ),
        "Initializer": mocker.patch("ecmascript.ecmascript.parse_Initializer", side_effect=init_sideeffect),
    }


def test_parse_AssignmentElement_dat(mocker, context):
    # AssignmentElement : DestructuringAssignmentTarget
    lexer = Lexer([DAT_REPLACEMENT])
    mocks = AssignmentElement_mocks(mocker)

    ae = e.parse_AssignmentElement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ae, e.P2_AssignmentElement_DestructuringAssignmentTarget)
    assert ae.DestructuringAssignmentTarget == "DestructuringAssignmentTarget"
    assert lexer.pos == 1
    mocks["DestructuringAssignmentTarget"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_AssignmentElement_dat_init(mocker, context):
    # AssignmentElement : DestructuringAssignmentTarget Initializer
    lexer = Lexer([DAT_REPLACEMENT, INIT_REPLACEMENT])
    mocks = AssignmentElement_mocks(mocker)

    ae = e.parse_AssignmentElement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(ae, e.P2_AssignmentElement_DestructuringAssignmentTarget_Initializer)
    assert ae.DestructuringAssignmentTarget == "DestructuringAssignmentTarget"
    assert ae.Initializer == "Initializer"
    assert lexer.pos == 2
    mocks["DestructuringAssignmentTarget"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    mocks["Initializer"].assert_called_with(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_AssignmentElement_errs(mocker, context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)
    AssignmentElement_mocks(mocker)

    ae = e.parse_AssignmentElement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert ae is None
    assert lexer.pos == 0


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
    are = e.P2_AssignmentRestElement(context, "StrictArg", ["child"])
    assert are.name == "AssignmentRestElement"
    assert are.context == context
    assert are.children == ["child"]


def test_P2_AssignmentRestElement_DestructuringAssignmentTarget_init(context):
    are = e.P2_AssignmentRestElement_DestructuringAssignmentTarget(context, "StrictArg", ["...", "child"])
    assert are.name == "AssignmentRestElement"
    assert are.DestructuringAssignmentTarget == "child"


def AssignmentRestElement_mocks(mocker):
    return {
        "DestructuringAssignmentTarget": mocker.patch(
            "ecmascript.ecmascript.parse_DestructuringAssignmentTarget", side_effect=dat_sideeffect
        )
    }


def test_parse_AssignmentRestElement_01(mocker, context):
    # AssignmentRestElement : ... DestructuringAssignmentTarget
    lexer = Lexer([DOTDOTDOT, DAT_REPLACEMENT])
    mocks = AssignmentRestElement_mocks(mocker)

    are = e.parse_AssignmentRestElement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(are, e.P2_AssignmentRestElement_DestructuringAssignmentTarget)
    assert are.DestructuringAssignmentTarget == "DestructuringAssignmentTarget"
    assert lexer.pos == 2
    mocks["DestructuringAssignmentTarget"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE], [DOTDOTDOT, MATCHES_NONE]])
def test_parse_AssignmentRestElement_02(mocker, context, token_stream):
    # Syntax errors
    lexer = Lexer(token_stream)
    AssignmentRestElement_mocks(mocker)

    are = e.parse_AssignmentRestElement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert are is None
    assert lexer.pos == 0


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
    dat = e.P2_DestructuringAssignmentTarget(context, "StrictArg", ["child"])
    assert dat.name == "DestructuringAssignmentTarget"
    assert dat.context == context
    assert dat.children == ["child"]


def test_P2_DestructuringAssignmentTarget_LeftHandSideExpression_init(context):
    dat = e.P2_DestructuringAssignmentTarget_LeftHandSideExpression(context, "StrictArg", ["child"])
    assert dat.name == "DestructuringAssignmentTarget"
    assert dat.LeftHandSideExpression == "child"


def DestructuringAssignmentTarget_mocks(mocker):
    return {
        "LeftHandSideExpression": mocker.patch(
            "ecmascript.ecmascript.parse_LeftHandSideExpression", side_effect=lhs_sideeffect
        )
    }


def test_parse_DestructuringAssignmentTarget_01(mocker, context):
    # DestructuringAssignmentTarget : LeftHandSideExpression
    lexer = Lexer([LHS_REPLACEMENT])
    mocks = DestructuringAssignmentTarget_mocks(mocker)

    dat = e.parse_DestructuringAssignmentTarget(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(dat, e.P2_DestructuringAssignmentTarget_LeftHandSideExpression)
    assert dat.LeftHandSideExpression == "LeftHandSideExpression"
    assert lexer.pos == 1
    mocks["LeftHandSideExpression"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_DestructuringAssignmentTarget_02(mocker, context, token_stream):
    # Syntax errors
    lexer = Lexer(token_stream)
    DestructuringAssignmentTarget_mocks(mocker)

    dat = e.parse_DestructuringAssignmentTarget(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert dat is None
    assert lexer.pos == 0


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


def Expression_mocks(mocker):
    return {"ae": mocker.patch("ecmascript.ecmascript.parse_AssignmentExpression", side_effect=ae_sideeffect)}


def test_parse_Expression_01(mocker, context):
    # Expression : AssignmentExpression
    lexer = Lexer([AE_REPLACEMENT])
    mocks = Expression_mocks(mocker)

    exp = e.parse_Expression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(exp, e.P2_Expression_AssignmentExpression)
    assert exp.AssignmentExpression == "AssignmentExpression"
    assert lexer.pos == 1
    mocks["ae"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


def test_parse_Expression_02(mocker, context):
    # Expression : Expression , AssignmentExpression
    lexer = Lexer([AE_REPLACEMENT, COMMA, AE_REPLACEMENT])
    mocks = Expression_mocks(mocker)

    exp = e.parse_Expression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(exp, e.P2_Expression_Expression_COMMA_AssignmentExpression)
    assert exp.AssignmentExpression == "AssignmentExpression"
    assert lexer.pos == 3
    mocks["ae"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    exp2 = exp.Expression
    assert isinstance(exp2, e.P2_Expression_AssignmentExpression)


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_Expression_03(mocker, context, token_stream):
    # Syntax errors (non-recursive)
    lexer = Lexer(token_stream)
    Expression_mocks(mocker)

    exp = e.parse_Expression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert exp is None
    assert lexer.pos == 0


@pytest.mark.parametrize("token_stream", [[AE_REPLACEMENT, COMMA, MATCHES_NONE]])
def test_parse_Expression_04(mocker, context, token_stream):
    # Syntax errors (recursive)
    lexer = Lexer(token_stream)
    Expression_mocks(mocker)

    exp = e.parse_Expression(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(exp, e.P2_Expression_AssignmentExpression)
    assert lexer.pos == 1


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
    statement = e.P2_Statement(context, "StrictArg", ["child"])
    assert statement.name == "Statement"
    assert statement.context == context
    assert statement.children == ["child"]


def test_P2_Statement_BlockStatement_init(context):
    statement = e.P2_Statement_BlockStatement(context, "StrictArg", ["child"])
    assert statement.name == "Statement"
    assert statement.BlockStatement == "child"


def test_P2_Statement_VariableStatement_init(context):
    statement = e.P2_Statement_VariableStatement(context, "StrictArg", ["child"])
    assert statement.name == "Statement"
    assert statement.VariableStatement == "child"


def test_P2_Statement_EmptyStatement_init(context):
    statement = e.P2_Statement_EmptyStatement(context, "StrictArg", ["child"])
    assert statement.name == "Statement"
    assert statement.EmptyStatement == "child"


def test_P2_Statement_ExpressionStatement_init(context):
    statement = e.P2_Statement_ExpressionStatement(context, "StrictArg", ["child"])
    assert statement.name == "Statement"
    assert statement.ExpressionStatement == "child"


def test_P2_Statement_IfStatement_init(context):
    statement = e.P2_Statement_IfStatement(context, "StrictArg", ["child"])
    assert statement.name == "Statement"
    assert statement.IfStatement == "child"


def test_P2_Statement_BreakableStatement_init(context):
    statement = e.P2_Statement_BreakableStatement(context, "StrictArg", ["child"])
    assert statement.name == "Statement"
    assert statement.BreakableStatement == "child"


def test_P2_Statement_ContinueStatement_init(context):
    statement = e.P2_Statement_ContinueStatement(context, "StrictArg", ["child"])
    assert statement.name == "Statement"
    assert statement.ContinueStatement == "child"


def test_P2_Statement_BreakStatement_init(context):
    statement = e.P2_Statement_BreakStatement(context, "StrictArg", ["child"])
    assert statement.name == "Statement"
    assert statement.BreakStatement == "child"


def test_P2_Statement_ReturnStatement_init(context):
    statement = e.P2_Statement_ReturnStatement(context, "StrictArg", ["child"])
    assert statement.name == "Statement"
    assert statement.ReturnStatement == "child"


def test_P2_Statement_WithStatement_init(context):
    statement = e.P2_Statement_WithStatement(context, "StrictArg", ["child"])
    assert statement.name == "Statement"
    assert statement.WithStatement == "child"


def test_P2_Statement_LabelledStatement_init(context):
    statement = e.P2_Statement_LabelledStatement(context, "StrictArg", ["child"])
    assert statement.name == "Statement"
    assert statement.LabelledStatement == "child"


def test_P2_Statement_ThrowStatement_init(context):
    statement = e.P2_Statement_ThrowStatement(context, "StrictArg", ["child"])
    assert statement.name == "Statement"
    assert statement.ThrowStatement == "child"


def test_P2_Statement_TryStatement_init(context):
    statement = e.P2_Statement_TryStatement(context, "StrictArg", ["child"])
    assert statement.name == "Statement"
    assert statement.TryStatement == "child"


def test_P2_Statement_DebuggerStatement_init(context):
    statement = e.P2_Statement_DebuggerStatement(context, "StrictArg", ["child"])
    assert statement.name == "Statement"
    assert statement.DebuggerStatement == "child"


def Statement_mocks(mocker):
    return {
        "BlockStatement": mocker.patch(
            "ecmascript.ecmascript.parse_BlockStatement", side_effect=block_statement_sideeffect
        ),
        "VariableStatement": mocker.patch(
            "ecmascript.ecmascript.parse_VariableStatement", side_effect=variable_statement_sideeffect
        ),
        "EmptyStatement": mocker.patch(
            "ecmascript.ecmascript.parse_EmptyStatement", side_effect=empty_statement_sideeffect
        ),
        "ExpressionStatement": mocker.patch(
            "ecmascript.ecmascript.parse_ExpressionStatement", side_effect=expression_statement_sideeffect
        ),
        "IfStatement": mocker.patch("ecmascript.ecmascript.parse_IfStatement", side_effect=if_statement_sideeffect),
        "BreakableStatement": mocker.patch(
            "ecmascript.ecmascript.parse_BreakableStatement", side_effect=breakable_statement_sideeffect
        ),
        "ContinueStatement": mocker.patch(
            "ecmascript.ecmascript.parse_ContinueStatement", side_effect=continue_statement_sideeffect
        ),
        "BreakStatement": mocker.patch(
            "ecmascript.ecmascript.parse_BreakStatement", side_effect=break_statement_sideeffect
        ),
        "ReturnStatement": mocker.patch(
            "ecmascript.ecmascript.parse_ReturnStatement", side_effect=return_statement_sideeffect
        ),
        "WithStatement": mocker.patch(
            "ecmascript.ecmascript.parse_WithStatement", side_effect=with_statement_sideeffect
        ),
        "LabelledStatement": mocker.patch(
            "ecmascript.ecmascript.parse_LabelledStatement", side_effect=labelled_statement_sideeffect
        ),
        "ThrowStatement": mocker.patch(
            "ecmascript.ecmascript.parse_ThrowStatement", side_effect=throw_statement_sideeffect
        ),
        "TryStatement": mocker.patch(
            "ecmascript.ecmascript.parse_TryStatement", side_effect=try_statement_sideeffect
        ),
        "DebuggerStatement": mocker.patch(
            "ecmascript.ecmascript.parse_DebuggerStatement", side_effect=debugger_statement_sideeffect
        ),
    }


@pytest.mark.parametrize("Return", [True, False])
def test_parse_Statement_BlockStatement(mocker, context, Return):
    # Statement : BlockStatement
    lexer = Lexer([BLOCK_STMT_REPLACEMENT])
    mocks = Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)
    assert isinstance(stmt, e.P2_Statement_BlockStatement)
    assert stmt.BlockStatement == "BlockStatement"
    assert lexer.pos == 1
    mocks["BlockStatement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)


@pytest.mark.parametrize("Return", [True, False])
def test_parse_Statement_VariableStatement(mocker, context, Return):
    # Statement : VariableStatement
    lexer = Lexer([VARIABLE_STMT_REPLACEMENT])
    mocks = Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)
    assert isinstance(stmt, e.P2_Statement_VariableStatement)
    assert stmt.VariableStatement == "VariableStatement"
    assert lexer.pos == 1
    mocks["VariableStatement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("Return", [True, False])
def test_parse_Statement_EmptyStatement(mocker, context, Return):
    # Statement : EmptyStatement
    lexer = Lexer([EMPTY_STMT_REPLACEMENT])
    mocks = Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)
    assert isinstance(stmt, e.P2_Statement_EmptyStatement)
    assert stmt.EmptyStatement == "EmptyStatement"
    assert lexer.pos == 1
    mocks["EmptyStatement"].assert_called_with(context, lexer, "StrictArg")


@pytest.mark.parametrize("Return", [True, False])
def test_parse_Statement_ExpressionStatement(mocker, context, Return):
    # Statement : ExpressionStatement
    lexer = Lexer([EXPRESSION_STMT_REPLACEMENT])
    mocks = Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)
    assert isinstance(stmt, e.P2_Statement_ExpressionStatement)
    assert stmt.ExpressionStatement == "ExpressionStatement"
    assert lexer.pos == 1
    mocks["ExpressionStatement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("Return", [True, False])
def test_parse_Statement_IfStatement(mocker, context, Return):
    # Statement : IfStatement
    lexer = Lexer([IF_STMT_REPLACEMENT])
    mocks = Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)
    assert isinstance(stmt, e.P2_Statement_IfStatement)
    assert stmt.IfStatement == "IfStatement"
    assert lexer.pos == 1
    mocks["IfStatement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)


@pytest.mark.parametrize("Return", [True, False])
def test_parse_Statement_BreakableStatement(mocker, context, Return):
    # Statement : BreakableStatement
    lexer = Lexer([BREAKABLE_STMT_REPLACEMENT])
    mocks = Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)
    assert isinstance(stmt, e.P2_Statement_BreakableStatement)
    assert stmt.BreakableStatement == "BreakableStatement"
    assert lexer.pos == 1
    mocks["BreakableStatement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)


@pytest.mark.parametrize("Return", [True, False])
def test_parse_Statement_ContinueStatement(mocker, context, Return):
    # Statement : ContinueStatement
    lexer = Lexer([CONTINUE_STMT_REPLACEMENT])
    mocks = Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)
    assert isinstance(stmt, e.P2_Statement_ContinueStatement)
    assert stmt.ContinueStatement == "ContinueStatement"
    assert lexer.pos == 1
    mocks["ContinueStatement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("Return", [True, False])
def test_parse_Statement_BreakStatement(mocker, context, Return):
    # Statement : BreakStatement
    lexer = Lexer([BREAK_STMT_REPLACEMENT])
    mocks = Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)
    assert isinstance(stmt, e.P2_Statement_BreakStatement)
    assert stmt.BreakStatement == "BreakStatement"
    assert lexer.pos == 1
    mocks["BreakStatement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_Statement_ReturnStatement(mocker, context):
    # Statement : ReturnStatement
    lexer = Lexer([RETURN_STMT_REPLACEMENT])
    mocks = Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", True)
    assert isinstance(stmt, e.P2_Statement_ReturnStatement)
    assert stmt.ReturnStatement == "ReturnStatement"
    assert lexer.pos == 1
    mocks["ReturnStatement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("Return", [True, False])
def test_parse_Statement_WithStatement(mocker, context, Return):
    # Statement : WithStatement
    lexer = Lexer([WITH_STMT_REPLACEMENT])
    mocks = Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)
    assert isinstance(stmt, e.P2_Statement_WithStatement)
    assert stmt.WithStatement == "WithStatement"
    assert lexer.pos == 1
    mocks["WithStatement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)


@pytest.mark.parametrize("Return", [True, False])
def test_parse_Statement_LabelledStatement(mocker, context, Return):
    # Statement : LabelledStatement
    lexer = Lexer([LABELLED_STMT_REPLACEMENT])
    mocks = Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)
    assert isinstance(stmt, e.P2_Statement_LabelledStatement)
    assert stmt.LabelledStatement == "LabelledStatement"
    assert lexer.pos == 1
    mocks["LabelledStatement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)


@pytest.mark.parametrize("Return", [True, False])
def test_parse_Statement_ThrowStatement(mocker, context, Return):
    # Statement : ThrowStatement
    lexer = Lexer([THROW_STMT_REPLACEMENT])
    mocks = Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)
    assert isinstance(stmt, e.P2_Statement_ThrowStatement)
    assert stmt.ThrowStatement == "ThrowStatement"
    assert lexer.pos == 1
    mocks["ThrowStatement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("Return", [True, False])
def test_parse_Statement_TryStatement(mocker, context, Return):
    # Statement : TryStatement
    lexer = Lexer([TRY_STMT_REPLACEMENT])
    mocks = Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)
    assert isinstance(stmt, e.P2_Statement_TryStatement)
    assert stmt.TryStatement == "TryStatement"
    assert lexer.pos == 1
    mocks["TryStatement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)


@pytest.mark.parametrize("Return", [True, False])
def test_parse_Statement_DebuggerStatement(mocker, context, Return):
    # Statement : DebuggerStatement
    lexer = Lexer([DEBUGGER_STMT_REPLACEMENT])
    mocks = Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)
    assert isinstance(stmt, e.P2_Statement_DebuggerStatement)
    assert stmt.DebuggerStatement == "DebuggerStatement"
    assert lexer.pos == 1
    mocks["DebuggerStatement"].assert_called_with(context, lexer, "StrictArg")


def test_parse_Statement_ReturnStatement_false(mocker, context):
    # Return when return isn't allowed
    lexer = Lexer([RETURN_STMT_REPLACEMENT])
    Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", False)
    assert stmt is None
    assert lexer.pos == 0


@pytest.mark.parametrize("Return", [True, False])
@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_Statement_SyntaxError(mocker, context, token_stream, Return):
    # Syntax Errors
    lexer = Lexer(token_stream)
    Statement_mocks(mocker)

    stmt = e.parse_Statement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Return)
    assert stmt is None
    assert lexer.pos == 0


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
    decl = e.P2_Declaration(context, "StrictArg", ["child"])
    assert decl.name == "Declaration"
    assert decl.context == context
    assert decl.children == ["child"]


def test_P2_Declaration_HoistableDeclaration_init(context):
    decl = e.P2_Declaration_HoistableDeclaration(context, "StrictArg", ["child"])
    assert decl.name == "Declaration"
    assert decl.HoistableDeclaration == "child"


def test_P2_Declaration_ClassDeclaration_init(context):
    decl = e.P2_Declaration_ClassDeclaration(context, "StrictArg", ["child"])
    assert decl.name == "Declaration"
    assert decl.ClassDeclaration == "child"


def test_P2_Declaration_LexicalDeclaration_init(context):
    decl = e.P2_Declaration_LexicalDeclaration(context, "StrictArg", ["child"])
    assert decl.name == "Declaration"
    assert decl.LexicalDeclaration == "child"


def Declaration_mocks(mocker):
    return {
        "HoistableDeclaration": mocker.patch(
            "ecmascript.ecmascript.parse_HoistableDeclaration", side_effect=hd_sideeffect
        ),
        "ClassDeclaration": mocker.patch("ecmascript.ecmascript.parse_ClassDeclaration", side_effect=cd_sideeffect),
        "LexicalDeclaration": mocker.patch(
            "ecmascript.ecmascript.parse_LexicalDeclaration", side_effect=ld_sideeffect
        ),
    }


@pytest.mark.parametrize(
    "token_stream, expected_class, parm_test, parm_name, subcall_args",
    [
        (
            [HD_REPLACEMENT],
            e.P2_Declaration_HoistableDeclaration,
            lambda decl: decl.HoistableDeclaration == "HoistableDeclaration",
            "HoistableDeclaration",
            ("YieldArg", "AwaitArg", False),
        ),
        (
            [CD_REPLACEMENT],
            e.P2_Declaration_ClassDeclaration,
            lambda decl: decl.ClassDeclaration == "ClassDeclaration",
            "ClassDeclaration",
            ("YieldArg", "AwaitArg", False),
        ),
        (
            [LD_REPLACEMENT],
            e.P2_Declaration_LexicalDeclaration,
            lambda decl: decl.LexicalDeclaration == "LexicalDeclaration",
            "LexicalDeclaration",
            (True, "YieldArg", "AwaitArg"),
        ),
    ],
)
def test_parse_Declaration_01(mocker, context, token_stream, expected_class, parm_test, parm_name, subcall_args):
    # Declaration : HoistableDeclaration
    # Declaration : ClassDeclaration
    # Declaration : LexicalDeclaration
    lexer = Lexer(token_stream)
    mocks = Declaration_mocks(mocker)

    decl = e.parse_Declaration(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(decl, expected_class)
    assert lexer.pos == 1
    assert parm_test(decl)
    mocks[parm_name].assert_called_with(context, lexer, "StrictArg", *subcall_args)


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_Declaration_02(mocker, context, token_stream):
    # Syntax errors
    lexer = Lexer(token_stream)
    Declaration_mocks(mocker)

    decl = e.parse_Declaration(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert decl is None
    assert lexer.pos == 0


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
    hd = e.P2_HoistableDeclaration(context, "StrictArg", ["child"])
    assert hd.name == "HoistableDeclaration"
    assert hd.context == context
    assert hd.children == ["child"]


def test_P2_HoistableDeclaration_FunctionDeclaration_init(context):
    hd = e.P2_HoistableDeclaration_FunctionDeclaration(context, "StrictArg", ["child"])
    assert hd.name == "HoistableDeclaration"
    assert hd.FunctionDeclaration == "child"


def test_P2_HoistableDeclaration_GeneratorDeclaration_init(context):
    hd = e.P2_HoistableDeclaration_GeneratorDeclaration(context, "StrictArg", ["child"])
    assert hd.name == "HoistableDeclaration"
    assert hd.GeneratorDeclaration == "child"


def test_P2_HoistableDeclaration_AsyncFunctionDeclaration_init(context):
    hd = e.P2_HoistableDeclaration_AsyncFunctionDeclaration(context, "StrictArg", ["child"])
    assert hd.name == "HoistableDeclaration"
    assert hd.AsyncFunctionDeclaration == "child"


def test_P2_HoistableDeclaration_AsyncGeneratorDeclaration_init(context):
    hd = e.P2_HoistableDeclaration_AsyncGeneratorDeclaration(context, "StrictArg", ["child"])
    assert hd.name == "HoistableDeclaration"
    assert hd.AsyncGeneratorDeclaration == "child"


def HoistableDeclaration_mocks(mocker):
    return {
        "FunctionDeclaration": mocker.patch(
            "ecmascript.ecmascript.parse_FunctionDeclaration", side_effect=functiondeclaration_sideeffect
        ),
        "GeneratorDeclaration": mocker.patch(
            "ecmascript.ecmascript.parse_GeneratorDeclaration", side_effect=gd_sideeffect
        ),
        "AsyncFunctionDeclaration": mocker.patch(
            "ecmascript.ecmascript.parse_AsyncFunctionDeclaration", side_effect=afd_sideeffect
        ),
        "AsyncGeneratorDeclaration": mocker.patch(
            "ecmascript.ecmascript.parse_AsyncGeneratorDeclaration", side_effect=agd_sideeffect
        ),
    }


@pytest.mark.parametrize(
    "token_stream, expected_class, prop_name",
    [
        ([FUNCTIONDECLARATION], e.P2_HoistableDeclaration_FunctionDeclaration, "FunctionDeclaration"),
        ([GD_REPLACEMENT], e.P2_HoistableDeclaration_GeneratorDeclaration, "GeneratorDeclaration"),
        ([AFD_REPLACEMENT], e.P2_HoistableDeclaration_AsyncFunctionDeclaration, "AsyncFunctionDeclaration"),
        ([AGD_REPLACEMENT], e.P2_HoistableDeclaration_AsyncGeneratorDeclaration, "AsyncGeneratorDeclaration"),
    ],
)
def test_parse_HoistableDeclaration_01(mocker, context, token_stream, expected_class, prop_name):
    lexer = Lexer(token_stream)
    mocks = HoistableDeclaration_mocks(mocker)

    hd = e.parse_HoistableDeclaration(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "DefaultArg")
    assert isinstance(hd, expected_class)
    assert getattr(hd, prop_name) == prop_name
    assert lexer.pos == 1
    mocks[prop_name].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "DefaultArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_HoistableDeclaration_02(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    HoistableDeclaration_mocks(mocker)

    hd = e.parse_HoistableDeclaration(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "DefaultArg")
    assert hd is None
    assert lexer.pos == 0


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
    bs = e.P2_BreakableStatement(context, "StrictArg", ["child"])
    assert bs.name == "BreakableStatement"
    assert bs.context == context
    assert bs.children == ["child"]


def test_P2_BreakableStatement_IterationStatement_init(context):
    bs = e.P2_BreakableStatement_IterationStatement(context, "StrictArg", ["child"])
    assert bs.name == "BreakableStatement"
    assert bs.IterationStatement == "child"


def test_P2_BreakableStatement_SwitchStatement_init(context):
    bs = e.P2_BreakableStatement_SwitchStatement(context, "StrictArg", ["child"])
    assert bs.name == "BreakableStatement"
    assert bs.SwitchStatement == "child"


def BreakableStatement_mocks(mocker):
    return {
        "IterationStatement": mocker.patch(
            "ecmascript.ecmascript.parse_IterationStatement", side_effect=is_sideeffect
        ),
        "SwitchStatement": mocker.patch("ecmascript.ecmascript.parse_SwitchStatement", side_effect=ss_sideeffect),
    }


@pytest.mark.parametrize(
    "token_stream, expected_class, prop_name",
    [
        ([IS_REPLACEMENT], e.P2_BreakableStatement_IterationStatement, "IterationStatement"),
        ([SS_REPLACEMENT], e.P2_BreakableStatement_SwitchStatement, "SwitchStatement"),
    ],
)
def test_parse_BreakableStatement_01(mocker, context, token_stream, expected_class, prop_name):
    lexer = Lexer(token_stream)
    mocks = BreakableStatement_mocks(mocker)

    bs = e.parse_BreakableStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
    assert isinstance(bs, expected_class)
    assert getattr(bs, prop_name) == prop_name
    assert lexer.pos == 1
    mocks[prop_name].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_BreakableStatement_02(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    BreakableStatement_mocks(mocker)

    bs = e.parse_BreakableStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
    assert bs is None
    assert lexer.pos == 0


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
    bs = e.P2_BlockStatement(context, "StrictArg", ["child"])
    assert bs.name == "BlockStatement"
    assert bs.context == context
    assert bs.children == ["child"]


def test_P2_BlockStatement_Block_init(context):
    bs = e.P2_BlockStatement_Block(context, "StrictArg", ["child"])
    assert bs.name == "BlockStatement"
    assert bs.Block == "child"


def BlockStatement_mocks(mocker):
    return {"Block": mocker.patch("ecmascript.ecmascript.parse_Block", side_effect=block_sideeffect)}


def test_parse_BlockStatement_01(mocker, context):
    lexer = Lexer([BLOCK])
    mocks = BlockStatement_mocks(mocker)

    bs = e.parse_BlockStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
    assert isinstance(bs, e.P2_BlockStatement_Block)
    assert bs.Block == "Block"
    assert lexer.pos == 1
    mocks["Block"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_BlockStatement_02(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    BlockStatement_mocks(mocker)

    bs = e.parse_BlockStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
    assert bs is None
    assert lexer.pos == 0


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
    block = e.P2_Block(context, "StrictArg", ["child"])
    assert block.name == "Block"
    assert block.context == context
    assert block.children == ["child"]


def test_P2_Block_Empty_init(context):
    block = e.P2_Block_Empty(context, "StrictArg", ["child"])
    assert block.name == "Block"


def test_P2_Block_StatementList_init(context):
    block = e.P2_Block_StatementList(context, "StrictArg", ["{", "stmt", "}"])
    assert block.name == "Block"
    assert block.StatementList == "stmt"


def Block_mocks(mocker):
    return {
        "StatementList": mocker.patch(
            "ecmascript.ecmascript.parse_StatementList", side_effect=statementlist_sideeffect
        )
    }


@pytest.mark.parametrize(
    "token_stream, expected_class, prop_name",
    [
        ([LCURLY, RCURLY], e.P2_Block_Empty, None),
        ([LCURLY, STATEMENTLIST, RCURLY], e.P2_Block_StatementList, "StatementList"),
    ],
)
def test_parse_Block_01(mocker, context, token_stream, expected_class, prop_name):
    lexer = Lexer(token_stream)
    mocks = Block_mocks(mocker)

    block = e.parse_Block(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
    assert isinstance(block, expected_class)
    assert lexer.pos == len(token_stream)
    if prop_name:
        assert getattr(block, prop_name) == prop_name
        mocks[prop_name].assert_called_with(context, lexer, "StrictArg", False, "YieldArg", "AwaitArg", "ReturnArg")


@pytest.mark.parametrize(
    "token_stream", [[], [MATCHES_NONE], [LCURLY, MATCHES_NONE], [LCURLY, STATEMENTLIST, MATCHES_NONE]]
)
def test_parse_Block_02(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    Block_mocks(mocker)

    block = e.parse_Block(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
    assert block is None
    assert lexer.pos == 0


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
    sl = e.P2_StatementList(context, "StrictArg", ["child"])
    assert sl.name == "StatementList"
    assert sl.context == context
    assert sl.children == ["child"]


def test_P2_StatementList_StatementListItem_init(context):
    sl = e.P2_StatementList_StatementListItem(context, "StrictArg", ["child"])
    assert sl.name == "StatementList"
    assert sl.StatementListItem == "child"


def test_P2_StatementList_StatementList_StatementListItem_init(context):
    sl = e.P2_StatementList_StatementList_StatementListItem(context, "StrictArg", ["sl", "sli"])
    assert sl.name == "StatementList"
    assert sl.StatementList == "sl"
    assert sl.StatementListItem == "sli"


def StatementList_mocks(mocker):
    return {"sli": mocker.patch("ecmascript.ecmascript.parse_StatementListItem", side_effect=sli_sideeffect)}


def test_parse_StatementList_StatementListItem(mocker, context):
    # StatementList : StatementListItem
    lexer = Lexer([SLI_REPLACEMENT])
    mocks = StatementList_mocks(mocker)

    sl = e.parse_StatementList(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg", "ReturnArg")
    assert isinstance(sl, e.P2_StatementList_StatementListItem)
    assert sl.StatementListItem.name == "StatementListItem"
    assert lexer.pos == 1
    mocks["sli"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")


def test_parse_StatementList_StatementList_StatementListItem(mocker, context):
    # StatementList : StatementList StatementListItem
    lexer = Lexer([SLI_REPLACEMENT, SLI_REPLACEMENT])
    mocks = StatementList_mocks(mocker)

    sl = e.parse_StatementList(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg", "ReturnArg")
    assert isinstance(sl, e.P2_StatementList_StatementList_StatementListItem)
    assert sl.StatementListItem.name == "StatementListItem"
    assert lexer.pos == 2
    mocks["sli"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
    sl2 = sl.StatementList
    assert isinstance(sl2, e.P2_StatementList_StatementListItem)


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_StatementList_errors_nr(mocker, context, token_stream):
    # Syntax Errors, non-recursive
    lexer = Lexer(token_stream)
    StatementList_mocks(mocker)

    sl = e.parse_StatementList(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg", "ReturnArg")
    assert sl is None
    assert lexer.pos == 0


def test_parse_StatementList_errors_r(mocker, context):
    # Syntax Errors, recursive
    lexer = Lexer([SLI_REPLACEMENT, MATCHES_NONE])
    StatementList_mocks(mocker)

    sl = e.parse_StatementList(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg", "ReturnArg")
    assert isinstance(sl, e.P2_StatementList_StatementListItem)
    assert lexer.pos == 1


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
    sli = e.P2_StatementListItem(context, "StrictArg", ["child"])
    assert sli.name == "StatementListItem"
    assert sli.context == context
    assert sli.children == ["child"]


def test_P2_StatementListItem_Statement_init(context):
    sli = e.P2_StatementListItem_Statement(context, "StrictArg", ["child"])
    assert sli.name == "StatementListItem"
    assert sli.Statement == "child"


def test_P2_StatementListItem_Declaration_init(context):
    sli = e.P2_StatementListItem_Declaration(context, "StrictArg", ["child"])
    assert sli.name == "StatementListItem"
    assert sli.Declaration == "child"


def StatementListItem_mocks(mocker):
    return {
        "stmt": mocker.patch("ecmascript.ecmascript.parse_Statement", side_effect=statement_sideeffect),
        "decl": mocker.patch("ecmascript.ecmascript.parse_Declaration", side_effect=decl_sideeffect),
    }


def test_parse_StatementListItem_Statement(mocker, context):
    # StatementListItem : Statement
    lexer = Lexer([STATEMENT])
    mocks = StatementListItem_mocks(mocker)

    sli = e.parse_StatementListItem(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
    assert isinstance(sli, e.P2_StatementListItem_Statement)
    assert sli.Statement == "Statement"
    assert lexer.pos == 1
    mocks["stmt"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")


def test_parse_StatementListItem_Declaration(mocker, context):
    # StatementListItem : Declaration
    lexer = Lexer([DECL_REPLACEMENT])
    mocks = StatementListItem_mocks(mocker)

    sli = e.parse_StatementListItem(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
    assert isinstance(sli, e.P2_StatementListItem_Declaration)
    assert sli.Declaration == "Declaration"
    assert lexer.pos == 1
    mocks["decl"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_StatementListItem_error(mocker, context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)
    StatementListItem_mocks(mocker)

    sli = e.parse_StatementListItem(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
    assert sli is None
    assert lexer.pos == 0


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
    ld = e.P2_LexicalDeclaration(context, "StrictArg", ["child"])
    assert ld.name == "LexicalDeclaration"
    assert ld.context == context
    assert ld.children == ["child"]


def test_P2_LexicalDeclaration_LetOrConst_BindingList_init(context):
    ld = e.P2_LexicalDeclaration_LetOrConst_BindingList(context, "StrictArg", ["const", "bindings", ";"])
    assert ld.name == "LexicalDeclaration"
    assert ld.LetOrConst == "const"
    assert ld.BindingList == "bindings"


def LexicalDeclaration_mocks(mocker):
    return {
        "LetOrConst": mocker.patch("ecmascript.ecmascript.parse_LetOrConst", side_effect=letorconst_sideeffect),
        "BindingList": mocker.patch("ecmascript.ecmascript.parse_BindingList", side_effect=bindinglist_sideeffect),
    }


def test_parse_LexicalDeclaration_01(mocker, context):
    lexer = Lexer([LETORCONST_REPLACEMENT, BINDINGLIST_REPLACEMENT, SEMICOLON])
    mocks = LexicalDeclaration_mocks(mocker)

    ld = e.parse_LexicalDeclaration(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(ld, e.P2_LexicalDeclaration_LetOrConst_BindingList)
    assert ld.LetOrConst == "LetOrConst"
    assert ld.BindingList == "BindingList"
    assert lexer.pos == 3
    mocks["LetOrConst"].assert_called_with(context, lexer, "StrictArg")
    mocks["BindingList"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "token_stream",
    [
        [],
        [MATCHES_NONE],
        [LETORCONST_REPLACEMENT, MATCHES_NONE],
        [LETORCONST_REPLACEMENT, BINDINGLIST_REPLACEMENT, MATCHES_NONE],
    ],
)
def test_parse_LexicalDeclaration_02(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    LexicalDeclaration_mocks(mocker)

    ld = e.parse_LexicalDeclaration(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert ld is None
    assert lexer.pos == 0


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
    loc = e.P2_LetOrConst(context, "StrictArg", ["child"])
    assert loc.name == "LetOrConst"
    assert loc.context == context
    assert loc.children == ["child"]


def test_P2_LetOrConst_Let_init(context):
    loc = e.P2_LetOrConst_Let(context, "StrictArg", ["child"])
    assert loc.name == "LetOrConst"


def test_P2_LetOrConst_Const_init(context):
    loc = e.P2_LetOrConst_Const(context, "StrictArg", ["child"])
    assert loc.name == "LetOrConst"


@pytest.mark.parametrize("token, expected_class", [(LET, e.P2_LetOrConst_Let), (CONST, e.P2_LetOrConst_Const)])
def test_LetOrConst_01(context, token, expected_class):
    lexer = Lexer([token])

    loc = e.parse_LetOrConst(context, lexer, "StrictArg")
    assert isinstance(loc, expected_class)
    assert lexer.pos == 1


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_LetOrConst_02(context, token_stream):
    lexer = Lexer(token_stream)

    loc = e.parse_LetOrConst(context, lexer, "StrictArg")
    assert loc is None
    assert lexer.pos == 0


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
    bl = e.P2_BindingList(context, "StrictArg", ["child"])
    assert bl.name == "BindingList"
    assert bl.context == context
    assert bl.children == ["child"]


def test_P2_BindingList_LexicalBinding_init(context):
    bl = e.P2_BindingList_LexicalBinding(context, "StrictArg", ["child"])
    assert bl.name == "BindingList"
    assert bl.LexicalBinding == "child"


def test_P2_BindingList_BindingList_LexicalBinding_init(context):
    bl = e.P2_BindingList_BindingList_LexicalBinding(context, "StrictArg", ["child", ",", "sibling"])
    assert bl.name == "BindingList"
    assert bl.LexicalBinding == "sibling"
    assert bl.BindingList == "child"


def BindingList_mocks(mocker):
    return {
        "LexicalBinding": mocker.patch(
            "ecmascript.ecmascript.parse_LexicalBinding", side_effect=lexicalbinding_sideeffect
        )
    }


def test_parse_BindingList_01(mocker, context):
    lexer = Lexer([LEXICALBINDING_REPLACEMENT])
    mocks = BindingList_mocks(mocker)

    bl = e.parse_BindingList(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(bl, e.P2_BindingList_LexicalBinding)
    assert lexer.pos == 1
    assert bl.LexicalBinding == "LexicalBinding"
    mocks["LexicalBinding"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


def test_parse_BindingList_02(mocker, context):
    lexer = Lexer([LEXICALBINDING_REPLACEMENT, COMMA, LEXICALBINDING_REPLACEMENT])
    mocks = BindingList_mocks(mocker)

    bl = e.parse_BindingList(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(bl, e.P2_BindingList_BindingList_LexicalBinding)
    assert bl.LexicalBinding == "LexicalBinding"
    assert lexer.pos == 3
    bl2 = bl.BindingList
    assert isinstance(bl2, e.P2_BindingList_LexicalBinding)
    mocks["LexicalBinding"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_BindingList_03(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    BindingList_mocks(mocker)

    bl = e.parse_BindingList(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert bl is None
    assert lexer.pos == 0


@pytest.mark.parametrize(
    "token_stream", [[LEXICALBINDING_REPLACEMENT, MATCHES_NONE], [LEXICALBINDING_REPLACEMENT, COMMA, MATCHES_NONE]]
)
def test_parse_BindingList_04(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    BindingList_mocks(mocker)

    bl = e.parse_BindingList(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(bl, e.P2_BindingList_LexicalBinding)
    assert lexer.pos == 1


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
    lb = e.P2_LexicalBinding(context, "StrictArg", ["child"])
    assert lb.name == "LexicalBinding"
    assert lb.context == context
    assert lb.children == ["child"]


def test_P2_LexicalBinding_BindingIdentifier_init(context):
    lb = e.P2_LexicalBinding_BindingIdentifier(context, "StrictArg", ["child"])
    assert lb.name == "LexicalBinding"
    assert lb.BindingIdentifier == "child"


def test_P2_LexicalBinding_BindingIdentifier_Initializer_init(context):
    lb = e.P2_LexicalBinding_BindingIdentifier_Initializer(context, "StrictArg", ["child", "init"])
    assert lb.name == "LexicalBinding"
    assert lb.BindingIdentifier == "child"
    assert lb.Initializer == "init"


def test_P2_LexicalBinding_BindingPattern_Initializer_init(context):
    lb = e.P2_LexicalBinding_BindingPattern_Initializer(context, "StrictArg", ["child", "init"])
    assert lb.name == "LexicalBinding"
    assert lb.BindingPattern == "child"
    assert lb.Initializer == "init"


def LexicalBinding_mocks(mocker):
    return {
        "BindingIdentifier": mocker.patch(
            "ecmascript.ecmascript.parse_BindingIdentifier", side_effect=bindingidentifier_sideeffect
        ),
        "BindingPattern": mocker.patch(
            "ecmascript.ecmascript.parse_BindingPattern", side_effect=bindingpattern_sideeffect
        ),
        "Initializer": mocker.patch("ecmascript.ecmascript.parse_Initializer", side_effect=init_sideeffect),
    }


@pytest.mark.parametrize(
    "token_stream, expected_class, prop_name, has_init",
    [
        ([BINDINGIDENTIFIER], e.P2_LexicalBinding_BindingIdentifier, "BindingIdentifier", False),
        (
            [BINDINGIDENTIFIER, INIT_REPLACEMENT],
            e.P2_LexicalBinding_BindingIdentifier_Initializer,
            "BindingIdentifier",
            True,
        ),
        ([BINDINGPATTERN, INIT_REPLACEMENT], e.P2_LexicalBinding_BindingPattern_Initializer, "BindingPattern", True),
    ],
)
def test_parse_LexicalBinding_01(mocker, context, token_stream, expected_class, prop_name, has_init):
    lexer = Lexer(token_stream)
    mocks = LexicalBinding_mocks(mocker)

    lb = e.parse_LexicalBinding(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(lb, expected_class)
    assert getattr(lb, prop_name) == prop_name
    assert lexer.pos == len(token_stream)
    mocks[prop_name].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    if has_init:
        assert lb.Initializer == "Initializer"
        mocks["Initializer"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE], [BINDINGPATTERN, MATCHES_NONE]])
def test_parse_LexicalBinding_02(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    LexicalBinding_mocks(mocker)

    lb = e.parse_LexicalBinding(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert lb is None
    assert lexer.pos == 0


@pytest.mark.parametrize("token_stream", [[BINDINGIDENTIFIER, MATCHES_NONE]])
def test_parse_LexicalBinding_03(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    LexicalBinding_mocks(mocker)

    lb = e.parse_LexicalBinding(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(lb, e.P2_LexicalBinding_BindingIdentifier)
    assert lexer.pos == 1


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
    vs = e.P2_VariableStatement(context, "StrictArg", ["child"])
    assert vs.name == "VariableStatement"
    assert vs.context == context
    assert vs.children == ["child"]


def test_P2_VariableStatement_VariableDeclarationList_init(context):
    vs = e.P2_VariableStatement_VariableDeclarationList(context, "StrictArg", ["var", "vdl", ";"])
    assert vs.name == "VariableStatement"
    assert vs.VariableDeclarationList == "vdl"


def VariableStatement_mocks(mocker):
    return {
        "VariableDeclarationList": mocker.patch(
            "ecmascript.ecmascript.parse_VariableDeclarationList", side_effect=variabledeclarationlist_sideeffect
        )
    }


def test_parse_VariableStatement_01(mocker, context):
    lexer = Lexer([VAR, VARIABLEDECLARATIONLIST, SEMICOLON])
    mocks = VariableStatement_mocks(mocker)

    vdl = e.parse_VariableStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(vdl, e.P2_VariableStatement_VariableDeclarationList)
    assert vdl.VariableDeclarationList == "VariableDeclarationList"
    assert lexer.pos == 3
    mocks["VariableDeclarationList"].assert_called_with(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "token_stream", [[], [MATCHES_NONE], [VAR, MATCHES_NONE], [VAR, VARIABLEDECLARATIONLIST, MATCHES_NONE]]
)
def test_parse_VariableStatement_02(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    VariableStatement_mocks(mocker)

    vdl = e.parse_VariableStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert vdl is None
    assert lexer.pos == 0


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
    vdl = e.P2_VariableDeclarationList(context, "StrictArg", ["child"])
    assert vdl.name == "VariableDeclarationList"
    assert vdl.context == context
    assert vdl.children == ["child"]


def test_P2_VariableDeclarationList_VariableDeclaration_init(context):
    vdl = e.P2_VariableDeclarationList_VariableDeclaration(context, "StrictArg", ["child"])
    assert vdl.name == "VariableDeclarationList"
    assert vdl.VariableDeclaration == "child"


def test_P2_VariableDeclarationList_VariableDeclarationList_VariableDeclaration_init(context):
    vdl = e.P2_VariableDeclarationList_VariableDeclarationList_VariableDeclaration(
        context, "StrictArg", ["child", ",", "sibling"]
    )
    assert vdl.name == "VariableDeclarationList"
    assert vdl.VariableDeclarationList == "child"
    assert vdl.VariableDeclaration == "sibling"


def VariableDeclarationList_mocks(mocker):
    return {
        "VariableDeclaration": mocker.patch(
            "ecmascript.ecmascript.parse_VariableDeclaration", side_effect=variabledeclaration_sideeffect
        )
    }


def test_parse_VariableDeclarationList_01(mocker, context):
    lexer = Lexer([VARIABLEDECLARATION])
    mocks = VariableDeclarationList_mocks(mocker)

    vdl = e.parse_VariableDeclarationList(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(vdl, e.P2_VariableDeclarationList_VariableDeclaration)
    assert vdl.VariableDeclaration == "VariableDeclaration"
    assert lexer.pos == 1
    mocks["VariableDeclaration"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


def test_parse_VariableDeclarationList_02(mocker, context):
    lexer = Lexer([VARIABLEDECLARATION, COMMA, VARIABLEDECLARATION])
    mocks = VariableDeclarationList_mocks(mocker)

    vdl = e.parse_VariableDeclarationList(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(vdl, e.P2_VariableDeclarationList_VariableDeclarationList_VariableDeclaration)
    assert vdl.VariableDeclaration == "VariableDeclaration"
    assert lexer.pos == 3
    mocks["VariableDeclaration"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    vdl2 = vdl.VariableDeclarationList
    assert isinstance(vdl2, e.P2_VariableDeclarationList_VariableDeclaration)


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_VariableDeclarationList_03(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    VariableDeclarationList_mocks(mocker)

    vdl = e.parse_VariableDeclarationList(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert vdl is None
    assert lexer.pos == 0


@pytest.mark.parametrize(
    "token_stream", [[VARIABLEDECLARATION, MATCHES_NONE], [VARIABLEDECLARATION, COMMA, MATCHES_NONE]]
)
def test_parse_VariableDeclarationList_04(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    VariableDeclarationList_mocks(mocker)

    vdl = e.parse_VariableDeclarationList(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(vdl, e.P2_VariableDeclarationList_VariableDeclaration)
    assert lexer.pos == 1


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
    vd = e.P2_VariableDeclaration(context, "StrictArg", ["child"])
    assert vd.name == "VariableDeclaration"
    assert vd.context == context
    assert vd.children == ["child"]


def test_P2_VariableDeclaration_BindingIdentifier_init(context):
    vd = e.P2_VariableDeclaration_BindingIdentifier(context, "StrictArg", ["child"])
    assert vd.name == "VariableDeclaration"
    assert vd.BindingIdentifier == "child"


def test_P2_VariableDeclaration_BindingIdentifier_Initializer_init(context):
    vd = e.P2_VariableDeclaration_BindingIdentifier_Initializer(context, "StrictArg", ["child", "init"])
    assert vd.name == "VariableDeclaration"
    assert vd.BindingIdentifier == "child"
    assert vd.Initializer == "init"


def test_P2_VariableDeclaration_BindingPattern_Initializer_init(context):
    vd = e.P2_VariableDeclaration_BindingPattern_Initializer(context, "StrictArg", ["child", "init"])
    assert vd.name == "VariableDeclaration"
    assert vd.BindingPattern == "child"
    assert vd.Initializer == "init"


def VariableDeclaration_mocks(mocker):
    return {
        "BindingIdentifier": mocker.patch(
            "ecmascript.ecmascript.parse_BindingIdentifier", side_effect=bindingidentifier_sideeffect
        ),
        "BindingPattern": mocker.patch(
            "ecmascript.ecmascript.parse_BindingPattern", side_effect=bindingpattern_sideeffect
        ),
        "Initializer": mocker.patch("ecmascript.ecmascript.parse_Initializer", side_effect=init_sideeffect),
    }


@pytest.mark.parametrize(
    "token_stream, expected_class, prop_name, has_init",
    [
        ([BINDINGIDENTIFIER], e.P2_VariableDeclaration_BindingIdentifier, "BindingIdentifier", False),
        (
            [BINDINGIDENTIFIER, INIT_REPLACEMENT],
            e.P2_VariableDeclaration_BindingIdentifier_Initializer,
            "BindingIdentifier",
            True,
        ),
        (
            [BINDINGPATTERN, INIT_REPLACEMENT],
            e.P2_VariableDeclaration_BindingPattern_Initializer,
            "BindingPattern",
            True,
        ),
    ],
)
def test_parse_VariableDeclaration_01(mocker, context, token_stream, expected_class, prop_name, has_init):
    lexer = Lexer(token_stream)
    mocks = VariableDeclaration_mocks(mocker)

    vd = e.parse_VariableDeclaration(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(vd, expected_class)
    assert getattr(vd, prop_name) == prop_name
    assert lexer.pos == len(token_stream)
    mocks[prop_name].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    if has_init:
        assert vd.Initializer == "Initializer"
        mocks["Initializer"].assert_called_with(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE], [BINDINGPATTERN, MATCHES_NONE]])
def test_parse_VariableDeclaration_02(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    VariableDeclaration_mocks(mocker)

    lb = e.parse_VariableDeclaration(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert lb is None
    assert lexer.pos == 0


@pytest.mark.parametrize("token_stream", [[BINDINGIDENTIFIER, MATCHES_NONE]])
def test_parse_VariableDeclaration_03(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    VariableDeclaration_mocks(mocker)

    lb = e.parse_VariableDeclaration(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
    assert isinstance(lb, e.P2_VariableDeclaration_BindingIdentifier)
    assert lexer.pos == 1


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
    bp = e.P2_BindingPattern(context, "StrictArg", ["child"])
    assert bp.name == "BindingPattern"
    assert bp.context == context
    assert bp.children == ["child"]


def test_P2_BindingPattern_ObjectBindingPattern_init(context):
    bp = e.P2_BindingPattern_ObjectBindingPattern(context, "StrictArg", ["child"])
    assert bp.name == "BindingPattern"
    assert bp.ObjectBindingPattern == "child"


def test_P2_BindingPattern_ArrayBindingPattern_init(context):
    bp = e.P2_BindingPattern_ArrayBindingPattern(context, "StrictArg", ["child"])
    assert bp.name == "BindingPattern"
    assert bp.ArrayBindingPattern == "child"


def BindingPattern_mocks(mocker):
    return {
        "ObjectBindingPattern": mocker.patch(
            "ecmascript.ecmascript.parse_ObjectBindingPattern", side_effect=objectbindingpattern_sideeffect
        ),
        "ArrayBindingPattern": mocker.patch(
            "ecmascript.ecmascript.parse_ArrayBindingPattern", side_effect=arraybindingpattern_sideeffect
        ),
    }


@pytest.mark.parametrize(
    "token, name, expected_class",
    [
        (OBJECTBINDINGPATTERN, "ObjectBindingPattern", e.P2_BindingPattern_ObjectBindingPattern),
        (ARRAYBINDINGPATTERN, "ArrayBindingPattern", e.P2_BindingPattern_ArrayBindingPattern),
    ],
)
def test_parse_BindingPattern_01(mocker, context, token, name, expected_class):
    lexer = Lexer([token])
    mocks = BindingPattern_mocks(mocker)

    bp = e.parse_BindingPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(bp, expected_class)
    assert getattr(bp, name) == name
    assert lexer.pos == 1
    mocks[name].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_BindingPattern_02(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    BindingPattern_mocks(mocker)

    bp = e.parse_BindingPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert bp is None
    assert lexer.pos == 0


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
    obp = e.P2_ObjectBindingPattern(context, "StrictArg", ["child"])
    assert obp.name == "ObjectBindingPattern"
    assert obp.context == context
    assert obp.children == ["child"]


def test_P2_ObjectBindingPattern_Empty_init(context):
    obp = e.P2_ObjectBindingPattern_Empty(context, "StrictArg", ["child"])
    assert obp.name == "ObjectBindingPattern"


def test_P2_ObjectBindingPattern_BindingRestProperty_init(context):
    obp = e.P2_ObjectBindingPattern_BindingRestProperty(context, "StrictArg", ["{", "child", "}"])
    assert obp.name == "ObjectBindingPattern"
    assert obp.BindingRestProperty == "child"


def test_P2_ObjectBindingPattern_BindingPropertyList_init(context):
    obp = e.P2_ObjectBindingPattern_BindingPropertyList(context, "StrictArg", ["{", "child", "}"])
    assert obp.name == "ObjectBindingPattern"
    assert obp.BindingPropertyList == "child"


def test_P2_ObjectBindingPattern_BindingPropertyList_BindingRestProperty_init(context):
    obp = e.P2_ObjectBindingPattern_BindingPropertyList_BindingRestProperty(
        context, "StrictArg", ["{", "child", ",", "sibling", "}"]
    )
    assert obp.name == "ObjectBindingPattern"
    assert obp.BindingPropertyList == "child"
    assert obp.BindingRestProperty == "sibling"


def ObjectBindingPattern_mocks(mocker):
    return {
        "BindingRestProperty": mocker.patch(
            "ecmascript.ecmascript.parse_BindingRestProperty", side_effect=bindingrestproperty_sideeffect
        ),
        "BindingPropertyList": mocker.patch(
            "ecmascript.ecmascript.parse_BindingPropertyList", side_effect=bindingpropertylist_sideeffect
        ),
    }


@pytest.mark.parametrize(
    "token_stream, expected_class, has_proplist, has_restprop, has_comma",
    [
        ([LCURLY, RCURLY], e.P2_ObjectBindingPattern_Empty, False, False, False),
        ([LCURLY, BINDINGRESTPROPERTY, RCURLY], e.P2_ObjectBindingPattern_BindingRestProperty, False, True, False),
        ([LCURLY, BINDINGPROPERTYLIST, RCURLY], e.P2_ObjectBindingPattern_BindingPropertyList, True, False, False),
        (
            [LCURLY, BINDINGPROPERTYLIST, COMMA, RCURLY],
            e.P2_ObjectBindingPattern_BindingPropertyList,
            True,
            False,
            True,
        ),
        (
            [LCURLY, BINDINGPROPERTYLIST, COMMA, BINDINGRESTPROPERTY, RCURLY],
            e.P2_ObjectBindingPattern_BindingPropertyList_BindingRestProperty,
            True,
            True,
            True,
        ),
    ],
)
def test_parse_ObjectBindingPattern_01(
    mocker, context, token_stream, expected_class, has_proplist, has_restprop, has_comma
):
    lexer = Lexer(token_stream)
    mocks = ObjectBindingPattern_mocks(mocker)

    obp = e.parse_ObjectBindingPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(obp, expected_class)
    assert (not has_proplist) or (obp.BindingPropertyList == "BindingPropertyList")
    assert (not has_restprop) or (obp.BindingRestProperty == "BindingRestProperty")
    if has_restprop:
        mocks["BindingRestProperty"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    if has_proplist:
        mocks["BindingPropertyList"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert lexer.pos == len(token_stream)
    assert obp.children[0] == LCURLY
    assert obp.children[-1] == RCURLY
    assert (not has_comma) or obp.children[2] == COMMA


@pytest.mark.parametrize(
    "token_stream",
    [
        [],
        [MATCHES_NONE],
        [LCURLY, MATCHES_NONE],
        [LCURLY, BINDINGRESTPROPERTY, MATCHES_NONE],
        [LCURLY, BINDINGPROPERTYLIST, MATCHES_NONE],
        [LCURLY, BINDINGPROPERTYLIST, COMMA, MATCHES_NONE],
        [LCURLY, BINDINGPROPERTYLIST, COMMA, BINDINGRESTPROPERTY, MATCHES_NONE],
    ],
)
def test_parse_ObjectBindingPattern_02(token_stream, mocker, context):
    lexer = Lexer(token_stream)
    ObjectBindingPattern_mocks(mocker)
    obp = e.parse_ObjectBindingPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert obp is None
    assert lexer.pos == 0


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
    abp = e.P2_ArrayBindingPattern(context, "StrictArg", ["child"])
    assert abp.name == "ArrayBindingPattern"
    assert abp.context == context
    assert abp.children == ["child"]


def test_P2_ArrayBindingPattern_Empty_init(context):
    abp = e.P2_ArrayBindingPattern_Empty(context, "StrictArg", ["child"])
    assert abp.name == "ArrayBindingPattern"


def test_P2_ArrayBindingPattern_Elision_init(context):
    abp = e.P2_ArrayBindingPattern_Elision(context, "StrictArg", ["[", "child", "]"])
    assert abp.name == "ArrayBindingPattern"
    assert abp.Elision == "child"


def test_P2_ArrayBindingPattern_BindingRestElement_init(context):
    abp = e.P2_ArrayBindingPattern_BindingRestElement(context, "StrictArg", ["[", "child", "]"])
    assert abp.name == "ArrayBindingPattern"
    assert abp.BindingRestElement == "child"


def test_P2_ArrayBindingPattern_Elision_BindingRestElement_init(context):
    abp = e.P2_ArrayBindingPattern_Elision_BindingRestElement(context, "StrictArg", ["[", ",,,", "child", "]"])
    assert abp.name == "ArrayBindingPattern"
    assert abp.Elision == ",,,"
    assert abp.BindingRestElement == "child"


def test_P2_ArrayBindingPattern_BindingElementList_init(context):
    abp = e.P2_ArrayBindingPattern_BindingElementList(context, "StrictArg", ["[", "child", "]"])
    assert abp.name == "ArrayBindingPattern"
    assert abp.BindingElementList == "child"


def test_P2_ArrayBindingPattern_BindingElementList_Elision_init(context):
    abp = e.P2_ArrayBindingPattern_BindingElementList_Elision(context, "StrictArg", ["[", "child", ",", ",,,", "]"])
    assert abp.name == "ArrayBindingPattern"
    assert abp.BindingElementList == "child"
    assert abp.Elision == ",,,"


def test_P2_ArrayBindingPattern_BindingElementList_BindingRestElement_init(context):
    abp = e.P2_ArrayBindingPattern_BindingElementList_BindingRestElement(
        context, "StrictArg", ["[", "child", ",", "...bre", "]"]
    )
    assert abp.name == "ArrayBindingPattern"
    assert abp.BindingElementList == "child"
    assert abp.BindingRestElement == "...bre"


def test_P2_ArrayBindingPattern_BindingElementList_Elision_BindingRestElement_init(context):
    abp = e.P2_ArrayBindingPattern_BindingElementList_Elision_BindingRestElement(
        context, "StrictArg", ["[", "child", ",", ",,,", "...bre", "]"]
    )
    assert abp.name == "ArrayBindingPattern"
    assert abp.BindingElementList == "child"
    assert abp.Elision == ",,,"
    assert abp.BindingRestElement == "...bre"


def ArrayBindingPattern_mocks(mocker):
    return {
        "BindingElementList": mocker.patch(
            "ecmascript.ecmascript.parse_BindingElementList", side_effect=bindingelementlist_sideeffect
        ),
        "Elision": mocker.patch("ecmascript.ecmascript.parse_Elision", side_effect=elision_sideeffect),
        "BindingRestElement": mocker.patch(
            "ecmascript.ecmascript.parse_BindingRestElement", side_effect=bindingrestelement_sideeffect
        ),
    }


@pytest.mark.parametrize(
    "token_stream, expected_class, has_bel, has_elision, has_bre",
    [
        ([LBRACKET, RBRACKET], e.P2_ArrayBindingPattern_Empty, False, False, False),
        ([LBRACKET, ELISION_REPLACEMENT, RBRACKET], e.P2_ArrayBindingPattern_Elision, False, True, False),
        ([LBRACKET, BINDINGRESTELEMENT, RBRACKET], e.P2_ArrayBindingPattern_BindingRestElement, False, False, True),
        (
            [LBRACKET, ELISION_REPLACEMENT, BINDINGRESTELEMENT, RBRACKET],
            e.P2_ArrayBindingPattern_Elision_BindingRestElement,
            False,
            True,
            True,
        ),
        ([LBRACKET, BINDINGELEMENTLIST, RBRACKET], e.P2_ArrayBindingPattern_BindingElementList, True, False, False),
        (
            [LBRACKET, BINDINGELEMENTLIST, COMMA, RBRACKET],
            e.P2_ArrayBindingPattern_BindingElementList,
            True,
            False,
            False,
        ),
        (
            [LBRACKET, BINDINGELEMENTLIST, COMMA, ELISION_REPLACEMENT, RBRACKET],
            e.P2_ArrayBindingPattern_BindingElementList_Elision,
            True,
            True,
            False,
        ),
        (
            [LBRACKET, BINDINGELEMENTLIST, COMMA, BINDINGRESTELEMENT, RBRACKET],
            e.P2_ArrayBindingPattern_BindingElementList_BindingRestElement,
            True,
            False,
            True,
        ),
        (
            [LBRACKET, BINDINGELEMENTLIST, COMMA, ELISION_REPLACEMENT, BINDINGRESTELEMENT, RBRACKET],
            e.P2_ArrayBindingPattern_BindingElementList_Elision_BindingRestElement,
            True,
            True,
            True,
        ),
    ],
)
def test_parse_ArrayBindingPattern_01(mocker, context, token_stream, expected_class, has_bel, has_elision, has_bre):
    lexer = Lexer(token_stream)
    mocks = ArrayBindingPattern_mocks(mocker)

    mock_check = gen_mock_check(mocker, mocks, context, lexer)
    elision_mock_check = mock_check("Elision", has_elision, "StrictArg")
    bre_mock_check = mock_check("BindingRestElement", has_bre, "StrictArg", "YieldArg", "AwaitArg")
    bel_mock_check = mock_check("BindingElementList", has_bel, "StrictArg", "YieldArg", "AwaitArg")

    abp = e.parse_ArrayBindingPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(abp, expected_class)
    assert lexer.pos == len(token_stream)
    assert abp.children[0] == LBRACKET
    assert abp.children[-1] == RBRACKET
    assert (not has_bel) or abp.BindingElementList == "BindingElementList"
    assert (not has_elision) or abp.Elision == "Elision"
    assert (not has_bre) or abp.BindingRestElement == "BindingRestElement"
    assert not has_bel or len(token_stream) < 4 or abp.children[2] == COMMA
    elision_mock_check()
    bre_mock_check()
    bel_mock_check()


@pytest.mark.parametrize(
    "token_stream",
    [
        [],
        [MATCHES_NONE],
        [LBRACKET, MATCHES_NONE],
        [LBRACKET, ELISION_REPLACEMENT, MATCHES_NONE],
        [LBRACKET, BINDINGRESTELEMENT, MATCHES_NONE],
        [LBRACKET, ELISION_REPLACEMENT, BINDINGRESTELEMENT, MATCHES_NONE],
        [LBRACKET, BINDINGELEMENTLIST, MATCHES_NONE],
        [LBRACKET, BINDINGELEMENTLIST, COMMA, MATCHES_NONE],
        [LBRACKET, BINDINGELEMENTLIST, COMMA, ELISION_REPLACEMENT, MATCHES_NONE],
        [LBRACKET, BINDINGELEMENTLIST, COMMA, BINDINGRESTELEMENT, MATCHES_NONE],
        [LBRACKET, BINDINGELEMENTLIST, COMMA, ELISION_REPLACEMENT, BINDINGRESTELEMENT, MATCHES_NONE],
    ],
)
def test_parse_ArrayBindingPattern_02(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    ArrayBindingPattern_mocks(mocker)

    abp = e.parse_ArrayBindingPattern(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert abp is None
    assert lexer.pos == 0


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
    brp = e.P2_BindingRestProperty(context, "StrictArg", ["child"])
    assert brp.name == "BindingRestProperty"
    assert brp.context == context
    assert brp.children == ["child"]


def test_P2_BindingRestProperty_BindingIdentifier_init(context):
    brp = e.P2_BindingRestProperty_BindingIdentifier(context, "StrictArg", ["...", "child"])
    assert brp.name == "BindingRestProperty"
    assert brp.BindingIdentifier == "child"


def BindingRestProperty_mocks(mocker):
    return {
        "BindingIdentifier": mocker.patch(
            "ecmascript.ecmascript.parse_BindingIdentifier", side_effect=bindingidentifier_sideeffect
        )
    }


def test_parse_BindingRestProperty_01(mocker, context):
    lexer = Lexer([DOTDOTDOT, BINDINGIDENTIFIER])
    mocks = BindingRestProperty_mocks(mocker)

    brp = e.parse_BindingRestProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(brp, e.P2_BindingRestProperty_BindingIdentifier)
    assert brp.BindingIdentifier == "BindingIdentifier"
    assert brp.children == [DOTDOTDOT, "BindingIdentifier"]
    assert lexer.pos == 2
    mocks["BindingIdentifier"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE], [DOTDOTDOT, MATCHES_NONE]])
def test_parse_BindingRestProperty_02(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    mocks = BindingRestProperty_mocks(mocker)
    mock_check = gen_mock_check(mocker, mocks, context, lexer)
    brp_check = mock_check("BindingIdentifier", False, "StrictArg", "YieldArg", "AwaitArg")

    brp = e.parse_BindingRestProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert brp is None
    assert lexer.pos == 0
    brp_check()


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
    bpl = e.P2_BindingPropertyList(context, "StrictArg", ["child"])
    assert bpl.name == "BindingPropertyList"
    assert bpl.context == context
    assert bpl.children == ["child"]


def test_P2_BindingPropertyList_BindingProperty_init(context):
    bpl = e.P2_BindingPropertyList_BindingProperty(context, "StrictArg", ["child"])
    assert bpl.name == "BindingPropertyList"
    assert bpl.BindingProperty == "child"


def test_P2_BindingPropertyList_BindingPropertyList_BindingProperty_init(context):
    bpl = e.P2_BindingPropertyList_BindingPropertyList_BindingProperty(context, "StrictArg", ["list", ",", "child"])
    assert bpl.name == "BindingPropertyList"
    assert bpl.BindingPropertyList == "list"
    assert bpl.BindingProperty == "child"


def BindingPropertyList_mocks(mocker):
    return {
        "BindingProperty": mocker.patch(
            "ecmascript.ecmascript.parse_BindingProperty", side_effect=bindingproperty_sideeffect
        )
    }


def test_parse_BindingPropertyList_01(mocker, context):
    lexer = Lexer([BINDINGPROPERTY])
    mocks = BindingPropertyList_mocks(mocker)

    bpl = e.parse_BindingPropertyList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(bpl, e.P2_BindingPropertyList_BindingProperty)
    assert bpl.BindingProperty == "BindingProperty"
    assert lexer.pos == 1
    mocks["BindingProperty"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_BindingPropertyList_02(mocker, context):
    lexer = Lexer([BINDINGPROPERTY, COMMA, BINDINGPROPERTY])
    mocks = BindingPropertyList_mocks(mocker)

    bpl = e.parse_BindingPropertyList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(bpl, e.P2_BindingPropertyList_BindingPropertyList_BindingProperty)
    assert bpl.BindingProperty == "BindingProperty"
    assert bpl.children[1] == COMMA
    bpl2 = bpl.BindingPropertyList
    assert isinstance(bpl2, e.P2_BindingPropertyList_BindingProperty)
    mocks["BindingProperty"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_BindingPropertyList_03(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    mocks = BindingPropertyList_mocks(mocker)
    mock_check = gen_mock_check(mocker, mocks, context, lexer)
    bp_check = mock_check("BindingProperty", False, "StrictArg", "YieldArg", "AwaitArg")

    bpl = e.parse_BindingPropertyList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert bpl is None
    assert lexer.pos == 0
    bp_check()


@pytest.mark.parametrize("token_stream", [[BINDINGPROPERTY, MATCHES_NONE], [BINDINGPROPERTY, COMMA, MATCHES_NONE]])
def test_parse_BindingPropertyList_04(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    mocks = BindingPropertyList_mocks(mocker)
    mock_check = gen_mock_check(mocker, mocks, context, lexer)
    bp_check = mock_check("BindingProperty", False, "StrictArg", "YieldArg", "AwaitArg")

    bpl = e.parse_BindingPropertyList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(bpl, e.P2_BindingPropertyList_BindingProperty)
    assert lexer.pos == 1
    bp_check()


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
    bel = e.P2_BindingElementList(context, "StrictArg", ["child"])
    assert bel.name == "BindingElementList"
    assert bel.context == context
    assert bel.children == ["child"]


def test_P2_BindingElementList_BindingElement_init(context):
    bel = e.P2_BindingElementList_BindingElisionElement(context, "StrictArg", ["child"])
    assert bel.name == "BindingElementList"
    assert bel.BindingElisionElement == "child"


def test_P2_BindingElementList_BindingElementList_BindingElisionElement_init(context):
    bel = e.P2_BindingElementList_BindingElementList_BindingElisionElement(
        context, "StrictArg", ["list", ",", "child"]
    )
    assert bel.name == "BindingElementList"
    assert bel.BindingElementList == "list"
    assert bel.BindingElisionElement == "child"


def BindingElementList_mocks(mocker):
    return {
        "BindingElisionElement": mocker.patch(
            "ecmascript.ecmascript.parse_BindingElisionElement", side_effect=bindingelisionelement_sideeffect
        )
    }


def test_parse_BindingElementList_01(mocker, context):
    lexer = Lexer([BINDINGELISIONELEMENT])
    mocks = BindingElementList_mocks(mocker)

    bel = e.parse_BindingElementList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(bel, e.P2_BindingElementList_BindingElisionElement)
    assert bel.BindingElisionElement == "BindingElisionElement"
    assert lexer.pos == 1
    mocks["BindingElisionElement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


def test_parse_BindingElementList_02(mocker, context):
    lexer = Lexer([BINDINGELISIONELEMENT, COMMA, BINDINGELISIONELEMENT])
    mocks = BindingElementList_mocks(mocker)

    bel = e.parse_BindingElementList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(bel, e.P2_BindingElementList_BindingElementList_BindingElisionElement)
    assert bel.BindingElisionElement == "BindingElisionElement"
    assert bel.children[1] == COMMA
    bel2 = bel.BindingElementList
    assert isinstance(bel2, e.P2_BindingElementList_BindingElisionElement)
    mocks["BindingElisionElement"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_BindingElementList_03(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    mocks = BindingElementList_mocks(mocker)
    mock_check = gen_mock_check(mocker, mocks, context, lexer)
    bee_check = mock_check("BindingElisionElement", False, "StrictArg", "YieldArg", "AwaitArg")

    bel = e.parse_BindingElementList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert bel is None
    assert lexer.pos == 0
    bee_check()


@pytest.mark.parametrize(
    "token_stream", [[BINDINGELISIONELEMENT, MATCHES_NONE], [BINDINGELISIONELEMENT, COMMA, MATCHES_NONE]]
)
def test_parse_BindingElementList_04(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    mocks = BindingElementList_mocks(mocker)
    mock_check = gen_mock_check(mocker, mocks, context, lexer)
    bee_check = mock_check("BindingElisionElement", False, "StrictArg", "YieldArg", "AwaitArg")

    bel = e.parse_BindingElementList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(bel, e.P2_BindingElementList_BindingElisionElement)
    assert lexer.pos == 1
    bee_check()


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
    bee = e.P2_BindingElisionElement(context, "StrictArg", ["child"])
    assert bee.name == "BindingElisionElement"
    assert bee.context == context
    assert bee.children == ["child"]


def test_P2_BindingElisionElement_BindingElement(context):
    bee = e.P2_BindingElisionElement_BindingElement(context, "StrictArg", ["child"])
    assert bee.name == "BindingElisionElement"
    assert bee.BindingElement == "child"


def test_P2_BindingElisionElement_Elision_BindingElement(context):
    bee = e.P2_BindingElisionElement_Elision_BindingElement(context, "StrictArg", [",,,", "child"])
    assert bee.name == "BindingElisionElement"
    assert bee.Elision == ",,,"
    assert bee.BindingElement == "child"


def BindingElisionElement_mocks(mocker):
    return {
        "Elision": mocker.patch("ecmascript.ecmascript.parse_Elision", side_effect=elision_sideeffect),
        "BindingElement": mocker.patch(
            "ecmascript.ecmascript.parse_BindingElement", side_effect=bindingelement_sideeffect
        ),
    }


@pytest.mark.parametrize(
    "token_stream, expected_class, has_elision",
    [
        ([BINDINGELEMENT], e.P2_BindingElisionElement_BindingElement, False),
        ([ELISION_REPLACEMENT, BINDINGELEMENT], e.P2_BindingElisionElement_Elision_BindingElement, True),
    ],
)
def test_parse_BindingElisionElement_01(mocker, context, token_stream, expected_class, has_elision):
    lexer = Lexer(token_stream)
    mocks = BindingElisionElement_mocks(mocker)
    mock_check = gen_mock_check(mocker, mocks, context, lexer)
    elision_mock_check = mock_check("Elision", has_elision, "StrictArg")
    be_mock_check = mock_check("BindingElement", True, "StrictArg", "YieldArg", "AwaitArg")

    bee = e.parse_BindingElisionElement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(bee, expected_class)
    assert bee.BindingElement == "BindingElement"
    assert not has_elision or bee.Elision == "Elision"
    assert lexer.pos == len(token_stream)
    elision_mock_check()
    be_mock_check()


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE], [ELISION_REPLACEMENT, MATCHES_NONE]])
def test_parse_BindingElisionElement_02(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    mocks = BindingElisionElement_mocks(mocker)
    mock_check = gen_mock_check(mocker, mocks, context, lexer)
    elision_mock_check = mock_check("Elision", False, "StrictArg")
    be_mock_check = mock_check("BindingElement", False, "StrictArg", "YieldArg", "AwaitArg")

    bee = e.parse_BindingElisionElement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert bee is None
    assert lexer.pos == 0
    elision_mock_check()
    be_mock_check()


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
    bp = e.P2_BindingProperty(context, "StrictArg", ["child"])
    assert bp.name == "BindingProperty"
    assert bp.context == context
    assert bp.children == ["child"]


def test_P2_BindingProperty_SingleNameBinding(context):
    bp = e.P2_BindingProperty_SingleNameBinding(context, "StrictArg", ["child"])
    assert bp.name == "BindingProperty"
    assert bp.SingleNameBinding == "child"


def test_P2_BindingProperty_PropertyName_BindingElement(context):
    bp = e.P2_BindingProperty_PropertyName_BindingElement(context, "StrictArg", ["prop", ":", "child"])
    assert bp.name == "BindingProperty"
    assert bp.PropertyName == "prop"
    assert bp.BindingElement == "child"


def BindingProperty_mocks(mocker):
    return {
        "SingleNameBinding": mocker.patch(
            "ecmascript.ecmascript.parse_SingleNameBinding", side_effect=singlenamebinding_sideeffect
        ),
        "PropertyName": mocker.patch("ecmascript.ecmascript.parse_PropertyName", side_effect=pn_sideeffect),
        "BindingElement": mocker.patch(
            "ecmascript.ecmascript.parse_BindingElement", side_effect=bindingelement_sideeffect
        ),
    }


@pytest.mark.parametrize(
    "token_stream, expected_class, has_single",
    [
        ([SINGLENAMEBINDING], e.P2_BindingProperty_SingleNameBinding, True),
        ([PN_REPLACEMENT, COLON, BINDINGELEMENT], e.P2_BindingProperty_PropertyName_BindingElement, False),
    ],
)
def test_parse_BindingProperty_01(mocker, context, token_stream, expected_class, has_single):
    lexer = Lexer(token_stream)
    mocks = BindingProperty_mocks(mocker)
    mock_check = gen_mock_check(mocker, mocks, context, lexer)
    snb_mock_check = mock_check("SingleNameBinding", has_single, "StrictArg", "YieldArg", "AwaitArg")
    pn_mock_check = mock_check("PropertyName", not has_single, "StrictArg", "YieldArg", "AwaitArg")
    be_mock_check = mock_check("BindingElement", not has_single, "StrictArg", "YieldArg", "AwaitArg")

    bp = e.parse_BindingProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(bp, expected_class)
    assert not has_single or bp.SingleNameBinding == "SingleNameBinding"
    assert has_single or bp.PropertyName == "PropertyName"
    assert has_single or bp.BindingElement == "BindingElement"
    assert has_single or bp.children[1] == COLON
    assert lexer.pos == len(token_stream)
    snb_mock_check()
    pn_mock_check()
    be_mock_check()


@pytest.mark.parametrize(
    "token_stream", [[], [MATCHES_NONE], [PN_REPLACEMENT, MATCHES_NONE], [PN_REPLACEMENT, COLON, MATCHES_NONE]]
)
def test_parse_BindingProperty_02(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    mocks = BindingProperty_mocks(mocker)
    mock_check = gen_mock_check(mocker, mocks, context, lexer)
    snb_mock_check = mock_check("SingleNameBinding", False, "StrictArg", "YieldArg", "AwaitArg")
    pn_mock_check = mock_check("PropertyName", False, "StrictArg", "YieldArg", "AwaitArg")
    be_mock_check = mock_check("BindingElement", False, "StrictArg", "YieldArg", "AwaitArg")

    bp = e.parse_BindingProperty(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert bp is None
    assert lexer.pos == 0
    snb_mock_check()
    pn_mock_check()
    be_mock_check()


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
    be = e.P2_BindingElement(context, "StrictArg", ["child"])
    assert be.name == "BindingElement"
    assert be.context == context
    assert be.children == ["child"]


def test_P2_BindingElement_SingleNameBinding_init(context):
    be = e.P2_BindingElement_SingleNameBinding(context, "StrictArg", ["child"])
    assert be.name == "BindingElement"
    assert be.SingleNameBinding == "child"


def test_P2_BindingElement_BindingPattern_init(context):
    be = e.P2_BindingElement_BindingPattern(context, "StrictArg", ["pattern"])
    assert be.name == "BindingElement"
    assert be.BindingPattern == "pattern"


def test_P2_BindingElement_BindingPattern_Initializer_init(context):
    be = e.P2_BindingElement_BindingPattern_Initializer(context, "StrictArg", ["pattern", "=child"])
    assert be.name == "BindingElement"
    assert be.BindingPattern == "pattern"
    assert be.Initializer == "=child"


def BindingElement_mocks(mocker):
    return {
        "SingleNameBinding": mocker.patch(
            "ecmascript.ecmascript.parse_SingleNameBinding", side_effect=singlenamebinding_sideeffect
        ),
        "BindingPattern": mocker.patch(
            "ecmascript.ecmascript.parse_BindingPattern", side_effect=bindingpattern_sideeffect
        ),
        "Initializer": mocker.patch("ecmascript.ecmascript.parse_Initializer", side_effect=init_sideeffect),
    }


@pytest.mark.parametrize(
    "token_stream, expected_class, has_single, has_init",
    [
        ([SINGLENAMEBINDING], e.P2_BindingElement_SingleNameBinding, True, False),
        ([BINDINGPATTERN], e.P2_BindingElement_BindingPattern, False, False),
        ([BINDINGPATTERN, INIT_REPLACEMENT], e.P2_BindingElement_BindingPattern_Initializer, False, True),
    ],
)
def test_parse_BindingElement_01(mocker, context, token_stream, expected_class, has_single, has_init):
    lexer = Lexer(token_stream)
    mocks = BindingElement_mocks(mocker)
    mock_check = gen_mock_check(mocker, mocks, context, lexer)
    snb_mock_check = mock_check("SingleNameBinding", has_single, "StrictArg", "YieldArg", "AwaitArg")
    bp_mock_check = mock_check("BindingPattern", not has_single, "StrictArg", "YieldArg", "AwaitArg")
    init_mock_check = mock_check("Initializer", has_init, "StrictArg", True, "YieldArg", "AwaitArg")

    be = e.parse_BindingElement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(be, expected_class)
    assert not has_single or be.SingleNameBinding == "SingleNameBinding"
    assert has_single or be.BindingPattern == "BindingPattern"
    assert not has_init or be.Initializer == "Initializer"
    assert lexer.pos == len(token_stream)
    snb_mock_check()
    bp_mock_check()
    init_mock_check()


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_BindingElement_02(mocker, context, token_stream):
    lexer = Lexer(token_stream)
    mocks = BindingElement_mocks(mocker)
    mock_check = gen_mock_check(mocker, mocks, context, lexer)
    snb_mock_check = mock_check("SingleNameBinding", False, "StrictArg", "YieldArg", "AwaitArg")
    bp_mock_check = mock_check("BindingPattern", False, "StrictArg", "YieldArg", "AwaitArg")
    init_mock_check = mock_check("Initializer", False, "StrictArg", True, "YieldArg", "AwaitArg")

    be = e.parse_BindingElement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert be is None
    assert lexer.pos == 0
    snb_mock_check()
    bp_mock_check()
    init_mock_check()


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
        snb = e.P2_SingleNameBinding(context, "StrictArg", ["child"])
        assert snb.name == "SingleNameBinding"
        assert snb.context == context
        assert snb.children == ["child"]

    def test_P2_SingleNameBinding_BindingIdentifier_init(self, context):
        snb = e.P2_SingleNameBinding_BindingIdentifier(context, "StrictArg", ["child"])
        assert snb.name == "SingleNameBinding"
        assert snb.BindingIdentifier == "child"

    def test_P2_SingleNameBinding_BindingIdentifier_Initializer_init(self, context):
        snb = e.P2_SingleNameBinding_BindingIdentifier_Initializer(context, "StrictArg", ["child", "=3"])
        assert snb.name == "SingleNameBinding"
        assert snb.BindingIdentifier == "child"
        assert snb.Initializer == "=3"

    @staticmethod
    def SingleNameBinding_mocks(mocker):
        return {
            "BindingIdentifier": mocker.patch(
                "ecmascript.ecmascript.parse_BindingIdentifier", side_effect=bindingidentifier_sideeffect
            ),
            "Initializer": mocker.patch("ecmascript.ecmascript.parse_Initializer", side_effect=init_sideeffect),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, has_init",
        [
            ([BINDINGIDENTIFIER], e.P2_SingleNameBinding_BindingIdentifier, False),
            ([BINDINGIDENTIFIER, INIT_REPLACEMENT], e.P2_SingleNameBinding_BindingIdentifier_Initializer, True),
        ],
    )
    def test_parse_SingleNameBinding_01(self, mocker, context, token_stream, expected_class, has_init):
        lexer = Lexer(token_stream)
        mocks = self.SingleNameBinding_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        bi_mock_check = mock_check("BindingIdentifier", True, "StrictArg", "YieldArg", "AwaitArg")
        init_mock_check = mock_check("Initializer", has_init, "StrictArg", True, "YieldArg", "AwaitArg")

        snb = e.parse_SingleNameBinding(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(snb, expected_class)
        assert snb.BindingIdentifier == "BindingIdentifier"
        assert (not has_init) or snb.Initializer == "Initializer"
        assert lexer.pos == len(token_stream)
        bi_mock_check()
        init_mock_check()

    @pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
    def test_parse_SingleNameBinding_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.SingleNameBinding_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        bi_mock_check = mock_check("BindingIdentifier", False, "StrictArg", "YieldArg", "AwaitArg")
        init_mock_check = mock_check("Initializer", False, "StrictArg", True, "YieldArg", "AwaitArg")

        snb = e.parse_SingleNameBinding(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert snb is None
        assert lexer.pos == 0
        bi_mock_check()
        init_mock_check()


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
        bre = e.P2_BindingRestElement(context, "StrictArg", ["child"])
        assert bre.name == "BindingRestElement"
        assert bre.context == context
        assert bre.children == ["child"]

    @staticmethod
    def test_P2_BindingRestElement_BindingIdentifier_init(context):
        bre = e.P2_BindingRestElement_BindingIdentifier(context, "StrictArg", ["...", "child"])
        assert bre.name == "BindingRestElement"
        assert bre.BindingIdentifier == "child"

    @staticmethod
    def test_P2_BindingRestElement_BindingPattern_init(context):
        bre = e.P2_BindingRestElement_BindingPattern(context, "StrictArg", ["...", "child"])
        assert bre.name == "BindingRestElement"
        assert bre.BindingPattern == "child"

    @staticmethod
    def BindingRestElement_mocks(mocker):
        return {
            "BindingIdentifier": mocker.patch(
                "ecmascript.ecmascript.parse_BindingIdentifier", side_effect=bindingidentifier_sideeffect
            ),
            "BindingPattern": mocker.patch(
                "ecmascript.ecmascript.parse_BindingPattern", side_effect=bindingpattern_sideeffect
            ),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, has_BindingIdentifier",
        [
            ([DOTDOTDOT, BINDINGIDENTIFIER], e.P2_BindingRestElement_BindingIdentifier, True),
            ([DOTDOTDOT, BINDINGPATTERN], e.P2_BindingRestElement_BindingPattern, False),
        ],
    )
    def test_parse_BindingRestElement_01(self, mocker, context, token_stream, expected_class, has_BindingIdentifier):
        lexer = Lexer(token_stream)
        mocks = self.BindingRestElement_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        bi_mock_check = mock_check("BindingIdentifier", has_BindingIdentifier, "StrictArg", "YieldArg", "AwaitArg")
        bp_mock_check = mock_check("BindingPattern", not has_BindingIdentifier, "StrictArg", "YieldArg", "AwaitArg")

        bre = e.parse_BindingRestElement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(bre, expected_class)
        assert bre.children[0] == DOTDOTDOT
        assert (not has_BindingIdentifier) or bre.BindingIdentifier == "BindingIdentifier"
        assert has_BindingIdentifier or bre.BindingPattern == "BindingPattern"
        assert lexer.pos == len(token_stream)
        bi_mock_check()
        bp_mock_check()

    @pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE], [DOTDOTDOT, MATCHES_NONE]])
    def test_parse_BindingRestElement_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.BindingRestElement_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        bi_mock_check = mock_check("BindingIdentifier", False, "StrictArg", "YieldArg", "AwaitArg")
        bp_mock_check = mock_check("BindingPattern", False, "StrictArg", "YieldArg", "AwaitArg")

        bre = e.parse_BindingRestElement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert bre is None
        assert lexer.pos == 0
        bi_mock_check()
        bp_mock_check()


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
    es = e.P2_EmptyStatement(context, "StrictArg", ["child"])
    assert es.name == "EmptyStatement"
    assert es.context == context
    assert es.children == ["child"]


def test_P2_EmptyStatement_SEMICOLON_init(context):
    es = e.P2_EmptyStatement_SEMICOLON(context, "StrictArg", ["child"])
    assert es.name == "EmptyStatement"


def test_parse_EmptyStatement_SEMICOLON(context):
    lexer = Lexer([SEMICOLON])

    es = e.parse_EmptyStatement(context, lexer, "StrictArg")
    assert isinstance(es, e.P2_EmptyStatement_SEMICOLON)
    assert lexer.pos == 1


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_EmptyStatement_error(context, token_stream):
    # Syntax Errors
    lexer = Lexer(token_stream)

    es = e.parse_EmptyStatement(context, lexer, "StrictArg")
    assert es is None
    assert lexer.pos == 0


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
    es = e.P2_ExpressionStatement(context, "StrictArg", ["child"])
    assert es.name == "ExpressionStatement"
    assert es.context == context
    assert es.children == ["child"]


def test_P2_ExpressionStatement_Expression_SEMICOLON_init(context):
    es = e.P2_ExpressionStatement_Expression_SEMICOLON(context, "StrictArg", ["exp_stmt", ";"])
    assert es.name == "ExpressionStatement"
    assert es.Expression == "exp_stmt"


def ExpressionStatement_mocks(mocker):
    return {"exp": mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect)}


def test_parse_ExpressionStatement_Expression_SEMI(mocker, context):
    # ExpressionStatement : Expression SEMICOLON
    lexer = Lexer([EXPRESSION, SEMICOLON])
    mocks = ExpressionStatement_mocks(mocker)

    exps = e.parse_ExpressionStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert isinstance(exps, e.P2_ExpressionStatement_Expression_SEMICOLON)
    assert exps.Expression == "Expression"
    assert lexer.pos == 2
    mocks["exp"].assert_called_with(context, lexer, "StrictArg", True, "YieldArg", "AwaitArg")


@pytest.mark.parametrize(
    "token_stream",
    [
        [],
        [MATCHES_NONE],
        [EXPRESSION, MATCHES_NONE],
        [LCURLY],
        [FUNCTION],
        [ASYNC, FUNCTION_NONL],
        [CLASS],
        [LET, LBRACKET],
    ],
)
def test_parse_ExpressionStatement_errs(mocker, context, token_stream):
    # Syntax and Lookahead errs
    lexer = Lexer(token_stream)
    ExpressionStatement_mocks(mocker)

    exps = e.parse_ExpressionStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
    assert exps is None
    assert lexer.pos == 0


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
        ifs = e.P2_IfStatement(context, "StrictArg", ["child"])
        assert ifs.name == "IfStatement"
        assert ifs.context == context
        assert ifs.children == ["child"]

    def test_P2_IfStatement_Expression_Statement_Statement_init(self, context):
        ifs = e.P2_IfStatement_Expression_Statement_Statement(context, "StrictArg", ["child"])
        assert ifs.name == "IfStatement"

    def test_P2_IfStatement_Expression_Statement_init(self, context):
        ifs = e.P2_IfStatement_Expression_Statement(context, "StrictArg", ["child"])
        assert ifs.name == "IfStatement"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "Expression": mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect),
            "Statement": mocker.patch("ecmascript.ecmascript.parse_Statement", side_effect=statement_sideeffect),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, has_else",
        [
            (
                [IF, LPAREN, EXPRESSION, RPAREN, STATEMENT, ELSE, STATEMENT],
                e.P2_IfStatement_Expression_Statement_Statement,
                True,
            ),
            ([IF, LPAREN, EXPRESSION, RPAREN, STATEMENT], e.P2_IfStatement_Expression_Statement, False),
        ],
    )
    def test_parse_IfStatement_01(self, mocker, context, token_stream, expected_class, has_else):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        exp_mock_check = mock_check("Expression", True, "StrictArg", True, "YieldArg", "AwaitArg")
        stmt_mock_check = mock_check("Statement", True, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")

        ifs = e.parse_IfStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert isinstance(ifs, expected_class)
        assert ifs.Expression == "Expression"
        assert ifs.children[0] == IF
        assert ifs.children[1] == LPAREN
        assert ifs.children[3] == RPAREN
        assert not has_else or ifs.children[5] == ELSE
        assert not has_else or ifs.Statement1 == "Statement"
        assert has_else or ifs.Statement == "Statement"
        assert not has_else or ifs.Statement2 == "Statement"
        assert lexer.pos == len(token_stream)
        exp_mock_check()
        stmt_mock_check()

    @pytest.mark.parametrize(
        "token_stream",
        [
            [],
            [MATCHES_NONE],
            [IF, MATCHES_NONE],
            [IF, LPAREN, MATCHES_NONE],
            [IF, LPAREN, EXPRESSION, MATCHES_NONE],
            [IF, LPAREN, EXPRESSION, RPAREN, MATCHES_NONE],
        ],
    )
    def test_parse_IfStatement_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        exp_mock_check = mock_check("Expression", False, "StrictArg", True, "YieldArg", "AwaitArg")
        stmt_mock_check = mock_check("Statement", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")

        ifs = e.parse_IfStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert ifs is None
        assert lexer.pos == 0
        exp_mock_check()
        stmt_mock_check()

    @pytest.mark.parametrize(
        "token_stream",
        [
            [IF, LPAREN, EXPRESSION, RPAREN, STATEMENT, MATCHES_NONE],
            [IF, LPAREN, EXPRESSION, RPAREN, STATEMENT, ELSE, MATCHES_NONE],
        ],
    )
    def test_parse_IfStatement_03(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        exp_mock_check = mock_check("Expression", False, "StrictArg", True, "YieldArg", "AwaitArg")
        stmt_mock_check = mock_check("Statement", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")

        ifs = e.parse_IfStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert isinstance(ifs, e.P2_IfStatement_Expression_Statement)
        assert lexer.pos == 5
        exp_mock_check()
        stmt_mock_check()


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
        istmt = e.P2_IterationStatement(context, "StrictArg", ["child"])
        assert istmt.name == "IterationStatement"
        assert istmt.context == context
        assert istmt.children == ["child"]
        assert istmt.strict == "StrictArg"

    def test_P2_IterationStatement_DO_Statement_WHILE_Expression_init(self, context):
        istmt = e.P2_IterationStatement_DO_Statement_WHILE_Expression(
            context, "StrictArg", ["do", "Statement", "while", "(", "Expression", ")", ";"]
        )
        assert istmt.name == "IterationStatement"
        assert istmt.Statement == "Statement"
        assert istmt.Expression == "Expression"

    def test_P2_IterationStatement_WHILE_Expression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_WHILE_Expression_Statement(
            context, "StrictArg", ["while", "(", "Expression", ")", "Statement"]
        )
        assert istmt.name == "IterationStatement"
        assert istmt.Expression == "Expression"
        assert istmt.Statement == "Statement"

    def test_P2_IterationStatement_FOR_ExpressionInit_ExpressionTest_ExpressionInc_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_ExpressionInit_ExpressionTest_ExpressionInc_Statement(
            context, "StrictArg", ["for", "(", "ExpInit", ";", "ExpTest", ";", "ExpInc", ")", "Statement"]
        )
        assert istmt.name == "IterationStatement"
        assert istmt.ExpressionInit == "ExpInit"
        assert istmt.ExpressionTest == "ExpTest"
        assert istmt.ExpressionInc == "ExpInc"
        assert istmt.Statement == "Statement"

    def test_P2_IterationStatement_FOR_VAR_VariableDeclarationList_ExpressionTest_ExpressionInc_Statement_init(
        self, context
    ):
        istmt = e.P2_IterationStatement_FOR_VAR_VariableDeclarationList_ExpressionTest_ExpressionInc_Statement(
            context,
            "StrictArg",
            [
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
            ],
        )
        assert istmt.name == "IterationStatement"
        assert istmt.VariableDeclarationList == "VariableDeclarationList"
        assert istmt.ExpressionTest == "Expression_Test"
        assert istmt.ExpressionInc == "Expression_Increment"
        assert istmt.Statement == "Statement"

    def test_P2_IterationStatement_FOR_LexicalDeclaration_ExpressionTest_ExpressionInc_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_LexicalDeclaration_ExpressionTest_ExpressionInc_Statement(
            context,
            "StrictArg",
            ["for", "(", "LexicalDeclaration", "Expression_Test", ";", "Expression_Increment", ")", "Statement"],
        )
        assert istmt.name == "IterationStatement"
        assert istmt.LexicalDeclaration == "LexicalDeclaration"
        assert istmt.ExpressionTest == "Expression_Test"
        assert istmt.ExpressionInc == "Expression_Increment"
        assert istmt.Statement == "Statement"

    def test_P2_IterationStatement_FOR_LeftHandSideExpression_IN_Expression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_LeftHandSideExpression_IN_Expression_Statement(
            context, "StrictArg", ["for", "(", "LeftHandSideExpression", "in", "Expression", ")", "Statement"]
        )
        assert istmt.name == "IterationStatement"
        assert istmt.LeftHandSideExpression == "LeftHandSideExpression"
        assert istmt.Expression == "Expression"
        assert istmt.Statement == "Statement"

    def test_P2_IterationStatement_FOR_VAR_ForBinding_IN_Expression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_VAR_ForBinding_IN_Expression_Statement(
            context, "StrictArg", ["for", "(", "var", "ForBinding", "in", "Expression", ")", "Statement"]
        )
        assert istmt.name == "IterationStatement"
        assert istmt.ForBinding == "ForBinding"
        assert istmt.Expression == "Expression"
        assert istmt.Statement == "Statement"

    def test_P2_IterationStatement_FOR_ForDeclaration_IN_Expression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_ForDeclaration_IN_Expression_Statement(
            context, "StrictArg", ["for", "(", "ForDeclaration", "in", "Expression", ")", "Statement"]
        )
        assert istmt.name == "IterationStatement"
        assert istmt.ForDeclaration == "ForDeclaration"
        assert istmt.Expression == "Expression"
        assert istmt.Statement == "Statement"

    def test_P2_IterationStatement_FOR_LeftHandSideExpression_OF_AssignmentExpression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_LeftHandSideExpression_OF_AssignmentExpression_Statement(
            context,
            "StrictArg",
            ["for", "(", "LeftHandSideExpression", "of", "AssignmentExpression", ")", "Statement"],
        )
        assert istmt.name == "IterationStatement"
        assert istmt.LeftHandSideExpression == "LeftHandSideExpression"
        assert istmt.AssignmentExpression == "AssignmentExpression"
        assert istmt.Statement == "Statement"

    def test_P2_IterationStatement_FOR_VAR_ForBinding_OF_AssignmentExpression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_VAR_ForBinding_OF_AssignmentExpression_Statement(
            context, "StrictArg", ["for", "(", "var", "ForBinding", "of", "AssignmentExpression", ")", "Statement"]
        )
        assert istmt.name == "IterationStatement"
        assert istmt.ForBinding == "ForBinding"
        assert istmt.AssignmentExpression == "AssignmentExpression"
        assert istmt.Statement == "Statement"

    def test_P2_IterationStatement_FOR_ForDeclaration_OF_AssignmentExpression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_ForDeclaration_OF_AssignmentExpression_Statement(
            context, "StrictArg", ["for", "(", "ForDeclaration", "of", "AssignmentExpression", ")", "Statement"]
        )
        assert istmt.name == "IterationStatement"
        assert istmt.ForDeclaration == "ForDeclaration"
        assert istmt.AssignmentExpression == "AssignmentExpression"
        assert istmt.Statement == "Statement"

    def test_P2_IterationStatement_FOR_AWAIT_LeftHandSideExpression_OF_AssignmentExpression_Statement_init(
        self, context
    ):
        istmt = e.P2_IterationStatement_FOR_AWAIT_LeftHandSideExpression_OF_AssignmentExpression_Statement(
            context,
            "StrictArg",
            ["for", "await", "(", "LeftHandSideExpression", "of", "AssignmentExpression", ")", "Statement"],
        )
        assert istmt.name == "IterationStatement"
        assert istmt.LeftHandSideExpression == "LeftHandSideExpression"
        assert istmt.AssignmentExpression == "AssignmentExpression"
        assert istmt.Statement == "Statement"

    def test_P2_IterationStatement_FOR_AWAIT_VAR_ForBinding_OF_AssignmentExpression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_AWAIT_VAR_ForBinding_OF_AssignmentExpression_Statement(
            context,
            "StrictArg",
            ["for", "await", "(", "var", "ForBinding", "of", "AssignmentExpression", ")", "Statement"],
        )
        assert istmt.name == "IterationStatement"
        assert istmt.ForBinding == "ForBinding"
        assert istmt.AssignmentExpression == "AssignmentExpression"
        assert istmt.Statement == "Statement"

    def test_P2_IterationStatement_FOR_AWAIT_ForDeclaration_OF_AssignmentExpression_Statement_init(self, context):
        istmt = e.P2_IterationStatement_FOR_AWAIT_ForDeclaration_OF_AssignmentExpression_Statement(
            context,
            "StrictArg",
            ["for", "await", "(", "ForDeclaration", "of", "AssignmentExpression", ")", "Statement"],
        )
        assert istmt.name == "IterationStatement"
        assert istmt.ForDeclaration == "ForDeclaration"
        assert istmt.AssignmentExpression == "AssignmentExpression"
        assert istmt.Statement == "Statement"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "Expression": mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect),
            "Statement": mocker.patch("ecmascript.ecmascript.parse_Statement", side_effect=statement_sideeffect),
            "VariableDeclarationList": mocker.patch(
                "ecmascript.ecmascript.parse_VariableDeclarationList", side_effect=variabledeclarationlist_sideeffect
            ),
            "LexicalDeclaration": mocker.patch(
                "ecmascript.ecmascript.parse_LexicalDeclaration", side_effect=ld_sideeffect
            ),
            "LeftHandSideExpression": mocker.patch(
                "ecmascript.ecmascript.parse_LeftHandSideExpression", side_effect=lhs_sideeffect
            ),
            "ForBinding": mocker.patch("ecmascript.ecmascript.parse_ForBinding", side_effect=forbinding_sideeffect),
            "ForDeclaration": mocker.patch(
                "ecmascript.ecmascript.parse_ForDeclaration", side_effect=fordeclaration_sideeffect
            ),
            "AssignmentExpression": mocker.patch(
                "ecmascript.ecmascript.parse_AssignmentExpression", side_effect=ae_sideeffect
            ),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks, flags",
        [
            (
                # do Statement while ( Expression ) ;
                [DO, STATEMENT, WHILE, LPAREN, EXPRESSION, RPAREN, SEMICOLON],
                e.P2_IterationStatement_DO_Statement_WHILE_Expression,
                ((0, DO), (2, WHILE), (3, LPAREN), (5, RPAREN), (6, SEMICOLON)),
                (("Statement", "Statement"), ("Expression", "Expression")),
                0,
            ),
            (
                # while ( Expression ) Statement
                [WHILE, LPAREN, EXPRESSION, RPAREN, STATEMENT],
                e.P2_IterationStatement_WHILE_Expression_Statement,
                ((0, WHILE), (1, LPAREN), (3, RPAREN)),
                (("Statement", "Statement"), ("Expression", "Expression")),
                0,
            ),
            (
                # for ( Expression ; Expression ; Expression ) Statement
                [FOR, LPAREN, EXPRESSION, SEMICOLON, EXPRESSION, SEMICOLON, EXPRESSION, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_ExpressionInit_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (3, SEMICOLON), (5, SEMICOLON), (7, RPAREN)),
                (
                    ("ExpressionInit", "Expression"),
                    ("ExpressionTest", "Expression"),
                    ("ExpressionInc", "Expression"),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            (
                # for ( ; ; ) Statement
                [FOR, LPAREN, SEMICOLON, SEMICOLON, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_ExpressionInit_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (3, SEMICOLON), (5, SEMICOLON), (7, RPAREN)),
                (
                    ("ExpressionInit", None),
                    ("ExpressionTest", None),
                    ("ExpressionInc", None),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            (
                # for ( ; ; Expression ) Statement
                [FOR, LPAREN, SEMICOLON, SEMICOLON, EXPRESSION, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_ExpressionInit_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (3, SEMICOLON), (5, SEMICOLON), (7, RPAREN)),
                (
                    ("ExpressionInit", None),
                    ("ExpressionTest", None),
                    ("ExpressionInc", "Expression"),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            (
                # for ( ; Expression ; ) Statement
                [FOR, LPAREN, SEMICOLON, EXPRESSION, SEMICOLON, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_ExpressionInit_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (3, SEMICOLON), (5, SEMICOLON), (7, RPAREN)),
                (
                    ("ExpressionInit", None),
                    ("ExpressionTest", "Expression"),
                    ("ExpressionInc", None),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            (
                # for ( ; Expression ; Expression ) Statement
                [FOR, LPAREN, SEMICOLON, EXPRESSION, SEMICOLON, EXPRESSION, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_ExpressionInit_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (3, SEMICOLON), (5, SEMICOLON), (7, RPAREN)),
                (
                    ("ExpressionInit", None),
                    ("ExpressionTest", "Expression"),
                    ("ExpressionInc", "Expression"),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            (
                # for ( Expression ; ; ) Statement
                [FOR, LPAREN, EXPRESSION, SEMICOLON, SEMICOLON, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_ExpressionInit_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (3, SEMICOLON), (5, SEMICOLON), (7, RPAREN)),
                (
                    ("ExpressionInit", "Expression"),
                    ("ExpressionTest", None),
                    ("ExpressionInc", None),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            (
                # for ( Expression ; ; Expression ) Statement
                [FOR, LPAREN, EXPRESSION, SEMICOLON, SEMICOLON, EXPRESSION, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_ExpressionInit_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (3, SEMICOLON), (5, SEMICOLON), (7, RPAREN)),
                (
                    ("ExpressionInit", "Expression"),
                    ("ExpressionTest", None),
                    ("ExpressionInc", "Expression"),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            (
                # for ( Expression ; Expression ; ) Statement
                [FOR, LPAREN, EXPRESSION, SEMICOLON, EXPRESSION, SEMICOLON, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_ExpressionInit_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (3, SEMICOLON), (5, SEMICOLON), (7, RPAREN)),
                (
                    ("ExpressionInit", "Expression"),
                    ("ExpressionTest", "Expression"),
                    ("ExpressionInc", None),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            (
                # for ( var VariableDeclarationList ; Expression ; Expression ) Statement
                [
                    FOR,
                    LPAREN,
                    VAR,
                    VARIABLEDECLARATIONLIST,
                    SEMICOLON,
                    EXPRESSION,
                    SEMICOLON,
                    EXPRESSION,
                    RPAREN,
                    STATEMENT,
                ],
                e.P2_IterationStatement_FOR_VAR_VariableDeclarationList_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (2, VAR), (4, SEMICOLON), (6, SEMICOLON), (8, RPAREN)),
                (
                    ("VariableDeclarationList", "VariableDeclarationList"),
                    ("ExpressionTest", "Expression"),
                    ("ExpressionInc", "Expression"),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            (
                # for ( var VariableDeclarationList ; Expression ; ) Statement
                [FOR, LPAREN, VAR, VARIABLEDECLARATIONLIST, SEMICOLON, EXPRESSION, SEMICOLON, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_VAR_VariableDeclarationList_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (2, VAR), (4, SEMICOLON), (6, SEMICOLON), (8, RPAREN)),
                (
                    ("VariableDeclarationList", "VariableDeclarationList"),
                    ("ExpressionTest", "Expression"),
                    ("ExpressionInc", None),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            (
                # for ( var VariableDeclarationList ; ; Expression ) Statement
                [FOR, LPAREN, VAR, VARIABLEDECLARATIONLIST, SEMICOLON, SEMICOLON, EXPRESSION, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_VAR_VariableDeclarationList_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (2, VAR), (4, SEMICOLON), (6, SEMICOLON), (8, RPAREN)),
                (
                    ("VariableDeclarationList", "VariableDeclarationList"),
                    ("ExpressionTest", None),
                    ("ExpressionInc", "Expression"),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            (
                # for ( var VariableDeclarationList ; ; ) Statement
                [FOR, LPAREN, VAR, VARIABLEDECLARATIONLIST, SEMICOLON, SEMICOLON, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_VAR_VariableDeclarationList_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (2, VAR), (4, SEMICOLON), (6, SEMICOLON), (8, RPAREN)),
                (
                    ("VariableDeclarationList", "VariableDeclarationList"),
                    ("ExpressionTest", None),
                    ("ExpressionInc", None),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            # for ( LexicalDeclaration[~In, ?Yield, ?Await] Expression[+In, ?Yield, ?Await]opt ; Expression[+In, ?Yield, ?Await]opt ) Statement[?Yield, ?Await, ?Return]
            (
                # for ( LexicalDeclaration ; ) Statement
                [FOR, LPAREN, LD_REPLACEMENT, SEMICOLON, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_LexicalDeclaration_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (4, SEMICOLON), (6, RPAREN)),
                (
                    ("LexicalDeclaration", "LexicalDeclaration"),
                    ("ExpressionTest", None),
                    ("ExpressionInc", None),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            (
                # for ( LexicalDeclaration Expression ; ) Statement
                [FOR, LPAREN, LD_REPLACEMENT, EXPRESSION, SEMICOLON, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_LexicalDeclaration_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (4, SEMICOLON), (6, RPAREN)),
                (
                    ("LexicalDeclaration", "LexicalDeclaration"),
                    ("ExpressionTest", "Expression"),
                    ("ExpressionInc", None),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            (
                # for ( LexicalDeclaration ; Expression ) Statement
                [FOR, LPAREN, LD_REPLACEMENT, SEMICOLON, EXPRESSION, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_LexicalDeclaration_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (4, SEMICOLON), (6, RPAREN)),
                (
                    ("LexicalDeclaration", "LexicalDeclaration"),
                    ("ExpressionTest", None),
                    ("ExpressionInc", "Expression"),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            (
                # for ( LexicalDeclaration Expression ; Expression ) Statement
                [FOR, LPAREN, LD_REPLACEMENT, EXPRESSION, SEMICOLON, EXPRESSION, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_LexicalDeclaration_ExpressionTest_ExpressionInc_Statement,
                ((0, FOR), (1, LPAREN), (4, SEMICOLON), (6, RPAREN)),
                (
                    ("LexicalDeclaration", "LexicalDeclaration"),
                    ("ExpressionTest", "Expression"),
                    ("ExpressionInc", "Expression"),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            # for ( [lookahead ∉ { let [ }]LeftHandSideExpression[?Yield, ?Await] in Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
            (
                # for ( LeftHandSideExpression in Expression ) Statement
                [FOR, LPAREN, LHS_REPLACEMENT, IN, EXPRESSION, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_LeftHandSideExpression_IN_Expression_Statement,
                ((0, FOR), (1, LPAREN), (3, IN), (5, RPAREN)),
                (
                    ("LeftHandSideExpression", "LeftHandSideExpression"),
                    ("Expression", "Expression"),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            # for ( var ForBinding[?Yield, ?Await] in Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
            (
                # for ( var ForBinding in Expression ) Statement
                [FOR, LPAREN, VAR, FORBINDING, IN, EXPRESSION, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_VAR_ForBinding_IN_Expression_Statement,
                ((0, FOR), (1, LPAREN), (2, VAR), (4, IN), (6, RPAREN)),
                (("ForBinding", "ForBinding"), ("Expression", "Expression"), ("Statement", "Statement")),
                ALT_EXPRESSION,
            ),
            # for ( ForDeclaration[?Yield, ?Await] in Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
            (
                # for ( ForDeclaration in Expression ) Statement
                [FOR, LPAREN, FORDECLARATION, IN, EXPRESSION, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_ForDeclaration_IN_Expression_Statement,
                ((0, FOR), (1, LPAREN), (3, IN), (5, RPAREN)),
                (("ForDeclaration", "ForDeclaration"), ("Expression", "Expression"), ("Statement", "Statement")),
                ALT_EXPRESSION,
            ),
            # for ( [lookahead ≠ let]LeftHandSideExpression[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
            (
                # for ( LeftHandSideExpression of AssignmentExpression ) Statement
                [FOR, LPAREN, LHS_REPLACEMENT, OF, AE_REPLACEMENT, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_LeftHandSideExpression_OF_AssignmentExpression_Statement,
                ((0, FOR), (1, LPAREN), (3, OF), (5, RPAREN)),
                (
                    ("LeftHandSideExpression", "LeftHandSideExpression"),
                    ("AssignmentExpression", "AssignmentExpression"),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            # for ( var ForBinding[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
            (
                # for ( var ForBinding of AssignmentExpression ) Statement
                [FOR, LPAREN, VAR, FORBINDING, OF, AE_REPLACEMENT, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_VAR_ForBinding_OF_AssignmentExpression_Statement,
                ((0, FOR), (1, LPAREN), (2, VAR), (4, OF), (6, RPAREN)),
                (
                    ("ForBinding", "ForBinding"),
                    ("AssignmentExpression", "AssignmentExpression"),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            # for ( ForDeclaration[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
            (
                # for ( ForDeclaration of AssignmentExpression ) Statement
                [FOR, LPAREN, FORDECLARATION, OF, AE_REPLACEMENT, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_ForDeclaration_OF_AssignmentExpression_Statement,
                ((0, FOR), (1, LPAREN), (3, OF), (5, RPAREN)),
                (
                    ("ForDeclaration", "ForDeclaration"),
                    ("AssignmentExpression", "AssignmentExpression"),
                    ("Statement", "Statement"),
                ),
                ALT_EXPRESSION,
            ),
            # [+Await] for await ( [lookahead ≠ let]LeftHandSideExpression[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
            (
                # for await ( LeftHandSideExpression of AssignmentExpression ) Statement
                [FOR, AWAIT, LPAREN, LHS_REPLACEMENT, OF, AE_REPLACEMENT, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_AWAIT_LeftHandSideExpression_OF_AssignmentExpression_Statement,
                ((0, FOR), (1, AWAIT), (2, LPAREN), (4, OF), (6, RPAREN)),
                (
                    ("LeftHandSideExpression", "LeftHandSideExpression"),
                    ("AssignmentExpression", "AssignmentExpression"),
                    ("Statement", "Statement"),
                ),
                AWAIT_ONLY,
            ),
            # [+Await] for await ( var ForBinding[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
            (
                # for await ( var ForBinding of AssignmentExpression ) Statement
                [FOR, AWAIT, LPAREN, VAR, FORBINDING, OF, AE_REPLACEMENT, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_AWAIT_VAR_ForBinding_OF_AssignmentExpression_Statement,
                ((0, FOR), (1, AWAIT), (2, LPAREN), (3, VAR), (5, OF), (7, RPAREN)),
                (
                    ("ForBinding", "ForBinding"),
                    ("AssignmentExpression", "AssignmentExpression"),
                    ("Statement", "Statement"),
                ),
                AWAIT_ONLY,
            ),
            # [+Await] for await ( ForDeclaration[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
            (
                # for await ( ForDeclaration of AssignmentExpression ) Statement
                [FOR, AWAIT, LPAREN, FORDECLARATION, OF, AE_REPLACEMENT, RPAREN, STATEMENT],
                e.P2_IterationStatement_FOR_AWAIT_ForDeclaration_OF_AssignmentExpression_Statement,
                ((0, FOR), (1, AWAIT), (2, LPAREN), (4, OF), (6, RPAREN)),
                (
                    ("ForDeclaration", "ForDeclaration"),
                    ("AssignmentExpression", "AssignmentExpression"),
                    ("Statement", "Statement"),
                ),
                AWAIT_ONLY,
            ),
        ],
    )
    @pytest.mark.parametrize("Await", [True, False])
    def test_parse_IterationStatement_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks, flags, Await
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)

        def alt_exp_check():
            # mocker check for Expression; first has In=False, the remainder have In=True. Must have at least one call.
            calls = mocks["Expression"].call_args_list
            assert len(calls) > 0
            assert calls[0] == mocker.call(context, lexer, "StrictArg", False, "YieldArg", Await)
            for call in calls[1:]:
                assert call == mocker.call(context, lexer, "StrictArg", True, "YieldArg", Await)

        must_be_called = lambda tok: tok in token_stream and (Await or not (flags & AWAIT_ONLY))
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("Statement", must_be_called(STATEMENT), "StrictArg", "YieldArg", Await, "ReturnArg"),
            mock_check(
                "VariableDeclarationList",
                must_be_called(VARIABLEDECLARATIONLIST),
                "StrictArg",
                False,
                "YieldArg",
                Await,
            ),
            mock_check("LexicalDeclaration", must_be_called(LD_REPLACEMENT), "StrictArg", False, "YieldArg", Await),
            mock_check("LeftHandSideExpression", must_be_called(LHS_REPLACEMENT), "StrictArg", "YieldArg", Await),
            mock_check("ForBinding", must_be_called(FORBINDING), "StrictArg", "YieldArg", Await),
            mock_check("ForDeclaration", must_be_called(FORDECLARATION), "StrictArg", "YieldArg", Await),
            mock_check("AssignmentExpression", must_be_called(AE_REPLACEMENT), "StrictArg", True, "YieldArg", Await),
            alt_exp_check
            if (flags & ALT_EXPRESSION)
            else mock_check("Expression", must_be_called(EXPRESSION), "StrictArg", True, "YieldArg", Await),
        )

        istmt = e.parse_IterationStatement(context, lexer, "StrictArg", "YieldArg", Await, "ReturnArg")
        if Await or not (flags & AWAIT_ONLY):
            assert isinstance(istmt, expected_class)
            for key, expected in production_checks:
                assert getattr(istmt, key) == expected, f"assert istmt.{key} == {expected!r}"
            for idx, expected in token_checks:
                assert istmt.children[idx] == expected, f"assert istmt.children[{idx}] == {expected!r}"
            assert lexer.pos == len(token_stream)
        else:
            assert istmt is None
            assert lexer.pos == 0
        for check in call_checks:
            check()

    @pytest.mark.parametrize(
        "token_stream",
        synerror_streams(
            [
                (DO, STATEMENT, WHILE, LPAREN, EXPRESSION, RPAREN),
                (WHILE, LPAREN, EXPRESSION, RPAREN, STATEMENT),
                (FOR, LPAREN, SEMICOLON, SEMICOLON, RPAREN, STATEMENT),
                (FOR, LPAREN, SEMICOLON, SEMICOLON, EXPRESSION, RPAREN, STATEMENT),
                (FOR, LPAREN, SEMICOLON, EXPRESSION, SEMICOLON, RPAREN, STATEMENT),
                (FOR, LPAREN, SEMICOLON, EXPRESSION, SEMICOLON, EXPRESSION, RPAREN, STATEMENT),
                (FOR, LPAREN, EXPRESSION, SEMICOLON, SEMICOLON, RPAREN, STATEMENT),
                (FOR, LPAREN, EXPRESSION, SEMICOLON, SEMICOLON, EXPRESSION, RPAREN, STATEMENT),
                (FOR, LPAREN, EXPRESSION, SEMICOLON, EXPRESSION, SEMICOLON, RPAREN, STATEMENT),
                (FOR, LPAREN, EXPRESSION, SEMICOLON, EXPRESSION, SEMICOLON, EXPRESSION, RPAREN, STATEMENT),
                (FOR, LPAREN, VAR, VARIABLEDECLARATIONLIST, SEMICOLON, SEMICOLON, RPAREN, STATEMENT),
                (FOR, LPAREN, VAR, VARIABLEDECLARATIONLIST, SEMICOLON, SEMICOLON, EXPRESSION, RPAREN, STATEMENT),
                (FOR, LPAREN, VAR, VARIABLEDECLARATIONLIST, SEMICOLON, EXPRESSION, SEMICOLON, RPAREN, STATEMENT),
                (
                    FOR,
                    LPAREN,
                    VAR,
                    VARIABLEDECLARATIONLIST,
                    SEMICOLON,
                    EXPRESSION,
                    SEMICOLON,
                    EXPRESSION,
                    RPAREN,
                    STATEMENT,
                ),
                (FOR, LPAREN, LD_REPLACEMENT, SEMICOLON, RPAREN, STATEMENT),
                (FOR, LPAREN, LD_REPLACEMENT, EXPRESSION, SEMICOLON, RPAREN, STATEMENT),
                (FOR, LPAREN, LD_REPLACEMENT, SEMICOLON, EXPRESSION, RPAREN, STATEMENT),
                (FOR, LPAREN, LD_REPLACEMENT, EXPRESSION, SEMICOLON, EXPRESSION, RPAREN, STATEMENT),
                (FOR, LPAREN, LHS_REPLACEMENT, IN, EXPRESSION, RPAREN, STATEMENT),
                (FOR, LPAREN, VAR, FORBINDING, IN, EXPRESSION, RPAREN, STATEMENT),
                (FOR, LPAREN, FORDECLARATION, IN, EXPRESSION, RPAREN, STATEMENT),
                (FOR, LPAREN, LHS_REPLACEMENT, OF, AE_REPLACEMENT, RPAREN, STATEMENT),
                (FOR, LPAREN, VAR, FORBINDING, OF, AE_REPLACEMENT, RPAREN, STATEMENT),
                (FOR, LPAREN, FORDECLARATION, OF, AE_REPLACEMENT, RPAREN, STATEMENT),
                (FOR, AWAIT, LPAREN, LHS_REPLACEMENT, OF, AE_REPLACEMENT, RPAREN, STATEMENT),
                (FOR, AWAIT, LPAREN, VAR, FORBINDING, OF, AE_REPLACEMENT, RPAREN, STATEMENT),
                (FOR, AWAIT, LPAREN, FORDECLARATION, OF, AE_REPLACEMENT, RPAREN, STATEMENT),
            ]
        )
        + [
            pytest.param((FOR, LPAREN, LET, IBOB, EQ, THREE, MATCHES_NONE), id="for ( let bob = 3 nope"),
            pytest.param(
                (FOR, AWAIT, LPAREN, LET, IBOB, EQ, THREE, MATCHES_NONE), id="for await ( let bob = 3 nope"
            ),
        ],
    )
    @pytest.mark.parametrize("Await", [True, False])
    def test_parse_IterationStatement_02(self, mocker, context, token_stream, Await):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)

        def alt_exp_check():
            # mocker check for Expression; In can be either true or false
            calls = mocks["Expression"].call_args_list
            for call in calls:
                assert call == mocker.call(
                    context, lexer, "StrictArg", True, "YieldArg", Await
                ) or call == mocker.call(context, lexer, "StrictArg", False, "YieldArg", Await)

        call_checks = (
            mock_check("Statement", False, "StrictArg", "YieldArg", Await, "ReturnArg"),
            mock_check("VariableDeclarationList", False, "StrictArg", False, "YieldArg", Await),
            mock_check("LexicalDeclaration", False, "StrictArg", False, "YieldArg", Await),
            mock_check("LeftHandSideExpression", False, "StrictArg", "YieldArg", Await),
            mock_check("ForBinding", False, "StrictArg", "YieldArg", Await),
            mock_check("ForDeclaration", False, "StrictArg", "YieldArg", Await),
            mock_check("AssignmentExpression", False, "StrictArg", True, "YieldArg", Await),
            alt_exp_check,
        )

        istmt = e.parse_IterationStatement(context, lexer, "StrictArg", "YieldArg", Await, "ReturnArg")
        assert istmt is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        fd = e.P2_ForDeclaration(context, "StrictArg", ["child"])
        assert fd.name == "ForDeclaration"
        assert fd.context == context
        assert fd.children == ["child"]

    def test_P2_ForDeclaration_LetOrConst_ForBinding_init(self, context):
        fd = e.P2_ForDeclaration_LetOrConst_ForBinding(context, "StrictArg", ["loc", "forbinding"])
        assert fd.name == "ForDeclaration"
        assert fd.LetOrConst == "loc"
        assert fd.ForBinding == "forbinding"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "LetOrConst": mocker.patch("ecmascript.ecmascript.parse_LetOrConst", side_effect=letorconst_sideeffect),
            "ForBinding": mocker.patch("ecmascript.ecmascript.parse_ForBinding", side_effect=forbinding_sideeffect),
        }

    def test_parse_ForDeclaration_01(self, mocker, context):
        lexer = Lexer([LETORCONST_REPLACEMENT, FORBINDING])
        mocks = self.setup_mocks(mocker)

        fd = e.parse_ForDeclaration(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(fd, e.P2_ForDeclaration_LetOrConst_ForBinding)
        assert fd.LetOrConst == "LetOrConst"
        assert fd.ForBinding == "ForBinding"
        assert lexer.pos == 2
        mocks["LetOrConst"].assert_called_with(context, lexer, "StrictArg")
        mocks["ForBinding"].assert_called_with(context, lexer, "StrictArg", "YieldArg", "AwaitArg")

    @pytest.mark.parametrize("token_stream", synerror_streams([(LETORCONST_REPLACEMENT, FORBINDING)]))
    def test_parse_ForDeclaration_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("LetOrConst", False, "StrictArg"),
            mock_check("ForBinding", False, "StrictArg", "YieldArg", "AwaitArg"),
        )

        fd = e.parse_ForDeclaration(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert fd is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        fb = e.P2_ForBinding(context, "StrictArg", ["child"])
        assert fb.name == "ForBinding"
        assert fb.context == context
        assert fb.children == ["child"]

    def test_P2_ForBinding_BindingIdentifier_init(self, context):
        fb = e.P2_ForBinding_BindingIdentifier(context, "StrictArg", ["child"])
        assert fb.name == "ForBinding"
        assert fb.BindingIdentifier == "child"

    def test_P2_ForBinding_BindingPattern_init(self, context):
        fb = e.P2_ForBinding_BindingPattern(context, "StrictArg", ["child"])
        assert fb.name == "ForBinding"
        assert fb.BindingPattern == "child"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "BindingIdentifier": mocker.patch(
                "ecmascript.ecmascript.parse_BindingIdentifier", side_effect=bindingidentifier_sideeffect
            ),
            "BindingPattern": mocker.patch(
                "ecmascript.ecmascript.parse_BindingPattern", side_effect=bindingpattern_sideeffect
            ),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, production_check",
        [
            ([BINDINGIDENTIFIER], e.P2_ForBinding_BindingIdentifier, "BindingIdentifier"),
            ([BINDINGPATTERN], e.P2_ForBinding_BindingPattern, "BindingPattern"),
        ],
    )
    def test_parse_ForBinding_01(self, mocker, context, token_stream, expected_class, production_check):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)

        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("BindingIdentifier", BINDINGIDENTIFIER in token_stream, "StrictArg", "YieldArg", "AwaitArg"),
            mock_check("BindingPattern", BINDINGPATTERN in token_stream, "StrictArg", "YieldArg", "AwaitArg"),
        )

        fb = e.parse_ForBinding(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(fb, expected_class)
        assert getattr(fb, production_check) == production_check
        assert lexer.pos == 1
        for check in call_checks:
            check()

    @pytest.mark.parametrize("token_stream", synerror_streams([(BINDINGIDENTIFIER,), (BINDINGPATTERN,)]))
    def test_parse_ForBinding_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)

        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("BindingIdentifier", False, "StrictArg", "YieldArg", "AwaitArg"),
            mock_check("BindingPattern", False, "StrictArg", "YieldArg", "AwaitArg"),
        )

        fb = e.parse_ForBinding(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert fb is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        cs = e.P2_ContinueStatement(context, "StrictArg", ["child"])
        assert cs.name == "ContinueStatement"
        assert cs.context == context
        assert cs.children == ["child"]

    def test_P2_ContinueStatement_CONTINUE_init(self, context):
        cs = e.P2_ContinueStatement_CONTINUE(context, "StrictArg", ["continue", ";"])
        assert cs.name == "ContinueStatement"

    def test_P2_ContinueStatement_CONTINUE_LabelIdentifier_init(self, context):
        cs = e.P2_ContinueStatement_CONTINUE_LabelIdentifier(context, "StrictArg", ["continue", "superduper", ";"])
        assert cs.name == "ContinueStatement"
        assert cs.LabelIdentifier == "superduper"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "LabelIdentifier": mocker.patch(
                "ecmascript.ecmascript.parse_LabelIdentifier", side_effect=labelidentifier_sideeffect
            )
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [CONTINUE, SEMICOLON],
                e.P2_ContinueStatement_CONTINUE,
                ((0, CONTINUE), (1, SEMICOLON)),
                (),
                id="continue ;",
            ),
            pytest.param(
                [CONTINUE, LABELIDENTIFIER_NONL, SEMICOLON],
                e.P2_ContinueStatement_CONTINUE_LabelIdentifier,
                ((0, CONTINUE), (2, SEMICOLON)),
                ("LabelIdentifier",),
                id="continue LabelIdentifier ;",
            ),
        ],
    )
    def test_parse_ContinueStatement_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        must_be_called = lambda tok: tok in token_stream
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("LabelIdentifier", must_be_called(LABELIDENTIFIER_NONL), "StrictArg", "YieldArg", "AwaitArg"),
        )

        cs = e.parse_ContinueStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(cs, expected_class)
        for key in production_checks:
            assert getattr(cs, key) == key, f"assert cs.{key} == {key!r}"
        for idx, expected in token_checks:
            assert cs.children[idx] == expected, f"assert cs.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize(
        "token_stream",
        synerror_streams([(CONTINUE, SEMICOLON), (CONTINUE, LABELIDENTIFIER_NONL, SEMICOLON)])
        + [pytest.param((CONTINUE, FUNCTION_NONL), id="continue function")],
    )
    def test_parse_ContinueStatement_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (mock_check("LabelIdentifier", False, "StrictArg", "YieldArg", "AwaitArg"),)
        cs = e.parse_ContinueStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert cs is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        bs = e.P2_BreakStatement(context, "StrictArg", ["child"])
        assert bs.name == "BreakStatement"
        assert bs.context == context
        assert bs.children == ["child"]

    def test_P2_BreakStatement_BREAK_LabelIdentifier_init(self, context):
        bs = e.P2_BreakStatement_BREAK_LabelIdentifier(context, "StrictArg", ["continue", "ident", ";"])
        assert bs.name == "BreakStatement"
        assert bs.LabelIdentifier == "ident"

    def test_P2_BreakStatement_BREAK_init(self, context):
        bs = e.P2_BreakStatement_BREAK(context, "StrictArg", ["break", ";"])
        assert bs.name == "BreakStatement"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "LabelIdentifier": mocker.patch(
                "ecmascript.ecmascript.parse_LabelIdentifier", side_effect=labelidentifier_sideeffect
            )
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [BREAK, SEMICOLON], e.P2_BreakStatement_BREAK, ((0, BREAK), (1, SEMICOLON)), (), id="break ;"
            ),
            pytest.param(
                [BREAK, LABELIDENTIFIER_NONL, SEMICOLON],
                e.P2_BreakStatement_BREAK_LabelIdentifier,
                ((0, BREAK), (2, SEMICOLON)),
                ("LabelIdentifier",),
                id="break LabelIdentifier ;",
            ),
        ],
    )
    def test_parse_BreakStatement_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        must_be_called = lambda tok: tok in token_stream
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("LabelIdentifier", must_be_called(LABELIDENTIFIER_NONL), "StrictArg", "YieldArg", "AwaitArg"),
        )

        cs = e.parse_BreakStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(cs, expected_class)
        for key in production_checks:
            assert getattr(cs, key) == key, f"assert cs.{key} == {key!r}"
        for idx, expected in token_checks:
            assert cs.children[idx] == expected, f"assert cs.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize(
        "token_stream",
        synerror_streams([(BREAK, SEMICOLON), (BREAK, LABELIDENTIFIER_NONL, SEMICOLON)])
        + [pytest.param((BREAK, FUNCTION_NONL), id="break function")],
    )
    def test_parse_BreakStatement_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (mock_check("LabelIdentifier", False, "StrictArg", "YieldArg", "AwaitArg"),)
        cs = e.parse_BreakStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert cs is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        rs = e.P2_ReturnStatement(context, "StrictArg", ["child"])
        assert rs.name == "ReturnStatement"
        assert rs.context == context
        assert rs.children == ["child"]

    def test_P2_ReturnStatement_RETURN_init(self, context):
        rs = e.P2_ReturnStatement_RETURN(context, "StrictArg", ["return", ";"])
        assert rs.name == "ReturnStatement"

    def test_P2_ReturnStatement_RETURN_Expression_init(self, context):
        rs = e.P2_ReturnStatement_RETURN_Expression(context, "StrictArg", ["return", "Expression", ";"])
        assert rs.name == "ReturnStatement"
        assert rs.Expression == "Expression"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "Expression": mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect)
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [RETURN, SEMICOLON], e.P2_ReturnStatement_RETURN, ((0, RETURN), (1, SEMICOLON)), (), id="return ;"
            ),
            pytest.param(
                [RETURN, EXPRESSION_NONL, SEMICOLON],
                e.P2_ReturnStatement_RETURN_Expression,
                ((0, RETURN), (2, SEMICOLON)),
                ("Expression",),
                id="return Expression ;",
            ),
        ],
    )
    def test_parse_ReturnStatement_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        must_be_called = lambda tok: tok in token_stream
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("Expression", must_be_called(EXPRESSION_NONL), "StrictArg", True, "YieldArg", "AwaitArg"),
        )

        rs = e.parse_ReturnStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(rs, expected_class)
        for key in production_checks:
            assert getattr(rs, key) == key, f"assert rs.{key} == {key!r}"
        for idx, expected in token_checks:
            assert rs.children[idx] == expected, f"assert rs.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize(
        "token_stream",
        synerror_streams([(RETURN, SEMICOLON), (RETURN, EXPRESSION_NONL, SEMICOLON)])
        + [pytest.param((RETURN, PLUSPLUS_NONL, SEMICOLON), id="return ++ ;")],
    )
    def test_parse_ReturnStatement_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (mock_check("Expression", False, "StrictArg", True, "YieldArg", "AwaitArg"),)
        rs = e.parse_ReturnStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert rs is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        ws = e.P2_WithStatement(context, "StrictArg", ["child"])
        assert ws.name == "WithStatement"
        assert ws.context == context
        assert ws.children == ["child"]

    def test_P2_WithStatement_WITH_Expression_Statement_init(self, context):
        ws = e.P2_WithStatement_WITH_Expression_Statement(
            context, "StrictArg", ["with", "(", "Expression", ")", "Statement"]
        )
        assert ws.name == "WithStatement"
        assert ws.Expression == "Expression"
        assert ws.Statement == "Statement"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "Expression": mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect),
            "Statement": mocker.patch("ecmascript.ecmascript.parse_Statement", side_effect=statement_sideeffect),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [WITH, LPAREN, EXPRESSION, RPAREN, STATEMENT],
                e.P2_WithStatement_WITH_Expression_Statement,
                ((0, WITH), (1, LPAREN), (3, RPAREN)),
                ("Expression", "Statement"),
                id="with ( Expression ) Statement",
            )
        ],
    )
    def test_parse_WithStatement_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("Expression", True, "StrictArg", True, "YieldArg", "AwaitArg"),
            mock_check("Statement", True, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
        )

        ws = e.parse_WithStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert isinstance(ws, expected_class)
        for key in production_checks:
            assert getattr(ws, key) == key, f"assert ws.{key} == {key!r}"
        for idx, expected in token_checks:
            assert ws.children[idx] == expected, f"assert ws.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize("token_stream", synerror_streams([(WITH, LPAREN, EXPRESSION, RPAREN, STATEMENT)]))
    def test_parse_WithStatement_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("Expression", False, "StrictArg", True, "YieldArg", "AwaitArg"),
            mock_check("Statement", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
        )
        ws = e.parse_WithStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert ws is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        ss = e.P2_SwitchStatement(context, "StrictArg", ["child"])
        assert ss.name == "SwitchStatement"
        assert ss.context == context
        assert ss.children == ["child"]

    def test_P2_SwitchStatement_SWITCH_Expression_CaseBlock_init(self, context):
        ss = e.P2_SwitchStatement_SWITCH_Expression_CaseBlock(
            context, "StrictArg", ["switch", "(", "Expression", ")", "CaseBlock"]
        )
        assert ss.name == "SwitchStatement"
        assert ss.Expression == "Expression"
        assert ss.CaseBlock == "CaseBlock"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "Expression": mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect),
            "CaseBlock": mocker.patch("ecmascript.ecmascript.parse_CaseBlock", side_effect=caseblock_sideeffect),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [SWITCH, LPAREN, EXPRESSION, RPAREN, CASEBLOCK],
                e.P2_SwitchStatement_SWITCH_Expression_CaseBlock,
                ((0, SWITCH), (1, LPAREN), (3, RPAREN)),
                ("Expression", "CaseBlock"),
                id="switch ( Expression ) CaseBlock",
            )
        ],
    )
    def test_parse_SwitchStatement_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("Expression", True, "StrictArg", True, "YieldArg", "AwaitArg"),
            mock_check("CaseBlock", True, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
        )

        ss = e.parse_SwitchStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert isinstance(ss, expected_class)
        for key in production_checks:
            assert getattr(ss, key) == key, f"assert ss.{key} == {key!r}"
        for idx, expected in token_checks:
            assert ss.children[idx] == expected, f"assert ss.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize("token_stream", synerror_streams([(SWITCH, LPAREN, EXPRESSION, RPAREN, CASEBLOCK)]))
    def test_parse_SwitchStatement_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("Expression", False, "StrictArg", True, "YieldArg", "AwaitArg"),
            mock_check("CaseBlock", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
        )
        ss = e.parse_SwitchStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert ss is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        cb = e.P2_CaseBlock(context, "StrictArg", ["child"])
        assert cb.name == "CaseBlock"
        assert cb.context == context
        assert cb.children == ["child"]

    def test_P2_CaseBlock_EMPTY_init(self, context):
        cb = e.P2_CaseBlock_EMPTY(context, "StrictArg", ["{", "}"])
        assert cb.name == "CaseBlock"
        assert cb.DefaultClause is None
        assert cb.CaseClausesBefore is None
        assert cb.CaseClausesAfter is None

    def test_P2_CaseBlock_DefaultClause_init(self, context):
        cb = e.P2_CaseBlock_DefaultClause(context, "StrictArg", ["{", "default", "}"])
        assert cb.name == "CaseBlock"
        assert cb.DefaultClause == "default"
        assert cb.CaseClausesAfter is None
        assert cb.CaseClausesBefore is None

    def test_P2_CaseBlock_CaseClauses_init(self, context):
        cb = e.P2_CaseBlock_CaseClauses(context, "StrictArg", ["{", "caseclauses", "}"])
        assert cb.name == "CaseBlock"
        assert cb.DefaultClause is None
        assert cb.CaseClausesBefore == "caseclauses"
        assert cb.CaseClausesAfter is None

    def test_P2_CaseBlock_CaseClauses_DefaultClause_init(self, context):
        cb = e.P2_CaseBlock_CaseClauses_DefaultClause(context, "StrictArg", ["{", "caseclauses", "default", "}"])
        assert cb.name == "CaseBlock"
        assert cb.CaseClausesBefore == "caseclauses"
        assert cb.DefaultClause == "default"
        assert cb.CaseClausesAfter is None

    def test_P2_CaseBlock_DefaultClause_CaseClauses_init(self, context):
        cb = e.P2_CaseBlock_DefaultClause_CaseClauses(context, "StrictArg", ["{", "default", "caseclauses", "}"])
        assert cb.name == "CaseBlock"
        assert cb.CaseClausesBefore is None
        assert cb.DefaultClause == "default"
        assert cb.CaseClausesAfter == "caseclauses"

    def test_P2_CaseBlock_CaseClauses_DefaultClause_CaseClauses_init(self, context):
        cb = e.P2_CaseBlock_CaseClauses_DefaultClause_CaseClauses(
            context, "StrictArg", ["{", "before", "default", "after", "}"]
        )
        assert cb.name == "CaseBlock"
        assert cb.CaseClausesBefore == "before"
        assert cb.DefaultClause == "default"
        assert cb.CaseClausesAfter == "after"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "CaseClauses": mocker.patch(
                "ecmascript.ecmascript.parse_CaseClauses", side_effect=caseclauses_sideeffect
            ),
            "DefaultClause": mocker.patch(
                "ecmascript.ecmascript.parse_DefaultClause", side_effect=defaultclause_sideeffect
            ),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [LCURLY, RCURLY],
                e.P2_CaseBlock_EMPTY,
                ((0, LCURLY), (1, RCURLY)),
                {"CaseClausesBefore": None, "DefaultClause": None, "CaseClausesAfter": None},
                id="{ }",
            ),
            pytest.param(
                [LCURLY, DEFAULTCLAUSE, RCURLY],
                e.P2_CaseBlock_DefaultClause,
                ((0, LCURLY), (2, RCURLY)),
                {"CaseClausesBefore": None, "DefaultClause": "DefaultClause", "CaseClausesAfter": None},
                id="{ DefaultClause }",
            ),
            pytest.param(
                [LCURLY, CASECLAUSES, RCURLY],
                e.P2_CaseBlock_CaseClauses,
                ((0, LCURLY), (2, RCURLY)),
                {"CaseClausesBefore": "CaseClauses", "DefaultClause": None, "CaseClausesAfter": None},
                id="{ CaseClauses }",
            ),
            pytest.param(
                [LCURLY, DEFAULTCLAUSE, CASECLAUSES, RCURLY],
                e.P2_CaseBlock_DefaultClause_CaseClauses,
                ((0, LCURLY), (3, RCURLY)),
                {"CaseClausesBefore": None, "DefaultClause": "DefaultClause", "CaseClausesAfter": "CaseClauses"},
                id="{ DefaultClause CaseClauses }",
            ),
            pytest.param(
                [LCURLY, CASECLAUSES, DEFAULTCLAUSE, RCURLY],
                e.P2_CaseBlock_CaseClauses_DefaultClause,
                ((0, LCURLY), (3, RCURLY)),
                {"CaseClausesBefore": "CaseClauses", "DefaultClause": "DefaultClause", "CaseClausesAfter": None},
                id="{ CaseClauses DefaultClause }",
            ),
            pytest.param(
                [LCURLY, CASECLAUSES, DEFAULTCLAUSE, CASECLAUSES, RCURLY],
                e.P2_CaseBlock_CaseClauses_DefaultClause_CaseClauses,
                ((0, LCURLY), (4, RCURLY)),
                {
                    "CaseClausesBefore": "CaseClauses",
                    "DefaultClause": "DefaultClause",
                    "CaseClausesAfter": "CaseClauses",
                },
                id="{ CaseClauses DefaultClause CaseClauses }",
            ),
        ],
    )
    def test_parse_CaseBlock_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("CaseClauses", is_present(CASECLAUSES), "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
            mock_check("DefaultClause", is_present(DEFAULTCLAUSE), "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
        )

        cb = e.parse_CaseBlock(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert isinstance(cb, expected_class)
        for key, value in production_checks.items():
            assert getattr(cb, key) == value, f"assert cb.{key} == {value!r}"
        for idx, expected in token_checks:
            assert cb.children[idx] == expected, f"assert cb.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize(
        "token_stream",
        synerror_streams(
            [
                (LCURLY, RCURLY),
                (LCURLY, CASECLAUSES, RCURLY),
                (LCURLY, DEFAULTCLAUSE, RCURLY),
                (LCURLY, CASECLAUSES, DEFAULTCLAUSE, RCURLY),
                (LCURLY, CASECLAUSES, DEFAULTCLAUSE, CASECLAUSES, RCURLY),
                (LCURLY, DEFAULTCLAUSE, CASECLAUSES),
            ]
        ),
    )
    def test_parse_CaseBlock_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("CaseClauses", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
            mock_check("DefaultClause", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
        )

        cb = e.parse_CaseBlock(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert cb is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        cc = e.P2_CaseClauses(context, "StrictArg", ["child"])
        assert cc.name == "CaseClauses"
        assert cc.context == context
        assert cc.children == ["child"]

    def test_P2_CaseClauses_CaseClause_init(self, context):
        cc = e.P2_CaseClauses_CaseClause(context, "StrictArg", ["caseclause"])
        assert cc.name == "CaseClauses"
        assert cc.CaseClause == "caseclause"

    def test_P2_CaseClauses_CaseClauses_CaseClause_init(self, context):
        cc = e.P2_CaseClauses_CaseClauses_CaseClause(context, "StrictArg", ["clauses", "clause"])
        assert cc.name == "CaseClauses"
        assert cc.CaseClauses == "clauses"
        assert cc.CaseClause == "clause"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "CaseClause": mocker.patch("ecmascript.ecmascript.parse_CaseClause", side_effect=caseclause_sideeffect)
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [CASECLAUSE], e.P2_CaseClauses_CaseClause, (), (("CaseClause", "CaseClause"),), id="CaseClause"
            ),
            pytest.param(
                [CASECLAUSE, CASECLAUSE],
                e.P2_CaseClauses_CaseClauses_CaseClause,
                (),
                (("CaseClause", "CaseClause"), ("CaseClauses", e.P2_CaseClauses_CaseClause)),
                id="CaseClauses CaseClause",
            ),
        ],
    )
    def test_parse_CaseClauses_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("CaseClause", is_present(CASECLAUSE), "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
        )

        cc = e.parse_CaseClauses(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert isinstance(cc, expected_class)
        for key, value in production_checks:
            if type(value) == type:
                assert isinstance(getattr(cc, key), value), f"assert isinstance(cc.{key}, {value!r})"
            else:
                assert getattr(cc, key) == value, f"assert cc.{key} == {value!r}"
        for idx, expected in token_checks:
            assert cc.children[idx] == expected, f"assert cc.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize("token_stream", synerror_streams([(CASECLAUSE,)]))
    def test_parse_CaseClauses_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (mock_check("CaseClause", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),)

        cc = e.parse_CaseClauses(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert cc is None
        assert lexer.pos == 0
        for check in call_checks:
            check()

    def test_parse_CaseClauses_03(self, mocker, context):
        lexer = Lexer([CASECLAUSE, MATCHES_NONE])
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (mock_check("CaseClause", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),)

        cc = e.parse_CaseClauses(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert isinstance(cc, e.P2_CaseClauses_CaseClause)
        assert lexer.pos == 1
        for check in call_checks:
            check()


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
        cc = e.P2_CaseClause(context, "StrictArg", ["child"])
        assert cc.name == "CaseClause"
        assert cc.context == context
        assert cc.children == ["child"]

    def test_P2_CaseClause_CASE_Expression_StatementList_init(self, context):
        cc = e.P2_CaseClause_CASE_Expression_StatementList(context, "StrictArg", ["case", "Exp", ":", "Stmts"])
        assert cc.name == "CaseClause"
        assert cc.Expression == "Exp"
        assert cc.StatementList == "Stmts"

    def test_P2_CaseClause_CASE_Expression_init(self, context):
        cc = e.P2_CaseClause_CASE_Expression(context, "StrictArg", ["case", "Exp", ":"])
        assert cc.name == "CaseClause"
        assert cc.Expression == "Exp"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "Expression": mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect),
            "StatementList": mocker.patch(
                "ecmascript.ecmascript.parse_StatementList", side_effect=statementlist_sideeffect
            ),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [CASE, EXPRESSION, COLON, STATEMENTLIST],
                e.P2_CaseClause_CASE_Expression_StatementList,
                ((0, CASE), (2, COLON)),
                ("Expression", "StatementList"),
                id="case Expression : StatementList",
            ),
            pytest.param(
                [CASE, EXPRESSION, COLON],
                e.P2_CaseClause_CASE_Expression,
                ((0, CASE), (2, COLON)),
                ("Expression",),
                id="case Expression :",
            ),
        ],
    )
    def test_parse_CaseClause_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("Expression", is_present(EXPRESSION), "StrictArg", True, "YieldArg", "AwaitArg"),
            mock_check(
                "StatementList", is_present(STATEMENTLIST), "StrictArg", False, "YieldArg", "AwaitArg", "ReturnArg"
            ),
        )

        cc = e.parse_CaseClause(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert isinstance(cc, expected_class)
        for key in production_checks:
            assert getattr(cc, key) == key, f"assert cc.{key} == {key!r}"
        for idx, expected in token_checks:
            assert cc.children[idx] == expected, f"assert cc.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize("token_stream", synerror_streams([(CASE, EXPRESSION, COLON)]))
    def test_parse_CaseClause_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("Expression", False, "StrictArg", True, "YieldArg", "AwaitArg"),
            mock_check("StatementList", False, "StrictArg", False, "YieldArg", "AwaitArg", "ReturnArg"),
        )

        cc = e.parse_CaseClause(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert cc is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        dc = e.P2_DefaultClause(context, "StrictArg", ["child"])
        assert dc.name == "DefaultClause"
        assert dc.context == context
        assert dc.children == ["child"]

    def test_P2_DefaultClause_DEFAULT_init(self, context):
        dc = e.P2_DefaultClause_DEFAULT(context, "StrictArg", ["default", ":"])
        assert dc.name == "DefaultClause"
        assert dc.StatementList is None

    def test_P2_DefaultClause_DEFAULT_StatementList_init(self, context):
        dc = e.P2_DefaultClause_DEFAULT_StatementList(context, "StrictArg", ["default", ":", "Stmts"])
        assert dc.name == "DefaultClause"
        assert dc.StatementList == "Stmts"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "StatementList": mocker.patch(
                "ecmascript.ecmascript.parse_StatementList", side_effect=statementlist_sideeffect
            )
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [DEFAULT, COLON, STATEMENTLIST],
                e.P2_DefaultClause_DEFAULT_StatementList,
                ((0, DEFAULT), (1, COLON)),
                ("StatementList",),
                id="default : StatementList",
            ),
            pytest.param(
                [DEFAULT, COLON], e.P2_DefaultClause_DEFAULT, ((0, DEFAULT), (1, COLON)), (), id="default :"
            ),
        ],
    )
    def test_parse_DefaultClause_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check(
                "StatementList", is_present(STATEMENTLIST), "StrictArg", False, "YieldArg", "AwaitArg", "ReturnArg"
            ),
        )

        dc = e.parse_DefaultClause(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert isinstance(dc, expected_class)
        for key in production_checks:
            assert getattr(dc, key) == key, f"assert dc.{key} == {key!r}"
        for idx, expected in token_checks:
            assert dc.children[idx] == expected, f"assert dc.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize("token_stream", synerror_streams([(DEFAULT, COLON)]))
    def test_parse_DefaultClause_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (mock_check("StatementList", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),)

        dc = e.parse_DefaultClause(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert dc is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        ls = e.P2_LabelledStatement(context, "StrictArg", ["child"])
        assert ls.name == "LabelledStatement"
        assert ls.context == context
        assert ls.children == ["child"]

    def test_P2_LabelledStatement_LabelIdentifier_LabelledItem_init(self, context):
        ls = e.P2_LabelledStatement_LabelIdentifier_LabelledItem(context, "StrictArg", ["id", ":", "item"])
        assert ls.name == "LabelledStatement"
        assert ls.LabelIdentifier == "id"
        assert ls.LabelledItem == "item"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "LabelIdentifier": mocker.patch(
                "ecmascript.ecmascript.parse_LabelIdentifier", side_effect=labelidentifier_sideeffect
            ),
            "LabelledItem": mocker.patch(
                "ecmascript.ecmascript.parse_LabelledItem", side_effect=labelleditem_sideeffect
            ),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [LABELIDENTIFIER, COLON, LABELLEDITEM],
                e.P2_LabelledStatement_LabelIdentifier_LabelledItem,
                ((1, COLON),),
                ("LabelIdentifier", "LabelledItem"),
                id="LabelIdentifier : LabelledItem",
            )
        ],
    )
    def test_parse_LabelledStatement_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("LabelIdentifier", is_present(LABELIDENTIFIER), "StrictArg", "YieldArg", "AwaitArg"),
            mock_check("LabelledItem", is_present(LABELLEDITEM), "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
        )

        ls = e.parse_LabelledStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert isinstance(ls, expected_class)
        for key in production_checks:
            assert getattr(ls, key) == key, f"assert ls.{key} == {key!r}"
        for idx, expected in token_checks:
            assert ls.children[idx] == expected, f"assert ls.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize("token_stream", synerror_streams([(LABELIDENTIFIER, COLON, LABELLEDITEM)]))
    def test_parse_LabelledStatement_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("LabelIdentifier", False, "StrictArg", "YieldArg", "AwaitArg"),
            mock_check("LabelledItem", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
        )

        ls = e.parse_LabelledStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert ls is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        li = e.P2_LabelledItem(context, "StrictArg", ["child"])
        assert li.name == "LabelledItem"
        assert li.context == context
        assert li.children == ["child"]

    def test_P2_LabelledItem_Statement_init(self, context):
        li = e.P2_LabelledItem_Statement(context, "StrictArg", ["child"])
        assert li.name == "LabelledItem"
        assert li.Statement == "child"

    def test_P2_LabelledItem_FunctionDeclaration_init(self, context):
        li = e.P2_LabelledItem_FunctionDeclaration(context, "StrictArg", ["child"])
        assert li.name == "LabelledItem"
        assert li.FunctionDeclaration == "child"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "Statement": mocker.patch("ecmascript.ecmascript.parse_Statement", side_effect=statement_sideeffect),
            "FunctionDeclaration": mocker.patch(
                "ecmascript.ecmascript.parse_FunctionDeclaration", side_effect=functiondeclaration_sideeffect
            ),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param([STATEMENT], e.P2_LabelledItem_Statement, (), ("Statement",), id="Statement"),
            pytest.param(
                [FUNCTIONDECLARATION],
                e.P2_LabelledItem_FunctionDeclaration,
                (),
                ("FunctionDeclaration",),
                id="FunctionDeclaration",
            ),
        ],
    )
    def test_parse_LabelledItem_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("Statement", is_present(STATEMENT), "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
            mock_check(
                "FunctionDeclaration", is_present(FUNCTIONDECLARATION), "StrictArg", "YieldArg", "AwaitArg", False
            ),
        )

        li = e.parse_LabelledItem(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert isinstance(li, expected_class)
        for key in production_checks:
            assert getattr(li, key) == key, f"assert li.{key} == {key!r}"
        for idx, expected in token_checks:
            assert li.children[idx] == expected, f"assert li.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize("token_stream", synerror_streams([(STATEMENT,), (FUNCTIONDECLARATION,)]))
    def test_parse_LabelledItem_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("Statement", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
            mock_check("FunctionDeclaration", False, "StrictArg", "YieldArg", "AwaitArg", False),
        )

        li = e.parse_LabelledItem(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert li is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        ts = e.P2_ThrowStatement(context, "StrictArg", ["child"])
        assert ts.name == "ThrowStatement"
        assert ts.context == context
        assert ts.children == ["child"]

    def P2_ThrowStatement_THROW_Expression_init(self, context):
        ts = e.P2_ThrowStatement_THROW_Expression(context, "StrictArg", ["throw", "exp", ";"])
        assert ts.name == "ThrowStatement"
        assert ts.Expression == "exp"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "Expression": mocker.patch("ecmascript.ecmascript.parse_Expression", side_effect=expression_sideeffect)
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [THROW, EXPRESSION_NONL, SEMICOLON],
                e.P2_ThrowStatement_THROW_Expression,
                ((0, THROW), (2, SEMICOLON)),
                ("Expression",),
                id="throw Expression ;",
            )
        ],
    )
    def test_parse_ThrowStatement_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("Expression", is_present(EXPRESSION_NONL), "StrictArg", True, "YieldArg", "AwaitArg"),
        )

        ts = e.parse_ThrowStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(ts, expected_class)
        for key in production_checks:
            assert getattr(ts, key) == key, f"assert ts.{key} == {key!r}"
        for idx, expected in token_checks:
            assert ts.children[idx] == expected, f"assert ts.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize(
        "token_stream",
        synerror_streams([(THROW, EXPRESSION_NONL, SEMICOLON)])
        + [(THROW, EXPRESSION, SEMICOLON), (THROW, PLUSPLUS_NONL, SEMICOLON)],
    )
    def test_parse_ThrowStatement_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (mock_check("Expression", False, "StrictArg", True, "YieldArg", "AwaitArg"),)

        ts = e.parse_ThrowStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert ts is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        ts = e.P2_TryStatement(context, "StrictArg", ["child"])
        assert ts.name == "TryStatement"
        assert ts.context == context
        assert ts.children == ["child"]

    def test_P2_TryStatement_TRY_Block_Catch_init(self, context):
        ts = e.P2_TryStatement_TRY_Block_Catch(context, "StrictArg", ["try", "block", "catch"])
        assert ts.name == "TryStatement"
        assert ts.Block == "block"
        assert ts.Catch == "catch"
        assert ts.Finally is None

    def test_P2_TryStatement_TRY_Block_Finally_init(self, context):
        ts = e.P2_TryStatement_TRY_Block_Finally(context, "StrictArg", ["try", "block", "finally"])
        assert ts.name == "TryStatement"
        assert ts.Block == "block"
        assert ts.Catch is None
        assert ts.Finally == "finally"

    def test_P2_TryStatement_TRY_Block_Catch_Finally_init(self, context):
        ts = e.P2_TryStatement_TRY_Block_Catch_Finally(context, "StrictArg", ["try", "block", "catch", "finally"])
        assert ts.name == "TryStatement"
        assert ts.Block == "block"
        assert ts.Catch == "catch"
        assert ts.Finally == "finally"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "Block": mocker.patch("ecmascript.ecmascript.parse_Block", side_effect=block_sideeffect),
            "Catch": mocker.patch("ecmascript.ecmascript.parse_Catch", side_effect=catch_sideeffect),
            "Finally": mocker.patch("ecmascript.ecmascript.parse_Finally", side_effect=finally_sideeffect),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [TRY, BLOCK, CATCH_PRODUCTION],
                e.P2_TryStatement_TRY_Block_Catch,
                ((0, TRY),),
                ("Block", "Catch"),
                id="try Block Catch",
            ),
            pytest.param(
                [TRY, BLOCK, FINALLY_PRODUCTION],
                e.P2_TryStatement_TRY_Block_Finally,
                ((0, TRY),),
                ("Block", "Finally"),
                id="try Block Finally",
            ),
            pytest.param(
                [TRY, BLOCK, CATCH_PRODUCTION, FINALLY_PRODUCTION],
                e.P2_TryStatement_TRY_Block_Catch_Finally,
                ((0, TRY),),
                ("Block", "Catch", "Finally"),
                id="try Block Catch Finally",
            ),
        ],
    )
    def test_parse_TryStatement_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("Block", is_present(BLOCK), "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
            mock_check("Catch", is_present(CATCH_PRODUCTION), "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
            mock_check("Finally", is_present(FINALLY_PRODUCTION), "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
        )

        ts = e.parse_TryStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert isinstance(ts, expected_class)
        for key in production_checks:
            assert getattr(ts, key) == key, f"assert ts.{key} == {key!r}"
        for idx, expected in token_checks:
            assert ts.children[idx] == expected, f"assert ts.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize(
        "token_stream", synerror_streams([(TRY, BLOCK, CATCH_PRODUCTION), (TRY, BLOCK, FINALLY_PRODUCTION)])
    )
    def test_parse_TryStatement_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("Block", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
            mock_check("Catch", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
            mock_check("Finally", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
        )

        ts = e.parse_TryStatement(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert ts is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        cat = e.P2_Catch(context, "StrictArg", ["child"])
        assert cat.name == "Catch"
        assert cat.context == context
        assert cat.children == ["child"]

    def test_P2_Catch_CATCH_CatchParameter_Block_init(self, context):
        cat = e.P2_Catch_CATCH_CatchParameter_Block(context, "StrictArg", ["catch", "(", "param", ")", "block"])
        assert cat.name == "Catch"
        assert cat.CatchParameter == "param"
        assert cat.Block == "block"

    def test_P2_Catch_CATCH_Block_init(self, context):
        cat = e.P2_Catch_CATCH_Block(context, "StrictArg", ["catch", "block"])
        assert cat.name == "Catch"
        assert cat.Block == "block"
        assert cat.CatchParameter is None

    @staticmethod
    def setup_mocks(mocker):
        return {
            "Block": mocker.patch("ecmascript.ecmascript.parse_Block", side_effect=block_sideeffect),
            "CatchParameter": mocker.patch(
                "ecmascript.ecmascript.parse_CatchParameter", side_effect=catchparameter_sideeffect
            ),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [CATCH, LPAREN, CATCHPARAMETER, RPAREN, BLOCK],
                e.P2_Catch_CATCH_CatchParameter_Block,
                ((0, CATCH), (1, LPAREN), (3, RPAREN)),
                ("CatchParameter", "Block"),
                id="catch ( CatchParameter ) Block",
            ),
            pytest.param([CATCH, BLOCK], e.P2_Catch_CATCH_Block, ((0, CATCH),), ("Block",), id="catch Block"),
        ],
    )
    def test_parse_Catch_01(self, mocker, context, token_stream, expected_class, token_checks, production_checks):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("Block", is_present(BLOCK), "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
            mock_check("CatchParameter", is_present(CATCHPARAMETER), "StrictArg", "YieldArg", "AwaitArg"),
        )

        cat = e.parse_Catch(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert isinstance(cat, expected_class)
        for key in production_checks:
            assert getattr(cat, key) == key, f"assert cat.{key} == {key!r}"
        for idx, expected in token_checks:
            assert cat.children[idx] == expected, f"assert cat.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize(
        "token_stream", synerror_streams([(CATCH, LPAREN, CATCHPARAMETER, RPAREN, BLOCK), (CATCH, BLOCK)])
    )
    def test_parse_Catch_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("Block", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),
            mock_check("CatchParameter", False, "StrictArg", "YieldArg", "AwaitArg"),
        )

        cat = e.parse_Catch(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert cat is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        fin = e.P2_Finally(context, "StrictArg", ["child"])
        assert fin.name == "Finally"
        assert fin.context == context
        assert fin.children == ["child"]

    def test_P2_Finally_FINALLY_Block_init(self, context):
        fin = e.P2_Finally_FINALLY_Block(context, "StrictArg", ["finally", "block"])
        assert fin.name == "Finally"
        assert fin.Block == "block"

    @staticmethod
    def setup_mocks(mocker):
        return {"Block": mocker.patch("ecmascript.ecmascript.parse_Block", side_effect=block_sideeffect)}

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [FINALLY, BLOCK], e.P2_Finally_FINALLY_Block, ((0, FINALLY),), ("Block",), id="finally Block"
            )
        ],
    )
    def test_parse_Finally_01(self, mocker, context, token_stream, expected_class, token_checks, production_checks):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (mock_check("Block", is_present(BLOCK), "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),)

        fin = e.parse_Finally(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert isinstance(fin, expected_class)
        for key in production_checks:
            assert getattr(fin, key) == key, f"assert fin.{key} == {key!r}"
        for idx, expected in token_checks:
            assert fin.children[idx] == expected, f"assert fin.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize("token_stream", synerror_streams([(FINALLY, BLOCK)]))
    def test_parse_Finally_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (mock_check("Block", False, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg"),)

        fin = e.parse_Finally(context, lexer, "StrictArg", "YieldArg", "AwaitArg", "ReturnArg")
        assert fin is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        cp = e.P2_CatchParameter(context, "StrictArg", ["child"])
        assert cp.name == "CatchParameter"
        assert cp.context == context
        assert cp.children == ["child"]

    def test_P2_CatchParameter_BindingIdentifier_init(self, context):
        cp = e.P2_CatchParameter_BindingIdentifier(context, "StrictArg", ["child"])
        assert cp.name == "CatchParameter"
        assert cp.BindingIdentifier == "child"

    def test_P2_CatchParameter_BindingPattern_init(self, context):
        cp = e.P2_CatchParameter_BindingPattern(context, "StrictArg", ["child"])
        assert cp.name == "CatchParameter"
        assert cp.BindingPattern == "child"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "BindingIdentifier": mocker.patch(
                "ecmascript.ecmascript.parse_BindingIdentifier", side_effect=bindingidentifier_sideeffect
            ),
            "BindingPattern": mocker.patch(
                "ecmascript.ecmascript.parse_BindingPattern", side_effect=bindingpattern_sideeffect
            ),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [BINDINGIDENTIFIER],
                e.P2_CatchParameter_BindingIdentifier,
                (),
                ("BindingIdentifier",),
                id="BindingIdentifier",
            ),
            pytest.param(
                [BINDINGPATTERN], e.P2_CatchParameter_BindingPattern, (), ("BindingPattern",), id="BindingPattern"
            ),
        ],
    )
    def test_parse_CatchParameter_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("BindingIdentifier", is_present(BINDINGIDENTIFIER), "StrictArg", "YieldArg", "AwaitArg"),
            mock_check("BindingPattern", is_present(BINDINGPATTERN), "StrictArg", "YieldArg", "AwaitArg"),
        )

        cp = e.parse_CatchParameter(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(cp, expected_class)
        for key in production_checks:
            assert getattr(cp, key) == key, f"assert cp.{key} == {key!r}"
        for idx, expected in token_checks:
            assert cp.children[idx] == expected, f"assert cp.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize("token_stream", synerror_streams([(BINDINGIDENTIFIER,), (BINDINGPATTERN,)]))
    def test_parse_CatchParameter_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("BindingIdentifier", False, "StrictArg", "YieldArg", "AwaitArg"),
            mock_check("BindingPattern", False, "StrictArg", "YieldArg", "AwaitArg"),
        )

        cp = e.parse_CatchParameter(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert cp is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        ds = e.P2_DebuggerStatement(context, "StrictArg", ["child"])
        assert ds.name == "DebuggerStatement"
        assert ds.context == context
        assert ds.children == ["child"]

    def test_P2_DebuggerStatement_DEBUGGER_init(self, context):
        ds = e.P2_DebuggerStatement_DEBUGGER(context, "StrictArg", ["child"])
        assert ds.name == "DebuggerStatement"

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [DEBUGGER, SEMICOLON],
                e.P2_DebuggerStatement_DEBUGGER,
                ((0, DEBUGGER), (1, SEMICOLON)),
                (),
                id="debugger ;",
            )
        ],
    )
    def test_parse_DebuggerStatement_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)

        ds = e.parse_DebuggerStatement(context, lexer, "StrictArg")
        assert isinstance(ds, expected_class)
        for key in production_checks:
            assert getattr(ds, key) == key, f"assert ds.{key} == {key!r}"
        for idx, expected in token_checks:
            assert ds.children[idx] == expected, f"assert ds.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)

    @pytest.mark.parametrize("token_stream", synerror_streams([(DEBUGGER, SEMICOLON)]))
    def test_parse_DebuggerStatement_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)

        ds = e.parse_DebuggerStatement(context, lexer, "StrictArg")
        assert ds is None
        assert lexer.pos == 0


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
        fd = e.P2_FunctionDeclaration(context, "StrictArg", ["child"])
        assert fd.name == "FunctionDeclaration"
        assert fd.context == context
        assert fd.children == ["child"]

    def test_P2_FunctionDeclaration_FUNCTION_BindingIdentifier_FormalParameters_FunctionBody_init(self, context):
        fd = e.P2_FunctionDeclaration_FUNCTION_BindingIdentifier_FormalParameters_FunctionBody(
            context,
            "StrictArg",
            ["function", "BindingIdentifier", "(", "FormalParameters", ")", "{", "FunctionBody", "}"],
        )
        assert fd.name == "FunctionDeclaration"
        assert fd.BindingIdentifier == "BindingIdentifier"
        assert fd.FormalParameters == "FormalParameters"
        assert fd.FunctionBody == "FunctionBody"

    def test_P2_FunctionDeclaration_FUNCTION_FormalParameters_FunctionBody_init(self, context):
        fd = e.P2_FunctionDeclaration_FUNCTION_FormalParameters_FunctionBody(
            context, "StrictArg", ["function", "(", "FormalParameters", ")", "{", "FunctionBody", "}"]
        )
        assert fd.name == "FunctionDeclaration"
        assert fd.FormalParameters == "FormalParameters"
        assert fd.FunctionBody == "FunctionBody"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "BindingIdentifier": mocker.patch(
                "ecmascript.ecmascript.parse_BindingIdentifier", side_effect=bindingidentifier_sideeffect
            ),
            "FormalParameters": mocker.patch(
                "ecmascript.ecmascript.parse_FormalParameters", side_effect=formalparameters_sideeffect
            ),
            "FunctionBody": mocker.patch(
                "ecmascript.ecmascript.parse_FunctionBody", side_effect=functionbody_sideeffect
            ),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks, Default",
        [
            pytest.param(
                [FUNCTION, BINDINGIDENTIFIER, LPAREN, FORMALPARAMETERS, RPAREN, LCURLY, FUNCTIONBODY, RCURLY],
                e.P2_FunctionDeclaration_FUNCTION_BindingIdentifier_FormalParameters_FunctionBody,
                ((0, FUNCTION), (2, LPAREN), (4, RPAREN), (5, LCURLY), (7, RCURLY)),
                ("BindingIdentifier", "FormalParameters", "FunctionBody"),
                Default,
                id=f"[{Tag}Default] function BindingIdentifier ( FormalParameters ) {{ FunctionBody }}",
            )
            for Default, Tag in ((False, "~"), (True, "+"))
        ]
        + [
            pytest.param(
                [FUNCTION, LPAREN, FORMALPARAMETERS, RPAREN, LCURLY, FUNCTIONBODY, RCURLY],
                e.P2_FunctionDeclaration_FUNCTION_FormalParameters_FunctionBody,
                ((0, FUNCTION), (1, LPAREN), (3, RPAREN), (4, LCURLY), (6, RCURLY)),
                ("FormalParameters", "FunctionBody"),
                True,
                id="[+Default] function ( FormalParameters ) { FunctionBody }",
            )
        ],
    )
    def test_parse_FunctionDeclaration_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks, Default
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("BindingIdentifier", is_present(BINDINGIDENTIFIER), "StrictArg", "YieldArg", "AwaitArg"),
            mock_check("FormalParameters", is_present(FORMALPARAMETERS), "StrictArg", False, False),
            mock_check("FunctionBody", is_present(FUNCTIONBODY), "StrictArg", False, False),
        )

        fd = e.parse_FunctionDeclaration(context, lexer, "StrictArg", "YieldArg", "AwaitArg", Default)
        assert isinstance(fd, expected_class)
        for key in production_checks:
            assert getattr(fd, key) == key, f"assert fd.{key} == {key!r}"
        for idx, expected in token_checks:
            assert fd.children[idx] == expected, f"assert fd.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize(
        "token_stream",
        synerror_streams(
            [
                (FUNCTION, BINDINGIDENTIFIER, LPAREN, FORMALPARAMETERS, RPAREN, LCURLY, FUNCTIONBODY, RCURLY),
                (FUNCTION, LPAREN, FORMALPARAMETERS, RPAREN, LCURLY, FUNCTIONBODY, RCURLY),
            ]
        ),
    )
    def test_parse_FunctionDeclaration_DefaultTrue(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("BindingIdentifier", False, "StrictArg", "YieldArg", "AwaitArg"),
            mock_check("FormalParameters", False, "StrictArg", False, False),
            mock_check("FunctionBody", False, "StrictArg", False, False),
        )

        fd = e.parse_FunctionDeclaration(context, lexer, "StrictArg", "YieldArg", "AwaitArg", True)
        assert fd is None
        assert lexer.pos == 0
        for check in call_checks:
            check()

    @pytest.mark.parametrize(
        "token_stream",
        synerror_streams(
            [(FUNCTION, BINDINGIDENTIFIER, LPAREN, FORMALPARAMETERS, RPAREN, LCURLY, FUNCTIONBODY, RCURLY)]
        )
        + [
            pytest.param(
                (FUNCTION, LPAREN, FORMALPARAMETERS, RPAREN, LCURLY, FUNCTIONBODY, RCURLY),
                id="function ( FormalParameters ) { FunctionBody }",
            )
        ],
    )
    def test_parse_FunctionDeclaration_DefaultFalse(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("BindingIdentifier", False, "StrictArg", "YieldArg", "AwaitArg"),
            mock_check("FormalParameters", False, "StrictArg", False, False),
            mock_check("FunctionBody", False, "StrictArg", False, False),
        )

        fd = e.parse_FunctionDeclaration(context, lexer, "StrictArg", "YieldArg", "AwaitArg", False)
        assert fd is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        fe = e.P2_FunctionExpression(context, "StrictArg", ["child"])
        assert fe.name == "FunctionExpression"
        assert fe.context == context
        assert fe.children == ["child"]

    def test_P2_FunctionExpression_FUNCTION_BindingIdentifier_FormalParameters_FunctionBody_init(self, context):
        fe = e.P2_FunctionExpression_FUNCTION_BindingIdentifier_FormalParameters_FunctionBody(
            context,
            "StrictArg",
            ["function", "BindingIdentifier", "(", "FormalParameters", ")", "{", "FunctionBody", "}"],
        )
        assert fe.name == "FunctionExpression"
        assert fe.BindingIdentifier == "BindingIdentifier"
        assert fe.FormalParameters == "FormalParameters"
        assert fe.FunctionBody == "FunctionBody"

    def test_P2_FunctionExpression_FUNCTION_FormalParameters_FunctionBody_init(self, context):
        fe = e.P2_FunctionExpression_FUNCTION_FormalParameters_FunctionBody(
            context, "StrictArg", ["function", "(", "FormalParameters", ")", "{", "FunctionBody", "}"]
        )
        assert fe.name == "FunctionExpression"
        assert fe.FormalParameters == "FormalParameters"
        assert fe.FunctionBody == "FunctionBody"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "BindingIdentifier": mocker.patch(
                "ecmascript.ecmascript.parse_BindingIdentifier", side_effect=bindingidentifier_sideeffect
            ),
            "FormalParameters": mocker.patch(
                "ecmascript.ecmascript.parse_FormalParameters", side_effect=formalparameters_sideeffect
            ),
            "FunctionBody": mocker.patch(
                "ecmascript.ecmascript.parse_FunctionBody", side_effect=functionbody_sideeffect
            ),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [FUNCTION, BINDINGIDENTIFIER, LPAREN, FORMALPARAMETERS, RPAREN, LCURLY, FUNCTIONBODY, RCURLY],
                e.P2_FunctionExpression_FUNCTION_BindingIdentifier_FormalParameters_FunctionBody,
                ((0, FUNCTION), (2, LPAREN), (4, RPAREN), (5, LCURLY), (7, RCURLY)),
                ("BindingIdentifier", "FormalParameters", "FunctionBody"),
                id="function BindingIdentifier ( FormalParameters ) { FunctionBody }",
            ),
            pytest.param(
                [FUNCTION, LPAREN, FORMALPARAMETERS, RPAREN, LCURLY, FUNCTIONBODY, RCURLY],
                e.P2_FunctionExpression_FUNCTION_FormalParameters_FunctionBody,
                ((0, FUNCTION), (1, LPAREN), (3, RPAREN), (4, LCURLY), (6, RCURLY)),
                ("FormalParameters", "FunctionBody"),
                id="function ( FormalParameters ) { FunctionBody }",
            ),
        ],
    )
    def test_parse_FunctionExpression_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("BindingIdentifier", is_present(BINDINGIDENTIFIER), "StrictArg", False, False),
            mock_check("FormalParameters", is_present(FORMALPARAMETERS), "StrictArg", False, False),
            mock_check("FunctionBody", is_present(FUNCTIONBODY), "StrictArg", False, False),
        )

        fe = e.parse_FunctionExpression(context, lexer, "StrictArg")
        assert isinstance(fe, expected_class)
        for key in production_checks:
            assert getattr(fe, key) == key, f"assert fe.{key} == {key!r}"
        for idx, expected in token_checks:
            assert fe.children[idx] == expected, f"assert fe.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize(
        "token_stream",
        synerror_streams(
            [
                (FUNCTION, BINDINGIDENTIFIER, LPAREN, FORMALPARAMETERS, RPAREN, LCURLY, FUNCTIONBODY, RCURLY),
                (FUNCTION, LPAREN, FORMALPARAMETERS, RPAREN, LCURLY, FUNCTIONBODY, RCURLY),
            ]
        ),
    )
    def test_parse_FunctionExpression_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("BindingIdentifier", False, "StrictArg", False, False),
            mock_check("FormalParameters", False, "StrictArg", False, False),
            mock_check("FunctionBody", False, "StrictArg", False, False),
        )

        fe = e.parse_FunctionExpression(context, lexer, "StrictArg")
        assert fe is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        ufp = e.P2_UniqueFormalParameters(context, "StrictArg", ["child"])
        assert ufp.name == "UniqueFormalParameters"
        assert ufp.context == context
        assert ufp.children == ["child"]

    def test_P2_UniqueFormalParameters_FormalParameters_init(self, context):
        ufp = e.P2_UniqueFormalParameters_FormalParameters(context, "StrictArg", ["child"])
        assert ufp.name == "UniqueFormalParameters"
        assert ufp.FormalParameters == "child"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "FormalParameters": mocker.patch(
                "ecmascript.ecmascript.parse_FormalParameters", side_effect=formalparameters_sideeffect
            )
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [FORMALPARAMETERS],
                e.P2_UniqueFormalParameters_FormalParameters,
                (),
                ("FormalParameters",),
                id="FormalParameters",
            )
        ],
    )
    def test_parse_UniqueFormalParameters_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("FormalParameters", is_present(FORMALPARAMETERS), "StrictArg", "YieldArg", "AwaitArg"),
        )

        ufp = e.parse_UniqueFormalParameters(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(ufp, expected_class)
        for key in production_checks:
            assert getattr(ufp, key) == key, f"assert ufp.{key} == {key!r}"
        for idx, expected in token_checks:
            assert ufp.children[idx] == expected, f"assert ufp.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize("token_stream", synerror_streams([(FORMALPARAMETERS,)]))
    def test_parse_UniqueFormalParameters_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (mock_check("FormalParameters", False, "StrictArg", "YieldArg", "AwaitArg"),)

        ufp = e.parse_UniqueFormalParameters(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert ufp is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        fp = e.P2_FormalParameters(context, "StrictArg", ["child"])
        assert fp.name == "FormalParameters"
        assert fp.context == context
        assert fp.children == ["child"]

    def test_P2_FormalParameters_EMPTY_init(self, context):
        fp = e.P2_FormalParameters_EMPTY(context, "StrictArg", ["child"])
        assert fp.name == "FormalParameters"
        assert fp.FormalParameterList is None
        assert fp.FunctionRestParameter is None

    def test_P2_FormalParameters_FunctionRestParameter_init(self, context):
        fp = e.P2_FormalParameters_FunctionRestParameter(context, "StrictArg", ["child"])
        assert fp.name == "FormalParameters"
        assert fp.FormalParameterList is None
        assert fp.FunctionRestParameter == "child"

    def test_P2_FormalParameters_FormalParameterList_init(self, context):
        fp = e.P2_FormalParameters_FormalParameterList(context, "StrictArg", ["child"])
        assert fp.name == "FormalParameters"
        assert fp.FormalParameterList == "child"
        assert fp.FunctionRestParameter is None

    def test_P2_FormalParameters_FormalParameterList_FunctionRestParameter_init(self, context):
        fp = e.P2_FormalParameters_FormalParameterList_FunctionRestParameter(
            context, "StrictArg", ["FormalParameterList", ",", "FunctionRestParameter"]
        )
        assert fp.name == "FormalParameters"
        assert fp.FormalParameterList == "FormalParameterList"
        assert fp.FunctionRestParameter == "FunctionRestParameter"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "FormalParameterList": mocker.patch(
                "ecmascript.ecmascript.parse_FormalParameterList", side_effect=formalparameterlist_sideeffect
            ),
            "FunctionRestParameter": mocker.patch(
                "ecmascript.ecmascript.parse_FunctionRestParameter", side_effect=functionrestparameter_sideeffect
            ),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param([], e.P2_FormalParameters_EMPTY, (), (), id="[empty]"),
            pytest.param(
                [FUNCTIONRESTPARAMETER],
                e.P2_FormalParameters_FunctionRestParameter,
                (),
                ("FunctionRestParameter",),
                id="FunctionRestParameter",
            ),
            pytest.param(
                [FORMALPARAMETERLIST],
                e.P2_FormalParameters_FormalParameterList,
                (),
                ("FormalParameterList",),
                id="FormalParameterList",
            ),
            pytest.param(
                [FORMALPARAMETERLIST, COMMA],
                e.P2_FormalParameters_FormalParameterList,
                ((1, COMMA),),
                ("FormalParameterList",),
                id="FormalParameterList ,",
            ),
            pytest.param(
                [FORMALPARAMETERLIST, COMMA, FUNCTIONRESTPARAMETER],
                e.P2_FormalParameters_FormalParameterList_FunctionRestParameter,
                ((1, COMMA),),
                ("FormalParameterList", "FunctionRestParameter"),
                id="FormalParameterList , FunctionRestParameter",
            ),
        ],
    )
    def test_parse_FormalParameters_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("FormalParameterList", is_present(FORMALPARAMETERLIST), "StrictArg", "YieldArg", "AwaitArg"),
            mock_check(
                "FunctionRestParameter", is_present(FUNCTIONRESTPARAMETER), "StrictArg", "YieldArg", "AwaitArg"
            ),
        )

        fpl = e.parse_FormalParameters(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(fpl, expected_class)
        for key in production_checks:
            assert getattr(fpl, key) == key, f"assert fpl.{key} == {key!r}"
        for idx, expected in token_checks:
            assert fpl.children[idx] == expected, f"assert fpl.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()


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
        fpl = e.P2_FormalParameterList(context, "StrictArg", ["child"])
        assert fpl.name == "FormalParameterList"
        assert fpl.context == context
        assert fpl.children == ["child"]

    def test_P2_FormalParameterList_FormalParameter_init(self, context):
        fpl = e.P2_FormalParameterList_FormalParameter(context, "StrictArg", ["child"])
        assert fpl.name == "FormalParameterList"
        assert fpl.FormalParameter == "child"

    def test_P2_FormalParameterList_FormalParameterList_FormalParameter_init(self, context):
        fpl = e.P2_FormalParameterList_FormalParameterList_FormalParameter(
            context, "StrictArg", ["FormalParameterList", ",", "FormalParameter"]
        )
        assert fpl.name == "FormalParameterList"
        assert fpl.FormalParameterList == "FormalParameterList"
        assert fpl.FormalParameter == "FormalParameter"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "FormalParameter": mocker.patch(
                "ecmascript.ecmascript.parse_FormalParameter", side_effect=formalparameter_sideeffect
            )
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [FORMALPARAMETER],
                e.P2_FormalParameterList_FormalParameter,
                (),
                (("FormalParameter", "FormalParameter"),),
                id="FormalParameter",
            ),
            pytest.param(
                [FORMALPARAMETER, COMMA, FORMALPARAMETER],
                e.P2_FormalParameterList_FormalParameterList_FormalParameter,
                ((1, COMMA),),
                (
                    ("FormalParameter", "FormalParameter"),
                    ("FormalParameterList", e.P2_FormalParameterList_FormalParameter),
                ),
                id="FormalParameterList , FormalParameter",
            ),
        ],
    )
    def test_parse_FormalParameterList_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("FormalParameter", is_present(FORMALPARAMETER), "StrictArg", "YieldArg", "AwaitArg"),
        )

        fpl = e.parse_FormalParameterList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(fpl, expected_class)
        for key, value in production_checks:
            if type(value) == type:
                assert isinstance(getattr(fpl, key), value), f"assert isinstance(fpl.{key}, {value})"
            else:
                assert getattr(fpl, key) == value, f"assert fpl.{key} == {value!r}"
        for idx, expected in token_checks:
            assert fpl.children[idx] == expected, f"assert fpl.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize("token_stream", synerror_streams([(FORMALPARAMETER, COMMA, FORMALPARAMETER)]))
    def test_parse_FormalParameterList_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (mock_check("FormalParameter", False, "StrictArg", "YieldArg", "AwaitArg"),)

        fpl = e.parse_FormalParameterList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        if len(token_stream) > 0 and token_stream[0] == FORMALPARAMETER:
            assert isinstance(fpl, e.P2_FormalParameterList_FormalParameter)
            assert lexer.pos == 1
        else:
            assert fpl is None
            assert lexer.pos == 0
        for check in call_checks:
            check()


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
        frp = e.P2_FunctionRestParameter(context, "StrictArg", ["child"])
        assert frp.name == "FunctionRestParameter"
        assert frp.context == context
        assert frp.children == ["child"]

    def test_P2_FunctionRestParameter_BindingRestElement_init(self, context):
        frp = e.P2_FunctionRestParameter_BindingRestElement(context, "StrictArg", ["child"])
        assert frp.name == "FunctionRestParameter"
        assert frp.BindingRestElement == "child"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "BindingRestElement": mocker.patch(
                "ecmascript.ecmascript.parse_BindingRestElement", side_effect=bindingrestelement_sideeffect
            )
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [BINDINGRESTELEMENT],
                e.P2_FunctionRestParameter_BindingRestElement,
                (),
                ("BindingRestElement",),
                id="BindingRestElement",
            )
        ],
    )
    def test_parse_FunctionRestParameter_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("BindingRestElement", is_present(BINDINGRESTELEMENT), "StrictArg", "YieldArg", "AwaitArg"),
        )

        frp = e.parse_FunctionRestParameter(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(frp, expected_class)
        for key in production_checks:
            assert getattr(frp, key) == key, f"assert frp.{key} == {key!r}"
        for idx, expected in token_checks:
            assert frp.children[idx] == expected, f"assert frp.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize("token_stream", synerror_streams([(BINDINGRESTELEMENT,)]))
    def test_parse_FunctionRestParameter_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (mock_check("BindingRestElement", False, "StrictArg", "YieldArg", "AwaitArg"),)

        frp = e.parse_FunctionRestParameter(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert frp is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        fp = e.P2_FormalParameter(context, "StrictArg", ["child"])
        assert fp.name == "FormalParameter"
        assert fp.context == context
        assert fp.children == ["child"]

    def test_P2_FormalParameter_BindingElement_init(self, context):
        fp = e.P2_FormalParameter_BindingElement(context, "StrictArg", ["child"])
        assert fp.name == "FormalParameter"
        assert fp.BindingElement == "child"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "BindingElement": mocker.patch(
                "ecmascript.ecmascript.parse_BindingElement", side_effect=bindingelement_sideeffect
            )
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [BINDINGELEMENT], e.P2_FormalParameter_BindingElement, (), ("BindingElement",), id="BindingElement"
            )
        ],
    )
    def test_parse_FormalParameter_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("BindingElement", is_present(BINDINGELEMENT), "StrictArg", "YieldArg", "AwaitArg"),
        )

        fp = e.parse_FormalParameter(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(fp, expected_class)
        for key in production_checks:
            assert getattr(fp, key) == key, f"assert fp.{key} == {key!r}"
        for idx, expected in token_checks:
            assert fp.children[idx] == expected, f"assert fp.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize("token_stream", synerror_streams([(BINDINGELEMENT,)]))
    def test_parse_FormalParameter_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (mock_check("BindingElement", False, "StrictArg", "YieldArg", "AwaitArg"),)

        fp = e.parse_FormalParameter(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert fp is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        fb = e.P2_FunctionBody(context, "StrictArg", ["child"])
        assert fb.name == "FunctionBody"
        assert fb.context == context
        assert fb.children == ["child"]

    def test_P2_FunctionBody_FunctionStatementList_init(self, context):
        fb = e.P2_FunctionBody_FunctionStatementList(context, "StrictArg", ["child"])
        assert fb.name == "FunctionBody"
        assert fb.FunctionStatementList == "child"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "FunctionStatementList": mocker.patch(
                "ecmascript.ecmascript.parse_FunctionStatementList", side_effect=functionstatementlist_sideeffect
            )
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [FUNCTIONSTATEMENTLIST],
                e.P2_FunctionBody_FunctionStatementList,
                (),
                ("FunctionStatementList",),
                id="FunctionStatementList",
            )
        ],
    )
    def test_parse_FunctionBody_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check(
                "FunctionStatementList", is_present(FUNCTIONSTATEMENTLIST), "StrictArg", "YieldArg", "AwaitArg"
            ),
        )

        fb = e.parse_FunctionBody(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(fb, expected_class)
        for key in production_checks:
            assert getattr(fb, key) == key, f"assert fb.{key} == {key!r}"
        for idx, expected in token_checks:
            assert fb.children[idx] == expected, f"assert fb.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize("token_stream", synerror_streams([(FUNCTIONSTATEMENTLIST,)]))
    def test_parse_FunctionBody_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (mock_check("FunctionStatementList", False, "StrictArg", "YieldArg", "AwaitArg"),)

        fb = e.parse_FunctionBody(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert fb is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        fsl = e.P2_FunctionStatementList(context, "StrictArg", ["child"])
        assert fsl.name == "FunctionStatementList"
        assert fsl.context == context
        assert fsl.children == ["child"]

    def test_P2_FunctionStatementList_StatementList_init(self, context):
        fsl = e.P2_FunctionStatementList_StatementList(context, "StrictArg", ["child"])
        assert fsl.name == "FunctionStatementList"
        assert fsl.StatementList == "child"

    def test_P2_FunctionStatementList_EMPTY_init(self, context):
        fsl = e.P2_FunctionStatementList_EMPTY(context, "StrictArg", ["child"])
        assert fsl.name == "FunctionStatementList"
        assert fsl.StatementList is None

    @staticmethod
    def setup_mocks(mocker):
        return {
            "StatementList": mocker.patch(
                "ecmascript.ecmascript.parse_StatementList", side_effect=statementlist_sideeffect
            )
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [STATEMENTLIST], e.P2_FunctionStatementList_StatementList, (), ("StatementList",), id="StatementList"
            ),
            pytest.param([], e.P2_FunctionStatementList_EMPTY, (), (), id="[empty]"),
        ],
    )
    def test_parse_FunctionStatementList_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("StatementList", is_present(STATEMENTLIST), "StrictArg", True, "YieldArg", "AwaitArg", True),
        )

        fsl = e.parse_FunctionStatementList(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(fsl, expected_class)
        for key in production_checks:
            assert getattr(fsl, key) == key, f"assert fsl.{key} == {key!r}"
        for idx, expected in token_checks:
            assert fsl.children[idx] == expected, f"assert fsl.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()


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
        af = e.P2_ArrowFunction(context, "StrictArg", ["child"])
        assert af.name == "ArrowFunction"
        assert af.context == context
        assert af.children == ["child"]

    def test_P2_ArrowFunction_ArrowParameters_ConciseBody_init(self, context):
        af = e.P2_ArrowFunction_ArrowParameters_ConciseBody(
            context, "StrictArg", ["arrowparameters", "=>", "concisebody"]
        )
        assert af.name == "ArrowFunction"
        assert af.ArrowParameters == "arrowparameters"
        assert af.ConciseBody == "concisebody"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "ArrowParameters": mocker.patch(
                "ecmascript.ecmascript.parse_ArrowParameters", side_effect=arrowparameters_sideeffect
            ),
            "ConciseBody": mocker.patch(
                "ecmascript.ecmascript.parse_ConciseBody", side_effect=concisebody_sideeffect
            ),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [ARROWPARAMETERS, EQGT_NONL, CONCISEBODY],
                e.P2_ArrowFunction_ArrowParameters_ConciseBody,
                ((1, EQGT_NONL),),
                ("ArrowParameters", "ConciseBody"),
                id="ArrowParameters => ConciseBody",
            )
        ],
    )
    def test_parse_ArrowFunction_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("ArrowParameters", is_present(ARROWPARAMETERS), "StrictArg", "YieldArg", "AwaitArg"),
            mock_check("ConciseBody", is_present(CONCISEBODY), "StrictArg", "InArg"),
        )

        af = e.parse_ArrowFunction(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
        assert isinstance(af, expected_class)
        for key in production_checks:
            assert getattr(af, key) == key, f"assert af.{key} == {key!r}"
        for idx, expected in token_checks:
            assert af.children[idx] == expected, f"assert af.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize(
        "token_stream",
        synerror_streams([(ARROWPARAMETERS, EQGT_NONL, CONCISEBODY)])
        + [
            pytest.param((ARROWPARAMETERS, EQGT, CONCISEBODY), id="ArrowParameters [newline] => ConciseBody"),
            pytest.param((ARROWPARAMETERS, FUNCTION_NONL), id="ArrowParameters [no newline] function"),
        ],
    )
    def test_parse_ArrowFunction_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("ArrowParameters", False, "StrictArg", "YieldArg", "AwaitArg"),
            mock_check("ConciseBody", False, "StrictArg", "InArg"),
        )

        af = e.parse_ArrowFunction(context, lexer, "StrictArg", "InArg", "YieldArg", "AwaitArg")
        assert af is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
        ap = e.P2_ArrowParameters(context, "StrictArg", ["child"])
        assert ap.name == "ArrowParameters"
        assert ap.context == context
        assert ap.children == ["child"]

    def test_P2_ArrowParameters_BindingIdentifier_init(self, context):
        ap = e.P2_ArrowParameters_BindingIdentifier(context, "StrictArg", ["child"])
        assert ap.name == "ArrowParameters"
        assert ap.BindingIdentifier == "child"

    def test_P2_ArrowParameters_CoverParenthesizedExpressionAndArrowParameterList_init(self, context):
        ap = e.P2_ArrowParameters_CoverParenthesizedExpressionAndArrowParameterList(context, "StrictArg", ["child"])
        assert ap.name == "ArrowParameters"
        assert ap.CoverParenthesizedExpressionAndArrowParameterList == "child"

    @staticmethod
    def setup_mocks(mocker):
        return {
            "BindingIdentifier": mocker.patch(
                "ecmascript.ecmascript.parse_BindingIdentifier", side_effect=bindingidentifier_sideeffect
            ),
            "CoverParenthesizedExpressionAndArrowParameterList": mocker.patch(
                "ecmascript.ecmascript.parse_CoverParenthesizedExpressionAndArrowParameterList",
                side_effect=coverparenthesizedexpressionandarrowparameterlist_sideeffect,
            ),
        }

    @pytest.mark.parametrize(
        "token_stream, expected_class, token_checks, production_checks",
        [
            pytest.param(
                [BINDINGIDENTIFIER],
                e.P2_ArrowParameters_BindingIdentifier,
                (),
                ("BindingIdentifier",),
                id="BindingIdentifier",
            ),
            pytest.param(
                [COVERPARENTHESIZEDEXPRESSIONANDARROWPARAMETERLIST],
                e.P2_ArrowParameters_CoverParenthesizedExpressionAndArrowParameterList,
                (),
                ("CoverParenthesizedExpressionAndArrowParameterList",),
                id="CoverParenthesizedExpressionAndArrowParameterList",
            ),
        ],
    )
    def test_parse_ArrowParameters_01(
        self, mocker, context, token_stream, expected_class, token_checks, production_checks
    ):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        is_present = lambda tok: tok in token_stream
        call_checks = (
            mock_check("BindingIdentifier", is_present(BINDINGIDENTIFIER), "StrictArg", "YieldArg", "AwaitArg"),
            mock_check(
                "CoverParenthesizedExpressionAndArrowParameterList",
                is_present(COVERPARENTHESIZEDEXPRESSIONANDARROWPARAMETERLIST),
                "StrictArg",
                "YieldArg",
                "AwaitArg",
            ),
        )

        ap = e.parse_ArrowParameters(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert isinstance(ap, expected_class)
        for key in production_checks:
            assert getattr(ap, key) == key, f"assert ap.{key} == {key!r}"
        for idx, expected in token_checks:
            assert ap.children[idx] == expected, f"assert ap.children[{idx}] == {expected!r}"
        assert lexer.pos == len(token_stream)
        for check in call_checks:
            check()

    @pytest.mark.parametrize(
        "token_stream",
        synerror_streams([(BINDINGIDENTIFIER,), (COVERPARENTHESIZEDEXPRESSIONANDARROWPARAMETERLIST,)]),
    )
    def test_parse_ArrowParameters_02(self, mocker, context, token_stream):
        lexer = Lexer(token_stream)
        mocks = self.setup_mocks(mocker)
        mock_check = gen_mock_check(mocker, mocks, context, lexer)
        call_checks = (
            mock_check("BindingIdentifier", False, "StrictArg", "YieldArg", "AwaitArg"),
            mock_check(
                "CoverParenthesizedExpressionAndArrowParameterList", False, "StrictArg", "YieldArg", "AwaitArg"
            ),
        )

        ap = e.parse_ArrowParameters(context, lexer, "StrictArg", "YieldArg", "AwaitArg")
        assert ap is None
        assert lexer.pos == 0
        for check in call_checks:
            check()


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
    script = e.P2_Script(context, "StrictArg", ["child"])
    assert script.name == "Script"
    assert script.context == context
    assert script.children == ["child"]


def test_P2_Script_Empty_init(context):
    script = e.P2_Script_Empty(context, "StrictArg", ["child"])
    assert script.name == "Script"
    assert script.ScriptBody is None


def test_P2_Script_ScriptBody_init(context):
    script = e.P2_Script_ScriptBody(context, "StrictArg", ["child"])
    assert script.name == "Script"
    assert script.ScriptBody == "child"


def Script_mocks(mocker):
    return {"ScriptBody": mocker.patch("ecmascript.ecmascript.parse_ScriptBody", side_effect=scriptbody_sideeffect)}


def test_parse_Script_empty(mocker, context):
    # Script : [empty]
    lexer = Lexer([])
    Script_mocks(mocker)

    script = e.parse_Script(context, lexer, "StrictArg")
    assert isinstance(script, e.P2_Script_Empty)
    assert lexer.pos == 0


def test_parse_Script_ScriptBody(mocker, context):
    # Script : ScriptBody
    lexer = Lexer([SCRIPTBODY_REPLACEMENT])
    mocks = Script_mocks(mocker)

    script = e.parse_Script(context, lexer, "StrictArg")
    assert isinstance(script, e.P2_Script_ScriptBody)
    assert script.ScriptBody == "ScriptBody"
    assert lexer.pos == 1
    mocks["ScriptBody"].assert_called_with(context, lexer, "StrictArg")


def test_parse_Script_errs(mocker, context):
    # Syntax errs
    lexer = Lexer([MATCHES_NONE])
    Script_mocks(mocker)

    script = e.parse_Script(context, lexer, "StrictArg")
    assert script is None
    assert lexer.pos == 0


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
    sb = e.P2_ScriptBody(context, "StrictArg", ["child"])
    assert sb.name == "ScriptBody"
    assert sb.context == context
    assert sb.children == ["child"]


def test_P2_ScriptBody_StatementList_init(context):
    sb = e.P2_ScriptBody_StatementList(context, "StrictArg", ["child"])
    assert sb.name == "ScriptBody"
    assert sb.StatementList == "child"


def ScriptBody_mocks(mocker):
    return {
        "StatementList": mocker.patch(
            "ecmascript.ecmascript.parse_StatementList", side_effect=statementlist_sideeffect
        )
    }


def test_parse_ScriptBody_StatementList(mocker, context):
    # ScriptBody : StatementList
    lexer = Lexer([STATEMENTLIST])
    mocks = ScriptBody_mocks(mocker)

    sb = e.parse_ScriptBody(context, lexer, "StrictArg")
    assert isinstance(sb, e.P2_ScriptBody_StatementList)
    assert sb.StatementList == "StatementList"
    assert lexer.pos == 1
    mocks["StatementList"].assert_called_with(context, lexer, "StrictArg", True, False, False, False)


@pytest.mark.parametrize("token_stream", [[], [MATCHES_NONE]])
def test_parse_ScriptBody_errs(mocker, context, token_stream):
    # Syntax Errs
    lexer = Lexer(token_stream)
    ScriptBody_mocks(mocker)

    sb = e.parse_ScriptBody(context, lexer, "StrictArg")
    assert sb is None
    assert lexer.pos == 0


##########################################################################################
##########################################################################################


@pytest.mark.parametrize(
    "stub",
    [
        e.parse_ClassExpression,
        e.parse_AsyncFunctionExpression,
        e.parse_AsyncGeneratorExpression,
        e.parse_AwaitExpression,
        e.parse_AsyncArrowFunction,
        e.parse_ClassDeclaration,
        e.parse_AsyncFunctionDeclaration,
        e.parse_AsyncGeneratorDeclaration,
    ],
)
def test_stubs(stub):
    # This really isn't testing anything. It's here to get to 100% coverage.
    # (All of these functions will go away, once implemented, and then this
    # entire test will be removed.)
    assert stub() is None

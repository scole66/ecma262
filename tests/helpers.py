from enum import Enum, unique, auto
from collections import namedtuple
from functools import reduce, wraps
from itertools import chain, product
import regex
from typing import TypeVar, Iterable, Tuple, Sized
import pytest
from dataclasses import dataclass

import snoop

from ecmascript.lexer2 import Span, RegExp  # , Token
from ecmascript.lexer2 import Token as NewToken
from ecmascript.ecmascript import ParseNode2


class Token:
    def __init__(self, tokentype, value, newlines=[(0, 1)], src=None):
        self.type = tokentype
        self.value = value
        self.newlines = tuple(newlines)
        self.src = src or str(value)
        self.span = Span(0, len(self.src))

    def __repr__(self):
        return f"Token[{self.type}({self.value})]"


class Lexer:
    def __init__(self, token_sequence):
        self.pos = 0
        self.sequence = token_sequence

    def peek_token(self, count=1):
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
REGULAREXPRESSIONLITERAL = Token("REGEXP", RegExp("abcd", ""))

MATCHES_NONE = Token("MATCHES_NONE", "nope")


def sideeffect(symbol):
    def side_effect(context, lexer, *args):
        peek = lexer.peek_token()
        if peek and peek.type == "REPLACEMENT" and peek.value == symbol:
            lexer.pos += 1
            return peek.value
        return None

    return side_effect


def parser_mock(symbol):
    return (Token("REPLACEMENT", symbol), sideeffect(symbol))


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
SLI_REPLACEMENT, sli_sideeffect = parser_mock("StatementListItem")
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
LITERAL, literal_sideeffect = parser_mock("Literal")
ARRAYLITERAL, arrayliteral_sideeffect = parser_mock("ArrayLiteral")
OBJECTLITERAL, objectliteral_sideeffect = parser_mock("ObjectLiteral")
TEMPLATELITERAL, templateliteral_sideeffect = parser_mock("TemplateLiteral")
FUNCTIONEXPRESSION, functionexpression_sideeffect = parser_mock("FunctionExpression")
CLASSEXPRESSION, classexpression_sideeffect = parser_mock("ClassExpression")
GENERATOREXPRESSION, generatorexpression_sideeffect = parser_mock("GeneratorExpression")
ASYNCFUNCTIONEXPRESSION, asyncfunctionexpression_sideeffect = parser_mock("AsyncFunctionExpression")
ASYNCGENERATOREXPRESSION, asyncgeneratorexpression_sideeffect = parser_mock("AsyncGeneratorExpression")


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


def syntax_errify(production, sentinel=MATCHES_NONE):
    collected = tuple()
    yield collected
    yield collected + (sentinel,)
    for symbol in production[:-1]:
        collected += (symbol,)
        yield collected + (sentinel,)


def errify_many(productions, sentinel=MATCHES_NONE):
    for production in productions:
        yield from syntax_errify(production, sentinel)


def synerror_streams(productions):
    def parametrize(seq):
        return pytest.param(seq, id=" ".join(x.value for x in seq))

    return [
        parametrize(item)
        for item in sorted(list(set(errify_many(productions))), key=lambda x: " ".join(term.value for term in x))
    ]


class Expected_Exception(Exception):
    pass


@dataclass
class FakeToken:
    value: str
    span: Span
    src: str
    contains_unenclosed_continue: bool = False
    contains_encapsulated_unenclosed_continue: bool = False
    contains_unenclosed_break: bool = False
    contains_encapsulated_unenclosed_break: bool = False


def FakeTokens(*children):
    src = " ".join(children) + " "

    def tokenstitch(children):
        prev = 0
        for ch in children:
            yield FakeToken(ch, Span(prev, prev + len(ch)), src)
            prev += len(ch) + 1

    return list(tokenstitch(children))


NOPE = "¿"


def stream_id(item):
    return " ".join(after or before for before, mark, after in (token.partition("¡") for token in item))


def _errclass(stream, productions):
    # If stream, not including the final sentinel, matches one of the production streams, return that stream's class.
    # Else return type(None).
    seq = stream[0:-1]
    try:
        return max(
            ((x[1], len(x[0])) for x in productions if len(seq) >= len(x[0]) and seq[0 : len(x[0])] == x[0]),
            key=lambda z: z[1],
        )[0]
    except ValueError:
        return type(None)


def synerror2_streams(productions):
    # productions is sequnce of pairs. The first item of each pair is a sequence of strings representing the parts
    # of the production, the second is the class that would be formed by this production.

    # Make productions into a sequence we can use multiple times
    prod_data = tuple(productions)

    # And then errify them. Errification makes a sequence of production parts; we then need to add back in the class
    # that would be formed. (This is often None, but may occasionally be a smaller form.)
    errified = tuple(
        (stream, _errclass(stream, prod_data)) for stream in set(errify_many(map(first, prod_data), NOPE))
    )

    # And then make them into pytest parameters
    return tuple(
        pytest.param(item, err_class, id=stream_id(item))
        for item, err_class in sorted(errified, key=lambda x: " ".join(x[0]))
    )


_gp_src = r"(\[(?P<flag>[+~])(?P<name>[A-Z][a-z]+)\])"
_guard_pattern = regex.compile(_gp_src)
_guardtok_pattern = regex.compile(rf"(?P<guard>{_gp_src})?(?P<core>.*)")


def _guardsplit(tokenstream):
    if tokenstream:
        m = _guardtok_pattern.match(tokenstream[0])
        guard = m.group("guard")
        first_token = m.group("core")
        return (guard, (first_token,) + tokenstream[1:])
    return (None, ())


def _prod_item(item, ecls):
    guard, tokenstream = _guardsplit(item)
    return pytest.param(tokenstream, ecls, guard, id=stream_id(tokenstream))


def prod_streams(productions):
    return tuple(_prod_item(item, ecls) for item, ecls in productions)


def _span_reduce(lst, val):
    start = lst[-1].after + 1 if lst else 0
    after = start + len(val)
    return lst + (Span(start, after),)


def _new_token(tok_type, src, span, newlines):
    parts = tok_type.partition("¡")
    ttype = parts[0]
    value = parts[2] or parts[0]
    newspan = Span(span.after - len(value), span.after)
    return NewToken(ttype, src, value, newspan, newlines)


def _new_id(ident, src, span, newlines):
    return NewToken("IDENTIFIER", src, ident, span, newlines)


_all_lowercase = regex.compile("^[a-z]+$")


def _new_token_or_id(tok, src, span, newlines):
    t = _new_token(tok, src, span, newlines)
    if _all_lowercase.match(t.type):
        return NewToken("IDENTIFIER", t.src, t.value, t.span, t.newlines)
    return t


def _gen_se_data(token_stream, offset):
    src = ("#" * offset) + (" ".join(token_stream))
    spans_fixed = reduce(_span_reduce, token_stream, ())
    spans = tuple(Span(start + offset, after + offset) for start, after in spans_fixed)
    prev = offset
    xlat = {}
    for si, span in enumerate(spans):
        start, after = span
        for idx in range(prev, start + 1):
            xlat[idx] = si
        prev = after
    return (src, spans, xlat)


def _token_se(token_stream, lex_pos, ctor):
    "Generate a side effect function for the token stream that starts at lex_pos"
    src, spans, xlat = _gen_se_data(token_stream, lex_pos)

    def side_effect(pos, tok_type=None, prior_newline_allowed=True, goal="unspecified"):
        "Side effect function for lexer.token_if and lexer.id_if."
        no_newline = False
        if pos in xlat:
            stream_index = xlat[pos]
            token = token_stream[stream_index]
            before, _, after = token.partition("¡")
            if before == "NO_LT":
                no_newline = True
                before = after
                token = after
            if tok_type is None or before == tok_type:
                span = spans[stream_index]
                newlines = () if no_newline else (Span(span.start, span.after),)
                if not prior_newline_allowed and len(newlines) > 0:
                    return None
                return ctor(token, src, span, newlines)
        return None

    return side_effect


def token_if_se(token_stream, lex_pos):
    return _token_se(token_stream, lex_pos, _new_token)


def id_if_se(token_stream, lex_pos):
    return _token_se(token_stream, lex_pos, _new_id)


def token_abs_se(token_stream, lex_pos):
    return _token_se(token_stream, lex_pos, _new_token_or_id)


def parse_se(token_stream, lex_pos, name, mocker, pn_ctor):
    src, spans, xlat = _gen_se_data(token_stream, lex_pos)

    def side_effect(context, lexer, pos, strict, *args, **kwargs):
        if pos in xlat:
            stream_index = xlat[pos]
            if token_stream[stream_index] == name:
                pn = pn_ctor(context, name, strict, [NewToken("REPLACEMENT", src, name, spans[stream_index], [])])
                pn.value = name
                return pn
        return None

    return side_effect


def lexer_mock(mocker, token_stream, lex_pos):
    return mocker.Mock(
        **{
            "token_if.side_effect": token_if_se(token_stream, lex_pos),
            "id_if.side_effect": id_if_se(token_stream, lex_pos),
            "token.side_effect": token_abs_se(token_stream, lex_pos),
            "reserved_words": ("for", "while", "return", "break", "continue"),
            "InputElementDiv": "InputElementDiv",
            "InputElementRegExp": "InputElementRegExp",
            "InputElementRegExpOrTemplateTail": "InputElementRegExpOrTemplateTail",
            "InputElementTemplateTail": "InputElementTemplateTail",
        }
    )


def prod_mocks(mocker, token_stream, lex_pos, pn_ctor, identifiers):
    parse_mock = lambda name: mocker.patch(
        f"ecmascript.ecmascript.parse_{name}", side_effect=parse_se(token_stream, lex_pos, name, mocker, pn_ctor)
    )
    return dict((name, parse_mock(name)) for name in identifiers)


_T = TypeVar("_T")


def first(seq: Iterable[_T]) -> _T:
    return next(iter(seq))


def prod_args(argnames):
    values = product((False, True), repeat=len(argnames))
    return (
        pytest.param(val, id="-".join(name.upper() if v else name.lower() for v, name in zip(val, argnames)))
        for val in values
    )


def ordinary_test_params(target_argnames, productions):
    def decorator(func):
        @wraps(func)
        @pytest.mark.parametrize("strict_flag", (pytest.param(False, id="strict"), pytest.param(True, id="STRICT")))
        @pytest.mark.parametrize("prod_args", prod_args(target_argnames))
        @pytest.mark.parametrize("lex_pos", (0, 10))
        @pytest.mark.parametrize("token_stream, expected_class, guard", prod_streams(productions))
        def wrapped(*args, **kwargs):
            return func(*args, **kwargs)

        return wrapped

    return decorator


def syntax_error_test_params(target_argnames, productions):
    def decorator(func):
        @wraps(func)
        @pytest.mark.parametrize("strict_flag", (pytest.param(False, id="strict"), pytest.param(True, id="STRICT")))
        @pytest.mark.parametrize("prod_args", prod_args(target_argnames))
        @pytest.mark.parametrize("lex_pos", (0, 10))
        @pytest.mark.parametrize("token_stream, error_class", synerror2_streams(productions))
        def wrapped(*args, **kwargs):
            return func(*args, **kwargs)

        return wrapped

    return decorator


def strict_params(func):
    @wraps(func)
    @pytest.mark.parametrize("strict", (pytest.param(False, id="loose"), pytest.param(True, id="strict")))
    def wrapped(*args, **kwargs):
        return func(*args, **kwargs)

    return wrapped


def _part(idx, item):
    try:
        return item[idx]
    except TypeError:
        return item


class parse_test:
    productions = ()

    @classmethod
    def setup_prod_mocks(cls, mocker, token_stream, lex_pos):
        return prod_mocks(
            mocker,
            token_stream,
            lex_pos,
            ParseNode2,
            set(
                p
                for p in chain.from_iterable(_guardsplit(prod)[1] for prod, ecls in cls.productions)
                if p[0].isupper() and "¡" not in p
            ),
        )

    target = staticmethod(lambda context, lexer, pos, strict, *args: None)
    target_argnames = ()
    called_argnames = {}

    def expected_args(self, name: str, args: Iterable[bool]) -> Tuple[bool]:
        assert name in self.called_argnames
        assert len(args) == len(self.target_argnames)
        assert all(len(param) > 0 for param in self.target_argnames)
        assert all(len(arg) > 1 and arg[0] in "+~?" for arg in self.called_argnames[name])
        # The argument names for the production
        tgt_names = self.target_argnames
        # The argument names (and markers) for the child production. Note that each item should have a marker, and
        # that marker should be one of "+", "~", or "?"
        ch_names = self.called_argnames[name]
        return tuple(
            True
            if chn[0] == "+"
            else False
            if chn[0] == "~"
            else first(bool(val) for name, val in zip(tgt_names, args) if name == chn[1:])
            for chn in ch_names
        )

    def _should_produce_node(self, guard, args):
        # Checks the args against guard to see if this production should count or not
        if guard is None:
            return True
        m = _guard_pattern.match(guard)
        assert m
        flag = m.group("flag")
        name = m.group("name")
        passed_args = dict(zip(self.target_argnames, args))
        return passed_args[name] == (flag == "+")

    def ordinary(self, mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag):
        lexer = lexer_mock(mocker, token_stream, lex_pos)
        prod_mocks = self.setup_prod_mocks(mocker, token_stream, lex_pos)

        rv = self.target(context, lexer, lex_pos, strict_flag, *prod_args)
        if self._should_produce_node(guard, prod_args):
            assert isinstance(rv, _part(0, expected_class))
            for name, mock in prod_mocks.items():
                if name in token_stream:
                    mock.assert_called_with(
                        context, lexer, mocker.ANY, strict_flag, *self.expected_args(name, prod_args)
                    )
            assert rv.strict == strict_flag
            terminals = list(rv.terminals())
            assert len(terminals) == len(token_stream)
            for actual, token in zip(terminals, token_stream):
                value = actual.value
                before, _, after = token.partition("¡")
                expected = after or before
                assert value == expected
        else:
            assert isinstance(rv, _part(1, expected_class))

    def syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        lexer = lexer_mock(mocker, token_stream, lex_pos)
        self.setup_prod_mocks(mocker, token_stream, lex_pos)

        rv = self.target(context, lexer, lex_pos, strict_flag, *prod_args)
        assert isinstance(rv, error_class)

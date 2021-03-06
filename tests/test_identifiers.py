from itertools import chain
import pytest

from .helpers import *

import ecmascript.ecmascript as ecmascript


def test_IdentifierReference_init(context):
    ir = ecmascript.P2_IdentifierReference(context, "strict", FakeTokens("child"), "YieldArg", "AwaitArg")

    assert ir.name == "IdentifierReference"
    assert ir.context == context
    assert [tok.value for tok in ir.children] == ["child"]
    assert ir.Yield == "YieldArg"
    assert ir.Await == "AwaitArg"
    assert ir.strict == "strict"


def test_IdentifierReference_Identifier_init(context):
    ir = ecmascript.P2_IdentifierReference_Identifier(context, "strict", FakeTokens("child"), "YieldArg", "AwaitArg")
    assert ir.name == "IdentifierReference"
    assert ir.Identifier.value == "child"


def test_IdentifierReference_YIELD_init(context):
    ir = ecmascript.P2_IdentifierReference_YIELD(context, "strict", FakeTokens("child"), "YieldArg", "AwaitArg")
    assert ir.name == "IdentifierReference"


def test_IdentifierReference_AWAIT_init(context):
    ir = ecmascript.P2_IdentifierReference_AWAIT(context, "strict", FakeTokens("child"), "YieldArg", "AwaitArg")
    assert ir.name == "IdentifierReference"


@pytest.mark.parametrize(
    "Yield, Await, identifiername, expected",
    [
        (False, False, "yield", 0),
        (False, False, "await", 0),
        (False, False, "normal", 0),
        (True, False, "yield", 1),
        (True, False, "await", 0),
        (True, False, "normal", 0),
        (False, True, "yield", 0),
        (False, True, "await", 1),
        (False, True, "normal", 0),
        (True, True, "yield", 1),
        (True, True, "await", 1),
        (True, True, "normal", 0),
    ],
)
def test_IdentifierReference_Identifier_EarlyErrors(context, Yield, Await, identifiername, expected):
    class Check(ecmascript.ParseNode2):
        StringValue = identifiername

    ir = ecmascript.P2_IdentifierReference_Identifier(
        context, "strict", [Check(context, "Check", "strict", FakeTokens(identifiername))], Yield, Await
    )
    errs = ir.EarlyErrors()
    assert len(errs) == expected
    for err in errs:
        assert type(err) == Expected_Exception


@pytest.mark.parametrize("goal, expected", [("Script", 0), ("Module", 1)])
def test_IdentifierReference_AWAIT_EarlyErrors(goal, expected):
    p2c = ecmascript.Parse2Context(goal=goal, syntax_error_ctor=Expected_Exception)
    ir = ecmascript.P2_IdentifierReference_AWAIT(p2c, True, FakeTokens("await"), False, False)
    errs = ir.EarlyErrors()
    assert len(errs) == expected
    for err in errs:
        assert type(err) == Expected_Exception


@pytest.mark.parametrize("strict, expected", [(True, 1), (False, 0)])
def test_IdentifierReference_YIELD_EarlyErrors(context, strict, expected):
    ir = ecmascript.P2_IdentifierReference_YIELD(context, strict, FakeTokens("yield"), False, False)
    errs = ir.EarlyErrors()
    assert len(errs) == expected
    for err in errs:
        assert type(err) == Expected_Exception


def test_IdentifierReference_YIELD_StringValue(context):
    ir = ecmascript.P2_IdentifierReference_YIELD(context, "strict", FakeTokens("yield"), False, False)
    assert ir.StringValue == "yield"


def test_IdentifierReference_AWAIT_StringValue(context):
    ir = ecmascript.P2_IdentifierReference_AWAIT(context, "strict", FakeTokens("await"), False, False)
    assert ir.StringValue == "await"


def test_IdentifierReference_Identifier_StringValue(context):
    class Check(ecmascript.ParseNode2):
        StringValue = "identifiername"

    ir = ecmascript.P2_IdentifierReference_Identifier(
        context, "strict", [Check(context, "Check", "strict", FakeTokens("tok"))], False, False
    )
    assert ir.StringValue == "identifiername"


def test_IdentifierReference_YIELD_AssignmentTargetType(context):
    ir = ecmascript.P2_IdentifierReference_YIELD(context, "strict", FakeTokens("yield"), False, False)
    assert ir.AssignmentTargetType == ecmascript.SIMPLE


def test_IdentifierReference_AWAIT_AssignmentTargetType(context):
    ir = ecmascript.P2_IdentifierReference_AWAIT(context, "strict", FakeTokens("await"), False, False)
    assert ir.AssignmentTargetType == ecmascript.SIMPLE


@pytest.mark.parametrize(
    "strict, name, expected",
    [
        pytest.param(
            strict, name, ecmascript.STRICT if strict and name in ("eval", "arguments") else ecmascript.SIMPLE
        )
        for strict in (False, True)
        for name in ("normal", "eval", "arguments")
    ],
)
def test_IdentifierReference_Identifier_AssignmentTargetType(context, strict, name, expected):
    class Check(ecmascript.ParseNode2):
        StringValue = name

    ir = ecmascript.P2_IdentifierReference_Identifier(
        context, "strict", [Check(context, "Check", "strict", FakeTokens("bob"))], False, False
    )
    ir.strict = strict
    assert ir.AssignmentTargetType == expected


def test_IdentifierReference_Identifier_evaluate(context, mocker):
    # 12.1.6 Runtime Semantics: Evaluation
    #       IdentifierReference : Identifier
    #   1. Return ? ResolveBinding(StringValue of Identifier).

    # Setup: StringValue will be 'identifier_test', and ResolveBinding will return 67.
    identifier = mocker.Mock()
    identifier.StringValue = "identifier_test"
    ir = ecmascript.P2_IdentifierReference_Identifier(context, "strict", [identifier], False, False)
    ir.strict = "strict"
    rb = mocker.patch("ecmascript.ecmascript.ResolveBinding", return_value=67)

    # Execute function under test
    rv = ir.evaluate()

    # Examine results
    assert rv == 67
    rb.assert_called_with("identifier_test", "strict")


def test_IdentifierReference_YIELD_evaluate(context, mocker):
    # 12.1.6 Runtime Semantics: Evaluation
    #       IdentifierReference : yield
    #   1. Return ? ResolveBinding("yield").

    # Setup: ResolveBinding will return 67.
    ir = ecmascript.P2_IdentifierReference_YIELD(context, "strict", FakeTokens("yield"), False, False)
    ir.strict = "strict"
    rb = mocker.patch("ecmascript.ecmascript.ResolveBinding", return_value=67)

    # Execute function under test
    rv = ir.evaluate()

    # Examine results
    assert rv == 67
    rb.assert_called_with("yield", "strict")


def test_IdentifierReference_AWAIT_evaluate(context, mocker):
    # 12.1.6 Runtime Semantics: Evaluation
    #       IdentifierReference : await
    #   1. Return ? ResolveBinding("await").

    # Setup: ResolveBinding will return 67.
    ir = ecmascript.P2_IdentifierReference_AWAIT(context, "strict", FakeTokens("await"), False, False)
    ir.strict = "strict"
    rb = mocker.patch("ecmascript.ecmascript.ResolveBinding", return_value=67)

    # Execute function under test
    rv = ir.evaluate()

    # Examine results
    assert rv == 67
    rb.assert_called_with("await", "strict")


class Test_parse_IdentifierReference(parse_test):
    # Syntax
    #   IdentifierReference[Yield, Await]:
    #       Identifier
    #       [~Yield]yield
    #       [~Await]await
    target = staticmethod(ecmascript.parse_IdentifierReference)
    target_argnames = ("Yield", "Await")

    IR_Ident = ecmascript.P2_IdentifierReference_Identifier
    IR_Yield = ecmascript.P2_IdentifierReference_YIELD
    IR_Await = ecmascript.P2_IdentifierReference_AWAIT
    productions = (
        (("Identifier",), IR_Ident),
        (("[~Yield]yield",), (IR_Yield, type(None))),
        (("[~Await]await",), (IR_Await, type(None))),
    )
    called_argnames = {
        "Identifier": (),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


def test_P2_Identifier_init(context):
    ident = ecmascript.P2_Identifier(context, "strict", FakeTokens("child"))
    assert ident.name == "Identifier"
    assert ident.context == context
    assert [tok.value for tok in ident.children] == ["child"]


def test_P2_Identifier_IdentifierName_init(context):
    ident = ecmascript.P2_Identifier_IdentifierName(context, "strict", FakeTokens("IdentifierName"))
    assert ident.name == "Identifier"
    assert ident.IdentifierName.value == "IdentifierName"


def test_P2_Identifier_IdentifierName_StringValue(context):
    ident = ecmascript.P2_Identifier_IdentifierName(context, "strict", [Token("IDENTFIER", "value")])
    assert ident.StringValue == "value"


@pytest.mark.parametrize(
    "strict, value, goal, expected",
    [
        pytest.param(True, value, "Script", 1, id=f"Strict: {value}")
        for value in (
            "implements",
            "interface",
            "let",
            "package",
            "private",
            "protected",
            "public",
            "static",
            "yield",
        )
    ]
    + [pytest.param(True, "await", "Module", 1, id="Module: await")]
    + [
        pytest.param(False, value, "Script", 1, id=f"Keyword: {value}")
        for value in (
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
            "enum",
            "null",
            "true",
            "false",
        )
    ]
    + [pytest.param(True, "normal", "Script", 0, id="ordinary")],
)
def test_P2_Identifier_IdentifierName_EarlyErrors(context, strict, value, goal, expected):
    context.goal = goal
    ident = ecmascript.P2_Identifier_IdentifierName(context, "strict", [Token("IDENTIFIER", value)])
    ident.strict = strict
    errs = ident.EarlyErrors()
    assert len(errs) == expected
    for err in errs:
        assert type(err) == Expected_Exception


class Test_parse_Identifier(parse_test):
    # Syntax
    #   Identifier:
    #       IdentifierName but not ReservedWord
    ID_IdentName = ecmascript.P2_Identifier_IdentifierName
    target = staticmethod(ecmascript.parse_Identifier)
    productions = (
        (("IDENTIFIER¡bob",), ID_IdentName),
        ((r"IDENTIFIER¡f\u006fr",), ID_IdentName),
    )
    target_argnames = ()

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


def test_P2_BindingIdentifier_init(context):
    ir = ecmascript.P2_BindingIdentifier(context, "strict", FakeTokens("child"), False, True)

    assert ir.name == "BindingIdentifier"
    assert ir.context == context
    assert [tok.value for tok in ir.children] == ["child"]
    assert not ir.Yield
    assert ir.Await


def test_P2_BindingIdentifier_Identifier_init(context):
    ir = ecmascript.P2_BindingIdentifier_Identifier(context, "strict", FakeTokens("child"), True, True)
    assert ir.name == "BindingIdentifier"
    assert ir.Identifier.value == "child"


@pytest.mark.parametrize(
    "Yield, Await, identifiername, strict, expected",
    [
        pytest.param(
            Yield,
            Await,
            sv,
            strict,
            int((Yield and sv == "yield") or (Await and sv == "await") or (strict and sv in ("eval", "arguments"))),
            id=f"{sv}{'-Yield' if Yield else ''}{'-Await' if Await else ''}{'-Strict' if strict else ''}",
        )
        for Yield in (False, True)
        for Await in (False, True)
        for strict in (False, True)
        for sv in ("yield", "await", "eval", "arguments", "normal")
    ],
)
def test_P2_BindingIdentifier_Identifier_EarlyErrors(context, Yield, Await, identifiername, strict, expected):
    class Check(ecmascript.ParseNode2):
        StringValue = identifiername

    bi = ecmascript.P2_BindingIdentifier_Identifier(
        context, "strict", [Check(context, "Check", "strict", FakeTokens("bog"))], Yield, Await
    )
    bi.strict = strict
    errs = bi.EarlyErrors()
    assert len(errs) == expected
    for err in errs:
        assert type(err) == Expected_Exception


def test_P2_BindingIdentifier_YIELD_init(context):
    ir = ecmascript.P2_BindingIdentifier_YIELD(context, "strict", FakeTokens("child"), False, True)
    assert ir.name == "BindingIdentifier"


def test_P2_BindingIdentifier_AWAIT_init(context):
    ir = ecmascript.P2_BindingIdentifier_AWAIT(context, "strict", FakeTokens("child"), False, False)
    assert ir.name == "BindingIdentifier"


@pytest.mark.parametrize(
    "goal, Await, expected",
    [
        pytest.param(goal, Await, int(goal == "Module" or Await), id=f"{goal}{'-Await' if Await else ''}")
        for goal in ("Script", "Module")
        for Await in (False, True)
    ],
)
def test_BindingIdentifier_AWAIT_EarlyErrors(goal, Await, expected):
    p2c = ecmascript.Parse2Context(goal=goal, syntax_error_ctor=Expected_Exception)
    bi = ecmascript.P2_BindingIdentifier_AWAIT(p2c, False, FakeTokens("await"), False, Await)
    errs = bi.EarlyErrors()
    assert len(errs) == expected
    for err in errs:
        assert type(err) == Expected_Exception


@pytest.mark.parametrize(
    "strict, Yield, expected",
    [
        pytest.param(strict, Yield, int(strict or Yield), id=f"Strict:{strict!r}, Yield:{Yield!r}")
        for strict in (False, True)
        for Yield in (False, True)
    ],
)
def test_BindingIdentifier_YIELD_EarlyErrors(context, strict, Yield, expected):
    bi = ecmascript.P2_BindingIdentifier_YIELD(context, "strict", FakeTokens("yield"), Yield, False)
    bi.strict = strict
    errs = bi.EarlyErrors()
    assert len(errs) == expected
    for err in errs:
        assert type(err) == Expected_Exception


def test_BindingIdentifier_Identifier_BoundNames(context):
    identifiername = "test_result"

    class Check(ecmascript.ParseNode2):
        StringValue = identifiername

    bi = ecmascript.P2_BindingIdentifier_Identifier(
        context, "strict", [Check(context, "Check", "strict", FakeTokens(identifiername))], False, False
    )
    assert bi.BoundNames() == [identifiername]


def test_BindingIdentifier_YIELD_BoundNames(context):
    bi = ecmascript.P2_BindingIdentifier_YIELD(context, "strict", FakeTokens("yield"), False, False)
    assert bi.BoundNames() == ["yield"]


def test_BindingIdentifier_AWAIT_BoundNames(context):
    bi = ecmascript.P2_BindingIdentifier_AWAIT(context, "strict", FakeTokens("await"), False, False)
    assert bi.BoundNames() == ["await"]


def test_BindingIdentifier_Identifier_BindingInitialization(context, mocker):
    #   1. Let name be StringValue of Identifier.
    #   2. Return ? InitializeBoundName(name, value, environment).

    # Setup: StringValue will be 'identifier_test', and InitializeBoundName will return 67.
    identifier = mocker.Mock()
    identifier.StringValue = "identifier_test"
    bi = ecmascript.P2_BindingIdentifier_Identifier(context, "strict", [identifier], False, False)
    bi.strict = "strict"
    ibn = mocker.patch("ecmascript.ecmascript.InitializeBoundName", return_value=67)

    # Execute function under test
    rv = bi.BindingInitialization("value", "environment")

    # Examine results
    assert rv == 67
    ibn.assert_called_with("identifier_test", "value", "environment", "strict")


def test_BindingIdentifier_YIELD_BindingInitialization(context, mocker):
    # 1. Return ? InitializeBoundName("yield", value, environment).

    # Setup: InitializeBoundName will return 67.
    bi = ecmascript.P2_BindingIdentifier_YIELD(context, "strict", FakeTokens("yield"), False, False)
    bi.strict = "strict"
    ibn = mocker.patch("ecmascript.ecmascript.InitializeBoundName", return_value=67)

    # Execute function under test
    rv = bi.BindingInitialization("value", "environment")

    # Examine results
    assert rv == 67
    ibn.assert_called_with("yield", "value", "environment", "strict")


def test_BindingIdentifier_AWAIT_BindingInitialization(context, mocker):
    # 1. Return ? InitializeBoundName("await", value, environment).

    # Setup: InitializeBoundName will return 67.
    bi = ecmascript.P2_BindingIdentifier_AWAIT(context, "strict", FakeTokens("await"), False, False)
    bi.strict = "strict"
    ibn = mocker.patch("ecmascript.ecmascript.InitializeBoundName", return_value=67)

    # Execute function under test
    rv = bi.BindingInitialization("value", "environment")

    # Examine results
    assert rv == 67
    ibn.assert_called_with("await", "value", "environment", "strict")


class Test_parse_BindingIdentifier(parse_test):
    # Syntax
    #   BindingIdentifier[Yield, Await] :
    #       Identifier
    #       yield
    #       await

    BI_Ident = ecmascript.P2_BindingIdentifier_Identifier
    BI_Yield = ecmascript.P2_BindingIdentifier_YIELD
    BI_Await = ecmascript.P2_BindingIdentifier_AWAIT
    productions = ((("Identifier",), BI_Ident), (("yield",), BI_Yield), (("await",), BI_Await))
    target = staticmethod(ecmascript.parse_BindingIdentifier)
    target_argnames = ("Yield", "Await")
    called_argnames = {"Identifier": ()}

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


def test_P2_LabelIdentifier_init(context):
    ir = ecmascript.P2_LabelIdentifier(context, "strict", FakeTokens("child"), False, True)

    assert ir.name == "LabelIdentifier"
    assert ir.context == context
    assert [tok.value for tok in ir.children] == ["child"]
    assert not ir.Yield
    assert ir.Await


def test_P2_LabelIdentifier_Identifier_init(context):
    ir = ecmascript.P2_LabelIdentifier_Identifier(context, "strict", FakeTokens("child"), True, True)
    assert ir.name == "LabelIdentifier"
    assert ir.Identifier.value == "child"


def test_P2_LabelIdentifier_YIELD_init(context):
    ir = ecmascript.P2_LabelIdentifier_YIELD(context, "strict", FakeTokens("child"), False, True)
    assert ir.name == "LabelIdentifier"


def test_P2_LabelIdentifier_AWAIT_init(context):
    ir = ecmascript.P2_LabelIdentifier_AWAIT(context, "strict", FakeTokens("child"), False, False)
    assert ir.name == "LabelIdentifier"


@pytest.mark.parametrize(
    "Yield, Await, identifiername, expected",
    [
        (False, False, "yield", 0),
        (False, False, "await", 0),
        (False, False, "normal", 0),
        (True, False, "yield", 1),
        (True, False, "await", 0),
        (True, False, "normal", 0),
        (False, True, "yield", 0),
        (False, True, "await", 1),
        (False, True, "normal", 0),
        (True, True, "yield", 1),
        (True, True, "await", 1),
        (True, True, "normal", 0),
    ],
)
def test_LabelIdentifier_Identifier_EarlyErrors(context, Yield, Await, identifiername, expected):
    class Check(ecmascript.ParseNode2):
        StringValue = identifiername

    li = ecmascript.P2_LabelIdentifier_Identifier(
        context, "strict", [Check(context, "Check", "strict", FakeTokens("a"))], Yield, Await
    )
    errs = li.EarlyErrors()
    assert len(errs) == expected
    for err in errs:
        assert type(err) == Expected_Exception


@pytest.mark.parametrize("goal, expected", [("Script", 0), ("Module", 1)])
def test_LabelIdentifier_AWAIT_EarlyErrors(goal, expected):
    p2c = ecmascript.Parse2Context(goal=goal, syntax_error_ctor=Expected_Exception)
    li = ecmascript.P2_LabelIdentifier_AWAIT(p2c, False, FakeTokens("await"), False, False)
    errs = li.EarlyErrors()
    assert len(errs) == expected
    for err in errs:
        assert type(err) == Expected_Exception


@pytest.mark.parametrize("strict, expected", [(True, 1), (False, 0)])
def test_LabelIdentifier_YIELD_EarlyErrors(context, strict, expected):
    li = ecmascript.P2_LabelIdentifier_YIELD(context, "strict", FakeTokens("yield"), False, False)
    li.strict = strict
    errs = li.EarlyErrors()
    assert len(errs) == expected
    for err in errs:
        assert type(err) == Expected_Exception


class Test_parse_LabelIdentifier(parse_test):
    # Syntax
    #   LabelIdentifier[Yield, Await]:
    #       Identifier
    #       [~Yield]yield
    #       [~Await]await

    LI_Ident = ecmascript.P2_LabelIdentifier_Identifier
    LI_Yield = ecmascript.P2_LabelIdentifier_YIELD
    LI_Await = ecmascript.P2_LabelIdentifier_AWAIT
    productions = (
        (("Identifier",), LI_Ident),
        (("[~Yield]yield",), (LI_Yield, type(None))),
        (("[~Await]await",), (LI_Await, type(None))),
    )
    target = staticmethod(ecmascript.parse_LabelIdentifier)
    target_argnames = ("Yield", "Await")
    called_argnames = {
        "Identifier": (),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


class Test_InitializeBoundName:
    # 12.1.5.1 Runtime Semantics: InitializeBoundName ( name, value, environment )
    #   1. Assert: Type(name) is String.
    #   2. If environment is not undefined, then
    #     a. Let env be the EnvironmentRecord component of environment.
    #     b. Perform env.InitializeBinding(name, value).
    #     c. Return NormalCompletion(undefined).
    #   3. Else,
    #     a. Let lhs be ResolveBinding(name).
    #     b. Return ? PutValue(lhs, value).

    def test_WithEnvironment(self, mocker):
        # This one is the "environment not undefined" case
        environment = mocker.Mock()
        environment.environment_record = mocker.Mock()
        environment.environment_record.InitializeBinding = mocker.Mock()

        rv = ecmascript.InitializeBoundName("name_arg", "value_arg", environment, "strict_arg")

        assert rv is None
        environment.environment_record.InitializeBinding.assert_called_once_with("name_arg", "value_arg")

    def test_WithoutEnvironemnt(self, mocker):
        ResolveBinding = mocker.patch("ecmascript.ecmascript.ResolveBinding", return_value="lefthandside")
        PutValue = mocker.patch("ecmascript.ecmascript.PutValue", return_value=67)

        rv = ecmascript.InitializeBoundName("name_arg", "value_arg", None, "strict_arg")

        assert rv == 67
        ResolveBinding.assert_called_once_with("name_arg", "strict_arg")
        PutValue.assert_called_once_with("lefthandside", "value_arg")

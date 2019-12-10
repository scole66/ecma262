import pytest
import re

import ecmascript.ecmascript as ecmascript


@pytest.mark.parametrize(
    "intrinsic_name, error_type",
    [
        ("ReferenceError", ReferenceError),
        ("TypeError", TypeError),
        ("SyntaxError", SyntaxError),
        ("RangeError", ArithmeticError),
    ],
)
def test_CreateErrorObject_01(intrinsic_name, error_type):
    # Here, realm is not defined, so we should be able to get the raw python errors.
    rv = ecmascript.CreateErrorObject("test message", f"%{intrinsic_name}%")
    assert isinstance(rv, error_type)


@pytest.mark.parametrize("intrinsic_name", ["ReferenceError", "TypeError", "SyntaxError", "RangeError"])
def test_CreateErrorObject_02(realm, intrinsic_name):
    rv = ecmascript.CreateErrorObject("test message", f"%{intrinsic_name}%")
    assert ecmascript.isObject(rv)
    assert ecmascript.ToString(rv) == f"{intrinsic_name}: test message"


def test_CreateReferenceError_01(realm):
    re = ecmascript.CreateReferenceError("test reference error")

    assert ecmascript.isObject(re)
    assert ecmascript.ToString(re) == "ReferenceError: test reference error"


def test_CreateTypeError_01(realm):
    re = ecmascript.CreateTypeError("test type error")

    assert ecmascript.isObject(re)
    assert ecmascript.ToString(re) == "TypeError: test type error"


def test_CreateSyntaxError_01(realm):
    re = ecmascript.CreateSyntaxError("test syntax error")

    assert ecmascript.isObject(re)
    assert ecmascript.ToString(re) == "SyntaxError: test syntax error"


def test_CreateRangeError_01(realm):
    re = ecmascript.CreateRangeError("test range error")

    assert ecmascript.isObject(re)
    assert ecmascript.ToString(re) == "RangeError: test range error"


def test_ESError_01(realm):
    err = ecmascript.ESError(ecmascript.CreateRangeError("test"))

    assert str(err) == "RangeError: test"
    assert re.match(r"ESError\(.*\)", repr(err))


def test_ESReferenceError_01(realm):
    err = ecmascript.ESReferenceError("ref error")
    assert str(err) == "ReferenceError: ref error"


def test_ESTypeError_01(realm):
    err = ecmascript.ESTypeError("type error")
    assert str(err) == "TypeError: type error"


def test_ESSyntaxError_01(realm):
    err = ecmascript.ESSyntaxError("syntax error")
    assert str(err) == "SyntaxError: syntax error"


def test_ESRangeError_01(realm):
    err = ecmascript.ESRangeError("range error")
    assert str(err) == "RangeError: range error"


def test_ESAbrupt_01(realm):
    completion = ecmascript.ESAbrupt(ecmascript.CompletionType.THROW)
    assert completion.completion == ecmascript.Completion(
        ecmascript.CompletionType.THROW, ecmascript.Empty.EMPTY, ecmascript.Empty.EMPTY
    )


def test_ESBreak_01(realm):
    completion = ecmascript.ESBreak()
    assert completion.completion == ecmascript.Completion(
        ecmascript.CompletionType.BREAK, ecmascript.Empty.EMPTY, ecmascript.Empty.EMPTY
    )


def test_ESContinue_01(realm):
    completion = ecmascript.ESContinue()
    assert completion.completion == ecmascript.Completion(
        ecmascript.CompletionType.CONTINUE, ecmascript.Empty.EMPTY, ecmascript.Empty.EMPTY
    )


def test_ESReturn_01(realm):
    completion = ecmascript.ESReturn(value=88)
    assert completion.completion == ecmascript.Completion(
        ecmascript.CompletionType.RETURN, 88, ecmascript.Empty.EMPTY
    )

import pytest

import ecmascript

def test_CreateReferenceError_01(realm):
    re = ecmascript.CreateReferenceError('test reference error')

    assert ecmascript.isObject(re)
    assert ecmascript.ToString(re) == 'ReferenceError: test reference error'

def test_CreateTypeError_01(realm):
    re = ecmascript.CreateTypeError('test type error')

    assert ecmascript.isObject(re)
    assert ecmascript.ToString(re) == 'TypeError: test type error'

def test_CreateSyntaxError_01(realm):
    re = ecmascript.CreateSyntaxError('test syntax error')

    assert ecmascript.isObject(re)
    assert ecmascript.ToString(re) == 'SyntaxError: test syntax error'

def test_CreateRangeError_01(realm):
    re = ecmascript.CreateRangeError('test range error')

    assert ecmascript.isObject(re)
    assert ecmascript.ToString(re) == 'RangeError: test range error'

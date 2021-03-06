import pytest
import math

from ecmascript.ecmascript import *


def test_isundefined():
    assert isUndefined(None)
    assert not isUndefined(1)
    assert not isUndefined("yogurt")
    assert not isUndefined(JSNull.NULL)
    assert not isUndefined(False)
    assert not isUndefined(True)
    assert not isUndefined(JSSymbol("new symbol"))
    assert not isUndefined(wks_match)
    assert not isUndefined(JSObject())
    assert not isUndefined([])
    assert not isUndefined({})


def test_isnull():
    assert not isNull(None)
    assert not isNull(1)
    assert not isNull("yogurt")
    assert isNull(JSNull.NULL)
    assert not isNull(False)
    assert not isNull(True)
    assert not isNull(JSSymbol("new symbol"))
    assert not isNull(wks_match)
    assert not isNull(JSObject())
    assert not isNull([])
    assert not isNull({})


def test_isboolean():
    assert not isBoolean(None)
    assert not isBoolean(1)
    assert not isBoolean("yogurt")
    assert not isBoolean(JSNull.NULL)
    assert isBoolean(False)
    assert isBoolean(True)
    assert not isBoolean(JSSymbol("new symbol"))
    assert not isBoolean(wks_match)
    assert not isBoolean(JSObject())
    assert not isBoolean([])
    assert not isBoolean({})


def test_isstring():
    assert not isString(None)
    assert not isString(1)
    assert isString("yogurt")
    assert not isString(JSNull.NULL)
    assert not isString(False)
    assert not isString(True)
    assert not isString(JSSymbol("new symbol"))
    assert not isString(wks_match)
    assert not isString(JSObject())
    assert not isString([])
    assert not isString({})


def test_issymbol():
    assert not isSymbol(None)
    assert not isSymbol(1)
    assert not isSymbol("yogurt")
    assert not isSymbol(JSNull.NULL)
    assert not isSymbol(False)
    assert not isSymbol(True)
    assert isSymbol(JSSymbol("new symbol"))
    assert isSymbol(wks_match)
    assert not isSymbol(JSObject())
    assert not isSymbol([])
    assert not isSymbol({})


def test_isnumber():
    assert not isNumber(None)
    assert isNumber(1)
    assert isNumber(600.23)
    assert isNumber(math.nan)
    assert isNumber(math.inf)
    assert not isNumber("yogurt")
    assert not isNumber(JSNull.NULL)
    assert not isNumber(False)
    assert not isNumber(True)
    assert not isNumber(JSSymbol("new symbol"))
    assert not isNumber(wks_match)
    assert not isNumber(JSObject())
    assert not isNumber([])
    assert not isNumber({})


def test_isobject():
    assert not isObject(None)
    assert not isObject(1)
    assert not isObject(600.23)
    assert not isObject(math.nan)
    assert not isObject(math.inf)
    assert not isObject("yogurt")
    assert not isObject(JSNull.NULL)
    assert not isObject(False)
    assert not isObject(True)
    assert not isObject(JSSymbol("new symbol"))
    assert not isObject(wks_match)
    assert isObject(JSObject())
    assert not isObject([])
    assert not isObject({})


def test_isecmavalue():
    assert isEcmaValue(None)
    assert isEcmaValue(1)
    assert isEcmaValue(600.23)
    assert isEcmaValue(math.nan)
    assert isEcmaValue(math.inf)
    assert isEcmaValue("yogurt")
    assert isEcmaValue(JSNull.NULL)
    assert isEcmaValue(False)
    assert isEcmaValue(True)
    assert isEcmaValue(JSSymbol("new symbol"))
    assert isEcmaValue(wks_match)
    assert isEcmaValue(JSObject())
    assert not isEcmaValue([])
    assert not isEcmaValue({})


def test_TypeOf_01():
    assert TypeOf(None) == JSType.UNDEFINED
    assert TypeOf(True) == JSType.BOOLEAN
    assert TypeOf(10) == JSType.NUMBER
    assert TypeOf(JSNull.NULL) == JSType.NULL
    assert TypeOf("test") == JSType.STRING
    assert TypeOf(wks_match) == JSType.SYMBOL
    assert TypeOf(JSObject()) == JSType.OBJECT

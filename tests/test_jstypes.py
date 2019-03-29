import pytest
import math

import jstypes

def test_isundefined():
    assert jstypes.isUndefined(None)
    assert not jstypes.isUndefined(1)
    assert not jstypes.isUndefined('yogurt')
    assert not jstypes.isUndefined(jstypes.JSNull.NULL)
    assert not jstypes.isUndefined(False)
    assert not jstypes.isUndefined(True)
    assert not jstypes.isUndefined(jstypes.JSSymbol('new symbol'))
    assert not jstypes.isUndefined(jstypes.wks_match)
    assert not jstypes.isUndefined(jstypes.JSObject())
    assert not jstypes.isUndefined([])
    assert not jstypes.isUndefined({})

def test_isnull():
    assert not jstypes.isNull(None)
    assert not jstypes.isNull(1)
    assert not jstypes.isNull('yogurt')
    assert jstypes.isNull(jstypes.JSNull.NULL)
    assert not jstypes.isNull(False)
    assert not jstypes.isNull(True)
    assert not jstypes.isNull(jstypes.JSSymbol('new symbol'))
    assert not jstypes.isNull(jstypes.wks_match)
    assert not jstypes.isNull(jstypes.JSObject())
    assert not jstypes.isNull([])
    assert not jstypes.isNull({})

def test_isboolean():
    assert not jstypes.isBoolean(None)
    assert not jstypes.isBoolean(1)
    assert not jstypes.isBoolean('yogurt')
    assert not jstypes.isBoolean(jstypes.JSNull.NULL)
    assert jstypes.isBoolean(False)
    assert jstypes.isBoolean(True)
    assert not jstypes.isBoolean(jstypes.JSSymbol('new symbol'))
    assert not jstypes.isBoolean(jstypes.wks_match)
    assert not jstypes.isBoolean(jstypes.JSObject())
    assert not jstypes.isBoolean([])
    assert not jstypes.isBoolean({})

def test_isstring():
    assert not jstypes.isString(None)
    assert not jstypes.isString(1)
    assert jstypes.isString('yogurt')
    assert not jstypes.isString(jstypes.JSNull.NULL)
    assert not jstypes.isString(False)
    assert not jstypes.isString(True)
    assert not jstypes.isString(jstypes.JSSymbol('new symbol'))
    assert not jstypes.isString(jstypes.wks_match)
    assert not jstypes.isString(jstypes.JSObject())
    assert not jstypes.isString([])
    assert not jstypes.isString({})

def test_issymbol():
    assert not jstypes.isSymbol(None)
    assert not jstypes.isSymbol(1)
    assert not jstypes.isSymbol('yogurt')
    assert not jstypes.isSymbol(jstypes.JSNull.NULL)
    assert not jstypes.isSymbol(False)
    assert not jstypes.isSymbol(True)
    assert jstypes.isSymbol(jstypes.JSSymbol('new symbol'))
    assert jstypes.isSymbol(jstypes.wks_match)
    assert not jstypes.isSymbol(jstypes.JSObject())
    assert not jstypes.isSymbol([])
    assert not jstypes.isSymbol({})

def test_isnumber():
    assert not jstypes.isNumber(None)
    assert jstypes.isNumber(1)
    assert jstypes.isNumber(600.23)
    assert jstypes.isNumber(math.nan)
    assert jstypes.isNumber(math.inf)
    assert not jstypes.isNumber('yogurt')
    assert not jstypes.isNumber(jstypes.JSNull.NULL)
    assert not jstypes.isNumber(False)
    assert not jstypes.isNumber(True)
    assert not jstypes.isNumber(jstypes.JSSymbol('new symbol'))
    assert not jstypes.isNumber(jstypes.wks_match)
    assert not jstypes.isNumber(jstypes.JSObject())
    assert not jstypes.isNumber([])
    assert not jstypes.isNumber({})

def test_isobject():
    assert not jstypes.isObject(None)
    assert not jstypes.isObject(1)
    assert not jstypes.isObject(600.23)
    assert not jstypes.isObject(math.nan)
    assert not jstypes.isObject(math.inf)
    assert not jstypes.isObject('yogurt')
    assert not jstypes.isObject(jstypes.JSNull.NULL)
    assert not jstypes.isObject(False)
    assert not jstypes.isObject(True)
    assert not jstypes.isObject(jstypes.JSSymbol('new symbol'))
    assert not jstypes.isObject(jstypes.wks_match)
    assert jstypes.isObject(jstypes.JSObject())
    assert not jstypes.isObject([])
    assert not jstypes.isObject({})

def test_isecmavalue():
    assert jstypes.isEcmaValue(None)
    assert jstypes.isEcmaValue(1)
    assert jstypes.isEcmaValue(600.23)
    assert jstypes.isEcmaValue(math.nan)
    assert jstypes.isEcmaValue(math.inf)
    assert jstypes.isEcmaValue('yogurt')
    assert jstypes.isEcmaValue(jstypes.JSNull.NULL)
    assert jstypes.isEcmaValue(False)
    assert jstypes.isEcmaValue(True)
    assert jstypes.isEcmaValue(jstypes.JSSymbol('new symbol'))
    assert jstypes.isEcmaValue(jstypes.wks_match)
    assert jstypes.isEcmaValue(jstypes.JSObject())
    assert not jstypes.isEcmaValue([])
    assert not jstypes.isEcmaValue({})

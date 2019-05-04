import pytest

import ecmascript as e
NORMAL = e.CompletionType.NORMAL
THROW = e.CompletionType.THROW

def test_StringObjectInit_01(realm):
    proto = realm.intrinsics['%ObjectPrototype%']

    str = e.StringObject('test value', proto)

    assert e.isObject(str)
    assert e.nc(str.IsExtensible())
    assert e.nc(str.GetPrototypeOf()) == proto
    assert f'{str!r}' == "String('test value')"

def test_StringCreate_01(realm):
    proto = realm.intrinsics['%ObjectPrototype%']

    res = e.StringCreate('test value', proto)

    assert isinstance(res, e.Completion)
    assert res.ctype == NORMAL
    assert res.target is None
    assert e.isObject(res.value)
    assert isinstance(res.value, e.StringObject)
    assert hasattr(res.value, 'StringData')
    assert res.value.StringData == 'test value'

@pytest.mark.parametrize('inp', [e.wks_search, '000003', '23.5', '-0', '-10', '100000'])
def test_StringGetOwnProperty_01(realm, inp):
    # All the many ways this returns None...
    proto = realm.intrinsics['%ObjectPrototype%']
    str = e.nc(e.StringCreate('test_value', proto))

    desc = e.StringGetOwnProperty(str, inp)
    assert desc == e.Completion(NORMAL, None, None)

def test_StringGetOwnProperty_02(realm):
    # Check the props for the character positions
    proto = realm.intrinsics['%ObjectPrototype%']
    test_str = 'I am a test string.'
    str = e.nc(e.StringCreate(test_str, proto))

    for idx, ch in enumerate(test_str):
        desc = e.StringGetOwnProperty(str, e.nc(e.ToString(idx)))
        result = (desc.ctype, desc.value.value, desc.value.writable, desc.value.enumerable, desc.value.configurable, desc.target)
        expected = (NORMAL, ch, False, True, False, None)
        assert result == expected

@pytest.mark.parametrize('inp, expected', [
    ('length', 10),
    ('2', 's'),
    ('nonsense', None),
])
def test_StringGetOwnPropertyMethod_01(realm, inp, expected):
    proto = realm.intrinsics['%ObjectPrototype%']
    str = e.StringObject('test value', proto)

    res = str.GetOwnProperty(inp)
    assert isinstance(res, e.Completion)

    if expected is None:
        assert res == e.Completion(NORMAL, None, None)
    else:
        assert res.ctype == NORMAL
        assert res.target is None
        assert isinstance(res.value, e.PropertyDescriptor)
        assert res.value.value == expected

@pytest.mark.parametrize('inp, expected', [
    ('', ['length']),
    ('abcd', ['0', '1', '2', '3', 'length'])
])
def test_StringOwnPropertyKeysMethod_01(realm, inp, expected):
    proto = realm.intrinsics['%ObjectPrototype%']
    str = e.StringObject(inp, proto)

    lst = str.OwnPropertyKeys()
    assert lst == expected

def test_StringOwnPropertyKeysMethod_02(realm):
    proto = realm.intrinsics['%ObjectPrototype%']
    str = e.StringObject('abcd', proto)

    e.Set(str, '45', 'u', False)
    e.Set(str, '20', 'twenty', False)
    e.Set(str, e.wks_search, 'yogurt', False)
    e.Set(str, 'zebra', 'animal', False)
    e.Set(str, 'antelope', 'grazer', False)

    lst = str.OwnPropertyKeys()
    assert lst == ['0', '1', '2', '3', '20', '45', 'length', 'zebra', 'antelope', e.wks_search]

def test_CreateStringConstructor_01(realm):
    # So the string constructor should already be in the realm's intrinsics list. No real need to actually
    # call the function here.
    sc = realm.intrinsics['%String%']

    # Check the prototype property. (Not the [[Prototype]] property.)
    p = e.nc(sc.GetOwnProperty('prototype'))
    assert p.value == realm.intrinsics['%StringPrototype%']
    assert p.writable == False
    assert p.enumerable == False
    assert p.configurable == False

@pytest.mark.parametrize('fname, fcn, length', [
    ('fromCharCode', 'String_fromCharCode', 1),
    ('fromCodePoint', 'String_fromCodePoint', 1),
    pytest.param('raw', 'String_raw', 1, marks=pytest.mark.xfail), # Not gonna bother with "raw" until templates go in
])
def test_CreateStringConstructor_02(realm, fname, fcn, length):
    sc = realm.intrinsics['%String%']
    # Check for the constructor functions
    p = e.nc(sc.GetOwnProperty(fname))
    assert p is not None
    assert p.configurable == True
    assert p.writable == True
    assert p.enumerable == False
    assert isinstance(p.value, e.BuiltinFunction)
    assert p.value.steps == getattr(e, fcn)
    assert e.nc(e.Get(p.value, 'length')) == length

def test_CreateStringConstructor_03(realm):
    # The constructor's [[Prototype]] object should be %FunctionPrototype%
    sc = realm.intrinsics['%String%']
    assert sc.Prototype == realm.intrinsics['%FunctionPrototype%']

# 21.1.2.1 String.fromCharCode ( ...codeUnits )
def test_String_fromCharCode_01(realm):
    # Calling fromCharCode with no args should return the empty string
    res = e.String_fromCharCode(None, None)
    assert res == e.Completion(NORMAL, '', None)

def test_String_fromCharCode_02(realm):
    # Calling fromCharCode with actual args should give us back a string
    res = e.String_fromCharCode(None, None, 65, 66, 67, 68)
    assert res == e.Completion(NORMAL, 'ABCD', None)

def test_String_fromCharCode_03(realm):
    res = e.String_fromCharCode(None, None, 65, e.wks_search, 66)
    assert isinstance(res, e.Completion)
    assert res.ctype == THROW
    assert res.target is None
    assert e.nc(e.ToString(res.value)).startswith('TypeError: ')

# 21.1.2.2 String.fromCodePoint ( ...codePoints )
def test_String_fromCodePoint_01(realm):
    # Calling with no args gets the empty string
    res = e.String_fromCodePoint(None, None)
    assert res == e.Completion(NORMAL, '', None)

def test_String_fromCodePoint_02(realm):
    # Actual args gets a string with content, and might get UTF-16 encoded pairs
    res = e.String_fromCodePoint(None, None, 65, 0x10232, 67, 68)

    assert res == e.Completion(NORMAL, 'A\ud800\ude32CD', None)

@pytest.mark.parametrize('val, errortype', [
    (e.wks_search, 'TypeError'),
    (65.5, 'RangeError'),
    (-22, 'RangeError'),
    (0x31234135123, 'RangeError'),
])
def test_String_fromCodePoint_04(realm, val, errortype):
    # Error condition 2: code point isn't an integer
    res = e.String_fromCodePoint(None, None, val)
    assert isinstance(res, e.Completion)
    assert res.ctype == THROW
    assert res.target is None
    assert e.nc(e.ToString(res.value)).startswith(f'{errortype}: ')

# 21.1.3 Properties of the String Prototype Object
def test_CreateStringPrototype_01(realm):
    sp = realm.intrinsics['%StringPrototype%']

    assert isinstance(sp, e.StringObject)
    assert hasattr(sp, 'StringData')
    assert sp.StringData == ''
    assert sp.Prototype == realm.intrinsics['%ObjectPrototype%']

def test_CreateStringPrototype_02(realm):
    sp = realm.intrinsics['%StringPrototype%']
    desc = e.nc(sp.GetOwnProperty('length'))
    assert (desc.value, desc.writable, desc.enumerable, desc.configurable) == (0, False, False, False)

def test_CreateStringPrototype_03(realm):
    sp = realm.intrinsics['%StringPrototype%']
    desc = e.nc(sp.GetOwnProperty('constructor'))
    assert (desc.value, desc.writable, desc.enumerable, desc.configurable) == (realm.intrinsics['%String%'], True, False, True)

@pytest.mark.parametrize('fname, fcn, length', [
    pytest.param('charAt', 'StringPrototype_charAt', 1, marks=pytest.mark.xfail),
    pytest.param('charCodeAt', 'StringPrototype_charCodeAt', 1, marks=pytest.mark.xfail),
    pytest.param('codePointAt', 'StringPrototype_codePointAt', 1, marks=pytest.mark.xfail),
    pytest.param('concat', 'StringPrototype_concat', 1, marks=pytest.mark.xfail),
    pytest.param('endsWith', 'StringPrototype_endsWith', 1, marks=pytest.mark.xfail),
    pytest.param('includes', 'StringPrototype_includes', 1, marks=pytest.mark.xfail),
    pytest.param('indexOf', 'StringPrototype_indexOf', 1, marks=pytest.mark.xfail),
    pytest.param('lastIndexOf', 'StringPrototype_lastIndexOf', 1, marks=pytest.mark.xfail),
    pytest.param('localeCompare', 'StringPrototype_localeCompare', 1, marks=pytest.mark.xfail),
    pytest.param('match', 'StringPrototype_match', 1, marks=pytest.mark.xfail),
    pytest.param('normalize', 'StringPrototype_normalize', 0, marks=pytest.mark.xfail),
    pytest.param('padEnd', 'StringPrototype_padEnd', 1, marks=pytest.mark.xfail),
    pytest.param('padStart', 'StringPrototype_padStart', 1, marks=pytest.mark.xfail),
    pytest.param('repeat', 'StringPrototype_repeat', 1, marks=pytest.mark.xfail),
    pytest.param('replace', 'StringPrototype_replace', 2, marks=pytest.mark.xfail),
    pytest.param('search', 'StringPrototype_search', 1, marks=pytest.mark.xfail),
    pytest.param('slice', 'StringPrototype_slice', 2, marks=pytest.mark.xfail),
    pytest.param('split', 'StringPrototype_split', 2, marks=pytest.mark.xfail),
    pytest.param('startsWith', 'StringPrototype_startsWith', 1, marks=pytest.mark.xfail),
    pytest.param('substring', 'StringPrototype_substring', 2, marks=pytest.mark.xfail),
    pytest.param('toLocaleLowerCase', 'StringPrototype_toLocaleLowerCase', 0, marks=pytest.mark.xfail),
    pytest.param('toLocaleUpperCase', 'StringPrototype_toLocaleUpperCase', 0, marks=pytest.mark.xfail),
    pytest.param('toLowerCase', 'StringPrototype_toLowerCase', 0, marks=pytest.mark.xfail),
    ('toString', 'StringPrototype_toString', 0),
    pytest.param('toUpperCase', 'StringPrototype_toUpperCase', 0, marks=pytest.mark.xfail),
    pytest.param('trim', 'StringPrototype_trim', 0, marks=pytest.mark.xfail),
    ('valueOf', 'StringPrototype_valueOf', 0),
    pytest.param(e.wks_iterator, 'StringPrototype_iterator', 0, marks=pytest.mark.xfail),
])
def test_CreateStringPrototype_04(realm, fname, fcn, length):
    sp = realm.intrinsics['%StringPrototype%']
    # Check for the prototype functions
    p = e.nc(sp.GetOwnProperty(fname))
    assert p is not None
    assert p.configurable == True
    assert p.writable == True
    assert p.enumerable == False
    assert isinstance(p.value, e.BuiltinFunction)
    assert p.value.steps == getattr(e, fcn)
    assert e.nc(e.Get(p.value, 'length')) == length

def test_thisStringValue_01(realm):
    # String values just come back
    res = e.thisStringValue('just come back')
    assert res == e.Completion(NORMAL, 'just come back', None)

def test_thisStringValue_02(realm):
    # String objects just hand over their string data.
    mystring = e.nc(e.StringCreate('object string', realm.intrinsics['%StringPrototype%']))
    res = e.thisStringValue(mystring)
    assert res == e.Completion(NORMAL, 'object string', None)

def test_thisStringValue_03(realm):
    # Non-string values throw a type error
    res = e.thisStringValue(67)
    assert isinstance(res, e.Completion)
    assert res.ctype == THROW
    assert res.target is None
    assert e.nc(e.ToString(res.value)).startswith('TypeError: ')

def test_thisStringValue_04(realm):
    # Non-string object throws a type errror
    obj = e.ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    res = e.thisStringValue(obj)
    assert isinstance(res, e.Completion)
    assert res.ctype == THROW
    assert res.target is None
    assert e.nc(e.ToString(res.value)).startswith('TypeError: ')
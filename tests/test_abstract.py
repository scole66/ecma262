import pytest
import math

import ecmascript # Need it this way for the mocker fixture
from ecmascript import *

NORMAL = CompletionType.NORMAL
THROW = CompletionType.THROW

@pytest.fixture
def realm():
    InitializeHostDefinedRealm()
    yield surrounding_agent.running_ec.realm
    surrounding_agent.ec_stack.pop()
    surrounding_agent.running_ec = None

@pytest.fixture
def obj(realm):
    return ObjectCreate(realm.intrinsics['%ObjectPrototype%'])

@pytest.fixture
def some_objects(realm):
    # A plain object.
    plain = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    # An object with a [[Get]] method that throws an exception with the value 'I am EvilGet'
    evil_get = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    evil_get.Get = types.MethodType(lambda _a, _b, _c: ThrowCompletion('I am EvilGet'), 'Get')
    # An object with a "toString" method that throws an exception with the value 'I am evil tostring'
    evil_tostring = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    CreateMethodPropertyOrThrow(evil_tostring, 'toString',
                                CreateBuiltinFunction(lambda _a, _b: ThrowCompletion('I am evil tostring'),
                                                      [], realm, JSNull.NULL))
    # An object whose "toString" and "toValue" methods both produce objects.
    bad_primitives = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    objfunc = CreateBuiltinFunction(
        lambda _a, _b: NormalCompletion(ObjectCreate(realm.intrinsics['%ObjectPrototype%'])),
        [], realm, JSNull.NULL)
    CreateMethodPropertyOrThrow(bad_primitives, 'toString', objfunc)
    CreateMethodPropertyOrThrow(bad_primitives, 'toValue', objfunc)
    # An object whose "toString" method returns 'You found the treasure', and whose 'valueOf' method returns 42
    treasure = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    objfunc = CreateBuiltinFunction(lambda _a, _b: NormalCompletion('You found the treasure'), [], realm,
                                    JSNull.NULL)
    CreateMethodPropertyOrThrow(treasure, 'toString', objfunc)
    objfunc = CreateBuiltinFunction(lambda _a, _b: NormalCompletion(42), [], realm, JSNull.NULL)
    CreateMethodPropertyOrThrow(treasure, 'valueOf', objfunc)

    return {
        'plain': plain,
        'evil_get': evil_get,
        'evil_tostring': evil_tostring,
        'bad_primitives': bad_primitives,
        'treasure': treasure
        }

@pytest.mark.parametrize('objname, cnvtype, result_val, result_type', [
    ('plain', 'string', '[object Object]', NORMAL),
    ('plain', 'number', '[object Object]', NORMAL),
    ('evil_get', 'string', 'I am EvilGet', THROW),
    ('evil_tostring', 'string', 'I am evil tostring', THROW),
    ('bad_primitives', 'string', TypeError(), THROW),
    ('treasure', 'string', 'You found the treasure', NORMAL),
    ('treasure', 'number', 42, NORMAL)
])
def test_OrdinaryToPrimitive(some_objects, objname, cnvtype, result_val, result_type):
    cr = OrdinaryToPrimitive(some_objects[objname], cnvtype)
    if result_type == THROW and isinstance(result_val, TypeError):
        assert isinstance(cr, Completion)
        assert cr.ctype == THROW
        assert isinstance(cr.value, TypeError)
        assert cr.target is None
    else:
        assert cr == Completion(ctype=result_type, value=result_val, target=None)

def test_ToPrimitive_notobj():
    # Need the object inputs, in the future!!
    cr = ToPrimitive('string')
    assert cr == Completion(ctype=NORMAL, value='string', target=None)

@pytest.mark.parametrize('objname, cnvtype, result_val, result_type',
[
    ('plain', 'string', '[object Object]', NORMAL),
    ('plain', 'number', '[object Object]', NORMAL),
    ('evil_get', 'string', 'I am EvilGet', THROW),
    ('evil_tostring', 'string', 'I am evil tostring', THROW),
    ('bad_primitives', 'string', TypeError(), THROW),
    ('treasure', 'string', 'You found the treasure', NORMAL),
    ('treasure', 'number', 42, NORMAL),
    ('treasure', 'default', 42, NORMAL)
])
def test_ToPrimitive(some_objects, objname, cnvtype, result_val, result_type):
    cr = ToPrimitive(some_objects[objname], cnvtype)
    if result_type == THROW and isinstance(result_val, TypeError):
        assert isinstance(cr, Completion)
        assert cr.ctype == THROW
        assert isinstance(cr.value, TypeError)
        assert cr.target is None
    else:
        assert cr == Completion(ctype=result_type, value=result_val, target=None)

def test_ToPrimitive_GetMethodThrows(realm):
    # If an object has a @@toPrimitive property, the ToPrimitive routine tries to use as a conversion method.
    # But if it can't actually be used as a method, we'll get an exception instead.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    CreateDataProperty(obj, wks_to_primitive, 100)

    cr = ToPrimitive(obj, 'number')
    assert isinstance(cr, Completion)
    assert cr.ctype == THROW
    assert isinstance(cr.value, TypeError)
    assert cr.target is None

@pytest.mark.parametrize('input,expected', [('number', 'I was passed number.'), ('string', 'I was passed string.')])
def test_ToPrimitive_exotictoprim(realm, input, expected):
    # If an object has a @@toPrimitive method, ToPrimitive should use it.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    def exotic_to_primitive(self, new_target, hint):
        return NormalCompletion('I was passed %s.' % hint)
    CreateMethodProperty(obj, wks_to_primitive, CreateBuiltinFunction(exotic_to_primitive, [], realm))

    cr = ToPrimitive(obj, input)
    assert cr == Completion(NORMAL, expected, None)

def test_ToPrimitive_exoticthrows(realm):
    # If an object's @@toPrimitive throws an error, it's not ignored.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    def exotic_to_primitive(self, new_target, hint):
        return ThrowCompletion('I am evil.')
    CreateMethodProperty(obj, wks_to_primitive, CreateBuiltinFunction(exotic_to_primitive, [], realm))

    cr = ToPrimitive(obj, 'string')
    assert cr == Completion(THROW, 'I am evil.', None)

def test_ToPrimitive_exoticreturnsobj(realm):
    # If an object's @@toPrimitive returns an Object, ToPrimitive throws a TypeError
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    def exotic_to_primitive(self, new_target, hint):
        return NormalCompletion(obj)
    CreateMethodProperty(obj, wks_to_primitive, CreateBuiltinFunction(exotic_to_primitive, [], realm))

    cr = ToPrimitive(obj, 'string')
    assert isinstance(cr, Completion)
    assert cr.ctype == THROW
    assert isinstance(cr.value, TypeError)
    assert cr.target is None

@pytest.mark.parametrize('input,expected', [
    (None, False),
    (True, True),
    (False, False),
    (JSNull.NULL, False),
    (math.nan, False),
    (math.inf, True),
    (0, False),
    (-0.0, False),
    (10, True),
    (-10, True),
    ('', False),
    ('yugoloth', True),
    (wks_to_primitive, True)
    ])
def test_to_boolean(input, expected):
    cr = ToBoolean(input)
    assert cr == expected

@pytest.mark.parametrize('input,expected', [
    (None, math.nan),
    (JSNull.NULL, 0),
    (True, 1),
    (False, 0),
    (67, 67),
    ('0', 0),
    ('1', 1),
    ('0b101', 5),
    ('0XAB', 0xab),
    ('0O773', 0o773),
    ('-10', -10),
    ('+10.5', 10.5),
    ('10.e3', 10000),
    ('1e+3', 1000),
    ('.9e-3', 0.0009),
    ('10.5e-2', 0.105),
    ('6.', 6),
    ('.5', 0.5),
    ('+Infinity', math.inf),
    ('-Infinity', -math.inf),
    ('Infinity', math.inf),
    ('inf', math.nan),
    ('\n\n56\t \r', 56),
    ('0x'+'75789240758907289067984302789758920789089729803478592078975819920375801'*4, math.inf),
    ('0b'+'1'*1025, math.inf),
    ('0o'+'7'*342, math.inf),
    ('100e4000', math.inf),
    ('-5e4000', -math.inf),
    ('5e-4000', 0)
    ])
def test_ToNumber_01(input, expected):
    cr = ToNumber(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)

@pytest.mark.parametrize('input,expected', [
    ('-0', '-0.0'),
    ('-.0', '-0.0'),
    ('-0.0', '-0.0')])
def test_ToNumber_negative_zero(input, expected):
    cr = ToNumber(input)
    cr2 = Completion(ctype=cr.ctype, value=str(cr.value), target=cr.target)
    assert cr2 == Completion(ctype=NORMAL, value=expected, target=None)

def test_ToNumber_symbol():
    cr = ToNumber(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

def test_ToNumber_04(obj):
    # Success: Calls ToPrimitive on the object, then ToNumber on the primitive
    def valueOf(self, new_target):
        return NormalCompletion(76)
    fcn = CreateBuiltinFunction(valueOf, [])
    CreateMethodPropertyOrThrow(obj, 'valueOf', fcn)

    res = ToNumber(obj)
    assert res == Completion(NORMAL, 76, None)

def test_ToNumber_05(obj, mocker):
    # When ToPrimitive throws an error
    mocker.patch('ecmascript.ToPrimitive', return_value=Completion(THROW, 'throw test', None))

    res = ToNumber(obj)
    assert res == Completion(THROW, 'throw test', None)

def test_ToNumber_06(obj):
    # When ToNumber throws an error in the recursive call when getting the value of an object.
    def valueOf(self, new_target):
        return NormalCompletion(wks_to_primitive)
    fcn = CreateBuiltinFunction(valueOf, [])
    CreateMethodPropertyOrThrow(obj, 'valueOf', fcn)

    res = ToNumber(obj)
    assert res.ctype == THROW
    assert isinstance(res.value, TypeError)
    assert res.target is None

@pytest.mark.parametrize('input,expected', [
    ('goblin', 0),
    (0, 0),
    (-math.inf, -math.inf),
    (math.inf, math.inf),
    (10.3, 10.0),
    (-23.3, -23.0)])
def test_to_integer(input, expected):
    cr = ToInteger(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)

def test_to_integer_negative_zero():
    cr = ToInteger(-0.0)
    cr2 = Completion(ctype=cr.ctype, value=str(cr.value), target=cr.target)
    assert cr2 == Completion(ctype=NORMAL, value='-0.0', target=None)

def test_to_integer_symbol():
    cr = ToInteger(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    (math.nan, 0),
    (0, 0),
    (math.inf, 0),
    (-math.inf, 0),
    (0x1234567890, 0x34567890),
    (0xb4567890, -0x4ba98770),
    (367, 367),
    (-56, -56)])
def test_to_int32(input, expected):
    cr = ToInt32(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)
    assert cr.value == ToInt32(cr.value).value  # Idempotentcy check

def test_to_int32_neg_zero():
    cr = ToInt32(-0.0)
    cr2 = Completion(ctype=cr.ctype, value=str(cr.value), target=cr.target)
    assert cr2 == Completion(ctype=NORMAL, value='0', target=None)

def test_to_int32_symbol():
    cr = ToInt32(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    (math.nan, 0),
    (0, 0),
    (math.inf, 0),
    (-math.inf, 0),
    (0x1234567890, 0x34567890),
    (0xb4567890, 0xb4567890),
    (367, 367),
    (-56, 0xffffffc8)])
def test_to_uint32(input, expected):
    cr = ToUint32(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)

def test_to_uint32_symbol():
    cr = ToUint32(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    (math.nan, 0),
    (0, 0),
    (math.inf, 0),
    (-math.inf, 0),
    (0x1234567890, 0x7890),
    (0xf890, -0x770),
    (367, 367),
    (-56, -56)])
def test_to_int16(input, expected):
    cr = ToInt16(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)
def test_to_int16_symbol():
    cr = ToInt16(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    (math.nan, 0),
    (0, 0),
    (math.inf, 0),
    (-math.inf, 0),
    (0x1234567890, 0x7890),
    (0xb456f890, 0xf890),
    (367, 367),
    (-56, 0xffc8)])
def test_to_uint16(input, expected):
    cr = ToUint16(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)

def test_to_uint16_symbol():
    cr = ToUint16(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    (math.nan, 0),
    (0, 0),
    (math.inf, 0),
    (-math.inf, 0),
    (0x12345678, 0x78),
    (0x90, -0x70),
    (67, 67),
    (-56, -56)])
def test_to_int8(input, expected):
    cr = ToInt8(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)
def test_to_int8_symbol():
    cr = ToInt8(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    (math.nan, 0),
    (0, 0),
    (math.inf, 0),
    (-math.inf, 0),
    (0x12345678, 0x78),
    (0xb456f890, 0x90),
    (67, 67),
    (-56, 0xc8)])
def test_to_uint8(input, expected):
    cr = ToUint8(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)

def test_to_uint8_symbol():
    cr = ToUint8(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    ('goblin', 0),
    ('-0.0', 0),
    (903, 255),
    (-3, 0),
    (math.inf, 255),
    ('-Infinity', 0),
    (20, 20),
    (19.2, 19),
    (19.7, 20),
    (19.5, 20),
    (18.5, 18)])
def test_to_uint8_clamp(input, expected):
    cr = ToUint8Clamp(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)
def test_to_uint8_clamp_symbol():
    cr = ToUint8Clamp(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    (None, 'undefined'),
    (JSNull.NULL, 'null'),
    (True, 'true'),
    (False, 'false'),
    ('green', 'green'),
    (3.25, '3.25'),
    (math.inf, 'Infinity'),
    (-math.inf, '-Infinity'),
    (math.nan, 'NaN'),
    (-0.0, '0'),
    (1, '1'),
    (1.0, '1'),
    (-1, '-1'),
    (-1.0, '-1'),
    (10.5, '10.5')
])
def test_ToString_01(input, expected):
    cr = ToString(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)
def test_ToString_symbol():
    cr = ToString(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)
def test_ToString_03(mocker, obj):
    # Input is an object, and ToPrimitive throws.
    mocker.patch('ecmascript.ToPrimitive', side_effect=lambda a, b: ThrowCompletion('test throw'))
    res = ToString(obj)
    assert res == Completion(THROW, 'test throw', None)
def test_ToString_04(obj):
    # Input is an object, and it has a valid toString method.
    fcn = CreateBuiltinFunction(lambda thisvalue, newtarget: 'test object string', [])
    CreateMethodProperty(obj, 'toString', fcn)
    res = ToString(obj)
    assert res == Completion(NORMAL, 'test object string', None)

@pytest.mark.parametrize('arg,expected', [(math.nan, 'NaN'),
    (0.0, '0'),
    (-0.0, '0'),
    (-3, '-3'),
    (math.inf, 'Infinity'),
    (21000, '21000'),
    (500000000000000000000, '500000000000000000000'),
    (5.25,'5.25'),
    (0.00390625, '0.00390625'),
    (5e21, '5e+21'),
    pytest.param(2**-19, '0.0000019073486328125', marks=pytest.mark.xfail),
    pytest.param(2**-20, '9.5367431640625e-7', marks=pytest.mark.xfail),
    ])
def test_NumberToString_01(arg, expected):
   # Looking at the steps in the spec:
   # 1. NaN --> 'NaN'
   # 2a. +0 --> '0'
   # 2b. -0 --> '0'
   # 3. -3 --> result starts with exactly one '-'
   # 4. Infinity --> 'Infinity'
   # 5. Things start to get interesting. (No direct test for step 5.)
   # 6a. m = 21000 ::: k=2; s=21; n=5  (k <= n <= 21)  ==> '21000'
   # 6b. m = 500000000000000000000 ::: k=1; s=5; n=21 ==> '500000000000000000000'
   # 7a. m = 5.25 ::: k=3; s=525; n=1 ==> '5.25'
   # 8. m = 0.00390625 ::: k=6; s=390625; n=-2 ==> '0.00390625'
   # 9. m = 5e21 ::: k=1, s=5, n=22 ==> '5e+21'

   # we fail on 2**-19. Phooey. (The n==-5 case)
   assert NumberToString(arg) == expected

def test_ToObject_01(obj):
    # Object -> Object
    cr = ToObject(obj)
    assert cr == Completion(ctype=NORMAL, value=obj, target=None)

def test_ToObject_02(realm):
    # Boolean -> Object
    res = ToObject(True)
    assert res.ctype == NORMAL
    assert res.target is None
    assert isObject(res.value)
    assert hasattr(res.value, 'BooleanData')
    assert res.value.BooleanData

def test_ToObject_03(realm):
    # Null -> Error
    res = ToObject(JSNull.NULL)
    assert res.ctype == THROW
    assert res.target is None
    assert isinstance(res.value, TypeError)

def test_ToObject_04(realm):
    # Undefined -> Error
    res = ToObject(None)
    assert res.ctype == THROW
    assert res.target is None
    assert isinstance(res.value, TypeError)

def test_ToObject_05(realm):
    # Number -> Object
    res = ToObject(34000)
    assert res.ctype == NORMAL
    assert res.target is None
    assert isObject(res.value)
    assert hasattr(res.value, 'NumberData')
    assert res.value.NumberData == 34000

@pytest.mark.xfail(reason='Needs String Object Support')
def test_ToObject_06(realm):
    # String -> Object
    res = ToObject('tricky')
    assert res.ctype == NORMAL
    assert res.target is None
    assert isObject(res.value)
    assert hasattr(res.value, 'StringData')
    assert res.value.StringData == 'tricky'

@pytest.mark.xfail(reason='Needs Symbol Object Support')
def test_ToObject_07(realm):
    # Symbol -> Object
    res = ToObject(wks_to_primitive)
    assert res.ctype == NORMAL
    assert res.target is None
    assert isObject(res.value)
    assert hasattr(res.value, 'SymbolData')
    assert res.value.SymbooData == wks_to_primitive

@pytest.mark.parametrize('arg,expected', [
    ('1', 1),
    ('Infinity', math.inf),
    ('-3', -3),
    ('1.125', 1.125),
    ('     1.125', None),
    ('0x13', None)])
def test_CanonicalNumericIndexString_(arg, expected):
    assert CanonicalNumericIndexString(arg) == expected
def test_CanonicalNumericIndexString_nan():
    assert math.isnan(CanonicalNumericIndexString('NaN'))
def test_CanonicalNumericIndexString_negative_zero():
    val = CanonicalNumericIndexString('-0')
    assert math.copysign(1.0, val) == -1.0 and val == 0.0


@pytest.mark.parametrize('arg_x,arg_y,expected', [
    (0, '0', False),
    (0.0, 0.0, True),
    (-0.0, 0.0, False),
    (0.0, -0.0, False),
    (-0.0, -0.0, True),
    (math.nan, math.nan, True),
    (13.0, 13.0, True),
    (-12.0, 12.0, False),
    (True, True, True),
    (False, True, False),
    (None, None, True),
    (JSNull.NULL, JSNull.NULL, True),
    ('blue', 'blue', True),
    ('blue', 'green', False),
    (wks_to_primitive, wks_to_primitive, True),
    (wks_to_primitive, wks_match, False)])
def test_SameValue(arg_x, arg_y, expected):
    assert SameValue(arg_x, arg_y) == expected

@pytest.mark.parametrize('arg,expected', [
    ('thing', True),
    ('', True),
    (78, False),
    (True, False),
    (None, False),
    (wks_match, True)])
def test_IsPropertyKey(arg, expected):
    assert IsPropertyKey(arg) == expected

@pytest.mark.parametrize('arg,expected', [
    ('green', False),
    (-0.0, True),
    (math.nan, False),
    (math.inf, False),
    (-math.inf, False),
    (5.0, True),
    (17.5, False),
    (-2391.2, False),
    (-78, True),
    (2**50, True)
])
def test_IsInteger(arg, expected):
    assert IsInteger(arg) == expected

def test_IsCallable_01():
    # A non-object
    res = IsCallable(67)
    assert not res

def test_IsCallable_02(obj):
    # An object without a [[Call]] slot
    res = IsCallable(obj)
    assert not res

def test_IsCallable_03(realm):
    # An object with a [[Call]] slot
    fcn_obj = CreateBuiltinFunction(lambda self, new_target: NormalCompletion(None), [])
    res = IsCallable(fcn_obj)
    assert res


# 7.2.4 IsConstructor ( argument )
# The abstract operation IsConstructor determines if argument, which must be an ECMAScript language value, is a function object
# with a [[Construct]] internal method.
#
# 1. If Type(argument) is not Object, return false.
# 2. If argument has a [[Construct]] internal method, return true.
# 3. Return false.

def test_IsConstructor_01():
    # The result from step 1: A non-object
    res = IsConstructor(35)
    assert not res

def test_IsConstructor_02(obj):
    # The result from step 3: an object with no [[Construct]] method
    res = IsConstructor(obj)
    assert not res

def test_IsConstructor_03(realm):
    # The result from step 2: an object with a [[Construct]] method
    res = IsConstructor(realm.intrinsics['%Boolean%'])
    assert res

# 7.2.5 IsExtensible ( O )
#
# The abstract operation IsExtensible is used to determine whether additional properties can be added to the object
# that is O. A Boolean value is returned. This abstract operation performs the following steps:
#
# 1. Assert: Type(O) is Object.
# 2. Return ? O.[[IsExtensible]]().
def test_IsExtensible_01(obj, mocker):
    # All this is doing is deferring to its object's method. So that's all we should confirm here.
    ie = mocker.Mock(return_value=True)
    obj.IsExtensible = ie
    res = IsExtensible(obj)
    ie.assert_called_once_with()

# 7.1.14 ToPropertyKey ( argument )
# The abstract operation ToPropertyKey converts argument to a value that can be used as a property key by performing
# the following steps:
#
# 1. Let key be ? ToPrimitive(argument, hint String).
# 2. If Type(key) is Symbol, then
#   a. Return key.
# 3. Return ! ToString(key).
def test_ToPropertyKey_01():
    res = ToPropertyKey(69)
    assert res == Completion(NORMAL, '69', None)

def test_ToPropertyKey_02():
    res = ToPropertyKey(wks_to_primitive)
    assert res == Completion(NORMAL, wks_to_primitive, None)

def test_ToPropertyKey_03(mocker):
    mocker.patch('ecmascript.ToPrimitive', return_value=Completion(THROW, 'throw test', None))
    res = ToPropertyKey('a')
    assert res == Completion(THROW, 'throw test', None)

# 7.2.1 RequireObjectCoercible ( argument )
#
# The abstract operation RequireObjectCoercible throws an error if argument is a value that cannot be converted to an
# Object using ToObject. It is defined by Table 13:
#
# Table 13: RequireObjectCoercible Results
# +---------------+-------------------------------
# | Argument Type | Result
# +---------------+-------------------------------
# | Undefined     | Throw a TypeError exception.
# | Null          | Throw a TypeError exception.
# | Boolean       | Return argument.
# | Number        | Return argument.
# | String        | Return argument.
# | Symbol        | Return argument.
# | Object        | Return argument.
# +---------------+-------------------------------
@pytest.mark.parametrize('inp', [None, JSNull.NULL])
def test_RequireObjectCoercible_01(inp):
    # The "I throw a type error exception" case.
    res = RequireObjectCoercible(inp)
    assert res.ctype == THROW
    assert res.target is None
    assert isinstance(res.value, TypeError)

@pytest.mark.parametrize('inp', [True, 10, 'flower', wks_to_primitive])
def test_RequireObjectCoercible_02(inp):
    # Boolean, Number, String, and Symbol values
    res = RequireObjectCoercible(inp)
    assert res == Completion(NORMAL, inp, None)

def test_RequireObjectCoercible_03(obj):
    # An Object value
    res = RequireObjectCoercible(obj)
    assert res == Completion(NORMAL, obj, None)

# 7.2.2 IsArray ( argument )
#
# The abstract operation IsArray takes one argument argument, and performs the following steps:
#
# 1. If Type(argument) is not Object, return false.
# 2. If argument is an Array exotic object, return true.
# 3. If argument is a Proxy exotic object, then
#    a. If argument.[[ProxyHandler]] is null, throw a TypeError exception.
#    b. Let target be argument.[[ProxyTarget]].
#    c. Return ? IsArray(target).
# 4. Return false.
def test_IsArray_01(realm):
    # arg isn't an object.
    res = IsArray('foo')
    assert res == Completion(NORMAL, False, None)

def test_IsArray_02(realm):
    # arg is an array.
    arg = nc(ArrayCreate(10, realm.intrinsics['%ObjectPrototype%']))
    res = IsArray(arg)
    assert res == Completion(NORMAL, True, None)

def test_IsArray_03(realm):
    # arg is a busted proxy object.
    arg = ProxyObject()
    arg.ProxyHandler = JSNull.NULL
    res = IsArray(arg)
    assert res.ctype == THROW
    assert res.target is None
    assert isinstance(res.value, TypeError)

def test_IsArray_04(realm):
    # arg is a proxy object with a proxy target that's an array.
    arg = ProxyObject()
    arg.ProxyHandler = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    arg.ProxyTarget = nc(ArrayCreate(10, realm.intrinsics['%ObjectPrototype%']))
    res = IsArray(arg)
    assert res == Completion(NORMAL, True, None)

def test_IsArray_05(realm):
    # arg is a proxy object with a proxy target that isn't an array.
    arg = ProxyObject()
    arg.ProxyHandler = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    arg.ProxyTarget = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    res = IsArray(arg)
    assert res == Completion(NORMAL, False, None)

def test_IsArray_06(obj):
    # An object that's not an array
    res = IsArray(obj)
    assert res == Completion(NORMAL, False, None)

# 7.2.13 Abstract Relational Comparison
# The comparison x < y, where x and y are values, produces true, false, or undefined (which indicates that at least one
# operand is NaN). In addition to x and y the algorithm takes a Boolean flag named LeftFirst as a parameter. The flag
# is used to control the order in which operations with potentially visible side-effects are performed upon x and y. It
# is necessary because ECMAScript specifies left to right evaluation of expressions. The default value of LeftFirst is
# true and indicates that the x parameter corresponds to an expression that occurs to the left of the y parameter's
# corresponding expression. If LeftFirst is false, the reverse is the case and operations must be performed upon y
# before x. Such a comparison is performed as follows:
#
# 1. If the LeftFirst flag is true, then
#    a. Let px be ? ToPrimitive(x, hint Number).
#    b. Let py be ? ToPrimitive(y, hint Number).
# 2. Else the order of evaluation needs to be reversed to preserve left to right evaluation,
#    a. Let py be ? ToPrimitive(y, hint Number).
#    b. Let px be ? ToPrimitive(x, hint Number).
# 3. If Type(px) is String and Type(py) is String, then
#    a. If IsStringPrefix(py, px) is true, return false.
#    b. If IsStringPrefix(px, py) is true, return true.
#    c. Let k be the smallest nonnegative integer such that the code unit at index k within px is different from the
#       code unit at index k within py. (There must be such a k, for neither String is a prefix of the other.)
#    d. Let m be the integer that is the numeric value of the code unit at index k within px.
#    e. Let n be the integer that is the numeric value of the code unit at index k within py.
#    f. If m < n, return true. Otherwise, return false.
# 4. Else,
#    a. NOTE: Because px and py are primitive values evaluation order is not important.
#    b. Let nx be ? ToNumber(px).
#    c. Let ny be ? ToNumber(py).
#    d. If nx is NaN, return undefined.
#    e. If ny is NaN, return undefined.
#    f. If nx and ny are the same Number value, return false.
#    g. If nx is +0 and ny is -0, return false.
#    h. If nx is -0 and ny is +0, return false.
#    i. If nx is +∞, return false.
#    j. If ny is +∞, return true.
#    k. If ny is -∞, return false.
#    l. If nx is -∞, return true.
#    m. If the mathematical value of nx is less than the mathematical value of ny—note that these mathematical values
#       are both finite and not both zero—return true. Otherwise, return false.
# NOTE 1
# Step 3 differs from step 7 in the algorithm for the addition operator + (12.8.3) by using the logical-and operation
# instead of the logical-or operation.#
#
# NOTE 2
# The comparison of Strings uses a simple lexicographic ordering on sequences of code unit values. There is no attempt
# to use the more complex, semantically oriented definitions of character or string equality and collating order
# defined in the Unicode specification. Therefore String values that are canonically equal according to the Unicode
# standard could test as unequal. In effect this algorithm assumes that both Strings are already in normalized form.
# Also, note that for strings containing supplementary characters, lexicographic ordering on sequences of UTF-16 code
# unit values differs from that on sequences of code point values.
from unittest.mock import call
def test_AbstractRelationalComparison_01(mocker):
    # LeftFirst True: make sure we evaluate the left arg before the right arg.
    mocker.patch('ecmascript.ToPrimitive', side_effect=lambda a, b: a)
    res = AbstractRelationalComparison(10, 20, True)

    # assert the order
    ecmascript.ToPrimitive.assert_has_calls([call(10, 'number'), call(20, 'number')])
    # and check the result, just because
    assert res == Completion(NORMAL, True, None)

def test_AbstractRelationalComparison_02(mocker):
    # LeftFirst False: make sure we evaluate the right arg before the left arg
    mocker.patch('ecmascript.ToPrimitive', side_effect=lambda a, b: a)
    res = AbstractRelationalComparison(10, 20, False)

    # assert the order
    ecmascript.ToPrimitive.assert_has_calls([call(20, 'number'), call(10, 'number')])
    assert res == Completion(NORMAL, True, None)

@pytest.mark.parametrize('left, right, expected', [
    ('th', 'thing', True), # Left is a substring of Right (3b)
    ('thing', 'th', False), # Right is a substring of Left (3a)
    ('thing', 'thang', False), # 3f: strings where one is not a prefix of the other
    ('thing', 'thzzz', True), # 3f: strings where one is not a prefix of the other
    (math.nan, 67, None), # 4d left is nan
    (67, math.nan, None), # 4e right is nan
    (88, 88, False), # 4f Same number
    (0, -0.0, False), # 4g
    (-0.0, 0, False), # 4h
    (math.inf, math.inf, False), # 4f again
    (math.inf, 888, False), # 4i
    (222, math.inf, True), # 4j
    (987, -math.inf, False), # 4k
    (-math.inf, 8193, True), # 4l
    (100, 1000, True), # 4m
    (1000, 100, False), # 4m
])
def test_AbstractRelationalComparison_03(realm, left, right, expected):
    res = AbstractRelationalComparison(left, right, True)
    assert res == Completion(NORMAL, expected, None)

@pytest.mark.parametrize('left, right, lrf', [
    (20, 10, True),
    (10, 20, True),
    (20, 10, False),
    (10, 20, False),
] )
def test_AbstractRelationalComparison_04(realm, mocker, left, right, lrf):
    # The ToPrimitive error paths
    mocker.patch('ecmascript.ToPrimitive', side_effect=lambda a, b: 10 if a == 10 else ThrowCompletion('err thrown'))
    res = AbstractRelationalComparison(left, right, lrf)
    assert res == Completion(THROW, 'err thrown', None)

@pytest.mark.parametrize('left, right', [
    (10, 20),
    (20, 10),
])
def test_AbstractRelationalComparison_05(realm, mocker, left, right):
    # The ToNumber error paths
    mocker.patch('ecmascript.ToNumber', side_effect=lambda a: 10 if a == 10 else ThrowCompletion('err thrown'))
    res = AbstractRelationalComparison(left, right, True)
    assert res == Completion(THROW, 'err thrown', None)

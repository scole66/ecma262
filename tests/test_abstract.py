import pytest
import math

# pylint: disable=unused-wildcard-import
import ecmascript  # Need it this way for the mocker fixture
from ecmascript import *


@pytest.fixture
def obj(realm):
    return ObjectCreate(realm.intrinsics["%ObjectPrototype%"])


def make_thrower(msg):
    def thrower(*args, **kwargs):
        raise ESTypeError(msg)

    return thrower


@pytest.fixture
def some_objects(realm):
    # A plain object.
    plain = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    # An object with a [[Get]] method that throws an exception with a TypeError and the message 'I am EvilGet'
    evil_get = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    evil_get.Get = types.MethodType(make_thrower("I am EvilGet"), "Get")
    # An object with a "toString" method that throws an exception with the value 'I am evil tostring'

    evil_tostring = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    CreateMethodPropertyOrThrow(
        evil_tostring, "toString", CreateBuiltinFunction(make_thrower("I am evil tostring"), [], realm, JSNull.NULL)
    )
    # An object whose "toString" and "toValue" methods both produce objects.
    bad_primitives = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    objfunc = CreateBuiltinFunction(
        lambda _a, _b: ObjectCreate(realm.intrinsics["%ObjectPrototype%"]), [], realm, JSNull.NULL
    )
    CreateMethodPropertyOrThrow(bad_primitives, "toString", objfunc)
    CreateMethodPropertyOrThrow(bad_primitives, "toValue", objfunc)
    # An object whose "toString" method returns 'You found the treasure', and whose 'valueOf' method returns 42
    treasure = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    objfunc = CreateBuiltinFunction(lambda _a, _b: "You found the treasure", [], realm, JSNull.NULL)
    CreateMethodPropertyOrThrow(treasure, "toString", objfunc)
    objfunc = CreateBuiltinFunction(lambda _a, _b: 42, [], realm, JSNull.NULL)
    CreateMethodPropertyOrThrow(treasure, "valueOf", objfunc)
    # An object whose valueOf method isn't actually a function.
    no_value = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    Set(no_value, "valueOf", 67, False)
    objfunc = CreateBuiltinFunction(lambda _a, _b: "The no_value object", [], realm, JSNull.NULL)
    CreateMethodProperty(no_value, "toString", objfunc)

    return {
        "plain": plain,
        "evil_get": evil_get,
        "evil_tostring": evil_tostring,
        "bad_primitives": bad_primitives,
        "treasure": treasure,
        "no_value": no_value,
    }


@pytest.mark.parametrize(
    "objname, cnvtype, result_val",
    [
        ("plain", "string", "[object Object]"),
        ("plain", "number", "[object Object]"),
        ("treasure", "string", "You found the treasure"),
        ("treasure", "number", 42),
        ("no_value", "number", "The no_value object"),
    ],
)
def test_OrdinaryToPrimitive_01(some_objects, objname, cnvtype, result_val):
    cr = OrdinaryToPrimitive(some_objects[objname], cnvtype)
    assert cr == result_val


@pytest.mark.parametrize(
    "objname, cnvtype, result_val",
    [
        ("evil_get", "string", "I am EvilGet"),
        ("evil_tostring", "string", "I am evil tostring"),
        ("bad_primitives", "string", ""),
    ],
)
def test_OrdinaryToPrimitive_02(some_objects, objname, cnvtype, result_val):
    with pytest.raises(ESTypeError, match=f"TypeError: {result_val}"):
        OrdinaryToPrimitive(some_objects[objname], cnvtype)


def test_ToPrimitive_notobj():
    # Need the object inputs, in the future!!
    cr = ToPrimitive("string")
    assert cr == "string"


@pytest.mark.parametrize(
    "objname, cnvtype, result_val",
    [
        ("plain", "string", "[object Object]"),
        ("plain", "number", "[object Object]"),
        ("treasure", "string", "You found the treasure"),
        ("treasure", "number", 42),
        ("treasure", "default", 42),
    ],
)
def test_ToPrimitive_01(some_objects, objname, cnvtype, result_val):
    cr = ToPrimitive(some_objects[objname], cnvtype)
    assert cr == result_val


@pytest.mark.parametrize(
    "objname, cnvtype, result_val",
    [
        ("evil_get", "string", "I am EvilGet"),
        ("evil_tostring", "string", "I am evil tostring"),
        ("bad_primitives", "string", ""),
    ],
)
def test_ToPrimitive_02(some_objects, objname, cnvtype, result_val):
    with pytest.raises(ESTypeError, match=f"TypeError: {result_val}"):
        ToPrimitive(some_objects[objname], cnvtype)


def test_ToPrimitive_GetMethodThrows(realm):
    # If an object has a @@toPrimitive property, the ToPrimitive routine tries to use as a conversion method.
    # But if it can't actually be used as a method, we'll get an exception instead.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    CreateDataProperty(obj, wks_to_primitive, 100)

    with pytest.raises(ESTypeError):
        ToPrimitive(obj, "number")


@pytest.mark.parametrize("input,expected", [("number", "I was passed number."), ("string", "I was passed string.")])
def test_ToPrimitive_exotictoprim(realm, input, expected):
    # If an object has a @@toPrimitive method, ToPrimitive should use it.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    def exotic_to_primitive(self, new_target, hint):
        return "I was passed %s." % hint

    CreateMethodProperty(obj, wks_to_primitive, CreateBuiltinFunction(exotic_to_primitive, [], realm))

    cr = ToPrimitive(obj, input)
    assert cr == expected


def test_ToPrimitive_exoticthrows(realm):
    # If an object's @@toPrimitive throws an error, it's not ignored.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    def exotic_to_primitive(self, new_target, hint):
        raise ESTypeError("I am evil.")

    CreateMethodProperty(obj, wks_to_primitive, CreateBuiltinFunction(exotic_to_primitive, [], realm))

    with pytest.raises(ESTypeError, match="I am evil\\."):
        ToPrimitive(obj, "string")


def test_ToPrimitive_exoticreturnsobj(realm):
    # If an object's @@toPrimitive returns an Object, ToPrimitive throws a TypeError
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    def exotic_to_primitive(self, new_target, hint):
        return obj

    CreateMethodProperty(obj, wks_to_primitive, CreateBuiltinFunction(exotic_to_primitive, [], realm))

    with pytest.raises(ESTypeError):
        ToPrimitive(obj, "string")


@pytest.mark.parametrize(
    "input,expected",
    [
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
        ("", False),
        ("yugoloth", True),
        (wks_to_primitive, True),
    ],
)
def test_to_boolean(input, expected):
    cr = ToBoolean(input)
    assert cr == expected


@pytest.mark.parametrize(
    "input,expected",
    [
        (None, math.nan),
        (JSNull.NULL, 0),
        (True, 1),
        (False, 0),
        (67, 67),
        ("0", 0),
        ("1", 1),
        ("0b101", 5),
        ("0XAB", 0xAB),
        ("0O773", 0o773),
        ("-10", -10),
        ("+10.5", 10.5),
        ("10.e3", 10000),
        ("1e+3", 1000),
        (".9e-3", 0.0009),
        ("10.5e-2", 0.105),
        ("6.", 6),
        (".5", 0.5),
        ("+Infinity", math.inf),
        ("-Infinity", -math.inf),
        ("Infinity", math.inf),
        ("inf", math.nan),
        ("\n\n56\t \r", 56),
        ("0x" + "75789240758907289067984302789758920789089729803478592078975819920375801" * 4, math.inf),
        ("0b" + "1" * 1025, math.inf),
        ("0o" + "7" * 342, math.inf),
        ("100e4000", math.inf),
        ("-5e4000", -math.inf),
        ("5e-4000", 0),
    ],
)
def test_ToNumber_01(input, expected):
    cr = ToNumber(input)
    if math.isnan(expected):
        assert math.isnan(cr)
    else:
        assert cr == expected


@pytest.mark.parametrize("input,expected", [("-0", "-0.0"), ("-.0", "-0.0"), ("-0.0", "-0.0")])
def test_ToNumber_negative_zero(input, expected):
    cr = ToNumber(input)
    assert str(cr) == expected


def test_ToNumber_symbol(realm):
    with pytest.raises(ESTypeError):
        ToNumber(wks_to_primitive)


def test_ToNumber_04(obj):
    # Success: Calls ToPrimitive on the object, then ToNumber on the primitive
    def valueOf(self, new_target):
        return 76

    fcn = CreateBuiltinFunction(valueOf, [])
    CreateMethodPropertyOrThrow(obj, "valueOf", fcn)

    res = ToNumber(obj)
    assert res == 76


def test_ToNumber_05(obj, mocker):
    # When ToPrimitive throws an error
    mocker.patch("ecmascript.ToPrimitive", side_effect=ESTypeError("throw test"))

    with pytest.raises(ESTypeError):
        ToNumber(obj)


def test_ToNumber_06(obj):
    # When ToNumber throws an error in the recursive call when getting the value of an object.
    def valueOf(self, new_target):
        return wks_to_primitive

    fcn = CreateBuiltinFunction(valueOf, [])
    CreateMethodPropertyOrThrow(obj, "valueOf", fcn)

    with pytest.raises(ESTypeError):
        ToNumber(obj)


@pytest.mark.parametrize(
    "input,expected",
    [("goblin", 0), (0, 0), (-math.inf, -math.inf), (math.inf, math.inf), (10.3, 10.0), (-23.3, -23.0)],
)
def test_to_integer(input, expected):
    cr = ToInteger(input)
    assert cr == expected


def test_to_integer_negative_zero():
    cr = ToInteger(-0.0)
    assert str(cr) == "-0.0"


def test_to_integer_symbol(realm):
    with pytest.raises(ESTypeError):
        ToInteger(wks_to_primitive)


@pytest.mark.parametrize(
    "input,expected",
    [
        (math.nan, 0),
        (0, 0),
        (math.inf, 0),
        (-math.inf, 0),
        (0x1234567890, 0x34567890),
        (0xB4567890, -0x4BA98770),
        (367, 367),
        (-56, -56),
    ],
)
def test_to_int32(input, expected):
    cr = ToInt32(input)
    assert cr == expected
    assert cr == ToInt32(cr)  # Idempotentcy check


def test_to_int32_neg_zero():
    cr = ToInt32(-0.0)
    assert str(cr) == "0"


def test_to_int32_symbol(realm):
    with pytest.raises(ESTypeError):
        ToInt32(wks_to_primitive)


@pytest.mark.parametrize(
    "input,expected",
    [
        (math.nan, 0),
        (0, 0),
        (math.inf, 0),
        (-math.inf, 0),
        (0x1234567890, 0x34567890),
        (0xB4567890, 0xB4567890),
        (367, 367),
        (-56, 0xFFFFFFC8),
    ],
)
def test_to_uint32(input, expected):
    cr = ToUint32(input)
    assert cr == expected


def test_to_uint32_symbol(realm):
    with pytest.raises(ESTypeError):
        ToUint32(wks_to_primitive)


@pytest.mark.parametrize(
    "input,expected",
    [
        (math.nan, 0),
        (0, 0),
        (math.inf, 0),
        (-math.inf, 0),
        (0x1234567890, 0x7890),
        (0xF890, -0x770),
        (367, 367),
        (-56, -56),
    ],
)
def test_to_int16(input, expected):
    cr = ToInt16(input)
    assert cr == expected


def test_to_int16_symbol(realm):
    with pytest.raises(ESTypeError):
        ToInt16(wks_to_primitive)


@pytest.mark.parametrize(
    "input,expected",
    [
        (math.nan, 0),
        (0, 0),
        (math.inf, 0),
        (-math.inf, 0),
        (0x1234567890, 0x7890),
        (0xB456F890, 0xF890),
        (367, 367),
        (-56, 0xFFC8),
    ],
)
def test_to_uint16(input, expected):
    cr = ToUint16(input)
    assert cr == expected


def test_to_uint16_symbol(realm):
    with pytest.raises(ESTypeError):
        ToUint16(wks_to_primitive)


@pytest.mark.parametrize(
    "input,expected",
    [(math.nan, 0), (0, 0), (math.inf, 0), (-math.inf, 0), (0x12345678, 0x78), (0x90, -0x70), (67, 67), (-56, -56)],
)
def test_to_int8(input, expected):
    cr = ToInt8(input)
    assert cr == expected


def test_to_int8_symbol(realm):
    with pytest.raises(ESTypeError):
        ToInt8(wks_to_primitive)


@pytest.mark.parametrize(
    "input,expected",
    [
        (math.nan, 0),
        (0, 0),
        (math.inf, 0),
        (-math.inf, 0),
        (0x12345678, 0x78),
        (0xB456F890, 0x90),
        (67, 67),
        (-56, 0xC8),
    ],
)
def test_to_uint8(input, expected):
    cr = ToUint8(input)
    assert cr == expected


def test_to_uint8_symbol(realm):
    with pytest.raises(ESTypeError):
        ToUint8(wks_to_primitive)


@pytest.mark.parametrize(
    "input,expected",
    [
        ("goblin", 0),
        ("-0.0", 0),
        (903, 255),
        (-3, 0),
        (math.inf, 255),
        ("-Infinity", 0),
        (20, 20),
        (19.2, 19),
        (19.7, 20),
        (19.5, 20),
        (18.5, 18),
    ],
)
def test_to_uint8_clamp(input, expected):
    cr = ToUint8Clamp(input)
    assert cr == expected


def test_to_uint8_clamp_symbol(realm):
    with pytest.raises(ESTypeError):
        ToUint8Clamp(wks_to_primitive)


@pytest.mark.parametrize(
    "input,expected",
    [
        (None, "undefined"),
        (JSNull.NULL, "null"),
        (True, "true"),
        (False, "false"),
        ("green", "green"),
        (3.25, "3.25"),
        (math.inf, "Infinity"),
        (-math.inf, "-Infinity"),
        (math.nan, "NaN"),
        (-0.0, "0"),
        (1, "1"),
        (1.0, "1"),
        (-1, "-1"),
        (-1.0, "-1"),
        (10.5, "10.5"),
    ],
)
def test_ToString_01(input, expected):
    cr = ToString(input)
    assert cr == expected


def test_ToString_symbol(realm):
    with pytest.raises(ESTypeError):
        ToString(wks_to_primitive)


def test_ToString_03(mocker, obj):
    # Input is an object, and ToPrimitive throws.
    mocker.patch("ecmascript.ToPrimitive", side_effect=ESTypeError)
    with pytest.raises(ESTypeError):
        ToString(obj)


def test_ToString_04(obj):
    # Input is an object, and it has a valid toString method.
    fcn = CreateBuiltinFunction(lambda thisvalue, newtarget: "test object string", [])
    CreateMethodProperty(obj, "toString", fcn)
    res = ToString(obj)
    assert res == "test object string"


@pytest.mark.parametrize(
    "arg,expected",
    [
        (math.nan, "NaN"),
        (0.0, "0"),
        (-0.0, "0"),
        (-3, "-3"),
        (math.inf, "Infinity"),
        (21000, "21000"),
        (500000000000000000000, "500000000000000000000"),
        (5.25, "5.25"),
        (0.00390625, "0.00390625"),
        (5e21, "5e+21"),
        pytest.param(2 ** -19, "0.0000019073486328125", marks=pytest.mark.xfail),
        pytest.param(2 ** -20, "9.5367431640625e-7", marks=pytest.mark.xfail),
    ],
)
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
    assert cr == obj


def test_ToObject_02(realm):
    # Boolean -> Object
    res = ToObject(True)
    assert isObject(res)
    assert hasattr(res, "BooleanData")
    assert res.BooleanData


def test_ToObject_03(realm):
    # Null -> Error
    with pytest.raises(ESTypeError):
        ToObject(JSNull.NULL)


def test_ToObject_04(realm):
    # Undefined -> Error
    with pytest.raises(ESTypeError):
        ToObject(None)


def test_ToObject_05(realm):
    # Number -> Object
    res = ToObject(34000)
    assert isObject(res)
    assert hasattr(res, "NumberData")
    assert res.NumberData == 34000


def test_ToObject_06(realm):
    # String -> Object
    res = ToObject("tricky")
    assert isObject(res)
    assert hasattr(res, "StringData")
    assert res.StringData == "tricky"


@pytest.mark.xfail(reason="Needs Symbol Object Support")
def test_ToObject_07(realm):
    # Symbol -> Object
    res = ToObject(wks_to_primitive)
    assert isObject(res)
    assert hasattr(res, "SymbolData")
    assert res.SymbolData == wks_to_primitive


@pytest.mark.parametrize(
    "arg,expected",
    [("1", 1), ("Infinity", math.inf), ("-3", -3), ("1.125", 1.125), ("     1.125", None), ("0x13", None)],
)
def test_CanonicalNumericIndexString_(arg, expected):
    assert CanonicalNumericIndexString(arg) == expected


def test_CanonicalNumericIndexString_nan():
    assert math.isnan(CanonicalNumericIndexString("NaN"))


def test_CanonicalNumericIndexString_negative_zero():
    val = CanonicalNumericIndexString("-0")
    assert math.copysign(1.0, val) == -1.0 and val == 0.0


@pytest.mark.parametrize(
    "arg_x,arg_y,expected",
    [
        (0, "0", False),
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
        ("blue", "blue", True),
        ("blue", "green", False),
        (wks_to_primitive, wks_to_primitive, True),
        (wks_to_primitive, wks_match, False),
    ],
)
def test_SameValue(arg_x, arg_y, expected):
    assert SameValue(arg_x, arg_y) == expected


@pytest.mark.parametrize(
    "arg,expected", [("thing", True), ("", True), (78, False), (True, False), (None, False), (wks_match, True)]
)
def test_IsPropertyKey(arg, expected):
    assert IsPropertyKey(arg) == expected


@pytest.mark.parametrize(
    "arg,expected",
    [
        ("green", False),
        (-0.0, True),
        (math.nan, False),
        (math.inf, False),
        (-math.inf, False),
        (5.0, True),
        (17.5, False),
        (-2391.2, False),
        (-78, True),
        (2 ** 50, True),
    ],
)
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
    fcn_obj = CreateBuiltinFunction(lambda self, new_target: None, [])
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
    res = IsConstructor(realm.intrinsics["%Boolean%"])
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
    IsExtensible(obj)
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
    assert res == "69"


def test_ToPropertyKey_02():
    res = ToPropertyKey(wks_to_primitive)
    assert res == wks_to_primitive


def test_ToPropertyKey_03(realm, mocker):
    mocker.patch("ecmascript.ToPrimitive", side_effect=ESTypeError)
    with pytest.raises(ESTypeError):
        ToPropertyKey("a")


# 7.1.15 ToLength ( argument )
# The abstract operation ToLength converts argument to an integer suitable for use as the length of an array-like
# object. It performs the following steps:
#
# 1. Let len be ? ToInteger(argument).
# 2. If len ≤ +0, return +0.
# 3. Return min(len, 2^53-1).
@pytest.mark.parametrize("inp, expected", [(10, 10), (-10, 0), (2 ** 53 - 1, 2 ** 53 - 1), (2 ** 53, 2 ** 53 - 1)])
def test_ToLength_01(realm, inp, expected):
    result = ToLength(inp)
    assert result == expected


# 7.1.17 ToIndex ( value )
# The abstract operation ToIndex returns value argument converted to a numeric value if it is a valid integer index
# value. This abstract operation functions as follows:
#
#   1. If value is undefined, then
#       a. Let index be 0.
#   2. Else,
#       a. Let integerIndex be ? ToInteger(value).
#       b. If integerIndex < 0, throw a RangeError exception.
#       c. Let index be ! ToLength(integerIndex).
#       d. If SameValueZero(integerIndex, index) is false, throw a RangeError exception.
#   3. Return index.
class Test_ToIndex:
    @pytest.mark.parametrize("inp, expected", [(None, 0), (10, 10), (0, 0), (10.33, 10)])
    def test_ToIndex_normal(self, realm, inp, expected):
        rv = ToIndex(inp)
        assert rv == expected

    @pytest.mark.parametrize("inp", [-1, 55e55])
    def test_ToIndex_errors(self, realm, inp):
        with pytest.raises(ESRangeError):
            ToIndex(inp)


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
@pytest.mark.parametrize("inp", [None, JSNull.NULL])
def test_RequireObjectCoercible_01(realm, inp):
    # The "I throw a type error exception" case.
    with pytest.raises(ESTypeError):
        RequireObjectCoercible(inp)


@pytest.mark.parametrize("inp", [True, 10, "flower", wks_to_primitive])
def test_RequireObjectCoercible_02(inp):
    # Boolean, Number, String, and Symbol values
    res = RequireObjectCoercible(inp)
    assert res == inp


def test_RequireObjectCoercible_03(obj):
    # An Object value
    res = RequireObjectCoercible(obj)
    assert res == obj


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
    res = IsArray("foo")
    assert res == False


def test_IsArray_02(realm):
    # arg is an array.
    arg = ArrayCreate(10, realm.intrinsics["%ObjectPrototype%"])
    res = IsArray(arg)
    assert res == True


def test_IsArray_03(realm):
    # arg is a busted proxy object.
    arg = ProxyObject()
    arg.ProxyHandler = JSNull.NULL
    with pytest.raises(ESTypeError):
        IsArray(arg)


def test_IsArray_04(realm):
    # arg is a proxy object with a proxy target that's an array.
    arg = ProxyObject()
    arg.ProxyHandler = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    arg.ProxyTarget = ArrayCreate(10, realm.intrinsics["%ObjectPrototype%"])
    res = IsArray(arg)
    assert res == True


def test_IsArray_05(realm):
    # arg is a proxy object with a proxy target that isn't an array.
    arg = ProxyObject()
    arg.ProxyHandler = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    arg.ProxyTarget = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    res = IsArray(arg)
    assert res == False


def test_IsArray_06(obj):
    # An object that's not an array
    res = IsArray(obj)
    assert res == False


# 7.2.11 SameValueZero ( x, y )
# The internal comparison abstract operation SameValueZero(x, y), where x and y are ECMAScript language values,
# produces true or false. Such a comparison is performed as follows:
#
#   1. If Type(x) is different from Type(y), return false.
#   2. If Type(x) is Number, then
#       a. If x is NaN and y is NaN, return true.
#       b. If x is +0 and y is -0, return true.
#       c. If x is -0 and y is +0, return true.
#       d. If x is the same Number value as y, return true.
#       e. Return false.
#   3. Return SameValueNonNumber(x, y).
class Test_SameValueZero:
    @pytest.mark.parametrize(
        "x, y, expected",
        [
            ("string", 67, False),
            (math.nan, math.nan, True),
            (0, -0.0, True),
            (-0.0, 0, True),
            (22, 22, True),
            (78, 899, False),
        ],
    )
    def test_SameValueZero_simple(self, realm, x, y, expected):
        rv = SameValueZero(x, y)
        assert rv == expected

    def test_SameValueZero_nonnumber(self, realm, mocker):
        svnn = mocker.patch("ecmascript.SameValueNonNumber")
        SameValueZero("this", "that")
        svnn.assert_called_once_with("this", "that")


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
    mocker.patch("ecmascript.ToPrimitive", side_effect=lambda a, b: a)
    res = AbstractRelationalComparison(10, 20, True)

    # assert the order
    ecmascript.ToPrimitive.assert_has_calls([call(10, "number"), call(20, "number")])  # pylint: disable=no-member
    # and check the result, just because
    assert res == True


def test_AbstractRelationalComparison_02(mocker):
    # LeftFirst False: make sure we evaluate the right arg before the left arg
    mocker.patch("ecmascript.ToPrimitive", side_effect=lambda a, b: a)
    res = AbstractRelationalComparison(10, 20, False)

    # assert the order
    ecmascript.ToPrimitive.assert_has_calls([call(20, "number"), call(10, "number")])  # pylint: disable=no-member
    assert res == True


@pytest.mark.parametrize(
    "left, right, expected",
    [
        ("th", "thing", True),  # Left is a substring of Right (3b)
        ("thing", "th", False),  # Right is a substring of Left (3a)
        ("thing", "thang", False),  # 3f: strings where one is not a prefix of the other
        ("thing", "thzzz", True),  # 3f: strings where one is not a prefix of the other
        (math.nan, 67, None),  # 4d left is nan
        (67, math.nan, None),  # 4e right is nan
        (88, 88, False),  # 4f Same number
        (0, -0.0, False),  # 4g
        (-0.0, 0, False),  # 4h
        (math.inf, math.inf, False),  # 4f again
        (math.inf, 888, False),  # 4i
        (222, math.inf, True),  # 4j
        (987, -math.inf, False),  # 4k
        (-math.inf, 8193, True),  # 4l
        (100, 1000, True),  # 4m
        (1000, 100, False),  # 4m
    ],
)
def test_AbstractRelationalComparison_03(realm, left, right, expected):
    res = AbstractRelationalComparison(left, right, True)
    assert res == expected


@pytest.mark.parametrize("left, right, lrf", [(20, 10, True), (10, 20, True), (20, 10, False), (10, 20, False)])
def test_AbstractRelationalComparison_04(realm, mocker, left, right, lrf):
    # The ToPrimitive error paths
    def mocked_tp(a, b):
        if a == 10:
            return 10
        else:
            raise ESTypeError("err thrown")

    mocker.patch("ecmascript.ToPrimitive", side_effect=mocked_tp)
    with pytest.raises(ESTypeError):
        AbstractRelationalComparison(left, right, lrf)


@pytest.mark.parametrize("left, right", [(10, 20), (20, 10)])
def test_AbstractRelationalComparison_05(realm, mocker, left, right):
    # The ToNumber error paths
    def mocked_tn(a):
        if a == 10:
            return 10
        raise ESTypeError("err thrown")

    mocker.patch("ecmascript.ToNumber", side_effect=mocked_tn)
    with pytest.raises(ESTypeError):
        AbstractRelationalComparison(left, right, True)


# 7.2.14 Abstract Equality Comparison
@pytest.mark.parametrize(
    "left, right",
    [
        (None, None),
        (JSNull.NULL, JSNull.NULL),
        (True, False),
        (100, 200),
        ("abc", "def"),
        (wks_to_primitive, wks_search),
    ],
)
def test_AbstractEqualityComparsion_01(realm, mocker, left, right):
    # If Type(left) == Type(right) we're supposed to defer to StrictEqualityComparison
    mocker.patch("ecmascript.StrictEqualityComparison")

    AbstractEqualityComparison(left, right)
    ecmascript.StrictEqualityComparison.assert_called_once_with(left, right)  # pylint: disable=no-member


@pytest.mark.parametrize(
    "left, right, expected",
    [
        (JSNull.NULL, None, True),  # If x is null and y is undefined, return true.
        (None, JSNull.NULL, True),  # If x is undefined and y is null, return true.
        (
            10,
            "10",
            True,
        ),  # If Type(x) is Number and Type(y) is String, return the result of the comparison x == ! ToNumber(y).
        (15, "0xf", True),
        (
            "0b0111",
            7,
            True,
        ),  # If Type(x) is String and Type(y) is Number, return the result of the comparison ! ToNumber(x) == y.
        ("67", 99, False),
        (True, 1, True),  # If Type(x) is Boolean, return the result of the comparison ! ToNumber(x) == y.
        (False, 0, True),
        (1, False, False),  # If Type(y) is Boolean, return the result of the comparison x == ! ToNumber(y).
        (1, True, True),
        ("0", False, True),
        ("teststring", None, False),
    ],
)
def test_AbstractEqualityComparsion_02(realm, left, right, expected):
    res = AbstractEqualityComparison(left, right)
    assert res == expected


@pytest.mark.parametrize("nonobj, expected", [(55, False), ("[object Object]", True)])
def test_AbstractEqualityComparsion_03(obj, nonobj, expected):
    res = AbstractEqualityComparison(nonobj, obj)
    assert res == expected


@pytest.mark.parametrize("nonobj, expected", [(55, False), ("[object Object]", True)])
def test_AbstractEqualityComparsion_04(obj, nonobj, expected):
    res = AbstractEqualityComparison(obj, nonobj)
    assert res == expected


def test_AbstractEqualityComparison_05(obj, mocker):
    mocker.patch("ecmascript.ToPrimitive", side_effect=ESTypeError)
    with pytest.raises(ESTypeError):
        AbstractEqualityComparison(obj, 67)


def test_AbstractEqualityComparison_06(obj, mocker):
    mocker.patch("ecmascript.ToPrimitive", side_effect=ESTypeError)
    with pytest.raises(ESTypeError):
        AbstractEqualityComparison(99, obj)


# 7.3.18 Invoke ( V, P [ , argumentsList ] )
def test_Invoke_01(realm):
    # Something simple. ToString on a number perhaps.
    res = Invoke(67, "toString")
    assert res == "67"


def test_Invoke_02(realm):
    # When GetV throws, we pass the exception along.
    with pytest.raises(ESTypeError):
        Invoke(None, "toString")

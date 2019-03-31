"""
7 Abstract Operations

These operations are not a part of the ECMAScript language; they are defined here to solely to aid the specification of
the semantics of the ECMAScript language. Other, more specialized abstract operations are defined throughout this
specification.
"""
import math
import re

from completion_record import CompletionType, NormalCompletion, ThrowCompletion
from jstypes import JSObject, JSSymbol, wks_to_primitive, isEcmaValue, isObject, isUndefined, isNull, isBoolean, isNumber, isString, isSymbol
from lexer import Lexer
from errors import CreateTypeError

# 7.1 Type Conversion
#
# The ECMAScript language implicitly performs automatic type conversion as needed. To clarify the semantics of certain
# constructs it is useful to define a set of conversion abstract operations. The conversion abstract operations are
# polymorphic; they can accept a value of any ECMAScript language type. But no other specification types are used with
# these operations.

# 7.1.1 ToPrimitive ( input [ , PreferredType ] )
def ToPrimitive(input, preferred_type='default'):
    """
    The abstract operation ToPrimitive takes an input argument and an optional argument PreferredType. The abstract
    operation ToPrimitive converts its input argument to a non-Object type. If an object is capable of converting to
    more than one primitive type, it may use the optional hint PreferredType to favour that type.
    """
    # Conversion occurs according to the following algorithm:

    # 1. Assert: input is an ECMAScript language value.
    assert isEcmaValue(input)
    assert preferred_type in ['default', 'string', 'number']
    # 2. If Type(input) is Object, then
    if isObject(input):
        # a. If PreferredType is not present, let hint be "default".
        # b. Else if PreferredType is hint String, let hint be "string".
        # c. Else PreferredType is hint Number, let hint be "number".
        # d. Let exoticToPrim be ? GetMethod(input, @@toPrimitive).
        cr = GetMethod(input, wks_to_primitive)
        if cr.ctype != CompletionType.NORMAL:
            return cr
        exotic_to_prim = cr.value
        # e. If exoticToPrim is not undefined, then
        if exotic_to_prim is not None:
            # i. Let result be ? Call(exoticToPrim, input, « hint »).
            cr = Call(exotic_to_prim, input, [ preferred_type ])
            if cr.ctype != CompletionType.NORMAL:
                return cr
            result = cr.value
            # ii. If Type(result) is not Object, return result.
            if not isObject(result):
                return NormalCompletion(result)
            # iii. Throw a TypeError exception.
            exception = CreateTypeError()
            return ThrowCompletion(exception)
        # f. If hint is "default", set hint to "number".
        if preferred_type == 'default':
            preferred_type = 'number'
        # g. Return ? OrdinaryToPrimitive(input, hint).
        return OrdinaryToPrimitive(input, preferred_type) # Because we're just returning, we don't need to validate error condition
    # 3. Return input.
    return NormalCompletion(input)

    # NOTE
    #
    # When ToPrimitive is called with no hint, then it generally behaves as if the hint were Number. However, objects
    # may over-ride this behaviour by defining a @@toPrimitive method. Of the objects defined in this specification only
    # Date objects (see 20.3.4.45) and Symbol objects (see 19.4.3.4) over-ride the default ToPrimitive behaviour. Date
    # objects treat no hint as if the hint were String.

# 7.1.1.1 OrdinaryToPrimitive ( O, hint )
def OrdinaryToPrimitive(obj, hint):
    # When the abstract operation OrdinaryToPrimitive is called with arguments O and hint, the following steps are
    # taken:

    # 1. Assert: Type(O) is Object.
    assert isObject(obj)
    # 2. Assert: Type(hint) is String and its value is either "string" or "number".
    assert hint in ['string', 'number']
    # 3. If hint is "string", then
    if hint == 'string':
        # a. Let methodNames be « "toString", "valueOf" ».
        method_names = [ 'toString', 'valueOf' ]
    # 4. Else,
    else:
        # a. Let methodNames be « "valueOf", "toString" ».
        method_names = ['valueOf', 'toString']
    # 5. For each name in methodNames in List order, do
    for name in method_names:
        # a. Let method be ? Get(O, name).
        cr = Get(obj, name)
        if cr.ctype != CompletionType.NORMAL:
            return cr
        method = cr.value
        # b. If IsCallable(method) is true, then
        if IsCallable(method):
            # i. Let result be ? Call(method, O).
            cr = Call(method, obj)
            if cr.ctype != CompletionType.NORMAL:
                return cr
            result = cr.value
            # ii. If Type(result) is not Object, return result.
            if not isObject(result):
                return NormalCompletion(result)
    # 6. Throw a TypeError exception.
    return ThrowCompletion(CreateTypeError())

# 7.1.2 ToBoolean ( argument )
def ToBoolean(arg):
    # The abstract operation ToBoolean converts argument to a value of type Boolean according to Table 9:
    #
    # Table 9: ToBoolean Conversions
    # +---------------+---------------------------------------------------------------------------------------------
    # | Argument Type |  Result
    # +---------------+---------------------------------------------------------------------------------------------
    # | Undefined     | Return false.
    # | Null          | Return false.
    # | Boolean       | Return argument.
    # | Number        | If argument is +0, -0, or NaN, return false; otherwise return true.
    # | String        | If argument is the empty String (its length is zero), return false; otherwise return true.
    # | Symbol        | Return true.
    # | Object        | Return true.
    # +---------------+---------------------------------------------------------------------------------------------

    if isUndefined(arg) or isNull(arg):
        result = False
    elif isBoolean(arg):
        result = arg
    elif isNumber(arg):
        result = arg != 0.0 and not math.isnan(arg)
    elif isString(arg):
        result = len(arg) > 0
    else: # isSymbol(arg) or isObject(arg)
        result = True
    return NormalCompletion(result)

# 7.1.3 ToNumber ( argument )
def ToNumber(arg):
    if isUndefined(arg):
        result = math.nan
    elif isNull(arg):
        result = 0
    elif isBoolean(arg):
        result = 1 if arg else 0
    elif isNumber(arg):
        result = arg
    elif isSymbol(arg):
        return ThrowCompletion(CreateTypeError())
    elif isObject(arg):
        cr = ToPrimitive(arg, 'number')
        if cr.ctype != CompletionType.NORMAL:
            return cr
        prim_value = cr.value
        cr = ToNumber(prim_value)
        if cr.ctype != CompletionType.NORMAL:
            return cr
        result = cr.value
    else: # String!
        digits = arg.strip(Lexer.whitespace + Lexer.line_terminators)
        binary = re.match(r'0[bB]([0-1]+)$', digits)
        if binary:
            try:
                result = float(int(binary.group(1), 2))
            except OverflowError:
                result = math.inf
        else:
            octal = re.match(r'0[oO]([0-7]+)$', digits)
            if octal:
                try:
                    result = float(int(octal.group(1), 8))
                except OverflowError:
                    result = math.inf
            else:
                hexdigs = re.match(r'0[xX]([0-9a-fA-F]+)$', digits)
                if hexdigs:
                    try:
                        result = float(int(hexdigs.group(1), 16))
                    except OverflowError:
                        result = math.inf
                else:
                    # StrUnsignedDecimalLiteral :::
                    #   Infinity
                    #   DecimalDigits . DecimalDigits ExponentPart
                    #   DecimalDigits . DecimalDigits
                    #   DecimalDigits . ExponentPart
                    #   DecimalDigits .
                    #   . DecimalDigits ExponentPart
                    #   . DecimalDigits
                    #   DecimalDigits ExponentPart
                    #   DecimalDigits

                    decimal = re.match(r'[-+]?((Infinity)|([0-9]+\.([0-9]+)?([eE][-+]?[0-9]+)?)|(\.[0-9]+([eE][-+]?[0-9]+)?)|([0-9]+([eE][-+]?[0-9]+)?))$', digits)
                    if decimal:
                        result = float(digits)
                    else:
                        result = math.nan
    return NormalCompletion(result)

# 7.1.4 ToInteger ( argument )
def ToInteger(arg):
    cr = ToNumber(arg)
    if cr.ctype != CompletionType.NORMAL:
        return cr
    number = cr.value
    if math.isnan(number):
        result = 0
    elif number == 0.0 or abs(number) == math.inf:
        result = number
    else:
        result = math.floor(abs(number))
        if number < 0:
            result = -result
    return NormalCompletion(result)

# 7.1.5 ToInt32 ( argument )
def ToInt32(arg):
    cr = ToNumber(arg)
    if cr.ctype != CompletionType.NORMAL:
        return cr
    number = cr.value
    if math.isnan(number) or number == 0 or abs(number) == math.inf:
        return NormalCompletion(0)
    this_int = math.floor(abs(number))
    if number < 0:
        this_int = -this_int
    int32bit = this_int % 2**32
    if int32bit >= 2**31:
        return NormalCompletion(int32bit - 2**32)
    return NormalCompletion(int32bit)

# 7.1.6 ToUint32 ( argument )
def ToUint32(arg):
    cr = ToNumber(arg)
    if cr.ctype != CompletionType.NORMAL:
        return cr
    number = cr.value
    if math.isnan(number) or number == 0 or abs(number) == math.inf:
        return NormalCompletion(0)
    this_int = math.floor(abs(number))
    if number < 0:
        this_int = -this_int
    int32bit = this_int % 2**32
    return NormalCompletion(int32bit)

# 7.1.7 ToInt16 ( argument )
def ToInt16(arg):
    cr = ToNumber(arg)
    if cr.ctype != CompletionType.NORMAL:
        return cr
    number = cr.value
    if math.isnan(number) or number == 0 or abs(number) == math.inf:
        return NormalCompletion(0)
    this_int = math.floor(abs(number))
    if number < 0:
        this_int = -this_int
    int16bit = this_int % 2**16
    if int16bit >= 2**15:
        return NormalCompletion(int16bit - 2**16)
    return NormalCompletion(int16bit)

# 7.1.8 ToUint16 ( argument )
def ToUint16(arg):
    cr = ToNumber(arg)
    if cr.ctype != CompletionType.NORMAL:
        return cr
    number = cr.value
    if math.isnan(number) or number == 0 or abs(number) == math.inf:
        return NormalCompletion(0)
    this_int = math.floor(abs(number))
    if number < 0:
        this_int = -this_int
    int16bit = this_int % 2**16
    return NormalCompletion(int16bit)

# 7.1.9 ToInt8 ( argument )
def ToInt8(arg):
    cr = ToNumber(arg)
    if cr.ctype != CompletionType.NORMAL:
        return cr
    number = cr.value
    if math.isnan(number) or number == 0 or abs(number) == math.inf:
        return NormalCompletion(0)
    this_int = math.floor(abs(number))
    if number < 0:
        this_int = -this_int
    int8bit = this_int % 2**8
    if int8bit >= 2**7:
        return NormalCompletion(int8bit - 2**8)
    return NormalCompletion(int8bit)

# 7.1.10 ToUint8 ( argument )
def ToUint8(arg):
    cr = ToNumber(arg)
    if cr.ctype != CompletionType.NORMAL:
        return cr
    number = cr.value
    if math.isnan(number) or number == 0 or abs(number) == math.inf:
        return NormalCompletion(0)
    this_int = math.floor(abs(number))
    if number < 0:
        this_int = -this_int
    int8bit = this_int % 2**8
    return NormalCompletion(int8bit)

# 7.1.11 ToUint8Clamp ( argument )
def ToUint8Clamp(arg):
    cr = ToNumber(arg)
    if cr.ctype != CompletionType.NORMAL:
        return cr
    number = cr.value
    if math.isnan(number) or number <= 0:
        return NormalCompletion(0)
    if number >= 255:
        return NormalCompletion(255)
    f = math.floor(number)
    if f + 0.5 < number:
        return NormalCompletion(f+1)
    if number < f + 0.5 or f & 1 == 0:
        return NormalCompletion(f)
    return NormalCompletion(f+1)

# 7.1.12 ToString ( argument )
def ToString(arg):
    if isUndefined(arg):
        return NormalCompletion('undefined')
    if isNull(arg):
        return NormalCompletion('null')
    if isBoolean(arg):
        return NormalCompletion('true' if arg else 'false')
    if isNumber(arg):
        return NormalCompletion(NumberToString(arg))
    if isString(arg):
        return NormalCompletion(arg)
    if isSymbol(arg):
        return ThrowCompletion(CreateTypeError())
    # isObject
    cr = ToPrimitive(arg, 'string')
    if cr.ctype != CompletionType.NORMAL:
        return cr
    prim_value = cr.value
    return ToString(prim_value)

# 7.1.12.1 NumberToString ( m )
def NumberToString(m):
    if math.isnan(m):
        return 'NaN'
    if m == 0:
        return '0'
    if m < 0:
        return '-' + NumberToString(-m)
    if m == math.inf:
        return 'Infinity'
    return str(m)

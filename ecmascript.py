"""
Scole's ECMAScript 9 system
"""
from enum import Enum, unique, auto
from collections import namedtuple, deque
from copy import copy
import re
import math
from itertools import chain
import unicodedata
import uuid
import random
import types
import traceback

surrounding_agent = None

@unique
class Empty(Enum):
    EMPTY = auto()
EMPTY = Empty.EMPTY

@unique
class CompletionType(Enum):
    NORMAL = auto()
    BREAK = auto()
    CONTINUE = auto()
    RETURN = auto()
    THROW = auto()

Completion = namedtuple('Completion', ['ctype', 'value', 'target'])


def CreateErrorObject(msg, intrinsic):
    error_constructor = surrounding_agent.running_ec.realm.intrinsics[intrinsic]
    errobj = Construct(error_constructor, [msg])
    Set(errobj, 'stack', ''.join(traceback.format_stack()[:-2]), False)
    return errobj
def CreateReferenceError(msg=''):
    return CreateErrorObject(msg, '%ReferenceError%')
def CreateTypeError(msg=''):
    return CreateErrorObject(msg, '%TypeError%')
def CreateSyntaxError(msg=''):
    return CreateErrorObject(msg, '%SyntaxError%')
def CreateRangeError(msg=''):
    return CreateErrorObject(msg, '%RangeError%')

class ESError(BaseException):
    def __init__(self, object):
        self.ecma_object = object
    def __str__(self):
        return ToString(self.ecma_object)
    def __repr__(self):
        return f'ESError({self.ecma_object!r})'
class ESReferenceError(ESError):
    def __init__(self, msg=''):
        super().__init__(CreateReferenceError(msg))
class ESTypeError(ESError):
    def __init__(self, msg=''):
        super().__init__(CreateTypeError(msg))
class ESSyntaxError(ESError):
    def __init__(self, msg=''):
        super().__init__(CreateSyntaxError(msg))
class ESRangeError(ESError):
    def __init__(self, msg=''):
        super().__init__(CreateRangeError(msg))
class ESAbrupt(BaseException):
    def __init__(self, ctype=CompletionType.THROW, value=Empty.EMPTY, target=Empty.EMPTY):
        self.completion = Completion(ctype, value, target)
class ESBreak(ESAbrupt):
    def __init__(self, value=Empty.EMPTY, target=Empty.EMPTY):
        super().__init__(ctype=CompletionType.BREAK, value=value, target=target)
class ESContinue(ESAbrupt):
    def __init__(self, value=Empty.EMPTY, target=Empty.EMPTY):
        super().__init__(ctype=CompletionType.CONTINUE, value=value, target=target)
class ESReturn(ESAbrupt):
    def __init__(self, value=Empty.EMPTY, target=Empty.EMPTY):
        super().__init__(ctype=CompletionType.RETURN, value=value, target=target)

class missing(Enum):
    MISSING = auto()

# 6.1 ECMAScript Language Types

@unique
class JSType(Enum):
    UNDEFINED = auto()
    NULL = auto()
    BOOLEAN = auto()
    STRING = auto()
    SYMBOL = auto()
    NUMBER = auto()
    OBJECT = auto()
    ILLEGAL = auto() # Not actually a JS type.

# 6.1.1 The Undefined Type
# The Undefined type has exactly one value, called "undefined". Any variable that has not been assigned a value has the
# value "undefined".
# Implementation: we shall use the python value "None" to represent "undefined".
def isUndefined(arg):
    return arg is None

# 6.1.2 The Null Type
@unique
class JSNull(Enum):
    # The Null type has exactly one value, called "null".
    NULL = auto()
def isNull(arg):
    return isinstance(arg, JSNull)

# 6.1.3 The Boolean Type
# The Boolean type represents a logical entity having two values, called "true" and "false".
# Implementation: we shall use the python bool type to be a ECMAScript Boolean.
def isBoolean(arg):
    return isinstance(arg, bool)

# 6.1.4 The String Type
# The String type is the set of all ordered sequences of zero or more 16-bit unsigned integer values (�elements�) up
# to a maximum length of 2^53-1 elements. The String type is generally used to represent textual data in a running
# ECMAScript program, in which case each element in the String is treated as a UTF-16 code unit value. Each element is
# regarded as occupying a position within the sequence.
# Implementation: we shall use the python str type to be an ECMAScript String.
def isString(arg):
    return isinstance(arg, str)

# 6.1.5 The Symbol Type
# The Symbol type is the set of all non-String values that may be used as the key of an Object property (6.1.7).
#
# Each possible Symbol value is unique and immutable.
#
# Each Symbol value immutably holds an associated value called [[Description]] that is either undefined or a String
# value.
# Implementation: Just use a one-item namedtuple. They're pretty lightweight.
JSSymbol = namedtuple('Symbol', ['description'])
def isSymbol(arg):
    return isinstance(arg, JSSymbol)

# 6.1.5.1 Well-Known Symbols
#
# Well-known symbols are built-in Symbol values that are explicitly referenced by algorithms of this specification. They
# are typically used as the keys of properties whose values serve as extension points of a specification algorithm.
# Unless otherwise specified, well-known symbols values are shared by all realms (8.2).
#
# Within this specification a well-known symbol is referred to by using a notation of the form @@name, where �name� is
# one of the values listed in Table 1.
#
# Implementation Note: we don't do leading '@' for Python identifiers, so these are referred to in code as wks_name.
# ("wks" as an acronym for "well-known symbol"). We also snake-case them.
#
# Table 1: Well-known Symbols
# +----------------------+-----------------------------+---------------------------------------------------------------
# | Specification Name   | [[Description]]             | Value and Purpose
# +----------------------+-----------------------------+---------------------------------------------------------------
# | @@asyncIterator      | "Symbol.asyncIterator"      | A method that returns the default AsyncIterator for an object.
# |                      |                             | Called by the semantics of the for-await-of statement.
# +----------------------+-----------------------------+---------------------------------------------------------------
# | @@hasInstance        | "Symbol.hasInstance"        | A method that determines if a constructor object recognizes an
# |                      |                             | object as one of the constructor's instances. Called by the
# |                      |                             | semantics of the instanceof operator.
# +----------------------+-----------------------------+---------------------------------------------------------------
# | @@isConcatSpreadable | "Symbol.isConcatSpreadable" | A Boolean valued property that if true indicates that an
# |                      |                             | object should be flattened to its array elements by
# |                      |                             | Array.prototype.concat.
# +----------------------+-----------------------------+---------------------------------------------------------------
# | @@iterator           | "Symbol.iterator"           | A method that returns the default Iterator for an object.
# |                      |                             | Called by the semantics of the for-of statement.
# +----------------------+-----------------------------+---------------------------------------------------------------
# | @@match              | "Symbol.match"              | A regular expression method that matches the regular
# |                      |                             | expression against a string. Called by the
# |                      |                             | String.prototype.match method.
# +----------------------+-----------------------------+---------------------------------------------------------------
# | @@replace            | "Symbol.replace"            | A regular expression method that replaces matched substrings
# |                      |                             | of a string. Called by the String.prototype.replace method.
# +----------------------+-----------------------------+---------------------------------------------------------------
# | @@search             | "Symbol.search"             | A regular expression method that returns the index within a
# |                      |                             | string that matches the regular expression. Called by the
# |                      |                             | String.prototype.search method.
# +----------------------+-----------------------------+---------------------------------------------------------------
# | @@species            | "Symbol.species"            | A function valued property that is the constructor function
# |                      |                             | that is used to create derived objects.
# +----------------------+-----------------------------+---------------------------------------------------------------
# | @@split              | "Symbol.split"              | A regular expression method that splits a string at the
# |                      |                             | indices that match the regular expression. Called by the
# |                      |                             | String.prototype.split method.
# +----------------------+-----------------------------+---------------------------------------------------------------
# | @@toPrimitive        | "Symbol.toPrimitive"        | A method that converts an object to a corresponding primitive
# |                      |                             | value. Called by the ToPrimitive abstract operation.
# +----------------------+-----------------------------+---------------------------------------------------------------
# | @@toStringTag        | "Symbol.toStringTag"        | A String valued property that is used in the creation of the
# |                      |                             | default string description of an object. Accessed by the
# |                      |                             | built-in method  Object.prototype.toString.
# +----------------------+-----------------------------+---------------------------------------------------------------
# | @@unscopables        | "Symbol.unscopables"        | An object valued property whose own and inherited property
# |                      |                             | names are property names that are excluded from the with
# |                      |                             | environment bindings of the associated object.
# +----------------------+-----------------------------+---------------------------------------------------------------
wks_async_iterator = JSSymbol('Symbol.asyncIterator')
wks_has_instance = JSSymbol('Symbol.hasInstance')
wks_is_concat_spreadable = JSSymbol('Symbol.isConcatSpreadable')
wks_iterator = JSSymbol('Symbol.iterator')
wks_match = JSSymbol('Symbol.match')
wks_replace = JSSymbol('Symbol.replace')
wks_search = JSSymbol('Symbol.search')
wks_species = JSSymbol('Symbol.species')
wks_split = JSSymbol('Symbol.split')
wks_to_primitive = JSSymbol('Symbol.toPrimitive')
wks_to_string_tag = JSSymbol('Symbol.toStringTag')
wks_unscopables = JSSymbol('Symbol.unscopables')

# 6.1.6 The Number Type
# The Number type has exactly 18437736874454810627 (that is, 2^64-2^53+3) values, representing the double-precision
# 64-bit format IEEE 754-2008 values as specified in the IEEE Standard for Binary Floating-Point Arithmetic, except
# that the 9007199254740990 (that is, 2^53-2) distinct “Not-a-Number” values of the IEEE Standard are represented in
# ECMAScript as a single special NaN value. (Note that the NaN value is produced by the program expression NaN.) In
# some implementations, external code might be able to detect a difference between various Not-a-Number values, but
# such behaviour is implementation-dependent; to ECMAScript code, all NaN values are indistinguishable from each other.
#
# NOTE
# The bit pattern that might be observed in an ArrayBuffer (see 24.1) or a SharedArrayBuffer (see 24.2) after a Number
# value has been stored into it is not necessarily the same as the internal representation of that Number value used
# by the ECMAScript implementation.
#
# There are two other special values, called positive Infinity and negative Infinity. For brevity, these values are
# also referred to for expository purposes by the symbols +∞ and -∞, respectively. (Note that these two infinite
# Number values are produced by the program expressions +Infinity (or simply Infinity) and -Infinity.)
#
# The other 18437736874454810624 (that is, 2^64-2^53) values are called the finite numbers. Half of these are positive
# numbers and half are negative numbers; for every finite positive Number value there is a corresponding negative
# value having the same magnitude.
#
# Note that there is both a positive zero and a negative zero. For brevity, these values are also referred to for
# expository purposes by the symbols +0 and -0, respectively. (Note that these two different zero Number values are
# produced by the program expressions +0 (or simply 0) and -0.)
#
# The 18437736874454810622 (that is, 2^64-2^53-2) finite nonzero values are of two kinds:
#
# 18428729675200069632 (that is, 2^64-2^54) of them are normalized, having the form
#
# s × m × 2^e
#
# where s is +1 or -1, m is a positive integer less than 2^53 but not less than 2^52, and e is an integer ranging from
# -1074 to 971, inclusive.
#
# The remaining 9007199254740990 (that is, 2^53-2) values are denormalized, having the form
#
# s × m × 2^e
#
# where s is +1 or -1, m is a positive integer less than 2^52, and e is -1074.
#
# Note that all the positive and negative integers whose magnitude is no greater than 253 are representable in the
# Number type (indeed, the integer 0 has two representations, +0 and -0).
#
# A finite number has an odd significand if it is nonzero and the integer m used to express it (in one of the two
# forms shown above) is odd. Otherwise, it has an even significand.
#
# In this specification, the phrase “the Number value for x” where x represents an exact real mathematical quantity
# (which might even be an irrational number such as π) means a Number value chosen in the following manner. Consider
# the set of all finite values of the Number type, with -0 removed and with two additional values added to it that are
# not representable in the Number type, namely 2^1024 (which is +1 × 2^53 × 2^971) and -2^1024 (which is
# -1 × 2^53 × 2^971). Choose the member of this set that is closest in value to x. If two values of the set are equally
# close, then the one with an even significand is chosen; for this purpose, the two extra values 2^1024 and -2^1024 are
# considered to have even significands. Finally, if 2^1024 was chosen, replace it with +∞; if -2^1024 was chosen,
# replace it with -∞; if +0 was chosen, replace it with -0 if and only if x is less than zero; any other chosen value
# is used unchanged. The result is the Number value for x. (This procedure corresponds exactly to the behaviour of the
# IEEE 754-2008 “round to nearest, ties to even” mode.)
#
# Some ECMAScript operators deal only with integers in specific ranges such as -2^31 through 2^31-1, inclusive, or in
# the range 0 through 2^16-1, inclusive. These operators accept any value of the Number type but first convert each
# such value to an integer value in the expected range. See the descriptions of the numeric conversion operations in
# 7.1.
#
# Implementation: A Python float works fine. We also will take integers if they happen by.
def isNumber(arg):
    return isinstance(arg, (int, float)) and not isinstance(arg, bool)

# 6.1.7 The Object Type
#
# An Object is logically a collection of properties. Each property is either a data property, or an accessor property:
#
#   * A data property associates a key value with an ECMAScript language value and a set of Boolean attributes.
#   * An accessor property associates a key value with one or two accessor functions, and a set of Boolean attributes.
#     The accessor functions are used to store or retrieve an ECMAScript language value that is associated with the
#     property.
#
# Properties are identified using key values. A property key value is either an ECMAScript String value or a Symbol
# value. All String and Symbol values, including the empty string, are valid as property keys. A property name is a
# property key that is a String value.
#
# An integer index is a String-valued property key that is a canonical numeric String (see 7.1.16) and whose numeric
# value is either +0 or a positive integer ≤ 2^53-1. An array index is an integer index whose numeric value i is in
# the range +0 ≤ i < 2^32-1.
#
# Property keys are used to access properties and their values. There are two kinds of access for properties: get and
# set, corresponding to value retrieval and assignment, respectively. The properties accessible via get and set access
# includes both own properties that are a direct part of an object and inherited properties which are provided by
# another associated object via a property inheritance relationship. Inherited properties may be either own or
# inherited properties of the associated object. Each own property of an object must each have a key value that is
# distinct from the key values of the other own properties of that object.
#
# All objects are logically collections of properties, but there are multiple forms of objects that differ in their
# semantics for accessing and manipulating their properties. Ordinary objects are the most common form of objects and
# have the default object semantics. An exotic object is any form of object whose property semantics differ in any way
# from the default semantics.

class JSObject:
    class Property:
        def __init__(self, **kwargs):
            for key, val in kwargs.items():
                setattr(self, key, val)
        def __repr__(self):
            return 'Property(' + ', '.join(f'{name}={getattr(self,name)!r}' for name in ['Get', 'Set', 'value', 'writable', 'enumerable', 'configurable'] if hasattr(self, name)) + ')'

    def __init__(self):
        self.Prototype = JSNull.NULL
        self.Extensible = False
        self.properties = {}

    # 9.1.1 [[GetPrototypeOf]] ( )
    def GetPrototypeOf(self):
        """Determine the object that provides inherited properties for this object. A null value indicates that there are no
           inherited properties."""
        # 1. Return O.[[Prototype]].
        return self.Prototype

    # 9.1.2 [[SetPrototypeOf]] ( V )
    def SetPrototypeOf(self, value):
        """Associate this object with another object that provides inherited properties. Passing null indicates that there are
           no inherited properties. Returns true indicating that the operation was completed successfully or false indicating
           that the operation was not successful."""
        # When the abstract operation OrdinarySetPrototypeOf is called with Object O and value V, the following steps
        # are taken:
        # 1. Assert: Either Type(V) is Object or Type(V) is Null.
        assert isObject(value) or isNull(value)
        # 2. Let extensible be O.[[Extensible]].
        extensible = self.Extensible
        # 3. Let current be O.[[Prototype]].
        current = self.Prototype
        # 4. If SameValue(V, current) is true, return true.
        if SameValue(value, current):
            return True
        # 5. If extensible is false, return false.
        if not extensible:
            return False
        # 6. Let p be V.
        p = value
        # 7. Let done be false.
        done = False
        # 8. Repeat, while done is false,
        while not done:
            # a. If p is null, set done to true.
            if isNull(p):
                done = True
            # b. Else if SameValue(p, O) is true, return false.
            elif SameValue(p, self):
                return False
            # c. Else,
            else:
                # i. If p.[[GetPrototypeOf]] is not the ordinary object internal method defined in 9.1.1, set done to
                #    true.
                # There has to be a better way!
                method_repr = str(p.GetPrototypeOf)
                match = re.match(r'<bound method (.*) of <[^>]+>>', method_repr)
                if not (match and match.group(1) == 'JSObject.GetPrototypeOf'):
                    done = True
                # ii. Else, set p to p.[[Prototype]].
                else:
                    p = p.Prototype
        # 9. Set O.[[Prototype]] to V.
        self.Prototype = value
        # 10. Return true.
        return True
        # NOTE
        # The loop in step 8 guarantees that there will be no circularities in any prototype chain that only includes
        # objects that use the ordinary object definitions for [[GetPrototypeOf]] and [[SetPrototypeOf]].

    # 9.1.3 [[IsExtensible]] ( )
    def IsExtensible(self):
        """Determine whether it is permitted to add additional properties to this object."""
        # 1. Return O.[[Extensible]].
        return self.Extensible

    # 9.1.4 [[PreventExtensions]] ( )
    def PreventExtensions(self):
        """Control whether new properties may be added to this object. Returns true if the operation was successful or false if
           the operation was unsuccessful."""
        # 1. Set O.[[Extensible]] to false.
        self.Extensible = False
        # 2. Return true.
        return True

    # 9.1.5 [[GetOwnProperty]] ( P )
    def GetOwnProperty(self, propkey):
        """Return a Property Descriptor for the own property of this object whose key is propertyKey, or undefined if no such
           property exists."""
        return OrdinaryGetOwnProperty(self, propkey)

    # 9.1.6 [[DefineOwnProperty]] ( P, Desc )
    def DefineOwnProperty(self, propkey, desc):
        """Create or alter the own property, whose key is propertyKey, to have the state described by PropertyDescriptor.
           Return true if that property was successfully created/updated or false if the property could not be created or
           updated."""
        return OrdinaryDefineOwnProperty(self, propkey, desc)

    # 9.1.7 [[HasProperty]] ( P )
    def HasProperty(self, propkey):
        """Return a Boolean value indicating whether this object already has either an own or inherited property whose key is
           propertyKey."""
        # 1. Assert: IsPropertyKey(P) is true.
        assert IsPropertyKey(propkey)
        # 2. Let hasOwn be ? O.[[GetOwnProperty]](P).
        has_own = self.GetOwnProperty(propkey)
        # 3. If hasOwn is not undefined, return true.
        if has_own is not None:
            return True
        # 4. Let parent be ? O.[[GetPrototypeOf]]().
        parent = self.GetPrototypeOf()
        # 5. If parent is not null, then
        #    a. Return ? parent.[[HasProperty]](P).
        # 6. Return false.
        return not isNull(parent) and parent.HasProperty(propkey)

    # 9.1.8 [[Get]] ( P, Receiver )
    def Get(self, propkey, receiver):
        """Return the value of the property whose key is propertyKey from this object. If any ECMAScript code must be executed
           to retrieve the property value, Receiver is used as the this value when evaluating the code."""
        # When the abstract operation OrdinaryGet is called with Object O, property key P, and ECMAScript language
        # value Receiver, the following steps are taken:
        #
        # 1. Assert: IsPropertyKey(P) is true.
        assert IsPropertyKey(propkey)
        # 2. Let desc be ? O.[[GetOwnProperty]](P).
        desc = self.GetOwnProperty(propkey)
        # 3. If desc is undefined, then
        if desc is None:
            # a. Let parent be ? O.[[GetPrototypeOf]]().
            parent = self.GetPrototypeOf()
            # b. If parent is null, return undefined.
            if isNull(parent):
                return None
            # c. Return ? parent.[[Get]](P, Receiver).
            return parent.Get(propkey, receiver)
        # 4. If IsDataDescriptor(desc) is true, return desc.[[Value]].
        if desc.is_data_descriptor():
            return desc.value
        # 5. Assert: IsAccessorDescriptor(desc) is true.
        assert desc.is_accessor_descriptor()
        # 6. Let getter be desc.[[Get]].
        getter = desc.Get
        # 7. If getter is undefined, return undefined.
        if getter is None:
            return None
        # 8. Return ? Call(getter, Receiver).
        return Call(getter, receiver)

    # 9.1.9 [[Set]] ( P, V, Receiver )
    def Set(self, propkey, value, receiver):
        """Set the value of the property whose key is propertyKey to value. If any ECMAScript code must be executed to set the
           property value, Receiver is used as the this value when evaluating the code. Returns true if the property value was
           set or false if it could not be set."""
        # When the [[Set]] internal method of O is called with property key P, value V, and ECMAScript language value
        # Receiver, the following steps are taken:
        #
        # 1. Return ? OrdinarySet(O, P, V, Receiver).
        return OrdinarySet(self, propkey, value, receiver)

    # 9.1.10 [[Delete]] ( P )
    def Delete(self, propkey):
        """Remove the own property whose key is propertyKey from this object. Return false if the property was not deleted and
           is still present. Return true if the property was deleted or is not present."""
        # When the [[Delete]] internal method of O is called with property key P, the following steps are taken:
        #
        # 1. Return ? OrdinaryDelete(O, P).
        return OrdinaryDelete(self, propkey)

    # 9.1.11 [[OwnPropertyKeys]] ( )
    def OwnPropertyKeys(self):
        """Return a List whose elements are all of the own property keys for the object."""
        # When the [[OwnPropertyKeys]] internal method of O is called, the following steps are taken:
        #
        # 1. Return ! OrdinaryOwnPropertyKeys(O).
        return OrdinaryOwnPropertyKeys(self)

# 9.1.6.3 ValidateAndApplyPropertyDescriptor ( O, P, extensible, Desc, current )
def ValidateAndApplyPropertyDescriptor(obj, propkey, extensible, desc, current):
    # When the abstract operation ValidateAndApplyPropertyDescriptor is called with Object O, property key P, Boolean
    # value extensible, and Property Descriptors Desc, and current, the following steps are taken:
    #
    # NOTE
    # If undefined is passed as O, only validation is performed and no object updates are performed.
    #
    # 1. Assert: If O is not undefined, then IsPropertyKey(P) is true.
    assert obj is None or IsPropertyKey(propkey)
    # 2. If current is undefined, then
    if current is None:
        # a. If extensible is false, return false.
        if not extensible:
            return False
        # b. Assert: extensible is true.
        # c. If IsGenericDescriptor(Desc) is true or IsDataDescriptor(Desc) is true, then
        if IsDataDescriptor(desc) or IsGenericDescriptor(desc):
            # i. If O is not undefined, create an own data property named P of object O whose [[Value]], [[Writable]],
            #    [[Enumerable]] and [[Configurable]] attribute values are described by Desc. If the value of an
            #    attribute field of Desc is absent, the attribute of the newly created property is set to its default
            #    value.
            if obj is not None:
                newprop = JSObject.Property(
                    value = desc.value if hasattr(desc, 'value') else None,
                    writable = desc.writable if hasattr(desc, 'writable') else False,
                    enumerable = desc.enumerable if hasattr(desc, 'enumerable') else False,
                    configurable = desc.configurable if hasattr(desc, 'configurable') else False)
                obj.properties[propkey] = newprop
        # d. Else Desc must be an accessor Property Descriptor,
        else:
            # i. If O is not undefined, create an own accessor property named P of object O whose [[Get]], [[Set]],
            #    [[Enumerable]] and [[Configurable]] attribute values are described by Desc. If the value of an
            #    attribute field of Desc is absent, the attribute of the newly created property is set to its default
            #    value.
            if obj is not None:
                newprop = JSObject.Property(
                    Get = desc.Get if hasattr(desc, 'Get') else None,
                    Set = desc.Set if hasattr(desc, 'Set') else None,
                    enumerable = desc.enumerable if hasattr(desc, 'enumerable') else False,
                    configurable = desc.configurable if hasattr(desc, 'configurable') else False)
                obj.properties[propkey] = newprop
        # e. Return true.
        return True
    # 3. If every field in Desc is absent, return true.
    if not any(hasattr(desc, field) for field in ['value', 'writable', 'Get', 'Set', 'enumerable', 'configurable']):
        return True
    # 4. If current.[[Configurable]] is false, then
    if not current.configurable:
        # a. If Desc.[[Configurable]] is present and its value is true, return false.
        if hasattr(desc, 'configurable') and desc.configurable:
            return False
        # b. If Desc.[[Enumerable]] is present and the [[Enumerable]] fields of current and Desc are the Boolean
        #    negation of each other, return false.
        if hasattr(desc, 'enumerable') and desc.enumerable != current.enumerable:
            return False
    # 5. If IsGenericDescriptor(Desc) is true, no further validation is required.
    if IsGenericDescriptor(desc):
        pass
    # 6. Else if IsDataDescriptor(current) and IsDataDescriptor(Desc) have different results, then
    elif IsDataDescriptor(current) != IsDataDescriptor(desc):
        # a. If current.[[Configurable]] is false, return false.
        if not current.configurable:
            return False
        # b. If IsDataDescriptor(current) is true, then
        if IsDataDescriptor(current):
            # i. If O is not undefined, convert the property named P of object O from a data property to an accessor
            #    property. Preserve the existing values of the converted property's [[Configurable]] and [[Enumerable]]
            #    attributes and set the rest of the property's attributes to their default values.
            if obj is not None:
                obj.properties[propkey].Get = None
                obj.properties[propkey].Set = None
                del obj.properties[propkey].value
                del obj.properties[propkey].writable
        # c. Else,
        else:
            # i. If O is not undefined, convert the property named P of object O from an accessor property to a data
            #    property. Preserve the existing values of the converted property's [[Configurable]] and [[Enumerable]]
            #    attributes and set the rest of the property's attributes to their default values.
            if obj is not None:
                obj.properties[propkey].value = None
                obj.properties[propkey].writable = False
                del obj.properties[propkey].Get
                del obj.properties[propkey].Set
    # 7. Else if IsDataDescriptor(current) and IsDataDescriptor(Desc) are both true, then
    elif IsDataDescriptor(current) and IsDataDescriptor(desc):
        # a. If current.[[Configurable]] is false and current.[[Writable]] is false, then
        if not current.configurable and not current.writable:
            # i. If Desc.[[Writable]] is present and Desc.[[Writable]] is true, return false.
            if hasattr(desc, 'writable') and desc.writable:
                return False
            # ii. If Desc.[[Value]] is present and SameValue(Desc.[[Value]], current.[[Value]]) is false, return false.
            if hasattr(desc, 'value') and not SameValue(desc.value, current.value):
                return False
            # iii. Return true.
            return True
    # 8. Else IsAccessorDescriptor(current) and IsAccessorDescriptor(Desc) are both true,
    else:
        # a. If current.[[Configurable]] is false, then
        if not current.configurable:
            # i. If Desc.[[Set]] is present and SameValue(Desc.[[Set]], current.[[Set]]) is false, return false.
            if hasattr(desc, 'Set') and not SameValue(desc.Set, current.Set):
                return False
            # ii. If Desc.[[Get]] is present and SameValue(Desc.[[Get]], current.[[Get]]) is false, return false.
            if hasattr(desc, 'Get') and not SameValue(desc.Get, current.Get):
                return False
            # iii. Return true.
            return True
    # 9. If O is not undefined, then
    if obj is not None:
        # a. For each field of Desc that is present, set the corresponding attribute of the property named P of object
        #    O to the value of the field.
        for fieldname in (f for f in ['value', 'writable', 'Get', 'Set', 'configurable', 'enumerable']
                          if hasattr(desc, f)):
            setattr(obj.properties[propkey], fieldname, getattr(desc, fieldname))
    # 10. Return true.
    return True

# 9.1.9.1 OrdinarySet ( O, P, V, Receiver )
def OrdinarySet(obj, propkey, value, receiver):
    # When the abstract operation OrdinarySet is called with Object O, property key P, value V, and ECMAScript language
    # value Receiver, the following steps are taken:
    #
    # 1. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(propkey)
    # 2. Let ownDesc be ? O.[[GetOwnProperty]](P).
    own_desc = obj.GetOwnProperty(propkey)
    # 3. Return OrdinarySetWithOwnDescriptor(O, P, V, Receiver, ownDesc).
    return OrdinarySetWithOwnDescriptor(obj, propkey, value, receiver, own_desc)

# 9.1.9.2 OrdinarySetWithOwnDescriptor ( O, P, V, Receiver, ownDesc )
def OrdinarySetWithOwnDescriptor(obj, propkey, value, receiver, own_desc):
    # When the abstract operation OrdinarySetWithOwnDescriptor is called with Object O, property key P, value V,
    # ECMAScript language value Receiver, and Property Descriptor (or undefined) ownDesc, the following steps are
    # taken:
    #
    # 1. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(propkey)
    # 2. If ownDesc is undefined, then
    if own_desc is None:
        # a. Let parent be ? O.[[GetPrototypeOf]]().
        parent = obj.GetPrototypeOf()
        # b. If parent is not null, then
        if not isNull(parent):
            # i. Return ? parent.[[Set]](P, V, Receiver).
            return parent.Set(propkey, value, receiver)
        # c. Else,
        else:
            # i. Set ownDesc to the PropertyDescriptor { [[Value]]: undefined, [[Writable]]: true,
            #    [[Enumerable]]: true, [[Configurable]]: true }.
            own_desc = PropertyDescriptor()
            own_desc.value = None
            own_desc.writable = True
            own_desc.enumerable = True
            own_desc.configurable = True
    # 3. If IsDataDescriptor(ownDesc) is true, then
    if own_desc.is_data_descriptor():
        # a. If ownDesc.[[Writable]] is false, return false.
        if not own_desc.writable:
            return False
        # b. If Type(Receiver) is not Object, return false.
        if not isObject(receiver):
            return False
        # c. Let existingDescriptor be ? Receiver.[[GetOwnProperty]](P).
        existing_descriptor = receiver.GetOwnProperty(propkey)
        # d. If existingDescriptor is not undefined, then
        if existing_descriptor is not None:
            # i. If IsAccessorDescriptor(existingDescriptor) is true, return false.
            if existing_descriptor.is_accessor_descriptor():
                return False
            # ii. If existingDescriptor.[[Writable]] is false, return false.
            if not existing_descriptor.writable:
                return False
            # iii. Let valueDesc be the PropertyDescriptor { [[Value]]: V }.
            value_desc = PropertyDescriptor()
            value_desc.value = value
            # iv. Return ? Receiver.[[DefineOwnProperty]](P, valueDesc).
            return receiver.DefineOwnProperty(propkey, value_desc)
        # e. Else Receiver does not currently have a property P,
        else:
            # i. Return ? CreateDataProperty(Receiver, P, V).
            return CreateDataProperty(receiver, propkey, value)
    # 4. Assert: IsAccessorDescriptor(ownDesc) is true.
    assert own_desc.is_accessor_descriptor()
    # 5. Let setter be ownDesc.[[Set]].
    setter = own_desc.Set
    # 6. If setter is undefined, return false.
    if setter is None:
        return False
    # 7. Perform ? Call(setter, Receiver, « V »).
    Call(setter, receiver, [value])
    # 8. Return true.
    return True

# 9.1.10.1 OrdinaryDelete ( O, P )
def OrdinaryDelete(obj, propkey):
    # When the abstract operation OrdinaryDelete is called with Object O and property key P, the following steps are
    # taken:
    #
    assert IsPropertyKey(propkey)                           # 1. Assert: IsPropertyKey(P) is true.
    desc = obj.GetOwnProperty(propkey)                      # 2. Let desc be ? O.[[GetOwnProperty]](P).
    if desc is None:                                        # 3. If desc is undefined, return true.
        return True                                         #
    if desc.configurable:                                   # 4. If desc.[[Configurable]] is true, then
        del obj.properties[propkey]                         #    a. Remove the own property with name P from O.
        return True                                         #    b. Return true.
    return False                                            # 5. Return false.

def isIntegerIndex(key):
    # An integer index is a String-valued property key that is a canonical numeric String (see 7.1.16) and whose
    # numeric value is either +0 or a positive integer ≤ 2^53-1.
    if not isString(key):
        return False
    n = CanonicalNumericIndexString(key)
    return n is not None and IsInteger(n) and math.copysign(1.0, n) == 1.0 and n <= 2**53-1

# 9.1.11.1 OrdinaryOwnPropertyKeys ( O )
def OrdinaryOwnPropertyKeys(obj):
    # When the abstract operation OrdinaryOwnPropertyKeys is called with Object O, the following steps are taken:
    #
    # 1. Let keys be a new empty List.
    keys = []
    # 2. For each own property key P of O that is an integer index, in ascending numeric index order, do
    index_keys = [key for key in obj.properties.keys() if isIntegerIndex(key)]
    index_keys.sort(key=lambda k: CanonicalNumericIndexString(k))
    for p in index_keys:
        # a. Add P as the last element of keys.
        keys.append(p)
    # 3. For each own property key P of O that is a String but is not an integer index, in ascending chronological
    # order of property creation, do
    for p in (key for key in obj.properties.keys() if isString(key) and not isIntegerIndex(key)):
        # a. Add P as the last element of keys.
        keys.append(p)
    # 4. For each own property key P of O that is a Symbol, in ascending chronological order of property creation, do
    for p in (key for key in obj.properties.keys() if isSymbol(key)):
        # a. Add P as the last element of keys.
        keys.append(p)
    # 5. Return keys.
    return keys

# 9.1.12 ObjectCreate ( proto [ , internalSlotsList ] )
def ObjectCreate(proto, internal_slots_list=[]):
    """Creates a new ECMAScript object.

    Arguments:
       proto: This is the object's prototype, and is stored in the internal
           [[Prototype]] slot.
       internal_slots_list: This is a list of strings which are additional "slots"
           to add to the object. They share the same namespace as the rest of the
           object's internal members, so please tread lightly. (Adding "Prototype"
           to the list, for instance, while not necessarily harmful, does **not**
           mean that there are now two [[Prototype]] slots. There's still only one.)

    Returns the created object.

    The object is an "ordinary" object, in that it inherits all the standard
    internal object methods.

    The object is also marked as Extensible; that is: additional properties may be
    added to it. If this is not desired, use the [[PreventExtensions]] method to
    disable further changes. (A typical use case is to expand as you need during
    initialization and then prevent further changes.)

    This function is defined in section 9.1.12 of the ECMAScript specification.
    """
    # The abstract operation ObjectCreate with argument proto (an object or null) is used to specify the runtime
    # creation of new ordinary objects. The optional argument internalSlotsList is a List of the names of additional
    # internal slots that must be defined as part of the object. If the list is not provided, a new empty List is used.
    # This abstract operation performs the following steps:
    #
    # 1. If internalSlotsList is not present, set internalSlotsList to a new empty List.
    # 2. Let obj be a newly created object with an internal slot for each name in internalSlotsList.
    obj = JSObject()
    for name in internal_slots_list:
        setattr(obj, name, None)
    # 3. Set obj's essential internal methods to the default ordinary object definitions specified in 9.1.
    # 4. Set obj.[[Prototype]] to proto.
    obj.Prototype = proto
    # 5. Set obj.[[Extensible]] to true.
    obj.Extensible = True
    # 6. Return obj.
    return obj

# 9.1.13 OrdinaryCreateFromConstructor ( constructor, intrinsicDefaultProto [ , internalSlotsList ] )
def OrdinaryCreateFromConstructor(constructor, intrinsic_default_proto, internal_slots_list=[]):
    """Create a new ECMAScript object, using the constructor's prototype.

    Arguments:
       constructor: An object, hopefully with a "prototype" property.
       intrinsic_default_proto: The string name of an intrinsic property, used to
          supply a prototype if the constructor didn't have one.
       internal_slots_list: This is a list of strings which are additional "slots"
           to add to the object. They share the same namespace as the rest of the
           object's internal members, so please tread lightly. (Adding "Prototype"
           to the list, for instance, while not necessarily harmful, does **not**
           mean that there are now two [[Prototype]] slots. There's still only one.)

    Returns the created object.

    The [[Prototype]] intenal slot of the new object is set to:
       * The value of the constructor's "prototype" property, if it exists
       * The value of the intrinsic object named by intrinsic_default_proto. (Note
         that the realm used to find the intrinsic is the one associated with the
         constructor, not the running execution context.)

    The [[Extensible]] internal slot is set to True. (In other words, the object is
    extensible.)

    Beyond that, the object is completely normal. In particular, any function body
    connected to the constructor is **not** run.

    (This function is typically used in the built-in constructors for objects as
    the first step in the build process.)

    This function is defined in section 9.1.13 of the ECMAScript specificiation.
    """
    # The abstract operation OrdinaryCreateFromConstructor creates an ordinary object whose [[Prototype]] value is
    # retrieved from a constructor's prototype property, if it exists. Otherwise the intrinsic named by
    # intrinsicDefaultProto is used for [[Prototype]]. The optional internalSlotsList is a List of the names of
    # additional internal slots that must be defined as part of the object. If the list is not provided, a new empty
    # List is used. This abstract operation performs the following steps:
    #
    # 1. Assert: intrinsicDefaultProto is a String value that is this specification's name of an intrinsic object. The
    #    corresponding object must be an intrinsic that is intended to be used as the [[Prototype]] value of an object.
    # 2. Let proto be ? GetPrototypeFromConstructor(constructor, intrinsicDefaultProto).
    proto = GetPrototypeFromConstructor(constructor, intrinsic_default_proto)
    # 3. Return ObjectCreate(proto, internalSlotsList).
    return ObjectCreate(proto, internal_slots_list)

# 9.1.14 GetPrototypeFromConstructor ( constructor, intrinsicDefaultProto )
def GetPrototypeFromConstructor(constructor, intrinsic_default_proto):
    """Determine the [[Prototype]] to create an object from a constructor.

    This operation determines the [[Prototype]] value that should be used to create
    an object corresponding to a specific constructor. The value is retrieved from
    the constructor's "prototype" property, if it exists. Otherwise the intricsic
    named by the intrinsic_default_proto argument is used.

    Arguments:
       constructor: The constructor to create an object from
       intrinsic_default_proto: The string name of the intrinsic prototype to use if
          the constructor itself is unable to provide one.

    Returns the prototype object.

    Note: If "constructor" does not supply a [[Prototype]] value, the default value
    that is used is obtained from the realm of the constructor function rather than
    from the running execution context.

    This process is described in section 9.1.14 of the ECMAScript specification.
    """
    # The abstract operation GetPrototypeFromConstructor determines the [[Prototype]] value that should be used to
    # create an object corresponding to a specific constructor. The value is retrieved from the constructor's prototype
    # property, if it exists. Otherwise the intrinsic named by intrinsicDefaultProto is used for [[Prototype]]. This
    # abstract operation performs the following steps:
    #
    # 1. Assert: intrinsicDefaultProto is a String value that is this specification's name of an intrinsic object. The
    #    corresponding object must be an intrinsic that is intended to be used as the [[Prototype]] value of an object.
    # 2. Assert: IsCallable(constructor) is true.
    assert IsCallable(constructor)
    # 3. Let proto be ? Get(constructor, "prototype").
    proto = Get(constructor, 'prototype')
    # 4. If Type(proto) is not Object, then
    if not isObject(proto):
        # a. Let realm be ? GetFunctionRealm(constructor).
        realm = GetFunctionRealm(constructor)
        # b. Set proto to realm's intrinsic object named intrinsicDefaultProto.
        proto = realm.intrinsics[intrinsic_default_proto]
    # 5. Return proto.
    return proto
    # NOTE
    # If constructor does not supply a [[Prototype]] value, the default value that is used is obtained from the realm of
    # the constructor function rather than from the running execution context.

def isObject(arg):
    return isinstance(arg, JSObject)

def isEcmaValue(arg):
    """Returns True if 'arg' is a ECMAScript Value."""
    return isinstance(arg, (type(None), str, int, float, bool, JSSymbol, JSNull, JSObject))

def TypeOf(arg):
    if isUndefined(arg):
        return JSType.UNDEFINED
    if isNull(arg):
        return JSType.NULL
    if isString(arg):
        return JSType.STRING
    if isNumber(arg):
        return JSType.NUMBER
    if isBoolean(arg):
        return JSType.BOOLEAN
    if isSymbol(arg):
        return JSType.SYMBOL
    if isObject(arg):
        return JSType.OBJECT
    return JSType.ILLEGAL

# 6.2.1 The List and Record Specification Types
#
# The List type is used to explain the evaluation of argument lists (see 12.3.6) in new expressions, in function calls,
# and in other algorithms where a simple ordered list of values is needed. Values of the List type are simply ordered
# sequences of list elements containing the individual values. These sequences may be of any length. The elements of a
# list may be randomly accessed using 0-origin indices. For notational convenience an array-like syntax can be used to
# access List elements. For example, arguments[2] is shorthand for saying the 3rd element of the List arguments.
#
# For notational convenience within this specification, a literal syntax can be used to express a new List value. For
# example, « 1, 2 » defines a List value that has two elements each of which is initialized to a specific value. A new
# empty List can be expressed as « ».
#
# The Record type is used to describe data aggregations within the algorithms of this specification. A Record type
# value consists of one or more named fields. The value of each field is either an ECMAScript value or an abstract
# value represented by a name associated with the Record type. Field names are always enclosed in double brackets, for
# example [[Value]].
#
# For notational convenience within this specification, an object literal-like syntax can be used to express a Record
# value. For example, { [[Field1]]: 42, [[Field2]]: false, [[Field3]]: empty } defines a Record value that has three
# fields, each of which is initialized to a specific value. Field name order is not significant. Any fields that are
# not explicitly listed are considered to be absent.
#
# In specification text and algorithms, dot notation may be used to refer to a specific field of a Record value. For
# example, if R is the record shown in the previous paragraph then R.[[Field2]] is shorthand for “the field of R named
# [[Field2]]”.
#
# Schema for commonly used Record field combinations may be named, and that name may be used as a prefix to a literal
# Record value to identify the specific kind of aggregations that is being described. For example: PropertyDescriptor
# { [[Value]]: 42, [[Writable]]: false, [[Configurable]]: true }.
class Record:
    def __init__(self, **kwargs):
        for key, val in kwargs.items():
            setattr(self, key, val)


# Section 6.2.3
#
# The Completion Record Specification Type
#
# The Completion type is a Record used to explain the runtime propagation of values and control flow such as the
# behaviour of statements (break, continue, return and throw) that perform nonlocal transfers of control.
# Values of the Completion type are Record values whose fields are defined as by Table 8. Such values are referred to as
# Completion Records.
#
# Table 8: Completion Record Fields
#
# | Field Name | Value                                            | Meaning  |
# | [[Type]]   | One of normal, break, continue, return, or throw | The type of completion that occurred.
# | [[Value]]  | any ECMAScript language value or empty           | The value that was produced.
# | [[Target]] | any ECMAScript string or empty                   | The target label for directed control transfers.

# The term "abrupt completion" refers to any completion with a [[Type]] value other than normal.

# 6.3.2.4 UpdateEmpty
def UpdateEmpty(cr, value):
    # The abstract operation UpdateEmpty with arguments completionRecord and value performs the following steps:

    # 1. Assert: If completionRecord.[[Type]] is either return or throw, then completionRecord.[[Value]] is not empty.
    #assert cr.value != Empty.EMPTY if cr.ctype in [CompletionType.RETURN, CompletionType.THROW] else True

    # 2. If completionRecord.[[Value]] is not empty, return Completion(completionRecord).
    #if cr.value != Empty.EMPTY:
    #    return Completion(cr.ctype, cr.value, cr.target)

    # 3. Return Completion { [[Type]]: completionRecord.[[Type]], [[Value]]: value,
    #                        [[Target]]: completionRecord.[[Target]] }.
    #return Completion(cr.ctype, value, cr.target)
    if cr != Empty.EMPTY:
        return cr
    return value


# Section 6.2.4
#
# The Reference Specification Type
#
# NOTE  | The Reference type is used to explain the behaviour of such operators as delete, typeof, the assignment
#       | operators, the super keyword and other language features. For example, the left-hand operand of an assignment
#       | is expected to produce a reference.
#
# A Reference is a resolved name or property binding. A Reference consists of three components, the base value
# component, the referenced name component, and the Boolean-valued strict reference flag. The base value component is
# either undefined, an Object, a Boolean, a String, a Symbol, a Number, or an Environment Record. A base value
# component of undefined indicates that the Reference could not be resolved to a binding. The referenced name component
# is a String or Symbol value.
#
# A Super Reference is a Reference that is used to represent a name binding that was expressed using the super keyword.
# A Super Reference has an additional thisValue component, and its base value component will never be an Environment
# Record.

class Reference:
    def __init__(self, base, name, strict):
        self.base = base
        self.name = name
        self.strict = strict
    def __repr__(self):
        return f'Reference({"S " if self.strict else ""}{self.name!r}, base={self.base!r})'
class SuperReference(Reference):
    def __init__(self, base, name, strict, this_value):
        super().__init__(base, name, strict)
        self.this_value = this_value
    def __repr__(self):
        return f'SuperReference({"S " if self.strict else ""}{self.name!r}, base={self.base!r}, thisValue={self.this_value!r})'

# 6.2.4.1 GetBase ( V )
def GetBase(value):
    # 1. Assert: Type(V) is Reference.
    assert isinstance(value, Reference)
    # 2. Return the base value component of V.
    return value.base

# 6.2.4.2 GetReferencedName ( V )
def GetReferencedName(value):
    # 1. Assert: Type(V) is Reference.
    assert isinstance(value, Reference)
    # 2. Return the referenced name component of V.
    return value.name

# 6.2.4.3 IsStrictReference ( V )
def IsStrictReference(value):
    # 1. Assert: Type(V) is Reference.
    assert isinstance(value, Reference)
    # 2. Return the strict reference flag of V.
    return value.strict

# 6.2.4.4 HasPrimitiveBase ( V )
def HasPrimitiveBase(value):
    # 1. Assert: Type(V) is Reference.
    assert isinstance(value, Reference)
    # 2. If Type(V's base value component) is Boolean, String, Symbol, or Number, return true; otherwise return false.
    return isBoolean(value.base) or isString(value.base) or isSymbol(value.base) or isNumber(value.base)

# 6.2.4.5 IsPropertyReference ( V )
def IsPropertyReference(value):
    # 1. Assert: Type(V) is Reference.
    assert isinstance(value, Reference)
    # 2. If either the base value component of V is an Object or HasPrimitiveBase(V) is true, return true; otherwise
    #    return false.
    return isObject(value.base) or HasPrimitiveBase(value)

# 6.2.4.6 IsUnresolvableReference ( V )
def IsUnresolvableReference(value):
    # 1. Assert: Type(V) is Reference.
    assert isinstance(value, Reference)
    # 2. If the base value component of V is undefined, return true; otherwise return false.
    return value.base is None

# 6.2.4.7 IsSuperReference ( V )
def IsSuperReference(value):
    # 1. Assert: Type(V) is Reference.
    assert isinstance(value, Reference)
    # 2. If V has a thisValue component, return true; otherwise return false.
    return hasattr(value, 'this_value')

# 6.2.4.8 GetValue ( V )
def GetValue(value):
    # 1. ReturnIfAbrupt(V).
    # 2. If Type(V) is not Reference, return V.
    if not isinstance(value, Reference):
        return value
    # 3. Let base be GetBase(V).
    base = GetBase(value)
    # 4. If IsUnresolvableReference(V) is true, throw a ReferenceError exception.
    if IsUnresolvableReference(value):
        raise ESReferenceError(f'\'{GetReferencedName(value)}\': unknown')
    # 5. If IsPropertyReference(V) is true, then
    if IsPropertyReference(value):
        # a. If HasPrimitiveBase(V) is true, then
        if HasPrimitiveBase(value):
            # i. Assert: In this case, base will never be undefined or null.
            assert base is not None and not isNull(base)
            # ii. Set base to ! ToObject(base).
            base = ToObject(base)
        # b. Return ? base.[[Get]](GetReferencedName(V), GetThisValue(V)).
        return base.Get(GetReferencedName(value), GetThisValue(value))
    # 6. Else base must be an Environment Record,
    # a. Return ? base.GetBindingValue(GetReferencedName(V), IsStrictReference(V)) (see 8.1.1).
    return base.GetBindingValue(GetReferencedName(value), IsStrictReference(value))
    # NOTE
    # The object that may be created in step 5.a.ii is not accessible outside of the above abstract operation and the
    # ordinary object [[Get]] internal method. An implementation might choose to avoid the actual creation of the
    # object.

# 6.2.4.9 PutValue ( V, W )
def PutValue(ref, value):
    # 1. ReturnIfAbrupt(V).
    # 2. ReturnIfAbrupt(W).
    # 3. If Type(V) is not Reference, throw a ReferenceError exception.
    if not isinstance(ref, Reference):
        raise ESReferenceError(f'Bad Reference: {ref!r}')
    # 4. Let base be GetBase(V).
    base = GetBase(ref)
    # 5. If IsUnresolvableReference(V) is true, then
    if IsUnresolvableReference(ref):
        # a. If IsStrictReference(V) is true, then
        if IsStrictReference(ref):
            # i. Throw a ReferenceError exception.
            raise ESReferenceError(f'Cannot create {GetReferencedName(ref)} in strict context')
        # b. Let globalObj be GetGlobalObject().
        global_obj = GetGlobalObject()
        # c. Return ? Set(globalObj, GetReferencedName(V), W, false).
        return Set(global_obj, GetReferencedName(ref), value, False)
    # 6. Else if IsPropertyReference(V) is true, then
    elif IsPropertyReference(ref):
        # a. If HasPrimitiveBase(V) is true, then
        if HasPrimitiveBase(ref):
            # i. Assert: In this case, base will never be undefined or null.
            assert base is not None and not isNull(base)
            # ii. Set base to ! ToObject(base).
            base = ToObject(base)
        # b. Let succeeded be ? base.[[Set]](GetReferencedName(V), W, GetThisValue(V)).
        succeeded = base.Set(GetReferencedName(ref), value, GetThisValue(ref))
        # c. If succeeded is false and IsStrictReference(V) is true, throw a TypeError exception.
        if not succeeded and IsStrictReference(ref):
            raise ESTypeError(f'Unable to write to {GetReferencedName(ref)}')
        # d. Return.
        return None
    # 7. Else base must be an Environment Record,
    # a. Return ? base.SetMutableBinding(GetReferencedName(V), W, IsStrictReference(V)) (see 8.1.1).
    return base.SetMutableBinding(GetReferencedName(ref), value, IsStrictReference(ref))
    # NOTE
    # The object that may be created in step 6.a.ii is not accessible outside of the above algorithm and the ordinary
    # object [[Set]] internal method. An implementation might choose to avoid the actual creation of that object.

# 6.2.4.10 GetThisValue ( V )
def GetThisValue(ref):
    # 1. Assert: IsPropertyReference(V) is true.
    assert IsPropertyReference(ref)
    # 2. If IsSuperReference(V) is true, then
    if IsSuperReference(ref):
        # a. Return the value of the thisValue component of the reference V.
        return ref.this_value
    # 3. Return GetBase(V).
    return GetBase(ref)

# 6.2.4.11 InitializeReferencedBinding ( V, W )
def InitializeReferencedBinding(ref, value):
    # 1. ReturnIfAbrupt(V).
    # 2. ReturnIfAbrupt(W).
    # 3. Assert: Type(V) is Reference.
    assert isinstance(ref, Reference)
    # 4. Assert: IsUnresolvableReference(V) is false.
    assert not IsUnresolvableReference(ref)
    # 5. Let base be GetBase(V).
    base = GetBase(ref)
    # 6. Assert: base is an Environment Record.
    # 7. Return base.InitializeBinding(GetReferencedName(V), W).
    return base.InitializeBinding(GetReferencedName(ref), value)

# 6.2.5 The Property Descriptor Specification Type
#
# The Property Descriptor type is used to explain the manipulation and reification of Object property attributes. Values
# of the Property Descriptor type are Records. Each field's name is an attribute name and its value is a corresponding
# attribute value as specified in 6.1.7.1. In addition, any field may be present or absent. The schema name used within
# this specification to tag literal descriptions of Property Descriptor records is "PropertyDescriptor".
#
# Property Descriptor values may be further classified as data Property Descriptors and accessor Property Descriptors
# based upon the existence or use of certain fields. A data Property Descriptor is one that includes any fields named
# either [[Value]] or [[Writable]]. An accessor Property Descriptor is one that includes any fields named either [[Get]]
# or [[Set]]. Any Property Descriptor may have fields named [[Enumerable]] and [[Configurable]]. A Property Descriptor
# value may not be both a data Property Descriptor and an accessor Property Descriptor; however, it may be neither. A
# generic Property Descriptor is a Property Descriptor value that is neither a data Property Descriptor nor an accessor
# Property Descriptor. A fully populated Property Descriptor is one that is either an accessor Property Descriptor or a
# data Property Descriptor and that has all of the fields that correspond to the property attributes defined in either
# Table 2 or Table 3.

class PropertyDescriptor(Record):
    def __repr__(self):
        return 'Descriptor(' + ', '.join(f'{name}={getattr(self,name)!r}' for name in ['Get', 'Set', 'value', 'writable', 'enumerable', 'configurable'] if hasattr(self, name)) + ')'

    def is_accessor_descriptor(self):
        "Returns True if this descriptor is an accessor style descriptor."
        # 2. If both Desc.[[Get]] and Desc.[[Set]] are absent, return false.
        # 3. Return true.
        return hasattr(self, 'Get') or hasattr(self, 'Set')

    def is_data_descriptor(self):
        "Returns True if this descriptor is a data style descriptor."
        # 2. If both Desc.[[Value]] and Desc.[[Writable]] are absent, return false.
        # 3. Return true.
        return hasattr(self, 'value') or hasattr(self, 'writable')

    def is_generic_descriptor(self):
        "Returns True if this descriptor cannot be described as either a accessor or data descriptor."
        # 2. If IsAccessorDescriptor(Desc) and IsDataDescriptor(Desc) are both false, return true.
        # 3. Return false.
        return not (self.is_accessor_descriptor() or self.is_data_descriptor())

    def complete_property_descriptor(self):
        "Fills in missing fields for a property descriptor."
        # 2. Let like be Record { [[Value]]: undefined, [[Writable]]: false, [[Get]]: undefined, [[Set]]: undefined,
        #    [[Enumerable]]: false, [[Configurable]]: false }.
        # 3. If IsGenericDescriptor(Desc) is true or IsDataDescriptor(Desc) is true, then
        if self.is_data_descriptor() or self.is_generic_descriptor():
            # a. If Desc does not have a [[Value]] field, set Desc.[[Value]] to like.[[Value]].
            if not hasattr(self, 'value'):
                self.value = None
            # b. If Desc does not have a [[Writable]] field, set Desc.[[Writable]] to like.[[Writable]].
            if not hasattr(self, 'writable'):
                self.writable = False
        # 4. Else,
        else:
            # a. If Desc does not have a [[Get]] field, set Desc.[[Get]] to like.[[Get]].
            if not hasattr(self, 'Get'):
                self.Get = None
            # b. If Desc does not have a [[Set]] field, set Desc.[[Set]] to like.[[Set]].
            if not hasattr(self, 'Set'):
                self.Set = None
        # 5. If Desc does not have an [[Enumerable]] field, set Desc.[[Enumerable]] to like.[[Enumerable]].
        if not hasattr(self, 'enumerable'):
            self.enumerable = False
        # 6. If Desc does not have a [[Configurable]] field, set Desc.[[Configurable]] to like.[[Configurable]].
        if not hasattr(self, 'configurable'):
            self.configurable = False
        # 7. Return Desc.
        return self

# 6.2.5.1 IsAccessorDescriptor ( Desc )
def IsAccessorDescriptor(desc):
    # When the abstract operation IsAccessorDescriptor is called with Property Descriptor Desc, the following steps are
    # taken:
    #
    # 1. If Desc is undefined, return false.
    if desc is None:
        return False
    return desc.is_accessor_descriptor()

# 6.2.5.2 IsDataDescriptor ( Desc )
def IsDataDescriptor(desc):
    # When the abstract operation IsDataDescriptor is called with Property Descriptor Desc, the following steps are
    # taken:
    #
    # 1. If Desc is undefined, return false.
    if desc is None:
        return False
    return desc.is_data_descriptor()

# 6.2.5.3 IsGenericDescriptor ( Desc )
def IsGenericDescriptor(desc):
    # When the abstract operation IsGenericDescriptor is called with Property Descriptor Desc, the following steps are
    # taken:
    #
    # 1. If Desc is undefined, return false.
    if desc is None:
        return False
    return desc.is_generic_descriptor()

# 6.2.5.4 FromPropertyDescriptor ( Desc )
def FromPropertyDescriptor(desc):
    # When the abstract operation FromPropertyDescriptor is called with Property Descriptor Desc, the following steps
    # are taken:
    #
    # 1. If Desc is undefined, return undefined.
    if desc is None:
        return None
    # 2. Let obj be ObjectCreate(%ObjectPrototype%).
    obj = ObjectCreate(surrounding_agent.running_ec.realm.intrinsics['%ObjectPrototype%'])
    # 3. Assert: obj is an extensible ordinary object with no own properties.
    # 4. If Desc has a [[Value]] field, then
    cr = True
    if hasattr(desc, 'value'):
        # a. Perform CreateDataProperty(obj, "value", Desc.[[Value]]).
        cr = CreateDataProperty(obj, 'value', desc.value) and cr
    # 5. If Desc has a [[Writable]] field, then
    if hasattr(desc, 'writable'):
        # a. Perform CreateDataProperty(obj, "writable", Desc.[[Writable]]).
        cr = CreateDataProperty(obj, 'writable', desc.writable) and cr
    # 6. If Desc has a [[Get]] field, then
    if hasattr(desc, 'Get'):
        # a. Perform CreateDataProperty(obj, "get", Desc.[[Get]]).
        cr = CreateDataProperty(obj, 'get', desc.Get) and cr
    # 7. If Desc has a [[Set]] field, then
    if hasattr(desc, 'Set'):
        # a. Perform CreateDataProperty(obj, "set", Desc.[[Set]]).
        cr = CreateDataProperty(obj, 'set', desc.Set) and cr
    # 8. If Desc has an [[Enumerable]] field, then
    if hasattr(desc, 'enumerable'):
        # a. Perform CreateDataProperty(obj, "enumerable", Desc.[[Enumerable]]).
        cr = CreateDataProperty(obj, 'enumerable', desc.enumerable) and cr
    # 9. If Desc has a [[Configurable]] field, then
    if hasattr(desc, 'configurable'):
        # a. Perform CreateDataProperty(obj, "configurable", Desc.[[Configurable]]).
        cr = CreateDataProperty(obj, 'configurable', desc.configurable) and cr
    # 10. Assert: All of the above CreateDataProperty operations return true.
    assert cr
    # 11. Return obj.
    return obj

# 6.2.5.5 ToPropertyDescriptor ( Obj )
def ToPropertyDescriptor(obj):
    # When the abstract operation ToPropertyDescriptor is called with object Obj, the following steps are taken:
    #
    # 1. If Type(Obj) is not Object, throw a TypeError exception.
    if not isObject(obj):
        raise ESTypeError('ToPropertyDescriptor called with non-object')
    # 2. Let desc be a new Property Descriptor that initially has no fields.
    desc = PropertyDescriptor()
    # 3. Let hasEnumerable be ? HasProperty(Obj, "enumerable").
    has_enumerable =HasProperty(obj, 'enumerable')
    # 4. If hasEnumerable is true, then
    if has_enumerable:
        # a. Let enum be ToBoolean(? Get(Obj, "enumerable")).
        enumble = Get(obj, 'enumerable')
        # b. Set desc.[[Enumerable]] to enum.
        desc.enumerable = ToBoolean(enumble)
    # 5. Let hasConfigurable be ? HasProperty(Obj, "configurable").
    has_configurable = HasProperty(obj, 'configurable')
    # 6. If hasConfigurable is true, then
    if has_configurable:
        # a. Let conf be ToBoolean(? Get(Obj, "configurable")).
        conf = Get(obj, 'configurable')
        # b. Set desc.[[Configurable]] to conf.
        desc.configurable = ToBoolean(conf)
    # 7. Let hasValue be ? HasProperty(Obj, "value").
    has_value = HasProperty(obj, 'value')
    # 8. If hasValue is true, then
    if has_value:
        # a. Let value be ? Get(Obj, "value").
        value = Get(obj, 'value')
        # b. Set desc.[[Value]] to value.
        desc.value = value
    # 9. Let hasWritable be ? HasProperty(Obj, "writable").
    has_writable = HasProperty(obj, 'writable')
    # 10. If hasWritable is true, then
    if has_writable:
        # a. Let writable be ToBoolean(? Get(Obj, "writable")).
        writable = Get(obj, 'writable')
        # b. Set desc.[[Writable]] to writable.
        desc.writable = ToBoolean(writable)
    # 11. Let hasGet be ? HasProperty(Obj, "get").
    has_get = HasProperty(obj, 'get')
    # 12. If hasGet is true, then
    if has_get:
        # a. Let getter be ? Get(Obj, "get").
        getter = Get(obj, 'get')
        # b. If IsCallable(getter) is false and getter is not undefined, throw a TypeError exception.
        if not IsCallable(getter) and getter is not None:
            raise ESTypeError('Getter Object not callable')
        # c. Set desc.[[Get]] to getter.
        desc.Get = getter
    # 13. Let hasSet be ? HasProperty(Obj, "set").
    has_set = HasProperty(obj, 'set')
    # 14. If hasSet is true, then
    if has_set:
        # a. Let setter be ? Get(Obj, "set").
        setter = Get(obj, 'set')
        # b. If IsCallable(setter) is false and setter is not undefined, throw a TypeError exception.
        if not IsCallable(setter) and setter is not None:
            raise ESTypeError('Setter object not callable')
        # c. Set desc.[[Set]] to setter.
        desc.Set = setter
    # 15. If desc.[[Get]] is present or desc.[[Set]] is present, then
    if hasattr(desc, 'Get') or hasattr(desc, 'Set'):
        # a. If desc.[[Value]] is present or desc.[[Writable]] is present, throw a TypeError exception.
        if hasattr(desc, 'value') or hasattr(desc, 'writable'):
            raise ESTypeError('ToPropertyDescriptor: Had a declarative vs accessor conflict')
    # 16. Return desc.
    return desc

# 6.2.5.6 CompletePropertyDescriptor ( Desc )
def CompletePropertyDescriptor(desc):
    # When the abstract operation CompletePropertyDescriptor is called with Property Descriptor Desc, the following
    # steps are taken:
    #
    # 1. Assert: Desc is a Property Descriptor.
    assert isinstance(desc, PropertyDescriptor)
    return desc.complete_property_descriptor()


# 7 Abstract Operations
#
# These operations are not a part of the ECMAScript language; they are defined here to solely to aid the specification
# of the semantics of the ECMAScript language. Other, more specialized abstract operations are defined throughout this
# specification.

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
        exotic_to_prim = GetMethod(input, wks_to_primitive)
        # e. If exoticToPrim is not undefined, then
        if exotic_to_prim is not None:
            # i. Let result be ? Call(exoticToPrim, input, � hint �).
            result = Call(exotic_to_prim, input, [preferred_type])
            # ii. If Type(result) is not Object, return result.
            if not isObject(result):
                return result
            # iii. Throw a TypeError exception.
            raise ESTypeError(f'Cannot convert to primitive: {input!r}')
        # f. If hint is "default", set hint to "number".
        if preferred_type == 'default':
            preferred_type = 'number'
        # g. Return ? OrdinaryToPrimitive(input, hint).
        return OrdinaryToPrimitive(input, preferred_type)
    # 3. Return input.
    return input

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
        # a. Let methodNames be [ "toString", "valueOf" ].
        method_names = [ 'toString', 'valueOf' ]
    # 4. Else,
    else:
        # a. Let methodNames be [ "valueOf", "toString" ].
        method_names = ['valueOf', 'toString']
    # 5. For each name in methodNames in List order, do
    for name in method_names:
        # a. Let method be ? Get(O, name).
        method = Get(obj, name)
        # b. If IsCallable(method) is true, then
        if IsCallable(method):
            # i. Let result be ? Call(method, O).
            result = Call(method, obj)
            # ii. If Type(result) is not Object, return result.
            if not isObject(result):
                return result
    # 6. Throw a TypeError exception.
    raise ESTypeError(f'Can\'t convert {obj!r} to {hint}')

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

    # Note: The ToBoolean function does not return a CompletionRecord; it can't fail.

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
    return result

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
        raise ESTypeError('symbols cannot be converted to numbers')
    elif isObject(arg):
        prim_value = ToPrimitive(arg, 'number')
        result = ToNumber(prim_value)
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
    return result

# 7.1.4 ToInteger ( argument )
def ToInteger(arg):
    number = ToNumber(arg)
    if math.isnan(number):
        result = 0
    elif number == 0.0 or abs(number) == math.inf:
        result = number
    else:
        result = math.floor(abs(number))
        if number < 0:
            result = -result
    return result

# 7.1.5 ToInt32 ( argument )
def ToInt32(arg):
    number = ToNumber(arg)
    if math.isnan(number) or number == 0 or abs(number) == math.inf:
        return 0
    this_int = math.floor(abs(number))
    if number < 0:
        this_int = -this_int
    int32bit = this_int % 2**32
    if int32bit >= 2**31:
        return int32bit - 2**32
    return int32bit

# 7.1.6 ToUint32 ( argument )
def ToUint32(arg):
    number = ToNumber(arg)
    if math.isnan(number) or number == 0 or abs(number) == math.inf:
        return 0
    this_int = math.floor(abs(number))
    if number < 0:
        this_int = -this_int
    int32bit = this_int % 2**32
    return int32bit

# 7.1.7 ToInt16 ( argument )
def ToInt16(arg):
    number = ToNumber(arg)
    if math.isnan(number) or number == 0 or abs(number) == math.inf:
        return 0
    this_int = math.floor(abs(number))
    if number < 0:
        this_int = -this_int
    int16bit = this_int % 2**16
    if int16bit >= 2**15:
        return int16bit - 2**16
    return int16bit

# 7.1.8 ToUint16 ( argument )
def ToUint16(arg):
    number = ToNumber(arg)
    if math.isnan(number) or number == 0 or abs(number) == math.inf:
        return 0
    this_int = math.floor(abs(number))
    if number < 0:
        this_int = -this_int
    int16bit = this_int % 2**16
    return int16bit

# 7.1.9 ToInt8 ( argument )
def ToInt8(arg):
    number = ToNumber(arg)
    if math.isnan(number) or number == 0 or abs(number) == math.inf:
        return 0
    this_int = math.floor(abs(number))
    if number < 0:
        this_int = -this_int
    int8bit = this_int % 2**8
    if int8bit >= 2**7:
        return int8bit - 2**8
    return int8bit

# 7.1.10 ToUint8 ( argument )
def ToUint8(arg):
    number = ToNumber(arg)
    if math.isnan(number) or number == 0 or abs(number) == math.inf:
        return 0
    this_int = math.floor(abs(number))
    if number < 0:
        this_int = -this_int
    int8bit = this_int % 2**8
    return int8bit

# 7.1.11 ToUint8Clamp ( argument )
def ToUint8Clamp(arg):
    number = ToNumber(arg)
    if math.isnan(number) or number <= 0:
        return 0
    if number >= 255:
        return 255
    f = math.floor(number)
    if f + 0.5 < number:
        return f+1
    if number < f + 0.5 or f & 1 == 0:
        return f
    return f+1

# 7.1.12 ToString ( argument )
def ToString(arg):
    if isUndefined(arg):
        return 'undefined'
    if isNull(arg):
        return 'null'
    if isBoolean(arg):
        return 'true' if arg else 'false'
    if isNumber(arg):
        return NumberToString(arg)
    if isString(arg):
        return arg
    if isSymbol(arg):
        raise ESTypeError('Symbol not convertable to string')
    # isObject
    prim_value = ToPrimitive(arg, 'string')
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
    return '{:.21g}'.format(m)

# 7.1.13 ToObject ( argument )
def ToObject(argument):
    intrinsics = surrounding_agent.running_ec.realm.intrinsics
    # The abstract operation ToObject converts argument to a value of type Object according to Table 12:
    #
    # Table 12: ToObject Conversions
    # +---------------+-------------------------------------------------------------------------------------------------
    # | Argument Type | Result
    # +---------------+-------------------------------------------------------------------------------------------------
    # | Undefined     | Throw a TypeError exception.
    # +---------------+-------------------------------------------------------------------------------------------------
    # | Null          | Throw a TypeError exception.
    # +---------------+-------------------------------------------------------------------------------------------------
    # | Boolean       | Return a new Boolean object whose [[BooleanData]] internal slot is set to argument. See 19.3
    # |               | for a description of Boolean objects.
    # +---------------+-------------------------------------------------------------------------------------------------
    # | Number        | Return a new Number object whose [[NumberData]] internal slot is set to argument. See 20.1 for
    # |               | a description of Number objects.
    # +---------------+-------------------------------------------------------------------------------------------------
    # | String        | Return a new String object whose [[StringData]] internal slot is set to argument. See 21.1 for
    # |               | a description of String objects.
    # +---------------+-------------------------------------------------------------------------------------------------
    # | Symbol        | Return a new Symbol object whose [[SymbolData]] internal slot is set to argument. See 19.4 for
    # |               | a description of Symbol objects.
    # +---------------+-------------------------------------------------------------------------------------------------
    # | Object        | Return argument.
    # +---------------+-------------------------------------------------------------------------------------------------
    if isBoolean(argument):
        return Construct(intrinsics['%Boolean%'], [argument])
    if isNumber(argument):
        return Construct(intrinsics['%Number%'], [argument])
    if isString(argument):
        return Construct(intrinsics['%String%'], [argument])
    if isSymbol(argument):
        return Construct(intrinsics['%Symbol%'], [argument])
    if isObject(argument):
        return argument
    raise ESTypeError('undefined and null cannot be converted to objects')

# 7.1.14 ToPropertyKey ( argument )
def ToPropertyKey(argument):
    # The abstract operation ToPropertyKey converts argument to a value that can be used as a property key by
    # performing the following steps:
    #
    # 1. Let key be ? ToPrimitive(argument, hint String).
    # 2. If Type(key) is Symbol, then
    #    a. Return key.
    # 3. Return ! ToString(key).
    key = ToPrimitive(argument, 'string')
    if isSymbol(key):
        return key
    return ToString(key)

# 7.1.15 ToLength ( argument )
def ToLength(argument):
    # The abstract operation ToLength converts argument to an integer suitable for use as the length of an array-like
    # object. It performs the following steps:
    #
    #   1. Let len be ? ToInteger(argument).
    #   2. If len ≤ +0, return +0.
    #   3. Return min(len, 2^53-1).
    length = ToInteger(argument)
    if length <= 0:
        return 0
    return min(length, 2**53-1)

# 7.1.16 CanonicalNumericIndexString ( argument )
def CanonicalNumericIndexString(arg):
    # The abstract operation CanonicalNumericIndexString returns argument converted to a numeric value if it is a
    # String representation of a Number that would be produced by ToString, or the string "-0". Otherwise, it returns
    # undefined. This abstract operation functions as follows:
    #
    # 1. Assert: Type(argument) is String.
    assert isString(arg)
    # 2. If argument is "-0", return -0.
    if arg == '-0':
        return -0.0
    # 3. Let n be ! ToNumber(argument).
    n = ToNumber(arg)
    # 4. If SameValue(! ToString(n), argument) is false, return undefined.
    if not SameValue(ToString(n), arg):
        return None
    # 5. Return n.
    return n
    # A canonical numeric string is any String value for which the CanonicalNumericIndexString abstract operation does
    # not return undefined.

#################################################################################################################################################################################################################################################################################################################
#
# 8888888888      .d8888b.      88888888888                   888    d8b                                              888      .d8888b.                                                   d8b                                 .d88888b.                                     888    d8b
#       d88P     d88P  Y88b         888                       888    Y8P                                              888     d88P  Y88b                                                  Y8P                                d88P" "Y88b                                    888    Y8P
#      d88P             888         888                       888                                                     888     888    888                                                                                     888     888                                    888
#     d88P            .d88P         888      .d88b.  .d8888b  888888 888 88888b.   .d88b.       8888b.  88888b.   .d88888     888         .d88b.  88888b.d88b.  88888b.   8888b.  888d888 888 .d8888b   .d88b.  88888b.      888     888 88888b.   .d88b.  888d888  8888b.  888888 888  .d88b.  88888b.  .d8888b
#  88888888       .od888P"          888     d8P  Y8b 88K      888    888 888 "88b d88P"88b         "88b 888 "88b d88" 888     888        d88""88b 888 "888 "88b 888 "88b     "88b 888P"   888 88K      d88""88b 888 "88b     888     888 888 "88b d8P  Y8b 888P"       "88b 888    888 d88""88b 888 "88b 88K
#   d88P         d88P"              888     88888888 "Y8888b. 888    888 888  888 888  888     .d888888 888  888 888  888     888    888 888  888 888  888  888 888  888 .d888888 888     888 "Y8888b. 888  888 888  888     888     888 888  888 88888888 888     .d888888 888    888 888  888 888  888 "Y8888b.
#  d88P      d8b 888"               888     Y8b.          X88 Y88b.  888 888  888 Y88b 888     888  888 888  888 Y88b 888     Y88b  d88P Y88..88P 888  888  888 888 d88P 888  888 888     888      X88 Y88..88P 888  888     Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  888 Y88..88P 888  888      X88
# d88P       Y8P 888888888          888      "Y8888   88888P'  "Y888 888 888  888  "Y88888     "Y888888 888  888  "Y88888      "Y8888P"   "Y88P"  888  888  888 88888P"  "Y888888 888     888  88888P'  "Y88P"  888  888      "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888 888  "Y88P"  888  888  88888P'
#                                                                                      888                                                                      888                                                                      888
#                                                                                 Y8b d88P                                                                      888                                                                      888
#                                                                                  "Y88P"                                                                       888                                                                      888
#
#################################################################################################################################################################################################################################################################################################################
# 7.2.1 RequireObjectCoercible ( argument )
def RequireObjectCoercible(argument):
    # The abstract operation RequireObjectCoercible throws an error if argument is a value that cannot be converted to an Object using ToObject. It is defined by Table 13:
    #
    # Table 13: RequireObjectCoercible Results
    # Argument Type	Result
    # Undefined	Throw a TypeError exception.
    # Null	Throw a TypeError exception.
    # Boolean	Return argument.
    # Number	Return argument.
    # String	Return argument.
    # Symbol	Return argument.
    # Object	Return argument.
    if isNull(argument) or isUndefined(argument):
        raise ESTypeError('Must be a coercible value')
    return argument

# 7.2.2 IsArray ( argument )
def IsArray(arg):
    # The abstract operation IsArray takes one argument argument, and performs the following steps:
    #
    # 1. If Type(argument) is not Object, return false.
    if not isObject(arg):
        return False
    # 2. If argument is an Array exotic object, return true.
    if isinstance(arg, ArrayObject):
        return True
    # 3. If argument is a Proxy exotic object, then
    if isinstance(arg, ProxyObject):
        # a. If argument.[[ProxyHandler]] is null, throw a TypeError exception.
        if isNull(arg.ProxyHandler):
            raise ESTypeError('IsArray: Proxy object is missing a handler')
        # b. Let target be argument.[[ProxyTarget]].
        target = arg.ProxyTarget
        # c. Return ? IsArray(target).
        return IsArray(target)
    # 4. Return false.
    return False

# 7.2.3 IsCallable ( argument )
def IsCallable(arg):
    # The abstract operation IsCallable determines if argument, which must be an ECMAScript language value, is a
    # callable function with a [[Call]] internal method.
    #
    # 1. If Type(argument) is not Object, return false.
    if not isObject(arg):
        return False
    # 2. If argument has a [[Call]] internal method, return true.
    # 3. Return false.
    return hasattr(arg, 'Call')

# 7.2.4 IsConstructor ( argument )
def IsConstructor(arg):
    # The abstract operation IsConstructor determines if argument, which must be an ECMAScript language value, is a
    # function object with a [[Construct]] internal method.
    #
    # 1. If Type(argument) is not Object, return false.
    if not isObject(arg):
        return False
    # 2. If argument has a [[Construct]] internal method, return true.
    # 3. Return false.
    return hasattr(arg, 'Construct')

# 7.2.5 IsExtensible ( O )
def IsExtensible(o_value):
    # The abstract operation IsExtensible is used to determine whether additional properties can be added to the object
    # that is O. A Boolean value is returned. This abstract operation performs the following steps:
    #
    # 1. Assert: Type(O) is Object.
    assert isObject(o_value)
    # 2. Return ? O.[[IsExtensible]]().
    return o_value.IsExtensible()

# 7.2.6 IsInteger ( argument )
def IsInteger(argument):
    # The abstract operation IsInteger determines if argument is a finite integer numeric value.
    #
    # 1. If Type(argument) is not Number, return false.
    if not isNumber(argument):
        return False
    # 2. If argument is NaN, +∞, or -∞, return false.
    if math.isnan(argument) or abs(argument) == math.inf:
        return False
    # 3. If floor(abs(argument)) ≠ abs(argument), return false.
    # 4. Return true.
    return math.floor(abs(argument)) == abs(argument)

# 7.2.7 IsPropertyKey ( argument )
def IsPropertyKey(arg):
    # The abstract operation IsPropertyKey determines if argument, which must be an ECMAScript language value, is a
    # value that may be used as a property key.
    # 1. If Type(argument) is String, return true.
    # 2. If Type(argument) is Symbol, return true.
    # 3. Return false.
    return isString(arg) or isSymbol(arg)

# 7.2.10 SameValue ( x, y )
def SameValue(x, y):
    # The internal comparison abstract operation SameValue(x, y), where x and y are ECMAScript language values,
    # produces true or false. Such a comparison is performed as follows:
    #
    # 1. If Type(x) is different from Type(y), return false.
    if TypeOf(x) != TypeOf(y):
        return False
    # 2. If Type(x) is Number, then
    if isNumber(x):
        # a. If x is NaN and y is NaN, return true.
        if math.isnan(x) and math.isnan(y):
            return True
        # b. If x is +0 and y is -0, return false.
        # c. If x is -0 and y is +0, return false.
        sign_x = math.copysign(1.0, x)
        sign_y = math.copysign(1.0, y)
        if x == 0.0 and y == 0.0 and sign_x != sign_y:
            return False
        # d. If x is the same Number value as y, return true.
        # e. Return false.
        return x == y
    # 3. Return SameValueNonNumber(x, y).
    return SameValueNonNumber(x, y)
    # NOTE
    # This algorithm differs from the Strict Equality Comparison Algorithm in its treatment of signed zeroes and NaNs.

# 7.2.12 SameValueNonNumber ( x, y )
def SameValueNonNumber(x, y):
    # The internal comparison abstract operation SameValueNonNumber(x, y), where neither x nor y are Number values,
    # produces true or false. Such a comparison is performed as follows:
    #
    # 1. Assert: Type(x) is not Number.
    assert not isNumber(x)
    # 2. Assert: Type(x) is the same as Type(y).
    assert TypeOf(x) == TypeOf(y)
    # 3. If Type(x) is Undefined, return true.
    # 4. If Type(x) is Null, return true.
    # 5. If Type(x) is String, then
        # a. If x and y are exactly the same sequence of code units (same length and same code units at corresponding
        #    indices), return true; otherwise, return false.
    # 6. If Type(x) is Boolean, then
        # a. If x and y are both true or both false, return true; otherwise, return false.
    # 7. If Type(x) is Symbol, then
        # a. If x and y are both the same Symbol value, return true; otherwise, return false.
    # 8. If x and y are the same Object value, return true. Otherwise, return false.
    return x == y

# 7.2.13 Abstract Relational Comparison
def AbstractRelationalComparison(x, y, LeftFirst):
    # The comparison x < y, where x and y are values, produces true, false, or undefined (which indicates that at least one
    # operand is NaN). In addition to x and y the algorithm takes a Boolean flag named LeftFirst as a parameter. The flag is
    # used to control the order in which operations with potentially visible side-effects are performed upon x and y. It is
    # necessary because ECMAScript specifies left to right evaluation of expressions. The default value of LeftFirst is true
    # and indicates that the x parameter corresponds to an expression that occurs to the left of the y parameter's
    # corresponding expression. If LeftFirst is false, the reverse is the case and operations must be performed upon y before
    # x. Such a comparison is performed as follows:
    #
    # 1. If the LeftFirst flag is true, then
    if LeftFirst:
        # a. Let px be ? ToPrimitive(x, hint Number).
        px = ToPrimitive(x, 'number')
        # b. Let py be ? ToPrimitive(y, hint Number).
        py = ToPrimitive(y, 'number')
    # 2. Else the order of evaluation needs to be reversed to preserve left to right evaluation,
    else:
        # a. Let py be ? ToPrimitive(y, hint Number).
        py = ToPrimitive(y, 'number')
        # b. Let px be ? ToPrimitive(x, hint Number).
        px = ToPrimitive(x, 'number')
    # 3. If Type(px) is String and Type(py) is String, then
    if isString(px) and isString(py):
        # a. If IsStringPrefix(py, px) is true, return false.
        # b. If IsStringPrefix(px, py) is true, return true.
        # c. Let k be the smallest nonnegative integer such that the code unit at index k within px is different from the code
        #    unit at index k within py. (There must be such a k, for neither String is a prefix of the other.)
        # d. Let m be the integer that is the numeric value of the code unit at index k within px.
        # e. Let n be the integer that is the numeric value of the code unit at index k within py.
        # f. If m < n, return true. Otherwise, return false.
        # -- Steps a-f are what python already does. So this is easy.
        return px < py
    # 4. Else,
    # a. NOTE: Because px and py are primitive values evaluation order is not important.
    # b. Let nx be ? ToNumber(px).
    nx = ToNumber(px)
    # c. Let ny be ? ToNumber(py).
    ny = ToNumber(py)
    # d. If nx is NaN, return undefined.
    # e. If ny is NaN, return undefined.
    if math.isnan(nx) or math.isnan(ny):
        return None
    # f. If nx and ny are the same Number value, return false.
    # g. If nx is +0 and ny is -0, return false.
    # h. If nx is -0 and ny is +0, return false.
    # i. If nx is +∞, return false.
    # j. If ny is +∞, return true.
    # k. If ny is -∞, return false.
    # l. If nx is -∞, return true.
    # m. If the mathematical value of nx is less than the mathematical value of ny—note that these mathematical values are both
    #    finite and not both zero—return true. Otherwise, return false.
    # --- Rules f-m are followed by python, so we let it do the work.
    return nx < ny
    #
    # NOTE 1
    # Step 3 differs from step 7 in the algorithm for the addition operator + (12.8.3) by using the logical-and operation
    # instead of the logical-or operation.
    #
    # NOTE 2
    # The comparison of Strings uses a simple lexicographic ordering on sequences of code unit values. There is no attempt to
    # use the more complex, semantically oriented definitions of character or string equality and collating order defined in
    # the Unicode specification. Therefore String values that are canonically equal according to the Unicode standard could
    # test as unequal. In effect this algorithm assumes that both Strings are already in normalized form. Also, note that for
    # strings containing supplementary characters, lexicographic ordering on sequences of UTF-16 code unit values differs from
    # that on sequences of code point values.

# 7.2.14 Abstract Equality Comparison
def AbstractEqualityComparison(x, y):
    # The comparison x == y, where x and y are values, produces true or false. Such a comparison is performed as follows:
    #
    # 1. If Type(x) is the same as Type(y), then
    if TypeOf(x) == TypeOf(y):
        # a. Return the result of performing Strict Equality Comparison x === y.
        return StrictEqualityComparison(x, y)
    # 2. If x is null and y is undefined, return true.
    if isNull(x) and isUndefined(y):
        return True
    # 3. If x is undefined and y is null, return true.
    if isUndefined(x) and isNull(y):
        return True
    # 4. If Type(x) is Number and Type(y) is String, return the result of the comparison x == ! ToNumber(y).
    if isNumber(x) and isString(y):
        return AbstractEqualityComparison(x, ToNumber(y))
    # 5. If Type(x) is String and Type(y) is Number, return the result of the comparison ! ToNumber(x) == y.
    if isString(x) and isNumber(y):
        return AbstractEqualityComparison(ToNumber(x), y)
    # 6. If Type(x) is Boolean, return the result of the comparison ! ToNumber(x) == y.
    if isBoolean(x):
        return AbstractEqualityComparison(ToNumber(x), y)
    # 7. If Type(y) is Boolean, return the result of the comparison x == ! ToNumber(y).
    if isBoolean(y):
        return AbstractEqualityComparison(x, ToNumber(y))
    # 8. If Type(x) is either String, Number, or Symbol and Type(y) is Object, return the result of the comparison x == ToPrimitive(y).
    if (isString(x) or isNumber(x) or isSymbol(x)) and isObject(y):
        prim = ToPrimitive(y)
        return AbstractEqualityComparison(x, prim)
    # 9. If Type(x) is Object and Type(y) is either String, Number, or Symbol, return the result of the comparison ToPrimitive(x) == y.
    if isObject(x) and (isString(y) or isNumber(y) or isSymbol(y)):
        prim = ToPrimitive(x)
        return AbstractEqualityComparison(prim, y)
    # 10. Return false.
    return False

# 7.2.15 Strict Equality Comparison
def StrictEqualityComparison(x, y):
    # The comparison x === y, where x and y are values, produces true or false. Such a comparison is performed as follows:
    #
    # 1. If Type(x) is different from Type(y), return false.
    if TypeOf(x) != TypeOf(y):
        return False
    # 2. If Type(x) is Number, then
    if isNumber(x):
        # a. If x is NaN, return false.
        # b. If y is NaN, return false.
        # c. If x is the same Number value as y, return true.
        # d. If x is +0 and y is -0, return true.
        # e. If x is -0 and y is +0, return true.
        # f. Return false.
        # Implementation note: This is exactly how the Python == works, so we're good.
        return x == y
    # 3. Return SameValueNonNumber(x, y).
    return SameValueNonNumber(x, y)
    # NOTE
    # This algorithm differs from the SameValue Algorithm in its treatment of signed zeroes and NaNs.
#################################################################################################################################################################################################################
#
# 8888888888      .d8888b.       .d88888b.                                     888    d8b                                                       .d88888b.  888         d8b                   888
#       d88P     d88P  Y88b     d88P" "Y88b                                    888    Y8P                                                      d88P" "Y88b 888         Y8P                   888
#      d88P           .d88P     888     888                                    888                                                             888     888 888                               888
#     d88P           8888"      888     888 88888b.   .d88b.  888d888  8888b.  888888 888  .d88b.  88888b.  .d8888b       .d88b.  88888b.      888     888 88888b.    8888  .d88b.   .d8888b 888888 .d8888b
#  88888888           "Y8b.     888     888 888 "88b d8P  Y8b 888P"       "88b 888    888 d88""88b 888 "88b 88K          d88""88b 888 "88b     888     888 888 "88b   "888 d8P  Y8b d88P"    888    88K
#   d88P         888    888     888     888 888  888 88888888 888     .d888888 888    888 888  888 888  888 "Y8888b.     888  888 888  888     888     888 888  888    888 88888888 888      888    "Y8888b.
#  d88P      d8b Y88b  d88P     Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  888 Y88..88P 888  888      X88     Y88..88P 888  888     Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.       X88
# d88P       Y8P  "Y8888P"       "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888 888  "Y88P"  888  888  88888P'      "Y88P"  888  888      "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888  88888P'
#                                           888                                                                                                                        888
#                                           888                                                                                                                       d88P
#                                           888                                                                                                                     888P"
#
#################################################################################################################################################################################################################
# 7.3.1 Get ( O, P )
def Get(obj, propkey):
    # The abstract operation Get is used to retrieve the value of a specific property of an object. The operation is
    # called with arguments O and P where O is the object and P is the property key. This abstract operation performs
    # the following steps:
    #
    # 1. Assert: Type(O) is Object.
    assert isObject(obj)
    # 2. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(propkey)
    # Return ? O.[[Get]](P, O).
    return obj.Get(propkey, obj)

# 7.3.2 GetV ( V, P )
def GetV(value, propkey):
    # The abstract operation GetV is used to retrieve the value of a specific property of an ECMAScript language value.
    # If the value is not an object, the property lookup is performed using a wrapper object appropriate for the type
    # of the value. The operation is called with arguments V and P where V is the value and P is the property key. This
    # abstract operation performs the following steps:
    #
    # 1. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(propkey)
    # 2. Let O be ? ToObject(V).
    obj = ToObject(value)
    # 3. Return ? O.[[Get]](P, V).
    return obj.Get(propkey, value)

# 7.3.3 Set ( O, P, V, Throw )
def Set(O, P, V, Throw):
    # The abstract operation Set is used to set the value of a specific property of an object. The operation is called with
    # arguments O, P, V, and Throw where O is the object, P is the property key, V is the new value for the property and Throw
    # is a Boolean flag. This abstract operation performs the following steps:
    #
    # 1. Assert: Type(O) is Object.
    assert isObject(O)
    # 2. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(P)
    # 3. Assert: Type(Throw) is Boolean.
    assert isBoolean(Throw)
    # 4. Let success be ? O.[[Set]](P, V, O).
    success = O.Set(P, V, O)
    # 5. If success is false and Throw is true, throw a TypeError exception.
    if not success and Throw:
        raise ESTypeError(f'unable to set property \'{P}\'')
    # 6. Return success.
    return success

# 7.3.4 CreateDataProperty ( O, P, V )
def CreateDataProperty(obj, propkey, value):
    # The abstract operation CreateDataProperty is used to create a new own property of an object. The operation is
    # called with arguments O, P, and V where O is the object, P is the property key, and V is the value for the
    # property. This abstract operation performs the following steps:
    #
    # 1. Assert: Type(O) is Object.
    assert isObject(obj)
    # 2. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(propkey)
    # 3. Let newDesc be the PropertyDescriptor { [[Value]]: V, [[Writable]]: true, [[Enumerable]]: true,
    #    [[Configurable]]: true }.
    new_desc = PropertyDescriptor()
    new_desc.value = value
    new_desc.writable = True
    new_desc.enumerable = True
    new_desc.configurable = True
    # Return ? O.[[DefineOwnProperty]](P, newDesc).
    return obj.DefineOwnProperty(propkey, new_desc)
    # NOTE
    # This abstract operation creates a property whose attributes are set to the same defaults used for properties
    # created by the ECMAScript language assignment operator. Normally, the property will not already exist. If it does
    # exist and is not configurable or if O is not extensible, [[DefineOwnProperty]] will return false.

# 7.3.5 CreateMethodProperty ( O, P, V )
def CreateMethodProperty(obj, propkey, value):
    # The abstract operation CreateMethodProperty is used to create a new own property of an object. The operation is
    # called with arguments O, P, and V where O is the object, P is the property key, and V is the value for the
    # property. This abstract operation performs the following steps:
    #
    # 1. Assert: Type(O) is Object.
    assert isObject(obj)
    # 2. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(propkey)
    # 3. Let newDesc be the PropertyDescriptor { [[Value]]: V, [[Writable]]: true, [[Enumerable]]: false,
    #    [[Configurable]]: true }.
    new_desc = PropertyDescriptor(value=value, writable=True, enumerable=False, configurable=True)
    # 4. Return ? O.[[DefineOwnProperty]](P, newDesc).
    return obj.DefineOwnProperty(propkey, new_desc)
    # NOTE
    # This abstract operation creates a property whose attributes are set to the same defaults used for built-in
    # methods and methods defined using class declaration syntax. Normally, the property will not already exist. If it
    # does exist and is not configurable or if O is not extensible, [[DefineOwnProperty]] will return false.

# 7.3.6 CreateDataPropertyOrThrow ( O, P, V )
def CreateDataPropertyOrThrow(O, P, V):
    # The abstract operation CreateDataPropertyOrThrow is used to create a new own property of an object. It throws a
    # TypeError exception if the requested property update cannot be performed. The operation is called with arguments
    # O, P, and V where O is the object, P is the property key, and V is the value for the property. This abstract
    # operation performs the following steps:
    #
    # 1. Assert: Type(O) is Object.
    # 2. Assert: IsPropertyKey(P) is true.
    # 3. Let success be ? CreateDataProperty(O, P, V).
    # 4. If success is false, throw a TypeError exception.
    # 5. Return success.
    assert isObject(O)
    assert IsPropertyKey(P)
    success = CreateDataProperty(O, P, V)
    if not success:
        raise ESTypeError(f'Cannot create property \'{P}\'')
    return success

def CreateMethodPropertyOrThrow(obj, propkey, value):
    success = CreateMethodProperty(obj, propkey, value)
    if not success:
        raise ESTypeError(f'Cannot create method \'{propkey}\'')
    return success

# 7.3.7 DefinePropertyOrThrow ( O, P, desc )
def DefinePropertyOrThrow(obj, propkey, desc):
    # The abstract operation DefinePropertyOrThrow is used to call the [[DefineOwnProperty]] internal method of an
    # object in a manner that will throw a TypeError exception if the requested property update cannot be performed.
    # The operation is called with arguments O, P, and desc where O is the object, P is the property key, and desc is
    # the Property Descriptor for the property. This abstract operation performs the following steps:
    #
    # 1. Assert: Type(O) is Object.
    assert isObject(obj)
    # 2. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(propkey)
    # 3. Let success be ? O.[[DefineOwnProperty]](P, desc).
    success = obj.DefineOwnProperty(propkey, desc)
    # 4. If success is false, throw a TypeError exception.
    if not success:
        raise ESTypeError(f'cannot define property \'{propkey}\'')
    # 5. Return success.
    return success

# ------------------------------------ 𝟕.𝟑.𝟖 𝑫𝒆𝒍𝒆𝒕𝒆𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚𝑶𝒓𝑻𝒉𝒓𝒐𝒘 ( 𝑶, 𝑷 ) ------------------------------------
# 7.3.8 DeletePropertyOrThrow ( O, P )
def DeletePropertyOrThrow(O, P):
    # The abstract operation DeletePropertyOrThrow is used to remove a specific own property of an object. It throws an
    # exception if the property is not configurable. The operation is called with arguments O and P where O is the
    # object and P is the property key. This abstract operation performs the following steps:
    #
    # 1. Assert: Type(O) is Object.
    # 2. Assert: IsPropertyKey(P) is true.
    # 3. Let success be ? O.[[Delete]](P).
    # 4. If success is false, throw a TypeError exception.
    # 5. Return success.
    assert isObject(O)
    assert IsPropertyKey(P)
    success = O.Delete(P)
    if not success:
        raise ESTypeError(f'cannot delete property \'{P}\'')
    return success

# ------------------------------------ 𝟕.𝟑.𝟗 𝑮𝒆𝒕𝑴𝒆𝒕𝒉𝒐𝒅 ( 𝑽, 𝑷 ) ------------------------------------
# 7.3.9 GetMethod ( V, P )
def GetMethod(value, propkey):
    # The abstract operation GetMethod is used to get the value of a specific property of an ECMAScript language value
    # when the value of the property is expected to be a function. The operation is called with arguments V and P where
    # V is the ECMAScript language value, P is the property key. This abstract operation performs the following steps:
    #
    # 1. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(propkey)
    # 2. Let func be ? GetV(V, P).
    func = GetV(value, propkey)
    # 3. If func is either undefined or null, return undefined.
    if isUndefined(func) or isNull(func):
        return None
    # 4. If IsCallable(func) is false, throw a TypeError exception.
    if not IsCallable(func):
        raise ESTypeError(f'property \'{propkey}\' is not a method')
    # 5. Return func.
    return func

# ------------------------------------ 𝟕.𝟑.𝟏𝟎 𝑯𝒂𝒔𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚 ( 𝑶, 𝑷 ) ------------------------------------
# 7.3.10 HasProperty ( O, P )
def HasProperty(O, P):
    # The abstract operation HasProperty is used to determine whether an object has a property with the specified property key.
    # The property may be either an own or inherited. A Boolean value is returned. The operation is called with arguments O and
    # P where O is the object and P is the property key. This abstract operation performs the following steps:
    #
    # 1. Assert: Type(O) is Object.
    assert isObject(O)
    # 2. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(P)
    # 3. Return ? O.[[HasProperty]](P).
    return O.HasProperty(P)

# ------------------------------------ 𝟕.𝟑.𝟏𝟏 𝑯𝒂𝒔𝑶𝒘𝒏𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚 ( 𝑶, 𝑷 ) ------------------------------------
# 7.3.11 HasOwnProperty ( O, P )
def HasOwnProperty(O, P):
    # The abstract operation HasOwnProperty is used to determine whether an object has an own property with the specified
    # property key. A Boolean value is returned. The operation is called with arguments O and P where O is the object and P is
    # the property key. This abstract operation performs the following steps:
    #
    # 1. Assert: Type(O) is Object.
    assert isObject(O)
    # 2. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(P)
    # 3. Let desc be ? O.[[GetOwnProperty]](P).
    desc = O.GetOwnProperty(P)
    # 4. If desc is undefined, return false.
    # 5. Return true.
    return desc is not None

# ------------------------------------ 𝟕.𝟑.𝟏𝟐 𝑪𝒂𝒍𝒍 ( 𝑭, 𝑽 [ , 𝒂𝒓𝒈𝒖𝒎𝒆𝒏𝒕𝒔𝑳𝒊𝒔𝒕 ] ) ------------------------------------
# 7.3.12 Call ( F, V [ , argumentsList ] )
def Call(func, value, args=[]):
    # The abstract operation Call is used to call the [[Call]] internal method of a function object. The operation is
    # called with arguments F, V, and optionally argumentsList where F is the function object, V is an ECMAScript
    # language value that is the this value of the [[Call]], and argumentsList is the value passed to the corresponding
    # argument of the internal method. If argumentsList is not present, a new empty List is used as its value. This
    # abstract operation performs the following steps:
    #
    # 1. If argumentsList is not present, set argumentsList to a new empty List.
    # 2. If IsCallable(F) is false, throw a TypeError exception.
    if not IsCallable(func):
        raise ESTypeError(f'\'{func!r}\' is not a callable function')
    # 3. Return ? F.[[Call]](V, argumentsList).
    return func.Call(value, args)

# ------------------------------------ 𝟕.𝟑.𝟏𝟑 𝑪𝒐𝒏𝒔𝒕𝒓𝒖𝒄𝒕 ( 𝑭 [ , 𝒂𝒓𝒈𝒖𝒎𝒆𝒏𝒕𝒔𝑳𝒊𝒔𝒕 [ , 𝒏𝒆𝒘𝑻𝒂𝒓𝒈𝒆𝒕 ]] ) ------------------------------------
# 7.3.13 Construct ( F [ , argumentsList [ , newTarget ]] )
def Construct(func, args=[], newTarget=missing.MISSING):
    # The abstract operation Construct is used to call the [[Construct]] internal method of a function object. The operation is
    # called with arguments F, and optionally argumentsList, and newTarget where F is the function object. argumentsList and
    # newTarget are the values to be passed as the corresponding arguments of the internal method. If argumentsList is not
    # present, a new empty List is used as its value. If newTarget is not present, F is used as its value. This abstract
    # operation performs the following steps:
    #
    # 1. If newTarget is not present, set newTarget to F.
    if newTarget == missing.MISSING:
        newTarget = func
    # 2. If argumentsList is not present, set argumentsList to a new empty List.
    # 3. Assert: IsConstructor(F) is true.
    assert IsConstructor(func)
    # 4. Assert: IsConstructor(newTarget) is true.
    assert IsConstructor(newTarget)
    # 5. Return ? F.[[Construct]](argumentsList, newTarget).
    return func.Construct(args, newTarget)
    # NOTE
    # If newTarget is not present, this operation is equivalent to: new F(...argumentsList)

# ------------------------------------ 𝟕.𝟑.𝟏𝟒 𝑺𝒆𝒕𝑰𝒏𝒕𝒆𝒈𝒓𝒊𝒕𝒚𝑳𝒆𝒗𝒆𝒍 ( 𝑶, 𝒍𝒆𝒗𝒆𝒍 ) ------------------------------------
# 7.3.14 SetIntegrityLevel ( O, level )
def SetIntegrityLevel(O, level):
    # The abstract operation SetIntegrityLevel is used to fix the set of own properties of an object. This abstract
    # operation performs the following steps:
    #
    # 1. Assert: Type(O) is Object.
    # 2. Assert: level is either "sealed" or "frozen".
    # 3. Let status be ? O.[[PreventExtensions]]().
    # 4. If status is false, return false.
    # 5. Let keys be ? O.[[OwnPropertyKeys]]().
    # 6. If level is "sealed", then
    #    a. For each element k of keys, do
    #       i. Perform ? DefinePropertyOrThrow(O, k, PropertyDescriptor { [[Configurable]]: false }).
    # 7. Else level is "frozen",
    #    a. For each element k of keys, do
    #       i. Let currentDesc be ? O.[[GetOwnProperty]](k).
    #      ii. If currentDesc is not undefined, then
    #          1. If IsAccessorDescriptor(currentDesc) is true, then
    #             a. Let desc be the PropertyDescriptor { [[Configurable]]: false }.
    #          2. Else,
    #             a. Let desc be the PropertyDescriptor { [[Configurable]]: false, [[Writable]]: false }.
    #          3. Perform ? DefinePropertyOrThrow(O, k, desc).
    # 8. Return true.
    assert isObject(O)
    assert level in ['sealed', 'frozen']
    status = O.PreventExtensions()
    if not status:
        return False
    keys = O.OwnPropertyKeys()
    if level == 'sealed':
        for k in keys:
            DefinePropertyOrThrow(O, k, PropertyDescriptor(configurable=False))
    else:
        for k in keys:
            currentDesc = O.GetOwnProperty(k)
            if currentDesc is not None:
                if IsAccessorDescriptor(currentDesc):
                    desc = PropertyDescriptor(configurable=False)
                else:
                    desc = PropertyDescriptor(writable=False, configurable=False)
                DefinePropertyOrThrow(O, k, desc)
    return True

# ------------------------------------ 𝟕.𝟑.𝟏𝟓 𝑻𝒆𝒔𝒕𝑰𝒏𝒕𝒆𝒈𝒓𝒊𝒕𝒚𝑳𝒆𝒗𝒆𝒍 ( 𝑶, 𝒍𝒆𝒗𝒆𝒍 ) ------------------------------------
# 7.3.15 TestIntegrityLevel ( O, level )
def TestIntegrityLevel(o_value, level):
    # The abstract operation TestIntegrityLevel is used to determine if the set of own properties of an object are
    # fixed. This abstract operation performs the following steps:
    #
    # 1. Assert: Type(O) is Object.
    assert isObject(o_value)
    # 2. Assert: level is either "sealed" or "frozen".
    assert level in ['sealed', 'frozen']
    # 3. Let status be ? IsExtensible(O).
    status = IsExtensible(o_value)
    # 4. If status is true, return false.
    if status:
        return False
    # 5. NOTE: If the object is extensible, none of its properties are examined.
    # 6. Let keys be ? O.[[OwnPropertyKeys]]().
    keys = o_value.OwnPropertyKeys()
    # 7. For each element k of keys, do
    for k in keys:
        # a. Let currentDesc be ? O.[[GetOwnProperty]](k).
        current_desc = o_value.GetOwnProperty(k)
        # b. If currentDesc is not undefined, then
        if current_desc is not None:
            # i. If currentDesc.[[Configurable]] is true, return false.
            if current_desc.configurable:
                return False
            # ii. If level is "frozen" and IsDataDescriptor(currentDesc) is true, then
            if level == 'frozen' and IsDataDescriptor(current_desc):
                # 1. If currentDesc.[[Writable]] is true, return false.
                if current_desc.writable:
                    return False
    # 8. Return true.
    return True

# ------------------------------------ 𝟕.𝟑.𝟏𝟔 𝑪𝒓𝒆𝒂𝒕𝒆𝑨𝒓𝒓𝒂𝒚𝑭𝒓𝒐𝒎𝑳𝒊𝒔𝒕 ( 𝒆𝒍𝒆𝒎𝒆𝒏𝒕𝒔 ) ------------------------------------
# 7.3.16 CreateArrayFromList ( elements )
def CreateArrayFromList(elements):
    # The abstract operation CreateArrayFromList is used to create an Array object whose elements are provided by a
    # List. This abstract operation performs the following steps:
    #
    # 1. Assert: elements is a List whose elements are all ECMAScript language values.
    # 2. Let array be ! ArrayCreate(0).
    # 3. Let n be 0.
    # 4. For each element e of elements, do
    #    a. Let status be CreateDataProperty(array, ! ToString(n), e).
    #    b. Assert: status is true.
    #    c. Increment n by 1.
    # 5. Return array.
    assert isinstance(elements, list)
    assert all(isEcmaValue(x) for x in elements)
    array = ArrayCreate(0)
    for n, e in enumerate(elements):
        status = CreateDataProperty(array, ToString(n), e)
        assert status
    return array

# ------------------------------------ 𝟕.𝟑.𝟏𝟖 𝑰𝒏𝒗𝒐𝒌𝒆 ( 𝑽, 𝑷 [ , 𝒂𝒓𝒈𝒖𝒎𝒆𝒏𝒕𝒔𝑳𝒊𝒔𝒕 ] ) ------------------------------------
# 7.3.18 Invoke ( V, P [ , argumentsList ] )
def Invoke(v, p, arguments_list=[]):
    # The abstract operation Invoke is used to call a method property of an ECMAScript language value. The operation is
    # called with arguments V, P, and optionally argumentsList where V serves as both the lookup point for the property
    # and the this value of the call, P is the property key, and argumentsList is the list of arguments values passed
    # to the method. If argumentsList is not present, a new empty List is used as its value. This abstract operation
    # performs the following steps:
    #
    # 1. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(p)
    # 2. If argumentsList is not present, set argumentsList to a new empty List.
    # 3. Let func be ? GetV(V, P).
    func = GetV(v, p)
    # 4. Return ? Call(func, V, argumentsList).
    return Call(func, v, arguments_list)

# ------------------------------------ 𝟕.𝟑.𝟏𝟗 𝑶𝒓𝒅𝒊𝒏𝒂𝒓𝒚𝑯𝒂𝒔𝑰𝒏𝒔𝒕𝒂𝒏𝒄𝒆 ( 𝑪, 𝑶 ) ------------------------------------
# 7.3.19 OrdinaryHasInstance ( C, O )
def OrdinaryHasInstance(C, O):
    # The abstract operation OrdinaryHasInstance implements the default algorithm for determining if an object O inherits from
    # the instance object inheritance path provided by constructor C. This abstract operation performs the following steps:
    #
    # 1. If IsCallable(C) is false, return false.
    if not IsCallable(C):
        return False
    # 2. If C has a [[BoundTargetFunction]] internal slot, then
    if hasattr(C, 'BoundTargetFunction'):
        # a. Let BC be C.[[BoundTargetFunction]].
        BC = C.BoundTargetFunction
        # b. Return ? InstanceofOperator(O, BC).
        return InstanceofOperator(O, BC)
    # 3. If Type(O) is not Object, return false.
    if not isObject(O):
        return False
    # 4. Let P be ? Get(C, "prototype").
    P = Get(C, 'prototype')
    # 5. If Type(P) is not Object, throw a TypeError exception.
    if not isObject(P):
        raise ESTypeError()
    # 6. Repeat,
    while 1:
        # a. Set O to ? O.[[GetPrototypeOf]]().
        O = O.GetPrototypeOf()
        # b. If O is null, return false.
        if isNull(O):
            return False
        # c. If SameValue(P, O) is true, return true.
        if SameValue(P, O):
            return True

# ------------------------------------ 𝟕.𝟑.𝟐𝟏 𝑬𝒏𝒖𝒎𝒆𝒓𝒂𝒃𝒍𝒆𝑶𝒘𝒏𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚𝑵𝒂𝒎𝒆𝒔 ( 𝑶, 𝒌𝒊𝒏𝒅 ) ------------------------------------
# 7.3.21 EnumerableOwnPropertyNames ( O, kind )
def EnumerableOwnPropertyNames(O, kind):
    # When the abstract operation EnumerableOwnPropertyNames is called with Object O and String kind the following
    # steps are taken:
    #
    #   1. Assert: Type(O) is Object.
    #   2. Let ownKeys be ? O.[[OwnPropertyKeys]]().
    #   3. Let properties be a new empty List.
    #   4. For each element key of ownKeys in List order, do
    #       a. If Type(key) is String, then
    #           i. Let desc be ? O.[[GetOwnProperty]](key).
    #           ii. If desc is not undefined and desc.[[Enumerable]] is true, then
    #               1. If kind is "key", append key to properties.
    #               2. Else,
    #                   a. Let value be ? Get(O, key).
    #                   b. If kind is "value", append value to properties.
    #                   c. Else,
    #                       i. Assert: kind is "key+value".
    #                       ii. Let entry be CreateArrayFromList(« key, value »).
    #                       iii. Append entry to properties.
    #   5. Order the elements of properties so they are in the same relative order as would be produced by the Iterator
    #      that would be returned if the EnumerateObjectProperties internal method were invoked with O.
    #   6. Return properties.
    assert isObject(O)
    ownKeys = O.OwnPropertyKeys()
    properties = []
    for key in ownKeys:
        if isString(key):
            desc = O.GetOwnProperty(key)
            if desc is not None and desc.enumerable:
                if kind == 'key':
                    properties.append(key)
                else:
                    value = Get(O, key)
                    if kind == 'value':
                        properties.append(value)
                    else:
                        assert kind == 'key+value'
                        entry = CreateArrayFromList([key, value])
                        properties.append(entry)
    # The ordering rule is just to ensure the two functions are consistent, not that there's a particular order the
    # spec provides. We're good, because the true ordering is already done in OwnPropertyKeys.
    return properties

# ------------------------------------ 𝟕.𝟑.𝟐𝟐 𝑮𝒆𝒕𝑭𝒖𝒏𝒄𝒕𝒊𝒐𝒏𝑹𝒆𝒂𝒍𝒎 ( 𝒐𝒃𝒋 ) ------------------------------------
# 7.3.22 GetFunctionRealm ( obj )
def GetFunctionRealm(obj):
    # The abstract operation GetFunctionRealm with argument obj performs the following steps:
    #
    # 1. Assert: obj is a callable object.
    assert IsCallable(obj)
    # 2. If obj has a [[Realm]] internal slot, then
    if hasattr(obj, 'Realm'):
        # a. Return obj.[[Realm]].
        return obj.Realm
    # 3. If obj is a Bound Function exotic object, then
    if hasattr(obj, 'BoundTargetFunction'):
        # a. Let target be obj.[[BoundTargetFunction]].
        target = obj.BoundTargetFunction
        # b. Return ? GetFunctionRealm(target).
        return GetFunctionRealm(target)
    # 4. If obj is a Proxy exotic object, then
    if hasattr(obj, 'ProxyHandler'):
        # a. If obj.[[ProxyHandler]] is null, throw a TypeError exception.
        if isNull(obj.ProxyHandler):
            raise ESTypeError()
        # b. Let proxyTarget be obj.[[ProxyTarget]].
        proxy_target = obj.ProxyTarget
        # c. Return ? GetFunctionRealm(proxyTarget).
        return GetFunctionRealm(proxy_target)
    # 5. Return the current Realm Record.
    return surrounding_agent.running_ec.realm
    # NOTE
    # Step 5 will only be reached if obj is a non-standard function exotic object that does not have a [[Realm]]
    # internal slot.

# ------------------------------------ 𝟕.𝟑.𝟐𝟑 𝑪𝒐𝒑𝒚𝑫𝒂𝒕𝒂𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒊𝒆𝒔 ( 𝒕𝒂𝒓𝒈𝒆𝒕, 𝒔𝒐𝒖𝒓𝒄𝒆, 𝒆𝒙𝒄𝒍𝒖𝒅𝒆𝒅𝑰𝒕𝒆𝒎𝒔 ) ------------------------------------
# 7.3.23 CopyDataProperties ( target, source, excludedItems )
def CopyDataProperties(target, source, excludedItems):
    # When the abstract operation CopyDataProperties is called with arguments target, source, and excludedItems, the
    # following steps are taken:
    #
    #   1. Assert: Type(target) is Object.
    #   2. Assert: excludedItems is a List of property keys.
    #   3. If source is undefined or null, return target.
    #   4. Let from be ! ToObject(source).
    #   5. Let keys be ? from.[[OwnPropertyKeys]]().
    #   6. For each element nextKey of keys in List order, do
    #       a. Let excluded be false.
    #       b. For each element e of excludedItems in List order, do
    #           i. If SameValue(e, nextKey) is true, then
    #               1. Set excluded to true.
    #       c. If excluded is false, then
    #           i. Let desc be ? from.[[GetOwnProperty]](nextKey).
    #           ii. If desc is not undefined and desc.[[Enumerable]] is true, then
    #               1. Let propValue be ? Get(from, nextKey).
    #               2. Perform ! CreateDataProperty(target, nextKey, propValue).
    #   7. Return target.
    #
    # NOTE The target passed in here is always a newly created object which is not directly accessible in case of
    # an error being thrown.
    #
    assert isObject(target)
    assert all(IsPropertyKey(key) for key in excludedItems)
    if source is None or isNull(source):
        return target
    from_obj = ToObject(source)
    keys = from_obj.OwnPropertyKeys()
    for nextKey in keys:
        excluded = any(SameValue(e, nextKey) for e in excludedItems)
        if not excluded:
            desc = from_obj.GetOwnProperty(nextKey)
            if desc is not None and desc.enumerable:
                propValue = Get(from_obj, nextKey)
                CreateDataProperty(target, nextKey, propValue)
    return target

##################################################################################################################################################################################################################################################################################
#
# 8888888888         d8888       .d88888b.                                     888    d8b                                                      8888888 888                              888                          .d88888b.  888         d8b                   888
#       d88P        d8P888      d88P" "Y88b                                    888    Y8P                                                        888   888                              888                         d88P" "Y88b 888         Y8P                   888
#      d88P        d8P 888      888     888                                    888                                                               888   888                              888                         888     888 888                               888
#     d88P        d8P  888      888     888 88888b.   .d88b.  888d888  8888b.  888888 888  .d88b.  88888b.  .d8888b       .d88b.  88888b.        888   888888  .d88b.  888d888  8888b.  888888  .d88b.  888d888     888     888 88888b.    8888  .d88b.   .d8888b 888888 .d8888b
#  88888888      d88   888      888     888 888 "88b d8P  Y8b 888P"       "88b 888    888 d88""88b 888 "88b 88K          d88""88b 888 "88b       888   888    d8P  Y8b 888P"       "88b 888    d88""88b 888P"       888     888 888 "88b   "888 d8P  Y8b d88P"    888    88K
#   d88P         8888888888     888     888 888  888 88888888 888     .d888888 888    888 888  888 888  888 "Y8888b.     888  888 888  888       888   888    88888888 888     .d888888 888    888  888 888         888     888 888  888    888 88888888 888      888    "Y8888b.
#  d88P      d8b       888      Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  888 Y88..88P 888  888      X88     Y88..88P 888  888       888   Y88b.  Y8b.     888     888  888 Y88b.  Y88..88P 888         Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.       X88
# d88P       Y8P       888       "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888 888  "Y88P"  888  888  88888P'      "Y88P"  888  888     8888888  "Y888  "Y8888  888     "Y888888  "Y888  "Y88P"  888          "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888  88888P'
#                                           888                                                                                                                                                                                             888
#                                           888                                                                                                                                                                                            d88P
#                                           888                                                                                                                                                                                          888P"
#
##################################################################################################################################################################################################################################################################################
# ------------------------------------ 𝟕.𝟒.𝟏 𝑮𝒆𝒕𝑰𝒕𝒆𝒓𝒂𝒕𝒐𝒓 ( 𝒐𝒃𝒋 [ , 𝒉𝒊𝒏𝒕 [ , 𝒎𝒆𝒕𝒉𝒐𝒅 ] ] ) ------------------------------------
# 7.4.1 GetIterator ( obj [ , hint [ , method ] ] )
@unique
class IteratorHint(Enum):
    SYNC = auto()
    ASYNC = auto()
SYNC = IteratorHint.SYNC
ASYNC = IteratorHint.ASYNC
def GetIterator(obj, hint=SYNC, method=EMPTY):
    # The abstract operation GetIterator with argument obj and optional arguments hint and method performs the
    # following steps:
    #
    #   1. If hint is not present, set hint to sync.
    #   2. Assert: hint is either sync or async.
    #   3. If method is not present, then
    #       a. If hint is async, then
    #           i. Set method to ? GetMethod(obj, @@asyncIterator).
    #           ii. If method is undefined, then
    #               1. Let syncMethod be ? GetMethod(obj, @@iterator).
    #               2. Let syncIteratorRecord be ? GetIterator(obj, sync, syncMethod).
    #               3. Return ? CreateAsyncFromSyncIterator(syncIteratorRecord).
    #       b. Otherwise, set method to ? GetMethod(obj, @@iterator).
    #   4. Let iterator be ? Call(method, obj).
    #   5. If Type(iterator) is not Object, throw a TypeError exception.
    #   6. Let nextMethod be ? GetV(iterator, "next").
    #   7. Let iteratorRecord be Record { [[Iterator]]: iterator, [[NextMethod]]: nextMethod, [[Done]]: false }.
    #   8. Return iteratorRecord.
    assert hint in [SYNC, ASYNC]
    if method == EMPTY:
        if hint == ASYNC:
            method = GetMethod(obj, wks_async_iterator)
            if method is None:
                syncMethod = GetMethod(obj, wks_iterator)
                syncIteratorRecord = GetIterator(obj, SYNC, syncMethod)
                raise NotImplementedError
                #return CreateAsyncFromSyncIterator(syncIteratorRecord)
        method = GetMethod(obj, wks_iterator)
    iterator = Call(method, obj)
    if not isObject(iterator):
        raise ESTypeError('Iterator not an object')
    nextMethod = GetV(iterator, 'next')
    return Record(Iterator=iterator, NextMethod=nextMethod, Done=False)

# 7.4.2 IteratorNext ( iteratorRecord [ , value ] )
def IteratorNext(iteratorRecord, value=EMPTY):
    # The abstract operation IteratorNext with argument iteratorRecord and optional argument value performs the
    # following steps:
    #
    #   1. If value is not present, then
    #       a. Let result be ? Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]], « »).
    #   2. Else,
    #       a. Let result be ? Call(iteratorRecord.[[NextMethod]], iteratorRecord.[[Iterator]], « value »).
    #   3. If Type(result) is not Object, throw a TypeError exception.
    #   4. Return result.
    if value == EMPTY:
        arglist = []
    else:
        arglist = [value]
    result = Call(iteratorRecord.NextMethod, iteratorRecord.Iterator, arglist)
    if not isObject(result):
        raise ESTypeError('Iterator Next Method returned a non-object')
    return result

# 7.4.3 IteratorComplete ( iterResult )
def IteratorComplete(iterResult):
    # The abstract operation IteratorComplete with argument iterResult performs the following steps:
    #
    # 1. Assert: Type(iterResult) is Object.
    # 2. Return ToBoolean(? Get(iterResult, "done")).
    assert isObject(iterResult)
    return ToBoolean(Get(iterResult, 'done'))

# 7.4.4 IteratorValue ( iterResult )
def IteratorValue(iterResult):
    # The abstract operation IteratorValue with argument iterResult performs the following steps:
    #
    # 1. Assert: Type(iterResult) is Object.
    # 2. Return ? Get(iterResult, "value").
    assert isObject(iterResult)
    return Get(iterResult, 'value')

# 7.4.5 IteratorStep ( iteratorRecord )
def IteratorStep(iteratorRecord):
    # The abstract operation IteratorStep with argument iteratorRecord requests the next value from
    # iteratorRecord.[[Iterator]] by calling iteratorRecord.[[NextMethod]] and returns either false indicating that the
    # iterator has reached its end or the IteratorResult object if a next value is available. IteratorStep performs the
    # following steps:
    #
    # 1. Let result be ? IteratorNext(iteratorRecord).
    # 2. Let done be ? IteratorComplete(result).
    # 3. If done is true, return false.
    # 4. Return result.
    result = IteratorNext(iteratorRecord)
    done = IteratorComplete(result)
    if done:
        return False
    return result

# 7.4.6 IteratorClose ( iteratorRecord, completion )
def IteratorClose(iteratorRecord):
    # The abstract operation IteratorClose with arguments iteratorRecord and completion is used to notify an iterator
    # that it should perform any actions it would normally perform when it has reached its completed state:
    #
    #   1. Assert: Type(iteratorRecord.[[Iterator]]) is Object.
    #   2. Assert: completion is a Completion Record.
    #   3. Let iterator be iteratorRecord.[[Iterator]].
    #   4. Let return be ? GetMethod(iterator, "return").
    #   5. If return is undefined, return Completion(completion).
    #   6. Let innerResult be Call(return, iterator, « »).
    #   7. If completion.[[Type]] is throw, return Completion(completion).
    #   8. If innerResult.[[Type]] is throw, return Completion(innerResult).
    #   9. If Type(innerResult.[[Value]]) is not Object, throw a TypeError exception.
    #   10. Return Completion(completion).
    assert isObject(iteratorRecord.Iterator)
    iterator = iteratorRecord.Iterator
    return_method = GetMethod(iterator, 'return')
    if return_method:
        innerResult = Call(return_method, iterator, [])
        if not isObject(innerResult):
            raise ESTypeError(f'Bad result from iterator \'return\' method. (Got {ToString(innerResult)})')

# ------------------------------------ 𝟕.𝟒.𝟖 𝑪𝒓𝒆𝒂𝒕𝒆𝑰𝒕𝒆𝒓𝑹𝒆𝒔𝒖𝒍𝒕𝑶𝒃𝒋𝒆𝒄𝒕 ( 𝒗𝒂𝒍𝒖𝒆, 𝒅𝒐𝒏𝒆 ) ------------------------------------
# 7.4.8 CreateIterResultObject ( value, done )
def CreateIterResultObject(value, done):
    # The abstract operation CreateIterResultObject with arguments value and done creates an object that supports the
    #   IteratorResult interface by performing the following steps:
    #
    #   1. Assert: Type(done) is Boolean.
    #   2. Let obj be ObjectCreate(%ObjectPrototype%).
    #   3. Perform CreateDataProperty(obj, "value", value).
    #   4. Perform CreateDataProperty(obj, "done", done).
    #   5. Return obj.
    assert isBoolean(done)
    obj = ObjectCreate(surrounding_agent.running_ec.realm.intrinsics['%ObjectPrototype%'])
    CreateDataProperty(obj, 'value', value)
    CreateDataProperty(obj, 'done', done)
    return obj

# ------------------------------------ 𝟕.𝟒.𝟗 𝑪𝒓𝒆𝒂𝒕𝒆𝑳𝒊𝒔𝒕𝑰𝒕𝒆𝒓𝒂𝒕𝒐𝒓𝑹𝒆𝒄𝒐𝒓𝒅 ( 𝒍𝒊𝒔𝒕 ) ------------------------------------
# 7.4.9 CreateListIteratorRecord ( list )
def CreateListIteratorRecord(lst):
    # The abstract operation CreateListIteratorRecord with argument list creates an Iterator (25.1.1.2) object record
    # whose next method returns the successive elements of list. It performs the following steps:
    #
    #   1. Let iterator be ObjectCreate(%IteratorPrototype%, « [[IteratedList]], [[ListIteratorNextIndex]] »).
    #   2. Set iterator.[[IteratedList]] to list.
    #   3. Set iterator.[[ListIteratorNextIndex]] to 0.
    #   4. Let steps be the algorithm steps defined in ListIterator next (7.4.9.1).
    #   5. Let next be CreateBuiltinFunction(steps, « »).
    #   6. Return Record { [[Iterator]]: iterator, [[NextMethod]]: next, [[Done]]: false }.
    # NOTE The list iterator object is never directly accessible to ECMAScript code.
    iterator = ObjectCreate(surrounding_agent.running_ec.realm.intrinsics['%IteratorPrototype%'], ['IteratedList', 'ListIteratorNextIndex'])
    iterator.IteratedList = lst
    iterator.ListIteratorNextIndex = 0
    next_fcn = CreateBuiltinFunction(ListIterator_next, [])
    return Record(Iterator=iterator, NextMethod=next_fcn, Done=False)

# ------------------------------------ 𝟕.𝟒.𝟗.𝟏 𝑳𝒊𝒔𝒕𝑰𝒕𝒆𝒓𝒂𝒕𝒐𝒓 𝒏𝒆𝒙𝒕 ( ) ------------------------------------
# 7.4.9.1 ListIterator next ( )
def ListIterator_next(this_value, new_target):
    # The ListIterator next method is a standard built-in function object (clause 17) that performs the following steps:
    #
    #   1. Let O be the this value.
    #   2. Assert: Type(O) is Object.
    #   3. Assert: O has an [[IteratedList]] internal slot.
    #   4. Let list be O.[[IteratedList]].
    #   5. Let index be O.[[ListIteratorNextIndex]].
    #   6. Let len be the number of elements of list.
    #   7. If index ≥ len, then
    #       a. Return CreateIterResultObject(undefined, true).
    #   8. Set O.[[ListIteratorNextIndex]] to index+1.
    #   9. Return CreateIterResultObject(list[index], false).
    O = this_value
    assert isObject(O)
    assert hasattr(O, 'IteratedList')
    lst = O.IteratedList
    index = O.ListIteratorNextIndex
    length = len(lst)
    if index >= length:
        return CreateIterResultObject(None, True)
    O.ListIteratorNextIndex = index + 1
    return CreateIterResultObject(lst[index], False)

###############################################################################################################################################
#
#  .d8888b.  888                        888                          .d8888b.
# d88P  Y88b 888                        888                         d88P  Y88b
# 888    888 888                        888                         Y88b. d88P
# 888        88888b.   8888b.  88888b.  888888  .d88b.  888d888      "Y88888"
# 888        888 "88b     "88b 888 "88b 888    d8P  Y8b 888P"       .d8P""Y8b.
# 888    888 888  888 .d888888 888  888 888    88888888 888         888    888
# Y88b  d88P 888  888 888  888 888 d88P Y88b.  Y8b.     888         Y88b  d88P
#  "Y8888P"  888  888 "Y888888 88888P"   "Y888  "Y8888  888          "Y8888P"
#                              888
#                              888
#                              888
#
###############################################################################################################################################
# Chapter 8: Executable Code and Execution Contexts

###############################################################################################################################################
#
#  .d8888b.       d888       888                        d8b                   888     8888888888                   d8b                                                           888
# d88P  Y88b     d8888       888                        Y8P                   888     888                          Y8P                                                           888
# Y88b. d88P       888       888                                              888     888                                                                                        888
#  "Y88888"        888       888       .d88b.  888  888 888  .d8888b  8888b.  888     8888888    88888b.  888  888 888 888d888  .d88b.  88888b.  88888b.d88b.   .d88b.  88888b.  888888 .d8888b
# .d8P""Y8b.       888       888      d8P  Y8b `Y8bd8P' 888 d88P"        "88b 888     888        888 "88b 888  888 888 888P"   d88""88b 888 "88b 888 "888 "88b d8P  Y8b 888 "88b 888    88K
# 888    888       888       888      88888888   X88K   888 888      .d888888 888     888        888  888 Y88  88P 888 888     888  888 888  888 888  888  888 88888888 888  888 888    "Y8888b.
# Y88b  d88P d8b   888       888      Y8b.     .d8""8b. 888 Y88b.    888  888 888     888        888  888  Y8bd8P  888 888     Y88..88P 888  888 888  888  888 Y8b.     888  888 Y88b.       X88
#  "Y8888P"  Y8P 8888888     88888888  "Y8888  888  888 888  "Y8888P "Y888888 888     8888888888 888  888   Y88P   888 888      "Y88P"  888  888 888  888  888  "Y8888  888  888  "Y888  88888P'
#
###############################################################################################################################################
# 8.1 Lexical Environments
#
# A Lexical Environment is a specification type used to define the association of Identifiers to specific variables and
# functions based upon the lexical nesting structure of ECMAScript code. A Lexical Environment consists of an
# Environment Record and a possibly null reference to an outer Lexical Environment. Usually a Lexical Environment is
# associated with some specific syntactic structure of ECMAScript code such as a FunctionDeclaration, a BlockStatement,
# or a Catch clause of a TryStatement and a new Lexical Environment is created each time such code is evaluated.
#
# An Environment Record records the identifier bindings that are created within the scope of its associated Lexical
# Environment. It is referred to as the Lexical Environment's EnvironmentRecord.
#
# The outer environment reference is used to model the logical nesting of Lexical Environment values. The outer
# reference of a (inner) Lexical Environment is a reference to the Lexical Environment that logically surrounds the
# inner Lexical Environment. An outer Lexical Environment may, of course, have its own outer Lexical Environment. A
# Lexical Environment may serve as the outer environment for multiple inner Lexical Environments. For example, if a
# FunctionDeclaration contains two nested FunctionDeclarations then the Lexical Environments of each of the nested
# functions will have as their outer Lexical Environment the Lexical Environment of the current evaluation of the
# surrounding function.
#
# A global environment is a Lexical Environment which does not have an outer environment. The global environment's
# outer environment reference is null. A global environment's EnvironmentRecord may be prepopulated with identifier
# bindings and includes an associated global object whose properties provide some of the global environment's
# identifier bindings. As ECMAScript code is executed, additional properties may be added to the global object and the
# initial properties may be modified.
#
# A module environment is a Lexical Environment that contains the bindings for the top level declarations of a Module.
# It also contains the bindings that are explicitly imported by the Module. The outer environment of a module
# environment is a global environment.
#
# A function environment is a Lexical Environment that corresponds to the invocation of an ECMAScript function object.
# A function environment may establish a new this binding. A function environment also captures the state necessary to
# support super method invocations.
#
# Lexical Environments and Environment Record values are purely specification mechanisms and need not correspond to any
# specific artefact of an ECMAScript implementation. It is impossible for an ECMAScript program to directly access or
# manipulate such values.

#
# 8.1.1 Environment Records
#
# There are two primary kinds of Environment Record values used in this specification: declarative Environment Records
# and object Environment Records. Declarative Environment Records are used to define the effect of ECMAScript language
# syntactic elements such as FunctionDeclarations, VariableDeclarations, and Catch clauses that directly associate
# identifier bindings with ECMAScript language values. Object Environment Records are used to define the effect of
# ECMAScript elements such as WithStatement that associate identifier bindings with the properties of some object.
# Global Environment Records and function Environment Records are specializations that are used for specifically for
# Script global declarations and for top-level declarations within functions.
#
# For specification purposes Environment Record values are values of the Record specification type and can be thought
# of as existing in a simple object-oriented hierarchy where Environment Record is an abstract class with three
# concrete subclasses, declarative Environment Record, object Environment Record, and global Environment Record.
# Function Environment Records and module Environment Records are subclasses of declarative Environment Record. The
# abstract class includes the abstract specification methods defined in Table 14. These abstract methods have distinct
# concrete algorithms for each of the concrete subclasses.
#
# Table 14: Abstract Methods of Environment Records
#
# +------------------------------+-------------------------------------------------------------------------------------
# | Method                       | Purpose
# +------------------------------+-------------------------------------------------------------------------------------
# | HasBinding(N)                | Determine if an Environment Record has a binding for the String value N. Return true
# |                              | if it does and false if it does not.
# +------------------------------+-------------------------------------------------------------------------------------
# | CreateMutableBinding(N, D)   | Create a new but uninitialized mutable binding in an Environment Record. The String
# |                              | value N is the text of the bound name. If the Boolean argument D is true the binding
# |                              | may be subsequently deleted.
# +------------------------------+-------------------------------------------------------------------------------------
# | CreateImmutableBinding(N, S) | Create a new but uninitialized immutable binding in an Environment Record. The
# |                              | String value N is the text of the bound name. If S is true then attempts to set it
# |                              | after it has been initialized will always throw an exception, regardless of the
# |                              | strict mode setting of operations that reference that binding.
# +------------------------------+-------------------------------------------------------------------------------------
# | InitializeBinding(N, V)      | Set the value of an already existing but uninitialized binding in an Environment
# |                              | Record. The String value N is the text of the bound name. V is the value for the
# |                              | binding and is a value of any ECMAScript language type.
# +------------------------------+-------------------------------------------------------------------------------------
# | SetMutableBinding(N, V, S)   | Set the value of an already existing mutable binding in an Environment Record. The
# |                              | String value N is the text of the bound name. V is the value for the binding and may
# |                              | be a value of any ECMAScript language type. S is a Boolean flag. If S is true and
# |                              | the binding cannot be set throw a TypeError exception.
# +------------------------------+-------------------------------------------------------------------------------------
# | GetBindingValue(N, S)        | Returns the value of an already existing binding from an Environment Record. The
# |                              | String value N is the text of the bound name. S is used to identify references
# |                              | originating in strict mode code or that otherwise require strict mode reference
# |                              | semantics. If S is true and the binding does not exist throw a ReferenceError
# |                              | exception. If the binding exists but is uninitialized a ReferenceError is thrown,
# |                              | regardless of the value of S.
# +------------------------------+-------------------------------------------------------------------------------------
# | DeleteBinding(N)             | Delete a binding from an Environment Record. The String value N is the text of the
# |                              | bound name. If a binding for N exists, remove the binding and return true. If the
# |                              | binding exists but cannot be removed return false. If the binding does not exist
# |                              | return true.
# +------------------------------+-------------------------------------------------------------------------------------
# | HasThisBinding()             | Determine if an Environment Record establishes a this binding. Return true if it
# |                              | does and false if it does not.
# +------------------------------+-------------------------------------------------------------------------------------
# | HasSuperBinding()            | Determine if an Environment Record establishes a super method binding. Return true
# |                              | if it does and false if it does not.
# +------------------------------+-------------------------------------------------------------------------------------
# | WithBaseObject()             | If this Environment Record is associated with a with statement, return the with
# |                              | object. Otherwise, return undefined.
# +------------------------------+-------------------------------------------------------------------------------------

# 8.1.1.1 Declarative Environment Records
#
# Each declarative Environment Record is associated with an ECMAScript program scope containing variable, constant,
# let, class, module, import, and/or function declarations. A declarative Environment Record binds the set of
# identifiers defined by the declarations contained within its scope.
#
class DeclarativeEnvironmentRecord:
    Binding = namedtuple('Binding', ['value', 'mutable', 'strict', 'deletable', 'initialized'])
    def __init__(self):
        self.bindings = {}

    # 8.1.1.1.1 HasBinding ( N )
    def HasBinding(self, N):
        """Determine if an Environment Record has a binding for the String value N. Return true if it does and false if
           it does not."""
        # The concrete Environment Record method HasBinding for declarative Environment Records simply determines if
        # the argument identifier is one of the identifiers bound by the record:
        # 1. Let envRec be the declarative Environment Record for which the method was invoked.
        # 2. If envRec has a binding for the name that is the value of N, return true.
        # 3. Return false.
        return N in self.bindings

    # 8.1.1.1.2 CreateMutableBinding ( N, D )
    def CreateMutableBinding(self, N, D):
        """Create a new but uninitialized mutable binding in an Environment Record. The String value N is the text of
           the bound name. If the Boolean argument D is true the binding may be subsequently deleted."""
        # The concrete Environment Record method CreateMutableBinding for declarative Environment Records creates a new
        # mutable binding for the name N that is uninitialized. A binding must not already exist in this Environment
        # Record for N. If Boolean argument D has the value true the new binding is marked as being subject to deletion.
        # 1. Let envRec be the declarative Environment Record for which the method was invoked.
        # 2. Assert: envRec does not already have a binding for N.
        assert N not in self.bindings
        # 3. Create a mutable binding in envRec for N and record that it is uninitialized. If D is true, record that
        #    the newly created binding may be deleted by a subsequent DeleteBinding call.
        self.bindings[N] = self.Binding(None, True, False, D, False)
        # 4. Return NormalCompletion(empty).
        return Empty.EMPTY

    # 8.1.1.1.3 CreateImmutableBinding ( N, S )
    def CreateImmutableBinding(self, N, S):
        """Create a new but uninitialized immutable binding in an Environment Record. The String value N is the text of
           the bound name. If S is true then attempts to set it after it has been initialized will always throw an
           exception, regardless of the strict mode setting of operations that reference that binding."""
        # The concrete Environment Record method CreateImmutableBinding for declarative Environment Records creates a
        # new immutable binding for the name N that is uninitialized. A binding must not already exist in this
        # Environment Record for N. If the Boolean argument S has the value true the new binding is marked as a strict
        # binding.
        # 1. Let envRec be the declarative Environment Record for which the method was invoked.
        # 2. Assert: envRec does not already have a binding for N.
        assert N not in self.bindings
        # 3. Create an immutable binding in envRec for N and record that it is uninitialized. If S is true, record that
        #    the newly created binding is a strict binding.
        self.bindings[N] = self.Binding(None, False, S, False, False)
        # 4. Return NormalCompletion(empty).
        return Empty.EMPTY

    # 8.1.1.1.4 InitializeBinding ( N, V )
    def InitializeBinding(self, N, V):
        """Set the value of an already existing but uninitialized binding in an Environment
           Record. The String value N is the text of the bound name. V is the value for the
           binding and is a value of any ECMAScript language type."""
        # The concrete Environment Record method InitializeBinding for declarative Environment Records is used to set
        # the bound value of the current binding of the identifier whose name is the value of the argument N to the
        # value of argument V. An uninitialized binding for N must already exist.
        # 1. Let envRec be the declarative Environment Record for which the method was invoked.
        # 2. Assert: envRec must have an uninitialized binding for N.
        assert N in self.bindings and not self.bindings[N].initialized
        # 3. Set the bound value for N in envRec to V.
        # 4. Record that the binding for N in envRec has been initialized.
        self.bindings[N] = self.bindings[N]._replace(value=V, initialized=True)
        # 5. Return NormalCompletion(empty).
        return Empty.EMPTY

    # 8.1.1.1.5 SetMutableBinding ( N, V, S )
    def SetMutableBinding(self, N, V, S):
        """Set the value of an already existing mutable binding in an Environment Record. The
        String value N is the text of the bound name. V is the value for the binding and may
        be a value of any ECMAScript language type. S is a Boolean flag. If S is true and
        the binding cannot be set throw a TypeError exception."""
        # The concrete Environment Record method SetMutableBinding for declarative Environment Records attempts to
        # change the bound value of the current binding of the identifier whose name is the value of the argument N to
        # the value of argument V. A binding for N normally already exists, but in rare cases it may not. If the
        # binding is an immutable binding, a TypeError is thrown if S is true.
        # 1. Let envRec be the declarative Environment Record for which the method was invoked.
        # 2. If envRec does not have a binding for N, then
        if N not in self.bindings:
            # a. If S is true, throw a ReferenceError exception.
            if S:
                raise ESReferenceError()
            # b. Perform envRec.CreateMutableBinding(N, true).
            self.CreateMutableBinding(N, True)
            # c. Perform envRec.InitializeBinding(N, V).
            self.InitializeBinding(N, V)
            # d. Return NormalCompletion(empty).
            return Empty.EMPTY
        # 3. If the binding for N in envRec is a strict binding, set S to true.
        if self.bindings[N].strict:
            S = True
        # 4. If the binding for N in envRec has not yet been initialized, throw a ReferenceError exception.
        if not self.bindings[N].initialized:
            raise ESReferenceError()
        # 5. Else if the binding for N in envRec is a mutable binding, change its bound value to V.
        if self.bindings[N].mutable:
            self.bindings[N] = self.bindings[N]._replace(value=V)
        # 6. Else,
        else:
            # a. Assert: This is an attempt to change the value of an immutable binding.
            # b. If S is true, throw a TypeError exception.
            if S:
                raise ESTypeError()
        # 7. Return NormalCompletion(empty).
        return Empty.EMPTY
        # NOTE
        # An example of ECMAScript code that results in a missing binding at step 2 is:
        #
        # function f(){eval("var x; x = (delete x, 0);")}

    # 8.1.1.1.6 GetBindingValue ( N, S )
    def GetBindingValue(self, N, _):
        # The concrete Environment Record method GetBindingValue for declarative Environment Records simply returns
        # the value of its bound identifier whose name is the value of the argument N. If the binding exists but is
        # uninitialized a ReferenceError is thrown, regardless of the value of S.
        # 1. Let envRec be the declarative Environment Record for which the method was invoked.
        # 2. Assert: envRec has a binding for N.
        # 3. If the binding for N in envRec is an uninitialized binding, throw a ReferenceError exception.
        if not self.bindings[N].initialized:
            raise ESReferenceError()
        # 4. Return the value currently bound to N in envRec.
        return self.bindings[N].value

    # 8.1.1.1.7 DeleteBinding ( N )
    def DeleteBinding(self, N):
        # The concrete Environment Record method DeleteBinding for declarative Environment Records can only delete
        # bindings that have been explicitly designated as being subject to deletion.
        # 1. Let envRec be the declarative Environment Record for which the method was invoked.
        # 2. Assert: envRec has a binding for the name that is the value of N.
        # 3. If the binding for N in envRec cannot be deleted, return false.
        if not self.bindings[N].deletable:
            return False
        # 4. Remove the binding for N from envRec.
        del self.bindings[N]
        # 5. Return true.
        return True

    # 8.1.1.1.8 HasThisBinding ( )
    def HasThisBinding(self):
        # Regular declarative Environment Records do not provide a this binding.
        #
        # 1. Return false.
        return False

    # 8.1.1.1.9 HasSuperBinding ( )
    def HasSuperBinding(self):
        # Regular declarative Environment Records do not provide a super binding.
        #
        # 1. Return false.
        return False

    # 8.1.1.1.10 WithBaseObject ( )
    def WithBaseObject(self):
        # Declarative Environment Records always return undefined as their WithBaseObject.
        #
        # 1. Return undefined.
        return None

# 8.1.1.2 Object Environment Records
#
# Each object Environment Record is associated with an object called its binding object. An object Environment Record
# binds the set of string identifier names that directly correspond to the property names of its binding object.
# Property keys that are not strings in the form of an IdentifierName are not included in the set of bound identifiers.
# Both own and inherited properties are included in the set regardless of the setting of their [[Enumerable]]
# attribute. Because properties can be dynamically added and deleted from objects, the set of identifiers bound by an
# object Environment Record may potentially change as a side-effect of any operation that adds or deletes properties.
# Any bindings that are created as a result of such a side-effect are considered to be a mutable binding even if the
# Writable attribute of the corresponding property has the value false. Immutable bindings do not exist for object
# Environment Records.
#
# Object Environment Records created for with statements (13.11) can provide their binding object as an implicit this
# value for use in function calls. The capability is controlled by a withEnvironment Boolean value that is associated
# with each object Environment Record. By default, the value of withEnvironment is false for any object Environment
# Record.

class ObjectEnvironmentRecord:
    def __init__(self, binding_object, with_environment=False):
        self.binding_object = ToObject(binding_object)
        self.with_environment = ToBoolean(with_environment)

    # 8.1.1.2.1 HasBinding ( N )
    def HasBinding(self, N):
        # The concrete Environment Record method HasBinding for object Environment Records determines if its associated
        # binding object has a property whose name is the value of the argument N:
        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. Let bindings be the binding object for envRec.
        # 3. Let foundBinding be ? HasProperty(bindings, N).
        found_binding = HasProperty(self.binding_object, N)
        # 4. If foundBinding is false, return false.
        if not found_binding:
            return False
        # 5. If the withEnvironment flag of envRec is false, return true.
        if not self.with_environment:
            return True
        # 6. Let unscopables be ? Get(bindings, @@unscopables).
        unscopables = Get(self.binding_object, wks_unscopables)
        # 7. If Type(unscopables) is Object, then
        if isObject(unscopables):
            # a. Let blocked be ToBoolean(? Get(unscopables, N)).
            blocked = Get(unscopables, N)
            blocked = ToBoolean(blocked)
            # b. If blocked is true, return false.
            if blocked:
                return False
        # 8. Return true.
        return True

    # 8.1.1.2.2 CreateMutableBinding ( N, D )
    def CreateMutableBinding(self, name, deletable):
        # The concrete Environment Record method CreateMutableBinding for object Environment Records creates in an
        # Environment Record's associated binding object a property whose name is the String value and initializes it
        # to the value undefined. If Boolean argument D has the value true the new property's [[Configurable]]
        # attribute is set to true; otherwise it is set to false.

        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. Let bindings be the binding object for envRec.
        # 3. Return ? DefinePropertyOrThrow(bindings, N, PropertyDescriptor { [[Value]]: undefined, [[Writable]]: true,
        #             [[Enumerable]]: true, [[Configurable]]: D }).
        return DefinePropertyOrThrow(self.binding_object, name,
                                     PropertyDescriptor(value=None, writable=True,
                                                        enumerable=True, configurable=deletable))
        # NOTE
        # Normally envRec will not have a binding for N but if it does, the semantics of DefinePropertyOrThrow may
        # result in an existing binding being replaced or shadowed or cause an abrupt completion to be returned.

    # 8.1.1.2.4 InitializeBinding ( N, V )
    def InitializeBinding(self, name, value):
        # The concrete Environment Record method InitializeBinding for object Environment Records is used to set the
        # bound value of the current binding of the identifier whose name is the value of the argument N to the value of
        # argument V. An uninitialized binding for N must already exist.

        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. Assert: envRec must have an uninitialized binding for N.
        # 3. Record that the binding for N in envRec has been initialized.
        # 4. Return ? envRec.SetMutableBinding(N, V, false).
        return self.SetMutableBinding(name, value, False)
        # NOTE
        # In this specification, all uses of CreateMutableBinding for object Environment Records are immediately
        # followed by a call to InitializeBinding for the same name. Hence, implementations do not need to explicitly
        # track the initialization state of individual object Environment Record bindings.

    # 8.1.1.2.5 SetMutableBinding ( N, V, S )
    def SetMutableBinding(self, name, value, strict):
        # The concrete Environment Record method SetMutableBinding for object Environment Records attempts to set the
        # value of the Environment Record's associated binding object's property whose name is the value of the
        # argument N to the value of argument V. A property named N normally already exists but if it does not or is
        # not currently writable, error handling is determined by the value of the Boolean argument S.

        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. Let bindings be the binding object for envRec.
        # 3. Return ? Set(bindings, N, V, S).
        return Set(self.binding_object, name, value, strict)

    # 8.1.1.2.6 GetBindingValue ( N, S )
    def GetBindingValue(self, name, strict):
        # The concrete Environment Record method GetBindingValue for object Environment Records returns the value of
        # its associated binding object's property whose name is the String value of the argument identifier N. The
        # property should already exist but if it does not the result depends upon the value of the S argument:

        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. Let bindings be the binding object for envRec.
        # 3. Let value be ? HasProperty(bindings, N).
        value = HasProperty(self.binding_object, name)
        # 4. If value is false, then
        if not value:
            # a. If S is false, return the value undefined; otherwise throw a ReferenceError exception.
            if not strict:
                return None
            raise ESReferenceError()
        # 5. Return ? Get(bindings, N).
        return Get(self.binding_object, name)

    # 8.1.1.2.7 DeleteBinding ( N )
    def DeleteBinding(self, name):
        # The concrete Environment Record method DeleteBinding for object Environment Records can only delete bindings
        # that correspond to properties of the environment object whose [[Configurable]] attribute have the value true.

        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. Let bindings be the binding object for envRec.
        # 3. Return ? bindings.[[Delete]](N).
        return self.binding_object.Delete(name)

    # 8.1.1.2.8 HasThisBinding ( )
    def HasThisBinding(self):
        # Regular object Environment Records do not provide a this binding.
        # 1. Return false.
        return False

    # 8.1.1.2.9 HasSuperBinding ( )
    def HasSuperBinding(self):
        # Regular object Environment Records do not provide a super binding.
        # 1. Return false.
        return False

    # 8.1.1.2.10 WithBaseObject ( )
    def WithBaseObject(self):
        # Object Environment Records return undefined as their WithBaseObject unless their withEnvironment flag is
        # true.

        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. If the withEnvironment flag of envRec is true, return the binding object for envRec.
        if self.with_environment:
            return self.binding_object
        # 3. Otherwise, return undefined.
        return None

# 8.1.1.3 Function Environment Records
#
# A function Environment Record is a declarative Environment Record that is used to represent the top-level scope of a
# function and, if the function is not an ArrowFunction, provides a this binding. If a function is not an ArrowFunction
# function and references super, its function Environment Record also contains the state that is used to perform super
# method invocations from within the function.
#
# Function Environment Records have the additional state fields listed in Table 15.
#
# Table 15: Additional Fields of Function Environment Records
# +-----------------------+---------------------------+-----------------------------------------------------------------
# | Field Name            | Value                     | Meaning
# +-----------------------+---------------------------+-----------------------------------------------------------------
# | [[ThisValue]]         | Any                       | This is the this value used for this invocation of the function.
# +-----------------------+---------------------------+-----------------------------------------------------------------
# | [[ThisBindingStatus]] | "lexical" | "initialized" | If the value is "lexical", this is an ArrowFunction and does not
# |                       |   | "uninitialized"       | have a local this value.
# +-----------------------+---------------------------+-----------------------------------------------------------------
# | [[FunctionObject]]    | Object                    | The function object whose invocation caused this Environment
# |                       |                           | Record to be created.
# +-----------------------+---------------------------+-----------------------------------------------------------------
# | [[HomeObject]]        | Object | undefined        | If the associated function has super property accesses and is
# |                       |                           | not an ArrowFunction, [[HomeObject]] is the object that the
# |                       |                           | function is bound to as a method. The default value for
# |                       |                           | [[HomeObject]] is undefined.
# +-----------------------+---------------------------+-----------------------------------------------------------------
# | [[NewTarget]]         | Object | undefined        | If this Environment Record was created by the [[Construct]]
# |                       |                           | internal method, [[NewTarget]] is the value of the [[Construct]]
# |                       |                           | newTarget parameter. Otherwise, its value is undefined.
# +-----------------------+---------------------------+-----------------------------------------------------------------
#
# Function Environment Records support all of the declarative Environment Record methods listed in Table 14 and share
# the same specifications for all of those methods except for HasThisBinding and HasSuperBinding. In addition, function
# Environment Records support the methods listed in Table 16:
#
# Table 16: Additional Methods of Function Environment Records
# +------------------+--------------------------------------------------------------------------------------------------
# | Method           | Purpose
# +------------------+--------------------------------------------------------------------------------------------------
# | BindThisValue(V) | Set the [[ThisValue]] and record that it has been initialized.
# +------------------+--------------------------------------------------------------------------------------------------
# | GetThisBinding() | Return the value of this Environment Record's this binding. Throws a ReferenceError if the this
# |                  | binding has not been initialized.
# +------------------+--------------------------------------------------------------------------------------------------
# | GetSuperBase()   | Return the object that is the base for super property accesses bound in this Environment Record.
# |                  | The object is derived from this Environment Record's [[HomeObject]] field. The value undefined
# |                  | indicates that super property accesses will produce runtime errors.
# +------------------+--------------------------------------------------------------------------------------------------
class FunctionEnvironmentRecord(DeclarativeEnvironmentRecord):
    def __init__(self):
        super().__init__()
        self.this_value = None
        self.this_binding_status = 'uninitialized'
        self.function_object = JSNull.NULL
        self.home_object = None
        self.new_target = None

    # 8.1.1.3.1 BindThisValue ( V )
    def BindThisValue(self, value):
        # 1. Let envRec be the function Environment Record for which the method was invoked.
        # 2. Assert: envRec.[[ThisBindingStatus]] is not "lexical".
        assert self.this_binding_status != 'lexical'
        # 3. If envRec.[[ThisBindingStatus]] is "initialized", throw a ReferenceError exception.
        if self.this_binding_status == 'initialized':
            raise ESReferenceError()
        # 4. Set envRec.[[ThisValue]] to V.
        self.this_value = value
        # 5. Set envRec.[[ThisBindingStatus]] to "initialized".
        self.this_binding_status = 'initialized'
        # 6. Return V.
        return value

    # 8.1.1.3.2 HasThisBinding ( )
    def HasThisBinding(self):
        # 1. Let envRec be the function Environment Record for which the method was invoked.
        # 2. If envRec.[[ThisBindingStatus]] is "lexical", return false; otherwise, return true.
        return self.this_binding_status != 'lexical'

    # 8.1.1.3.3 HasSuperBinding ( )
    def HasSuperBinding(self):
        # 1. Let envRec be the function Environment Record for which the method was invoked.
        # 2. If envRec.[[ThisBindingStatus]] is "lexical", return false.
        if self.this_binding_status == 'lexical':
            return False
        # e. If envRec.[[HomeObject]] has the value undefined, return false; otherwise, return true.
        return self.home_object is not None

    # 8.1.1.3.4 GetThisBinding ( )
    def GetThisBinding(self):
        # 1. Let envRec be the function Environment Record for which the method was invoked.
        # 2. Assert: envRec.[[ThisBindingStatus]] is not "lexical".
        assert self.this_binding_status != 'lexical'
        # 3. If envRec.[[ThisBindingStatus]] is "uninitialized", throw a ReferenceError exception.
        if self.this_binding_status == 'uninitialized':
            raise ESReferenceError()
        # 4. Return envRec.[[ThisValue]].
        return self.this_value

    # 8.1.1.3.5 GetSuperBase ( )
    def GetSuperBase(self):
        # 1. Let envRec be the function Environment Record for which the method was invoked.
        # 2. Let home be envRec.[[HomeObject]].
        home = self.home_object
        # 3. If home has the value undefined, return undefined.
        if home is None:
            return None
        # 4. Assert: Type(home) is Object.
        assert isObject(home)
        # 5. Return ? home.[[GetPrototypeOf]]().
        return home.GetPrototypeOf()


# 8.1.1.4 Global Environment Records
#
# A global Environment Record is used to represent the outer most scope that is shared by all of the ECMAScript Script
# elements that are processed in a common realm. A global Environment Record provides the bindings for built-in globals
# (clause 18), properties of the global object, and for all top-level declarations (13.2.8, 13.2.10) that occur within
# a Script.
#
# A global Environment Record is logically a single record but it is specified as a composite encapsulating an object
# Environment Record and a declarative Environment Record. The object Environment Record has as its base object the
# global object of the associated Realm Record. This global object is the value returned by the global Environment
# Record's GetThisBinding concrete method. The object Environment Record component of a global Environment Record
# contains the bindings for all built-in globals (clause 18) and all bindings introduced by a FunctionDeclaration,
# GeneratorDeclaration, AsyncFunctionDeclaration, AsyncGeneratorDeclaration, or VariableStatement contained in global
# code. The bindings for all other ECMAScript declarations in global code are contained in the declarative Environment
# Record component of the global Environment Record.
#
# Properties may be created directly on a global object. Hence, the object Environment Record component of a global
# Environment Record may contain both bindings created explicitly by FunctionDeclaration, GeneratorDeclaration,
# AsyncFunctionDeclaration, AsyncGeneratorDeclaration, or VariableDeclaration declarations and bindings created
# implicitly as properties of the global object. In order to identify which bindings were explicitly created using
# declarations, a global Environment Record maintains a list of the names bound using its CreateGlobalVarBinding and
# CreateGlobalFunctionBinding concrete methods.
#
# Global Environment Records have the additional fields listed in Table 17 and the additional methods listed in Table
# 18.
#
# Table 17: Additional Fields of Global Environment Records
# +-----------------------+--------------------------------+------------------------------------------------------------
# | Field Name            | Value                          | Meaning
# +-----------------------+--------------------------------+------------------------------------------------------------
# | [[ObjectRecord]]      | Object Environment Record      | Binding object is the global object. It contains global
# |                       |                                | built-in bindings as well as FunctionDeclaration,
# |                       |                                | GeneratorDeclaration, AsyncFunctionDeclaration,
# |                       |                                | AsyncGeneratorDeclaration, and VariableDeclaration bindings
# |                       |                                | in global code for the associated realm.
# +-----------------------+--------------------------------+------------------------------------------------------------
# | [[GlobalThisValue]]   | Object                         | The value returned by this in global scope. Hosts may
# |                       |                                | provide any ECMAScript Object value.
# +-----------------------+--------------------------------+------------------------------------------------------------
# | [[DeclarativeRecord]] | Declarative Environment Record | Contains bindings for all declarations in global code for
# |                       |                                | the associated realm code except for FunctionDeclaration,
# |                       |                                | GeneratorDeclaration, AsyncFunctionDeclaration,
# |                       |                                | AsyncGeneratorDeclaration, and VariableDeclaration
# |                       |                                | bindings.
# +-----------------------+--------------------------------+------------------------------------------------------------
# | [[VarNames]]          | List of String                 | The string names bound by FunctionDeclaration,
# |                       |                                | GeneratorDeclaration, AsyncFunctionDeclaration,
# |                       |                                | AsyncGeneratorDeclaration, and VariableDeclaration
# |                       |                                | declarations in global code for the associated realm.
# +-----------------------+--------------------------------+------------------------------------------------------------
#
# Table 18: Additional Methods of Global Environment Records
# +--------------------------------------+------------------------------------------------------------------------------
# | Method                               | Purpose
# +--------------------------------------+------------------------------------------------------------------------------
# | GetThisBinding()                     | Return the value of this Environment Record's this binding.
# +--------------------------------------+------------------------------------------------------------------------------
# | HasVarDeclaration (N)                | Determines if the argument identifier has a binding in this Environment
# |                                      | Record that was created using a VariableDeclaration, FunctionDeclaration,
# |                                      | GeneratorDeclaration, AsyncFunctionDeclaration, or AsyncGeneratorDeclaration.
# +--------------------------------------+------------------------------------------------------------------------------
# | HasLexicalDeclaration (N)            | Determines if the argument identifier has a binding in this Environment
# |                                      | Record that was created using a lexical declaration such as a
# |                                      | LexicalDeclaration or a ClassDeclaration.
# +--------------------------------------+------------------------------------------------------------------------------
# | HasRestrictedGlobalProperty (N)      | Determines if the argument is the name of a global object property that may
# |                                      | not be shadowed by a global lexical binding.
# +--------------------------------------+------------------------------------------------------------------------------
# | CanDeclareGlobalVar (N)              | Determines if a corresponding CreateGlobalVarBinding call would succeed if
# |                                      | called for the same argument N.
# +--------------------------------------+------------------------------------------------------------------------------
# | CanDeclareGlobalFunction (N)         | Determines if a corresponding CreateGlobalFunctionBinding call would succeed
# |                                      | if called for the same argument N.
# +--------------------------------------+------------------------------------------------------------------------------
# | CreateGlobalVarBinding(N, D)         | Used to create and initialize to undefined a global var binding in the
# |                                      | [[ObjectRecord]] component of a global Environment Record. The binding will
# |                                      | be a mutable binding. The corresponding global object property will have
# |                                      | attribute values appropriate for a var. The String value N is the bound name.
# |                                      | If D is true the binding may be deleted. Logically equivalent to
# |                                      | CreateMutableBinding followed by a SetMutableBinding but it allows var
# |                                      | declarations to receive special treatment.
# +--------------------------------------+------------------------------------------------------------------------------
# | CreateGlobalFunctionBinding(N, V, D) | Create and initialize a global function binding in the [[ObjectRecord]]
# |                                      | component of a global Environment Record. The binding will be a mutable
# |                                      | binding. The corresponding global object property will have attribute values
# |                                      | appropriate for a function. The String value N is the bound name. V is the
# |                                      | initialization value. If the Boolean argument D is true the binding may be
# |                                      | deleted. Logically equivalent to CreateMutableBinding followed by a
# |                                      | SetMutableBinding but it allows function declarations to receive special
# |                                      | treatment.
# +--------------------------------------+------------------------------------------------------------------------------

class GlobalEnvironmentRecord:
    def __init__(self, binding_object, global_this_value):
        self.object_record = ObjectEnvironmentRecord(binding_object, False)
        self.global_this_value = ToObject(global_this_value)
        self.declarative_record = DeclarativeEnvironmentRecord()
        self.var_names = []

    # 8.1.1.4.1 HasBinding ( N )
    def HasBinding(self, name):
        # The concrete Environment Record method HasBinding for global Environment Records simply determines if the
        # argument identifier is one of the identifiers bound by the record:
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. If DclRec.HasBinding(N) is true, return true.
        if self.declarative_record.HasBinding(name):
            return True
        # 4. Let ObjRec be envRec.[[ObjectRecord]].
        # 5. Return ? ObjRec.HasBinding(N).
        return self.object_record.HasBinding(name)

    # 8.1.1.4.2 CreateMutableBinding ( N, D )
    def CreateMutableBinding(self, name, deletable):
        # The concrete Environment Record method CreateMutableBinding for global Environment Records creates a new
        # mutable binding for the name N that is uninitialized. The binding is created in the associated
        # DeclarativeRecord. A binding for N must not already exist in the DeclarativeRecord. If Boolean argument D has
        # the value true the new binding is marked as being subject to deletion.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. If DclRec.HasBinding(N) is true, throw a TypeError exception.
        if self.declarative_record.HasBinding(name):
            raise ESTypeError()
        # 4. Return DclRec.CreateMutableBinding(N, D).
        return self.declarative_record.CreateMutableBinding(name, deletable)

    # 8.1.1.4.3 CreateImmutableBinding ( N, S )
    def CreateImmutableBinding(self, name, strict):
        # The concrete Environment Record method CreateImmutableBinding for global Environment Records creates a new
        # immutable binding for the name N that is uninitialized. A binding must not already exist in this Environment
        # Record for N. If the Boolean argument S has the value true the new binding is marked as a strict binding.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. If DclRec.HasBinding(N) is true, throw a TypeError exception.
        if self.declarative_record.HasBinding(name):
            raise ESTypeError()
        # 4. Return DclRec.CreateImmutableBinding(N, S).
        return self.declarative_record.CreateImmutableBinding(name, strict)

    # 8.1.1.4.4 InitializeBinding ( N, V )
    def InitializeBinding(self, name, value):
        # The concrete Environment Record method InitializeBinding for global Environment Records is used to set the
        # bound value of the current binding of the identifier whose name is the value of the argument N to the value
        # of argument V. An uninitialized binding for N must already exist.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. If DclRec.HasBinding(N) is true, then
        if self.declarative_record.HasBinding(name):
            # a. Return DclRec.InitializeBinding(N, V).
            return self.declarative_record.InitializeBinding(name, value)
        # 4. Assert: If the binding exists, it must be in the object Environment Record.
        # 5. Let ObjRec be envRec.[[ObjectRecord]].
        # 6. Return ? ObjRec.InitializeBinding(N, V).
        return self.object_record.InitializeBinding(name, value)

    # 8.1.1.4.5 SetMutableBinding ( N, V, S )
    def SetMutableBinding(self, name, value, strict):
        # The concrete Environment Record method SetMutableBinding for global Environment Records attempts to change
        # the bound value of the current binding of the identifier whose name is the value of the argument N to the
        # value of argument V. If the binding is an immutable binding, a TypeError is thrown if S is true. A property
        # named N normally already exists but if it does not or is not currently writable, error handling is determined
        # by the value of the Boolean argument S.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. If DclRec.HasBinding(N) is true, then
        if self.declarative_record.HasBinding(name):
            # a. Return DclRec.SetMutableBinding(N, V, S).
            return self.declarative_record.SetMutableBinding(name, value, strict)
        # 4. Let ObjRec be envRec.[[ObjectRecord]].
        # 5. Return ? ObjRec.SetMutableBinding(N, V, S).
        return self.object_record.SetMutableBinding(name, value, strict)

    # 8.1.1.4.6 GetBindingValue ( N, S )
    def GetBindingValue(self, name, strict):
        # The concrete Environment Record method GetBindingValue for global Environment Records returns the value of
        # its bound identifier whose name is the value of the argument N. If the binding is an uninitialized binding
        # throw a ReferenceError exception. A property named N normally already exists but if it does not or is not
        # currently writable, error handling is determined by the value of the Boolean argument S.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. If DclRec.HasBinding(N) is true, then
        if self.declarative_record.HasBinding(name):
            # a. Return DclRec.GetBindingValue(N, S).
            return self.declarative_record.GetBindingValue(name, strict)
        # 4. Let ObjRec be envRec.[[ObjectRecord]].
        # 5. Return ? ObjRec.GetBindingValue(N, S).
        return self.object_record.GetBindingValue(name, strict)

    # 8.1.1.4.7 DeleteBinding ( N )
    def DeleteBinding(self, name):
        # The concrete Environment Record method DeleteBinding for global Environment Records can only delete bindings
        # that have been explicitly designated as being subject to deletion.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. If DclRec.HasBinding(N) is true, then
        if self.declarative_record.HasBinding(name):
            # a. Return DclRec.DeleteBinding(N).
            return self.declarative_record.DeleteBinding(name)
        # 4. Let ObjRec be envRec.[[ObjectRecord]].
        # 5. Let globalObject be the binding object for ObjRec.
        global_object = self.object_record.binding_object
        # 6. Let existingProp be ? HasOwnProperty(globalObject, N).
        existing_prop = HasOwnProperty(global_object, name)
        # 7. If existingProp is true, then
        if existing_prop:
            # a. Let status be ? ObjRec.DeleteBinding(N).
            status = self.object_record.DeleteBinding(name)
            # b. If status is true, then
            if status:
                # i. Let varNames be envRec.[[VarNames]].
                # ii. If N is an element of varNames, remove that element from the varNames.
                try:
                    self.var_names.remove(name)
                except ValueError:
                    pass
            # c. Return status.
            return status
        # 8. Return true.
        return True

    # 8.1.1.4.8 HasThisBinding ( )
    def HasThisBinding(self):
        # Return true.
        return True

    # 8.1.1.4.9 HasSuperBinding ( )
    def HasSuperBinding(self):
        # Return false.
        return False

    # 8.1.1.4.10 WithBaseObject ( )
    def WithBaseObject(self):
        # Global Environment Records always return undefined as their WithBaseObject.
        # Return undefined.
        return None

    # 8.1.1.4.11 GetThisBinding ( )
    def GetThisBinding(self):
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Return envRec.[[GlobalThisValue]].
        return self.global_this_value

    # 8.1.1.4.12 HasVarDeclaration ( N )
    def HasVarDeclaration(self, name):
        # The concrete Environment Record method HasVarDeclaration for global Environment Records determines if the
        # argument identifier has a binding in this record that was created using a VariableStatement or a
        # FunctionDeclaration:
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let varDeclaredNames be envRec.[[VarNames]].
        # 3. If varDeclaredNames contains N, return true.
        # 4. Return false.
        return name in self.var_names

    # 8.1.1.4.13 HasLexicalDeclaration ( N )
    def HasLexicalDeclaration(self, name):
        # The concrete Environment Record method HasLexicalDeclaration for global Environment Records determines if the
        # argument identifier has a binding in this record that was created using a lexical declaration such as a
        # LexicalDeclaration or a ClassDeclaration:
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. Return DclRec.HasBinding(N).
        return self.declarative_record.HasBinding(name)

    # 8.1.1.4.14 HasRestrictedGlobalProperty ( N )
    def HasRestrictedGlobalProperty(self, name):
        # The concrete Environment Record method HasRestrictedGlobalProperty for global Environment Records determines
        # if the argument identifier is the name of a property of the global object that must not be shadowed by a
        # global lexical binding:
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let ObjRec be envRec.[[ObjectRecord]].
        # 3. Let globalObject be the binding object for ObjRec.
        global_object = self.object_record.binding_object
        # 4. Let existingProp be ? globalObject.[[GetOwnProperty]](N).
        existing_prop = global_object.GetOwnProperty(name)
        # 5. If existingProp is undefined, return false.
        if isUndefined(existing_prop):
            return False
        # 6. If existingProp.[[Configurable]] is true, return false.
        # 7. Return true.
        return not existing_prop.Configurable
        # NOTE
        # Properties may exist upon a global object that were directly created rather than being declared using a var
        # or function declaration. A global lexical binding may not be created that has the same name as a
        # non-configurable property of the global object. The global property undefined is an example of such a
        # property.

    # 8.1.1.4.15 CanDeclareGlobalVar ( N )
    def CanDeclareGlobalVar(self, name):
        # The concrete Environment Record method CanDeclareGlobalVar for global Environment Records determines if a
        # corresponding CreateGlobalVarBinding call would succeed if called for the same argument N. Redundant var
        # declarations and var declarations for pre-existing global object properties are allowed.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let ObjRec be envRec.[[ObjectRecord]].
        # 3. Let globalObject be the binding object for ObjRec.
        global_object = self.object_record.binding_object
        # 4. Let hasProperty be ? HasOwnProperty(globalObject, N).
        has_property = HasOwnProperty(global_object, name)
        # 5. If hasProperty is true, return true.
        if has_property:
            return True
        # 6. Return ? IsExtensible(globalObject).
        return IsExtensible(global_object)

    # 8.1.1.4.16 CanDeclareGlobalFunction ( N )
    def CanDeclareGlobalFunction(self, name):
        # The concrete Environment Record method CanDeclareGlobalFunction for global Environment Records determines if
        # a corresponding CreateGlobalFunctionBinding call would succeed if called for the same argument N.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let ObjRec be envRec.[[ObjectRecord]].
        # 3. Let globalObject be the binding object for ObjRec.
        global_object = self.object_record.binding_object
        # 4. Let existingProp be ? globalObject.[[GetOwnProperty]](N).
        existing_prop = global_object.GetOwnProperty(name)
        # 5. If existingProp is undefined, return ? IsExtensible(globalObject).
        if isUndefined(existing_prop):
            return IsExtensible(global_object)
        # 6. If existingProp.[[Configurable]] is true, return true.
        if existing_prop['configurable']:
            return True
        # 7. If IsDataDescriptor(existingProp) is true and existingProp has attribute values { [[Writable]]: true,
        #    [[Enumerable]]: true }, return true.
        if IsDataDescriptor(existing_prop) and existing_prop['writable'] and existing_prop['enumerable']:
            return True
        # 8. Return false.
        return False

    # 8.1.1.4.17 CreateGlobalVarBinding ( N, D )
    def CreateGlobalVarBinding(self, name, deletable):
        # The concrete Environment Record method CreateGlobalVarBinding for global Environment Records creates and
        # initializes a mutable binding in the associated object Environment Record and records the bound name in the
        # associated [[VarNames]] List. If a binding already exists, it is reused and assumed to be initialized.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let ObjRec be envRec.[[ObjectRecord]].
        # 3. Let globalObject be the binding object for ObjRec.
        global_object = self.object_record.binding_object
        # 4. Let hasProperty be ? HasOwnProperty(globalObject, N).
        has_property = HasOwnProperty(global_object, name)
        # 5. Let extensible be ? IsExtensible(globalObject).
        extensible = IsExtensible(global_object)
        # 6. If hasProperty is false and extensible is true, then
        if not has_property and extensible:
            # a. Perform ? ObjRec.CreateMutableBinding(N, D).
            self.object_record.CreateMutableBinding(name, deletable)
            # b. Perform ? ObjRec.InitializeBinding(N, undefined).
            self.object_record.InitializeBinding(name, None)
        # 7. Let varDeclaredNames be envRec.[[VarNames]].
        # 8. If varDeclaredNames does not contain N, then
        if name not in self.var_names:
            # a. Append N to varDeclaredNames.
            self.var_names.append(name)
        # 9. Return NormalCompletion(empty).
        return Empty.EMPTY

    # 8.1.1.4.18 CreateGlobalFunctionBinding ( N, V, D )
    def CreateGlobalFunctionBinding(self, name, value, deletable):
        # The concrete Environment Record method CreateGlobalFunctionBinding for global Environment Records creates and
        # initializes a mutable binding in the associated object Environment Record and records the bound name in the
        # associated [[VarNames]] List. If a binding already exists, it is replaced.
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let ObjRec be envRec.[[ObjectRecord]].
        # 3. Let globalObject be the binding object for ObjRec.
        global_object = self.object_record.binding_object
        # 4. Let existingProp be ? globalObject.[[GetOwnProperty]](N).
        existing_prop = global_object.GetOwnProperty(name)
        # 5. If existingProp is undefined or existingProp.[[Configurable]] is true, then
        if existing_prop is None or existing_prop['configurable']:
            # a. Let desc be the PropertyDescriptor { [[Value]]: V, [[Writable]]: true, [[Enumerable]]: true,
            #    [[Configurable]]: D }.
            desc = { 'value': value, 'writable': True, 'enumerable': True, 'configurable': deletable }
        # 6. Else,
        else:
            # a. Let desc be the PropertyDescriptor { [[Value]]: V }.
            desc = { 'value': value }
        # 7. Perform ? DefinePropertyOrThrow(globalObject, N, desc).
        DefinePropertyOrThrow(global_object, name, desc)
        # 8. Record that the binding for N in ObjRec has been initialized.
        # .... object records aren't supposed to need to do this, so I'm not sure where I'm storing this info and when
        # it's used. A problem for later....
        pass
        # 9. Perform ? Set(globalObject, N, V, false).
        Set(global_object, name, value, False)
        # 10. Let varDeclaredNames be envRec.[[VarNames]].
        # 11. If varDeclaredNames does not contain N, then
        if name not in self.var_names:
            # a. Append N to varDeclaredNames.
            self.var_names.append(name)
        # 12. Return NormalCompletion(empty).
        return Empty.EMPTY
        # NOTE
        # Global function declarations are always represented as own properties of the global object. If possible, an
        # existing own property is reconfigured to have a standard set of attribute values. Steps 8-9 are equivalent to
        # what calling the InitializeBinding concrete method would do and if globalObject is a Proxy will produce the
        # same sequence of Proxy trap calls.

class LexicalEnvironment:
    pass

# 8.1.2.1 GetIdentifierReference ( lex, name, strict )
def GetIdentifierReference(lex, name, strict):
    # The abstract operation GetIdentifierReference is called with a Lexical Environment lex, a String name, and a
    # Boolean flag strict. The value of lex may be null. When called, the following steps are performed:
    #
    # 1. If lex is the value null, then
    if isNull(lex):
        # a. Return a value of type Reference whose base value component is undefined, whose referenced name component
        #    is name, and whose strict reference flag is strict.
        return Reference(None, name, strict)
    # 2. Let envRec be lex's EnvironmentRecord.
    env_rec = lex.environment_record
    # 3. Let exists be ? envRec.HasBinding(name).
    exists = env_rec.HasBinding(name)
    # 4. If exists is true, then
    if exists:
        # a. Return a value of type Reference whose base value component is envRec, whose referenced name component is
        # name, and whose strict reference flag is strict.
        return Reference(env_rec, name, strict)
    # 5. Else,
    # a. Let outer be the value of lex's outer environment reference.
    outer = lex.outer
    # b. Return ? GetIdentifierReference(outer, name, strict).
    return GetIdentifierReference(outer, name, strict)

# 8.1.2.2 NewDeclarativeEnvironment ( E )
def NewDeclarativeEnvironment(outer):
    # When the abstract operation NewDeclarativeEnvironment is called with a Lexical Environment as argument E the
    # following steps are performed:
    #
    # 1. Let env be a new Lexical Environment.
    env = LexicalEnvironment()
    # 2. Let envRec be a new declarative Environment Record containing no bindings.
    env_rec = DeclarativeEnvironmentRecord()
    # 3. Set env's EnvironmentRecord to envRec.
    env.environment_record = env_rec
    # 4. Set the outer lexical environment reference of env to E.
    env.outer = outer
    # 5. Return env.
    return env

# 8.1.2.3 NewObjectEnvironment ( O, E )
def NewObjectEnvironment(obj, outer):
    # When the abstract operation NewObjectEnvironment is called with an Object O and a Lexical Environment E as
    # arguments, the following steps are performed:
    #
    # 1. Let env be a new Lexical Environment.
    env = LexicalEnvironment()
    # 2. Let envRec be a new object Environment Record containing O as the binding object.
    env_rec = ObjectEnvironmentRecord(obj)
    # 3. Set env's EnvironmentRecord to envRec.
    env.environment_record = env_rec
    # 4. Set the outer lexical environment reference of env to E.
    env.outer = outer
    # 5. Return env.
    return env

# 8.1.2.4 NewFunctionEnvironment ( F, newTarget )
def NewFunctionEnvironment(func, new_target):
    # When the abstract operation NewFunctionEnvironment is called with arguments F and newTarget the following steps
    # are performed:
    #
    # 1. Assert: F is an ECMAScript function.
    # 2. Assert: Type(newTarget) is Undefined or Object.
    assert isUndefined(new_target) or isObject(new_target)
    # 3. Let env be a new Lexical Environment.
    env = LexicalEnvironment()
    # 4. Let envRec be a new function Environment Record containing no bindings.
    env_rec = FunctionEnvironmentRecord()
    # 5. Set envRec.[[FunctionObject]] to F.
    env_rec.function_object = func
    # 6. If F.[[ThisMode]] is lexical, set envRec.[[ThisBindingStatus]] to "lexical".
    if func.ThisMode == TM.LEXICAL:
        env_rec.this_binding_status = 'lexical'
    # 7. Else, set envRec.[[ThisBindingStatus]] to "uninitialized".
    else:
        env_rec.this_binding_status = 'uninitialized'
    # 8. Let home be F.[[HomeObject]].
    home = func.HomeObject
    # 9. Set envRec.[[HomeObject]] to home.
    env_rec.home_object = home
    # 10. Set envRec.[[NewTarget]] to newTarget.
    env_rec.new_target = new_target
    # 11. Set env's EnvironmentRecord to envRec.
    env.environment_record = env_rec
    # 12. Set the outer lexical environment reference of env to F.[[Environment]].
    env.outer = func.Environment
    # 13. Return env.
    return env

# 8.1.2.5 NewGlobalEnvironment ( G, thisValue )
def NewGlobalEnvironment(global_obj, this_value):
    # When the abstract operation NewGlobalEnvironment is called with arguments G and thisValue, the following steps
    # are performed:
    #
    # 1. Let env be a new Lexical Environment.
    env = LexicalEnvironment()
    # 2. Let objRec be a new object Environment Record containing G as the binding object.
    # 3. Let dclRec be a new declarative Environment Record containing no bindings.
    # 4. Let globalRec be a new global Environment Record.
    # 5. Set globalRec.[[ObjectRecord]] to objRec.
    # 6. Set globalRec.[[GlobalThisValue]] to thisValue.
    # 7. Set globalRec.[[DeclarativeRecord]] to dclRec.
    # 8. Set globalRec.[[VarNames]] to a new empty List.
    global_rec = GlobalEnvironmentRecord(global_obj, this_value)
    # 9. Set env's EnvironmentRecord to globalRec.
    env.environment_record = global_rec
    # 10. Set the outer lexical environment reference of env to null.
    env.outer = JSNull.NULL
    # 11. Return env.
    return env

#########################################################################################################################
#
#  .d8888b.       .d8888b.      8888888b.                    888
# d88P  Y88b     d88P  Y88b     888   Y88b                   888
# Y88b. d88P            888     888    888                   888
#  "Y88888"           .d88P     888   d88P  .d88b.   8888b.  888 88888b.d88b.  .d8888b
# .d8P""Y8b.      .od888P"      8888888P"  d8P  Y8b     "88b 888 888 "888 "88b 88K
# 888    888     d88P"          888 T88b   88888888 .d888888 888 888  888  888 "Y8888b.
# Y88b  d88P d8b 888"           888  T88b  Y8b.     888  888 888 888  888  888      X88
#  "Y8888P"  Y8P 888888888      888   T88b  "Y8888  "Y888888 888 888  888  888  88888P'
#
#########################################################################################################################
# 8.2 Realms
#
# Before it is evaluated, all ECMAScript code must be associated with a realm. Conceptually, a realm consists of a set
# of intrinsic objects, an ECMAScript global environment, all of the ECMAScript code that is loaded within the scope of
# that global environment, and other associated state and resources.
#
# A realm is represented in this specification as a Realm Record with the fields specified in Table 20:
#
# Table 20: Realm Record Fields
# +------------------+----------------------------------------+------------------------------------------------------
# | Field Name       | Value                                  | Meaning
# +------------------+----------------------------------------+------------------------------------------------------
# | [[Intrinsics]]   | Record whose field names are intrinsic | The intrinsic values used by code associated with
# |                  | keys and whose values are objects      | this realm
# +------------------+----------------------------------------+------------------------------------------------------
# | [[GlobalObject]] | Object                                 | The global object for this realm
# +------------------+----------------------------------------+------------------------------------------------------
# | [[GlobalEnv]]    | Lexical Environment                    | The global environment for this realm
# +------------------+----------------------------------------+------------------------------------------------------
# | [[TemplateMap]]  | A List of Record                       | Template objects are canonicalized separately for
# |                  | { [[Site]]: Parse Node,                | each realm using its Realm Record's [[TemplateMap]].
# |                  |   [[Array]]: Object }.                 | Each [[Site]] value is a Parse Node that is a
# |                  |                                        | TemplateLiteral. The associated [[Array]] value is the
# |                  |                                        | corresponding template object that is passed to a tag
# |                  |                                        | function.
# +------------------+----------------------------------------+------------------------------------------------------
# | [[HostDefined]]  |   Any, default value is undefined.     | Field reserved for use by host environments that need
# |                  |                                        | to associate additional information with a Realm
# |                  |                                        | Record.
# +------------------+----------------------------------------+------------------------------------------------------
#
# NOTE (for [[TemplateMap]])
# Once a Parse Node becomes unreachable, the corresponding [[Array]] is also unreachable, and it would be unobservable
# if an implementation removed the pair from the [[TemplateMap]] list.

class Realm:
    def __init__(self):
        self.intrinsics = {}
        self.global_object = None
        self.global_env = None
        self.template_map = []
        self.host_defined = None

# 8.2.1 CreateRealm ( )
def CreateRealm():
    # The abstract operation CreateRealm with no arguments performs the following steps:
    # 1. Let realmRec be a new Realm Record.
    realm_rec = Realm()
    # 2. Perform CreateIntrinsics(realmRec).
    CreateIntrinsics(realm_rec)
    # 3. Set realmRec.[[GlobalObject]] to undefined.
    pass  # Did this in the record creation already
    # 4. Set realmRec.[[GlobalEnv]] to undefined.
    pass  # Did this in the record creation already
    # 5. Set realmRec.[[TemplateMap]] to a new empty List.
    pass
    # 6. Return realmRec.
    return realm_rec

def internal_throw_type_error(*args, **kwargs):
    raise ESTypeError('type invalid for operation')

# 8.2.2 CreateIntrinsics ( realmRec )
def CreateIntrinsics(realm_rec):
    # The abstract operation CreateIntrinsics with argument realmRec performs the following steps:
    #
    # 1. Let intrinsics be a new Record.
    intrinsics = {}
    # 2. Set realmRec.[[Intrinsics]] to intrinsics.
    realm_rec.intrinsics = intrinsics
    # 3. Let objProto be ObjectCreate(null).
    obj_proto = ObjectCreate(JSNull.NULL)
    # Transform that prototype into an exotic object. See 19.1.3.
    obj_proto.SetPrototypeOf = types.MethodType(SetImmutablePrototype, obj_proto)
    # 4. Set intrinsics.[[%ObjectPrototype%]] to objProto.
    intrinsics['%ObjectPrototype%'] = obj_proto
    # 5. Let throwerSteps be the algorithm steps specified in 9.2.9.1 for the %ThrowTypeError% function.
    thrower_steps = internal_throw_type_error
    # 6. Let thrower be CreateBuiltinFunction(throwerSteps, « », realmRec, null).
    thrower = CreateBuiltinFunction(thrower_steps, [], realm_rec, JSNull.NULL)
    # 7. Set intrinsics.[[%ThrowTypeError%]] to thrower.
    intrinsics['%ThrowTypeError%'] = thrower
    # 8. Let noSteps be an empty sequence of algorithm steps.
    no_steps = lambda *args, **kwargs: None
    # 9. Let funcProto be CreateBuiltinFunction(noSteps, « », realmRec, objProto).
    func_proto = CreateBuiltinFunction(no_steps, [], realm_rec, obj_proto)
    # 10. Set intrinsics.[[%FunctionPrototype%]] to funcProto.
    intrinsics['%FunctionPrototype%'] = func_proto
    # 11. Call thrower.[[SetPrototypeOf]](funcProto).
    thrower.SetPrototypeOf(func_proto)
    # 12. Perform AddRestrictedFunctionProperties(funcProto, realmRec).
    AddRestrictedFunctionProperties(func_proto, realm_rec)
    # 13. Set fields of intrinsics with the values listed in Table 7 that have not already been handled above. The
    #     field names are the names listed in column one of the table. The value of each field is a new object
    #     value fully and recursively populated with property values as defined by the specification of each object
    #     in clauses 18-26. All object property values are newly created object values. All values that are
    #     built-in function objects are created by performing CreateBuiltinFunction(<steps>, <slots>, realmRec,
    #     <prototype>) where <steps> is the definition of that function provided by this specification, <slots> is
    #     a list of the names, if any, of the function's specified internal slots, and <prototype> is the specified
    #     value of the function's [[Prototype]] internal slot. The creation of the intrinsics and their properties
    #     must be ordered to avoid any dependencies upon objects that have not yet been created.
    intrinsics['%Object%'] = CreateObjectConstructor(realm_rec)
    AddObjectPrototypeProps(realm_rec)
    intrinsics['%Error%'] = CreateErrorConstructor(realm_rec)
    intrinsics['%ErrorPrototype%'] = CreateErrorPrototype(realm_rec)
    ErrorFixups(realm_rec)
    for name in ['Eval', 'Range', 'Reference', 'Syntax', 'Type', 'URI']:
        intrinsics[f'%{name}Error%'] = CreateNativeErrorConstructor(realm_rec, name)
        intrinsics[f'%{name}ErrorPrototype%'] = CreateNativeErrorPrototype(realm_rec, name)
    NativeErrorFixups(realm_rec)
    intrinsics['%IteratorPrototype%'] = CreateIteratorPrototype(realm_rec)
    intrinsics['%Boolean%'] = CreateBooleanConstructor(realm_rec)
    intrinsics['%BooleanPrototype%'] = CreateBooleanPrototype(realm_rec)
    BooleanFixups(realm_rec)
    intrinsics['%Number%'] = CreateNumberConstructor(realm_rec)
    intrinsics['%NumberPrototype%'] = CreateNumberPrototype(realm_rec)
    NumberFixups(realm_rec)
    intrinsics['%String%'] = CreateStringConstructor(realm_rec)
    intrinsics['%StringPrototype%'] = CreateStringPrototype(realm_rec)
    StringFixups(realm_rec)
    intrinsics['%Array%'] = CreateArrayConstructor(realm_rec)
    intrinsics['%ArrayPrototype%'] = CreateArrayPrototype(realm_rec)
    ArrayFixups(realm_rec)
    intrinsics['%ArrayIteratorPrototype%'] = CreateArrayIteratorPrototype(realm_rec)

    # 14. Return intrinsics.
    return intrinsics

# 8.2.3 SetRealmGlobalObject ( realmRec, globalObj, thisValue )
def SetRealmGlobalObject(realm_rec, global_obj, this_value):
    # The abstract operation SetRealmGlobalObject with arguments realmRec, globalObj, and thisValue performs the
    # following steps:
    #
    # 1. If globalObj is undefined, then
    if global_obj is None:
        # a. Let intrinsics be realmRec.[[Intrinsics]].
        intrinsics = realm_rec.intrinsics
        # b. Set globalObj to ObjectCreate(intrinsics.[[%ObjectPrototype%]]).
        global_obj = ObjectCreate(intrinsics['%ObjectPrototype%'])
    # 2. Assert: Type(globalObj) is Object.
    assert isObject(global_obj)
    # 3. If thisValue is undefined, set thisValue to globalObj.
    if this_value is None:
        this_value = global_obj
    # 4. Set realmRec.[[GlobalObject]] to globalObj.
    realm_rec.global_object = global_obj
    # 5. Let newGlobalEnv be NewGlobalEnvironment(globalObj, thisValue).
    new_global_env = NewGlobalEnvironment(global_obj, this_value)
    # 6. Set realmRec.[[GlobalEnv]] to newGlobalEnv.
    realm_rec.global_env = new_global_env
    # 7. Return realmRec.
    return realm_rec

# 8.2.4 SetDefaultGlobalBindings ( realmRec )
def SetDefaultGlobalBindings(realm_rec):
    # The abstract operation SetDefaultGlobalBindings with argument realmRec performs the following steps:
    #
    # 1. Let global be realmRec.[[GlobalObject]].
    globl = realm_rec.global_object
    # 2. For each property of the Global Object specified in clause 18, do
    global_values = [
        ('Infinity', math.inf),
        ('NaN', math.nan),
        ('undefined', None)
    ]
    global_intrinsics = [
        'eval',
        'isFinfite',
        'isNaN',
        'parseFloat',
        'parseInt',
        'decodeURI',
        'decodeURIComponent',
        'encodeURI',
        'encodeURIComponent',
        'Array',
        'ArrayBuffer',
        'Boolean',
        'DataView',
        'Date',
        'Error',
        'EvalError',
        'Float32Array',
        'Float64Array',
        'Function',
        'Int8Array',
        'Int16Array',
        'Int32Array',
        'Map', 'Number', 'Object', 'Promise', 'Proxy', 'RangeError', 'ReferenceError', 'RegExp', 'Set', 'SharedArrayBuffer',
        'String', 'Symbol', 'SyntaxError', 'TypeError', 'Uint8Array', 'Uint8ClampedArray', 'Uint16Array', 'Uint32Array',
        'URIError', 'WeapMap', 'WeakSet', 'Atomics', 'JSON', 'Math', 'Reflect']
    # @@@ Note: The "if" clause, below, should not be there. It's just to allow this fcn to work even if I haven't
    # implemented everything yet.
    for name, value in chain(global_values, ((name, realm_rec.intrinsics['%'+name+'%']) for name in global_intrinsics if '%'+name+"%" in realm_rec.intrinsics)):
        # a. Let name be the String value of the property name.x
        # b. Let desc be the fully populated data property descriptor for the property containing the specified
        #    attributes for the property. For properties listed in 18.2, 18.3, or 18.4 the value of the [[Value]]
        #    attribute is the corresponding intrinsic object from realmRec.
        # c. Perform ? DefinePropertyOrThrow(global, name, desc).
        desc = PropertyDescriptor(value=value, writable=False, enumerable=False, configurable=False)
        DefinePropertyOrThrow(globl, name, desc)
    # 3. Return global.
    return globl

######################################################################################################################################################################################
#
#  .d8888b.       .d8888b.      8888888888                                     888    d8b                        .d8888b.                    888                      888
# d88P  Y88b     d88P  Y88b     888                                            888    Y8P                       d88P  Y88b                   888                      888
# Y88b. d88P          .d88P     888                                            888                              888    888                   888                      888
#  "Y88888"          8888"      8888888    888  888  .d88b.   .d8888b 888  888 888888 888  .d88b.  88888b.      888         .d88b.  88888b.  888888  .d88b.  888  888 888888 .d8888b
# .d8P""Y8b.          "Y8b.     888        `Y8bd8P' d8P  Y8b d88P"    888  888 888    888 d88""88b 888 "88b     888        d88""88b 888 "88b 888    d8P  Y8b `Y8bd8P' 888    88K
# 888    888     888    888     888          X88K   88888888 888      888  888 888    888 888  888 888  888     888    888 888  888 888  888 888    88888888   X88K   888    "Y8888b.
# Y88b  d88P d8b Y88b  d88P     888        .d8""8b. Y8b.     Y88b.    Y88b 888 Y88b.  888 Y88..88P 888  888     Y88b  d88P Y88..88P 888  888 Y88b.  Y8b.     .d8""8b. Y88b.       X88
#  "Y8888P"  Y8P  "Y8888P"      8888888888 888  888  "Y8888   "Y8888P  "Y88888  "Y888 888  "Y88P"  888  888      "Y8888P"   "Y88P"  888  888  "Y888  "Y8888  888  888  "Y888  88888P'
#
######################################################################################################################################################################################
# 8.3 Execution Contexts
#
# An execution context is a specification device that is used to track the runtime evaluation of code by an ECMAScript
# implementation. At any point in time, there is at most one execution context per agent that is actually executing
# code. This is known as the agent's running execution context. All references to the running execution context in this
# specification denote the running execution context of the surrounding agent.
#
# The execution context stack is used to track execution contexts. The running execution context is always the top
# element of this stack. A new execution context is created whenever control is transferred from the executable code
# associated with the currently running execution context to executable code that is not associated with that execution
# context. The newly created execution context is pushed onto the stack and becomes the running execution context.
#
# An execution context contains whatever implementation specific state is necessary to track the execution progress of
# its associated code. Each execution context has at least the state components listed in Table 21.
#
# Table 21: State Components for All Execution Contexts
# +-----------------------+---------------------------------------------------------------------------------------------
# | Component             | Purpose
# +-----------------------+---------------------------------------------------------------------------------------------
# | code evaluation state | Any state needed to perform, suspend, and resume evaluation of the code associated with
# |                       | this execution context.
# +-----------------------+---------------------------------------------------------------------------------------------
# | Function              | If this execution context is evaluating the code of a function object, then the value of
# |                       | this component is that function object. If the context is evaluating the code of a Script
# |                       | or Module, the value is null.
# +-----------------------+---------------------------------------------------------------------------------------------
# | Realm                 | The Realm Record from which associated code accesses ECMAScript resources.
# +-----------------------+---------------------------------------------------------------------------------------------
# | ScriptOrModule        | The Module Record or Script Record from which associated code originates. If there is no
# |                       | originating script or module, as is the case for the original execution context created in
# |                       | InitializeHostDefinedRealm, the value is null.
# +-----------------------+---------------------------------------------------------------------------------------------
#
# Evaluation of code by the running execution context may be suspended at various points defined within this
# specification. Once the running execution context has been suspended a different execution context may become the
# running execution context and commence evaluating its code. At some later time a suspended execution context may
# again become the running execution context and continue evaluating its code at the point where it had previously been
# suspended. Transition of the running execution context status among execution contexts usually occurs in stack-like
# last-in/first-out manner. However, some ECMAScript features require non-LIFO transitions of the running execution
# context.
#
# The value of the Realm component of the running execution context is also called the current Realm Record. The value
# of the Function component of the running execution context is also called the active function object.
#
# Execution contexts for ECMAScript code have the additional state components listed in Table 22.
#
# Table 22: Additional State Components for ECMAScript Code Execution Contexts
# +---------------------+-----------------------------------------------------------------------------------------------
# | Component           | Purpose
# +---------------------+-----------------------------------------------------------------------------------------------
# | LexicalEnvironment  | Identifies the Lexical Environment used to resolve identifier references made by code within
# |                     | this execution context.
# +---------------------+-----------------------------------------------------------------------------------------------
# | VariableEnvironment | Identifies the Lexical Environment whose EnvironmentRecord holds bindings created by
# |                     | VariableStatements within this execution context.
# +---------------------+-----------------------------------------------------------------------------------------------
#
# The LexicalEnvironment and VariableEnvironment components of an execution context are always Lexical Environments.
#
# Execution contexts representing the evaluation of generator objects have the additional state components listed in
# Table 23.
#
# Table 23: Additional State Components for Generator Execution Contexts
# +-----------+---------------------------------------------------------------------------------------------------------
# | Component | Purpose
# +-----------+---------------------------------------------------------------------------------------------------------
# | Generator | The GeneratorObject that this execution context is evaluating.
# +-----------+---------------------------------------------------------------------------------------------------------
#
# In most situations only the running execution context (the top of the execution context stack) is directly manipulated
# by algorithms within this specification. Hence when the terms “LexicalEnvironment”, and “VariableEnvironment” are
# used without qualification they are in reference to those components of the running execution context.
#
# An execution context is purely a specification mechanism and need not correspond to any particular artefact of an
# ECMAScript implementation. It is impossible for ECMAScript code to directly access or observe an execution context.
class ExecutionContext:
    def __init__(self):
        self.state = None
        self.function = JSNull.NULL
        self.realm = None
        self.script_or_module = JSNull.NULL
        self.lexical_environment = None
        self.variable_environment = None
        self.generator = None
    def suspend(self):
        pass

# 8.3.1 GetActiveScriptOrModule ( )
def GetActiveScriptOrModule():
    # The GetActiveScriptOrModule abstract operation is used to determine the running script or module, based on the
    # running execution context. GetActiveScriptOrModule performs the following steps:
    #
    # 1. If the execution context stack is empty, return null.
    # 2. Let ec be the topmost execution context on the execution context stack whose ScriptOrModule component is not
    #    null.
    # 3. If no such execution context exists, return null. Otherwise, return ec's ScriptOrModule component.
    for idx in range(len(surrounding_agent.ec_stack)-1,-1,-1):
        som = surrounding_agent.ec_stack[idx].script_or_module
        if not isNull(som):
            return som
    return JSNull.NULL

# 8.3.2 ResolveBinding ( name [ , env ] )
def ResolveBinding(name, env=None):
    # The ResolveBinding abstract operation is used to determine the binding of name passed as a String value. The
    # optional argument env can be used to explicitly provide the Lexical Environment that is to be searched for the
    # binding. During execution of ECMAScript code, ResolveBinding is performed using the following algorithm:
    #
    # 1. If env is not present or if env is undefined, then
    if env is None:
        # a. Set env to the running execution context's LexicalEnvironment.
        env = surrounding_agent.running_ec.lexical_environment
    # 2. Assert: env is a Lexical Environment.
    assert isinstance(env, LexicalEnvironment)
    # 3. If the code matching the syntactic production that is being evaluated is contained in strict mode code, let
    #    strict be true, else let strict be false.
    strict = False #FigureOutWhatTheHeckThisIs()  # See 10.2.1.
    # 4. Return ? GetIdentifierReference(env, name, strict).
    return GetIdentifierReference(env, name, strict)
    # NOTE
    # The result of ResolveBinding is always a Reference value with its referenced name component equal to the name
    # argument.

# 8.3.3 GetThisEnvironment ( )
def GetThisEnvironment():
    # The abstract operation GetThisEnvironment finds the Environment Record that currently supplies the binding of the
    # keyword this. GetThisEnvironment performs the following steps:
    #
    # 1. Let lex be the running execution context's LexicalEnvironment.
    lex = surrounding_agent.running_ec.lexical_environment
    # 2. Repeat,
    while 1:
        # a. Let envRec be lex's EnvironmentRecord.
        env_rec = lex.environment_record
        # b. Let exists be envRec.HasThisBinding().
        exists = env_rec.HasThisBinding()
        # c. If exists is true, return envRec.
        if exists:
            return env_rec
        # d. Let outer be the value of lex's outer environment reference.
        outer = lex.outer
        # e. Assert: outer is not null.
        assert not isNull(outer)
        # f. Set lex to outer.
        lex = outer
    # NOTE
    # The loop in step 2 will always terminate because the list of environments always ends with the global environment
    # which has a this binding.

# 8.3.4 ResolveThisBinding ( )
def ResolveThisBinding():
    # The abstract operation ResolveThisBinding determines the binding of the keyword this using the LexicalEnvironment
    # of the running execution context. ResolveThisBinding performs the following steps:
    #
    # 1. Let envRec be GetThisEnvironment().
    env_rec = GetThisEnvironment()
    # 2. Return ? envRec.GetThisBinding().
    return env_rec.GetThisBinding()

# 8.3.5 GetNewTarget ( )
def GetNewTarget():
    # The abstract operation GetNewTarget determines the NewTarget value using the LexicalEnvironment of the running
    # execution context. GetNewTarget performs the following steps:
    #
    # 1. Let envRec be GetThisEnvironment().
    env_rec = GetThisEnvironment()
    # 2. Assert: envRec has a [[NewTarget]] field.
    assert hasattr(env_rec, 'new_target')
    # 3. Return envRec.[[NewTarget]].
    return env_rec.new_target

def GetActiveFunction():
    # From the header of section 8.3:
    # The value of the Function component of the running execution context is also called the active function object.
    return surrounding_agent.running_ec.function

# 8.3.6 GetGlobalObject ( )
def GetGlobalObject():
    # The abstract operation GetGlobalObject returns the global object used by the currently running execution context.
    # GetGlobalObject performs the following steps:
    #
    # 1. Let ctx be the running execution context.
    ctx = surrounding_agent.running_ec
    # 2. Let currentRealm be ctx's Realm.
    current_realm = ctx.realm
    # 3. Return currentRealm.[[GlobalObject]].
    return current_realm.global_object

# 8.4 Jobs and Job Queues
#
# A Job is an abstract operation that initiates an ECMAScript computation when no other ECMAScript computation is
# currently in progress. A Job abstract operation may be defined to accept an arbitrary set of job parameters.
#
# Execution of a Job can be initiated only when there is no running execution context and the execution context stack
# is empty. A PendingJob is a request for the future execution of a Job. A PendingJob is an internal Record whose
# fields are specified in Table 24. Once execution of a Job is initiated, the Job always executes to completion. No
# other Job may be initiated until the currently running Job completes. However, the currently running Job or external
# events may cause the enqueuing of additional PendingJobs that may be initiated sometime after completion of the
# currently running Job.
#
# Table 24: PendingJob Record Fields
# +--------------------+----------------------------+------------------------------------------------------------------
# | Field Name         | Value                      | Meaning
# +--------------------+----------------------------+------------------------------------------------------------------
# | [[Job]]            | The name of a Job abstract | This is the abstract operation that is performed when execution
# |                    | operation                  |  of this PendingJob is initiated.
# +--------------------+----------------------------+------------------------------------------------------------------
# | [[Arguments]]      | A List                     | The List of argument values that are to be passed to [[Job]] when
# |                    |                            | it is activated.
# +--------------------+----------------------------+------------------------------------------------------------------
# | [[Realm]]          | A Realm Record             | The Realm Record for the initial execution context when this
# |                    |                            | PendingJob is initiated.
# +--------------------+----------------------------+------------------------------------------------------------------
# | [[ScriptOrModule]] | A Script Record or Module  | The script or module for the initial execution context when this
# |                    | Record                     | PendingJob is initiated.
# +--------------------+----------------------------+------------------------------------------------------------------
# | [[HostDefined]]    | Any, default value is      | Field reserved for use by host environments that need to
# |                    | undefined.                 |  associate additional information with a pending Job.
# +--------------------+----------------------------+------------------------------------------------------------------
#
# A Job Queue is a FIFO queue of PendingJob records. Each Job Queue has a name and the full set of available Job Queues
# are defined by an ECMAScript implementation. Every ECMAScript implementation has at least the Job Queues defined in
# Table 25.
#
# Each agent has its own set of named Job Queues. All references to a named job queue in this specification denote the
# named job queue of the surrounding agent.
#
# Table 25: Required Job Queues
# +-------------+------------------------------------------------------------------------------------------------------
# | Name        | Purpose
# +-------------+------------------------------------------------------------------------------------------------------
# | ScriptJobs  | Jobs that validate and evaluate ECMAScript Script and Module source text. See clauses 10 and 15.
# +-------------+------------------------------------------------------------------------------------------------------
# | PromiseJobs | Jobs that are responses to the settlement of a Promise (see 25.6).
# +-------------+------------------------------------------------------------------------------------------------------
#
# A request for the future execution of a Job is made by enqueueing, on a Job Queue, a PendingJob record that includes
# a Job abstract operation name and any necessary argument values. When there is no running execution context and the
# execution context stack is empty, the ECMAScript implementation removes the first PendingJob from a Job Queue and
# uses the information contained in it to create an execution context and starts execution of the associated Job
# abstract operation.
#
# The PendingJob records from a single Job Queue are always initiated in FIFO order. This specification does not define
# the order in which multiple Job Queues are serviced. An ECMAScript implementation may interweave the FIFO evaluation
# of the PendingJob records of a Job Queue with the evaluation of the PendingJob records of one or more other Job
# Queues. An implementation must define what occurs when there are no running execution context and all Job Queues are
# empty.
#
# NOTE
# Typically an ECMAScript implementation will have its Job Queues pre-initialized with at least one PendingJob and one
# of those Jobs will be the first to be executed. An implementation might choose to free all resources and terminate if
# the current Job completes and all Job Queues are empty. Alternatively, it might choose to wait for a some
# implementation specific agent or mechanism to enqueue new PendingJob requests.



# 8.4.1 EnqueueJob ( queueName, job, arguments )
def EnqueueJob(queue_name, job, arguments):
    # The EnqueueJob abstract operation requires three arguments: queueName, job, and arguments. It performs the
    # following steps:
    #
    # 1. Assert: Type(queueName) is String and its value is the name of a Job Queue recognized by this implementation.
    assert isString(queue_name)
    # 2. Assert: job is the name of a Job.
    # 3. Assert: arguments is a List that has the same number of elements as the number of parameters required by job.
    # 4. Let callerContext be the running execution context.
    caller_context = surrounding_agent.running_ec
    # 5. Let callerRealm be callerContext's Realm.
    caller_realm = caller_context.realm
    # 6. Let callerScriptOrModule be callerContext's ScriptOrModule.
    caller_script_or_module = caller_context.script_or_module
    # 7. Let pending be PendingJob { [[Job]]: job, [[Arguments]]: arguments, [[Realm]]: callerRealm,
    #    [[ScriptOrModule]]: callerScriptOrModule, [[HostDefined]]: undefined }.
    pending = Record(job=job, arguments=arguments, realm=caller_realm, script_or_module=caller_script_or_module,
                     host_defined=None)
    # 8. Perform any implementation or host environment defined processing of pending. This may include modifying the
    #    [[HostDefined]] field or any other field of pending.
    # 9. Add pending at the back of the Job Queue named by queueName.
    surrounding_agent.job_queues[queue_name].append(pending)
    # 10. Return NormalCompletion(empty).
    return Empty.EMPTY

# 8.5 InitializeHostDefinedRealm ( )
def InitializeHostDefinedRealm():
    # The abstract operation InitializeHostDefinedRealm performs the following steps:
    #
    # 1. Let realm be CreateRealm().
    realm = CreateRealm()
    # 2. Let newContext be a new execution context.
    new_context = ExecutionContext()
    # 3. Set the Function of newContext to null.
    new_context.ecma_function = None
    # 4. Set the Realm of newContext to realm.
    new_context.realm = realm
    # 5. Set the ScriptOrModule of newContext to null.
    new_context.script_or_module = JSNull.NULL
    # 6. Push newContext onto the execution context stack; newContext is now the running execution context.
    surrounding_agent.ec_stack.append(new_context)
    surrounding_agent.running_ec = new_context
    # 7. If the host requires use of an exotic object to serve as realm's global object, let global be such an object
    #    created in an implementation-defined manner. Otherwise, let global be undefined, indicating that an ordinary
    #    object should be created as the global object.
    global_exotic = None
    # 8. If the host requires that the this binding in realm's global scope return an object other than the global
    #    object, let thisValue be such an object created in an implementation-defined manner. Otherwise, let thisValue
    #    be undefined, indicating that realm's global this binding should be the global object.
    this_value = None
    # 9. Perform SetRealmGlobalObject(realm, global, thisValue).
    SetRealmGlobalObject(realm, global_exotic, this_value)
    # 10. Let globalObj be ? SetDefaultGlobalBindings(realm).
    SetDefaultGlobalBindings(realm)
    # 11. Create any implementation-defined global object properties on globalObj.
    pass # (Assign the prior to a var and edit the object) Gonna want to add things like "console" here...
    # 12. Return NormalCompletion(empty).
    return Empty.EMPTY

# 8.6 RunJobs
def RunJobs(scripts=[], modules=[]):
    # 1. Perform ? InitializeHostDefinedRealm().
    InitializeHostDefinedRealm()
    # 2. In an implementation-dependent manner, obtain the ECMAScript source texts (see clause 10) and any
    #    associated host-defined values for zero or more ECMAScript scripts and/or ECMAScript modules.
    host_defined = None
    #    For each such sourceText and hostDefined, do
    #        a. If sourceText is the source code of a script, then
    #            i. Perform EnqueueJob("ScriptJobs", ScriptEvaluationJob, « sourceText, hostDefined »).
    #        b. Else sourceText is the source code of a module,
    #            i. Perform EnqueueJob("ScriptJobs", TopLevelModuleEvaluationJob, « sourceText, hostDefined »).
    for source_text in modules:
        EnqueueJob("ScriptJobs", TopLevelModuleEvaluationJob, [source_text, host_defined])
    for source_text in scripts:
        EnqueueJob("ScriptJobs", ScriptEvaluationJob, [source_text, host_defined])
    # 3. Repeat,
    while 1:
        # a. Suspend the running execution context and remove it from the execution context stack.
        surrounding_agent.running_ec.suspend()
        surrounding_agent.running_ec = None
        surrounding_agent.ec_stack.pop() # discarding the result
        # b. Assert: The execution context stack is now empty.
        assert len(surrounding_agent.ec_stack) == 0
        # c. Let nextQueue be a non-empty Job Queue chosen in an implementation-defined manner. If all Job Queues are
        #    empty, the result is implementation-defined.
        non_empty_job_queues = [ name for name, queue in surrounding_agent.job_queues.items() if len(queue) > 0 ]
        if len(non_empty_job_queues) == 0:
            break
        next_queue = random.choice(non_empty_job_queues)
        # d. Let nextPending be the PendingJob record at the front of nextQueue. Remove that record from nextQueue.
        next_pending = surrounding_agent.job_queues[next_queue].popleft()
        # e. Let newContext be a new execution context.
        new_context = ExecutionContext()
        # f. Set newContext's Function to null.
        new_context.ecma_function = JSNull.NULL
        # g. Set newContext's Realm to nextPending.[[Realm]].
        new_context.realm = next_pending.realm
        # h. Set newContext's ScriptOrModule to nextPending.[[ScriptOrModule]].
        new_context.script_or_module = next_pending.script_or_module
        # i. Push newContext onto the execution context stack; newContext is now the running execution context.
        surrounding_agent.ec_stack.append(new_context)
        surrounding_agent.running_ec = new_context
        # j. Perform any implementation or host environment defined job initialization using nextPending.
        # (Nothing yet.)
        # k. Let result be the result of performing the abstract operation named by nextPending.[[Job]] using the
        #    elements of nextPending.[[Arguments]] as its arguments.
        try:
            result = (next_pending.job)(*next_pending.arguments)
        except ESError as err:
            # l. If result is an abrupt completion, perform HostReportErrors(« result.[[Value]] »).
            HostReportErrors([err.ecma_object])
            result = err.ecma_object
    return result

# 8.7 Agents

# An agent comprises a set of ECMAScript execution contexts, an execution context stack, a running execution context, a
# set of named job queues, an Agent Record, and an executing thread. Except for the executing thread, the constituents
# of an agent belong exclusively to that agent.

# An agent's executing thread executes the jobs in the agent's job queues on the agent's execution contexts
# independently of other agents, except that an executing thread may be used as the executing thread by multiple
# agents, provided none of the agents sharing the thread have an Agent Record whose [[CanBlock]] property is true.

# While an agent's executing thread executes the jobs in the agent's job queues, the agent is the surrounding agent
# for the code in those jobs. The code uses the surrounding agent to access the specification level execution objects
# held within the agent: the running execution context, the execution context stack, the named job queues, and the
# Agent Record's fields.
class AgentRecord(object):
    def __init__(self):
        self.little_endian = True
        self.can_block = True # as a first guess
        self.signifier = uuid.uuid4()
        self.is_lock_free_1 = True
        self.is_lock_free_2 = True

class Agent(object):
    def __init__(self):
        self.ec_stack = [] # For this stack, add with "append", remove with "pop". (LIFO)
        self.running_ec = None
        self.job_queues = {  # For these queues, add with "append", remove with "popleft". (FIFO)
            'ScriptJobs': deque([]),
            'PromiseJobs': deque([])
        }
        self.agent_record = AgentRecord()

# Global: the "surrounding agent". (We only have one agent, so it's always the surrounding agent.)
surrounding_agent = Agent()

# 8.7.1 AgentSignifier()
def AgentSignifier():
    # The abstract operation AgentSignifier takes no arguments. It performs the following steps:
    # 1. Let AR be the Agent Record of the surrounding agent.
    AR = surrounding_agent.agent_record
    # 2. Return AR.[[Signifier]].
    return AR.signifier

# 8.7.2 AgentCanSuspend()
def AgentCanSuspend():
    # The abstract operation AgentCanSuspend takes no arguments. It performs the following steps:
    # 1. Let AR be the Agent Record of the surrounding agent.
    AR = surrounding_agent.agent_record
    # 2. Return AR.[[CanBlock]].
    return AR.can_block
    # NOTE: In some environments it may not be reasonable for a given agent to suspend. For example, in a
    # web browser environment, it may be reasonable to disallow suspending a document's main event
    # handling thread, while still allowing workers' event handling threads to suspend.

# 9.1.5.1 OrdinaryGetOwnProperty ( O, P )
def OrdinaryGetOwnProperty(obj, propkey):
    # When the abstract operation OrdinaryGetOwnProperty is called with Object O and with property key P, the following steps
    # are taken:
    #
    # 1. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(propkey)
    # 2. If O does not have an own property with key P, return undefined.
    if propkey not in obj.properties:
        return None
    # 3. Let D be a newly created Property Descriptor with no fields.
    desc = PropertyDescriptor()
    # 4. Let X be O's own property whose key is P.
    own = obj.properties[propkey]
    # 5. If X is a data property, then
    if hasattr(own, 'value'):
        # a. Set D.[[Value]] to the value of X's [[Value]] attribute.
        desc.value = own.value
        # b. Set D.[[Writable]] to the value of X's [[Writable]] attribute.
        desc.writable = own.writable
    # 6. Else X is an accessor property,
    else:
        # a. Set D.[[Get]] to the value of X's [[Get]] attribute.
        desc.Get = own.Get
        # b. Set D.[[Set]] to the value of X's [[Set]] attribute.
        desc.Set = own.Set
    # 7. Set D.[[Enumerable]] to the value of X's [[Enumerable]] attribute.
    desc.enumerable = own.enumerable
    # 8. Set D.[[Configurable]] to the value of X's [[Configurable]] attribute.
    desc.configurable = own.configurable
    # 9. Return D.
    return desc

# 9.1.6.1 OrdinaryDefineOwnProperty ( O, P, Desc )
def OrdinaryDefineOwnProperty(obj, propkey, desc):
    # When the abstract operation OrdinaryDefineOwnProperty is called with Object O, property key P, and Property Descriptor
    # Desc, the following steps are taken:
    #
    # 1. Let current be ? O.[[GetOwnProperty]](P).
    current = obj.GetOwnProperty(propkey)
    # 2. Let extensible be O.[[Extensible]].
    extensible = obj.Extensible
    # 3. Return ValidateAndApplyPropertyDescriptor(O, P, extensible, Desc, current).
    return ValidateAndApplyPropertyDescriptor(obj, propkey, extensible, desc, current)

#######################################################################################################################################
#
#  .d8888b.       .d8888b.      8888888888  .d8888b.  888b     d888        d8888  .d8888b.                   d8b          888
# d88P  Y88b     d88P  Y88b     888        d88P  Y88b 8888b   d8888       d88888 d88P  Y88b                  Y8P          888
# 888    888            888     888        888    888 88888b.d88888      d88P888 Y88b.                                    888
# Y88b. d888          .d88P     8888888    888        888Y88888P888     d88P 888  "Y888b.    .d8888b 888d888 888 88888b.  888888
#  "Y888P888      .od888P"      888        888        888 Y888P 888    d88P  888     "Y88b. d88P"    888P"   888 888 "88b 888
#        888     d88P"          888        888    888 888  Y8P  888   d88P   888       "888 888      888     888 888  888 888
# Y88b  d88P d8b 888"           888        Y88b  d88P 888   "   888  d8888888888 Y88b  d88P Y88b.    888     888 888 d88P Y88b.
#  "Y8888P"  Y8P 888888888      8888888888  "Y8888P"  888       888 d88P     888  "Y8888P"   "Y8888P 888     888 88888P"   "Y888
#                                                                                                                888
# 8888888888                            888    d8b                        .d88888b.  888         d8b             888   888
# 888                                   888    Y8P                       d88P" "Y88b 888         Y8P             888   888
# 888                                   888                              888     888 888                               888
# 8888888    888  888 88888b.   .d8888b 888888 888  .d88b.  88888b.      888     888 88888b.    8888  .d88b.   .d8888b 888888 .d8888b
# 888        888  888 888 "88b d88P"    888    888 d88""88b 888 "88b     888     888 888 "88b   "888 d8P  Y8b d88P"    888    88K
# 888        888  888 888  888 888      888    888 888  888 888  888     888     888 888  888    888 88888888 888      888    "Y8888b.
# 888        Y88b 888 888  888 Y88b.    Y88b.  888 Y88..88P 888  888     Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.       X88
# 888         "Y88888 888  888  "Y8888P  "Y888 888  "Y88P"  888  888      "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888  88888P'
#                                                                                                888
#                                                                                               d88P
#                                                                                             888P"
#
#######################################################################################################################################
# 9.2 ECMAScript Function Objects
#
# ECMAScript function objects encapsulate parameterized ECMAScript code closed over a lexical environment and support the
# dynamic evaluation of that code. An ECMAScript function object is an ordinary object and has the same internal slots and the
# same internal methods as other ordinary objects. The code of an ECMAScript function object may be either strict mode code
# (10.2.1) or non-strict code. An ECMAScript function object whose code is strict mode code is called a strict function. One
# whose code is not strict mode code is called a non-strict function.
#
# ECMAScript function objects have the additional internal slots listed in Table 27.
#
# Table 27: Internal Slots of ECMAScript Function Objects
# +----------------------+---------------------+-------------------------------------------------------------------------------
# | Internal Slot        | Type                | Description
# +----------------------+---------------------+-------------------------------------------------------------------------------
# | [[Environment]]      | Lexical Environment | The Lexical Environment that the function was closed over. Used as the outer
# |                      |                     | environment when evaluating the code of the function.
# +----------------------+---------------------+-------------------------------------------------------------------------------
# | [[FormalParameters]] | Parse Node          | The root parse node of the source text that defines the function's formal
# |                      |                     | parameter list.
# +----------------------+---------------------+-------------------------------------------------------------------------------
# | [[FunctionKind]]     | String              | Either "normal", "classConstructor", "generator", or "async".
# +----------------------+---------------------+-------------------------------------------------------------------------------
# | [[ECMAScriptCode]]   | Parse Node          | The root parse node of the source text that defines the function's body.
# +----------------------+---------------------+-------------------------------------------------------------------------------
# | [[ConstructorKind]]  | String              | Either "base" or "derived".
# +----------------------+---------------------+-------------------------------------------------------------------------------
# | [[Realm]]            | Realm Record        | The realm in which the function was created and which provides any intrinsic
# |                      |                     | objects that are accessed when evaluating the function.
# +----------------------+---------------------+-------------------------------------------------------------------------------
# | [[ScriptOrModule]]   | Script Record or    | The script or module in which the function was created.
# |                      | Module Record       |
# +----------------------+---------------------+-------------------------------------------------------------------------------
# | [[ThisMode]]         | (lexical, strict,   | Defines how this references are interpreted within the formal parameters and
# |                      | global)             | code body of the function. lexical means that this refers to the this value of
# |                      |                     | a lexically enclosing function. strict means that the this value is used
# |                      |                     | exactly as provided by an invocation of the function. global means that a this
# |                      |                     | value of undefined is interpreted as a reference to the global object.
# +----------------------+---------------------+-------------------------------------------------------------------------------
# | [[Strict]]           | Boolean             | true if this is a strict function, false if this is a non-strict function.
# +----------------------+---------------------+-------------------------------------------------------------------------------
# | [[HomeObject]]       | Object              | If the function uses super, this is the object whose [[GetPrototypeOf]]
# |                      |                     | provides the object where  super property lookups begin.
# +----------------------+---------------------+-------------------------------------------------------------------------------
#
# All ECMAScript function objects have the [[Call]] internal method defined here. ECMAScript functions that are also
# constructors in addition have the [[Construct]] internal method.
@unique
class TM(Enum):
    LEXICAL = auto()
    STRICT = auto()
    GLOBAL = auto()
class JSFunction(JSObject):
    def __init__(self):
        super().__init__()
        self.Environment = None
        self.FormalParameters = None
        self.FunctionKind = None
        self.ECMAScriptCode = None
        self.ConstructorKind = None
        self.Realm = None
        self.ScriptOrModule = None
        self.ThisMode = None
        self.Strict = None
        self.HomeObject = None

    # 9.2.1 [[Call]] ( thisArgument, argumentsList )
    def Call(self, thisArgument, argumentsList):
        # The [[Call]] internal method for an ECMAScript function object F is called with parameters thisArgument and
        # argumentsList, a List of ECMAScript language values. The following steps are taken:
        pass                                                      # 1. Assert: F is an ECMAScript function object.
        if self.FunctionKind == 'classConstructor':               # 2. If F.[[FunctionKind]] is "classConstructor",
            raise ESTypeError()             #    throw a TypeError exception
        callerContext = surrounding_agent.running_ec              # 3. Let callerContext be the running execution context.
        calleeContext = PrepareForOrdinaryCall(self, None)        # 4. Let calleeContext be PrepareForOrdinaryCall(F, undefined).
        assert calleeContext == surrounding_agent.running_ec      # 5. Assert: calleeContext is now the running execution context.
        OrdinaryCallBindThis(self, calleeContext, thisArgument)   # 6. Perform OrdinaryCallBindThis(F, calleeContext, thisArgument).
        try:
            OrdinaryCallEvaluateBody(self, argumentsList)    # 7. Let result be OrdinaryCallEvaluateBody(F, argumentsList).
            result = None
        except ESReturn as abrupt:
            result = abrupt.completion.value
        surrounding_agent.ec_stack.pop()                          # 8. Remove calleeContext from the execution context stack and
        surrounding_agent.running_ec = surrounding_agent.ec_stack[-1]   # restore callerContext as the running execution context.
        assert surrounding_agent.running_ec == callerContext
        return result
        # NOTE
        # When calleeContext is removed from the execution context stack in step 8 it must not be destroyed if it is suspended
        # and retained for later resumption by an accessible generator object.

# 9.2.1.1 PrepareForOrdinaryCall ( F, newTarget )
def PrepareForOrdinaryCall(F, newTarget):
    # When the abstract operation PrepareForOrdinaryCall is called with function object F and ECMAScript language value
    # newTarget, the following steps are taken:
    #
    assert newTarget is None or isObject(newTarget)               # 1. Assert: Type(newTarget) is Undefined or Object.
    callerContext = surrounding_agent.running_ec                  # 2. Let callerContext be the running execution context.
    calleeContext = ExecutionContext()                            # 3. Let calleeContext be a new ECMAScript code execution context.
    calleeContext.function = F                                    # 4. Set the Function of calleeContext to F.
    calleeRealm = F.Realm                                         # 5. Let calleeRealm be F.[[Realm]].
    calleeContext.realm = calleeRealm                             # 6. Set the Realm of calleeContext to calleeRealm.
    calleeContext.script_or_module = F.ScriptOrModule             # 7. Set the ScriptOrModule of calleeContext to F.[[ScriptOrModule]].
    localEnv = NewFunctionEnvironment(F, newTarget)               # 8. Let localEnv be NewFunctionEnvironment(F, newTarget).
    calleeContext.lexical_environment = localEnv                  # 9. Set the LexicalEnvironment of calleeContext to localEnv.
    calleeContext.variable_environment = localEnv                 # 10. Set the VariableEnvironment of calleeContext to localEnv.
    callerContext.suspend()                                       # 11. If callerContext is not already suspended, suspend callerContext.
    surrounding_agent.ec_stack.append(calleeContext)              # 12. Push calleeContext onto the execution context stack; calleeContext
    surrounding_agent.running_ec = calleeContext                  #     is now the running execution context.
    # 13. NOTE: Any exception objects produced after this point are associated with calleeRealm.
    return calleeContext                                          # 14. Return calleeContext.

# 9.2.1.2 OrdinaryCallBindThis ( F, calleeContext, thisArgument )
def OrdinaryCallBindThis(F, calleeContext, thisArgument):
    # When the abstract operation OrdinaryCallBindThis is called with function object F, execution context calleeContext, and
    # ECMAScript value thisArgument, the following steps are taken:
    #
    thisMode = F.ThisMode                                         # 1. Let thisMode be F.[[ThisMode]].
    if thisMode == TM.LEXICAL:                                    # 2. If thisMode is lexical, return undefined
        return None
    calleeRealm = F.Realm                                         # 3. Let calleeRealm be F.[[Realm]].
    localEnv = calleeContext.lexical_environment                  # 4. Let localEnv be the LexicalEnvironment of calleeContext.
    if thisMode == TM.STRICT:                                     # 5. If thisMode is strict, let thisValue be thisArgument.
        thisValue = thisArgument
    else:                                                         # 6. Else,
        if thisArgument is None or isNull(thisArgument):          #    a. If thisArgument is undefined or null, then
            globalEnv = calleeRealm.global_env                    #       i. Let globalEnv be calleeRealm.[[GlobalEnv]].
            globalEnvRec = globalEnv.environment_record           #      ii. Let globalEnvRec be globalEnv's EnvironmentRecord.
            assert isinstance(globalEnvRec, GlobalEnvironmentRecord) #  iii. Assert: globalEnvRec is a global Environment Record.
            thisValue = globalEnvRec.global_this_value            #      iv. Let thisValue be globalEnvRec.[[GlobalThisValue]].
        else:                                                     #    b. Else,
            thisValue = ToObject(thisArgument)                    #       i. Let thisValue be ! ToObject(thisArgument).
            # ii. NOTE: ToObject produces wrapper objects using calleeRealm.
    envRec = localEnv.environment_record                          # 7. Let envRec be localEnv's EnvironmentRecord.
    assert isinstance(envRec, FunctionEnvironmentRecord)          # 8. Assert: envRec is a function Environment Record.
    # 9. Assert: The next step never returns an abrupt completion because envRec.[[ThisBindingStatus]] is not "initialized".
    assert envRec.this_binding_status != 'initialized'
    return envRec.BindThisValue(thisValue)                        # 10. Return envRec.BindThisValue(thisValue).

# 9.2.1.3 OrdinaryCallEvaluateBody ( F, argumentsList )
def OrdinaryCallEvaluateBody(F, argumentsList):
    # When the abstract operation OrdinaryCallEvaluateBody is called with function object F and List argumentsList, the
    # following steps are taken:
    #
    # 1. Return the result of EvaluateBody of the parsed code that is F.[[ECMAScriptCode]] passing F and argumentsList as the
    #    arguments.
    return F.ECMAScriptCode.EvaluateBody(F, argumentsList)

# 9.2.2 [[Construct]] ( argumentsList, newTarget )
def JSFunction_Construct(F, argumentsList, newTarget):
    # The [[Construct]] internal method for an ECMAScript function object F is called with parameters argumentsList and
    # newTarget. argumentsList is a possibly empty List of ECMAScript language values. The following steps are taken:
    #
    # 1. Assert: F is an ECMAScript function object.
    assert isinstance(F, JSFunction)
    # 2. Assert: Type(newTarget) is Object.
    assert isObject(newTarget)
    # 3. Let callerContext be the running execution context.
    #callerContext = surrounding_agent.running_ec
    # 4. Let kind be F.[[ConstructorKind]].
    kind = F.ConstructorKind
    # 5. If kind is "base", then
    if kind == 'base':
        # a. Let thisArgument be ? OrdinaryCreateFromConstructor(newTarget, "%ObjectPrototype%").
        thisArgument = OrdinaryCreateFromConstructor(newTarget, '%ObjectPrototype%')
    # 6. Let calleeContext be PrepareForOrdinaryCall(F, newTarget).
    calleeContext = PrepareForOrdinaryCall(F, newTarget)
    # 7. Assert: calleeContext is now the running execution context.
    assert surrounding_agent.running_ec == calleeContext
    # 8. If kind is "base", perform OrdinaryCallBindThis(F, calleeContext, thisArgument).
    if kind == 'base':
        OrdinaryCallBindThis(F, calleeContext, thisArgument)
    # 9. Let constructorEnv be the LexicalEnvironment of calleeContext.
    constructorEnv = calleeContext.lexical_environment
    # 10. Let envRec be constructorEnv's EnvironmentRecord.
    envRec = constructorEnv.environment_record
    # 11. Let result be OrdinaryCallEvaluateBody(F, argumentsList).
    try:
        OrdinaryCallEvaluateBody(F, argumentsList)
        saw_return = False
    except ESReturn as abrupt:
        result = abrupt.completion.value
        saw_return = True
    # 12. Remove calleeContext from the execution context stack and restore callerContext as the running execution context.
    surrounding_agent.ec_stack.pop()
    surrounding_agent.running_ec = surrounding_agent.ec_stack[-1]
    # 13. If result.[[Type]] is return, then
    if saw_return:
        # a. If Type(result.[[Value]]) is Object, return result.[[Value]])
        if isObject(result):
            return result
        # b. If kind is "base", return thisArgument
        if kind == 'base':
            return thisArgument
        # c. If result.[[Value]] is not undefined, throw a TypeError exception.
        if result is not None:
            raise ESTypeError('Constructor failed to create an object')
    # 14. Else, ReturnIfAbrupt(result).
    # 15. Return ? envRec.GetThisBinding().
    return envRec.GetThisBinding()

# 9.2.3 FunctionAllocate ( functionPrototype, strict, functionKind )
def FunctionAllocate(functionPrototype, strict, functionKind):
    # The abstract operation FunctionAllocate requires the three arguments functionPrototype, strict and functionKind.
    # FunctionAllocate performs the following steps:
    #
    # 1. Assert: Type(functionPrototype) is Object.
    assert isObject(functionPrototype)
    # 2. Assert: functionKind is either "normal", "non-constructor", "generator", "async", or "async generator".
    assert functionKind in ['normal', 'non-constructor', 'generator', 'async', 'async generator']
    # 3. If functionKind is "normal", let needsConstruct be true.
    # 4. Else, let needsConstruct be false.
    needsConstruct = functionKind == 'normal'
    # 5. If functionKind is "non-constructor", set functionKind to "normal".
    if functionKind == 'non-constructor':
        functionKind = 'normal'
    # 6. Let F be a newly created ECMAScript function object with the internal slots listed in Table 27. All of those internal
    #    slots are initialized to undefined.
    # 7. Set F's essential internal methods to the default ordinary object definitions specified in 9.1.
    # 8. Set F.[[Call]] to the definition specified in 9.2.1.
    F = JSFunction()
    # 9. If needsConstruct is true, then
    if needsConstruct:
        # a. Set F.[[Construct]] to the definition specified in 9.2.2.
        F.Construct = types.MethodType(JSFunction_Construct, F)
        # b. Set F.[[ConstructorKind]] to "base".
        F.ConstructorKind = 'base'
    # 10. Set F.[[Strict]] to strict.
    F.Strict = strict
    # 11. Set F.[[FunctionKind]] to functionKind.
    F.FunctionKind = functionKind
    # 12. Set F.[[Prototype]] to functionPrototype.
    F.Prototype = functionPrototype
    # 13. Set F.[[Extensible]] to true.
    F.Extensible = True
    # 14. Set F.[[Realm]] to the current Realm Record.
    F.Realm = surrounding_agent.running_ec.realm
    # 15. Return F.
    return F

# 9.2.4 FunctionInitialize ( F, kind, ParameterList, Body, Scope )
@unique
class FNKind(Enum):
    NORMAL = auto()
    METHOD = auto()
    ARROW = auto()
def FunctionInitialize(F, kind, ParameterList, Body, Scope):
    # The abstract operation FunctionInitialize requires the arguments: a function object F, kind which is one of (Normal,
    # Method, Arrow), a parameter list Parse Node specified by ParameterList, a body Parse Node specified by Body, a Lexical
    # Environment specified by Scope. FunctionInitialize performs the following steps:
    #
    # 1. Let len be the ExpectedArgumentCount of ParameterList.
    len_ = ParameterList.ExpectedArgumentCount()
    # 2. Perform ! SetFunctionLength(F, len).
    SetFunctionLength(F, len_)
    # 3. Let Strict be F.[[Strict]].
    Strict = F.Strict
    # 4. Set F.[[Environment]] to Scope.
    F.Environment = Scope
    # 5. Set F.[[FormalParameters]] to ParameterList.
    F.FormalParameters = ParameterList
    # 6. Set F.[[ECMAScriptCode]] to Body.
    F.ECMAScriptCode = Body
    # 7. Set F.[[ScriptOrModule]] to GetActiveScriptOrModule().
    F.ScriptOrModule = GetActiveScriptOrModule()
    # 8. If kind is Arrow, set F.[[ThisMode]] to lexical.
    if kind == FNKind.ARROW:
        F.ThisMode = TM.LEXICAL
    # 9. Else if Strict is true, set F.[[ThisMode]] to strict.
    elif Strict:
        F.ThisMode = TM.STRICT
    # 10. Else, set F.[[ThisMode]] to global.
    else:
        F.ThisMode = TM.GLOBAL
    # 11. Return F.
    return F

# 9.2.5 FunctionCreate ( kind, ParameterList, Body, Scope, Strict [ , prototype ] )
def FunctionCreate(kind, ParameterList, Body, Scope, Strict, prototype=missing.MISSING):
    # The abstract operation FunctionCreate requires the arguments: kind which is one of (Normal, Method, Arrow), a parameter
    # list Parse Node specified by ParameterList, a body Parse Node specified by Body, a Lexical Environment specified by
    # Scope, a Boolean flag Strict, and optionally, an object prototype. FunctionCreate performs the following steps:
    #
    # 1. If prototype is not present, then
    if prototype == missing.MISSING:
        # a. Set prototype to the intrinsic object %FunctionPrototype%.
        prototype = surrounding_agent.running_ec.realm.intrinsics['%FunctionPrototype%']
    # 2. If kind is not Normal, let allocKind be "non-constructor".
    if kind != FNKind.NORMAL:
        allocKind = 'non-constructor'
    # 3. Else, let allocKind be "normal".
    else:
        allocKind = 'normal'
    # 4. Let F be FunctionAllocate(prototype, Strict, allocKind).
    F = FunctionAllocate(prototype, Strict, allocKind)
    # 5. Return FunctionInitialize(F, kind, ParameterList, Body, Scope).
    return FunctionInitialize(F, kind, ParameterList, Body, Scope)

# 9.2.6 GeneratorFunctionCreate ( kind, ParameterList, Body, Scope, Strict )
def GeneratorFunctionCreate(kind, ParameterList, Body, Scope, Strict):
    # The abstract operation GeneratorFunctionCreate requires the arguments: kind which is one of (Normal, Method), a parameter
    # list Parse Node specified by ParameterList, a body Parse Node specified by Body, a Lexical Environment specified by
    # Scope, and a Boolean flag Strict. GeneratorFunctionCreate performs the following steps:
    #
    # 1. Let functionPrototype be the intrinsic object %Generator%.
    functionPrototype = surrounding_agent.running_ec.realm.intrinsics['%Generator%']
    # 2. Let F be FunctionAllocate(functionPrototype, Strict, "generator").
    F = FunctionAllocate(functionPrototype, Strict, 'generator')
    # 3. Return FunctionInitialize(F, kind, ParameterList, Body, Scope).
    return FunctionInitialize(F, kind, ParameterList, Body, Scope)

# 9.2.7 AsyncGeneratorFunctionCreate ( kind, ParameterList, Body, Scope, Strict )
def AsyncGeneratorFunctionCreate(kind, ParameterList, Body, Scope, Strict):
    # The abstract operation AsyncGeneratorFunctionCreate requires the arguments: kind which is one of (Normal, Method), a
    # parameter list Parse Node specified by ParameterList, a body Parse Node specified by Body, a Lexical Environment
    # specified by Scope, and a Boolean flag Strict. AsyncGeneratorFunctionCreate performs the following steps:
    #
    # 1. Let functionPrototype be the intrinsic object %AsyncGenerator%.
    functionPrototype = surrounding_agent.running_ec.realm.intrinsics['%AsyncGenerator%']
    # 2. Let F be ! FunctionAllocate(functionPrototype, Strict, "generator").
    F = FunctionAllocate(functionPrototype, Strict, 'generator')
    # 3. Return ! FunctionInitialize(F, kind, ParameterList, Body, Scope).
    return FunctionInitialize(F, kind, ParameterList, Body, Scope)

# 9.2.8 AsyncFunctionCreate ( kind, parameters, body, Scope, Strict )
def AsyncFunctionCreate(kind, parameters, body, Scope, Strict):
    # The abstract operation AsyncFunctionCreate requires the arguments: kind which is one of (Normal, Method, Arrow), a
    # parameter list Parse Node specified by parameters, a body Parse Node specified by body, a Lexical Environment specified
    # by Scope, and a Boolean flag Strict. AsyncFunctionCreate performs the following steps:
    #
    # 1. Let functionPrototype be the intrinsic object %AsyncFunctionPrototype%.
    functionPrototype = surrounding_agent.running_ec.realm.intrinsics['%AsyncFunctionPrototype%']
    # 2. Let F be ! FunctionAllocate(functionPrototype, Strict, "async").
    F = FunctionAllocate(functionPrototype, Strict, 'async')
    # 3. Return ! FunctionInitialize(F, kind, parameters, body, Scope).
    return FunctionInitialize(F, kind, parameters, body, Scope)

# 9.2.9 AddRestrictedFunctionProperties ( F, realm )
def AddRestrictedFunctionProperties(func, realm):
    # The abstract operation AddRestrictedFunctionProperties is called with a function object F and Realm Record realm
    # as its argument. It performs the following steps:
    #
    # 1. Assert: realm.[[Intrinsics]].[[%ThrowTypeError%]] exists and has been initialized.
    assert isinstance(realm.intrinsics['%ThrowTypeError%'], BuiltinFunction)
    # 2. Let thrower be realm.[[Intrinsics]].[[%ThrowTypeError%]].
    thrower = realm.intrinsics['%ThrowTypeError%']
    # 3. Perform ! DefinePropertyOrThrow(F, "caller", PropertyDescriptor { [[Get]]: thrower, [[Set]]: thrower,
    #    [[Enumerable]]: false, [[Configurable]]: true }).
    DefinePropertyOrThrow(func, 'caller', PropertyDescriptor(Get=thrower, Set=thrower, enumerable=False, configurable=True))
    # 4. Return ! DefinePropertyOrThrow(F, "arguments", PropertyDescriptor { [[Get]]: thrower, [[Set]]: thrower,
    #    [[Enumerable]]: false, [[Configurable]]: true }).
    return DefinePropertyOrThrow(func, 'arguments', PropertyDescriptor(Get=thrower, Set=thrower, enumerable=False, configurable=True))

# 9.2.10 MakeConstructor ( F [ , writablePrototype [ , prototype ] ] )
def MakeConstructor(F, writeablePrototype=True, prototype=missing.MISSING):
    # The abstract operation MakeConstructor requires a Function argument F and optionally, a Boolean writablePrototype and an
    # object prototype. If prototype is provided it is assumed to already contain, if needed, a "constructor" property whose
    # value is F. This operation converts F into a constructor by performing the following steps:
    #
    # 1. Assert: F is an ECMAScript function object.
    assert isinstance(F, JSFunction)
    # 2. Assert: IsConstructor(F) is true.
    assert IsConstructor(F)
    # 3. Assert: F is an extensible object that does not have a prototype own property.
    assert IsExtensible(F) and not HasOwnProperty(F, 'prototype')
    # 4. If writablePrototype is not present, set writablePrototype to true.
    # 5. If prototype is not present, then
    if prototype == missing.MISSING:
        # a. Set prototype to ObjectCreate(%ObjectPrototype%).
        prototype = ObjectCreate(surrounding_agent.running_ec.realm.intrinsics['%ObjectPrototype%'])
        # b. Perform ! DefinePropertyOrThrow(prototype, "constructor", PropertyDescriptor { [[Value]]: F, [[Writable]]: writablePrototype, [[Enumerable]]: false, [[Configurable]]: true }).
        DefinePropertyOrThrow(prototype, 'constructor', PropertyDescriptor(value=F, writable=writeablePrototype, enumerable=False, configurable=True))
    # 6. Perform ! DefinePropertyOrThrow(F, "prototype", PropertyDescriptor { [[Value]]: prototype, [[Writable]]: writablePrototype, [[Enumerable]]: false, [[Configurable]]: false }).
    DefinePropertyOrThrow(F, 'prototype', PropertyDescriptor(value=prototype, writable=writeablePrototype, enumerable=False, configurable=False))
    # 7. Return undefined)
    return None

# 9.2.11 MakeClassConstructor ( F )
def MakeClassConstructor(F):
    # The abstract operation MakeClassConstructor with argument F performs the following steps:
    #
    # 1. Assert: F is an ECMAScript function object.
    assert isinstance(F, JSFunction)
    # 2. Assert: F.[[FunctionKind]] is "normal".
    assert F.FunctionKind == 'normal'
    # 3. Set F.[[FunctionKind]] to "classConstructor".
    F.FunctionKind = 'classConstructor'
    # 4. Return undefined)
    return None

# 9.2.12 MakeMethod ( F, homeObject )
def MakeMethod(F, homeObject):
    # The abstract operation MakeMethod with arguments F and homeObject configures F as a method by performing the following
    # steps:
    #
    # 1. Assert: F is an ECMAScript function object.
    assert isinstance(F, JSFunction)
    # 2. Assert: Type(homeObject) is Object.
    assert isObject(homeObject)
    # 3. Set F.[[HomeObject]] to homeObject.
    F.HomeObject = homeObject
    # 4. Return undefined)
    return None

# 9.2.13 SetFunctionName ( F, name [ , prefix ] )
def SetFunctionName(F, name, prefix=missing.MISSING):
    # The abstract operation SetFunctionName requires a Function argument F, a String or Symbol argument name and optionally a
    # String argument prefix. This operation adds a name property to F by performing the following steps:
    #
    # 1. Assert: F is an extensible object that does not have a name own property.
    assert IsExtensible(F) and not HasOwnProperty(F, 'name')
    # 2. Assert: Type(name) is either Symbol or String.
    assert isString(name) or isSymbol(name)
    # 3. Assert: If prefix is present, then Type(prefix) is String.
    assert  prefix == missing.MISSING or isString(prefix)
    # 4. If Type(name) is Symbol, then
    if isSymbol(name):
        # a. Let description be name's [[Description]] value.
        description = name.description
        # b. If description is undefined, set name to the empty String.
        if description is None:
            name = ''
        # c. Else, set name to the string-concatenation of "[", description, and "]".
        else:
            name = f'[{description}]'
    # 5. If prefix is present, then
    if prefix != missing.MISSING:
        # a. Set name to the string-concatenation of prefix, the code unit 0x0020 (SPACE), and name.
        name = f'{prefix} {name}'
    # 6. Return ! DefinePropertyOrThrow(F, "name", PropertyDescriptor { [[Value]]: name, [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true }).
    return DefinePropertyOrThrow(F, 'name', PropertyDescriptor(value=name, writable=False, enumerable=False, configurable=True))

# 9.2.14 SetFunctionLength ( F, length )
def SetFunctionLength(F, length):
    # The abstract operation SetFunctionLength requires a Function argument F and a Number argument length. This operation adds
    # a length property to F by performing the following steps:
    #
    # 1. Assert: F is an extensible object that does not have a length own property.
    assert IsExtensible(F) and not HasOwnProperty(F, 'length')
    # 2. Assert: Type(length) is Number.
    assert isNumber(length)
    # 3. Assert: length ≥ 0 and ! ToInteger(length) is equal to length.
    assert length >= 0 and ToInteger(length) == length
    # 4. Return ! DefinePropertyOrThrow(F, "length",
    #        PropertyDescriptor { [[Value]]: length, [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true }).
    desc = PropertyDescriptor(value=length, writable=False, enumerable=False, configurable=True)
    return DefinePropertyOrThrow(F, 'length', desc)

# 9.2.15 FunctionDeclarationInstantiation ( func, argumentsList )
# NOTE 1
# When an execution context is established for evaluating an ECMAScript function a new function Environment Record is created
# and bindings for each formal parameter are instantiated in that Environment Record. Each declaration in the function body is
# also instantiated. If the function's formal parameters do not include any default value initializers then the body
# declarations are instantiated in the same Environment Record as the parameters. If default value parameter initializers
# exist, a second Environment Record is created for the body declarations. Formal parameters and functions are initialized as
# part of FunctionDeclarationInstantiation. All other bindings are initialized during evaluation of the function body.
def FunctionDeclarationInstantiation(func, argumentsList):
    # FunctionDeclarationInstantiation is performed as follows using arguments func and argumentsList. func is the function
    # object for which the execution context is being established.
    #
    # 1. Let calleeContext be the running execution context.
    calleeContext = surrounding_agent.running_ec
    # 2. Let env be the LexicalEnvironment of calleeContext.
    env = calleeContext.lexical_environment
    # 3. Let envRec be env's EnvironmentRecord.
    envRec = env.environment_record
    # 4. Let code be func.[[ECMAScriptCode]].
    code = func.ECMAScriptCode
    # 5. Let strict be func.[[Strict]].
    strict = func.Strict
    # 6. Let formals be func.[[FormalParameters]].
    formals = func.FormalParameters
    # 7. Let parameterNames be the BoundNames of formals.
    parameterNames = formals.BoundNames()
    # 8. If parameterNames has any duplicate entries, let hasDuplicates be true. Otherwise, let hasDuplicates be false.
    hasDuplicates = len(parameterNames) != len(set(parameterNames))
    # 9. Let simpleParameterList be IsSimpleParameterList of formals.
    simpleParameterList = formals.IsSimpleParameterList()
    # 10. Let hasParameterExpressions be ContainsExpression of formals.
    hasParameterExpressions = formals.ContainsExpression()
    # 11. Let varNames be the VarDeclaredNames of code.
    varNames = code.VarDeclaredNames()
    # 12. Let varDeclarations be the VarScopedDeclarations of code.
    varDeclarations = code.VarScopedDeclarations()
    # 13. Let lexicalNames be the LexicallyDeclaredNames of code.
    lexicalNames = code.LexicallyDeclaredNames()
    # 14. Let functionNames be a new empty List.
    functionNames = deque([])
    # 15. Let functionsToInitialize be a new empty List.
    functionsToInitialize = deque([])
    # 16. For each d in varDeclarations, in reverse list order, do
    for d in (varDeclarations[x] for x in range(len(varDeclarations)-1, -1, -1)):
        # a. If d is neither a VariableDeclaration nor a ForBinding nor a BindingIdentifier, then
        if d.name not in ['VariableDeclaration', 'ForBinding', 'BindingIdentifier']:
            # i. Assert: d is either a FunctionDeclaration, a GeneratorDeclaration, an AsyncFunctionDeclaration, or an
            #    AsyncGeneratorDeclaration.
            assert d.name in ['FunctionDeclaration', 'GeneratorDeclaration', 'AsyncFunctionDeclaration', 'AsyncGeneratorDeclaration']
            # ii. Let fn be the sole element of the BoundNames of d.
            fn = d.BoundNames()[0]
            # iii. If fn is not an element of functionNames, then
            if fn not in functionNames:
                # 1. Insert fn as the first element of functionNames.
                functionNames.appendleft(fn)
                # 2. NOTE: If there are multiple function declarations for the same name, the last declaration is used.
                # 3. Insert d as the first element of functionsToInitialize.
                functionsToInitialize.appendleft(d)
    # 17. Let argumentsObjectNeeded be true.
    argumentsObjectNeeded = True
    # 18. If func.[[ThisMode]] is lexical, then
    if func.ThisMode == TM.LEXICAL:
        # a. NOTE: Arrow functions never have an arguments objects.
        # b. Set argumentsObjectNeeded to false.
        argumentsObjectNeeded = False
    # 19. Else if "arguments" is an element of parameterNames, then
    elif 'arguments' in parameterNames:
        # a. Set argumentsObjectNeeded to false.
        argumentsObjectNeeded = False
    # 20. Else if hasParameterExpressions is false, then
    elif not hasParameterExpressions:
        # a. If "arguments" is an element of functionNames or if "arguments" is an element of lexicalNames, then
        if 'arguments' in functionNames or 'arguments' in lexicalNames:
            # i. Set argumentsObjectNeeded to false.
            argumentsObjectNeeded = False
    # 21. For each String paramName in parameterNames, do
    for paramName in parameterNames:
        # a. Let alreadyDeclared be envRec.HasBinding(paramName).
        alreadyDeclared = envRec.HasBinding(paramName)
        # b. NOTE: Early errors ensure that duplicate parameter names can only occur in non-strict functions that do not have
        #          parameter default values or rest parameters.
        # c. If alreadyDeclared is false, then
        if not alreadyDeclared:
            # i. Perform ! envRec.CreateMutableBinding(paramName, false).
            envRec.CreateMutableBinding(paramName, False)
            # ii. If hasDuplicates is true, then
            if hasDuplicates:
                # 1. Perform ! envRec.InitializeBinding(paramName, undefined).
                envRec.InitializeBinding(paramName, None)
    # 22. If argumentsObjectNeeded is true, then
    if argumentsObjectNeeded:
        # a. If strict is true or if simpleParameterList is false, then
        if strict or not simpleParameterList:
            # i. Let ao be CreateUnmappedArgumentsObject(argumentsList).
            ao = CreateUnmappedArgumentsObject(argumentsList)
        # b. Else,
            # i. NOTE: mapped argument object is only provided for non-strict functions that don't have a rest parameter, any
            #    parameter default value initializers, or any destructured parameters.
            # ii. Let ao be CreateMappedArgumentsObject(func, formals, argumentsList, envRec).
            ao = CreateMappedArgumentsObject(func, formals, argumentsList, envRec)
        # c. If strict is true, then
        if strict:
            # i. Perform ! envRec.CreateImmutableBinding("arguments", false).
            envRec.CreateImmutableBinding('arguments', False)
        # d. Else,
            # i. Perform ! envRec.CreateMutableBinding("arguments", false).
            envRec.CreateMutableBinding('arguments', False)
        # e. Call envRec.InitializeBinding("arguments", ao).
        envRec.InitializeBinding('arguments', ao)
        # f. Let parameterBindings be a new List of parameterNames with "arguments" appended.
        parameterBindings = parameterNames + ['arguments']
    # 23. Else,
        # a. Let parameterBindings be parameterNames.
        parameterBindings = parameterNames
    # 24. Let iteratorRecord be CreateListIteratorRecord(argumentsList).
    iteratorRecord = CreateListIteratorRecord(argumentsList)
    # 25. If hasDuplicates is true, then
    if hasDuplicates:
        # a. Perform ? IteratorBindingInitialization for formals with iteratorRecord and undefined as arguments.
        formals.IteratorBindingInitialization(iteratorRecord, None)
    # 26. Else,
        # a. Perform ? IteratorBindingInitialization for formals with iteratorRecord and env as arguments.
        formals.IteratorBindingInitialization(iteratorRecord, env)
    # 27. If hasParameterExpressions is false, then
    if not hasParameterExpressions:
        # a. NOTE: Only a single lexical environment is needed for the parameters and top-level vars.
        # b. Let instantiatedVarNames be a copy of the List parameterBindings.
        instantiatedVarNames = list(parameterBindings)
        # c. For each n in varNames, do
        for n in varNames:
            # i. If n is not an element of instantiatedVarNames, then
            if n not in instantiatedVarNames:
                # 1. Append n to instantiatedVarNames.
                instantiatedVarNames.append(n)
                # 2. Perform ! envRec.CreateMutableBinding(n, false).
                envRec.CreateMutableBinding(n, False)
                # 3. Call envRec.InitializeBinding(n, undefined).
                envRec.InitializeBinding(n, None)
        # d. Let varEnv be env.
        varEnv = env
        # e. Let varEnvRec be envRec.
        varEnvRec = envRec
    # 28. Else,
        # a. NOTE: A separate Environment Record is needed to ensure that closures created by expressions in the formal
        #    parameter list do not have visibility of declarations in the function body.
        # b. Let varEnv be NewDeclarativeEnvironment(env).
        varEnv = NewDeclarativeEnvironment(env)
        # c. Let varEnvRec be varEnv's EnvironmentRecord.
        varEnvRec = varEnv.environment_record
        # d. Set the VariableEnvironment of calleeContext to varEnv.
        calleeContext.variable_environment = varEnv
        # e. Let instantiatedVarNames be a new empty List.
        instantiatedVarNames = []
        # f. For each n in varNames, do
        for n in varNames:
            # i. If n is not an element of instantiatedVarNames, then
            if n not in instantiatedVarNames:
                # 1. Append n to instantiatedVarNames.
                instantiatedVarNames.append(n)
                # 2. Perform ! varEnvRec.CreateMutableBinding(n, false).
                varEnvRec.CreateMutableBinding(n, False)
                # 3. If n is not an element of parameterBindings or if n is an element of functionNames, let initialValue be undefined.
                if n not in parameterBindings or n in functionNames:
                    initialValue = None
                # 4. Else,
                    # a. Let initialValue be ! envRec.GetBindingValue(n, false).
                    initialValue = envRec.GetBindingValue(n, False)
                # 5. Call varEnvRec.InitializeBinding(n, initialValue).
                varEnvRec.InitializeBinding(n, initialValue)
                # 6. NOTE: vars whose names are the same as a formal parameter, initially have the same value as the
                #    corresponding initialized parameter.
    # 29. NOTE: Annex B.3.3.1 adds additional steps at this point.
    # 30. If strict is false, then
    if not strict:
        # a. Let lexEnv be NewDeclarativeEnvironment(varEnv).
        lexEnv = NewDeclarativeEnvironment(varEnv)
        # b. NOTE: Non-strict functions use a separate lexical Environment Record for top-level lexical declarations so that a
        #          direct eval can determine whether any var scoped declarations introduced by the eval code conflict with
        #          pre-existing top-level lexically scoped declarations. This is not needed for strict functions because a
        #          strict direct eval always places all declarations into a new Environment Record.
    # 31. Else, let lexEnv be varEnv.
    else:
        lexEnv = varEnv
    # 32. Let lexEnvRec be lexEnv's EnvironmentRecord.
    lexEnvRec = lexEnv.environment_record
    # 33. Set the LexicalEnvironment of calleeContext to lexEnv.
    calleeContext.lexical_environment = lexEnv
    # 34. Let lexDeclarations be the LexicallyScopedDeclarations of code.
    lexDeclarations = code.LexicallyScopedDeclarations()
    # 35. For each element d in lexDeclarations, do
    for d in lexDeclarations:
        # a. NOTE: A lexically declared name cannot be the same as a function/generator declaration, formal parameter, or a var
        #          name. Lexically declared names are only instantiated here but not initialized.
        # b. For each element dn of the BoundNames of d, do
        for dn in d.BoundNames():
            # i. If IsConstantDeclaration of d is true, then
            if d.IsConstantDeclaration():
                # 1. Perform ! lexEnvRec.CreateImmutableBinding(dn, true).
                lexEnvRec.CreateImmutableBinding(dn, True)
            # ii. Else,
            else:
                # 1. Perform ! lexEnvRec.CreateMutableBinding(dn, false).
                lexEnvRec.CreateMutableBinding(dn, False)
    # 36. For each Parse Node f in functionsToInitialize, do
    for f in functionsToInitialize:
        # a. Let fn be the sole element of the BoundNames of f.
        fn = f.BoundNames()[0]
        # b. Let fo be the result of performing InstantiateFunctionObject for f with argument lexEnv.
        fo = f.InstantiateFunctionObject(lexEnv)
        # c. Perform ! varEnvRec.SetMutableBinding(fn, fo, false).
        varEnvRec.SetMutableBinding(fn, fo, False)
    # 37. Return NormalCompletion(empty).
    return Empty.EMPTY
    # NOTE 2
    # B.3.3 provides an extension to the above algorithm that is necessary for backwards compatibility with web browser
    # implementations of ECMAScript that predate ECMAScript 2015.
    # NOTE 3
    # Parameter Initializers may contain direct eval expressions. Any top level declarations of such evals are only visible
    # to the eval code (10.2). The creation of the environment for such declarations is described in 14.1.19.
#region 9.3 Built-in Function Objects
################################################################################################################################################################################################################################
#
#  .d8888b.       .d8888b.      888888b.            d8b 888 888           d8b              8888888888                            888    d8b                        .d88888b.  888         d8b                   888
# d88P  Y88b     d88P  Y88b     888  "88b           Y8P 888 888           Y8P              888                                   888    Y8P                       d88P" "Y88b 888         Y8P                   888
# 888    888          .d88P     888  .88P               888 888                            888                                   888                              888     888 888                               888
# Y88b. d888         8888"      8888888K.  888  888 888 888 888888        888 88888b.      8888888    888  888 88888b.   .d8888b 888888 888  .d88b.  88888b.      888     888 88888b.    8888  .d88b.   .d8888b 888888 .d8888b
#  "Y888P888          "Y8b.     888  "Y88b 888  888 888 888 888           888 888 "88b     888        888  888 888 "88b d88P"    888    888 d88""88b 888 "88b     888     888 888 "88b   "888 d8P  Y8b d88P"    888    88K
#        888     888    888     888    888 888  888 888 888 888    888888 888 888  888     888        888  888 888  888 888      888    888 888  888 888  888     888     888 888  888    888 88888888 888      888    "Y8888b.
# Y88b  d88P d8b Y88b  d88P     888   d88P Y88b 888 888 888 Y88b.         888 888  888     888        Y88b 888 888  888 Y88b.    Y88b.  888 Y88..88P 888  888     Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.       X88
#  "Y8888P"  Y8P  "Y8888P"      8888888P"   "Y88888 888 888  "Y888        888 888  888     888         "Y88888 888  888  "Y8888P  "Y888 888  "Y88P"  888  888      "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888  88888P'
#                                                                                                                                                                                         888
#                                                                                                                                                                                        d88P
#                                                                                                                                                                                      888P"
#
################################################################################################################################################################################################################################

class BuiltinFunction(JSObject):
    def __init__(self, steps, realm, prototype, extensible, script_or_module, internal_slots_list):
        super().__init__()
        self.steps = steps
        self.realm = realm
        self.Prototype = prototype
        self.Extensible = extensible
        self.script_or_module = script_or_module
        for slotname in internal_slots_list:
            setattr(self, slotname, None)

    # 9.3.1 [[Call]] ( thisArgument, argumentsList )
    def Call(self, this_argument, arguments_list):
        # The [[Call]] internal method for a built-in function object F is called with parameters thisArgument and
        # argumentsList, a List of ECMAScript language values. The following steps are taken:
        #
        # 1. Let callerContext be the running execution context.
        caller_context = surrounding_agent.running_ec
        # 2. If callerContext is not already suspended, suspend callerContext.
        caller_context.suspend()
        # 3. Let calleeContext be a new ECMAScript code execution context.
        callee_context = ExecutionContext()
        # 4. Set the Function of calleeContext to F.
        callee_context.function = self
        # 5. Let calleeRealm be F.[[Realm]].
        callee_realm = self.realm
        # 6. Set the Realm of calleeContext to calleeRealm.
        callee_context.realm = callee_realm
        # 7. Set the ScriptOrModule of calleeContext to F.[[ScriptOrModule]].
        callee_context.script_or_module = self.script_or_module
        # 8. Perform any necessary implementation-defined initialization of calleeContext.
        # 9. Push calleeContext onto the execution context stack; calleeContext is now the running execution context.
        surrounding_agent.ec_stack.append(callee_context)
        surrounding_agent.running_ec = callee_context
        # 10. Let result be the Completion Record that is the result of evaluating F in an implementation-defined
        #     manner that conforms to the specification of F. thisArgument is the this value, argumentsList provides
        #     the named parameters, and the NewTarget value is undefined.
        result = self.steps(this_argument, None, *arguments_list)
        if not isinstance(result, Completion):
            result = result
        # 11. Remove calleeContext from the execution context stack and restore callerContext as the running execution
        #     context.
        surrounding_agent.ec_stack.pop()
        surrounding_agent.running_ec = caller_context
        # 12. Return result.
        return result
        # NOTE
        # When calleeContext is removed from the execution context stack it must not be destroyed if it has been
        # suspended and retained by an accessible generator object for later resumption.


# 9.3.2[[Construct]] ( argumentsList, newTarget )
def BuiltinFunction_Construct(self, arguments_list, new_target):
    # The [[Construct]] internal method for built-in function object F is called with parameters argumentsList and newTarget.
    # The steps performed are the same as [[Call]] (see 9.3.1) except that step 10 is replaced by:
    #
    # 10. Let result be the Completion Record that is the result of evaluating F in an implementation-defined manner that
    #     conforms to the specification of F. The this value is uninitialized, argumentsList provides the named parameters, and
    #     newTarget provides the NewTarget value.
    #
    # Implementation note: The [[Construct]] method is **not** present on all Builtin Functions --- it's only there for
    # functions which are also constructors. So we don't put it in the class definition. Instead we leave it here and add it
    # during the CreateBuiltinFunction call, if we detect a "Construct" sitting in the interal_slots_list.
    #
    # 1. Let callerContext be the running execution context.
    caller_context = surrounding_agent.running_ec
    # 2. If callerContext is not already suspended, suspend callerContext.
    caller_context.suspend()
    # 3. Let calleeContext be a new ECMAScript code execution context.
    callee_context = ExecutionContext()
    # 4. Set the Function of calleeContext to F.
    callee_context.function = self
    # 5. Let calleeRealm be F.[[Realm]].
    callee_realm = self.realm
    # 6. Set the Realm of calleeContext to calleeRealm.
    callee_context.realm = callee_realm
    # 7. Set the ScriptOrModule of calleeContext to F.[[ScriptOrModule]].
    callee_context.script_or_module = self.script_or_module
    # 8. Perform any necessary implementation-defined initialization of calleeContext.
    # 9. Push calleeContext onto the execution context stack; calleeContext is now the running execution context.
    surrounding_agent.ec_stack.append(callee_context)
    surrounding_agent.running_ec = callee_context
    # 10. Let result be the Completion Record that is the result of evaluating F in an implementation-defined manner that
    #     conforms to the specification of F. The this value is uninitialized, argumentsList provides the named parameters, and
    #     newTarget provides the NewTarget value.
    result = self.steps(None, new_target, *arguments_list)
    if not isinstance(result, Completion):
        result = result
    # 11. Remove calleeContext from the execution context stack and restore callerContext as the running execution
    #     context.
    surrounding_agent.ec_stack.pop()
    surrounding_agent.running_ec = caller_context
    # 12. Return result.
    return result
    # NOTE
    # When calleeContext is removed from the execution context stack it must not be destroyed if it has been
    # suspended and retained by an accessible generator object for later resumption.


# 9.3.3 CreateBuiltinFunction ( steps, internalSlotsList [ , realm [ , prototype ] ] )
def CreateBuiltinFunction(steps, internal_slots_list, realm=missing.MISSING, prototype=missing.MISSING):
    # The abstract operation CreateBuiltinFunction takes arguments steps, internalSlotsList, realm, and prototype. The
    # argument internalSlotsList is a List of the names of additional internal slots that must be defined as part of the
    # object. CreateBuiltinFunction returns a built-in function object created by the following steps:
    #
    # 1. Assert: steps is either a set of algorithm steps or other definition of a function's behaviour provided in this
    #    specification.
    # 2. If realm is not present, set realm to the current Realm Record.
    if realm == missing.MISSING:
        realm = surrounding_agent.running_ec.realm
    # 3. Assert: realm is a Realm Record.
    assert isinstance(realm, Realm)
    # 4. If prototype is not present, set prototype to realm.[[Intrinsics]].[[%FunctionPrototype%]].
    if prototype == missing.MISSING:
        prototype = realm.intrinsics['%FunctionPrototype%']
    # 5. Let func be a new built-in function object that when called performs the action described by steps. The new
    #    function object has internal slots whose names are the elements of internalSlotsList. The initial value of each
    #    of those internal slots is undefined.
    func = BuiltinFunction(steps, realm, prototype, True, JSNull.NULL, internal_slots_list)
    if 'Construct' in internal_slots_list:
        func.Construct = types.MethodType(BuiltinFunction_Construct, func)
    # 6. Set func.[[Realm]] to realm.
    # 7. Set func.[[Prototype]] to prototype.
    # 8. Set func.[[Extensible]] to true.
    # 9. Set func.[[ScriptOrModule]] to null.
    # 10. Return func.
    return func
    # Each built-in function defined in this specification is created by calling the CreateBuiltinFunction abstract
    # operation.
#endregion
#region 9.4 Built-in Exotic Object Internal Methods and Slots
#################################################################################################################################################################################################################
#
#  .d8888b.          d8888      888888b.            d8b 888 888           d8b              8888888888                   888    d8b               .d88888b.  888         d8b                   888
# d88P  Y88b        d8P888      888  "88b           Y8P 888 888           Y8P              888                          888    Y8P              d88P" "Y88b 888         Y8P                   888
# 888    888       d8P 888      888  .88P               888 888                            888                          888                     888     888 888                               888
# Y88b. d888      d8P  888      8888888K.  888  888 888 888 888888        888 88888b.      8888888    888  888  .d88b.  888888 888  .d8888b     888     888 88888b.    8888  .d88b.   .d8888b 888888
#  "Y888P888     d88   888      888  "Y88b 888  888 888 888 888           888 888 "88b     888        `Y8bd8P' d88""88b 888    888 d88P"        888     888 888 "88b   "888 d8P  Y8b d88P"    888
#        888     8888888888     888    888 888  888 888 888 888    888888 888 888  888     888          X88K   888  888 888    888 888          888     888 888  888    888 88888888 888      888
# Y88b  d88P d8b       888      888   d88P Y88b 888 888 888 Y88b.         888 888  888     888        .d8""8b. Y88..88P Y88b.  888 Y88b.        Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.
#  "Y8888P"  Y8P       888      8888888P"   "Y88888 888 888  "Y888        888 888  888     8888888888 888  888  "Y88P"   "Y888 888  "Y8888P      "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888
#                                                                                                                                                                       888
#                                                                                                                                                                      d88P
#                                                                                                                                                                    888P"
#
# 8888888          888                                       888     888b     d888          888    888                    888                                     888      .d8888b.  888          888
#   888            888                                       888     8888b   d8888          888    888                    888                                     888     d88P  Y88b 888          888
#   888            888                                       888     88888b.d88888          888    888                    888                                     888     Y88b.      888          888
#   888   88888b.  888888  .d88b.  888d888 88888b.   8888b.  888     888Y88888P888  .d88b.  888888 88888b.   .d88b.   .d88888 .d8888b       8888b.  88888b.   .d88888      "Y888b.   888  .d88b.  888888 .d8888b
#   888   888 "88b 888    d8P  Y8b 888P"   888 "88b     "88b 888     888 Y888P 888 d8P  Y8b 888    888 "88b d88""88b d88" 888 88K              "88b 888 "88b d88" 888         "Y88b. 888 d88""88b 888    88K
#   888   888  888 888    88888888 888     888  888 .d888888 888     888  Y8P  888 88888888 888    888  888 888  888 888  888 "Y8888b.     .d888888 888  888 888  888           "888 888 888  888 888    "Y8888b.
#   888   888  888 Y88b.  Y8b.     888     888  888 888  888 888     888   "   888 Y8b.     Y88b.  888  888 Y88..88P Y88b 888      X88     888  888 888  888 Y88b 888     Y88b  d88P 888 Y88..88P Y88b.       X88
# 8888888 888  888  "Y888  "Y8888  888     888  888 "Y888888 888     888       888  "Y8888   "Y888 888  888  "Y88P"   "Y88888  88888P'     "Y888888 888  888  "Y88888      "Y8888P"  888  "Y88P"   "Y888  88888P'
#
#################################################################################################################################################################################################################
# 9.4 Built-in Exotic Object Internal Methods and Slots
#
# This specification defines several kinds of built-in exotic objects. These objects generally behave similar to ordinary
# objects except for a few specific situations. The following exotic objects use the ordinary object internal methods except
# where it is explicitly specified otherwise below:
#
# 9.4.1 Bound Function Exotic Objects
#
# A bound function is an exotic object that wraps another function object. A bound function is callable (it has a [[Call]]
# internal method and may have a [[Construct]] internal method). Calling a bound function generally results in a call of its
# wrapped function.
#
# Bound function objects do not have the internal slots of ECMAScript function objects defined in Table 27. Instead they have
# the internal slots defined in Table 28.
#
# Table 28: Internal Slots of Bound Function Exotic Objects
# +-------------------------+-----------------+--------------------------------------------------------------------------------
# | Internal Slot           | Type            | Description
# +-------------------------+-----------------+--------------------------------------------------------------------------------
# | [[BoundTargetFunction]] | Callable Object | The wrapped function object.
# +-------------------------+-----------------+--------------------------------------------------------------------------------
# | [[BoundThis]]           | Any             | The value that is always passed as the this value when calling the wrapped
# |                         |                 | function.
# +-------------------------+-----------------+--------------------------------------------------------------------------------
# | [[BoundArguments]]      | List of Any     | A list of values whose elements are used as the first arguments to any call to
# |                         |                 | the wrapped function.
# +-------------------------+-----------------+--------------------------------------------------------------------------------
# Bound function objects provide all of the essential internal methods as specified in 9.1. However, they use the following
# definitions for the essential internal methods of function objects.
class BoundFunctionObject(JSObject):
    # 9.4.1.1 [[Call]] ( thisArgument, argumentsList )
    def Call(self, thisArgument, argumentsList):
        # When the [[Call]] internal method of a bound function exotic object, F, which was created using the bind function is
        # called with parameters thisArgument and argumentsList, a List of ECMAScript language values, the following steps are
        # taken:
        #
        # 1. Let target be F.[[BoundTargetFunction]].
        target = self.BoundTargetFunction
        # 2. Let boundThis be F.[[BoundThis]].
        boundThis = self.BoundThis
        # 3. Let boundArgs be F.[[BoundArguments]].
        boundArgs = self.BoundArguments
        # 4. Let args be a new list containing the same values as the list boundArgs in the same order followed by the same
        #    values as the list argumentsList in the same order.
        args = boundArgs + argumentsList
        # 5. Return ? Call(target, boundThis, args).
        return Call(target, boundThis, *args)

# 9.4.1.2 [[Construct]] ( argumentsList, newTarget )
def BoundFunction_Construct(self, argumentsList, newTarget):
    # When the [[Construct]] internal method of a bound function exotic object, F that was created using the bind function is
    # called with a list of arguments argumentsList and newTarget, the following steps are taken:
    #
    # 1. Let target be F.[[BoundTargetFunction]].
    target = self.BoundTargetFunction
    # 2. Assert: IsConstructor(target) is true.
    assert IsConstructor(target)
    # 3. Let boundArgs be F.[[BoundArguments]].
    boundArgs = self.BoundArguments
    # 4. Let args be a new list containing the same values as the list boundArgs in the same order followed by the same values
    #    as the list argumentsList in the same order.
    args = boundArgs + argumentsList
    # 5. If SameValue(F, newTarget) is true, set newTarget to target.
    if SameValue(self, newTarget):
        newTarget = target
    # 6. Return ? Construct(target, args, newTarget).
    return Construct(target, args, newTarget)

# 9.4.1.3 BoundFunctionCreate ( targetFunction, boundThis, boundArgs )
def BoundFunctionCreate(targetFunction, boundThis, boundArgs):
    # The abstract operation BoundFunctionCreate with arguments targetFunction, boundThis and boundArgs is used to specify the
    # creation of new Bound Function exotic objects. It performs the following steps:
    #
    # 1. Assert: Type(targetFunction) is Object.
    assert isObject(targetFunction)
    # 2. Let proto be ? targetFunction.[[GetPrototypeOf]]().
    proto = targetFunction.GetPrototypeOf()
    # 3. Let obj be a newly created object.
    # 4. Set obj's essential internal methods to the default ordinary object definitions specified in 9.1.
    # 5. Set obj.[[Call]] as described in 9.4.1.1.
    obj = BoundFunctionObject()
    # 6. If IsConstructor(targetFunction) is true, then
    if IsConstructor(targetFunction):
        # a. Set obj.[[Construct]] as described in 9.4.1.2.
        obj.Construct = types.MethodType(BoundFunction_Construct, obj)
    # 7. Set obj.[[Prototype]] to proto.
    obj.Prototype = proto
    # 8. Set obj.[[Extensible]] to true.
    obj.Extensible = True
    # 9. Set obj.[[BoundTargetFunction]] to targetFunction.
    obj.BoundTargetFunction = targetFunction
    # 10. Set obj.[[BoundThis]] to boundThis.
    obj.BoundThis = boundThis
    # 11. Set obj.[[BoundArguments]] to boundArgs.
    obj.BoundArguments = boundArgs
    # 12. Return obj.
    return obj

# ------------------------------------ 𝟗.𝟒.𝟐 𝑨𝒓𝒓𝒂𝒚 𝑬𝒙𝒐𝒕𝒊𝒄 𝑶𝒃𝒋𝒆𝒄𝒕𝒔 ------------------------------------
# 9.4.2 Array Exotic Objects
#
# An Array object is an exotic object that gives special treatment to array index property keys (see 6.1.7). A property
# whose property name is an array index is also called an element. Every Array object has a length property whose value
# is always a nonnegative integer less than 2^32. The value of the length property is numerically greater than the name
# of every own property whose name is an array index; whenever an own property of an Array object is created or changed,
# other properties are adjusted as necessary to maintain this invariant. Specifically, whenever an own property is
# added whose name is an array index, the value of the length property is changed, if necessary, to be one more than the
# numeric value of that array index; and whenever the value of the length property is changed, every own property whose
# name is an array index whose value is not smaller than the new length is deleted. This constraint applies only to own
# properties of an Array object and is unaffected by length or array index properties that may be inherited from its
# prototypes.
#
# NOTE
# A String property name P is an array index if and only if ToString(ToUint32(P)) is equal to P and ToUint32(P) is not
# equal to 2^32-1.
#
# Array exotic objects always have a non-configurable property named "length".
#
# Array exotic objects provide an alternative definition for the [[DefineOwnProperty]] internal method. Except for that
# internal method, Array exotic objects provide all of the other essential internal methods as specified in 9.1.
#
class ArrayObject(JSObject):
    # -------------------------------- 𝟗.𝟒.𝟐.𝟏 [[𝑫𝒆𝒇𝒊𝒏𝒆𝑶𝒘𝒏𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚]] ( 𝑷, 𝑫𝒆𝒔𝒄 ) ------------------------------------
    # 9.4.2.1 [[DefineOwnProperty]] ( P, Desc )
    def DefineOwnProperty(self, P, Desc):
        # When the [[DefineOwnProperty]] internal method of an Array exotic object A is called with property key P, and
        # Property Descriptor Desc, the following steps are taken:
        #
        # 1. Assert: IsPropertyKey(P) is true.
        assert IsPropertyKey(P)
        # 2. If P is "length", then
        if P == 'length':
            # a. Return ? ArraySetLength(A, Desc).
            return ArraySetLength(self, Desc)
        # 3. Else if P is an array index, then
        if isString(P):
            index = ToUint32(P)
            if ToString(index) == P and index != 0xffffffff:
                # a. Let oldLenDesc be OrdinaryGetOwnProperty(A, "length").
                oldLenDesc = OrdinaryGetOwnProperty(self, 'length')
                # b. Assert: oldLenDesc will never be undefined or an accessor descriptor because Array objects are created
                #    with a length data property that cannot be deleted or reconfigured.
                assert oldLenDesc is not None and IsDataDescriptor(oldLenDesc)
                # c. Let oldLen be oldLenDesc.[[Value]].
                oldLen = oldLenDesc.value
                # d. Let index be ! ToUint32(P).
                # e. If index ≥ oldLen and oldLenDesc.[[Writable]] is false, return false.
                if index >= oldLen and not oldLenDesc.writable:
                    return False
                # f. Let succeeded be ! OrdinaryDefineOwnProperty(A, P, Desc).
                succeeded = OrdinaryDefineOwnProperty(self, P, Desc)
                # g. If succeeded is false, return false.
                if not succeeded:
                    return False
                # h. If index ≥ oldLen, then
                if index >= oldLen:
                    # i. Set oldLenDesc.[[Value]] to index + 1.
                    oldLenDesc.value = index + 1
                    # ii. Let succeeded be OrdinaryDefineOwnProperty(A, "length", oldLenDesc).
                    succeeded = OrdinaryDefineOwnProperty(self, 'length', oldLenDesc)
                    # iii. Assert: succeeded is true.
                    assert succeeded
                # i. Return true.
                return True
        # 4. Return OrdinaryDefineOwnProperty(A, P, Desc).
        return OrdinaryDefineOwnProperty(self, P, Desc)

# ------------------------------------ 𝟗.𝟒.𝟐.𝟐 𝑨𝒓𝒓𝒂𝒚𝑪𝒓𝒆𝒂𝒕𝒆 ( 𝒍𝒆𝒏𝒈𝒕𝒉 [ , 𝒑𝒓𝒐𝒕𝒐 ] ) ------------------------------------
# 9.4.2.2 ArrayCreate ( length [ , proto ] )
def ArrayCreate(length, proto=None):
    # The abstract operation ArrayCreate with argument length (either 0 or a positive integer) and optional argument proto is
    # used to specify the creation of new Array exotic objects. It performs the following steps:
    #
    # 1. Assert: length is an integer Number ≥ 0.
    assert isNumber(length) and length >= 0 and math.floor(length) == length
    # 2. If length is -0, set length to +0.
    if math.copysign(1.0, length) < 0:
        length = 0
    # 3. If length > 2^32-1, throw a RangeError exception.
    if length > 0xffffffff:
        raise ESRangeError(f'length {length} is too large for array indices')
    # 4. If proto is not present, set proto to the intrinsic object %ArrayPrototype%.
    if proto is None:
        proto = surrounding_agent.running_ec.realm.intrinsics['%ArrayPrototype%']
    # 5. Let A be a newly created Array exotic object.
    # 6. Set A's essential internal methods except for [[DefineOwnProperty]] to the default ordinary object definitions
    #    specified in 9.1.
    # 7. Set A.[[DefineOwnProperty]] as specified in 9.4.2.1.
    A = ArrayObject()
    # 8. Set A.[[Prototype]] to proto.
    A.Prototype = proto
    # 9. Set A.[[Extensible]] to true.
    A.Extensible = True
    # 10. Perform ! OrdinaryDefineOwnProperty(A, "length",
    #           PropertyDescriptor { [[Value]]: length, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false }).
    OrdinaryDefineOwnProperty(A, 'length',
                                 PropertyDescriptor(value=length, writable=True, enumerable=False, configurable=False))
    # 11. Return A.
    return A

# ------------------------------------ 𝟗.𝟒.𝟐.𝟑 𝑨𝒓𝒓𝒂𝒚𝑺𝒑𝒆𝒄𝒊𝒆𝒔𝑪𝒓𝒆𝒂𝒕𝒆 ( 𝒐𝒓𝒊𝒈𝒊𝒏𝒂𝒍𝑨𝒓𝒓𝒂𝒚, 𝒍𝒆𝒏𝒈𝒕𝒉 ) ------------------------------------
# 9.4.2.3 ArraySpeciesCreate ( originalArray, length )
def ArraySpeciesCreate(originalArray, length):
    # The abstract operation ArraySpeciesCreate with arguments originalArray and length is used to specify the creation of a
    # new Array object using a constructor function that is derived from originalArray. It performs the following steps:
    #
    # 1. Assert: length is an integer Number ≥ 0.
    assert isNumber(length) and length >= 0 and math.floor(length) == length
    # 2. If length is -0, set length to +0.
    if math.copysign(1.0, length) < 0:
        length = 0
    # 3. Let isArray be ? IsArray(originalArray).
    isArray = IsArray(originalArray)
    # 4. If isArray is false, return ? ArrayCreate(length).
    if not isArray:
        return ArrayCreate(length)
    # 5. Let C be ? Get(originalArray, "constructor").
    C = Get(originalArray, 'constructor')
    # 6. If IsConstructor(C) is true, then
    if IsConstructor(C):
        # a. Let thisRealm be the current Realm Record.
        thisRealm = surrounding_agent.running_ec.realm
        # b. Let realmC be ? GetFunctionRealm(C).
        realmC = GetFunctionRealm(C)
        # c. If thisRealm and realmC are not the same Realm Record, then
        if thisRealm != realmC:
            # i. If SameValue(C, realmC.[[Intrinsics]].[[%Array%]]) is true, set C to undefined.
            if SameValue(C, realmC.intrinsics['%Array%']):
                C = None
    # 7. If Type(C) is Object, then
    if isObject(C):
        # a. Set C to ? Get(C, @@species).
        C = Get(C, wks_species)
        # b. If C is null, set C to undefined.
        if isNull(C):
            C = None
    # 8. If C is undefined, return ? ArrayCreate(length).
    if C is None:
        return ArrayCreate(length)
    # 9. If IsConstructor(C) is false, throw a TypeError exception.
    if not IsConstructor(C):
        raise ESTypeError()
    # 10. Return ? Construct(C, « length »).
    return Construct(C, [length])
    # NOTE
    # If originalArray was created using the standard built-in Array constructor for a realm that is not the realm of the
    # running execution context, then a new Array is created using the realm of the running execution context. This maintains
    # compatibility with Web browsers that have historically had that behaviour for the Array.prototype methods that now are
    # defined using ArraySpeciesCreate.

# ------------------------------------ 𝟗.𝟒.𝟐.𝟒 𝑨𝒓𝒓𝒂𝒚𝑺𝒆𝒕𝑳𝒆𝒏𝒈𝒕𝒉 ( 𝑨, 𝑫𝒆𝒔𝒄 ) ------------------------------------
# 9.4.2.4 ArraySetLength ( A, Desc )
def ArraySetLength(A, Desc):
    # When the abstract operation ArraySetLength is called with an Array exotic object A, and Property Descriptor Desc,
    # the following steps are taken:
    #
    #   1. If Desc.[[Value]] is absent, then
    #       a. Return OrdinaryDefineOwnProperty(A, "length", Desc).
    #   2. Let newLenDesc be a copy of Desc.
    #   3. Let newLen be ? ToUint32(Desc.[[Value]]).
    #   4. Let numberLen be ? ToNumber(Desc.[[Value]]).
    #   5. If newLen ≠ numberLen, throw a RangeError exception.
    #   6. Set newLenDesc.[[Value]] to newLen.
    #   7. Let oldLenDesc be OrdinaryGetOwnProperty(A, "length").
    #   8. Assert: oldLenDesc will never be undefined or an accessor descriptor because Array objects are created with
    #      a length data property that cannot be deleted or reconfigured.
    #   9. Let oldLen be oldLenDesc.[[Value]].
    #   10. If newLen ≥ oldLen, then
    #       a. Return OrdinaryDefineOwnProperty(A, "length", newLenDesc).
    #   11. If oldLenDesc.[[Writable]] is false, return false.
    #   12. If newLenDesc.[[Writable]] is absent or has the value true, let newWritable be true.
    #   13. Else,
    #       a. Need to defer setting the [[Writable]] attribute to false in case any elements cannot be deleted.
    #       b. Let newWritable be false.
    #       c. Set newLenDesc.[[Writable]] to true.
    #   14. Let succeeded be ! OrdinaryDefineOwnProperty(A, "length", newLenDesc).
    #   15. If succeeded is false, return false.
    #   16. Repeat, while newLen < oldLen,
    #       a. Set oldLen to oldLen - 1.
    #       b. Let deleteSucceeded be ! A.[[Delete]](! ToString(oldLen)).
    #       c. If deleteSucceeded is false, then
    #           i. Set newLenDesc.[[Value]] to oldLen + 1.
    #           ii. If newWritable is false, set newLenDesc.[[Writable]] to false.
    #           iii. Perform ! OrdinaryDefineOwnProperty(A, "length", newLenDesc).
    #           iv. Return false.
    #   17. If newWritable is false, then
    #       a. Return OrdinaryDefineOwnProperty(A, "length", PropertyDescriptor { [[Writable]]: false }). This call
    #          will always return true.
    #   18. Return true.
    # NOTE
    # In steps 3 and 4, if Desc.[[Value]] is an object then its valueOf method is called twice. This is legacy
    # behaviour that was specified with this effect starting with the 2nd Edition of this specification.
    if not hasattr(Desc, 'value'):
        return OrdinaryDefineOwnProperty(A, 'length', Desc)
    newLenDesc = copy(Desc)
    newLen = ToUint32(Desc.value)
    numberLen = ToNumber(Desc.value)
    if newLen != numberLen:
        raise ESRangeError(f'Array length {ToString(Desc.value)} is invalid')
    newLenDesc.value = newLen
    oldLenDesc = OrdinaryGetOwnProperty(A, 'length')
    assert oldLenDesc is not None and not oldLenDesc.is_accessor_descriptor()
    oldLen = oldLenDesc.value
    if newLen >= oldLen:
        return OrdinaryDefineOwnProperty(A, 'length', newLenDesc)
    if not oldLenDesc.writable:
        return False
    if getattr(newLenDesc, 'writable', True):
        newWritable = True
    else:
        newWritable = False
        newLenDesc.writable = True # Need to defer setting [[Writable]] in case any elements cannot be deleted
    succeeded = OrdinaryDefineOwnProperty(A, 'length', newLenDesc)
    if not succeeded:
        return False
    while newLen < oldLen:
        oldLen -= 1
        deleteSucceeded = A.Delete(ToString(oldLen))
        if not deleteSucceeded:
            newLenDesc.value = oldLen + 1
            if not newWritable:
                newLenDesc.writable = False
            OrdinaryDefineOwnProperty(A, 'length', newLenDesc)
            return False
    if not newWritable:
        return OrdinaryDefineOwnProperty(A, 'length', PropertyDescriptor(writable=False))
    return True

# ------------------------------------ 𝟗.𝟒.𝟑 𝑺𝒕𝒓𝒊𝒏𝒈 𝑬𝒙𝒐𝒕𝒊𝒄 𝑶𝒃𝒋𝒆𝒄𝒕𝒔 ------------------------------------
# 9.4.3 String Exotic Objects
# A String object is an exotic object that encapsulates a String value and exposes virtual integer-indexed data
# properties corresponding to the individual code unit elements of the String value. String exotic objects always
# have a data property named "length" whose value is the number of code unit elements in the encapsulated String value.
# Both the code unit data properties and the "length" property are non-writable and non-configurable.
#
# String exotic objects have the same internal slots as ordinary objects. They also have a [[StringData]] internal
# slot.
#
# String exotic objects provide alternative definitions for the following internal methods. All of the other String
# exotic object essential internal methods that are not defined below are as specified in 9.1.
class StringObject(JSObject):
    def __init__(self, value, prototype):
        # Essentially, this is all just StringCreate
        super().__init__()
        self.StringData = value
        self.Prototype = prototype
        self.Extensible = True
        desc = PropertyDescriptor(value=len(value), writable=False, enumerable=False, configurable=False)
        DefinePropertyOrThrow(self, 'length', desc)
    def __repr__(self):
        return f'String({self.StringData!r})'

    # -------------------------------- 𝟗.𝟒.𝟑.𝟏 [[𝑮𝒆𝒕𝑶𝒘𝒏𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚]] ( 𝑷 ) ------------------------------------
    # 9.4.3.1 [[GetOwnProperty]] ( P )
    def GetOwnProperty(self, P):
        # When the [[GetOwnProperty]] internal method of a String exotic object S is called with property key P, the
        # following steps are taken:
        #
        # 1. Assert: IsPropertyKey(P) is true.
        # 2. Let desc be OrdinaryGetOwnProperty(S, P).
        # 3. If desc is not undefined, return desc.
        # 4. Return ! StringGetOwnProperty(S, P).
        assert IsPropertyKey(P)
        desc = OrdinaryGetOwnProperty(self, P)
        if desc is not None:
            return desc
        return StringGetOwnProperty(self, P)

    # -------------------------------- 𝟗.𝟒.𝟑.𝟐 [[𝑫𝒆𝒇𝒊𝒏𝒆𝑶𝒘𝒏𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚]] ( 𝑷, 𝑫𝒆𝒔𝒄 ) ------------------------------------
    # 9.4.3.2 [[DefineOwnProperty]] ( P, Desc )
    def DefineOwnProperty(self, P, Desc):
        # When the [[DefineOwnProperty]] internal method of a String exotic object S is called with property key P, and
        # Property Descriptor Desc, the following steps are taken:
        #
        # 1. Assert: IsPropertyKey(P) is true.
        # 2. Let stringDesc be ! StringGetOwnProperty(S, P).
        # 3. If stringDesc is not undefined, then
        #    a. Let extensible be S.[[Extensible]].
        #    b. Return ! IsCompatiblePropertyDescriptor(extensible, Desc, stringDesc).
        # 4. Return ! OrdinaryDefineOwnProperty(S, P, Desc).
        assert IsPropertyKey(P)
        stringDesc = StringGetOwnProperty(self, P)
        if stringDesc is not None:
            extensible = self.IsExtensible
            rval = IsCompatiblePropertyDescriptor(extensible, Desc, stringDesc)
            return rval
        rval = OrdinaryDefineOwnProperty(self, P, Desc)
        return rval

    # -------------------------------- 𝟗.𝟒.𝟑.𝟑 [[𝑶𝒘𝒏𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚𝑲𝒆𝒚𝒔]] ( ) ------------------------------------
    # 9.4.3.3 [[OwnPropertyKeys]] ( )
    def OwnPropertyKeys(self):
        # When the [[OwnPropertyKeys]] internal method of a String exotic object O is called, the following steps are
        # taken:
        #
        # 1. Let keys be a new empty List.
        # 2. Let str be the String value of O.[[StringData]].
        # 3. Let len be the length of str.
        # 4. For each integer i starting with 0 such that i < len, in ascending order, do
        #    a. Add ! ToString(i) as the last element of keys.
        # 5. For each own property key P of O such that P is an integer index and ToInteger(P) ≥ len, in ascending
        #    numeric index order, do
        #    a. Add P as the last element of keys.
        # 6. For each own property key P of O such that Type(P) is String and P is not an integer index, in ascending
        #    chronological order of property creation, do
        #    a. Add P as the last element of keys.
        # 7. For each own property key P of O such that Type(P) is Symbol, in ascending chronological order of property
        #    creation, do
        #    a. Add P as the last element of keys.
        # 8. Return keys.
        g1 = (ToString(i) for i in range(len(self.StringData)))
        g2 = sorted((key for key in self.properties.keys() if isIntegerIndex(key) and ToInteger(key) >= len(self.StringData)),
                    key=lambda x: ToInteger(x))
        g3 = (key for key in self.properties.keys() if isString(key) and not isIntegerIndex(key))
        g4 = (key for key in self.properties.keys() if isSymbol(key))
        return list(chain(g1, g2, g3, g4))

# ------------------------------------ 𝟗.𝟒.𝟑.𝟒 𝑺𝒕𝒓𝒊𝒏𝒈𝑪𝒓𝒆𝒂𝒕𝒆 ( 𝒗𝒂𝒍𝒖𝒆, 𝒑𝒓𝒐𝒕𝒐𝒕𝒚𝒑𝒆 ) ------------------------------------
# 9.4.3.4 StringCreate ( value, prototype )
def StringCreate(value, prototype):
    # The abstract operation StringCreate with arguments value and prototype is used to specify the creation of new
    # String exotic objects. It performs the following steps:
    #
    # 1. Assert: Type(value) is String.
    # 2. Let S be a newly created String exotic object.
    # 3. Set S.[[StringData]] to value.
    # 4. Set S's essential internal methods to the default ordinary object definitions specified in 9.1.
    # 5. Set S.[[GetOwnProperty]] as specified in 9.4.3.1.
    # 6. Set S.[[DefineOwnProperty]] as specified in 9.4.3.2.
    # 7. Set S.[[OwnPropertyKeys]] as specified in 9.4.3.3.
    # 8. Set S.[[Prototype]] to prototype.
    # 9. Set S.[[Extensible]] to true.
    # 10. Let length be the number of code unit elements in value.
    # 11. Perform ! DefinePropertyOrThrow(S, "length", PropertyDescriptor { [[Value]]: length, [[Writable]]: false,
    #     [[Enumerable]]: false, [[Configurable]]: false }).
    # 12. Return S.
    assert isString(value)
    return StringObject(value, prototype)

# ------------------------------------ 𝟗.𝟒.𝟑.𝟓 𝑺𝒕𝒓𝒊𝒏𝒈𝑮𝒆𝒕𝑶𝒘𝒏𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚 ( 𝑺, 𝑷 ) ------------------------------------
# 9.4.3.5 StringGetOwnProperty ( S, P )
def StringGetOwnProperty(S, P):
    # The abstract operation StringGetOwnProperty called with arguments S and P performs the following steps:
    #
    # 1. Assert: S is an Object that has a [[StringData]] internal slot.
    # 2. Assert: IsPropertyKey(P) is true.
    # 3. If Type(P) is not String, return undefined.
    # 4. Let index be ! CanonicalNumericIndexString(P).
    # 5. If index is undefined, return undefined.
    # 6. If IsInteger(index) is false, return undefined.
    # 7. If index = -0, return undefined.
    # 8. Let str be the String value of S.[[StringData]].
    # 9. Let len be the length of str.
    # 10. If index < 0 or len ≤ index, return undefined.
    # 11. Let resultStr be the String value of length 1, containing one code unit from str, specifically the code unit
    #     at index index.
    # 12. Return a PropertyDescriptor { [[Value]]: resultStr, [[Writable]]: false, [[Enumerable]]: true,
    #     [[Configurable]]: false }.
    assert isObject(S) and hasattr(S, 'StringData')
    assert IsPropertyKey(P)
    if not isString(P):
        return None
    index = CanonicalNumericIndexString(P)
    if index is None:
        return None
    if not IsInteger(index):
        return None
    if index == 0 and math.copysign(1.0, index) < 0:
        return None
    if index < 0 or len(S.StringData) <= index:
        return None
    return PropertyDescriptor(value=S.StringData[int(index)], writable=False,
                                               enumerable=True, configurable=False)
# 9.4.7 Immutable Prototype Exotic Objects
#
# An immutable prototype exotic object is an exotic object that has a [[Prototype]] internal slot that will not change
# once it is initialized.
#
# Immutable prototype exotic objects have the same internal slots as ordinary objects. They are exotic only in the
# following internal methods. All other internal methods of immutable prototype exotic objects that are not explicitly
# defined below are instead defined as in ordinary objects.
#
# 9.4.7.1 [[SetPrototypeOf]] ( V )
# When the [[SetPrototypeOf]] internal method of an immutable prototype exotic object O is called with argument V, the
# following steps are taken:
#
# 1. Return ? SetImmutablePrototype(O, V).
#
# 9.4.7.2 SetImmutablePrototype ( O, V )
def SetImmutablePrototype(obj, value):
    # When the SetImmutablePrototype abstract operation is called with arguments O and V, the following steps are
    # taken:
    #
    # 1. Assert: Either Type(V) is Object or Type(V) is Null.
    assert isObject(value) or isNull(value)
    # 2. Let current be ? O.[[GetPrototypeOf]]().
    current = obj.GetPrototypeOf()
    # 3. If SameValue(V, current) is true, return true.
    # 4. Return false.
    return SameValue(value, current)
#endregion

# 9.5 ProxyObjects
class ProxyObject(JSObject):
    pass



# Stuff from chap 10: Source Text

# https://www.ecma-international.org/ecma-262/9.0/index.html#sec-utf16encoding
# 10.1.1 Static Semantics: UTF16Encoding(cp)
#
# The UTF16Encoding of a numeric code point value, cp, is determined as follows:
#
# Assert: 0 ≤ cp ≤ 0x10FFFF.
# If cp ≤ 0xFFFF, return cp.
# Let cu1 be floor((cp - 0x10000) / 0x400) + 0xD800.
# Let cu2 be ((cp - 0x10000) modulo 0x400) + 0xDC00.
# Return the code unit sequence consisting of cu1 followed by cu2.

def utf_16_encoding(cp):
    assert 0 <= cp and cp <= 0x10ffff
    if cp <= 0xffff:
        return [cp]
    cpx = cp - 0x10000
    return [(cpx >> 10) + 0xD800, (cpx & 0x3ff) + 0xdc00]


# https://www.ecma-international.org/ecma-262/9.0/index.html#sec-utf16decode
# 10.1.2 Static Semantics: UTF16Decode ( lead, trail )
# Two code units, lead and trail, that form a UTF-16 surrogate pair are converted to a code point by performing the following steps:
#
# Assert: lead is a leading surrogate and trail is a trailing surrogate.
# Let cp be (lead - 0xD800) × 0x400 + (trail - 0xDC00) + 0x10000.
# Return the code point cp.
def utf_16_decode(lead, trail):
    assert 0xd800 <= lead and lead <= 0xdbff
    assert 0xdc00 <= trail and trail <= 0xdfff
    return ((lead - 0xd800) << 10) + trail - 0xdc00 + 0x10000

class LexerError(Exception):
    def __init__(self, message):
        super().__init__()
        self.message = message

class Token:
    '''
    Representation of a single token.
    '''
    __slots__ = ('type', 'value', 'lineno', 'index')
    def __repr__(self):
        return f'Token(type={self.type!r}, value={self.value!r}, lineno={self.lineno}, index={self.index})'

class Lexer():

    @unique
    class Goal(Enum):
        InputElementDiv = auto()
        InputElementRegExp = auto()
        InputElementRegExpOrTemplateTail = auto()
        InputElementTemplateTail = auto()

    tokens = {
        'NUMERIC',
        'IDENTIFIER',
        'STRING',
        'EQUALS',
        'BANG',
        'BANGEQ',
        'BANGEQEQ',
        'PLUS',
        'MINUS',
        'PERCENT',
        'PERCENTEQ',
        'PERIOD',
        'DOTDOTDOT',
        'XOR',
        'XOREQ',
        'PIPE',
        'PIPEPIPE',
        'PIPEEQ',
        'AMP',
        'AMPAMP',
        'AMPEQ',
        'LPAREN_',
        'LPAREN_LET',
        'LPAREN_LBRACKET',
        'RPAREN',
        'LBRACKET',
        'RBRACKET',
        'LCURLY',
        'RCURLY',
        'STAR',
        'STARSTAR',
        'STAREQ',
        'STARSTAREQ',
        'PLUSPLUS',
        'MINUSMINUS',
        'PLUSEQ',
        'MINUSEQ',
        'COMMA',
        'SEMICOLON',
        'COLON',
        'LT',
        'LE',
        'LTLT',
        'LTLE',
        'EQEQ',
        'EQEQEQ',
        'EQGT',
        'GT',
        'GE',
        'GTGT',
        'GTGE',
        'GTGTGT',
        'GTGTGE',
        'QUESTION',
        'DIV',
        'DIVEQ',
        'TILDE',
        'AWAIT', 'BREAK', 'CASE', 'CATCH', 'CLASS', 'CONST', 'CONTINUE',
        'DEBUGGER', 'DEFAULT', 'DELETE', 'DO', 'ELSE', 'EXPORT', 'EXTENDS',
        'FINALLY', 'FOR', 'FUNCTION', 'IF', 'IMPORT', 'IN', 'INSTANCEOF',
        'NEW', 'RETURN', 'SUPER', 'SWITCH', 'THIS', 'THROW', 'TRY', 'TYPEOF',
        'VAR', 'VOID', 'WHILE', 'WITH', 'YIELD', 'ENUM', 'NULL', 'TRUE',
        'FALSE',

        'OF', # Not marked as a ReservedWord in the spec, but it's used as one in for statements.
        'LET', # Also not marked as a ReservedWord

        'GOAL_SCRIPT', 'GOAL_CALLMEMBEREXPRESSION', 'GOAL_PARENTHESIZEDEXPRESSION', 'GOAL_ASSIGNMENTPATTERN'
         }

    # Tokens we will fiter out (the parser should never see these)
    temp_tokens = { 'LPAREN' }

    GoalTokens = {
        'Script': 'GOAL_SCRIPT',
        'CallMemberExpression': 'GOAL_CALLMEMBEREXPRESSION',
        'ParenthesizedExpression': 'GOAL_PARENTHESIZEDEXPRESSION',
        'AssignmentPattern': 'GOAL_ASSIGNMENTPATTERN',
    }

    class TokenValue:
        __slots__ = ('name', 'value', 'lt_follows', 'index', 'length')
        def __repr__(self):
            return f'TokenValue(value={self.value!r}, name={self.name}, lt_follows={self.lt_follows})'

    whitespace = (
        '\u0009'  # <TAB> CHARACTER TABULATION
        '\u000b'  # <VT> LINE TABULATION
        '\u000c'  # <FF> FORM FEED
        '\u0020'  # <SP> SPACE
        '\u00a0'  # <NBSP> NO-BREAK SPACE
        '\ufeff'  # <ZWNBSP> ZERO WIDTH NO-BREAK SPACE
    )
    line_terminators = (
        '\u000a'  # <LF> LINE FEED
        '\u000d'  # <CR> CARRIAGE RETURN
        '\u2028'  # <LS> LINE SEPARATOR
        '\u2029'  # <PS> PARAGRAPH SEPARATOR
    )

    def __init__(self, source_text, first_token='Script'):
        super().__init__()
        self.source = source_text
        self.linenum = 1
        self.start = 0
        self.pos = 0
        self.first_token = self.GoalTokens[first_token]

    def _swallow(self, end_prior):
        self.start = self.pos
        if end_prior:
            self.start -= 1

    def _make_token(self, type, value, end_prior):
        assert type in self.tokens or type in self.temp_tokens
        tok = Token()
        val = self.TokenValue()
        val.value = value
        val.lt_follows = False
        val.name = type
        val.index = self.start
        val.length = self.pos - self.start - (1 if end_prior else 0)
        tok.value = val
        tok.type = type
        tok.index = self.start
        tok.lineno = self.linenum
        self._swallow(end_prior)
        return tok

    one_char_punctuators = {
        '(': 'LPAREN',
        ')': 'RPAREN',
        ',': 'COMMA',
        ':': 'COLON',
        ';': 'SEMICOLON',
        '?': 'QUESTION',
        '[': 'LBRACKET',
        ']': 'RBRACKET',
        '{': 'LCURLY',
        '}': 'RCURLY',
        '~': 'TILDE'
    }

    def _initial(self, ch, lookahead):
        # Empty string means we're done
        if ch == '':
            return (self._done, [])
        # Initial State for InputElementDiv:
        # We're looking at the first char for the production
        # InputElementDiv ::
        #    WhiteSpace
        #    LineTerminator
        #    Comment
        #    CommonToken
        #    DivPunctuator
        #    RightBracePunctuator
        if ch in self.whitespace or unicodedata.category(ch) == 'Zs':
            # WhiteSpace --- we ignore this
            self._swallow(False)
            return (self._initial, [])
        elif ch in self.line_terminators:
            # LineTerminator
            if ch != '\u000d' or lookahead != '\u000a':
                self.linenum += 1
            self._swallow(False)
            return (self._initial, [])
        elif ch == '/':
            # Might be Comment::SingleLineComment, Comment::MultiLineComment, DivPunctuator::/, or DivPunctuator::/=
            return (self._comment_or_div, [])
        elif ch in '(),:;?[]{}~':
            # These are CommonToken::Punctuator or RightBracePunctuator that are uniquely one character in size
            return (self._initial, [self._make_token(self.one_char_punctuators[ch], ch, False)])
        elif ch == '!':
            return (self._bang, [])
        elif ch == '%':
            return (self._percent, [])
        elif ch == '&':
            return (self._ampersand, [])
        elif ch == '*':
            return (self._asterisk, [])
        elif ch == '+':
            return (self._plus, [])
        elif ch == '-':
            return (self._minus, [])
        elif ch == '.':
            return (self._period, [])
        elif ch == '<':
            return (self._less_than, [])
        elif ch == '=':
            return (self._equals, [])
        elif ch == '>':
            return (self._greater_than, [])
        elif ch == '^':
            return (self._caret, [])
        elif ch == '|':
            return (self._pipe, [])

        # NumericLiterals start with the digits 0 through 9, or the period. (Period is handled in self._period.)
        elif ch in '0123456789':
            return (self._numeric_start, [])

        # IdentifierName also manages to include reserved words (this function also captures the start of a unicode
        # escape sequence).
        elif self.is_identifier_start(ch):
            if ch == '\\':
                return (self._ident_start_escape, [])
            return (self._ident_capture, [])

        elif ch == "'":
            return (self._single_string_capture, [])
        elif ch == '"':
            return (self._double_string_capture, [])

        # More to add still...
        return (self._initial, [])

    def _done(self, ch, lookahead):
        pass  # should never get here, honestly.

    def _bang(self, ch, lookahead):
        # We already have !. Might be !, !=, or !==.
        if ch == '=':
            return (self._bang_equals, [])
        tok = self._make_token('BANG', '!', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _bang_equals(self, ch, lookahead):
        # We already have !=. Might also be !==.
        if ch == '=':
            return (self._initial, [self._make_token('BANGEQEQ', '!==', False)])
        tok = self._make_token('BANGEQ', '!=', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _percent(self, ch, lookahead):
        # We already have %. Might also be %=.
        if ch == '=':
            return (self._initial, [self._make_token('PERCENTEQ', '%=', False)])
        tok = self._make_token('PERCENT', '%', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _ampersand(self, ch, lookahead):
        # We already have &. Might also be && or &=.
        if ch == '&':
            return (self._initial, [self._make_token('AMPAMP', '&&', False)])
        if ch == '=':
            return (self._initial, [self._make_token('AMPEQ', '&=', False)])
        tok = self._make_token('AMP', '&', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _asterisk(self, ch, lookahead):
        # We already have *. Might also be **, **=, or *=
        if ch == '=':
            return (self._initial, [self._make_token('STAREQ', '*=', False)])
        if ch == '*':
            return (self._star_star, [])
        tok = self._make_token('STAR', '*', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _star_star(self, ch, lookahead):
        # We already have **. Might also be **=.
        if ch == '=':
            return (self._initial, [self._make_token('STARSTAREQ', '**=', False)])
        tok = self._make_token('STARSTAR', '**', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _plus(self, ch, lookahead):
        # We already have +. Might also be ++ or +=.
        if ch == '+':
            return (self._initial, [self._make_token('PLUSPLUS', '++', False)])
        if ch == '=':
            return (self._initial, [self._make_token('PLUSEQ', '+=', False)])
        tok = self._make_token('PLUS', '+', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _minus(self, ch, lookahead):
        # We already have -. Might also be -- or -=.
        if ch == '-':
            return (self._initial, [self._make_token('MINUSMINUS', '--', False)])
        if ch == '=':
            return (self._initial, [self._make_token('MINUSEQ', '-=', False)])
        tok = self._make_token('MINUS', '-', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _period(self, ch, lookahead):
        # We already have '.'. Might also be '...'.
        if ch == '.':
            return (self._dot_dot, [])
        if ch and ch in '0123456789':
            # Hey, look, it's a number.
            return (self._after_decimal, [])
        tok = self._make_token('PERIOD', '.', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _dot_dot(self, ch, lookahead):
        # We already have '..'. Might also be '...'.
        if ch == '.':
            return (self._initial, [self._make_token('DOTDOTDOT', '...', False)])
        # (But there's no '..', so that tokenizes into two individual dots.)
        tok = self._make_token('PERIOD', '.', True)
        self.start -= 1 # back up start to point to the 2nd dot
        tok2 = self._make_token('PERIOD', '.', True) # This just got the 2nd dot
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok, tok2], results))

    def _less_than(self, ch, lookahead):
        # We already have '<'. Might also be '<<', '<<=', or '<='.
        if ch == '=':
            return (self._initial, [self._make_token('LE', '<=', False)])
        if ch == '<':
            return (self._less_less, [])
        tok = self._make_token('LT', '<', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _less_less(self, ch, lookahead):
        # We have <<. Might also be <<=.
        if ch == '=':
            return (self._initial, [self._make_token('LTLE', '<<=', False)])
        tok = self._make_token('LTLT', '<<', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _equals(self, ch, lookahead):
        # We have =. Might also be ==, ===, or =>.
        if ch == '>':
            return (self._initial, [self._make_token('EQGT', '=>', False)])
        if ch == '=':
            return (self._equal_equal, [])
        tok = self._make_token('EQUALS', '=', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _equal_equal(self, ch, lookahead):
        # We have ==. Might also be ===.
        if ch == '=':
            return (self._initial, [self._make_token('EQEQEQ', '===', False)])
        tok = self._make_token('EQEQ', '==', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _greater_than(self, ch, lookahead):
        # We have >. Might also be >=, >>, >>=, >>>, or >>>=.
        if ch == '=':
            return (self._initial, [self._make_token('GE', '>=', False)])
        if ch == '>':
            return (self._greater_greater, [])
        tok = self._make_token('GT', '>', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _greater_greater(self, ch, lookahead):
        # We have >>. Might also be >>=, >>>, or >>>=.
        if ch == '=':
            return (self._initial, [self._make_token('GTGE', '>>=', False)])
        if ch == '>':
            return (self._greater_x3, [])
        tok = self._make_token('GTGT', '>>', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _greater_x3(self, ch, lookahead):
        # We have >>>. Might also be >>>=.
        if ch == '=':
            return (self._initial, [self._make_token('GTGTGE', '>>>=', False)])
        tok = self._make_token('GTGTGT', '>>>', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _caret(self, ch, lookahead):
        # We have ^. Might also be ^=.
        if ch == '=':
            return (self._initial, [self._make_token('XOREQ', '^=', False)])
        tok = self._make_token('XOR', '^', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _pipe(self, ch, lookahead):
        # We have |. Might also be |= or ||.
        if ch == '=':
            return (self._initial, [self._make_token('PIPEEQ', '|=', False)])
        if ch == '|':
            return (self._initial, [self._make_token('PIPEPIPE', '||', False)])
        tok = self._make_token('PIPE', '|', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _comment_or_div(self, ch, lookahead):
        # We have one slash. All kinds of things this might be.
        if ch == '=':
            return (self._initial, [self._make_token('DIVEQ', '/=', False)])
        if ch == '/':
            # A SingleLineComment
            return (self._single_line_comment, [])
        if ch == '*':
            # A MultiLineComment
            return (self._multi_line_comment, [])
        tok = self._make_token('DIV', '/', True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _single_line_comment(self, ch, lookahead):
        if ch and ch not in self.line_terminators:
            return (self._single_line_comment, [])
        self._swallow(True)
        return self._initial(ch, lookahead)

    def _multi_line_comment(self, ch, lookahead):
        if ch == '':
            # Hit EOF! That's an unterminated comment error.
            raise LexerError('Unterminated multi-line comment')
        if ch in self.line_terminators:
            if ch != '\u000d' or lookahead != '\u000a':
                self.linenum += 1
        pos = self.pos
        if ch == '/' and (pos - self.start) >= 4 and self.source[pos-2] == '*':
            self._swallow(False)
            return (self._initial, [])
        return (self._multi_line_comment, [])

    def _numeric_start(self, ch, lookahead):
        # We have the first char of a number (though, not a period)
        # The new char might indicate a different base. Check that.
        if self.source[self.start] == '0':
            if ch and ch in 'bB':
                return (self._binary_digits, [])
            if ch and ch in 'oO':
                return (self._octal_digits, [])
            if ch and ch in 'xX':
                return (self._hex_digits, [])
            if ch and ch == '.':
                return (self._after_decimal, [])
            if ch and ch in 'eE':
                return (self._after_e, [])
        elif ch and ch in '0123456789':
            return (self._integer_part, [])
        elif ch and ch in 'eE':
            return (self._after_e, [])
        elif ch == '.':
            return (self._after_decimal, [])

        if ch and (ch in '0123456789' or self.is_identifier_start(ch)):
            raise LexerError('Invalid chars after Numeric Literal')

        tok = self._make_token('NUMERIC', float(self.source[self.start:self.pos-1]), True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _binary_digits(self, ch, lookahead):
        if ch and ch in '01':
            return (self._binary_digits, [])
        if ch and (ch in '23456789' or self.is_identifier_start(ch)):
            raise LexerError('Invalid chars after Numeric Literal')
        if self.source[self.pos-2] not in '01':
            raise LexerError('Invalid numeric literal')
        tok = self._make_token('NUMERIC', float(int(self.source[self.start:self.pos-1], 2)), True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _octal_digits(self, ch, lookahead):
        if ch and ch in '01234567':
            return (self._octal_digits, [])
        if ch and (ch in '89' or self.is_identifier_start(ch)):
            raise LexerError('Invalid chars after Numeric Literal')
        if self.source[self.pos-2] not in '01234567':
            raise LexerError('Invalid numeric literal')
        tok = self._make_token('NUMERIC', float(int(self.source[self.start:self.pos-1], 8)), True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _hex_digits(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._hex_digits, [])
        if ch and self.is_identifier_start(ch):
            raise LexerError('Invalid chars after Numeric Literal')
        if self.source[self.pos-2] not in '0123456789abcdefABCDEF':
            raise LexerError('Invalid numeric literal')
        tok = self._make_token('NUMERIC', float(int(self.source[self.start:self.pos-1], 16)), True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _integer_part(self, ch, lookahead):
        if ch and ch in '0123456789':
            return (self._integer_part, [])
        if ch == '.':
            return (self._after_decimal, [])
        if ch and ch in 'eE':
            return (self._after_e, [])

        if ch and self.is_identifier_start(ch):
            raise LexerError('Invalid chars after Numeric Literal')
        tok = self._make_token('NUMERIC', float(self.source[self.start:self.pos-1]), True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _after_decimal(self, ch, lookahead):
        if ch and ch in '0123456789':
            return (self._after_decimal, [])
        if ch and ch in 'eE':
            return (self._after_e, [])

        if ch and self.is_identifier_start(ch):
            raise LexerError('Invalid chars after Numeric Literal')
        tok = self._make_token('NUMERIC', float(self.source[self.start:self.pos-1]), True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _after_e(self, ch, lookahead):
        if ch and ch in '-+0123456789':
            return (self._exponent_digits, [])
        # If we saw an 'e' but no number following, we wind up with a syntax error, thanks to the rule: "The
        # SourceCharacter immediately following a NumericLiteral must not be an IdentifierStart or DecimalDigit."
        #
        # I.e.: the lexical rules actually say that "3e" is the NumericLiteral "3" followed by the Identifier "e", but
        # the rule makes that a syntax error. (Note that "3e1" is a NumericLiteral on its own. Splitting it into two
        # tokens in the prior case makes things very confusing.) Anyway: we don't have to get tricky here, we just
        # raise an exception.
        raise LexerError('Invalid numeric literal')

    def _exponent_digits(self, ch, lookahead):
        if ch and ch in '0123456789':
            return (self._exponent_digits, [])
        if ch and self.is_identifier_start(ch):
            raise LexerError('Invalid chars after Numeric Literal')
        tok = self._make_token('NUMERIC', float(self.source[self.start:self.pos-1]), True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    @staticmethod
    def is_unicode_id_start(ch):
        # ID_Start characters are derived from the Unicode General_Category of uppercase letters, lowercase letters,
        # titlecase letters, modifier letters, other letters, letter numbers, plus Other_ID_Start, minus Pattern_Syntax and
        # Pattern_White_Space code points.

        # The General_Category parts of that are: Lu Ll Lt Lm Lo and Nl.
        # Other_ID_Start is U+1885, U+1886, U+2118, U+212E, U+309B, and U+309C (which have categories Mn Sm So and Sk).
        # Pattern_Syntax is a long list, but doesn't have anything from Other_ID_Start, and only 2E2F (vertical tilde) from
        # the category matches.
        # Pattern_White_Space is short, but doesn't have anything from the prior categories, so I'm not sure why it's even
        # listed.

        cat = unicodedata.category(ch)
        return ((cat in ['Lu', 'Ll', 'Lt', 'Lm', 'Lo', 'Nl'] and ch != '\u2e2f') or
                ch in '\u1885\u1886\u2118\u212e\u309b\u309c')

    @classmethod
    def is_unicode_id_continue(cls, ch):
        # ID_Continue characters include ID_Start characters, plus characters having the Unicode General_Category of
        # nonspacing marks, spacing combining marks, decimal number, connector punctuation, plus Other_ID_Continue , minus
        # Pattern_Syntax and Pattern_White_Space code points.

        # In set notation:
        # [\p{ID_Start}\p{Mn}\p{Mc}\p{Nd}\p{Pc}\p{Other_ID_Continue}-\p{Pattern_Syntax}-\p{Pattern_White_Space}]

        # ================================================
        #
        # 00B7          ; Other_ID_Continue # Po       MIDDLE DOT
        # 0387          ; Other_ID_Continue # Po       GREEK ANO TELEIA
        # 1369..1371    ; Other_ID_Continue # No   [9] ETHIOPIC DIGIT ONE..ETHIOPIC DIGIT NINE
        # 19DA          ; Other_ID_Continue # No       NEW TAI LUE THAM DIGIT ONE
        #
        # Total code points: 12
        other_id_continue = '\u00b7\u0387\u1369\u136a\u136b\u136c\u136d\u136e\u136f\u1370\u1371\u19da'

        # Again, the Pattern_Syntax and Pattern_White_Space don't actually take anything out -- those chars aren't in the
        # valid list to begin with.
        return (cls.is_unicode_id_start(ch) or
                (unicodedata.category(ch) in [ 'Mn', 'Mc', 'Nd', 'Pc' ] or ch in other_id_continue))

    @classmethod
    def is_identifier_start(cls, ch):
        return cls.is_unicode_id_start(ch) or ch in '$_\\'

    @staticmethod
    def _detect_reserved_word(word):
        reserved_word_tokens = [
            'await', 'break', 'case', 'catch', 'class', 'const', 'continue',
            'debugger', 'default', 'delete', 'do', 'else', 'export', 'extends',
            'finally', 'for', 'function', 'if', 'import', 'in', 'instanceof',
            'new', 'return', 'super', 'switch', 'this', 'throw', 'try', 'typeof',
            'var', 'void', 'while', 'with', 'yield', 'enum', 'null', 'true',
            'false', 'of', 'let' ] # ('of' and 'let' are not technically reserved, but we have to treat them as word tokens)
        return word.upper() if word in reserved_word_tokens else None

    @staticmethod
    def _identifier_string_value(word):
        escape_matcher = re.compile(r'\\u(?:([0-9A-Fa-f]{4})|(?:\{([0-9a-fA-F]+)\}))')
        while 1:
            # You might think this "replace again" method is fraught with errors, since an encoded slash would illegally be
            # decoded multiple times. It turns out that's not a problem because the slash is not a valid identifier character
            # and was rejected before we ever got here. At this point, we're guaranteed that all escaped characters are
            # valid identifier characters, and none of those will get decoded any more than one time.
            match = escape_matcher.search(word)
            if match is None:
                return word
            word = word.replace(match.group(0), chr(int(match.group(1) or match.group(2), 16)))

    def _tok_rw_or_id(self):
        word = self.source[self.start:self.pos-1]
        token_type = self._detect_reserved_word(word)
        if token_type:
            return self._make_token(token_type, word, True)
        return self._make_token('IDENTIFIER', self._identifier_string_value(word), True)

    def _ident_capture(self, ch, lookahead):
        if ch and (self.is_unicode_id_continue(ch) or ch in '$\u200c\u200d'):
            return (self._ident_capture, [])
        if ch == '\\':
            return (self._identpart_escape_1, [])
        tok = self._tok_rw_or_id()
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _identpart_escape_1(self, ch, lookahead):
        if ch != 'u':
            raise LexerError('Invalid IdentifierName escape sequence')
        return (self._identpart_escape_2, [])

    def _ident_start_escape(self, ch, lookahead):
        if ch != 'u':
            raise LexerError('Invalid IdentifierName escape sequence')
        return (self._identstart_escape_2, [])

    def _identpart_escape_2(self, ch, lookahead):
        # We have '\u'. Next is either exactly 4 hex digits, or '{' <many digits> '}'.
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._3_digits_left, [])
        if ch == '{':
            return (self._n_digits_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _identstart_escape_2(self, ch, lookahead):
        # We have '\u'. Next is either exactly 4 hex digits, or '{' <many digits> '}'.
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._istart_3_digits_left, [])
        if ch == '{':
            return (self._istart_n_digits_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _3_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._2_digits_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _istart_3_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._istart_2_digits_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _2_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._1_digit_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _istart_2_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._istart_1_digit_left, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _1_digit_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            pos = self.pos
            escaped_char = chr(int(self.source[pos-4:pos], 16))
            if self.is_unicode_id_continue(escaped_char) or escaped_char in '$\u200c\u200d':
                return (self._ident_capture, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _istart_1_digit_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            pos = self.pos
            escaped_char = chr(int(self.source[pos-4:pos], 16))
            if self.is_unicode_id_start(escaped_char) or escaped_char in '$_':
                return (self._ident_capture, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _n_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._n_digits_left, [])
        pos = self.pos
        if ch == '}':
            if self.source[pos-2] == '{':
                # No digits!
                raise LexerError('Invalid IdentifierName escape sequence')
            bracket = self.source.rfind('{', self.start, pos)
            charcode = int(self.source[bracket+1:pos-1], 16)
            if charcode <= 0x10FFFF:
                escaped_char = chr(charcode)
                if self.is_unicode_id_continue(escaped_char) or escaped_char in '$\u200c\u200d':
                    return (self._ident_capture, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    def _istart_n_digits_left(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._istart_n_digits_left, [])
        pos = self.pos
        if ch == '}':
            if self.source[pos-2] == '{':
                # No digits!
                raise LexerError('Invalid IdentifierName escape sequence')
            bracket = self.source.rfind('{', self.start, pos)
            charcode = int(self.source[bracket+1:pos-1], 16)
            if charcode <= 0x10FFFF:
                escaped_char = chr(charcode)
                if self.is_unicode_id_start(escaped_char) or escaped_char in '$_':
                    return (self._ident_capture, [])
        raise LexerError('Invalid IdentifierName escape sequence')

    @classmethod
    def _single_string_value(cls, chars):
        assert chars[0] == "'" and chars[-1] == "'"
        # Remove the quotes
        chars = chars[1:len(chars)-1]
        # Translate any escape sequences
        if chars.find('\\') < 0:
            # No escapes. We're done
            return chars
        decoded = ''
        index = 0
        while index < len(chars):
            slash = chars.find('\\', index)
            if slash < 0: # No more!
                decoded += chars[index:]
                break
            decoded += chars[index:slash]
            index = slash
            if chars[index+1] == 'x':
                decoded += chr(int(chars[index+2:index+4], 16))
                index += 4
            elif chars[index+1] == 'u':
                if chars[index+2] != '{':
                    decoded += chr(int(chars[index+2:index+6], 16))
                    index += 6
                else:
                    closing = chars.find('}', index+3)
                    decoded += chr(int(chars[index+3:closing], 16))
                    index = closing + 1
            elif chars[index+1] == '0':
                decoded += '\u0000'
                index += 2
            elif chars[index+1] == 'b':
                decoded += '\u0008'
                index += 2
            elif chars[index+1] == 't':
                decoded += '\u0009'
                index += 2
            elif chars[index+1] == 'n':
                decoded += '\u000a'
                index += 2
            elif chars[index+1] == 'v':
                decoded += '\u000b'
                index += 2
            elif chars[index+1] == 'f':
                decoded += '\u000c'
                index += 2
            elif chars[index+1] == 'r':
                decoded += '\u000d'
                index += 2
            elif chars[index+1] in cls.line_terminators:
                if chars[index+1] == '\r' and chars[index+2] == '\n':
                    index += 3
                else:
                    index += 2
            else:
                decoded += chars[index+1]
                index += 2
        return decoded

    def _single_string_capture(self, ch, lookahead):
        if ch == "'":
            return (self._initial, [self._make_token('STRING', self._single_string_value(self.source[self.start:self.pos]), False)])
        if ch == '\\':
            return (self._string_escape, [])
        if ch == '' or ch in self.line_terminators:
            raise LexerError('Unterminated String')
        return (self._single_string_capture, [])

    def _string_escape(self, ch, lookahead):
        # We've already consumed the leading slash of one of:
        #  *  SingleStringCharacter :: \ EscapeSequence
        #  *  SingleStringCharacter :: LineContinuation :: \ LineTerminatorSequence
        if ch == '':
            raise LexerError('Syntax Error: Unterminated string escape')
        if ch in self.line_terminators:
            # This is the LineContinuation bit
            if ch != '\r' or lookahead != '\n':
                self.linenum += 1
                return (self._single_string_capture, [])
            return (self._single_string_lfonly, [])
        if ch == '0' and (lookahead == '' or lookahead not in '0123456789'):
            # EscapeSequence :: 0 (lookahead not in DecimalDigit)
            return (self._single_string_capture, [])
        if ch == 'x':
            return (self._single_string_hex_escape, [])
        if ch == 'u':
            return (self._single_string_unicode_escape, [])
        if ch in '0123456789':
            raise LexerError('Syntax Error in string escape')
        return (self._single_string_capture, [])

    def _single_string_lfonly(self, ch, lookahead):
        # we already know ch is \n, so just capture and move on
        return (self._single_string_capture, [])

    def _single_string_hex_escape(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            if self.source[self.pos-2] == 'x':
                return (self._single_string_hex_escape, [])
            return (self._single_string_capture, [])
        raise LexerError('Syntax Error in Hex Value for String Escape')

    def _single_string_unicode_escape(self, ch, lookahead):
        # Either 4 hex digits, or '{' <many hex digits> '}'
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._single_string_unicode_4digits, [])
        if ch == '{':
            return (self._single_string_unicode_brackets, [])
        raise LexerError('Syntax Error in Unicode String Escape')

    def _single_string_unicode_4digits(self, ch, lookahead):
        # We've gotten \u<digit> .. with maybe more digits.
        if ch and ch in '0123456789abcdefABCDEF':
            upos = self.source.rfind('u', self.start, self.pos)
            if self.pos - upos == 5:
                return (self._single_string_capture, [])
            return (self._single_string_unicode_4digits, [])
        raise LexerError('Syntax Error in Unicode String Escape')

    def _single_string_unicode_brackets(self, ch, lookahead):
        # We've gotten \u{ and maybe digits
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._single_string_unicode_brackets, [])
        if ch == '}' and self.source[self.pos-2] != '{':
            bracket = self.source.rfind('{', self.start, self.pos)
            charcode = int(self.source[bracket+1:self.pos-1], 16)
            if charcode <= 0x10FFFF:
                return (self._single_string_capture, [])
        raise LexerError('Syntax Error in Unicode String Escape')

    def lex(self, goal=Goal.InputElementDiv):
        token_buffer = deque([])
        for token in self.lex_nolooks(goal):
            token_buffer.append(token)
            if len(token_buffer) > 1:
                src = token_buffer.popleft()
                if src.type == 'LPAREN':
                    if token_buffer[0].type == 'LBRACKET':
                        src.type = 'LPAREN_LBRACKET'  # A left-parenthesis, where lookahead = [
                    elif token_buffer[0].type == 'LET':
                        src.type = 'LPAREN_LET' #  left-parenthesis, where lookahead = let
                    else:
                        src.type = 'LPAREN_'  # left-parenthesis, where lookahead isn't something we care about
                yield src
        for tok in token_buffer:
            yield tok
    def lex_nolooks(self, goal=Goal.InputElementDiv):
        state = self._initial
        token_buffer = deque([self._make_token(self.first_token, None, False)])

        try:
            ch = self.source[self.pos]
        except IndexError:
            ch = ''
        try:
            lookahead = self.source[self.pos+1]
        except IndexError:
            lookahead = ''
        self.pos += 1

        while state != self._done:
            state, results = state(ch, lookahead)
            token_buffer.extend(results)

            while len(token_buffer) > 1:
                if token_buffer[0].lineno < token_buffer[1].lineno:
                    token_buffer[0].value.lt_follows = True
                yield token_buffer.popleft()
            ch = lookahead
            try:
                lookahead = self.source[self.pos+1]
            except IndexError:
                lookahead = ''
            self.pos += 1
        while len(token_buffer) > 0:
            yield token_buffer.popleft()

ReservedWords = [
            'await', 'break', 'case', 'catch', 'class', 'const', 'continue',
            'debugger', 'default', 'delete', 'do', 'else', 'export', 'extends',
            'finally', 'for', 'function', 'if', 'import', 'in', 'instanceof',
            'new', 'return', 'super', 'switch', 'this', 'throw', 'try', 'typeof',
            'var', 'void', 'while', 'with', 'yield', 'enum', 'null', 'true',
            'false' ]

class ParseNode:
    def __init__(self, name, p):
        self.name = name
        self.children = [p[z] for z in range(len(p))]
        self.ctx = None
    def __repr__(self):
        #return f'{self.name}[{",".join(repr(child) for child in self.children)}]'
        children = ' '.join(ch.name if isinstance(ch, ParseNode) else str(ch.value) for ch in self.children)
        terms = ' '.join(repr(trm.value) if trm.name == 'STRING' else str(trm.value) for trm in self.terminals())
        return f'ParseNode[{self.name} : ' + children + '] (' + terms + ')'
    def terminals(self):
        for child in self.children:
            if not isinstance(child, ParseNode):
                yield child
            else:
                for sub_token in child.terminals():
                    yield sub_token
    def first_terminal(self):
        for child in self.children:
            if isinstance(child, ParseNode):
                t = child.first_terminal()
                if t is not None:
                    return t
            else:
                return child
        return None
    def last_terminal(self):
        for child in (self.children[idx] for idx in range(len(self.children)-1,-1,-1)):
            if isinstance(child, ParseNode):
                l = child.last_terminal()
                if l is not None:
                    return l
            else:
                return child
        return None
    def source_range(self):
        # Find the first terminal:
        t = self.first_terminal()
        start_idx = t.index
        # Find the last terminal:
        l = self.last_terminal()
        end_idx = l.index + l.length
        return (start_idx, end_idx)
    def covering(self, symbol):
        start, end = self.source_range()
        source = self.ctx.source_text[start:end]
        subparser = Ecma262Parser(start=symbol, source_text=source)
        sublexer = Lexer(source, symbol)
        try:
            tree = subparser.parse(sublexer.lex())
        except ESError:
            tree = None
        return tree
    def Contains(self, symbol):
        return (any(child.name == symbol for child in self.children) or
                any(child.Contains(symbol) for child in self.children if isinstance(child, ParseNode)))
    def Is(self, symbol):
        return len(self.children) == 1 and (self.children[0].name == symbol or (isinstance(self.children[0], ParseNode) and self.children[0].Is(symbol)))
    def EarlyErrorsScan(self):
        errs = []
        # Check the children first
        for ch in (child for child in self.children if isinstance(child, ParseNode)):
            errs.extend(ch.EarlyErrorsScan())
        # Now myself.
        errs.extend(self.EarlyErrors())
        return errs
    def EarlyErrors(self):
        return []
    def defer_target(self):
        # When a function defers by default to its children, it picks the sole nonterminal. The routine here figures out which
        # parse node that actually is. (And asserts if there wasn't a sole nonterminal.)
        child_nonterminals = [ch for ch in self.children if isinstance(ch, ParseNode)]
        assert len(child_nonterminals) == 1
        return child_nonterminals[0]
    def IsFunctionDefinition(self):
        return self.defer_target().IsFunctionDefinition()
    def IsValidSimpleAssignmentTarget(self):
        return self.defer_target().IsValidSimpleAssignmentTarget()
    def LexicallyDeclaredNames(self):
        return self.defer_target().LexicallyDeclaredNames()
    def TopLevelLexicallyDeclaredNames(self):
        return self.defer_target().TopLevelLexicallyDeclaredNames()
    def VarDeclaredNames(self):
        return self.defer_target().VarDeclaredNames()
    def TopLevelVarDeclaredNames(self):
        return self.defer_target().TopLevelVarDeclaredNames()
    def VarScopedDeclarations(self):
        return self.defer_target().VarScopedDeclarations()
    def TopLevelVarScopedDeclarations(self):
        return self.defer_target().TopLevelVarScopedDeclarations()
    def LexicallyScopedDeclarations(self):
        return self.defer_target().LexicallyScopedDeclarations()
    def TopLevelLexicallyScopedDeclarations(self):
        return self.defer_target().TopLevelLexicallyScopedDeclarations()
    def ContainsDuplicateLabels(self, lst):
        return self.defer_target().ContainsDuplicateLabels(lst)
    def ContainsUndefinedBreakTarget(self, lst):
        return self.defer_target().ContainsUndefinedBreakTarget(lst)
    def ContainsUndefinedContinueTarget(self, iterationSet, labelSet):
        return self.defer_target().ContainsUndefinedContinueTarget(iterationSet, labelSet)
    def BoundNames(self):
        return self.defer_target().BoundNames()
    def StringValue(self):
        return self.defer_target().StringValue()
    def ArgumentListEvaluation(self):
        return self.defer_target().ArgumentListEvaluation()
    def IsConstantDeclaration(self):
        return self.defer_target().IsConstantDeclaration()
    def PropertyDefinitionEvaluation(self, object, enumerable):
        return self.defer_target().PropertyDefinitionEvaluation(object, enumerable)
    def IteratorDestructuringAssignmentEvaluation(self, iteratorRecord):
        return self.defer_target().IteratorDestructuringAssignmentEvaluation(iteratorRecord)
    def DestructuringAssignmentEvaluation(self, value):
        return self.defer_target().DestructuringAssignmentEvaluation(value)
    def PropertyBindingInitialization(self, value, environment):
        return self.defer_target().PropertyBindingInitialization(value, environment)

    def evaluate(self):
        # Subclasses need to override this, or we'll throw an AttributeError when we hit a terminal.
        return self.defer_target().evaluate()

#######################################################################################################################
#
#  d888    .d8888b.       d888       8888888      888                   888    d8b  .d888 d8b
# d8888   d88P  Y88b     d8888         888        888                   888    Y8P d88P"  Y8P
#   888          888       888         888        888                   888        888
#   888        .d88P       888         888    .d88888  .d88b.  88888b.  888888 888 888888 888  .d88b.  888d888 .d8888b
#   888    .od888P"        888         888   d88" 888 d8P  Y8b 888 "88b 888    888 888    888 d8P  Y8b 888P"   88K
#   888   d88P"            888         888   888  888 88888888 888  888 888    888 888    888 88888888 888     "Y8888b.
#   888   888"       d8b   888         888   Y88b 888 Y8b.     888  888 Y88b.  888 888    888 Y8b.     888          X88
# 8888888 888888888  Y8P 8888888     8888888  "Y88888  "Y8888  888  888  "Y888 888 888    888  "Y8888  888      88888P'
#
#######################################################################################################################
class PN_IdentifierReference(ParseNode):
    def __init__(self, ctx, p, yield_=False, await_=False):
        super().__init__('IdentifierReference', p)
        self.yield_ = yield_
        self.await_ = await_
        self.strict = ctx.strict
        self.goal = ctx.goal
class PN_IdentifierReference_Identifier(PN_IdentifierReference):
    def EarlyErrors(self):
        # Early Errors
        if self.yield_ and self.children[0].StringValue() == 'yield':
            return [CreateSyntaxError('\'yield\' not allowed in this context')]
        if self.await_ and self.children[0].StringValue() == 'await':
            return [CreateSyntaxError('\'await\' not allowed in this context')]
        return []
    def IsValidSimpleAssignmentTarget(self):
        return not (self.strict and self.children[0].StringValue() in ['eval', 'arguments'])
    def StringValue(self):
        return self.children[0].StringValue()
    def evaluate(self):
        return ResolveBinding(self.children[0].StringValue())
class PN_IdentifierReference_AWAIT(PN_IdentifierReference):
    def EarlyErrors(self):
        # Early Errors
        if self.goal == 'Module':
            return [CreateSyntaxError('\'await\' not allowed in modules')]
        return []
    def IsValidSimpleAssignmentTarget(self):
        return True
    def StringValue(self):
        return 'await'
    def evaluate(self):
        return ResolveBinding('await')
class PN_IdentifierReference_YIELD(PN_IdentifierReference):
    def EarlyErrors(self):
        # Early Errors
        if self.strict:
            return [CreateSyntaxError('\'yield\' not allowed in this context')]
        return []
    def IsValidSimpleAssignmentTarget(self):
        return True
    def StringValue(self):
        return 'yield'
    def evaluate(self):
        return ResolveBinding('yield')

class PN_BindingIdentifier(ParseNode):
    def __init__(self, ctx, p, yield_=False, await_=False):
        super().__init__('BindingIdentifier', p)
        self.yield_ = yield_
        self.await_ = await_
        self.strict = ctx.strict
        self.goal = ctx.goal
class PN_BindingIdentifier_Identifier(PN_BindingIdentifier):
    @property
    def Identifier(self):
        return self.children[0]
    def EarlyErrors(self):
        # 12.1.1 Static Semantics: Early Errors
        #   BindingIdentifier : Identifier
        #   * It is a Syntax Error if the code matched by this production is contained in strict mode code and the
        #     StringValue of Identifier is  "arguments" or "eval".
        #   * It is a Syntax Error if this production has a [Yield] parameter and StringValue of Identifier is "yield".
        #   * It is a Syntax Error if this production has an [Await] parameter and StringValue of Identifier is "await".
        errs = []
        sv = self.Identifier.StringValue()
        if self.strict and sv in ['arguments', 'eval']:
            errs.append(CreateSyntaxError('In strict mode, an identifier name may be neither \'arguments\' nor \'eval\'.'))
        if self.yield_ and sv == 'yield':
            errs.append(CreateSyntaxError('\'yield\' may not be used as an identifier in this context'))
        if self.await_ and sv == 'await':
            errs.append(CreateSyntaxError('\'await\' may not be used as an identifier in this context'))
        return errs
    def BoundNames(self):
        # 12.1.2 Static Semantics: BoundNames
        #   BindingIdentifier : Identifier
        #   1. Return a new List containing the StringValue of Identifier.
        return [self.Identifier.StringValue()]
    def BindingInitialization(self, value, environment):
        # 12.1.5 Runtime Semantics: BindingInitialization
        #   BindingIdentifier : Identifier
        #   1. Let name be StringValue of Identifier.
        #   2. Return ? InitializeBoundName(name, value, environment).
        return InitializeBoundName(self.Identifier.StringValue(), value, environment)
class PN_BindingIdentifier_YIELD(PN_BindingIdentifier):
    def EarlyErrors(self):
        # 12.1.1 Static Semantics: Early Errors
        #           BindingIdentifier : yield
        # * It is a Syntax Error if the code matched by this production is contained in strict mode code.
        # * It is a Syntax Error if this production has a [Yield] parameter.
        errs = []
        if self.strict:
            errs.append(CreateSyntaxError('\'yield\' is an illegal identifier in strict code mode'))
        if self.yield_:
            errs.append(CreateSyntaxError('\'yield\' may not be used as an indentifier in this context'))
    def BoundNames(self):
        # 12.1.2 Static Semantics: BoundNames
        #           BindingIdentifier : yield
        # 1. Return a new List containing "yield".
        return ['yield']
    def StringValue(self):
        # 12.1.4 Static Semantics: StringValue
        #           BindingIdentifier : yield
        # 1. Return "yield".
        return 'yield'
    def BindingInitialization(self, value, environment):
        # 12.1.5 Runtime Semantics: BindingInitialization
        #   With parameters value and environment.
        #           BindingIdentifier : yield
        # 1. Return ? InitializeBoundName("yield", value, environment).
        return InitializeBoundName('yield', value, environment)
class PN_BindingIdentifier_AWAIT(PN_BindingIdentifier):
    def EarlyErrors(self):
        # 12.1.1 Static Semantics: Early Errors
        #           BindingIdentifier : await
        # * It is a Syntax Error if the goal symbol of the syntactic grammar is Module.
        # * It is a Syntax Error if this production has a [Await] parameter.
        errs = []
        if self.goal == 'Module':
            errs.append(CreateSyntaxError('\'await\' is an illegal identifier in modules'))
        if self.await_:
            errs.append(CreateSyntaxError('\'await\' may not be used as an indentifier in this context'))
    def BoundNames(self):
        # 12.1.2 Static Semantics: BoundNames
        #           BindingIdentifier : await
        # 1. Return a new List containing "await".
        return ['await']
    def StringValue(self):
        # 12.1.4 Static Semantics: StringValue
        #           BindingIdentifier : await
        # 1. Return "await".
        return 'await'
    def BindingInitialization(self, value, environment):
        # 12.1.5 Runtime Semantics: BindingInitialization
        #   With parameters value and environment.
        #           BindingIdentifier : await
        # 1. Return ? InitializeBoundName("await", value, environment).
        return InitializeBoundName('await', value, environment)
# 12.1.5.1 Runtime Semantics: InitializeBoundName ( name, value, environment )
def InitializeBoundName(name, value, environment):
    # 1. Assert: Type(name) is String.
    assert isString(name)
    # 2. If environment is not undefined, then
    if environment is not None:
        # a. Let env be the EnvironmentRecord component of environment.
        env = environment.environment_record
        # b. Perform env.InitializeBinding(name, value).
        env.InitializeBinding(name, value)
        # c. Return undefined)
        return None
    # 3. Else,
    # a. Let lhs be ResolveBinding(name).
    lhs = ResolveBinding(name)
    # b. Return ? PutValue(lhs, value).
    return PutValue(lhs, value)

class PN_LabelIdentifier(ParseNode):
    def __init__(self, ctx, p, yield_=False, await_=False):
        super().__init__('LabelIdentifier', p)
        self.yield_ = yield_
        self.await_ = await_
        self.goal = ctx.goal
        self.strict = ctx.strict
class PN_LabelIdentifier_Identifier(PN_LabelIdentifier):
    @property
    def Identifier(self):
        return self.children[0]
    def EarlyErrors(self):
        # 12.1.1 Static Semantics: Early Errors
        #           LabelIdentifier : Identifier
        # * It is a Syntax Error if this production has a [Yield] parameter and StringValue of Identifier is "yield".
        # * It is a Syntax Error if this production has an [Await] parameter and StringValue of Identifier is "await".
        errs = []
        sv = self.Identifier.StringValue()
        if self.yield_ and sv == 'yield':
            errs.append(CreateSyntaxError('\'yield\' may not be used as an identifier in this context'))
        if self.await_ and sv == 'await':
            errs.append(CreateSyntaxError('\'await\' may not be used as an identifier in this context'))
        return errs
class PN_LabelIdentifier_YIELD(PN_LabelIdentifier):
    def EarlyErrors(self):
        # 12.1.1 Static Semantics: Early Errors
        #           LabelIdentifier : yield
        # * It is a Syntax Error if the code matched by this production is contained in strict mode code.
        if self.strict:
            return [CreateSyntaxError('\'yield\' may not be used as an identifier in strict code mode')]
        return []
    def StringValue(self):
        # 12.1.4 Static Semantics: StringValue
        #           LabelIdentifier : yield
        # 1. Return "yield".
        return 'yield'
class PN_LabelIdentifier_AWAIT(PN_LabelIdentifier):
    def EarlyErrors(self):
        # 12.1.1 Static Semantics: Early Errors
        #           LabelIdentifier : await
        # * It is a Syntax Error if the goal symbol of the syntactic grammar is Module.
        if self.goal == 'Module':
            return [CreateSyntaxError('\'await\' may not be used as an identifier in a module')]
        return []
    def StringValue(self):
        # 12.1.4 Static Semantics: StringValue
        #           LabelIdentifier : await
        # 1. Return "await".
        return 'await'

class PN_Identifier(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('Identifier', p)
        self.strict = ctx.strict
        self.goal = ctx.goal
    def EarlyErrors(self):
        identifier_name = self.children[0].value
        # Early Errors
        if self.strict:
            if identifier_name in ['implements', 'interface', 'let', 'package', 'private', 'protected', 'public', 'static', 'yield']:
                return [CreateSyntaxError(f'\'{identifier_name}\' is a reserved word in strict mode')]
        if self.goal == 'Module':
            if identifier_name == 'await':
                return [CreateSyntaxError('\'await\' is a reserved word for modules')]
        if identifier_name in ['break', 'case', 'catch', 'class', 'const', 'continue',
            'debugger', 'default', 'delete', 'do', 'else', 'export', 'extends',
            'finally', 'for', 'function', 'if', 'import', 'in', 'instanceof',
            'new', 'return', 'super', 'switch', 'this', 'throw', 'try', 'typeof',
            'var', 'void', 'while', 'with', 'enum', 'null', 'true',
            'false']:
            return [CreateSyntaxError(f'\'{identifier_name}\' is a reserved word')]
        return []
    def StringValue(self):
        return self.children[0].value
class PN_ReservedWord(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ReservedWord', p)
    def StringValue(self):
        return self.children[0].value
class PN_IdentifierName(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('IdentifierName', p)
#################################################################################################################################################################################################
#
#  d888    .d8888b.       .d8888b.      8888888b.          d8b                                             8888888888                                                      d8b
# d8888   d88P  Y88b     d88P  Y88b     888   Y88b         Y8P                                             888                                                             Y8P
#   888          888            888     888    888                                                         888
#   888        .d88P          .d88P     888   d88P 888d888 888 88888b.d88b.   8888b.  888d888 888  888     8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.
#   888    .od888P"       .od888P"      8888888P"  888P"   888 888 "888 "88b     "88b 888P"   888  888     888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b
#   888   d88P"          d88P"          888        888     888 888  888  888 .d888888 888     888  888     888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888
#   888   888"       d8b 888"           888        888     888 888  888  888 888  888 888     Y88b 888     888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888
# 8888888 888888888  Y8P 888888888      888        888     888 888  888  888 "Y888888 888      "Y88888     8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888
#                                                                                                  888                         888
#                                                                                             Y8b d88P                         888
#                                                                                              "Y88P"                          888
#
#################################################################################################################################################################################################
class PN_PrimaryExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('PrimaryExpression', p)
class PN_PrimaryExpression_THIS(PN_PrimaryExpression):
    def evaluate(self):
        return ResolveThisBinding()
    def IsFunctionDefinition(self):
        return False
    def IsIdentifierRef(self):
        return False
    def IsValidSimpleAssignmentTarget(self):
        return False
class PN_PrimaryExpression_IdentifierReference(PN_PrimaryExpression):
    def IsFunctionDefinition(self):
        return False
    def IsIdentifierRef(self):
        return True
class PN_PrimaryExpression_Literal(PN_PrimaryExpression):
    def IsFunctionDefinition(self):
        return False
    def IsIdentifierRef(self):
        return False
    def IsValidSimpleAssignmentTarget(self):
        return False
class PN_PrimaryExpression_ArrayLiteral(PN_PrimaryExpression):
    def IsFunctionDefinition(self):
        return False
    def IsIdentifierRef(self):
        return False
    def IsValidSimpleAssignmentTarget(self):
        return False
class PN_PrimaryExpression_ObjectLiteral(PN_PrimaryExpression):
    def IsFunctionDefinition(self):
        return False
    def IsIdentifierRef(self):
        return False
    def IsValidSimpleAssignmentTarget(self):
        return False
class PN_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList(PN_PrimaryExpression):
    # 12.2.10 The Grouping Operator
    def __init__(self, ctx, p):
        super().__init__(ctx, p)
        self.ctx = ctx
        self.children[0].covered_production = self.covering('ParenthesizedExpression')
    @property
    def CoverParenthesizedExpressionAndArrowParameterList(self):
        return self.children[0]
    def EarlyErrors(self):
        # 12.2.10.1 Static Semantics: Early Errors
        #           PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
        # * It is a Syntax Error if CoverParenthesizedExpressionAndArrowParameterList is not covering a
        #   ParenthesizedExpression.
        # * All Early Error rules for ParenthesizedExpression and its derived productions also apply to
        #   CoveredParenthesizedExpression of CoverParenthesizedExpressionAndArrowParameterList.
        errs = []
        expr = self.CoverParenthesizedExpressionAndArrowParameterList.CoveredParenthesizedExpression()
        if not isinstance(expr, ParseNode):
            errs.append(CreateSyntaxError('Bad grouping syntax'))
        else:
            errs.extend(expr.EarlyErrorsScan())
        return errs
    def HasName(self):
        # 12.2.1.2 Static Semantics: HasName
        #           PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
        # 1. Let expr be CoveredParenthesizedExpression of CoverParenthesizedExpressionAndArrowParameterList.
        # 2. If IsFunctionDefinition of expr is false, return false.
        # 3. Return HasName of expr.
        expr = self.CoverParenthesizedExpressionAndArrowParameterList.CoveredParenthesizedExpression()
        return expr.IsFunctionDefinition() and expr.HasName()
    def IsFunctionDefinition(self):
        # 12.2.1.3 Static Semantics: IsFunctionDefinition
        #           PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
        # 1. Let expr be CoveredParenthesizedExpression of CoverParenthesizedExpressionAndArrowParameterList.
        # 2. Return IsFunctionDefinition of expr.
        expr = self.CoverParenthesizedExpressionAndArrowParameterList.CoveredParenthesizedExpression()
        return expr.IsFunctionDefinition()
    def IsIdentifierRef(self):
        return False
    def IsValidSimpleAssignmentTarget(self):
        # 12.2.1.5 Static Semantics: IsValidSimpleAssignmentTarget
        #           PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
        # 1. Let expr be CoveredParenthesizedExpression of CoverParenthesizedExpressionAndArrowParameterList.
        # 2. Return IsValidSimpleAssignmentTarget of expr.
        expr = self.CoverParenthesizedExpressionAndArrowParameterList.CoveredParenthesizedExpression()
        return expr.IsValidSimpleAssignmentTarget()
    def evaluate(self):
        # 12.2.10.4 Runtime Semantics: Evaluation
        #           PrimaryExpression : CoverParenthesizedExpressionAndArrowParameterList
        # 1. Let expr be CoveredParenthesizedExpression of CoverParenthesizedExpressionAndArrowParameterList.
        # 2. Return the result of evaluating expr.
        return self.CoverParenthesizedExpressionAndArrowParameterList.CoveredParenthesizedExpression().evaluate()

class PN_CoverParenthesizedExpressionAndArrowParameterList(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('CoverParenthesizedExpressionAndArrowParameterList', p)
    def CoveredParenthesizedExpression(self):
        return getattr(self, 'covered_production', None)
class PN_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_RPAREN(PN_CoverParenthesizedExpressionAndArrowParameterList):
    pass
class PN_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_RPAREN(PN_CoverParenthesizedExpressionAndArrowParameterList):
    pass
class PN_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_RPAREN(PN_CoverParenthesizedExpressionAndArrowParameterList):
    pass
class PN_ParenthesizedExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ParenthesizedExpression', p)
class PN_ParenthesizedExpression_LPAREN_Expression_RPAREN(PN_ParenthesizedExpression):
    @property
    def Expression(self):
        return self.children[1]
    def evaluate(self):
        # 12.2.10.4 Runtime Semantics: Evaluation
        #           ParenthesizedExpression : ( Expression )
        # 1. Return the result of evaluating Expression. This may be of type Reference.
        # NOTE
        # This algorithm does not apply GetValue to the result of evaluating Expression. The principal motivation for
        # this is so that operators such as delete and typeof may be applied to parenthesized expressions.
        return self.Expression.evaluate()

# ------------------------------------ 𝟏𝟐.𝟐.𝟒 𝑳𝒊𝒕𝒆𝒓𝒂𝒍𝒔 ------------------------------------
class PN_Literal(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('Literal', p)
class PN_Literal_NULL(PN_Literal):
    def evaluate(self):
        return JSNull.NULL
class PN_Literal_BOOLEAN(PN_Literal):
    def evaluate(self):
        return self.children[0].value == 'true'
class PN_Literal_NUMERIC(PN_Literal):
    def evaluate(self):
        return self.children[0].value
class PN_Literal_STRING(PN_Literal):
    def evaluate(self):
        return self.children[0].value

# ------------------------------------ 𝟏𝟐.𝟐.𝟓 𝑨𝒓𝒓𝒂𝒚 𝑰𝒏𝒊𝒕𝒊𝒂𝒍𝒊𝒛𝒆𝒓 ------------------------------------
# 12.2.5 Array Initializer
#
# NOTE
# An ArrayLiteral is an expression describing the initialization of an Array object, using a list, of zero or more
# expressions each of which represents an array element, enclosed in square brackets. The elements need not be
# literals; they are evaluated each time the array initializer is evaluated.
#
# Array elements may be elided at the beginning, middle or end of the element list. Whenever a comma in the element
# list is not preceded by an AssignmentExpression (i.e., a comma at the beginning or after another comma), the missing
# array element contributes to the length of the Array and increases the index of subsequent elements. Elided array
# elements are not defined. If an element is elided at the end of an array, that element does not contribute to the
# length of the Array.
#
# ------------------------------------ 𝑨𝒓𝒓𝒂𝒚𝑳𝒊𝒕𝒆𝒓𝒂𝒍 ------------------------------------
class PN_ArrayLiteral(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ArrayLiteral', p)
class PN_ArrayLiteral_LBRACKET_RBRACKET(PN_ArrayLiteral):
    def evaluate(self):
        # 12.2.5.3 Runtime Semantics: Evaluation
        # ArrayLiteral : [ ]
        #   1. Let array be ! ArrayCreate(0).
        #   2. Let pad be 0.
        #   3. Perform Set(array, "length", ToUint32(pad), false).
        #   4. NOTE: The above Set cannot fail because of the nature of the object returned by ArrayCreate.
        #   5. Return array.
        array = ArrayCreate(0)
        Set(array, 'length', 0, False)
        return array
class PN_ArrayLiteral_LBRACKET_Elision_RBRACKET(PN_ArrayLiteral):
    @property
    def Elision(self):
        return self.children[1]
    def evaluate(self):
        # 12.2.5.3 Runtime Semantics: Evaluation
        # ArrayLiteral : [ Elision ]
        #   1. Let array be ! ArrayCreate(0).
        #   2. Let pad be the ElisionWidth of Elision.
        #   3. Perform Set(array, "length", ToUint32(pad), false).
        #   4. NOTE: The above Set cannot fail because of the nature of the object returned by ArrayCreate.
        #   5. Return array.
        array = ArrayCreate(0)
        Set(array, 'length', self.Elision.ElisionWidth(), False)
        return array
class PN_ArrayLiteral_LBRACKET_ElementList_RBRACKET(PN_ArrayLiteral):
    @property
    def ElementList(self):
        return self.children[1]
    def evaluate(self):
        # 12.2.5.3 Runtime Semantics: Evaluation
        # ArrayLiteral : [ ElementList ]
        #   1. Let array be ! ArrayCreate(0).
        #   2. Let len be the result of performing ArrayAccumulation for ElementList with arguments array and 0.
        #   3. ReturnIfAbrupt(len).
        #   4. Perform Set(array, "length", ToUint32(len), false).
        #   5. NOTE: The above Set cannot fail because of the nature of the object returned by ArrayCreate.
        #   6. Return array.
        array = ArrayCreate(0)
        length = self.ElementList.ArrayAccumulation(array, 0)
        Set(array, 'length', ToUint32(length), False)
        return array
class PN_ArrayLiteral_LBRACKET_ElementList_COMMA_RBRACKET(PN_ArrayLiteral_LBRACKET_ElementList_RBRACKET):
    pass
class PN_ArrayLiteral_LBRACKET_ElementList_COMMA_Elision_RBRACKET(PN_ArrayLiteral):
    @property
    def ElementList(self):
        return self.children[1]
    @property
    def Elision(self):
        return self.children[3]
    def evaluate(self):
        # 12.2.5.3 Runtime Semantics: Evaluation
        # ArrayLiteral : [ ElementList , Elision ]
        #   1. Let array be ! ArrayCreate(0).
        #   2. Let len be the result of performing ArrayAccumulation for ElementList with arguments array and 0.
        #   3. ReturnIfAbrupt(len).
        #   4. Let padding be the ElisionWidth of Elision.
        #   5. Perform Set(array, "length", ToUint32(padding+len), false).
        #   6. NOTE: The above Set cannot fail because of the nature of the object returned by ArrayCreate.
        #   7. Return array.
        array = ArrayCreate(0)
        length = self.ElementList.ArrayAccumulation(array, 0)
        padding = self.Elision.ElisionWidth()
        Set(array, 'length', ToUint32(padding+length), False)
        return array
# ------------------------------------ 𝑬𝒍𝒆𝒎𝒆𝒏𝒕𝑳𝒊𝒔𝒕 ------------------------------------
class PN_ElementList(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ElementList', p)
class PN_ElementList_Elision_AssignmentExpression(PN_ElementList):
    @property
    def Elision(self):
        return self.children[0]
    @property
    def AssignmentExpression(self):
        return self.children[1]
    def ArrayAccumulation(self, array, nextIndex):
        # 12.2.5.2 Runtime Semantics: ArrayAccumulation
        #   With parameters array and nextIndex.
        # ElementList : Elision AssignmentExpression
        # ElementList : AssignmentExpression
        #   1. Let padding be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
        #   2. Let initResult be the result of evaluating AssignmentExpression.
        #   3. Let initValue be ? GetValue(initResult).
        #   4. Let created be CreateDataProperty(array, ToString(ToUint32(nextIndex+padding)), initValue).
        #   5. Assert: created is true.
        #   6. Return nextIndex+padding+1.
        padding = self.Elision.ElisionWidth() if self.Elision else 0
        initResult = self.AssignmentExpression.evaluate()
        initValue = GetValue(initResult)
        created = CreateDataProperty(array, ToString(ToUint32(nextIndex + padding)), initValue)
        assert created
        return nextIndex + padding + 1
class PN_ElementList_AssignmentExpression(PN_ElementList_Elision_AssignmentExpression):
    @property
    def AssignmentExpression(self):
        return self.children[0]
    @property
    def Elision(self):
        return None
class PN_ElementList_Elision_SpreadElement(PN_ElementList):
    @property
    def Elision(self):
        return self.children[0]
    @property
    def SpreadElement(self):
        return self.children[1]
    def ArrayAccumulation(self, array, nextIndex):
        # 12.2.5.2 Runtime Semantics: ArrayAccumulation
        #   With parameters array and nextIndex.
        # ElementList : Elision SpreadElement
        # ElementList : SpreadElement
        #   1. Let padding be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
        #   2. Return the result of performing ArrayAccumulation for SpreadElement with arguments array and nextIndex+padding.
        padding = self.Elision.ElisionWidth() if self.Elision else 0
        return self.SpreadElement.ArrayAccumulation(array, nextIndex + padding)
class PN_ElementList_SpreadElement(PN_ElementList_Elision_SpreadElement):
    @property
    def SpreadElement(self):
        return self.children[0]
    @property
    def Elision(self):
        return None
class PN_ElementList_ElementList_COMMA_Elision_AssignmentExpression(PN_ElementList):
    @property
    def ElementList(self):
        return self.children[0]
    @property
    def Elision(self):
        return self.children[2]
    @property
    def AssignmentExpression(self):
        return self.children[3]
    def ArrayAccumulation(self, array, nextIndex):
        # 12.2.5.2 Runtime Semantics: ArrayAccumulation
        #   With parameters array and nextIndex.
        # ElementList : ElementList , Elision AssignmentExpression
        # ElementList : ElementList , AssignmentExpression
        #   1. Let postIndex be the result of performing ArrayAccumulation for ElementList with arguments array and nextIndex.
        #   2. ReturnIfAbrupt(postIndex).
        #   3. Let padding be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
        #   4. Let initResult be the result of evaluating AssignmentExpression.
        #   5. Let initValue be ? GetValue(initResult).
        #   6. Let created be CreateDataProperty(array, ToString(ToUint32(postIndex+padding)), initValue).
        #   7. Assert: created is true.
        #   8. Return postIndex+padding+1.
        postIndex = self.ElementList.ArrayAccumulation(array, nextIndex)
        padding = self.Elision.ElisionWidth() if self.Elision else 0
        initResult = self.AssignmentExpression.evaluate()
        initValue = GetValue(initResult)
        created = CreateDataProperty(array, ToString(ToUint32(postIndex + padding)), initValue)
        assert created
        return postIndex + padding + 1
class PN_ElementList_ElementList_COMMA_AssignmentExpression(PN_ElementList_ElementList_COMMA_Elision_AssignmentExpression):
    @property
    def ElementList(self):
        return self.children[0]
    @property
    def AssignmentExpression(self):
        return self.children[2]
    @property
    def Elision(self):
        return None
class PN_ElementList_ElementList_COMMA_Elision_SpreadElement(PN_ElementList):
    @property
    def ElementList(self):
        return self.children[0]
    @property
    def Elision(self):
        return self.children[2]
    @property
    def SpreadElement(self):
        return self.children[3]
    def ArrayAccumulation(self, array, nextIndex):
        # 12.2.5.2 Runtime Semantics: ArrayAccumulation
        #   With parameters array and nextIndex.
        # ElementList : ElementList , Elision SpreadElement
        # ElementList : ElementList , SpreadElement
        #   1. Let postIndex be the result of performing ArrayAccumulation for ElementList with arguments array and nextIndex.
        #   2. ReturnIfAbrupt(postIndex).
        #   3. Let padding be the ElisionWidth of Elision; if Elision is not present, use the numeric value zero.
        #   4. Return the result of performing ArrayAccumulation for SpreadElement with arguments array and postIndex+padding.
        postIndex = self.ElementList.ArrayAccumulation(array, nextIndex)
        padding = self.Elision.ElisionWidth() if self.Elision else 0
        return self.SpreadElement.ArrayAccumulation(array, postIndex + padding)
class PN_ElementList_ElementList_COMMA_SpreadElement(PN_ElementList_ElementList_COMMA_Elision_SpreadElement):
    @property
    def ElementList(self):
        return self.children[0]
    @property
    def SpreadElement(self):
        return self.children[2]
    @property
    def Elision(self):
        return None
# ------------------------------------ 𝑬𝒍𝒊𝒔𝒊𝒐𝒏 ------------------------------------
class PN_Elision(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('Elision', p)
class PN_Elision_COMMA(PN_Elision):
    def ElisionWidth(self):
        # 12.2.5.1 Static Semantics: ElisionWidth
        #           Elision : ,
        # 1. Return the numeric value 1.
        return 1
    def IteratorDestructuringAssignmentEvaluation(self, iteratorRecord):
        # 12.15.5.5 Runtime Semantics: IteratorDestructuringAssignmentEvaluation
        #   With parameter iteratorRecord.
        # Elision : ,
        #   1. If iteratorRecord.[[Done]] is false, then
        #       a. Let next be IteratorStep(iteratorRecord).
        #       b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #       c. ReturnIfAbrupt(next).
        #       d. If next is false, set iteratorRecord.[[Done]] to true.
        #   2. Return NormalCompletion(empty).
        if not iteratorRecord.Done:
            try:
                nextVal = IteratorStep(iteratorRecord)
            except (ESError, ESAbrupt):
                iteratorRecord.Done = True
                raise
            if not nextVal:
                iteratorRecord.Done = True
        return EMPTY
class PN_Elision_Elision_COMMA(PN_Elision):
    @property
    def Elision(self):
        return self.children[0]
    def ElisionWidth(self):
        # 12.2.5.1 Static Semantics: ElisionWidth
        #           Elision : Elision ,
        # 1. Let preceding be the ElisionWidth of Elision.
        # 2. Return preceding+1.
        return 1 + self.Elision.ElisionWidth()
    def IteratorDestructuringAssignmentEvaluation(self, iteratorRecord):
        # 12.15.5.5 Runtime Semantics: IteratorDestructuringAssignmentEvaluation
        #   With parameter iteratorRecord.
        # Elision : Elision ,
        #   1. Perform ? IteratorDestructuringAssignmentEvaluation of Elision with iteratorRecord as the argument.
        #   2. If iteratorRecord.[[Done]] is false, then
        #       a. Let next be IteratorStep(iteratorRecord).
        #       b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #       c. ReturnIfAbrupt(next).
        #       d. If next is false, set iteratorRecord.[[Done]] to true.
        #   3. Return NormalCompletion(empty).
        self.Elision.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
        if not iteratorRecord.Done:
            try:
                nextVal = IteratorStep(iteratorRecord)
            except (ESError, ESAbrupt):
                iteratorRecord.Done = True
                raise
            if not nextVal:
                iteratorRecord.Done = True
        return EMPTY
# ------------------------------------ 𝑺𝒑𝒓𝒆𝒂𝒅𝑬𝒍𝒆𝒎𝒆𝒏𝒕 ------------------------------------
class PN_SpreadElement(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('SpreadElement', p)
class PN_SpreadElement_DOTDOTDOT_AssignmentExpression(PN_SpreadElement):
    @property
    def AssignmentExpression(self):
        return self.children[1]
    def ArrayAccumulation(self, array, nextIndex):
        # 12.2.5.2 Runtime Semantics: ArrayAccumulation
        #   With parameters array and nextIndex.
        # SpreadElement : ... AssignmentExpression
        #   1. Let spreadRef be the result of evaluating AssignmentExpression.
        #   2. Let spreadObj be ? GetValue(spreadRef).
        #   3. Let iteratorRecord be ? GetIterator(spreadObj).
        #   4. Repeat,
        #       a. Let next be ? IteratorStep(iteratorRecord).
        #       b. If next is false, return nextIndex.
        #       c. Let nextValue be ? IteratorValue(next).
        #       d. Let status be CreateDataProperty(array, ToString(ToUint32(nextIndex)), nextValue).
        #       e. Assert: status is true.
        #       f. Let nextIndex be nextIndex + 1.
        spreadRef = self.AssignmentExpression.evaluate()
        spreadObj = GetValue(spreadRef)
        iteratorRecord = GetIterator(spreadObj)
        while 1:
            next = IteratorStep(iteratorRecord)
            if not next:
                return nextIndex
            nextValue = IteratorValue(next)
            status = CreateDataProperty(array, ToString(ToUint32(nextIndex)), nextValue)
            assert status
            nextIndex += 1

# ------------------------------------ 𝟏𝟐.𝟐.𝟔 𝑶𝒃𝒋𝒆𝒄𝒕 𝑰𝒏𝒊𝒕𝒊𝒂𝒍𝒊𝒛𝒆𝒓 ------------------------------------
# NOTE 1
# An object initializer is an expression describing the initialization of an Object, written in a form resembling a
# literal. It is a list of zero or more pairs of property keys and associated values, enclosed in curly brackets. The
# values need not be literals; they are evaluated each time the object initializer is evaluated.
# ------------------------------------ 𝑶𝒃𝒋𝒆𝒄𝒕𝑳𝒊𝒕𝒆𝒓𝒂𝒍 ------------------------------------
class PN_ObjectLiteral(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ObjectLiteral', p)
class PN_ObjectLiteral_LCURLY_RCURLY(PN_ObjectLiteral):
    def evaluate(self):
        # 12.2.6.7 Runtime Semantics: Evaluation
        # ObjectLiteral : { }
        #   1. Return ObjectCreate(%ObjectPrototype%).
        return ObjectCreate(surrounding_agent.running_ec.realm.intrinsics['%ObjectPrototype%'])
class PN_ObjectLiteral_LCURLY_PropertyDefinitionList_RCURLY(PN_ObjectLiteral):
    @property
    def PropertyDefinitionList(self):
        return self.children[1]
    def evaluate(self):
        # 12.2.6.7 Runtime Semantics: Evaluation
        # ObjectLiteral : { PropertyDefinitionList }
        # ObjectLiteral : { PropertyDefinitionList , }
        #   1. Let obj be ObjectCreate(%ObjectPrototype%).
        #   2. Perform ? PropertyDefinitionEvaluation of PropertyDefinitionList with arguments obj and true.
        #   3. Return obj.
        obj = ObjectCreate(surrounding_agent.running_ec.realm.intrinsics['%ObjectPrototype%'])
        self.PropertyDefinitionList.PropertyDefinitionEvaluation(obj, True)
        return obj
class PN_ObjectLiteral_LCURLY_PropertyDefinitionList_COMMA_RCURLY(PN_ObjectLiteral_LCURLY_PropertyDefinitionList_RCURLY):
    pass
# ------------------------------------ 𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚𝑫𝒆𝒇𝒊𝒏𝒊𝒕𝒊𝒐𝒏𝑳𝒊𝒔𝒕 ------------------------------------
class PN_PropertyDefinitionList(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('PropertyDefinitionList', p)
class PN_PropertyDefinitionList_PropertyDefinition(PN_PropertyDefinitionList):
    @property
    def PropertyDefinition(self):
        return self.children[0]
    def PropertyNameList(self):
        # 12.2.6.6 Static Semantics: PropertyNameList
        # PropertyDefinitionList : PropertyDefinition
        #   1. If PropName of PropertyDefinition is empty, return a new empty List.
        #   2. Return a new List containing PropName of PropertyDefinition.
        pn = self.PropertyDefinition.PropName()
        return [pn] if pn != EMPTY else []
class PN_PropertyDefinitionList_PropertyDefinitionList_COMMA_PropertyDefinition(PN_PropertyDefinitionList):
    @property
    def PropertyDefinitionList(self):
        return self.children[0]
    @property
    def PropertyDefinition(self):
        return self.children[2]
    def PropertyNameList(self):
        # 12.2.6.6 Static Semantics: PropertyNameList
        # PropertyDefinitionList : PropertyDefinitionList , PropertyDefinition
        #   1. Let list be PropertyNameList of PropertyDefinitionList.
        #   2. If PropName of PropertyDefinition is empty, return list.
        #   3. Append PropName of PropertyDefinition to the end of list.
        #   4. Return list.
        lst = self.PropertyDefinitionList.PropertyNameList()
        pn = self.PropertyDefinition.PropName()
        if pn != EMPTY:
            lst.append(pn)
        return lst
    def PropertyDefinitionEvaluation(self, object, enumerable):
        # 12.2.6.8 Runtime Semantics: PropertyDefinitionEvaluation
        #   With parameters object and enumerable.
        # PropertyDefinitionList : PropertyDefinitionList , PropertyDefinition
        #   1. Perform ? PropertyDefinitionEvaluation of PropertyDefinitionList with arguments object and enumerable.
        #   2. Return the result of performing PropertyDefinitionEvaluation of PropertyDefinition with arguments object
        #      and enumerable.
        self.PropertyDefinitionList.PropertyDefinitionEvaluation(object, enumerable)
        return self.PropertyDefinition.PropertyDefinitionEvaluation(object, enumerable)
# ------------------------------------ 𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚𝑫𝒆𝒇𝒊𝒏𝒊𝒕𝒊𝒐𝒏 ------------------------------------
class PN_PropertyDefinition(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('PropertyDefinition', p)
class PN_PropertyDefinition_IdentifierReference(PN_PropertyDefinition):
    @property
    def IdentifierReference(self):
        return self.children[0]
    def PropName(self):
        # 12.2.6.5 Static Semantics: PropName
        # PropertyDefinition : IdentifierReference
        #   1. Return StringValue of IdentifierReference.
        return self.IdentifierReference.StringValue()
    def PropertyDefinitionEvaluation(self, object, enumerable):
        # 12.2.6.8 Runtime Semantics: PropertyDefinitionEvaluation
        #   With parameters object and enumerable.
        # PropertyDefinition : IdentifierReference
        #   1. Let propName be StringValue of IdentifierReference.
        #   2. Let exprValue be the result of evaluating IdentifierReference.
        #   3. Let propValue be ? GetValue(exprValue).
        #   4. Assert: enumerable is true.
        #   5. Return CreateDataPropertyOrThrow(object, propName, propValue).
        propName = self.IdentifierReference.StringValue()
        exprValue = self.IdentifierReference.evaluate()
        propValue = GetValue(exprValue)
        assert enumerable
        return CreateDataPropertyOrThrow(object, propName, propValue)
class PN_PropertyDefinition_CoverInitializedName(PN_PropertyDefinition):
    @property
    def CoverInitializedName(self):
        return self.children[0]
    def EarlyErrors(self):
        # 12.2.6.1 Static Semantics: Early Errors
        #
        # In addition to describing an actual object initializer the ObjectLiteral productions are also used as a cover
        # grammar for ObjectAssignmentPattern and may be recognized as part of a
        # CoverParenthesizedExpressionAndArrowParameterList. When ObjectLiteral appears in a context where
        # ObjectAssignmentPattern is required the following Early Error rules are not applied. In addition, they are
        # not applied when initially parsing a CoverParenthesizedExpressionAndArrowParameterList or
        # CoverCallExpressionAndAsyncArrowHead.
        #
        # PropertyDefinition : CoverInitializedName
        # * Always throw a Syntax Error if code matches this production.
        #
        # NOTE
        # This production exists so that ObjectLiteral can serve as a cover grammar for ObjectAssignmentPattern. It
        # cannot occur in an actual object initializer.
        return [CreateSyntaxError('Production not allowed in object initializers')]
class PN_PropertyDefinition_PropertyName_COLON_AssignmentExpression(PN_PropertyDefinition):
    @property
    def PropertyName(self):
        return self.children[0]
    @property
    def AssignmentExpression(self):
        return self.children[2]
    def PropName(self):
        # 12.2.6.5 Static Semantics: PropName
        # PropertyDefinition : PropertyName : AssignmentExpression
        #   1. Return PropName of PropertyName.
        return self.PropertyName.PropName()
    def PropertyDefinitionEvaluation(self, object, enumerable):
        # 12.2.6.8 Runtime Semantics: PropertyDefinitionEvaluation
        #   With parameters object and enumerable.
        # PropertyDefinition : PropertyName : AssignmentExpression
        #   1. Let propKey be the result of evaluating PropertyName.
        #   2. ReturnIfAbrupt(propKey).
        #   3. Let exprValueRef be the result of evaluating AssignmentExpression.
        #   4. Let propValue be ? GetValue(exprValueRef).
        #   5. If IsAnonymousFunctionDefinition(AssignmentExpression) is true, then
        #       a. Let hasNameProperty be ? HasOwnProperty(propValue, "name").
        #       b. If hasNameProperty is false, perform SetFunctionName(propValue, propKey).
        #   6. Assert: enumerable is true.
        #   7. Return CreateDataPropertyOrThrow(object, propKey, propValue).
        propKey = self.PropertyName.evaluate()
        exprValueRef = self.AssignmentExpression.evaluate()
        propValue = GetValue(exprValueRef)
        if IsAnonymousFunctionDefinition(self.AssignmentExpression):
            if not HasOwnProperty(propValue, 'name'):
                SetFunctionName(propValue, propKey)
        assert enumerable
        return CreateDataPropertyOrThrow(object, propKey, propValue)
class PN_PropertyDefinition_MethodDefinition(PN_PropertyDefinition):
    @property
    def MethodDefinition(self):
        return self.children[0]
    def EarlyErrors(self):
        # 12.2.6.1 Static Semantics: Early Errors
        # PropertyDefinition : MethodDefinition
        # * It is a Syntax Error if HasDirectSuper of MethodDefinition is true.
        if self.MethodDefinition.HasDirectSuper():
            return [CreateSyntaxError('super not allowed in object literal creation')]
    def Contains(self, symbol):
        # 12.2.6.3 Static Semantics: Contains
        #   With parameter symbol.
        # PropertyDefinition : MethodDefinition
        #   1. If symbol is MethodDefinition, return true.
        #   2. Return the result of ComputedPropertyContains for MethodDefinition with argument symbol.
        return symbol == 'MethodDefinition' or self.MethodDefinition.ComputedPropertyContains(symbol)
        # NOTE
        # Static semantic rules that depend upon substructure generally do not look into function definitions.
class PN_PropertyDefinition_DOTDOTDOT_AssignmentExpression(PN_PropertyDefinition):
    @property
    def AssignmentExpression(self):
        return self.children[1]
    def PropName(self):
        # 12.2.6.5 Static Semantics: PropName
        # PropertyDefinition : ... AssignmentExpression
        #   1. Return empty.
        return EMPTY
    def PropertyDefinitionEvaluation(self, object, enumerable):
        # 12.2.6.8 Runtime Semantics: PropertyDefinitionEvaluation
        #   With parameters object and enumerable.
        # PropertyDefinition : ... AssignmentExpression
        #   1. Let exprValue be the result of evaluating AssignmentExpression.
        #   2. Let fromValue be ? GetValue(exprValue).
        #   3. Let excludedNames be a new empty List.
        #   4. Return ? CopyDataProperties(object, fromValue, excludedNames).
        exprValue = self.AssignmentExpression.evaluate()
        fromValue = GetValue(exprValue)
        return CopyDataProperties(object, fromValue, [])
# ------------------------------------ 𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚𝑵𝒂𝒎𝒆 ------------------------------------
class PN_PropertyName(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('PropertyName', p)
class PN_PropertyName_LiteralPropertyName(PN_PropertyName):
    @property
    def LiteralPropertyName(self):
        return self.children[0]
    def ComputedPropertyContains(self, symbol):
        # 12.2.6.2 Static Semantics: ComputedPropertyContains
        #   With parameter symbol.
        #           PropertyName : LiteralPropertyName
        # 1. Return false.
        return False
    def IsComputedPropertyName(self):
        # 12.2.6.4 Static Semantics: IsComputedPropertyKey
        #           PropertyName : LiteralPropertyName
        # 1. Return false.
        return False
class PN_PropertyName_ComputedPropertyName(PN_PropertyName):
    @property
    def ComputedPropertyName(self):
        return self.children[0]
    def ComputedPropertyContains(self, symbol):
        # 12.2.6.2 Static Semantics: ComputedPropertyContains
        #   With parameter symbol.
        #           PropertyName : ComputedPropertyName
        # 1. Return the result of ComputedPropertyName Contains symbol.
        return self.ComputedPropertyName.Contains(symbol)
    def IsComputedPropertyName(self):
        # 12.2.6.4 Static Semantics: IsComputedPropertyKey
        #           PropertyName : ComputedPropertyName
        # 1. Return true.
        return True
# ------------------------------------ 𝑳𝒊𝒕𝒆𝒓𝒂𝒍𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚𝑵𝒂𝒎𝒆 ------------------------------------
class PN_LiteralPropertyName(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('LiteralProperty', p)
class PN_LiteralPropertyName_IdentifierName(PN_LiteralPropertyName):
    @property
    def IdentifierName(self):
        return self.children[0]
    def Contains(self, symbol):
        # 12.2.6.3 Static Semantics: Contains
        #   With parameter symbol.
        #           LiteralPropertyName : IdentifierName
        # 1. If symbol is a ReservedWord, return false.
        # 2. If symbol is an Identifier and StringValue of symbol is the same value as the StringValue of
        #    IdentifierName, return true.
        # 3. Return false.
        return symbol not in ReservedWords and symbol == self.IdentifierName.StringValue()
    def PropName(self):
        # 12.2.6.5 Static Semantics: PropName
        #           LiteralPropertyName : IdentifierName
        # 1. Return StringValue of IdentifierName.
        return self.IdentifierName.StringValue()
    def evaluate(self):
        return self.PropName()
class PN_LiteralPropertyName_StringLiteral(PN_LiteralPropertyName):
    @property
    def StringLiteral(self):
        return self.children[0]
    def PropName(self):
        # 12.2.6.5 Static Semantics: PropName
        #           LiteralPropertyName : StringLiteral
        # 1. Return the String value whose code units are the SV of the StringLiteral.
        return self.StringLiteral.value
    def evaluate(self):
        return self.PropName()
class PN_LiteralPropertyName_NumericLiteral(PN_LiteralPropertyName):
    @property
    def NumericLiteral(self):
        return self.children[0]
    def PropName(self):
        # 12.2.6.5 Static Semantics: PropName
        #           LiteralPropertyName : StringLiteral
        # 1. Let nbr be the result of forming the value of the NumericLiteral.
        # 2. Return ! ToString(nbr).
        return ToString(self.NumericLiteral.value)
    def evaluate(self):
        return self.PropName()
# ------------------------------------ 𝑪𝒐𝒎𝒑𝒖𝒕𝒆𝒅𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚𝑵𝒂𝒎𝒆 ------------------------------------
class PN_ComputedPropertyName(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ComputedPropertyName', p)
class PN_ComputedPropertyName_LBRACKET_AssignmentExpression_RBRACKET(PN_ComputedPropertyName):
    @property
    def AssignmentExpression(self):
        return self.children[1]
    def PropName(self):
        # 12.2.6.5 Static Semantics: PropName
        #           ComputedPropertyName : [ AssignmentExpression ]
        # 1. Return empty.
        return Empty.EMPTY
    def evaluate(self):
        # 12.2.6.7 Runtime Semantics: Evaluation
        #           ComputedPropertyName : [ AssignmentExpression ]
        # 1. Let exprValue be the result of evaluating AssignmentExpression.
        # 2. Let propName be ? GetValue(exprValue).
        # 3. Return ? ToPropertyKey(propName).
        exprValue = self.AssignmentExpression.evaluate()
        propName = GetValue(exprValue)
        return ToPropertyKey(propName)
# ------------------------------------ 𝑪𝒐𝒗𝒆𝒓𝑰𝒏𝒊𝒕𝒊𝒂𝒍𝒊𝒛𝒆𝒅𝑵𝒂𝒎𝒆 ------------------------------------
class PN_CoverInitializedName(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('CoverInitializedName', p)
class PN_CoverInitializedName_IdentifierReference_Initialzier(PN_CoverInitializedName):
    @property
    def IdentiferReference(self):
        return self.children[0]
    @property
    def Initializer(self):
        return self.children[1]
# ------------------------------------ 𝑰𝒏𝒊𝒕𝒊𝒂𝒍𝒊𝒛𝒆𝒓 ------------------------------------
class PN_Initializer(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('Initializer', p)
class PN_Initializer_EQUALS_AssignmentExpression(PN_Initializer):
    pass

################################################################################################################################################################################################
#
#  d888    .d8888b.       .d8888b.      888                .d888 888           888    888                        888         .d8888b.  d8b      888
# d8888   d88P  Y88b     d88P  Y88b     888               d88P"  888           888    888                        888        d88P  Y88b Y8P      888
#   888          888          .d88P     888               888    888           888    888                        888        Y88b.               888
#   888        .d88P         8888"      888       .d88b.  888888 888888        8888888888  8888b.  88888b.   .d88888         "Y888b.   888  .d88888  .d88b.
#   888    .od888P"           "Y8b.     888      d8P  Y8b 888    888           888    888     "88b 888 "88b d88" 888            "Y88b. 888 d88" 888 d8P  Y8b
#   888   d88P"          888    888     888      88888888 888    888    888888 888    888 .d888888 888  888 888  888 888888       "888 888 888  888 88888888
#   888   888"       d8b Y88b  d88P     888      Y8b.     888    Y88b.         888    888 888  888 888  888 Y88b 888        Y88b  d88P 888 Y88b 888 Y8b.
# 8888888 888888888  Y8P  "Y8888P"      88888888  "Y8888  888     "Y888        888    888 "Y888888 888  888  "Y88888         "Y8888P"  888  "Y88888  "Y8888
#
# 8888888888                                                      d8b
# 888                                                             Y8P
# 888
# 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.  .d8888b
# 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b 88K
# 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888 "Y8888b.
# 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888      X88
# 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888  88888P'
#                     888
#                     888
#                     888
#
################################################################################################################################################################################################
# = - = - = - = - = - = - = - = - = MemberExpression - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = -
class PN_MemberExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('MemberExpression', p)
        self.strict = ctx.strict
class PN_MemberExpression_PrimaryExpression(PN_MemberExpression):
    pass
class PN_MemberExpression_NotPassThru(PN_MemberExpression):
    def IsFunctionDefinition(self):
        # 12.3.1.3 Static Semantics: IsFunctionDefinition
        return False
    def IsDestructuring(self):
        # 12.3.1.4 Static Semantics: IsDestructuring
        return False
    def IdentifierRef(self):
        # 12.3.1.5 Static Semantics: IsIdentifierRef
        return False
class PN_MemberExpression_NEW_MemberExpression_Arguments(PN_MemberExpression_NotPassThru):
    @property
    def MemberExpression(self):
        return self.children[1]
    @property
    def Arguments(self):
        return self.children[2]
    def IsValidSimpleAssignmentTarget(self):
        # 12.3.1.6 Static Semantics: IsValidSimpleAssignmentTarget
        return False
    def evaluate(self):
        # 12.3.3.1 Runtime Semantics: Evaluation
        #           MemberExpression: new MemberExpression Arguments
        # 1. Return ? EvaluateNew(MemberExpression, Arguments).
        return EvaluateNew(self.MemberExpression, self.Arguments)
class PN_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET(PN_MemberExpression_NotPassThru):
    @property
    def MemberExpression(self):
        return self.children[0]
    @property
    def Expression(self):
        return self.children[2]
    def IsValidSimpleAssignmentTarget(self):
        return True
    def evaluate(self):
        # 12.3.2.1 Runtime Semantics: Evaluation
        #           MemberExpression : MemberExpression [ Expression ]
        # 1. Let baseReference be the result of evaluating MemberExpression.
        # 2. Let baseValue be ? GetValue(baseReference).
        # 3. Let propertyNameReference be the result of evaluating Expression.
        # 4. Let propertyNameValue be ? GetValue(propertyNameReference).
        # 5. Let bv be ? RequireObjectCoercible(baseValue).
        # 6. Let propertyKey be ? ToPropertyKey(propertyNameValue).
        # 7. If the code matched by this MemberExpression is strict mode code, let strict be true, else let strict be false.
        # 8. Return a value of type Reference whose base value component is bv, whose referenced name component is propertyKey, and whose strict reference flag is strict.
        baseReference = self.MemberExpression.evaluate()
        baseValue = GetValue(baseReference)
        propertyNameReference = self.Expression.evaluate()
        propertyNameValue = GetValue(propertyNameReference)
        bv = RequireObjectCoercible(baseValue)
        propertyKey = ToPropertyKey(propertyNameValue)
        return Reference(bv, propertyKey, self.strict)
class PN_MemberExpression_MemberExpression_DOT_IdentifierName(PN_MemberExpression_NotPassThru):
    @property
    def MemberExpression(self):
        return self.children[0]
    @property
    def IdentifierName(self):
        return self.children[2]
    def Contains(self, symbol):
        # 12.3.1.2 Static Semantics: Contains
        # With parameter symbol.
        #           MemberExpression : MemberExpression . IdentifierName
        # 1. If MemberExpression Contains symbol is true, return true.
        # 2. If symbol is a ReservedWord, return false.
        # 3. If symbol is an Identifier and StringValue of symbol is the same value as the StringValue of IdentifierName, return true.
        # 4. Return false.
        if self.MemberExpression.Contains(symbol):
            return True
        if symbol in ReservedWords:
            return False
        return self.IdentifierName.StringValue() == symbol
    def IsValidSimpleAssignmentTarget(self):
        return True
    def evaluate(self):
        # 12.3.2.1 Runtime Semantics: Evaluation
        #           MemberExpression : MemberExpression . IdentifierName
        # 1. Let baseReference be the result of evaluating MemberExpression.
        # 2. Let baseValue be ? GetValue(baseReference).
        # 3. Let bv be ? RequireObjectCoercible(baseValue).
        # 4. Let propertyNameString be StringValue of IdentifierName.
        # 5. If the code matched by this MemberExpression is strict mode code, let strict be true, else let strict be false.
        # 6. Return a value of type Reference whose base value component is bv, whose referenced name component is propertyNameString, and whose strict reference flag is strict.
        baseReference = self.MemberExpression.evaluate()
        baseValue = GetValue(baseReference)
        bv = RequireObjectCoercible(baseValue)
        propertyNameString = self.IdentifierName.StringValue()
        return Reference(bv, propertyNameString, self.strict)
# = - = - = - = - = - = - = - = - = NewExpression - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = -
class PN_NewExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('NewExpression', p)
class PN_NewExpression_MemberExpression(PN_NewExpression):
    pass
class PN_NewExpression_NEW_NewExpression(PN_NewExpression):
    # 12.3.3 The new Operator
    def IsFunctionDefinition(self):
        # 12.3.1.3 Static Semantics: IsFunctionDefinition
        return False
    def IsDestructuring(self):
        # 12.3.1.4 Static Semantics: IsDestructuring
        return False
    def IdentifierRef(self):
        # 12.3.1.5 Static Semantics: IsIdentifierRef
        return False
    def IsValidSimpleAssignmentTarget(self):
        # 12.3.1.6 Static Semantics: IsValidSimpleAssignmentTarget
        return False
    def evaluate(self):
        # 12.3.3.1 Runtime Semantics: Evaluation
        #           NewExpression : new NewExpression
        # 1. Return ? EvaluateNew(NewExpression, empty).
        NewExpression = self.children[1]
        return EvaluateNew(NewExpression, Empty.EMPTY)
# 12.3.3.1.1 Runtime Semantics: EvaluateNew ( constructExpr, arguments )
def EvaluateNew(constructExpr, arguments):
    # The abstract operation EvaluateNew with arguments constructExpr, and arguments performs the following steps:
    #
    # 1. Assert: constructExpr is either a NewExpression or a MemberExpression.
    # 2. Assert: arguments is either empty or an Arguments.
    # 3. Let ref be the result of evaluating constructExpr.
    # 4. Let constructor be ? GetValue(ref).
    # 5. If arguments is empty, let argList be a new empty List.
    # 6. Else,
    #    a. Let argList be ArgumentListEvaluation of arguments.
    #    b. ReturnIfAbrupt(argList).
    # 7. If IsConstructor(constructor) is false, throw a TypeError exception.
    # 8. Return ? Construct(constructor, argList).
    assert constructExpr.name in ['NewExpression', 'MemberExpression']
    assert arguments == Empty.EMPTY or arguments.name == 'Arguments'
    ref = constructExpr.evaluate()
    constructor = GetValue(ref)
    if arguments == Empty.EMPTY:
        argList = []
    else:
        argList = arguments.ArgumentListEvaluation()
    if not IsConstructor(constructor):
        raise ESTypeError(f'{GetReferencedName(ref)} is not a constructor')
    return Construct(constructor, argList)
# = - = - = - = - = - = - = - = - = CallExpression - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = -
# 12.3.4 Function Calls
class PN_CallExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('CallExpression', p)
class PN_CallExpression_CallExpression_Arguments(PN_CallExpression):
    @property
    def CallExpression(self):
        return self.children[0]
    @property
    def Arguments(self):
        return self.children[1]
    def IsValidSimpleAssignmentTarget(self):
        # 12.3.1.6 Static Semantics: IsValidSimpleAssignmentTarget
        #           CallExpression : CallExpression Arguments
        # 1. Return false.
        return False
    def evaluate(self):
        # 12.3.4.1 Runtime Semantics: Evaluation
        #           CallExpression : CallExpression Arguments
        # 1. Let ref be the result of evaluating CallExpression.
        # 2. Let func be ? GetValue(ref).
        # 3. Let thisCall be this CallExpression.
        # 4. Let tailCall be IsInTailPosition(thisCall).
        # 5. Return ? EvaluateCall(func, ref, Arguments, tailCall).
        CallExpression = self.CallExpression
        Arguments = self.Arguments
        ref = CallExpression.evaluate()
        func = GetValue(ref)
        thisCall = CallExpression
        tailCall = IsInTailPosition(thisCall)
        return EvaluateCall(func, ref, Arguments, tailCall)
class PN_CallExpression_CoverCallExpressionAndAsyncArrowHead(PN_CallExpression):
    def __init__(self, context, p):
        super().__init__(context, p)
        start, end = self.source_range()
        source = context.source_text[start:end]
        subparser = Ecma262Parser(start='CallMemberExpression', source_text=source)
        sublexer = Lexer(source, 'CallMemberExpression')
        tree = subparser.parse(sublexer.lex())
        self.covered_production = tree
        self.strict = context.strict
    @property
    def CoverCallExpressionAndAsyncArrowHead(self):
        return self.children[0]
    def CoveredCallExpression(self):
        # 12.3.1.1 Static Semantics: CoveredCallExpression
        return self.covered_production
    def IsValidSimpleAssignmentTarget(self):
        # 12.3.1.6 Static Semantics: IsValidSimpleAssignmentTarget
        #           CallExpression : CoverCallExpressionAndAsyncArrowHead
        # 1. Return false.
        return False
    def evaluate(self):
        # 12.3.4.1 Runtime Semantics: Evaluation
        #           CallExpression : CoverCallExpressionAndAsyncArrowHead
        # 1. Let expr be CoveredCallExpression of CoverCallExpressionAndAsyncArrowHead.
        # 2. Let memberExpr be the MemberExpression of expr.
        # 3. Let arguments be the Arguments of expr.
        # 4. Let ref be the result of evaluating memberExpr.
        # 5. Let func be ? GetValue(ref).
        # 6. If Type(ref) is Reference and IsPropertyReference(ref) is false and GetReferencedName(ref) is "eval", then
        #    a. If SameValue(func, %eval%) is true, then
        #       i. Let argList be ? ArgumentListEvaluation of arguments.
        #       ii. If argList has no elements, return undefined.
        #       iii. Let evalText be the first element of argList.
        #       iv. If the source code matching this CallExpression is strict mode code, let strictCaller be true. Otherwise let strictCaller be false.
        #       v. Let evalRealm be the current Realm Record.
        #       vi. Perform ? HostEnsureCanCompileStrings(evalRealm, evalRealm).
        #       vii. Return ? PerformEval(evalText, evalRealm, strictCaller, true).
        # 7. Let thisCall be this CallExpression.
        # 8. Let tailCall be IsInTailPosition(thisCall).
        # 9. Return ? EvaluateCall(func, ref, arguments, tailCall).
        # A CallExpression evaluation that executes step 6.a.vii is a direct eval.
        expr = self.CoveredCallExpression()
        memberExpr = expr.MemberExpression
        arguments = expr.Arguments
        ref = memberExpr.evaluate()
        func = GetValue(ref)
        if isinstance(ref, Reference) and not IsPropertyReference(ref) and GetReferencedName(ref) == 'eval':
            if SameValue(func, surrounding_agent.running_ec.realm.intrinsics['%eval%']):
                argList = arguments.ArgumentListEvaluation()
                if len(argList) == 0:
                    return None
                evalText = argList[0]
                strictCaller = self.strict
                evalRealm = surrounding_agent.running_ec.realm
                HostEnsureCanCompileStrings(evalRealm, evalRealm)
                return PerformEval(evalText, evalRealm, strictCaller, True)
        #thisCall = self
        tailCall = False #IsInTailPosition(thisCall)
        return EvaluateCall(func, ref, arguments, tailCall)
class PN_CallMemberExpression_MemberExpression_Arguments(ParseNode):
    def __init__(self, context, p):
        super().__init__('CallMemberExpression', p)
    @property
    def MemberExpression(self):
        return self.children[0]
    @property
    def Arguments(self):
        return self.children[1]

# 12.3.4.2 Runtime Semantics: EvaluateCall ( func, ref, arguments, tailPosition )
def EvaluateCall(func, ref, arguments, tailPosition):
    # The abstract operation EvaluateCall takes as arguments a value func, a value ref, a Parse Node arguments, and a
    # Boolean argument tailPosition. It performs the following steps:
    #
    # 1. If Type(ref) is Reference, then
    #    a. If IsPropertyReference(ref) is true, then
    #       i. Let thisValue be GetThisValue(ref).
    #    b. Else the base of ref is an Environment Record,
    #       i. Let refEnv be GetBase(ref).
    #       ii. Let thisValue be refEnv.WithBaseObject().
    # 2. Else Type(ref) is not Reference,
    #    a. Let thisValue be undefined.
    # 3. Let argList be ArgumentListEvaluation of arguments.
    # 4. ReturnIfAbrupt(argList).
    # 5. If Type(func) is not Object, throw a TypeError exception.
    # 6. If IsCallable(func) is false, throw a TypeError exception.
    # 7. If tailPosition is true, perform PrepareForTailCall().
    # 8. Let result be Call(func, thisValue, argList).
    # 9. Assert: If tailPosition is true, the above call will not return here, but instead evaluation will continue as
    #    if the following return has already occurred.
    # 10. Assert: If result is not an abrupt completion, then Type(result) is an ECMAScript language type.
    # 11. Return result.
    if isinstance(ref, Reference):
        if IsPropertyReference(ref):
            thisValue = GetThisValue(ref)
        else:
            refEnv = GetBase(ref)
            thisValue = refEnv.WithBaseObject()
    else:
        thisValue = None
    argList = arguments.ArgumentListEvaluation()
    if not isObject(func):
        raise ESTypeError('Not a function')
    if not IsCallable(func):
        raise ESTypeError('Not a function')
    if tailPosition:
        PrepareForTailCall()
    result = Call(func, thisValue, argList)
    # assert not tailPosition
    # assert (isinstance(result, Completion) and (result.ctype != CompletionType.NORMAL or isEcmaValue(result.value))) or isEcmaValue(result)
    return result
# = - = - = - = - = - = - = - = - = ArgumentList - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = -
# 12.3.6 Argument Lists
# NOTE
# The evaluation of an argument list produces a List of values.
class PN_ArgumentList(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ArgumentList', p)
class PN_ArgumentList_AssignmentExpression(PN_ArgumentList):
    def ArgumentListEvaluation(self):
        # 12.3.6.1 Runtime Semantics: ArgumentListEvaluation
        #           ArgumentList : AssignmentExpression
        # 1. Let ref be the result of evaluating AssignmentExpression.
        # 2. Let arg be ? GetValue(ref).
        # 3. Return a List whose sole item is arg.
        AssignmentExpression = self.children[0]
        ref = AssignmentExpression.evaluate()
        arg = GetValue(ref)
        return [arg]
class PN_ArgumentList_DOTDOTDOT_AssignmentExpression(PN_ArgumentList):
    def ArgumentListEvaluation(self):
        # 12.3.6.1 Runtime Semantics: ArgumentListEvaluation
        #           ArgumentList : ... AssignmentExpression
        # 1. Let list be a new empty List.
        # 2. Let spreadRef be the result of evaluating AssignmentExpression.
        # 3. Let spreadObj be ? GetValue(spreadRef).
        # 4. Let iteratorRecord be ? GetIterator(spreadObj).
        # 5. Repeat,
        #    a. Let next be ? IteratorStep(iteratorRecord).
        #    b. If next is false, return list.
        #    c. Let nextArg be ? IteratorValue(next).
        #    d. Append nextArg as the last element of list.
        AssignmentExpression = self.children[1]
        lst = []
        spreadRef = AssignmentExpression.evaluate()
        spreadObj = GetValue(spreadRef)
        iteratorRecord = GetIterator(spreadObj)
        while 1:
            nxt = IteratorStep(iteratorRecord)
            if not nxt:
                return lst
            nextArg = IteratorValue(nxt)
            lst.append(nextArg)
class PN_ArgumentList_ArgumentList_COMMA_AssignmentExpression(PN_ArgumentList):
    def ArgumentListEvaluation(self):
        # 12.3.6.1 Runtime Semantics: ArgumentListEvaluation
        #           ArgumentList : ArgumentList , AssignmentExpression
        # 1. Let precedingArgs be ArgumentListEvaluation of ArgumentList.
        # 2. ReturnIfAbrupt(precedingArgs).
        # 3. Let ref be the result of evaluating AssignmentExpression.
        # 4. Let arg be ? GetValue(ref).
        # 5. Append arg to the end of precedingArgs.
        # 6. Return precedingArgs.
        ArgumentList = self.children[0]
        AssignmentExpression = self.children[2]
        precedingArgs = ArgumentList.ArgumentListEvaluation()
        ref = AssignmentExpression.evaluate()
        arg = GetValue(ref)
        precedingArgs.append(arg)
        return precedingArgs
class PN_ArgumentList_ArgumentList_COMMA_DOTDOTDOT_AssignmentExpression(PN_ArgumentList):
    def ArgumentListEvaluation(self):
        # 12.3.6.1 Runtime Semantics: ArgumentListEvaluation
        #           ArgumentList : ArgumentList , ... AssignmentExpression
        # 1. Let precedingArgs be ArgumentListEvaluation of ArgumentList.
        # 2. ReturnIfAbrupt(precedingArgs).
        # 3. Let spreadRef be the result of evaluating AssignmentExpression.
        # 4. Let iteratorRecord be ? GetIterator(? GetValue(spreadRef)).
        # 5. Repeat,
        #    a. Let next be ? IteratorStep(iteratorRecord).
        #    b. If next is false, return precedingArgs.
        #    c. Let nextArg be ? IteratorValue(next).
        #    d. Append nextArg as the last element of precedingArgs.
        ArgumentList = self.children[0]
        AssignmentExpression = self.children[3]
        precedingArgs = ArgumentList.ArgumentListEvaluation
        spreadRef = AssignmentExpression.evaluate()
        spreadObj = GetValue(spreadRef)
        iteratorRecord = GetIterator(spreadObj)
        while 1:
            nxt = IteratorStep(iteratorRecord)
            if not nxt:
                return precedingArgs
            nextArg = IteratorValue(nxt)
            precedingArgs.append(nextArg)
# = - = - = - = - = - = - = - = - = Arguments - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = -
class PN_Arguments(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('Arguments', p)
class PN_Arguments_LPAREN_RPAREN(PN_Arguments):
    def ArgumentListEvaluation(self):
        # 12.3.6.1 Runtime Semantics: ArgumentListEvaluation
        #           Arguments : ( )
        # 1. Return a new empty List.
        return []
    pass
class PN_Arguments_LPAREN_ArgumentList_RPAREN(PN_Arguments):
    pass
class PN_Arguments_LPAREN_ArgumentList_COMMA_RPAREN(PN_Arguments):
    pass
# = - = - = - = - = - = - = - = - = LeftHandSideExpression - = - = - = - = - = - = - = - = - = - = - = - = - = - = - = -
class PN_LeftHandSideExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('LeftHandSideExpression', p)
        self.ctx = ctx
class PN_LeftHandSideExpression_NewExpression(PN_LeftHandSideExpression):
    @property
    def NewExpression(self):
        return self.children[0]
class PN_LeftHandSideExpression_CallExpression(PN_LeftHandSideExpression):
    @property
    def CallExpression(self):
        return self.children[0]
    def IsFunctionDefinition(self):
        # 12.3.1.3 Static Semantics: IsFunctionDefinition
        return False
    def IsDestructuring(self):
        # 12.3.1.4 Static Semantics: IsDestructuring
        return False
    def IdentifierRef(self):
        # 12.3.1.5 Static Semantics: IsIdentifierRef
        return False
################################################################################################################################################################################################
#
#  d888    .d8888b.          d8888      888     888               888          888
# d8888   d88P  Y88b        d8P888      888     888               888          888
#   888          888       d8P 888      888     888               888          888
#   888        .d88P      d8P  888      888     888 88888b.   .d88888  8888b.  888888  .d88b.
#   888    .od888P"      d88   888      888     888 888 "88b d88" 888     "88b 888    d8P  Y8b
#   888   d88P"          8888888888     888     888 888  888 888  888 .d888888 888    88888888
#   888   888"       d8b       888      Y88b. .d88P 888 d88P Y88b 888 888  888 Y88b.  Y8b.
# 8888888 888888888  Y8P       888       "Y88888P"  88888P"   "Y88888 "Y888888  "Y888  "Y8888
#                                                   888
#                                                   888
#                                                   888
# 8888888888                                                      d8b
# 888                                                             Y8P
# 888
# 8888888    888  888 88888b.  888d888  .d88b.  .d8888b  .d8888b  888  .d88b.  88888b.  .d8888b
# 888        `Y8bd8P' 888 "88b 888P"   d8P  Y8b 88K      88K      888 d88""88b 888 "88b 88K
# 888          X88K   888  888 888     88888888 "Y8888b. "Y8888b. 888 888  888 888  888 "Y8888b.
# 888        .d8""8b. 888 d88P 888     Y8b.          X88      X88 888 Y88..88P 888  888      X88
# 8888888888 888  888 88888P"  888      "Y8888   88888P'  88888P' 888  "Y88P"  888  888  88888P'
#                     888
#                     888
#                     888
#
################################################################################################################################################################################################
class PN_UpdateExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('UpdateExpression', p)
class PN_UpdateExpression_LeftHandSideExpression(PN_UpdateExpression):
    pass
class PN_UpdateExpression_NotFallthru(PN_UpdateExpression):
    # 12.4.2 Static Semantics: IsFunctionDefinition
    def IsFunctionDefinition(self):
        return False
    # 12.4.3 Static Semantics: IsValidSimpleAssignmentTarget
    def IsValidSimpleAssignmentTarget(self):
        return False
class PN_UpdateExpression_prefix(PN_UpdateExpression_NotFallthru):
    # 12.4.1 Static Semantics: Early Errors
    def EarlyErrors(self):
        UnaryExpression = self.children[1]
        if not UnaryExpression.IsValidSimpleAssignmentTarget():
            return [CreateReferenceError('Invalid Reference for prefix update')]
        return []
class PN_UpdateExpression_postfix(PN_UpdateExpression_NotFallthru):
    # 12.4.1 Static Semantics: Early Errors
    def EarlyErrors(self):
        LeftHandSideExpression = self.children[0]
        return [CreateReferenceError('Invalid Reference for postfix update')] if not LeftHandSideExpression.IsValidSimpleAssignmentTarget() else []
class PN_UpdateExpression_LeftHandSideExpression_PLUSPLUS(PN_UpdateExpression_postfix):
    # 12.4.4 Postfix Increment Operator
    def evaluate(self):
        # 12.4.4.1 Runtime Semantics: Evaluation
        #           UpdateExpression : LeftHandSideExpression ++
        # 1. Let lhs be the result of evaluating LeftHandSideExpression.
        # 2. Let oldValue be ? ToNumber(? GetValue(lhs)).
        # 3. Let newValue be the result of adding the value 1 to oldValue, using the same rules as for the + operator (see 12.8.5).
        # 4. Perform ? PutValue(lhs, newValue).
        # 5. Return oldValue.
        LeftHandSideExpression = self.children[0]
        lhs = LeftHandSideExpression.evaluate()
        oldValue = GetValue(lhs)
        oldValue = ToNumber(oldValue)
        PutValue(lhs, oldValue + 1)
        return oldValue
class PN_UpdateExpression_LeftHandSideExpression_MINUSMINUS(PN_UpdateExpression_postfix):
    # 12.4.5 Postfix Decrement Operator
    def evaluate(self):
        # 12.4.5.1 Runtime Semantics: Evaluation
        #           UpdateExpression : LeftHandSideExpression --
        # 1. Let lhs be the result of evaluating LeftHandSideExpression.
        # 2. Let oldValue be ? ToNumber(? GetValue(lhs)).
        # 3. Let newValue be the result of subtracting the value 1 from oldValue, using the same rules as for the - operator (see 12.8.5).
        # 4. Perform ? PutValue(lhs, newValue).
        # 5. Return oldValue.
        LeftHandSideExpression = self.children[0]
        lhs = LeftHandSideExpression.evaluate()
        oldValue = GetValue(lhs)
        oldValue = ToNumber(oldValue)
        PutValue(lhs, oldValue - 1)
        return oldValue
class PN_UpdateExpression_PLUSPLUS_UnaryExpression(PN_UpdateExpression_prefix):
    # 12.4.6 Prefix Increment Operator
    def evaluate(self):
        # 12.4.6.1 Runtime Semantics: Evaluation
        #           UpdateExpression : ++ UnaryExpression
        # 1. Let expr be the result of evaluating UnaryExpression.
        # 2. Let oldValue be ? ToNumber(? GetValue(expr)).
        # 3. Let newValue be the result of adding the value 1 to oldValue, using the same rules as for the + operator (see 12.8.5).
        # 4. Perform ? PutValue(expr, newValue).
        # 5. Return newValue.
        UnaryExpression = self.children[1]
        expr = UnaryExpression.evaluate()
        oldValue = GetValue(expr)
        oldValue = ToNumber(oldValue)
        newValue = oldValue + 1
        PutValue(expr, newValue)
        return newValue
class PN_UpdateExpression_MINUSMINUS_UnaryExpression(PN_UpdateExpression_prefix):
    # 12.4.7 Prefix Decrement Operator
    def evaluate(self):
        # 12.4.7.1 Runtime Semantics: Evaluation
        #           UpdateExpression : -- UnaryExpression
        # 1. Let expr be the result of evaluating UnaryExpression.
        # 2. Let oldValue be ? ToNumber(? GetValue(expr)).
        # 3. Let newValue be the result of subtracting the value 1 from oldValue, using the same rules as for the - operator (see 12.8.5).
        # 4. Perform ? PutValue(expr, newValue).
        # 5. Return newValue.
        UnaryExpression = self.children[1]
        expr = UnaryExpression.evaluate()
        oldValue = GetValue(expr)
        oldValue = ToNumber(oldValue)
        newValue = oldValue - 1
        PutValue(expr, newValue)
        return newValue
################################################################################################################################################################################################
#
#  d888    .d8888b.      888888888      888     888
# d8888   d88P  Y88b     888            888     888
#   888          888     888            888     888
#   888        .d88P     8888888b.      888     888 88888b.   8888b.  888d888 888  888
#   888    .od888P"           "Y88b     888     888 888 "88b     "88b 888P"   888  888
#   888   d88P"                 888     888     888 888  888 .d888888 888     888  888
#   888   888"       d8b Y88b  d88P     Y88b. .d88P 888  888 888  888 888     Y88b 888
# 8888888 888888888  Y8P  "Y8888P"       "Y88888P"  888  888 "Y888888 888      "Y88888
#                                                                                  888
#                                                                             Y8b d88P
#                                                                              "Y88P"
#  .d88888b.                                     888
# d88P" "Y88b                                    888
# 888     888                                    888
# 888     888 88888b.   .d88b.  888d888  8888b.  888888  .d88b.  888d888 .d8888b
# 888     888 888 "88b d8P  Y8b 888P"       "88b 888    d88""88b 888P"   88K
# 888     888 888  888 88888888 888     .d888888 888    888  888 888     "Y8888b.
# Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  Y88..88P 888          X88
#  "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888  "Y88P"  888      88888P'
#             888
#             888
#             888
#
################################################################################################################################################################################################
class PN_UnaryExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('UnaryExpression', p)
        self.strict = ctx.strict
class PN_UnaryExpression_UpdateExpression(PN_UnaryExpression):
    pass
class PN_UnaryExpression_op(PN_UnaryExpression):
    def IsFunctionDefinition(self):
        return False
    def IsValidSimpleAssignmentTarget(self):
        return False
class PN_UnaryExpression_DELETE_UnaryExpression(PN_UnaryExpression_op):
    # 12.5.3 The delete Operator
    def EarlyErrors(self):
        errs = []
        if self.strict:
            # * It is a Syntax Error if the UnaryExpression is contained in strict mode code and the derived
            #   UnaryExpression is PrimaryExpression:IdentifierReference .
            # * It is a Syntax Error if the derived UnaryExpression is
            #   PrimaryExpression:CoverParenthesizedExpressionAndArrowParameterList and
            #   CoverParenthesizedExpressionAndArrowParameterList ultimately derives a phrase that, if used in place of
            #   UnaryExpression, would produce a Syntax Error according to these rules. This rule is recursively applied.
            # NOTE
            # The last rule means that expressions such as delete (((foo))) produce early errors because of recursive
            # application of the first rule.
            # --> I'm pretty sure the only way this would happen is exactly the example they give.
            #
            UnaryExpression = self.children[1]
            p = UnaryExpression
            while 1:
                if len(p.children) != 1 or not isinstance(p.children[0], ParseNode):
                    break
                if p.name == 'PrimaryExpression':
                    if p.children[0].name == 'IdentifierReference':
                        errs.append(CreateSyntaxError('Cannot delete a top-level property in strict mode'))
                        break
                    if (p.children[0].name == 'CoverParenthesizedExpressionAndArrowParameterList' and
                        len(p.children[0].children) == 3 and
                        p.children[0].children[1].name == 'Expression'):
                        p = p.children[0].children[1]
                        continue
                p = p.children[0]
        return errs
    def evaluate(self):
        # 12.5.3.2 Runtime Semantics: Evaluation
        #           UnaryExpression : delete UnaryExpression
        UnaryExpression = self.children[1]
        #   1. Let ref be the result of evaluating UnaryExpression.
        #   2. ReturnIfAbrupt(ref).
        ref = UnaryExpression.evaluate()
        #   3. If Type(ref) is not Reference, return true.
        if not isinstance(ref, Reference):
            return True
        #   4. If IsUnresolvableReference(ref) is true, then
        if IsUnresolvableReference(ref):
            #   a. Assert: IsStrictReference(ref) is false.
            assert not IsStrictReference(ref)
            #   b. Return true.
            return True
        #   5. If IsPropertyReference(ref) is true, then
        if IsPropertyReference(ref):
            #   a. If IsSuperReference(ref) is true, throw a ReferenceError exception.
            if IsSuperReference(ref):
                raise ESReferenceError('Can\'t delete super references')
            #   b. Let baseObj be ! ToObject(GetBase(ref)).
            baseObj = ToObject(GetBase(ref))
            #   c. Let deleteStatus be ? baseObj.[[Delete]](GetReferencedName(ref)).
            deleteStatus = baseObj.Delete(GetReferencedName(ref))
            #   d. If deleteStatus is false and IsStrictReference(ref) is true, throw a TypeError exception.
            if not deleteStatus and IsStrictReference(ref):
                raise ESTypeError(f'Couldn\'t delete {GetReferencedName(ref)!r}.')
            #   e. Return deleteStatus.
            return deleteStatus
        #   6. Else ref is a Reference to an Environment Record binding,
        #   a. Let bindings be GetBase(ref).
        bindings = GetBase(ref)
        #   b. Return ? bindings.DeleteBinding(GetReferencedName(ref)).
        return bindings.DeleteBinding(GetReferencedName(ref))
        # NOTE
        # When a delete operator occurs within strict mode code, a SyntaxError exception is thrown if its
        # UnaryExpression is a direct reference to a variable, function argument, or function name. In addition, if a
        # delete operator occurs within strict mode code and the property to be deleted has the attribute
        # { [[Configurable]]: false }, a TypeError exception is thrown.
class PN_UnaryExpression_VOID_UnaryExpression(PN_UnaryExpression_op):
    # 12.5.4 The void Operator
    def evaluate(self):
        # 12.5.4.1 Runtime Semantics: Evaluation
        #           UnaryExpression : void UnaryExpression
        # 1. Let expr be the result of evaluating UnaryExpression.
        # 2. Perform ? GetValue(expr).
        # 3. Return undefined.
        # NOTE
        # GetValue must be called even though its value is not used because it may have observable side-effects.
        GetValue(self.children[1].evaluate())
        return None
class PN_UnaryExpression_TYPEOF_UnaryExpression(PN_UnaryExpression_op):
    # 12.5.5 The typeof Operator
    def evaluate(self):
        # 12.5.5.1 Runtime Semantics: Evaluation
        #           UnaryExpression : typeof UnaryExpression
        # 1. Let val be the result of evaluating UnaryExpression.
        # 2. If Type(val) is Reference, then
        #   a. If IsUnresolvableReference(val) is true, return "undefined".
        # 3. Set val to ? GetValue(val).
        # 4. Return a String according to Table 35.
        #
        # Table 35: typeof Operator Results
        # +----------------------------------------------------------+-------------------------------------------------
        # | Type of val                                              | Result
        # +----------------------------------------------------------+-------------------------------------------------
        # | Undefined                                                | "undefined"
        # +----------------------------------------------------------+-------------------------------------------------
        # | Null                                                     | "object"
        # +----------------------------------------------------------+-------------------------------------------------
        # | Boolean                                                  | "boolean"
        # +----------------------------------------------------------+-------------------------------------------------
        # | Number                                                   | "number"
        # +----------------------------------------------------------+-------------------------------------------------
        # | String                                                   | "string"
        # +----------------------------------------------------------+-------------------------------------------------
        # | Symbol                                                   | "symbol"
        # +----------------------------------------------------------+-------------------------------------------------
        # | Object (ordinary and does not implement [[Call]])        | "object"
        # +----------------------------------------------------------+-------------------------------------------------
        # | Object (standard exotic and does not implement [[Call]]) | "object"
        # +----------------------------------------------------------+-------------------------------------------------
        # | Object (implements [[Call]])                             | "function"
        # +----------------------------------------------------------+-------------------------------------------------
        # | Object (non-standard exotic and does not                 | Implementation-defined. Must not be "undefined",
        # | implement [[Call]])                                      | "boolean", "function", "number", "symbol", or
        # |                                                          |  "string".
        # +----------------------------------------------------------+-------------------------------------------------
        # NOTE
        # Implementations are discouraged from defining new typeof result values for non-standard exotic objects. If
        # possible "object" should be used for such objects.
        val = self.children[1].evaluate()
        if isinstance(val, Reference) and IsUnresolvableReference(val):
            return 'undefined'
        val = GetValue(val)
        type_matrix = [
            (isUndefined, 'undefined'),
            (isNull, 'object'),
            (isBoolean, 'boolean'),
            (isNumber, 'number'),
            (isString, 'string'),
            (isSymbol, 'symbol'),
            (lambda x: isObject(x) and not hasattr(x, 'Call'), 'object'),
            (isObject, 'function'),
        ]
        return next(result for check, result in type_matrix if check(val))
class PN_UnaryExpression_PLUS_UnaryExpression(PN_UnaryExpression_op):
    # 12.5.6 Unary + Operator
    # NOTE
    # The unary + operator converts its operand to Number type.
    def evaluate(self):
        # 12.5.6.1 Runtime Semantics: Evaluation
        #           UnaryExpression : + UnaryExpression
        # 1. Let expr be the result of evaluating UnaryExpression.
        # 2. Return ? ToNumber(? GetValue(expr)).
        val = GetValue(self.children[1].evaluate())
        return ToNumber(val)
class PN_UnaryExpression_MINUS_UnaryExpression(PN_UnaryExpression_op):
    # 12.5.7 Unary - Operator
    # NOTE
    # The unary - operator converts its operand to Number type and then negates it. Negating +0 produces -0, and
    # negating -0 produces +0.
    def evaluate(self):
        # 12.5.7.1 Runtime Semantics: Evaluation
        #           UnaryExpression : - UnaryExpression
        # 1. Let expr be the result of evaluating UnaryExpression.
        # 2. Let oldValue be ? ToNumber(? GetValue(expr)).
        # 3. If oldValue is NaN, return NaN.
        # 4. Return the result of negating oldValue; that is, compute a Number with the same magnitude but opposite
        #    sign.
        UnaryExpression = self.children[1]
        expr = GetValue(UnaryExpression.evaluate())
        oldValue = ToNumber(expr)
        if math.isnan(oldValue):
            return math.nan
        return -oldValue
class PN_UnaryExpression_TILDE_UnaryExpression(PN_UnaryExpression_op):
    # 12.5.8 Bitwise NOT Operator ( ~ )
    def evaluate(self):
        # 12.5.8.1 Runtime Semantics: Evaluation
        #           UnaryExpression : ~ UnaryExpression
        # 1. Let expr be the result of evaluating UnaryExpression.
        # 2. Let oldValue be ? ToInt32(? GetValue(expr)).
        # 3. Return the result of applying bitwise complement to oldValue. The result is a signed 32-bit integer.
        UnaryExpression = self.children[1]
        expr = GetValue(UnaryExpression.evaluate())
        oldValue = ToInt32(expr)
        return ToInt32(~oldValue)
class PN_UnaryExpression_BANG_UnaryExpression(PN_UnaryExpression_op):
    # 12.5.9 Logical NOT Operator ( ! )
    def evaluate(self):
        # 12.5.9.1 Runtime Semantics: Evaluation
        #           UnaryExpression : ! UnaryExpression
        # 1. Let expr be the result of evaluating UnaryExpression.
        # 2. Let oldValue be ToBoolean(? GetValue(expr)).
        # 3. If oldValue is true, return false.
        # 4. Return true.
        expr = GetValue(self.children[1].evaluate())
        oldValue = ToBoolean(expr)
        return not oldValue
################################################################################################################################################################################################
#
#  d888    .d8888b.       .d8888b.      8888888888                                                       888    d8b          888    d8b
# d8888   d88P  Y88b     d88P  Y88b     888                                                              888    Y8P          888    Y8P
#   888          888     888            888                                                              888                 888
#   888        .d88P     888d888b.      8888888    888  888 88888b.   .d88b.  88888b.   .d88b.  88888b.  888888 888  8888b.  888888 888  .d88b.  88888b.
#   888    .od888P"      888P "Y88b     888        `Y8bd8P' 888 "88b d88""88b 888 "88b d8P  Y8b 888 "88b 888    888     "88b 888    888 d88""88b 888 "88b
#   888   d88P"          888    888     888          X88K   888  888 888  888 888  888 88888888 888  888 888    888 .d888888 888    888 888  888 888  888
#   888   888"       d8b Y88b  d88P     888        .d8""8b. 888 d88P Y88..88P 888  888 Y8b.     888  888 Y88b.  888 888  888 Y88b.  888 Y88..88P 888  888
# 8888888 888888888  Y8P  "Y8888P"      8888888888 888  888 88888P"   "Y88P"  888  888  "Y8888  888  888  "Y888 888 "Y888888  "Y888 888  "Y88P"  888  888
#                                                           888
#                                                           888
#                                                           888
#  .d88888b.                                     888
# d88P" "Y88b                                    888
# 888     888                                    888
# 888     888 88888b.   .d88b.  888d888  8888b.  888888  .d88b.  888d888
# 888     888 888 "88b d8P  Y8b 888P"       "88b 888    d88""88b 888P"
# 888     888 888  888 88888888 888     .d888888 888    888  888 888
# Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  Y88..88P 888
#  "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888  "Y88P"  888
#             888
#             888
#             888
#
################################################################################################################################################################################################
class PN_ExponentiationExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ExponentiationExpression', p)
class PN_ExponentiationExpression_UnaryExpression(PN_ExponentiationExpression):
    pass
class PN_ExponentiationExpression_UpdateExpression_STARSTAR_ExponentiationExpression(PN_ExponentiationExpression):
    def IsFunctionDefinition(self):
        return False
    def IsValidSimpleAssignmentTarget(self):
        return False
    def evaluate(self):
        lval = GetValue(self.children[0].evaluate())
        rval = GetValue(self.children[2].evaluate())
        return ExponentiationOperation(lval, rval)
################################################################################################################################################################################################
#
#  d888    .d8888b.      8888888888     888b     d888          888 888    d8b          888 d8b                   888    d8b
# d8888   d88P  Y88b           d88P     8888b   d8888          888 888    Y8P          888 Y8P                   888    Y8P
#   888          888          d88P      88888b.d88888          888 888                 888                       888
#   888        .d88P         d88P       888Y88888P888 888  888 888 888888 888 88888b.  888 888  .d8888b  8888b.  888888 888 888  888  .d88b.
#   888    .od888P"       88888888      888 Y888P 888 888  888 888 888    888 888 "88b 888 888 d88P"        "88b 888    888 888  888 d8P  Y8b
#   888   d88P"            d88P         888  Y8P  888 888  888 888 888    888 888  888 888 888 888      .d888888 888    888 Y88  88P 88888888
#   888   888"       d8b  d88P          888   "   888 Y88b 888 888 Y88b.  888 888 d88P 888 888 Y88b.    888  888 Y88b.  888  Y8bd8P  Y8b.
# 8888888 888888888  Y8P d88P           888       888  "Y88888 888  "Y888 888 88888P"  888 888  "Y8888P "Y888888  "Y888 888   Y88P    "Y8888
#                                                                             888
#                                                                             888
#                                                                             888
#  .d88888b.                                     888
# d88P" "Y88b                                    888
# 888     888                                    888
# 888     888 88888b.   .d88b.  888d888  8888b.  888888  .d88b.  888d888 .d8888b
# 888     888 888 "88b d8P  Y8b 888P"       "88b 888    d88""88b 888P"   88K
# 888     888 888  888 88888888 888     .d888888 888    888  888 888     "Y8888b.
# Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  Y88..88P 888          X88
#  "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888  "Y88P"  888      88888P'
#             888
#             888
#             888
#
################################################################################################################################################################################################
class PN_MultiplicativeExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('MultiplicativeExpression', p)
class PN_MultiplicativeExpression_ExponentiationExpression(PN_MultiplicativeExpression):
    pass
class PN_MultiplicativeExpression_MultiplicativeOperator_ExponentiationExpression(PN_MultiplicativeExpression):
    def IsFunctionDefinition(self):
        # 12.7.1 Static Semantics: IsFunctionDefinition
        #       MultiplicativeExpression : MultiplicativeExpression MultiplicativeOperator ExponentiationExpression
        #   1. Return false.
        return False
    def IsValidSimpleAssignmentTarget(self):
        # 12.7.2 Static Semantics: IsValidSimpleAssignmentTarget
        #       MultiplicativeExpression : MultiplicativeExpression MultiplicativeOperator ExponentiationExpression
        #   1. Return false.
        return False
    def evaluate(self):
        # 12.7.3 Runtime Semantics: Evaluation
        #       MultiplicativeExpression : MultiplicativeExpression MultiplicativeOperator ExponentiationExpression
        MultiplicativeExpression = self.children[0]
        MultiplicativeOperator = self.children[1]
        ExponentiationExpression = self.children[2]
        # 1. Let left be the result of evaluating MultiplicativeExpression.
        left = MultiplicativeExpression.evaluate()
        # 2. Let leftValue be ? GetValue(left).
        leftValue = GetValue(left)
        # 3. Let right be the result of evaluating ExponentiationExpression.
        right = ExponentiationExpression.evaluate()
        # 4. Let rightValue be ? GetValue(right).
        rightValue = GetValue(right)
        # 5. Let lnum be ? ToNumber(leftValue).
        # 6. Let rnum be ? ToNumber(rightValue).
        # 7. Return the result of applying the MultiplicativeOperator (*, /, or %) to lnum and rnum as specified in
        #    12.7.3.1, 12.7.3.2, or 12.7.3.3.
        return {'*': MultiplyOperation, '/': DivideOperation, '%': ModuloOperation}[MultiplicativeOperator.op()](leftValue, rightValue)
class PN_MultiplicativeOperator(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('MultiplicativeOperator', p)
    def op(self):
        return self.children[0].value  # Will be '*' or '/' or '%'
################################################################################################################################################################################################
#
#  d888    .d8888b.       .d8888b.             d8888      888      888 d8b 888    d8b                        .d88888b.                                     888
# d8888   d88P  Y88b     d88P  Y88b           d88888      888      888 Y8P 888    Y8P                       d88P" "Y88b                                    888
#   888          888     Y88b. d88P          d88P888      888      888     888                              888     888                                    888
#   888        .d88P      "Y88888"          d88P 888  .d88888  .d88888 888 888888 888 888  888  .d88b.      888     888 88888b.   .d88b.  888d888  8888b.  888888  .d88b.  888d888 .d8888b
#   888    .od888P"      .d8P""Y8b.        d88P  888 d88" 888 d88" 888 888 888    888 888  888 d8P  Y8b     888     888 888 "88b d8P  Y8b 888P"       "88b 888    d88""88b 888P"   88K
#   888   d88P"          888    888       d88P   888 888  888 888  888 888 888    888 Y88  88P 88888888     888     888 888  888 88888888 888     .d888888 888    888  888 888     "Y8888b.
#   888   888"       d8b Y88b  d88P      d8888888888 Y88b 888 Y88b 888 888 Y88b.  888  Y8bd8P  Y8b.         Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  Y88..88P 888          X88
# 8888888 888888888  Y8P  "Y8888P"      d88P     888  "Y88888  "Y88888 888  "Y888 888   Y88P    "Y8888       "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888  "Y88P"  888      88888P'
#                                                                                                                       888
#                                                                                                                       888
#                                                                                                                       888
#
################################################################################################################################################################################################
class PN_AdditiveExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('AdditiveExpression', p)
class PN_AdditiveExpression_MultiplicativeExpression(PN_AdditiveExpression):
    pass
class PN_AdditiveExpression_A_op_M(PN_AdditiveExpression):
    # 12.8.1 Static Semantics: IsFunctionDefinition
    def IsFunctionDefinition(self):
        # 1. Return false.
        return False
    # 12.8.2 Static Semantics: IsValidSimpleAssignmentTarget
    def IsValidSimpleAssignmentTarget(self):
        # 1. Return false.
        return False
class PN_AdditiveExpression_AdditiveExpression_PLUS_MultiplicativeExpression(PN_AdditiveExpression_A_op_M):
    # 12.8.3 The Addition Operator ( + )
    # NOTE
    # The addition operator either performs string concatenation or numeric addition.

    # 12.8.3.1 Runtime Semantics: Evaluation
    def evaluate(self):
        # 1. Let lref be the result of evaluating AdditiveExpression.
        lref = self.children[0].evaluate()
        # 2. Let lval be ? GetValue(lref).
        lval = GetValue(lref)
        # 3. Let rref be the result of evaluating MultiplicativeExpression.
        rref = self.children[2].evaluate()
        # 4. Let rval be ? GetValue(rref).
        rval = GetValue(rref)
        # 5. Let lprim be ? ToPrimitive(lval).
        lprim = ToPrimitive(lval)
        # 6. Let rprim be ? ToPrimitive(rval).
        rprim = ToPrimitive(rval)
        # 7. If Type(lprim) is String or Type(rprim) is String, then
        if isString(lprim) or isString(rprim):
            # a. Let lstr be ? ToString(lprim).
            lstr = ToString(lprim)
            # b. Let rstr be ? ToString(rprim).
            rstr = ToString(rprim)
            # c. Return the string-concatenation of lstr and rstr.
            return lstr + rstr
        # 8. Let lnum be ? ToNumber(lprim).
        lnum = ToNumber(lprim)
        # 9. Let rnum be ? ToNumber(rprim).
        rnum = ToNumber(rprim)
        # 10. Return the result of applying the addition operation to lnum and rnum. See the Note below 12.8.5.
        return lnum + rnum
        # NOTE 1
        # No hint is provided in the calls to ToPrimitive in steps 5 and 6. All standard objects except Date objects handle the
        # absence of a hint as if the hint Number were given; Date objects handle the absence of a hint as if the hint String
        # were given. Exotic objects may handle the absence of a hint in some other manner.
        # NOTE 2
        # Step 7 differs from step 3 of the Abstract Relational Comparison algorithm, by using the logical-or operation instead
        # of the logical-and operation.
class PN_AdditiveExpression_AdditiveExpression_MINUS_MultiplicativeExpression(PN_AdditiveExpression_A_op_M):
    # 12.8.4 The Subtraction Operator ( - )
    # 12.8.4.1 Runtime Semantics: Evaluation
    def evaluate(self):
        # 1.Let lref be the result of evaluating AdditiveExpression.
        lref = self.children[0].evaluate()
        # 2. Let lval be ? GetValue(lref).
        lval = GetValue(lref)
        # 3. Let rref be the result of evaluating MultiplicativeExpression.
        rref = self.children[2].evaluate()
        # 4. Let rval be ? GetValue(rref).
        rval = GetValue(rref)
        # 5. Let lnum be ? ToNumber(lval).
        lnum = ToNumber(lval)
        # 6. Let rnum be ? ToNumber(rval).
        rnum = ToNumber(rval)
        # 7. Return the result of applying the subtraction operation to lnum and rnum. See the note below 12.8.5.
        return lnum - rnum
# 12.8.5 Applying the Additive Operators to Numbers
#
# The + operator performs addition when applied to two operands of numeric type, producing the sum of the operands. The -
# operator performs subtraction, producing the difference of two numeric operands.
#
# Addition is a commutative operation, but not always associative.
#
# The result of an addition is determined using the rules of IEEE 754-2008 binary double-precision arithmetic:
#
# * If either operand is NaN, the result is NaN.
# * The sum of two infinities of opposite sign is NaN.
# * The sum of two infinities of the same sign is the infinity of that sign.
# * The sum of an infinity and a finite value is equal to the infinite operand.
# * sum of two negative zeroes is -0. The sum of two positive zeroes, or of two zeroes of opposite sign, is +0.
# * sum of a zero and a nonzero finite value is equal to the nonzero operand.
# * The sum of two nonzero finite values of the same magnitude and opposite sign is +0.
# * The remaining cases, where neither an infinity, nor a zero, nor NaN is involved, and the operands have the same sign or
#   have different magnitudes, the sum is computed and rounded to the nearest representable value using IEEE 754-2008 round
#   to nearest, ties to even mode. If the magnitude is too large to represent, the operation overflows and the result is then
#   an infinity of appropriate sign. The ECMAScript language requires support of gradual underflow as defined by IEEE 754-2008.
# NOTE
# The - operator performs subtraction when applied to two operands of numeric type, producing the difference of its operands;
# the left operand is the minuend and the right operand is the subtrahend. Given numeric operands a and b, it is always the
# case that a-b produces the same result as a+(-b).
##############################################################################################################################################################################################################
#
#  d888    .d8888b.       .d8888b.      888888b.   d8b 888                  d8b                        .d8888b.  888      d8b  .d888 888         .d88888b.                                     888
# d8888   d88P  Y88b     d88P  Y88b     888  "88b  Y8P 888                  Y8P                       d88P  Y88b 888      Y8P d88P"  888        d88P" "Y88b                                    888
#   888          888     888    888     888  .88P      888                                            Y88b.      888          888    888        888     888                                    888
#   888        .d88P     Y88b. d888     8888888K.  888 888888 888  888  888 888 .d8888b   .d88b.       "Y888b.   88888b.  888 888888 888888     888     888 88888b.   .d88b.  888d888  8888b.  888888  .d88b.  888d888 .d8888b
#   888    .od888P"       "Y888P888     888  "Y88b 888 888    888  888  888 888 88K      d8P  Y8b         "Y88b. 888 "88b 888 888    888        888     888 888 "88b d8P  Y8b 888P"       "88b 888    d88""88b 888P"   88K
#   888   d88P"                 888     888    888 888 888    888  888  888 888 "Y8888b. 88888888           "888 888  888 888 888    888        888     888 888  888 88888888 888     .d888888 888    888  888 888     "Y8888b.
#   888   888"       d8b Y88b  d88P     888   d88P 888 Y88b.  Y88b 888 d88P 888      X88 Y8b.         Y88b  d88P 888  888 888 888    Y88b.      Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  Y88..88P 888          X88
# 8888888 888888888  Y8P  "Y8888P"      8888888P"  888  "Y888  "Y8888888P"  888  88888P'  "Y8888       "Y8888P"  888  888 888 888     "Y888      "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888  "Y88P"  888      88888P'
#                                                                                                                                                           888
#                                                                                                                                                           888
#                                                                                                                                                           888
#
##############################################################################################################################################################################################################
# 12.9 Bitwise Shift Operators
class PN_ShiftExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ShiftExpression', p)
class PN_ShiftExpression_S_op_A(PN_ShiftExpression):
    @property
    def lval_is_signed(self):
        raise NotImplementedError('Abstract Class should not be instantiated')
    def operate(self, lnum, shiftcount):
        raise NotImplementedError('Abstract Class should not be instantiated')
    # 12.9.1 Static Semantics: IsFunctionDefinition
    def IsFunctionDefinition(self):
        # 1. Return false.
        return False
    # 12.9.2 Static Semantics: IsValidSimpleAssignmentTarget
    def IsValidSimpleAssignmentTarget(self):
        # 1. Return false.
        return False
    # 12.9.3.1 Runtime Semantics: Evaluation
    # --- The first 7 steps are (nearly) common for all 3 productions.
    def evaluate(self):
        # 1. Let lref be the result of evaluating ShiftExpression.
        lref = self.children[0].evaluate()
        # 2. Let lval be ? GetValue(lref).
        lval = GetValue(lref)
        # 3. Let rref be the result of evaluating AdditiveExpression.
        rref = self.children[2].evaluate()
        # 4. Let rval be ? GetValue(rref).
        rval = GetValue(rref)
        # 5. Let lnum be ? ToInt32(lval), or ? ToUint32(lval), depending.
        lnum = ToInt32(lval) if self.lval_is_signed else ToUint32(lval)
        # 6. Let rnum be ? ToUint32(rval).
        rnum = ToUint32(rval)
        # 7. Let shiftCount be the result of masking out all but the least significant 5 bits of rnum, that is, compute
        #    rnum & 0x1F.
        shiftCount = rnum & 0x1f
        return self.operate(lnum, shiftCount)
class PN_ShiftExpression_AdditiveExpression(PN_ShiftExpression):
    pass # Nothing more for pass-thru productions.
class PN_ShiftExpression_LTLT_AdditiveExpression(PN_ShiftExpression_S_op_A):
    lval_is_signed = True
    def operate(self, lnum, shiftCount):
        # 8. Return the result of left shifting lnum by shiftCount bits. The result is a signed 32-bit integer.
        return ToInt32(lnum << shiftCount)
class PN_ShiftExpression_GTGT_AdditiveExpression(PN_ShiftExpression_S_op_A):
    lval_is_signed = True
    def operate(self, lnum, shiftCount):
        # 8. Return the result of performing a sign-extending right shift of lnum by shiftCount bits. The most significant bit
        #    is propagated. The result is a signed 32-bit integer.
        return lnum >> shiftCount
class PN_ShiftExpression_GTGTGT_AdditiveExpression(PN_ShiftExpression_S_op_A):
    lval_is_signed = False
    def operate(self, lnum, shiftCount):
        # 8. Return the result of performing a zero-filling right shift of lnum by shiftCount bits. Vacated bits are filled
        #    with zero. The result is an unsigned 32-bit integer.
        return lnum >> shiftCount
##############################################################################################################################################################################################################
#
#  d888    .d8888b.       d888    .d8888b.      8888888b.           888          888    d8b                            888      .d88888b.                                     888
# d8888   d88P  Y88b     d8888   d88P  Y88b     888   Y88b          888          888    Y8P                            888     d88P" "Y88b                                    888
#   888          888       888   888    888     888    888          888          888                                   888     888     888                                    888
#   888        .d88P       888   888    888     888   d88P  .d88b.  888  8888b.  888888 888  .d88b.  88888b.   8888b.  888     888     888 88888b.   .d88b.  888d888  8888b.  888888  .d88b.  888d888 .d8888b
#   888    .od888P"        888   888    888     8888888P"  d8P  Y8b 888     "88b 888    888 d88""88b 888 "88b     "88b 888     888     888 888 "88b d8P  Y8b 888P"       "88b 888    d88""88b 888P"   88K
#   888   d88P"            888   888    888     888 T88b   88888888 888 .d888888 888    888 888  888 888  888 .d888888 888     888     888 888  888 88888888 888     .d888888 888    888  888 888     "Y8888b.
#   888   888"       d8b   888   Y88b  d88P     888  T88b  Y8b.     888 888  888 Y88b.  888 Y88..88P 888  888 888  888 888     Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  Y88..88P 888          X88
# 8888888 888888888  Y8P 8888888  "Y8888P"      888   T88b  "Y8888  888 "Y888888  "Y888 888  "Y88P"  888  888 "Y888888 888      "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888  "Y88P"  888      88888P'
#                                                                                                                                          888
#                                                                                                                                          888
#                                                                                                                                          888
#
##############################################################################################################################################################################################################
# 12.10 Relational Operators
class PN_RelationalExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('RelationalExpression', p)
class PN_RelationalExpression_ShiftExpression(PN_RelationalExpression):
    pass # Nothing more for the pass-thru production
class PN_RelationalExpression_R_op_S(PN_RelationalExpression):
    def operate(self, lval, rval):
        raise NotImplementedError('Abstract Class should not be instantiated')
    # 12.10.1 Static Semantics: IsFunctionDefinition
    def IsFunctionDefinition(self):
        # 1. Return false.
        return False
    # 12.10.2 Static Semantics: IsValidSimpleAssignmentTarget
    def IsValidSimpleAssignmentTarget(self):
        # 1. Return false.
        return False
    # 12.10.3 Runtime Semantics: Evaluation
    #    -- The first 4 steps are the same for each production
    def evaluate(self):
        # 1. Let lref be the result of evaluating RelationalExpression.
        lref = self.children[0].evaluate()
        # 2. Let lval be ? GetValue(lref).
        lval = GetValue(lref)
        # 3. Let rref be the result of evaluating ShiftExpression.
        rref = self.children[2].evaluate()
        # 4. Let rval be ? GetValue(rref).
        rval = GetValue(rref)
        # 5+: defer to subclass
        return self.operate(lval, rval)
class PN_RelationalExpression_RelationalExpression_LT_ShiftExpression(PN_RelationalExpression_R_op_S):
    # 12.10.3 Runtime Semantics: Evaluation
    def operate(self, lval, rval):
        # 5. Let r be the result of performing Abstract Relational Comparison lval < rval.
        # 6. ReturnIfAbrupt(r).
        r = AbstractRelationalComparison(lval, rval, True)
        # 7. If r is undefined, return false. Otherwise, return r.
        return r or False
class PN_RelationalExpression_RelationalExpression_GT_ShiftExpression(PN_RelationalExpression_R_op_S):
    # 12.10.3 Runtime Semantics: Evaluation
    def operate(self, lval, rval):
        # 5. Let r be the result of performing Abstract Relational Comparison rval < lval with LeftFirst equal to false.
        # 6. ReturnIfAbrupt(r).
        r = AbstractRelationalComparison(rval, lval, False)
        # 7. If r is undefined, return false. Otherwise, return r.
        return r or False
class PN_RelationalExpression_RelationalExpression_LE_ShiftExpression(PN_RelationalExpression_R_op_S):
    # 12.10.3 Runtime Semantics: Evaluation
    def operate(self, lval, rval):
        # 5. Let r be the result of performing Abstract Relational Comparison rval < lval with LeftFirst equal to false.
        # 6. ReturnIfAbrupt(r).
        r = AbstractRelationalComparison(rval, lval, False)
        # 7. If r is true or undefined, return false. Otherwise, return true.
        return not (r is None or r)
class PN_RelationalExpression_RelationalExpression_GE_ShiftExpression(PN_RelationalExpression_R_op_S):
    # 12.10.3 Runtime Semantics: Evaluation
    def operate(self, lval, rval):
        # 5. Let r be the result of performing Abstract Relational Comparison lval < rval.
        # 6. ReturnIfAbrupt(r).
        r = AbstractRelationalComparison(lval, rval, True)
        # 7. If r is true or undefined, return false. Otherwise, return true.
        return not (r is None or r)
class PN_RelationalExpression_RelationalExpression_INSTANCEOF_ShiftExpression(PN_RelationalExpression_R_op_S):
    # 12.10.3 Runtime Semantics: Evaluation
    def operate(self, lval, rval):
        # 5. Return ? InstanceofOperator(lval, rval).
        return InstanceofOperator(lval, rval)
class PN_RelationalExpression_RelationalExpression_IN_ShiftExpression(PN_RelationalExpression_R_op_S):
    # 12.10.3 Runtime Semantics: Evaluation
    def operate(self, lval, rval):
        # 5. If Type(rval) is not Object, throw a TypeError exception.
        # 6. Return ? HasProperty(rval, ToPropertyKey(lval)).
        if not isObject(rval):
            raise ESTypeError(f"Cannot use 'in' operator to search for '{ToString(lval)}' in {ToString(rval)}")
        key = ToPropertyKey(lval)
        return HasProperty(rval, key)
# 12.10.4 Runtime Semantics: InstanceofOperator ( V, target )
def InstanceofOperator(V, target):
    # The abstract operation InstanceofOperator(V, target) implements the generic algorithm for determining if ECMAScript value
    # V is an instance of object target either by consulting target's @@hasinstance method or, if absent, determining whether
    # the value of target's prototype property is present in V's prototype chain. This abstract operation performs the
    # following steps:
    #
    # 1. If Type(target) is not Object, throw a TypeError exception.
    if not isObject(target):
        raise ESTypeError()
    # 2. Let instOfHandler be ? GetMethod(target, @@hasInstance).
    instOfHandler = GetMethod(target, wks_has_instance)
    # 3. If instOfHandler is not undefined, then
    if instOfHandler is not None:
        # a. Return ToBoolean(? Call(instOfHandler, target, « V »)).
        val = Call(instOfHandler, target, [V])
        return ToBoolean(val)
    # 4. If IsCallable(target) is false, throw a TypeError exception.
    if not IsCallable(target):
        raise ESTypeError()
    # 5. Return ? OrdinaryHasInstance(target, V).
    return OrdinaryHasInstance(target, V)
    # NOTE
    # Steps 4 and 5 provide compatibility with previous editions of ECMAScript that did not use a @@hasInstance method to
    # define the instanceof operator semantics. If an object does not define or inherit @@hasInstance it uses the default
    # instanceof semantics.
########################################################################################################################
#
#  d888    .d8888b.       d888    d888       8888888888                            888 d8b 888                  .d88888b.                                     888
# d8888   d88P  Y88b     d8888   d8888       888                                   888 Y8P 888                 d88P" "Y88b                                    888
#   888          888       888     888       888                                   888     888                 888     888                                    888
#   888        .d88P       888     888       8888888     .d88888 888  888  8888b.  888 888 888888 888  888     888     888 88888b.   .d88b.  888d888  8888b.  888888  .d88b.  888d888 .d8888b
#   888    .od888P"        888     888       888        d88" 888 888  888     "88b 888 888 888    888  888     888     888 888 "88b d8P  Y8b 888P"       "88b 888    d88""88b 888P"   88K
#   888   d88P"            888     888       888        888  888 888  888 .d888888 888 888 888    888  888     888     888 888  888 88888888 888     .d888888 888    888  888 888     "Y8888b.
#   888   888"       d8b   888     888       888        Y88b 888 Y88b 888 888  888 888 888 Y88b.  Y88b 888     Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  Y88..88P 888          X88
# 8888888 888888888  Y8P 8888888 8888888     8888888888  "Y88888  "Y88888 "Y888888 888 888  "Y888  "Y88888      "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888  "Y88P"  888      88888P'
#                                                            888                                       888                 888
#                                                            888                                  Y8b d88P                 888
#                                                            888                                   "Y88P"                  888
#
########################################################################################################################
# 12.11 Equality Operators
class PN_EqualityExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('EqualityExpression', p)
class PN_EqualityExpression_RelationalExpression(PN_EqualityExpression):
    pass # Nothing extra for the pass-thru case
class PN_EqualityExpression_E_op_R(PN_EqualityExpression):
    def operation(self, lval, rval):
        raise NotImplementedError('Abstract Class should not be instantiated')
    # 12.11.1 Static Semantics: IsFunctionDefinition
    def IsFunctionDefinition(self):
        # 1. Return false.
        return False
    # 12.11.2 Static Semantics: IsValidSimpleAssignmentTarget
    def IsValidSimpleAssignmentTarget(self):
        # 1. Return false.
        return False
    # 12.11.3 Runtime Semantics: Evaluation
    #    The first four steps are the same for each production
    def evaluate(self):
        # 1. Let lref be the result of evaluating EqualityExpression.
        lref = self.children[0].evaluate()
        # 2. Let lval be ? GetValue(lref).
        lval = GetValue(lref)
        # 3. Let rref be the result of evaluating RelationalExpression.
        rref = self.children[2].evaluate()
        # 4. Let rval be ? GetValue(rref).
        rval = GetValue(rref)
        # 5-6. Defer to subclasses
        return self.operation(lval, rval)
class PN_EqualityExpression_EqualityExpression_EQEQ_RelationalExpression(PN_EqualityExpression_E_op_R):
    # 12.11.3 Runtime Semantics: Evaluation
    #   For EqualityExpression:EqualityExpression == RelationalExpression
    def operation(self, lval, rval):
        # 5. Return the result of performing Abstract Equality Comparison rval == lval.
        return AbstractEqualityComparison(rval, lval)
class PN_EqualityExpression_EqualityExpression_BANGEQ_RelationalExpression(PN_EqualityExpression_E_op_R):
    # 12.11.3 Runtime Semantics: Evaluation
    #   For EqualityExpression : EqualityExpression != RelationalExpression
    def operation(self, lval, rval):
        # 5. Let r be the result of performing Abstract Equality Comparison rval == lval.
        # 6. If r is true, return false. Otherwise, return true.
        r = AbstractEqualityComparison(rval, lval)
        return not r
class PN_EqualityExpression_EqualityExpression_EQEQEQ_RelationalExpression(PN_EqualityExpression_E_op_R):
    # 12.11.3 Runtime Semantics: Evaluation
    #   For EqualityExpression : EqualityExpression === RelationalExpression
    def operation(self, lval, rval):
        # 5. Return the result of performing Strict Equality Comparison rval === lval.
        return StrictEqualityComparison(rval, lval)
class PN_EqualityExpression_EqualityExpression_BANGEQEQ_RelationalExpression(PN_EqualityExpression_E_op_R):
    # 12.11.3 Runtime Semantics: Evaluation
    #   For EqualityExpression : EqualityExpression !== RelationalExpression
    def operation(self, lval, rval):
        # 5. Let r be the result of performing Strict Equality Comparison rval === lval.
        # 6. If r is true, return false. Otherwise, return true.
        return not StrictEqualityComparison(rval, lval)
########################################################################################################################

###################################################################################################################################################################################################################################################
#
#  d888    .d8888b.       d888    .d8888b.      888888b.   d8b                                        888888b.   d8b 888                  d8b                        .d88888b.                                     888
# d8888   d88P  Y88b     d8888   d88P  Y88b     888  "88b  Y8P                                        888  "88b  Y8P 888                  Y8P                       d88P" "Y88b                                    888
#   888          888       888          888     888  .88P                                             888  .88P      888                                            888     888                                    888
#   888        .d88P       888        .d88P     8888888K.  888 88888b.   8888b.  888d888 888  888     8888888K.  888 888888 888  888  888 888 .d8888b   .d88b.      888     888 88888b.   .d88b.  888d888  8888b.  888888  .d88b.  888d888 .d8888b
#   888    .od888P"        888    .od888P"      888  "Y88b 888 888 "88b     "88b 888P"   888  888     888  "Y88b 888 888    888  888  888 888 88K      d8P  Y8b     888     888 888 "88b d8P  Y8b 888P"       "88b 888    d88""88b 888P"   88K
#   888   d88P"            888   d88P"          888    888 888 888  888 .d888888 888     888  888     888    888 888 888    888  888  888 888 "Y8888b. 88888888     888     888 888  888 88888888 888     .d888888 888    888  888 888     "Y8888b.
#   888   888"       d8b   888   888"           888   d88P 888 888  888 888  888 888     Y88b 888     888   d88P 888 Y88b.  Y88b 888 d88P 888      X88 Y8b.         Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  Y88..88P 888          X88
# 8888888 888888888  Y8P 8888888 888888888      8888888P"  888 888  888 "Y888888 888      "Y88888     8888888P"  888  "Y888  "Y8888888P"  888  88888P'  "Y8888       "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888  "Y88P"  888      88888P'
#                                                                                             888                                                                               888
#                                                                                        Y8b d88P                                                                               888
#                                                                                         "Y88P"                                                                                888
#
###################################################################################################################################################################################################################################################
# 12.12 Binary Bitwise Operators
class PN_BitwiseExpression(ParseNode):
    def operate(self, lnum, rnum):
        raise NotImplementedError('Abstract Class should not be instantiated')
    # 12.12.1 Static Semantics: IsFunctionDefinition
    def IsFunctionDefinition(self):
        # 1. Return false.
        return False
    # 12.12.2 Static Semantics: IsValidSimpleAssignmentTarget
    def IsValidSimpleAssignmentTarget(self):
        # 1. Return false.
        return False
    # 12.12.3 Runtime Semantics: Evaluation
    def evaluate(self):
        # The production A:A@B , where @ is one of the bitwise operators in the productions above, is evaluated as follows:
        # 1. Let lref be the result of evaluating A.
        lref = self.children[0].evaluate()
        # 2. Let lval be ? GetValue(lref).
        lval = GetValue(lref)
        # 3. Let rref be the result of evaluating B.
        rref = self.children[2].evaluate()
        # 4. Let rval be ? GetValue(rref).
        rval = GetValue(rref)
        # 5. Let lnum be ? ToInt32(lval).
        lnum = ToInt32(lval)
        # 6. Let rnum be ? ToInt32(rval).
        rnum = ToInt32(rval)
        # 7+. Defer to subclasses
        return self.operate(lnum, rnum)
# '&' Productions
class PN_BitwiseANDExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('BitwiseANDExpression', p)
class PN_BitwiseANDExpression_EqualityExpression(PN_BitwiseANDExpression):
    pass # nothing more for the pass-thru production
class PN_BitwiseANDExpression_BitwiseANDExpression_AMP_EqualityExpression(PN_BitwiseANDExpression, PN_BitwiseExpression):
    def operate(self, lnum, rnum):
        # 7. Return the result of applying the bitwise operator @ to lnum and rnum. The result is a signed 32-bit integer.
        return lnum & rnum
# '^' Productions
class PN_BitwiseXORExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('BitwiseXORExpression', p)
class PN_BitwiseXORExpression_BitwiseANDExpression(PN_BitwiseXORExpression):
    pass # Nothing more for pass-thru production
class PN_BitwiseXORExpression_BitwiseXORExpression_XOR_BitwiseANDExpression(PN_BitwiseXORExpression, PN_BitwiseExpression):
    # 12.12.3 Runtime Semantics: Evaluation
    def operate(self, lnum, rnum):
        # 7. Return the result of applying the bitwise operator @ to lnum and rnum. The result is a signed 32-bit integer.
        return lnum ^ rnum
# '|' Productions
class PN_BitwiseORExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('BitwiseORExpression', p)
class PN_BitwiseORExpression_BitwiseXORExpression(PN_BitwiseORExpression):
    pass # nothing more needed for pass-thru productions
class PN_BitwiseORExpression_BitwiseORExpression_PIPE_BitwiseXORExpression(PN_BitwiseORExpression, PN_BitwiseExpression):
    # 12.12.3 Runtime Semantics: Evaluation
    def operate(self, lnum, rnum):
        # 7. Return the result of applying the bitwise operator @ to lnum and rnum. The result is a signed 32-bit integer.
        return lnum | rnum
##############################################################################################################################################################################################################################################
#
#  d888    .d8888b.       d888    .d8888b.      888888b.   d8b                                        888                        d8b                   888      .d88888b.                                     888
# d8888   d88P  Y88b     d8888   d88P  Y88b     888  "88b  Y8P                                        888                        Y8P                   888     d88P" "Y88b                                    888
#   888          888       888        .d88P     888  .88P                                             888                                              888     888     888                                    888
#   888        .d88P       888       8888"      8888888K.  888 88888b.   8888b.  888d888 888  888     888       .d88b.   .d88b.  888  .d8888b  8888b.  888     888     888 88888b.   .d88b.  888d888  8888b.  888888  .d88b.  888d888 .d8888b
#   888    .od888P"        888        "Y8b.     888  "Y88b 888 888 "88b     "88b 888P"   888  888     888      d88""88b d88P"88b 888 d88P"        "88b 888     888     888 888 "88b d8P  Y8b 888P"       "88b 888    d88""88b 888P"   88K
#   888   d88P"            888   888    888     888    888 888 888  888 .d888888 888     888  888     888      888  888 888  888 888 888      .d888888 888     888     888 888  888 88888888 888     .d888888 888    888  888 888     "Y8888b.
#   888   888"       d8b   888   Y88b  d88P     888   d88P 888 888  888 888  888 888     Y88b 888     888      Y88..88P Y88b 888 888 Y88b.    888  888 888     Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  Y88..88P 888          X88
# 8888888 888888888  Y8P 8888888  "Y8888P"      8888888P"  888 888  888 "Y888888 888      "Y88888     88888888  "Y88P"   "Y88888 888  "Y8888P "Y888888 888      "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888  "Y88P"  888      88888P'
#                                                                                             888                            888                                           888
#                                                                                        Y8b d88P                       Y8b d88P                                           888
#                                                                                         "Y88P"                         "Y88P"                                            888
#
##############################################################################################################################################################################################################################################
# 12.13 Binary Logical Operators
class PN_LogicalExpression(ParseNode):
    # 12.13.1 Static Semantics: IsFunctionDefinition
    def IsFunctionDefinition(self):
        # 1. Return false.
        return False
    # 12.13.2 Static Semantics: IsValidSimpleAssignmentTarget
    def IsValidSimpleAssignmentTarget(self):
        # 1. Return false.
        return False
# '&&' Productions
class PN_LogicalANDExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('LogicalANDExpression', p)
class PN_LogicalANDExpression_BitwiseORExpression(PN_LogicalANDExpression):
    pass # Nothing more for pass-thru productions
class PN_LogicalANDExpression_LogicalANDExpression_AMPAMP_BitwiseORExpression(PN_LogicalANDExpression, PN_LogicalExpression):
    # 12.13.3 Runtime Semantics: Evaluation
    def evaluate(self):
        # 1. Let lref be the result of evaluating LogicalANDExpression.
        lref = self.children[0].evaluate()
        # 2. Let lval be ? GetValue(lref).
        lval = GetValue(lref)
        # 3. Let lbool be ToBoolean(lval).
        lbool = ToBoolean(lval)
        # 4. If lbool is false, return lval.
        if not lbool:
            return lval
        # 5. Let rref be the result of evaluating BitwiseORExpression.
        rref = self.children[2].evaluate()
        # 6. Return ? GetValue(rref).
        return GetValue(rref)
# '||' Productions
class PN_LogicalORExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('LogicalORExpression', p)
class PN_LogicalORExpression_LogicalANDExpression(PN_LogicalORExpression):
    pass # Nothing more for pass-thru productions
class PN_LogicalORExpression_LogicalORExpression_PIPEPIPE_LogicalANDExpression(PN_LogicalORExpression, PN_LogicalExpression):
    # 12.13.3 Runtime Semantics: Evaluation
    def evaluate(self):
        # 1. Let lref be the result of evaluating LogicalORExpression.
        lref = self.children[0].evaluate()
        # 2. Let lval be ? GetValue(lref).
        lval = GetValue(lref)
        # 3. Let lbool be ToBoolean(lval).
        lbool = ToBoolean(lval)
        # 4. If lbool is true, return lval.
        if lbool:
            return lval
        # 5. Let rref be the result of evaluating LogicalANDExpression.
        rref = self.children[2].evaluate()
        # 6. Return ? GetValue(rref).
        return GetValue(rref)
############################################################################################################################################################################################################################################################
#
#  d888    .d8888b.       d888       d8888       .d8888b.                         888 d8b 888    d8b                            888      .d88888b.                                     888                           .d88      .d8888b.              88b.
# d8888   d88P  Y88b     d8888      d8P888      d88P  Y88b                        888 Y8P 888    Y8P                            888     d88P" "Y88b                                    888                          d88P"     d88P  Y88b             "Y88b
#   888          888       888     d8P 888      888    888                        888     888                                   888     888     888                                    888                         d88P            .d88P               Y88b
#   888        .d88P       888    d8P  888      888         .d88b.  88888b.   .d88888 888 888888 888  .d88b.  88888b.   8888b.  888     888     888 88888b.   .d88b.  888d888  8888b.  888888  .d88b.  888d888     888           .d88P"      d8b        888
#   888    .od888P"        888   d88   888      888        d88""88b 888 "88b d88" 888 888 888    888 d88""88b 888 "88b     "88b 888     888     888 888 "88b d8P  Y8b 888P"       "88b 888    d88""88b 888P"       888           888"        Y8P        888
#   888   d88P"            888   8888888888     888    888 888  888 888  888 888  888 888 888    888 888  888 888  888 .d888888 888     888     888 888  888 88888888 888     .d888888 888    888  888 888         Y88b          888                   d88P
#   888   888"       d8b   888         888      Y88b  d88P Y88..88P 888  888 Y88b 888 888 Y88b.  888 Y88..88P 888  888 888  888 888     Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  Y88..88P 888          Y88b.                    d8b     .d88P
# 8888888 888888888  Y8P 8888888       888       "Y8888P"   "Y88P"  888  888  "Y88888 888  "Y888 888  "Y88P"  888  888 "Y888888 888      "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888  "Y88P"  888           "Y88        888         Y8P     88P"
#                                                                                                                                                   888
#                                                                                                                                                   888
#                                                                                                                                                   888
#
############################################################################################################################################################################################################################################################
class PN_ConditionalExpression_LogicalORExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ConditionalExpression', p)
class PN_ConditionalExpression_QUESTION_AssignmentExpression_COLON_AssignmentExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ConditionalExpression', p)
    def IsFunctionDefinition(self):
        return False
    def IsValidSimpleAssignmentTarget(self):
        return False
    # 12.14.3 Runtime Semantics: Evaluation
    # ConditionalExpression : LogicalORExpression ? AssignmentExpression : AssignmentExpression
    def evaluate(self):
        # 1. Let lref be the result of evaluating LogicalORExpression.
        lref = self.children[0].evaluate()
        # 2. Let lval be ToBoolean(? GetValue(lref)).
        lval = GetValue(lref)
        lval = ToBoolean(lval)
        # 3. If lval is true, then
        if lval:
            # a. Let trueRef be the result of evaluating the first AssignmentExpression.
            trueRef = self.children[2].evaluate()
            # b. Return ? GetValue(trueRef).
            return GetValue(trueRef)
        # 4. Else,
        # a. Let falseRef be the result of evaluating the second AssignmentExpression.
        falseRef = self.children[4].evaluate()
        # b. Return ? GetValue(falseRef).
        return GetValue(falseRef)
################################################################################################################################################################################################################################
#
#  d888    .d8888b.       d888   888888888             d8888                   d8b                                                   888         .d88888b.                                     888
# d8888   d88P  Y88b     d8888   888                  d88888                   Y8P                                                   888        d88P" "Y88b                                    888
#   888          888       888   888                 d88P888                                                                         888        888     888                                    888
#   888        .d88P       888   8888888b.          d88P 888 .d8888b  .d8888b  888  .d88b.  88888b.  88888b.d88b.   .d88b.  88888b.  888888     888     888 88888b.   .d88b.  888d888  8888b.  888888  .d88b.  888d888 .d8888b
#   888    .od888P"        888        "Y88b        d88P  888 88K      88K      888 d88P"88b 888 "88b 888 "888 "88b d8P  Y8b 888 "88b 888        888     888 888 "88b d8P  Y8b 888P"       "88b 888    d88""88b 888P"   88K
#   888   d88P"            888          888       d88P   888 "Y8888b. "Y8888b. 888 888  888 888  888 888  888  888 88888888 888  888 888        888     888 888  888 88888888 888     .d888888 888    888  888 888     "Y8888b.
#   888   888"       d8b   888   Y88b  d88P      d8888888888      X88      X88 888 Y88b 888 888  888 888  888  888 Y8b.     888  888 Y88b.      Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  Y88..88P 888          X88
# 8888888 888888888  Y8P 8888888  "Y8888P"      d88P     888  88888P'  88888P' 888  "Y88888 888  888 888  888  888  "Y8888  888  888  "Y888      "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888  "Y88P"  888      88888P'
#                                                                                       888                                                                 888
#                                                                                  Y8b d88P                                                                 888
#                                                                                   "Y88P"                                                                  888
#
################################################################################################################################################################################################################################
class PN_AssignmentExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('AssignmentExpression', p)
class PN_AssignmentExpression_ConditionalExpression(PN_AssignmentExpression):
    @property
    def ConditionalExpression(self):
        return self.children[0]
class PN_AssignmentExpression_LeftHandSideExpression_EQUALS_AssignmentExpression(PN_AssignmentExpression):
    @property
    def LeftHandSideExpression(self):
        return self.children[0]
    @property
    def AssignmentExpression(self):
        return self.children[2]
    @property
    def AssignmentPattern(self):
        if not hasattr(self, '_assignment_pattern'):
            self._assignment_pattern = self.LeftHandSideExpression.covering('AssignmentPattern')
        return self._assignment_pattern
    def EarlyErrors(self):
        # 12.15.1 Static Semantics: Early Errors
        # AssignmentExpression : LeftHandSideExpression = AssignmentExpression
        #   * It is a Syntax Error if LeftHandSideExpression is either an ObjectLiteral or an ArrayLiteral and
        #     LeftHandSideExpression is not covering an AssignmentPattern.
        #   * It is an early Reference Error if LeftHandSideExpression is neither an ObjectLiteral nor an ArrayLiteral
        #     and IsValidSimpleAssignmentTarget of LeftHandSideExpression is false.
        is_structured = self.LeftHandSideExpression.Is('ObjectLiteral') or self.LeftHandSideExpression.Is('ArrayLiteral')
        if is_structured:
            if not self.AssignmentPattern:
                return [CreateSyntaxError('Not a valid target for an assignment statement')]
        else:
            if not self.LeftHandSideExpression.IsValidSimpleAssignmentTarget():
                return [CreateReferenceError('Not a valid target for an assignment statement')]
        return []
    def IsFunctionDefinition(self):
        return False
    def IsValidSimpleAssignmentTarget(self):
        return False
    def evaluate(self):
        # 12.15.4 Runtime Semantics: Evaluation
        # AssignmentExpression : LeftHandSideExpression = AssignmentExpression
        #   1. If LeftHandSideExpression is neither an ObjectLiteral nor an ArrayLiteral, then
        #       a. Let lref be the result of evaluating LeftHandSideExpression.
        #       b. ReturnIfAbrupt(lref).
        #       c. Let rref be the result of evaluating AssignmentExpression.
        #       d. Let rval be ? GetValue(rref).
        #       e. If IsAnonymousFunctionDefinition(AssignmentExpression) and IsIdentifierRef of LeftHandSideExpression are both true, then
        #           i. Let hasNameProperty be ? HasOwnProperty(rval, "name").
        #           ii. If hasNameProperty is false, perform SetFunctionName(rval, GetReferencedName(lref)).
        #       f. Perform ? PutValue(lref, rval).
        #       g. Return rval.
        #   2. Let assignmentPattern be the AssignmentPattern that is covered by LeftHandSideExpression.
        #   3. Let rref be the result of evaluating AssignmentExpression.
        #   4. Let rval be ? GetValue(rref).
        #   5. Perform ? DestructuringAssignmentEvaluation of assignmentPattern using rval as the argument.
        #   6. Return rval.
        if not (self.LeftHandSideExpression.Is('ObjectLiteral') or self.LeftHandSideExpression.Is('ArrayLiteral')):
            lref = self.LeftHandSideExpression.evaluate()
            rref = self.AssignmentExpression.evaluate()
            rval = GetValue(rref)
            if IsAnonymousFunctionDefinition(self.LeftHandSideExpression) and self.LeftHandSideExpression.IsIdentifierRef():
                if not HasOwnProperty(rval, 'name'):
                    SetFunctionName(rval, GetReferencedName(lref))
            PutValue(lref, rval)
            return rval
        rref = self.AssignmentExpression.evaluate()
        rval = GetValue(rref)
        self.AssignmentPattern.DestructuringAssignmentEvaluation(rval)
        return rval
def prep_for_bitwise(lval, rval):
    lnum = ToInt32(lval)  # 1. Let lnum be ? ToInt32(lval).
    rnum = ToInt32(rval)  # 2. Let rnum be ? ToInt32(rval).
    return (lnum, rnum)  # Return (lnum, rnum)
def BitwiseANDOperation(lval, rval):
    # Do integer conversion on the operands, forming lnum and rnum
    operands = prep_for_bitwise(lval, rval)
    lnum, rnum = operands
    # Return the result of applying the bitwise operator & to lnum and rnum. The result is a signed 32-bit integer.
    return lnum & rnum
def BitwiseXOROperation(lval, rval):
    # Do integer conversion on the operands, forming lnum and rnum
    operands = prep_for_bitwise(lval, rval)
    lnum, rnum = operands
    # Return the result of applying the bitwise operator ^ to lnum and rnum. The result is a signed 32-bit integer.
    return lnum ^ rnum
def BitwiseOROperation(lval, rval):
    # Do integer conversion on the operands, forming lnum and rnum
    operands = prep_for_bitwise(lval, rval)
    lnum, rnum = operands
    # Return the result of applying the bitwise operator | to lnum and rnum. The result is a signed 32-bit integer.
    return lnum | rnum
def prep_for_math(lval, rval):
    # Converts args to Number values, in preparation for math.
    lnum = ToNumber(lval)  # 1. Let lnum be ? ToNumber(lval).
    rnum = ToNumber(rval)  # 2. Let rnum be ? ToNumber(rval).
    return (lnum, rnum)  # 3. Return (lnum, rnum)
def MultiplyOperation(lval, rval):
    # Do number conversion on the operands, forming lnum and rnum
    operands = prep_for_math(lval, rval)
    lnum, rnum = operands
    # Return the result of multiplying lnum and rnum.
    return lnum * rnum
def DivideOperation(lval, rval):
    # Do number conversion on the operands, forming lnum and rnum
    operands = prep_for_math(lval, rval)
    lnum, rnum = operands
    # Return the result of dividing lnum and rnum.
    return lnum / rnum
def ModuloOperation(lval, rval):
    # Do number conversion on the operands, forming lnum and rnum
    operands = prep_for_math(lval, rval)
    lnum, rnum = operands
    # Return the result of applying the modulo operator to lnum and rnum.
    return lnum % rnum
def AdditionOperation(lval, rval):
    lprim = ToPrimitive(lval)
    rprim = ToPrimitive(rval)
    if isString(lprim) or isString(rprim):
        lstr = ToString(lprim)
        rstr = ToString(rprim)
        return lstr + rstr
    # Do number conversion on the operands, forming lnum and rnum
    operands = prep_for_math(lval, rval)
    lnum, rnum = operands
    # Return the result of applying the addition operator to lnum and rnum.
    return lnum + rnum
def SubtractionOperation(lval, rval):
    # Do number conversion on the operands, forming lnum and rnum
    operands = prep_for_math(lval, rval)
    lnum, rnum = operands
    # Return the result of applying the modulo operator to lnum and rnum.
    return lnum - rnum
def prep_for_signed_shift(lval, rval):
    # Converts args to integer values, in preparation for signed shifting.
    lnum = ToInt32(lval)  # 1. Let lnum be ? ToInt32(lval).
    rnum = ToUint32(rval)  # 2. Let rnum be ? ToUnit32(rval).
    return (int(lnum), int(rnum))  # 3. Return (lnum, rnum).
def LeftShiftOperation(lval, rval):
    # Do number conversion on the operands, forming lnum and rnum
    operands = prep_for_signed_shift(lval, rval)
    lnum, rnum = operands
    # Strip the right operand to 5 bits and shift the left. Then put it back into a 32-bit int.
    shiftCount = rnum & 0x1f
    return ToInt32(lnum << shiftCount)
def RightShiftOperation(lval, rval):
    # Do number conversion on the operands, forming lnum and rnum
    operands = prep_for_signed_shift(lval, rval)
    lnum, rnum = operands
    shiftCount = rnum & 0x1f
    return lnum >> shiftCount
def prep_for_unsigned_shift(lval, rval):
    # Converts args to integer values, in preparation for unsigned shifting.
    lnum = ToUint32(lval)  # 1. Let lnum be ? ToUint32(lval)
    rnum = ToUint32(rval)  # 2. Let rnum be ? ToUnit32(rval)
    return (int(lnum), int(rnum))  # 3. Return (lnum, rnum)
def UnsignedRightShiftOperation(lval, rval):
    # Do number conversion on the operands, forming lnum and rnum
    operands = prep_for_unsigned_shift(lval, rval)
    lnum, rnum = operands
    shiftCount = rnum & 0x1f
    return lnum >> shiftCount
def ExponentiationOperation(lval, rval):
    operands = prep_for_math(lval, rval)
    lnum, rnum = operands
    if abs(lnum) == 1.0 and abs(rnum) == math.inf:
        return math.nan
    if lnum < 0.0 and math.isfinite(lnum) and math.isfinite(rnum) and math.floor(rnum) != rnum:
        return math.nan
    return lnum ** rnum

class PN_AssignmentOperator(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('AssignmentOperator', p)
    def op_function(self):
        xlat = {
            '*=': MultiplyOperation,
            '/=': DivideOperation,
            '%=': ModuloOperation,
            '+=': AdditionOperation,
            '-=': SubtractionOperation,
            '<<=': LeftShiftOperation,
            '>>=': RightShiftOperation,
            '>>>=': UnsignedRightShiftOperation,
            '&=': BitwiseANDOperation,
            '|=': BitwiseOROperation,
            '^=': BitwiseXOROperation,
            '**=': ExponentiationOperation
        }
        return xlat[self.children[0].value]
class PN_AssignmentExpression_LeftHandSideExpression_AssignmentOperator_AssignmentExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('AssignmentExpression', p)
    def EarlyErrors(self):
        if not self.children[0].IsValidSimpleAssignmentTarget():
            return [CreateReferenceError('Not a valid target for an assignment statement')]
        return []
    def IsFunctionDefinition(self):
        return False
    def IsValidSimpleAssignmentTarget(self):
        return False
    def evaluate(self):
        # 1. Let lref be the result of evaluating LeftHandSideExpression.
        lref = self.children[0].evaluate()
        # 2. Let lval be ? GetValue(lref).
        lval = GetValue(lref)
        # 3. Let rref be the result of evaluating AssignmentExpression.
        rref = self.children[2].evaluate()
        # 4. Let rval be ? GetValue(rref).
        rval = GetValue(rref)
        # 5. Let op be the @ where AssignmentOperator is @=.
        op = self.children[1].op_function()
        # 6. Let r be the result of applying op to lval and rval as if evaluating the expression lval op rval.
        r = op(lval, rval)
        # 7. Perform ? PutValue(lref, r).
        PutValue(lref, r)
        # 8. Return r.
        return r
        # NOTE
        # When an assignment occurs within strict mode code, it is a runtime error if lref in step 1.f of the first algorithm
        # or step 7 of the second algorithm it is an unresolvable reference. If it is, a ReferenceError exception is thrown.
        # The LeftHandSideExpression also may not be a reference to a data property with the attribute value
        # { [[Writable]]: false }, to an accessor property with the attribute value { [[Set]]: undefined }, nor to a
        # non-existent property of an object for which the IsExtensible predicate returns the value false. In these cases a
        # TypeError exception is thrown.

# ------------------------------------ 𝑨𝒔𝒔𝒊𝒈𝒏𝒎𝒆𝒏𝒕𝑷𝒂𝒕𝒕𝒆𝒓𝒏 ------------------------------------
class PN_AssignmentPattern(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('AssignmentPattern', p)
class PN_AssignmentPattern_ArrayAssignmentPattern(PN_AssignmentPattern):
    @property
    def ArrayAssignmentPattern(self):
        return self.children[0]
class PN_AssignmentPattern_ObjectAssignmentPattern(PN_AssignmentPattern):
    @property
    def ObjectAssignmentPattern(self):
        return self.children[0]
# ------------------------------------ 𝑶𝒃𝒋𝒆𝒄𝒕𝑨𝒔𝒔𝒊𝒈𝒏𝒎𝒆𝒏𝒕𝑷𝒂𝒕𝒕𝒆𝒓𝒏 ------------------------------------
class PN_ObjectAssignmentPattern(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ObjectAssignmentPattern', p)
class PN_ObjectAssignmentPattern_LCURLY_RCURLY(PN_ObjectAssignmentPattern):
    def DestructuringAssignmentEvaluation(self, value):
        # 12.15.5.2 Runtime Semantics: DestructuringAssignmentEvaluation
        #   With parameter value.
        # ObjectAssignmentPattern : { }
        #   1. Perform ? RequireObjectCoercible(value).
        #   2. Return NormalCompletion(empty).
        RequireObjectCoercible(value)
        return EMPTY
class PN_ObjectAssignmentPattern_LCURLY_AssignmentRestProperty_RCURLY(PN_ObjectAssignmentPattern):
    @property
    def AssignmentRestProperty(self):
        return self.children[1]
    def DestructuringAssignmentEvaluation(self, value):
        # 12.15.5.2 Runtime Semantics: DestructuringAssignmentEvaluation
        #   With parameter value.
        # ObjectAssignmentPattern : { AssignmentRestProperty }
        #   1. Let excludedNames be a new empty List.
        #   2. Return the result of performing RestDestructuringAssignmentEvaluation of AssignmentRestProperty with
        #      value and excludedNames as the arguments.
        return self.AssignmentRestProperty.RestDestructuringAssignmentEvaluation(value, [])
class PN_ObjectAssignmentPattern_LCURLY_AssignmentPropertyList_RCURLY(PN_ObjectAssignmentPattern):
    @property
    def AssignmentPropertyList(self):
        return self.children[1]
    def DestructuringAssignmentEvaluation(self, value):
        # 12.15.5.2 Runtime Semantics: DestructuringAssignmentEvaluation
        #   With parameter value.
        # ObjectAssignmentPattern : { AssignmentPropertyList }
        # ObjectAssignmentPattern : { AssignmentPropertyList , }
        #   1. Perform ? RequireObjectCoercible(value).
        #   2. Perform ? PropertyDestructuringAssignmentEvaluation for AssignmentPropertyList using value as the argument.
        #   3. Return NormalCompletion(empty).
        RequireObjectCoercible(value)
        self.AssignmentPropertyList.PropertyDestructuringAssignmentEvaluation(value)
        return EMPTY
class PN_ObjectAssignmentPattern_LCURLY_AssignmentPropertyList_COMMA_RCURLY(PN_ObjectAssignmentPattern_LCURLY_AssignmentPropertyList_RCURLY):
    pass
class PN_ObjectAssignmentPattern_LCURLY_AssignmentPropertyList_COMMA_AssignmentRestProperty_RCURLY(PN_ObjectAssignmentPattern):
    @property
    def AssignmentPropertyList(self):
        return self.children[1]
    @property
    def AssignmentRestProperty(self):
        return self.children[3]
    def DestructuringAssignmentEvaluation(self, value):
        # 12.15.5.2 Runtime Semantics: DestructuringAssignmentEvaluation
        #   With parameter value.
        # ObjectAssignmentPattern : { AssignmentPropertyList , AssignmentRestProperty }
        #   1. Let excludedNames be the result of performing ? PropertyDestructuringAssignmentEvaluation for
        #      AssignmentPropertyList using value as the argument.
        #   2. Return the result of performing RestDestructuringAssignmentEvaluation of AssignmentRestProperty with
        #      value and excludedNames as the arguments.
        excludedNames = self.AssignmentPropertyList.PropertyDestructuringAssignmentEvaluation(value)
        return self.AssignmentRestProperty.RestDestructuringAssignmentEvaluation(value, excludedNames)
# ------------------------------------ 𝑨𝒓𝒓𝒂𝒚𝑨𝒔𝒔𝒊𝒈𝒏𝒎𝒆𝒏𝒕𝑷𝒂𝒕𝒕𝒆𝒓𝒏 ------------------------------------
class PN_ArrayAssignmentPattern(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ArrayAssignmentPattern', p)
class PN_ArrayAssignmentPattern_LBRACKET_RBRACKET(PN_ArrayAssignmentPattern):
    def DestructuringAssignmentEvaluation(self, value):
        # 12.15.5.2 Runtime Semantics: DestructuringAssignmentEvaluation
        #   With parameter value.
        # ArrayAssignmentPattern : [ ]
        #   1. Let iteratorRecord be ? GetIterator(value).
        #   2. Return ? IteratorClose(iteratorRecord, NormalCompletion(empty)).
        iteratorRecord = GetIterator(value)
        IteratorClose(iteratorRecord)
        return EMPTY
class PN_ArrayAssignmentPattern_LBRACKET_Elision_RBRACKET(PN_ArrayAssignmentPattern):
    @property
    def Elision(self):
        return self.children[1]
    def DestructuringAssignmentEvaluation(self, value):
        # 12.15.5.2 Runtime Semantics: DestructuringAssignmentEvaluation
        #   With parameter value.
        # ArrayAssignmentPattern : [ Elision ]
        #   1. Let iteratorRecord be ? GetIterator(value).
        #   2. Let result be the result of performing IteratorDestructuringAssignmentEvaluation of Elision with
        #      iteratorRecord as the argument.
        #   3. If iteratorRecord.[[Done]] is false, return ? IteratorClose(iteratorRecord, result).
        #   4. Return result.
        iteratorRecord = GetIterator(value)
        try:
            result = self.Elision.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
        finally:
            if not iteratorRecord.Done:
                IteratorClose(iteratorRecord)
        return result
class PN_ArrayAssignmentPattern_LBRACKET_Elision_AssignmentRestElement_RBRACKET(PN_ArrayAssignmentPattern):
    @property
    def Elision(self):
        return self.children[1]
    @property
    def AssignmentRestElement(self):
        return self.children[2]
    def DestructuringAssignmentEvaluation(self, value):
        # 12.15.5.2 Runtime Semantics: DestructuringAssignmentEvaluation
        #   With parameter value.
        # ArrayAssignmentPattern : [ Elision AssignmentRestElement ]
        #   1. Let iteratorRecord be ? GetIterator(value).
        #   2. If Elision is present, then
        #       a. Let status be the result of performing IteratorDestructuringAssignmentEvaluation of Elision with
        #          iteratorRecord as the argument.
        #       b. If status is an abrupt completion, then
        #           i. Assert: iteratorRecord.[[Done]] is true.
        #           ii. Return Completion(status).
        #   3. Let result be the result of performing IteratorDestructuringAssignmentEvaluation of
        #      AssignmentRestElement with iteratorRecord as the argument.
        #   4. If iteratorRecord.[[Done]] is false, return ? IteratorClose(iteratorRecord, result).
        #   5. Return result.
        iteratorRecord = GetIterator(value)
        if self.Elision:
            try:
                self.Elision.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
            except (ESError, ESAbrupt):
                assert iteratorRecord.Done
                raise
        try:
            result = self.AssignmentRestElement.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
        finally:
            if not iteratorRecord.Done:
                IteratorClose(iteratorRecord)
        return result
class PN_ArrayAssignmentPattern_LBRACKET_AssignmentRestElement_RBRACKET(PN_ArrayAssignmentPattern_LBRACKET_Elision_AssignmentRestElement_RBRACKET):
    @property
    def AssignmentRestElement(self):
        return self.children[1]
    @property
    def Elision(self):
        return None
class PN_ArrayAssignmentPattern_LBRACKET_AssignmentElementList_RBRACKET(PN_ArrayAssignmentPattern):
    @property
    def AssignmentElementList(self):
        return self.children[1]
    def DestructuringAssignmentEvaluation(self, value):
        # 12.15.5.2 Runtime Semantics: DestructuringAssignmentEvaluation
        #   With parameter value.
        # ArrayAssignmentPattern : [ AssignmentElementList ]
        #   1. Let iteratorRecord be ? GetIterator(value).
        #   2. Let result be the result of performing IteratorDestructuringAssignmentEvaluation of
        #      AssignmentElementList using iteratorRecord as the argument.
        #   3. If iteratorRecord.[[Done]] is false, return ? IteratorClose(iteratorRecord, result).
        #   4. Return result.
        iteratorRecord = GetIterator(value)
        try:
            result = self.AssignmentElementList.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
        finally:
            if not iteratorRecord.Done:
                IteratorClose(iteratorRecord)
        return result
class PN_ArrayAssignmentPattern_LBRACKET_AssignmentElementList_COMMA_Elision_AssignmentRestElement_RBRACKET(PN_ArrayAssignmentPattern):
    @property
    def AssignmentElementList(self):
        return self.children[1]
    @property
    def Elision(self):
        return self.children[3]
    @property
    def AssignmentRestElement(self):
        return self.children[4]
    def DestructuringAssignmentEvaluation(self, value):
        # 12.15.5.2 Runtime Semantics: DestructuringAssignmentEvaluation
        #   With parameter value.
        # ArrayAssignmentPattern : [ AssignmentElementList , Elision AssignmentRestElement ]
        #   1. Let iteratorRecord be ? GetIterator(value).
        #   2. Let status be the result of performing IteratorDestructuringAssignmentEvaluation of
        #      AssignmentElementList using iteratorRecord as the argument.
        #   3. If status is an abrupt completion, then
        #       a. If iteratorRecord.[[Done]] is false, return ? IteratorClose(iteratorRecord, status).
        #       b. Return Completion(status).
        #   4. If Elision is present, then
        #       a. Set status to the result of performing IteratorDestructuringAssignmentEvaluation of Elision with
        #          iteratorRecord as the argument.
        #       b. If status is an abrupt completion, then
        #           i. Assert: iteratorRecord.[[Done]] is true.
        #           ii. Return Completion(status).
        #   5. If AssignmentRestElement is present, then
        #       a. Set status to the result of performing IteratorDestructuringAssignmentEvaluation of
        #          AssignmentRestElement with iteratorRecord as the argument.
        #   6. If iteratorRecord.[[Done]] is false, return ? IteratorClose(iteratorRecord, status).
        #   7. Return Completion(status).
        iteratorRecord = GetIterator(value)
        try:
            status = self.AssignmentElementList.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
            if self.Elision:
                status = self.Elision.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
            if self.AssignmentRestElement:
                status = self.AssignmentRestElement.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
        finally:
            if not iteratorRecord.Done:
                IteratorClose(iteratorRecord)
        return status
class PN_ArrayAssignmentPattern_LBRACKET_AssignmentElementList_COMMA_RBRACKET(PN_ArrayAssignmentPattern_LBRACKET_AssignmentElementList_COMMA_Elision_AssignmentRestElement_RBRACKET):
    @property
    def Elision(self):
        return None
    @property
    def AssignmentRestElement(self):
        return None
class PN_ArrayAssignmentPattern_LBRACKET_AssignmentElementList_COMMA_Elision_RBRACKET(PN_ArrayAssignmentPattern_LBRACKET_AssignmentElementList_COMMA_Elision_AssignmentRestElement_RBRACKET):
    @property
    def AssignmentRestElement(self):
        return None
class PN_ArrayAssignmentPattern_LBRACKET_AssignmentElementList_COMMA_AssignmentRestElement_RBRACKET(PN_ArrayAssignmentPattern_LBRACKET_AssignmentElementList_COMMA_Elision_AssignmentRestElement_RBRACKET):
    @property
    def Elision(self):
        return None
    @property
    def AssignmentRestElement(self):
        return self.children[3]
# ------------------------------------ 𝑨𝒔𝒔𝒊𝒈𝒏𝒎𝒆𝒏𝒕𝑹𝒆𝒔𝒕𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚 ------------------------------------
class PN_AssignmentRestProperty(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('AssignmentRestProperty', p)
class PN_AssignmentRestProperty_DOTDOTDOT_DestructuringAssignmentTarget(PN_AssignmentRestProperty):
    @property
    def DestructuringAssignmentTarget(self):
        return self.children[1]
    def EarlyErrors(self):
        # 12.15.5.1 Static Semantics: Early Errors
        # AssignmentRestProperty : ... DestructuringAssignmentTarget
        #   * It is a Syntax Error if DestructuringAssignmentTarget is an ArrayLiteral or an ObjectLiteral.
        if self.DestructuringAssignmentTarget.Is('ArrayLiteral') or self.DestructuringAssignmentTarget.Is('ObjectLiteral'):
            return [CreateSyntaxError('Object and Array literals not allowed here')]
    def RestDestructuringAssignmentEvaluation(self, value, excludedNames):
        # 12.15.5.4 Runtime Semantics: RestDestructuringAssignmentEvaluation
        #   With parameters value and excludedNames.
        # AssignmentRestProperty : ... DestructuringAssignmentTarget
        #   1. Let lref be the result of evaluating DestructuringAssignmentTarget.
        #   2. ReturnIfAbrupt(lref).
        #   3. Let restObj be ObjectCreate(%ObjectPrototype%).
        #   4. Perform ? CopyDataProperties(restObj, value, excludedNames).
        #   5. Return PutValue(lref, restObj).
        lref = self.DestructuringAssignmentTarget.evaluate()
        restObj = ObjectCreate(surrounding_agent.running_ec.realm.intrinsics['%ObjectPrototype%'])
        CopyDataProperties(restObj, value, excludedNames)
        return PutValue(lref, restObj)
# ------------------------------------ 𝑨𝒔𝒔𝒊𝒈𝒏𝒎𝒆𝒏𝒕𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚𝑳𝒊𝒔𝒕 ------------------------------------
class PN_AssignmentPropertyList(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('AssignmentPropertyList', p)
class PN_AssignmentPropertyList_AssignmentProperty(PN_AssignmentPropertyList):
    @property
    def AssignmentProperty(self):
        return self.children[0]
class PN_AssignmentPropertyList_AssignmentPropertyList_COMMA_AssignmentProperty(PN_AssignmentPropertyList):
    @property
    def AssignmentPropertyList(self):
        return self.children[0]
    @property
    def AssignmentProperty(self):
        return self.children[2]
    def PropertyDestructuringAssignmentEvaluation(self, value):
        # 12.15.5.3 Runtime Semantics: PropertyDestructuringAssignmentEvaluation
        #   With parameter value.
        # NOTE
        # The following operations collect a list of all destructured property names.
        # AssignmentPropertyList : AssignmentPropertyList , AssignmentProperty
        #   1. Let propertyNames be the result of performing ? PropertyDestructuringAssignmentEvaluation for
        #      AssignmentPropertyList using value as the argument.
        #   2. Let nextNames be the result of performing ? PropertyDestructuringAssignmentEvaluation for
        #      AssignmentProperty using value as the argument.
        #   3. Append each item in nextNames to the end of propertyNames.
        #   4. Return propertyNames.
        propertyNames = self.AssignmentPropertyList.PropertyDestructuringAssignmentEvaluation(value)
        nextNames = self.AssignmentProperty.PropertyDestructuringAssignmentEvaluation(value)
        propertyNames.extend(nextNames)
        return propertyNames
# ------------------------------------ 𝑨𝒔𝒔𝒊𝒈𝒏𝒎𝒆𝒏𝒕𝑬𝒍𝒆𝒎𝒆𝒏𝒕𝑳𝒊𝒔𝒕 ------------------------------------
class PN_AssignmentElementList(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('AssignmentElementList', p)
class PN_AssignmentElementList_AssignmentElisionElement(PN_AssignmentElementList):
    @property
    def AssignmentElisionElement(self):
        return self.children[0]
class PN_AssignmentElementList_AssignmentElementList_COMMA_AssignmentElisionElement(PN_AssignmentElementList):
    @property
    def AssignmentElementList(self):
        return self.children[0]
    @property
    def AssignmentElisionElement(self):
        return self.children[2]
    def IteratorDestructuringAssignmentEvaluation(self, iteratorRecord):
        # 12.15.5.5 Runtime Semantics: IteratorDestructuringAssignmentEvaluation
        #   With parameter iteratorRecord.
        # AssignmentElementList : AssignmentElementList , AssignmentElisionElement
        #   1. Perform ? IteratorDestructuringAssignmentEvaluation of AssignmentElementList using iteratorRecord as the
        #      argument.
        #   2. Return the result of performing IteratorDestructuringAssignmentEvaluation of AssignmentElisionElement
        #      using iteratorRecord as the argument.
        self.AssignmentElementList.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
        return self.AssignmentElisionElement.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
# ------------------------------------ 𝑨𝒔𝒔𝒊𝒈𝒏𝒎𝒆𝒏𝒕𝑬𝒍𝒊𝒔𝒊𝒐𝒏𝑬𝒍𝒆𝒎𝒆𝒏𝒕 ------------------------------------
class PN_AssignmentElisionElement(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('AssignmentElisionElement', p)
class PN_AssignmentElisionElement_AssignmentElement(PN_AssignmentElisionElement):
    @property
    def AssignmentElement(self):
        return self.children[0]
class PN_AssignmentElisionElement_Elision_AssignmentElement(PN_AssignmentElisionElement):
    @property
    def Elision(self):
        return self.children[0]
    @property
    def AssignmentElement(self):
        return self.children[1]
    def IteratorDestructuringAssignmentEvaluation(self, iteratorRecord):
        # 12.15.5.5 Runtime Semantics: IteratorDestructuringAssignmentEvaluation
        #   With parameter iteratorRecord.
        # AssignmentElisionElement : Elision AssignmentElement
        #   1. Perform ? IteratorDestructuringAssignmentEvaluation of Elision with iteratorRecord as the argument.
        #   2. Return the result of performing IteratorDestructuringAssignmentEvaluation of AssignmentElement with
        #      iteratorRecord as the argument.
        self.Elision.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
        return self.AssignmentElement.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
# ------------------------------------ 𝑨𝒔𝒔𝒊𝒈𝒏𝒎𝒆𝒏𝒕𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚 ------------------------------------
class PN_AssignmentProperty(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('AssignmentProperty', p)
class PN_AssignmentProperty_IdentifierReference_Initializer(PN_AssignmentProperty):
    @property
    def IdentifierReference(self):
        return self.children[0]
    @property
    def Initializer(self):
        return self.children[1]
    def EarlyErrors(self):
        # 12.15.5.1 Static Semantics: Early Errors
        # AssignmentProperty : IdentifierReference Initializer
        #   * It is a Syntax Error if IsValidSimpleAssignmentTarget of IdentifierReference is false.
        if not self.IdentifierReference.IsValidSimpleAssignmentTarget():
            return [CreateSyntaxError(f'Bad assignment target: {self.IdentifierReference}')]
        return []
    def PropertyDestructuringAssignmentEvaluation(self, value):
        # 12.15.5.3 Runtime Semantics: PropertyDestructuringAssignmentEvaluation
        #   With parameter value.
        # AssignmentProperty : IdentifierReference Initializer
        #   1. Let P be StringValue of IdentifierReference.
        #   2. Let lref be ? ResolveBinding(P).
        #   3. Let v be ? GetV(value, P).
        #   4. If Initializer is present and v is undefined, then
        #       a. Let defaultValue be the result of evaluating Initializer.
        #       b. Set v to ? GetValue(defaultValue).
        #       c. If IsAnonymousFunctionDefinition(Initializer) is true, then
        #           i. Let hasNameProperty be ? HasOwnProperty(v, "name").
        #           ii. If hasNameProperty is false, perform SetFunctionName(v, P).
        #   5. Perform ? PutValue(lref, v).
        #   6. Return a new List containing P.
        P = self.IdentifierReference.StringValue()
        lref = ResolveBinding(P)
        v = GetV(value, P)
        if self.Initializer and v is None:
            defaultValue = self.Initializer.evaluate()
            v = GetValue(defaultValue)
            if IsAnonymousFunctionDefinition(self.Initializer):
                hasNameProperty = HasOwnProperty(v, 'name')
                if not hasNameProperty:
                    SetFunctionName(v, P)
        PutValue(lref, v)
        return [P]
class PN_AssignmentProperty_IdentifierReference(PN_AssignmentProperty_IdentifierReference_Initializer):
    @property
    def Initializer(self):
        return None
class PN_AssignmentProperty_PropertyName_COLON_AssignmentElement(PN_AssignmentProperty):
    @property
    def PropertyName(self):
        return self.children[0]
    @property
    def AssignmentElement(self):
        return self.children[2]
    def PropertyDestructuringAssignmentEvaluation(self, value):
        # 12.15.5.3 Runtime Semantics: PropertyDestructuringAssignmentEvaluation
        #   With parameter value.
        # AssignmentProperty : PropertyName : AssignmentElement
        #   1. Let name be the result of evaluating PropertyName.
        #   2. ReturnIfAbrupt(name).
        #   3. Perform ? KeyedDestructuringAssignmentEvaluation of AssignmentElement with value and name as the arguments.
        #   4. Return a new List containing name.
        name = self.PropertyName.evaluate()
        self.AssignmentElement.KeyedDestructuringAssignmentEvaluation(value, name)
        return [name]
# ------------------------------------ 𝑨𝒔𝒔𝒊𝒈𝒏𝒎𝒆𝒏𝒕𝑬𝒍𝒆𝒎𝒆𝒏𝒕 ------------------------------------
class PN_AssignmentElement(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('AssignmentElement', p)
class PN_AssignmentElement_DestructuringAssignmentTarget_Initializer(PN_AssignmentElement):
    def __init__(self, ctx, p):
        super().__init__(ctx, p)
        self.ctx = ctx
    @property
    def DestructuringAssignmentTarget(self):
        return self.children[0]
    @property
    def Initializer(self):
        return self.children[1]
    def IteratorDestructuringAssignmentEvaluation(self, iteratorRecord):
        # 12.15.5.5 Runtime Semantics: IteratorDestructuringAssignmentEvaluation
        #   With parameter iteratorRecord.
        # AssignmentElement : DestructuringAssignmentTarget Initializer
        #   1. If DestructuringAssignmentTarget is neither an ObjectLiteral nor an ArrayLiteral, then
        #       a. Let lref be the result of evaluating DestructuringAssignmentTarget.
        #       b. ReturnIfAbrupt(lref).
        #   2. If iteratorRecord.[[Done]] is false, then
        #       a. Let next be IteratorStep(iteratorRecord).
        #       b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #       c. ReturnIfAbrupt(next).
        #       d. If next is false, set iteratorRecord.[[Done]] to true.
        #       e. Else,
        #           i. Let value be IteratorValue(next).
        #           ii. If value is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #           iii. ReturnIfAbrupt(value).
        #   3. If iteratorRecord.[[Done]] is true, let value be undefined.
        #   4. If Initializer is present and value is undefined, then
        #       a. Let defaultValue be the result of evaluating Initializer.
        #       b. Let v be ? GetValue(defaultValue).
        #   5. Else, let v be value.
        #   6. If DestructuringAssignmentTarget is an ObjectLiteral or an ArrayLiteral, then
        #       a. Let nestedAssignmentPattern be the AssignmentPattern that is covered by DestructuringAssignmentTarget.
        #       b. Return the result of performing DestructuringAssignmentEvaluation of nestedAssignmentPattern with v as the argument.
        #   7. If Initializer is present and value is undefined and IsAnonymousFunctionDefinition(Initializer) and IsIdentifierRef of DestructuringAssignmentTarget are both true, then
        #       a. Let hasNameProperty be ? HasOwnProperty(v, "name").
        #       b. If hasNameProperty is false, perform SetFunctionName(v, GetReferencedName(lref)).
        #   8. Return ? PutValue(lref, v).
        is_structured_literal = self.DestructuringAssignmentTarget.Is('ObjectLiteral') or self.DestructuringAssignmentTarget.Is('ArrayLiteral')
        if not is_structured_literal:
            lref = self.DestructuringAssignmentTarget.evaluate()
        if not iteratorRecord.Done:
            try:
                nextStep = IteratorStep(iteratorRecord)
                if not nextStep:
                    iteratorRecord.Done = True
                else:
                    value = IteratorValue(nextStep)
            except (ESError, ESAbrupt):
                iteratorRecord.Done = True
                raise
        if iteratorRecord.Done:
            value = None
        if self.Initializer and value is None:
            defaultValue = self.Initializer.evaluate()
            v = GetValue(defaultValue)
        else:
            v = value
        if is_structured_literal:
            nestedAssignmentPattern = self.DestructuringAssignmentTarget.covering('AssignmentPattern')
            return nestedAssignmentPattern.DestructuringAssignmentEvaluation(v)
        if self.Initializer and value is None and IsAnonymousFunctionDefinition(self.Initializer) and self.DestructuringAssignmentTarget.IsIdentifierRef():
            if not HasOwnProperty(v, 'name'):
                SetFunctionName(v, GetReferencedName(lref))
        return PutValue(lref, v)
        # NOTE
        # Left to right evaluation order is maintained by evaluating a DestructuringAssignmentTarget that is not a
        # destructuring pattern prior to accessing the iterator or evaluating the Initializer.
    def KeyedDestructuringAssignmentEvaluation(self, value, propertyName):
        # 12.15.5.6 Runtime Semantics: KeyedDestructuringAssignmentEvaluation
        #   With parameters value and propertyName.
        # AssignmentElement : DestructuringAssignmentTarget Initializer
        #   1. If DestructuringAssignmentTarget is neither an ObjectLiteral nor an ArrayLiteral, then
        #       a. Let lref be the result of evaluating DestructuringAssignmentTarget.
        #       b. ReturnIfAbrupt(lref).
        #   2. Let v be ? GetV(value, propertyName).
        #   3. If Initializer is present and v is undefined, then
        #       a. Let defaultValue be the result of evaluating Initializer.
        #       b. Let rhsValue be ? GetValue(defaultValue).
        #   4. Else, let rhsValue be v.
        #   5. If DestructuringAssignmentTarget is an ObjectLiteral or an ArrayLiteral, then
        #       a. Let assignmentPattern be the AssignmentPattern that is covered by DestructuringAssignmentTarget.
        #       b. Return the result of performing DestructuringAssignmentEvaluation of assignmentPattern with rhsValue as the argument.
        #   6. If Initializer is present and v is undefined and IsAnonymousFunctionDefinition(Initializer) and IsIdentifierRef of DestructuringAssignmentTarget are both true, then
        #       a. Let hasNameProperty be ? HasOwnProperty(rhsValue, "name").
        #       b. If hasNameProperty is false, perform SetFunctionName(rhsValue, GetReferencedName(lref)).
        #   7. Return ? PutValue(lref, rhsValue).
        is_structured_literal = self.DestructuringAssignmentTarget.Is('ObjectLiteral') or self.DestructuringAssignmentTarget.Is('ArrayLiteral')
        if not is_structured_literal:
            lref = self.DestructuringAssignmentTarget.evaluate()
        v = GetV(value, propertyName)
        if self.Initializer and v is None:
            defaultValue = self.Initializer.evaluate()
            rhsValue = GetValue(defaultValue)
        else:
            rhsValue = v
        if is_structured_literal:
            assignmentPattern = self.DestructuringAssignmentTarget.covering('AssignmentPattern')
            return assignmentPattern.DestructuringAssignmentEvaluation(rhsValue)
        if self.Initializer and v is None and IsAnonymousFunctionDefinition(self.Initializer) and self.DestructuringAssignmentTarget.IsIdentifierRef():
            if not HasOwnProperty(rhsValue, 'name'):
                SetFunctionName(rhsValue, GetReferencedName(lref))
        return PutValue(lref, rhsValue)
class PN_AssignmentElement_DestructuringAssignmentTarget(PN_AssignmentElement_DestructuringAssignmentTarget_Initializer):
    @property
    def Initializer(self):
        return None
# ------------------------------------ 𝑨𝒔𝒔𝒊𝒈𝒏𝒎𝒆𝒏𝒕𝑹𝒆𝒔𝒕𝑬𝒍𝒆𝒎𝒆𝒏𝒕 ------------------------------------
class PN_AssignmentRestElement(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('AssignmentRestElement', p)
class PN_AssignmentRestElement_DOTDOTDOT_DestructuringAssignmentTarget(PN_AssignmentRestElement):
    @property
    def DestructuringAssignmentTarget(self):
        return self.children[1]
    def IteratorDestructuringAssignmentEvaluation(self, iteratorRecord):
        # 12.15.5.5 Runtime Semantics: IteratorDestructuringAssignmentEvaluation
        #   With parameter iteratorRecord.
        # AssignmentRestElement : ... DestructuringAssignmentTarget
        #   1. If DestructuringAssignmentTarget is neither an ObjectLiteral nor an ArrayLiteral, then
        #       a. Let lref be the result of evaluating DestructuringAssignmentTarget.
        #       b. ReturnIfAbrupt(lref).
        #   2. Let A be ! ArrayCreate(0).
        #   3. Let n be 0.
        #   4. Repeat, while iteratorRecord.[[Done]] is false,
        #       a. Let next be IteratorStep(iteratorRecord).
        #       b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #       c. ReturnIfAbrupt(next).
        #       d. If next is false, set iteratorRecord.[[Done]] to true.
        #       e. Else,
        #           i. Let nextValue be IteratorValue(next).
        #           ii. If nextValue is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #           iii. ReturnIfAbrupt(nextValue).
        #           iv. Let status be CreateDataProperty(A, ! ToString(n), nextValue).
        #           v. Assert: status is true.
        #           vi. Increment n by 1.
        #   5. If DestructuringAssignmentTarget is neither an ObjectLiteral nor an ArrayLiteral, then
        #       a. Return ? PutValue(lref, A).
        #   6. Let nestedAssignmentPattern be the AssignmentPattern that is covered by DestructuringAssignmentTarget.
        #   7. Return the result of performing DestructuringAssignmentEvaluation of nestedAssignmentPattern with A as the argument.
        is_structured_literal = self.DestructuringAssignmentTarget.Is('ObjectLiteral') or self.DestructuringAssignmentTarget.Is('ArrayLiteral')
        if not is_structured_literal:
            lref = self.DestructuringAssignmentTarget.evaluate()
        A = ArrayCreate(0)
        n = 0
        while not iteratorRecord.Done:
            try:
                next_step = IteratorStep(iteratorRecord)
                if not next_step:
                    iteratorRecord.Done = True
                else:
                    nextValue = IteratorValue(next_step)
            except (ESError, ESAbrupt):
                iteratorRecord.Done = True
                raise
            status = CreateDataProperty(A, ToString(n), nextValue)
            assert status
            n += 1
        if not is_structured_literal:
            return PutValue(lref, A)
        nestedAssignmentPattern = self.DestructuringAssignmentTarget.covering('AssignmentPattern')
        return nestedAssignmentPattern.DestructuringAssignmentEvaluation(A)
# ------------------------------------ 𝑫𝒆𝒔𝒕𝒓𝒖𝒄𝒕𝒖𝒓𝒊𝒏𝒈𝑨𝒔𝒔𝒊𝒈𝒏𝒎𝒆𝒏𝒕𝑻𝒂𝒓𝒈𝒆𝒕 ------------------------------------
class PN_DestructuringAssignmentTarget(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('DestructuringAssignmentTarget', p)
class PN_DestructuringAssignmentTarget_LeftHandSideExpression(PN_DestructuringAssignmentTarget):
    @property
    def LeftHandSideExpression(self):
        return self.children[0]
    def EarlyErrors(self):
        # 12.15.5.1 Static Semantics: Early Errors
        # DestructuringAssignmentTarget : LeftHandSideExpression
        #   * It is a Syntax Error if LeftHandSideExpression is either an ObjectLiteral or an ArrayLiteral and if
        #     LeftHandSideExpression is not covering an AssignmentPattern.
        #   * It is a Syntax Error if LeftHandSideExpression is neither an ObjectLiteral nor an ArrayLiteral and
        #     IsValidSimpleAssignmentTarget(LeftHandSideExpression) is false.
        errs = []
        is_structured_literal = self.LeftHandSideExpression.Is('ObjectLiteral') or self.LeftHandSideExpression.Is('ArrayLiteral')
        if is_structured_literal and not self.LeftHandSideExpression.covering('AssignmentPattern'):
            errs.append(CreateSyntaxError(f'Invalid Assignment Pattern: {self.LeftHandSideExpression}'))
        if not is_structured_literal and not self.LeftHandSideExpression.IsValidSimpleAssignmentTarget():
            errs.append(CreateSyntaxError(f'Invalid Assignment Target: {self.LeftHandSideExpression}'))
        return errs

##################################################################################################################################################################################################################
#
#  d888    .d8888b.       d888    .d8888b.       .d8888b.                                                     .d88888b.                                     888                           .d88             88b.
# d8888   d88P  Y88b     d8888   d88P  Y88b     d88P  Y88b                                                   d88P" "Y88b                                    888                          d88P"             "Y88b
#   888          888       888   888            888    888                                                   888     888                                    888                         d88P                 Y88b
#   888        .d88P       888   888d888b.      888         .d88b.  88888b.d88b.  88888b.d88b.   8888b.      888     888 88888b.   .d88b.  888d888  8888b.  888888  .d88b.  888d888     888                   888
#   888    .od888P"        888   888P "Y88b     888        d88""88b 888 "888 "88b 888 "888 "88b     "88b     888     888 888 "88b d8P  Y8b 888P"       "88b 888    d88""88b 888P"       888                   888
#   888   d88P"            888   888    888     888    888 888  888 888  888  888 888  888  888 .d888888     888     888 888  888 88888888 888     .d888888 888    888  888 888         Y88b                 d88P
#   888   888"       d8b   888   Y88b  d88P     Y88b  d88P Y88..88P 888  888  888 888  888  888 888  888     Y88b. .d88P 888 d88P Y8b.     888     888  888 Y88b.  Y88..88P 888          Y88b.     d8b     .d88P
# 8888888 888888888  Y8P 8888888  "Y8888P"       "Y8888P"   "Y88P"  888  888  888 888  888  888 "Y888888      "Y88888P"  88888P"   "Y8888  888     "Y888888  "Y888  "Y88P"  888           "Y88     88P     88P"
#                                                                                                                        888                                                                       8P
#                                                                                                                        888                                                                       "
#                                                                                                                        888
#
##################################################################################################################################################################################################################
class PN_Expression_AssignmentExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('Expression', p)
class PN_Expression_Expression_COMMA_AssignmentExpression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('Expression', p)
    def IsFunctionDefinition(self):
        return False
    def IsValidSimpleAssignmentTarget(self):
        return False
    def evaluate(self):
        lref = self.children[0].evaluate()
        GetValue(lref) # Have to run, thanks to side effect.
        rref = self.children[2].evaluate()
        return GetValue(rref)
################################################################################################################################################################################3################################################################################################################################################################################3###########################################################################
#
#  d888    .d8888b.      8888888888  .d8888b.  888b     d888        d8888  .d8888b.                   d8b          888        888                                                                              .d8888b.  888             888                                             888                                        888     8888888b.                    888                           888    d8b
# d8888   d88P  Y88b     888        d88P  Y88b 8888b   d8888       d88888 d88P  Y88b                  Y8P          888        888                                                                             d88P  Y88b 888             888                                             888                                        888     888  "Y88b                   888                           888    Y8P
#   888        .d88P     888        888    888 88888b.d88888      d88P888 Y88b.                                    888        888                                                                             Y88b.      888             888                                             888                                        888     888    888                   888                           888
#   888       8888"      8888888    888        888Y88888P888     d88P 888  "Y888b.    .d8888b 888d888 888 88888b.  888888     888       8888b.  88888b.   .d88b.  888  888  8888b.   .d88b.   .d88b.  d8b      "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888 .d8888b       8888b.  88888b.   .d88888     888    888  .d88b.   .d8888b 888  8888b.  888d888  8888b.  888888 888  .d88b.  88888b.  .d8888b
#   888        "Y8b.     888        888        888 Y888P 888    d88P  888     "Y88b. d88P"    888P"   888 888 "88b 888        888          "88b 888 "88b d88P"88b 888  888     "88b d88P"88b d8P  Y8b Y8P         "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888    88K              "88b 888 "88b d88" 888     888    888 d8P  Y8b d88P"    888     "88b 888P"       "88b 888    888 d88""88b 888 "88b 88K
#   888   888    888     888        888    888 888  Y8P  888   d88P   888       "888 888      888     888 888  888 888        888      .d888888 888  888 888  888 888  888 .d888888 888  888 88888888               "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888    "Y8888b.     .d888888 888  888 888  888     888    888 88888888 888      888 .d888888 888     .d888888 888    888 888  888 888  888 "Y8888b.
#   888   Y88b  d88P     888        Y88b  d88P 888   "   888  d8888888888 Y88b  d88P Y88b.    888     888 888 d88P Y88b.      888      888  888 888  888 Y88b 888 Y88b 888 888  888 Y88b 888 Y8b.     d8b     Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.       X88     888  888 888  888 Y88b 888     888  .d88P Y8b.     Y88b.    888 888  888 888     888  888 Y88b.  888 Y88..88P 888  888      X88
# 8888888  "Y8888P"      8888888888  "Y8888P"  888       888 d88P     888  "Y8888P"   "Y8888P 888     888 88888P"   "Y888     88888888 "Y888888 888  888  "Y88888  "Y88888 "Y888888  "Y88888  "Y8888  Y8P      "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888  88888P'     "Y888888 888  888  "Y88888     8888888P"   "Y8888   "Y8888P 888 "Y888888 888     "Y888888  "Y888 888  "Y88P"  888  888  88888P'
#                                                                                                         888                                                 888                        888
#                                                                                                         888                                            Y8b d88P                   Y8b d88P
#                                                                                                         888                                             "Y88P"                     "Y88P"
#
################################################################################################################################################################################3################################################################################################################################################################################3###########################################################################
class PN_ExpressionStatement_Expression(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ExpressionStatement', p)
    def evaluate(self):
        exprRef = self.children[0].evaluate()
        return GetValue(exprRef)
class PN_EmptyStatement_SEMICOLON(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('EmptyStatement', p)
    def evaluate(self):
        return Empty.EMPTY
class PN_Statement(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('Statement', p)
class PN_Statement_ExpressionStatement(PN_Statement):
    def VarDeclaredNames(self):
        return []
    def VarScopedDeclarations(self):
        return []
    def ContainsDuplicateLabels(self, labelSet):
        return False
    def ContainsUndefinedBreakTarget(self, labelSet):
        return False
    def ContainsUndefinedContinueTarget(self, iterationSet, labelSet):
        return False
class PN_Statement_EmptyStatement(PN_Statement):
    def VarDeclaredNames(self):
        return []
    def VarScopedDeclarations(self):
        return []
    def ContainsDuplicateLabels(self, labelSet):
        return False
    def ContainsUndefinedBreakTarget(self, labelSet):
        return False
    def ContainsUndefinedContinueTarget(self, iterationSet, labelSet):
        return False
class PN_Statement_COLON_LabelledStatement(PN_Statement):
    pass
class PN_Statement_BlockStatement(PN_Statement):
    pass
class PN_Statement_VariableStatement(PN_Statement):
    def ContainsDuplicateLabels(self, lst):
        return False
    def ContainsUndefinedBreakTarget(self, lst):
        return False
    def ContainsUndefinedContinueTarget(self, iterationSet, labelSet):
        return False
class PN_Statement_IfStatement(PN_Statement):
    pass
class PN_Statement_BreakableStatement(PN_Statement):
    pass
class PN_Statement_ContinueStatement(PN_Statement):
    def ContainsDuplicateLabels(self, lst):
        return False
    def ContainsUndefinedBreakTarget(self, labelSet):
        return False
    def VarDeclaredNames(self):
        return []
    def VarScopedDeclarations(self):
        return []

class PN_BreakableStatement(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('BreakableStatement', p)
class PN_BreakableStatement_IterationStatement(PN_BreakableStatement):
    @property
    def IterationStatement(self):
        return self.children[0]
    def ContainsUndefinedContinueTarget(self, iterationSet, labelSet):
        # 13.1.3 Static Semantics: ContainsUndefinedContinueTarget
        #   With parameters iterationSet and labelSet.
        #           BreakableStatement : IterationStatement
        # 1. Let newIterationSet be a copy of iterationSet with all the elements of labelSet appended.
        # 2. Return ContainsUndefinedContinueTarget of IterationStatement with arguments newIterationSet and « ».
        newIterationSet = iterationSet.copy()
        newIterationSet.extend(labelSet)
        return self.IterationStatement.ContainsUndefinedContinueTarget(newIterationSet, [])
    def LabelledEvaluation(self, labelSet):
        # 13.1.7 Runtime Semantics: LabelledEvaluation
        #   With parameter labelSet.
        #           BreakableStatement : IterationStatement
        # 1. Let stmtResult be the result of performing LabelledEvaluation of IterationStatement with argument labelSet.
        # 2. If stmtResult.[[Type]] is break, then
        #     a. If stmtResult.[[Target]] is empty, then
        #        i. If stmtResult.[[Value]] is empty, set stmtResult to undefined)
        #       ii. Else, set stmtResult to stmtResult.[[Value]])
        # 3. Return Completion(stmtResult).
        try:
            stmtResult = self.IterationStatement.LabelledEvaluation(labelSet)
        except ESBreak as abrupt:
            if abrupt.completion.target != Empty.EMPTY:
                raise
            stmtResult = abrupt.completion.value if abrupt.completion.value != Empty.EMPTY else None
        return stmtResult
    def evaluate(self):
        # 13.1.8 Runtime Semantics: Evaluation
        #           BreakableStatement : IterationStatement
        # 1. Let newLabelSet be a new empty List.
        # 2. Return the result of performing LabelledEvaluation of this BreakableStatement with argument newLabelSet.
        return self.LabelledEvaluation([])
class PN_BreakableStatement_SwitchStatement(PN_BreakableStatement):
    @property
    def SwitchStatement(self):
        return self.children[0]
    def LabelledEvaluation(self, labelSet):
        # 13.1.7 Runtime Semantics: LabelledEvaluation
        #   With parameter labelSet.
        #           BreakableStatement : SwitchStatement
        # 1. Let stmtResult be the result of evaluating SwitchStatement.
        # 2. If stmtResult.[[Type]] is break, then
        #    a. If stmtResult.[[Target]] is empty, then
        #       i. If stmtResult.[[Value]] is empty, set stmtResult to NormalCompletion(undefined).
        #      ii. Else, set stmtResult to NormalCompletion(stmtResult.[[Value]]).
        # 3. Return Completion(stmtResult).
        try:
            stmtResult = self.SwitchStatement.evaluate()
        except ESBreak as abrupt:
            if abrupt.completion.target != Empty.EMPTY:
                raise
            stmtResult = abrupt.completion.value if abrupt.completion.value != Empty.EMPTY else None
        return stmtResult
    def evaluate(self):
        # 13.1.8 Runtime Semantics: Evaluation
        #           BreakableStatement : SwitchStatement
        # 1. Let newLabelSet be a new empty List.
        # 2. Return the result of performing LabelledEvaluation of this BreakableStatement with argument newLabelSet.
        return self.LabelledEvaluation([])

class PN_Declaration(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('Declaration', p)
class PN_Declaration_LexicalDeclaration(PN_Declaration):
    @property
    def LexicalDeclaration(self):
        return self.children[0]
    def DeclarationPart(self):
        # 13.1.4 Static Semantics: DeclarationPart
        #           Declaration : LexicalDeclaration
        # 1. Return LexicalDeclaration.
        return self.LexicalDeclaration
class PN_Declaration_HoistableDeclaration(PN_Declaration):
    @property
    def HoistableDeclaration(self):
        return self.children[0]

class PN_HoistableDeclaration(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('HoistableDeclaration', p)
class PN_HoistableDeclaration_FunctionDeclaration(PN_HoistableDeclaration):
    @property
    def FunctionDeclaration(self):
        return self.children[0]
    def DeclarationPart(self):
        return self.FunctionDeclaration
    #def evaluate(self):
    #    # 13.1.8 Runtime Semantics: Evaluation
    #    #           HoistableDeclaration : FunctionDeclaration
    #    # 1. Return the result of evaluating FunctionDeclaration.
    # wtf? This happens by default.
##########################################################################################################################
#
#  d888    .d8888b.       .d8888b.      888888b.   888                   888
# d8888   d88P  Y88b     d88P  Y88b     888  "88b  888                   888
#   888        .d88P            888     888  .88P  888                   888
#   888       8888"           .d88P     8888888K.  888  .d88b.   .d8888b 888  888
#   888        "Y8b.      .od888P"      888  "Y88b 888 d88""88b d88P"    888 .88P
#   888   888    888     d88P"          888    888 888 888  888 888      888888K
#   888   Y88b  d88P d8b 888"           888   d88P 888 Y88..88P Y88b.    888 "88b
# 8888888  "Y8888P"  Y8P 888888888      8888888P"  888  "Y88P"   "Y8888P 888  888
#
##########################################################################################################################
class PN_BlockStatement(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('BlockStatement', p)
class PN_Block(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('Block', p)
class PN_StatementList(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('StatementList', p)
class PN_StatementListItem(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('StatementListItem', p)
class PN_Block_LCURLY_StatementList_RCURLY(PN_Block):
    @property
    def StatementList(self):
        return self.children[1]
    def EarlyErrors(self):
        StatementList = self.StatementList
        errs = []
        ldn = StatementList.LexicallyDeclaredNames()
        ldn_set = set(ldn)
        if len(ldn) != len(ldn_set):
            errs.append(CreateSyntaxError('Duplicate names dectected in the Lexically Declared Names list'))
        if not ldn_set.isdisjoint(set(StatementList.VarDeclaredNames())):
            errs.append(CreateSyntaxError('Name clash between Lexically Declared Names and Var Declared Names'))
        return errs
    def evaluate(self):
        # 13.2.13 Runtime Semantics: Evaluation
        # Block : { StatementList }
        #   1. Let oldEnv be the running execution context's LexicalEnvironment.
        #   2. Let blockEnv be NewDeclarativeEnvironment(oldEnv).
        #   3. Perform BlockDeclarationInstantiation(StatementList, blockEnv).
        #   4. Set the running execution context's LexicalEnvironment to blockEnv.
        #   5. Let blockValue be the result of evaluating StatementList.
        #   6. Set the running execution context's LexicalEnvironment to oldEnv.
        #   7. Return blockValue.
        # NOTE 1
        # No matter how control leaves the Block the LexicalEnvironment is always restored to its former state.
        oldEnv = surrounding_agent.running_ec.lexical_environment
        blockEnv = NewDeclarativeEnvironment(oldEnv)
        BlockDeclarationInstantiation(self.StatementList, blockEnv)
        surrounding_agent.running_ec.lexical_environment = blockEnv
        try:
            return self.StatementList.evaluate()
        finally:
            surrounding_agent.running_ec.lexical_environment = oldEnv
class PN_Block_LCURLY_RCURLY(PN_Block):
    def ContainsDuplicateLabels(self, labelSet):
        return False
    def ContainsUndefinedBreakTarget(self, labelSet):
        return False
    def ContainsUndefinedContinueTarget(self, iterationSet, labelSet):
        return False
    def LexicallyDeclaredNames(self):
        return []
    def TopLevelLexicallyScopedDeclarations(self):
        return []
    def TopLevelVarDeclaredNames(self):
        return []
    def TopLevelVarScopedDeclarations(self):
        return []
    def VarDeclaredNames(self):
        return []
    def VarScopedDeclarations(self):
        return []
    def evaluate(self):
        return Empty.EMPTY
class PN_BlockStatement_Block(PN_BlockStatement):
    pass
class PN_StatementListItem_Statement(PN_StatementListItem):
    @property
    def Statement(self):
        return self.children[0]
    def TopLevelLexicallyDeclaredNames(self):
        return []
    def TopLevelVarDeclaredNames(self):
        if isinstance(self.children[0], PN_Statement_COLON_LabelledStatement):
            return self.children[0].TopLevelVarDeclaredNames()
        return self.children[0].VarDeclaredNames()
    def TopLevelVarScopedDeclarations(self):
        if isinstance(self.children[0], PN_Statement_COLON_LabelledStatement):
            return self.children[0].TopLevelVarScopedDeclarations()
        return self.children[0].VarScopedDeclarations()
    def LexicallyScopedDeclarations(self):
        if isinstance(self.children[0], PN_Statement_COLON_LabelledStatement):
            return self.children[2].LexicallyScopedDeclarations()
        return []
    def TopLevelLexicallyScopedDeclarations(self):
        return []
    def LexicallyDeclaredNames(self):
        # 13.2.5 Static Semantics: LexicallyDeclaredNames
        #   StatementListItem : Statement
        #   1. If Statement is Statement : LabelledStatement , return LexicallyDeclaredNames of LabelledStatement.
        Statement = self.Statement
        if len(Statement.children) == 1 and Statement.children[0].name == 'LabelledStatement':
            return Statement.children[0].LexicallyDeclaredNames()
        #   2. Return a new empty List.
        return []
class PN_StatementListItem_Declaration(PN_StatementListItem):
    @property
    def Declaration(self):
        return self.children[0]
    def ContainsDuplicateLabels(self, labelSet):
        # 13.2.2 Static Semantics: ContainsDuplicateLabels
        #   With parameter labelSet.
        #           StatementListItem : Declaration
        # 1. Return false.
        return False
    def ContainsUndefinedBreakTarget(self, labelSet):
        # 13.2.3 Static Semantics: ContainsUndefinedBreakTarget
        #   With parameter labelSet.
        #           StatementListItem : Declaration
        # 1. Return false.
        return False
    def ContainsUndefinedContinueTarget(self, iterationSet, labelSet):
        # 13.2.4 Static Semantics: ContainsUndefinedContinueTarget
        #   With parameters iterationSet and labelSet.
        #           StatementListItem : Declaration
        # 1. Return false.
        return False
    def LexicallyDeclaredNames(self):
        # 13.2.5 Static Semantics: LexicallyDeclaredNames
        #           StatementListItem : Declaration
        # 1. Return the BoundNames of Declaration.
        return self.Declaration.BoundNames()
    def LexicallyScopedDeclarations(self):
        # 13.2.6 Static Semantics: LexicallyScopedDeclarations
        #           StatementListItem : Declaration
        # 1. Return a new List containing DeclarationPart of Declaration.
        return [self.Declaration.DeclarationPart()]
    def TopLevelLexicallyDeclaredNames(self):
        # 13.2.7 Static Semantics: TopLevelLexicallyDeclaredNames
        #           StatementListItem : Declaration
        # 1. If Declaration is Declaration : HoistableDeclaration , then
        #    a. Return « ».
        # 2. Return the BoundNames of Declaration.
        if len(self.Declaration.children) == 1 and self.Declaration.children[0].name == 'HoistableDeclaration':
            return []
        return self.Declaration.BoundNames()
    def TopLevelLexicallyScopedDeclarations(self):
        # 13.2.8 Static Semantics: TopLevelLexicallyScopedDeclarations
        #           StatementListItem : Declaration
        # 1. If Declaration is Declaration:HoistableDeclaration , then
        #    a. Return « ».
        # 2. Return a new List containing Declaration.
        if len(self.Declaration.children) == 1 and self.Declaration.children[0].name == 'HoistableDeclaration':
            return []
        return [self.Declaration]
    def TopLevelVarDeclaredNames(self):
        # 13.2.9 Static Semantics: TopLevelVarDeclaredNames
        #           StatementListItem : Declaration
        # 1. If Declaration is Declaration:HoistableDeclaration , then
        #    a. Return the BoundNames of HoistableDeclaration.
        # 2. Return a new empty List.
        if len(self.Declaration.children) == 1 and self.Declaration.children[0].name == 'HoistableDeclaration':
            return self.Declaration.HoistableDeclaration.BoundNames()
        return []
    def TopLevelVarScopedDeclarations(self):
        # 13.2.10 Static Semantics: TopLevelVarScopedDeclarations
        #           StatementListItem : Declaration
        # 1. If Declaration is Declaration:HoistableDeclaration , then
        #    a. Let declaration be DeclarationPart of HoistableDeclaration.
        #    b. Return « declaration ».
        # 2. Return a new empty List.
        if len(self.Declaration.children) == 1 and self.Declaration.children[0].name == 'HoistableDeclaration':
            return [self.Declaration.HoistableDeclaration.DeclarationPart()]
        return []
    def VarDeclaredNames(self):
        # 13.2.11 Static Semantics: VarDeclaredNames
        #           StatementListItem : Declaration
        # 1. Return a new empty List.
        return []
    def VarScopedDeclarations(self):
        # 13.2.11 Static Semantics: VarScopedDeclarations
        #           StatementListItem : Declaration
        # 1. Return a new empty List.
        return []
class PN_StatementList_StatementListItem(PN_StatementList):
    pass
class PN_StatementList_StatementList_StatementListItem(PN_StatementList):
    @property
    def StatementList(self):
        return self.children[0]
    @property
    def StatementListItem(self):
        return self.children[1]
    def evaluate(self):
        sl = self.StatementList.evaluate()
        s = self.StatementListItem.evaluate()
        return UpdateEmpty(s, sl)
    def TopLevelLexicallyDeclaredNames(self):
        names = self.children[0].TopLevelLexicallyDeclaredNames()
        names.extend(self.children[1].TopLevelLexicallyDeclaredNames())
        return names
    def VarDeclaredNames(self):
        names = self.children[0].VarDeclaredNames()
        names.extend(self.children[1].VarDeclaredNames())
        return names
    def TopLevelVarDeclaredNames(self):
        names = self.children[0].TopLevelVarDeclaredNames()
        names.extend(self.children[1].TopLevelVarDeclaredNames())
        return names
    def VarScopedDeclarations(self):
        declarations = self.children[0].VarScopedDeclarations()
        declarations.extend(self.children[1].VarScopedDeclarations())
        return declarations
    def TopLevelVarScopedDeclarations(self):
        declarations = self.children[0].TopLevelVarScopedDeclarations()
        declarations.extend(self.children[1].TopLevelVarScopedDeclarations())
        return declarations
    def TopLevelLexicallyScopedDeclarations(self):
        declarations = self.children[0].TopLevelLexicallyScopedDeclarations()
        declarations.extend(self.children[1].TopLevelLexicallyScopedDeclarations())
        return declarations
    def ContainsDuplicateLabels(self, labelSet):
        # 13.2.2 StatementList : StatementList StatementListItem
        #    1. Let hasDuplicates be ContainsDuplicateLabels of StatementList with argument labelSet.
        #    2. If hasDuplicates is true, return true.
        #    3. Return ContainsDuplicateLabels of StatementListItem with argument labelSet.
        StatementList = self.children[0]
        StatementListItem = self.children[1]
        return StatementList.ContainsDuplicateLabels(labelSet) or StatementListItem.ContainsDuplicateLabels(labelSet)
    def ContainsUndefinedBreakTarget(self, labelSet):
        # 13.2.3 StatementList : StatementList StatementListItem
        #    1. Let hasUndefinedLabels be ContainsUndefinedBreakTarget of StatementList with argument labelSet.
        #    2. If hasUndefinedLabels is true, return true.
        #    3. Return ContainsUndefinedBreakTarget of StatementListItem with argument labelSet.
        StatementList = self.children[0]
        StatementListItem = self.children[1]
        return  StatementList.ContainsUndefinedBreakTarget(labelSet) or StatementListItem.ContainsUndefinedBreakTarget(labelSet)
    def ContainsUndefinedContinueTarget(self, iterationSet, labelSet):
        # 13.2.4 StatementList : StatementList StatementListItem
        #    1. Let hasUndefinedLabels be ContainsUndefinedContinueTarget of StatementList with arguments iterationSet and « ».
        #    2. If hasUndefinedLabels is true, return true.
        #    3. Return ContainsUndefinedContinueTarget of StatementListItem with arguments iterationSet and « ».
        StatementList = self.children[0]
        StatementListItem = self.children[1]
        return (StatementList.ContainsUndefinedContinueTarget(iterationSet, []) or
                StatementListItem.ContainsUndefinedContinueTarget(iterationSet, []))
    def LexicallyDeclaredNames(self):
        # 13.2.5 StatementList : StatementList StatementListItem
        #    1. Let names be LexicallyDeclaredNames of StatementList.
        #    2. Append to names the elements of the LexicallyDeclaredNames of StatementListItem.
        #    3. Return names.
        StatementList = self.children[0]
        StatementListItem = self.children[1]
        return StatementList.LexicallyDeclaredNames() + StatementListItem.LexicallyDeclaredNames()
    def LexicallyScopedDeclarations(self):
        # 13.2.6 StatementList : StatementList StatementListItem
        #    1. Let declarations be LexicallyScopedDeclarations of StatementList.
        #    2. Append to declarations the elements of the LexicallyScopedDeclarations of StatementListItem.
        #    3. Return declarations.
        StatementList = self.children[0]
        StatementListItem = self.children[1]
        return StatementList.LexicallyScopedDeclarations() + StatementListItem.LexicallyScopedDeclarations()

# 13.2.14 Runtime Semantics: BlockDeclarationInstantiation ( code, env )
def BlockDeclarationInstantiation(code, env):
    # BlockDeclarationInstantiation is performed as follows using arguments code and env. code is the Parse Node
    # corresponding to the body of the block. env is the Lexical Environment in which bindings are to be created.
    # NOTE
    # When a Block or CaseBlock is evaluated a new declarative Environment Record is created and bindings for each
    # block scoped variable, constant, function, or class declared in the block are instantiated in the Environment
    # Record.
    #
    #   1. Let envRec be env's EnvironmentRecord.
    #   2. Assert: envRec is a declarative Environment Record.
    #   3. Let declarations be the LexicallyScopedDeclarations of code.
    #   4. For each element d in declarations, do
    #       a. For each element dn of the BoundNames of d, do
    #           i. If IsConstantDeclaration of d is true, then
    #               1. Perform ! envRec.CreateImmutableBinding(dn, true).
    #           ii. Else,
    #               1. Perform ! envRec.CreateMutableBinding(dn, false).
    #       b. If d is a FunctionDeclaration, a GeneratorDeclaration, an AsyncFunctionDeclaration, or an AsyncGeneratorDeclaration, then
    #           i. Let fn be the sole element of the BoundNames of d.
    #           ii. Let fo be the result of performing InstantiateFunctionObject for d with argument env.
    #           iii. Perform envRec.InitializeBinding(fn, fo).
    envRec = env.environment_record
    assert isinstance(envRec, DeclarativeEnvironmentRecord)
    declarations = code.LexicallyScopedDeclarations()
    for d in declarations:
        for dn in d.BoundNames():
            if d.IsConstantDeclaration():
                envRec.CreateImmutableBinding(dn, True)
            else:
                envRec.CreateMutableBinding(dn, False)
        if d.name in ['FunctionDeclaration', 'GeneratorDeclaration', 'AsyncFunctionDeclaration', 'AsyncGeneratorDeclaration']:
            fn = d.BoundNames()[0]
            fo = d.InstantiateFunctionObject(env)
            envRec.InitializeBinding(fn, fo)
###############################################################################################################################################################################################################################################################################################################################################################
#
#  d888    .d8888b.       .d8888b.      8888888b.                    888                           888    d8b                                                       888     888    888                   888     888                  d8b          888      888               .d8888b.  888             888                                             888
# d8888   d88P  Y88b     d88P  Y88b     888  "Y88b                   888                           888    Y8P                                                       888     888    888                   888     888                  Y8P          888      888              d88P  Y88b 888             888                                             888
#   888        .d88P          .d88P     888    888                   888                           888                                                              888     888    888                   888     888                               888      888              Y88b.      888             888                                             888
#   888       8888"          8888"      888    888  .d88b.   .d8888b 888  8888b.  888d888  8888b.  888888 888  .d88b.  88888b.  .d8888b       8888b.  88888b.   .d88888     888888 88888b.   .d88b.      Y88b   d88P  8888b.  888d888 888  8888b.  88888b.  888  .d88b.       "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
#   888        "Y8b.          "Y8b.     888    888 d8P  Y8b d88P"    888     "88b 888P"       "88b 888    888 d88""88b 888 "88b 88K              "88b 888 "88b d88" 888     888    888 "88b d8P  Y8b      Y88b d88P      "88b 888P"   888     "88b 888 "88b 888 d8P  Y8b         "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
#   888   888    888     888    888     888    888 88888888 888      888 .d888888 888     .d888888 888    888 888  888 888  888 "Y8888b.     .d888888 888  888 888  888     888    888  888 88888888       Y88o88P   .d888888 888     888 .d888888 888  888 888 88888888           "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
#   888   Y88b  d88P d8b Y88b  d88P     888  .d88P Y8b.     Y88b.    888 888  888 888     888  888 Y88b.  888 Y88..88P 888  888      X88     888  888 888  888 Y88b 888     Y88b.  888  888 Y8b.            Y888P    888  888 888     888 888  888 888 d88P 888 Y8b.         Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 8888888  "Y8888P"  Y8P  "Y8888P"      8888888P"   "Y8888   "Y8888P 888 "Y888888 888     "Y888888  "Y888 888  "Y88P"  888  888  88888P'     "Y888888 888  888  "Y88888      "Y888 888  888  "Y8888          Y8P     "Y888888 888     888 "Y888888 88888P"  888  "Y8888       "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
###############################################################################################################################################################################################################################################################################################################################################################
# 13.3.1 Let and Const Declarations
# NOTE
# let and const declarations define variables that are scoped to the running execution context's LexicalEnvironment.
# The variables are created when their containing Lexical Environment is instantiated but may not be accessed in any
# way until the variable's LexicalBinding is evaluated. A variable defined by a LexicalBinding with an Initializer is
# assigned the value of its Initializer's AssignmentExpression when the LexicalBinding is evaluated, not when the
# variable is created. If a LexicalBinding in a let declaration does not have an Initializer the variable is assigned
# the value undefined when the LexicalBinding is evaluated.

class PN_LexicalDeclaration(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('LexicalDeclaration', p)
class PN_LexicalDeclaration_LetOrConst_BindingList_SEMICOLON(PN_LexicalDeclaration):
    @property
    def BindingList(self):
        return self.children[1]
    @property
    def LetOrConst(self):
        return self.children[0]
    def __init__(self, ctx, p):
        super().__init__(ctx, p)
        # We want to have our child LexicalBindings to be able to refer to their containing declaration, so we set
        # that up here.
        bl = self.BindingList
        while bl:
            bl.LexicalBinding.parent_declaration = self
            bl = bl.BindingList

    def EarlyErrors(self):
        # 13.3.1.1 Static Semantics: Early Errors
        #           LexicalDeclaration : LetOrConst BindingList ;
        # 1. It is a Syntax Error if the BoundNames of BindingList contains "let".
        # 2. It is a Syntax Error if the BoundNames of BindingList contains any duplicate entries.
        errs = []
        bn = self.BindingList.BoundNames()
        if 'let' in bn:
            errs.append(CreateSyntaxError("let is disallowed as a lexically bound name"))
        if len(set(bn)) != len(bn):
            errs.append(CreateSyntaxError('duplicate labels not allowed for lexically bound identifiers'))
        return errs
    def BoundNames(self):
        # 13.3.1.2 Static Semantics: BoundNames
        #           LexicalDeclaration : LetOrConst BindingList ;
        # 1. Return the BoundNames of BindingList.
        return self.BindingList.BoundNames()
    def IsConstantDeclaration(self):
        # 13.3.1.3 Static Semantics: IsConstantDeclaration
        #           LexicalDeclaration : LetOrConst BindingList ;
        # 1. Return IsConstantDeclaration of LetOrConst.
        return self.LetOrConst.IsConstantDeclaration()
    def evaluate(self):
        # 13.3.1.4 Runtime Semantics: Evaluation
        #           LexicalDeclaration : LetOrConst BindingList ;
        # 1. Let next be the result of evaluating BindingList.
        # 2. ReturnIfAbrupt(next).
        # 3. Return NormalCompletion(empty).
        self.BindingList.evaluate()
        return Empty.EMPTY

class PN_LetOrConst(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('LetOrConst', p)
class PN_LetOrConst_LET(PN_LetOrConst):
    def IsConstantDeclaration(self):
        # 13.3.1.3 Static Semantics: IsConstantDeclaration
        #           LetOrConst : let
        # 1. Return false.
        return False
class PN_LetOrConst_CONST(PN_LetOrConst):
    def IsConstantDeclaration(self):
        # 13.3.1.3 Static Semantics: IsConstantDeclaration
        #           LetOrConst : const
        # 1. Return true.
        return True

class PN_BindingList(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('BindingList', p)
class PN_BindingList_LexicalBinding(PN_BindingList):
    @property
    def BindingList(self):
        return None
    @property
    def LexicalBinding(self):
        return self.children[0]
class PN_BindingList_BindingList_COMMA_LexicalBinding(PN_BindingList):
    @property
    def BindingList(self):
        return self.children[0]
    @property
    def LexicalBinding(self):
        return self.children[2]
    def BoundNames(self):
        # 13.3.1.2 Static Semantics: BoundNames
        #           BindingList : BindingList , LexicalBinding
        # 1. Let names be the BoundNames of BindingList.
        # 2. Append to names the elements of the BoundNames of LexicalBinding.
        # 3. Return names.
        names = self.BindingList.BoundNames()
        names.extend(self.LexicalBinding.BoundNames())
        return names
    def evaluate(self):
        # 13.3.1.4 Runtime Semantics: Evaluation
        #           BindingList : BindingList , LexicalBinding
        # 1. Let next be the result of evaluating BindingList.
        # 2. ReturnIfAbrupt(next).
        # 3. Return the result of evaluating LexicalBinding.
        self.BindingList.evaluate()
        return self.LexicalBinding.evaluate()

class PN_LexicalBinding(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('LexicalBinding', p)
    @property
    def BindingIdentifier(self):
        raise NotImplementedError('Abstract classes cannot be instantiated')
    def BoundNames(self):
        # 13.3.1.2 Static Semantics: BoundNames
        #           LexicalBinding : BindingIdentifier Initializer
        # 1. Return the BoundNames of BindingIdentifier.
        return self.BindingIdentifier.BoundNames()
class PN_LexicalBinding_BindingIdentifier_Initializer(PN_LexicalBinding):
    @property
    def BindingIdentifier(self):
        return self.children[0]
    @property
    def Initializer(self):
        return self.children[1]
    def evaluate(self):
        # 13.3.1.4 Runtime Semantics: Evaluation
        #           LexicalBinding : BindingIdentifier Initializer
        # 1. Let bindingId be StringValue of BindingIdentifier.
        # 2. Let lhs be ResolveBinding(bindingId).
        # 3. Let rhs be the result of evaluating Initializer.
        # 4. Let value be ? GetValue(rhs).
        # 5. If IsAnonymousFunctionDefinition(Initializer) is true, then
        #    a. Let hasNameProperty be ? HasOwnProperty(value, "name").
        #    b. If hasNameProperty is false, perform SetFunctionName(value, bindingId).
        # 6. Return InitializeReferencedBinding(lhs, value).
        bindingId = self.BindingIdentifier.StringValue()
        lhs = ResolveBinding(bindingId)
        rhs = self.Initializer.evaluate()
        value = GetValue(rhs)
        if IsAnonymousFunctionDefinition(self.Initializer):
            hasNameProperty = HasOwnProperty(value, 'name')
            if not hasNameProperty:
                SetFunctionName(value, bindingId)
        return InitializeReferencedBinding(lhs, value)
class PN_LexicalBinding_BindingIdentifier(PN_LexicalBinding):
    def __init__(self, ctx, p):
        super().__init__(ctx, p)
        self.parent_declaration = None # Set by parent parse node
    @property
    def BindingIdentifier(self):
        return self.children[0]
    @property
    def Initializer(self):
        return None
    def EarlyErrors(self):
        # 13.3.1.1 Static Semantics: Early Errors
        #           LexicalBinding : BindingIdentifier Initializer
        # * It is a Syntax Error if Initializer is not present and IsConstantDeclaration of the LexicalDeclaration
        #   containing this LexicalBinding is true.
        if self.parent_declaration.IsConstantDeclaration():
            return [CreateSyntaxError('constant declarations must have initializers')]
        return []
    def evaluate(self):
        # 13.3.1.4 Runtime Semantics: Evaluation
        #           LexicalBinding : BindingIdentifier
        # 1. Let lhs be ResolveBinding(StringValue of BindingIdentifier).
        # 2. Return InitializeReferencedBinding(lhs, undefined).
        # NOTE
        # A static semantics rule ensures that this form of LexicalBinding never occurs in a const declaration.
        lhs = ResolveBinding(self.BindingIdentifier.StringValue())
        return InitializeReferencedBinding(lhs, None)
class PN_LexicalBinding_BindingPattern_Initializer(PN_LexicalBinding):
    @property
    def BindingPattern(self):
        return self.children[0]
    @property
    def Initializer(self):
        return self.children[1]
    def BoundNames(self):
        # 13.3.1.2 Static Semantics: BoundNames
        #           LexicalBinding : BindingPattern Initializer
        # 1. Return the BoundNames of BindingPattern.
        return self.BindingPattern.BoundNames()
    def evaluate(self):
        # 13.3.1.4 Runtime Semantics: Evaluation
        #           LexicalBinding : BindingPattern Initializer
        # 1. Let rhs be the result of evaluating Initializer.
        # 2. Let value be ? GetValue(rhs).
        # 3. Let env be the running execution context's LexicalEnvironment.
        # 4. Return the result of performing BindingInitialization for BindingPattern using value and env as the
        #    arguments.
        rhs = self.Initializer.evaluate()
        value = GetValue(rhs)
        env = surrounding_agent.running_ec.lexical_environment
        return self.BindingPattern.BindingInitialization(value, env)
#######################################################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.      888     888                  d8b          888      888
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b     888     888                  Y8P          888      888
#   888        .d88P          .d88P            888     888     888                               888      888
#   888       8888"          8888"           .d88P     Y88b   d88P  8888b.  888d888 888  8888b.  88888b.  888  .d88b.
#   888        "Y8b.          "Y8b.      .od888P"       Y88b d88P      "88b 888P"   888     "88b 888 "88b 888 d8P  Y8b
#   888   888    888     888    888     d88P"            Y88o88P   .d888888 888     888 .d888888 888  888 888 88888888
#   888   Y88b  d88P d8b Y88b  d88P d8b 888"              Y888P    888  888 888     888 888  888 888 d88P 888 Y8b.
# 8888888  "Y8888P"  Y8P  "Y8888P"  Y8P 888888888          Y8P     "Y888888 888     888 "Y888888 88888P"  888  "Y8888
#
#  .d8888b.  888             888                                             888
# d88P  Y88b 888             888                                             888
# Y88b.      888             888                                             888
#  "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
#     "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
#       "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
# Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
#  "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
#######################################################################################################################
class PN_VariableStatement(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('VariableStatement', p)
class PN_VariableStatement_VAR_VariableDeclarationList(PN_VariableStatement):
    def VarDeclaredNames(self):
        # 13.3.2.2 Static Semantics: VarDeclaredNames
        #       VariableStatement : var VariableDeclarationList ;
        #   1. Return BoundNames of VariableDeclarationList.
        return self.children[1].BoundNames()
    def evaluate(self):
        # 13.3.2.4 Runtime Semantics: Evaluation
        #       VariableStatement : varVariableDeclarationList ;
        # 1. Let next be the result of evaluating VariableDeclarationList.
        # 2. ReturnIfAbrupt(next).
        # 3. Return NormalCompletion(empty).
        self.children[1].evaluate()
        return Empty.EMPTY
class PN_VariableDeclarationList(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('VariableDeclarationList', p)
class PN_VariableDeclarationList_VariableDeclaration(PN_VariableDeclarationList):
    def VarScopedDeclarations(self):
        # 13.3.2.3 Static Semantics: VarScopedDeclarations
        #       VariableDeclarationList : VariableDeclaration
        #   1. Return a new List containing VariableDeclaration.
        return [self.children[0]]
class PN_VariableDeclarationList_VariableDeclarationList_COMMA_VariableDeclaration(PN_VariableDeclarationList):
    def BoundNames(self):
        # 13.3.2.1 Static Semantics: BoundNames
        #       VariableDeclarationList : VariableDeclarationList , VariableDeclaration
        #   1. Let names be BoundNames of VariableDeclarationList.
        #   2. Append to names the elements of BoundNames of VariableDeclaration.
        #   3. Return names.
        VariableDeclarationList = self.children[0]
        VariableDeclaration = self.children[2]
        return VariableDeclarationList.BoundNames() + VariableDeclaration.BoundNames()
    def VarScopedDeclarations(self):
        # 13.3.2.3 Static Semantics: VarScopedDeclarations
        #       VariableDeclarationList : VariableDeclarationList , VariableDeclaration
        #   1. Let declarations be VarScopedDeclarations of VariableDeclarationList.
        #   2. Append VariableDeclaration to declarations.
        #   3. Return declarations.
        declarations =  self.children[0].VarScopedDeclarations()
        declarations.append(self.children[2])
        return declarations
    def evaluate(self):
        # 13.3.2.4 Runtime Semantics: Evaluation
        #       VariableDeclarationList : VariableDeclarationList , VariableDeclaration
        #   1. Let next be the result of evaluating VariableDeclarationList.
        #   2. ReturnIfAbrupt(next).
        #   3. Return the result of evaluating VariableDeclaration.
        self.children[0].evaluate()
        return self.children[2].evaluate()

# ------------------------------------ 𝑽𝒂𝒓𝒊𝒂𝒃𝒍𝒆𝑫𝒆𝒄𝒍𝒂𝒓𝒂𝒕𝒊𝒐𝒏 ------------------------------------
class PN_VariableDeclaration(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('VariableDeclaration', p)
class PN_VariableDeclaration_BindingIdentifier(PN_VariableDeclaration):
    def evaluate(self):
        # 13.3.2.4 Runtime Semantics: Evaluation
        #       VariableDeclaration : BindingIdentifier
        #   1. Return NormalCompletion(empty).
        return Empty.EMPTY
class PN_VariableDeclaration_BindingIdentifier_Initializer(PN_VariableDeclaration):
    @property
    def BindingIdentifier(self):
        return self.children[0]
    @property
    def Initializer(self):
        return self.children[1]
    def BoundNames(self):
        # 13.3.2.1 Static Semantics: BoundNames
        #   VariableDeclaration : BindingIdentifier Initializer
        #   1. Return the BoundNames of BindingIdentifier.
        return self.BindingIdentifier.BoundNames()
    def evaluate(self):
        # 13.3.2.4 Runtime Semantics: Evaluation
        #       VariableDeclaration : BindingIdentifier Initializer
        #   1. Let bindingId be StringValue of BindingIdentifier.
        #   2. Let lhs be ? ResolveBinding(bindingId).
        #   3. Let rhs be the result of evaluating Initializer.
        #   4. Let value be ? GetValue(rhs).
        #   5. If IsAnonymousFunctionDefinition(Initializer) is true, then
        #       a. Let hasNameProperty be ? HasOwnProperty(value, "name").
        #       b. If hasNameProperty is false, perform SetFunctionName(value, bindingId).
        #   6. Return ? PutValue(lhs, value).
        bindingId = self.BindingIdentifier.StringValue()
        lhs = ResolveBinding(bindingId)
        rhs = self.Initializer.evaluate()
        value = GetValue(rhs)
        if IsAnonymousFunctionDefinition(self.Initializer):
            hasNameProperty = HasOwnProperty(value, 'name')
            if not hasNameProperty:
                SetFunctionName(value, bindingId)
        return PutValue(lhs, value)
class PN_VariableDeclaration_BindingPattern_Initializer(PN_VariableDeclaration):
    @property
    def BindingPattern(self):
        return self.children[0]
    @property
    def Initializer(self):
        return self.children[1]
    def BoundNames(self):
        # 13.3.2.1 Static Semantics: BoundNames
        # VariableDeclaration : BindingPattern Initializer
        #       1. Return the BoundNames of BindingPattern.
        return self.BindingPattern.BoundNames()
    def evaluate(self):
        # 13.3.2.4 Runtime Semantics: Evaluation
        # VariableDeclaration : BindingPattern Initializer
        #       1. Let rhs be the result of evaluating Initializer.
        #       2. Let rval be ? GetValue(rhs).
        #       3. Return the result of performing BindingInitialization for BindingPattern passing rval and undefined
        #          as arguments.
        rhs = self.Initializer.evaluate()
        rval = GetValue(rhs)
        return self.BindingPattern.BindingInitialization(rval, None)
#######################################################################################################################################################################################################################################################################################################
#
#  d888    .d8888b.       .d8888b.       .d8888b.      8888888b.                    888                              888                     d8b                       888888b.   d8b               888 d8b                       8888888b.           888    888
# d8888   d88P  Y88b     d88P  Y88b     d88P  Y88b     888  "Y88b                   888                              888                     Y8P                       888  "88b  Y8P               888 Y8P                       888   Y88b          888    888
#   888        .d88P          .d88P          .d88P     888    888                   888                              888                                               888  .88P                    888                           888    888          888    888
#   888       8888"          8888"          8888"      888    888  .d88b.  .d8888b  888888 888d888 888  888  .d8888b 888888 888  888 888d888 888 88888b.   .d88b.      8888888K.  888 88888b.   .d88888 888 88888b.   .d88b.      888   d88P  8888b.  888888 888888  .d88b.  888d888 88888b.  .d8888b
#   888        "Y8b.          "Y8b.          "Y8b.     888    888 d8P  Y8b 88K      888    888P"   888  888 d88P"    888    888  888 888P"   888 888 "88b d88P"88b     888  "Y88b 888 888 "88b d88" 888 888 888 "88b d88P"88b     8888888P"      "88b 888    888    d8P  Y8b 888P"   888 "88b 88K
#   888   888    888     888    888     888    888     888    888 88888888 "Y8888b. 888    888     888  888 888      888    888  888 888     888 888  888 888  888     888    888 888 888  888 888  888 888 888  888 888  888     888        .d888888 888    888    88888888 888     888  888 "Y8888b.
#   888   Y88b  d88P d8b Y88b  d88P d8b Y88b  d88P     888  .d88P Y8b.          X88 Y88b.  888     Y88b 888 Y88b.    Y88b.  Y88b 888 888     888 888  888 Y88b 888     888   d88P 888 888  888 Y88b 888 888 888  888 Y88b 888     888        888  888 Y88b.  Y88b.  Y8b.     888     888  888      X88
# 8888888  "Y8888P"  Y8P  "Y8888P"  Y8P  "Y8888P"      8888888P"   "Y8888   88888P'  "Y888 888      "Y88888  "Y8888P  "Y888  "Y88888 888     888 888  888  "Y88888     8888888P"  888 888  888  "Y88888 888 888  888  "Y88888     888        "Y888888  "Y888  "Y888  "Y8888  888     888  888  88888P'
#                                                                                                                                                              888                                                        888
#                                                                                                                                                         Y8b d88P                                                   Y8b d88P
#                                                                                                                                                          "Y88P"                                                     "Y88P"
#
#######################################################################################################################################################################################################################################################################################################
# ------------------------------------ 𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑷𝒂𝒕𝒕𝒆𝒓𝒏 ------------------------------------
class PN_BindingPattern(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('BindingPattern', p)
class PN_BindingPattern_ArrayBindingPattern(PN_BindingPattern):
    # -------------------------------- 𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑷𝒂𝒕𝒕𝒆𝒓𝒏 : 𝑨𝒓𝒓𝒂𝒚𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑷𝒂𝒕𝒕𝒆𝒓𝒏 --------------------------------
    @property
    def ArrayBindingPattern(self):
        return self.children[0]
    def BindingInitialization(self, value, environment):
        # 13.3.3.5 Runtime Semantics: BindingInitialization
        #   With parameters value and environment.
        #           BindingPattern : ArrayBindingPattern
        #
        # NOTE
        # When undefined is passed for environment it indicates that a PutValue operation should be used to assign the
        # initialization value. This is the case for formal parameter lists of non-strict functions. In that case the
        # formal parameter bindings are preinitialized in order to deal with the possibility of multiple parameters
        # with the same name.
        #
        # 1. Let iteratorRecord be ? GetIterator(value).
        # 2. Let result be IteratorBindingInitialization for ArrayBindingPattern using iteratorRecord and environment as arguments.
        # 3. If iteratorRecord.[[Done]] is false, return ? IteratorClose(iteratorRecord, result).
        # 4. Return result.
        iteratorRecord = GetIterator(value)
        try:
            result = self.ArrayBindingPattern.IteratorBindingInitialization(iteratorRecord, environment)
        finally:
            if not iteratorRecord.Done:
                IteratorClose(iteratorRecord)
        return result
class PN_BindingPattern_ObjectBindingPattern(PN_BindingPattern):
    # -------------------------------- 𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑷𝒂𝒕𝒕𝒆𝒓𝒏 : 𝑶𝒃𝒋𝒆𝒄𝒕𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑷𝒂𝒕𝒕𝒆𝒓𝒏 --------------------------------
    @property
    def ObjectBindingPattern(self):
        return self.children[0]
    def BindingInitialization(self, value, environment):
        # 13.3.3.5 Runtime Semantics: BindingInitialization
        #   With parameters value and environment.
        #           BindingPattern : ObjectBindingPattern
        #
        # NOTE
        # When undefined is passed for environment it indicates that a PutValue operation should be used to assign the
        # initialization value. This is the case for formal parameter lists of non-strict functions. In that case the
        # formal parameter bindings are preinitialized in order to deal with the possibility of multiple parameters
        # with the same name.
        #
        # 1. Perform ? RequireObjectCoercible(value).
        # 2. Return the result of performing BindingInitialization for ObjectBindingPattern using value and environment
        #    as arguments.
        RequireObjectCoercible(value)
        return self.ObjectBindingPattern.BindingInitialization(value, environment)

# ------------------------------------ 𝑶𝒃𝒋𝒆𝒄𝒕𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑷𝒂𝒕𝒕𝒆𝒓𝒏 ------------------------------------
class PN_ObjectBindingPattern(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ObjectBindingPattern', p)
class PN_ObjectBindingPattern_LCURLY_RCURLY(PN_ObjectBindingPattern):
    # -------------------------------- 𝑶𝒃𝒋𝒆𝒄𝒕𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑷𝒂𝒕𝒕𝒆𝒓𝒏 : { } --------------------------------
    def BoundNames(self):
        # 13.3.3.1 Static Semantics: BoundNames
        #           ObjectBindingPattern : { }
        # 1. Return a new empty List.
        return []
    def ContainsExpression(self):
        # 13.3.3.2 Static Semantics: ContainsExpression
        #           ObjectBindingPattern : { }
        # 1. Return false.
        return False
    def BindingInitialization(self, value, environment):
        # 13.3.3.5 Runtime Semantics: BindingInitialization
        #   With parameters value and environment.
        #           ObjectBindingPattern : { }
        # 1. Return NormalCompletion(empty).
        return Empty.EMPTY
class PN_ObjectBindingPattern_LCURLY_BindingRestProperty_RCURLY(PN_ObjectBindingPattern):
    # -------------------------------- 𝑶𝒃𝒋𝒆𝒄𝒕𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑷𝒂𝒕𝒕𝒆𝒓𝒏 : { 𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑹𝒆𝒔𝒕𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚 } --------------------------------
    @property
    def BindingRestProperty(self):
        return self.children[1]
    def BindingInitialization(self, value, environment):
        # 13.3.3.5 Runtime Semantics: BindingInitialization
        #   With parameters value and environment.
        #           ObjectBindingPattern : { BindingRestProperty }
        # 1. Let excludedNames be a new empty List.
        # 2. Return the result of performing RestBindingInitialization of BindingRestProperty with value, environment,
        #    and excludedNames as the arguments.
        return self.BindingRestProperty.RestBindingInitialization(value, environment, [])
class PN_ObjectBindingPattern_LCURLY_BindingPropertyList_RCURLY(PN_ObjectBindingPattern):
    # -------------------------------- 𝑶𝒃𝒋𝒆𝒄𝒕𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑷𝒂𝒕𝒕𝒆𝒓𝒏 : { 𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚𝑳𝒊𝒔𝒕 } --------------------------------
    @property
    def BindingPropertyList(self):
        return self.children[1]
    def BindingInitialization(self, value, environment):
        # 13.3.3.5 Runtime Semantics: BindingInitialization
        #   With parameters value and environment.
        #           ObjectBindingPattern : { BindingPropertyList }
        # 1. Perform ? PropertyBindingInitialization for BindingPropertyList using value and environment as the arguments.
        # 2. Return NomalCompletion(empty).
        self.BindingPropertyList.PropertyBindingInitialization(value, environment)
        return Empty.EMPTY
class PN_ObjectBindingPattern_LCURLY_BindingPropertyList_COMMA_RCURLY(PN_ObjectBindingPattern):
    @property
    def BindingPropertyList(self):
        return self.children[1]
    def BindingInitialization(self, value, environment):
        # 13.3.3.5 Runtime Semantics: BindingInitialization
        #   With parameters value and environment.
        #           ObjectBindingPattern : { BindingPropertyList , }
        # 1. Perform ? PropertyBindingInitialization for BindingPropertyList using value and environment as the arguments.
        # 2. Return NormalCompletion(empty).
        self.BindingPropertyList.PropertyBindingInitialization(value, environment)
        return Empty.EMPTY
class PN_ObjectBindingPattern_LCURLY_BindingPropertyList_COMMA_BindingRestProperty_RCURLY(PN_ObjectBindingPattern):
    @property
    def BindingPropertyList(self):
        return self.children[1]
    @property
    def BindingRestProperty(self):
        return self.children[3]
    def BindingInitialization(self, value, environment):
        # 13.3.3.5 Runtime Semantics: BindingInitialization
        #   With parameters value and environment.
        #           ObjectBindingPattern : { BindingPropertyList , BindingRestProperty }
        # 1. Let excludedNames be the result of performing ? PropertyBindingInitialization of BindingPropertyList using
        #    value and environment as arguments.
        # 2. Return the result of performing RestBindingInitialization of BindingRestProperty with value, environment,
        #    and excludedNames as the arguments.
        excludedNames = self.BindingPropertyList.PropertyBindingInitialization(value, environment)
        return self.BindingRestProperty.RestBindingInitialization(value, environment, excludedNames)

# ------------------------------------ 𝑨𝒓𝒓𝒂𝒚𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑷𝒂𝒕𝒕𝒆𝒓𝒏 ------------------------------------
class PN_ArrayBindingPattern(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ArrayBindingPattern', p)
    @property
    def BindingRestElement(self):
        raise NotImplementedError('Abstract classes cannot be instantiated')
    @property
    def BindingElementList(self):
        raise NotImplementedError('Abstract classes cannot be instantiated')
    def BoundNames(self):
        # 13.3.3.1 Static Semantics: BoundNames
        #           ArrayBindingPattern : [ Elision ]
        # 1. Return a new empty List.
        #           ArrayBindingPattern : [ Elision BindingRestElement ]
        # 1. Return the BoundNames of BindingRestElement.
        #           ArrayBindingPattern : [ BindingElementList , Elision ]
        # 1. Return the BoundNames of BindingElementList.
        #           ArrayBindingPattern : [ BindingElementList , Elision BindingRestElement ]
        # 1. Let names be BoundNames of BindingElementList.
        # 2. Append to names the elements of BoundNames of BindingRestElement.
        # 3. Return names.
        class NoBoundNames:
            def BoundNames(self):
                return []
        names = (self.BindingElementList or NoBoundNames()).BoundNames()
        names.extend((self.BindingRestElement or NoBoundNames()).BoundNames())
        return names
    def ContainsExpression(self):
        # 13.3.3.2 Static Semantics: ContainsExpression
        #           ArrayBindingPattern : [ Elision ]
        # 1. Return false.
        #           ArrayBindingPattern : [ Elision BindingRestElement ]
        # 1. Return ContainsExpression of BindingRestElement.
        #           ArrayBindingPattern : [ BindingElementList , Elision ]
        # 1. Return ContainsExpression of BindingElementList.
        #           ArrayBindingPattern : [ BindingElementList , Elision BindingRestElement ]
        # 1. Let has be ContainsExpression of BindingElementList.
        # 2. If has is true, return true.
        # 3. Return ContainsExpression of BindingRestElement.
        return (self.BindingElementList is not None and self.BindingElementList.ContainsExpression()
                or
                self.BindingRestElement is not None and self.BindingRestElement.ContainsExpression())
class PN_ArrayBindingPattern_LBRACKET_RBRACKET(PN_ArrayBindingPattern):
    @property
    def BindingRestElement(self):
        return None
    @property
    def BindingElementList(self):
        return None
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # ArrayBindingPattern : [ ]
        #       1. Return NormalCompletion(empty).
        return Empty.EMPTY
class PN_ArrayBindingPattern_LBRACKET_Elision_RBRACKET(PN_ArrayBindingPattern):
    @property
    def Elision(self):
        return self.children[1]
    @property
    def BindingRestElement(self):
        return None
    @property
    def BindingElementList(self):
        return None
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # ArrayBindingPattern : [ Elision ]
        #       1. Return the result of performing IteratorDestructuringAssignmentEvaluation of Elision with
        #          iteratorRecord as the argument.
        return self.Elision.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
class PN_ArrayBindingPattern_LBRACKET_BindingRestElement_RBRACKET(PN_ArrayBindingPattern):
    @property
    def BindingRestElement(self):
        return self.children[1]
    @property
    def BindingElementList(self):
        return None
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # ArrayBindingPattern : [ BindingRestElement ]
        #       1. Return the result of performing IteratorBindingInitialization for BindingRestElement with
        #          iteratorRecord and environment as arguments.
        return self.BindingRestElement.IteratorBindingInitialization(iteratorRecord, environment)
class PN_ArrayBindingPattern_LBRACKET_Elision_BindingRestElement_RBRACKET(PN_ArrayBindingPattern):
    @property
    def Elision(self):
        return self.children[1]
    @property
    def BindingRestElement(self):
        return self.children[2]
    @property
    def BindingElementList(self):
        return None
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # ArrayBindingPattern : [ Elision BindingRestElement ]
        #       1. Perform ? IteratorDestructuringAssignmentEvaluation of Elision with iteratorRecord as the argument.
        #       2. Return the result of performing IteratorBindingInitialization for BindingRestElement with
        #          iteratorRecord and environment as arguments.
        self.Elision.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
        return self.BindingRestElement.IteratorBindingInitialization(iteratorRecord, environment)
class PN_ArrayBindingPattern_LBRACKET_BindingElementList_RBRACKET(PN_ArrayBindingPattern):
    @property
    def BindingElementList(self):
        return self.children[1]
    @property
    def BindingRestElement(self):
        return None
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # ArrayBindingPattern : [ BindingElementList ]
        #       1. Return the result of performing IteratorBindingInitialization for BindingElementList with
        #          iteratorRecord and environment as arguments.
        return self.BindingElementList.IteratorBindingInitialization(iteratorRecord, environment)
class PN_ArrayBindingPattern_LBRACKET_BindingElementList_COMMA_RBRACKET(PN_ArrayBindingPattern):
    @property
    def BindingElementList(self):
        return self.children[1]
    @property
    def BindingRestElement(self):
        return None
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # ArrayBindingPattern : [ BindingElementList , ]
        #       1. Return the result of performing IteratorBindingInitialization for BindingElementList with
        #          iteratorRecord and environment as arguments.
        return self.BindingElementList.IteratorBindingInitialization(iteratorRecord, environment)
class PN_ArrayBindingPattern_LBRACKET_BindingElementList_COMMA_Elision_RBRACKET(PN_ArrayBindingPattern):
    @property
    def BindingElementList(self):
        return self.children[1]
    @property
    def Elision(self):
        return self.children[3]
    @property
    def BindingRestElement(self):
        return None
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # ArrayBindingPattern : [ BindingElementList , Elision ]
        #       1. Perform ? IteratorBindingInitialization for BindingElementList with iteratorRecord and environment
        #          as arguments.
        #       2. Return the result of performing IteratorDestructuringAssignmentEvaluation of Elision with
        #          iteratorRecord as the argument.
        self.BindingElementList.IteratorBindingInitialization(iteratorRecord, environment)
        return self.Elision.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
class PN_ArrayBindingPattern_LBRACKET_BindingElementList_COMMA_BindingRestElement_RBRACKET(PN_ArrayBindingPattern):
    @property
    def BindingElementList(self):
        return self.children[1]
    @property
    def BindingRestElement(self):
        return self.children[3]
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # ArrayBindingPattern : [ BindingElementList , BindingRestElement ]
        #       1. Perform ? IteratorBindingInitialization for BindingElementList with iteratorRecord and environment
        #          as arguments.
        #       2. Return the result of performing IteratorBindingInitialization for BindingRestElement with
        #          iteratorRecord and environment as arguments.
        self.BindingElementList.IteratorBindingInitialization(iteratorRecord, environment)
        return self.BindingRestElement.IteratorBindingInitialization(iteratorRecord, environment)
class PN_ArrayBindingPattern_LBRACKET_BindingElementList_COMMA_Elision_BindingRestElement_RBRACKET(PN_ArrayBindingPattern):
    @property
    def BindingElementList(self):
        return self.children[1]
    @property
    def Elision(self):
        return self.children[3]
    @property
    def BindingRestElement(self):
        return self.children[4]
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # ArrayBindingPattern : [ BindingElementList , Elision BindingRestElement ]
        #       1. Perform ? IteratorBindingInitialization for BindingElementList with iteratorRecord and environment
        #          as arguments.
        #       2. Perform ? IteratorDestructuringAssignmentEvaluation of Elision with iteratorRecord as the argument.
        #       3. Return the result of performing IteratorBindingInitialization for BindingRestElement with
        #          iteratorRecord and environment as arguments.
        self.BindingElementList.IteratorBindingInitialization(iteratorRecord, environment)
        self.Elision.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
        return self.BindingRestElement.IteratorBindingInitialization(iteratorRecord, environment)

# ------------------------------------ 𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑹𝒆𝒔𝒕𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚 ------------------------------------
class PN_BindingRestProperty(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('BindingRestProperty', p)
class PN_BindingRestProperty_DOTDOTDOT_BindingIdentifier(PN_BindingRestProperty):
    @property
    def BindingIdentifier(self):
        return self.children[1]
    def RestBindingInitialization(self, value, environment, excludedNames):
        # 13.3.3.7 Runtime Semantics: RestBindingInitialization
        #       With parameters value, environment, and excludedNames.
        # BindingRestProperty : ... BindingIdentifier
        #       1. Let lhs be ? ResolveBinding(StringValue of BindingIdentifier, environment).
        #       2. Let restObj be ObjectCreate(%ObjectPrototype%).
        #       3. Perform ? CopyDataProperties(restObj, value, excludedNames).
        #       4. If environment is undefined, return PutValue(lhs, restObj).
        #       5. Return InitializeReferencedBinding(lhs, restObj).
        lhs = ResolveBinding(self.BindingIdentifier.StringValue(), environment)
        restObj = ObjectCreate(surrounding_agent.running_ec.realm.intrinsics['%ObjectPrototype%'])
        CopyDataProperties(restObj, value, excludedNames)
        if environment is None:
            return PutValue(lhs, restObj)
        return InitializeReferencedBinding(lhs, restObj)

# ------------------------------------ 𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚𝑳𝒊𝒔𝒕 ------------------------------------
class PN_BindingPropertyList(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('BindingPropertyList', p)
class PN_BindingPropertyList_BindingProperty(PN_BindingPropertyList):
    @property
    def BindingProperty(self):
        return self.children[0]
class PN_BindingPropertyList_BindingPropertyList_COMMA_BindingProperty(PN_BindingPropertyList):
    @property
    def BindingPropertyList(self):
        return self.children[0]
    @property
    def BindingProperty(self):
        return self.children[2]
    def BoundNames(self):
        # 13.3.3.1 Static Semantics: BoundNames
        # BindingPropertyList : BindingPropertyList , BindingProperty
        #       1. Let names be BoundNames of BindingPropertyList.
        #       2. Append to names the elements of BoundNames of BindingProperty.
        #       3. Return names.
        names = self.BindingPropertyList.BoundNames()
        names.extend(self.BindingProperty.BoundNames())
        return names
    def ContainsExpression(self):
        # 13.3.3.2 Static Semantics: ContainsExpression
        # BindingPropertyList : BindingPropertyList , BindingProperty
        #       1. Let has be ContainsExpression of BindingPropertyList.
        #       2. If has is true, return true.
        #       3. Return ContainsExpression of BindingProperty.
        return self.BindingPropertyList.ContainsExpression() or self.BindingProperty.ContainsExpression()
    def PropertyBindingInitialization(self, value, environment):
        # 13.3.3.6 Runtime Semantics: PropertyBindingInitialization
        #   With parameters value and environment.
        #
        # NOTE
        # These collect a list of all bound property names rather than just empty completion.
        #
        # BindingPropertyList : BindingPropertyList , BindingProperty
        #       1. Let boundNames be the result of performing ? PropertyBindingInitialization for BindingPropertyList
        #          using value and environment as arguments.
        #       2. Let nextNames be the result of performing ? PropertyBindingInitialization for BindingProperty using
        #          value and environment as arguments.
        #       3. Append each item in nextNames to the end of boundNames.
        #       4. Return boundNames.
        boundNames = self.BindingPropertyList.PropertyBindingInitialization(value, environment)
        nextNames = self.BindingProperty.PropertyBindingInitialization(value, environment)
        boundNames.extend(nextNames)
        return boundNames

# ------------------------------------ 𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑬𝒍𝒆𝒎𝒆𝒏𝒕𝑳𝒊𝒔𝒕 ------------------------------------
class PN_BindingElementList(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('BindingElementList', p)
class PN_BindingElementList_BindingElisionElement(PN_BindingElementList):
    @property
    def BindingElisionElement(self):
        return self.children[0]
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # BindingElementList : BindingElisionElement
        #       1. Return the result of performing IteratorBindingInitialization for BindingElisionElement with
        #          iteratorRecord and environment as arguments.
        return self.BindingElisionElement.IteratorBindingInitialization(iteratorRecord, environment)
class PN_BindingElementList_BindingElementList_COMMA_BindingElisionElement(PN_BindingElementList):
    @property
    def BindingElementList(self):
        return self.children[0]
    @property
    def BindingElisionElement(self):
        return self.children[2]
    def BoundNames(self):
        # 13.3.3.1 Static Semantics: BoundNames
        # BindingElementList : BindingElementList , BindingElisionElement
        #       1. Let names be BoundNames of BindingElementList.
        #       2. Append to names the elements of BoundNames of BindingElisionElement.
        #       3. Return names.
        names = self.BindingElementList.BoundNames()
        names.extend(self.BindingElisionElement.BoundNames())
        return names
    def ContainsExpression(self):
        # 13.3.3.2 Static Semantics: ContainsExpression
        # BindingElementList : BindingElementList , BindingElisionElement
        #       1. Let has be ContainsExpression of BindingElementList.
        #       2. If has is true, return true.
        #       3. Return ContainsExpression of BindingElisionElement.
        return self.BindingElementList.ContainsExpression() or self.BindingElisionElement.ContainsExpression()
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # BindingElementList : BindingElementList , BindingElisionElement
        #       1. Perform ? IteratorBindingInitialization for BindingElementList with iteratorRecord and environment
        #          as arguments.
        #       2. Return the result of performing IteratorBindingInitialization for BindingElisionElement using
        #          iteratorRecord and environment as arguments.
        self.BindingElementList.IteratorBindingInitialization(iteratorRecord, environment)
        return self.BindingElisionElement.IteratorBindingInitialization(iteratorRecord, environment)

# ------------------------------------ 𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑬𝒍𝒊𝒔𝒊𝒐𝒏𝑬𝒍𝒆𝒎𝒆𝒏𝒕 ------------------------------------
class PN_BindingElisionElement(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('BindingElisionElement', p)
class PN_BindingElisionElement_BindingElement(PN_BindingElisionElement):
    @property
    def BindingElement(self):
        return self.children[0]
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #       With parameters iteratorRecord and environment.
        # BindingElisionElement : BindingElement
        #       1. Return the result of performing IteratorBindingInitialization of BindingElement with iteratorRecord
        #          and environment as the arguments.
        return self.BindingElement.IteratorBindingInitialization(iteratorRecord, environment)
class PN_BindingElisionElement_Elision_BindingElement(PN_BindingElisionElement):
    @property
    def Elision(self):
        return self.children[0]
    @property
    def BindingElement(self):
        return self.children[1]
    def BoundNames(self):
        # 13.3.3.1 Static Semantics: BoundNames
        # BindingElisionElement: Elsision BindingElement
        #       Return BoundNames of BindingElement.
        return self.BindingElement.BoundNames()
    def ContainsExpression(self):
        # 13.3.3.2 Static Semantics: ContainsExpression
        # BindingElisionElement: Elsision BindingElement
        #       1. Return ContainsExpression of BindingElement.
        return self.BindingElement.ContainsExpression()
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #       With parameters iteratorRecord and environment.
        # BindingElisionElement: Elsision BindingElement
        #       1. Perform ? IteratorDestructuringAssignmentEvaluation of Elision with iteratorRecord as the argument.
        #       2. Return the result of performing IteratorBindingInitialization of BindingElement with iteratorRecord
        #          and environment as the arguments.
        self.Elision.IteratorDestructuringAssignmentEvaluation(iteratorRecord)
        return self.BindingElement.IteratorBindingInitialization(iteratorRecord, environment)

# ------------------------------------ 𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒚 ------------------------------------
class PN_BindingProperty(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('BindingProperty', p)
class PN_BindingProperty_SingleNameBinding(PN_BindingProperty):
    @property
    def SingleNameBinding(self):
        return self.children[0]
    def PropertyBindingInitialization(self, value, environment):
        # 13.3.3.6 Runtime Semantics: PropertyBindingInitialization
        #   With parameters value and environment.
        # BindingProperty : SingleNameBinding
        #       1. Let name be the string that is the only element of BoundNames of SingleNameBinding.
        #       2. Perform ? KeyedBindingInitialization for SingleNameBinding using value, environment, and name as
        #          the arguments.
        #       3. Return a new List containing name.
        name = self.SingleNameBinding.BoundNames()[0]
        self.SingleNameBinding.KeyedBindingInitialization(value, environment, name)
        return [name]
class PN_BindingProperty_PropertyName_COLON_BindingElement(PN_BindingProperty):
    @property
    def PropertyName(self):
        return self.children[0]
    @property
    def BindingElement(self):
        return self.children[2]
    def BoundNames(self):
        # 13.3.3.1 Static Semantics: BoundNames
        # BindingProperty : PropertyName : BindingElement
        #       1. Return the BoundNames of BindingElement.
        return self.BindingElement.BoundNames()
    def ContainsExpression(self):
        # 13.3.3.2 Static Semantics: ContainsExpression
        # BindingProperty : PropertyName : BindingElement
        #       1. Let has be IsComputedPropertyKey of PropertyName.
        #       2. If has is true, return true.
        #       3. Return ContainsExpression of BindingElement.
        return self.PropertyName.IsComputedPropertyKey() or self.BindingElement.ContainsExpression()
    def PropertyBindingInitialization(self, value, environment):
        # 13.3.3.6 Runtime Semantics: PropertyBindingInitialization
        #   With parameters value and environment.
        # BindingProperty : PropertyName : BindingElement
        #       1. Let P be the result of evaluating PropertyName.
        #       2. ReturnIfAbrupt(P).
        #       3. Perform ? KeyedBindingInitialization of BindingElement with value, environment, and P as the arguments.
        #       4. Return a new List containing P.
        P = self.PropertyName.evaluate()
        self.BindingElement.KeyedBindingInitialization(value, environment, P)
        return [P]

# ------------------------------------ 𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑬𝒍𝒆𝒎𝒆𝒏𝒕 ------------------------------------
class PN_BindingElement(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('BindingElement', p)
class PN_BindingElement_SingleNameBinding(PN_BindingElement):
    @property
    def SingleNameBinding(self):
        return self.children[0]
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # BindingElement : SingleNameBinding
        #       1. Return the result of performing IteratorBindingInitialization for SingleNameBinding with
        #          iteratorRecord and environment as the arguments.
        return self.SingleNameBinding.IteratorBindingInitialization(iteratorRecord, environment)
class PN_BindingElement_BindingPattern(PN_BindingElement):
    @property
    def BindingPattern(self):
        return self.children[0]
    def HasInitializer(self):
        # 13.3.3.3 Static Semantics: HasInitializer
        # BindingElement : BindingPattern
        #       1. Return false.
        return False
    def IsSimpleParameterList(self):
        # 13.3.3.4 Static Semantics: IsSimpleParameterList
        # BindingElement : BindingPattern
        #       1. Return false.
        return False
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # BindingElement : BindingPattern
        #       1. If iteratorRecord.[[Done]] is false, then
        #          a. Let next be IteratorStep(iteratorRecord).
        #          b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #          c. ReturnIfAbrupt(next).
        #          d. If next is false, set iteratorRecord.[[Done]] to true.
        #          e. Else,
        #             i. Let v be IteratorValue(next).
        #            ii. If v is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #           iii. ReturnIfAbrupt(v).
        #       2. If iteratorRecord.[[Done]] is true, let v be undefined.
        #       3. Return the result of performing BindingInitialization of BindingPattern with v and environment as
        #          the arguments.)
        if not iteratorRecord.Done:
            try:
                next = IteratorStep(iteratorRecord)
            except (ESError, ESAbrupt):
                iteratorRecord.Done = True
                raise
            if not next:
                iteratorRecord.Done = True
            else:
                try:
                    v = IteratorValue(next)
                except (ESError, ESAbrupt):
                    iteratorRecord.Done = True
                    raise
        if iteratorRecord.Done:
            v = None
        return self.BindingPattern.BindingInitialization(v, environment)
    def KeyedBindingInitialization(self, value, environment, propertyName):
        # 13.3.3.9 Runtime Semantics: KeyedBindingInitialization
        #   With parameters value, environment, and propertyName.
        # BindingElement : BindingPattern
        #       1. Let v be ? GetV(value, propertyName).
        #       2. Return the result of performing BindingInitialization for BindingPattern passing v and environment
        #          as arguments.
        v = GetV(value, propertyName)
        return self.BindingPattern.BindingInitialization(v, environment)
class PN_BindingElement_BindingPattern_Initializer(PN_BindingElement):
    @property
    def BindingPattern(self):
        return self.children[0]
    @property
    def Initializer(self):
        return self.children[1]
    def BoundNames(self):
        # 13.3.3.1 Static Semantics: BoundNames
        # BindingElement : BindingPattern Initializer
        #       1. Return the BoundNames of BindingPattern.
        return self.BindingPattern.BoundNames()
    def ContainsExpression(self):
        # 13.3.3.2 Static Semantics: ContainsExpression
        # BindingElement : BindingPattern Initializer
        #       1. Return true.
        return True
    def HasInitializer(self):
        # 13.3.3.3 Static Semantics: HasInitializer
        # BindingElement : BindingPattern Initializer
        #       1. Return true.
        return True
    def IsSimpleParameterList(self):
        # 13.3.3.4 Static Semantics: IsSimpleParameterList
        # BindingElement : BindingPattern Initializer
        #       1. Return false.
        return False
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # BindingElement : BindingPattern Initializer
        #       1. If iteratorRecord.[[Done]] is false, then
        #          a. Let next be IteratorStep(iteratorRecord).
        #          b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #          c. ReturnIfAbrupt(next).
        #          d. If next is false, set iteratorRecord.[[Done]] to true.
        #          e. Else,
        #             i. Let v be IteratorValue(next).
        #            ii. If v is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #           iii. ReturnIfAbrupt(v).
        #       2. If iteratorRecord.[[Done]] is true, let v be undefined.
        #       3. If v is undefined, then
        #          a. Let defaultValue be the result of evaluating Initializer.
        #          b. Set v to ? GetValue(defaultValue).
        #       4. Return the result of performing BindingInitialization of BindingPattern with v and environment as
        #          the arguments.)
        if not iteratorRecord.Done:
            try:
                next = IteratorStep(iteratorRecord)
            except (ESError, ESAbrupt):
                iteratorRecord.Done = True
                raise
            if not next:
                iteratorRecord.Done = True
            else:
                try:
                    v = IteratorValue(next)
                except (ESError, ESAbrupt):
                    iteratorRecord.Done = True
                    raise
        if iteratorRecord.Done:
            v = None
        if v is None:
            defaultValue = self.Initializer.evaluate()
            v = GetValue(defaultValue)
        return self.BindingPattern.BindingInitialization(v, environment)
    def KeyedBindingInitialization(self, value, environment, propertyName):
        # 13.3.3.9 Runtime Semantics: KeyedBindingInitialization
        #   With parameters value, environment, and propertyName.
        # BindingElement : BindingPattern Initializer
        #       1. Let v be ? GetV(value, propertyName).
        #       2. If v is undefined, then
        #          a. Let defaultValue be the result of evaluating Initializer.
        #          b. Set v to ? GetValue(defaultValue).
        #       3. Return the result of performing BindingInitialization for BindingPattern passing v and environment
        #          as arguments.
        v = GetV(value, propertyName)
        if v is None:
            defaultValue = self.Initializer.evaluate()
            v = GetValue(defaultValue)
        return self.BindingPattern.BindingInitialization(v, environment)

# ------------------------------------ 𝑺𝒊𝒏𝒈𝒍𝒆𝑵𝒂𝒎𝒆𝑩𝒊𝒏𝒅𝒊𝒏𝒈 ------------------------------------
class PN_SingleNameBinding(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('SingleNameBinding', p)
class PN_SingleNameBinding_BindingIdentifier(PN_SingleNameBinding):
    @property
    def BindingIdentifier(self):
        return self.children[0]
    def ContainsExpression(self):
        # 13.3.3.2 Static Semantics: ContainsExpression
        # SingleNameBinding : BindingIdentifier
        #       1. Return false.
        return False
    def HasInitializer(self):
        # 13.3.3.3 Static Semantics: HasInitializer
        # SingleNameBinding : BindingIdentifier
        #       1. Return false.
        return False
    def IsSimpleParameterList(self):
        # 13.3.3.4 Static Semantics: IsSimpleParameterList
        # SingleNameBinding : BindingIdentifier
        #       1. Return true.
        return True
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # SingleNameBinding : BindingIdentifier
        #       1. Let bindingId be StringValue of BindingIdentifier.
        #       2. Let lhs be ? ResolveBinding(bindingId, environment).
        #       3. If iteratorRecord.[[Done]] is false, then
        #           a. Let next be IteratorStep(iteratorRecord).
        #           b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #           c. ReturnIfAbrupt(next).
        #           d. If next is false, set iteratorRecord.[[Done]] to true.
        #           e. Else,
        #               i. Let v be IteratorValue(next).
        #               ii. If v is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #               iii. ReturnIfAbrupt(v).
        #       4. If iteratorRecord.[[Done]] is true, let v be undefined.
        #       5. If environment is undefined, return ? PutValue(lhs, v).
        #       6. Return InitializeReferencedBinding(lhs, v).
        bindingId = self.BindingIdentifier.StringValue()
        lhs = ResolveBinding(bindingId, environment)
        if not iteratorRecord.Done:
            try:
                next = IteratorStep(iteratorRecord)
            except (ESError, ESAbrupt):
                iteratorRecord.Done = True
                raise
            if not next:
                iteratorRecord.Done = True
            else:
                try:
                    v = IteratorValue(next)
                except (ESError, ESAbrupt):
                    iteratorRecord.Done = True
                    raise
        if iteratorRecord.Done:
            v = None
        if environment is None:
            return PutValue(lhs, v)
        return InitializeReferencedBinding(lhs, v)
    def KeyedBindingInitialization(self, value, environment, propertyName):
        # 13.3.3.9 Runtime Semantics: KeyedBindingInitialization
        #   With parameters value, environment, and propertyName.
        # SingleNameBinding : BindingIdentifier
        #       1. Let bindingId be StringValue of BindingIdentifier.
        #       2. Let lhs be ? ResolveBinding(bindingId, environment).
        #       3. Let v be ? GetV(value, propertyName).
        #       4. If environment is undefined, return ? PutValue(lhs, v).
        #       5. Return InitializeReferencedBinding(lhs, v).
        bindingId = self.BindingIdentifier.StringValue()
        lhs = ResolveBinding(bindingId, environment)
        v = GetV(value, propertyName)
        if environment is None:
            return PutValue(lhs, v)
        return InitializeReferencedBinding(lhs, v)
class PN_SingleNameBinding_BindingIdentifier_Initializer(PN_SingleNameBinding):
    @property
    def BindingIdentifier(self):
        return self.children[0]
    @property
    def Initializer(self):
        return self.children[1]
    def BoundNames(self):
        # 13.3.3.1 Static Semantics: BoundNames
        # SingleNameBinding : BindingIdentifier Initializer
        #       1. Return the BoundNames of BindingIdentifier.
        return self.BindingIdentifier.BoundNames()
    def ContainsExpression(self):
        # 13.3.3.2 Static Semantics: ContainsExpression
        # SingleNameBinding : BindingIdentifier Initializer
        #       1. Return true.
        return True
    def HasInitializer(self):
        # 13.3.3.3 Static Semantics: HasInitializer
        # SingleNameBinding : BindingIdentifier Initializer
        #       1. Return true.
        return True
    def IsSimpleParameterList(self):
        # 13.3.3.4 Static Semantics: IsSimpleParameterList
        # SingleNameBinding : BindingIdentifier Initializer
        #       1. Return false.
        return False
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        # 13.3.3.8 Runtime Semantics: IteratorBindingInitialization
        #   With parameters iteratorRecord and environment.
        # SingleNameBinding : BindingIdentifier Initializer
        #       1. Let bindingId be StringValue of BindingIdentifier.
        #       2. Let lhs be ? ResolveBinding(bindingId, environment).
        #       3. If iteratorRecord.[[Done]] is false, then
        #           a. Let next be IteratorStep(iteratorRecord).
        #           b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #           c. ReturnIfAbrupt(next).
        #           d. If next is false, set iteratorRecord.[[Done]] to true.
        #           e. Else,
        #               i. Let v be IteratorValue(next).
        #               ii. If v is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #               iii. ReturnIfAbrupt(v).
        #       4. If iteratorRecord.[[Done]] is true, let v be undefined.
        #       5. If v is undefined, then
        #           a. Let defaultValue be the result of evaluating Initializer.
        #           b. Set v to ? GetValue(defaultValue).
        #           c. If IsAnonymousFunctionDefinition(Initializer) is true, then
        #               i. Let hasNameProperty be ? HasOwnProperty(v, "name").
        #               ii. If hasNameProperty is false, perform SetFunctionName(v, bindingId).
        #       6. If environment is undefined, return ? PutValue(lhs, v).
        #       7. Return InitializeReferencedBinding(lhs, v).
        bindingId = self.BindingIdentifier.StringValue()
        lhs = ResolveBinding(bindingId, environment)
        if not iteratorRecord.Done:
            try:
                next = IteratorStep(iteratorRecord)
            except (ESError, ESAbrupt):
                iteratorRecord.Done = True
                raise
            if not next:
                iteratorRecord.Done = True
            else:
                try:
                    v = IteratorValue(next)
                except (ESError, ESAbrupt):
                    iteratorRecord.Done = True
                    raise
        if iteratorRecord.Done:
            v = None
        if v is None:
            defaultValue = self.Initializer.evaluate()
            v = GetValue(defaultValue)
            if IsAnonymousFunctionDefinition(self.Initializer):
                hasNameProperty = HasOwnProperty(v, 'name')
                if not hasNameProperty:
                    SetFunctionName(v, bindingId)
        if environment is None:
            return PutValue(lhs, v)
        return InitializeReferencedBinding(lhs, v)
    def KeyedBindingInitialization(self, value, environment, propertyName):
        # 13.3.3.9 Runtime Semantics: KeyedBindingInitialization
        #   With parameters value, environment, and propertyName.
        # SingleNameBinding : BindingIdentifier Initializer
        #       1. Let bindingId be StringValue of BindingIdentifier.
        #       2. Let lhs be ? ResolveBinding(bindingId, environment).
        #       3. Let v be ? GetV(value, propertyName).
        #       4. If v is undefined, then
        #           a. Let defaultValue be the result of evaluating Initializer.
        #           b. Set v to ? GetValue(defaultValue).
        #           c. If IsAnonymousFunctionDefinition(Initializer) is true, then
        #               i. Let hasNameProperty be ? HasOwnProperty(v, "name").
        #               ii. If hasNameProperty is false, perform SetFunctionName(v, bindingId).
        #       5. If environment is undefined, return ? PutValue(lhs, v).
        #       6. Return InitializeReferencedBinding(lhs, v).
        bindingId = self.BindingIdentifier.StringValue()
        lhs = ResolveBinding(bindingId, environment)
        v = GetV(value, propertyName)
        if v is None:
            defaultValue = self.Initializer.evaluate()
            v = GetValue(defaultValue)
            if IsAnonymousFunctionDefinition(self.Initializer):
                hasNameProperty = HasOwnProperty(v, 'name')
                if not hasNameProperty:
                    SetFunctionName(v, bindingId)
        if environment is None:
            return PutValue(lhs, v)
        return InitializeReferencedBinding(lhs, v)

# ------------------------------------ 𝑩𝒊𝒏𝒅𝒊𝒏𝒈𝑹𝒆𝒔𝒕𝑬𝒍𝒆𝒎𝒆𝒏𝒕 ------------------------------------
class PN_BindingRestElement(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('BindingRestElement', p)
class PN_BindingRestElement_DOTDOTDOT_BindingIdentifier(PN_BindingRestElement):
    @property
    def BindingIdentifier(self):
        return self.children[1]
    def ContainsExpression(self):
        return False
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        #       1. Let lhs be ? ResolveBinding(StringValue of BindingIdentifier, environment).
        #       2. Let A be ! ArrayCreate(0).
        #       3. Let n be 0.
        #       4. Repeat,
        #           a. If iteratorRecord.[[Done]] is false, then
        #               i. Let next be IteratorStep(iteratorRecord).
        #               ii. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #               iii. ReturnIfAbrupt(next).
        #               iv. If next is false, set iteratorRecord.[[Done]] to true.
        #           b. If iteratorRecord.[[Done]] is true, then
        #               i. If environment is undefined, return ? PutValue(lhs, A).
        #               ii. Return InitializeReferencedBinding(lhs, A).
        #           c. Let nextValue be IteratorValue(next).
        #           d. If nextValue is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #           e. ReturnIfAbrupt(nextValue).
        #           f. Let status be CreateDataProperty(A, ! ToString(n), nextValue).
        #           g. Assert: status is true.
        #           h. Increment n by 1.
        lhs = ResolveBinding(self.BindingIdentifier.StringValue(), environment)
        A = ArrayCreate(0)
        n = 0
        while 1:
            if not iteratorRecord.Done:
                try:
                    next = IteratorStep(iteratorRecord)
                except (ESError, ESAbrupt):
                    iteratorRecord.Done = True
                    raise
                if not next:
                    iteratorRecord.Done = True
            if iteratorRecord.Done:
                if environment is None:
                    return PutValue(lhs, A)
                return InitializeReferencedBinding(lhs, A)
            try:
                nextValue = IteratorValue(next)
            except (ESError, ESAbrupt):
                iteratorRecord.Done = True
                raise
            status = CreateDataProperty(A, ToString(n), nextValue)
            assert status
            n += 1
class PN_BindingRestElement_DOTDOTDOT_BindingPattern(PN_BindingRestElement):
    @property
    def BindingPattern(self):
        return self.children[1]
    def ContainsExpression(self):
        return self.BindingPattern.ContainsExpression()
    def IteratorBindingInitialization(self, iteratorRecord, environment):
        #       1. Let A be ! ArrayCreate(0).
        #       2. Let n be 0.
        #       3. Repeat,
        #           a. If iteratorRecord.[[Done]] is false, then
        #               i. Let next be IteratorStep(iteratorRecord).
        #               ii. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #               iii. ReturnIfAbrupt(next).
        #               iv. If next is false, set iteratorRecord.[[Done]] to true.
        #           b. If iteratorRecord.[[Done]] is true, then
        #               i. Return the result of performing BindingInitialization of BindingPattern with A and
        #                  environment as the arguments.
        #           c. Let nextValue be IteratorValue(next).
        #           d. If nextValue is an abrupt completion, set iteratorRecord.[[Done]] to true.
        #           e. ReturnIfAbrupt(nextValue).
        #           f. Let status be CreateDataProperty(A, ! ToString(n), nextValue).
        #           g. Assert: status is true.
        #           h. Increment n by 1.
        A = ArrayCreate(0)
        n = 0
        while 1:
            if not iteratorRecord.Done:
                try:
                    next = IteratorStep(iteratorRecord)
                except (ESError, ESAbrupt):
                    iteratorRecord.Done = True
                    raise
                if not next:
                    iteratorRecord.Done = True
            if iteratorRecord.Done:
                return self.BindingPattern.BindingInitialization(A, environment)
            try:
                nextValue = IteratorValue(next)
            except (ESError, ESAbrupt):
                iteratorRecord.Done = True
                raise
            status = CreateDataProperty(A, ToString(n), nextValue)
            assert status
            n += 1
###########################################################################################################################################################################
#
#  d888    .d8888b.       .d8888b.      88888888888 888                   d8b  .d888      .d8888b.  888             888                                             888
# d8888   d88P  Y88b     d88P  Y88b         888     888                   Y8P d88P"      d88P  Y88b 888             888                                             888
#   888        .d88P     888                888     888                       888        Y88b.      888             888                                             888
#   888       8888"      888d888b.          888     88888b.   .d88b.      888 888888      "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
#   888        "Y8b.     888P "Y88b         888     888 "88b d8P  Y8b     888 888            "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
#   888   888    888     888    888         888     888  888 88888888     888 888              "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
#   888   Y88b  d88P d8b Y88b  d88P         888     888  888 Y8b.         888 888        Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 8888888  "Y8888P"  Y8P  "Y8888P"          888     888  888  "Y8888      888 888         "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
###########################################################################################################################################################################
# 13.6 The if Statement
class PN_IfStatement(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('IfStatement', p)
class PN_IfStatement_IF_LPAREN_Expression_RPAREN_Statement_ELSE_Statement(PN_IfStatement):
    @property
    def Expression(self):
        return self.children[2]
    @property
    def Statement1(self):
        return self.children[4]
    @property
    def Statement2(self):
        return self.children[6]
    def ContainsDuplicateLabels(self, labelSet):
        # 13.6.2 Static Semantics: ContainsDuplicateLabels
        #   With parameter labelSet.
        #           IfStatement : if ( Expression ) Statement else Statement
        # 1. Let hasDuplicate be ContainsDuplicateLabels of the first Statement with argument labelSet.
        # 2. If hasDuplicate is true, return true.
        # 3. Return ContainsDuplicateLabels of the second Statement with argument labelSet.
        return self.Statement1.ContainsDuplicateLabels(labelSet) or self.Statement2.ContainsDuplicateLabels(labelSet)
    def ContainsUndefinedBreakTarget(self, labelSet):
        # 13.6.3 Static Semantics: ContainsUndefinedBreakTarget
        #   With parameter labelSet.
        #           IfStatement : if ( Expression ) Statement else Statement
        # 1. Let hasUndefinedLabels be ContainsUndefinedBreakTarget of the first Statement with argument labelSet.
        # 2. If hasUndefinedLabels is true, return true.
        # 3. Return ContainsUndefinedBreakTarget of the second Statement with argument labelSet.
        return (self.Statement1.ContainsUndefinedBreakTarget(labelSet) or
                self.Statement2.ContainsUndefinedBreakTarget(labelSet))
    def ContainsUndefinedContinueTarget(self, iterationSet, labelSet):
        # 13.6.4 Static Semantics: ContainsUndefinedContinueTarget
        #   With parameters iterationSet and labelSet.
        #           IfStatement : if ( Expression ) Statement else Statement
        # 1. Let hasUndefinedLabels be ContainsUndefinedContinueTarget of the first Statement with arguments iterationSet and « ».
        # 2. If hasUndefinedLabels is true, return true.
        # 3. Return ContainsUndefinedContinueTarget of the second Statement with arguments iterationSet and « ».
        return (self.Statement1.ContainsUndefinedContinueTarget(iterationSet, []) or
                self.Statement2.ContainsUndefinedContinueTarget(iterationSet, []))
    def VarDeclaredNames(self):
        # 13.6.5 Static Semantics: VarDeclaredNames
        #           IfStatement : if ( Expression ) Statement else Statement
        # 1. Let names be VarDeclaredNames of the first Statement.
        # 2. Append to names the elements of the VarDeclaredNames of the second Statement.
        # 3. Return names.
        names = self.Statement1.VarDeclaredNames()
        names.extend(self.Statement2.VarDeclaredNames())
        return names
    def VarScopedDeclarations(self):
        # 13.6.6 Static Semantics: VarScopedDeclarations
        #           IfStatement : if ( Expression ) Statement else Statement
        # 1. Let declarations be VarScopedDeclarations of the first Statement.
        # 2. Append to declarations the elements of the VarScopedDeclarations of the second Statement.
        # 3. Return declarations.
        declarations = self.Statement1.VarScopedDeclarations()
        declarations.extend(self.Statement2.VarScopedDeclarations())
        return declarations
    def evaluate(self):
        # 13.6.7 Runtime Semantics: Evaluation
        #           IfStatement : if ( Expression ) Statement else Statement
        # 1. Let exprRef be the result of evaluating Expression.
        # 2. Let exprValue be ToBoolean(? GetValue(exprRef)).
        # 3. If exprValue is true, then
        #    a. Let stmtCompletion be the result of evaluating the first Statement.
        # 4. Else,
        #    a. Let stmtCompletion be the result of evaluating the second Statement.
        # 5. Return Completion(UpdateEmpty(stmtCompletion, undefined)).
        exprValue = GetValue(self.Expression.evaluate())
        stmtCompletion = self.Statement1.evaluate() if ToBoolean(exprValue) else self.Statement2.evaluate()
        return UpdateEmpty(stmtCompletion, None)
class PN_IfStatement_IF_LPAREN_Expression_RPAREN_Statement(PN_IfStatement):
    @property
    def Expression(self):
        return self.children[2]
    @property
    def Statement(self):
        return self.children[4]
    def ContainsDuplicateLabels(self, labelSet):
        # 13.6.2 Static Semantics: ContainsDuplicateLabels
        #   With parameter labelSet.
        #           IfStatement : if ( Expression ) Statement
        # 1. Return ContainsDuplicateLabels of Statement with argument labelSet.
        return self.Statement.ContainsDuplicateLabels(labelSet)
    def ContainsUndefinedBreakTarget(self, labelSet):
        # 13.6.3 Static Semantics: ContainsUndefinedBreakTarget
        #   With parameter labelSet.
        #           IfStatement : if ( Expression ) Statement
        # 1. Return ContainsUndefinedBreakTarget of Statement with argument labelSet.
        return self.Statement.ContainsUndefinedBreakTarget(labelSet)
    def ContainsUndefinedContinueTarget(self, iterationSet, labelSet):
        # 13.6.4 Static Semantics: ContainsUndefinedContinueTarget
        #   With parameters iterationSet and labelSet.
        #           IfStatement : if ( Expression ) Statement
        # 1. Return ContainsUndefinedContinueTarget of Statement with arguments iterationSet and « ».
        return self.Statement.ContainsUndefinedContinueTarget(iterationSet, [])
    def VarDeclaredNames(self):
        # 13.6.5 Static Semantics: VarDeclaredNames
        #           IfStatement : if ( Expression ) Statement
        # 1. Return the VarDeclaredNames of Statement.
        return self.Statement.VarDeclaredNames()
    def VarScopedDeclarations(self):
        # 13.6.6 Static Semantics: VarScopedDeclarations
        #           IfStatement : if ( Expression ) Statement
        # 1. Return the VarScopedDeclarations of Statement.
        return self.Statement.VarScopedDeclarations()
    def evaluate(self):
        # 13.6.7 Runtime Semantics: Evaluation
        #           IfStatement : if ( Expression ) Statement
        # 1. Let exprRef be the result of evaluating Expression.
        # 2. Let exprValue be ToBoolean(? GetValue(exprRef)).
        # 3. If exprValue is false, then
        #    a. Return NormalCompletion(undefined).
        # 4. Else,
        #    a. Let stmtCompletion be the result of evaluating Statement.
        #    b. Return Completion(UpdateEmpty(stmtCompletion, undefined)).
        exprValue = GetValue(self.Expression.evaluate())
        return UpdateEmpty(self.Statement.evaluate(), None) if ToBoolean(exprValue) else None
############################################################################################################################################################################################################
#
#  d888    .d8888b.      8888888888     8888888 888                              888    d8b                        .d8888b.  888             888                                             888
# d8888   d88P  Y88b           d88P       888   888                              888    Y8P                       d88P  Y88b 888             888                                             888
#   888        .d88P          d88P        888   888                              888                              Y88b.      888             888                                             888
#   888       8888"          d88P         888   888888  .d88b.  888d888  8888b.  888888 888  .d88b.  88888b.       "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888 .d8888b
#   888        "Y8b.      88888888        888   888    d8P  Y8b 888P"       "88b 888    888 d88""88b 888 "88b         "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888    88K
#   888   888    888       d88P           888   888    88888888 888     .d888888 888    888 888  888 888  888           "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888    "Y8888b.
#   888   Y88b  d88P d8b  d88P            888   Y88b.  Y8b.     888     888  888 Y88b.  888 Y88..88P 888  888     Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.       X88
# 8888888  "Y8888P"  Y8P d88P           8888888  "Y888  "Y8888  888     "Y888888  "Y888 888  "Y88P"  888  888      "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888  88888P'
#
############################################################################################################################################################################################################
class PN_IterationStatement(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('IterationStatement', p)
class PN_IterationStatement_DO_Statement_WHILE_LPAREN_Expression_RPAREN_SEMICOLON(PN_IterationStatement):
    # 13.7.2 The do-while Statement
    @property
    def Statement(self):
        return self.children[1]
    @property
    def Expression(self):
        return self.children[4]
    def ContainsDuplicateLabels(self, labelSet):
        # 13.7.2.1 Static Semantics: ContainsDuplicateLabels
        #   With parameter labelSet.
        #           IterationStatement : do Statement while ( Expression ) ;
        # 1. Return ContainsDuplicateLabels of Statement with argument labelSet.
        return self.Statement.ContainsDuplicateLabels(labelSet)
    def ContainsUndefinedBreakTarget(self, labelSet):
        # 13.7.2.2 Static Semantics: ContainsUndefinedBreakTarget
        #   With parameter labelSet.
        #           IterationStatement : do Statement while ( Expression ) ;
        # 1. Return ContainsUndefinedBreakTarget of Statement with argument labelSet.
        return self.Statement.ContainsUndefinedBreakTarget(labelSet)
    def ContainsUndefinedContinueTarget(self, iterationSet, labelSet):
        # 13.7.2.3 Static Semantics: ContainsUndefinedContinueTarget
        #   With parameters iterationSet and labelSet.
        #           IterationStatement : do Statement while ( Expression ) ;
        # 1. Return ContainsUndefinedContinueTarget of Statement with arguments iterationSet and « ».
        return self.Statement.ContainsUndefinedContinueTarget(iterationSet, [])
    def VarDeclaredNames(self):
        # 13.7.2.4 Static Semantics: VarDeclaredNames
        #           IterationStatement : do Statement while ( Expression ) ;
        # 1. Return the VarDeclaredNames of Statement.
        return self.Statement.VarDeclaredNames()
    def VarScopedDeclarations(self):
        # 13.7.2.5 Static Semantics: VarScopedDeclarations
        #           IterationStatement : do Statement while ( Expression ) ;
        # 1. Return the VarScopedDeclarations of Statement.
        return self.Statement.VarScopedDeclarations()
    def LabelledEvaluation(self, labelSet):
        # 13.7.2.6 Runtime Semantics: LabelledEvaluation
        #   With parameter labelSet.
        #           IterationStatement : do Statement while ( Expression ) ;
        # 1. Let V be undefined.
        # 2. Repeat,
        #    a. Let stmtResult be the result of evaluating Statement.
        #    b. If LoopContinues(stmtResult, labelSet) is false, return Completion(UpdateEmpty(stmtResult, V)).
        #    c. If stmtResult.[[Value]] is not empty, set V to stmtResult.[[Value]].
        #    d. Let exprRef be the result of evaluating Expression.
        #    e. Let exprValue be ? GetValue(exprRef).
        #    f. If ToBoolean(exprValue) is false, return NormalCompletion(V).
        V = None
        while 1:
            try:
                stmtResult = self.Statement.evaluate()
            except ESAbrupt as abrupt:
                c = abrupt.completion
                if not LoopContinues(c.value, labelSet):
                    raise type(abrupt)(value=UpdateEmpty(c.value, V), target=c.target)
                stmtResult = c.value
            if stmtResult != Empty.EMPTY:
                V = stmtResult
            exprValue = GetValue(self.Expression.evaluate())
            if not ToBoolean(exprValue):
                return V
class PN_IterationStatement_WHILE_LPAREN_Expression_RPAREN_Statement(PN_IterationStatement):
    # 13.7.3 The while Statement
    @property
    def Expression(self):
        return self.children[2]
    @property
    def Statement(self):
        return self.children[4]
    def ContainsDuplicateLabels(self, labelSet):
        # 13.7.3.1 Static Semantics: ContainsDuplicateLabels
        #   With parameter labelSet.
        #           IterationStatement : while ( Expression ) Statement
        # 1. Return ContainsDuplicateLabels of Statement with argument labelSet.
        return self.Statement.ContainsDuplicateLabels(labelSet)
    def ContainsUndefinedBreakTarget(self, labelSet):
        # 13.7.3.2 Static Semantics: ContainsUndefinedBreakTarget
        #   With parameter labelSet.
        #           IterationStatement : while ( Expression ) Statement
        # 1. Return ContainsUndefinedBreakTarget of Statement with argument labelSet.
        return self.Statement.ContainsUndefinedBreakTarget(labelSet)
    def ContainsUndefinedContinueTarget(self, iterationSet, labelSet):
        # 13.7.3.3 Static Semantics: ContainsUndefinedContinueTarget
        #   With parameters iterationSet and labelSet.
        #           IterationStatement : while ( Expression ) Statement
        # 1. Return ContainsUndefinedContinueTarget of Statement with arguments iterationSet and « ».
        return self.Statement.ContainsUndefinedContinueTarget(iterationSet, [])
    def VarDeclaredNames(self):
        # 13.7.3.4 Static Semantics: VarDeclaredNames
        #           IterationStatement : while ( Expression ) Statement
        # 1. Return the VarDeclaredNames of Statement.
        return self.Statement.VarDeclaredNames()
    def VarScopedDeclarations(self):
        # 13.7.3.5 Static Semantics: VarScopedDeclarations
        #           IterationStatement : while ( Expression ) Statement
        # 1. Return the VarScopedDeclarations of Statement.
        return self.Statement.VarScopedDeclarations()
    def LabelledEvaluation(self, labelSet):
        # 13.7.3.6 Runtime Semantics: LabelledEvaluation
        #   With parameter labelSet.
        #           IterationStatement : while ( Expression ) Statement
        # 1. Let V be undefined.
        # 2. Repeat,
        #    a. Let exprRef be the result of evaluating Expression.
        #    b. Let exprValue be ? GetValue(exprRef).
        #    c. If ToBoolean(exprValue) is false, return NormalCompletion(V).
        #    d. Let stmtResult be the result of evaluating Statement.
        #    e. If LoopContinues(stmtResult, labelSet) is false, return Completion(UpdateEmpty(stmtResult, V)).
        #    f. If stmtResult.[[Value]] is not empty, set V to stmtResult.[[Value]].
        V = None
        while 1:
            exprValue = GetValue(self.Expression.evaluate())
            if not ToBoolean(exprValue):
                return V
            try:
                stmtResult = self.Statement.evaluate()
            except ESAbrupt as abrupt:
                c = abrupt.completion
                if not LoopContinues(c.value, labelSet):
                    raise type(abrupt)(value=UpdateEmpty(c.value, V), target=c.target)
                stmtResult = c.value
            if stmtResult != Empty.EMPTY:
                V = stmtResult
def LoopContinues(completion, labelSet):
    # 13.7.1.2 Runtime Semantics: LoopContinues ( completion, labelSet )
    # The abstract operation LoopContinues with arguments completion and labelSet is defined by the following steps:
    #
    # 1. If completion.[[Type]] is normal, return true.
    # 2. If completion.[[Type]] is not continue, return false.
    # 3. If completion.[[Target]] is empty, return true.
    # 4. If completion.[[Target]] is an element of labelSet, return true.
    # 5. Return false.
    # NOTE
    # Within the Statement part of an IterationStatement a ContinueStatement may be used to begin a new iteration.
    return (completion.ctype == CompletionType.NORMAL or
            (completion.ctype == CompletionType.CONTINUE and (completion.target == Empty.EMPTY or completion.target in labelSet)))
class PN_IterationStatement_For_Expressions(PN_IterationStatement):
    @property
    def Statement(self):
        raise NotImplementedError('Abstract classes cannot be instantiated')
    # This is for the for statements with simple-ish expresions (no "in" or "of")
    def ContainsDuplicateLabels(self, labelSet):
        # 13.7.4.2 Static Semantics: ContainsDuplicateLabels
        #   With parameter labelSet.
        #       IterationStatement :
        #           for ( Expression[opt] ; Expression[opt] ; Expression[opt] ) Statement
        #           for ( var VariableDeclarationList ; Expression[opt] ; Expression[opt] ) Statement
        #           for ( LexicalDeclaration Expression[opt] ; Expression[opt] ) Statement
        # 1. Return ContainsDuplicateLabels of Statement with argument labelSet.
        return self.Statement.ContainsDuplicateLabels(labelSet)
    def ContainsUndefinedBreakTarget(self, labelSet):
        # 13.7.4.3 Static Semantics: ContainsUndefinedBreakTarget
        #   With parameter labelSet.
        #       IterationStatement :
        #           for ( Expression[opt] ; Expression[opt] ; Expression[opt] ) Statement
        #           for ( var VariableDeclarationList ; Expression[opt] ; Expression[opt] ) Statement
        #           for ( LexicalDeclaration Expression[opt] ; Expression[opt] ) Statement
        # 1. Return ContainsUndefinedBreakTarget of Statement with argument labelSet.
        return self.Statement.ContainsUndefinedBreakTarget(labelSet)
    def ContainsUndefinedContinueTarget(self, iterationSet, labelSet):
        # 13.7.4.4 Static Semantics: ContainsUndefinedContinueTarget
        #   With parameters iterationSet and labelSet.
        #       IterationStatement :
        #           for ( Expression[opt] ; Expression[opt] ; Expression[opt] ) Statement
        #           for ( var VariableDeclarationList ; Expression[opt] ; Expression[opt] ) Statement
        #           for ( LexicalDeclaration Expression[opt] ; Expression[opt] ) Statement
        # 1. Return ContainsUndefinedContinueTarget of Statement with arguments iterationSet and « ».
        return self.Statement.ContainsUndefinedContinueTarget(iterationSet, [])
    def VarDeclaredNames(self):
        # 13.7.4.5 Static Semantics: VarDeclaredNames
        # IterationStatement : for ( Expression ; Expression ; Expression ) Statement
        # IterationStatement : for ( LexicalDeclaration Expression ; Expression ) Statement
        #   1. Return the VarDeclaredNames of Statement.
        return self.Statement.VarDeclaredNames()
    def VarScopedDeclarations(self):
        # 13.7.4.6 Static Semantics: VarScopedDeclarations
        # IterationStatement : for ( Expression ; Expression ; Expression ) Statement
        # IterationStatement : for ( LexicalDeclaration Expression ; Expression ) Statement
        #   1. Return the VarScopedDeclarations of Statement.
        return self.Statement.VarScopedDeclarations()
class PN_IterationStatement_For_Expressions_only(PN_IterationStatement_For_Expressions):
    @property
    def Expression1(self):
        raise NotImplementedError('Abstract classes cannot be instantiated')
    @property
    def Expression2(self):
        raise NotImplementedError('Abstract classes cannot be instantiated')
    @property
    def Expression3(self):
        raise NotImplementedError('Abstract classes cannot be instantiated')
    def LabelledEvaluation(self, labelSet):
        # 13.7.4.7 Runtime Semantics: LabelledEvaluation
        #   With parameter labelSet.
        #           IterationStatement : for ( Expression ; Expression ; Expression ) Statement
        # 1. If the first Expression is present, then
        #    a. Let exprRef be the result of evaluating the first Expression.
        #    b. Perform ? GetValue(exprRef).
        # 1. Return ? ForBodyEvaluation(the second Expression, the third Expression, Statement, « », labelSet).
        if self.Expression1:
            GetValue(self.Expression1.evaluate())
        return ForBodyEvaluation(self.Expression2, self.Expression3, self.Statement, [], labelSet)
class PN_IterationStatement_FOR_LPAREN_Expression_SEMICOLON_Expression_SEMICOLON_Expression_RPAREN_Statement(PN_IterationStatement_For_Expressions_only):
    @property
    def Statement(self):
        return self.children[8]
    @property
    def Expression1(self):
        return self.children[2]
    @property
    def Expression2(self):
        return self.children[4]
    @property
    def Expression3(self):
        return self.children[6]
class PN_IterationStatement_FOR_LPAREN_SEMICOLON_Expression_SEMICOLON_Expression_RPAREN_Statement(PN_IterationStatement_For_Expressions_only):
    @property
    def Statement(self):
        return self.children[7]
    @property
    def Expression1(self):
        return None
    @property
    def Expression2(self):
        return self.children[3]
    @property
    def Expression3(self):
        return self.children[5]
class PN_IterationStatement_FOR_LPAREN_Expression_SEMICOLON_SEMICOLON_Expression_RPAREN_Statement(PN_IterationStatement_For_Expressions_only):
    @property
    def Statement(self):
        return self.children[7]
    @property
    def Expression1(self):
        return self.children[2]
    @property
    def Expression2(self):
        return None
    @property
    def Expression3(self):
        return self.children[5]
class PN_IterationStatement_FOR_LPAREN_Expression_SEMICOLON_Expression_SEMICOLON_RPAREN_Statement(PN_IterationStatement_For_Expressions_only):
    @property
    def Statement(self):
        return self.children[7]
    @property
    def Expression1(self):
        return self.children[2]
    @property
    def Expression2(self):
        return self.children[4]
    @property
    def Expression3(self):
        return None
class PN_IterationStatement_FOR_LPAREN_Expression_SEMICOLON_SEMICOLON_RPAREN_Statement(PN_IterationStatement_For_Expressions_only):
    @property
    def Statement(self):
        return self.children[6]
    @property
    def Expression1(self):
        return self.children[2]
    @property
    def Expression2(self):
        return None
    @property
    def Expression3(self):
        return None
class PN_IterationStatement_FOR_LPAREN_SEMICOLON_Expression_SEMICOLON_RPAREN_Statement(PN_IterationStatement_For_Expressions_only):
    @property
    def Statement(self):
        return self.children[6]
    @property
    def Expression1(self):
        return None
    @property
    def Expression2(self):
        return self.children[3]
    @property
    def Expression3(self):
        return None
class PN_IterationStatement_FOR_LPAREN_SEMICOLON_SEMICOLON_Expression_RPAREN_Statement(PN_IterationStatement_For_Expressions_only):
    @property
    def Statement(self):
        return self.children[6]
    @property
    def Expression1(self):
        return None
    @property
    def Expression2(self):
        return None
    @property
    def Expression3(self):
        return self.children[4]
class PN_IterationStatement_FOR_LPAREN_SEMICOLON_SEMICOLON_RPAREN_Statement(PN_IterationStatement_For_Expressions_only):
    @property
    def Statement(self):
        return self.children[5]
    @property
    def Expression1(self):
        return None
    @property
    def Expression2(self):
        return None
    @property
    def Expression3(self):
        return None
class PN_IterationStatement_For_varlist(PN_IterationStatement_For_Expressions):
    @property
    def VariableDeclarationList(self):
        raise NotImplementedError('Abstract classes cannot be instantiated')
    def Expression1(self):
        raise NotImplementedError('Abstract classes cannot be instantiated')
    def Expression2(self):
        raise NotImplementedError('Abstract classes cannot be instantiated')
    def VarDeclaredNames(self):
        # 13.7.4.5 Static Semantics: VarDeclaredNames
        #           IterationStatement : for ( var VariableDeclarationList ; Expression ; Expression ) Statement
        # 1. Let names be BoundNames of VariableDeclarationList.
        # 2. Append to names the elements of the VarDeclaredNames of Statement.
        # 3. Return names.
        names = self.VariableDeclarationList.BoundNames()
        names.extend(self.Statement.VarDeclaredNames())
        return names
    def VarScopedDeclarations(self):
        # 13.7.4.6 Static Semantics: VarScopedDeclarations
        #           IterationStatement : for ( var VariableDeclarationList ; Expression ; Expression ) Statement
        # 1. Let declarations be VarScopedDeclarations of VariableDeclarationList.
        # 2. Append to declarations the elements of the VarScopedDeclarations of Statement.
        # 3. Return declarations.
        declarations = self.VariableDeclarationList.VarScopedDeclarations()
        declarations.extend(self.Statement.VarScopedDeclarations())
        return declarations
    def LabelledEvaluation(self, labelSet):
        # 13.7.4.7 Runtime Semantics: LabelledEvaluation
        #   With parameter labelSet.
        #           IterationStatement : for ( var VariableDeclarationList ; Expression ; Expression ) Statement
        # 1. Let varDcl be the result of evaluating VariableDeclarationList.
        # 2. ReturnIfAbrupt(varDcl).
        # 3. Return ? ForBodyEvaluation(the first Expression, the second Expression, Statement, « », labelSet).
        self.VariableDeclarationList.evaluate()
        return ForBodyEvaluation(self.Expression1, self.Expression2, self.Statement, [], labelSet)
class PN_IterationStatement_FOR_LPAREN_VAR_VariableDeclarationList_SEMICOLON_Expression_SEMICOLON_Expression_RPAREN_Statement(PN_IterationStatement_For_varlist):
    @property
    def VariableDeclarationList(self):
        return self.children[3]
    @property
    def Expression1(self):
        return self.children[5]
    @property
    def Expression2(self):
        return self.children[7]
    @property
    def Statement(self):
        return self.children[9]
class PN_IterationStatement_FOR_LPAREN_VAR_VariableDeclarationList_SEMICOLON_SEMICOLON_Expression_RPAREN_Statement(PN_IterationStatement_For_varlist):
    @property
    def VariableDeclarationList(self):
        return self.children[3]
    @property
    def Expression1(self):
        return None
    @property
    def Expression2(self):
        return self.children[6]
    @property
    def Statement(self):
        return self.children[8]
class PN_IterationStatement_FOR_LPAREN_VAR_VariableDeclarationList_SEMICOLON_Expression_SEMICOLON_RPAREN_Statement(PN_IterationStatement_For_varlist):
    @property
    def VariableDeclarationList(self):
        return self.children[3]
    @property
    def Expression1(self):
        return self.children[5]
    @property
    def Expression2(self):
        return None
    @property
    def Statement(self):
        return self.children[8]
class PN_IterationStatement_FOR_LPAREN_VAR_VariableDeclarationList_SEMICOLON_SEMICOLON_RPAREN_Statement(PN_IterationStatement_For_varlist):
    @property
    def VariableDeclarationList(self):
        return self.children[3]
    @property
    def Expression1(self):
        return None
    @property
    def Expression2(self):
        return None
    @property
    def Statement(self):
        return self.children[7]
class PN_IterationStatement_For_Lexical(PN_IterationStatement_For_Expressions):
    @property
    def Expression1(self):
        raise NotImplementedError('Abstract classes cannot be instantiated')
    @property
    def Expression2(self):
        raise NotImplementedError('Abstract classes cannot be instantiated')
    @property
    def LexicalDeclaration(self):
        raise NotImplementedError('Abstract classes cannot be instantiated')
    def EarlyErrors(self):
        # 13.7.4.1 Static Semantics: Early Errors
        # IterationStatement : for ( LexicalDeclaration Expression ; Expression ) Statement
        # * It is a Syntax Error if any element of the BoundNames of LexicalDeclaration also occurs in the
        #   VarDeclaredNames of Statement.
        lex_set = set(self.LexicalDeclaration.BoundNames())
        var_set = set(self.Statement.VarDeclaredNames())
        if not lex_set.isdisjoint(var_set):
            dups = lex_set.intersection(var_set)
            return [CreateSyntaxError(f'Identifier \'{dups.pop()}\' has already been declared')]
        return []
    def LabelledEvaluation(self, labelSet):
        # 13.7.4.7 Runtime Semantics: LabelledEvaluation
        #   With parameter labelSet.
        # IterationStatement : for ( LexicalDeclaration Expression ; Expression ) Statement
        #   1. Let oldEnv be the running execution context's LexicalEnvironment.
        #   2. Let loopEnv be NewDeclarativeEnvironment(oldEnv).
        #   3. Let loopEnvRec be loopEnv's EnvironmentRecord.
        #   4. Let isConst be the result of performing IsConstantDeclaration of LexicalDeclaration.
        #   5. Let boundNames be the BoundNames of LexicalDeclaration.
        #   6. For each element dn of boundNames, do
        #       a. If isConst is true, then
        #           i. Perform ! loopEnvRec.CreateImmutableBinding(dn, true).
        #       b. Else,
        #           i. Perform ! loopEnvRec.CreateMutableBinding(dn, false).
        #   7. Set the running execution context's LexicalEnvironment to loopEnv.
        #   8. Let forDcl be the result of evaluating LexicalDeclaration.
        #   9. If forDcl is an abrupt completion, then
        #       a. Set the running execution context's LexicalEnvironment to oldEnv.
        #       b. Return Completion(forDcl).
        #   10. If isConst is false, let perIterationLets be boundNames; otherwise let perIterationLets be « ».
        #   11. Let bodyResult be ForBodyEvaluation(the first Expression, the second Expression, Statement, perIterationLets, labelSet).
        #   12. Set the running execution context's LexicalEnvironment to oldEnv.
        #   13. Return Completion(bodyResult).
        oldEnv = surrounding_agent.running_ec.lexical_environment
        loopEnv = NewDeclarativeEnvironment(oldEnv)
        loopEnvRec = loopEnv.environment_record
        isConst = self.LexicalDeclaration.IsConstantDeclaration()
        boundNames = self.LexicalDeclaration.BoundNames()
        for dn in boundNames:
            if isConst:
                loopEnvRec.CreateImmutableBinding(dn, True)
            else:
                loopEnvRec.CreateMutableBinding(dn, False)
        surrounding_agent.running_ec.lexical_environment = loopEnv
        try:
            self.LexicalDeclaration.evaluate()
            if not isConst:
                perIterationLets = boundNames
            else:
                perIterationLets = []
            return ForBodyEvaluation(self.Expression1, self.Expression2, self.Statement, perIterationLets, labelSet)
        finally:
            surrounding_agent.running_ec.lexical_environment = oldEnv

class PN_IterationStatement_FOR_LPAREN_LexicalDeclaration_Expression_SEMICOLON_Expression_RPAREN_Statement(PN_IterationStatement_For_Lexical):
    @property
    def LexicalDeclaration(self):
        return self.children[2]
    @property
    def Expression1(self):
        return self.children[3]
    @property
    def Expression2(self):
        return self.children[5]
    @property
    def Statement(self):
        return self.children[7]
class PN_IterationStatement_FOR_LPAREN_LexicalDeclaration_SEMICOLON_Expression_RPAREN_Statement(PN_IterationStatement_For_Lexical):
    @property
    def LexicalDeclaration(self):
        return self.children[2]
    @property
    def Expression1(self):
        return None
    @property
    def Expression2(self):
        return self.children[4]
    @property
    def Statement(self):
        return self.children[6]
class PN_IterationStatement_FOR_LPAREN_LexicalDeclaration_Expression_SEMICOLON_RPAREN_Statement(PN_IterationStatement_For_Lexical):
    @property
    def LexicalDeclaration(self):
        return self.children[2]
    @property
    def Expression1(self):
        return self.children[3]
    @property
    def Expression2(self):
        return None
    @property
    def Statement(self):
        return self.children[6]
class PN_IterationStatement_FOR_LPAREN_LexicalDeclaration_SEMICOLON_RPAREN_Statement(PN_IterationStatement_For_Lexical):
    @property
    def LexicalDeclaration(self):
        return self.children[2]
    @property
    def Expression1(self):
        return None
    @property
    def Expression2(self):
        return None
    @property
    def Statement(self):
        return self.children[5]


# 13.7.4.8 Runtime Semantics: ForBodyEvaluation ( test, increment, stmt, perIterationBindings, labelSet )
def ForBodyEvaluation(test, increment, stmt, perIterationBindings, labelSet):
    # The abstract operation ForBodyEvaluation with arguments test, increment, stmt, perIterationBindings, and
    # labelSet is performed as follows:
    #
    # 1. Let V be undefined.
    # 2. Perform ? CreatePerIterationEnvironment(perIterationBindings).
    # 3. Repeat,
    #    a. If test is not [empty], then
    #       i. Let testRef be the result of evaluating test.
    #      ii. Let testValue be ? GetValue(testRef).
    #     iii. If ToBoolean(testValue) is false, return NormalCompletion(V).
    #    b. Let result be the result of evaluating stmt.
    #    c. If LoopContinues(result, labelSet) is false, return Completion(UpdateEmpty(result, V)).
    #    d. If result.[[Value]] is not empty, set V to result.[[Value]].
    #    e. Perform ? CreatePerIterationEnvironment(perIterationBindings).
    #    f. If increment is not [empty], then
    #       i. Let incRef be the result of evaluating increment.
    #      ii. Perform ? GetValue(incRef).
    V = None
    CreatePerIterationEnvironment(perIterationBindings)
    while 1:
        if test:
            testValue = GetValue(test.evaluate())
            if not ToBoolean(testValue):
                return V
        try:
            result = stmt.evaluate()
        except ESAbrupt as abrupt:
            c = abrupt.completion
            if not LoopContinues(c, labelSet):
                raise type(abrupt)(value=UpdateEmpty(c.value, V), target=c.target)
            result = c.value
        if result != Empty.EMPTY:
            V = result
        CreatePerIterationEnvironment(perIterationBindings)
        if increment:
            GetValue(increment.evaluate())
# 13.7.4.9 Runtime Semantics: CreatePerIterationEnvironment ( perIterationBindings )
def CreatePerIterationEnvironment(perIterationBindings):
    # The abstract operation CreatePerIterationEnvironment with argument perIterationBindings is performed as follows:
    #
    # 1. If perIterationBindings has any elements, then
    #    a. Let lastIterationEnv be the running execution context's LexicalEnvironment.
    #    b. Let lastIterationEnvRec be lastIterationEnv's EnvironmentRecord.
    #    c. Let outer be lastIterationEnv's outer environment reference.
    #    d. Assert: outer is not null.
    #    e. Let thisIterationEnv be NewDeclarativeEnvironment(outer).
    #    f. Let thisIterationEnvRec be thisIterationEnv's EnvironmentRecord.
    #    g. For each element bn of perIterationBindings, do
    #       i. Perform ! thisIterationEnvRec.CreateMutableBinding(bn, false).
    #      ii. Let lastValue be ? lastIterationEnvRec.GetBindingValue(bn, true).
    #     iii. Perform thisIterationEnvRec.InitializeBinding(bn, lastValue).
    #    h. Set the running execution context's LexicalEnvironment to thisIterationEnv.
    # 2. Return undefined.
    if perIterationBindings:
        lastIterationEnv = surrounding_agent.running_ec.lexical_environment
        lastIterationEnvRec = lastIterationEnv.environment_record
        outer = lastIterationEnv.outer
        assert outer and not isNull(outer)
        thisIterationEnv = NewDeclarativeEnvironment(outer)
        thisIterationEnvRec = thisIterationEnv.environment_record
        for bn in perIterationBindings:
            thisIterationEnvRec.CreateMutableBinding(bn, False)
            lastValue = lastIterationEnvRec.GetBindingValue(bn, True)
            thisIterationEnvRec.InitializeBinding(bn, lastValue)
        surrounding_agent.running_ec.lexical_environment = thisIterationEnv
    return None

# ------------------------------------ 𝑭𝒐𝒓𝑫𝒆𝒄𝒍𝒂𝒓𝒂𝒕𝒊𝒐𝒏 ------------------------------------
class PN_ForDeclaration(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ForDeclaration', p)
class PN_ForDeclaration_LetOrConst_ForBinding(PN_ForDeclaration):
    @property
    def LetOrConst(self):
        return self.children[0]
    @property
    def ForBinding(self):
        return self.children[1]
    def BoundNames(self):
        # 13.7.5.2 Static Semantics: BoundNames
        #           ForDeclaration : LetOrConst ForBinding
        # 1. Return the BoundNames of ForBinding.
        return self.ForBinding.BoundNames()
    def IsDestructuring(self):
        # 13.7.5.6 Static Semantics: IsDestructuring
        #           ForDeclaration : LetOrConstForBinding
        # 1. Return IsDestructuring of ForBinding.
        return self.ForBinding.IsDestructuring()
    def BindingInitialization(self, value, environment):
        # 13.7.5.9 Runtime Semantics: BindingInitialization
        #   With parameters value and environment.
        #
        # NOTE
        # undefined is passed for environment to indicate that a PutValue operation should be used to assign the
        # initialization value. This is the case for var statements and the formal parameter lists of some non-strict
        # functions (see 9.2.15). In those cases a lexical binding is hoisted and preinitialized prior to evaluation of
        # its initializer.
        #
        #           ForDeclaration : LetOrConst ForBinding
        # 1. Return the result of performing BindingInitialization for ForBinding passing value and environment as the
        #    arguments.
        return self.ForBinding.BindingInitialization(value, environment)
    def BindingInstantiation(self, environment):
        # 13.7.5.10 Runtime Semantics: BindingInstantiation
        #   With parameter environment.
        #           ForDeclaration : LetOrConst ForBinding
        # 1. Let envRec be environment's EnvironmentRecord.
        # 2. Assert: envRec is a declarative Environment Record.
        # 3. For each element name of the BoundNames of ForBinding, do
        #    a. If IsConstantDeclaration of LetOrConst is true, then
        #       i. Perform ! envRec.CreateImmutableBinding(name, true).
        #    b. Else,
        #       i. Perform ! envRec.CreateMutableBinding(name, false).
        envRec = environment.environment_record
        assert isinstance(envRec, DeclarativeEnvironmentRecord)
        for name in self.ForBinding.BoundNames():
            if self.LetOrConst.IsConstantDeclaration():
                envRec.CreateImmutableBinding(name, True)
            else:
                envRec.CreateMutableBinding(name, False)

# ------------------------------------ 𝑭𝒐𝒓𝑩𝒊𝒏𝒅𝒊𝒏𝒈 ------------------------------------
class PN_ForBinding(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ForBinding', p)
class PN_ForBinding_BindingIdentifier(PN_ForBinding):
    @property
    def BindingIdentifier(self):
        return self.children[0]
    def IsDestructuring(self):
        # 13.7.5.6 Static Semantics: IsDestructuring
        #           ForBinding : BindingIdentifier
        # 1. Return false.
        return False
    def evaluate(self):
        # 13.7.5.14 Runtime Semantics: Evaluation
        #           ForBinding : BindingIdentifier
        # 1. Let bindingId be StringValue of BindingIdentifier.
        # 2. Return ? ResolveBinding(bindingId).
        return ResolveBinding(self.BindingIdentifier.StringValue())
class PN_ForBinding_BindingPattern(PN_ForBinding):
    @property
    def BindingPattern(self):
        return self.children[0]
    def IsDestructuring(self):
        # 13.7.5.6 Static Semantics: IsDestructuring
        #           ForBinding : BindingPattern
        # 1. Return true.
        return True


###############################################################################################################################
#
#  d888    .d8888b.       .d8888b.      88888888888 888                                              888    d8b                                 .d8888b.  888             888                                             888
# d8888   d88P  Y88b     d88P  Y88b         888     888                                              888    Y8P                                d88P  Y88b 888             888                                             888
#   888        .d88P     Y88b. d88P         888     888                                              888                                       Y88b.      888             888                                             888
#   888       8888"       "Y88888"          888     88888b.   .d88b.       .d8888b  .d88b.  88888b.  888888 888 88888b.  888  888  .d88b.       "Y888b.   888888  8888b.  888888  .d88b.  88888b.d88b.   .d88b.  88888b.  888888
#   888        "Y8b.     .d8P""Y8b.         888     888 "88b d8P  Y8b     d88P"    d88""88b 888 "88b 888    888 888 "88b 888  888 d8P  Y8b         "Y88b. 888        "88b 888    d8P  Y8b 888 "888 "88b d8P  Y8b 888 "88b 888
#   888   888    888     888    888         888     888  888 88888888     888      888  888 888  888 888    888 888  888 888  888 88888888           "888 888    .d888888 888    88888888 888  888  888 88888888 888  888 888
#   888   Y88b  d88P d8b Y88b  d88P         888     888  888 Y8b.         Y88b.    Y88..88P 888  888 Y88b.  888 888  888 Y88b 888 Y8b.         Y88b  d88P Y88b.  888  888 Y88b.  Y8b.     888  888  888 Y8b.     888  888 Y88b.
# 8888888  "Y8888P"  Y8P  "Y8888P"          888     888  888  "Y8888       "Y8888P  "Y88P"  888  888  "Y888 888 888  888  "Y88888  "Y8888       "Y8888P"   "Y888 "Y888888  "Y888  "Y8888  888  888  888  "Y8888  888  888  "Y888
#
###############################################################################################################################
class PN_ContinueStatement(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ContinueStatement', p)
    def EarlyErrors(self):
        # 13.8.1 Static Semantics: Early Errors
        #           ContinueStatement : continue ;
        #           ContinueStatement : continue LabelIdentifier ;
        #  * It is a Syntax Error if this ContinueStatement is not nested, directly or indirectly (but not crossing
        #    function boundaries), within an IterationStatement.
        # ... I'm not actually sure how to test that. Probably need to add a marker function to the __init__ function
        # of IterationStatement that scans children for continues, and doesn't enter functions.
        return []
class PN_ContinueStatement_CONTINUE_SEMICOLON(PN_ContinueStatement):
    def ContainsUndefinedContinueTarget(self, iterationSet, labelSet):
        # 13.8.2 Static Semantics: ContainsUndefinedContinueTarget
        #   With parameters iterationSet and labelSet.
        #           ContinueStatement : continue;
        # 1. Return false.
        return False
    def evaluate(self):
        # 13.8.3 Runtime Semantics: Evaluation
        #           ContinueStatement : continue ;
        # 1. Return Completion { [[Type]]: continue, [[Value]]: empty, [[Target]]: empty }.
        raise ESContinue
class PN_ContinueStatement_CONTINUE_LabelIdentifier_SEMICOLON(PN_ContinueStatement):
    @property
    def LabelIdentifier(self):
        return self.children[1]
    def ContainsUndefinedContinueTarget(self, iterationSet, labelSet):
        # 13.8.2 Static Semantics: ContainsUndefinedContinueTarget
        #   With parameters iterationSet and labelSet.
        #           ContinueStatement : continue LabelIdentifier ;
        # 1. If the StringValue of LabelIdentifier is not an element of iterationSet, return true.
        # 2. Return false.
        return self.LabelIdentifier.StringValue() not in iterationSet
    def evaluate(self):
        # 13.8.3 Runtime Semantics: Evaluation
        #           ContinueStatement : continue LabelIdentifier ;
        # 1. Let label be the StringValue of LabelIdentifier.
        # 2. Return Completion { [[Type]]: continue, [[Value]]: empty, [[Target]]: label }.
        return ESContinue(target=self.LabelIdentifier.StringValue())

###############################################################################################################################
class PN_CoverCallExpressionAndAsyncArrowHead(ParseNode):
    def __init__(self, context, p):
        super().__init__('CoverCallExpressionAndAsyncArrowHead', p)
class PN_CoverCallExpressionAndAsyncArrowHead_MemberExpression_Arguments(PN_CoverCallExpressionAndAsyncArrowHead):
    pass
# 14.1.10 Static Semantics: IsAnonymousFunctionDefinition ( expr )
def IsAnonymousFunctionDefinition(expr):
    # The abstract operation IsAnonymousFunctionDefinition determines if its argument is a function definition that
    # does not bind a name. The argument expr is the result of parsing an AssignmentExpression or Initializer. The
    # following steps are taken:
    #
    # 1. If IsFunctionDefinition of expr is false, return false.
    # 2. Let hasName be the result of HasName of expr.
    # 3. If hasName is true, return false.
    # 4. Return true.
    return expr.IsFunctionDefinition() and not expr.HasName()
###############################################################################################################################
#
#  d888   888888888       d888        .d8888b.                   d8b          888
# d8888   888            d8888       d88P  Y88b                  Y8P          888
#   888   888              888       Y88b.                                    888
#   888   8888888b.        888        "Y888b.    .d8888b 888d888 888 88888b.  888888 .d8888b
#   888        "Y88b       888           "Y88b. d88P"    888P"   888 888 "88b 888    88K
#   888          888       888             "888 888      888     888 888  888 888    "Y8888b.
#   888   Y88b  d88P d8b   888       Y88b  d88P Y88b.    888     888 888 d88P Y88b.       X88
# 8888888  "Y8888P"  Y8P 8888888      "Y8888P"   "Y8888P 888     888 88888P"   "Y888  88888P'
#                                                                    888
#                                                                    888
#                                                                    888
#
###############################################################################################################################
class PN_Script(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('Script', p)
class PN_Script_ScriptBody(PN_Script):
    @property
    def ScriptBody(self):
        return self.children[0]
    def EarlyErrors(self):
        # 15.1.1 Static Semantics: Early Errors
        # Script : ScriptBody
        #   * It is a Syntax Error if the LexicallyDeclaredNames of ScriptBody contains any duplicate entries.
        #   * It is a Syntax Error if any element of the LexicallyDeclaredNames of ScriptBody also occurs in the VarDeclaredNames of ScriptBody.
        errs = []
        lexnames = self.ScriptBody.LexicallyDeclaredNames()
        lexnameset = set(lexnames)
        if len(lexnames) != len(lexnameset):
            errs.append('Duplicate Lexical Declarations')
        if not lexnameset.isdisjoint(set(self.ScriptBody.VarDeclaredNames())):
            errs.append('Var declaration mirrors lexical declaration')
        return [CreateSyntaxError(msg) for msg in errs]
class PN_Script_empty(PN_Script):
    def LexicallyDeclaredNames(self):
        return []
    def VarDeclaredNames(self):
        return []
    def VarScopedDeclarations(self):
        return []
    def LexicallyScopedDeclarations(self):
        return []
    def evaluate(self):
        # 15.1.7 Runtime Semantics: Evaluation
        # Script : [empty]
        #      1. Return NormalCompletion(undefined).
        return None
class PN_ScriptBody_StatementList(ParseNode):
    def __init__(self, ctx, p):
        super().__init__('ScriptBody', p)
        self.direct_eval = ctx.direct_eval
    @property
    def StatementList(self):
        return self.children[0]
    def EarlyErrors(self):
        # 15.1.1 Static Semantics: Early Errors
        # ScriptBody : StatementList
        # * It is a Syntax Error if StatementList Contains super unless the source code containing super is eval code that is
        #   being processed by a direct eval. Additional early error rules for super within direct eval are defined in
        #   18.2.1.1.
        # * It is a Syntax Error if StatementList Contains NewTarget unless the source code containing NewTarget is eval code
        #   that is being processed by a direct eval. Additional early error rules for NewTarget in direct eval are defined in
        #   18.2.1.1.
        # * It is a Syntax Error if ContainsDuplicateLabels of StatementList with argument « » is true.
        # * It is a Syntax Error if ContainsUndefinedBreakTarget of StatementList with argument « » is true.
        # * It is a Syntax Error if ContainsUndefinedContinueTarget of StatementList with arguments « » and « » is true.
        errs = []
        StatementList = self.StatementList
        if not self.direct_eval and StatementList.Contains('SUPER'):
            errs.append("'super' not allowed in this context")
        if not self.direct_eval and StatementList.Contains('NewTarget'):
            errs.append("'new.target' not allowed in this context")
        if StatementList.ContainsDuplicateLabels([]):
            errs.append('Duplicate Labels Detected')
        if StatementList.ContainsUndefinedBreakTarget([]):
            errs.append('Undefined Break Target')
        if StatementList.ContainsUndefinedContinueTarget([], []):
            errs.append('Undefined Continue Target')
        return [CreateSyntaxError(msg) for msg in errs]
    def IsStrict(self):
        # 15.1.2 Static Semantics: IsStrict
        # ScriptBody : StatementList
        #    1. If the Directive Prologue of StatementList contains a Use Strict Directive, return true; otherwise, return false.
        StatementList = self.children[0]
        dp = StatementList.DirectivePrologue()  # This is a list of strings, subsets of the source text
        return "'use strict'" in dp or '"use strict"' in dp
    def LexicallyDeclaredNames(self):
        return self.children[0].TopLevelLexicallyDeclaredNames()
    def VarDeclaredNames(self):
        return self.children[0].TopLevelVarDeclaredNames()
    def VarScopedDeclarations(self):
        return self.children[0].TopLevelVarScopedDeclarations()
    def LexicallyScopedDeclarations(self):
        return self.children[0].TopLevelLexicallyScopedDeclarations()

from sly import Parser
class Ecma262Parser(Parser):
    tokens = Lexer.tokens
    start = 'SpecialStart'
    debugfile = 'parser.out'

    def error(self, p):
        raise SyntaxError(f'Syntax Error in script. Offending token: {p!r}')

    class ParseContext:
        __slots__ = ['goal', 'strict', 'direct_eval', 'source_text']
        def __init__(self, goal, strict, source_text):
            self.goal = goal
            self.strict = strict
            self.direct_eval = False
            self.source_text = source_text
        def __repr__(self):
            return f'ParseContext(goal={self.goal}, strict={self.strict})'

    def __init__(self, strict=False, start=None, source_text=''):
        super().__init__()
        self.context = self.ParseContext(self.start, strict, source_text)

    # pylint: disable=function-redefined
    @_('GOAL_SCRIPT Script')  # pylint: disable=undefined-variable
    def SpecialStart(self, p):
        # The goal target for this parse is 'Script'
        return p[1]
    @_('GOAL_CALLMEMBEREXPRESSION CallMemberExpression')  # pylint: disable=undefined-variable
    def SpecialStart(self, p):
        # The goal target for this parse is 'CallMemberExpression'
        return p[1]
    @_('GOAL_PARENTHESIZEDEXPRESSION ParenthesizedExpression')  # pylint: disable=undefined-variable
    def SpecialStart(self, p):
        return p[1]
    @_('GOAL_ASSIGNMENTPATTERN AssignmentPattern')  # pylint: disable=undefined-variable
    def SpecialStart(self, p):
        return p[1]
    ########################################################################################################################
    # 15.1 Scripts
    # Syntax
    # Script :
    #       [empty]
    #       ScriptBody
    # ScriptBody :
    #       StatementList[~Yield, ~Await, ~Return]
    #
    @_('ScriptBody')  # pylint: disable=undefined-variable
    def Script(self, p):
        return PN_Script_ScriptBody(self.context, p)
    @_('empty')  # pylint: disable=undefined-variable
    def Script(self, p):
        return PN_Script_empty(self.context, p)
    @_('')  # pylint: disable=undefined-variable
    def empty(self, p):
        pass
    @_('StatementList')  # pylint: disable=undefined-variable
    def ScriptBody(self, p):
        return PN_ScriptBody_StatementList(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 14.8 Async Arrow Function Definitions
    #
    # Syntax
    #
    # @@@ Need to add the rest
    #
    # CoverCallExpressionAndAsyncArrowHead[Yield, Await] :
    #               MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
    @_('MemberExpression Arguments')  # pylint: disable=undefined-variable
    def CoverCallExpressionAndAsyncArrowHead(self, p):
        return PN_CoverCallExpressionAndAsyncArrowHead_MemberExpression_Arguments(self.context, p)
    @_('MemberExpression_Restricted Arguments')  # pylint: disable=undefined-variable
    def CoverCallExpressionAndAsyncArrowHead_Restricted(self, p):
        return PN_CoverCallExpressionAndAsyncArrowHead_MemberExpression_Arguments(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 14.1 Function Definitions
    #
    # Syntax
    #
    # FunctionDeclaration[Yield, Await, Default] :
    #           function BindingIdentifier[?Yield, ?Await] ( FormalParameters[~Yield, ~Await] ) { FunctionBody[~Yield, ~Await] }
    #           [+Default] function ( FormalParameters[~Yield, ~Await] ) { FunctionBody[~Yield, ~Await] }
    @_('FUNCTION BindingIdentifier LPAREN FormalParameters RPAREN LBRACKET FunctionBody RBRACKET')  # pylint: disable=undefined-variable
    def FunctionDeclaration(self, p):
        return PN_FUNCTION_BindingIdentifier_LPAREN_FormalParameters_RPAREN_LBRACKET_FunctionBody_RBRACKET(self.context, p)
    #
    # FunctionExpression :
    #           function BindingIdentifier[~Yield, ~Await] ( FormalParameters[~Yield, ~Await] ) { FunctionBody[~Yield, ~Await] }
    #           function ( FormalParameters[~Yield, ~Await] ) { FunctionBody[~Yield, ~Await] }
    #
    # UniqueFormalParameters[Yield, Await] :
    #           FormalParameters[?Yield, ?Await]
    #
    # FormalParameters[Yield, Await] :
    #           [empty]
    #           FunctionRestParameter[?Yield, ?Await]
    #           FormalParameterList[?Yield, ?Await]
    #           FormalParameterList[?Yield, ?Await] ,
    #           FormalParameterList[?Yield, ?Await] , FunctionRestParameter[?Yield, ?Await]
    @_('empty')  # pylint: disable=undefined-variable
    def FormalParameters(self, p):
        return PN_FormalParameters_empty(self.context, p)
    @_('FunctionRestParameter')  # pylint: disable=undefined-variable
    def FormalParameters(self, p):
        return PN_FormalParameters_FunctionRestParameter(self.context, p)
    @_('FormalParameterList')  # pylint: disable=undefined-variable
    def FormalParameters(self, p):
        return PN_FormalParameters_FormalParameterList(self.context, p)
    @_('FormalParameterList COMMA')  # pylint: disable=undefined-variable
    def FormalParameters(self, p):
        return PN_FormalParameters_FormalParameterList_COMMA(self.context, p)
    @_('FormalParameterList COMMA FunctionRestParameter')  # pylint: disable=undefined-variable
    def FormalParameters(self, p):
        return PN_FormalParameters_FormalParameterList_COMMA_FunctionRestParameter(self.context, p)
    #
    # FormalParameterList[Yield, Await] :
    #           FormalParameter[?Yield, ?Await]
    #           FormalParameterList[?Yield, ?Await] , FormalParameter[?Yield, ?Await]
    @_('FormalParameter')  # pylint: disable=undefined-variable
    def FormalParameterList(self, p):
        return PN_FormalParameterList_FormalParameter(self.context, p)
    @_('FormalParameterList COMMA FormalParameter')  # pylint: disable=undefined-variable
    def FormalParameterList(self, p):
        return PN_FormalParameterList_FormalParameterList_COMMA_FormalParameter(self.context, p)
    #
    # FunctionRestParameter[Yield, Await] :
    #           BindingRestElement[?Yield, ?Await]
    @_('BindingRestElement')  # pylint: disable=undefined-variable
    def FunctionRestParameter(self, p):
        return PN_FunctionRestParameter_BindingRestElement(self.context, p)
    #
    # FormalParameter[Yield, Await] :
    #           BindingElement[?Yield, ?Await]
    @_('BindingElement')  # pylint: disable=undefined-variable
    def FormalParameter(self, p):
        return PN_FormalParameter_BindingElement(self.context, p)
    #
    # FunctionBody[Yield, Await] :
    #           FunctionStatementList[?Yield, ?Await]
    @_('FunctionStatementList')  # pylint: disable=undefined-variable
    def FunctionBody(self, p):
        return PN_FunctionBody_FunctionStatementList(self.context, p)
    #
    # FunctionStatementList[Yield, Await] :
    #           [empty]
    #           StatementList[?Yield, ?Await, +Return]
    @_('empty')  # pylint: disable=undefined-variable
    def FunctionStatementList(self, p):
        return PN_FunctionStatementList_empty(self.context, p)
    @_('StatementList_Return')  # pylint: disable=undefined-variable
    def FunctionStatementList(self, p):
        return PN_FunctionStatementList_StatementList(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 13.8 The continue statement
    #
    # Syntax
    #
    # ContinueStatement[Yield, Await] :
    #               continue ;
    #               continue [no LineTerminator here] LabelIdentifier[?Yield, ?Await] ;
    #
    @_('CONTINUE SEMICOLON')  # pylint: disable=undefined-variable
    def ContinueStatement(self, p):
        return PN_ContinueStatement_CONTINUE_SEMICOLON(self.context, p)
    @_('CONTINUE LabelIdentifier SEMICOLON')  # pylint: disable=undefined-variable
    def ContinueStatement(self, p):
        return PN_ContinueStatement_CONTINUE_LabelIdentifier_SEMICOLON(self.context, p)
    ########################################################################################################################

    # LPAREN_  ---   ( [lookahead ∉ { let [ }]
    # LPAREN_LET --- ( [lookahead = let ]
    # LPAREN_LBRACKET --- ( [lookahead = [ ]
    @_('LPAREN_', 'LPAREN_LET', 'LPAREN_LBRACKET')  # pylint: disable=undefined-variable
    def LPAREN(self, p):    # LPAREN --- (
        return p[0]
    @_('LPAREN_', 'LPAREN_LBRACKET') # --- ( [lookahead != let ]  # pylint: disable=undefined-variable
    def LPAREN_NOTLET(self, p):
        return p[0]
    ########################################################################################################################
    # 13.7 Iteration Statements
    #
    # Syntax
    #
    # IterationStatement[Yield, Await, Return] :
    #           do Statement[?Yield, ?Await, ?Return] while ( Expression[+In, ?Yield, ?Await] ) ;
    #           while ( Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           for ( [lookahead ∉ { let [ }] Expression[~In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           for ( [lookahead ∉ { let [ }] Expression[~In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ; ) Statement[?Yield, ?Await, ?Return]
    #           for ( [lookahead ∉ { let [ }] Expression[~In, ?Yield, ?Await] ; ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           for ( [lookahead ∉ { let [ }] Expression[~In, ?Yield, ?Await] ; ; ) Statement[?Yield, ?Await, ?Return]
    #           for ( [lookahead ∉ { let [ }] ; Expression[+In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           for ( [lookahead ∉ { let [ }] ; Expression[+In, ?Yield, ?Await] ; ) Statement[?Yield, ?Await, ?Return]
    #           for ( [lookahead ∉ { let [ }] ; ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           for ( [lookahead ∉ { let [ }] ; ; ) Statement[?Yield, ?Await, ?Return]
    #           for ( var VariableDeclarationList[~In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           for ( var VariableDeclarationList[~In, ?Yield, ?Await] ; ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           for ( var VariableDeclarationList[~In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ; ) Statement[?Yield, ?Await, ?Return]
    #           for ( var VariableDeclarationList[~In, ?Yield, ?Await] ; ; ) Statement[?Yield, ?Await, ?Return]
    #           for ( LexicalDeclaration[~In, ?Yield, ?Await] Expression[+In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           for ( LexicalDeclaration[~In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           for ( LexicalDeclaration[~In, ?Yield, ?Await] Expression[+In, ?Yield, ?Await] ; ) Statement[?Yield, ?Await, ?Return]
    #           for ( LexicalDeclaration[~In, ?Yield, ?Await] ; ) Statement[?Yield, ?Await, ?Return]
    #           for ( [lookahead ∉ { let [ }] LeftHandSideExpression[?Yield, ?Await] in Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           for ( var ForBinding[?Yield, ?Await] in Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           for ( ForDeclaration[?Yield, ?Await] in Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           for ( [lookahead ≠ let] LeftHandSideExpression[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           for ( var ForBinding[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           for ( ForDeclaration[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           [+Await] for await ( [lookahead ≠ let] LeftHandSideExpression[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           [+Await] for await ( var ForBinding[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #           [+Await] for await ( ForDeclaration[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #
    # ForDeclaration[Yield, Await] :
    #           LetOrConst ForBinding[?Yield, ?Await]
    #
    # ForBinding[Yield, Await] :
    #           BindingIdentifier[?Yield, ?Await]
    #           BindingPattern[?Yield, ?Await]
    #
    @_('DO Statement WHILE LPAREN Expression_In RPAREN SEMICOLON')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_DO_Statement_WHILE_LPAREN_Expression_RPAREN_SEMICOLON(self.context, p)
    @_('WHILE LPAREN Expression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_WHILE_LPAREN_Expression_RPAREN_Statement(self.context, p)
    @_('FOR LPAREN_ Expression SEMICOLON Expression_In SEMICOLON Expression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_Expression_SEMICOLON_Expression_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    @_('FOR LPAREN_ Expression SEMICOLON Expression_In SEMICOLON RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_Expression_SEMICOLON_Expression_SEMICOLON_RPAREN_Statement(self.context, p)
    @_('FOR LPAREN_ Expression SEMICOLON SEMICOLON Expression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_Expression_SEMICOLON_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    @_('FOR LPAREN_ Expression SEMICOLON SEMICOLON RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_Expression_SEMICOLON_SEMICOLON_RPAREN_Statement(self.context, p)
    # for ( [lookahead ∉ { let [ }] ; Expression[+In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN_ SEMICOLON Expression_In SEMICOLON Expression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_SEMICOLON_Expression_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    # for ( [lookahead ∉ { let [ }] ; Expression[+In, ?Yield, ?Await] ; ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN_ SEMICOLON Expression_In SEMICOLON RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_SEMICOLON_Expression_SEMICOLON_RPAREN_Statement(self.context, p)
    # for ( [lookahead ∉ { let [ }] ; ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN_ SEMICOLON SEMICOLON Expression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_SEMICOLON_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    # for ( [lookahead ∉ { let [ }] ; ; ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN_ SEMICOLON SEMICOLON RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_SEMICOLON_SEMICOLON_RPAREN_Statement(self.context, p)
    #           for ( var VariableDeclarationList[~In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN VAR VariableDeclarationList SEMICOLON Expression_In SEMICOLON Expression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_VAR_VariableDeclarationList_SEMICOLON_Expression_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    #           for ( var VariableDeclarationList[~In, ?Yield, ?Await] ; ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN VAR VariableDeclarationList SEMICOLON SEMICOLON Expression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_VAR_VariableDeclarationList_SEMICOLON_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    #           for ( var VariableDeclarationList[~In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ; ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN VAR VariableDeclarationList SEMICOLON Expression_In SEMICOLON RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_VAR_VariableDeclarationList_SEMICOLON_Expression_SEMICOLON_RPAREN_Statement(self.context, p)
    #           for ( var VariableDeclarationList[~In, ?Yield, ?Await] ; ; ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN VAR VariableDeclarationList SEMICOLON SEMICOLON RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_VAR_VariableDeclarationList_SEMICOLON_SEMICOLON_RPAREN_Statement(self.context, p)
    #           for ( LexicalDeclaration[~In, ?Yield, ?Await] Expression[+In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN LexicalDeclaration Expression_In SEMICOLON Expression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_LexicalDeclaration_Expression_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    #           for ( LexicalDeclaration[~In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN LexicalDeclaration SEMICOLON Expression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_LexicalDeclaration_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    #           for ( LexicalDeclaration[~In, ?Yield, ?Await] Expression[+In, ?Yield, ?Await] ; ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN LexicalDeclaration Expression_In SEMICOLON RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_LexicalDeclaration_Expression_SEMICOLON_RPAREN_Statement(self.context, p)
    #           for ( LexicalDeclaration[~In, ?Yield, ?Await] ; ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN LexicalDeclaration SEMICOLON RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_LexicalDeclaration_SEMICOLON_RPAREN_Statement(self.context, p)
    #           for ( [lookahead ∉ { let [ }] LeftHandSideExpression[?Yield, ?Await] in Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN_ LeftHandSideExpression IN Expression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_LeftHandSideExpression_IN_Expression_RPAREN_Statement(self.context, p)
    #           for ( var ForBinding[?Yield, ?Await] in Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN VAR ForBinding IN Expression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_VAR_ForBinding_IN_Expression_RPAREN_Statement(self.context, p)
    #           for ( ForDeclaration[?Yield, ?Await] in Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN ForDeclaration IN Expression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_VAR_ForBinding_IN_Expression_RPAREN_Statement(self.context, p)
    #           for ( [lookahead ≠ let] LeftHandSideExpression[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN_NOTLET LeftHandSideExpression OF AssignmentExpression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_LeftHandSideExpression_OF_AssignmentExpression_RPAREN_Statement(self.context, p)
    #           for ( var ForBinding[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN VAR ForBinding OF AssignmentExpression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_VAR_ForBinding_OF_AssignmentExpression_RPAREN_Statement(self.context, p)
    #           for ( ForDeclaration[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN ForDeclaration OF AssignmentExpression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IterationStatement(self, p):
        return PN_IterationStatement_FOR_LPAREN_ForDeclaration_OF_AssignmentExpression_RPAREN_Statement(self.context, p)

    @_('DO Statement_Return WHILE LPAREN Expression_In RPAREN SEMICOLON')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_DO_Statement_WHILE_LPAREN_Expression_RPAREN_SEMICOLON(self.context, p)
    @_('WHILE LPAREN Expression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_WHILE_LPAREN_Expression_RPAREN_Statement(self.context, p)
    @_('FOR LPAREN_ Expression SEMICOLON Expression_In SEMICOLON Expression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_Expression_SEMICOLON_Expression_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    @_('FOR LPAREN_ Expression SEMICOLON Expression_In SEMICOLON RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_Expression_SEMICOLON_Expression_SEMICOLON_RPAREN_Statement(self.context, p)
    @_('FOR LPAREN_ Expression SEMICOLON SEMICOLON Expression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_Expression_SEMICOLON_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    @_('FOR LPAREN_ Expression SEMICOLON SEMICOLON RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_Expression_SEMICOLON_SEMICOLON_RPAREN_Statement(self.context, p)
    # for ( [lookahead ∉ { let [ }] ; Expression[+In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN_ SEMICOLON Expression_In SEMICOLON Expression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_SEMICOLON_Expression_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    # for ( [lookahead ∉ { let [ }] ; Expression[+In, ?Yield, ?Await] ; ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN_ SEMICOLON Expression_In SEMICOLON RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_SEMICOLON_Expression_SEMICOLON_RPAREN_Statement(self.context, p)
    # for ( [lookahead ∉ { let [ }] ; ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN_ SEMICOLON SEMICOLON Expression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_SEMICOLON_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    # for ( [lookahead ∉ { let [ }] ; ; ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN_ SEMICOLON SEMICOLON RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_SEMICOLON_SEMICOLON_RPAREN_Statement(self.context, p)
    #           for ( var VariableDeclarationList[~In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN VAR VariableDeclarationList SEMICOLON Expression_In SEMICOLON Expression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_VAR_VariableDeclarationList_SEMICOLON_Expression_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    #           for ( var VariableDeclarationList[~In, ?Yield, ?Await] ; ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN VAR VariableDeclarationList SEMICOLON SEMICOLON Expression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_VAR_VariableDeclarationList_SEMICOLON_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    #           for ( var VariableDeclarationList[~In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ; ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN VAR VariableDeclarationList SEMICOLON Expression_In SEMICOLON RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_VAR_VariableDeclarationList_SEMICOLON_Expression_SEMICOLON_RPAREN_Statement(self.context, p)
    #           for ( var VariableDeclarationList[~In, ?Yield, ?Await] ; ; ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN VAR VariableDeclarationList SEMICOLON SEMICOLON RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_VAR_VariableDeclarationList_SEMICOLON_SEMICOLON_RPAREN_Statement(self.context, p)
    #           for ( LexicalDeclaration[~In, ?Yield, ?Await] Expression[+In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN LexicalDeclaration Expression_In SEMICOLON Expression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_LexicalDeclaration_Expression_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    #           for ( LexicalDeclaration[~In, ?Yield, ?Await] ; Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN LexicalDeclaration SEMICOLON Expression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_LexicalDeclaration_SEMICOLON_Expression_RPAREN_Statement(self.context, p)
    #           for ( LexicalDeclaration[~In, ?Yield, ?Await] Expression[+In, ?Yield, ?Await] ; ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN LexicalDeclaration Expression_In SEMICOLON RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_LexicalDeclaration_Expression_SEMICOLON_RPAREN_Statement(self.context, p)
    #           for ( LexicalDeclaration[~In, ?Yield, ?Await] ; ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN LexicalDeclaration SEMICOLON RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_LexicalDeclaration_SEMICOLON_RPAREN_Statement(self.context, p)
    #           for ( [lookahead ∉ { let [ }] LeftHandSideExpression[?Yield, ?Await] in Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN_ LeftHandSideExpression IN Expression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_LeftHandSideExpression_IN_Expression_RPAREN_Statement(self.context, p)
    #           for ( var ForBinding[?Yield, ?Await] in Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN VAR ForBinding IN Expression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_VAR_ForBinding_IN_Expression_RPAREN_Statement(self.context, p)
    #           for ( ForDeclaration[?Yield, ?Await] in Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN ForDeclaration IN Expression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_VAR_ForBinding_IN_Expression_RPAREN_Statement(self.context, p)
    #           for ( [lookahead ≠ let] LeftHandSideExpression[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN_NOTLET LeftHandSideExpression OF AssignmentExpression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_LeftHandSideExpression_OF_AssignmentExpression_RPAREN_Statement(self.context, p)
    #           for ( var ForBinding[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN VAR ForBinding OF AssignmentExpression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_VAR_ForBinding_OF_AssignmentExpression_RPAREN_Statement(self.context, p)
    #           for ( ForDeclaration[?Yield, ?Await] of AssignmentExpression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    @_('FOR LPAREN ForDeclaration OF AssignmentExpression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IterationStatement_Return(self, p):
        return PN_IterationStatement_FOR_LPAREN_ForDeclaration_OF_AssignmentExpression_RPAREN_Statement(self.context, p)

    @_('LetOrConst ForBinding')  # pylint: disable=undefined-variable
    def ForDeclaration(self, p):
        return PN_ForDeclaration_LetOrConst_ForBinding(self.context, p)

    @_('BindingIdentifier')  # pylint: disable=undefined-variable
    def ForBinding(self, p):
        return PN_ForBinding_BindingIdentifier(self.context, p)
    @_('BindingPattern')  # pylint: disable=undefined-variable
    def ForBinding(self, p):
        return PN_ForBinding_BindingPattern(self.context, p)
    ########################################################################################################################
    # 13.6 The if Statement
    #
    # Syntax
    #
    # IfStatement[Yield, Await, Return] :
    #           if ( Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return] else Statement[?Yield, ?Await, ?Return]
    #           if ( Expression[+In, ?Yield, ?Await] ) Statement[?Yield, ?Await, ?Return]
    #
    # Each else for which the choice of associated if is ambiguous shall be associated with the nearest possible if
    # that would otherwise have no corresponding else.
    ########################################################################################################################
    @_('IF LPAREN Expression_In RPAREN Statement ELSE Statement')  # pylint: disable=undefined-variable
    def IfStatement(self, p):
        return PN_IfStatement_IF_LPAREN_Expression_RPAREN_Statement_ELSE_Statement(self.context, p)
    @_('IF LPAREN Expression_In RPAREN Statement')  # pylint: disable=undefined-variable
    def IfStatement(self, p):
        return PN_IfStatement_IF_LPAREN_Expression_RPAREN_Statement(self.context, p)
    @_('IF LPAREN Expression_In RPAREN Statement_Return ELSE Statement_Return')  # pylint: disable=undefined-variable
    def IfStatement_Return(self, p):
        return PN_IfStatement_IF_LPAREN_Expression_RPAREN_Statement_ELSE_Statement(self.context, p)
    @_('IF LPAREN Expression_In RPAREN Statement_Return')  # pylint: disable=undefined-variable
    def IfStatement_Return(self, p):
        return PN_IfStatement_IF_LPAREN_Expression_RPAREN_Statement(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 13.4 Empty Statement
    #
    # Syntax
    #
    # EmptyStatement :
    #       ;
    #
    @_('SEMICOLON')  # pylint: disable=undefined-variable
    def EmptyStatement(self, p):
        return PN_EmptyStatement_SEMICOLON(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 13.3.3 Destructuring Binding Patterns
    #
    # Syntax
    #
    # BindingPattern[Yield, Await] :
    #           ObjectBindingPattern[?Yield, ?Await]
    #           ArrayBindingPattern[?Yield, ?Await]
    @_('ObjectBindingPattern')  # pylint: disable=undefined-variable
    def BindingPattern(self, p):
        return PN_BindingPattern_ObjectBindingPattern(self.context, p)
    @_('ArrayBindingPattern')  # pylint: disable=undefined-variable
    def BindingPattern(self, p):
        return PN_BindingPattern_ArrayBindingPattern(self.context, p)
    #
    # ObjectBindingPattern[Yield, Await] :
    #           { }
    #           { BindingRestProperty[?Yield, ?Await] }
    #           { BindingPropertyList[?Yield, ?Await] }
    #           { BindingPropertyList[?Yield, ?Await] , }
    #           { BindingPropertyList[?Yield, ?Await] , BindingRestProperty[?Yield, ?Await] }
    @_('LCURLY RCURLY')  # pylint: disable=undefined-variable
    def ObjectBindingPattern(self, p):
        return PN_ObjectBindingPattern_LCURLY_RCURLY(self.context, p)
    @_('LCURLY BindingRestProperty RCURLY')  # pylint: disable=undefined-variable
    def ObjectBindingPattern(self, p):
        return PN_ObjectBindingPattern_LCURLY_BindingRestProperty_RCURLY(self.context, p)
    @_('LCURLY BindingPropertyList RCURLY')  # pylint: disable=undefined-variable
    def ObjectBindingPattern(self, p):
        return PN_ObjectBindingPattern_LCURLY_BindingPropertyList_RCURLY(self.context, p)
    @_('LCURLY BindingPropertyList COMMA RCURLY')  # pylint: disable=undefined-variable
    def ObjectBindingPattern(self, p):
        return PN_ObjectBindingPattern_LCURLY_BindingPropertyList_COMMA_RCURLY(self.context, p)
    @_('LCURLY BindingPropertyList COMMA BindingRestProperty RCURLY')  # pylint: disable=undefined-variable
    def ObjectBindingPattern(self, p):
        return PN_ObjectBindingPattern_LCURLY_BindingPropertyList_COMMA_BindingRestProperty_RCURLY(self.context, p)
    #
    # ArrayBindingPattern[Yield, Await] :
    #           [ ]
    #           [ Elision ]
    #           [ BindingRestElement[?Yield, ?Await] ]
    #           [ Elision BindingRestElement[?Yield, ?Await] ]
    #           [ BindingElementList[?Yield, ?Await] ]
    #           [ BindingElementList[?Yield, ?Await] , ]
    #           [ BindingElementList[?Yield, ?Await] , Elision ]
    #           [ BindingElementList[?Yield, ?Await] , BindingRestElement[?Yield, ?Await] ]
    #           [ BindingElementList[?Yield, ?Await] , Elision BindingRestElement[?Yield, ?Await] ]
    @_('LBRACKET RBRACKET')  # pylint: disable=undefined-variable
    def ArrayBindingPattern(self, p):
        return PN_ArrayBindingPattern_LBRACKET_RBRACKET(self.context, p)
    @_('LBRACKET Elision RBRACKET')  # pylint: disable=undefined-variable
    def ArrayBindingPattern(self, p):
        return PN_ArrayBindingPattern_LBRACKET_Elision_RBRACKET(self.context, p)
    @_('LBRACKET BindingRestElement RBRACKET')  # pylint: disable=undefined-variable
    def ArrayBindingPattern(self, p):
        return PN_ArrayBindingPattern_LBRACKET_BindingRestElement_RBRACKET(self.context, p)
    @_('LBRACKET Elision BindingRestElement RBRACKET')  # pylint: disable=undefined-variable
    def ArrayBindingPattern(self, p):
        return PN_ArrayBindingPattern_LBRACKET_Elision_BindingRestElement_RBRACKET(self.context, p)
    @_('LBRACKET BindingElementList RBRACKET')  # pylint: disable=undefined-variable
    def ArrayBindingPattern(self, p):
        return PN_ArrayBindingPattern_LBRACKET_BindingElementList_RBRACKET(self.context, p)
    @_('LBRACKET BindingElementList COMMA RBRACKET')  # pylint: disable=undefined-variable
    def ArrayBindingPattern(self, p):
        return PN_ArrayBindingPattern_LBRACKET_BindingElementList_COMMA_RBRACKET(self.context, p)
    @_('LBRACKET BindingElementList COMMA Elision RBRACKET')  # pylint: disable=undefined-variable
    def ArrayBindingPattern(self, p):
        return PN_ArrayBindingPattern_LBRACKET_BindingElementList_COMMA_Elision_RBRACKET(self.context, p)
    @_('LBRACKET BindingElementList COMMA BindingRestElement RBRACKET')  # pylint: disable=undefined-variable
    def ArrayBindingPattern(self, p):
        return PN_ArrayBindingPattern_LBRACKET_BindingElementList_COMMA_BindingRestElement_RBRACKET(self.context, p)
    @_('LBRACKET BindingElementList COMMA Elision BindingRestElement RBRACKET')  # pylint: disable=undefined-variable
    def ArrayBindingPattern(self, p):
        return PN_ArrayBindingPattern_LBRACKET_BindingElementList_COMMA_Elision_BindingRestElement_RBRACKET(self.context, p)
    #
    # BindingRestProperty[Yield, Await] :
    #           ... BindingIdentifier[?Yield, ?Await]
    @_('DOTDOTDOT BindingIdentifier')  # pylint: disable=undefined-variable
    def BindingRestProperty(self, p):
        return PN_BindingRestProperty_DOTDOTDOT_BindingIdentifier(self.context, p)
    #
    # BindingPropertyList[Yield, Await] :
    #           BindingProperty[?Yield, ?Await]
    #           BindingPropertyList[?Yield, ?Await] , BindingProperty[?Yield, ?Await]
    @_('BindingProperty')  # pylint: disable=undefined-variable
    def BindingPropertyList(self, p):
        return PN_BindingPropertyList_BindingProperty(self.context, p)
    @_('BindingPropertyList COMMA BindingProperty')  # pylint: disable=undefined-variable
    def BindingPropertyList(self, p):
        return PN_BindingPropertyList_BindingPropertyList_COMMA_BindingProperty(self.context, p)
    #
    # BindingElementList[Yield, Await] :
    #           BindingElisionElement[?Yield, ?Await]
    #           BindingElementList[?Yield, ?Await] , BindingElisionElement[?Yield, ?Await]
    @_('BindingElisionElement')  # pylint: disable=undefined-variable
    def BindingElementList(self, p):
        return PN_BindingElementList_BindingElisionElement(self.context, p)
    @_('BindingElementList COMMA BindingElisionElement')  # pylint: disable=undefined-variable
    def BindingElementList(self, p):
        return PN_BindingElementList_BindingElementList_COMMA_BindingElisionElement(self.context, p)
    #
    # BindingElisionElement[Yield, Await] :
    #           BindingElement[?Yield, ?Await]
    #           Elision BindingElement[?Yield, ?Await]
    @_('BindingElement')  # pylint: disable=undefined-variable
    def BindingElisionElement(self, p):
        return PN_BindingElisionElement_BindingElement(self.context, p)
    @_('Elision BindingElement')  # pylint: disable=undefined-variable
    def BindingElisionElement(self, p):
        return PN_BindingElisionElement_Elision_BindingElement(self.context, p)
    #
    # BindingProperty[Yield, Await] :
    #           SingleNameBinding[?Yield, ?Await]
    #           PropertyName[?Yield, ?Await] : BindingElement[?Yield, ?Await]
    @_('SingleNameBinding')  # pylint: disable=undefined-variable
    def BindingProperty(self, p):
        return PN_BindingProperty_SingleNameBinding(self.context, p)
    @_('PropertyName COLON BindingElement')  # pylint: disable=undefined-variable
    def BindingProperty(self, p):
        return PN_BindingProperty_PropertyName_COLON_BindingElement(self.context, p)
    #
    # BindingElement[Yield, Await] :
    #           SingleNameBinding[?Yield, ?Await]
    #           BindingPattern[?Yield, ?Await]
    #           BindingPattern[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]
    @_('SingleNameBinding')  # pylint: disable=undefined-variable
    def BindingElement(self, p):
        return PN_BindingElement_SingleNameBinding(self.context, p)
    @_('BindingPattern')  # pylint: disable=undefined-variable
    def BindingElement(self, p):
        return PN_BindingElement_BindingPattern(self.context, p)
    @_('BindingPattern Initializer_In')  # pylint: disable=undefined-variable
    def BindingElement(self, p):
        return PN_BindingElement_BindingPattern_Initializer(self.context, p)
    #
    # SingleNameBinding[Yield, Await] :
    #           BindingIdentifier[?Yield, ?Await]
    #           BindingIdentifier[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]
    @_('BindingIdentifier')  # pylint: disable=undefined-variable
    def SingleNameBinding(self, p):
        return PN_SingleNameBinding_BindingIdentifier(self.context, p)
    @_('BindingIdentifier Initializer_In')  # pylint: disable=undefined-variable
    def SingleNameBinding(self, p):
        return PN_SingleNameBinding_BindingIdentifier_Initializer(self.context, p)
    #
    # BindingRestElement[Yield, Await] :
    #           ... BindingIdentifier[?Yield, ?Await]
    #           ... BindingPattern[?Yield, ?Await]
    @_('DOTDOTDOT BindingIdentifier')  # pylint: disable=undefined-variable
    def BindingRestElement(self, p):
        return PN_BindingRestElement_DOTDOTDOT_BindingIdentifier(self.context, p)
    @_('DOTDOTDOT BindingPattern')  # pylint: disable=undefined-variable
    def BindingRestElement(self, p):
        return PN_BindingRestElement_DOTDOTDOT_BindingPattern(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 13.3.2 Variable Statement
    # NOTE
    # A var statement declares variables that are scoped to the running execution context's VariableEnvironment. Var
    # variables are created when their containing Lexical Environment is instantiated and are initialized to undefined
    # when created. Within the scope of any VariableEnvironment a common BindingIdentifier may appear in more than one
    # VariableDeclaration but those declarations collectively define only one variable. A variable defined by a
    # VariableDeclaration with an Initializer is assigned the value of its Initializer's AssignmentExpression when the
    # VariableDeclaration is executed, not when the variable is created.
    #
    # Syntax
    #
    # VariableStatement[Yield, Await] :
    #       var VariableDeclarationList[+In, ?Yield, ?Await] ;
    #
    # VariableDeclarationList[In, Yield, Await] :
    #       VariableDeclaration[?In, ?Yield, ?Await]
    #       VariableDeclarationList[?In, ?Yield, ?Await] , VariableDeclaration[?In, ?Yield, ?Await]
    #
    # VariableDeclaration[In, Yield, Await] :
    #       BindingIdentifier[?Yield, ?Await]
    #       BindingIdentifier[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]
    #       BindingPattern[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]
    #
    @_('VAR VariableDeclarationList_In SEMICOLON')  # pylint: disable=undefined-variable
    def VariableStatement(self, p):
        return PN_VariableStatement_VAR_VariableDeclarationList(self.context, p)
    @_('VariableDeclaration_In')  # pylint: disable=undefined-variable
    def VariableDeclarationList_In(self, p):
        return PN_VariableDeclarationList_VariableDeclaration(self.context, p)
    @_('VariableDeclaration')  # pylint: disable=undefined-variable
    def VariableDeclarationList(self, p):
        return PN_VariableDeclarationList_VariableDeclaration(self.context, p)
    @_('VariableDeclarationList_In COMMA VariableDeclaration_In')  # pylint: disable=undefined-variable
    def VariableDeclarationList_In(self, p):
        return PN_VariableDeclarationList_VariableDeclarationList_COMMA_VariableDeclaration(self.context, p)
    @_('VariableDeclarationList COMMA VariableDeclaration')  # pylint: disable=undefined-variable
    def VariableDeclarationList(self, p):
        return PN_VariableDeclarationList_VariableDeclarationList_COMMA_VariableDeclaration(self.context, p)
    @_('BindingIdentifier')  # pylint: disable=undefined-variable
    def VariableDeclaration_In(self, p):
        return PN_VariableDeclaration_BindingIdentifier(self.context, p)
    @_('BindingIdentifier Initializer_In')  # pylint: disable=undefined-variable
    def VariableDeclaration_In(self, p):
        return PN_VariableDeclaration_BindingIdentifier_Initializer(self.context, p)
    @_('BindingPattern Initializer_In')  # pylint: disable=undefined-variable
    def VariableDeclaration_In(self, p):
        return PN_VariableDeclaration_BindingPattern_Initializer(self.context, p)
    @_('BindingIdentifier')  # pylint: disable=undefined-variable
    def VariableDeclaration(self, p):
        return PN_VariableDeclaration_BindingIdentifier(self.context, p)
    @_('BindingIdentifier Initializer')  # pylint: disable=undefined-variable
    def VariableDeclaration(self, p):
        return PN_VariableDeclaration_BindingIdentifier_Initializer(self.context, p)
    @_('BindingPattern Initializer')  # pylint: disable=undefined-variable
    def VariableDeclaration(self, p):
        return PN_VariableDeclaration_BindingPattern_Initializer(self.context, p)
    ########################################################################################################################

    # 13.3.1
    # LexicalDeclaration[In, Yield, Await] :
    #           LetOrConst BindingList[?In, ?Yield, ?Await] ;
    #
    # LetOrConst :
    #           let
    #           const
    #
    # BindingList[In, Yield, Await] :
    #           LexicalBinding[?In, ?Yield, ?Await]
    #           BindingList[?In, ?Yield, ?Await] , LexicalBinding[?In, ?Yield, ?Await]
    #
    # LexicalBinding[In, Yield, Await] :
    #           BindingIdentifier[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]
    #           BindingIdentifier[?Yield, ?Await]
    #           BindingPattern[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]
    #
    @_('LetOrConst BindingList SEMICOLON')  # pylint: disable=undefined-variable
    def LexicalDeclaration(self, p):
        return PN_LexicalDeclaration_LetOrConst_BindingList_SEMICOLON(self.context, p)
    @_('LetOrConst BindingList_In SEMICOLON')  # pylint: disable=undefined-variable
    def LexicalDeclaration_In(self, p):
        return PN_LexicalDeclaration_LetOrConst_BindingList_SEMICOLON(self.context, p)

    @_('LET')  # pylint: disable=undefined-variable
    def LetOrConst(self, p):
        return PN_LetOrConst_LET(self.context, p)
    @_('CONST')  # pylint: disable=undefined-variable
    def LetOrConst(self, p):
        return PN_LetOrConst_CONST(self.context, p)

    @_('LexicalBinding')  # pylint: disable=undefined-variable
    def BindingList(self, p):
        return PN_BindingList_LexicalBinding(self.context, p)
    @_('BindingList COMMA LexicalBinding')  # pylint: disable=undefined-variable
    def BindingList(self, p):
        return PN_BindingList_BindingList_COMMA_LexicalBinding(self.context, p)
    @_('LexicalBinding_In')  # pylint: disable=undefined-variable
    def BindingList_In(self, p):
        return PN_BindingList_LexicalBinding(self.context, p)
    @_('BindingList_In COMMA LexicalBinding_In')  # pylint: disable=undefined-variable
    def BindingList_In(self, p):
        return PN_BindingList_BindingList_COMMA_LexicalBinding(self.context, p)

    @_('BindingIdentifier Initializer')  # pylint: disable=undefined-variable
    def LexicalBinding(self, p):
        return PN_LexicalBinding_BindingIdentifier_Initializer(self.context, p)
    @_('BindingIdentifier')  # pylint: disable=undefined-variable
    def LexicalBinding(self, p):
        return PN_LexicalBinding_BindingIdentifier(self.context, p)
    @_('BindingPattern Initializer')  # pylint: disable=undefined-variable
    def LexicalBinding(self, p):
        return PN_LexicalBinding_BindingPattern_Initializer(self.context, p)
    @_('BindingIdentifier Initializer_In')  # pylint: disable=undefined-variable
    def LexicalBinding_In(self, p):
        return PN_LexicalBinding_BindingIdentifier_Initializer(self.context, p)
    @_('BindingIdentifier')  # pylint: disable=undefined-variable
    def LexicalBinding_In(self, p):
        return PN_LexicalBinding_BindingIdentifier(self.context, p)
    @_('BindingPattern Initializer_In')  # pylint: disable=undefined-variable
    def LexicalBinding_In(self, p):
        return PN_LexicalBinding_BindingPattern_Initializer(self.context, p)
    ########################################################################################################################
    # 13.2 Block
    # Syntax
    #
    # BlockStatement[Yield, Await, Return] :
    #       Block[?Yield, ?Await, ?Return]
    #
    # Block[Yield, Await, Return] :
    #       { }
    #       { StatementList[?Yield, ?Await, ?Return] }
    #
    # StatementList[Yield, Await, Return] :
    #       StatementListItem[?Yield, ?Await, ?Return]
    #       StatementList[?Yield, ?Await, ?Return] StatementListItem[?Yield, ?Await, ?Return]
    #
    # StatementListItem[Yield, Await, Return] :
    #       Statement[?Yield, ?Await, ?Return]
    #       Declaration[?Yield, ?Await]
    #
    @_('Block')  # pylint: disable=undefined-variable
    def BlockStatement(self, p):
        return PN_BlockStatement_Block(self.context, p)
    @_('Block_Return')  # pylint: disable=undefined-variable
    def BlockStatement_Return(self, p):
        return PN_BlockStatement_Block(self.context, p)

    @_('LCURLY RCURLY')  # pylint: disable=undefined-variable
    def Block(self, p):
        return PN_Block_LCURLY_RCURLY(self.context, p)
    @_('LCURLY StatementList RCURLY')  # pylint: disable=undefined-variable
    def Block(self, p):
        return PN_Block_LCURLY_StatementList_RCURLY(self.context, p)
    @_('LCURLY RCURLY')  # pylint: disable=undefined-variable
    def Block_Return(self, p):
        return PN_Block_LCURLY_RCURLY(self.context, p)
    @_('LCURLY StatementList_Return RCURLY')  # pylint: disable=undefined-variable
    def Block_Return(self, p):
        return PN_Block_LCURLY_StatementList_RCURLY(self.context, p)

    @_('StatementListItem')  # pylint: disable=undefined-variable
    def StatementList(self, p):
        return PN_StatementList_StatementListItem(self.context, p)
    @_('StatementList StatementListItem')  # pylint: disable=undefined-variable
    def StatementList(self, p):
        return PN_StatementList_StatementList_StatementListItem(self.context, p)
    @_('StatementListItem_Return')  # pylint: disable=undefined-variable
    def StatementList_Return(self, p):
        return PN_StatementList_StatementListItem(self.context, p)
    @_('StatementList_Return StatementListItem_Return')  # pylint: disable=undefined-variable
    def StatementList_Return(self, p):
        return PN_StatementList_StatementList_StatementListItem(self.context, p)

    @_('Statement')  # pylint: disable=undefined-variable
    def StatementListItem(self, p):
        return PN_StatementListItem_Statement(self.context, p)
    @_('Declaration')  # pylint: disable=undefined-variable
    def StatementListItem(self, p):
        return PN_StatementListItem_Declaration(self.context, p)
    @_('Statement_Return')  # pylint: disable=undefined-variable
    def StatementListItem_Return(self, p):
        return PN_StatementListItem_Statement(self.context, p)
    @_('Declaration')  # pylint: disable=undefined-variable
    def StatementListItem_Return(self, p):
        return PN_StatementListItem_Declaration(self.context, p)
    ########################################################################################################################
    # 13 ECMAScript Language: Statements and Declarations
    #
    # Syntax
    #
    # Statement[Yield, Await, Return] :
    #       BlockStatement[?Yield, ?Await, ?Return]
    #       VariableStatement[?Yield, ?Await]
    #       EmptyStatement
    #       ExpressionStatement[?Yield, ?Await]
    #       IfStatement[?Yield, ?Await, ?Return]
    #       BreakableStatement[?Yield, ?Await, ?Return]
    #       ContinueStatement[?Yield, ?Await]
    #       BreakStatement[?Yield, ?Await]
    #       [+Return]ReturnStatement[?Yield, ?Await]
    #       WithStatement[?Yield, ?Await, ?Return]
    #       LabelledStatement[?Yield, ?Await, ?Return]
    #       ThrowStatement[?Yield, ?Await]
    #       TryStatement[?Yield, ?Await, ?Return]
    #       DebuggerStatement
    #
    # Declaration[Yield, Await] :
    #       HoistableDeclaration[?Yield, ?Await, ~Default]
    #       ClassDeclaration[?Yield, ?Await, ~Default]
    #       LexicalDeclaration[+In, ?Yield, ?Await]
    #
    # HoistableDeclaration[Yield, Await, Default] :
    #       FunctionDeclaration[?Yield, ?Await, ?Default]
    #       GeneratorDeclaration[?Yield, ?Await, ?Default]
    #       AsyncFunctionDeclaration[?Yield, ?Await, ?Default]
    #       AsyncGeneratorDeclaration[?Yield, ?Await, ?Default]
    #
    # BreakableStatement[Yield, Await, Return] :
    #       IterationStatement[?Yield, ?Await, ?Return]
    #       SwitchStatement[?Yield, ?Await, ?Return]
    #
    @_('BlockStatement')  # pylint: disable=undefined-variable
    def Statement(self, p):
        return PN_Statement_BlockStatement(self.context, p)
    @_('VariableStatement')  # pylint: disable=undefined-variable
    def Statement(self, p):
        return PN_Statement_VariableStatement(self.context, p)
    @_('ExpressionStatement')  # pylint: disable=undefined-variable
    def Statement(self, p):
        return PN_Statement_ExpressionStatement(self.context, p)
    @_('EmptyStatement')  # pylint: disable=undefined-variable
    def Statement(self, p):
        return PN_Statement_EmptyStatement(self.context, p)
    @_('IfStatement')  # pylint: disable=undefined-variable
    def Statement(self, p):
        return PN_Statement_IfStatement(self.context, p)
    @_('BreakableStatement')  # pylint: disable=undefined-variable
    def Statement(self, p):
        return PN_Statement_BreakableStatement(self.context, p)
    @_('ContinueStatement')  # pylint: disable=undefined-variable
    def Statement(self, p):
        return PN_Statement_ContinueStatement(self.context, p)
    @_('BlockStatement_Return')  # pylint: disable=undefined-variable
    def Statement_Return(self, p):
        return PN_Statement_BlockStatement(self.context, p)
    @_('VariableStatement')  # pylint: disable=undefined-variable
    def Statement_Return(self, p):
        return PN_Statement_VariableStatement(self.context, p)
    @_('ExpressionStatement')  # pylint: disable=undefined-variable
    def Statement_Return(self, p):
        return PN_Statement_ExpressionStatement(self.context, p)
    @_('EmptyStatement')  # pylint: disable=undefined-variable
    def Statement_Return(self, p):
        return PN_Statement_EmptyStatement(self.context, p)
    @_('IfStatement_Return')  # pylint: disable=undefined-variable
    def Statement_Return(self, p):
        return PN_Statement_IfStatement(self.context, p)
    @_('BreakableStatement_Return')  # pylint: disable=undefined-variable
    def Statement_Return(self, p):
        return PN_Statement_BreakableStatement(self.context, p)
    @_('ContinueStatement')  # pylint: disable=undefined-variable
    def Statement_Return(self, p):
        return PN_Statement_ContinueStatement(self.context, p)

    @_('LexicalDeclaration_In')  # pylint: disable=undefined-variable
    def Declaration(self, p):
        return PN_Declaration_LexicalDeclaration(self.context, p)
    @_('HoistableDeclaration')  # pylint: disable=undefined-variable
    def Declaration(self, p):
        return PN_Declaration_HoistableDeclaration(self.context, p)

    @_('FunctionDeclaration')  # pylint: disable=undefined-variable
    def HoistableDeclaration(self, p):
        return PN_HoistableDeclaration_FunctionDeclaration(self.context, p)

    @_('IterationStatement')  # pylint: disable=undefined-variable
    def BreakableStatement(self, p):
        return PN_BreakableStatement_IterationStatement(self.context, p)
    #@_('SwitchStatement')
    #def BreakableStatement(self, p):
    #    return PN_BreakableStatement_SwitchStatement(self.context, p)
    @_('IterationStatement_Return')  # pylint: disable=undefined-variable
    def BreakableStatement_Return(self, p):
        return PN_BreakableStatement_IterationStatement(self.context, p)
    #@_('SwitchStatement_Return')
    #def BreakableStatement_Return(self, p):
    #    return PN_BreakableStatement_SwitchStatement(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 13.5 Expression Statement
    #
    # Syntax
    #
    # ExpressionStatement[Yield, Await] :
    #       [lookahead ∉ { {, function, async [no LineTerminator here] function, class, let [ }] Expression[+In, ?Yield, ?Await] ;
    #
    # NOTE
    # An ExpressionStatement cannot start with a U+007B (LEFT CURLY BRACKET) because that might make it ambiguous with
    # a Block. An ExpressionStatement cannot start with the function or class keywords because that would make it
    # ambiguous with a FunctionDeclaration, a GeneratorDeclaration, or a ClassDeclaration. An ExpressionStatement
    # cannot start with async function because that would make it ambiguous with an AsyncFunctionDeclaration or a
    # AsyncGeneratorDeclaration. An ExpressionStatement cannot start with the two token sequence let [ because that
    # would make it ambiguous with a let LexicalDeclaration whose first LexicalBinding was an ArrayBindingPattern.
    #
    @_('Expression_In_Restricted SEMICOLON')  # pylint: disable=undefined-variable
    def ExpressionStatement(self, p):
        return PN_ExpressionStatement_Expression(self.context, p)
    ########################################################################################################################
    @_('AssignmentExpression')  # pylint: disable=undefined-variable
    def Expression(self, p):
        return PN_Expression_AssignmentExpression(self.context, p)
    @_('Expression COMMA AssignmentExpression')  # pylint: disable=undefined-variable
    def Expression(self, p):
        return PN_Expression_Expression_COMMA_AssignmentExpression(self.context, p)
    @_('AssignmentExpression_In')  # pylint: disable=undefined-variable
    def Expression_In(self, p):
        return PN_Expression_AssignmentExpression(self.context, p)
    @_('Expression_In COMMA AssignmentExpression_In')  # pylint: disable=undefined-variable
    def Expression_In(self, p):
        return PN_Expression_Expression_COMMA_AssignmentExpression(self.context, p)
    @_('AssignmentExpression_In_Restricted')  # pylint: disable=undefined-variable
    def Expression_In_Restricted(self, p):
        return PN_Expression_AssignmentExpression(self.context, p)
    @_('Expression_In_Restricted COMMA AssignmentExpression_In')  # pylint: disable=undefined-variable
    def Expression_In_Restricted(self, p):
        return PN_Expression_Expression_COMMA_AssignmentExpression(self.context, p)

    ########################################################################################################################
    # 12.15 Assignment Operators
    #
    # Syntax
    #
    # AssignmentExpression[In, Yield, Await] :
    #       ConditionalExpression[?In, ?Yield, ?Await]
    #       [+Yield] YieldExpression[?In, ?Await]
    #       ArrowFunction[?In, ?Yield, ?Await]
    #       AsyncArrowFunction[?In, ?Yield, ?Await]
    #       LeftHandSideExpression[?Yield, ?Await] = AssignmentExpression[?In, ?Yield, ?Await]
    #       LeftHandSideExpression[?Yield, ?Await] AssignmentOperator AssignmentExpression[?In, ?Yield, ?Await]
    #
    # AssignmentOperator : one of
    #       *= /= %= += -= <<= >>= >>>= &= ^= |= **=
    #
    @_('ConditionalExpression')  # pylint: disable=undefined-variable
    def AssignmentExpression(self, p):
        return PN_AssignmentExpression_ConditionalExpression(self.context, p)
    @_('LeftHandSideExpression EQUALS AssignmentExpression')  # pylint: disable=undefined-variable
    def AssignmentExpression(self, p):
        return PN_AssignmentExpression_LeftHandSideExpression_EQUALS_AssignmentExpression(self.context, p)
    @_('LeftHandSideExpression AssignmentOperator AssignmentExpression')  # pylint: disable=undefined-variable
    def AssignmentExpression(self, p):
        return PN_AssignmentExpression_LeftHandSideExpression_AssignmentOperator_AssignmentExpression(self.context, p)
    @_('ConditionalExpression_In')  # pylint: disable=undefined-variable
    def AssignmentExpression_In(self, p):
        return PN_AssignmentExpression_ConditionalExpression(self.context, p)
    @_('LeftHandSideExpression EQUALS AssignmentExpression_In')  # pylint: disable=undefined-variable
    def AssignmentExpression_In(self, p):
        return PN_AssignmentExpression_LeftHandSideExpression_EQUALS_AssignmentExpression(self.context, p)
    @_('LeftHandSideExpression AssignmentOperator AssignmentExpression_In')  # pylint: disable=undefined-variable
    def AssignmentExpression_In(self, p):
        return PN_AssignmentExpression_LeftHandSideExpression_AssignmentOperator_AssignmentExpression(self.context, p)
    @_('ConditionalExpression_In_Restricted')  # pylint: disable=undefined-variable
    def AssignmentExpression_In_Restricted(self, p):
        return PN_AssignmentExpression_ConditionalExpression(self.context, p)
    @_('LeftHandSideExpression_Restricted EQUALS AssignmentExpression_In')  # pylint: disable=undefined-variable
    def AssignmentExpression_In_Restricted(self, p):
        return PN_AssignmentExpression_LeftHandSideExpression_EQUALS_AssignmentExpression(self.context, p)
    @_('LeftHandSideExpression_Restricted AssignmentOperator AssignmentExpression_In')  # pylint: disable=undefined-variable
    def AssignmentExpression_In_Restricted(self, p):
        return PN_AssignmentExpression_LeftHandSideExpression_AssignmentOperator_AssignmentExpression(self.context, p)
    @_('STAREQ', 'DIVEQ', 'PERCENTEQ', 'PLUSEQ', 'MINUSEQ', 'LTLE', 'GTGE', 'GTGTGE', 'AMPEQ', 'XOREQ', 'PIPEEQ', 'STARSTAREQ')  # pylint: disable=undefined-variable
    def AssignmentOperator(self, p):
        return PN_AssignmentOperator(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 12.15.5 Destructuring Assignment
    #
    # Supplemental Syntax
    #
    # In certain circumstances when processing an instance of the production
    # AssignmentExpression : LeftHandSideExpression = AssignmentExpression the following grammar is used to refine the
    # interpretation of LeftHandSideExpression.
    #
    # AssignmentPattern[Yield, Await] :
    #       ObjectAssignmentPattern[?Yield, ?Await]
    #       ArrayAssignmentPattern[?Yield, ?Await]
    #
    @_('ObjectAssignmentPattern')  # pylint: disable=undefined-variable
    def AssignmentPattern(self, p):
        return PN_AssignmentPattern_ObjectAssignmentPattern(self.context, p)
    @_('ArrayAssignmentPattern')  # pylint: disable=undefined-variable
    def AssignmentPattern(self, p):
        return PN_AssignmentPattern_ArrayAssignmentPattern(self.context, p)
    #
    # ObjectAssignmentPattern[Yield, Await] :
    #       { }
    #       { AssignmentRestProperty[?Yield, ?Await] }
    #       { AssignmentPropertyList[?Yield, ?Await] }
    #       { AssignmentPropertyList[?Yield, ?Await] , }
    #       { AssignmentPropertyList[?Yield, ?Await] , AssignmentRestProperty[?Yield, ?Await] }
    #
    @_('LCURLY RCURLY')  # pylint: disable=undefined-variable
    def ObjectAssignmentPattern(self, p):
        return PN_ObjectAssignmentPattern_LCURLY_RCURLY(self.context, p)
    @_('LCURLY AssignmentRestProperty RCURLY')  # pylint: disable=undefined-variable
    def ObjectAssignmentPattern(self, p):
        return PN_ObjectAssignmentPattern_LCURLY_AssignmentRestProperty_RCURLY(self.context, p)
    @_('LCURLY AssignmentPropertyList RCURLY')  # pylint: disable=undefined-variable
    def ObjectAssignmentPattern(self, p):
        return PN_ObjectAssignmentPattern_LCURLY_AssignmentPropertyList_RCURLY(self.context, p)
    @_('LCURLY AssignmentPropertyList COMMA RCURLY')  # pylint: disable=undefined-variable
    def ObjectAssignmentPattern(self, p):
        return PN_ObjectAssignmentPattern_LCURLY_AssignmentPropertyList_COMMA_RCURLY(self.context, p)
    @_('LCURLY AssignmentPropertyList COMMA AssignmentRestProperty RCURLY')  # pylint: disable=undefined-variable
    def ObjectAssignmentPattern(self, p):
        return PN_ObjectAssignmentPattern_LCURLY_AssignmentPropertyList_COMMA_AssignmentRestProperty_RCURLY(self.context, p)
    #
    # ArrayAssignmentPattern[Yield, Await] :
    #       [ ]
    #       [ Elision ]
    #       [ AssignmentRestElement[?Yield, ?Await] ]
    #       [ Elision AssignmentRestElement[?Yield, ?Await] ]
    #       [ AssignmentElementList[?Yield, ?Await] ]
    #       [ AssignmentElementList[?Yield, ?Await] , ]
    #       [ AssignmentElementList[?Yield, ?Await] , Elision ]
    #       [ AssignmentElementList[?Yield, ?Await] , AssignmentRestElement[?Yield, ?Await] ]
    #       [ AssignmentElementList[?Yield, ?Await] , Elision AssignmentRestElement[?Yield, ?Await] ]
    #
    @_('LBRACKET RBRACKET')  # pylint: disable=undefined-variable
    def ArrayAssignmentPattern(self, p):
        return PN_ArrayAssignmentPattern_LBRACKET_RBRACKET(self.context, p)
    @_('LBRACKET Elision RBRACKET')  # pylint: disable=undefined-variable
    def ArrayAssignmentPattern(self, p):
        return PN_ArrayAssignmentPattern_LBRACKET_Elision_RBRACKET(self.context, p)
    @_('LBRACKET AssignmentRestElement RBRACKET')  # pylint: disable=undefined-variable
    def ArrayAssignmentPattern(self, p):
        return PN_ArrayAssignmentPattern_LBRACKET_AssignmentRestElement_RBRACKET(self.context, p)
    @_('LBRACKET Elision AssignmentRestElement RBRACKET')  # pylint: disable=undefined-variable
    def ArrayAssignmentPattern(self, p):
        return PN_ArrayAssignmentPattern_LBRACKET_Elision_AssignmentRestElement_RBRACKET(self.context, p)
    @_('LBRACKET AssignmentElementList RBRACKET')  # pylint: disable=undefined-variable
    def ArrayAssignmentPattern(self, p):
        return PN_ArrayAssignmentPattern_LBRACKET_AssignmentElementList_RBRACKET(self.context, p)
    @_('LBRACKET AssignmentElementList COMMA RBRACKET')  # pylint: disable=undefined-variable
    def ArrayAssignmentPattern(self, p):
        return PN_ArrayAssignmentPattern_LBRACKET_AssignmentElementList_COMMA_RBRACKET(self.context, p)
    @_('LBRACKET AssignmentElementList COMMA Elision RBRACKET')  # pylint: disable=undefined-variable
    def ArrayAssignmentPattern(self, p):
        return PN_ArrayAssignmentPattern_LBRACKET_AssignmentElementList_COMMA_Elision_RBRACKET(self.context, p)
    @_('LBRACKET AssignmentElementList COMMA AssignmentRestElement RBRACKET')  # pylint: disable=undefined-variable
    def ArrayAssignmentPattern(self, p):
        return PN_ArrayAssignmentPattern_LBRACKET_AssignmentElementList_COMMA_AssignmentRestElement_RBRACKET(self.context, p)
    @_('LBRACKET AssignmentElementList COMMA Elision AssignmentRestElement RBRACKET')  # pylint: disable=undefined-variable
    def ArrayAssignmentPattern(self, p):
        return PN_ArrayAssignmentPattern_LBRACKET_AssignmentElementList_COMMA_Elision_AssignmentRestElement_RBRACKET(self.context, p)
    #
    # AssignmentRestProperty[Yield, Await] :
    #       ... DestructuringAssignmentTarget[?Yield, ?Await]
    #
    @_('DOTDOTDOT DestructuringAssignmentTarget')  # pylint: disable=undefined-variable
    def AssignmentRestProperty(self, p):
        return PN_AssignmentRestProperty_DOTDOTDOT_DestructuringAssignmentTarget(self.context, p)
    #
    # AssignmentPropertyList[Yield, Await] :
    #       AssignmentProperty[?Yield, ?Await]
    #       AssignmentPropertyList[?Yield, ?Await] , AssignmentProperty[?Yield, ?Await]
    #
    @_('AssignmentProperty')  # pylint: disable=undefined-variable
    def AssignmentPropertyList(self, p):
        return PN_AssignmentPropertyList_AssignmentProperty(self.context, p)
    @_('AssignmentPropertyList COMMA AssignmentProperty')  # pylint: disable=undefined-variable
    def AssignmentPropertyList(self, p):
        return PN_AssignmentPropertyList_AssignmentPropertyList_COMMA_AssignmentProperty(self.context, p)
    #
    # AssignmentElementList[Yield, Await] :
    #       AssignmentElisionElement[?Yield, ?Await]
    #       AssignmentElementList[?Yield, ?Await] , AssignmentElisionElement[?Yield, ?Await]
    #
    @_('AssignmentElisionElement')  # pylint: disable=undefined-variable
    def AssignmentElementList(self, p):
        return PN_AssignmentElementList_AssignmentElisionElement(self.context, p)
    @_('AssignmentElementList COMMA AssignmentElisionElement')  # pylint: disable=undefined-variable
    def AssignmentElementList(self, p):
        return PN_AssignmentElementList_AssignmentElementList_COMMA_AssignmentElisionElement(self.context, p)
    #
    # AssignmentElisionElement[Yield, Await] :
    #       AssignmentElement[?Yield, ?Await]
    #       Elision AssignmentElement[?Yield, ?Await]
    #
    @_('AssignmentElement')  # pylint: disable=undefined-variable
    def AssignmentElisionElement(self, p):
        return PN_AssignmentElisionElement_AssignmentElement(self.context, p)
    @_('Elision AssignmentElement')  # pylint: disable=undefined-variable
    def AssignmentElisionElement(self, p):
        return PN_AssignmentElisionElement_Elision_AssignmentElement(self.context, p)
    #
    # AssignmentProperty[Yield, Await] :
    #       IdentifierReference[?Yield, ?Await]
    #       IdentifierReference[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]
    #       PropertyName[?Yield, ?Await] : AssignmentElement[?Yield, ?Await]
    #
    @_('IdentifierReference')  # pylint: disable=undefined-variable
    def AssignmentProperty(self, p):
        return PN_AssignmentProperty_IdentifierReference(self.context, p)
    @_('IdentifierReference Initializer_In')  # pylint: disable=undefined-variable
    def AssignmentProperty(self, p):
        return PN_AssignmentProperty_IdentifierReference_Initializer(self.context, p)
    @_('PropertyName COLON AssignmentElement')  # pylint: disable=undefined-variable
    def AssignmentProperty(self, p):
        return PN_AssignmentProperty_PropertyName_COLON_AssignmentElement(self.context, p)
    #
    # AssignmentElement[Yield, Await] :
    #       DestructuringAssignmentTarget[?Yield, ?Await]
    #       DestructuringAssignmentTarget[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]
    #
    @_('DestructuringAssignmentTarget')  # pylint: disable=undefined-variable
    def AssignmentElement(self, p):
        return PN_AssignmentElement_DestructuringAssignmentTarget(self.context, p)
    @_('DestructuringAssignmentTarget Initializer_In')  # pylint: disable=undefined-variable
    def AssignmentElement(self, p):
        return PN_AssignmentElement_DestructuringAssignmentTarget_Initializer(self.context, p)
    #
    # AssignmentRestElement[Yield, Await] :
    #       ... DestructuringAssignmentTarget[?Yield, ?Await]
    #
    @_('DOTDOTDOT DestructuringAssignmentTarget')  # pylint: disable=undefined-variable
    def AssignmentRestElement(self, p):
        return PN_AssignmentRestElement_DOTDOTDOT_DestructuringAssignmentTarget(self.context, p)
    #
    # DestructuringAssignmentTarget[Yield, Await] :
    #       LeftHandSideExpression[?Yield, ?Await]
    #
    @_('LeftHandSideExpression')  # pylint: disable=undefined-variable
    def DestructuringAssignmentTarget(self, p):
        return PN_DestructuringAssignmentTarget_LeftHandSideExpression(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    @_('LogicalORExpression')  # pylint: disable=undefined-variable
    def ConditionalExpression(self, p):
        return PN_ConditionalExpression_LogicalORExpression(self.context, p)
    @_('LogicalORExpression QUESTION AssignmentExpression_In COLON AssignmentExpression')  # pylint: disable=undefined-variable
    def ConditionalExpression(self, p):
        return PN_ConditionalExpression_QUESTION_AssignmentExpression_COLON_AssignmentExpression(self.context, p)
    @_('LogicalORExpression_In')  # pylint: disable=undefined-variable
    def ConditionalExpression_In(self, p):
        return PN_ConditionalExpression_LogicalORExpression(self.context, p)
    @_('LogicalORExpression_In QUESTION AssignmentExpression_In COLON AssignmentExpression_In')  # pylint: disable=undefined-variable
    def ConditionalExpression_In(self, p):
        return PN_ConditionalExpression_QUESTION_AssignmentExpression_COLON_AssignmentExpression(self.context, p)
    @_('LogicalORExpression_In_Restricted')  # pylint: disable=undefined-variable
    def ConditionalExpression_In_Restricted(self, p):
        return PN_ConditionalExpression_LogicalORExpression(self.context, p)
    @_('LogicalORExpression_In_Restricted QUESTION AssignmentExpression_In COLON AssignmentExpression_In')  # pylint: disable=undefined-variable
    def ConditionalExpression_In_Restricted(self, p):
        return PN_ConditionalExpression_QUESTION_AssignmentExpression_COLON_AssignmentExpression(self.context, p)

    ########################################################################################################################
    # 12.13 Binary Logical Operators
    # Syntax
    # LogicalANDExpression[In, Yield, Await]:
    #                 BitwiseORExpression[?In, ?Yield, ?Await]
    #                 LogicalANDExpression[?In, ?Yield, ?Await] && BitwiseORExpression[?In, ?Yield, ?Await]
    @_('BitwiseORExpression')  # pylint: disable=undefined-variable
    def LogicalANDExpression(self, p):
        return PN_LogicalANDExpression_BitwiseORExpression(self.context, p)
    @_('LogicalANDExpression AMPAMP BitwiseORExpression')  # pylint: disable=undefined-variable
    def LogicalANDExpression(self, p):
        return PN_LogicalANDExpression_LogicalANDExpression_AMPAMP_BitwiseORExpression(self.context, p)
    @_('BitwiseORExpression_In')  # pylint: disable=undefined-variable
    def LogicalANDExpression_In(self, p):
        return PN_LogicalANDExpression_BitwiseORExpression(self.context, p)
    @_('LogicalANDExpression_In AMPAMP BitwiseORExpression_In')  # pylint: disable=undefined-variable
    def LogicalANDExpression_In(self, p):
        return PN_LogicalANDExpression_LogicalANDExpression_AMPAMP_BitwiseORExpression(self.context, p)
    @_('BitwiseORExpression_In_Restricted')  # pylint: disable=undefined-variable
    def LogicalANDExpression_In_Restricted(self, p):
        return PN_LogicalANDExpression_BitwiseORExpression(self.context, p)
    @_('LogicalANDExpression_In_Restricted AMPAMP BitwiseORExpression_In')  # pylint: disable=undefined-variable
    def LogicalANDExpression_In_Restricted(self, p):
        return PN_LogicalANDExpression_LogicalANDExpression_AMPAMP_BitwiseORExpression(self.context, p)
    #
    # LogicalORExpression[In, Yield, Await]:
    #                 LogicalANDExpression[?In, ?Yield, ?Await]
    #                 LogicalORExpression[?In, ?Yield, ?Await] || LogicalANDExpression[?In, ?Yield, ?Await]
    @_('LogicalANDExpression')  # pylint: disable=undefined-variable
    def LogicalORExpression(self, p):
        return PN_LogicalORExpression_LogicalANDExpression(self.context, p)
    @_('LogicalORExpression PIPEPIPE LogicalANDExpression')  # pylint: disable=undefined-variable
    def LogicalORExpression(self, p):
        return PN_LogicalORExpression_LogicalORExpression_PIPEPIPE_LogicalANDExpression(self.context, p)
    @_('LogicalANDExpression_In')  # pylint: disable=undefined-variable
    def LogicalORExpression_In(self, p):
        return PN_LogicalORExpression_LogicalANDExpression(self.context, p)
    @_('LogicalORExpression_In PIPEPIPE LogicalANDExpression_In')  # pylint: disable=undefined-variable
    def LogicalORExpression_In(self, p):
        return PN_LogicalORExpression_LogicalORExpression_PIPEPIPE_LogicalANDExpression(self.context, p)
    @_('LogicalANDExpression_In_Restricted')  # pylint: disable=undefined-variable
    def LogicalORExpression_In_Restricted(self, p):
        return PN_LogicalORExpression_LogicalANDExpression(self.context, p)
    @_('LogicalORExpression_In_Restricted PIPEPIPE LogicalANDExpression_In')  # pylint: disable=undefined-variable
    def LogicalORExpression_In_Restricted(self, p):
        return PN_LogicalORExpression_LogicalORExpression_PIPEPIPE_LogicalANDExpression(self.context, p)
    # NOTE
    # The value produced by a && or || operator is not necessarily of type Boolean. The value produced will always be the value
    # of one of the two operand expressions.
    ########################################################################################################################

    ########################################################################################################################
    # 12.12 Binary Bitwise Operators
    # Syntax
    # BitwiseANDExpression[In, Yield, Await]:
    #                 EqualityExpression[?In, ?Yield, ?Await]
    #                 BitwiseANDExpression[?In, ?Yield, ?Await] & EqualityExpression[?In, ?Yield, ?Await]
    @_('EqualityExpression')  # pylint: disable=undefined-variable
    def BitwiseANDExpression(self, p):
        return PN_BitwiseANDExpression_EqualityExpression(self.context, p)
    @_('BitwiseANDExpression AMP EqualityExpression')  # pylint: disable=undefined-variable
    def BitwiseANDExpression(self, p):
        return PN_BitwiseANDExpression_BitwiseANDExpression_AMP_EqualityExpression(self.context, p)
    @_('EqualityExpression_In')  # pylint: disable=undefined-variable
    def BitwiseANDExpression_In(self, p):
        return PN_BitwiseANDExpression_EqualityExpression(self.context, p)
    @_('BitwiseANDExpression_In AMP EqualityExpression_In')  # pylint: disable=undefined-variable
    def BitwiseANDExpression_In(self, p):
        return PN_BitwiseANDExpression_BitwiseANDExpression_AMP_EqualityExpression(self.context, p)
    @_('EqualityExpression_In_Restricted')  # pylint: disable=undefined-variable
    def BitwiseANDExpression_In_Restricted(self, p):
        return PN_BitwiseANDExpression_EqualityExpression(self.context, p)
    @_('BitwiseANDExpression_In_Restricted AMP EqualityExpression_In')  # pylint: disable=undefined-variable
    def BitwiseANDExpression_In_Restricted(self, p):
        return PN_BitwiseANDExpression_BitwiseANDExpression_AMP_EqualityExpression(self.context, p)
    #
    # BitwiseXORExpression[In, Yield, Await]:
    #                 BitwiseANDExpression[?In, ?Yield, ?Await]
    #                 BitwiseXORExpression[?In, ?Yield, ?Await] ^ BitwiseANDExpression[?In, ?Yield, ?Await]
    @_('BitwiseANDExpression')  # pylint: disable=undefined-variable
    def BitwiseXORExpression(self, p):
        return PN_BitwiseXORExpression_BitwiseANDExpression(self.context, p)
    @_('BitwiseXORExpression XOR BitwiseANDExpression')  # pylint: disable=undefined-variable
    def BitwiseXORExpression(self, p):
        return PN_BitwiseXORExpression_BitwiseXORExpression_XOR_BitwiseANDExpression(self.context, p)
    @_('BitwiseANDExpression_In')  # pylint: disable=undefined-variable
    def BitwiseXORExpression_In(self, p):
        return PN_BitwiseXORExpression_BitwiseANDExpression(self.context, p)
    @_('BitwiseXORExpression_In XOR BitwiseANDExpression_In')  # pylint: disable=undefined-variable
    def BitwiseXORExpression_In(self, p):
        return PN_BitwiseXORExpression_BitwiseXORExpression_XOR_BitwiseANDExpression(self.context, p)
    @_('BitwiseANDExpression_In_Restricted')  # pylint: disable=undefined-variable
    def BitwiseXORExpression_In_Restricted(self, p):
        return PN_BitwiseXORExpression_BitwiseANDExpression(self.context, p)
    @_('BitwiseXORExpression_In_Restricted XOR BitwiseANDExpression_In')  # pylint: disable=undefined-variable
    def BitwiseXORExpression_In_Restricted(self, p):
        return PN_BitwiseXORExpression_BitwiseXORExpression_XOR_BitwiseANDExpression(self.context, p)
    #
    # BitwiseORExpression[In, Yield, Await]:
    #                 BitwiseXORExpression[?In, ?Yield, ?Await]
    #                 BitwiseORExpression[?In, ?Yield, ?Await] | BitwiseXORExpression[?In, ?Yield, ?Await]
    @_('BitwiseXORExpression')  # pylint: disable=undefined-variable
    def BitwiseORExpression(self, p):
        return PN_BitwiseORExpression_BitwiseXORExpression(self.context, p)
    @_('BitwiseORExpression PIPE BitwiseXORExpression')  # pylint: disable=undefined-variable
    def BitwiseORExpression(self, p):
        return PN_BitwiseORExpression_BitwiseORExpression_PIPE_BitwiseXORExpression(self.context, p)
    @_('BitwiseXORExpression_In')  # pylint: disable=undefined-variable
    def BitwiseORExpression_In(self, p):
        return PN_BitwiseORExpression_BitwiseXORExpression(self.context, p)
    @_('BitwiseORExpression_In PIPE BitwiseXORExpression_In')  # pylint: disable=undefined-variable
    def BitwiseORExpression_In(self, p):
        return PN_BitwiseORExpression_BitwiseORExpression_PIPE_BitwiseXORExpression(self.context, p)
    @_('BitwiseXORExpression_In_Restricted')  # pylint: disable=undefined-variable
    def BitwiseORExpression_In_Restricted(self, p):
        return PN_BitwiseORExpression_BitwiseXORExpression(self.context, p)
    @_('BitwiseORExpression_In_Restricted PIPE BitwiseXORExpression_In')  # pylint: disable=undefined-variable
    def BitwiseORExpression_In_Restricted(self, p):
        return PN_BitwiseORExpression_BitwiseORExpression_PIPE_BitwiseXORExpression(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 12.11 Equality Operators
    # NOTE
    # The result of evaluating an equality operator is always of type Boolean, reflecting whether the relationship named by the
    # operator holds between its two operands.
    #
    # Syntax
    # EqualityExpression[In, Yield, Await]:
    #                 RelationalExpression[?In, ?Yield, ?Await]
    #                 EqualityExpression[?In, ?Yield, ?Await] == RelationalExpression[?In, ?Yield, ?Await]
    #                 EqualityExpression[?In, ?Yield, ?Await] != RelationalExpression[?In, ?Yield, ?Await]
    #                 EqualityExpression[?In, ?Yield, ?Await] === RelationalExpression[?In, ?Yield, ?Await]
    #                 EqualityExpression[?In, ?Yield, ?Await] !== RelationalExpression[?In, ?Yield, ?Await]
    @_('RelationalExpression')  # pylint: disable=undefined-variable
    def EqualityExpression(self, p):
        return PN_EqualityExpression_RelationalExpression(self.context, p)
    @_('EqualityExpression EQEQ RelationalExpression')  # pylint: disable=undefined-variable
    def EqualityExpression(self, p):
        return PN_EqualityExpression_EqualityExpression_EQEQ_RelationalExpression(self.context, p)
    @_('EqualityExpression BANGEQ RelationalExpression')  # pylint: disable=undefined-variable
    def EqualityExpression(self, p):
        return PN_EqualityExpression_EqualityExpression_BANGEQ_RelationalExpression(self.context, p)
    @_('EqualityExpression EQEQEQ RelationalExpression')  # pylint: disable=undefined-variable
    def EqualityExpression(self, p):
        return PN_EqualityExpression_EqualityExpression_EQEQEQ_RelationalExpression(self.context, p)
    @_('EqualityExpression BANGEQEQ RelationalExpression')  # pylint: disable=undefined-variable
    def EqualityExpression(self, p):
        return PN_EqualityExpression_EqualityExpression_BANGEQEQ_RelationalExpression(self.context, p)
    @_('RelationalExpression_In')  # pylint: disable=undefined-variable
    def EqualityExpression_In(self, p):
        return PN_EqualityExpression_RelationalExpression(self.context, p)
    @_('EqualityExpression_In EQEQ RelationalExpression_In')  # pylint: disable=undefined-variable
    def EqualityExpression_In(self, p):
        return PN_EqualityExpression_EqualityExpression_EQEQ_RelationalExpression(self.context, p)
    @_('EqualityExpression_In BANGEQ RelationalExpression_In')  # pylint: disable=undefined-variable
    def EqualityExpression_In(self, p):
        return PN_EqualityExpression_EqualityExpression_BANGEQ_RelationalExpression(self.context, p)
    @_('EqualityExpression_In EQEQEQ RelationalExpression_In')  # pylint: disable=undefined-variable
    def EqualityExpression_In(self, p):
        return PN_EqualityExpression_EqualityExpression_EQEQEQ_RelationalExpression(self.context, p)
    @_('EqualityExpression_In BANGEQEQ RelationalExpression_In')  # pylint: disable=undefined-variable
    def EqualityExpression_In(self, p):
        return PN_EqualityExpression_EqualityExpression_BANGEQEQ_RelationalExpression(self.context, p)
    @_('RelationalExpression_In_Restricted')  # pylint: disable=undefined-variable
    def EqualityExpression_In_Restricted(self, p):
        return PN_EqualityExpression_RelationalExpression(self.context, p)
    @_('EqualityExpression_In_Restricted EQEQ RelationalExpression_In')  # pylint: disable=undefined-variable
    def EqualityExpression_In_Restricted(self, p):
        return PN_EqualityExpression_EqualityExpression_EQEQ_RelationalExpression(self.context, p)
    @_('EqualityExpression_In_Restricted BANGEQ RelationalExpression_In')  # pylint: disable=undefined-variable
    def EqualityExpression_In_Restricted(self, p):
        return PN_EqualityExpression_EqualityExpression_BANGEQ_RelationalExpression(self.context, p)
    @_('EqualityExpression_In_Restricted EQEQEQ RelationalExpression_In')  # pylint: disable=undefined-variable
    def EqualityExpression_In_Restricted(self, p):
        return PN_EqualityExpression_EqualityExpression_EQEQEQ_RelationalExpression(self.context, p)
    @_('EqualityExpression_In_Restricted BANGEQEQ RelationalExpression_In')  # pylint: disable=undefined-variable
    def EqualityExpression_In_Restricted(self, p):
        return PN_EqualityExpression_EqualityExpression_BANGEQEQ_RelationalExpression(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 12.10 Relational Operators
    # NOTE 1
    # The result of evaluating a relational operator is always of type Boolean, reflecting whether the relationship named by
    # the operator holds between its two operands.
    #
    # Syntax
    #
    # RelationalExpression[In, Yield, Await]:
    #             ShiftExpression[?Yield, ?Await]
    #             RelationalExpression[?In, ?Yield, ?Await] < ShiftExpression[?Yield, ?Await]
    #             RelationalExpression[?In, ?Yield, ?Await] > ShiftExpression[?Yield, ?Await]
    #             RelationalExpression[?In, ?Yield, ?Await] <= ShiftExpression[?Yield, ?Await]
    #             RelationalExpression[?In, ?Yield, ?Await] >= ShiftExpression[?Yield, ?Await]
    #             RelationalExpression[?In, ?Yield, ?Await] instanceof ShiftExpression[?Yield, ?Await]
    #             [+In]RelationalExpression[+In, ?Yield, ?Await] in ShiftExpression[?Yield, ?Await]
    #
    # NOTE 2
    # The [In] grammar parameter is needed to avoid confusing the in operator in a relational expression with the in operator
    # in a for statement.
    #
    @_('ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression(self, p):
        return PN_RelationalExpression_ShiftExpression(self.context, p)
    @_('RelationalExpression LT ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression(self, p):
        return PN_RelationalExpression_RelationalExpression_LT_ShiftExpression(self.context, p)
    @_('RelationalExpression GT ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression(self, p):
        return PN_RelationalExpression_RelationalExpression_GT_ShiftExpression(self.context, p)
    @_('RelationalExpression LE ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression(self, p):
        return PN_RelationalExpression_RelationalExpression_LE_ShiftExpression(self.context, p)
    @_('RelationalExpression GE ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression(self, p):
        return PN_RelationalExpression_RelationalExpression_GE_ShiftExpression(self.context, p)
    @_('RelationalExpression INSTANCEOF ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression(self, p):
        return PN_RelationalExpression_RelationalExpression_INSTANCEOF_ShiftExpression(self.context, p)
    @_('ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression_In(self, p):
        return PN_RelationalExpression_ShiftExpression(self.context, p)
    @_('RelationalExpression_In LT ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression_In(self, p):
        return PN_RelationalExpression_RelationalExpression_LT_ShiftExpression(self.context, p)
    @_('RelationalExpression_In GT ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression_In(self, p):
        return PN_RelationalExpression_RelationalExpression_GT_ShiftExpression(self.context, p)
    @_('RelationalExpression_In LE ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression_In(self, p):
        return PN_RelationalExpression_RelationalExpression_LE_ShiftExpression(self.context, p)
    @_('RelationalExpression_In GE ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression_In(self, p):
        return PN_RelationalExpression_RelationalExpression_GE_ShiftExpression(self.context, p)
    @_('RelationalExpression_In INSTANCEOF ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression_In(self, p):
        return PN_RelationalExpression_RelationalExpression_INSTANCEOF_ShiftExpression(self.context, p)
    @_('RelationalExpression_In IN ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression_In(self, p):
        return PN_RelationalExpression_RelationalExpression_IN_ShiftExpression(self.context, p)
    @_('ShiftExpression_Restricted')  # pylint: disable=undefined-variable
    def RelationalExpression_In_Restricted(self, p):
        return PN_RelationalExpression_ShiftExpression(self.context, p)
    @_('RelationalExpression_In_Restricted LT ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression_In_Restricted(self, p):
        return PN_RelationalExpression_RelationalExpression_LT_ShiftExpression(self.context, p)
    @_('RelationalExpression_In_Restricted GT ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression_In_Restricted(self, p):
        return PN_RelationalExpression_RelationalExpression_GT_ShiftExpression(self.context, p)
    @_('RelationalExpression_In_Restricted LE ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression_In_Restricted(self, p):
        return PN_RelationalExpression_RelationalExpression_LE_ShiftExpression(self.context, p)
    @_('RelationalExpression_In_Restricted GE ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression_In_Restricted(self, p):
        return PN_RelationalExpression_RelationalExpression_GE_ShiftExpression(self.context, p)
    @_('RelationalExpression_In_Restricted INSTANCEOF ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression_In_Restricted(self, p):
        return PN_RelationalExpression_RelationalExpression_INSTANCEOF_ShiftExpression(self.context, p)
    @_('RelationalExpression_In_Restricted IN ShiftExpression')  # pylint: disable=undefined-variable
    def RelationalExpression_In_Restricted(self, p):
        return PN_RelationalExpression_RelationalExpression_IN_ShiftExpression(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 12.9 Bitwise Shift Operators
    #
    # Syntax
    #
    # ShiftExpression[Yield, Await]:
    #              AdditiveExpression[?Yield, ?Await]
    #              ShiftExpression[?Yield, ?Await] << AdditiveExpression[?Yield, ?Await]
    #              ShiftExpression[?Yield, ?Await] >> AdditiveExpression[?Yield, ?Await]
    #              ShiftExpression[?Yield, ?Await] >>> AdditiveExpression[?Yield, ?Await]
    #
    @_('AdditiveExpression')  # pylint: disable=undefined-variable
    def ShiftExpression(self, p):
        return PN_ShiftExpression_AdditiveExpression(self.context, p)
    @_('ShiftExpression LTLT AdditiveExpression')  # pylint: disable=undefined-variable
    def ShiftExpression(self, p):
        return PN_ShiftExpression_LTLT_AdditiveExpression(self.context, p)
    @_('ShiftExpression GTGT AdditiveExpression')  # pylint: disable=undefined-variable
    def ShiftExpression(self, p):
        return PN_ShiftExpression_GTGT_AdditiveExpression(self.context, p)
    @_('ShiftExpression GTGTGT AdditiveExpression')  # pylint: disable=undefined-variable
    def ShiftExpression(self, p):
        return PN_ShiftExpression_GTGTGT_AdditiveExpression(self.context, p)
    @_('AdditiveExpression_Restricted')  # pylint: disable=undefined-variable
    def ShiftExpression_Restricted(self, p):
        return PN_ShiftExpression_AdditiveExpression(self.context, p)
    @_('ShiftExpression_Restricted LTLT AdditiveExpression')  # pylint: disable=undefined-variable
    def ShiftExpression_Restricted(self, p):
        return PN_ShiftExpression_LTLT_AdditiveExpression(self.context, p)
    @_('ShiftExpression_Restricted GTGT AdditiveExpression')  # pylint: disable=undefined-variable
    def ShiftExpression_Restricted(self, p):
        return PN_ShiftExpression_GTGT_AdditiveExpression(self.context, p)
    @_('ShiftExpression_Restricted GTGTGT AdditiveExpression')  # pylint: disable=undefined-variable
    def ShiftExpression_Restricted(self, p):
        return PN_ShiftExpression_GTGTGT_AdditiveExpression(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 12.8 Additive Operators
    #
    # Syntax
    #
    # AdditiveExpression[Yield, Await]:
    #               MultiplicativeExpression[?Yield, ?Await]
    #               AdditiveExpression[?Yield, ?Await] + MultiplicativeExpression[?Yield, ?Await]
    #               AdditiveExpression[?Yield, ?Await] - MultiplicativeExpression[?Yield, ?Await]
    #
    @_('MultiplicativeExpression')  # pylint: disable=undefined-variable
    def AdditiveExpression(self, p):
        return PN_AdditiveExpression_MultiplicativeExpression(self.context, p)
    @_('AdditiveExpression PLUS MultiplicativeExpression')  # pylint: disable=undefined-variable
    def AdditiveExpression(self, p):
        return PN_AdditiveExpression_AdditiveExpression_PLUS_MultiplicativeExpression(self.context, p)
    @_('AdditiveExpression MINUS MultiplicativeExpression')  # pylint: disable=undefined-variable
    def AdditiveExpression(self, p):
        return PN_AdditiveExpression_AdditiveExpression_MINUS_MultiplicativeExpression(self.context, p)
    @_('MultiplicativeExpression_Restricted')  # pylint: disable=undefined-variable
    def AdditiveExpression_Restricted(self, p):
        return PN_AdditiveExpression_MultiplicativeExpression(self.context, p)
    @_('AdditiveExpression_Restricted PLUS MultiplicativeExpression')  # pylint: disable=undefined-variable
    def AdditiveExpression_Restricted(self, p):
        return PN_AdditiveExpression_AdditiveExpression_PLUS_MultiplicativeExpression(self.context, p)
    @_('AdditiveExpression_Restricted MINUS MultiplicativeExpression')  # pylint: disable=undefined-variable
    def AdditiveExpression_Restricted(self, p):
        return PN_AdditiveExpression_AdditiveExpression_MINUS_MultiplicativeExpression(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 12.7 Multiplicative Operators
    #
    # Syntax
    #
    # MultiplicativeExpression[Yield, Await] :
    #               ExponentiationExpression[?Yield, ?Await]
    #               MultiplicativeExpression[?Yield, ?Await] MultiplicativeOperator ExponentiationExpression[?Yield, ?Await]
    #
    # MultiplicativeOperator : one of
    #               * / %
    #
    @_('ExponentiationExpression')  # pylint: disable=undefined-variable
    def MultiplicativeExpression(self, p):
        return PN_MultiplicativeExpression_ExponentiationExpression(self.context, p)
    @_('MultiplicativeExpression MultiplicativeOperator ExponentiationExpression')  # pylint: disable=undefined-variable
    def MultiplicativeExpression(self, p):
        return PN_MultiplicativeExpression_MultiplicativeOperator_ExponentiationExpression(self.context, p)
    @_('ExponentiationExpression_Restricted')  # pylint: disable=undefined-variable
    def MultiplicativeExpression_Restricted(self, p):
        return PN_MultiplicativeExpression_ExponentiationExpression(self.context, p)
    @_('MultiplicativeExpression_Restricted MultiplicativeOperator ExponentiationExpression')  # pylint: disable=undefined-variable
    def MultiplicativeExpression_Restricted(self, p):
        return PN_MultiplicativeExpression_MultiplicativeOperator_ExponentiationExpression(self.context, p)
    @_('STAR', 'DIV', 'PERCENT')  # pylint: disable=undefined-variable
    def MultiplicativeOperator(self, p):
        return PN_MultiplicativeOperator(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 12.6 Exponentiation Operator
    #
    # Syntax
    #
    # ExponentiationExpression[Yield, Await] :
    #           UnaryExpression[?Yield, ?Await]
    #           UpdateExpression[?Yield, ?Await] ** ExponentiationExpression[?Yield, ?Await]
    #
    @_('UnaryExpression')  # pylint: disable=undefined-variable
    def ExponentiationExpression(self, p):
        return PN_ExponentiationExpression_UnaryExpression(self.context, p)
    @_('UpdateExpression STARSTAR ExponentiationExpression')  # pylint: disable=undefined-variable
    def ExponentiationExpression(self, p):
        return PN_ExponentiationExpression_UpdateExpression_STARSTAR_ExponentiationExpression(self.context, p)
    @_('UnaryExpression_Restricted')  # pylint: disable=undefined-variable
    def ExponentiationExpression_Restricted(self, p):
        return PN_ExponentiationExpression_UnaryExpression(self.context, p)
    @_('UpdateExpression_Restricted STARSTAR ExponentiationExpression')  # pylint: disable=undefined-variable
    def ExponentiationExpression_Restricted(self, p):
        return PN_ExponentiationExpression_UpdateExpression_STARSTAR_ExponentiationExpression(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 12.5 Unary Operators
    #
    # Syntax
    #
    # UnaryExpression[Yield, Await] :
    #           UpdateExpression[?Yield, ?Await]
    #           delete UnaryExpression[?Yield, ?Await]
    #           void UnaryExpression[?Yield, ?Await]
    #           typeof UnaryExpression[?Yield, ?Await]
    #           + UnaryExpression[?Yield, ?Await]
    #           - UnaryExpression[?Yield, ?Await]
    #           ~ UnaryExpression[?Yield, ?Await]
    #           ! UnaryExpression[?Yield, ?Await]
    #           [+Await]AwaitExpression[?Yield]
    @_('UpdateExpression')  # pylint: disable=undefined-variable
    def UnaryExpression(self, p):
        return PN_UnaryExpression_UpdateExpression(self.context, p)
    @_('DELETE UnaryExpression')  # pylint: disable=undefined-variable
    def UnaryExpression(self, p):
        return PN_UnaryExpression_DELETE_UnaryExpression(self.context, p)
    @_('TYPEOF UnaryExpression')  # pylint: disable=undefined-variable
    def UnaryExpression(self, p):
        return PN_UnaryExpression_TYPEOF_UnaryExpression(self.context, p)
    @_('PLUS UnaryExpression')  # pylint: disable=undefined-variable
    def UnaryExpression(self, p):
        return PN_UnaryExpression_PLUS_UnaryExpression(self.context, p)
    @_('MINUS UnaryExpression')  # pylint: disable=undefined-variable
    def UnaryExpression(self, p):
        return PN_UnaryExpression_MINUS_UnaryExpression(self.context, p)
    @_('TILDE UnaryExpression')  # pylint: disable=undefined-variable
    def UnaryExpression(self, p):
        return PN_UnaryExpression_TILDE_UnaryExpression(self.context, p)
    @_('BANG UnaryExpression')  # pylint: disable=undefined-variable
    def UnaryExpression(self, p):
        return PN_UnaryExpression_BANG_UnaryExpression(self.context, p)
    @_('UpdateExpression_Restricted')  # pylint: disable=undefined-variable
    def UnaryExpression_Restricted(self, p):
        return PN_UnaryExpression_UpdateExpression(self.context, p)
    @_('DELETE UnaryExpression')  # pylint: disable=undefined-variable
    def UnaryExpression_Restricted(self, p):
        return PN_UnaryExpression_DELETE_UnaryExpression(self.context, p)
    @_('TYPEOF UnaryExpression')  # pylint: disable=undefined-variable
    def UnaryExpression_Restricted(self, p):
        return PN_UnaryExpression_TYPEOF_UnaryExpression(self.context, p)
    @_('PLUS UnaryExpression')  # pylint: disable=undefined-variable
    def UnaryExpression_Restricted(self, p):
        return PN_UnaryExpression_PLUS_UnaryExpression(self.context, p)
    @_('MINUS UnaryExpression')  # pylint: disable=undefined-variable
    def UnaryExpression_Restricted(self, p):
        return PN_UnaryExpression_MINUS_UnaryExpression(self.context, p)
    @_('TILDE UnaryExpression')  # pylint: disable=undefined-variable
    def UnaryExpression_Restricted(self, p):
        return PN_UnaryExpression_TILDE_UnaryExpression(self.context, p)
    @_('BANG UnaryExpression')  # pylint: disable=undefined-variable
    def UnaryExpression_Restricted(self, p):
        return PN_UnaryExpression_BANG_UnaryExpression(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 12.4 Update Expressions
    #
    # Syntax
    #
    # UpdateExpression[Yield, Await] :
    #           LeftHandSideExpression[?Yield, ?Await]
    #           LeftHandSideExpression[?Yield, ?Await] [no LineTerminator here] ++
    #           LeftHandSideExpression[?Yield, ?Await] [no LineTerminator here] --
    #           ++ UnaryExpression[?Yield, ?Await]
    #           -- UnaryExpression[?Yield, ?Await]
    #
    @_('LeftHandSideExpression')  # pylint: disable=undefined-variable
    def UpdateExpression(self, p):
        return PN_UpdateExpression_LeftHandSideExpression(self.context, p)
    @_('LeftHandSideExpression PLUSPLUS')  # pylint: disable=undefined-variable
    def UpdateExpression(self, p):
        return PN_UpdateExpression_LeftHandSideExpression_PLUSPLUS(self.context, p)
    @_('LeftHandSideExpression MINUSMINUS')  # pylint: disable=undefined-variable
    def UpdateExpression(self, p):
        return PN_UpdateExpression_LeftHandSideExpression_MINUSMINUS(self.context, p)
    @_('PLUSPLUS UnaryExpression')  # pylint: disable=undefined-variable
    def UpdateExpression(self, p):
        return PN_UpdateExpression_PLUSPLUS_UnaryExpression(self.context, p)
    @_('MINUSMINUS UnaryExpression')  # pylint: disable=undefined-variable
    def UpdateExpression(self, p):
        return PN_UpdateExpression_MINUSMINUS_UnaryExpression(self.context, p)
    @_('LeftHandSideExpression_Restricted')  # pylint: disable=undefined-variable
    def UpdateExpression_Restricted(self, p):
        return PN_UpdateExpression_LeftHandSideExpression(self.context, p)
    @_('LeftHandSideExpression_Restricted PLUSPLUS')  # pylint: disable=undefined-variable
    def UpdateExpression_Restricted(self, p):
        return PN_UpdateExpression_LeftHandSideExpression_PLUSPLUS(self.context, p)
    @_('LeftHandSideExpression_Restricted MINUSMINUS')  # pylint: disable=undefined-variable
    def UpdateExpression_Restricted(self, p):
        return PN_UpdateExpression_LeftHandSideExpression_MINUSMINUS(self.context, p)
    @_('PLUSPLUS UnaryExpression')  # pylint: disable=undefined-variable
    def UpdateExpression_Restricted(self, p):
        return PN_UpdateExpression_PLUSPLUS_UnaryExpression(self.context, p)
    @_('MINUSMINUS UnaryExpression')  # pylint: disable=undefined-variable
    def UpdateExpression_Restricted(self, p):
        return PN_UpdateExpression_MINUSMINUS_UnaryExpression(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 12.3 Left-Hand-Side Expressions
    #
    # Syntax
    #
    # MemberExpression[Yield, Await] :
    #           PrimaryExpression[?Yield, ?Await]
    #           MemberExpression[?Yield, ?Await] [ Expression[+In, ?Yield, ?Await] ]
    #           MemberExpression[?Yield, ?Await] . IdentifierName
    #           MemberExpression[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
    #           SuperProperty[?Yield, ?Await]
    #           MetaProperty
    #           new MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
    #
    # SuperProperty[Yield, Await] :
    #           super [ Expression[+In, ?Yield, ?Await] ]
    #           super . IdentifierName
    #
    # MetaProperty :
    #           NewTarget
    #
    # NewTarget :
    #           new . target
    #
    # NewExpression[Yield, Await] :
    #           MemberExpression[?Yield, ?Await]
    #           new NewExpression[?Yield, ?Await]
    #
    # CallExpression[Yield, Await] :
    #           CoverCallExpressionAndAsyncArrowHead[?Yield, ?Await]
    #           SuperCall[?Yield, ?Await]
    #           CallExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
    #           CallExpression[?Yield, ?Await] [ Expression[+In, ?Yield, ?Await] ]
    #           CallExpression[?Yield, ?Await] . IdentifierName
    #           CallExpression[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
    #
    # SuperCall[Yield, Await] :
    #           super Arguments[?Yield, ?Await]
    #
    # Arguments[Yield, Await] :
    #           ( )
    #           ( ArgumentList[?Yield, ?Await] )
    #           ( ArgumentList[?Yield, ?Await] , )
    #
    # ArgumentList[Yield, Await] :
    #           AssignmentExpression[+In, ?Yield, ?Await]
    #           ... AssignmentExpression[+In, ?Yield, ?Await]
    #           ArgumentList[?Yield, ?Await] , AssignmentExpression[+In, ?Yield, ?Await]
    #           ArgumentList[?Yield, ?Await] , ...AssignmentExpression[+In, ?Yield, ?Await]
    #
    # LeftHandSideExpression[Yield, Await] :
    #           NewExpression[?Yield, ?Await]
    #           CallExpression[?Yield, ?Await]
    #
    @_('NewExpression')  # pylint: disable=undefined-variable
    def LeftHandSideExpression(self, p):
        return PN_LeftHandSideExpression_NewExpression(self.context, p)
    @_('CallExpression')  # pylint: disable=undefined-variable
    def LeftHandSideExpression(self, p):
        return PN_LeftHandSideExpression_CallExpression(self.context, p)
    @_('NewExpression_Restricted')  # pylint: disable=undefined-variable
    def LeftHandSideExpression_Restricted(self, p):
        return PN_LeftHandSideExpression_NewExpression(self.context, p)
    @_('CallExpression_Restricted')  # pylint: disable=undefined-variable
    def LeftHandSideExpression_Restricted(self, p):
        return PN_LeftHandSideExpression_CallExpression(self.context, p)
    @_('CoverCallExpressionAndAsyncArrowHead')  # pylint: disable=undefined-variable
    def CallExpression(self, p):
        return PN_CallExpression_CoverCallExpressionAndAsyncArrowHead(self.context, p)
    @_('CallExpression Arguments')  # pylint: disable=undefined-variable
    def CallExpression(self, p):
        return PN_CallExpression_CallExpression_Arguments(self.context, p)
    @_('CoverCallExpressionAndAsyncArrowHead_Restricted')  # pylint: disable=undefined-variable
    def CallExpression_Restricted(self, p):
        return PN_CallExpression_CoverCallExpressionAndAsyncArrowHead(self.context, p)
    @_('CallExpression_Restricted Arguments')  # pylint: disable=undefined-variable
    def CallExpression_Restricted(self, p):
        return PN_CallExpression_CallExpression_Arguments(self.context, p)
    @_('MemberExpression Arguments')  # pylint: disable=undefined-variable
    def CallMemberExpression(self, p):
        return PN_CallMemberExpression_MemberExpression_Arguments(self.context, p)
    @_('AssignmentExpression_In')  # pylint: disable=undefined-variable
    def ArgumentList(self, p):
        return PN_ArgumentList_AssignmentExpression(self.context, p)
    @_('DOTDOTDOT AssignmentExpression_In')  # pylint: disable=undefined-variable
    def ArgumentList(self, p):
        return PN_ArgumentList_DOTDOTDOT_AssignmentExpression(self.context, p)
    @_('ArgumentList COMMA AssignmentExpression_In')  # pylint: disable=undefined-variable
    def ArgumentList(self, p):
        return PN_ArgumentList_ArgumentList_COMMA_AssignmentExpression(self.context, p)
    @_('ArgumentList COMMA DOTDOTDOT AssignmentExpression_In')  # pylint: disable=undefined-variable
    def ArgumentList(self, p):
        return PN_ArgumentList_ArgumentList_COMMA_DOTDOTDOT_AssignmentExpression(self.context, p)
    @_('LPAREN RPAREN')  # pylint: disable=undefined-variable
    def Arguments(self, p):
        return PN_Arguments_LPAREN_RPAREN(self.context, p)
    @_('LPAREN ArgumentList RPAREN')  # pylint: disable=undefined-variable
    def Arguments(self, p):
        return PN_Arguments_LPAREN_ArgumentList_RPAREN(self.context, p)
    @_('LPAREN ArgumentList COMMA RPAREN')  # pylint: disable=undefined-variable
    def Arguments(self, p):
        return PN_Arguments_LPAREN_ArgumentList_COMMA_RPAREN(self.context, p)
    @_('MemberExpression')  # pylint: disable=undefined-variable
    def NewExpression(self, p):
        return PN_NewExpression_MemberExpression(self.context, p)
    @_('NEW NewExpression')  # pylint: disable=undefined-variable
    def NewExpression(self, p):
        return PN_NewExpression_NEW_NewExpression(self.context, p)
    @_('MemberExpression_Restricted')  # pylint: disable=undefined-variable
    def NewExpression_Restricted(self, p):
        return PN_NewExpression_MemberExpression(self.context, p)
    @_('NEW NewExpression')  # pylint: disable=undefined-variable
    def NewExpression_Restricted(self, p):
        return PN_NewExpression_NEW_NewExpression(self.context, p)
    @_('PrimaryExpression')  # pylint: disable=undefined-variable
    def MemberExpression(self, p):
        return PN_MemberExpression_PrimaryExpression(self.context, p)
    @_('NEW MemberExpression Arguments')  # pylint: disable=undefined-variable
    def MemberExpression(self, p):
        return PN_MemberExpression_NEW_MemberExpression_Arguments(self.context, p)
    @_('MemberExpression LBRACKET Expression_In RBRACKET')  # pylint: disable=undefined-variable
    def MemberExpression(self, p):
        return PN_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET(self.context, p)
    @_('MemberExpression PERIOD  IdentifierName')  # pylint: disable=undefined-variable
    def MemberExpression(self, p):
        return PN_MemberExpression_MemberExpression_DOT_IdentifierName(self.context, p)
    @_('PrimaryExpression_Restricted')  # pylint: disable=undefined-variable
    def MemberExpression_Restricted(self, p):
        return PN_MemberExpression_PrimaryExpression(self.context, p)
    @_('NEW MemberExpression Arguments')  # pylint: disable=undefined-variable
    def MemberExpression_Restricted(self, p):
        return PN_MemberExpression_NEW_MemberExpression_Arguments(self.context, p)
    @_('MemberExpression_Restricted LBRACKET Expression_In RBRACKET')  # pylint: disable=undefined-variable
    def MemberExpression_Restricted(self, p):
        return PN_MemberExpression_MemberExpression_LBRACKET_Expression_RBRACKET(self.context, p)
    @_('MemberExpression_Restricted PERIOD  IdentifierName')  # pylint: disable=undefined-variable
    def MemberExpression_Restricted(self, p):
        return PN_MemberExpression_MemberExpression_DOT_IdentifierName(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 12.2.6 Object Initializer
    #
    # NOTE 1
    # An object initializer is an expression describing the initialization of an Object, written in a form resembling
    # a literal. It is a list of zero or more pairs of property keys and associated values, enclosed in curly brackets.
    # The values need not be literals; they are evaluated each time the object initializer is evaluated.
    #
    # Syntax
    #
    # ObjectLiteral[Yield, Await] :
    #           { }
    #           { PropertyDefinitionList[?Yield, ?Await] }
    #           { PropertyDefinitionList[?Yield, ?Await] , }
    @_('LCURLY RCURLY')  # pylint: disable=undefined-variable
    def ObjectLiteral(self, p):
        return PN_ObjectLiteral_LCURLY_RCURLY(self.context, p)
    @_('LCURLY PropertyDefinitionList RCURLY')  # pylint: disable=undefined-variable
    def ObjectLiteral(self, p):
        return PN_ObjectLiteral_LCURLY_PropertyDefinitionList_RCURLY(self.context, p)
    @_('LCURLY PropertyDefinitionList COMMA RCURLY')  # pylint: disable=undefined-variable
    def ObjectLiteral(self, p):
        return PN_ObjectLiteral_LCURLY_PropertyDefinitionList_COMMA_RCURLY(self.context, p)
    #
    # PropertyDefinitionList[Yield, Await] :
    #           PropertyDefinition[?Yield, ?Await]
    #           PropertyDefinitionList[?Yield, ?Await] , PropertyDefinition[?Yield, ?Await]
    @_('PropertyDefinition')  # pylint: disable=undefined-variable
    def PropertyDefinitionList(self, p):
        return PN_PropertyDefinitionList_PropertyDefinition(self.context, p)
    @_('PropertyDefinitionList COMMA PropertyDefinition')  # pylint: disable=undefined-variable
    def PropertyDefinitionList(self, p):
        return PN_PropertyDefinitionList_PropertyDefinitionList_COMMA_PropertyDefinition(self.context, p)
    #
    # PropertyDefinition[Yield, Await]:
    #           IdentifierReference[?Yield, ?Await]
    #           CoverInitializedName[?Yield, ?Await]
    #           PropertyName[?Yield, ?Await] : AssignmentExpression[+In, ?Yield, ?Await]
    #           MethodDefinition[?Yield, ?Await]
    #           ... AssignmentExpression[+In, ?Yield, ?Await]
    @_('IdentifierReference')  # pylint: disable=undefined-variable
    def PropertyDefinition(self, p):
        return PN_PropertyDefinition_IdentifierReference(self.context, p)
    @_('CoverInitializedName')  # pylint: disable=undefined-variable
    def PropertyDefinition(self, p):
        return PN_PropertyDefinition_CoverInitializedName(self.context, p)
    @_('PropertyName COLON AssignmentExpression_In')  # pylint: disable=undefined-variable
    def PropertyDefinition(self, p):
        return PN_PropertyDefinition_PropertyName_COLON_AssignmentExpression(self.context, p)
#    @_('MethodDefinition')
#    def PropertyDefinition(self, p):
#        return PN_PropertyDefinition_MethodDefinition(self.context, p)
    @_('DOTDOTDOT AssignmentExpression_In')  # pylint: disable=undefined-variable
    def PropertyDefinition(self, p):
        return PN_PropertyDefinition_DOTDOTDOT_AssignmentExpression(self.context, p)
    #
    # PropertyName[Yield, Await] :
    #           LiteralPropertyName
    #           ComputedPropertyName[?Yield, ?Await]
    @_('LiteralPropertyName')  # pylint: disable=undefined-variable
    def PropertyName(self, p):
        return PN_PropertyName_LiteralPropertyName(self.context, p)
    @_('ComputedPropertyName')  # pylint: disable=undefined-variable
    def PropertyName(self, p):
        return PN_PropertyName_ComputedPropertyName(self.context, p)
    #
    # LiteralPropertyName:
    #           IdentifierName
    #           StringLiteral
    #           NumericLiteral
    @_('IdentifierName')  # pylint: disable=undefined-variable
    def LiteralPropertyName(self, p):
        return PN_LiteralPropertyName_IdentifierName(self.context, p)
    @_('STRING')  # pylint: disable=undefined-variable
    def LiteralPropertyName(self, p):
        return PN_LiteralPropertyName_StringLiteral(self.context, p)
    @_('NUMERIC')  # pylint: disable=undefined-variable
    def LiteralPropertyName(self, p):
        return PN_LiteralPropertyName_NumericLiteral(self.context, p)
    #
    # ComputedPropertyName[Yield, Await] :
    #           [ AssignmentExpression[+In, ?Yield, ?Await] ]
    @_('LBRACKET AssignmentExpression_In RBRACKET')  # pylint: disable=undefined-variable
    def ComputedPropertyName(self, p):
        return PN_ComputedPropertyName_LBRACKET_AssignmentExpression_RBRACKET(self.context, p)
    #
    # CoverInitializedName[Yield, Await] :
    #           IdentifierReference[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]
    @_('IdentifierReference Initializer_In')  # pylint: disable=undefined-variable
    def CoverInitializedName(self, p):
        return PN_CoverInitializedName_IdentifierReference_Initialzier(self.context, p)
    #
    # Initializer[In, Yield, Await] :
    #           = AssignmentExpression[?In, ?Yield, ?Await]
    @_('EQUALS AssignmentExpression_In')  # pylint: disable=undefined-variable
    def Initializer_In(self, p):
        return PN_Initializer_EQUALS_AssignmentExpression(self.context, p)
    @_('EQUALS AssignmentExpression')  # pylint: disable=undefined-variable
    def Initializer(self, p):
        return PN_Initializer_EQUALS_AssignmentExpression(self.context, p)
    #
    # NOTE 2
    # MethodDefinition is defined in 14.3.
    #
    # NOTE 3
    # In certain contexts, ObjectLiteral is used as a cover grammar for a more restricted secondary grammar. The
    # CoverInitializedName production is necessary to fully cover these secondary grammars. However, use of this
    # production results in an early Syntax Error in normal contexts where an actual ObjectLiteral is expected.
    ########################################################################################################################

    ########################################################################################################################
    # 12.2.5 Array Initializer
    #
    # NOTE
    # An ArrayLiteral is an expression describing the initialization of an Array object, using a list, of zero or more
    # expressions each of which represents an array element, enclosed in square brackets. The elements need not be
    # literals; they are evaluated each time the array initializer is evaluated.
    #
    # Array elements may be elided at the beginning, middle or end of the element list. Whenever a comma in the element
    # list is not preceded by an AssignmentExpression (i.e., a comma at the beginning or after another comma), the
    # missing array element contributes to the length of the Array and increases the index of subsequent elements.
    # Elided array elements are not defined. If an element is elided at the end of an array, that element does not
    # contribute to the length of the Array.
    #
    # Syntax
    #
    # ArrayLiteral[Yield, Await] :
    #           [ ]
    #           [ Elision ]
    #           [ ElementList[?Yield, ?Await] ]
    #           [ ElementList[?Yield, ?Await] , ]
    #           [ ElementList[?Yield, ?Await] , Elision ]
    @_('LBRACKET RBRACKET')  # pylint: disable=undefined-variable
    def ArrayLiteral(self, p):
        return PN_ArrayLiteral_LBRACKET_RBRACKET(self.context, p)
    @_('LBRACKET Elision RBRACKET')  # pylint: disable=undefined-variable
    def ArrayLiteral(self, p):
        return PN_ArrayLiteral_LBRACKET_Elision_RBRACKET(self.context, p)
    @_('LBRACKET ElementList RBRACKET')  # pylint: disable=undefined-variable
    def ArrayLiteral(self, p):
        return PN_ArrayLiteral_LBRACKET_ElementList_RBRACKET(self.context, p)
    @_('LBRACKET ElementList COMMA RBRACKET')  # pylint: disable=undefined-variable
    def ArrayLiteral(self, p):
        return PN_ArrayLiteral_LBRACKET_ElementList_COMMA_RBRACKET(self.context, p)
    @_('LBRACKET ElementList COMMA Elision RBRACKET')  # pylint: disable=undefined-variable
    def ArrayLiteral(self, p):
        return PN_ArrayLiteral_LBRACKET_ElementList_COMMA_Elision_RBRACKET(self.context, p)
    #
    # ElementList[Yield, Await] :
    #           AssignmentExpression[+In, ?Yield, ?Await]
    #           Elision AssignmentExpression[+In, ?Yield, ?Await]
    #           SpreadElement[?Yield, ?Await]
    #           Elision SpreadElement[?Yield, ?Await]
    #           ElementList[?Yield, ?Await] , AssignmentExpression[+In, ?Yield, ?Await]
    #           ElementList[?Yield, ?Await] , Elision AssignmentExpression[+In, ?Yield, ?Await]
    #           ElementList[?Yield, ?Await] , SpreadElement[?Yield, ?Await]
    #           ElementList[?Yield, ?Await] , Elision SpreadElement[?Yield, ?Await]
    @_('AssignmentExpression_In')  # pylint: disable=undefined-variable
    def ElementList(self, p):
        return PN_ElementList_AssignmentExpression(self.context, p)
    @_('Elision AssignmentExpression_In')  # pylint: disable=undefined-variable
    def ElementList(self, p):
        return PN_ElementList_Elision_AssignmentExpression(self.context, p)
    @_('SpreadElement')  # pylint: disable=undefined-variable
    def ElementList(self, p):
        return PN_ElementList_SpreadElement(self.context, p)
    @_('Elision SpreadElement')  # pylint: disable=undefined-variable
    def ElementList(self, p):
        return PN_ElementList_Elision_SpreadElement(self.context, p)
    @_('ElementList COMMA AssignmentExpression_In')  # pylint: disable=undefined-variable
    def ElementList(self, p):
        return PN_ElementList_ElementList_COMMA_AssignmentExpression(self.context, p)
    @_('ElementList COMMA Elision AssignmentExpression_In')  # pylint: disable=undefined-variable
    def ElementList(self, p):
        return PN_ElementList_ElementList_COMMA_Elision_AssignmentExpression(self.context, p)
    @_('ElementList COMMA SpreadElement')  # pylint: disable=undefined-variable
    def ElementList(self, p):
        return PN_ElementList_ElementList_COMMA_SpreadElement(self.context, p)
    @_('ElementList COMMA Elision SpreadElement')  # pylint: disable=undefined-variable
    def ElementList(self, p):
        return PN_ElementList_ElementList_COMMA_Elision_SpreadElement(self.context, p)
    #
    # Elision :
    #           ,
    #           Elision ,
    @_('COMMA')  # pylint: disable=undefined-variable
    def Elision(self, p):
        return PN_Elision_COMMA(self.context, p)
    @_('Elision COMMA')  # pylint: disable=undefined-variable
    def Elision(self, p):
        return PN_Elision_Elision_COMMA(self.context, p)
    #
    # SpreadElement[Yield, Await] :
    #           ... AssignmentExpression[+In, ?Yield, ?Await]
    @_('DOTDOTDOT AssignmentExpression_In')  # pylint: disable=undefined-variable
    def SpreadElement(self, p):
        return PN_SpreadElement_DOTDOTDOT_AssignmentExpression(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 12.2.4 Literals
    #
    # Syntax
    #
    # Literal:
    #           NullLiteral
    #           BooleanLiteral
    #           NumericLiteral
    #           StringLiteral
    @_('NULL')  # pylint: disable=undefined-variable
    def Literal(self, p):
        return PN_Literal_NULL(self.context, p)
    @_('TRUE', 'FALSE')  # pylint: disable=undefined-variable
    def Literal(self, p):
        return PN_Literal_BOOLEAN(self.context, p)
    @_('NUMERIC')  # pylint: disable=undefined-variable
    def Literal(self, p):
        return PN_Literal_NUMERIC(self.context, p)
    @_('STRING')  # pylint: disable=undefined-variable
    def Literal(self, p):
        return PN_Literal_STRING(self.context, p)
    ########################################################################################################################

    ########################################################################################################################
    # 12.2 Primary Expression
    #
    # Syntax
    #
    # PrimaryExpression[Yield, Await] :
    #           this
    #           IdentifierReference[?Yield, ?Await]
    #           Literal
    #           ArrayLiteral[?Yield, ?Await]
    #           ObjectLiteral[?Yield, ?Await]
    #           FunctionExpression
    #           ClassExpression[?Yield, ?Await]
    #           GeneratorExpression
    #           AsyncFunctionExpression
    #           AsyncGeneratorExpression
    #           RegularExpressionLiteral
    #           TemplateLiteral[?Yield, ?Await, ~Tagged]
    #           CoverParenthesizedExpressionAndArrowParameterList[?Yield, ?Await]
    #
    # CoverParenthesizedExpressionAndArrowParameterList[Yield, Await] :
    #           ( Expression[+In, ?Yield, ?Await] )
    #           ( Expression[+In, ?Yield, ?Await] , )
    #           ( )
    #           ( ... BindingIdentifier[?Yield, ?Await] )
    #           ( ... BindingPattern[?Yield, ?Await] )
    #           ( Expression[+In, ?Yield, ?Await] , ... BindingIdentifier[?Yield, ?Await] )
    #           ( Expression[+In, ?Yield, ?Await] , ... BindingPattern[?Yield, ?Await] )
    #
    # ParenthesizedExpression[Yield, Await] :
    #           ( Expression[+In, ?Yield, ?Await] )
    @_('THIS')  # pylint: disable=undefined-variable
    def PrimaryExpression(self, p):
        return PN_PrimaryExpression_THIS(self.context, p)
    @_('IdentifierReference')  # pylint: disable=undefined-variable
    def PrimaryExpression(self, p):
        return PN_PrimaryExpression_IdentifierReference(self.context, p)
    @_('Literal')  # pylint: disable=undefined-variable
    def PrimaryExpression(self, p):
        return PN_PrimaryExpression_Literal(self.context, p)
    @_('ObjectLiteral')  # pylint: disable=undefined-variable
    def PrimaryExpression(self, p):
        return PN_PrimaryExpression_ObjectLiteral(self.context, p)
    @_('ArrayLiteral')  # pylint: disable=undefined-variable
    def PrimaryExpression(self, p):
        return PN_PrimaryExpression_ArrayLiteral(self.context, p)
    @_('CoverParenthesizedExpressionAndArrowParameterList')  # pylint: disable=undefined-variable
    def PrimaryExpression(self, p):
        return PN_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList(self.context, p)
    @_('THIS')  # pylint: disable=undefined-variable
    def PrimaryExpression_Restricted(self, p):
        return PN_PrimaryExpression_THIS(self.context, p)
    @_('IdentifierReference')  # pylint: disable=undefined-variable
    def PrimaryExpression_Restricted(self, p):
        return PN_PrimaryExpression_IdentifierReference(self.context, p)
    @_('Literal')  # pylint: disable=undefined-variable
    def PrimaryExpression_Restricted(self, p):
        return PN_PrimaryExpression_Literal(self.context, p)
    @_('ArrayLiteral')  # pylint: disable=undefined-variable
    def PrimaryExpression_Restricted(self, p):
        return PN_PrimaryExpression_ArrayLiteral(self.context, p)
    @_('CoverParenthesizedExpressionAndArrowParameterList')  # pylint: disable=undefined-variable
    def PrimaryExpression_Restricted(self, p):
        return PN_PrimaryExpression_CoverParenthesizedExpressionAndArrowParameterList(self.context, p)

    @_('LPAREN Expression_In RPAREN')  # pylint: disable=undefined-variable
    def CoverParenthesizedExpressionAndArrowParameterList(self, p):
        return PN_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_RPAREN(self.context, p)
    @_('LPAREN Expression_In COMMA RPAREN')  # pylint: disable=undefined-variable
    def CoverParenthesizedExpressionAndArrowParameterList(self, p):
        return PN_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_Expression_COMMA_RPAREN(self.context, p)
    @_('LPAREN RPAREN')  # pylint: disable=undefined-variable
    def CoverParenthesizedExpressionAndArrowParameterList(self, p):
        return PN_CoverParenthesizedExpressionAndArrowParameterList_LPAREN_RPAREN(self.context, p)

    @_('LPAREN Expression_In RPAREN')  # pylint: disable=undefined-variable
    def ParenthesizedExpression(self, p):
        return PN_ParenthesizedExpression_LPAREN_Expression_RPAREN(self.context, p)

    ########################################################################################################################
    # 12.1 Identifiers
    #
    # Syntax
    #
    # IdentifierReference[Yield, Await] :
    #       Identifier
    #       [~Yield]yield
    #       [~Await]await
    #
    # BindingIdentifier[Yield, Await] :
    #       Identifier
    #       yield
    #       await
    #
    # LabelIdentifier[Yield, Await] :
    #       Identifier
    #       [~Yield]yield
    #       [~Await]await
    #
    # Identifier :
    #       IdentifierName but not ReservedWord
    #
    # Note: In this implementation, the tokenizer recognizes Identifiers, so really we have:
    # IdentifierName :
    #       Identifier
    #       ReservedWord
    #
    @_('Identifier')  # pylint: disable=undefined-variable
    def IdentifierReference(self, p):
        return PN_IdentifierReference_Identifier(self.context, p)
    @_('AWAIT')  # pylint: disable=undefined-variable
    def IdentifierReference(self, p):
        return PN_IdentifierReference_AWAIT(self.context, p)
    @_('YIELD')  # pylint: disable=undefined-variable
    def IdentifierReference(self, p):
        return PN_IdentifierReference_YIELD(self.context, p)
    @_('Identifier')  # pylint: disable=undefined-variable
    def IdentifierReference_Yield(self, p):
        return PN_IdentifierReference_Identifier(self.context, p, yield_=True)
    @_('AWAIT')  # pylint: disable=undefined-variable
    def IdentifierReference_Yield(self, p):
        return PN_IdentifierReference_AWAIT(self.context, p, yield_=True)
    @_('Identifier')  # pylint: disable=undefined-variable
    def IdentifierReference_Await(self, p):
        return PN_IdentifierReference_Identifier(self.context, p, await_=True)
    @_('YIELD')  # pylint: disable=undefined-variable
    def IdentifierReference_Await(self, p):
        return PN_IdentifierReference_YIELD(self.context, p, await_=True)
    @_('Identifier')  # pylint: disable=undefined-variable
    def IdentifierReference_Yield_Await(self, p):
        return PN_IdentifierReference_Identifier(self.context, p, yield_=True, await_=True)

    @_('Identifier')  # pylint: disable=undefined-variable
    def BindingIdentifier(self, p):
        return PN_BindingIdentifier_Identifier(self.context, p)
    @_('YIELD')  # pylint: disable=undefined-variable
    def BindingIdentifier(self, p):
        return PN_BindingIdentifier_YIELD(self.context, p)
    @_('AWAIT')  # pylint: disable=undefined-variable
    def BindingIdentifier(self, p):
        return PN_BindingIdentifier_AWAIT(self.context, p)

    @_('Identifier')  # pylint: disable=undefined-variable
    def LabelIdentifier(self, p):
        return PN_LabelIdentifier_Identifier(self.context, p)
    @_('YIELD')  # pylint: disable=undefined-variable
    def LabelIdentifier(self, p):
        return PN_LabelIdentifier_YIELD(self.context, p)
    @_('AWAIT')  # pylint: disable=undefined-variable
    def LabelIdentifier(self, p):
        return PN_LabelIdentifier_AWAIT(self.context, p)

    @_('Identifier', 'ReservedWord')  # pylint: disable=undefined-variable
    def IdentifierName(self, p):
        return PN_IdentifierName(self.context, p)
    @_('AWAIT', 'BREAK', 'CASE', 'CATCH', 'CLASS', 'CONST', 'CONTINUE',  # pylint: disable=undefined-variable
       'DEBUGGER', 'DEFAULT', 'DELETE', 'DO', 'ELSE', 'EXPORT', 'EXTENDS',
        'FINALLY', 'FOR', 'FUNCTION', 'IF', 'IMPORT', 'IN', 'INSTANCEOF',
        'NEW', 'RETURN', 'SUPER', 'SWITCH', 'THIS', 'THROW', 'TRY', 'TYPEOF',
        'VAR', 'VOID', 'WHILE', 'WITH', 'YIELD', 'ENUM', 'NULL', 'TRUE',
        'FALSE')
    def ReservedWord(self, p):
        return PN_ReservedWord(self.context, p)

    @_('IDENTIFIER', 'OF', 'LET')  # pylint: disable=undefined-variable
    def Identifier(self, p):
        return PN_Identifier(self.context, p)

# 15.1.8 Script Records
class ScriptRecord(Record):
    __slots__ = ['Realm', 'Environment', 'ECMAScriptCode', 'HostDefined']

# 15.1.9 ParseScript ( sourceText, realm, hostDefined )
def ParseScript(sourceText, realm, hostDefined):
    # The abstract operation ParseScript with arguments sourceText, realm, and hostDefined creates a Script Record based upon
    # the result of parsing sourceText as a Script. ParseScript performs the following steps:
    #
    # 1. Assert: sourceText is an ECMAScript source text (see clause 10).
    # 2. Parse sourceText using Script as the goal symbol and analyse the parse result for any Early Error conditions. If the
    #    parse was successful and no early errors were found, let body be the resulting parse tree. Otherwise, let body be a
    #    List of one or more SyntaxError or ReferenceError objects representing the parsing errors and/or early errors. Parsing
    #    and early error detection may be interweaved in an implementation-dependent manner. If more than one parsing error or
    #    early error is present, the number and ordering of error objects in the list is implementation-dependent, but at least
    #    one must be present.
    lex = Lexer(sourceText)
    psr = Ecma262Parser(start='Script', source_text=sourceText)
    tree = psr.parse(lex.lex())
    errs = tree.EarlyErrorsScan()
    body = errs or tree
    # 3. If body is a List of errors, return body.
    if isinstance(body, list):
        return body
    # 4. Return Script Record { [[Realm]]: realm, [[Environment]]: undefined, [[ECMAScriptCode]]: body,
    #                           [[HostDefined]]: hostDefined }.
    return ScriptRecord(Realm=realm, Environment=None, ECMAScriptCode=body, HostDefined=hostDefined)
    # NOTE
    # An implementation may parse script source text and analyse it for Early Error conditions prior to evaluation of
    # ParseScript for that script source text. However, the reporting of any errors must be deferred until the point where this
    # specification actually performs ParseScript upon that source text.

# 15.1.10 ScriptEvaluation ( scriptRecord )
def ScriptEvaluation(scriptRecord):
    # 1. Let globalEnv be scriptRecord.[[Realm]].[[GlobalEnv]].
    globalEnv = scriptRecord.Realm.global_env
    # 2. Let scriptCxt be a new ECMAScript code execution context.
    scriptCtx = ExecutionContext()
    # 3. Set the Function of scriptCxt to null.
    scriptCtx.function = JSNull.NULL
    # 4. Set the Realm of scriptCxt to scriptRecord.[[Realm]].
    scriptCtx.realm = scriptRecord.Realm
    # 5. Set the ScriptOrModule of scriptCxt to scriptRecord.
    scriptCtx.script_or_module = scriptRecord
    # 6. Set the VariableEnvironment of scriptCxt to globalEnv.
    scriptCtx.variable_environment = globalEnv
    # 7. Set the LexicalEnvironment of scriptCxt to globalEnv.
    scriptCtx.lexical_environment = globalEnv
    # 8. Suspend the currently running execution context.
    surrounding_agent.running_ec.suspend()
    # 9. Push scriptCxt on to the execution context stack; scriptCxt is now the running execution context.
    surrounding_agent.ec_stack.append(scriptCtx)
    surrounding_agent.running_ec = scriptCtx
    # 10. Let scriptBody be scriptRecord.[[ECMAScriptCode]].
    scriptBody = scriptRecord.ECMAScriptCode
    # 11. Let result be GlobalDeclarationInstantiation(scriptBody, globalEnv).
    try:
        GlobalDeclarationInstantiation(scriptBody, globalEnv)
        # 12. If result.[[Type]] is normal, then
        # a. Set result to the result of evaluating scriptBody.
        result = scriptBody.evaluate()
        # 13. If result.[[Type]] is normal and result.[[Value]] is empty, then
        # a. Set result to NormalCompletion(undefined).
        if result == Empty.EMPTY:
            result = None
    finally:
        # 14. Suspend scriptCxt and remove it from the execution context stack.
        scriptCtx.suspend()
        surrounding_agent.ec_stack.pop()
        # 15. Assert: The execution context stack is not empty.
        assert len(surrounding_agent.ec_stack) > 0
        # 16. Resume the context that is now on the top of the execution context stack as the running execution context.
        surrounding_agent.running_ec = surrounding_agent.ec_stack[-1]
    # 17. Return Completion(result).
    return result

# 15.1.11 Runtime Semantics: GlobalDeclarationInstantiation ( script, env )
def GlobalDeclarationInstantiation(script, env):
    # NOTE 1
    # When an execution context is established for evaluating scripts, declarations are instantiated in the current global
    # environment. Each global binding declared in the code is instantiated.
    #
    # GlobalDeclarationInstantiation is performed as follows using arguments script and env. script is the ScriptBody for
    # which the execution context is being established. env is the global lexical environment in which bindings are to be
    # created.
    #
    # 1. Let envRec be env's EnvironmentRecord.
    envRec = env.environment_record
    # 2. Assert: envRec is a global Environment Record.
    assert hasattr(envRec, 'global_this_value')
    # 3. Let lexNames be the LexicallyDeclaredNames of script.
    lexNames = script.LexicallyDeclaredNames()
    # 4. Let varNames be the VarDeclaredNames of script.
    varNames = script.VarDeclaredNames()
    # 5. For each name in lexNames, do
    for name in lexNames:
        # a. If envRec.HasVarDeclaration(name) is true, throw a SyntaxError exception.
        if envRec.HasVarDeclaration(name):
            raise ESSyntaxError()
        # b. If envRec.HasLexicalDeclaration(name) is true, throw a SyntaxError exception.
        if envRec.HasLexicalDeclaration(name):
            raise ESSyntaxError()
        # c. Let hasRestrictedGlobal be ? envRec.HasRestrictedGlobalProperty(name).
        hasRestrictedGlobal = envRec.HasRestrictedGlobalProperty(name)
        # d. If hasRestrictedGlobal is true, throw a SyntaxError exception.
        if hasRestrictedGlobal:
            raise ESSyntaxError()
    # 6. For each name in varNames, do
    for name in varNames:
        # a. If envRec.HasLexicalDeclaration(name) is true, throw a SyntaxError exception.
        if envRec.HasLexicalDeclaration(name):
            raise ESSyntaxError()
    # 7. Let varDeclarations be the VarScopedDeclarations of script.
    varDeclarations = script.VarScopedDeclarations()
    # 8. Let functionsToInitialize be a new empty List.
    functionsToInitialize = deque()
    # 9. Let declaredFunctionNames be a new empty List.
    declaredFunctionNames = []
    # 10. For each d in varDeclarations, in reverse list order, do
    for d in (varDeclarations[-x-1] for x in range(len(varDeclarations))):
        # --- each of these 'd' is a ParseNode.
        # a. If d is neither a VariableDeclaration nor a ForBinding nor a BindingIdentifier, then
        if d.name not in ['VariableDeclaration', 'ForBinding', 'BindingIdentifier']:
            # i. Assert: d is either a FunctionDeclaration, a GeneratorDeclaration, an AsyncFunctionDeclaration, or an
            #    AsyncGeneratorDeclaration.
            assert d.name in ['FunctionDeclaration', 'GeneratorDeclaration', 'AsyncFunctionDeclaration',
                              'AsyncGeneratorDeclaration']
            # ii. NOTE: If there are multiple function declarations for the same name, the last declaration is used.
            # iii. Let fn be the sole element of the BoundNames of d.
            fn = d.BoundNames()[0]
            # iv. If fn is not an element of declaredFunctionNames, then
            if fn not in declaredFunctionNames:
                # 1. Let fnDefinable be ? envRec.CanDeclareGlobalFunction(fn).
                fnDefinable = envRec.CanDeclareGlobalFunction(fn)
                # 2. If fnDefinable is false, throw a TypeError exception.
                if not fnDefinable:
                    raise ESTypeError()
                # 3. Append fn to declaredFunctionNames.
                declaredFunctionNames.append(fn)
                # 4. Insert d as the first element of functionsToInitialize.
                functionsToInitialize.appendleft(d)
    # 11. Let declaredVarNames be a new empty List.
    declaredVarNames = []
    # 12. For each d in varDeclarations, do
    for d in varDeclarations:
        # a. If d is a VariableDeclaration, a ForBinding, or a BindingIdentifier, then
        if d.name in ['VariableDeclaration', 'ForBinding', 'BindingIdentifier']:
            # i. For each String vn in the BoundNames of d, do
            for vn in d.BoundNames():
                # 1. If vn is not an element of declaredFunctionNames, then
                if vn not in declaredFunctionNames:
                    # a. Let vnDefinable be ? envRec.CanDeclareGlobalVar(vn).
                    vnDefinable = envRec.CanDeclareGlobalVar(vn)
                    # b. If vnDefinable is false, throw a TypeError exception.
                    if not vnDefinable:
                        raise ESTypeError()
                    # c. If vn is not an element of declaredVarNames, then
                    if vn not in declaredVarNames:
                        # i. Append vn to declaredVarNames.
                        declaredVarNames.append(vn)
    # 13. NOTE: No abnormal terminations occur after this algorithm step if the global object is an ordinary object. However,
    #     if the global object is a Proxy exotic object it may exhibit behaviours that cause abnormal terminations in some of
    #     the following steps.
    # 14. NOTE: Annex B.3.3.2 adds additional steps at this point.
    # 15. Let lexDeclarations be the LexicallyScopedDeclarations of script.
    lexDeclarations = script.LexicallyScopedDeclarations()
    # 16. For each element d in lexDeclarations, do
    for d in lexDeclarations:
        # a. NOTE: Lexically declared names are only instantiated here but not initialized.
        # b. For each element dn of the BoundNames of d, do
        for dn in d.BoundNames():
            # i. If IsConstantDeclaration of d is true, then
            if d.IsConstantDeclaration():
                # 1. Perform ? envRec.CreateImmutableBinding(dn, true).
                envRec.CreateImmutableBinding(dn, True)
            # ii. Else,
            else:
                # 1. Perform ? envRec.CreateMutableBinding(dn, false).
                envRec.CreateMutableBinding(dn, False)
    # 17. For each Parse Node f in functionsToInitialize, do
    for f in functionsToInitialize:
        # a. Let fn be the sole element of the BoundNames of f.
        fn = f.BoundNames()[0]
        # b. Let fo be the result of performing InstantiateFunctionObject for f with argument env.
        fo = f.InstantiateFunctionObject(env)
        # c. Perform ? envRec.CreateGlobalFunctionBinding(fn, fo, false).
        envRec.CreateGlobalFunctionBinding(fn, fo, False)
    # 18.For each String vn in declaredVarNames, in list order, do
    for vn in declaredVarNames:
        # a. Perform ? envRec.CreateGlobalVarBinding(vn, false).
        envRec.CreateGlobalVarBinding(vn, False)
    # 19. Return NormalCompletion(empty).
    return Empty.EMPTY
    # NOTE 2
    # Early errors specified in 15.1.1 prevent name conflicts between function/var declarations and let/const/class
    # declarations as well as redeclaration of let/const/class bindings for declaration contained within a single Script.
    # However, such conflicts and redeclarations that span more than one Script are detected as runtime errors during
    # GlobalDeclarationInstantiation. If any such errors are detected, no bindings are instantiated for the script. However, if
    # the global object is defined using Proxy exotic objects then the runtime tests for conflicting declarations may be
    # unreliable resulting in an abrupt completion and some global declarations not being instantiated. If this occurs, the
    # code for the Script is not evaluated.
    #
    # Unlike explicit var or function declarations, properties that are directly created on the global object result in global
    # bindings that may be shadowed by let/const/class declarations.

# 15.1.12 Runtime Semantics: ScriptEvaluationJob ( sourceText, hostDefined )
def ScriptEvaluationJob(source_text, host_defined):
    # The job ScriptEvaluationJob with parameters sourceText and hostDefined parses, validates, and evaluates
    # sourceText as a Script.
    #
    # 1. Assert: sourceText is an ECMAScript source text (see clause 10).
    assert isString(source_text)
    # 2. Let realm be the current Realm Record.
    realm = surrounding_agent.running_ec.realm
    # 3. Let s be ParseScript(sourceText, realm, hostDefined).
    script_nodes = ParseScript(source_text, realm, host_defined)
    # 4. If s is a List of errors, then
    if isinstance(script_nodes, list):
        # a. Perform HostReportErrors(s).
        HostReportErrors(script_nodes)
        # b. Return NormalCompletion(undefined).
        return None
    # 5. Return ? ScriptEvaluation(s).
    return ScriptEvaluation(script_nodes)

# 16.1 HostReportErrors ( errorList )
def HostReportErrors(errorList):
    # HostReportErrors is an implementation-defined abstract operation that allows host environments to report parsing
    # errors, early errors, and runtime errors.
    #
    # An implementation of HostReportErrors must complete normally in all cases. The default implementation of
    # HostReportErrors is to unconditionally return an empty normal completion.
    #
    # NOTE
    # errorList will be a List of ECMAScript language values. If the errors are parsing errors or early errors, these
    # will always be SyntaxError or ReferenceError objects. Runtime errors, however, can be any ECMAScript value.

    for err in errorList:
        print(ToString(err))

    return Empty.EMPTY

###########################################################################################################################################################
#
#  d888    .d8888b.       d888        .d88888b.  888         d8b                   888         .d88888b.  888         d8b                   888
# d8888   d88P  Y88b     d8888       d88P" "Y88b 888         Y8P                   888        d88P" "Y88b 888         Y8P                   888
#   888   888    888       888       888     888 888                               888        888     888 888                               888
#   888   Y88b. d888       888       888     888 88888b.    8888  .d88b.   .d8888b 888888     888     888 88888b.    8888  .d88b.   .d8888b 888888 .d8888b
#   888    "Y888P888       888       888     888 888 "88b   "888 d8P  Y8b d88P"    888        888     888 888 "88b   "888 d8P  Y8b d88P"    888    88K
#   888          888       888       888     888 888  888    888 88888888 888      888        888     888 888  888    888 88888888 888      888    "Y8888b.
#   888   Y88b  d88P d8b   888       Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.      Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.       X88
# 8888888  "Y8888P"  Y8P 8888888      "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888      "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888  88888P'
#                                                            888                                                      888
#                                                           d88P                                                     d88P
#                                                         888P"                                                    888P"
#
###########################################################################################################################################################

# 19.1.1 The Object Constructor
#
# The Object constructor:
#
#     * is the intrinsic object %Object%.
#     * is the initial value of the Object property of the global object.
#     * creates a new ordinary object when called as a constructor.
#     * performs a type conversion when called as a function rather than as a constructor.
#     * is designed to be subclassable. It may be used as the value of an extends clause of a class definition.
def BindBuiltinFunctions(realm, obj, details):
    for key, fcn, length in details:
        func_obj = CreateBuiltinFunction(fcn, [], realm)
        DefinePropertyOrThrow(func_obj, 'length', PropertyDescriptor(value=length, writable=False, enumerable=False, configurable=True))
        DefinePropertyOrThrow(func_obj, 'name', PropertyDescriptor(value=key, writable=False, enumerable=False, configurable=True))
        CreateMethodPropertyOrThrow(obj, key, func_obj)
    return None

def CreateObjectConstructor(realm):
    intrinsics = realm.intrinsics
    obj = CreateBuiltinFunction(ObjectFunction, ['Construct'], realm=realm)
    for key, value in [('length', 1), ('name', 'Object')]:
        DefinePropertyOrThrow(obj, key, PropertyDescriptor(value=value, writable=False, enumerable=False, configurable=True))
    DefinePropertyOrThrow(obj, 'prototype', PropertyDescriptor(value=intrinsics['%ObjectPrototype%'], writable=False, enumerable=False, configurable=False))
    BindBuiltinFunctions(realm, obj, [
        ('assign', ObjectMethod_assign, 2),
        ('create', ObjectMethod_create, 2),
        ('defineProperties', ObjectMethod_defineProperties, 2),
        ('defineProperty', ObjectMethod_defineProperty, 3),
        ('entries', ObjectMethod_entries, 1),
        ('freeze', ObjectMethod_freeze, 1),
        ('getOwnPropertyDescriptor', ObjectMethod_getOwnPropertyDescriptor, 2),
        ('getOwnPropertyDescriptors', ObjectMethod_getOwnPropertyDescriptors, 1),
        ('getOwnPropertyNames', ObjectMethod_getOwnPropertyNames, 1),
        ('getOwnPropertySymbols', ObjectMethod_getOwnPropertySymbols, 1),
        ('getPrototypeOf', ObjectMethod_getPrototypeOf, 1),
        ('is', ObjectMethod_is, 2),
        ('isExtensible', ObjectMethod_isExtensible, 1),
        ('isFrozen', ObjectMethod_isFrozen, 1),
        ('isSealed', ObjectMethod_isSealed, 1),
        ('keys', ObjectMethod_keys, 1),
        ('preventExtensions', ObjectMethod_preventExtensions, 1),
        ('seal', ObjectMethod_seal, 1),
        ('setPrototypeOf', ObjectMethod_setPrototypeOf, 2),
        ('values', ObjectMethod_values, 1)])
    return obj

# 19.1.1.1 Object ( [ value ] )
def ObjectFunction(_, new_target, value=None):
    # When the Object function is called with optional argument value, the following steps are taken:
    #
    # 1. If NewTarget is neither undefined nor the active function, then
    active_function = GetActiveFunction()
    if new_target is not None and new_target != active_function:
        # a. Return ? OrdinaryCreateFromConstructor(NewTarget, "%ObjectPrototype%").
        return OrdinaryCreateFromConstructor(new_target, "%ObjectPrototype%")
    # 2. If value is null, undefined or not supplied, return ObjectCreate(%ObjectPrototype%).
    if value is None or isNull(value):
        return ObjectCreate(surrounding_agent.running_ec.realm.intrinsics['%ObjectPrototype%'])
    # 3. Return ! ToObject(value).
    return ToObject(value)

# 19.1.2.1 Object.assign ( target, ...sources )
def ObjectMethod_assign(_a, _b, target, *sources):
    # The assign function is used to copy the values of all of the enumerable own properties from one or more source
    # objects to a target object. When the assign function is called, the following steps are taken:
    #
    # 1. Let to be ? ToObject(target).
    to_obj = ToObject(target)
    # 2. If only one argument was passed, return to.
    if len(sources) == 0:
        return to_obj
    # 3. Let sources be the List of argument values starting with the second argument.
    # 4. For each element nextSource of sources, in ascending index order, do
    for next_source in sources:
        # a. If nextSource is undefined or null, let keys be a new empty List.
        if isNull(next_source) or next_source is None:
            keys = []
        # b. Else,
        else:
            # i. Let from be ! ToObject(nextSource).
            from_obj = ToObject(next_source)
            # ii. Let keys be ? from.[[OwnPropertyKeys]]().
            keys = from_obj.OwnPropertyKeys()
        # c. For each element nextKey of keys in List order, do
        for next_key in keys:
            # i. Let desc be ? from.[[GetOwnProperty]](nextKey).
            desc = from_obj.GetOwnProperty(next_key)
            # ii. If desc is not undefined and desc.[[Enumerable]] is true, then
            if desc is not None and desc.enumerable:
                # 1. Let propValue be ? Get(from, nextKey).
                prop_value = Get(from_obj, next_key)
                # 2. Perform ? Set(to, nextKey, propValue, true).
                Set(to_obj, next_key, prop_value, True)
    # 5. Return to.
    return to_obj

# 19.1.2.2 Object.create ( O, Properties )
def ObjectMethod_create(_a, _b, o_value, properties):
    # The create function creates a new object with a specified prototype. When the create function is called, the
    # following steps are taken:
    #
    # 1. If Type(O) is neither Object nor Null, throw a TypeError exception.
    if not isObject(o_value) and not isNull(o_value):
        raise ESTypeError()
    # 2. Let obj be ObjectCreate(O).
    obj = ObjectCreate(o_value)
    # 3. If Properties is not undefined, then
    if properties is not None:
        # a. Return ? ObjectDefineProperties(obj, Properties).
        return ObjectDefineProperties(obj, properties)
    # 4. Return obj.
    return obj

# 19.1.2.3 Object.defineProperties ( O, Properties )
def ObjectMethod_defineProperties(_a, _b, o_value, properties):
    # The defineProperties function is used to add own properties and/or update the attributes of existing own
    # properties of an object. When the defineProperties function is called, the following steps are taken:
    #
    # 1. Return ? ObjectDefineProperties(O, Properties).
    return ObjectDefineProperties(o_value, properties)

# 19.1.2.3.1 Runtime Semantics: ObjectDefineProperties ( O, Properties )
def ObjectDefineProperties(o_value, properties):
    # The abstract operation ObjectDefineProperties with arguments O and Properties performs the following steps:
    #
    # 1. If Type(O) is not Object, throw a TypeError exception.
    if not isObject(o_value):
        raise ESTypeError()
    # 2. Let props be ? ToObject(Properties).
    props = ToObject(properties)
    # 3. Let keys be ? props.[[OwnPropertyKeys]]().
    keys = props.OwnPropertyKeys()
    # 4. Let descriptors be a new empty List.
    descriptors = []
    # 5. For each element nextKey of keys in List order, do
    for next_key in keys:
        # a. Let propDesc be ? props.[[GetOwnProperty]](nextKey).
        prop_desc = props.GetOwnProperty(next_key)
        # b. If propDesc is not undefined and propDesc.[[Enumerable]] is true, then
        if prop_desc is not None and prop_desc.enumerable:
            # i. Let descObj be ? Get(props, nextKey).
            desc_obj = Get(props, next_key)
            # ii. Let desc be ? ToPropertyDescriptor(descObj).
            desc = ToPropertyDescriptor(desc_obj)
            # iii. Append the pair (a two element List) consisting of nextKey and desc to the end of descriptors.
            descriptors.append((next_key, desc))
    # 6. For each pair from descriptors in list order, do
    for prop_key, desc in descriptors:
        # a. Let P be the first element of pair.
        # b. Let desc be the second element of pair.
        # c. Perform ? DefinePropertyOrThrow(O, P, desc).
        DefinePropertyOrThrow(o_value, prop_key, desc)
    # 7. Return O.
    return o_value

# 19.1.2.4 Object.defineProperty ( O, P, Attributes )
def ObjectMethod_defineProperty(_a, _b, o_value, prop, attributes):
    # The defineProperty function is used to add an own property and/or update the attributes of an existing own
    # property of an object. When the defineProperty function is called, the following steps are taken:
    #
    # 1. If Type(O) is not Object, throw a TypeError exception.
    if not isObject(o_value):
        raise ESTypeError()
    # 2. Let key be ? ToPropertyKey(P).
    key = ToPropertyKey(prop)
    # 3. Let desc be ? ToPropertyDescriptor(Attributes).
    desc = ToPropertyDescriptor(attributes)
    # 4. Perform ? DefinePropertyOrThrow(O, key, desc).
    DefinePropertyOrThrow(o_value, key, desc)
    # 5. Return O.
    return o_value

# 19.1.2.5 Object.entries ( O )
def ObjectMethod_entries(_a, _b, o_value):
    # When the entries function is called with argument O, the following steps are taken:
    #
    # 1. Let obj be ? ToObject(O).
    obj = ToObject(o_value)
    # 2. Let nameList be ? EnumerableOwnPropertyNames(obj, "key+value").
    name_list = EnumerableOwnPropertyNames(obj, 'key+value')
    # 3. Return CreateArrayFromList(nameList).
    return CreateArrayFromList(name_list)

# 19.1.2.6 Object.freeze ( O )
def ObjectMethod_freeze(_a, _b, o_value):
    # When the freeze function is called, the following steps are taken:
    #
    # 1. If Type(O) is not Object, return O.
    if not isObject(o_value):
        return o_value
    # 2. Let status be ? SetIntegrityLevel(O, "frozen").
    status = SetIntegrityLevel(o_value, 'frozen')
    # 3. If status is false, throw a TypeError exception.
    if not status:
        raise ESTypeError()
    # 4. Return O.
    return o_value

# 19.1.2.7 Object.getOwnPropertyDescriptor ( O, P )
def ObjectMethod_getOwnPropertyDescriptor(_a, _b, o_value, propkey):
    # When the getOwnPropertyDescriptor function is called, the following steps are taken:
    #
    # 1. Let obj be ? ToObject(O).
    obj = ToObject(o_value)
    # 2. Let key be ? ToPropertyKey(P).
    key = ToPropertyKey(propkey)
    # 3. Let desc be ? obj.[[GetOwnProperty]](key).
    desc = obj.GetOwnProperty(key)
    # 4. Return FromPropertyDescriptor(desc).
    return FromPropertyDescriptor(desc)

# 19.1.2.8 Object.getOwnPropertyDescriptors ( O )
def ObjectMethod_getOwnPropertyDescriptors(_a, _b, o_value):
    # When the getOwnPropertyDescriptors function is called, the following steps are taken:
    #
    # 1. Let obj be ? ToObject(O).
    obj = ToObject(o_value)
    # 2. Let ownKeys be ? obj.[[OwnPropertyKeys]]().
    own_keys = obj.OwnPropertyKeys()
    # 3. Let descriptors be ! ObjectCreate(%ObjectPrototype%).
    descriptors = ObjectCreate(surrounding_agent.running_ec.realm.intrinsics['%ObjectPrototype%'])
    # 4. For each element key of ownKeys in List order, do
    for key in own_keys:
        # a. Let desc be ? obj.[[GetOwnProperty]](key).
        desc = obj.GetOwnProperty(key)
        # b. Let descriptor be ! FromPropertyDescriptor(desc).
        descriptor = FromPropertyDescriptor(desc)
        # c. If descriptor is not undefined, perform ! CreateDataProperty(descriptors, key, descriptor).
        if descriptor is not None:
            CreateDataProperty(descriptors, key, descriptor)
    # 5. Return descriptors.
    return descriptors

# 19.1.2.9 Object.getOwnPropertyNames ( O )
def ObjectMethod_getOwnPropertyNames(_a, _b, o_value):
    # When the getOwnPropertyNames function is called, the following steps are taken:
    #
    # 1. Return ? GetOwnPropertyKeys(O, String).
    return GetOwnPropertyKeys(o_value, isString)

# 19.1.2.10 Object.getOwnPropertySymbols ( O )
def ObjectMethod_getOwnPropertySymbols(_a, _b, o_value):
    # When the getOwnPropertySymbols function is called with argument O, the following steps are taken:
    #
    # 1. Return ? GetOwnPropertyKeys(O, Symbol).
    return GetOwnPropertyKeys(o_value, isSymbol)

# 19.1.2.10.1 Runtime Semantics: GetOwnPropertyKeys ( O, Type )
def GetOwnPropertyKeys(o_value, type_checker):
    # The abstract operation GetOwnPropertyKeys is called with arguments O and Type where O is an Object and Type is
    # one of the ECMAScript specification types String or Symbol. The following steps are taken:
    #
    # 1. Let obj be ? ToObject(O).
    obj = ToObject(o_value)
    # 2. Let keys be ? obj.[[OwnPropertyKeys]]().
    keys = obj.OwnPropertyKeys()
    # 3. Let nameList be a new empty List.
    # 4. For each element nextKey of keys in List order, do
        # a. If Type(nextKey) is Type, then
            # i. Append nextKey as the last element of nameList.
    name_list = [key for key in keys if type_checker(key)]
    # 5. Return CreateArrayFromList(nameList).
    return CreateArrayFromList(name_list)

# 19.1.2.11 Object.getPrototypeOf ( O )
def ObjectMethod_getPrototypeOf(_a, _b, o_value):
    # When the getPrototypeOf function is called with argument O, the following steps are taken:
    #
    # 1. Let obj be ? ToObject(O).
    obj = ToObject(o_value)
    # 2. Return ? obj.[[GetPrototypeOf]]().
    return obj.GetPrototypeOf()

# 19.1.2.12 Object.is ( value1, value2 )
def ObjectMethod_is(_a, _b, value1, value2):
    # When the is function is called with arguments value1 and value2, the following steps are taken:
    #
    # 1. Return SameValue(value1, value2).
    return SameValue(value1, value2)

# 19.1.2.13 Object.isExtensible ( O )
def ObjectMethod_isExtensible(_a, _b, o_value):
    # When the isExtensible function is called with argument O, the following steps are taken:
    #
    # 1. If Type(O) is not Object, return false.
    if not isObject(o_value):
        return False
    # 2. Return ? IsExtensible(O).
    return IsExtensible(o_value)

# 19.1.2.14 Object.isFrozen ( O )
def ObjectMethod_isFrozen(_a, _b, o_value):
    # When the isFrozen function is called with argument O, the following steps are taken:
    #
    # 1. If Type(O) is not Object, return true.
    if not isObject(o_value):
        return True
    # 2. Return ? TestIntegrityLevel(O, "frozen").
    return TestIntegrityLevel(o_value, 'frozen')

# 19.1.2.15 Object.isSealed ( O )
def ObjectMethod_isSealed(_a, _b, o_value):
    # When the isSealed function is called with argument O, the following steps are taken:
    #
    # 1. If Type(O) is not Object, return true.
    if not isObject(o_value):
        return True
    # 2. Return ? TestIntegrityLevel(O, "sealed").
    return TestIntegrityLevel(o_value, 'sealed')

# 19.1.2.16 Object.keys ( O )
def ObjectMethod_keys(_a, _b, o_value):
    # When the keys function is called with argument O, the following steps are taken:
    #
    # 1. Let obj be ? ToObject(O).
    obj = ToObject(o_value)
    # 2. Let nameList be ? EnumerableOwnPropertyNames(obj, "key").
    name_list = EnumerableOwnPropertyNames(obj, 'key')
    # 3. Return CreateArrayFromList(nameList).
    return CreateArrayFromList(name_list)

# 19.1.2.17 Object.preventExtensions ( O )
def ObjectMethod_preventExtensions(_a, _b, o_value):
    # When the preventExtensions function is called, the following steps are taken:
    #
    # 1. If Type(O) is not Object, return O.
    if not isObject(o_value):
        return o_value
    # 2. Let status be ? O.[[PreventExtensions]]().
    status = o_value.PreventExtensions()
    # 3. If status is false, throw a TypeError exception.
    if not status:
        raise ESTypeError()
    # 4. Return O.
    return o_value

# 19.1.2.19 Object.seal ( O )
def ObjectMethod_seal(_a, _b, o_value):
    # When the seal function is called, the following steps are taken:
    #
    # 1. If Type(O) is not Object, return O.
    if not isObject(o_value):
        return o_value
    # 2. Let status be ? SetIntegrityLevel(O, "sealed").
    status = SetIntegrityLevel(o_value, 'sealed')
    # 3. If status is false, throw a TypeError exception.
    if not status:
        raise ESTypeError()
    # 4. Return O.
    return o_value

# 19.1.2.20 Object.setPrototypeOf ( O, proto )
def ObjectMethod_setPrototypeOf(_a, _b, o_value, proto):
    # When the setPrototypeOf function is called with arguments O and proto, the following steps are taken:
    #
    # 1. Let O be ? RequireObjectCoercible(O).
    o_value = RequireObjectCoercible(o_value)
    # 2. If Type(proto) is neither Object nor Null, throw a TypeError exception.
    if not isObject(proto) and not isNull(proto):
        raise ESTypeError()
    # 3. If Type(O) is not Object, return O.
    if not isObject(o_value):
        return o_value
    # 4. Let status be ? O.[[SetPrototypeOf]](proto).
    status = o_value.SetPrototypeOf(proto)
    # 5. If status is false, throw a TypeError exception.
    if not status:
        raise ESTypeError()
    # 6. Return O.
    return o_value

# 19.1.2.21 Object.values ( O )
def ObjectMethod_values(_a, _b, o_value):
    # When the values function is called with argument O, the following steps are taken:
    #
    # 1. Let obj be ? ToObject(O).
    obj = ToObject(o_value)
    # 2. Let nameList be ? EnumerableOwnPropertyNames(obj, "value").
    name_list = EnumerableOwnPropertyNames(obj, 'value')
    # 3. Return CreateArrayFromList(nameList).
    return CreateArrayFromList(name_list)

# 19.1.3 Properties of the Object Prototype Object
#
# The Object prototype object:
#    * is the intrinsic object %ObjectPrototype%.
#    * is an immutable prototype exotic object.
#    * has a [[Prototype]] internal slot whose value is null.
#
def AddObjectPrototypeProps(realm_rec):
    intrinsics = realm_rec.intrinsics
    obj = intrinsics['%ObjectPrototype%']
    # 19.1.3.1 Object.prototype.constructor
    # The initial value of Object.prototype.constructor is the intrinsic object %Object%.
    DefinePropertyOrThrow(obj, 'constructor', PropertyDescriptor(value=intrinsics['%Object%'], writable=False, enumerable=False, configurable=False))
    BindBuiltinFunctions(realm_rec, obj, [
        ('hasOwnProperty', ObjectPrototype_hasOwnProperty, 1),
        ('isPrototypeOf', ObjectPrototype_isPrototypeOf, 1),
        ('propertyIsEnumerable', ObjectPrototype_propertyIsEnumerable, 1),
        ('toLocaleString', ObjectPrototype_toLocaleString, 0),
        ('toString', ObjectPrototype_toString, 0),
        ('valueOf', ObjectPrototype_valueOf, 0)
        ])
    return None

# 19.1.3.2 Object.prototype.hasOwnProperty ( V )
def ObjectPrototype_hasOwnProperty(this_value, _, key):
    # When the hasOwnProperty method is called with argument V, the following steps are taken:
    #
    # 1. Let P be ? ToPropertyKey(V).
    p = ToPropertyKey(key)
    # 2. Let O be ? ToObject(this value).
    o = ToObject(this_value)
    # 3. Return ? HasOwnProperty(O, P).
    return HasOwnProperty(o, p)
    # NOTE
    # The ordering of steps 1 and 2 is chosen to ensure that any exception that would have been thrown by step 1 in
    # previous editions of this specification will continue to be thrown even if the this value is undefined or null.

# 19.1.3.3 Object.prototype.isPrototypeOf ( V )
def ObjectPrototype_isPrototypeOf(this_value, _, obj):
    # When the isPrototypeOf method is called with argument V, the following steps are taken:
    #
    # 1. If Type(V) is not Object, return false.
    if not isObject(obj):
        return False
    # 2. Let O be ? ToObject(this value).
    o = ToObject(this_value)
    # 3. Repeat,
    while 1:
        # a. Let V be ? V.[[GetPrototypeOf]]().
        obj = obj.GetPrototypeOf()
        # b. If V is null, return false.
        if isNull(obj):
            return False
        # c. If SameValue(O, V) is true, return true.
        if SameValue(o, obj):
            return True
    # NOTE
    # The ordering of steps 1 and 2 preserves the behaviour specified by previous editions of this specification for
    # the case where V is not an object and the this value is undefined or null.

# 19.1.3.4 Object.prototype.propertyIsEnumerable ( V )
def ObjectPrototype_propertyIsEnumerable(this_value, _, v):
    # When the propertyIsEnumerable method is called with argument V, the following steps are taken:
    #
    # 1. Let P be ? ToPropertyKey(V).
    p = ToPropertyKey(v)
    # 2. Let O be ? ToObject(this value).
    o = ToObject(this_value)
    # 3. Let desc be ? O.[[GetOwnProperty]](P).
    desc = o.GetOwnProperty(p)
    # 4. If desc is undefined, return false.
    if desc is None:
        return False
    # 5. Return desc.[[Enumerable]].
    return desc.enumerable
    # NOTE 1
    # This method does not consider objects in the prototype chain.
    # NOTE 2
    # The ordering of steps 1 and 2 is chosen to ensure that any exception that would have been thrown by step 1 in
    # previous editions of this specification will continue to be thrown even if the this value is undefined or null.

# 19.1.3.5 Object.prototype.toLocaleString ( [ reserved1 [ , reserved2 ] ] )
def ObjectPrototype_toLocaleString(this_value, _, reserved1=None, reserved2=None):
    # When the toLocaleString method is called, the following steps are taken:
    #
    # 1. Let O be the this value.
    o = this_value
    # 2. Return ? Invoke(O, "toString").
    return Invoke(o, 'toString')
    # The optional parameters to this function are not used but are intended to correspond to the parameter pattern
    # used by ECMA-402  toLocaleString functions. Implementations that do not include ECMA-402 support must not use
    # those parameter positions for other purposes.
    #
    # NOTE 1
    # This function provides a generic toLocaleString implementation for objects that have no locale-specific toString
    # behaviour. Array, Number, Date, and Typed Arrays provide their own locale-sensitive toLocaleString methods.
    # NOTE 2
    # ECMA-402 intentionally does not provide an alternative to this default implementation.

# 19.1.3.6 Object.prototype.toString ( )
def ObjectPrototype_toString(this_value, _):
    # When the toString method is called, the following steps are taken:
    #
    # 1. If the this value is undefined, return "[object Undefined]".
    if this_value is None:
        return '[object Undefined]'
    # 2. If the this value is null, return "[object Null]".
    if isNull(this_value):
        return '[object Null]'
    # 3. Let O be ! ToObject(this value).
    o = ToObject(this_value)
    # 4. Let isArray be ? IsArray(O).
    is_array = IsArray(o)
    # 5. If isArray is true, let builtinTag be "Array".
    if is_array:
        builtin_tag = 'Array'
    # 6. Else if O is a String exotic object, let builtinTag be "String".
    elif isinstance(o, StringObject):
        builtin_tag = 'String'
    # 7. Else if O has a [[ParameterMap]] internal slot, let builtinTag be "Arguments".
    elif hasattr(o, 'ParameterMap'):
        builtin_tag = 'Arguments'
    # 8. Else if O has a [[Call]] internal method, let builtinTag be "Function".
    elif hasattr(o, 'Call'):
        builtin_tag = 'Function'
    # 9. Else if O has an [[ErrorData]] internal slot, let builtinTag be "Error".
    elif hasattr(o, 'ErrorData'):
        builtin_tag = 'Error'
    # 10. Else if O has a [[BooleanData]] internal slot, let builtinTag be "Boolean".
    elif hasattr(o, 'BooleanData'):
        builtin_tag = 'Boolean'
    # 11. Else if O has a [[NumberData]] internal slot, let builtinTag be "Number".
    elif hasattr(o, 'NumberData'):
        builtin_tag = 'Number'
    # 12. Else if O has a [[DateValue]] internal slot, let builtinTag be "Date".
    elif hasattr(o, 'DateValue'):
        builtin_tag = 'Date'
    # 13. Else if O has a [[RegExpMatcher]] internal slot, let builtinTag be "RegExp".
    elif hasattr(o, 'RegExpMatcher'):
        builtin_tag = 'RegExp'
    # 14. Else, let builtinTag be "Object".
    else:
        builtin_tag = 'Object'
    # 15. Let tag be ? Get(O, @@toStringTag).
    tag = Get(o, wks_to_string_tag)
    # 16. If Type(tag) is not String, let tag be builtinTag.
    if not isString(tag):
        tag = builtin_tag
    # 17. Return the string-concatenation of "[object ", tag, and "]".
    return f'[object {tag}]'
    # This function is the %ObjProto_toString% intrinsic object.
    # NOTE
    # Historically, this function was occasionally used to access the String value of the [[Class]] internal slot that
    # was used in previous editions of this specification as a nominal type tag for various built-in objects. The above
    # definition of toString preserves compatibility for legacy code that uses toString as a test for those specific
    # kinds of built-in objects. It does not provide a reliable type testing mechanism for other kinds of built-in or
    # program defined objects. In addition, programs can use @@toStringTag in ways that will invalidate the reliability
    # of such legacy type tests.

# 19.1.3.7 Object.prototype.valueOf ( )
def ObjectPrototype_valueOf(this_value, _):
    # When the valueOf method is called, the following steps are taken:
    #
    # 1. Return ? ToObject(this value).
    return ToObject(this_value)
    # This function is the %ObjProto_valueOf% intrinsic object.

#####################################################################################################################################################################
#
#  d888    .d8888b.       .d8888b.      8888888888                            888    d8b                        .d88888b.  888         d8b                   888
# d8888   d88P  Y88b     d88P  Y88b     888                                   888    Y8P                       d88P" "Y88b 888         Y8P                   888
#   888   888    888            888     888                                   888                              888     888 888                               888
#   888   Y88b. d888          .d88P     8888888    888  888 88888b.   .d8888b 888888 888  .d88b.  88888b.      888     888 88888b.    8888  .d88b.   .d8888b 888888 .d8888b
#   888    "Y888P888      .od888P"      888        888  888 888 "88b d88P"    888    888 d88""88b 888 "88b     888     888 888 "88b   "888 d8P  Y8b d88P"    888    88K
#   888          888     d88P"          888        888  888 888  888 888      888    888 888  888 888  888     888     888 888  888    888 88888888 888      888    "Y8888b.
#   888   Y88b  d88P d8b 888"           888        Y88b 888 888  888 Y88b.    Y88b.  888 Y88..88P 888  888     Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.       X88
# 8888888  "Y8888P"  Y8P 888888888      888         "Y88888 888  888  "Y8888P  "Y888 888  "Y88P"  888  888      "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888  88888P'
#                                                                                                                                      888
#                                                                                                                                     d88P
#                                                                                                                                   888P"
#
#####################################################################################################################################################################

#####################################################################################################################################################################
#
#  d888    .d8888b.       .d8888b.      888888b.                     888                                 .d88888b.  888         d8b                   888
# d8888   d88P  Y88b     d88P  Y88b     888  "88b                    888                                d88P" "Y88b 888         Y8P                   888
#   888   888    888          .d88P     888  .88P                    888                                888     888 888                               888
#   888   Y88b. d888         8888"      8888888K.   .d88b.   .d88b.  888  .d88b.   8888b.  88888b.      888     888 88888b.    8888  .d88b.   .d8888b 888888 .d8888b
#   888    "Y888P888          "Y8b.     888  "Y88b d88""88b d88""88b 888 d8P  Y8b     "88b 888 "88b     888     888 888 "88b   "888 d8P  Y8b d88P"    888    88K
#   888          888     888    888     888    888 888  888 888  888 888 88888888 .d888888 888  888     888     888 888  888    888 88888888 888      888    "Y8888b.
#   888   Y88b  d88P d8b Y88b  d88P     888   d88P Y88..88P Y88..88P 888 Y8b.     888  888 888  888     Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.       X88
# 8888888  "Y8888P"  Y8P  "Y8888P"      8888888P"   "Y88P"   "Y88P"  888  "Y8888  "Y888888 888  888      "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888  88888P'
#                                                                                                                               888
#                                                                                                                              d88P
#                                                                                                                            888P"
#
#####################################################################################################################################################################
# 19.3 Boolean Objects
#
# 19.3.1 The Boolean Constructor
#
# The Boolean constructor:
#
#   * is the intrinsic object %Boolean%.
#   * is the initial value of the Boolean property of the global object.
#   * creates and initializes a new Boolean object when called as a constructor.
#   * performs a type conversion when called as a function rather than as a constructor.
#   * is designed to be subclassable. It may be used as the value of an extends clause of a class definition. Subclass
#     constructors that intend to inherit the specified Boolean behaviour must include a super call to the Boolean constructor
#     to create and initialize the subclass instance with a [[BooleanData]] internal slot.
def CreateBooleanConstructor(realm):
    obj = CreateBuiltinFunction(BooleanFunction, ['Construct'], realm=realm)
    for key, value in [('length', 1), ('name', 'Boolean')]:
        desc = PropertyDescriptor(value=value, writable=False, enumerable=False, configurable=True)
        DefinePropertyOrThrow(obj, key, desc)
    return obj

# 19.3.1.1 Boolean ( value )
def BooleanFunction(_, new_target, value):
    # When Boolean is called with argument value, the following steps are taken:
    #
    # 1. Let b be ToBoolean(value).
    b = ToBoolean(value)
    # 2. If NewTarget is undefined, return b.
    if new_target is None:
        return b
    # 3. Let O be ? OrdinaryCreateFromConstructor(NewTarget, "%BooleanPrototype%", « [[BooleanData]] »).
    o = OrdinaryCreateFromConstructor(new_target, '%BooleanPrototype%', ['BooleanData'])
    # 4. Set O.[[BooleanData]] to b.
    o.BooleanData = b
    # 5. Return O.
    return o

def BooleanFixups(realm):
    boolean_constructor = realm.intrinsics['%Boolean%']
    boolean_prototype = realm.intrinsics['%BooleanPrototype%']
    DefinePropertyOrThrow(boolean_constructor, 'prototype',
                PropertyDescriptor(value=boolean_prototype, writable=False, enumerable=False, configurable=False))
    DefinePropertyOrThrow(boolean_prototype, 'constructor', PropertyDescriptor(value=boolean_constructor))
    return None

# 19.3.3 Properties of the Boolean Prototype Object
#
# The Boolean prototype object:
#
#   * is the intrinsic object %BooleanPrototype%.
#   * is an ordinary object.
#   * is itself a Boolean object; it has a [[BooleanData]] internal slot with the value false.
#   * has a [[Prototype]] internal slot whose value is the intrinsic object %ObjectPrototype%.
def CreateBooleanPrototype(realm):
    boolean_prototype = ObjectCreate(realm.intrinsics['%ObjectPrototype%'], ['BooleanData'])
    boolean_prototype.BooleanData = False
    BindBuiltinFunctions(realm, boolean_prototype, [
        ('toString', BooleanPrototype_toString, 0),
        ('valueOf', BooleanPrototype_valueOf, 0)
        ])
    return boolean_prototype

def thisBooleanValue(value):
    # The abstract operation thisBooleanValue(value) performs the following steps:
    #
    # 1. If Type(value) is Boolean, return value.
    if isBoolean(value):
        return value
    # 2. If Type(value) is Object and value has a [[BooleanData]] internal slot, then
    if isObject(value) and hasattr(value, 'BooleanData'):
        # a. Let b be value.[[BooleanData]].
        b = value.BooleanData
        # b. Assert: Type(b) is Boolean.
        assert isBoolean(b)
        # c. Return b.
        return b
    # 3. Throw a TypeError exception.
    raise ESTypeError()

# 19.3.3.2 Boolean.prototype.toString ( )
def BooleanPrototype_toString(this_value, _):
    # The following steps are taken:
    #
    # 1. Let b be ? thisBooleanValue(this value).
    b = thisBooleanValue(this_value)
    # 2. If b is true, return "true"; else return "false".
    return 'true' if b else 'false'

# 19.3.3.3 Boolean.prototype.valueOf ( )
def BooleanPrototype_valueOf(this_value, _):
    # The following steps are taken:
    #
    # 1. Return ? thisBooleanValue(this value).
    return thisBooleanValue(this_value)
##################################################################################################################################################################################
# ------------------------------------ 𝟏𝟗.𝟒.𝟑.𝟐.𝟏 𝑺𝒚𝒎𝒃𝒐𝒍𝑫𝒆𝒔𝒄𝒓𝒊𝒑𝒕𝒊𝒗𝒆𝑺𝒕𝒓𝒊𝒏𝒈 ( 𝒔𝒚𝒎 ) ------------------------------------
# 19.4.3.2.1 Runtime Semantics: SymbolDescriptiveString ( sym )
def SymbolDescriptiveString(sym):
    # When the abstract operation SymbolDescriptiveString is called with argument sym, the following steps are taken:
    #
    # 1. Assert: Type(sym) is Symbol.
    # 2. Let desc be sym's [[Description]] value.
    # 3. If desc is undefined, let desc be the empty string.
    # 4. Assert: Type(desc) is String.
    # 5. Return the string-concatenation of "Symbol(", desc, and ")".
    assert isSymbol(sym)
    desc = sym.description or ''
    assert isString(desc)
    return f'Symbol({desc})'

##################################################################################################################################################################################
#
#  d888    .d8888b.      888888888      8888888888                                       .d88888b.  888         d8b                   888
# d8888   d88P  Y88b     888            888                                             d88P" "Y88b 888         Y8P                   888
#   888   888    888     888            888                                             888     888 888                               888
#   888   Y88b. d888     8888888b.      8888888    888d888 888d888  .d88b.  888d888     888     888 88888b.    8888  .d88b.   .d8888b 888888 .d8888b
#   888    "Y888P888          "Y88b     888        888P"   888P"   d88""88b 888P"       888     888 888 "88b   "888 d8P  Y8b d88P"    888    88K
#   888          888            888     888        888     888     888  888 888         888     888 888  888    888 88888888 888      888    "Y8888b.
#   888   Y88b  d88P d8b Y88b  d88P     888        888     888     Y88..88P 888         Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.       X88
# 8888888  "Y8888P"  Y8P  "Y8888P"      8888888888 888     888      "Y88P"  888          "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888  88888P'
#                                                                                                               888
#                                                                                                              d88P
#                                                                                                            888P"
#
##################################################################################################################################################################################
# 19.5 Error Objects
# Instances of Error objects are thrown as exceptions when runtime errors occur. The Error objects may also serve as
# base objects for user-defined exception classes.
#
# 19.5.1 The Error Constructor
# The Error constructor:
#
#  * is the intrinsic object %Error%.
#  * is the initial value of the Error property of the global object.
#  * creates and initializes a new Error object when called as a function rather than as a constructor. Thus the
#    function call Error(…) is equivalent to the object creation expression new Error(…) with the same arguments.
#  * is designed to be subclassable. It may be used as the value of an extends clause of a class definition. Subclass
#    constructors that intend to inherit the specified Error behaviour must include a super call to the Error
#    constructor to create and initialize subclass instances with an [[ErrorData]] internal slot.
def CreateErrorConstructor(realm):
    obj = CreateBuiltinFunction(ErrorFunction, ['Construct'], realm=realm)
    for key, value in [('length', 1), ('name', 'Error')]:
        desc = PropertyDescriptor(value=value, writable=False, enumerable=False, configurable=True)
        DefinePropertyOrThrow(obj, key, desc)
    return obj

# 19.5.1.1 Error ( message )
def ErrorFunction(this_value, new_target, message):
    # When the Error function is called with argument message, the following steps are taken:
    #
    # 1. If NewTarget is undefined, let newTarget be the active function object, else let newTarget be NewTarget.
    # 2. Let O be ? OrdinaryCreateFromConstructor(newTarget, "%ErrorPrototype%", « [[ErrorData]] »).
    # 3. If message is not undefined, then
    #    a. Let msg be ? ToString(message).
    #    b. Let msgDesc be the PropertyDescriptor
    #       { [[Value]]: msg, [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: true }.
    #    c. Perform ! DefinePropertyOrThrow(O, "message", msgDesc).
    # 4. Return O.
    newTarget = new_target or surrounding_agent.running_ec.function
    O = OrdinaryCreateFromConstructor(newTarget, '%ErrorPrototype%', ['ErrorData'])
    if message is not None:
        msg = ToString(message)
        msgDesc = PropertyDescriptor(value=msg, writable=True, enumerable=False, configurable=True)
        DefinePropertyOrThrow(O, 'message', msgDesc)
    return O

# 19.5.2 Properties of the Error Constructor
# The Error constructor:
#
#   * has a [[Prototype]] internal slot whose value is the intrinsic object %FunctionPrototype%.
#   * has the following properties:

# 19.5.2.1 Error.prototype
# The initial value of Error.prototype is the intrinsic object %ErrorPrototype%.
# This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: false }.

def ErrorFixups(realm):
    error_constructor = realm.intrinsics['%Error%']
    error_prototype = realm.intrinsics['%ErrorPrototype%']
    DefinePropertyOrThrow(error_constructor, 'prototype',
                PropertyDescriptor(value=error_prototype, writable=False, enumerable=False, configurable=False))
    # 19.5.3.1 Error.prototype.constructor
    # The initial value of Error.prototype.constructor is the intrinsic object %Error%.
    DefinePropertyOrThrow(error_prototype, 'constructor', PropertyDescriptor(value=error_constructor))
    return None

# 19.5.3 Properties of the Error Prototype Object
# The Error prototype object:
#
#   * is the intrinsic object %ErrorPrototype%.
#   * is an ordinary object.
#   * is not an Error instance and does not have an [[ErrorData]] internal slot.
#   * has a [[Prototype]] internal slot whose value is the intrinsic object %ObjectPrototype%.
def CreateErrorPrototype(realm):
    error_prototype = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    # 19.5.3.2 Error.prototype.message
    # The initial value of Error.prototype.message is the empty String.
    # 19.5.3.3 Error.prototype.name
    # The initial value of Error.prototype.name is "Error".
    for key, value in [('message', ''), ('name', 'Error')]:
        Set(error_prototype, key, value, True)
    BindBuiltinFunctions(realm, error_prototype, [('toString', ErrorPrototype_toString, 1)])
    return error_prototype

# 19.5.3.4 Error.prototype.toString ( )
def ErrorPrototype_toString(this_value, _):
    # The following steps are taken:
    #
    # 1. Let O be the this value.
    # 2. If Type(O) is not Object, throw a TypeError exception.
    # 3. Let name be ? Get(O, "name").
    # 4. If name is undefined, let name be "Error"; otherwise let name be ? ToString(name).
    # 5. Let msg be ? Get(O, "message").
    # 6. If msg is undefined, let msg be the empty String; otherwise let msg be ? ToString(msg).
    # 7. If name is the empty String, return msg.
    # 8. If msg is the empty String, return name.
    # 9. Return the string-concatenation of name, the code unit 0x003A (COLON), the code unit 0x0020 (SPACE), and msg.
    if not isObject(this_value):
        raise ESTypeError('Method used on non-object')
    name = Get(this_value, 'name')
    if name is None:
        name = 'Error'
    else:
        name = ToString(name)
    msg = Get(this_value, 'message')
    if msg is None:
        msg = ''
    else:
        msg = ToString(msg)
    if name == '':
        return msg
    if msg == '':
        return name
    return f'{name}: {msg}'

def CreateNativeErrorConstructor(realm, errorname):
    errfunc = CreateErrorConstructorFunction(errorname)
    obj = CreateBuiltinFunction(errfunc, ['Construct'], realm=realm, prototype=realm.intrinsics['%Error%'])
    for key, value in [('length', 1), ('name', f'{errorname}Error')]:
        desc = PropertyDescriptor(value=value, writable=False, enumerable=False, configurable=True)
        DefinePropertyOrThrow(obj, key, desc)
    return obj

def CreateErrorConstructorFunction(name):
    def native_error_function(this_value, new_target, message):
        newTarget = new_target or surrounding_agent.running_ec.function
        O = OrdinaryCreateFromConstructor(newTarget, f'%{name}ErrorPrototype%', ['ErrorData'])
        if message is not None:
            msg = ToString(message)
            msgDesc = PropertyDescriptor(value=msg, writable=True, enumerable=False, configurable=True)
            DefinePropertyOrThrow(O, 'message', msgDesc)
        return O
    return native_error_function

def NativeErrorFixups(realm):
    for name in ['Eval', 'Range', 'Reference', 'Syntax', 'Type', 'URI']:
        constructor = realm.intrinsics[f'%{name}Error%']
        prototype = realm.intrinsics[f'%{name}ErrorPrototype%']
        DefinePropertyOrThrow(constructor, 'prototype',
                    PropertyDescriptor(value=prototype, writable=False, enumerable=False, configurable=False))
        DefinePropertyOrThrow(prototype, 'constructor', PropertyDescriptor(value=constructor))
    return None

def CreateNativeErrorPrototype(realm, name):
    prototype = ObjectCreate(realm.intrinsics['%ErrorPrototype%'])
    for key, value in [('message', ''), ('name', f'{name}Error')]:
        Set(prototype, key, value, True)
    return prototype


##################################################################################################################################################################################
##################################################################################################################################################################################
##
##  .d8888b.   .d8888b.      888b    888                        888                                                           888     8888888b.           888
## d88P  Y88b d88P  Y88b     8888b   888                        888                                                           888     888  "Y88b          888
##        888 888    888     88888b  888                        888                                                           888     888    888          888
##      .d88P 888    888     888Y88b 888 888  888 88888b.d88b.  88888b.   .d88b.  888d888 .d8888b       8888b.  88888b.   .d88888     888    888  8888b.  888888  .d88b.  .d8888b
##  .od888P"  888    888     888 Y88b888 888  888 888 "888 "88b 888 "88b d8P  Y8b 888P"   88K              "88b 888 "88b d88" 888     888    888     "88b 888    d8P  Y8b 88K
## d88P"      888    888     888  Y88888 888  888 888  888  888 888  888 88888888 888     "Y8888b.     .d888888 888  888 888  888     888    888 .d888888 888    88888888 "Y8888b.
## 888"       Y88b  d88P     888   Y8888 Y88b 888 888  888  888 888 d88P Y8b.     888          X88     888  888 888  888 Y88b 888     888  .d88P 888  888 Y88b.  Y8b.          X88
## 888888888   "Y8888P"      888    Y888  "Y88888 888  888  888 88888P"   "Y8888  888      88888P'     "Y888888 888  888  "Y88888     8888888P"  "Y888888  "Y888  "Y8888   88888P'
##
##################################################################################################################################################################################
##################################################################################################################################################################################

#######################################################################################################################################################################
#
#  .d8888b.   .d8888b.       d888       888b    888                        888                            .d88888b.  888         d8b                   888
# d88P  Y88b d88P  Y88b     d8888       8888b   888                        888                           d88P" "Y88b 888         Y8P                   888
#        888 888    888       888       88888b  888                        888                           888     888 888                               888
#      .d88P 888    888       888       888Y88b 888 888  888 88888b.d88b.  88888b.   .d88b.  888d888     888     888 88888b.    8888  .d88b.   .d8888b 888888 .d8888b
#  .od888P"  888    888       888       888 Y88b888 888  888 888 "888 "88b 888 "88b d8P  Y8b 888P"       888     888 888 "88b   "888 d8P  Y8b d88P"    888    88K
# d88P"      888    888       888       888  Y88888 888  888 888  888  888 888  888 88888888 888         888     888 888  888    888 88888888 888      888    "Y8888b.
# 888"       Y88b  d88P d8b   888       888   Y8888 Y88b 888 888  888  888 888 d88P Y8b.     888         Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.       X88
# 888888888   "Y8888P"  Y8P 8888888     888    Y888  "Y88888 888  888  888 88888P"   "Y8888  888          "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888  88888P'
#                                                                                                                                888
#                                                                                                                               d88P
#                                                                                                                             888P"
#
#######################################################################################################################################################################
# 20.1.1 The Number Constructor
# The Number constructor:
#
#   * is the intrinsic object %Number%.
#   * is the initial value of the Number property of the global object.
#   * creates and initializes a new Number object when called as a constructor.
#   * performs a type conversion when called as a function rather than as a constructor.
#   * is designed to be subclassable. It may be used as the value of an extends clause of a class definition. Subclass
#     constructors that intend to inherit the specified Number behaviour must include a super call to the Number constructor
#     to create and initialize the subclass instance with a [[NumberData]] internal slot.
#
def CreateNumberConstructor(realm):
    obj = CreateBuiltinFunction(NumberFunction, ['Construct'], realm=realm)
    for key, value in [('length', 1), ('name', 'Number')]:
        desc = PropertyDescriptor(value=value, writable=False, enumerable=False, configurable=True)
        DefinePropertyOrThrow(obj, key, desc)
    return obj

# 20.1.1.1 Number ( value )
def NumberFunction(_, new_target, value=missing.MISSING):
    # When Number is called with argument value, the following steps are taken:
    #
    # 1. If no arguments were passed to this function invocation, let n be +0.
    if value == missing.MISSING:
        n = 0
    # 2. Else, let n be ? ToNumber(value).
    else:
        n = ToNumber(value)
    # 3. If NewTarget is undefined, return n.
    if new_target is None:
        return n
    # 4. Let O be ? OrdinaryCreateFromConstructor(NewTarget, "%NumberPrototype%", « [[NumberData]] »).
    o = OrdinaryCreateFromConstructor(new_target, '%NumberPrototype%', ['NumberData'])
    # 5. Set O.[[NumberData]] to n.
    o.NumberData = n
    # 6. Return O.
    return o

# 20.1.3 Properties of the Number Prototype Object
# The Number prototype object:
#
#   * is the intrinsic object %NumberPrototype%.
#   * is an ordinary object.
#   * is itself a Number object; it has a [[NumberData]] internal slot with the value +0.
#   * has a [[Prototype]] internal slot whose value is the intrinsic object %ObjectPrototype%.
#   * Unless explicitly stated otherwise, the methods of the Number prototype object defined below are not generic and the this
#     value passed to them must be either a Number value or an object that has a [[NumberData]] internal slot that has been
#     initialized to a Number value.
def CreateNumberPrototype(realm):
    number_prototype = ObjectCreate(realm.intrinsics['%ObjectPrototype%'], ['NumberData'])
    number_prototype.NumberData = 0
    BindBuiltinFunctions(realm, number_prototype, [
        ('toString', NumberPrototype_toString, 1),
        ('valueOf', NumberPrototype_valueOf, 0)
        ])
    return number_prototype

def thisNumberValue(value):
    # The abstract operation thisNumberValue(value) performs the following steps:
    #
    # 1. If Type(value) is Number, return value.
    if isNumber(value):
        return value
    # 2. If Type(value) is Object and value has a [[NumberData]] internal slot, then
    if isObject(value) and hasattr(value, 'NumberData'):
        # a. Let n be value.[[NumberData]].
        n = value.NumberData
        # b. Assert: Type(n) is Number.
        assert(isNumber(n))
        # c. Return n.
        return n
    # 3. Throw a TypeError exception.
    raise ESTypeError()
    # The phrase “this Number value” within the specification of a method refers to the result returned by calling the abstract
    # operation thisNumberValue with the this value of the method invocation passed as the argument.

# 20.1.3.6 Number.prototype.toString ( [ radix ] )
def NumberPrototype_toString(this_value, _, radix=None):
    # NOTE
    # The optional radix should be an integer value in the inclusive range 2 to 36. If radix is not present or is undefined the
    # Number 10 is used as the value of radix.
    #
    # The following steps are performed:
    #
    # 1. Let x be ? thisNumberValue(this value).
    x = thisNumberValue(this_value)
    # 2. If radix is not present, let radixNumber be 10.
    # 3. Else if radix is undefined, let radixNumber be 10.
    if radix is None:
        radixNumber = 10
    # 4. Else, let radixNumber be ? ToInteger(radix).
    else:
        radixNumber = ToInteger(radix)
    # 5. If radixNumber < 2 or radixNumber > 36, throw a RangeError exception.
    if radixNumber < 2 or radixNumber > 36:
        raise ESRangeError()
    # 6. If radixNumber = 10, return ! ToString(x).
    if radixNumber == 10:
        return ToString(x)
    # 7. Return the String representation of this Number value using the radix specified by radixNumber. Letters a-z are used
    #    for digits with values 10 through 35. The precise algorithm is implementation-dependent, however the algorithm should
    #    be a generalization of that specified in 7.1.12.1.
    raise NotImplementedError()
    # The toString function is not generic; it throws a TypeError exception if its this value is not a Number or a Number
    # object. Therefore, it cannot be transferred to other kinds of objects for use as a method.

# 20.1.3.7 Number.prototype.valueOf ( )
def NumberPrototype_valueOf(this_value, _):
    # 1. Return ? thisNumberValue(this value).
    return thisNumberValue(this_value)

def NumberFixups(realm):
    number_constructor = realm.intrinsics['%Number%']
    number_prototype = realm.intrinsics['%NumberPrototype%']
    proto_desc = PropertyDescriptor(value=number_prototype, writable=False, enumerable=False, configurable=False)
    DefinePropertyOrThrow(number_constructor, 'prototype', proto_desc)
    DefinePropertyOrThrow(number_prototype, 'constructor', PropertyDescriptor(value=number_constructor))
    return None

# ------------------------------------ 𝟐𝟏 𝑻𝒆𝒙𝒕 𝑷𝒓𝒐𝒄𝒆𝒔𝒔𝒊𝒏𝒈 ------------------------------------
# ------------------------------------ 𝟐𝟏.𝟏 𝑺𝒕𝒓𝒊𝒏𝒈 𝑶𝒃𝒋𝒆𝒄𝒕𝒔 ------------------------------------
#######################################################################################################################################################
#
#  .d8888b.   d888        d888        .d8888b.  888            d8b                        .d88888b.  888         d8b                   888
# d88P  Y88b d8888       d8888       d88P  Y88b 888            Y8P                       d88P" "Y88b 888         Y8P                   888
#        888   888         888       Y88b.      888                                      888     888 888                               888
#      .d88P   888         888        "Y888b.   888888 888d888 888 88888b.   .d88b.      888     888 88888b.    8888  .d88b.   .d8888b 888888 .d8888b
#  .od888P"    888         888           "Y88b. 888    888P"   888 888 "88b d88P"88b     888     888 888 "88b   "888 d8P  Y8b d88P"    888    88K
# d88P"        888         888             "888 888    888     888 888  888 888  888     888     888 888  888    888 88888888 888      888    "Y8888b.
# 888"         888   d8b   888       Y88b  d88P Y88b.  888     888 888  888 Y88b 888     Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.       X88
# 888888888  8888888 Y8P 8888888      "Y8888P"   "Y888 888     888 888  888  "Y88888      "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888  88888P'
#                                                                                888                             888
#                                                                           Y8b d88P                            d88P
#                                                                            "Y88P"                           888P"
#
#######################################################################################################################################################
# ------------------------------------ 𝟐𝟏.𝟏.𝟏 𝑻𝒉𝒆 𝑺𝒕𝒓𝒊𝒏𝒈 𝑪𝒐𝒏𝒔𝒕𝒓𝒖𝒄𝒕𝒐𝒓 ------------------------------------
# 21.1.1 The String Constructor
# The String constructor:
#
#   * is the intrinsic object %String%.
#   * is the initial value of the String property of the global object.
#   * creates and initializes a new String object when called as a constructor.
#   * performs a type conversion when called as a function rather than as a constructor.
#   * is designed to be subclassable. It may be used as the value of an extends clause of a class definition. Subclass
#     constructors that intend to inherit the specified String behaviour must include a super call to the String
#     constructor to create and initialize the subclass instance with a [[StringData]] internal slot.
#
def CreateStringConstructor(realm):
    obj = CreateBuiltinFunction(StringFunction, ['Construct'], realm=realm)
    for key, value in [('length', 1), ('name', 'String')]:
        desc = PropertyDescriptor(value=value, writable=False, enumerable=False, configurable=True)
        DefinePropertyOrThrow(obj, key, desc)
    BindBuiltinFunctions(realm, obj, [
        ('fromCharCode', String_fromCharCode, 1),
        ('fromCodePoint', String_fromCodePoint, 1),
    ])
    return obj

# ------------------------------------ 𝟐𝟏.𝟏.𝟏.𝟏 𝑺𝒕𝒓𝒊𝒏𝒈 ( 𝒗𝒂𝒍𝒖𝒆 ) ------------------------------------
def StringFunction(this_value, new_target, value=missing.MISSING):
    # When String is called with argument value, the following steps are taken:
    #
    # 1. If no arguments were passed to this function invocation, let s be "".
    # 2. Else,
    #    a. If NewTarget is undefined and Type(value) is Symbol, return SymbolDescriptiveString(value).
    #    b. Let s be ? ToString(value).
    # 3. If NewTarget is undefined, return s.
    # 4. Return ? StringCreate(s, ? GetPrototypeFromConstructor(NewTarget, "%StringPrototype%")).
    if value == missing.MISSING:
        s = ''
    else:
        if new_target is None and isSymbol(value):
            return SymbolDescriptiveString(value)
        s = ToString(value)
    if new_target is None:
        return s
    proto = GetPrototypeFromConstructor(new_target, '%StringPrototype%')
    return StringCreate(s, proto)

# ------------------------------------ 𝟐𝟏.𝟏.𝟐 𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒊𝒆𝒔 𝒐𝒇 𝒕𝒉𝒆 𝑺𝒕𝒓𝒊𝒏𝒈 𝑪𝒐𝒏𝒔𝒕𝒓𝒖𝒄𝒕𝒐𝒓 ------------------------------------
# 21.1.2 Properties of the String Constructor
#
# The String constructor:
#
#   * has a [[Prototype]] internal slot whose value is the intrinsic object %FunctionPrototype%.
#   * has the following properties:

# ------------------------------------ 𝟐𝟏.𝟏.𝟐.𝟏 𝑺𝒕𝒓𝒊𝒏𝒈.𝒇𝒓𝒐𝒎𝑪𝒉𝒂𝒓𝑪𝒐𝒅𝒆 ( ...𝒄𝒐𝒅𝒆𝑼𝒏𝒊𝒕𝒔 ) ------------------------------------
# 21.1.2.1 String.fromCharCode ( ...codeUnits )
def String_fromCharCode(this_value, new_target, *codeUnits):
    # The String.fromCharCode function may be called with any number of arguments which form the rest parameter
    # codeUnits. The following steps are taken:
    #
    # 1. Let codeUnits be a List containing the arguments passed to this function.
    # 2. Let length be the number of elements in codeUnits.
    # 3. Let elements be a new empty List.
    # 4. Let nextIndex be 0.
    # 5. Repeat, while nextIndex < length
    #    a. Let next be codeUnits[nextIndex].
    #    b. Let nextCU be ? ToUint16(next).
    #    c. Append nextCU to the end of elements.
    #    d. Let nextIndex be nextIndex + 1.
    # 6. Return the String value whose elements are, in order, the elements in the List elements. If length is 0, the
    #    empty string is returned.
    elements = []
    for next in codeUnits:
        codevalue = ToUint16(next)
        elements.append(codevalue)
    return ''.join(chr(x) for x in elements)
    # The length property of the fromCharCode function is 1.

# ------------------------------------ 𝟐𝟏.𝟏.𝟐.𝟐 𝑺𝒕𝒓𝒊𝒏𝒈.𝒇𝒓𝒐𝒎𝑪𝒐𝒅𝒆𝑷𝒐𝒊𝒏𝒕 ( ...𝒄𝒐𝒅𝒆𝑷𝒐𝒊𝒏𝒕𝒔 ) ------------------------------------
# 21.1.2.2 String.fromCodePoint ( ...codePoints )
def String_fromCodePoint(this_value, new_target, *codePoints):
    # The String.fromCodePoint function may be called with any number of arguments which form the rest parameter
    # codePoints. The following steps are taken:
    #
    # 1. Let codePoints be a List containing the arguments passed to this function.
    # 2. Let length be the number of elements in codePoints.
    # 3. Let elements be a new empty List.
    # 4. Let nextIndex be 0.
    # 5. Repeat, while nextIndex < length
    #    a. Let next be codePoints[nextIndex].
    #    b. Let nextCP be ? ToNumber(next).
    #    c. If SameValue(nextCP, ToInteger(nextCP)) is false, throw a RangeError exception.
    #    d. If nextCP < 0 or nextCP > 0x10FFFF, throw a RangeError exception.
    #    e. Append the elements of the UTF16Encoding of nextCP to the end of elements.
    #    f. Let nextIndex be nextIndex + 1.
    # 6. Return the String value whose elements are, in order, the elements in the List elements. If length is 0, the
    #    empty string is returned.
    elements = []
    for next in codePoints:
        nextCP = ToNumber(next)
        if not SameValue(nextCP, ToInteger(nextCP)):
            raise ESRangeError('code points must be integers')
        if nextCP < 0 or nextCP > 0x10FFFF:
            raise ESRangeError('code points must be in the range 0x0..0x10ffff')
        elements.extend(utf_16_encoding(nextCP))
    return ''.join(chr(x) for x in elements)
    # The length property of the fromCodePoint function is 1.

# ------------------------------------ 𝟐𝟏.𝟏.𝟑 𝑷𝒓𝒐𝒑𝒆𝒓𝒕𝒊𝒆𝒔 𝒐𝒇 𝒕𝒉𝒆 𝑺𝒕𝒓𝒊𝒏𝒈 𝑷𝒓𝒐𝒕𝒐𝒕𝒚𝒑𝒆 𝑶𝒃𝒋𝒆𝒄𝒕 ------------------------------------
# 21.1.3 Properties of the String Prototype Object
# The String prototype object:
#
#   * is the intrinsic object %StringPrototype%.
#   * is a String exotic object and has the internal methods specified for such objects.
#   * has a [[StringData]] internal slot whose value is the empty String.
#   * has a length property whose initial value is 0 and whose attributes are { [[Writable]]: false,
#     [[Enumerable]]: false, [[Configurable]]: false }.
#   * has a [[Prototype]] internal slot whose value is the intrinsic object %ObjectPrototype%.
#
# Unless explicitly stated otherwise, the methods of the String prototype object defined below are not generic and the
# this value passed to them must be either a String value or an object that has a [[StringData]] internal slot that has
# been initialized to a String value.
def CreateStringPrototype(realm):
    string_prototype = StringCreate('', realm.intrinsics['%ObjectPrototype%'])
    BindBuiltinFunctions(realm, string_prototype, [
        ('toString', StringPrototype_toString, 0),
        ('valueOf', StringPrototype_valueOf, 0),
    ])
    return string_prototype

def thisStringValue(value):
    # The abstract operation thisStringValue(value) performs the following steps:
    #
    # 1. If Type(value) is String, return value.
    # 2. If Type(value) is Object and value has a [[StringData]] internal slot, then
    #    a. Assert: value.[[StringData]] is a String value.
    #    b. Return value.[[StringData]].
    # 3. Throw a TypeError exception.
    if isString(value):
        return value
    if isObject(value) and hasattr(value, 'StringData'):
        assert isString(value.StringData)
        return value.StringData
    raise ESTypeError('Not a string value')

# ------------------------------------ 𝟐𝟏.𝟏.𝟑.𝟐𝟓 𝑺𝒕𝒓𝒊𝒏𝒈.𝒑𝒓𝒐𝒕𝒐𝒕𝒚𝒑𝒆.𝒕𝒐𝑺𝒕𝒓𝒊𝒏𝒈 ( ) ------------------------------------
# 21.1.3.25 String.prototype.toString ( )
def StringPrototype_toString(this_value, new_target):
    # When the toString method is called, the following steps are taken:
    #
    # 1. Return ? thisStringValue(this value).
    # NOTE
    # For a String object, the toString method happens to return the same thing as the valueOf method.
    return thisStringValue(this_value)

# ------------------------------------ 𝟐𝟏.𝟏.𝟑.𝟐𝟖 𝑺𝒕𝒓𝒊𝒏𝒈.𝒑𝒓𝒐𝒕𝒐𝒕𝒚𝒑𝒆.𝒗𝒂𝒍𝒖𝒆𝑶𝒇 ( ) ------------------------------------
# 21.1.3.28 String.prototype.valueOf ( )
def StringPrototype_valueOf(this_value, new_target):
    # When the valueOf method is called, the following steps are taken:
    #
    # 1. Return ? thisStringValue(this value).
    return thisStringValue(this_value)

def StringFixups(realm):
    string_constructor = realm.intrinsics['%String%']
    string_prototype = realm.intrinsics['%StringPrototype%']
    proto_desc = PropertyDescriptor(value=string_prototype, writable=False, enumerable=False, configurable=False)
    DefinePropertyOrThrow(string_constructor, 'prototype', proto_desc)
    const_desc = PropertyDescriptor(value=string_constructor, writable=True, enumerable=False, configurable=True)
    DefinePropertyOrThrow(string_prototype, 'constructor', const_desc)
    return None

# ------------------------------------ 𝟐𝟐 𝑰𝒏𝒅𝒆𝒙𝒆𝒅 𝑪𝒐𝒍𝒍𝒆𝒄𝒕𝒊𝒐𝒏𝒔 ------------------------------------
# ------------------------------------ 𝟐𝟐.𝟏 𝑨𝒓𝒓𝒂𝒚 𝑶𝒃𝒋𝒆𝒄𝒕𝒔 ------------------------------------
########################################################################################################################################################
#
#  .d8888b.   .d8888b.       d888              d8888                                        .d88888b.  888         d8b                   888
# d88P  Y88b d88P  Y88b     d8888             d88888                                       d88P" "Y88b 888         Y8P                   888
#        888        888       888            d88P888                                       888     888 888                               888
#      .d88P      .d88P       888           d88P 888 888d888 888d888  8888b.  888  888     888     888 88888b.    8888  .d88b.   .d8888b 888888 .d8888b
#  .od888P"   .od888P"        888          d88P  888 888P"   888P"       "88b 888  888     888     888 888 "88b   "888 d8P  Y8b d88P"    888    88K
# d88P"      d88P"            888         d88P   888 888     888     .d888888 888  888     888     888 888  888    888 88888888 888      888    "Y8888b.
# 888"       888"       d8b   888        d8888888888 888     888     888  888 Y88b 888     Y88b. .d88P 888 d88P    888 Y8b.     Y88b.    Y88b.       X88
# 888888888  888888888  Y8P 8888888     d88P     888 888     888     "Y888888  "Y88888      "Y88888P"  88888P"     888  "Y8888   "Y8888P  "Y888  88888P'
#                                                                                  888                             888
#                                                                             Y8b d88P                            d88P
#                                                                              "Y88P"                           888P"
#
########################################################################################################################################################
# Array objects are exotic objects that give special treatment to a certain class of property names. See 9.4.2 for a
# definition of this special treatment.
# ------------------------------------ 𝟐𝟐.𝟏.𝟏 𝑻𝒉𝒆 𝑨𝒓𝒓𝒂𝒚 𝑪𝒐𝒏𝒔𝒕𝒓𝒖𝒄𝒕𝒐𝒓 ------------------------------------
# 22.1.1 The Array Constructor
# The Array constructor:
#
#   * is the intrinsic object %Array%.
#   * is the initial value of the Array property of the global object.
#   * creates and initializes a new Array exotic object when called as a constructor.
#   * also creates and initializes a new Array object when called as a function rather than as a constructor. Thus the
#     function call Array(…) is equivalent to the object creation expression new Array(…) with the same arguments.
#   * is a single function whose behaviour is overloaded based upon the number and types of its arguments.
#   * is designed to be subclassable. It may be used as the value of an extends clause of a class definition. Subclass
#     constructors that intend to inherit the exotic Array behaviour must include a super call to the Array constructor
#     to initialize subclass instances that are Array exotic objects. However, most of the Array.prototype methods are
#     generic methods that are not dependent upon their this value being an Array exotic object.
#   * has a length property whose value is 1.
#
def CreateArrayConstructor(realm):
    obj = CreateBuiltinFunction(ArrayFunction, ['Construct'], realm=realm)
    for key, value in [('length', 1), ('name', 'Array')]:
        desc = PropertyDescriptor(value=value, writable=False, enumerable=False, configurable=True)
        DefinePropertyOrThrow(obj, key, desc)
    BindBuiltinFunctions(realm, obj, [
        ('from', Array_from, 1),
        #('isArray', Array_isArray, 1),
        #('of', Array_of, 0),
    ])
    def get_species(this_value, new_target):
        return this_value
    fcn_obj = CreateBuiltinFunction(get_species, [], realm)
    DefinePropertyOrThrow(fcn_obj, 'length', PropertyDescriptor(value=0, writable=False, enumerable=False, configurable=True))
    DefinePropertyOrThrow(fcn_obj, 'name', PropertyDescriptor(value='get [Symbol.species]', writable=False, enumerable=False, configurable=True))
    DefinePropertyOrThrow(obj, wks_species, PropertyDescriptor(Get=fcn_obj, enumerable=False, configurable=True))

    return obj

# ------------------------------------ 𝟐𝟐.𝟏.𝟏.𝟏 𝑨𝒓𝒓𝒂𝒚 ( ) ------------------------------------
# 22.1.1.1 Array ( )
def ArrayFunction_no_args(this_value, new_target):
    # This description applies if and only if the Array constructor is called with no arguments.
    #
    #   1. Let numberOfArgs be the number of arguments passed to this function call.
    #   2. Assert: numberOfArgs = 0.
    #   3. If NewTarget is undefined, let newTarget be the active function object, else let newTarget be NewTarget.
    #   4. Let proto be ? GetPrototypeFromConstructor(newTarget, "%ArrayPrototype%").
    #   5. Return ! ArrayCreate(0, proto).
    newTarget = new_target or GetActiveFunction()
    proto = GetPrototypeFromConstructor(newTarget, '%ArrayPrototype%')
    return ArrayCreate(0, proto)

# ------------------------------------ 𝟐𝟐.𝟏.𝟏.𝟐 𝑨𝒓𝒓𝒂𝒚 ( 𝒍𝒆𝒏 ) ------------------------------------
# 22.1.1.2 Array ( len )
def ArrayFunction_one_arg(this_value, new_target, length):
    # This description applies if and only if the Array constructor is called with exactly one argument.
    #
    #   1. Let numberOfArgs be the number of arguments passed to this function call.
    #   2. Assert: numberOfArgs = 1.
    #   3. If NewTarget is undefined, let newTarget be the active function object, else let newTarget be NewTarget.
    #   4. Let proto be ? GetPrototypeFromConstructor(newTarget, "%ArrayPrototype%").
    #   5. Let array be ! ArrayCreate(0, proto).
    #   6. If Type(len) is not Number, then
    #       a. Let defineStatus be CreateDataProperty(array, "0", len).
    #       b. Assert: defineStatus is true.
    #       c. Let intLen be 1.
    #   7. Else,
    #       a. Let intLen be ToUint32(len).
    #       b. If intLen ≠ len, throw a RangeError exception.
    #   8. Perform ! Set(array, "length", intLen, true).
    #   9. Return array.
    newTarget = new_target or GetActiveFunction()
    proto = GetPrototypeFromConstructor(newTarget, '%ArrayPrototype%')
    array = ArrayCreate(0, proto)
    if not isNumber(length):
        defineStatus = CreateDataProperty(array, '0', length)
        assert defineStatus
        intLen = 1
    else:
        intLen = ToUint32(length)
        if intLen != length:
            raise ESRangeError(f'Invalid array length: {length}')
    Set(array, 'length', intLen, True)
    return array

# ------------------------------------ 𝟐𝟐.𝟏.𝟏.𝟑 𝑨𝒓𝒓𝒂𝒚 ( ...𝒊𝒕𝒆𝒎𝒔 ) ------------------------------------
# 22.1.1.3 Array ( ...items )
def ArrayFunction(this_value, new_target, *items):
    # This description applies if and only if the Array constructor is called with at least two arguments.
    #
    # When the Array function is called, the following steps are taken:
    #
    #   1. Let numberOfArgs be the number of arguments passed to this function call.
    #   2. Assert: numberOfArgs ≥ 2.
    #   3. If NewTarget is undefined, let newTarget be the active function object, else let newTarget be NewTarget.
    #   4. Let proto be ? GetPrototypeFromConstructor(newTarget, "%ArrayPrototype%").
    #   5. Let array be ? ArrayCreate(numberOfArgs, proto).
    #   6. Let k be 0.
    #   7. Let items be a zero-origined List containing the argument items in order.
    #   8. Repeat, while k < numberOfArgs
    #       a. Let Pk be ! ToString(k).
    #       b. Let itemK be items[k].
    #       c. Let defineStatus be CreateDataProperty(array, Pk, itemK).
    #       d. Assert: defineStatus is true.
    #       e. Increase k by 1.
    #   9. Assert: The value of array's length property is numberOfArgs.
    #   10. Return array.
    numberOfArgs = len(items)
    if numberOfArgs == 0:
        return ArrayFunction_no_args(this_value, new_target)
    if numberOfArgs == 1:
        return ArrayFunction_one_arg(this_value, new_target, items[0])
    newTarget = new_target or GetActiveFunction()
    proto = GetPrototypeFromConstructor(newTarget, '%ArrayPrototype%')
    array = ArrayCreate(numberOfArgs, proto)
    for k, itemK in enumerate(items):
        Pk = ToString(k)
        defineStatus = CreateDataProperty(array, Pk, itemK)
        assert defineStatus
    assert Get(array, 'length') == numberOfArgs
    return array

# 22.1.2 Properties of the Array Constructor
# The Array constructor:
#
#   * has a [[Prototype]] internal slot whose value is the intrinsic object %FunctionPrototype%.
#   * has the following properties:

# ------------------------------------ 𝟐𝟐.𝟏.𝟐.𝟏 𝑨𝒓𝒓𝒂𝒚.𝒇𝒓𝒐𝒎 ( 𝒊𝒕𝒆𝒎𝒔 [ , 𝒎𝒂𝒑𝒇𝒏 [ , 𝒕𝒉𝒊𝒔𝑨𝒓𝒈 ] ] ) ------------------------------------
# 22.1.2.1 Array.from ( items [ , mapfn [ , thisArg ] ] )
def Array_from(this_value, new_target, items, mapfn=EMPTY, thisArg=EMPTY):
    # When the from method is called with argument items and optional arguments mapfn and thisArg, the following steps
    # are taken:
    #
    #   1. Let C be the this value.
    #   2. If mapfn is undefined, let mapping be false.
    #   3. Else,
    #       a. If IsCallable(mapfn) is false, throw a TypeError exception.
    #       b. If thisArg is present, let T be thisArg; else let T be undefined.
    #       c. Let mapping be true.
    #   4. Let usingIterator be ? GetMethod(items, @@iterator).
    #   5. If usingIterator is not undefined, then
    #       a. If IsConstructor(C) is true, then
    #           i. Let A be ? Construct(C).
    #       b. Else,
    #           i. Let A be ! ArrayCreate(0).
    #       c. Let iteratorRecord be ? GetIterator(items, sync, usingIterator).
    #       d. Let k be 0.
    #       e. Repeat,
    #           i. If k ≥ 2^53-1, then
    #               1. Let error be ThrowCompletion(a newly created TypeError object).
    #               2. Return ? IteratorClose(iteratorRecord, error).
    #           ii. Let Pk be ! ToString(k).
    #           iii. Let next be ? IteratorStep(iteratorRecord).
    #           iv. If next is false, then
    #               1. Perform ? Set(A, "length", k, true).
    #               2. Return A.
    #           v. Let nextValue be ? IteratorValue(next).
    #           vi. If mapping is true, then
    #               1. Let mappedValue be Call(mapfn, T, « nextValue, k »).
    #               2. If mappedValue is an abrupt completion, return ? IteratorClose(iteratorRecord, mappedValue).
    #               3. Let mappedValue be mappedValue.[[Value]].
    #           vii. Else, let mappedValue be nextValue.
    #           viii. Let defineStatus be CreateDataPropertyOrThrow(A, Pk, mappedValue).
    #           ix. If defineStatus is an abrupt completion, return ? IteratorClose(iteratorRecord, defineStatus).
    #           x. Increase k by 1.
    #   6. NOTE: items is not an Iterable so assume it is an array-like object.
    #   7. Let arrayLike be ! ToObject(items).
    #   8. Let len be ? ToLength(? Get(arrayLike, "length")).
    #   9. If IsConstructor(C) is true, then
    #       a. Let A be ? Construct(C, « len »).
    #   10. Else,
    #       a. Let A be ? ArrayCreate(len).
    #   11. Let k be 0.
    #   12. Repeat, while k < len
    #       a. Let Pk be ! ToString(k).
    #       b. Let kValue be ? Get(arrayLike, Pk).
    #       c. If mapping is true, then
    #           i. Let mappedValue be ? Call(mapfn, T, « kValue, k »).
    #       d. Else, let mappedValue be kValue.
    #       e. Perform ? CreateDataPropertyOrThrow(A, Pk, mappedValue).
    #       f. Increase k by 1.
    #   13. Perform ? Set(A, "length", len, true).
    #   14. Return A.
    # NOTE
    # The from function is an intentionally generic factory method; it does not require that its this value be the
    # Array constructor. Therefore it can be transferred to or inherited by any other constructors that may be called
    # with a single numeric argument.
    raise NotImplementedError # This wants iterators. @@@ I'm not there yet.

# 22.1.3 Properties of the Array Prototype Object
# The Array prototype object:
#
#   * is the intrinsic object %ArrayPrototype%.
#   * is an Array exotic object and has the internal methods specified for such objects.
#   * has a length property whose initial value is 0 and whose attributes are
#     { [[Writable]]: true, [[Enumerable]]: false, [[Configurable]]: false }.
#   * has a [[Prototype]] internal slot whose value is the intrinsic object %ObjectPrototype%.
# NOTE
# The Array prototype object is specified to be an Array exotic object to ensure compatibility with ECMAScript code
# that was created prior to the ECMAScript 2015 specification.

def CreateArrayPrototype(realm):
    proto = ArrayCreate(0, realm.intrinsics['%ObjectPrototype%'])
    BindBuiltinFunctions(realm, proto, [
        ('toString', ArrayPrototype_toString, 0),
        ('join', ArrayPrototype_join, 1),
        ('values', ArrayPrototype_values, 0),
    ])
    Set(proto, wks_iterator, Get(proto, 'values'), True)
    return proto

# 22.1.3.13 Array.prototype.join ( separator )
def ArrayPrototype_join(this_value, new_target, separator=','):
    # NOTE 1
    # The elements of the array are converted to Strings, and these Strings are then concatenated, separated by
    # occurrences of the separator. If no separator is provided, a single comma is used as the separator.
    #
    # The join method takes one argument, separator, and performs the following steps:
    #
    #   1. Let O be ? ToObject(this value).
    #   2. Let len be ? ToLength(? Get(O, "length")).
    #   3. If separator is undefined, let sep be the single-element String ",".
    #   4. Else, let sep be ? ToString(separator).
    #   5. Let R be the empty String.
    #   6. Let k be 0.
    #   7. Repeat, while k < len
    #       a. If k > 0, let R be the string-concatenation of R and sep.
    #       b. Let element be ? Get(O, ! ToString(k)).
    #       c. If element is undefined or null, let next be the empty String; otherwise, let next be
    #          ? ToString(element).
    #       d. Set R to the string-concatenation of R and next.
    #       e. Increase k by 1.
    #   8. Return R.
    # NOTE 2
    # The join function is intentionally generic; it does not require that its this value be an Array object.
    # Therefore, it can be transferred to other kinds of objects for use as a method.
    O = ToObject(this_value)
    length = ToLength(Get(O, 'length'))
    sep = ToString(separator)
    return sep.join(ToString(element) if not (isUndefined(element) or isNull(element)) else '' for element in (Get(O, ToString(k)) for k in range(length)))

# 22.1.3.28 Array.prototype.toString ( )
def ArrayPrototype_toString(this_value, new_target):
    # When the toString method is called, the following steps are taken:
    #
    #   1. Let array be ? ToObject(this value).
    #   2. Let func be ? Get(array, "join").
    #   3. If IsCallable(func) is false, let func be the intrinsic function %ObjProto_toString%.
    #   4. Return ? Call(func, array).
    # NOTE
    # The toString function is intentionally generic; it does not require that its this value be an Array object.
    # Therefore it can be transferred to other kinds of objects for use as a method.
    array = ToObject(this_value)
    func = Get(array, 'join')
    if not IsCallable(func):
        func = surrounding_agent.running_ec.realm.intrinsics['%ObjProto_toString%']
    return Call(func, array)

# 22.1.3.30 Array.prototype.values ( )
def ArrayPrototype_values(this_value, new_target):
    # The following steps are taken:
    #
    #   1. Let O be ? ToObject(this value).
    #   2. Return CreateArrayIterator(O, "value").
    O = ToObject(this_value)
    return CreateArrayIterator(O, 'value')
    # This function is the %ArrayProto_values% intrinsic object.

def ArrayFixups(realm):
    array_constructor = realm.intrinsics['%Array%']
    array_prototype = realm.intrinsics['%ArrayPrototype%']
    proto_desc = PropertyDescriptor(value=array_prototype, writable=False, enumerable=False, configurable=False)
    DefinePropertyOrThrow(array_constructor, 'prototype', proto_desc)
    const_desc = PropertyDescriptor(value=array_constructor, writable=True, enumerable=False, configurable=True)
    DefinePropertyOrThrow(array_prototype, 'constructor', const_desc)
    realm.intrinsics['%ArrayProto_values%'] = Get(array_prototype, 'values')

# 22.1.5 Array Iterator Objects
# An Array Iterator is an object, that represents a specific iteration over some specific Array instance object. There
# is not a named constructor for Array Iterator objects. Instead, Array iterator objects are created by calling certain
# methods of Array instance objects.

# 22.1.5.1 CreateArrayIterator ( array, kind )
def CreateArrayIterator(array, kind):
    # Several methods of Array objects return Iterator objects. The abstract operation CreateArrayIterator with
    # arguments array and kind is used to create such iterator objects. It performs the following steps:
    #
    #   1. Assert: Type(array) is Object.
    #   2. Let iterator be ObjectCreate(%ArrayIteratorPrototype%, « [[IteratedObject]],
    #      [[ArrayIteratorNextIndex]], [[ArrayIterationKind]] »).
    #   3. Set iterator.[[IteratedObject]] to array.
    #   4. Set iterator.[[ArrayIteratorNextIndex]] to 0.
    #   5. Set iterator.[[ArrayIterationKind]] to kind.
    #   6. Return iterator.
    assert isObject(array)
    iterator = ObjectCreate(surrounding_agent.running_ec.realm.intrinsics['%ArrayIteratorPrototype%'], ['IteratedObject', 'ArrayIteratorNextIndex', 'ArrayIterationKind'])
    iterator.IteratedObject = array
    iterator.ArrayIteratorNextIndex = 0
    iterator.ArrayIterationKind = kind
    return iterator

# 22.1.5.2 The %ArrayIteratorPrototype% Object
# The %ArrayIteratorPrototype% object:
#
#   * has properties that are inherited by all Array Iterator Objects.
#   * is an ordinary object.
#   * has a [[Prototype]] internal slot whose value is the intrinsic object %IteratorPrototype%.
#   * has the following properties:

def CreateArrayIteratorPrototype(realm):
    proto = ObjectCreate(realm.intrinsics['%IteratorPrototype%'])
    BindBuiltinFunctions(realm, proto, [
        ('next', ArrayIteratorPrototype_next, 0),
    ])
    # 22.1.5.2.2 %ArrayIteratorPrototype% [ @@toStringTag ]
    # The initial value of the @@toStringTag property is the String value "Array Iterator".
    # This property has the attributes { [[Writable]]: false, [[Enumerable]]: false, [[Configurable]]: true }.
    pdesc = PropertyDescriptor(value='Array Iterator', writable=False, enumerable=False, configurable=True)
    DefinePropertyOrThrow(proto, wks_to_string_tag, pdesc)
    return proto

# 22.1.5.2.1 %ArrayIteratorPrototype%.next ( )
def ArrayIteratorPrototype_next(this_value, new_target):
    #   1. Let O be the this value.
    #   2. If Type(O) is not Object, throw a TypeError exception.
    #   3. If O does not have all of the internal slots of an Array Iterator Instance (22.1.5.3), throw a TypeError exception.
    #   4. Let a be O.[[IteratedObject]].
    #   5. If a is undefined, return CreateIterResultObject(undefined, true).
    #   6. Let index be O.[[ArrayIteratorNextIndex]].
    #   7. Let itemKind be O.[[ArrayIterationKind]].
    #   8. If a has a [[TypedArrayName]] internal slot, then
    #       a. If IsDetachedBuffer(a.[[ViewedArrayBuffer]]) is true, throw a TypeError exception.
    #       b. Let len be a.[[ArrayLength]].
    #   9. Else,
    #       a. Let len be ? ToLength(? Get(a, "length")).
    #   10. If index ≥ len, then
    #       a. Set O.[[IteratedObject]] to undefined.
    #       b. Return CreateIterResultObject(undefined, true).
    #   11. Set O.[[ArrayIteratorNextIndex]] to index+1.
    #   12. If itemKind is "key", return CreateIterResultObject(index, false).
    #   13. Let elementKey be ! ToString(index).
    #   14. Let elementValue be ? Get(a, elementKey).
    #   15. If itemKind is "value", let result be elementValue.
    #   16. Else,
    #       a. Assert: itemKind is "key+value".
    #       b. Let result be CreateArrayFromList(« index, elementValue »).
    #   17. Return CreateIterResultObject(result, false).
    O = this_value
    if not isObject(O):
        raise ESTypeError('next must be called as a method')
    if any(not hasattr(O, attr) for attr in ['IteratedObject', 'ArrayIteratorNextIndex', 'ArrayIterationKind']):
        raise ESTypeError('next must be a method of an array iterator object')
    a = O.IteratedObject
    if not a:
        return CreateIterResultObject(None, True)
    index = O.ArrayIteratorNextIndex
    itemKind = O.ArrayIterationKind
    if hasattr(a, 'TypedArrayName'):
        if IsDetachedBuffer(a.ViewedArrayBuffer):
            raise ESTypeError('next can\'t handle detached buffers')
        length = a.ArrayLength
    else:
        length = ToLength(Get(a, 'length'))
    if index >= length:
        O.IteratedObject = None
        return CreateIterResultObject(None, True)
    O.ArrayIteratorNextIndex = index + 1
    if itemKind == 'key':
        return CreateIterResultObject(index, False)
    elementKey = ToString(index)
    elementValue = Get(a, elementKey)
    if itemKind == 'value':
        result = elementValue
    else:
        assert itemKind == 'key+value'
        result = CreateArrayFromList([index, elementValue])
    return CreateIterResultObject(result, False)

# 25.1.2 The %IteratorPrototype% Object
# The %IteratorPrototype% object:
#   * has a [[Prototype]] internal slot whose value is the intrinsic object %ObjectPrototype%.
#   * is an ordinary object.
def CreateIteratorPrototype(realm):
    proto = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    func_obj = CreateBuiltinFunction(IteratorPrototype_iterator, [], realm)
    DefinePropertyOrThrow(func_obj, 'length', PropertyDescriptor(value=0, writable=False, enumerable=False, configurable=True))
    DefinePropertyOrThrow(func_obj, 'name', PropertyDescriptor(value='[Symbol.iterator]', writable=False, enumerable=False, configurable=False))
    CreateMethodPropertyOrThrow(proto, wks_iterator, func_obj)
    return proto

# 25.1.2.1 %IteratorPrototype% [ @@iterator ] ( )
def IteratorPrototype_iterator(this_value, new_target):
    return this_value

#######################################################################################################################################################
if __name__ == '__main__':
    try:
        rv = RunJobs(scripts=["let colors = [ 'red', 'green', 'blue' ]; let [ firstColor, secondColor ] = colors; firstColor + '-' + secondColor;"])
    except ESError as err:
        InitializeHostDefinedRealm()
        print(err)
    else:
        InitializeHostDefinedRealm()
        print('Script returned %s' % ToString(rv))
    surrounding_agent.ec_stack.pop()
    surrounding_agent.running_ec = None

# Banners produced using font "Colossal" on https://www.messletters.com/en/big-text/

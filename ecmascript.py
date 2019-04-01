"""
Scole's ECMAScript 9 system
"""
from enum import Enum, unique, auto
from collections import namedtuple, deque
import re
import math
from itertools import chain
import unicodedata
import uuid


def CreateReferenceError():
    return ReferenceError() # This is a python object, not an ecmascript object. This will change when objects are turned on.
def CreateTypeError():
    return TypeError() # This is a python object, not an ecmascript object. This will change when objects are turned on.

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
        pass
    def __init__(self):
        self.Prototype = JSNull.NULL
        self.Extensible = False
        self.properties = {}

    # 9.1.1 [[GetPrototypeOf]] ( )
    def GetPrototypeOf(self):
        # 1. Return O.[[Prototype]].
        return NormalCompletion(self.Prototype)

    # 9.1.2 [[SetPrototypeOf]] ( V )
    def SetPrototypeOf(self, value):
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
            return NormalCompletion(True)
        # 5. If extensible is false, return false.
        if not extensible:
            return NormalCompletion(False)
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
                return NormalCompletion(False)
            # c. Else,
            else:
                # i. If p.[[GetPrototypeOf]] is not the ordinary object internal method defined in 9.1.1, set done to
                #    true.
                # There has to be a better way!
                match = re.match(r'<bound method of (.*) <[^>]+>>', str(p.GetPrototypeOf))
                if not (match and match.group(1) == 'JSObject.GetPrototypeOf'):
                    done = True
                # ii. Else, set p to p.[[Prototype]].
                else:
                    p = p.Prototype
        # 9. Set O.[[Prototype]] to V.
        self.Prototype = value
        # 10. Return true.
        return NormalCompletion(True)
        # NOTE
        # The loop in step 8 guarantees that there will be no circularities in any prototype chain that only includes
        # objects that use the ordinary object definitions for [[GetPrototypeOf]] and [[SetPrototypeOf]].

    # 9.1.3 [[IsExtensible]] ( )
    def IsExtensible(self):
        # 1. Return O.[[Extensible]].
        return NormalCompletion(self.Extensible)

    # 9.1.4 [[PreventExtensions]] ( )
    def PreventExtensions(self):
        # 1. Set O.[[Extensible]] to false.
        self.Extensible = False
        # 2. Return true.
        return NormalCompletion(True)

    # 9.1.5 [[GetOwnProperty]] ( P )
    def GetOwnProperty(self, propkey):
        # 1. Assert: IsPropertyKey(P) is true.
        assert IsPropertyKey(propkey)
        # 2. If O does not have an own property with key P, return undefined.
        if propkey not in self.properties:
            return NormalCompletion(None)
        # 3. Let D be a newly created Property Descriptor with no fields.
        desc = PropertyDescriptor()
        # 4. Let X be O's own property whose key is P.
        own = self.properties[propkey]
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
        return NormalCompletion(desc)

    # 9.1.6 [[DefineOwnProperty]] ( P, Desc )
    def DefineOwnProperty(self, propkey, desc):
        # 1. Let current be ? O.[[GetOwnProperty]](P).
        current, ok = ec(self.GetOwnProperty(propkey))
        if not ok:
            return current
        # 2. Let extensible be O.[[Extensible]].
        extensible = self.Extensible
        # 3. Return ValidateAndApplyPropertyDescriptor(O, P, extensible, Desc, current).
        return ValidateAndApplyPropertyDescriptor(self, propkey, extensible, desc, current)

    # 9.1.7 [[HasProperty]] ( P )
    def HasProperty(self, propkey):
        # 1. Assert: IsPropertyKey(P) is true.
        assert IsPropertyKey(propkey)
        # 2. Let hasOwn be ? O.[[GetOwnProperty]](P).
        has_own, ok = ec(self.GetOwnProperty(propkey))
        if not ok:
            return has_own
        # 3. If hasOwn is not undefined, return true.
        if has_ownW is not None:
            return NormalCompletion(True)
        # 4. Let parent be ? O.[[GetPrototypeOf]]().
        parent, ok = ec(self.GetPrototypeOf())
        if not ok:
            return parent
        # 5. If parent is not null, then
        #    a. Return ? parent.[[HasProperty]](P).
        # 6. Return false.
        if isNull(parent):
            return NormalCompletion(False)
        return parent.HasProperty(propkey)

    # 9.1.8 [[Get]] ( P, Receiver )
    def Get(self, propkey, receiver):
        # When the abstract operation OrdinaryGet is called with Object O, property key P, and ECMAScript language
        # value Receiver, the following steps are taken:
        #
        # 1. Assert: IsPropertyKey(P) is true.
        assert IsPropertyKey(propkey)
        # 2. Let desc be ? O.[[GetOwnProperty]](P).
        desc, ok = ec(self.GetOwnProperty(propkey))
        if not ok:
            return desc
        # 3. If desc is undefined, then
        if desc is None:
            # a. Let parent be ? O.[[GetPrototypeOf]]().
            parent, ok = ec(self.GetPrototypeOf())
            if not ok:
                return parent
            # b. If parent is null, return undefined.
            if isNull(parent):
                return NormalCompletion(None)
            # c. Return ? parent.[[Get]](P, Receiver).
            return parent.Get(propkey, receiver)
        # 4. If IsDataDescriptor(desc) is true, return desc.[[Value]].
        if desc.is_data_descriptor():
            return NormalCompletion(desc.value)
        # 5. Assert: IsAccessorDescriptor(desc) is true.
        assert desc.is_accessor_descriptor()
        # 6. Let getter be desc.[[Get]].
        getter = desc.Get
        # 7. If getter is undefined, return undefined.
        if getter is None:
            return NormalCompletion(None)
        # 8. Return ? Call(getter, Receiver).
        return Call(getter, receiver)

    # 9.1.9 [[Set]] ( P, V, Receiver )
    def Set(self, propkey, value, receiver):
        # When the [[Set]] internal method of O is called with property key P, value V, and ECMAScript language value
        # Receiver, the following steps are taken:
        #
        # 1. Return ? OrdinarySet(O, P, V, Receiver).
        return OrdinarySet(self, propkey, value, receiver)

    # 9.1.10 [[Delete]] ( P )
    def Delete(self, propkey):
        # When the [[Delete]] internal method of O is called with property key P, the following steps are taken:
        #
        # 1. Return ? OrdinaryDelete(O, P).
        return OrdinaryDelete(self, propkey)

    # 9.1.11 [[OwnPropertyKeys]] ( )
    def OwnPropertyKeys(self):
        # When the [[OwnPropertyKeys]] internal method of O is called, the following steps are taken:
        #
        # 1. Return ! OrdinaryOwnPropertyKeys(O).
        return nc(OrdinaryOwnPropertyKeys(self))

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
            return NormalCompletion(False)
        # b. Assert: extensible is true.
        # c. If IsGenericDescriptor(Desc) is true or IsDataDescriptor(Desc) is true, then
        if IsDataDescriptor(desc) or IsGenericDescriptor(desc):
            # i. If O is not undefined, create an own data property named P of object O whose [[Value]], [[Writable]],
            #    [[Enumerable]] and [[Configurable]] attribute values are described by Desc. If the value of an
            #    attribute field of Desc is absent, the attribute of the newly created property is set to its default
            #    value.
            if obj is not None:
                newprop = JSObject.Property()
                newprop.value = desc.value if hasattr(desc, 'value') else None
                newprop.writable = desc.writable if hasattr(desc, 'writable') else False
                newprop.enumerable = desc.enumerable if hasattr(desc, 'enumerable') else False
                newprop.configurable = desc.configurable if hasattr(desc, 'configurable') else False
                obj.properties[propkey] = newprop
        # d. Else Desc must be an accessor Property Descriptor,
        else:
            # i. If O is not undefined, create an own accessor property named P of object O whose [[Get]], [[Set]],
            #    [[Enumerable]] and [[Configurable]] attribute values are described by Desc. If the value of an
            #    attribute field of Desc is absent, the attribute of the newly created property is set to its default
            #    value.
            if obj is not None:
                newprop = JSObject.Property()
                newprop.Get = desc.Get if hasattr(desc, 'Get') else None
                newprop.Set = desc.Set if hasattr(desc, 'Set') else None
                newprop.enumerable = desc.enumerable if hasattr(desc, 'enumerable') else False
                newprop.configurable = desc.configurable if hasattr(desc, 'configurable') else False
                obj.properties[propkey] = newprop
        # e. Return true.
        return NormalCompletion(True)
    # 3. If every field in Desc is absent, return true.
    if not any(hasattr(field, desc) for field in ['value', 'writable', 'Get', 'Set', 'enumerable', 'configurable']):
        return NormalCompletion(True)
    # 4. If current.[[Configurable]] is false, then
    if not current.configurable:
        # a. If Desc.[[Configurable]] is present and its value is true, return false.
        if hasattr(desc, 'configurable') and desc.configurable:
            return NormalCompletion(False)
        # b. If Desc.[[Enumerable]] is present and the [[Enumerable]] fields of current and Desc are the Boolean
        #    negation of each other, return false.
        if hasattr(desc, 'enumerable') and desc.enumerable != current.enumerable:
            return NormalCompletion(False)
    # 5. If IsGenericDescriptor(Desc) is true, no further validation is required.
    if IsGenericDescriptor(desc):
        pass
    # 6. Else if IsDataDescriptor(current) and IsDataDescriptor(Desc) have different results, then
    elif IsDataDescriptor(current) != IsDataDescriptor(desc):
        # a. If current.[[Configurable]] is false, return false.
        if not current.configurable:
            return NormalCompletion(False)
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
                return NormalCompletion(False)
            # ii. If Desc.[[Value]] is present and SameValue(Desc.[[Value]], current.[[Value]]) is false, return false.
            if hasattr(desc, 'value') and not SameValue(desc.value, current.value):
                return NormalCompletion(False)
            # iii. Return true.
            return NormalCompletion(True)
    # 8. Else IsAccessorDescriptor(current) and IsAccessorDescriptor(Desc) are both true,
    else:
        # a. If current.[[Configurable]] is false, then
        if not current.configurable:
            # i. If Desc.[[Set]] is present and SameValue(Desc.[[Set]], current.[[Set]]) is false, return false.
            if hasattr(desc, 'Set') and not SameValue(desc.Set, current.Set):
                return NormalCompletion(False)
            # ii. If Desc.[[Get]] is present and SameValue(Desc.[[Get]], current.[[Get]]) is false, return false.
            if hasattr(desc, 'Get') and not SameValue(desc.Get, current.Get):
                return NormalCompletion(False)
            # iii. Return true.
            return NormalCompletion(True)
    # 9. If O is not undefined, then
    if obj is not None:
        # a. For each field of Desc that is present, set the corresponding attribute of the property named P of object
        #    O to the value of the field.
        for fieldname in (f for f in ['value', 'writable', 'Get', 'Set', 'configurable', 'enumerable'] if hasattr(desc, f)):
            setattr(obj.properties[propkey], fieldname, getattr(desc, fieldname))
    # 10. Return true.
    return NormalCompletion(True)

# 9.1.9.1 OrdinarySet ( O, P, V, Receiver )
def OrdinarySet(obj, propkey, value, receiver):
    # When the abstract operation OrdinarySet is called with Object O, property key P, value V, and ECMAScript language
    # value Receiver, the following steps are taken:
    #
    # 1. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(propkey)
    # 2. Let ownDesc be ? O.[[GetOwnProperty]](P).
    own_desc, ok = ec(obj.GetOwnProperty(propkey))
    if not ok:
        return own_desc
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
        parent, ok = ec(obj.GetPrototypeOf())
        if not ok:
            return parent
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
            return NormalCompletion(False)
        # b. If Type(Receiver) is not Object, return false.
        if not isObject(receiver):
            return NormalCompletion(False)
        # c. Let existingDescriptor be ? Receiver.[[GetOwnProperty]](P).
        existing_descriptor, ok = ec(receiver.GetOwnProperty(propkey))
        if not ok:
            return existing_descriptor
        # d. If existingDescriptor is not undefined, then
        if existing_descriptor is not None:
            # i. If IsAccessorDescriptor(existingDescriptor) is true, return false.
            if existing_descriptor.is_accessor_descriptor():
                return NormalCompletion(False)
            # ii. If existingDescriptor.[[Writable]] is false, return false.
            if not existing_descriptor.writable:
                return NormalCompletion(False)
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
        return NormalCompletion(False)
    # 7. Perform ? Call(setter, Receiver, « V »).
    result, ok = ec(Call(setter, receiver, [value]))
    if not ok:
        return result
    # 8. Return true.
    return NormalCompletion(True)

# 9.1.10.1 OrdinaryDelete ( O, P )
def OrdinaryDelete(obj, propkey):
    # When the abstract operation OrdinaryDelete is called with Object O and property key P, the following steps are
    # taken:
    #
    # 1. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(propkey)
    # 2. Let desc be ? O.[[GetOwnProperty]](P).
    desc, ok = ec(obj.GetOwnProperty(propkey))
    if not ok:
        return desc
    # 3. If desc is undefined, return true.
    if desc is None:
        return NormalCompletion(True)
    # 4. If desc.[[Configurable]] is true, then
    if desc.configurable:
        # a. Remove the own property with name P from O.
        del obj.properties[propkey]
        # b. Return true.
        return NormalCompletion(True)
    # 5. Return false.
    return NormalCompletion(False)

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
    sort(index_keys, key=lambda k: CanonicalNumericIndexString(k))
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
    # The abstract operation OrdinaryCreateFromConstructor creates an ordinary object whose [[Prototype]] value is
    # retrieved from a constructor's prototype property, if it exists. Otherwise the intrinsic named by
    # intrinsicDefaultProto is used for [[Prototype]]. The optional internalSlotsList is a List of the names of
    # additional internal slots that must be defined as part of the object. If the list is not provided, a new empty
    # List is used. This abstract operation performs the following steps:
    #
    # 1. Assert: intrinsicDefaultProto is a String value that is this specification's name of an intrinsic object. The
    #    corresponding object must be an intrinsic that is intended to be used as the [[Prototype]] value of an object.
    # 2. Let proto be ? GetPrototypeFromConstructor(constructor, intrinsicDefaultProto).
    proto, ok = ec(GetPrototypeFromConstructor(constructor, intrinsic_default_proto))
    if not ok:
        return proto
    # 3. Return ObjectCreate(proto, internalSlotsList).
    return NormalCompletion(ObjectCreate(proto, internal_slots_list))

# 9.1.14 GetPrototypeFromConstructor ( constructor, intrinsicDefaultProto )
def GetPrototypeFromConstructor(constructor, intrinsic_default_proto):
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
    proto, ok = ec(Get(constructor, 'prototype'))
    if not ok:
        return proto
    # 4. If Type(proto) is not Object, then
    if not isObject(proto):
        # a. Let realm be ? GetFunctionRealm(constructor).
        realm, ok = ec(GetFunctionRealm(constructor))
        if not ok:
            return realm
        # b. Set proto to realm's intrinsic object named intrinsicDefaultProto.
        proto = realm.intrinsics[intrinsic_default_proto]
    # 5. Return proto.
    return NormalCompletion(proto)
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

@unique
class Empty(Enum):
    EMPTY = auto()

@unique
class CompletionType(Enum):
    NORMAL = auto()
    BREAK = auto()
    CONTINUE = auto()
    RETURN = auto()
    THROW = auto()

Completion = namedtuple('Completion', ['ctype', 'value', 'target'])

# 6.2.3.2 NormalCompletion

def NormalCompletion(arg):
    # The abstract operation NormalCompletion with a single argument, such as:
    #
    # 1. Return NormalCompletion(argument).
    #
    # Is a shorthand that is defined as follows:
    #
    # 1. Return Completion { [[Type]]: normal, [[Value]]: argument, [[Target]]: empty }.
    return Completion(ctype=CompletionType.NORMAL, value=arg, target=None)

# 6.2.3.3 ThrowCompletion
def ThrowCompletion(arg):
    # The abstract operation ThrowCompletion with a single argument, such as:
    #
    # 1. Return ThrowCompletion(argument).
    #
    # Is a shorthand that is defined as follows:
    #
    # 1. Return Completion { [[Type]]: throw, [[Value]]: argument, [[Target]]: empty }.
    return Completion(ctype=CompletionType.THROW, value=arg, target=None)

# 6.3.2.4 UpdateEmpty
def UpdateEmpty(cr, value):
    # The abstract operation UpdateEmpty with arguments completionRecord and value performs the following steps:

    # 1. Assert: If completionRecord.[[Type]] is either return or throw, then completionRecord.[[Value]] is not empty.
    assert cr.value != Empty.EMPTY if cr.ctype in [CompletionType.RETURN, CompletionType.THROW] else True

    # 2. If completionRecord.[[Value]] is not empty, return Completion(completionRecord).
    if cr.value != Empty.EMPTY:
        return Completion(cr.ctype, cr.value, cr.target)

    # 3. Return Completion { [[Type]]: completionRecord.[[Type]], [[Value]]: value, [[Target]]: completionRecord.[[Target]] }.
    return Completion(cr.ctype, value, cr.target)

# 5.2.3.3 ReturnIfAbrupt
# So I'm adding a lot of boilerplate to each function to handle this, and I'd like to simplify that.
#
# Old Way:
# obj = ? someroutine()
#
# would be written
#
# obj = someroutine()
# if obj.ctype != CompletionType.NORMAL:
#    return obj
# obj = obh.value
#
# Which is a lot of lines. And I realize I'm not doing the isinstance check there, which I really should be.
#
# How about instead:
#
# obj, ok = ec(someroutine())
# if not ok:
#    return obj
#
# (ec shorthand for "error check")
def ec(val):
    """ErrorCheck:
       Check val for an abrupt completion, returning "not ok" if that's the case. Else unwrap the Completion and return
       the actual value. Just response with the value itself, if this isn't a completion record."""
    if isinstance(val, Completion):
        if val.ctype != CompletionType.NORMAL:
            return (val, False)
        val = val.value
    return (val, True)

def nc(val):
    """NeverCheck:
       Assert that if the val is a Completion Record, that the completion type is Normal. Then just return the value.
       """
    if isinstance(val, Completion):
        assert val.ctype == CompletionType.NORMAL
        val = val.value
    return val

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
    return IsBoolean(value.base) or IsString(value.base) or IsSymbol(value.base) or IsNumber(value.base)

# 6.2.4.5 IsPropertyReference ( V )
def IsPropertyReference(value):
    # 1. Assert: Type(V) is Reference.
    assert isinstance(value, Reference)
    # 2. If either the base value component of V is an Object or HasPrimitiveBase(V) is true, return true; otherwise
    #    return false.
    return IsObject(value.base) or HasPrimitiveBase(value)

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
    value, ok = ec(value)
    if not ok:
        return value
    # 2. If Type(V) is not Reference, return V.
    if not isinstance(value, Reference):
        return NormalCompletion(value)
    # 3. Let base be GetBase(V).
    base = GetBase(value)
    # 4. If IsUnresolvableReference(V) is true, throw a ReferenceError exception.
    if IsUnresolvableReference(value):
        return ThrowCompletion(CreateReferenceError())
    # 5. If IsPropertyReference(V) is true, then
    if IsPropertyReference(value):
        # a. If HasPrimitiveBase(V) is true, then
        if HasPrimitiveBase(value):
            # i. Assert: In this case, base will never be undefined or null.
            assert base is not None and not IsNull(base)
            # ii. Set base to ! ToObject(base).
            base = nc(ToObject(base))
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
    ref, ok = ec(ref)
    if not ok:
        return ref
    # 2. ReturnIfAbrupt(W).
    value, ok = ec(ref)
    if not ok:
        return ref
    # 3. If Type(V) is not Reference, throw a ReferenceError exception.
    if not isinstance(ref, Reference):
        return ThrowCompletion(CreateReferenceError())
    # 4. Let base be GetBase(V).
    base = GetBase(ref)
    # 5. If IsUnresolvableReference(V) is true, then
    if IsUnresolvableReference(ref):
        # a. If IsStrictReference(V) is true, then
        if IsStrictReference(ref):
            # i. Throw a ReferenceError exception.
            return ThrowCompletion(CreateReferenceError())
        # b. Let globalObj be GetGlobalObject().
        global_obj = GetGlobalObject()
        # c. Return ? Set(globalObj, GetReferencedName(V), W, false).
        return Set(global_obj, GetReferencedName(ref), value, False)
    # 6. Else if IsPropertyReference(V) is true, then
    elif IsPropertyReference(ref):
        # a. If HasPrimitiveBase(V) is true, then
        if HasPrimitiveBase(ref):
            # i. Assert: In this case, base will never be undefined or null.
            assert base is not None and not IsNull(base)
            # ii. Set base to ! ToObject(base).
            base = nc(ToObject(base))
        # b. Let succeeded be ? base.[[Set]](GetReferencedName(V), W, GetThisValue(V)).
        succeeded, ok = ec(base.Set(GetReferencedName(ref), value, GetThisValue(ref)))
        if not ok:
            return succeeded
        # c. If succeeded is false and IsStrictReference(V) is true, throw a TypeError exception.
        if not succeeded and IsStrictReference(ref):
            return ThrowCompletion(CreateTypeError())
        # d. Return.
        return NormalCompletion(None)
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
    ref, ok = ec(ref)
    if not ok:
        return ref
    # 2. ReturnIfAbrupt(W).
    value, ok = ec(value)
    if not ok:
        return value
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

class PropertyDescriptor:
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
    obj = ObjectCreate(intrinsics.ObjectPrototype)
    # 3. Assert: obj is an extensible ordinary object with no own properties.
    # 4. If Desc has a [[Value]] field, then
    crs = []
    if hasattr(desc, 'value'):
        # a. Perform CreateDataProperty(obj, "value", Desc.[[Value]]).
        cr = CreateDataProperty(obj, 'value', desc.value)
        crs.append(cr)
    # 5. If Desc has a [[Writable]] field, then
    if hasattr(desc, 'writable'):
        # a. Perform CreateDataProperty(obj, "writable", Desc.[[Writable]]).
        cr = CreateDataProperty(obj, 'writable', desc.writable)
        crs.append(cr)
    # 6. If Desc has a [[Get]] field, then
    if hasattr(desc, 'Get'):
        # a. Perform CreateDataProperty(obj, "get", Desc.[[Get]]).
        cr = CreateDataProperty(obj, 'get', desc.Get)
        crs.append(cr)
    # 7. If Desc has a [[Set]] field, then
    if hasattr(desc, 'Set'):
        # a. Perform CreateDataProperty(obj, "set", Desc.[[Set]]).
        cr = CreateDataProperty(obj, 'set', desc.Set)
        crs.append(cr)
    # 8. If Desc has an [[Enumerable]] field, then
    if hasattr(desc, 'enumerable'):
        # a. Perform CreateDataProperty(obj, "enumerable", Desc.[[Enumerable]]).
        cr = CreateDataProperty(obj, 'enumerable', desc.enumerable)
        crs.append(cr)
    # 9. If Desc has a [[Configurable]] field, then
    if hasattr(desc, 'configurable'):
        # a. Perform CreateDataProperty(obj, "configurable", Desc.[[Configurable]]).
        cr = CreateDataProperty(obj, 'configurable', desc.configurable)
        crs.append(cr)
    # 10. Assert: All of the above CreateDataProperty operations return true.
    assert all(cr.ctype == CompletionType.NORMAL and cr.value for cr in crs)
    # 11. Return obj.
    return obj

# 6.2.5.5 ToPropertyDescriptor ( Obj )
def ToPropertyDescriptor(obj):
    # When the abstract operation ToPropertyDescriptor is called with object Obj, the following steps are taken:
    #
    # 1. If Type(Obj) is not Object, throw a TypeError exception.
    if not isObject(obj):
        return ThrowCompletion(CreateTypeError())
    # 2. Let desc be a new Property Descriptor that initially has no fields.
    desc = PropertyDescriptor()
    # 3. Let hasEnumerable be ? HasProperty(Obj, "enumerable").
    has_enumerable, ok = ec(HasProperty(obj, 'enumerable'))
    if not ok:
        return has_enumerable
    # 4. If hasEnumerable is true, then
    if has_enumerable:
        # a. Let enum be ToBoolean(? Get(Obj, "enumerable")).
        enumble, ok = ec(Get(obj, 'enumerable'))
        if not ok:
            return enumble
        # b. Set desc.[[Enumerable]] to enum.
        desc.enumerable = ToBoolean(enumble.value)
    # 5. Let hasConfigurable be ? HasProperty(Obj, "configurable").
    has_configurable, ok = ec(HasProperty(obj, 'configurable'))
    if not ok:
        return has_configurable
    # 6. If hasConfigurable is true, then
    if has_configurable:
        # a. Let conf be ToBoolean(? Get(Obj, "configurable")).
        conf, ok = ec(Get(obj, 'configurable'))
        if not ok:
            return conf
        # b. Set desc.[[Configurable]] to conf.
        desc.configurable = ToBoolean(conf)
    # 7. Let hasValue be ? HasProperty(Obj, "value").
    has_value, ok = ec(HasProperty(obj, 'value'))
    if not ok:
        return has_value
    # 8. If hasValue is true, then
    if has_value:
        # a. Let value be ? Get(Obj, "value").
        value, ok = ec(Get(obj, 'value'))
        if not ok:
            return value
        # b. Set desc.[[Value]] to value.
        desc.value = value
    # 9. Let hasWritable be ? HasProperty(Obj, "writable").
    has_writable, ok = ec(HasProperty(obj, 'writable'))
    if not ok:
        return has_writable
    # 10. If hasWritable is true, then
    if has_writable:
        # a. Let writable be ToBoolean(? Get(Obj, "writable")).
        writable, ok = ec(Get(obj, 'writable'))
        if not ok:
            return writable
        # b. Set desc.[[Writable]] to writable.
        desc.writable = ToBoolean(writable)
    # 11. Let hasGet be ? HasProperty(Obj, "get").
    has_get, ok = ec(HasProperty(obj, 'get'))
    if not ok:
        return has_get
    # 12. If hasGet is true, then
    if has_get:
        # a. Let getter be ? Get(Obj, "get").
        getter, ok = ec(Get(obj, 'get'))
        if not ok:
            return getter
        # b. If IsCallable(getter) is false and getter is not undefined, throw a TypeError exception.
        if not IsCallable(getter) and getter is not None:
            return ThrowCompletion(CreateTypeError())
        # c. Set desc.[[Get]] to getter.
        desc.Get = getter
    # 13. Let hasSet be ? HasProperty(Obj, "set").
    has_set, ok = ec(HasProperty(obj, 'set'))
    if not ok:
        return has_set
    # 14. If hasSet is true, then
    if has_set:
        # a. Let setter be ? Get(Obj, "set").
        setter, ok = ec(Get(obj, 'set'))
        if not ok:
            return setter
        # b. If IsCallable(setter) is false and setter is not undefined, throw a TypeError exception.
        if not IsCallable(setter) and setter is not None:
            return ThrowCompletion(CreateTypeError())
        # c. Set desc.[[Set]] to setter.
        desc.Set = setter
    # 15. If desc.[[Get]] is present or desc.[[Set]] is present, then
    if hasattr(desc, 'Get') or hasattr(desc, 'Set'):
        # a. If desc.[[Value]] is present or desc.[[Writable]] is present, throw a TypeError exception.
        if hasattr(desc, 'value') or hasattr(desc, 'writable'):
            return ThrowCompletion(CreateTypeError())
    # 16. Return desc.
    return NormalCompletion(desc)

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
# These operations are not a part of the ECMAScript language; they are defined here to solely to aid the specification of
# the semantics of the ECMAScript language. Other, more specialized abstract operations are defined throughout this
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
        exotic_to_prim, ok = ec(GetMethod(input, wks_to_primitive))
        if not ok:
            return exotic_to_prim
        # e. If exoticToPrim is not undefined, then
        if exotic_to_prim is not None:
            # i. Let result be ? Call(exoticToPrim, input, � hint �).
            result, ok = ec(Call(exotic_to_prim, input, [ preferred_type ]))
            if not ok:
                return result
            # ii. If Type(result) is not Object, return result.
            if not isObject(result):
                return NormalCompletion(result)
            # iii. Throw a TypeError exception.
            return ThrowCompletion(CreateTypeError())
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
        # a. Let methodNames be [ "toString", "valueOf" ].
        method_names = [ 'toString', 'valueOf' ]
    # 4. Else,
    else:
        # a. Let methodNames be [ "valueOf", "toString" ].
        method_names = ['valueOf', 'toString']
    # 5. For each name in methodNames in List order, do
    for name in method_names:
        # a. Let method be ? Get(O, name).
        method, ok = ec(Get(obj, name))
        if not ok:
            return method
        # b. If IsCallable(method) is true, then
        if IsCallable(method):
            # i. Let result be ? Call(method, O).
            result, ok = ec(Call(method, obj))
            if not ok:
                return result
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
        return ThrowCompletion(CreateTypeError())
    elif isObject(arg):
        prim_value, ok = ec(ToPrimitive(arg, 'number'))
        if not ok:
            return prim_value
        result, ok = ec(ToNumber(prim_value))
        if not ok:
            return result
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
    number, ok = ec(ToNumber(arg))
    if not ok:
        return number
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
    number, ok = ec(ToNumber(arg))
    if not ok:
        return number
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
    number, ok = ec(ToNumber(arg))
    if not ok:
        return number
    if math.isnan(number) or number == 0 or abs(number) == math.inf:
        return NormalCompletion(0)
    this_int = math.floor(abs(number))
    if number < 0:
        this_int = -this_int
    int32bit = this_int % 2**32
    return NormalCompletion(int32bit)

# 7.1.7 ToInt16 ( argument )
def ToInt16(arg):
    number, ok = ec(ToNumber(arg))
    if not ok:
        return number
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
    number, ok = ec(ToNumber(arg))
    if not ok:
        return number
    if math.isnan(number) or number == 0 or abs(number) == math.inf:
        return NormalCompletion(0)
    this_int = math.floor(abs(number))
    if number < 0:
        this_int = -this_int
    int16bit = this_int % 2**16
    return NormalCompletion(int16bit)

# 7.1.9 ToInt8 ( argument )
def ToInt8(arg):
    number, ok = ec(ToNumber(arg))
    if not ok:
        return number
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
    number, ok = ec(ToNumber(arg))
    if not ok:
        return number
    if math.isnan(number) or number == 0 or abs(number) == math.inf:
        return NormalCompletion(0)
    this_int = math.floor(abs(number))
    if number < 0:
        this_int = -this_int
    int8bit = this_int % 2**8
    return NormalCompletion(int8bit)

# 7.1.11 ToUint8Clamp ( argument )
def ToUint8Clamp(arg):
    number, ok = ec(ToNumber(arg))
    if not ok:
        return number
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
    prim_value, ok = ec(ToPrimitive(arg, 'string'))
    if not ok:
        return prim_value
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

# 7.1.13 ToObject ( argument )
def ToObject(argument):
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
    if isObject(argument):
        return NormalCompletion(argument)
    # @@@ Add in basic types here as their object interfaces are added
    return ThrowCompletion(CreateTypeError())


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
    n = nc(ToNumber(arg))
    # 4. If SameValue(! ToString(n), argument) is false, return undefined.
    if not SameValue(nc(ToString(n)), arg):
        return None
    # 5. Return n.
    return n
    # A canonical numeric string is any String value for which the CanonicalNumericIndexString abstract operation does
    # not return undefined.

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
    obj, ok = ec(ToObject(value))
    if not ok:
        return obj
    # 3. Return ? O.[[Get]](P, V).
    return obj.Get(propkey, value)

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

# 7.3.9 GetMethod ( V, P )
def GetMethod(value, propkey):
    # The abstract operation GetMethod is used to get the value of a specific property of an ECMAScript language value
    # when the value of the property is expected to be a function. The operation is called with arguments V and P where
    # V is the ECMAScript language value, P is the property key. This abstract operation performs the following steps:
    #
    # 1. Assert: IsPropertyKey(P) is true.
    assert IsPropertyKey(propkey)
    # 2. Let func be ? GetV(V, P).
    func, ok = ec(GetV(value, propkey))
    if not ok:
        return func
    # 3. If func is either undefined or null, return undefined.
    if isUndefined(func) or isNull(func):
        return NormalCompletion(None)
    # 4. If IsCallable(func) is false, throw a TypeError exception.
    if not IsCallable(func):
        return ThrowCompletion(CreateTypeError())
    # 5. Return func.
    return NormalCompletion(func)

# 7.3.12 Call ( F, V [ , argumentsList ] )
def Call(func, value, *args):
    # The abstract operation Call is used to call the [[Call]] internal method of a function object. The operation is
    # called with arguments F, V, and optionally argumentsList where F is the function object, V is an ECMAScript
    # language value that is the this value of the [[Call]], and argumentsList is the value passed to the corresponding
    # argument of the internal method. If argumentsList is not present, a new empty List is used as its value. This
    # abstract operation performs the following steps:
    #
    # 1. If argumentsList is not present, set argumentsList to a new empty List.
    # 2. If IsCallable(F) is false, throw a TypeError exception.
    if not IsCallable(func):
        return ThrowCompletion(CreateTypeError())
    # 3. Return ? F.[[Call]](V, argumentsList).
    return func.Call(value, list(args))

# 7.3.22 GetFunctionRealm ( obj )
def GetFunctionRealm(obj):
    # The abstract operation GetFunctionRealm with argument obj performs the following steps:
    #
    # 1. Assert: obj is a callable object.
    assert IsCallable(obj)
    # 2. If obj has a [[Realm]] internal slot, then
    if hasattr(obj, 'Realm'):
        # a. Return obj.[[Realm]].
        return NormalCompletion(obj.Realm)
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
            return ThrowCompletion(CreateTypeError())
        # b. Let proxyTarget be obj.[[ProxyTarget]].
        proxy_target = obj.ProxyTarget
        # c. Return ? GetFunctionRealm(proxyTarget).
        return GetFunctionRealm(proxy_target)
    # 5. Return the current Realm Record.
    return None ## @@@FIXME@@@
    # NOTE
    # Step 5 will only be reached if obj is a non-standard function exotic object that does not have a [[Realm]]
    # internal slot.


# Chapter 8: Executable Code and Execution Contexts

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
        return NormalCompletion(Empty.EMPTY)

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
        return NormalCompletion(Empty.EMPTY)

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
        return NormalCompletion(Empty.EMPTY)

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
                return ThrowCompletion(CreateReferenceError())
            # b. Perform envRec.CreateMutableBinding(N, true).
            self.CreateMutableBinding(N, True)
            # c. Perform envRec.InitializeBinding(N, V).
            self.InitializeBinding(N, V)
            # d. Return NormalCompletion(empty).
            return NormalCompletion(Empty.EMPTY)
        # 3. If the binding for N in envRec is a strict binding, set S to true.
        if self.bindings[N].strict:
            S = True
        # 4. If the binding for N in envRec has not yet been initialized, throw a ReferenceError exception.
        if not self.bindings[N].initialized:
            return ThrowCompletion(CreateReferenceError())
        # 5. Else if the binding for N in envRec is a mutable binding, change its bound value to V.
        if self.bindings[N].mutable:
            self.bindings[N] = self.bindings[N]._replace(value=V)
        # 6. Else,
        else:
            # a. Assert: This is an attempt to change the value of an immutable binding.
            # b. If S is true, throw a TypeError exception.
            if S:
                return ThrowCompletion(CreateTypeError())
        # 7. Return NormalCompletion(empty).
        return NormalCompletion(Empty.EMPTY)
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
            return ThrowCompletion(CreateReferenceError())
        # 4. Return the value currently bound to N in envRec.
        return NormalCompletion(self.bindings[N].value)

    # 8.1.1.1.7 DeleteBinding ( N )
    def DeleteBinding(self, N):
        # The concrete Environment Record method DeleteBinding for declarative Environment Records can only delete
        # bindings that have been explicitly designated as being subject to deletion.
        # 1. Let envRec be the declarative Environment Record for which the method was invoked.
        # 2. Assert: envRec has a binding for the name that is the value of N.
        # 3. If the binding for N in envRec cannot be deleted, return false.
        if not self.bindings[N].deletable:
            return NormalCompletion(False)
        # 4. Remove the binding for N from envRec.
        del self.bindings[N]
        # 5. Return true.
        return NormalCompletion(True)

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
    def __init__(self, binding_object, with_environment):
        self.binding_object = ToObject(binding_object)
        self.with_environment = ToBoolean(with_environment)

    # 8.1.1.2.1 HasBinding ( N )
    def HasBinding(self, N):
        # The concrete Environment Record method HasBinding for object Environment Records determines if its associated
        # binding object has a property whose name is the value of the argument N:
        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. Let bindings be the binding object for envRec.
        # 3. Let foundBinding be ? HasProperty(bindings, N).
        cr = HasProperty(self.binding_object, N)
        if cr.ctype != CompletionType.NORMAL:
            return cr
        found_binding = cr.value
        # 4. If foundBinding is false, return false.
        if not found_binding:
            return NormalCompletion(False)
        # 5. If the withEnvironment flag of envRec is false, return true.
        if not with_environment:
            return NormalCompletion(True)
        # 6. Let unscopables be ? Get(bindings, @@unscopables).
        cr = Get(self.binding_object, wks_unscopables)
        if cr.ctype != CompletionType.NORMAL:
            return cr
        unscopables = cr.value
        # 7. If Type(unscopables) is Object, then
        if isObject(unscopables):
            # a. Let blocked be ToBoolean(? Get(unscopables, N)).
            cr = Get(unscopables, N)
            if cr.ctype != CompletionType.NORMAL:
                return cr
            blocked = ToBoolean(cr.value)
            # b. If blocked is true, return false.
            if blocked:
                return NormalCompletion(False)
        # 8. Return true.
        return NormalCompletion(True)

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
                                     PropertyDescriptor(Value=None, Writable=True,
                                                        Enumerable=True, Configurable=deletable))
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
        value, ok = ec(HasProperty(self.binding_object, name))
        if not ok:
            return value
        # 4. If value is false, then
        if not value:
            # a. If S is false, return the value undefined; otherwise throw a ReferenceError exception.
            if not strict:
                return NormalCompletion(None)
            return ThrowCompletion(CreateReferenceError())
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
        return NormalCompletion(False)

    # 8.1.1.2.9 HasSuperBinding ( )
    def HasSuperBinding(self):
        # Regular object Environment Records do not provide a super binding.
        # 1. Return false.
        return NormalCompletion(False)

    # 8.1.1.2.10 WithBaseObject ( )
    def WithBaseObject(self):
        # Object Environment Records return undefined as their WithBaseObject unless their withEnvironment flag is
        # true.

        # 1. Let envRec be the object Environment Record for which the method was invoked.
        # 2. If the withEnvironment flag of envRec is true, return the binding object for envRec.
        if self.with_environment:
            return NormalCompletion(self.binding_object)
        # 3. Otherwise, return undefined.
        return NormalCompletion(None)


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
            return NormalCompletion(True)
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
            return ThrowCompletion(CreateTypeError())
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
            return ThrowCompletion(CreateTypeError())
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
        existing_prop, ok = ec(HasOwnProperty(global_object, name))
        if not ok:
            return existing_prop
        # 7. If existingProp is true, then
        if existing_prop:
            # a. Let status be ? ObjRec.DeleteBinding(N).
            status, ok = ec(self.object_record.DeleteBinding(name))
            if not ok:
                return status
            # b. If status is true, then
            if status:
                # i. Let varNames be envRec.[[VarNames]].
                # ii. If N is an element of varNames, remove that element from the varNames.
                try:
                    self.var_names.remove(name)
                except ValueError:
                    pass
            # c. Return status.
            return NormalCompletion(status)
        # 8. Return true.
        return NormalCompletion(True)

    # 8.1.1.4.8 HasThisBinding ( )
    def HasThisBinding(self):
        # Return true.
        return NormalCompletion(True)

    # 8.1.1.4.9 HasSuperBinding ( )
    def HasSuperBinding(self):
        # Return false.
        return NormalCompletion(False)

    # 8.1.1.4.10 WithBaseObject ( )
    def WithBaseObject(self):
        # Global Environment Records always return undefined as their WithBaseObject.
        # Return undefined.
        return NormalCompletion(None)

    # 8.1.1.4.11 GetThisBinding ( )
    def GetThisBinding(self):
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Return envRec.[[GlobalThisValue]].
        return NormalCompletion(self.global_this_value)

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
        return NormalCompletion(name in self.var_names)

    # 8.1.1.4.13 HasLexicalDeclaration ( N )
    def HasLexicalDeclaration(self, name):
        # The concrete Environment Record method HasLexicalDeclaration for global Environment Records determines if the
        # argument identifier has a binding in this record that was created using a lexical declaration such as a
        # LexicalDeclaration or a ClassDeclaration:
        #
        # 1. Let envRec be the global Environment Record for which the method was invoked.
        # 2. Let DclRec be envRec.[[DeclarativeRecord]].
        # 3. Return DclRec.HasBinding(N).
        return NormalCompletion(self.declarative_record.HasBinding(name))

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
        existing_prop, ok = ec(global_object.GetOwnProperty(name))
        if not ok:
            return existing_prop
        # 5. If existingProp is undefined, return false.
        if isUndefined(existing_prop):
            return NormalCompletion(False)
        # 6. If existingProp.[[Configurable]] is true, return false.
        # 7. Return true.
        return NormalCompletion(not existing_prop.Configurable)
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
        has_property, ok = ec(HasOwnProperty(global_object, name))
        if not ok:
            return has_property
        # 5. If hasProperty is true, return true.
        if has_property:
            return NormalCompletion(True)
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
        existing_prop, ok = ec(global_object.GetOwnProperty(name))
        if not ok:
            return existing_prop
        # 5. If existingProp is undefined, return ? IsExtensible(globalObject).
        if isUndefined(existing_prop):
            return IsExtensible(global_object)
        # 6. If existingProp.[[Configurable]] is true, return true.
        if existing_prop['configurable']:
            return NormalCompletion(True)
        # 7. If IsDataDescriptor(existingProp) is true and existingProp has attribute values { [[Writable]]: true,
        #    [[Enumerable]]: true }, return true.
        if IsDataDescriptor(existing_prop) and existing_prop['writable'] and existing_prop['enumerable']:
            return NormalCompletion(True)
        # 8. Return false.
        return NormalCompletion(False)

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
        has_property, ok = ec(HasOwnProperty(global_object, name))
        if not ok:
            return has_property
        # 5. Let extensible be ? IsExtensible(globalObject).
        extensible, ok = ec(IsExtensible(global_object))
        if not ok:
            return extensible
        # 6. If hasProperty is false and extensible is true, then
        if not has_property and extensible:
            # a. Perform ? ObjRec.CreateMutableBinding(N, D).
            cr, ok = ec(self.object_record.CreateMutableBinding(name, deletable))
            if not ok:
                return cr
            # b. Perform ? ObjRec.InitializeBinding(N, undefined).
            cr, ok = ec(self.object_record.InitializeBinding(name, None))
            if not ok:
                return cr
        # 7. Let varDeclaredNames be envRec.[[VarNames]].
        # 8. If varDeclaredNames does not contain N, then
        if name not in self.var_names:
            # a. Append N to varDeclaredNames.
            self.var_names.append(name)
        # 9. Return NormalCompletion(empty).
        return NormalCompletion(Empty.EMPTY)

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
        existing_prop, ok = ec(global_object.GetOwnProperty(name))
        if not ok:
            return existing_prop
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
        cr, ok = ec(DefinePropertyOrThrow(global_object, name, desc))
        if not ok:
            return cr
        # 8. Record that the binding for N in ObjRec has been initialized.
        # .... object records aren't supposed to need to do this, so I'm not sure where I'm storing this info and when
        # it's used. A problem for later....
        pass
        # 9. Perform ? Set(globalObject, N, V, false).
        cr, ok = ec(Set(global_object, name, value, False))
        if not ok:
            return cr
        # 10. Let varDeclaredNames be envRec.[[VarNames]].
        # 11. If varDeclaredNames does not contain N, then
        if name not in self.var_names:
            # a. Append N to varDeclaredNames.
            self.var_names.append(name)
        # 12. Return NormalCompletion(empty).
        return NormalCompletion(Empty.EMPTY)
        # NOTE
        # Global function declarations are always represented as own properties of the global object. If possible, an
        # existing own property is reconfigured to have a standard set of attribute values. Steps 8-9 are equivalent to
        # what calling the InitializeBinding concrete method would do and if globalObject is a Proxy will produce the
        # same sequence of Proxy trap calls.


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

# 8.2.2 CreateIntrinsics ( realmRec )
def CreateIntrinsics(realm_rec):
    # The abstract operation CreateIntrinsics with argument realmRec performs the following steps:
    #
    # 1. Let intrinsics be a new Record.
    intrinsics = {}
    # 2. Set realmRec.[[Intrinsics]] to intrinsics.
    realm_rec.intrinsics = intrinsics
    # 3. Let objProto be ObjectCreate(null).
    obj_proto = ObjectCreate(None)
    # 4. Set intrinsics.[[%ObjectPrototype%]] to objProto.
    intrinsics['%ObjectPrototype%'] = obj_proto
    # 5. Let throwerSteps be the algorithm steps specified in 9.2.9.1 for the %ThrowTypeError% function.
    thrower_steps = object_behaviors.ThrowTypeError
    # 6. Let thrower be CreateBuiltinFunction(throwerSteps, « », realmRec, null).
    thrower = CreateBuiltinFunction(thrower_steps, [], self, None)
    # 7. Set intrinsics.[[%ThrowTypeError%]] to thrower.
    intrinsics['%ThrowTypeError%'] = thrower
    # 8. Let noSteps be an empty sequence of algorithm steps.
    no_steps = lambda: None
    # 9. Let funcProto be CreateBuiltinFunction(noSteps, « », realmRec, objProto).
    func_proto = CreateBuiltinFunction(no_steps, [], realm_rec, obj_proto)
    # 10. Set intrinsics.[[%FunctionPrototype%]] to funcProto.
    intrinsics['%FunctionPrototype%'] = func_proto
    # 11. Call thrower.[[SetPrototypeOf]](funcProto).
    thrower.set_prototype_of(func_proto)
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
    pass # yeah, later, dudez
    # 14. Return intrinsics.
    return intrinsics


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
    new_context.script_or_module = None
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
    global_obj, ok = ec(SetDefaultGlobalBindings(realm))
    if not ok:
        return global_obj
    # 11. Create any implementation-defined global object properties on globalObj.
    pass # Gonna want to add things like "console" here...
    # 12. Return NormalCompletion(empty).
    return NormalCompletion(Empty.EMPTY)

# 8.6 RunJobs
def RunJobs(scripts=[], modules=[]):
    # 1. Perform ? InitializeHostDefinedRealm().
    cr, ok = ec(InitializeHostDefinedRealm())
    if not ok:
        return cr
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
        next_pending = surrounding_agent.job_queues[name].popleft()
        # e. Let newContext be a new execution context.
        new_context = ExecutionContext()
        # f. Set newContext's Function to null.
        new_context.ecma_function = None
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
        result, ok = ec((next_pending.job)(*next_pending.arguments))
        # l. If result is an abrupt completion, perform HostReportErrors(« result.[[Value]] »).
        if not ok:
            HostReportErrors([result])

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


class Lexer():

    @unique
    class Goal(Enum):
        InputElementDiv = auto()
        InputElementRegExp = auto()
        InputElementRegExpOrTemplateTail = auto()
        InputElementTemplateTail = auto()

    @unique
    class Type(Enum):
        Token = auto()
        Whitespace = auto()
        Comment = auto()
        LineTerminator = auto()

    class Token():
        def __init__(self, lexer, type, start, length):
            self.lexer = lexer
            self.type = type
            self.start = start
            self.length = length
        def value(self):
            return self.lexer.source[self.start:self.start+self.length]

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

    def __init__(self, source_text):
        super().__init__()
        self.source = source_text
        self.linenum = 1
        self.start = 0
        self.pos = 0

    def _make_token(self, type, end_prior):
        where = self.pos
        length = where - self.start
        if end_prior:
            length -= 1
        tok = self.Token(self, type, self.start, length)
        self.start += length
        return tok

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
            # WhiteSpace
            return (self._initial, [self._make_token(self.Type.Whitespace, False)])
        elif ch in self.line_terminators:
            # LineTerminator
            if ch != '\u000d' or lookahead != '\u000a':
                self.linenum += 1
            return (self._initial, [self._make_token(self.Type.LineTerminator, False)])
        elif ch == '/':
            # Might be Comment::SingleLineComment, Comment::MultiLineComment, DivPunctuator::/, or DivPunctuator::/=
            return (self._comment_or_div, [])
        elif ch in '(),:;?[]{}~':
            # These are CommonToken::Punctuator or RightBracePunctuator that are uniquely one character in size
            return (self._initial, [self._make_token(self.Type.Token, False)])
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
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _bang_equals(self, ch, lookahead):
        # We already have !=. Might also be !==.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _percent(self, ch, lookahead):
        # We already have %. Might also be %=.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _ampersand(self, ch, lookahead):
        # We already have &. Might also be && or &=.
        if ch == '&' or ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _asterisk(self, ch, lookahead):
        # We already have *. Might also be **, **=, or *=
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        if ch == '*':
            return (self._star_star, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _star_star(self, ch, lookahead):
        # We already have **. Might also be **=.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _plus(self, ch, lookahead):
        # We already have +. Might also be ++ or +=.
        if ch == '+' or ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _minus(self, ch, lookahead):
        # We already have -. Might also be -- or -=.
        if ch == '-' or ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _period(self, ch, lookahead):
        # We already have '.'. Might also be '...'.
        if ch == '.':
            return (self._dot_dot, [])
        if ch and ch in '0123456789':
            # Hey, look, it's a number.
            return (self._after_decimal, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _dot_dot(self, ch, lookahead):
        # We already have '..'. Might also be '...'.
        if ch == '.':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        # (But there's no '..', so that tokenizes into two individual dots.)
        tok = self._make_token(self.Type.Token, True) # This is a '..' token...
        tok.length -= 1 # Convert it to '.' (the first dot)
        self.start -= 1 # back up start to point to the 2nd dot
        tok2 = self._make_token(self.Type.Token, True) # This just got the 2nd dot
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok, tok2], results))

    def _less_than(self, ch, lookahead):
        # We already have '<'. Might also be '<<', '<<=', or '<='.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        if ch == '<':
            return (self._less_less, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _less_less(self, ch, lookahead):
        # We have <<. Might also be <<=.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _equals(self, ch, lookahead):
        # We have =. Might also be ==, ===, or =>.
        if ch == '>':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        if ch == '=':
            return (self._equal_equal, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _equal_equal(self, ch, lookahead):
        # We have ==. Might also be ===.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _greater_than(self, ch, lookahead):
        # We have >. Might also be >=, >>, >>=, >>>, or >>>=.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        if ch == '>':
            return (self._greater_greater, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _greater_greater(self, ch, lookahead):
        # We have >>. Might also be >>=, >>>, or >>>=.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        if ch == '>':
            return (self._greater_x3, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _greater_x3(self, ch, lookahead):
        # We have >>>. Might also be >>>=.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _caret(self, ch, lookahead):
        # We have ^. Might also be ^=.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _pipe(self, ch, lookahead):
        # We have |. Might also be |= or ||.
        if ch and ch in '=|':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _comment_or_div(self, ch, lookahead):
        # We have one slash. All kinds of things this might be.
        if ch == '=':
            return (self._initial, [self._make_token(self.Type.Token, False)])
        if ch == '/':
            # A SingleLineComment
            return (self._single_line_comment, [])
        if ch == '*':
            # A MultiLineComment
            return (self._multi_line_comment, [])
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _single_line_comment(self, ch, lookahead):
        if ch and ch not in self.line_terminators:
            return (self._single_line_comment, [])
        tok = self._make_token(self.Type.Comment, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _multi_line_comment(self, ch, lookahead):
        if ch == '':
            # Hit EOF! That's an unterminated comment error.
            raise LexerError('Unterminated multi-line comment')
        pos = self.pos
        if ch == '/' and (pos - self.start) >= 4 and self.source[pos-2] == '*':
            return (self._initial, [self._make_token(self.Type.Comment, False)])
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

        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _binary_digits(self, ch, lookahead):
        if ch and ch in '01':
            return (self._binary_digits, [])
        if ch and (ch in '23456789' or self.is_identifier_start(ch)):
            raise LexerError('Invalid chars after Numeric Literal')
        if self.source[self.pos-2] not in '01':
            raise LexerError('Invalid numeric literal')
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _octal_digits(self, ch, lookahead):
        if ch and ch in '01234567':
            return (self._octal_digits, [])
        if ch and (ch in '89' or self.is_identifier_start(ch)):
            raise LexerError('Invalid chars after Numeric Literal')
        if self.source[self.pos-2] not in '01234567':
            raise LexerError('Invalid numeric literal')
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _hex_digits(self, ch, lookahead):
        if ch and ch in '0123456789abcdefABCDEF':
            return (self._hex_digits, [])
        if ch and self.is_identifier_start(ch):
            raise LexerError('Invalid chars after Numeric Literal')
        if self.source[self.pos-2] not in '0123456789abcdefABCDEF':
            raise LexerError('Invalid numeric literal')
        tok = self._make_token(self.Type.Token, True)
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
        tok = self._make_token(self.Type.Token, True)
        state, results = self._initial(ch, lookahead)
        return (state, chain([tok], results))

    def _after_decimal(self, ch, lookahead):
        if ch and ch in '0123456789':
            return (self._after_decimal, [])
        if ch and ch in 'eE':
            return (self._after_e, [])

        if ch and self.is_identifier_start(ch):
            raise LexerError('Invalid chars after Numeric Literal')
        tok = self._make_token(self.Type.Token, True)
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
        tok = self._make_token(self.Type.Token, True)
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

    def _ident_capture(self, ch, lookahead):
        if ch and (self.is_unicode_id_continue(ch) or ch in '$\u200c\u200d'):
            return (self._ident_capture, [])
        if ch == '\\':
            return (self._identpart_escape_1, [])
        tok = self._make_token(self.Type.Token, True)
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

    def _single_string_capture(self, ch, lookahead):
        if ch == "'":
            return (self._initial, [self._make_token(self.Type.Token, False)])
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
        state = self._initial

        ch = self.source[self.pos]
        try:
            lookahead = self.source[self.pos+1]
        except IndexError:
            lookahead = ''
        self.pos += 1

        while state != self._done:
            state, results = state(ch, lookahead)
            for rval in results:
                yield rval
            ch = lookahead
            try:
                lookahead = self.source[self.pos+1]
            except IndexError:
                lookahead = ''
            self.pos += 1
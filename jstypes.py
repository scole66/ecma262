# 6.1 ECMAScript Language Types
from enum import Enum, unique, auto
from collections import namedtuple

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

# 6.1.7.1 Property Attributes
DataProperty = namedtuple('DataProperty', ['value', 'writable', 'enumerable', 'configurable'])
AccessorProperty = namedtuple('AccessorProperty', ['Get', 'Set', 'enumerable', 'configurable'])
class JSObject:
    # This is all handled in chapter 9. I'll wait until I get there before dealing with the rest of this.
    pass

def isObject(arg):
    return isinstance(arg, JSObject)

def isEcmaValue(arg):
    """Returns True if 'arg' is a ECMAScript Value."""
    return isinstance(arg, (type(None), str, int, float, bool, JSSymbol, JSNull, JSObject))

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

from enum import Enum, unique, auto
from collections import namedtuple

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
    if isinstance(val, Completion):
        if val.ctype != CompletionType.NORMAL:
            return (val, False)
        val = val.value
    return (val, True)

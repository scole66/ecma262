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

class Completion():
    @unique
    class Type(Enum):
        NORMAL = auto()
        BREAK = auto()
        CONTINUE = auto()
        RETURN = auto()
        THROW = auto()

    def __init__(self, ctype=None, value=None, target=None):
        self.ctype = ctype
        self.value = value
        self.target = target

# 6.2.3.2 NormalCompletion

def NormalCompletion(arg):
    # The abstract operation NormalCompletion with a single argument, such as:
    #
    # 1. Return NormalCompletion(argument).
    #
    # Is a shorthand that is defined as follows:
    #
    # 1. Return Completion { [[Type]]: normal, [[Value]]: argument, [[Target]]: empty }.
    return Completion(ctype=Completion.Type.NORMAL, value=arg)

# 6.2.3.3 ThrowCompletion
def ThrowCompletion(arg):
    # The abstract operation ThrowCompletion with a single argument, such as:
    #
    # 1. Return ThrowCompletion(argument).
    #
    # Is a shorthand that is defined as follows:
    #
    # 1. Return Completion { [[Type]]: throw, [[Value]]: argument, [[Target]]: empty }.
    return Completion(ctype=Completion.Type.THROW, value=arg)

# 6.3.2.4 UpdateEmpty
def UpdateEmpty(cr, value):
    # The abstract operation UpdateEmpty with arguments completionRecord and value performs the following steps:

    # 1. Assert: If completionRecord.[[Type]] is either return or throw, then completionRecord.[[Value]] is not empty.
    assert cr.value is not None if cr.ctype in [Completion.Type.RETURN, Completion.Type.THROW] else True

    # 2. If completionRecord.[[Value]] is not empty, return Completion(completionRecord).
    if cr.value is not None:
        return Completion(cr.ctype, cr.value, cr.target)

    # 3. Return Completion { [[Type]]: completionRecord.[[Type]], [[Value]]: value, [[Target]]: completionRecord.[[Target]] }.
    return Completion(cr.ctype, value, cr.target)

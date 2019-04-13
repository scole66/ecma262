import pytest

from ecmascript import *

def test_NormalCompletion_01():
    z = NormalCompletion('test_string')

    assert z.ctype == CompletionType.NORMAL
    assert z.value == 'test_string'
    assert z.target is None

def test_NormalCompletion_02():
    z = NormalCompletion('abc')
    r = NormalCompletion(z)

    assert r == Completion(CompletionType.NORMAL, 'abc', None)

def test_ThrowCompletion():
    z = ThrowCompletion('test_string')

    assert z.ctype == CompletionType.THROW
    assert z.value == 'test_string'
    assert z.target is None

def test_UpdateEmpty():
    z = Completion(CompletionType.BREAK, Empty.EMPTY, None)

    r = UpdateEmpty(z, 'update_string')

    assert r.ctype == z.ctype
    assert r.target == z.target
    assert r.value == 'update_string'

    z2 = NormalCompletion('gobbledygook')
    r2 = UpdateEmpty(z2, 'update_string')

    assert r2.ctype == z2.ctype
    assert r2.target == z2.target
    assert r2.value == 'gobbledygook'

    z3 = Completion(CompletionType.CONTINUE, None, None)
    r3 = UpdateEmpty(z3, 'update_string')

    assert r3.ctype == z3.ctype
    assert r3.target == z3.target
    assert r3.value is None

def test_ec():
    okcr = Completion(CompletionType.NORMAL, 'green', None)
    abortcr = Completion(CompletionType.THROW, 'blue', None)

    assert ec(okcr) == ('green', True)
    assert ec(abortcr) == (abortcr, False)

def test_nc():
    okcr = Completion(CompletionType.NORMAL, 'green', None)
    assert nc(okcr) == 'green'

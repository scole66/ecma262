import pytest

from ecmascript import *

def test_getbase():
    ref = Reference('base', 'name', False)
    assert GetBase(ref) == 'base'

def test_getreferencedname():
    ref = Reference('base', 'name', False)
    assert GetReferencedName(ref) == 'name'

@pytest.mark.parametrize('strict,expected', [ (True, True), (False, False) ])
def test_isstrictreference(strict, expected):
    ref = Reference('base', 'name', strict)
    assert IsStrictReference(ref) == expected

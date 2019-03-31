import pytest

import reference

def test_getbase():
    ref = reference.Reference('base', 'name', False)
    assert reference.GetBase(ref) == 'base'

def test_getreferencedname():
    ref = reference.Reference('base', 'name', False)
    assert reference.GetReferencedName(ref) == 'name'

@pytest.mark.parametrize('strict,expected', [ (True, True), (False, False) ])
def test_isstrictreference(strict, expected):
    ref = reference.Reference('base', 'name', strict)
    assert reference.IsStrictReference(ref) == expected

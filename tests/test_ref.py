import pytest

from ecmascript import *

@pytest.fixture
def realm():
    InitializeHostDefinedRealm()
    return surrounding_agent.running_ec.realm

@pytest.fixture
def obj(realm):
    return ObjectCreate(realm.intrinsics['%ObjectPrototype%'])

def test_reference_repr_01():
    ref = Reference(None, 'name', True)
    val = repr(ref)
    assert 'base=None' in val
    assert 'name' in val
    assert 'S' in val

def test_reference_repr_02():
    ref = Reference('base', 'silly', False)
    val = repr(ref)
    assert 'S' not in val

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

@pytest.mark.parametrize('base, expected', [ (True, True), ('cherry', True), (67, True), (wks_unscopables, True), (JSNull.NULL, False)])
def test_HasPrimitiveBase_01(base, expected):
    ref = Reference(base, 'refname', False)
    assert HasPrimitiveBase(ref) == expected

def test_HasPrimitiveBase_02(obj):
    ref = Reference(obj, 'refname', False)
    assert not HasPrimitiveBase(ref)

def test_IsPropertyReference_01(obj):
    ref = Reference(obj, 'refname', False)
    assert IsPropertyReference(ref)

@pytest.mark.parametrize('base, expected', [(None, False), (False, True)])
def test_IsPropertyReference_02(base, expected):
    ref = Reference(base, 'refname', False)
    assert IsPropertyReference(ref) == expected

@pytest.mark.parametrize('useObject, expected', [(False, True), (True, False)])
def test_IsUnresolvableReference_01(obj, useObject, expected):
    ref = Reference(obj if useObject else None, 'refname', False)
    assert IsUnresolvableReference(ref) == expected

@pytest.mark.parametrize('hasSuper, expected', ([False, False], [True, True]))
def test_IsSuperReference_01(obj, hasSuper, expected):
    ref = Reference(obj, 'refname', False)
    if hasSuper:
        ref.this_value = obj
    assert IsSuperReference(ref) == expected

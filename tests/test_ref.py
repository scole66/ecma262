import pytest
import math

from ecmascript import *

THROW = CompletionType.THROW
NORMAL = CompletionType.NORMAL

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

@pytest.mark.parametrize('ref, expected', [
    (Completion(THROW, 'thrown', None), Completion(THROW, 'thrown', None)),  # Input is abrupt completion
    ('monkey', Completion(NORMAL, 'monkey', None)), # Input is not a reference
])
def test_GetValue_01(ref, expected):
    rval = GetValue(ref)
    assert rval == expected

def test_GetValue_02():
    # If the input reference is unresolvable, throw a reference error.
    rval = GetValue(Reference(None, 'unresolvable', False))
    assert rval.ctype == THROW
    assert isinstance(rval.value, ReferenceError)
    assert rval.target is None

def test_GetValue_03(obj):
    # The common case: resolve the reference to an object
    CreateDataProperty(obj, 'value', 'elephant')
    rval = GetValue(Reference(obj, 'value', False))
    assert rval == Completion(NORMAL, 'elephant', None)

@pytest.mark.xfail(reason='Needs String Object support')
def test_GetValue_04():
    # Now, the case where the base is a common object, and we try and get a property from that. One example is the 'length'
    # property of strings. This will fail for now, because I don't have string objects implemented.
    rval = GetValue(Reference('short', 'length', False))
    assert rval == Completion(NORMAL, 5, None)

def test_GetValue_05():
    # But we can still try to dereference a value from a primitive type object; we should just get None back.
    rval = GetValue(Reference(45, 'digits', False))
    assert rval == Completion(NORMAL, None, None)

def test_GetValue_06(realm):
    # Finally, there's the Environment Record form.
    rval = GetValue(Reference(realm.global_env.environment_record, 'NaN', False))
    assert rval == Completion(NORMAL, math.nan, None)

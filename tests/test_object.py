import pytest

from ecmascript import *

NORMAL = CompletionType.NORMAL
THROW = CompletionType.THROW

@pytest.fixture
def realm():
    InitializeHostDefinedRealm()
    return surrounding_agent.running_ec.realm

@pytest.fixture
def object_chain(realm):
    # An ancestor object
    ancestor = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    # A parent object
    parent = ObjectCreate(ancestor)
    # A child object
    child = ObjectCreate(parent)

    return {
        'realm': realm,
        'ancestor': ancestor,
        'parent': parent,
        'child': child
        }

def test_object_GetPrototypeOf_method(object_chain):
    assert object_chain['child'].GetPrototypeOf() == Completion(NORMAL, object_chain['parent'], None)
    assert object_chain['parent'].GetPrototypeOf() == Completion(NORMAL, object_chain['ancestor'], None)

@pytest.mark.parametrize('extensible,result', [(False, True), (True, True)])
def test_object_SetPrototypeOf_method_01(realm, extensible, result):
    # If you set the prototype of an object to the same thing it already has, this should return success, independent
    # of the value of the object's [[Extensible]] property.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    obj.Extensible = extensible
    assert obj.SetPrototypeOf(realm.intrinsics['%ObjectPrototype%']) == Completion(NORMAL, result, None)
    assert obj.GetPrototypeOf().value == realm.intrinsics['%ObjectPrototype%']

def test_object_SetPrototypeOf_method_02(realm):
    # On the other hand, if [[Extensible]] is False, nothing can _change_ the prototype.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    obj.Extensible = False
    assert obj.SetPrototypeOf(realm.intrinsics['%FunctionPrototype%']) == Completion(NORMAL, False, None)
    assert obj.GetPrototypeOf().value == realm.intrinsics['%ObjectPrototype%']

def test_object_SetPrototypeOf_method_03(object_chain):
    realm = object_chain['realm']
    # We should not be able to make prototype chain loops.
    assert object_chain['ancestor'].SetPrototypeOf(object_chain['child']) == Completion(NORMAL, False, None)
    assert object_chain['ancestor'].GetPrototypeOf().value == realm.intrinsics['%ObjectPrototype%']

def test_object_SetPrototypeOf_method_04(object_chain):
    realm = object_chain['realm']
    # On the other hand, if we don't detect a loop, setting a prototype is fine.
    assert object_chain['child'].SetPrototypeOf(realm.intrinsics['%FunctionPrototype%']) == Completion(NORMAL, True, None)
    assert object_chain['child'].GetPrototypeOf().value == realm.intrinsics['%FunctionPrototype%']

def test_object_SetPrototypeOf_method_05(object_chain):
    realm = object_chain['realm']
    # If an object in the prototype chain has a nonstandard GetPrototypeOf, we abort the loop detector and just set it.
    object_chain['parent'].GetPrototypeOf = types.MethodType(lambda _: realm.intrinsics['%FunctionPrototype%'], 'GetPrototypeOf')

    assert object_chain['ancestor'].SetPrototypeOf(object_chain['child']) == Completion(NORMAL, True, None)
    assert object_chain['ancestor'].GetPrototypeOf().value == object_chain['child']

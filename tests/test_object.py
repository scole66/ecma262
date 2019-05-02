from itertools import zip_longest

import pytest

from ecmascript import *
import ecmascript # Need it this way for the mocker fixture

NORMAL = CompletionType.NORMAL
THROW = CompletionType.THROW

@pytest.fixture
def realm():
    InitializeHostDefinedRealm()
    yield surrounding_agent.running_ec.realm
    surrounding_agent.ec_stack.pop()
    surrounding_agent.running_ec = None

@pytest.fixture
def obj(realm):
    return ObjectCreate(realm.intrinsics['%ObjectPrototype%'])

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

def test_Property_repr():
    p = JSObject.Property(value=100, enumerable=True, polio_vaccine='useful')
    s = repr(p)
    assert 'value=100' in s
    assert 'enumerable=True' in s
    assert 'polio_vaccine' not in s

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
    object_chain['parent'].GetPrototypeOf = types.MethodType(lambda _: realm.intrinsics['%FunctionPrototype%'],
                                                             object_chain['parent'])

    assert object_chain['ancestor'].SetPrototypeOf(object_chain['child']) == Completion(NORMAL, True, None)
    assert object_chain['ancestor'].GetPrototypeOf().value == object_chain['child']

@pytest.mark.parametrize('input, expected', [(True, True), (False, False)])
def test_object_IsExtensible_method(realm, input, expected):
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    obj.Extensible = input

    assert obj.IsExtensible() == Completion(NORMAL, expected, None)

def test_object_PreventExtensions_method(realm):
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    assert obj.IsExtensible().value
    assert obj.PreventExtensions() == Completion(NORMAL, True, None)
    assert not obj.IsExtensible().value

def test_object_GetOwnProperty_method_01(realm):
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    # If O doesn't have the property, returns undefined.
    assert obj.GetOwnProperty('not_here') == Completion(NORMAL, None, None)

def test_object_GetOwnProperty_method_02(realm):
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    desc = PropertyDescriptor(value=100, writable=True, enumerable=True, configurable=True)
    DefinePropertyOrThrow(obj, 'testkey', desc)

    cr = obj.GetOwnProperty('testkey')
    assert isinstance(cr, Completion)
    assert (cr.ctype, cr.target) == (NORMAL, None)
    assert isinstance(cr.value, PropertyDescriptor)
    assert cr.value.is_data_descriptor() and not cr.value.is_accessor_descriptor()
    assert (cr.value.value, cr.value.writable, cr.value.enumerable, cr.value.configurable) == (100, True, True, True)

def test_object_GetOwnProperty_method_03(realm):
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'], ['test_slot'])
    def test_get(self, new_target):
        return NormalCompletion(self.test_slot)
    def test_set(self, new_target, val):
        self.test_slot = val
        return NormalCompletion(None)
    get_func = CreateBuiltinFunction(test_get, [], realm)
    set_func = CreateBuiltinFunction(test_set, [], realm)
    desc = PropertyDescriptor(Get=get_func, Set=set_func, enumerable=False, configurable=True)
    DefinePropertyOrThrow(obj, 'testkey', desc)

    cr = obj.GetOwnProperty('testkey')
    assert isinstance(cr, Completion)
    assert (cr.ctype, cr.target) == (NORMAL, None)
    assert isinstance(cr.value, PropertyDescriptor)
    assert not cr.value.is_data_descriptor() and cr.value.is_accessor_descriptor()
    assert (cr.value.Get, cr.value.Set, cr.value.enumerable, cr.value.configurable) == (get_func, set_func, False, True)

def test_ValidateAndApplyPropertyDescriptor_01(realm):
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])

    # If current == None, and not extensible return False.
    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', False, PropertyDescriptor(), None)
    assert cr == Completion(NORMAL, False, None)

def test_ValidateAndApplyPropertyDescriptor_02(realm):
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])

    # If current == None, and the descriptor is empty, we make a default property.
    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, PropertyDescriptor(), None)
    assert cr == Completion(NORMAL, True, None)
    pdesc = obj.GetOwnProperty('testkey').value
    assert pdesc.is_data_descriptor() and not pdesc.is_accessor_descriptor()
    assert (pdesc.value, pdesc.writable, pdesc.enumerable, pdesc.configurable) == (None, False, False, False)

def test_ValidateAndApplyPropertyDescriptor_03(realm):
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])

    # If current == None, and the descriptor is full, we make a copy.
    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, PropertyDescriptor(value=102, writable=True, enumerable=True, configurable=True), None)
    assert cr == Completion(NORMAL, True, None)
    pdesc = obj.GetOwnProperty('testkey').value
    assert pdesc.is_data_descriptor() and not pdesc.is_accessor_descriptor()
    assert (pdesc.value, pdesc.writable, pdesc.enumerable, pdesc.configurable) == (102, True, True, True)

def test_ValidateAndApplyPropertyDescriptor_04(realm):
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])

    # If current == None, and the descriptor is partial, the rest gets filled with defaults.
    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, PropertyDescriptor(Get=None, enumerable=True), None)
    assert cr == Completion(NORMAL, True, None)
    pdesc = obj.GetOwnProperty('testkey').value
    assert not pdesc.is_data_descriptor() and pdesc.is_accessor_descriptor()
    assert (pdesc.Get, pdesc.Set, pdesc.enumerable, pdesc.configurable) == (None, None, True, False)

def test_ValidateAndApplyPropertyDescriptor_05(realm):
    # If current is None, obj is None, and extensible is True, it doesn't matter what the descriptor has. We just
    # return True.
    cr = ValidateAndApplyPropertyDescriptor(None, None, True, None, None)
    assert cr == Completion(NORMAL, True, None)

def test_ValidateAndApplyPropertyDescriptor_06(realm):
    # Current has a value and the descriptor is empty: it's all good.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    CreateDataProperty(obj, 'testkey', 89)
    current = obj.GetOwnProperty('testkey').value

    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, PropertyDescriptor(), current)
    assert cr == Completion(NORMAL, True, None)
    # Nothing should have been changed:
    after = obj.GetOwnProperty('testkey').value
    assert (current.value, current.writable, current.enumerable, current.configurable) == \
                (after.value, after.writable, after.enumerable, after.configurable)

def test_ValidateAndApplyPropertyDescriptor_07(realm):
    # If the current property is not configurable, we can't change it to *be* configurable.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=False))
    current = obj.GetOwnProperty('testkey').value

    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, PropertyDescriptor(configurable=True), current)
    assert cr == Completion(NORMAL, False, None)
    # Nothing should have been changed:
    after = obj.GetOwnProperty('testkey').value
    assert (current.value, current.writable, current.enumerable, current.configurable) == \
                (after.value, after.writable, after.enumerable, after.configurable)

def test_ValidateAndApplyPropertyDescriptor_08(realm):
    # If the current property is not configurable, we can't change its enumerability
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=False))
    current = obj.GetOwnProperty('testkey').value

    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, PropertyDescriptor(enumerable=False), current)
    assert cr == Completion(NORMAL, False, None)
    # Nothing should have been changed:
    after = obj.GetOwnProperty('testkey').value
    assert (current.value, current.writable, current.enumerable, current.configurable) == \
                (after.value, after.writable, after.enumerable, after.configurable)

def test_ValidateAndApplyPropertyDescriptor_09(realm):
    # If the configurable/enumerable flags are ok, but there's nothing else, just be true.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=False))
    current = obj.GetOwnProperty('testkey').value

    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, PropertyDescriptor(enumerable=True), current)
    assert cr == Completion(NORMAL, True, None)
    # Nothing should have been changed:
    after = obj.GetOwnProperty('testkey').value
    assert (current.value, current.writable, current.enumerable, current.configurable) == \
                (after.value, after.writable, after.enumerable, after.configurable)

def test_ValidateAndApplyPropertyDescriptor_10(realm):
    # If the current prop is not configurable, switching from data to accessor isn't allowed (or vice versa, for that
    # matter).
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=False))
    current = obj.GetOwnProperty('testkey').value

    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, PropertyDescriptor(Get=None, Set=None), current)
    assert cr == Completion(NORMAL, False, None)
    # Nothing should have been changed:
    after = obj.GetOwnProperty('testkey').value
    assert (current.value, current.writable, current.enumerable, current.configurable) == \
                (after.value, after.writable, after.enumerable, after.configurable)

def test_ValidateAndApplyPropertyDescriptor_11(realm):
    # Switch from a data descriptor to an accessor descriptor
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=True))
    current = obj.GetOwnProperty('testkey').value

    def test_get(self, new_target):
        return NormalCompletion('nonsense')
    def test_set(self, new_target, val):
        return NormalCompletion(None)
    get_func = CreateBuiltinFunction(test_get, [], realm)
    set_func = CreateBuiltinFunction(test_set, [], realm)
    desc = PropertyDescriptor(Get=get_func, Set=set_func, enumerable=False, configurable=True)

    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, desc, current)
    assert cr == Completion(NORMAL, True, None)
    after = obj.GetOwnProperty('testkey').value
    assert after.is_accessor_descriptor() and not after.is_data_descriptor()
    assert (after.Get, after.Set, after.enumerable, after.configurable) == (get_func, set_func, False, True)

def test_ValidateAndApplyPropertyDescriptor_12(realm):
    # Switch from an accessor descriptor to a data descriptor
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    def test_get(self, new_target):
        return NormalCompletion('nonsense')
    def test_set(self, new_target, val):
        return NormalCompletion(None)
    get_func = CreateBuiltinFunction(test_get, [], realm)
    set_func = CreateBuiltinFunction(test_set, [], realm)
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(Get=get_func, Set=set_func, enumerable=True, configurable=True))
    current = obj.GetOwnProperty('testkey').value

    desc = PropertyDescriptor(value=33, writable=True, enumerable=False, configurable=True)

    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, desc, current)
    assert cr == Completion(NORMAL, True, None)
    after = obj.GetOwnProperty('testkey').value
    assert not after.is_accessor_descriptor() and after.is_data_descriptor()
    assert (after.value, after.writable, after.enumerable, after.configurable) == (33, True, False, True)

def test_ValidateAndApplyPropertyDescriptor_13(realm):
    # If the property is not configurable, changing from "not writable" to "writable" is not allowed.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(value=10, writable=False, enumerable=True, configurable=False))
    current = obj.GetOwnProperty('testkey').value

    desc = PropertyDescriptor(writable=True)
    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, desc, current)
    assert cr == Completion(NORMAL, False, None)
    # Nothing should have been changed:
    after = obj.GetOwnProperty('testkey').value
    assert (current.value, current.writable, current.enumerable, current.configurable) == \
                (after.value, after.writable, after.enumerable, after.configurable)

def test_ValidateAndApplyPropertyDescriptor_14(realm):
    # But: going from "writable" to "not writable" is just fine.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=False))
    current = obj.GetOwnProperty('testkey').value

    desc = PropertyDescriptor(writable=False)
    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, desc, current)
    assert cr == Completion(NORMAL, True, None)
    # Only writable has changed:
    after = obj.GetOwnProperty('testkey').value
    assert (current.value, False, current.enumerable, current.configurable) == \
                (after.value, after.writable, after.enumerable, after.configurable)

def test_ValidateAndApplyPropertyDescriptor_15(realm):
    # If the property is not configurable, nor writable, then changing the value is not allowed.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(value=10, writable=False, enumerable=True, configurable=False))
    current = obj.GetOwnProperty('testkey').value

    desc = PropertyDescriptor(value=11)
    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, desc, current)
    assert cr == Completion(NORMAL, False, None)
    # Nothing should have been changed:
    after = obj.GetOwnProperty('testkey').value
    assert (current.value, current.writable, current.enumerable, current.configurable) == \
                (after.value, after.writable, after.enumerable, after.configurable)

def test_ValidateAndApplyPropertyDescriptor_16(realm):
    # If the property is not configurable, nor writable, setting the value but not changing it is fine.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(value=10, writable=False, enumerable=True, configurable=False))
    current = obj.GetOwnProperty('testkey').value

    desc = PropertyDescriptor(value=10)
    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, desc, current)
    assert cr == Completion(NORMAL, True, None)
    # Nothing should have been changed:
    after = obj.GetOwnProperty('testkey').value
    assert (current.value, current.writable, current.enumerable, current.configurable) == \
                (after.value, after.writable, after.enumerable, after.configurable)

def test_ValidateAndApplyPropertyDescriptor_17(realm):
    # For Accessor descriptors, changing Get or Set is not allowed if the property is not configurable
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    def test_get(self, new_target):
        return NormalCompletion('nonsense')
    def test_set(self, new_target, val):
        return NormalCompletion(None)
    get_func = CreateBuiltinFunction(test_get, [], realm)
    set_func = CreateBuiltinFunction(test_set, [], realm)
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(Get=get_func, Set=set_func, enumerable=True, configurable=False))
    current = obj.GetOwnProperty('testkey').value

    desc = PropertyDescriptor(Get=None)
    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, desc, current)
    assert cr == Completion(NORMAL, False, None)
    # Nothing should have been changed:
    after = obj.GetOwnProperty('testkey').value
    assert (current.Get, current.Set, current.enumerable, current.configurable) == \
                (after.Get, after.Set, after.enumerable, after.configurable)

def test_ValidateAndApplyPropertyDescriptor_18(realm):
    # For Accessor descriptors, changing Get or Set is not allowed if the property is not configurable
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    def test_get(self, new_target):
        return NormalCompletion('nonsense')
    def test_set(self, new_target, val):
        return NormalCompletion(None)
    get_func = CreateBuiltinFunction(test_get, [], realm)
    set_func = CreateBuiltinFunction(test_set, [], realm)
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(Get=get_func, Set=set_func, enumerable=True, configurable=False))
    current = obj.GetOwnProperty('testkey').value

    desc = PropertyDescriptor(Set=None)
    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, desc, current)
    assert cr == Completion(NORMAL, False, None)
    # Nothing should have been changed:
    after = obj.GetOwnProperty('testkey').value
    assert (current.Get, current.Set, current.enumerable, current.configurable) == \
                (after.Get, after.Set, after.enumerable, after.configurable)

def test_ValidateAndApplyPropertyDescriptor_19(realm):
    # For non-configurable Accessor descriptors, setting Get or Set to their current values is fine.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    def test_get(self, new_target):
        return NormalCompletion('nonsense')
    def test_set(self, new_target, val):
        return NormalCompletion(None)
    get_func = CreateBuiltinFunction(test_get, [], realm)
    set_func = CreateBuiltinFunction(test_set, [], realm)
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(Get=get_func, Set=set_func, enumerable=True, configurable=False))
    current = obj.GetOwnProperty('testkey').value

    desc = PropertyDescriptor(Get=get_func, Set=set_func)
    cr = ValidateAndApplyPropertyDescriptor(obj, 'testkey', True, desc, current)
    # Nothing should have been changed:
    after = obj.GetOwnProperty('testkey').value
    assert (current.Get, current.Set, current.enumerable, current.configurable) == \
                (after.Get, after.Set, after.enumerable, after.configurable)

def test_ValidateAndApplyPropertyDescriptor_20(realm):
    # A validation request with an data descriptor and no "current" just returns True
    desc = PropertyDescriptor(value='foo')
    cr = ValidateAndApplyPropertyDescriptor(None, 'myprop', True, desc, None)
    assert cr == Completion(NORMAL, True, None)

def test_ValidateAndApplyPropertyDescriptor_21(realm):
    # Switching from a data desriptor to an accessor descriptor for validation only is ok.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    def test_get(self, new_target):
        return NormalCompletion('nonsense')
    get_func = CreateBuiltinFunction(test_get, [], realm)
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(value='hoop', writable=True, enumerable=True, configurable=True))
    current = obj.GetOwnProperty('testkey').value

    desc = PropertyDescriptor(Get=get_func)
    cr = ValidateAndApplyPropertyDescriptor(None, 'testkey', True, desc, current)
    assert cr == Completion(NORMAL, True, None)

def test_ValidateAndApplyPropertyDescriptor_22(realm):
    # Switching from an accessor desriptor to a data descriptor for validation only is ok.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    def test_get(self, new_target):
        return NormalCompletion('nonsense')
    get_func = CreateBuiltinFunction(test_get, [], realm)
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(Get=get_func, enumerable=True, configurable=True))
    current = obj.GetOwnProperty('testkey').value

    desc = PropertyDescriptor(value='bar')
    cr = ValidateAndApplyPropertyDescriptor(None, 'testkey', True, desc, current)
    assert cr == Completion(NORMAL, True, None)

def test_ValidateAndApplyPropertyDescriptor_23(realm):
    # Switch an accessor property
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    def test_get(self, new_target):
        return NormalCompletion('nonsense')
    get_func = CreateBuiltinFunction(test_get, [], realm)
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(Get=get_func, enumerable=True, configurable=True))
    current = obj.GetOwnProperty('testkey').value

    def test_get2(self, new_target):
        return NormalCompletion('alternate')
    get_func2 = CreateBuiltinFunction(test_get2, [], realm)
    desc = PropertyDescriptor(Get=get_func2)
    cr = ValidateAndApplyPropertyDescriptor(None, 'testkey', True, desc, current)
    assert cr == Completion(NORMAL, True, None)

# [[DefineOwnProperty]] on objects: if we assume ValidateAndApplyPropertyDescriptor works properly,
# all we care about here:
#   * Ensuring that we pass the Extensible property correctly;
#   * Ensuring the arguments to our method (the key and the descriptor) get to where they need to go;
#   * Ensuring the exception paths are exercised
def test_object_DefineOwnProperty_method_01(realm):
    # We can replace properties on extensible objects
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    DefinePropertyOrThrow(obj, 'testkey', PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=True))

    desc = PropertyDescriptor(value=15)
    cr = obj.DefineOwnProperty('testkey', desc)
    assert cr == Completion(NORMAL, True, None)
    after = obj.GetOwnProperty('testkey').value
    assert (after.value, after.writable, after.enumerable, after.configurable) == (15, True, True, True)

def test_object_DefineOwnProperty_method_02(realm):
    # We can add properties to extensible objects
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])

    desc = PropertyDescriptor(value=120)
    cr = obj.DefineOwnProperty('testkey', desc)
    assert cr == Completion(NORMAL, True, None)
    after = obj.GetOwnProperty('testkey').value
    assert (after.value, after.writable, after.enumerable, after.configurable) == (120, False, False, False)

def test_object_DefineOwnProperty_method_03(realm):
    # We cannot add new properties to non-extensible objects
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    obj.PreventExtensions()

    desc = PropertyDescriptor(value=120)
    cr = obj.DefineOwnProperty('testkey', desc)
    assert cr == Completion(NORMAL, False, None)
    assert not obj.HasProperty('testkey').value

def test_object_DefineOwnProperty_method_04(realm):
    # If an exception happens in [[GetOwnProperty]], it bubbles up. The default implementaion of that method can't actually
    # fail, though, so we have to make one that throws deliberately.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    obj.GetOwnProperty = types.MethodType(lambda _a, _b: ThrowCompletion('Thrown from 04'), obj)

    desc = PropertyDescriptor(value=120)
    cr = obj.DefineOwnProperty('testkey', desc)
    assert cr == Completion(THROW, 'Thrown from 04', None)

def test_object_HasProperty_method_01(realm):
    # Returns True when the key is an own property
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    CreateDataProperty(obj, 'testkey', 10)

    cr = obj.HasProperty('testkey')
    assert cr == Completion(NORMAL, True, None)

def test_object_HasProperty_method_02(realm):
    # Returns True when the key is on the prototype chain
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])

    cr = obj.HasProperty('toString')
    assert cr == Completion(NORMAL, True, None)

def test_object_HasProperty_method_03(realm):
    # Returns False when the key is nowhere to be found
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])

    cr = obj.HasProperty('toElephant')
    assert cr == Completion(NORMAL, False, None)

def test_object_HasProperty_method_04(realm):
    # Exceptions from [[GetOwnProperty]] bubble up. (Step 2, in the algorithm.)
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    obj.GetOwnProperty = types.MethodType(lambda _a, _b: ThrowCompletion('Thrown from 04'), obj)

    cr = obj.HasProperty('toElephant')
    assert cr == Completion(THROW, 'Thrown from 04', None)

def test_object_HasProperty_method_05(realm):
    # Exceptions from [[GetPrototypeOf]] bubble up.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    obj.GetPrototypeOf = types.MethodType(lambda _: ThrowCompletion('Thrown from 05'), obj)

    cr = obj.HasProperty('toElephant')
    assert cr == Completion(THROW, 'Thrown from 05', None)

@pytest.fixture
def get_tree(realm):
    parent = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    child = ObjectCreate(parent)
    CreateDataProperty(parent, 'onparent', 100)
    CreateDataProperty(child, 'onchild', -100)
    def special_get(self, new_target):
        return NormalCompletion(self)
    get_fcn = nc(CreateBuiltinFunction(special_get, [], realm=realm))
    DefinePropertyOrThrow(child, 'accessor', PropertyDescriptor(Get=get_fcn, enumerable=True, configurable=True))
    DefinePropertyOrThrow(child, 'dead', PropertyDescriptor(Get=None, enumerabl=True, configurable=True))
    return child

@pytest.mark.parametrize('input, expected', [
    ('onchild', -100), # Getting a data property on self works.
    ('onparent', 100), # Getting a data prop on parent works.
    ('circus_performers', None), # A property that isn't there at all returns undefined
    ('accessor', 345), # An accessor property works.
    ('dead', None) # An empty Get function on the accessor prop means Undefined happens.
    ])
def test_object_Get_method(get_tree, input, expected):
    val = get_tree.Get(input, 345)
    assert val == Completion(NORMAL, expected, None)

def test_object_Get_method_01(get_tree):
    # An exception in the [[GetOwnProperty]] code path...
    get_tree.GetOwnProperty = types.MethodType(lambda _a, _b: ThrowCompletion('Thrown from Get_01'), get_tree)
    val = get_tree.Get('thimble', 345)
    assert val == Completion(THROW, 'Thrown from Get_01', None)

def test_object_Get_method_02(get_tree):
    # An exception in the [[GetPrototypeOf]] code path...
    get_tree.GetPrototypeOf = types.MethodType(lambda _: ThrowCompletion('Thrown from Get_02'), get_tree)
    val = get_tree.Get('needle', 345)
    assert val == Completion(THROW, 'Thrown from Get_02', None)

def test_object_Get_method_03(get_tree):
    # Now try an exception from the parent's recursion.
    parent = nc(get_tree.GetPrototypeOf())
    parent.GetOwnProperty = types.MethodType(lambda _a, _b: ThrowCompletion('Thrown from Get_03'), parent)
    val = get_tree.Get('haystack', 345)
    assert val == Completion(THROW, 'Thrown from Get_03', None)

def test_object_Set_method(obj):
    # This method just shifts responsibility to the OrdinarySet function, so all we're really doing here is checking the code
    # path.
    val = obj.Set('testkey', 89, obj)
    assert val == Completion(NORMAL, True, None)
    assert nc(obj.Get('testkey', obj)) == 89

def test_OrdinarySet_01(obj):
    # This one just gets a property description and then passes control to OrdinarySetWithOwnDescriptor. So we're really just
    # checking code paths (both successful, and when catching errors).
    val = OrdinarySet(obj, 'testkey', 67, obj)
    assert val == Completion(NORMAL, True, None)
    assert nc(obj.Get('testkey', obj)) == 67

def test_OrdinarySet_02(obj):
    obj.GetOwnProperty = types.MethodType(lambda _a, _b: ThrowCompletion('Thrown from OrdinarySet_02'), obj)
    val = OrdinarySet(obj, 'testkey', 68, obj)
    assert val == Completion(THROW, 'Thrown from OrdinarySet_02', None)

@pytest.fixture
def child(obj):
    # Makes a child object that has a parent whose prototype is %ObjectPrototype%
    child = ObjectCreate(obj, ['slot'])
    return child

@pytest.fixture
def setprops(child):
    # Sets up properties for the "set" tests.
    parent = nc(child.GetPrototypeOf())
    parent.DefineOwnProperty('not_enumerable',
        PropertyDescriptor(value=542, writable=True, enumerable=False, configurable=True))
    parent.DefineOwnProperty('not_writable',
        PropertyDescriptor(value=543, writable=False, enumerable=True, configurable=True))
    child.DefineOwnProperty('child_nw',
        PropertyDescriptor(value=546, writable=False, enumerable=True, configurable=True))
    child.DefineOwnProperty('normal',
        PropertyDescriptor(value='NORMAL', writable=True, enumerable=True, configurable=True))
    def settest_get(self, new_target):
        return NormalCompletion(self.slot)
    def settest_set(self, new_target, value):
        self.slot = value
        return NormalCompletion(True)
    get_fcn = CreateBuiltinFunction(settest_get, [], realm=surrounding_agent.running_ec.realm)
    set_fcn = CreateBuiltinFunction(settest_set, [], realm=surrounding_agent.running_ec.realm)
    child.DefineOwnProperty('codish',
        PropertyDescriptor(Get=get_fcn, Set=set_fcn, enumerable=True, configurable=True))
    return child

def test_OrdinarySetWithOwnDescriptor_01(setprops):
    # If the fourth arg is None (ownDesc), then the routine follows the prototype chain up to find a matching property and uses
    # the property description there to decide whether the property is writable or not.
    val = OrdinarySetWithOwnDescriptor(setprops, 'not_enumerable', 553, setprops, None) # Ok to write to this one
    assert val == Completion(NORMAL, True, None)
    # Though, when the property got added to the child, it became enumerable.
    after = nc(setprops.GetOwnProperty('not_enumerable'))
    assert (after.value, after.writable, after.enumerable, after.configurable) == (553, True, True, True)

def test_OrdinarySetWithOwnDescriptor_02(setprops):
    # If the fourth arg is None (ownDesc), then the routine follows the prototype chain up to find a matching property and uses
    # the property description there to decide whether the property is writable or not.
    val = OrdinarySetWithOwnDescriptor(setprops, 'not_writable', 562, setprops, None) # This should fail.
    assert val == Completion(NORMAL, False, None)
    # Make sure the property did not get added...
    assert 'not_writable' not in nc(setprops.OwnPropertyKeys())

def test_OrdinarySetWithOwnDescriptor_03(setprops):
    # If the ownDesc is None, and we're setting a property that's not on the prototype chain, it should just work.
    val = OrdinarySetWithOwnDescriptor(setprops, 'tiger', 9003, setprops, None)
    assert val == Completion(NORMAL, True, None)
    after = nc(setprops.GetOwnProperty('tiger'))
    assert (after.value, after.writable, after.enumerable, after.configurable) == (9003, True, True, True)

def test_OrdinarySetWithOwnDescriptor_04(setprops):
    # If ownDesc says this prop is not writable, the write should fail.
    desc = nc(setprops.GetOwnProperty('child_nw'))
    val = OrdinarySetWithOwnDescriptor(setprops, 'child_nw', 577, setprops, desc)
    assert val == Completion(NORMAL, False, None)
    after = nc(setprops.GetOwnProperty('child_nw'))
    assert (after.value, after.writable, after.enumerable, after.configurable) == (546, False, True, True)

def test_OrdinarySetWithOwnDescriptor_05(setprops):
    # If you've made a mistake and the receiver is not an object, bail.
    desc = nc(setprops.GetOwnProperty('normal'))
    val = OrdinarySetWithOwnDescriptor(setprops, 'normal', 'unusual', 89, desc)
    assert val == Completion(NORMAL, False, None)
    after = nc(setprops.GetOwnProperty('normal'))
    assert (after.value, after.writable, after.enumerable, after.configurable) == ('NORMAL', True, True, True)

def test_OrdinarySetWithOwnDescriptor_06(setprops):
    # If you try to trick the engine by altering a writable, it should fail
    desc = nc(setprops.GetOwnProperty('child_nw'))
    desc.writable = True
    val = OrdinarySetWithOwnDescriptor(setprops, 'child_nw', 'bwahaha', setprops, desc)
    assert val == Completion(NORMAL, False, None)
    after = nc(setprops.GetOwnProperty('child_nw'))
    assert (after.value, after.writable, after.enumerable, after.configurable) == (546, False, True, True)

def test_OrdinarySetWithOwnDescriptor_07(setprops):
    # You can't actually change enumerable or configurable or writable with [[Set]]. (And for that matter, value doesn't matter
    # either.)
    desc = PropertyDescriptor(value='odyssey', writable=True, enumerable=False, configurable=False)
    val = OrdinarySetWithOwnDescriptor(setprops, 'normal', 'purple', setprops, desc)
    assert val == Completion(NORMAL, True, None)
    after = nc(setprops.GetOwnProperty('normal'))
    assert (after.value, after.writable, after.enumerable, after.configurable) == ('purple', True, True, True)

def test_OrdinarySetWithOwnDescriptor_08(setprops):
    # You can't change an accessor descriptor to a data descriptor.
    before = nc(setprops.GetOwnProperty('codish'))
    val = OrdinarySetWithOwnDescriptor(setprops, 'codish', 100, setprops,
                                       PropertyDescriptor(value=1, writable=True, enumerable=True, configurable=True))
    assert val == Completion(NORMAL, False, None)
    after = nc(setprops.GetOwnProperty('codish'))
    assert after.is_accessor_descriptor() and not after.is_data_descriptor()
    assert (after.Get, after.Set, after.enumerable, after.configurable) == (before.Get, before.Set, True, True)

def test_OrdinarySetWithOwnDescriptor_09(setprops):
    # If you try to "Set" with an accessor descriptor that doesn't know how to set, fail.
    before = nc(setprops.GetOwnProperty('codish'))
    val = OrdinarySetWithOwnDescriptor(setprops, 'codish', 'testval', setprops, PropertyDescriptor(Set=None))
    assert val == Completion(NORMAL, False, None)
    after = nc(setprops.GetOwnProperty('codish'))
    assert (after.Get, after.Set, after.enumerable, after.configurable) == (before.Get, before.Set, True, True)

def test_OrdinarySetWithOwnDescriptor_10(setprops):
    # Setting an accessor descriptor should work.
    desc = nc(setprops.GetOwnProperty('codish'))
    val = OrdinarySetWithOwnDescriptor(setprops, 'codish', 'testval', setprops, desc)
    assert val == Completion(NORMAL, True, None)
    assert nc(Get(setprops, 'codish')) == 'testval'

def test_OrdinarySetWithOwnDescriptor_11(setprops):
    # Now exception handling. First, if GetPrototypeOf throws.
    setprops.GetPrototypeOf = types.MethodType(lambda _: ThrowCompletion('Thrown from OSWOD_11'), setprops)
    val = OrdinarySetWithOwnDescriptor(setprops, 'elephant', 1, setprops, None)
    assert val == Completion(THROW, 'Thrown from OSWOD_11', None)

def test_OrdinarySetWithOwnDescriptor_12(setprops):
    # If GetOwnProperty throws...
    setprops.GetOwnProperty = types.MethodType(lambda _a, _b: ThrowCompletion('Thrown from OSWOD_12'), setprops)
    val = OrdinarySetWithOwnDescriptor(setprops, 'normal', 1, setprops, None)
    assert val == Completion(THROW, 'Thrown from OSWOD_12', None)

def test_OrdinarySetWithOwnDescriptor_13(setprops):
    # If a custom setter throws
    def custom_setter(self, new_target, value):
        return ThrowCompletion('Thrown from custom_setter')
    set_fcn = CreateBuiltinFunction(custom_setter, [], realm=surrounding_agent.running_ec.realm)
    val = OrdinarySetWithOwnDescriptor(setprops, 'codish', -90, setprops, PropertyDescriptor(Set=set_fcn))
    assert val == Completion(THROW, 'Thrown from custom_setter', None)

@pytest.fixture()
def deletable(obj):
    obj.DefineOwnProperty('normal', PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=True))
    obj.DefineOwnProperty('permanent', PropertyDescriptor(value=9, writable=True, enumerable=True, configurable=False))
    return obj

def test_object_Delete_method(deletable):
    # This just delegates to OrdinaryDelete, so we're just confirming the code path.
    val = deletable.Delete('normal')
    assert val == Completion(NORMAL, True, None)
    assert 'normal' not in nc(deletable.OwnPropertyKeys())

def test_OrdinaryDelete_01(deletable):
    # A run of the mill, normal deletion
    val = OrdinaryDelete(deletable, 'normal')
    assert val == Completion(NORMAL, True, None)
    assert 'normal' not in nc(deletable.OwnPropertyKeys())

def test_OrdinaryDelete_02(deletable):
    # Trying to delete something that's not actually there.
    val = OrdinaryDelete(deletable, 'mystery')
    assert val == Completion(NORMAL, True, None)

def test_OrdinaryDelete_03(deletable):
    # Trying to delete a non-configurable property
    val = OrdinaryDelete(deletable, 'permanent')
    assert val == Completion(NORMAL, False, None)
    assert 'permanent' in nc(deletable.OwnPropertyKeys())

def test_OrdinaryDelete_04(deletable):
    deletable.GetOwnProperty = types.MethodType(lambda _a, _b: ThrowCompletion('Thrown from OrdinaryDelete_04'),
                                                deletable)
    val = OrdinaryDelete(deletable, 'normal')
    assert val == Completion(THROW, 'Thrown from OrdinaryDelete_04', None)

def test_object_OwnPropertyKeys_method(obj):
    # The [[OwnPropertyKeys]] method just delegates to OrdinaryOwnPropertyKeys, so we're just checking the code path here.
    val = obj.OwnPropertyKeys()
    assert val == Completion(NORMAL, [], None)

def test_OrdinaryOwnPropertyKeys_01(obj):
    # There's a bunch of sorting activity that goes on here, so I'm making lots of properties.
    symbol_1 = JSSymbol('Symbol.unittest')
    symbol_2 = JSSymbol('Symbol.elephant')
    big = nc(ToString(2**53 - 1))
    toobig = nc(ToString(2**53))
    for propkey in [ 'first', '100', '-30', 'second', 'third', 'fourth', 'fifth', '   67  ', '67', '-0', '0', '45.5', '0xb22',
                     symbol_1, symbol_2, '88', '0.0', big, toobig, 'last' ]:
        CreateDataProperty(obj, propkey, propkey)

    val = obj.OwnPropertyKeys()
    assert val == Completion(NORMAL, ['0', '67', '88', '100', big, 'first', '-30', 'second', 'third', 'fourth', 'fifth', '   67  ',
                                      '-0', '45.5', '0xb22', '0.0', toobig, 'last', symbol_1, symbol_2], None)

@pytest.mark.parametrize('input, expected', [
    ('0', True),
    ('-0', False),
    (JSSymbol('testsymbol'), False),
    ('notnumeric', False),
    ('9007199254740992', False),
    ('9007199254740991', True),
    ('2.5', False),
    ('351235', True)
])
def test_isIntegerIndex_(input, expected):
    assert isIntegerIndex(input) == expected

def test_ObjectCreate_(realm):
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    child = ObjectCreate(obj, ['testslot', '%crazy%'])

    assert nc(child.GetPrototypeOf()) == obj # validiate prototype chain
    assert hasattr(child, 'testslot')
    assert hasattr(child, '%crazy%')

def test_GetPrototypeFromConstructor_01(realm):
    # For the "normal" case, our constructor object needs to be callable.
    def fake_constructor(this_value, new_target):
        return NormalCompletion(None)
    constructor = CreateBuiltinFunction(fake_constructor, [])
    proto = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    CreateDataProperty(constructor, 'prototype', proto)

    val = GetPrototypeFromConstructor(constructor, '%BooleanPrototype%')
    assert val == Completion(NORMAL, proto, None)

def test_GetPrototypeFromConstructor_02(realm):
    # Constructor is a callable object, but doesn't have a prototype
    def fake_constructor(this_value, new_target):
        return NormalCompletion(None)
    constructor = CreateBuiltinFunction(fake_constructor, [])

    val = GetPrototypeFromConstructor(constructor, '%BooleanPrototype%')
    assert val == Completion(NORMAL, realm.intrinsics['%BooleanPrototype%'], None)

def test_GetPrototypeFromConstructor_03(realm):
    # Constructor is a callable object, but has a prototype with a non-object value
    def fake_constructor(this_value, new_target):
        return NormalCompletion(None)
    constructor = CreateBuiltinFunction(fake_constructor, [])
    CreateDataProperty(constructor, 'prototype', 5.6)

    val = GetPrototypeFromConstructor(constructor, '%BooleanPrototype%')
    assert val == Completion(NORMAL, realm.intrinsics['%BooleanPrototype%'], None)

def test_GetPrototypeFromConstructor_04(realm):
    # The [[Get]] call on the constructor throws.
    def fake_constructor(this_value, new_target):
        return NormalCompletion(None)
    constructor = CreateBuiltinFunction(fake_constructor, [])
    constructor.GetOwnProperty = types.MethodType(lambda _a, _b: ThrowCompletion('Thrown from GPFC_04'), constructor)

    val = GetPrototypeFromConstructor(constructor, '%ObjectPrototype%')
    assert val == Completion(THROW, 'Thrown from GPFC_04', None)

def test_GetPrototypeFromConstructor_05(realm):
    # GetFunctionRealm throws.
    def fake_constructor(this_value, new_target):
        return NormalCompletion(None)
    constructor = CreateBuiltinFunction(fake_constructor, ['ProxyHandler'])
    assert not hasattr(constructor, 'Realm') # This should be here, but isn't. I suspect I have a case mismatch problem. In any case, if this assert starts breaking, just 'del' the Realm attr here instead of asserting.
    constructor.ProxyHandler = JSNull.NULL

    val = GetPrototypeFromConstructor(constructor, '%ObjectPrototype%')
    assert val.ctype == THROW
    assert val.target is None
    assert nc(ToString(val.value)).startswith('TypeError')

def test_OrdinaryCreateFromConstructor_01(realm):
    # Test the successful path. (Most of the work happens in GetPrototypeFromConstructor, so we really don't need to check much
    # here.)
    def fake_constructor(this_value, new_target):
        return NormalCompletion(None)
    constructor = CreateBuiltinFunction(fake_constructor, [])
    proto = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    CreateDataProperty(constructor, 'prototype', proto)

    val = OrdinaryCreateFromConstructor(constructor, '%ObjectPrototype%', ['slotify', 'Lucious'])
    assert (val.ctype, val.target) == (NORMAL, None)
    assert isObject(val.value)
    obj = val.value
    assert hasattr(obj, 'slotify')
    assert hasattr(obj, 'Lucious')
    assert IsExtensible(obj)
    assert obj.Prototype == proto

def test_OrdinaryCreateFromConstructor_02(realm):
    # The [[Get]] call on the constructor throws.
    def fake_constructor(this_value, new_target):
        return NormalCompletion(None)
    constructor = CreateBuiltinFunction(fake_constructor, [])
    constructor.GetOwnProperty = types.MethodType(lambda _a, _b: ThrowCompletion('Thrown from OCFC_02'), constructor)

    val = OrdinaryCreateFromConstructor(constructor, '%ObjectPrototype%')
    assert val == Completion(THROW, 'Thrown from OCFC_02', None)

# 7.3.1 Get(O, P)
def test_Get_01(obj, mocker):
    # All this really does is pitch to the [[Get]] method of the object.
    m = mocker.patch.object(obj, 'Get')
    r = Get(obj, 'propkey')
    assert m.called_once_with('propkey', obj)

# 7.3.2 GetV(V, P)
def test_GetV_01(obj, mocker):
    # When given an object, this is just like Get.
    m = mocker.patch.object(obj, 'Get')
    r = GetV(obj, 'propkey')
    assert m.called_once_with('propkey', obj)

def test_GetV_02(realm):
    # When given a primitive, it gets wrapped in an object first.
    r = GetV(True, 'constructor')
    assert r == Completion(NORMAL, realm.intrinsics['%Boolean%'], None)

def test_GetV_03(realm, mocker):
    # Error path from ToObject
    mocker.patch('ecmascript.ToObject', side_effect=lambda a: ThrowCompletion('test'))
    r = GetV(True, 'constructor')
    assert r == Completion(THROW, 'test', None)

def test_GetV_04(realm):
    # In particular, the null and undefined values can't be converted to objects, so...
    r = GetV(None, 'constructor')
    assert isinstance(r, Completion)
    assert r.ctype == THROW
    assert r.target is None
    assert nc(ToString(r.value)).startswith('TypeError: ')

# 7.3.3 Set(O, P, V, Throw)
# Defers to the object's [[Set]] method, but adds an error on failure if Throw is true.
def test_Set_01(obj, mocker):
    # When Throw is false, just defer to [[Set]]
    m = mocker.patch.object(obj, 'Set')
    r = Set(obj, 'propkey', 100, False)
    assert m.called_once_with('propkey', 100, obj)

def test_Set_02(obj):
    # If the [[Set]] method returns False and Throw is True, an exception is raised
    obj.PreventExtensions()
    r = Set(obj, 'propkey', 100, True)
    assert isinstance(r, Completion)
    assert r.ctype == THROW
    assert r.target is None
    assert nc(ToString(r.value)).startswith('TypeError: ')

def test_Set_03(obj):
    # Just a quick integration test here.
    r = Set(obj, 'propkey', 100, True)
    assert r == Completion(NORMAL, True, None)
    assert nc(Get(obj, 'propkey')) == 100

def test_Set_04(obj, mocker):
    # Check that if the Set Method throws an error, we report it.
    mocker.patch('ecmascript.OrdinarySet', return_value=Completion(THROW, 'test', None))
    r = Set(obj, 'propkey', 100, True)
    assert r == Completion(THROW, 'test', None)

# 7.3.4 CreateDataProperty(O, P, V)
# This just really defers to the object's DefineOwnProperty method
def test_CreateDataProperty_01(obj, mocker):
    m = mocker.patch.object(obj, 'DefineOwnProperty', mocker.Mock(return_value=Completion(NORMAL, 'from test', None)))
    r = CreateDataProperty(obj, 'propkey', 100)
    assert m.called_once_with('propkey', PropertyDescriptor(value=100, writable=True, enumerable=True, configurable=True))
    assert r == Completion(NORMAL, 'from test', None)

# 7.3.5 CreateMethodProperty(O, P, V)
# This also just defers to the object's DefineOwnProperty method
def test_CreateMethodProperty_01(obj, mocker):
    m = mocker.patch.object(obj, 'DefineOwnProperty', mocker.Mock(return_value=Completion(NORMAL, 'from test', None)))
    r = CreateMethodProperty(obj, 'propkey', 100)
    assert m.called_once_with('propkey', PropertyDescriptor(value=100, writable=True, enumerable=False, configurable=True))
    assert r == Completion(NORMAL, 'from test', None)

# 7.3.6 CreateDataPropertyOrThrow(O, P, V)
# Wraps CreateDataProperty. Essentially:
#    If CreateDataProperty returns (not NORMAL, anything, anything), CreateDataPropertyOrThrow should return that same thing.
#    If CreateDataProperty returns (NORMAL, False, anything), CreateDataPropertyOrThrow should return a TypeError
#    Otheriwise, CreateDataProperty returns (NORMAL, True, None)
@pytest.mark.parametrize('cdp_rval, expected', [
    (Completion(THROW, 'test throw', None), Completion(THROW, 'test throw', None)),
    (Completion(NORMAL, True, None), Completion(NORMAL, True, None)),
    (Completion(NORMAL, False, None), 'TypeError'),
])
def test_CreateDataPropertyOrThrow_01(obj, mocker, cdp_rval, expected):
    mocker.patch('ecmascript.CreateDataProperty', return_value=cdp_rval)
    res = CreateDataPropertyOrThrow(obj, 'propkey', 'fudge')
    assert ecmascript.CreateDataProperty.called_once_with(obj, 'propkey', 'fudge')
    if expected == 'TypeError':
        assert isinstance(res, Completion)
        assert res.ctype == THROW
        assert res.target is None
        assert nc(ToString(res.value)).startswith('TypeError: ')
    else:
        assert res == expected

# CreateMethodPropertyOrThrow(O, P, V)
# Wraps CreateMethodProperty. Essentially:
#    If CreateMethodProperty returns (not NORMAL, anything, anything), CreateMethodPropertyOrThrow should return that same thing.
#    If CreateMethodProperty returns (NORMAL, False, anything), CreateMethodPropertyOrThrow should return a TypeError
#    Otheriwise, CreateMethodProperty returns (NORMAL, True, None)
@pytest.mark.parametrize('cdp_rval, expected', [
    (Completion(THROW, 'test throw', None), Completion(THROW, 'test throw', None)),
    (Completion(NORMAL, True, None), Completion(NORMAL, True, None)),
    (Completion(NORMAL, False, None), 'TypeError'),
])
def test_CreateMethodPropertyOrThrow_01(obj, mocker, cdp_rval, expected):
    mocker.patch('ecmascript.CreateMethodProperty', return_value=cdp_rval)
    res = CreateMethodPropertyOrThrow(obj, 'propkey', 'fudge')
    assert ecmascript.CreateMethodProperty.called_once_with(obj, 'propkey', 'fudge')
    if expected == 'TypeError':
        assert isinstance(res, Completion)
        assert res.ctype == THROW
        assert res.target is None
        assert nc(ToString(res.value)).startswith('TypeError: ')
    else:
        assert res == expected

# 7.3.7 DefinePropertyOrThrow ( O, P, desc )
# Wraps DefineOwnProperty method
@pytest.mark.parametrize('dpot_rval, expected', [
    (Completion(THROW, 'test throw', None), Completion(THROW, 'test throw', None)),
    (Completion(NORMAL, True, None), Completion(NORMAL, True, None)),
    (Completion(NORMAL, False, None), 'TypeError'),
])
def test_DefinePropertyOrThrow_01(obj, mocker, dpot_rval, expected):
    m = mocker.patch.object(obj, 'DefineOwnProperty', mocker.Mock(return_value=dpot_rval))
    res = DefinePropertyOrThrow(obj, 'propkey', PropertyDescriptor(value='test'))
    assert m.called_once_with(obj, 'propkey', PropertyDescriptor(value='test'))
    if expected == 'TypeError':
        assert isinstance(res, Completion)
        assert res.ctype == THROW
        assert res.target is None
        assert nc(ToString(res.value)).startswith('TypeError: ')
    else:
        assert res == expected

# 7.3.8 DeletePropertyOrThrow ( O, P )
@pytest.mark.parametrize('dpot_rval, expected', [
    (Completion(THROW, 'test throw', None), Completion(THROW, 'test throw', None)),
    (Completion(NORMAL, True, None), Completion(NORMAL, True, None)),
    (Completion(NORMAL, False, None), 'TypeError'),
])
def test_DeletePropertyOrThrow_01(obj, mocker, dpot_rval, expected):
    m = mocker.patch.object(obj, 'Delete', mocker.Mock(return_value=dpot_rval))
    res = DeletePropertyOrThrow(obj, 'propkey')
    assert m.called_once_with('propkey')
    if expected == 'TypeError':
        assert isinstance(res, Completion)
        assert res.ctype == THROW
        assert res.target is None
        assert nc(ToString(res.value)).startswith('TypeError: ')
    else:
        assert res == expected

# 7.3.9 GetMethod(V, P)
@pytest.mark.parametrize('inp', [JSNull.NULL, None])
def test_GetMethod_01(obj, inp):
    # Undefined & Null return Undefined
    Set(obj, 'something', inp, False)
    res = GetMethod(obj, 'something')
    assert res == Completion(NORMAL, None, None)

def test_GetMethod_02(obj):
    # When the thing isn't a method, throw a TypeError
    Set(obj, 'something', 89, False)
    res = GetMethod(obj, 'something')
    assert isinstance(res, Completion)
    assert res.ctype == THROW
    assert res.target is None
    assert nc(ToString(res.value)).startswith('TypeError: ')

def test_GetMethod_03(obj):
    myfunc = CreateBuiltinFunction(lambda this_value, new_target, arg: arg * 3 + 27, [])
    Set(obj, 'something', myfunc, False)
    res = GetMethod(obj, 'something')
    assert res == Completion(NORMAL, myfunc, None)

def test_GetMethod_04(obj, mocker):
    mocker.patch('ecmascript.GetV', return_value=ThrowCompletion('test throw'))
    res = GetMethod(obj, 'something')
    assert res == Completion(THROW, 'test throw', None)

# 7.3.10 HasProperty(O, P)
# Just wraps the [[HasProperty]] object method
def test_HasProperty_01(obj, mocker):
    m = mocker.patch.object(obj, 'HasProperty', mocker.Mock(return_value=Completion(NORMAL, 'testcase result', None)))
    res = HasProperty(obj, 'propkey')
    assert m.called_once_with(obj, 'propkey')
    assert res == Completion(NORMAL, 'testcase result', None)

# 7.3.11 HasOwnProperty(O, P)
def test_HasOwnProperty_01(obj):
    # a property that's not there.
    res = HasOwnProperty(obj, 'silly')
    assert res == Completion(NORMAL, False, None)

def test_HasOwnProperty_02(obj):
    # a property that is there.
    nc(Set(obj, 'getting_set', 'with_value', False))
    res = HasOwnProperty(obj, 'getting_set')
    assert res == Completion(NORMAL, True, None)

def test_HasOwnProperty_03(obj):
    # when an error is thrown
    obj.GetOwnProperty = types.MethodType(lambda _a, _b: ThrowCompletion('thrown'), obj)
    res = HasOwnProperty(obj, 'silly')
    assert res == Completion(THROW, 'thrown', None)

# 7.3.12 Call ( F, V [, argumentesList ] )
def test_Call_01(realm):
    # Test the not-callable scenario
    res = Call(True, None)
    assert isinstance(res, Completion)
    assert res.ctype == THROW
    assert res.target is None
    assert nc(ToString(res.value)).startswith('TypeError: ')

def test_Call_02(obj, mocker):
    func = CreateBuiltinFunction(lambda thisvalue, newtarget, x: NormalCompletion(x+10), [])
    # Validate that we use the object's [[Call]] method and return its result
    # (and that a missing arglist gets transformed into an empty list)
    m = mocker.patch.object(func, 'Call', mocker.Mock(return_value=Completion(NORMAL, 'test result', None)))
    res = Call(func, obj)
    assert m.called_once_with(func, obj, [])
    assert res == Completion(NORMAL, 'test result', None)

def test_Call_03(obj, mocker):
    # Same thing, but with args this time.
    func = CreateBuiltinFunction(lambda thisvalue, newtarget, x, y: NormalCompletion(x+10*y), [])
    m = mocker.patch.object(func, 'Call', mocker.Mock(return_value=Completion(NORMAL, 'test result', None)))
    res = Call(func, obj, [6, 8])
    assert m.called_once_with(func, obj, [6, 8])
    assert res == Completion(NORMAL, 'test result', None)

# 7.3.13 Construct ( F [, argumentsList [, newTaget ] ] )
def test_Construct_01(realm, mocker):
    # The one-argument case
    cstr = realm.intrinsics['%Boolean%']
    m = mocker.patch.object(cstr, 'Construct', mocker.Mock(return_value=Completion(NORMAL, 'test result', None)))
    res = Construct(cstr)
    assert m.called_once_with([], cstr)
    assert res == Completion(NORMAL, 'test result', None)

def test_Construct_02(realm, mocker):
    # The two-argument case
    cstr = realm.intrinsics['%Boolean%']
    m = mocker.patch.object(cstr, 'Construct', mocker.Mock(return_value=Completion(NORMAL, 'test result', None)))
    res = Construct(cstr, [True, False])
    assert m.called_once_with([True, False], cstr)
    assert res == Completion(NORMAL, 'test result', None)

def test_Construct_03(realm, mocker):
    # The three-argument case
    cstr = realm.intrinsics['%Boolean%']
    new_target = realm.intrinsics['%Number%']
    m = mocker.patch.object(cstr, 'Construct', mocker.Mock(return_value=Completion(NORMAL, 'test result', None)))
    res = Construct(cstr, [True], new_target)
    assert m.called_once_with([True], new_target)
    assert res == Completion(NORMAL, 'test result', None)

# 7.3.14 SetIntegrityLevel ( O, level )
# If level is 'sealed':
#    - Object is marked not-extensible
#    - All exisiting properties are marked not-configurable
#    - All other property attributes remain unchanged.
# If level is 'frozen':
#    - Object is marked not-extensible
#    - All existing properties are marked not-configurable
#    - All data descriptors are marked not-writable
@pytest.fixture
def props():
    return [
        PropertyDescriptor(value='p1', writable=True, enumerable=True, configurable=True),
        PropertyDescriptor(value='p2', writable=False, enumerable=True, configurable=True),
        PropertyDescriptor(value='p3', writable=True, enumerable=False, configurable=True),
        PropertyDescriptor(value='p4', writable=False, enumerable=False, configurable=True),
        PropertyDescriptor(value='p5', writable=True, enumerable=True, configurable=False),
        PropertyDescriptor(value='p6', writable=False, enumerable=True, configurable=False),
        PropertyDescriptor(value='p7', writable=True, enumerable=False, configurable=False),
        PropertyDescriptor(value='p8', writable=False, enumerable=False, configurable=False),
        PropertyDescriptor(Get=None, Set=None, enumerable=True, configurable=True),
        PropertyDescriptor(Get=None, Set=None, enumerable=False, configurable=True),
        PropertyDescriptor(Get=None, Set=None, enumerable=True, configurable=False),
        PropertyDescriptor(Get=None, Set=None, enumerable=False, configurable=False)
    ]

def test_SetIntegrityLevel_01(obj, props):
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f'property_{idx+1}', p)

    res = SetIntegrityLevel(obj, 'sealed')

    assert res == Completion(NORMAL, True, None)
    assert not nc(IsExtensible(obj))
    after = [nc(OrdinaryGetOwnProperty(obj, f'property_{idx}')) for idx in range(1,len(props)+1)]
    for result, inp in zip_longest(after, props):
        assert not result.configurable
        assert result.enumerable == inp.enumerable
        assert (result.writable == inp.writable) if hasattr(inp, 'writable') else True
        assert (result.value == inp.value) if hasattr(inp, 'value') else True
        assert (result.Get == inp.Get) if hasattr(inp, 'Get') else True
        assert (result.Set == inp.Set) if hasattr(inp, 'Set') else True

def test_SetIntegrityLevel_02(obj, props):
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f'property_{idx+1}', p)

    res = SetIntegrityLevel(obj, 'frozen')

    assert res == Completion(NORMAL, True, None)
    assert not nc(IsExtensible(obj))
    after = [nc(OrdinaryGetOwnProperty(obj, f'property_{idx}')) for idx in range(1,len(props)+1)]
    for result, inp in zip_longest(after, props):
        assert not result.configurable
        assert result.enumerable == inp.enumerable
        assert (not result.writable) if hasattr(inp, 'writable') else True
        assert (result.value == inp.value) if hasattr(inp, 'value') else True
        assert (result.Get == inp.Get) if hasattr(inp, 'Get') else True
        assert (result.Set == inp.Set) if hasattr(inp, 'Set') else True

def test_SetIntegrityLevel_03(obj, props, mocker):
    # If the [[PreventExtensions]] method returns false, we just bail and return false as well.
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f'property_{idx+1}', p)
    mocker.patch.object(obj, 'PreventExtensions', mocker.Mock(return_value=False))

    res = SetIntegrityLevel(obj, 'sealed')

    assert res == Completion(NORMAL, False, None)
    assert nc(IsExtensible(obj))
    after = [nc(OrdinaryGetOwnProperty(obj, f'property_{idx}')) for idx in range(1,len(props)+1)]
    for result, inp in zip_longest(after, props):
        assert result.configurable == inp.configurable
        assert result.enumerable == inp.enumerable
        assert (result.writable == inp.writable) if hasattr(inp, 'writable') else True
        assert (result.value == inp.value) if hasattr(inp, 'value') else True
        assert (result.Get == inp.Get) if hasattr(inp, 'Get') else True
        assert (result.Set == inp.Set) if hasattr(inp, 'Set') else True

def test_SetIntegrityLevel_04(obj, mocker):
    # If [[PreventExtensions]] throws, we exit with that
    mocker.patch.object(obj, 'PreventExtensions', mocker.Mock(return_value=Completion(THROW, 'test throw', None)))
    res = SetIntegrityLevel(obj, 'sealed')
    assert res == Completion(THROW, 'test throw', None)

def test_SetIntegrityLevel_05(obj, mocker):
    # If [[OwnPropertyKeys]] throws, we exit with that.
    mocker.patch.object(obj, 'OwnPropertyKeys', mocker.Mock(return_value=Completion(THROW, 'test throw', None)))
    res = SetIntegrityLevel(obj, 'sealed')
    assert res == Completion(THROW, 'test throw', None)

def test_SetIntegrityLevel_06(obj, mocker):
    # If [[GetOwnProperty]] throws, and we're freezing, we return that throw.
    Set(obj, 'random', 77, False) # Need to have a property to look up!
    mocker.patch.object(obj, 'GetOwnProperty', mocker.Mock(return_value=Completion(THROW, 'test throw', None)))
    res = SetIntegrityLevel(obj, 'frozen')
    assert res == Completion(THROW, 'test throw', None)

def test_SetIntegrityLevel_07(obj, mocker, props):
    # If DefinePropertyOrThrow throws, in the sealed case, rethrow
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f'property_{idx+1}', p)
    mocker.patch('ecmascript.DefinePropertyOrThrow', return_value=Completion(THROW, 'test throw', None))

    res = SetIntegrityLevel(obj, 'sealed')
    assert res == Completion(THROW, 'test throw', None)

def test_SetIntegrityLevel_08(obj, mocker, props):
    # If DefinePropertyOrThrow throws, in the frozen case, rethrow
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f'property_{idx+1}', p)
    mocker.patch('ecmascript.DefinePropertyOrThrow', return_value=Completion(THROW, 'test throw', None))

    res = SetIntegrityLevel(obj, 'frozen')
    assert res == Completion(THROW, 'test throw', None)

def test_SetIntegrityLevel_09(obj, mocker, props):
    # Test a particular branch I'm not sure how to really bring about -- when we can't
    # actually look up a property even though it's in the keys list, we just skip it.
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f'property_{idx+1}', p)
    mocker.patch.object(obj, 'OwnPropertyKeys', mocker.Mock(side_effect=lambda: ['mystery'] + OrdinaryOwnPropertyKeys(obj)))

    res = SetIntegrityLevel(obj, 'frozen')

    assert res == Completion(NORMAL, True, None)
    assert not nc(IsExtensible(obj))
    after = [nc(OrdinaryGetOwnProperty(obj, f'property_{idx}')) for idx in range(1,len(props)+1)]
    for result, inp in zip_longest(after, props):
        assert not result.configurable
        assert result.enumerable == inp.enumerable
        assert (not result.writable) if hasattr(inp, 'writable') else True
        assert (result.value == inp.value) if hasattr(inp, 'value') else True
        assert (result.Get == inp.Get) if hasattr(inp, 'Get') else True
        assert (result.Set == inp.Set) if hasattr(inp, 'Set') else True
    # Make sure it didn't get added
    assert nc(OrdinaryGetOwnProperty(obj, 'mystery')) is None

# 7.3.15 TestIntegrityLevel(O, level)
@pytest.mark.parametrize('level', ['sealed', 'frozen'])
def test_TestIntegrityLevel_01(obj, mocker, level):
    # If obj is extensible, we just return False for all levels, and don't actually check any of the properties
    opk = mocker.patch.object(obj, 'OwnPropertyKeys')
    gop = mocker.patch.object(obj, 'GetOwnProperty')

    res = TestIntegrityLevel(obj, level)
    assert res == Completion(NORMAL, False, None)
    assert opk.not_called()
    assert gop.not_called()

@pytest.mark.parametrize('level', ['sealed', 'frozen'])
def test_TestIntegrityLevel_02(obj, props, level):
    # check actual frozen or sealed.
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f'property_{idx+1}', p)
    SetIntegrityLevel(obj, level)

    res = TestIntegrityLevel(obj, level)
    assert res == Completion(NORMAL, True, None)

@pytest.mark.parametrize('level', ['sealed', 'frozen'])
def test_TestIntegrityLevel_03(obj, props, level):
    # test not actually frozen or sealed (but not extensible)
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f'property_{idx+1}', p)
    obj.PreventExtensions()

    res = TestIntegrityLevel(obj, level)
    assert res == Completion(NORMAL, False, None)

def test_TestIntegrityLevel_04(obj, mocker):
    # IsExtensible throws
    mocker.patch('ecmascript.IsExtensible', return_value=Completion(THROW, 'test throw', None))
    res = TestIntegrityLevel(obj, 'sealed')

    assert res == Completion(THROW, 'test throw', None)

def test_TestIntegrityLevel_05(obj, mocker):
    # OwnPropertyKeys method throws
    obj.PreventExtensions()
    mocker.patch.object(obj, 'OwnPropertyKeys', mocker.Mock(return_value=Completion(THROW, 'test throw', None)))

    res = TestIntegrityLevel(obj, 'sealed')

    assert res == Completion(THROW, 'test throw', None)

def test_TestIntegrityLevel_06(obj, mocker, props):
    # GetOwnProperty method throws
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f'property_{idx+1}', p)
    obj.PreventExtensions()
    mocker.patch.object(obj, 'GetOwnProperty', mocker.Mock(return_value=Completion(THROW, 'test throw', None)))

    res = TestIntegrityLevel(obj, 'sealed')

    assert res == Completion(THROW, 'test throw', None)

def test_TestIntegrityLevel_07(obj, props):
    # A sealed object is not necessarily frozen
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f'property_{idx+1}', p)
    SetIntegrityLevel(obj, 'sealed')

    res = TestIntegrityLevel(obj, 'frozen')

    assert res == Completion(NORMAL, False, None)

def test_TestIntegrityLevel_08(obj, mocker, props):
    # Test a particular branch I'm not sure how to really bring about -- when we can't
    # actually look up a property even though it's in the keys list, we just skip it.
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f'property_{idx+1}', p)
    SetIntegrityLevel(obj, 'frozen')
    mocker.patch.object(obj, 'OwnPropertyKeys', mocker.Mock(side_effect=lambda: ['mystery'] + OrdinaryOwnPropertyKeys(obj)))

    res = TestIntegrityLevel(obj, 'frozen')

    assert res == Completion(NORMAL, True, None)

# 7.3.16 CreateArrayFromList ( elements )
@pytest.mark.xfail(reason='Waiting for ArrayCreate to work')
@pytest.mark.parametrize('lst', [ [], [5, 1221, 'boop']])
def test_CreateArrayFromList_01(realm, lst):
    ary = CreateArrayFromList(lst)

    assert isinstance(ary, ArrayObject)
    assert nc(Get(ary, 'length')) == len(lst)
    for idx, value in enumerate(lst):
        assert nc(Get(ary, nc(ToString(idx)))) == value

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
    def test_get(self):
        return NormalCompletion(self.test_slot)
    def test_set(self, val):
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

    def test_get(self):
        return NormalCompletion('nonsense')
    def test_set(self, val):
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
    def test_get(self):
        return NormalCompletion('nonsense')
    def test_set(self, val):
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
    def test_get(self):
        return NormalCompletion('nonsense')
    def test_set(self, val):
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
    def test_get(self):
        return NormalCompletion('nonsense')
    def test_set(self, val):
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
    def test_get(self):
        return NormalCompletion('nonsense')
    def test_set(self, val):
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
    obj.GetOwnProperty = types.MethodType(lambda _a, _b: ThrowCompletion('Thrown from 04'), 'GetOwnProperty')

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
    obj.GetOwnProperty = types.MethodType(lambda _a, _b: ThrowCompletion('Thrown from 04'), 'GetOwnProperty')

    cr = obj.HasProperty('toElephant')
    assert cr == Completion(THROW, 'Thrown from 04', None)

def test_object_HasProperty_method_05(realm):
    # Exceptions from [[GetPrototypeOf]] bubble up.
    obj = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    obj.GetPrototypeOf = types.MethodType(lambda _: ThrowCompletion('Thrown from 05'), 'GetPrototypeOf')

    cr = obj.HasProperty('toElephant')
    assert cr == Completion(THROW, 'Thrown from 05', None)

@pytest.fixture
def get_tree(realm):
    parent = ObjectCreate(realm.intrinsics['%ObjectPrototype%'])
    child = ObjectCreate(parent)
    CreateDataProperty(parent, 'onparent', 100)
    CreateDataProperty(child, 'onchild', -100)
    def special_get(self):
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
    get_tree.GetOwnProperty = types.MethodType(lambda _a, _b: ThrowCompletion('Thrown from Get_01'), 'GetOwnProperty')
    val = get_tree.Get('thimble', 345)
    assert val == Completion(THROW, 'Thrown from Get_01', None)

def test_object_Get_method_02(get_tree):
    # An exception in the [[GetPrototypeOf]] code path...
    get_tree.GetPrototypeOf = types.MethodType(lambda _: ThrowCompletion('Thrown from Get_02'), 'GetPrototypeOf')
    val = get_tree.Get('needle', 345)
    assert val == Completion(THROW, 'Thrown from Get_02', None)

def test_object_Get_method_03(get_tree):
    # Now try an exception from the parent's recursion.
    parent = nc(get_tree.GetPrototypeOf())
    parent.GetOwnProperty = types.MethodType(lambda _a, _b: ThrowCompletion('Thrown from Get_03'), 'GetOwnProperty')
    val = get_tree.Get('haystack', 345)
    assert val == Completion(THROW, 'Thrown from Get_03', None)

# pylint: disable=unused-wildcard-import

from itertools import zip_longest

import pytest

from ecmascript.ecmascript import *
import ecmascript.ecmascript  # Need it this way for the mocker fixture


@pytest.fixture
def obj(realm):
    return ObjectCreate(realm.intrinsics["%ObjectPrototype%"])


@pytest.fixture
def object_chain(realm):
    # An ancestor object
    ancestor = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    # A parent object
    parent = ObjectCreate(ancestor)
    # A child object
    child = ObjectCreate(parent)

    return {"realm": realm, "ancestor": ancestor, "parent": parent, "child": child}


def test_Property_repr():
    p = JSObject.Property(value=100, enumerable=True, polio_vaccine="useful")
    s = repr(p)
    assert "value=100" in s
    assert "enumerable=True" in s
    assert "polio_vaccine" not in s


def test_object_GetPrototypeOf_method(object_chain):
    assert object_chain["child"].GetPrototypeOf() == object_chain["parent"]
    assert object_chain["parent"].GetPrototypeOf() == object_chain["ancestor"]


@pytest.mark.parametrize("extensible,result", [(False, True), (True, True)])
def test_object_SetPrototypeOf_method_01(realm, extensible, result):
    # If you set the prototype of an object to the same thing it already has, this should return success, independent
    # of the value of the object's [[Extensible]] property.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    obj.Extensible = extensible
    assert obj.SetPrototypeOf(realm.intrinsics["%ObjectPrototype%"]) == result  # pylint: disable=not-callable
    assert obj.GetPrototypeOf() == realm.intrinsics["%ObjectPrototype%"]  # pylint: disable=not-callable


def test_object_SetPrototypeOf_method_02(realm):
    # On the other hand, if [[Extensible]] is False, nothing can _change_ the prototype.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    obj.Extensible = False
    assert obj.SetPrototypeOf(realm.intrinsics["%FunctionPrototype%"]) == False  # pylint: disable=not-callable
    assert obj.GetPrototypeOf() == realm.intrinsics["%ObjectPrototype%"]  # pylint: disable=not-callable


def test_object_SetPrototypeOf_method_03(object_chain):
    realm = object_chain["realm"]
    # We should not be able to make prototype chain loops.
    assert object_chain["ancestor"].SetPrototypeOf(object_chain["child"]) == False
    assert object_chain["ancestor"].GetPrototypeOf() == realm.intrinsics["%ObjectPrototype%"]


def test_object_SetPrototypeOf_method_04(object_chain):
    realm = object_chain["realm"]
    # On the other hand, if we don't detect a loop, setting a prototype is fine.
    assert object_chain["child"].SetPrototypeOf(realm.intrinsics["%FunctionPrototype%"]) == True
    assert object_chain["child"].GetPrototypeOf() == realm.intrinsics["%FunctionPrototype%"]


def test_object_SetPrototypeOf_method_05(object_chain):
    realm = object_chain["realm"]
    # If an object in the prototype chain has a nonstandard GetPrototypeOf, we abort the loop detector and just set it.
    object_chain["parent"].GetPrototypeOf = types.MethodType(
        lambda _: realm.intrinsics["%FunctionPrototype%"], object_chain["parent"]
    )

    assert object_chain["ancestor"].SetPrototypeOf(object_chain["child"]) == True
    assert object_chain["ancestor"].GetPrototypeOf() == object_chain["child"]


@pytest.mark.parametrize("input, expected", [(True, True), (False, False)])
def test_object_IsExtensible_method(realm, input, expected):
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    obj.Extensible = input

    assert obj.IsExtensible() == expected


def test_object_PreventExtensions_method(realm):
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    assert obj.IsExtensible()
    assert obj.PreventExtensions() == True
    assert not obj.IsExtensible()


def test_object_GetOwnProperty_method_01(realm):
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    # If O doesn't have the property, returns undefined.
    assert obj.GetOwnProperty("not_here") == None


def test_object_GetOwnProperty_method_02(realm):
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    desc = PropertyDescriptor(value=100, writable=True, enumerable=True, configurable=True)
    DefinePropertyOrThrow(obj, "testkey", desc)

    cr = obj.GetOwnProperty("testkey")
    assert isinstance(cr, PropertyDescriptor)
    assert cr.is_data_descriptor() and not cr.is_accessor_descriptor()
    assert (cr.value, cr.writable, cr.enumerable, cr.configurable) == (100, True, True, True)


def test_object_GetOwnProperty_method_03(realm):
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"], ["test_slot"])

    def test_get(self, new_target):
        return self.test_slot

    def test_set(self, new_target, val):
        self.test_slot = val
        return None

    get_func = CreateBuiltinFunction(test_get, [], realm)
    set_func = CreateBuiltinFunction(test_set, [], realm)
    desc = PropertyDescriptor(Get=get_func, Set=set_func, enumerable=False, configurable=True)
    DefinePropertyOrThrow(obj, "testkey", desc)

    cr = obj.GetOwnProperty("testkey")
    assert isinstance(cr, PropertyDescriptor)
    assert not cr.is_data_descriptor() and cr.is_accessor_descriptor()
    assert (cr.Get, cr.Set, cr.enumerable, cr.configurable) == (get_func, set_func, False, True)


def test_ValidateAndApplyPropertyDescriptor_01(realm):
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    # If current == None, and not extensible return False.
    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", False, PropertyDescriptor(), None)
    assert cr == False


def test_ValidateAndApplyPropertyDescriptor_02(realm):
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    # If current == None, and the descriptor is empty, we make a default property.
    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", True, PropertyDescriptor(), None)
    assert cr == True
    pdesc = obj.GetOwnProperty("testkey")
    assert pdesc.is_data_descriptor() and not pdesc.is_accessor_descriptor()
    assert (pdesc.value, pdesc.writable, pdesc.enumerable, pdesc.configurable) == (None, False, False, False)


def test_ValidateAndApplyPropertyDescriptor_03(realm):
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    # If current == None, and the descriptor is full, we make a copy.
    cr = ValidateAndApplyPropertyDescriptor(
        obj, "testkey", True, PropertyDescriptor(value=102, writable=True, enumerable=True, configurable=True), None
    )
    assert cr == True
    pdesc = obj.GetOwnProperty("testkey")
    assert pdesc.is_data_descriptor() and not pdesc.is_accessor_descriptor()
    assert (pdesc.value, pdesc.writable, pdesc.enumerable, pdesc.configurable) == (102, True, True, True)


def test_ValidateAndApplyPropertyDescriptor_04(realm):
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    # If current == None, and the descriptor is partial, the rest gets filled with defaults.
    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", True, PropertyDescriptor(Get=None, enumerable=True), None)
    assert cr == True
    pdesc = obj.GetOwnProperty("testkey")
    assert not pdesc.is_data_descriptor() and pdesc.is_accessor_descriptor()
    assert (pdesc.Get, pdesc.Set, pdesc.enumerable, pdesc.configurable) == (None, None, True, False)


def test_ValidateAndApplyPropertyDescriptor_05(realm):
    # If current is None, obj is None, and extensible is True, it doesn't matter what the descriptor has. We just
    # return True.
    cr = ValidateAndApplyPropertyDescriptor(None, None, True, None, None)
    assert cr == True


def test_ValidateAndApplyPropertyDescriptor_06(realm):
    # Current has a value and the descriptor is empty: it's all good.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    CreateDataProperty(obj, "testkey", 89)
    current = obj.GetOwnProperty("testkey")

    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", True, PropertyDescriptor(), current)
    assert cr == True
    # Nothing should have been changed:
    after = obj.GetOwnProperty("testkey")
    assert (current.value, current.writable, current.enumerable, current.configurable) == (
        after.value,
        after.writable,
        after.enumerable,
        after.configurable,
    )


def test_ValidateAndApplyPropertyDescriptor_07(realm):
    # If the current property is not configurable, we can't change it to *be* configurable.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    DefinePropertyOrThrow(
        obj, "testkey", PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=False)
    )
    current = obj.GetOwnProperty("testkey")

    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", True, PropertyDescriptor(configurable=True), current)
    assert cr == False
    # Nothing should have been changed:
    after = obj.GetOwnProperty("testkey")
    assert (current.value, current.writable, current.enumerable, current.configurable) == (
        after.value,
        after.writable,
        after.enumerable,
        after.configurable,
    )


def test_ValidateAndApplyPropertyDescriptor_08(realm):
    # If the current property is not configurable, we can't change its enumerability
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    DefinePropertyOrThrow(
        obj, "testkey", PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=False)
    )
    current = obj.GetOwnProperty("testkey")

    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", True, PropertyDescriptor(enumerable=False), current)
    assert cr == False
    # Nothing should have been changed:
    after = obj.GetOwnProperty("testkey")
    assert (current.value, current.writable, current.enumerable, current.configurable) == (
        after.value,
        after.writable,
        after.enumerable,
        after.configurable,
    )


def test_ValidateAndApplyPropertyDescriptor_09(realm):
    # If the configurable/enumerable flags are ok, but there's nothing else, just be true.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    DefinePropertyOrThrow(
        obj, "testkey", PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=False)
    )
    current = obj.GetOwnProperty("testkey")

    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", True, PropertyDescriptor(enumerable=True), current)
    assert cr == True
    # Nothing should have been changed:
    after = obj.GetOwnProperty("testkey")
    assert (current.value, current.writable, current.enumerable, current.configurable) == (
        after.value,
        after.writable,
        after.enumerable,
        after.configurable,
    )


def test_ValidateAndApplyPropertyDescriptor_10(realm):
    # If the current prop is not configurable, switching from data to accessor isn't allowed (or vice versa, for that
    # matter).
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    DefinePropertyOrThrow(
        obj, "testkey", PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=False)
    )
    current = obj.GetOwnProperty("testkey")

    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", True, PropertyDescriptor(Get=None, Set=None), current)
    assert cr == False
    # Nothing should have been changed:
    after = obj.GetOwnProperty("testkey")
    assert (current.value, current.writable, current.enumerable, current.configurable) == (
        after.value,
        after.writable,
        after.enumerable,
        after.configurable,
    )


def test_ValidateAndApplyPropertyDescriptor_11(realm):
    # Switch from a data descriptor to an accessor descriptor
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    DefinePropertyOrThrow(
        obj, "testkey", PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=True)
    )
    current = obj.GetOwnProperty("testkey")

    def test_get(self, new_target):
        return "nonsense"

    def test_set(self, new_target, val):
        return None

    get_func = CreateBuiltinFunction(test_get, [], realm)
    set_func = CreateBuiltinFunction(test_set, [], realm)
    desc = PropertyDescriptor(Get=get_func, Set=set_func, enumerable=False, configurable=True)

    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", True, desc, current)
    assert cr == True
    after = obj.GetOwnProperty("testkey")
    assert after.is_accessor_descriptor() and not after.is_data_descriptor()
    assert (after.Get, after.Set, after.enumerable, after.configurable) == (get_func, set_func, False, True)


def test_ValidateAndApplyPropertyDescriptor_12(realm):
    # Switch from an accessor descriptor to a data descriptor
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    def test_get(self, new_target):
        return "nonsense"

    def test_set(self, new_target, val):
        return None

    get_func = CreateBuiltinFunction(test_get, [], realm)
    set_func = CreateBuiltinFunction(test_set, [], realm)
    DefinePropertyOrThrow(
        obj, "testkey", PropertyDescriptor(Get=get_func, Set=set_func, enumerable=True, configurable=True)
    )
    current = obj.GetOwnProperty("testkey")

    desc = PropertyDescriptor(value=33, writable=True, enumerable=False, configurable=True)

    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", True, desc, current)
    assert cr == True
    after = obj.GetOwnProperty("testkey")
    assert not after.is_accessor_descriptor() and after.is_data_descriptor()
    assert (after.value, after.writable, after.enumerable, after.configurable) == (33, True, False, True)


def test_ValidateAndApplyPropertyDescriptor_13(realm):
    # If the property is not configurable, changing from "not writable" to "writable" is not allowed.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    DefinePropertyOrThrow(
        obj, "testkey", PropertyDescriptor(value=10, writable=False, enumerable=True, configurable=False)
    )
    current = obj.GetOwnProperty("testkey")

    desc = PropertyDescriptor(writable=True)
    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", True, desc, current)
    assert cr == False
    # Nothing should have been changed:
    after = obj.GetOwnProperty("testkey")
    assert (current.value, current.writable, current.enumerable, current.configurable) == (
        after.value,
        after.writable,
        after.enumerable,
        after.configurable,
    )


def test_ValidateAndApplyPropertyDescriptor_14(realm):
    # But: going from "writable" to "not writable" is just fine.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    DefinePropertyOrThrow(
        obj, "testkey", PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=False)
    )
    current = obj.GetOwnProperty("testkey")

    desc = PropertyDescriptor(writable=False)
    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", True, desc, current)
    assert cr == True
    # Only writable has changed:
    after = obj.GetOwnProperty("testkey")
    assert (current.value, False, current.enumerable, current.configurable) == (
        after.value,
        after.writable,
        after.enumerable,
        after.configurable,
    )


def test_ValidateAndApplyPropertyDescriptor_15(realm):
    # If the property is not configurable, nor writable, then changing the value is not allowed.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    DefinePropertyOrThrow(
        obj, "testkey", PropertyDescriptor(value=10, writable=False, enumerable=True, configurable=False)
    )
    current = obj.GetOwnProperty("testkey")

    desc = PropertyDescriptor(value=11)
    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", True, desc, current)
    assert cr == False
    # Nothing should have been changed:
    after = obj.GetOwnProperty("testkey")
    assert (current.value, current.writable, current.enumerable, current.configurable) == (
        after.value,
        after.writable,
        after.enumerable,
        after.configurable,
    )


def test_ValidateAndApplyPropertyDescriptor_16(realm):
    # If the property is not configurable, nor writable, setting the value but not changing it is fine.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    DefinePropertyOrThrow(
        obj, "testkey", PropertyDescriptor(value=10, writable=False, enumerable=True, configurable=False)
    )
    current = obj.GetOwnProperty("testkey")

    desc = PropertyDescriptor(value=10)
    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", True, desc, current)
    assert cr == True
    # Nothing should have been changed:
    after = obj.GetOwnProperty("testkey")
    assert (current.value, current.writable, current.enumerable, current.configurable) == (
        after.value,
        after.writable,
        after.enumerable,
        after.configurable,
    )


def test_ValidateAndApplyPropertyDescriptor_17(realm):
    # For Accessor descriptors, changing Get or Set is not allowed if the property is not configurable
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    def test_get(self, new_target):
        return "nonsense"

    def test_set(self, new_target, val):
        return None

    get_func = CreateBuiltinFunction(test_get, [], realm)
    set_func = CreateBuiltinFunction(test_set, [], realm)
    DefinePropertyOrThrow(
        obj, "testkey", PropertyDescriptor(Get=get_func, Set=set_func, enumerable=True, configurable=False)
    )
    current = obj.GetOwnProperty("testkey")

    desc = PropertyDescriptor(Get=None)
    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", True, desc, current)
    assert cr == False
    # Nothing should have been changed:
    after = obj.GetOwnProperty("testkey")
    assert (current.Get, current.Set, current.enumerable, current.configurable) == (
        after.Get,
        after.Set,
        after.enumerable,
        after.configurable,
    )


def test_ValidateAndApplyPropertyDescriptor_18(realm):
    # For Accessor descriptors, changing Get or Set is not allowed if the property is not configurable
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    def test_get(self, new_target):
        return "nonsense"

    def test_set(self, new_target, val):
        return None

    get_func = CreateBuiltinFunction(test_get, [], realm)
    set_func = CreateBuiltinFunction(test_set, [], realm)
    DefinePropertyOrThrow(
        obj, "testkey", PropertyDescriptor(Get=get_func, Set=set_func, enumerable=True, configurable=False)
    )
    current = obj.GetOwnProperty("testkey")

    desc = PropertyDescriptor(Set=None)
    cr = ValidateAndApplyPropertyDescriptor(obj, "testkey", True, desc, current)
    assert cr == False
    # Nothing should have been changed:
    after = obj.GetOwnProperty("testkey")
    assert (current.Get, current.Set, current.enumerable, current.configurable) == (
        after.Get,
        after.Set,
        after.enumerable,
        after.configurable,
    )


def test_ValidateAndApplyPropertyDescriptor_19(realm):
    # For non-configurable Accessor descriptors, setting Get or Set to their current values is fine.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    def test_get(self, new_target):
        return "nonsense"

    def test_set(self, new_target, val):
        return None

    get_func = CreateBuiltinFunction(test_get, [], realm)
    set_func = CreateBuiltinFunction(test_set, [], realm)
    DefinePropertyOrThrow(
        obj, "testkey", PropertyDescriptor(Get=get_func, Set=set_func, enumerable=True, configurable=False)
    )
    current = obj.GetOwnProperty("testkey")

    desc = PropertyDescriptor(Get=get_func, Set=set_func)
    ValidateAndApplyPropertyDescriptor(obj, "testkey", True, desc, current)
    # Nothing should have been changed:
    after = obj.GetOwnProperty("testkey")
    assert (current.Get, current.Set, current.enumerable, current.configurable) == (
        after.Get,
        after.Set,
        after.enumerable,
        after.configurable,
    )


def test_ValidateAndApplyPropertyDescriptor_20(realm):
    # A validation request with an data descriptor and no "current" just returns True
    desc = PropertyDescriptor(value="foo")
    cr = ValidateAndApplyPropertyDescriptor(None, "myprop", True, desc, None)
    assert cr == True


def test_ValidateAndApplyPropertyDescriptor_21(realm):
    # Switching from a data desriptor to an accessor descriptor for validation only is ok.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    def test_get(self, new_target):
        return "nonsense"

    get_func = CreateBuiltinFunction(test_get, [], realm)
    DefinePropertyOrThrow(
        obj, "testkey", PropertyDescriptor(value="hoop", writable=True, enumerable=True, configurable=True)
    )
    current = obj.GetOwnProperty("testkey")

    desc = PropertyDescriptor(Get=get_func)
    cr = ValidateAndApplyPropertyDescriptor(None, "testkey", True, desc, current)
    assert cr == True


def test_ValidateAndApplyPropertyDescriptor_22(realm):
    # Switching from an accessor desriptor to a data descriptor for validation only is ok.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    def test_get(self, new_target):
        return "nonsense"

    get_func = CreateBuiltinFunction(test_get, [], realm)
    DefinePropertyOrThrow(obj, "testkey", PropertyDescriptor(Get=get_func, enumerable=True, configurable=True))
    current = obj.GetOwnProperty("testkey")

    desc = PropertyDescriptor(value="bar")
    cr = ValidateAndApplyPropertyDescriptor(None, "testkey", True, desc, current)
    assert cr == True


def test_ValidateAndApplyPropertyDescriptor_23(realm):
    # Switch an accessor property
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    def test_get(self, new_target):
        return "nonsense"

    get_func = CreateBuiltinFunction(test_get, [], realm)
    DefinePropertyOrThrow(obj, "testkey", PropertyDescriptor(Get=get_func, enumerable=True, configurable=True))
    current = obj.GetOwnProperty("testkey")

    def test_get2(self, new_target):
        return "alternate"

    get_func2 = CreateBuiltinFunction(test_get2, [], realm)
    desc = PropertyDescriptor(Get=get_func2)
    cr = ValidateAndApplyPropertyDescriptor(None, "testkey", True, desc, current)
    assert cr == True


# [[DefineOwnProperty]] on objects: if we assume ValidateAndApplyPropertyDescriptor works properly,
# all we care about here:
#   * Ensuring that we pass the Extensible property correctly;
#   * Ensuring the arguments to our method (the key and the descriptor) get to where they need to go;
#   * Ensuring the exception paths are exercised
def test_object_DefineOwnProperty_method_01(realm):
    # We can replace properties on extensible objects
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    DefinePropertyOrThrow(
        obj, "testkey", PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=True)
    )

    desc = PropertyDescriptor(value=15)
    cr = obj.DefineOwnProperty("testkey", desc)
    assert cr == True
    after = obj.GetOwnProperty("testkey")
    assert (after.value, after.writable, after.enumerable, after.configurable) == (15, True, True, True)


def test_object_DefineOwnProperty_method_02(realm):
    # We can add properties to extensible objects
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    desc = PropertyDescriptor(value=120)
    cr = obj.DefineOwnProperty("testkey", desc)
    assert cr == True
    after = obj.GetOwnProperty("testkey")
    assert (after.value, after.writable, after.enumerable, after.configurable) == (120, False, False, False)


def test_object_DefineOwnProperty_method_03(realm):
    # We cannot add new properties to non-extensible objects
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    obj.PreventExtensions()

    desc = PropertyDescriptor(value=120)
    cr = obj.DefineOwnProperty("testkey", desc)
    assert cr == False
    assert not obj.HasProperty("testkey")


def thrower(*args, **kwargs):
    raise ESTypeError("Thrown from 04")


def test_object_DefineOwnProperty_method_04(realm):
    # If an exception happens in [[GetOwnProperty]], it bubbles up. The default implementaion of that method can't actually
    # fail, though, so we have to make one that throws deliberately.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    obj.GetOwnProperty = types.MethodType(thrower, obj)

    desc = PropertyDescriptor(value=120)
    with pytest.raises(ESTypeError):
        obj.DefineOwnProperty("testkey", desc)


def test_object_HasProperty_method_01(realm):
    # Returns True when the key is an own property
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    CreateDataProperty(obj, "testkey", 10)

    cr = obj.HasProperty("testkey")
    assert cr == True


def test_object_HasProperty_method_02(realm):
    # Returns True when the key is on the prototype chain
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    cr = obj.HasProperty("toString")
    assert cr == True


def test_object_HasProperty_method_03(realm):
    # Returns False when the key is nowhere to be found
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    cr = obj.HasProperty("toElephant")
    assert cr == False


def test_object_HasProperty_method_04(realm):
    # Exceptions from [[GetOwnProperty]] bubble up. (Step 2, in the algorithm.)
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    obj.GetOwnProperty = types.MethodType(thrower, obj)

    with pytest.raises(ESTypeError):
        obj.HasProperty("toElephant")


def test_object_HasProperty_method_05(realm):
    # Exceptions from [[GetPrototypeOf]] bubble up.
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    obj.GetPrototypeOf = types.MethodType(thrower, obj)

    with pytest.raises(ESTypeError):
        obj.HasProperty("toElephant")


@pytest.fixture
def get_tree(realm):
    parent = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    child = ObjectCreate(parent)
    CreateDataProperty(parent, "onparent", 100)
    CreateDataProperty(child, "onchild", -100)

    def special_get(self, new_target):
        return self

    get_fcn = CreateBuiltinFunction(special_get, [], realm=realm)
    DefinePropertyOrThrow(child, "accessor", PropertyDescriptor(Get=get_fcn, enumerable=True, configurable=True))
    DefinePropertyOrThrow(child, "dead", PropertyDescriptor(Get=None, enumerabl=True, configurable=True))
    return child


@pytest.mark.parametrize(
    "input, expected",
    [
        ("onchild", -100),  # Getting a data property on self works.
        ("onparent", 100),  # Getting a data prop on parent works.
        ("circus_performers", None),  # A property that isn't there at all returns undefined
        ("accessor", 345),  # An accessor property works.
        ("dead", None),  # An empty Get function on the accessor prop means Undefined happens.
    ],
)
def test_object_Get_method(get_tree, input, expected):
    val = get_tree.Get(input, 345)
    assert val == expected


def test_object_Get_method_01(get_tree):
    # An exception in the [[GetOwnProperty]] code path...
    get_tree.GetOwnProperty = types.MethodType(thrower, get_tree)
    with pytest.raises(ESTypeError):
        get_tree.Get("thimble", 345)


def test_object_Get_method_02(get_tree):
    # An exception in the [[GetPrototypeOf]] code path...
    get_tree.GetPrototypeOf = types.MethodType(thrower, get_tree)
    with pytest.raises(ESTypeError):
        get_tree.Get("needle", 345)


def test_object_Get_method_03(get_tree):
    # Now try an exception from the parent's recursion.
    parent = get_tree.GetPrototypeOf()
    parent.GetOwnProperty = types.MethodType(thrower, parent)
    with pytest.raises(ESTypeError):
        get_tree.Get("haystack", 345)


def test_object_Set_method(obj):
    # This method just shifts responsibility to the OrdinarySet function, so all we're really doing here is checking the code
    # path.
    val = obj.Set("testkey", 89, obj)
    assert val == True
    assert obj.Get("testkey", obj) == 89


def test_OrdinarySet_01(obj):
    # This one just gets a property description and then passes control to OrdinarySetWithOwnDescriptor. So we're really just
    # checking code paths (both successful, and when catching errors).
    val = OrdinarySet(obj, "testkey", 67, obj)
    assert val == True
    assert obj.Get("testkey", obj) == 67


def test_OrdinarySet_02(obj):
    obj.GetOwnProperty = types.MethodType(thrower, obj)
    with pytest.raises(ESTypeError):
        OrdinarySet(obj, "testkey", 68, obj)


@pytest.fixture
def child(obj):
    # Makes a child object that has a parent whose prototype is %ObjectPrototype%
    child = ObjectCreate(obj, ["slot"])
    return child


@pytest.fixture
def setprops(child):
    # Sets up properties for the "set" tests.
    parent = child.GetPrototypeOf()
    parent.DefineOwnProperty(
        "not_enumerable", PropertyDescriptor(value=542, writable=True, enumerable=False, configurable=True)
    )
    parent.DefineOwnProperty(
        "not_writable", PropertyDescriptor(value=543, writable=False, enumerable=True, configurable=True)
    )
    child.DefineOwnProperty(
        "child_nw", PropertyDescriptor(value=546, writable=False, enumerable=True, configurable=True)
    )
    child.DefineOwnProperty(
        "normal", PropertyDescriptor(value="NORMAL", writable=True, enumerable=True, configurable=True)
    )

    def settest_get(self, new_target):
        return self.slot

    def settest_set(self, new_target, value):
        self.slot = value
        return True

    get_fcn = CreateBuiltinFunction(settest_get, [], realm=surrounding_agent.running_ec.realm)
    set_fcn = CreateBuiltinFunction(settest_set, [], realm=surrounding_agent.running_ec.realm)
    child.DefineOwnProperty("codish", PropertyDescriptor(Get=get_fcn, Set=set_fcn, enumerable=True, configurable=True))
    return child


def test_OrdinarySetWithOwnDescriptor_01(setprops):
    # If the fourth arg is None (ownDesc), then the routine follows the prototype chain up to find a matching property and uses
    # the property description there to decide whether the property is writable or not.
    val = OrdinarySetWithOwnDescriptor(setprops, "not_enumerable", 553, setprops, None)  # Ok to write to this one
    assert val == True
    # Though, when the property got added to the child, it became enumerable.
    after = setprops.GetOwnProperty("not_enumerable")
    assert (after.value, after.writable, after.enumerable, after.configurable) == (553, True, True, True)


def test_OrdinarySetWithOwnDescriptor_02(setprops):
    # If the fourth arg is None (ownDesc), then the routine follows the prototype chain up to find a matching property and uses
    # the property description there to decide whether the property is writable or not.
    val = OrdinarySetWithOwnDescriptor(setprops, "not_writable", 562, setprops, None)  # This should fail.
    assert val == False
    # Make sure the property did not get added...
    assert "not_writable" not in setprops.OwnPropertyKeys()


def test_OrdinarySetWithOwnDescriptor_03(setprops):
    # If the ownDesc is None, and we're setting a property that's not on the prototype chain, it should just work.
    val = OrdinarySetWithOwnDescriptor(setprops, "tiger", 9003, setprops, None)
    assert val == True
    after = setprops.GetOwnProperty("tiger")
    assert (after.value, after.writable, after.enumerable, after.configurable) == (9003, True, True, True)


def test_OrdinarySetWithOwnDescriptor_04(setprops):
    # If ownDesc says this prop is not writable, the write should fail.
    desc = setprops.GetOwnProperty("child_nw")
    val = OrdinarySetWithOwnDescriptor(setprops, "child_nw", 577, setprops, desc)
    assert val == False
    after = setprops.GetOwnProperty("child_nw")
    assert (after.value, after.writable, after.enumerable, after.configurable) == (546, False, True, True)


def test_OrdinarySetWithOwnDescriptor_05(setprops):
    # If you've made a mistake and the receiver is not an object, bail.
    desc = setprops.GetOwnProperty("normal")
    val = OrdinarySetWithOwnDescriptor(setprops, "normal", "unusual", 89, desc)
    assert val == False
    after = setprops.GetOwnProperty("normal")
    assert (after.value, after.writable, after.enumerable, after.configurable) == ("NORMAL", True, True, True)


def test_OrdinarySetWithOwnDescriptor_06(setprops):
    # If you try to trick the engine by altering a writable, it should fail
    desc = setprops.GetOwnProperty("child_nw")
    desc.writable = True
    val = OrdinarySetWithOwnDescriptor(setprops, "child_nw", "bwahaha", setprops, desc)
    assert val == False
    after = setprops.GetOwnProperty("child_nw")
    assert (after.value, after.writable, after.enumerable, after.configurable) == (546, False, True, True)


def test_OrdinarySetWithOwnDescriptor_07(setprops):
    # You can't actually change enumerable or configurable or writable with [[Set]]. (And for that matter, value doesn't matter
    # either.)
    desc = PropertyDescriptor(value="odyssey", writable=True, enumerable=False, configurable=False)
    val = OrdinarySetWithOwnDescriptor(setprops, "normal", "purple", setprops, desc)
    assert val == True
    after = setprops.GetOwnProperty("normal")
    assert (after.value, after.writable, after.enumerable, after.configurable) == ("purple", True, True, True)


def test_OrdinarySetWithOwnDescriptor_08(setprops):
    # You can't change an accessor descriptor to a data descriptor.
    before = setprops.GetOwnProperty("codish")
    val = OrdinarySetWithOwnDescriptor(
        setprops,
        "codish",
        100,
        setprops,
        PropertyDescriptor(value=1, writable=True, enumerable=True, configurable=True),
    )
    assert val == False
    after = setprops.GetOwnProperty("codish")
    assert after.is_accessor_descriptor() and not after.is_data_descriptor()
    assert (after.Get, after.Set, after.enumerable, after.configurable) == (before.Get, before.Set, True, True)


def test_OrdinarySetWithOwnDescriptor_09(setprops):
    # If you try to "Set" with an accessor descriptor that doesn't know how to set, fail.
    before = setprops.GetOwnProperty("codish")
    val = OrdinarySetWithOwnDescriptor(setprops, "codish", "testval", setprops, PropertyDescriptor(Set=None))
    assert val == False
    after = setprops.GetOwnProperty("codish")
    assert (after.Get, after.Set, after.enumerable, after.configurable) == (before.Get, before.Set, True, True)


def test_OrdinarySetWithOwnDescriptor_10(setprops):
    # Setting an accessor descriptor should work.
    desc = setprops.GetOwnProperty("codish")
    val = OrdinarySetWithOwnDescriptor(setprops, "codish", "testval", setprops, desc)
    assert val == True
    assert Get(setprops, "codish") == "testval"


def test_OrdinarySetWithOwnDescriptor_11(setprops):
    # Now exception handling. First, if GetPrototypeOf throws.
    setprops.GetPrototypeOf = types.MethodType(thrower, setprops)
    with pytest.raises(ESTypeError):
        OrdinarySetWithOwnDescriptor(setprops, "elephant", 1, setprops, None)


def test_OrdinarySetWithOwnDescriptor_12(setprops):
    # If GetOwnProperty throws...
    setprops.GetOwnProperty = types.MethodType(thrower, setprops)
    with pytest.raises(ESTypeError):
        OrdinarySetWithOwnDescriptor(setprops, "normal", 1, setprops, None)


def test_OrdinarySetWithOwnDescriptor_13(setprops):
    # If a custom setter throws
    def custom_setter(self, new_target, value):
        raise ESTypeError("Thrown from custom_setter")

    set_fcn = CreateBuiltinFunction(custom_setter, [], realm=surrounding_agent.running_ec.realm)
    with pytest.raises(ESTypeError):
        OrdinarySetWithOwnDescriptor(setprops, "codish", -90, setprops, PropertyDescriptor(Set=set_fcn))


@pytest.fixture()
def deletable(obj):
    obj.DefineOwnProperty("normal", PropertyDescriptor(value=10, writable=True, enumerable=True, configurable=True))
    obj.DefineOwnProperty("permanent", PropertyDescriptor(value=9, writable=True, enumerable=True, configurable=False))
    return obj


def test_object_Delete_method(deletable):
    # This just delegates to OrdinaryDelete, so we're just confirming the code path.
    val = deletable.Delete("normal")
    assert val == True
    assert "normal" not in deletable.OwnPropertyKeys()


def test_OrdinaryDelete_01(deletable):
    # A run of the mill, normal deletion
    val = OrdinaryDelete(deletable, "normal")
    assert val == True
    assert "normal" not in deletable.OwnPropertyKeys()


def test_OrdinaryDelete_02(deletable):
    # Trying to delete something that's not actually there.
    val = OrdinaryDelete(deletable, "mystery")
    assert val == True


def test_OrdinaryDelete_03(deletable):
    # Trying to delete a non-configurable property
    val = OrdinaryDelete(deletable, "permanent")
    assert val == False
    assert "permanent" in deletable.OwnPropertyKeys()


def test_OrdinaryDelete_04(deletable):
    deletable.GetOwnProperty = types.MethodType(thrower, deletable)
    with pytest.raises(ESTypeError):
        OrdinaryDelete(deletable, "normal")


def test_object_OwnPropertyKeys_method(obj):
    # The [[OwnPropertyKeys]] method just delegates to OrdinaryOwnPropertyKeys, so we're just checking the code path here.
    val = obj.OwnPropertyKeys()
    assert val == []


def test_OrdinaryOwnPropertyKeys_01(obj):
    # There's a bunch of sorting activity that goes on here, so I'm making lots of properties.
    symbol_1 = JSSymbol("Symbol.unittest")
    symbol_2 = JSSymbol("Symbol.elephant")
    big = ToString(2 ** 53 - 1)
    toobig = ToString(2 ** 53)
    for propkey in [
        "first",
        "100",
        "-30",
        "second",
        "third",
        "fourth",
        "fifth",
        "   67  ",
        "67",
        "-0",
        "0",
        "45.5",
        "0xb22",
        symbol_1,
        symbol_2,
        "88",
        "0.0",
        big,
        toobig,
        "last",
    ]:
        CreateDataProperty(obj, propkey, propkey)

    val = obj.OwnPropertyKeys()
    assert val == [
        "0",
        "67",
        "88",
        "100",
        big,
        "first",
        "-30",
        "second",
        "third",
        "fourth",
        "fifth",
        "   67  ",
        "-0",
        "45.5",
        "0xb22",
        "0.0",
        toobig,
        "last",
        symbol_1,
        symbol_2,
    ]


@pytest.mark.parametrize(
    "input, expected",
    [
        ("0", True),
        ("-0", False),
        (JSSymbol("testsymbol"), False),
        ("notnumeric", False),
        ("9007199254740992", False),
        ("9007199254740991", True),
        ("2.5", False),
        ("351235", True),
    ],
)
def test_isIntegerIndex_(input, expected):
    assert isIntegerIndex(input) == expected


def test_ObjectCreate_(realm):
    obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    child = ObjectCreate(obj, ["testslot", "%crazy%"])

    assert child.GetPrototypeOf() == obj  # pylint: disable=not-callable # validiate prototype chain
    assert hasattr(child, "testslot")
    assert hasattr(child, "%crazy%")


def test_GetPrototypeFromConstructor_01(realm):
    # For the "normal" case, our constructor object needs to be callable.
    def fake_constructor(this_value, new_target):
        return None

    constructor = CreateBuiltinFunction(fake_constructor, [])
    proto = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    CreateDataProperty(constructor, "prototype", proto)

    val = GetPrototypeFromConstructor(constructor, "%BooleanPrototype%")
    assert val == proto


def test_GetPrototypeFromConstructor_02(realm):
    # Constructor is a callable object, but doesn't have a prototype
    def fake_constructor(this_value, new_target):
        return None

    constructor = CreateBuiltinFunction(fake_constructor, [])

    val = GetPrototypeFromConstructor(constructor, "%BooleanPrototype%")
    assert val == realm.intrinsics["%BooleanPrototype%"]


def test_GetPrototypeFromConstructor_03(realm):
    # Constructor is a callable object, but has a prototype with a non-object value
    def fake_constructor(this_value, new_target):
        return None

    constructor = CreateBuiltinFunction(fake_constructor, [])
    CreateDataProperty(constructor, "prototype", 5.6)

    val = GetPrototypeFromConstructor(constructor, "%BooleanPrototype%")
    assert val == realm.intrinsics["%BooleanPrototype%"]


def test_GetPrototypeFromConstructor_04(realm):
    # The [[Get]] call on the constructor throws.
    def fake_constructor(this_value, new_target):
        return None

    constructor = CreateBuiltinFunction(fake_constructor, [])
    constructor.GetOwnProperty = types.MethodType(thrower, constructor)

    with pytest.raises(ESTypeError):
        GetPrototypeFromConstructor(constructor, "%ObjectPrototype%")


def test_GetPrototypeFromConstructor_05(realm):
    # GetFunctionRealm throws.
    def fake_constructor(this_value, new_target):
        return None

    constructor = CreateBuiltinFunction(fake_constructor, ["ProxyHandler"])
    assert not hasattr(
        constructor, "Realm"
    )  # This should be here, but isn't. I suspect I have a case mismatch problem. In any case, if this assert starts breaking, just 'del' the Realm attr here instead of asserting.
    constructor.ProxyHandler = JSNull.NULL

    with pytest.raises(ESTypeError):
        GetPrototypeFromConstructor(constructor, "%ObjectPrototype%")


def test_OrdinaryCreateFromConstructor_01(realm):
    # Test the successful path. (Most of the work happens in GetPrototypeFromConstructor, so we really don't need to check much
    # here.)
    def fake_constructor(this_value, new_target):
        return None

    constructor = CreateBuiltinFunction(fake_constructor, [])
    proto = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    CreateDataProperty(constructor, "prototype", proto)

    val = OrdinaryCreateFromConstructor(constructor, "%ObjectPrototype%", ["slotify", "Lucious"])
    assert isObject(val)
    obj = val
    assert hasattr(obj, "slotify")
    assert hasattr(obj, "Lucious")
    assert IsExtensible(obj)
    assert obj.Prototype == proto


def test_OrdinaryCreateFromConstructor_02(realm):
    # The [[Get]] call on the constructor throws.
    def fake_constructor(this_value, new_target):
        return None

    constructor = CreateBuiltinFunction(fake_constructor, [])
    constructor.GetOwnProperty = types.MethodType(thrower, constructor)

    with pytest.raises(ESTypeError):
        OrdinaryCreateFromConstructor(constructor, "%ObjectPrototype%")


# 7.3.1 Get(O, P)
def test_Get_01(obj, mocker):
    # All this really does is pitch to the [[Get]] method of the object.
    m = mocker.patch.object(obj, "Get")
    Get(obj, "propkey")
    assert m.called_once_with("propkey", obj)


# 7.3.2 GetV(V, P)
def test_GetV_01(obj, mocker):
    # When given an object, this is just like Get.
    m = mocker.patch.object(obj, "Get")
    GetV(obj, "propkey")
    assert m.called_once_with("propkey", obj)


def test_GetV_02(realm):
    # When given a primitive, it gets wrapped in an object first.
    r = GetV(True, "constructor")
    assert r == realm.intrinsics["%Boolean%"]


def test_GetV_03(realm, mocker):
    # Error path from ToObject
    mocker.patch("ecmascript.ecmascript.ToObject", side_effect=ESTypeError)
    with pytest.raises(ESTypeError):
        GetV(True, "constructor")


def test_GetV_04(realm):
    # In particular, the null and undefined values can't be converted to objects, so...
    with pytest.raises(ESTypeError):
        GetV(None, "constructor")


# 7.3.3 Set(O, P, V, Throw)
# Defers to the object's [[Set]] method, but adds an error on failure if Throw is true.
def test_Set_01(obj, mocker):
    # When Throw is false, just defer to [[Set]]
    m = mocker.patch.object(obj, "Set")
    Set(obj, "propkey", 100, False)
    assert m.called_once_with("propkey", 100, obj)


def test_Set_02(obj):
    # If the [[Set]] method returns False and Throw is True, an exception is raised
    obj.PreventExtensions()
    with pytest.raises(ESTypeError):
        Set(obj, "propkey", 100, True)


def test_Set_03(obj):
    # Just a quick integration test here.
    r = Set(obj, "propkey", 100, True)
    assert r == True
    assert Get(obj, "propkey") == 100


# 7.3.4 CreateDataProperty(O, P, V)
# This just really defers to the object's DefineOwnProperty method
def test_CreateDataProperty_01(obj, mocker):
    m = mocker.patch.object(obj, "DefineOwnProperty", mocker.Mock(return_value="from test"))
    r = CreateDataProperty(obj, "propkey", 100)
    assert m.called_once_with(
        "propkey", PropertyDescriptor(value=100, writable=True, enumerable=True, configurable=True)
    )
    assert r == "from test"


# 7.3.5 CreateMethodProperty(O, P, V)
# This also just defers to the object's DefineOwnProperty method
def test_CreateMethodProperty_01(obj, mocker):
    m = mocker.patch.object(obj, "DefineOwnProperty", mocker.Mock(return_value="from test"))
    r = CreateMethodProperty(obj, "propkey", 100)
    assert m.called_once_with(
        "propkey", PropertyDescriptor(value=100, writable=True, enumerable=False, configurable=True)
    )
    assert r == "from test"


# 7.3.6 CreateDataPropertyOrThrow(O, P, V)
# Wraps CreateDataProperty. Essentially:
#    If CreateDataProperty returns False, CreateDataPropertyOrThrow should return a TypeError
#    Otheriwise, CreateDataProperty returns True
def test_CreateDataPropertyOrThrow_01(obj, mocker):
    mocker.patch("ecmascript.ecmascript.CreateDataProperty", return_value=True)
    res = CreateDataPropertyOrThrow(obj, "propkey", "fudge")
    assert ecmascript.ecmascript.CreateDataProperty.called_once_with(
        obj, "propkey", "fudge"
    )  # pylint: disable=no-member
    assert res == True


def test_CreateDataPropertyOrThrow_02(obj, mocker):
    mocker.patch("ecmascript.ecmascript.CreateDataProperty", return_value=False)
    with pytest.raises(ESTypeError):
        CreateDataPropertyOrThrow(obj, "propkey", "fudge")
    assert ecmascript.ecmascript.CreateDataProperty.called_once_with(
        obj, "propkey", "fudge"
    )  # pylint: disable=no-member


# CreateMethodPropertyOrThrow(O, P, V)
# Wraps CreateMethodProperty. Essentially:
#    If CreateMethodProperty returns False, CreateMethodPropertyOrThrow should return a TypeError
#    Otheriwise, CreateMethodProperty returns True
def test_CreateMethodPropertyOrThrow_01(obj, mocker):
    mocker.patch("ecmascript.ecmascript.CreateMethodProperty", return_value=True)
    res = CreateMethodPropertyOrThrow(obj, "propkey", "fudge")
    assert ecmascript.ecmascript.CreateMethodProperty.called_once_with(
        obj, "propkey", "fudge"
    )  # pylint: disable=no-member
    assert res == True


def test_CreateMethodPropertyOrThrow_02(obj, mocker):
    mocker.patch("ecmascript.ecmascript.CreateMethodProperty", return_value=False)
    with pytest.raises(ESTypeError):
        CreateMethodPropertyOrThrow(obj, "propkey", "fudge")
    assert ecmascript.ecmascript.CreateMethodProperty.called_once_with(
        obj, "propkey", "fudge"
    )  # pylint: disable=no-member


# 7.3.7 DefinePropertyOrThrow ( O, P, desc )
# Wraps DefineOwnProperty method
def test_DefinePropertyOrThrow_01(obj, mocker):
    m = mocker.patch.object(obj, "DefineOwnProperty", mocker.Mock(return_value=True))
    res = DefinePropertyOrThrow(obj, "propkey", PropertyDescriptor(value="test"))
    assert m.called_once_with(obj, "propkey", PropertyDescriptor(value="test"))
    assert res == True


def test_DefinePropertyOrThrow_02(obj, mocker):
    m = mocker.patch.object(obj, "DefineOwnProperty", mocker.Mock(return_value=False))
    with pytest.raises(ESTypeError):
        DefinePropertyOrThrow(obj, "propkey", PropertyDescriptor(value="test"))
    assert m.called_once_with(obj, "propkey", PropertyDescriptor(value="test"))


# 7.3.8 DeletePropertyOrThrow ( O, P )
def test_DeletePropertyOrThrow_01(obj, mocker):
    m = mocker.patch.object(obj, "Delete", mocker.Mock(return_value=True))
    res = DeletePropertyOrThrow(obj, "propkey")
    assert m.called_once_with("propkey")
    assert res == True


def test_DeletePropertyOrThrow_02(obj, mocker):
    m = mocker.patch.object(obj, "Delete", mocker.Mock(return_value=False))
    with pytest.raises(ESTypeError):
        DeletePropertyOrThrow(obj, "propkey")
    assert m.called_once_with("propkey")


# 7.3.9 GetMethod(V, P)
@pytest.mark.parametrize("inp", [JSNull.NULL, None])
def test_GetMethod_01(obj, inp):
    # Undefined & Null return Undefined
    Set(obj, "something", inp, False)
    res = GetMethod(obj, "something")
    assert res == None


def test_GetMethod_02(obj):
    # When the thing isn't a method, throw a TypeError
    Set(obj, "something", 89, False)
    with pytest.raises(ESTypeError):
        GetMethod(obj, "something")


def test_GetMethod_03(obj):
    myfunc = CreateBuiltinFunction(lambda this_value, new_target, arg: arg * 3 + 27, [])
    Set(obj, "something", myfunc, False)
    res = GetMethod(obj, "something")
    assert res == myfunc


def test_GetMethod_04(obj, mocker):
    mocker.patch("ecmascript.ecmascript.GetV", side_effect=ESTypeError)
    with pytest.raises(ESTypeError):
        GetMethod(obj, "something")


# 7.3.10 HasProperty(O, P)
# Just wraps the [[HasProperty]] object method
def test_HasProperty_01(obj, mocker):
    m = mocker.patch.object(obj, "HasProperty", mocker.Mock(return_value="testcase result"))
    res = HasProperty(obj, "propkey")
    assert m.called_once_with(obj, "propkey")
    assert res == "testcase result"


# 7.3.11 HasOwnProperty(O, P)
def test_HasOwnProperty_01(obj):
    # a property that's not there.
    res = HasOwnProperty(obj, "silly")
    assert res == False


def test_HasOwnProperty_02(obj):
    # a property that is there.
    Set(obj, "getting_set", "with_value", False)
    res = HasOwnProperty(obj, "getting_set")
    assert res == True


def test_HasOwnProperty_03(obj):
    # when an error is thrown
    obj.GetOwnProperty = types.MethodType(thrower, obj)
    with pytest.raises(ESTypeError):
        HasOwnProperty(obj, "silly")


# 7.3.12 Call ( F, V [, argumentesList ] )
def test_Call_01(realm):
    # Test the not-callable scenario
    with pytest.raises(ESTypeError):
        Call(True, None)


def test_Call_02(obj, mocker):
    func = CreateBuiltinFunction(lambda thisvalue, newtarget, x: x + 10, [])
    # Validate that we use the object's [[Call]] method and return its result
    # (and that a missing arglist gets transformed into an empty list)
    m = mocker.patch.object(func, "Call", mocker.Mock(return_value="test result"))
    res = Call(func, obj)
    assert m.called_once_with(func, obj, [])
    assert res == "test result"


def test_Call_03(obj, mocker):
    # Same thing, but with args this time.
    func = CreateBuiltinFunction(lambda thisvalue, newtarget, x, y: x + 10 * y, [])
    m = mocker.patch.object(func, "Call", mocker.Mock(return_value="test result"))
    res = Call(func, obj, [6, 8])
    assert m.called_once_with(func, obj, [6, 8])
    assert res == "test result"


# 7.3.13 Construct ( F [, argumentsList [, newTaget ] ] )
def test_Construct_01(realm, mocker):
    # The one-argument case
    cstr = realm.intrinsics["%Boolean%"]
    m = mocker.patch.object(cstr, "Construct", mocker.Mock(return_value="test result"))
    res = Construct(cstr)
    assert m.called_once_with([], cstr)
    assert res == "test result"


def test_Construct_02(realm, mocker):
    # The two-argument case
    cstr = realm.intrinsics["%Boolean%"]
    m = mocker.patch.object(cstr, "Construct", mocker.Mock(return_value="test result"))
    res = Construct(cstr, [True, False])
    assert m.called_once_with([True, False], cstr)
    assert res == "test result"


def test_Construct_03(realm, mocker):
    # The three-argument case
    cstr = realm.intrinsics["%Boolean%"]
    new_target = realm.intrinsics["%Number%"]
    m = mocker.patch.object(cstr, "Construct", mocker.Mock(return_value="test result"))
    res = Construct(cstr, [True], new_target)
    assert m.called_once_with([True], new_target)
    assert res == "test result"


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
        PropertyDescriptor(value="p1", writable=True, enumerable=True, configurable=True),
        PropertyDescriptor(value="p2", writable=False, enumerable=True, configurable=True),
        PropertyDescriptor(value="p3", writable=True, enumerable=False, configurable=True),
        PropertyDescriptor(value="p4", writable=False, enumerable=False, configurable=True),
        PropertyDescriptor(value="p5", writable=True, enumerable=True, configurable=False),
        PropertyDescriptor(value="p6", writable=False, enumerable=True, configurable=False),
        PropertyDescriptor(value="p7", writable=True, enumerable=False, configurable=False),
        PropertyDescriptor(value="p8", writable=False, enumerable=False, configurable=False),
        PropertyDescriptor(Get=None, Set=None, enumerable=True, configurable=True),
        PropertyDescriptor(Get=None, Set=None, enumerable=False, configurable=True),
        PropertyDescriptor(Get=None, Set=None, enumerable=True, configurable=False),
        PropertyDescriptor(Get=None, Set=None, enumerable=False, configurable=False),
    ]


def test_SetIntegrityLevel_01(obj, props):
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f"property_{idx+1}", p)

    res = SetIntegrityLevel(obj, "sealed")

    assert res == True
    assert not IsExtensible(obj)
    after = [OrdinaryGetOwnProperty(obj, f"property_{idx}") for idx in range(1, len(props) + 1)]
    for result, inp in zip_longest(after, props):
        assert not result.configurable
        assert result.enumerable == inp.enumerable
        assert (result.writable == inp.writable) if hasattr(inp, "writable") else True
        assert (result.value == inp.value) if hasattr(inp, "value") else True
        assert (result.Get == inp.Get) if hasattr(inp, "Get") else True
        assert (result.Set == inp.Set) if hasattr(inp, "Set") else True


def test_SetIntegrityLevel_02(obj, props):
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f"property_{idx+1}", p)

    res = SetIntegrityLevel(obj, "frozen")

    assert res == True
    assert not IsExtensible(obj)
    after = [OrdinaryGetOwnProperty(obj, f"property_{idx}") for idx in range(1, len(props) + 1)]
    for result, inp in zip_longest(after, props):
        assert not result.configurable
        assert result.enumerable == inp.enumerable
        assert (not result.writable) if hasattr(inp, "writable") else True
        assert (result.value == inp.value) if hasattr(inp, "value") else True
        assert (result.Get == inp.Get) if hasattr(inp, "Get") else True
        assert (result.Set == inp.Set) if hasattr(inp, "Set") else True


def test_SetIntegrityLevel_03(obj, props, mocker):
    # If the [[PreventExtensions]] method returns false, we just bail and return false as well.
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f"property_{idx+1}", p)
    mocker.patch.object(obj, "PreventExtensions", mocker.Mock(return_value=False))

    res = SetIntegrityLevel(obj, "sealed")

    assert res == False
    assert IsExtensible(obj)
    after = [OrdinaryGetOwnProperty(obj, f"property_{idx}") for idx in range(1, len(props) + 1)]
    for result, inp in zip_longest(after, props):
        assert result.configurable == inp.configurable
        assert result.enumerable == inp.enumerable
        assert (result.writable == inp.writable) if hasattr(inp, "writable") else True
        assert (result.value == inp.value) if hasattr(inp, "value") else True
        assert (result.Get == inp.Get) if hasattr(inp, "Get") else True
        assert (result.Set == inp.Set) if hasattr(inp, "Set") else True


""" def test_SetIntegrityLevel_04(obj, mocker):
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
 """


def test_SetIntegrityLevel_09(obj, mocker, props):
    # Test a particular branch I'm not sure how to really bring about -- when we can't
    # actually look up a property even though it's in the keys list, we just skip it.
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f"property_{idx+1}", p)
    mocker.patch.object(
        obj, "OwnPropertyKeys", mocker.Mock(side_effect=lambda: ["mystery"] + OrdinaryOwnPropertyKeys(obj))
    )

    res = SetIntegrityLevel(obj, "frozen")

    assert res == True
    assert not IsExtensible(obj)
    after = [OrdinaryGetOwnProperty(obj, f"property_{idx}") for idx in range(1, len(props) + 1)]
    for result, inp in zip_longest(after, props):
        assert not result.configurable
        assert result.enumerable == inp.enumerable
        assert (not result.writable) if hasattr(inp, "writable") else True
        assert (result.value == inp.value) if hasattr(inp, "value") else True
        assert (result.Get == inp.Get) if hasattr(inp, "Get") else True
        assert (result.Set == inp.Set) if hasattr(inp, "Set") else True
    # Make sure it didn't get added
    assert OrdinaryGetOwnProperty(obj, "mystery") is None


# 7.3.15 TestIntegrityLevel(O, level)
@pytest.mark.parametrize("level", ["sealed", "frozen"])
def test_TestIntegrityLevel_01(obj, mocker, level):
    # If obj is extensible, we just return False for all levels, and don't actually check any of the properties
    opk = mocker.patch.object(obj, "OwnPropertyKeys")
    gop = mocker.patch.object(obj, "GetOwnProperty")

    res = TestIntegrityLevel(obj, level)
    assert res == False
    assert opk.not_called()
    assert gop.not_called()


@pytest.mark.parametrize("level", ["sealed", "frozen"])
def test_TestIntegrityLevel_02(obj, props, level):
    # check actual frozen or sealed.
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f"property_{idx+1}", p)
    SetIntegrityLevel(obj, level)

    res = TestIntegrityLevel(obj, level)
    assert res == True


@pytest.mark.parametrize("level", ["sealed", "frozen"])
def test_TestIntegrityLevel_03(obj, props, level):
    # test not actually frozen or sealed (but not extensible)
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f"property_{idx+1}", p)
    obj.PreventExtensions()

    res = TestIntegrityLevel(obj, level)
    assert res == False


# def test_TestIntegrityLevel_04(obj, mocker):
#     # IsExtensible throws
#     mocker.patch('ecmascript.IsExtensible', return_value=Completion(THROW, 'test throw', None))
#     res = TestIntegrityLevel(obj, 'sealed')

#     assert res == Completion(THROW, 'test throw', None)

# def test_TestIntegrityLevel_05(obj, mocker):
#     # OwnPropertyKeys method throws
#     obj.PreventExtensions()
#     mocker.patch.object(obj, 'OwnPropertyKeys', mocker.Mock(return_value=Completion(THROW, 'test throw', None)))

#     res = TestIntegrityLevel(obj, 'sealed')

#     assert res == Completion(THROW, 'test throw', None)

# def test_TestIntegrityLevel_06(obj, mocker, props):
#     # GetOwnProperty method throws
#     for idx, p in enumerate(props):
#         DefinePropertyOrThrow(obj, f'property_{idx+1}', p)
#     obj.PreventExtensions()
#     mocker.patch.object(obj, 'GetOwnProperty', mocker.Mock(return_value=Completion(THROW, 'test throw', None)))

#     res = TestIntegrityLevel(obj, 'sealed')

#     assert res == Completion(THROW, 'test throw', None)


def test_TestIntegrityLevel_07(obj, props):
    # A sealed object is not necessarily frozen
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f"property_{idx+1}", p)
    SetIntegrityLevel(obj, "sealed")

    res = TestIntegrityLevel(obj, "frozen")

    assert res == False


def test_TestIntegrityLevel_08(obj, mocker, props):
    # Test a particular branch I'm not sure how to really bring about -- when we can't
    # actually look up a property even though it's in the keys list, we just skip it.
    for idx, p in enumerate(props):
        DefinePropertyOrThrow(obj, f"property_{idx+1}", p)
    SetIntegrityLevel(obj, "frozen")
    mocker.patch.object(
        obj, "OwnPropertyKeys", mocker.Mock(side_effect=lambda: ["mystery"] + OrdinaryOwnPropertyKeys(obj))
    )

    res = TestIntegrityLevel(obj, "frozen")

    assert res == True


# 7.3.16 CreateArrayFromList ( elements )
@pytest.mark.parametrize("lst", [[], [5, 1221, "boop"]])
def test_CreateArrayFromList_01(realm, lst):
    ary = CreateArrayFromList(lst)

    assert isinstance(ary, ArrayObject)
    assert Get(ary, "length") == len(lst)
    for idx, value in enumerate(lst):
        assert Get(ary, ToString(idx)) == value


# 7.3.17 CreateListFromArrayLike ( obj [ , elementTypes ] )
# The abstract operation CreateListFromArrayLike is used to create a List value whose elements are provided by the
# indexed properties of an array-like object, obj. The optional argument elementTypes is a List containing the
# names of ECMAScript Language Types that are allowed for element values of the List that is created. This abstract
# operation performs the following steps:
#
#   1. If elementTypes is not present, set elementTypes to « Undefined, Null, Boolean, String, Symbol, Number,
#      Object ».
#   2. If Type(obj) is not Object, throw a TypeError exception.
#   3. Let len be ? ToLength(? Get(obj, "length")).
#   4. Let list be a new empty List.
#   5. Let index be 0.
#   6. Repeat, while index < len
#       a. Let indexName be ! ToString(index).
#       b. Let next be ? Get(obj, indexName).
#       c. If Type(next) is not an element of elementTypes, throw a TypeError exception.
#       d. Append next as the last element of list.
#       e. Set index to index + 1.
#   7. Return list.
class Test_CreateListFromArrayLike:
    def test_nonobject(self, realm):
        with pytest.raises(ESTypeError):
            CreateListFromArrayLike("just a string")

    def test_empty(self, realm):
        o = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
        CreateDataPropertyOrThrow(o, "length", 0)

        lst = CreateListFromArrayLike(o)
        assert lst == []

    @pytest.fixture
    def vals_obj(self, realm):
        o = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
        vals = ["this", 67.0, True, JSNull.NULL, None, o, wks_has_instance]
        for idx, v in enumerate(vals):
            CreateDataPropertyOrThrow(o, str(idx), v)
        CreateDataPropertyOrThrow(o, "length", len(vals))
        return (vals, o)

    def test_ordinary(self, vals_obj):
        vals, o = vals_obj
        lst = CreateListFromArrayLike(o)
        assert lst == vals

    def test_restricted(self, vals_obj):
        _, o = vals_obj
        with pytest.raises(ESTypeError):
            CreateListFromArrayLike(o, [JSType.NUMBER, JSType.STRING])


# 7.3.19 OrdinaryHasInstance ( C, O )
# The abstract operation OrdinaryHasInstance implements the default algorithm for determining if an object O inherits from
# the instance object inheritance path provided by constructor C. This abstract operation performs the following steps:
#
# 1. If IsCallable(C) is false, return false.
# 2. If C has a [[BoundTargetFunction]] internal slot, then
#   a. Let BC be C.[[BoundTargetFunction]].
#   b. Return ? InstanceofOperator(O, BC).
# 3. If Type(O) is not Object, return false.
# 4. Let P be ? Get(C, "prototype").
# 5. If Type(P) is not Object, throw a TypeError exception.
# 6. Repeat,
#   a. Set O to ? O.[[GetPrototypeOf]]().
#   b. If O is null, return false.
#   c. If SameValue(P, O) is true, return true.
class Test_OrdinaryHasInstance:
    def test_notcallable(self, mocker):
        iscallable = mocker.patch("ecmascript.ecmascript.IsCallable", return_value=False)
        rv = OrdinaryHasInstance(10, 11)
        iscallable.assert_called_with(10)
        assert rv == False

    def test_boundtarget(self, realm, mocker):
        mocker.patch("ecmascript.ecmascript.IsCallable", return_value=True)
        instanceofop = mocker.patch("ecmascript.ecmascript.InstanceofOperator", return_value=100)
        obj = ObjectCreate(realm.intrinsics["%ObjectPrototype%"], ["BoundTargetFunction"])
        obj.BoundTargetFunction = "myfunc"

        rv = OrdinaryHasInstance(obj, "other")
        instanceofop.assert_called_with("other", "myfunc")
        assert rv == 100

    def test_notobj(self, mocker, realm):
        mocker.patch("ecmascript.ecmascript.IsCallable", return_value=True)
        C = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
        rv = OrdinaryHasInstance(C, "a string")
        assert rv == False

    def test_badproto(self, mocker, realm):
        mocker.patch("ecmascript.ecmascript.IsCallable", return_value=True)
        C = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
        O = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
        CreateDataPropertyOrThrow(C, "prototype", 10)
        with pytest.raises(ESTypeError):
            OrdinaryHasInstance(C, O)

    def test_ordinary_true(self, realm):
        O = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
        rv = OrdinaryHasInstance(realm.intrinsics["%Object%"], O)
        assert rv

    def test_ordinary_false(self, realm):
        O = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
        rv = OrdinaryHasInstance(realm.intrinsics["%Array%"], O)
        assert not rv

import pytest
import math

# pylint: disable=unused-wildcard-import
from ecmascript.ecmascript import *


@pytest.fixture
def obj(realm):
    return ObjectCreate(realm.intrinsics["%ObjectPrototype%"])


def test_reference_repr_01():
    ref = Reference(None, "name", True)
    val = repr(ref)
    assert "base=None" in val
    assert "name" in val
    assert "S" in val


def test_reference_repr_02():
    ref = Reference("base", "silly", False)
    val = repr(ref)
    assert "S" not in val


def test_reference_repr_03():
    ref = SuperReference("base", "super", False, 67)
    val = repr(ref)
    assert val.startswith("SuperReference(")
    assert "thisValue=67" in val


def test_getbase():
    ref = Reference("base", "name", False)
    assert GetBase(ref) == "base"


def test_getreferencedname():
    ref = Reference("base", "name", False)
    assert GetReferencedName(ref) == "name"


@pytest.mark.parametrize("strict,expected", [(True, True), (False, False)])
def test_isstrictreference(strict, expected):
    ref = Reference("base", "name", strict)
    assert IsStrictReference(ref) == expected


@pytest.mark.parametrize(
    "base, expected", [(True, True), ("cherry", True), (67, True), (wks_unscopables, True), (JSNull.NULL, False)]
)
def test_HasPrimitiveBase_01(base, expected):
    ref = Reference(base, "refname", False)
    assert HasPrimitiveBase(ref) == expected


def test_HasPrimitiveBase_02(obj):
    ref = Reference(obj, "refname", False)
    assert not HasPrimitiveBase(ref)


def test_IsPropertyReference_01(obj):
    ref = Reference(obj, "refname", False)
    assert IsPropertyReference(ref)


@pytest.mark.parametrize("base, expected", [(None, False), (False, True)])
def test_IsPropertyReference_02(base, expected):
    ref = Reference(base, "refname", False)
    assert IsPropertyReference(ref) == expected


@pytest.mark.parametrize("useObject, expected", [(False, True), (True, False)])
def test_IsUnresolvableReference_01(obj, useObject, expected):
    ref = Reference(obj if useObject else None, "refname", False)
    assert IsUnresolvableReference(ref) == expected


@pytest.mark.parametrize("hasSuper, expected", ([False, False], [True, True]))
def test_IsSuperReference_01(obj, hasSuper, expected):
    ref = Reference(obj, "refname", False)
    if hasSuper:
        ref.this_value = obj
    assert IsSuperReference(ref) == expected


@pytest.mark.parametrize("ref, expected", [("monkey", "monkey")])  # Input is not a reference
def test_GetValue_01(ref, expected):
    rval = GetValue(ref)
    assert rval == expected


def test_GetValue_02(realm):
    # If the input reference is unresolvable, throw a reference error.
    with pytest.raises(ESReferenceError):
        GetValue(Reference(None, "unresolvable", False))


def test_GetValue_03(obj):
    # The common case: resolve the reference to an object
    CreateDataProperty(obj, "value", "elephant")
    rval = GetValue(Reference(obj, "value", False))
    assert rval == "elephant"


@pytest.mark.xfail(reason="Needs String Object support")
def test_GetValue_04():
    # Now, the case where the base is a common object, and we try and get a property from that. One example is the 'length'
    # property of strings. This will fail for now, because I don't have string objects implemented.
    rval = GetValue(Reference("short", "length", False))
    assert rval == 5


def test_GetValue_05(realm):
    # But we can still try to dereference a value from a primitive type object; we should just get None back.
    rval = GetValue(Reference(45, "digits", False))
    assert rval is None


def test_GetValue_06(realm):
    # Finally, there's the Environment Record form.
    rval = GetValue(Reference(realm.global_env.environment_record, "NaN", False))
    assert math.isnan(rval)


def test_PutValue_03(realm):
    # If the first arg is not a reference, throws a reference error.
    with pytest.raises(ESReferenceError):
        PutValue("monkey", "tigger")


def test_PutValue_04(realm):
    # If the reference is unresolvable but not strict, then the value goes onto the global object.
    rval = PutValue(Reference(None, "new_info", False), "Amazing Test!")
    assert rval == True
    global_obj = GetGlobalObject()
    rval2 = Get(global_obj, "new_info")
    assert rval2 == "Amazing Test!"


def test_PutValue_05(realm):
    # If the reference is unresolvable and strict, then we get a reference error.
    with pytest.raises(ESReferenceError):
        PutValue(Reference(None, "strict_field", True), "blocked!")


def test_PutValue_06(realm):
    # Setting a value on a primitive type should ususally succeed, but the temp object those values were placed on doesn't
    # persist, so it's not especially visible.
    rval = PutValue(Reference(True, "odd_field", False), "adding...")
    assert rval is None


def test_PutValue_07(realm):
    # Doing that but with the strict flag means that if the Put fails, we should throw a TypeError. Turns out, when you
    # try to Put a value on a primitive time, the Put always fails, so... that's easy to arrange...
    with pytest.raises(ESTypeError):
        PutValue(Reference(True, "odd_field", True), "add")


def test_PutValue_08(realm):
    # Finally, environment records:
    rval = PutValue(Reference(realm.global_env.environment_record, "testfield", False), "testval")
    assert rval == True
    rval2 = GetValue(Reference(realm.global_env.environment_record, "testfield", False))
    assert rval2 == "testval"


def test_GetThisValue_01(realm):
    obj1 = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    obj2 = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])
    ref = Reference(obj2, "property", False)
    ref.this_value = obj1

    rval = GetThisValue(ref)
    assert rval == obj1


def test_GetThisValue_02(obj):
    ref = Reference(obj, "property", False)
    rval = GetThisValue(ref)
    assert rval == obj


def test_InitializeReferencedBinding_01(realm):
    # The "it works" scenario
    er = realm.global_env.environment_record
    er.CreateMutableBinding("silly", True)
    ref = Reference(er, "silly", False)
    rval = InitializeReferencedBinding(ref, 89)
    assert rval == Empty.EMPTY
    assert GetValue(ref) == 89

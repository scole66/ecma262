import pytest

from ecmascript import *

NORMAL = CompletionType.NORMAL
THROW = CompletionType.THROW
empty = Empty.EMPTY


def test_de_HasBinding():
    # setup
    der = DeclarativeEnvironmentRecord()
    der.CreateMutableBinding("blue_moon", False)

    # probe
    assert der.HasBinding("blue_moon")
    assert not der.HasBinding("red_moon")


def test_de_CreateMutableBinding():
    # setup
    der = DeclarativeEnvironmentRecord()

    # probe
    cr1 = der.CreateMutableBinding("goblin", True)
    cr2 = der.CreateMutableBinding("kobold", False)

    # check
    assert cr1 == empty
    assert cr2 == empty
    assert der.bindings["goblin"].deletable
    assert not der.bindings["kobold"].deletable

    cr3 = der.DeleteBinding("goblin")
    assert cr3
    cr4 = der.DeleteBinding("kobold")
    assert not cr4

    assert not der.HasBinding("goblin")
    assert der.HasBinding("kobold")


def test_de_CreateImmutableBinding(realm):
    # setup
    der = DeclarativeEnvironmentRecord()

    # probe
    cr1 = der.CreateImmutableBinding("goblin", True)
    cr2 = der.CreateImmutableBinding("kobold", False)

    # check
    assert cr1 == empty
    assert cr2 == empty
    assert der.bindings["goblin"].strict
    assert not der.bindings["kobold"].strict

    cr3 = der.InitializeBinding("goblin", 13)
    cr4 = der.InitializeBinding("kobold", "fart")

    with pytest.raises(ESTypeError):
        der.SetMutableBinding("goblin", 14, False)

    der.SetMutableBinding("kobold", 88, False)


def test_de_InitializedBinding():
    der = DeclarativeEnvironmentRecord()

    der.CreateMutableBinding("goblin", True)
    der.CreateMutableBinding("kobold", False)
    der.CreateImmutableBinding("elf", True)
    der.CreateImmutableBinding("gnome", False)

    cr5 = der.InitializeBinding("goblin", 100)
    cr6 = der.InitializeBinding("kobold", 101)
    cr7 = der.InitializeBinding("elf", 103)
    cr8 = der.InitializeBinding("gnome", 104)

    assert cr5 == empty
    assert cr6 == empty
    assert cr7 == empty
    assert cr8 == empty

    assert der.bindings["goblin"].value == 100
    assert der.bindings["goblin"].initialized
    assert der.bindings["kobold"].value == 101
    assert der.bindings["kobold"].initialized
    assert der.bindings["elf"].value == 103
    assert der.bindings["elf"].initialized
    assert der.bindings["gnome"].value == 104
    assert der.bindings["gnome"].initialized


def test_SetMutableBinding(realm):
    # The concrete Environment Record method SetMutableBinding for declarative Environment Records attempts to
    # change the bound value of the current binding of the identifier whose name is the value of the argument N to
    # the value of argument V. A binding for N normally already exists, but in rare cases it may not. If the
    # binding is an immutable binding, a TypeError is thrown if S is true.

    der = DeclarativeEnvironmentRecord()

    der.CreateMutableBinding("goblin", True)
    der.CreateMutableBinding("kobold", False)
    der.CreateImmutableBinding("elf", True)
    der.CreateImmutableBinding("gnome", False)

    cr = der.SetMutableBinding("dwarf", 18, False)
    assert cr == empty
    cr = der.GetBindingValue("dwarf", False)
    assert cr == 18

    with pytest.raises(ESReferenceError):
        der.SetMutableBinding("turtle", 99, True)

    with pytest.raises(ESReferenceError):
        der.SetMutableBinding("goblin", 32122, False)

    der.InitializeBinding("goblin", "beef tenderloin")
    der.InitializeBinding("kobold", "pasta")
    der.InitializeBinding("elf", "egg salad")
    der.InitializeBinding("gnome", "baked potato")

    cr = der.SetMutableBinding("goblin", "volcano", False)
    assert cr == empty
    cr = der.GetBindingValue("goblin", False)
    assert cr == "volcano"
    with pytest.raises(ESTypeError):
        der.SetMutableBinding("elf", "suntan", False)
    cr = der.GetBindingValue("elf", False)
    assert cr == "egg salad"
    cr = der.SetMutableBinding("gnome", "beach", False)
    assert cr == empty
    cr = der.GetBindingValue("gnome", False)
    assert cr == "baked potato"
    with pytest.raises(ESTypeError):
        der.SetMutableBinding("gnome", "forest", True)
    cr = der.GetBindingValue("gnome", False)
    assert cr == "baked potato"


def test_GetBindingValue(realm):
    der = DeclarativeEnvironmentRecord()

    der.CreateMutableBinding("goblin", False)
    with pytest.raises(ESReferenceError):
        der.GetBindingValue("goblin", False)

    der.InitializeBinding("goblin", "grenade")
    cr = der.GetBindingValue("goblin", False)
    assert cr == "grenade"


def test_DeleteBinding():
    der = DeclarativeEnvironmentRecord()

    der.CreateMutableBinding("goblin", True)
    der.CreateMutableBinding("kobold", False)

    cr = der.DeleteBinding("goblin")
    assert cr
    cr = der.DeleteBinding("kobold")
    assert not cr


def test_remaining():
    der = DeclarativeEnvironmentRecord()
    assert not der.HasThisBinding()
    assert not der.HasSuperBinding()
    assert der.WithBaseObject() is None

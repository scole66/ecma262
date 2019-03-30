import pytest

from completion_record import CompletionType, Empty
import environment


NORMAL = CompletionType.NORMAL
THROW = CompletionType.THROW
empty = Empty.EMPTY

def test_de_HasBinding():
    # setup
    der = environment.DeclarativeEnvironmentRecord()
    der.CreateMutableBinding('blue_moon', False)

    # probe
    assert der.HasBinding('blue_moon')
    assert not der.HasBinding('red_moon')

def test_de_CreateMutableBinding():
    # setup
    der = environment.DeclarativeEnvironmentRecord()

    # probe
    cr1 = der.CreateMutableBinding('goblin', True)
    cr2 = der.CreateMutableBinding('kobold', False)

    # check
    assert cr1.ctype == NORMAL
    assert cr1.value == empty
    assert cr1.target is None
    assert cr2.ctype == NORMAL
    assert cr2.value == empty
    assert cr2.target is None
    assert der.bindings['goblin'].deletable
    assert not der.bindings['kobold'].deletable

    cr3 = der.DeleteBinding('goblin')
    assert cr3.value
    cr4 = der.DeleteBinding('kobold')
    assert not cr4.value

    assert not der.HasBinding('goblin')
    assert der.HasBinding('kobold')

def test_de_CreateImmutableBinding():
    # setup
    der = environment.DeclarativeEnvironmentRecord()

    # probe
    cr1 = der.CreateImmutableBinding('goblin', True)
    cr2 = der.CreateImmutableBinding('kobold', False)

    # check
    assert cr1.ctype == NORMAL
    assert cr1.value == empty
    assert cr1.target is None
    assert cr2.ctype == NORMAL
    assert cr2.value == empty
    assert cr2.target is None
    assert der.bindings['goblin'].strict
    assert not der.bindings['kobold'].strict

    cr3 = der.InitializeBinding('goblin', 13)
    cr4 = der.InitializeBinding('kobold', 'fart')

    cr5 = der.SetMutableBinding('goblin', 14, False)
    assert cr5.ctype == THROW

    cr6 = der.SetMutableBinding('kobold', 88, False)
    assert cr6.ctype == NORMAL

def test_de_InitializedBinding():
    der = environment.DeclarativeEnvironmentRecord()

    cr1 = der.CreateMutableBinding('goblin', True)
    cr2 = der.CreateMutableBinding('kobold', False)
    cr3 = der.CreateImmutableBinding('elf', True)
    cr4 = der.CreateImmutableBinding('gnome', False)

    assert cr1.ctype == NORMAL
    assert cr2.ctype == NORMAL
    assert cr3.ctype == NORMAL
    assert cr4.ctype == NORMAL

    cr5 = der.InitializeBinding('goblin', 100)
    cr6 = der.InitializeBinding('kobold', 101)
    cr7 = der.InitializeBinding('elf', 103)
    cr8 = der.InitializeBinding('gnome', 104)

    assert cr5.ctype == NORMAL
    assert cr5.value == empty
    assert cr5.target is None
    assert cr6.ctype == NORMAL
    assert cr6.value == empty
    assert cr6.target is None
    assert cr7.ctype == NORMAL
    assert cr7.value == empty
    assert cr7.target is None
    assert cr8.ctype == NORMAL
    assert cr8.value == empty
    assert cr8.target is None

    assert der.bindings['goblin'].value == 100
    assert der.bindings['goblin'].initialized
    assert der.bindings['kobold'].value == 101
    assert der.bindings['kobold'].initialized
    assert der.bindings['elf'].value == 103
    assert der.bindings['elf'].initialized
    assert der.bindings['gnome'].value == 104
    assert der.bindings['gnome'].initialized

def test_SetMutableBinding():
    # The concrete Environment Record method SetMutableBinding for declarative Environment Records attempts to
    # change the bound value of the current binding of the identifier whose name is the value of the argument N to
    # the value of argument V. A binding for N normally already exists, but in rare cases it may not. If the
    # binding is an immutable binding, a TypeError is thrown if S is true.

    der = environment.DeclarativeEnvironmentRecord()

    cr1 = der.CreateMutableBinding('goblin', True)
    cr2 = der.CreateMutableBinding('kobold', False)
    cr3 = der.CreateImmutableBinding('elf', True)
    cr4 = der.CreateImmutableBinding('gnome', False)

    assert cr1.ctype == NORMAL
    assert cr2.ctype == NORMAL
    assert cr3.ctype == NORMAL
    assert cr4.ctype == NORMAL

    cr = der.SetMutableBinding('dwarf', 18, False)
    assert cr.ctype == NORMAL
    assert cr.value == empty
    assert cr.target is None
    cr = der.GetBindingValue('dwarf', False)
    assert cr.ctype == NORMAL and cr.value == 18

    cr = der.SetMutableBinding('turtle', 99, True)
    assert cr.ctype == THROW

    cr = der.SetMutableBinding('goblin', 32122, False)
    assert cr.ctype == THROW

    cr = der.InitializeBinding('goblin', 'beef tenderloin')
    assert cr.ctype == NORMAL
    cr = der.InitializeBinding('kobold', 'pasta')
    assert cr.ctype == NORMAL
    cr = der.InitializeBinding('elf', 'egg salad')
    assert cr.ctype == NORMAL
    cr = der.InitializeBinding('gnome', 'baked potato')
    assert cr.ctype == NORMAL

    cr = der.SetMutableBinding('goblin', 'volcano', False)
    assert cr.ctype == NORMAL and cr.value == empty
    cr = der.GetBindingValue('goblin', False)
    assert cr.ctype == NORMAL and cr.value == 'volcano'
    cr = der.SetMutableBinding('elf', 'suntan', False)
    assert cr.ctype == THROW
    cr = der.GetBindingValue('elf', False)
    assert cr.ctype == NORMAL and cr.value == 'egg salad'
    cr = der.SetMutableBinding('gnome', 'beach', False)
    assert cr.ctype == NORMAL and cr.value == empty
    cr = der.GetBindingValue('gnome', False)
    assert cr.ctype == NORMAL and cr.value == 'baked potato'
    cr = der.SetMutableBinding('gnome', 'forest', True)
    assert cr.ctype == THROW
    cr = der.GetBindingValue('gnome', False)
    assert cr.ctype == NORMAL and cr.value == 'baked potato'

def test_GetBindingValue():
    der = environment.DeclarativeEnvironmentRecord()

    cr = der.CreateMutableBinding('goblin', False)
    assert cr.ctype == NORMAL

    cr = der.GetBindingValue('goblin', False)
    assert cr.ctype == THROW

    cr = der.InitializeBinding('goblin', 'grenade')
    assert cr.ctype == NORMAL

    cr = der.GetBindingValue('goblin', False)
    assert cr.ctype == NORMAL and cr.value == 'grenade' and cr.target is None

def test_DeleteBinding():
    der = environment.DeclarativeEnvironmentRecord()

    cr1 = der.CreateMutableBinding('goblin', True)
    cr2 = der.CreateMutableBinding('kobold', False)

    cr = der.DeleteBinding('goblin')
    assert cr.ctype == NORMAL and cr.value
    cr = der.DeleteBinding('kobold')
    assert cr.ctype == NORMAL and not cr.value

def test_remaining():
    der = environment.DeclarativeEnvironmentRecord()
    assert not der.HasThisBinding()
    assert not der.HasSuperBinding()
    assert der.WithBaseObject() is None

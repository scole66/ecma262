import pytest

import ecmascript    # We need to do the import for the mocker plugin to work
from ecmascript import *

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

@pytest.mark.parametrize('get,set,value,writable,expected', [
    (False, False, False, False, False),
    (False, False, False, True, False),
    (False, False, True, False, False),
    (False, False, True, True, False),
    (False, True, False, False, True),
    (False, True, False, True, True),
    (False, True, True, False, True),
    (False, True, True, True, True),
    (True, False, False, False, True),
    (True, False, False, True, True),
    (True, False, True, False, True),
    (True, False, True, True, True),
    (True, True, False, False, True),
    (True, True, False, True, True),
    (True, True, True, False, True),
    (True, True, True, True, True)])
def test_is_accessor_descriptor(get, set, value, writable, expected):
    pd = PropertyDescriptor()
    if get:
        pd.Get = None
    if set:
        pd.Set = None
    if value:
        pd.value = 10
    if writable:
        pd.value = True
    assert pd.is_accessor_descriptor() == expected

@pytest.mark.parametrize('get,set,value,writable,expected', [
    (False, False, False, False, False),
    (False, False, False, True, True),
    (False, False, True, False, True),
    (False, False, True, True, True),
    (False, True, False, False, False),
    (False, True, False, True, True),
    (False, True, True, False, True),
    (False, True, True, True, True),
    (True, False, False, False, False),
    (True, False, False, True, True),
    (True, False, True, False, True),
    (True, False, True, True, True),
    (True, True, False, False, False),
    (True, True, False, True, True),
    (True, True, True, False, True),
    (True, True, True, True, True)])
def test_is_data_descriptor(get, set, value, writable, expected):
    pd = PropertyDescriptor()
    if get:
        pd.Get = None
    if set:
        pd.Set = None
    if value:
        pd.value = 10
    if writable:
        pd.value = True
    assert pd.is_data_descriptor() == expected

@pytest.mark.parametrize('get,set,value,writable,expected', [
    (False, False, False, False, True),
    (False, False, False, True, False),
    (False, False, True, False, False),
    (False, False, True, True, False),
    (False, True, False, False, False),
    (False, True, False, True, False),
    (False, True, True, False, False),
    (False, True, True, True, False),
    (True, False, False, False, False),
    (True, False, False, True, False),
    (True, False, True, False, False),
    (True, False, True, True, False),
    (True, True, False, False, False),
    (True, True, False, True, False),
    (True, True, True, False, False),
    (True, True, True, True, False)])
def test_is_generic_descriptor(get, set, value, writable, expected):
    pd = PropertyDescriptor()
    if get:
        pd.Get = None
    if set:
        pd.Set = None
    if value:
        pd.value = 10
    if writable:
        pd.value = True
    assert pd.is_generic_descriptor() == expected

def test_complete_property_descriptor_empty():
    pd = PropertyDescriptor()
    filled = pd.complete_property_descriptor()

    assert filled == pd
    assert hasattr(filled, 'value')
    assert hasattr(filled, 'writable')
    assert not hasattr(filled, 'Get')
    assert not hasattr(filled, 'Set')
    assert hasattr(filled, 'enumerable')
    assert hasattr(filled, 'configurable')
    assert filled.value is None
    assert not filled.writable
    assert not filled.enumerable
    assert not filled.configurable

def test_complete_property_descriptor_value():
    pd = PropertyDescriptor()
    pd.value = 'green'
    filled = pd.complete_property_descriptor()

    assert filled == pd
    assert hasattr(filled, 'value')
    assert hasattr(filled, 'writable')
    assert not hasattr(filled, 'Get')
    assert not hasattr(filled, 'Set')
    assert hasattr(filled, 'enumerable')
    assert hasattr(filled, 'configurable')
    assert filled.value == 'green'
    assert not filled.writable
    assert not filled.enumerable
    assert not filled.configurable

def test_complete_property_descriptor_writable():
    pd = PropertyDescriptor()
    pd.writable = True
    filled = pd.complete_property_descriptor()

    assert filled == pd
    assert hasattr(filled, 'value')
    assert hasattr(filled, 'writable')
    assert not hasattr(filled, 'Get')
    assert not hasattr(filled, 'Set')
    assert hasattr(filled, 'enumerable')
    assert hasattr(filled, 'configurable')
    assert filled.value is None
    assert filled.writable
    assert not filled.enumerable
    assert not filled.configurable

def test_complete_property_descriptor_Get():
    pd = PropertyDescriptor()
    pd.Get = 'red'
    filled = pd.complete_property_descriptor()

    assert filled == pd
    assert not hasattr(filled, 'value')
    assert not hasattr(filled, 'writable')
    assert hasattr(filled, 'Get')
    assert hasattr(filled, 'Set')
    assert hasattr(filled, 'enumerable')
    assert hasattr(filled, 'configurable')
    assert filled.Get == 'red'
    assert filled.Set is None
    assert not filled.enumerable
    assert not filled.configurable

def test_complete_property_descriptor_Set():
    pd = PropertyDescriptor()
    pd.Set = 'blue'
    filled = pd.complete_property_descriptor()

    assert filled == pd
    assert not hasattr(filled, 'value')
    assert not hasattr(filled, 'writable')
    assert hasattr(filled, 'Get')
    assert hasattr(filled, 'Set')
    assert hasattr(filled, 'enumerable')
    assert hasattr(filled, 'configurable')
    assert filled.Get is None
    assert filled.Set == 'blue'
    assert not filled.enumerable
    assert not filled.configurable

def test_complete_property_descriptor_enumerable():
    pd = PropertyDescriptor()
    pd.enumerable = True
    filled = pd.complete_property_descriptor()

    assert filled == pd
    assert hasattr(filled, 'value')
    assert hasattr(filled, 'writable')
    assert not hasattr(filled, 'Get')
    assert not hasattr(filled, 'Set')
    assert hasattr(filled, 'enumerable')
    assert hasattr(filled, 'configurable')
    assert filled.value is None
    assert not filled.writable
    assert filled.enumerable
    assert not filled.configurable

def test_complete_property_descriptor_configurable():
    pd = PropertyDescriptor()
    pd.configurable = True
    filled = pd.complete_property_descriptor()

    assert filled == pd
    assert hasattr(filled, 'value')
    assert hasattr(filled, 'writable')
    assert not hasattr(filled, 'Get')
    assert not hasattr(filled, 'Set')
    assert hasattr(filled, 'enumerable')
    assert hasattr(filled, 'configurable')
    assert filled.value is None
    assert not filled.writable
    assert not filled.enumerable
    assert filled.configurable

def test_IsAccessorDescriptor_None():
    assert not IsAccessorDescriptor(None)
def test_IsAccessorDescriptor_True():
    pd = PropertyDescriptor()
    pd.Get = None
    pd.Set = None
    assert IsAccessorDescriptor(pd)

def test_IsDataDescriptor_None():
    assert not IsDataDescriptor(None)
def test_IsDataDescriptor_True():
    pd = PropertyDescriptor()
    pd.value = 'puce'
    pd.writable = True
    assert IsDataDescriptor(pd)

def test_IsGenericDescriptor_None():
    assert not IsGenericDescriptor(None)
def test_IsGenericDescriptor_True():
    pd = PropertyDescriptor()
    pd.configurable = True
    assert IsGenericDescriptor(pd)

def test_CompletePropertyDescriptor():
    pd = PropertyDescriptor()
    filled = CompletePropertyDescriptor(pd)

    assert filled == pd
    assert hasattr(filled, 'value')
    assert hasattr(filled, 'writable')
    assert not hasattr(filled, 'Get')
    assert not hasattr(filled, 'Set')
    assert hasattr(filled, 'enumerable')
    assert hasattr(filled, 'configurable')
    assert filled.value is None
    assert not filled.writable
    assert not filled.enumerable
    assert not filled.configurable

def test_FromPropertyDescriptor_01():
    res = FromPropertyDescriptor(None)
    assert res is None

def test_FromPropertyDescriptor_02(realm):
    res = FromPropertyDescriptor(PropertyDescriptor(value=777, writable=True, enumerable=False, configurable=True))
    assert isObject(res)
    assert nc(HasOwnProperty(res, 'value'))
    assert nc(HasOwnProperty(res, 'writable'))
    assert nc(HasOwnProperty(res, 'enumerable'))
    assert nc(HasOwnProperty(res, 'configurable'))
    assert not nc(HasOwnProperty(res, 'set'))
    assert not nc(HasOwnProperty(res, 'get'))
    assert nc(Get(res, 'value')) == 777
    assert nc(Get(res, 'writable')) == True
    assert nc(Get(res, 'enumerable')) == False
    assert nc(Get(res, 'configurable')) == True

def test_FromPropertyDescriptor_03(realm):
    res = FromPropertyDescriptor(PropertyDescriptor(Set=None, Get=None))
    assert isObject(res)
    assert not nc(HasOwnProperty(res, 'value'))
    assert not nc(HasOwnProperty(res, 'writable'))
    assert not nc(HasOwnProperty(res, 'enumerable'))
    assert not nc(HasOwnProperty(res, 'configurable'))
    assert nc(HasOwnProperty(res, 'set'))
    assert nc(HasOwnProperty(res, 'get'))
    assert nc(Get(res, 'set')) is None
    assert nc(Get(res, 'get')) is None

def test_ToPropertyDescriptor_01(realm):
    # Can't make a descriptor out of a non-object.
    res = ToPropertyDescriptor(88)
    assert res.ctype == THROW
    assert nc(ToString(res.value)).startswith('TypeError')
    assert res.target is None

def test_ToPropertyDescriptor_02(obj):
    # Make a data descriptor:
    Set(obj, 'value', 100, False)
    Set(obj, 'enumerable', True, False)
    Set(obj, 'writable', True, False)
    Set(obj, 'configurable', True, False)

    res = ToPropertyDescriptor(obj)
    assert res.ctype == NORMAL
    assert res.target is None
    assert (res.value.value, res.value.writable, res.value.enumerable, res.value.configurable) == (100, True, True, True)
    assert not hasattr(res.value, 'Get')
    assert not hasattr(res.value, 'Set')

def test_ToPropertyDescriptor_03(obj):
    # Make an accessor descriptor
    def getter(this_value, new_target, propkey, receiver):
        return NormalCompletion(f'I got asked for a property named {propkey!r}.')
    def setter(this_value, new_target, propkey, value, receiver):
        print(f'Got asked to store {nc(ToString(value))} in property {propkey!r}')
        return NormalCompletion(True)
    getter_fcn = CreateBuiltinFunction(getter, [])
    setter_fcn = CreateBuiltinFunction(setter, [])

    Set(obj, 'enumerable', True, False)
    Set(obj, 'configurable', True, False)
    Set(obj, 'set', setter_fcn, False)
    Set(obj, 'get', getter_fcn, False)

    res = ToPropertyDescriptor(obj)
    assert res.ctype == NORMAL
    assert res.target is None
    assert (res.value.Get, res.value.Set, res.value.enumerable, res.value.configurable) == (getter_fcn, setter_fcn, True, True)
    assert not hasattr(res.value, 'value')
    assert not hasattr(res.value, 'writable')

@pytest.mark.parametrize('field', ['enumerable', 'value', 'writable', 'configurable', 'get', 'set'])
def test_ToPropertyDescriptor_04(mocker, obj, field):
    # Errors thrown from HasProperty. We use the "mocker" plugin to enable thowing at just the right time.
    def gen_patch_fcn(field):
        return lambda a, b: Completion(THROW, 'test throw', None) if b == field else False
    mocker.patch('ecmascript.HasProperty', side_effect=gen_patch_fcn(field))

    res = ToPropertyDescriptor(obj)
    assert res == Completion(THROW, 'test throw', None)

@pytest.mark.parametrize('field', ['enumerable', 'value', 'writable', 'configurable', 'get', 'set'])
def test_ToPropertyDescriptor_05(mocker, obj, field):
    # Errors thrown from Get. We use the "mocker" plugin to enable thowing at just the right time.
    def gen_patch_fcn(field):
        return lambda a, b: Completion(THROW, 'test throw', None) if b == field else None
    mocker.patch('ecmascript.Get', side_effect=gen_patch_fcn(field))

    Set(obj, 'value', 10, False)
    Set(obj, 'writable', True, False)
    Set(obj, 'enumerable', True, False)
    Set(obj, 'configurable', True, False)
    Set(obj, 'get', None, False)
    Set(obj, 'set', None, False)
    res = ToPropertyDescriptor(obj)
    assert res == Completion(THROW, 'test throw', None)

@pytest.mark.parametrize('field', ['get', 'set'])
def test_ToPropertyDescriptor_06(obj, field):
    # When the getter/setter is something that's not callable.
    Set(obj, field, 102, False)

    res = ToPropertyDescriptor(obj)
    assert res.ctype == THROW
    assert nc(ToString(res.value)).startswith('TypeError')
    assert res.target is None

@pytest.mark.parametrize('fields',
                         [ ['get', 'value'],
                           ['get', 'writable'],
                           ['get', 'value', 'writable'],
                           ['set', 'value'],
                           ['set', 'writable'],
                           ['set', 'writable', 'value'],
                           ['get', 'set', 'value'],
                           ['get', 'set', 'writable'],
                           ['get', 'set', 'value', 'writable'],
                         ])
def test_ToPropertyDescriptor_17(obj, fields):
    # When there's an accessor/declarative mismatch.
    for f in fields:
        Set(obj, f, None, False)
    res = ToPropertyDescriptor(obj)
    assert res.ctype == THROW
    assert nc(ToString(res.value)).startswith('TypeError')
    assert res.target is None

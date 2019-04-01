import pytest

from completion_record import Completion, CompletionType

import property_descriptor

NORMAL = CompletionType.NORMAL
THROW = CompletionType.THROW

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
    pd = property_descriptor.PropertyDescriptor()
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
    pd = property_descriptor.PropertyDescriptor()
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
    pd = property_descriptor.PropertyDescriptor()
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
    pd = property_descriptor.PropertyDescriptor()
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
    pd = property_descriptor.PropertyDescriptor()
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
    pd = property_descriptor.PropertyDescriptor()
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
    pd = property_descriptor.PropertyDescriptor()
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
    pd = property_descriptor.PropertyDescriptor()
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
    pd = property_descriptor.PropertyDescriptor()
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
    pd = property_descriptor.PropertyDescriptor()
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
    assert not property_descriptor.IsAccessorDescriptor(None)
def test_IsAccessorDescriptor_True():
    pd = property_descriptor.PropertyDescriptor()
    pd.Get = None
    pd.Set = None
    assert property_descriptor.IsAccessorDescriptor(pd)

def test_IsDataDescriptor_None():
    assert not property_descriptor.IsDataDescriptor(None)
def test_IsDataDescriptor_True():
    pd = property_descriptor.PropertyDescriptor()
    pd.value = 'puce'
    pd.writable = True
    assert property_descriptor.IsDataDescriptor(pd)

def test_IsGenericDescriptor_None():
    assert not property_descriptor.IsGenericDescriptor(None)
def test_IsGenericDescriptor_True():
    pd = property_descriptor.PropertyDescriptor()
    pd.configurable = True
    assert property_descriptor.IsGenericDescriptor(pd)

def test_CompletePropertyDescriptor():
    pd = property_descriptor.PropertyDescriptor()
    filled = property_descriptor.CompletePropertyDescriptor(pd)

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

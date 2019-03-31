import pytest
import math

import abstract

from jstypes import JSNull, wks_to_primitive
from completion_record import Completion, CompletionType
NORMAL = CompletionType.NORMAL
THROW = CompletionType.THROW

def test_toprimitive():
    # Need the object inputs, in the future!!
    cr = abstract.ToPrimitive('string')
    assert cr == Completion(ctype=NORMAL, value='string', target=None)

@pytest.mark.parametrize('input,expected', [
    (None, False),
    (True, True),
    (False, False),
    (JSNull.NULL, False),
    (math.nan, False),
    (math.inf, True),
    (0, False),
    (-0.0, False),
    (10, True),
    (-10, True),
    ('', False),
    ('yugoloth', True),
    (wks_to_primitive, True)
    ])
def test_to_boolean(input, expected):
    cr = abstract.ToBoolean(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)

@pytest.mark.parametrize('input,expected', [
    (None, math.nan),
    (JSNull.NULL, 0),
    (True, 1),
    (False, 0),
    (67, 67),
    ('0', 0),
    ('1', 1),
    ('0b101', 5),
    ('0XAB', 0xab),
    ('0O773', 0o773),
    ('-10', -10),
    ('+10.5', 10.5),
    ('10.e3', 10000),
    ('1e+3', 1000),
    ('.9e-3', 0.0009),
    ('10.5e-2', 0.105),
    ('6.', 6),
    ('.5', 0.5),
    ('+Infinity', math.inf),
    ('-Infinity', -math.inf),
    ('Infinity', math.inf),
    ('inf', math.nan),
    ('\n\n56\t \r', 56),
    ('0x'+'75789240758907289067984302789758920789089729803478592078975819920375801'*4, math.inf),
    ('0b'+'1'*1025, math.inf),
    ('0o'+'7'*342, math.inf),
    ('100e4000', math.inf),
    ('-5e4000', -math.inf),
    ('5e-4000', 0)
    ])
def test_to_number(input, expected):
    cr = abstract.ToNumber(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)

@pytest.mark.parametrize('input,expected', [
    ('-0', '-0.0'),
    ('-.0', '-0.0'),
    ('-0.0', '-0.0')])
def test_to_number_negative_zero(input, expected):
    cr = abstract.ToNumber(input)
    cr2 = Completion(ctype=cr.ctype, value=str(cr.value), target=cr.target)
    assert cr2 == Completion(ctype=NORMAL, value=expected, target=None)

def test_to_number_symbol():
    cr = abstract.ToNumber(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    ('goblin', 0),
    (0, 0),
    (-math.inf, -math.inf),
    (math.inf, math.inf),
    (10.3, 10.0),
    (-23.3, -23.0)])
def test_to_integer(input, expected):
    cr = abstract.ToInteger(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)

def test_to_integer_negative_zero():
    cr = abstract.ToInteger(-0.0)
    cr2 = Completion(ctype=cr.ctype, value=str(cr.value), target=cr.target)
    assert cr2 == Completion(ctype=NORMAL, value='-0.0', target=None)

def test_to_integer_symbol():
    cr = abstract.ToInteger(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    (math.nan, 0),
    (0, 0),
    (math.inf, 0),
    (-math.inf, 0),
    (0x1234567890, 0x34567890),
    (0xb4567890, -0x4ba98770),
    (367, 367),
    (-56, -56)])
def test_to_int32(input, expected):
    cr = abstract.ToInt32(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)
    assert cr.value == abstract.ToInt32(cr.value).value  # Idempotentcy check

def test_to_int32_neg_zero():
    cr = abstract.ToInt32(-0.0)
    cr2 = Completion(ctype=cr.ctype, value=str(cr.value), target=cr.target)
    assert cr2 == Completion(ctype=NORMAL, value='0', target=None)

def test_to_int32_symbol():
    cr = abstract.ToInt32(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    (math.nan, 0),
    (0, 0),
    (math.inf, 0),
    (-math.inf, 0),
    (0x1234567890, 0x34567890),
    (0xb4567890, 0xb4567890),
    (367, 367),
    (-56, 0xffffffc8)])
def test_to_uint32(input, expected):
    cr = abstract.ToUint32(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)

def test_to_uint32_symbol():
    cr = abstract.ToUint32(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    (math.nan, 0),
    (0, 0),
    (math.inf, 0),
    (-math.inf, 0),
    (0x1234567890, 0x7890),
    (0xf890, -0x770),
    (367, 367),
    (-56, -56)])
def test_to_int16(input, expected):
    cr = abstract.ToInt16(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)
def test_to_int16_symbol():
    cr = abstract.ToInt16(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    (math.nan, 0),
    (0, 0),
    (math.inf, 0),
    (-math.inf, 0),
    (0x1234567890, 0x7890),
    (0xb456f890, 0xf890),
    (367, 367),
    (-56, 0xffc8)])
def test_to_uint16(input, expected):
    cr = abstract.ToUint16(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)

def test_to_uint16_symbol():
    cr = abstract.ToUint16(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    (math.nan, 0),
    (0, 0),
    (math.inf, 0),
    (-math.inf, 0),
    (0x12345678, 0x78),
    (0x90, -0x70),
    (67, 67),
    (-56, -56)])
def test_to_int8(input, expected):
    cr = abstract.ToInt8(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)
def test_to_int8_symbol():
    cr = abstract.ToInt8(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    (math.nan, 0),
    (0, 0),
    (math.inf, 0),
    (-math.inf, 0),
    (0x12345678, 0x78),
    (0xb456f890, 0x90),
    (67, 67),
    (-56, 0xc8)])
def test_to_uint8(input, expected):
    cr = abstract.ToUint8(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)

def test_to_uint8_symbol():
    cr = abstract.ToUint8(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    ('goblin', 0),
    ('-0.0', 0),
    (903, 255),
    (-3, 0),
    (math.inf, 255),
    ('-Infinity', 0),
    (20, 20),
    (19.2, 19),
    (19.7, 20),
    (19.5, 20),
    (18.5, 18)])
def test_to_uint8_clamp(input, expected):
    cr = abstract.ToUint8Clamp(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)
def test_to_uint8_clamp_symbol():
    cr = abstract.ToUint8Clamp(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

@pytest.mark.parametrize('input,expected', [
    (None, 'undefined'),
    (JSNull.NULL, 'null'),
    (True, 'true'),
    (False, 'false'),
    ('green', 'green'),
    (3.25, '3.25'),
    (math.inf, 'Infinity'),
    (-math.inf, '-Infinity'),
    (math.nan, 'NaN'),
    (-0.0, '0')
])
def test_to_string(input, expected):
    cr = abstract.ToString(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)
def test_to_string_symbol():
    cr = abstract.ToString(wks_to_primitive)
    assert cr.ctype == THROW
    assert cr.target is None
    assert isinstance(cr.value, TypeError)

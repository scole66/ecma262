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
    ('+Infinity', math.inf),
    ('-Infinity', -math.inf),
    ('Infinity', math.inf),
    ('inf', math.nan),
    ('\n\n56\t \r', 56),
    ('0x'+'75789240758907289067984302789758920789089729803478592078975819920375801'*1000, math.inf),
    ('100e4000', math.inf),
    ('-5e4000', -math.inf),
    ('5e-4000', 0)
    ])
def test_to_number(input, expected):
    cr = abstract.ToNumber(input)
    assert cr == Completion(ctype=NORMAL, value=expected, target=None)

def test_to_number_symbol():
    cr = abstract.ToNumber(wks_to_primitive)
    assert cr.ctype == THROW

import pytest

from ecmascript import *

@pytest.mark.parametrize('test_input,expected', [
    (ord('a'),[ord('a')]),
    (0x24, [0x24]),
    (0x20ac, [0x20ac]),
    (0x10437, [0xd801, 0xdc37]),
    (0x24b62, [0xd852, 0xdf62])
    ])
def test_utf_16_encoding(test_input, expected):
    assert utf_16_encoding(test_input) == expected

@pytest.mark.parametrize('lead,trail,expected', [
    (0xd801, 0xdc37, 0x10437),
    (0xd852, 0xdf62, 0x24b62)
    ])
def test_utf_16_decode(lead, trail, expected):
    assert utf_16_decode(lead, trail) == expected

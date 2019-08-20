import pytest

from ecmascript import *


@pytest.mark.parametrize(
    "test_input,expected",
    [
        (ord("a"), [ord("a")]),
        (0x24, [0x24]),
        (0x20AC, [0x20AC]),
        (0x10437, [0xD801, 0xDC37]),
        (0x24B62, [0xD852, 0xDF62]),
    ],
)
def test_utf_16_encoding(test_input, expected):
    assert utf_16_encoding(test_input) == expected


@pytest.mark.parametrize("lead,trail,expected", [(0xD801, 0xDC37, 0x10437), (0xD852, 0xDF62, 0x24B62)])
def test_utf_16_decode(lead, trail, expected):
    assert utf_16_decode(lead, trail) == expected

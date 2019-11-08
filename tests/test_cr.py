import pytest

from ecmascript.ecmascript import *


@pytest.mark.parametrize("cr, val, expected", [(56, EMPTY, 56), (EMPTY, "gobble", "gobble"), ("floop", 888, "floop")])
def test_UpdateEmpty(cr, val, expected):
    r = UpdateEmpty(cr, val)
    assert r == expected

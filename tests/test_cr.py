import pytest

from ecmascript import *


@pytest.mark.parametrize(
    "cr, val, expected", [(56, Empty.EMPTY, 56), (Empty.EMPTY, "gobble", "gobble"), ("floop", 888, "floop")]
)
def test_UpdateEmpty(cr, val, expected):
    r = UpdateEmpty(cr, val)
    assert r == expected

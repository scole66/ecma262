import pytest
from itertools import chain

import ecmascript

# 6.2.7.1 CreateByteDataBlock ( size )
# When the abstract operation CreateByteDataBlock is called with integer argument size, the following steps are
# taken:
#
#   1. Assert: size≥0.
#   2. Let db be a new Data Block value consisting of size bytes. If it is impossible to create such a Data Block,
#      throw a RangeError exception.
#   3. Set all of the bytes of db to 0.
#   4. Return db.
@pytest.mark.parametrize("testlen", [10, 100, 0])
def test_CreateByteDataBlock_01(testlen):
    res = ecmascript.CreateByteDataBlock(testlen)

    assert len(res) == testlen
    assert all(res[i] == 0 for i in range(testlen))


# 6.2.7.3 CopyDataBlockBytes ( toBlock, toIndex, fromBlock, fromIndex, count )
@pytest.fixture
def test_blocks():
    toBlock = ecmascript.CreateByteDataBlock(100)
    fromBlock = ecmascript.CreateByteDataBlock(100)

    for i in range(100):
        toBlock[i] = i
        fromBlock[i] = 100 - i

    return (toBlock, fromBlock)


@pytest.mark.parametrize(
    "to_index, from_index, count, expected",
    [
        (0, 0, 0, range(100)),
        (0, 0, 100, range(100, -1, -1)),
        (0, 10, 30, chain(range(90, 60, -1), range(30, 100))),
        (35, 0, 10, chain(range(35), range(100, 90, -1), range(45, 100))),
    ],
)
def test_CopyDataBlockBytes_01(test_blocks, to_index, from_index, count, expected):
    to_block, from_block = test_blocks
    res = ecmascript.CopyDataBlockBytes(to_block, to_index, from_block, from_index, count)

    assert res == ecmascript.EMPTY
    assert len(to_block) == 100
    assert len(from_block) == 100
    assert all(to_val == expected_val for to_val, expected_val in zip(to_block, expected))
    assert all(from_val == expected_val for from_val, expected_val in zip(from_block, range(100, -1, -1)))

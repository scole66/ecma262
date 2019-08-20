# Test-Stubs
#
# This is an in-progress test file, ensuring that our stubs do stub-like things. It's here to help get to complete
# test coverage. When the project is complete and there are no more stubs, this file should be empty.

import pytest

import ecmascript


def test_Await():
    with pytest.raises(NotImplementedError):
        ecmascript.Await("arg")

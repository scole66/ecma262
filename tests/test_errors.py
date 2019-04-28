import pytest

import ecmascript

@pytest.fixture
def realm():
    ecmascript.InitializeHostDefinedRealm()
    yield ecmascript.surrounding_agent.running_ec.realm
    ecmascript.surrounding_agent.ec_stack.pop()
    ecmascript.surrounding_agent.running_ec = None

def test_CreateReferenceError_01(realm):
    re = ecmascript.CreateReferenceError('test reference error')

    assert ecmascript.isObject(re)
    assert ecmascript.nc(ecmascript.ToString(re)) == 'ReferenceError: test reference error'

def test_CreateTypeError_01(realm):
    re = ecmascript.CreateTypeError('test type error')

    assert ecmascript.isObject(re)
    assert ecmascript.nc(ecmascript.ToString(re)) == 'TypeError: test type error'

def test_CreateSyntaxError_01(realm):
    re = ecmascript.CreateSyntaxError('test syntax error')

    assert ecmascript.isObject(re)
    assert ecmascript.nc(ecmascript.ToString(re)) == 'SyntaxError: test syntax error'

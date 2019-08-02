# conftest.py
#
# This file contains functions & fixtures that all test files can use (without import).
#

import pytest
import ecmascript.ecmascript
from .helpers import Expected_Exception


@pytest.fixture
def realm():
    ecmascript.ecmascript.InitializeHostDefinedRealm()
    yield ecmascript.ecmascript.surrounding_agent.running_ec.realm
    ecmascript.ecmascript.surrounding_agent.ec_stack = []
    ecmascript.ecmascript.surrounding_agent.running_ec = None


@pytest.fixture
def context():
    return ecmascript.ecmascript.Parse2Context(syntax_error_ctor=Expected_Exception)

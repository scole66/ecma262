# conftest.py
#
# This file contains functions & fixtures that all test files can use (without import).
#

import pytest
from ecmascript import InitializeHostDefinedRealm, surrounding_agent

@pytest.fixture
def realm():
    InitializeHostDefinedRealm()
    yield surrounding_agent.running_ec.realm
    surrounding_agent.ec_stack = []
    surrounding_agent.running_ec = None

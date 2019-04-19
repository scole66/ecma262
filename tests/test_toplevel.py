'''
This one is for top-level tests. I.e.: Send some JS into the parser and see what comes out after evaluation.
'''

import pytest
import math

from ecmascript import *
NORMAL = CompletionType.NORMAL

@pytest.fixture
def cleanup():
    yield None
    surrounding_agent.ec_stack = []
    surrounding_agent.running_ec = None

@pytest.mark.parametrize('script, result', [
    ('', None),
    ('Infinity;', math.inf),
    ('bob = 3;', 3),
    ('bob = 3; bob *= 2;', 6),
    ('7, 8;', 8),
    ('''
    true ? 'truth value' : 'falsehood value';
    ''',
    'truth value'
    ),
    ('''
    false ? 'truth value' : 'falsehood value';
    ''',
    'falsehood value'
    ),
    ('67 || 99;', 67),
    ('67 && 99;', 99),
    ('false || 102;', 102),
    ('null && \'thing\';', JSNull.NULL),
    ('0x80 | 0x34;', 0b10110100),
    ('0x73 & 0x29;', 0x21),
    ('0xff ^ 0x156;', 0x1a9),
    ("'3' == 3;", True),
    ("'-0' == 0;", True),
    ('67 == 89;', False),
    ("'t1' == 't1';", True),
    ("'3' != 3;", False),
    ("'-0' != 0;", False),
    ('67 != 89;', True),
    ("'t1' != 't1';", False),
    ("'3' === 3;", False),
    ("3 === 3;", True),
    ("'3' !== 3;", True),
    ("3 !== 3;", False),
    ('3 < 4;', True),
    ('3 <= 4;', True),
    ('3 > 4;', False),
    ('3 >= 3;', True),
    ("'green' instanceof Number;", False),
    ('Number instanceof Object;', True),
    ('0xffffffff >> 16;', -1),
    ('0xffffffff >>> 16;', 0x0000ffff),
    ('0xffffffff << 8;', -256),
    ('3 + 7;', 10),
    ('100 - 25;', 75),
    ("'th' + 'ing';", 'thing'),
    pytest.param('-Infinity + -Infinity;', -math.inf, marks=pytest.mark.xfail),
    ('Infinity + Infinity;', math.inf),
    ('Infinity + 88;', math.inf),
])
def test_scripts_01(cleanup, script, result):
    rv = RunJobs(scripts=[script])
    assert rv == Completion(NORMAL, result, None)

@pytest.mark.parametrize('script', [
    'NaN + 3;',
    '78 + NaN;',
    pytest.param('-Infinity + Infinity;', marks=pytest.mark.xfail),
    pytest.param('Infinity + -Infinity;', marks=pytest.mark.xfail),
])
def test_scripts_02(cleanup, script):
    # Check for those expressions that return NaN.
    rv = RunJobs(scripts=[script])
    assert rv.ctype == NORMAL
    assert rv.target is None
    assert math.isnan(rv.value)

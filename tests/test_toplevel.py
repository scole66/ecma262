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
    ('-Infinity + -Infinity;', -math.inf),
    ('Infinity + Infinity;', math.inf),
    ('Infinity + 88;', math.inf),
    ("'prototype' in Object;", True),
    ("'yuccaplant' in Object;", False),
    ('{}', None),
    ('{ b=3; b*=6; }', 18),
    ('var x;', None),
    ('var xyz=67+99; xyz;', 67+99),
    ('1 * 1;', 1),
    ('121 / 11;', 11),
    ('67 % 16;', 3),
    ('4**16;', 4**16),
    ('xyz=8881; delete xyz;', True),
    ('foo=NaN; typeof foo;', 'number'),
    ('typeof missing;', 'undefined'),
    ('typeof true;', 'boolean'),
    ('typeof \'a\';', 'string'),
    ('typeof Number;', 'function'),
    ('+89;', 89),
    ('-900;', -900),
    ('~0x80008000;', 0x7fff7fff),
    ('!true;', False),
    ('x=100; --x;', 99),
    ('x=100; x--;', 100),
    ('x=100; x--; x;', 99),
    ('x=100; ++x;', 101),
    ('x=100; x++;', 100),
    ('x=100; x++; x;', 101),
    ('new Number - 0;', 0),
    ('new Number(60) - 0;', 60),
    ("Number('55');", 55),
    ('      Boolean(60);', True),
    ('n = new Number(3); n[\'extra\'] = 6; n[\'extra\'];', 6),
    ('puppy = new Boolean(false); puppy[\'true\'] = 888; puppy.true;', 888),
    ('bob = new Object(); bob.thing_1 = 99; bob.thing_2 = 102; bob.NUMBER = -1234;', -1234),
    ('((4+8)*(12-3)+1)/(13-3);', 10.9),
    ("if (true) { 'true value'; } else { 'false value'; };", 'true value'),
    ("if (false) { 'true value'; } else { 'false value'; };", 'false value'),
    ("if (true) { 'true value'; };", 'true value'),
    ("if (false) { 'true value'; };", None),

])
def test_scripts_01(cleanup, script, result):
    rv = RunJobs(scripts=[script])
    assert rv == Completion(NORMAL, result, None)

@pytest.mark.parametrize('script', [
    'NaN + 3;',
    '78 + NaN;',
    '-Infinity + Infinity;',
    'Infinity + -Infinity;',
])
def test_scripts_02(cleanup, script):
    # Check for those expressions that return NaN.
    rv = RunJobs(scripts=[script])
    assert rv.ctype == NORMAL
    assert rv.target is None
    assert math.isnan(rv.value)

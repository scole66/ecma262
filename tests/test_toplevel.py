'''
This one is for top-level tests. I.e.: Send some JS into the parser and see what comes out after evaluation.
'''

import pytest
import math

from ecmascript import *
NORMAL = CompletionType.NORMAL

@pytest.mark.parametrize('script, result', [
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
])
def test_scripts_01(script, result):
    rv = RunJobs(scripts=[script])
    assert rv == Completion(NORMAL, result, None)

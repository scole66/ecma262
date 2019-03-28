import pytest

import completion_record

def test_NormalCompletion():
    z = completion_record.NormalCompletion('test_string')

    assert z.ctype == completion_record.CompletionType.NORMAL
    assert z.value == 'test_string'
    assert z.target is None

def test_ThrowCompletion():
    z = completion_record.ThrowCompletion('test_string')

    assert z.ctype == completion_record.CompletionType.THROW
    assert z.value == 'test_string'
    assert z.target is None

def test_UpdateEmpty():
    z = completion_record.Completion(completion_record.CompletionType.BREAK, None, None)

    r = completion_record.UpdateEmpty(z, 'update_string')

    assert r.ctype == z.ctype
    assert r.target == z.target
    assert r.value == 'update_string'

    z2 = completion_record.NormalCompletion('gobbledygook')
    r2 = completion_record.UpdateEmpty(z2, 'update_string')

    assert r2.ctype == z2.ctype
    assert r2.target == z2.target
    assert r2.value == 'gobbledygook'

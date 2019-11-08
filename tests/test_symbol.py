import pytest

from ecmascript.ecmascript import SymbolDescriptiveString, JSSymbol


@pytest.mark.parametrize("inp, expected", [("", "Symbol()"), ("word", "Symbol(word)"), (None, "Symbol()")])
def test_SymbolDescriptiveString_01(realm, inp, expected):
    sym = JSSymbol(inp)
    assert SymbolDescriptiveString(sym) == expected

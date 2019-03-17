# Stuff from chap 10: Source Text

# https://www.ecma-international.org/ecma-262/9.0/index.html#sec-utf16encoding
# 10.1.1 Static Semantics: UTF16Encoding(cp)
#
# The UTF16Encoding of a numeric code point value, cp, is determined as follows:
#
# Assert: 0 ≤ cp ≤ 0x10FFFF.
# If cp ≤ 0xFFFF, return cp.
# Let cu1 be floor((cp - 0x10000) / 0x400) + 0xD800.
# Let cu2 be ((cp - 0x10000) modulo 0x400) + 0xDC00.
# Return the code unit sequence consisting of cu1 followed by cu2.

def utf_16_encoding(cp):
    assert 0 <= cp and cp <= 0x10ffff
    if cp <= 0xffff:
        return [cp]
    cpx = cp - 0x10000
    return [(cpx >> 10) + 0xD800, (cpx & 0x3ff) + 0xdc00]


# https://www.ecma-international.org/ecma-262/9.0/index.html#sec-utf16decode
# 10.1.2 Static Semantics: UTF16Decode ( lead, trail )
# Two code units, lead and trail, that form a UTF-16 surrogate pair are converted to a code point by performing the following steps:
#
# Assert: lead is a leading surrogate and trail is a trailing surrogate.
# Let cp be (lead - 0xD800) × 0x400 + (trail - 0xDC00) + 0x10000.
# Return the code point cp.
def utf_16_decode(lead, trail):
    assert 0xd800 <= lead and lead <= 0xdbff
    assert 0xdc00 <= trail and trail <= 0xdfff
    return ((lead - 0xd800) << 10) + trail - 0xdc00 + 0x10000

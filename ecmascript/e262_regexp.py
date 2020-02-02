# Regexp parser (section 21.2.1) for ECMAScript 2019
#

# pylint: disable=assignment-from-none

from typing import (
    Optional,
    Tuple,
    Union,
    Callable,
    Generic,
    TypeVar,
    NamedTuple,
    cast,
    List,
    Iterable,
    Sequence,
    Iterator,
    Dict,
    Any,
    Set,
)
from collections import namedtuple, Counter
from copy import copy
from itertools import chain
from functools import reduce
from enum import Enum
import math
from .lexer2 import utf_16_encode, utf_16_decode
import unicodedata
import regex  # type: ignore
import snoop  # type: ignore
from typing import TYPE_CHECKING, TypeVar

if TYPE_CHECKING:

    def lru_cache(f: Callable[..., Any]) -> Callable[..., Any]:
        pass

    def cached_property(f: Callable[..., Any]) -> Callable[..., Any]:
        pass


else:
    from functools import lru_cache, cached_property

# pylint: disable=no-value-for-parameter
A = TypeVar("A")
B = TypeVar("B")

#
# The fundamental data object here is a span of characters in an input string.
#
class Span(NamedTuple):
    start: int
    after: int


class Failure(Enum):
    FAILURE = 1


class State(NamedTuple):
    endIndex: int
    captures: List[Optional[str]]


MatchResult = Union[State, Failure]
FAILURE: MatchResult = Failure.FAILURE
Continuation = Callable[[State], MatchResult]
Matcher = Callable[[State, Continuation], MatchResult]
AssertionTester = Callable[[State], bool]


class CharSet:
    _maxchar = 0x10FFFF

    class Range(NamedTuple):
        start: int
        after: int

    def __init__(self, charset: str, inverted: bool = False):
        choices = sorted(set(charset))
        # find all the places in that string where a "gap" exists. We'll chose the index of the char following
        # a gap as the value to track. Note that there are gaps at both the start and the end of the string (with values
        # 0 and len(choices))
        gaps = []
        for idx, ch in enumerate(choices):
            if idx == 0 or ch != chr(ord(choices[idx - 1]) + 1):
                gaps.append(idx)
        gaps.append(len(choices))
        # Form a list of Pairs: "first-index-of-range", "last-index-of-range"
        ranges = []
        for idx in range(len(gaps) - 1):
            f = gaps[idx]
            l = gaps[idx + 1] - 1
            ranges.append(self.Range(ord(choices[f]), ord(choices[l]) + 1))

        # Now invert, if needed.
        if inverted:
            if len(ranges) == 0:
                iranges = [self.Range(0, self._maxchar + 1)]
            else:
                if ranges[0].start != 0:
                    iranges = [self.Range(0, ranges[0].start)]
                for idx in range(len(ranges) - 1):
                    rleft = ranges[idx]
                    rright = ranges[idx + 1]
                    iranges.append(self.Range(rleft.after, rright.start))
                if ranges[-1].after != self._maxchar + 1:
                    iranges.append(self.Range(ranges[-1].after, self._maxchar + 1))
            self.ranges = iranges
        else:
            self.ranges = ranges

    def __contains__(self, item: str) -> bool:
        return (
            isinstance(item, str) and len(item) == 1 and any(r.start <= ord(item[0]) < r.after for r in self.ranges)
        )

    def __len__(self) -> int:
        return sum(r.after - r.start for r in self.ranges)

    def __or__(self, other: "CharSet") -> "CharSet":
        def cvtranges(*ranges) -> Iterator[int]:
            for range in ranges:
                for r in range:
                    yield r.start
                    yield -r.after

        cvtd = sorted(list(cvtranges(self.ranges, other.ranges)), key=lambda x: abs(x) + (0.5 if x < 0 else 0))

        def merge(points: List[int]) -> Iterator[int]:
            state = 0
            for p in points:
                if p >= 0:
                    state += 1
                    if state == 1:
                        yield p
                else:
                    state -= 1
                    if state == 0:
                        yield -p

        def arrange(points: Iterable[int]) -> Iterator[self.Range]:
            while 1:
                a = next(points, None)
                if a is None:
                    break
                b = next(points, None)
                if b is None:
                    break
                yield self.Range(a, b)

        ranges = list(arrange(merge(cvtd)))
        cs = CharSet("")
        cs.ranges = ranges
        return cs

    def __iter__(self) -> Iterator[str]:
        for r in self.ranges:
            for chval in range(r.start, r.after):
                yield chr(chval)


class Context:
    def __init__(
        self,
        n_capturing_parens: int,
        dot_all: bool,
        ignore_case: bool,
        multiline: bool,
        unicode: bool,
        pattern: "Pattern",
    ) -> None:
        self.NcapturingParens = n_capturing_parens
        self.DotAll = dot_all
        self.IgnoreCase = ignore_case
        self.Multiline = multiline
        self.Unicode = unicode
        self.Input = ""
        self.InputLength = 0
        self.pattern = pattern


# A production has a particular type, a set of child productions, and a span of characters that it covers.
Child = Union["Production", str]
Children = Tuple[Child, ...]


class Production:
    def __init__(self, src: str, span: Span, children: Children):
        self.span = span
        self.children = children
        self.src = src

    def __repr__(self) -> str:
        return f"{self.__class__.__name__}({self.src[self.span.start:self.span.after]}@{self.span.start})"

    @cached_property
    def earlyerrors(self) -> List[str]:
        return list(chain(*(c.earlyerrors for c in self.children if isinstance(c, Production))))

    @cached_property
    def group_names(self) -> List[str]:
        return list(chain(*(c.group_names for c in self.children if isinstance(c, Production))))

    @cached_property
    def group_references(self) -> List[str]:
        return list(chain(*(c.group_references for c in self.children if isinstance(c, Production))))

    @cached_property
    def NcapturingParens(self) -> int:
        return sum(c.NcapturingParens for c in self.children if isinstance(c, Production))

    @cached_property
    def group_numbers(self) -> List[int]:
        return list(chain(*(c.group_numbers for c in self.children if isinstance(c, Production))))

    @cached_property
    def capture_map(self) -> Dict[int, str]:
        # For each of our children, collect their own capture map and their NcapturingParens value,
        # then combine the dicts together in one dict, adjusting the keys by the prior child's capturingParens
        # value.
        data: List[Tuple[Dict[int, str], int]]
        data = [(ch.capture_map, ch.NcapturingParens) for ch in self.children if isinstance(ch, Production)]

        def reductor(
            acc: Tuple[Dict[int, str], int], item: Tuple[Dict[int, str], int]
        ) -> Tuple[Dict[int, str], int]:
            d, offset = acc
            new_d, offset_delta = item
            for key, value in new_d.items():
                d[key + offset] = value
            return (d, offset + offset_delta)

        empty: Dict[int, str] = {}
        return reduce(reductor, data, (empty, 0))[0]

    @lru_cache
    def parens_left_of(self, target: "Production") -> Tuple[int, bool]:
        if self == target:
            return (0, True)
        count = 0
        if target in self.children:
            for ch in self.children:
                if ch == target:
                    break
                count += ch.NcapturingParens
            return (count, True)
        for ch in (ch for ch in self.children if isinstance(ch, Production)):
            amt, found = ch.parens_left_of(target)
            count += amt
            if found:
                return (count, True)
        return (count, False)


def join(lst: Sequence[A], sep: B) -> Iterator[Union[A, B]]:
    for item in lst[:-1]:
        yield item
        yield sep
    if lst:
        yield lst[-1]


#### ClassEscape ######################################################################################################
#
#  .oOOOo.   o                    o.OOoOoo
# .O     o  O                      O
# o         o                      o
# o         O                      ooOO
# o         o  .oOoO' .oOo  .oOo   O       .oOo  .oOo  .oOoO' .oOo. .oOo.
# O         O  O   o  `Ooo. `Ooo.  o       `Ooo. O     O   o  O   o OooO'
# `o     .o o  o   O      O     O  O           O o     o   O  o   O O
#  `OoooO'  Oo `OoO'o `OoO' `OoO' ooOooOoO `OoO' `OoO' `OoO'o oOoO' `OoO'
#                                                             O
#                                                             o'
#
#######################################################################################################################
class ClassEscape(Production):
    pass


class ClassEscape_b(ClassEscape):
    def __init__(self, src: str, position: int):
        super.src = src
        super.span = Span(position, position + 1)
        super.children = ("b",)

    CharacterValue = 8  # BACKSPACE
    IsCharacterClass = False

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.19 ClassEscape
        # The ClassEscape productions evaluate as follows:
        # ClassEscape::b
        #   1. Let cv be the CharacterValue of this ClassEscape.
        #   2. Let c be the character whose character value is cv.
        #   3. Return the CharSet containing the single character c.
        return CharSet(chr(self.CharacterValue))


class ClassEscape_dash(ClassEscape):
    def __init__(self, src: str, position: int):
        super.src = src
        super.span = Span(position, position + 1)
        super.children = ("-",)

    CharacterValue = 0x2D  # HYPHEN-MINUS
    IsCharacterClass = False

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.19 ClassEscape
        # The ClassEscape productions evaluate as follows:
        # ClassEscape::-
        #   1. Let cv be the CharacterValue of this ClassEscape.
        #   2. Let c be the character whose character value is cv.
        #   3. Return the CharSet containing the single character c.
        return CharSet(chr(self.CharacterValue))


class ClassEscape_CCE(ClassEscape):
    def __init__(self, src: str, cce: "CharacterClassEscape"):
        self.src = src
        self.span = cce.span
        self.children = (cce,)
        self.character_class_escape = cce

    IsCharacterClass = True

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.19 ClassEscape
        # The ClassEscape productions evaluate as follows:
        # ClassEscape :: CharacterClassEscape
        #   1. Return the CharSet that is the result of evaluating CharacterClassEscape.
        return self.character_class_escape.evaluate(context)


class ClassEscape_CE(ClassEscape):
    def __init__(self, src: str, ce: "CharacterEscape"):
        self.src = src
        self.span = ce.span
        self.children = (ce,)
        self.character_escape = ce

    @cached_property
    def CharacterValue(self) -> int:
        return self.character_escape.CharacterValue

    IsCharacterClass = False

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.19 ClassEscape
        # The ClassEscape productions evaluate as follows:
        # ClassEscape::CharacterEscape
        #   1. Let cv be the CharacterValue of this ClassEscape.
        #   2. Let c be the character whose character value is cv.
        #   3. Return the CharSet containing the single character c.
        return CharSet(chr(self.CharacterValue))


def parse_ClassEscape(src: str, position: int, U: bool) -> Optional[ClassEscape]:
    # Syntax:
    #   ClassEscape[U]::
    #       b
    #       [+U] -
    #       CharacterClassEscape[?U]
    #       CharacterEscape[?U]
    choice1: Tuple[Callable[[], Optional[ClassEscape]], int] = (lambda: None, 0)
    choice2: Tuple[Callable[[], Optional[ClassEscape]], int] = (lambda: None, 0)
    choice3: Tuple[Callable[[], Optional[ClassEscape]], int] = (lambda: None, 0)
    choice4: Tuple[Callable[[], Optional[ClassEscape]], int] = (lambda: None, 0)
    if position < len(src) and src[position] == "b":
        choice1 = (lambda: ClassEscape_b(src, position), position + 1)
    if U and position < len(src) and src[position] == "-":
        choice2 = (lambda: ClassEscape_dash(src, position), position + 1)
    cce = parse_CharacterClassEscape(src, position, U)
    if cce:
        choice3 = (lambda: ClassEscape_CCE(src, cce), cce.span.after)
    ce = parse_CharacterEscape(src, position, U)
    if ce:
        choice4 = (lambda: ClassEscape_CE(src, ce), ce.span.after)
    make, _ = max((choice1, choice2, choice3, choice4), key=lambda t: t[1])
    return make()


#### ClassAtomNoDash ##################################################################################################
#
#  .oOOOo.   o                       Oo                         o.     O       o.OOOo.                 o
# .O     o  O                       o  O                        Oo     o        O    `o               O
# o         o                      O    o    O                  O O    O        o      O              o
# o         O                     oOooOoOo  oOo                 O  o   o        O      o              O
# o         o  .oOoO' .oOo  .oOo  o      O   o   .oOo. `oOOoOO. O   o  O .oOo.  o      O .oOoO' .oOo  OoOo.
# O         O  O   o  `Ooo. `Ooo. O      o   O   O   o  O  o  o o    O O O   o  O      o O   o  `Ooo. o   o
# `o     .o o  o   O      O     O o      O   o   o   O  o  O  O o     Oo o   O  o    .O' o   O      O o   O
#  `OoooO'  Oo `OoO'o `OoO' `OoO' O.     O   `oO `OoO'  O  o  o O     `o `OoO'  OooOO'   `OoO'o `OoO' O   o
#
#
#
#######################################################################################################################
class ClassAtomNoDash(Production):
    pass


class ClassAtomNoDash_source(ClassAtomNoDash):
    def __init__(self, src: str, position: int):
        self.src = src
        self.span = Span(position, position + 1)
        self.children = (src[position],)
        self.source_character = src[position]

    @cached_property
    def CharacterValue(self) -> int:
        return ord(self.source_character)

    IsCharacterClass = False

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.18 ClassAtomNoDash
        # The production ClassAtomNoDash::SourceCharacterbut not one of \ or ] or - evaluates as follows:
        #   1. Return the CharSet containing the character matched by SourceCharacter.
        return CharSet(self.source_character)


class ClassAtomNoDash_escape(ClassAtomNoDash):
    def __init__(self, src: str, class_escape: ClassEscape):
        self.src = src
        self.span = Span(class_escape.span.start - 1, class_escape.span.after)
        self.children = ("\\", class_escape)
        self.class_escape = class_escape

    @cached_property
    def CharacterValue(self) -> int:
        return self.class_escape.CharacterValue

    @cached_property
    def IsCharacterClass(self) -> bool:
        return self.class_escape.IsCharacterClass

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.18 ClassAtomNoDash
        # The production ClassAtomNoDash :: \ ClassEscape evaluates as follows:
        #   1. Return the CharSet that is the result of evaluating ClassEscape.
        return self.class_escape.evaluate(context)


def parse_ClassAtomNoDash(src: str, position: int, U: bool) -> Optional[ClassAtomNoDash]:
    # Syntax:
    #   ClassAtomNoDash[U]::
    #       SourceCharacter but not one of \ or ] or -
    #       \ ClassEscape[?U]
    if position < len(src):
        if src[position] == "\\":
            ce = parse_ClassEscape(src, position + 1, U)
            if ce:
                return ClassAtomNoDash_escape(src, ce)
        if src[position] not in "]-":
            return ClassAtomNoDash_source(src, position)
    return None


#### ClassAtom ########################################################################################################
#
#  .oOOOo.   o                       Oo
# .O     o  O                       o  O
# o         o                      O    o    O
# o         O                     oOooOoOo  oOo
# o         o  .oOoO' .oOo  .oOo  o      O   o   .oOo. `oOOoOO.
# O         O  O   o  `Ooo. `Ooo. O      o   O   O   o  O  o  o
# `o     .o o  o   O      O     O o      O   o   o   O  o  O  O
#  `OoooO'  Oo `OoO'o `OoO' `OoO' O.     O   `oO `OoO'  O  o  o
#
#
#
#######################################################################################################################
class ClassAtom(Production):
    pass


class ClassAtom_ClassAtomNoDash(ClassAtom):
    def __init__(self, src: str, cand: ClassAtomNoDash):
        self.src = src
        self.span = cand.span
        self.children = (cand,)
        self.class_atom_no_dash = cand

    @cached_property
    def CharacterValue(self) -> int:
        return self.class_atom_no_dash.CharacterValue

    @cached_property
    def IsCharacterClass(self) -> bool:
        return self.class_atom_no_dash.IsCharacterClass

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.17 ClassAtom
        # The production ClassAtom::ClassAtomNoDash evaluates as follows:
        #   1. Return the CharSet that is the result of evaluating ClassAtomNoDash.
        return self.class_atom_no_dash.evaluate(context)


class ClassAtom_dash(ClassAtom):
    def __init__(self, src: str, position: int):
        self.src = src
        self.span = Span(position, position + 1)
        self.children = ("-",)

    CharacterValue = ord("-")
    IsCharacterClass = False

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.17 ClassAtom
        # The production ClassAtom :: - evaluates as follows:
        #  1. Return the CharSet containing the single character - U+002D (HYPHEN-MINUS).
        return CharSet("-")


def parse_ClassAtom(src: str, position: int, U: bool) -> Optional[ClassAtom]:
    # Syntax:
    #   ClassAtom[U]::
    #       -
    #       ClassAtomNoDash[?U]
    cand = parse_ClassAtomNoDash(src, position, U)
    if cand:
        return ClassAtom_ClassAtomNoDash(src, cand)
    if position < len(src) and src[position] == "-":
        return ClassAtom_dash(src, position)
    return None


#### NonemptyClassRangesNoDash ########################################################################################
#
# o.     O                                                .oOOOo.   o                    `OooOOo.                                  o.     O       o.OOOo.                 o
# Oo     o                                               .O     o  O                      o     `o                                 Oo     o        O    `o               O
# O O    O                                     O         o         o                      O      O                                 O O    O        o      O              o
# O  o   o                                    oOo        o         O                      o     .O                                 O  o   o        O      o              O
# O   o  O .oOo. 'OoOo. .oOo. `oOOoOO. .oOo.   o   O   o o         o  .oOoO' .oOo  .oOo   OOooOO'  .oOoO' 'OoOo. .oOoO .oOo. .oOo  O   o  O .oOo.  o      O .oOoO' .oOo  OoOo.
# o    O O O   o  o   O OooO'  O  o  o O   o   O   o   O O         O  O   o  `Ooo. `Ooo.  o    o   O   o   o   O o   O OooO' `Ooo. o    O O O   o  O      o O   o  `Ooo. o   o
# o     Oo o   O  O   o O      o  O  O o   O   o   O   o `o     .o o  o   O      O     O  O     O  o   O   O   o O   o O         O o     Oo o   O  o    .O' o   O      O o   O
# O     `o `OoO'  o   O `OoO'  O  o  o oOoO'   `oO `OoOO  `OoooO'  Oo `OoO'o `OoO' `OoO'  O      o `OoO'o  o   O `OoOo `OoO' `OoO' O     `o `OoO'  OooOO'   `OoO'o `OoO' O   o
#                                      O               o                                                             O
#                                      o'           OoO'                                                          OoO'
#
#######################################################################################################################
class NonemptyClassRangesNoDash(Production):
    pass


class NonemptyClassRangesNoDash_ClassAtom(NonemptyClassRangesNoDash):
    def __init__(self, src: str, class_atom: ClassAtom):
        self.src = src
        self.span = class_atom.span
        self.children = (class_atom,)
        self.class_atom = class_atom

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.16 NonemptyClassRangesNoDash
        # The production NonemptyClassRangesNoDash::ClassAtom evaluates as follows:
        #   1. Return the CharSet that is the result of evaluating ClassAtom.
        return self.class_atom.evaluate(context)


class NonemptyClassRangesNoDash_nodash(NonemptyClassRangesNoDash):
    def __init__(self, src: str, cand: ClassAtomNoDash, ncrnd: NonemptyClassRangesNoDash):
        self.src = src
        self.span = Span(cand.span.start, ncrnd.span.after)
        self.children = (cand, ncrnd)
        self.class_atom_no_dash = cand
        self.nonempty_class_ranges_no_dash = ncrnd

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.16 NonemptyClassRangesNoDash
        # The production NonemptyClassRangesNoDash::ClassAtomNoDash NonemptyClassRangesNoDash evaluates as follows:
        #   1. Evaluate ClassAtomNoDash to obtain a CharSet A.
        #   2. Evaluate NonemptyClassRangesNoDash to obtain a CharSet B.
        #   3. Return the union of CharSets A and B.
        A = self.class_atom_no_dash.evaluate(context)
        B = self.nonempty_class_ranges_no_dash.evaluate(context)
        return A | B


class NonemptyClassRangesNoDash_withdash(NonemptyClassRangesNoDash):
    def __init__(self, src: str, cand: ClassAtomNoDash, ca: ClassAtom, cr: "ClassRanges"):
        self.src = src
        self.span = Span(cand.span.start, cr.span.after)
        self.children = (cand, "-", ca, cr)
        self.class_atom_no_dash = cand
        self.class_atom = ca
        self.class_ranges = cr

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.16 NonemptyClassRangesNoDash
        # The production NonemptyClassRangesNoDash::ClassAtomNoDash-ClassAtomClassRanges evaluates as follows:
        #
        #   1. Evaluate ClassAtomNoDash to obtain a CharSet A.
        #   2. Evaluate ClassAtom to obtain a CharSet B.
        #   3. Evaluate ClassRanges to obtain a CharSet C.
        #   4. Call CharacterRange(A, B) and let D be the resulting CharSet.
        #   5. Return the union of CharSets D and C.
        A = self.class_atom_no_dash.evaluate(context)
        B = self.class_atom.evaluate(context)
        C = self.class_ranges.evaluate(context)
        D = CharacterRange(A, B)
        return D | C


def parse_NonemptyClassRangesNoDash(src: str, position: int, U: bool) -> Optional[NonemptyClassRangesNoDash]:
    # Syntax:
    #   NonemptyClassRangesNoDash[U]::
    #       ClassAtom[?U]
    #       ClassAtomNoDash[?U] NonemptyClassRangesNoDash[?U]
    #       ClassAtomNoDash[?U] - ClassAtom[?U] ClassRanges[?U]
    after1 = 0
    after2 = 0
    after3 = 0
    make1: Callable[[], Optional[NonemptyClassRangesNoDash]] = lambda: None
    make2: Callable[[], Optional[NonemptyClassRangesNoDash]] = lambda: None
    make3: Callable[[], Optional[NonemptyClassRangesNoDash]] = lambda: None
    classatom = parse_ClassAtom(src, position, U)
    if classatom:
        after1 = classatom.span.after
        make1 = lambda: NonemptyClassRangesNoDash_ClassAtom(src, classatom)
    cand = parse_ClassAtomNoDash(src, position, U)
    if cand:
        after = cand.span.after
        ncrnd = parse_NonemptyClassRangesNoDash(src, after, U)
        if ncrnd:
            make2 = lambda: NonemptyClassRangesNoDash_nodash(src, cand, ncrnd)
            after2 = ncrnd.span.after
        if after < len(src) and src[after] == "-":
            classatom2 = parse_ClassAtom(src, after + 1, U)
            if classatom2:
                classranges = parse_ClassRanges(src, classatom2.span.after, U)
                if classranges:
                    after3 = classranges.span.after
                    make3 = lambda: NonemptyClassRangesNoDash_withdash(src, cand, classatom2, classranges)
    choices = ((make1, after1), (make2, after2), (make3, after3))
    make, _ = max(choices, key=lambda data: data[1])
    return make()


#### NonemptyClassRanges ##############################################################################################
#
# o.     O                                                .oOOOo.   o                    `OooOOo.
# Oo     o                                               .O     o  O                      o     `o
# O O    O                                     O         o         o                      O      O
# O  o   o                                    oOo        o         O                      o     .O
# O   o  O .oOo. 'OoOo. .oOo. `oOOoOO. .oOo.   o   O   o o         o  .oOoO' .oOo  .oOo   OOooOO'  .oOoO' 'OoOo. .oOoO .oOo. .oOo
# o    O O O   o  o   O OooO'  O  o  o O   o   O   o   O O         O  O   o  `Ooo. `Ooo.  o    o   O   o   o   O o   O OooO' `Ooo.
# o     Oo o   O  O   o O      o  O  O o   O   o   O   o `o     .o o  o   O      O     O  O     O  o   O   O   o O   o O         O
# O     `o `OoO'  o   O `OoO'  O  o  o oOoO'   `oO `OoOO  `OoooO'  Oo `OoO'o `OoO' `OoO'  O      o `OoO'o  o   O `OoOo `OoO' `OoO'
#                                      O               o                                                             O
#                                      o'           OoO'                                                          OoO'
#
#######################################################################################################################

# 21.2.2.15.1 Runtime Semantics: CharacterRange ( A, B )
def CharacterRange(A: CharSet, B: CharSet) -> CharSet:
    # The abstract operation CharacterRange takes two CharSet parameters A and B and performs the following steps:
    #   1. Assert: A and B each contain exactly one character.
    #   2. Let a be the one character in CharSet A.
    #   3. Let b be the one character in CharSet B.
    #   4. Let i be the character value of character a.
    #   5. Let j be the character value of character b.
    #   6. Assert: i ≤ j.
    #   7. Return the set containing all characters numbered i through j, inclusive.
    assert len(A) == 1 and len(B) == 1
    a = next(ch for ch in A)
    b = next(ch for ch in B)
    i = ord(a)
    j = ord(b)
    assert i <= j
    return CharSet("".join((chr(x) for x in range(i, j + 1))))


class NonemptyClassRanges(Production):
    pass


class NonemptyClassRanges_ca(NonemptyClassRanges):
    def __init__(self, src: str, class_atom: "ClassAtom"):
        self.src = src
        self.span = class_atom.span
        self.children = (class_atom,)
        self.class_atom = class_atom

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.15 NonemptyClassRanges
        # The production NonemptyClassRanges::ClassAtom evaluates as follows:
        #   1. Return the CharSet that is the result of evaluating ClassAtom.
        return self.class_atom.evaluate(context)


class NonemptyClassRanges_nodash(NonemptyClassRanges):
    def __init__(self, src: str, class_atom: ClassAtom, ncrnd: NonemptyClassRangesNoDash):
        self.src = src
        self.span = Span(class_atom.span.start, ncrnd.span.after)
        self.class_atom = class_atom
        self.nonempty_class_ranges_no_dash = ncrnd
        self.children = (class_atom, ncrnd)

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.15 NonemptyClassRanges
        # The production NonemptyClassRanges :: ClassAtom NonemptyClassRangesNoDash evaluates as follows:
        #   1. Evaluate ClassAtom to obtain a CharSet A.
        #   2. Evaluate NonemptyClassRangesNoDash to obtain a CharSet B.
        #   3. Return the union of CharSets A and B.
        A = self.class_atom.evaluate(context)
        B = self.nonempty_class_ranges_no_dash.evaluate(context)
        return A | B


class NonemptyClassRanges_withdash(NonemptyClassRanges):
    def __init__(
        self, src: str, class_atom_left: ClassAtom, class_atom_right: ClassAtom, class_ranges: "ClassRanges"
    ):
        self.src = src
        self.span = Span(class_atom_left.span.start, class_ranges.span.after)
        self.children = (class_atom_left, "-", class_atom_right, class_ranges)
        self.class_atom_left = class_atom_left
        self.class_atom_right = class_atom_right
        self.class_ranges = class_ranges

    @cached_property
    def earlyerrors(self) -> List[str]:
        # 21.2.1.1 Static Semantics: Early Errors
        # NonemptyClassRanges :: ClassAtom - ClassAtom ClassRanges
        #   * It is a Syntax Error if IsCharacterClass of the first ClassAtom is true or IsCharacterClass of the
        #     second ClassAtom is true.
        #   * It is a Syntax Error if IsCharacterClass of the first ClassAtom is false and IsCharacterClass of the
        #     second ClassAtom is false and the CharacterValue of the first ClassAtom is larger than the CharacterValue
        #     of the second ClassAtom.
        return list(
            chain(
                filter(
                    None,
                    [
                        (self.class_atom_left.IsCharacterClass or self.class_atom_right.IsCharacterClass)
                        and f"Confused class range: {self.src[self.span.start:self.span.after]}",
                        not self.class_atom_left.IsCharacterClass
                        and not self.class_atom_right.IsCharacterClass
                        and self.class_atom_left.CharacterValue > self.class_atom_right.CharacterValue
                        and f"Reversed class range: {self.src[self.span.start:self.span.after]}",
                    ],
                ),
                super().earlyerrors,
            )
        )

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.15 NonemptyClassRanges
        # The production NonemptyClassRanges :: ClassAtom - ClassAtom ClassRanges evaluates as follows:
        #   1. Evaluate the first ClassAtom to obtain a CharSet A.
        #   2. Evaluate the second ClassAtom to obtain a CharSet B.
        #   3. Evaluate ClassRanges to obtain a CharSet C.
        #   4. Call CharacterRange(A, B) and let D be the resulting CharSet.
        #   5. Return the union of CharSets D and C.
        A = self.class_atom_left.evaluate(context)
        B = self.class_atom_right.evaluate(context)
        C = self.class_ranges.evaluate(context)
        D = CharacterRange(A, B)
        return C | D


def parse_NonemptyClassRanges(src: str, position: int, U: bool) -> Optional[NonemptyClassRanges]:
    # Syntax:
    #   NonemptyClassRanges[U]::
    #       ClassAtom[?U]
    #       ClassAtom[?U] NonemptyClassRangesNoDash[?U]
    #       ClassAtom[?U] - ClassAtom[?U] ClassRanges[?U]
    make1: Callable[[], Optional[NonemptyClassRanges]] = lambda: None
    make2: Callable[[], Optional[NonemptyClassRanges]] = lambda: None
    make3: Callable[[], Optional[NonemptyClassRanges]] = lambda: None
    after1 = 0
    after2 = 0
    after3 = 0

    classatom = parse_ClassAtom(src, position, U)
    if classatom:
        make1 = lambda: NonemptyClassRanges_ca(src, classatom)
        after1 = classatom.span.after
        ncrnd = parse_NonemptyClassRangesNoDash(src, after1, U)
        if ncrnd:
            after2 = ncrnd.span.after
            make2 = lambda: NonemptyClassRanges_nodash(src, classatom, ncrnd)
        if after1 < len(src) and src[after1] == "-":
            classatom2 = parse_ClassAtom(src, after1 + 1, U)
            if classatom2:
                classranges = parse_ClassRanges(src, classatom2.span.after, U)
                if classranges:
                    after3 = classranges.span.after
                    make3 = lambda: NonemptyClassRanges_withdash(src, classatom, classatom2, classranges)
        choices: Tuple[Tuple[int, Callable[[], Optional[NonemptyClassRanges]]], ...] = (
            (after1, make1),
            (after2, make2),
            (after3, make3),
        )
        _, maker = max(choices, key=lambda data: data[0])
        return maker()
    return None


#### ClassRanges ######################################################################################################
#
#  .oOOOo.   o                    `OooOOo.
# .O     o  O                      o     `o
# o         o                      O      O
# o         O                      o     .O
# o         o  .oOoO' .oOo  .oOo   OOooOO'  .oOoO' 'OoOo. .oOoO .oOo. .oOo
# O         O  O   o  `Ooo. `Ooo.  o    o   O   o   o   O o   O OooO' `Ooo.
# `o     .o o  o   O      O     O  O     O  o   O   O   o O   o O         O
#  `OoooO'  Oo `OoO'o `OoO' `OoO'  O      o `OoO'o  o   O `OoOo `OoO' `OoO'
#                                                             O
#                                                          OoO'
#
#######################################################################################################################
class ClassRanges(Production):
    pass


class ClassRanges_empty(ClassRanges):
    def __init__(self, src: str, position: int):
        self.src = src
        self.span = Span(position, position)
        self.children = ()

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.14 ClassRanges
        # The production ClassRanges::[empty] evaluates as follows:
        #   1. Return the empty CharSet.
        return CharSet("")


class ClassRanges_notempty(ClassRanges):
    def __init__(self, src: str, ncr: NonemptyClassRanges):
        self.src = src
        self.span = ncr.span
        self.children = (ncr,)
        self.nonempty_class_ranges = ncr

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.14 ClassRanges
        # The production ClassRanges::NonemptyClassRanges evaluates as follows:
        #   1. Return the CharSet that is the result of evaluating NonemptyClassRanges.
        return self.nonempty_class_ranges.evaluate(context)


def parse_ClassRanges(src: str, position: int, U: bool) -> Optional[ClassRanges]:
    # Syntax:
    #   ClassRanges[U]::
    #       [empty]
    #       NonemptyClassRanges[?U]
    ncr = parse_NonemptyClassRanges(src, position, U)
    if ncr:
        return ClassRanges_notempty(src, ncr)
    return ClassRanges_empty(src, position)


#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################

# 21.2.2.6.2 Runtime Semantics: IsWordChar ( e )
def IsWordChar(context: Context, e: int) -> bool:
    # The abstract operation IsWordChar takes an integer parameter e and performs the following steps:
    #   1. If e is -1 or e is InputLength, return false.
    #   2. Let c be the character Input[e].
    #   3. Let wordChars be the result of ! WordCharacters().
    #   4. If c is in wordChars, return true.
    #   5. Return false.
    return 0 <= e < context.InputLength and context.Input[e] in WordCharacters(context)


class Assertion(Production):
    def __init__(self, *args):
        raise ValueError("Don't instantiate the base class!")

    def evaluate(self, context: Context) -> AssertionTester:
        raise NotImplementedError()  # @@@


class Assertion_start_of_input(Assertion):
    def __init__(self, src: str, position: int):
        self.src = src
        self.span = Span(position, position + 1)
        self.children = ("^",)

    def evaluate(self, context: Context) -> AssertionTester:
        # 21.2.2.6 Assertion
        # The production Assertion :: ^ evaluates as follows:
        #   1. Return an internal AssertionTester closure that takes a State argument x and performs the following steps when evaluated:
        #       a. Let e be x's endIndex.
        #       b. If e is zero, return true.
        #       c. If Multiline is false, return false.
        #       d. If the character Input[e - 1] is one of LineTerminator, return true.
        #       e. Return false.
        def closure(x: State) -> bool:
            e = x.endIndex
            return e == 0 or (
                context.Multiline and context.Input[e - 1] in "\N{LF}\N{CR}\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR}"
            )

        return closure


class Assertion_end_of_input(Assertion):
    def __init__(self, src: str, position: int):
        self.src = src
        self.span = Span(position, position + 1)
        self.children = ("$",)

    def evaluate(self, context: Context) -> AssertionTester:
        # 21.2.2.6 Assertion
        # The production Assertion :: $ evaluates as follows:
        #   1. Return an internal AssertionTester closure that takes a State argument x and performs the following steps when evaluated:
        #       a. Let e be x's endIndex.
        #       b. If e is equal to InputLength, return true.
        #       c. If Multiline is false, return false.
        #       d. If the character Input[e] is one of LineTerminator, return true.
        #       e. Return false.
        def closure(x: State) -> bool:
            e = x.endIndex
            return e == context.InputLength or (
                context.Multiline and context.Input[e] in "\N{LF}\N{CR}\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR}"
            )

        return closure


class Assertion_word_boundary(Assertion):
    def __init__(self, src: str, position: int):
        self.src = src
        self.span = Span(position, position + 2)
        self.children = ("\\b",)

    def evaluate(self, context: Context) -> AssertionTester:
        # 21.2.2.6 Assertion
        # The production Assertion::\b evaluates as follows:
        #   1. Return an internal AssertionTester closure that takes a State argument x and performs the following steps when evaluated:
        #       a. Let e be x's endIndex.
        #       b. Call IsWordChar(e - 1) and let a be the Boolean result.
        #       c. Call IsWordChar(e) and let b be the Boolean result.
        #       d. If a is true and b is false, return true.
        #       e. If a is false and b is true, return true.
        #       f. Return false.
        def closure(x: State) -> bool:
            e = x.endIndex
            return IsWordChar(context, e - 1) ^ IsWordChar(context, e)

        return closure


class Assertion_not_word_boundary(Assertion):
    def __init__(self, src: str, position: int):
        self.src = src
        self.span = Span(position, position + 2)
        self.children = ("\\B",)

    def evaluate(self, context: Context) -> AssertionTester:
        # 21.2.2.6 Assertion
        # The production Assertion::\B evaluates as follows:
        #   1. Return an internal AssertionTester closure that takes a State argument x and performs the following steps when evaluated:
        #       a. Let e be x's endIndex.
        #       b. Call IsWordChar(e - 1) and let a be the Boolean result.
        #       c. Call IsWordChar(e) and let b be the Boolean result.
        #       d. If a is true and b is false, return false.
        #       e. If a is false and b is true, return false.
        #       f. Return true.
        def closure(x: State) -> bool:
            e = x.endIndex
            return not (IsWordChar(context, e - 1) ^ IsWordChar(context, e))

        return closure


def parse_Assertion(src: str, position: int, U: bool, N: bool) -> Optional[Assertion]:
    # Syntax:
    #   Assertion[U, N]::
    #       ^
    #       $
    #       \ b
    #       \ B
    if position >= len(src):
        return None
    choices = (
        ("^", Assertion_start_of_input),
        ("$", Assertion_end_of_input),
        ("\\b", Assertion_word_boundary),
        ("\\B", Assertion_not_word_boundary),
    )

    for leader, ctor in choices:
        if src.startswith(leader, position):
            return ctor(src, position)
    return None


class Quantifier(Production):
    pass


class Quantifier_plain(Quantifier):
    def __init__(self, src: str, qp: "QuantifierPrefix"):
        self.src = src
        self.span = qp.span
        self.children = (qp,)
        self.quantifier_prefix = qp

    def evaluate(self, context: Context) -> Tuple[int, float, bool]:
        # 21.2.2.7 Quantifier
        # The production Quantifier::QuantifierPrefix evaluates as follows:
        #   1. Evaluate QuantifierPrefix to obtain the two results: an integer min and an integer (or ∞) max.
        #   2. Return the three results min, max, and true.
        return (*self.quantifier_prefix.evaluate(context), True)


class Quantifier_question(Quantifier):
    def __init__(self, src: str, qp: "QuantifierPrefix"):
        self.src = src
        self.span = Span(qp.span.start, qp.span.after + 1)
        self.children = (qp, "?")
        self.quantifier_prefix = qp

    def evaluate(self, context: Context) -> Tuple[int, float, bool]:
        # 21.2.2.7 Quantifier
        # The production Quantifier :: QuantifierPrefix ? evaluates as follows:
        #   1. Evaluate QuantifierPrefix to obtain the two results: an integer min and an integer (or ∞) max.
        #   2. Return the three results min, max, and false.
        return (*self.quantifier_prefix.evaluate(context), False)


def parse_Quantifier(src: str, position: int) -> Optional[Quantifier]:
    # Syntax:
    #   Quantifier::
    #       QuantifierPrefix
    #       QuantifierPrefix ?
    qp = parse_QuantifierPrefix(src, position)
    if qp:
        after = qp.span.after
        if after < len(src) and src[after] == "?":
            return Quantifier_question(src, qp)
        return Quantifier_plain(src, qp)
    return None


class QuantifierPrefix(Production):
    pass


class QuantifierPrefix_2numb(QuantifierPrefix):
    @cached_property
    def low(self) -> int:
        return int(self.children[1], 10)

    @cached_property
    def high(self) -> int:
        return int(self.children[3], 10)

    @cached_property
    def earlyerrors(self) -> List[str]:
        # 21.2.1.1 Static Semantics: Early Errors
        # QuantifierPrefix :: { DecimalDigits , DecimalDigits }
        #   * It is a Syntax Error if the MV of the first DecimalDigits is larger than the MV of the second DecimalDigits.
        return list(
            chain(
                filter(
                    None,
                    [
                        self.low > self.high
                        and f"Reversed quantifier values: {self.src[self.span.start:self.span.after]}"
                    ],
                ),
                super().earlyerrors,
            )
        )

    def evaluate(self, context: Context) -> Tuple[int, float]:
        # 21.2.2.7 Quantifier
        # The production QuantifierPrefix::{DecimalDigits,DecimalDigits} evaluates as follows:
        #   1. Let i be the MV of the first DecimalDigits.
        #   2. Let j be the MV of the second DecimalDigits.
        #   3. Return the two results i and j.
        return (self.low, self.high)


class QuantifierPrefix_star(QuantifierPrefix):
    def evaluate(self, context: Context) -> Tuple[int, float]:
        # 21.2.2.7 Quantifier
        # The production QuantifierPrefix :: * evaluates as follows:
        #   1. Return the two results 0 and ∞.
        return (0, math.inf)


class QuantifierPrefix_plus(QuantifierPrefix):
    def evaluate(self, context: Context) -> Tuple[int, float]:
        # 21.2.2.7 Quantifier
        # The production QuantifierPrefix::+ evaluates as follows:
        #   1. Return the two results 1 and ∞.
        return (1, math.inf)


class QuantifierPrefix_question(QuantifierPrefix):
    def evaluate(self, context: Context) -> Tuple[int, float]:
        # 21.2.2.7 Quantifier
        # The production QuantifierPrefix::? evaluates as follows:
        #   1. Return the two results 0 and 1.
        return (0, 1)


class QuantifierPrefix_1numb(QuantifierPrefix):
    def evaluate(self, context: Context) -> Tuple[int, float]:
        # 21.2.2.7 Quantifier
        # The production QuantifierPrefix::{DecimalDigits} evaluates as follows:
        #   1. Let i be the MV of DecimalDigits (see 11.8.3).
        #   2. Return the two results i and i.
        i = int(self.children[1], 10)
        return (i, i)


class QuantifierPrefix_numbcomma(QuantifierPrefix):
    def evaluate(self, context: Context) -> Tuple[int, float]:
        # 21.2.2.7 Quantifier
        # The production QuantifierPrefix::{DecimalDigits,} evaluates as follows:
        # Let i be the MV of DecimalDigits.
        # Return the two results i and ∞.
        i = int(self.children[1], 10)
        return (i, math.inf)


_qfp_regex = regex.compile(r"(?P<onechar>[*+?])|(?P<lc>\{)(?P<min>[0-9]+)((?P<comma>,)(?P<max>[0-9]+)?)?(?P<rc>\})")
_qfp_ctors = {
    "*": QuantifierPrefix_star,
    "+": QuantifierPrefix_plus,
    "?": QuantifierPrefix_question,
    3: QuantifierPrefix_1numb,
    4: QuantifierPrefix_numbcomma,
    5: QuantifierPrefix_2numb,
}


def parse_QuantifierPrefix(src: str, position: int) -> Optional[QuantifierPrefix]:
    # Syntax:
    #   QuantifierPrefix::
    #       *
    #       +
    #       ?
    #       { DecimalDigits }
    #       { DecimalDigits , }
    #       { DecimalDigits , DecimalDigits }
    m = _qfp_regex.match(src, position)
    if m:
        children: Tuple[str, ...] = tuple(
            filter(None, (m.group(x) for x in ("onechar", "lc", "min", "comma", "max", "rc")))
        )
        key = children[0] if len(children) == 1 else len(children)
        return _qfp_ctors[key](src, Span(*m.span()), children)
    return None


class PatternCharacter(Production):
    def __init__(self, src: str, pos: int):
        self.src = src
        self.span = Span(pos, pos + 1)
        self.children = (src[pos],)
        self.ch = src[pos]


def parse_PatternCharacter(src: str, position: int) -> Optional[PatternCharacter]:
    # Syntax:
    #   SyntaxCharacter :: one of
    #       ^ $ \ . * + ? ( ) [ ] { } |
    #   PatternCharacter ::
    #       SourceCharacter but not SyntaxCharacter
    if position < len(src) and src[position] not in "^$\\.*+?()[]{}|":
        return PatternCharacter(src, position)
    return None


class AtomEscape(Production):
    pass


class AtomEscape_group(AtomEscape):
    def __init__(self, src: str, group_name: "GroupName"):
        self.src = src
        self.span = Span(group_name.start - 1, group_name.after)
        self.children = ("k", group_name)
        self.group_name = group_name

    @cached_property
    def group_references(self) -> List[str]:
        return [self.group_name.children[1].StringValue]

    def evaluate(self, context: Context, direction: int) -> Matcher:
        # 21.2.2.9 AtomEscape
        #   With parameter direction.
        raise NotImplementedError()  # @@@


class AtomEscape_cce(AtomEscape):
    def __init__(self, src: str, cce: "CharacterClassEscape"):
        self.src = src
        self.span = cce.span
        self.children = (cce,)
        self.character_class_escape = cce

    def evaluate(self, context: Context, direction: int) -> Matcher:
        # 21.2.2.9 AtomEscape
        #   With parameter direction.
        raise NotImplementedError()  # @@@


class AtomEscape_ce(AtomEscape):
    def __init__(self, src: str, ce: "CharacterEscape"):
        self.src = src
        self.span = ce.span
        self.children = (ce,)
        self.character_escape = ce

    def evaluate(self, context: Context, direction: int) -> Matcher:
        # 21.2.2.9 AtomEscape
        #   With parameter direction.
        # The production AtomEscape::CharacterEscape evaluates as follows:
        #   1. Evaluate CharacterEscape to obtain a character ch.
        #   2. Let A be a one-element CharSet containing the character ch.
        #   3. Call CharacterSetMatcher(A, false, direction) and return its Matcher result.
        ch = self.character_escape.evaluate(context)
        A = CharSet(ch)
        return CharacterSetMatcher(context, A, False, direction)


class AtomEscape_decimal(AtomEscape):
    def __init__(self, src: str, de: "DecimalEscape"):
        self.src = src
        self.span = de.span
        self.children = (de,)
        self.decimal_escape = de

    @cached_property
    def group_numbers(self) -> List[int]:
        return [self.decimal_escape.CapturingGroupNumber]

    def evaluate(self, context: Context, direction: int) -> Matcher:
        # 21.2.2.9 AtomEscape
        #   With parameter direction.
        raise NotImplementedError()  # @@@


def parse_AtomEscape(src: str, position: int, U: bool, N: bool) -> Optional[AtomEscape]:
    # Syntax:
    #   AtomEscape[U, N] ::
    #       DecimalEscape
    #       CharacterClassEscape[?U]
    #       CharacterEscape[?U]
    #       [+N] k GroupName[?U]
    decimal_escape = parse_DecimalEscape(src, position)
    if decimal_escape:
        return AtomEscape_decimal(src, decimal_escape)
    cc_escape = parse_CharacterClassEscape(src, position, U)
    if cc_escape:
        return AtomEscape_cce(src, cc_escape)
    char_escape = parse_CharacterEscape(src, position, U)
    if char_escape:
        return AtomEscape_ce(src, char_escape)
    if N and position < len(src) and src[position] == "k":
        group_name = parse_GroupName(src, position + 1, U)
        if group_name:
            return AtomEscape_group(src, group_name)
    return None


class CharacterEscape(Production):
    @cached_property
    def CharacterValue(self) -> int:
        raise NotImplementedError("Base class should not be instantiated directly.")

    def evaluate(self, context: Context) -> str:
        # 21.2.2.10 CharacterEscape
        # The productions
        #   CharacterEscape :: ControlEscape
        #   CharacterEscape :: c ControlLetter
        #   CharacterEscape :: 0 [lookahead ∉ DecimalDigit]
        #   CharacterEscape :: HexEscapeSequence
        #   CharacterEscape :: RegExpUnicodeEscapeSequence
        #   CharacterEscape :: IdentityEscape
        # evaluate as follows:
        #   1. Let cv be the CharacterValue of this CharacterEscape.
        #   2. Return the character whose character value is cv.
        return chr(self.CharacterValue)


class CharacterEscape_ControlEscape(CharacterEscape):
    def __init__(self, src: str, ce: "ControlEscape"):
        self.src = src
        self.span = ce.span
        self.children = (ce,)
        self.control_escape = ce

    @cached_property
    def CharacterValue(self) -> int:
        return self.control_escape.CharacterValue


class CharacterEscape_control(CharacterEscape):
    def __init__(self, src: str, control_letter: "ControlLetter"):
        self.src = src
        self.span = Span(control_letter.span.start - 1, control_letter.span.after)
        self.children = ("c", control_letter)
        self.control_letter = control_letter

    @cached_property
    def CharacterValue(self) -> int:
        return self.control_letter.control_value


class CharacterEscape_zero(CharacterEscape):
    def __init__(self, src: str, position: int):
        self.src = src
        self.span = Span(position, position + 1)
        self.children = ("0",)

    CharacterValue = 0


class CharacterEscape_hex(CharacterEscape):
    def __init__(self, src: str, hes: "HexEscapeSequence"):
        self.src = src
        self.span = hes.span
        self.children = (hes,)
        self.hex_escape_sequence = hes

    @cached_property
    def CharacterValue(self) -> int:
        return self.hex_escape_sequence.CharacterValue


class CharacterEscape_unicode(CharacterEscape):
    def __init__(self, src: str, reues: "RegExpUnicodeEscapeSequence"):
        self.src = src
        self.span = reues.span
        self.children = (reues,)
        self.regexp_unicode_escape_sequence = reues

    @cached_property
    def CharacterValue(self) -> int:
        return self.regexp_unicode_escape_sequence.CharacterValue


class CharacterEscape_identity(CharacterEscape):
    def __init__(self, src: str, id: "IdentityEscape"):
        self.src = src
        self.span = id.span
        self.children = (id,)
        self.identity_escape = id

    @cached_property
    def CharacterValue(self) -> int:
        return self.identity_escape.CharacterValue


def parse_CharacterEscape(src: str, position: int, U: bool) -> Optional[CharacterEscape]:
    # Syntax:
    #   CharacterEscape[U] ::
    #       ControlEscape
    #       c ControlLetter
    #       0 [lookahead ∉ DecimalDigit]
    #       HexEscapeSequence
    #       RegExpUnicodeEscapeSequence[?U]
    #       IdentityEscape[?U]
    if position >= len(src):
        return None
    control_escape = parse_ControlEscape(src, position)
    if control_escape:
        return CharacterEscape_ControlEscape(src, control_escape)
    if src[position] == "c":
        control_letter = parse_ControlLetter(src, position + 1)
        if control_letter:
            return CharacterEscape_control(src, control_letter)
    if src[position] == "0" and (position == len(src) - 1 or src[position + 1] not in "0123456789"):
        return CharacterEscape_zero(src, position)
    hes = parse_HexEscapeSequence(src, position)
    if hes:
        return CharacterEscape_hex(src, hes)
    res = parse_RegExpUnicodeEscapeSequence(src, position, U)
    if res:
        return CharacterEscape_unicode(src, res)
    ie = parse_IdentityEscape(src, position, U)
    if ie:
        return CharacterEscape_identity(src, ie)
    return None


_control_escape_map = {
    "t": 9,  # TAB <HT>
    "n": 10,  # LINE FEED <LF>
    "v": 11,  # LINE TABULATION <VT>
    "f": 12,  # FORM FEED <FF>
    "r": 13,  # CARRIAGE RETURN <CR>
}


class ControlEscape(Production):
    @cached_property
    def CharacterValue(self) -> int:
        return _control_escape_map[self.children[0]]


def parse_ControlEscape(src: str, position: int) -> Optional[ControlEscape]:
    if position < len(src) and src[position] in "fnrtv":
        return ControlEscape(src, Span(position, position + 1), (src[position],))
    return None


class ControlLetter(Production):
    @cached_property
    def control_value(self) -> int:
        return ord(self.children[0]) % 32


def parse_ControlLetter(src: str, position: int) -> Optional[ControlLetter]:
    if position < len(src) and ("a" <= src[position] <= "z" or "A" <= src[position] <= "Z"):
        return ControlLetter(src, Span(position, position + 1), (src[position],))
    return None


class HexEscapeSequence(Production):
    @cached_property
    def CharacterValue(self) -> int:
        return int(self.children[0][1:], 16)


hes_regex = regex.compile(r"x[0-9a-fA-F]{2}")


def parse_HexEscapeSequence(src: str, position: int) -> Optional[HexEscapeSequence]:
    m = hes_regex.match(src, position)
    if m:
        return HexEscapeSequence(src, Span(*m.span()), (m.group(0),))
    return None


class CharacterClass(Production):
    pass


class CharacterClass_normal(CharacterClass):
    def __init__(self, src: str, span: Span, class_ranges: "ClassRanges"):
        self.src = src
        self.span = span
        self.children = ("[", class_ranges, "]")
        self.class_ranges = class_ranges

    def evaluate(self, context: Context) -> Tuple[CharSet, bool]:
        # 21.2.2.13 CharacterClass
        # The production CharacterClass::[ClassRanges] evaluates as follows:
        #   1. Evaluate ClassRanges to obtain a CharSet A.
        #   2. Return the two results A and false.
        return (self.class_ranges.evaluate(context), False)


class CharacterClass_inverted(CharacterClass):
    def __init__(self, src: str, span: Span, class_ranges: "ClassRanges"):
        self.src = src
        self.span = span
        self.children = ("[^", class_ranges, "]")
        self.class_ranges = class_ranges

    def evaluate(self, context: Context) -> Tuple[CharSet, bool]:
        # 21.2.2.13 CharacterClass
        # The production CharacterClass::[ClassRanges] evaluates as follows:
        #   1. Evaluate ClassRanges to obtain a CharSet A.
        #   2. Return the two results A and true.
        return (self.class_ranges.evaluate(context), True)


def parse_CharacterClass(src: str, position: int, U: bool) -> Optional[CharacterClass]:
    # Syntax:
    #   CharacterClass[U] ::
    #       [ [lookahead ∉ { ^ }] ClassRanges[?U] ]
    #       [ ^ ClassRanges[?U] ]
    if position >= len(src) or src[position] != "[":
        return None

    pos = position
    ctor = None
    for prefix, ctor in (("[^", CharacterClass_inverted), ("[", CharacterClass_normal)):
        if src.startswith(prefix, pos):
            pos += len(prefix)
            break
    if not ctor:
        return None
    class_ranges = parse_ClassRanges(src, pos, U)
    if class_ranges and class_ranges.span.after < len(src) and src[class_ranges.span.after] == "]":
        return ctor(src, Span(position, class_ranges.span.after + 1), class_ranges)
    return None


class GroupSpecifier(Production):
    pass


class GroupSpecifier_empty(GroupSpecifier):
    group_names: List[str] = []


class GroupSpecifier_GroupName(GroupSpecifier):
    @cached_property
    def group_names(self) -> List[str]:
        return [self.children[1].children[1].StringValue]

    @cached_property
    def capture_map(self) -> Dict[int, str]:
        return {0: self.children[1].children[1].StringValue}


def parse_GroupSpecifier(src: str, position: int, U: bool) -> Optional[GroupSpecifier]:
    # Syntax:
    #   GroupSpecifier[U]::
    #       [empty]
    #       ? GroupName[?U]
    if position < len(src) and src[position] == "?":
        gn = parse_GroupName(src, position + 1, U)
        if gn:
            return GroupSpecifier_GroupName(src, Span(position, gn.span.after), ("?", gn))
    return GroupSpecifier_empty(src, Span(position, position), ())


class GroupName(Production):
    pass


def parse_GroupName(src: str, position: int, U: bool) -> Optional[GroupName]:
    # Syntax:
    #   GroupName[U]::
    #       < RegExpIdentifierName[?U] >
    if position < len(src) and src[position] == "<":
        rein = parse_RegExpIdentifierName(src, position + 1, U)
        if rein and rein.span.after < len(src) and src[rein.span.after] == ">":
            return GroupName(src, Span(position, rein.span.after + 1), ("<", rein, ">"))
    return None


class RegExpIdentifierName(Production):
    @cached_property
    def StringValue(self) -> str:
        # 21.2.1.6 Static Semantics: StringValue
        # RegExpIdentifierName[U]::
        #   RegExpIdentifierStart[?U]
        #   RegExpIdentifierName[?U] RegExpIdentifierPart[?U]
        #
        #   1. Return the String value consisting of the sequence of code units corresponding to RegExpIdentifierName. In
        #      determining the sequence any occurrences of \ RegExpUnicodeEscapeSequence are first replaced with the code
        #      point represented by the RegExpUnicodeEscapeSequence and then the code points of the entire
        #      RegExpIdentifierName are converted to code units by UTF16Encoding each code point.
        return utf_16_encode("".join(chr(c.CharacterValue) for c in self.children))


def parse_RegExpIdentifierName(src: str, position: int, U: bool) -> Optional[RegExpIdentifierName]:
    # Syntax:
    #   RegExpIdentifierName[U]::
    #       RegExpIdentifierStart[?U]
    #       RegExpIdentifierName[?U] RegExpIdentifierPart[?U]
    # I.e.: Start followed by zero or more Parts
    start = parse_RegExpIdentiferStart(src, position, U)
    if start:
        children: Children = (start,)
        pos = start.span.after
        while 1:
            part = parse_RegExpIdentiferPart(src, pos, U)
            if part:
                children = children + (part,)
                pos = part.span.after
                continue
            return RegExpIdentifierName(src, Span(position, pos), children)
    return None


class RegExpIdentifierStart(Production):
    pass


class RegExpIdentifierStart_onech(RegExpIdentifierStart):
    @cached_property
    def CharacterValue(self) -> int:
        return ord(self.children[0])


_not_start_matcher = regex.compile(r"^[^\p{ID_Start}_$]$")


class RegExpIdentifierStart_escape(RegExpIdentifierStart):
    @cached_property
    def CharacterValue(self) -> int:
        return self.children[1].CharacterValue

    @cached_property
    def earlyerrors(self) -> List[str]:
        # RegExpIdentifierStart :: \ RegExpUnicodeEscapeSequence
        #   * It is a Syntax Error if SV(RegExpUnicodeEscapeSequence) is none of "$", or "_", or the UTF16Encoding of a
        #     code point matched by the UnicodeIDStart lexical grammar production.
        sv = utf_16_decode(self.children[1].SV, throw=False)
        return list(
            chain(
                filter(
                    None,
                    [
                        _not_start_matcher.match(sv)
                        and f"Invalid character for identifier start: {self.src[self.span.start:self.span.after]}"
                    ],
                ),
                super().earlyerrors,
            )
        )


_RegExpIdentifierStart_choice1 = regex.compile(r"[\p{ID_Start}_$]")


def parse_RegExpIdentiferStart(src: str, position: int, U: bool) -> Optional[RegExpIdentifierStart]:
    # Syntax:
    #   RegExpIdentifierStart[U]::
    #       UnicodeIDStart
    #       $
    #       _
    #       \ RegExpUnicodeEscapeSequence[?U]
    m = _RegExpIdentifierStart_choice1.match(src, pos=position)
    if m:
        return RegExpIdentifierStart_onech(src, Span(*m.span()), (m.group(0),))
    if position < len(src) and src[position] == "\\":
        reues = parse_RegExpUnicodeEscapeSequence(src, position + 1, U)
        if reues:
            return RegExpIdentifierStart_escape(src, Span(position, reues.span.after), ("\\", reues))
    return None


class RegExpIdentifierPart(Production):
    pass


class RegExpIdentifierPart_onech(RegExpIdentifierPart):
    @cached_property
    def CharacterValue(self) -> int:
        return ord(self.children[0])


_not_part_matcher = regex.compile(r"^[^\p{ID_Continue}_$\N{ZWJ}\N{ZWNJ}]$")


class RegExpIdentifierPart_escape(RegExpIdentifierPart):
    @cached_property
    def CharacterValue(self) -> int:
        return self.children[1].CharacterValue

    @cached_property
    def earlyerrors(self) -> List[str]:
        # RegExpIdentifierPart :: \ RegExpUnicodeEscapeSequence
        #   * It is a Syntax Error if SV(RegExpUnicodeEscapeSequence) is none of "$", or "_", or the UTF16Encoding of
        #     either <ZWNJ> or <ZWJ>, or the UTF16Encoding of a Unicode code point that would be matched by the
        #     UnicodeIDContinue lexical grammar production.
        sv = utf_16_decode(self.children[1].SV, throw=False)
        return list(
            chain(
                filter(
                    None,
                    [
                        _not_part_matcher.match(sv)
                        and f"Invalid identifier character: {self.src[self.span.start:self.span.after]}"
                    ],
                ),
                super().earlyerrors,
            )
        )


_RegExpIdentifierPart_choice1 = regex.compile(r"[\p{ID_Continue}$\N{ZWJ}\N{ZWNJ}]")


def parse_RegExpIdentiferPart(src: str, position: int, U: bool) -> Optional[RegExpIdentifierPart]:
    # Syntax:
    #   RegExpIdentifierPart[U]::
    #       UnicodeIDContinue
    #       $
    #       \ RegExpUnicodeEscapeSequence[?U]
    #       <ZWNJ>
    #       <ZWJ>
    m = _RegExpIdentifierPart_choice1.match(src, pos=position)
    if m:
        return RegExpIdentifierPart_onech(src, Span(*m.span()), (m.group(0),))
    if position < len(src) and src[position] == "\\":
        reues = parse_RegExpUnicodeEscapeSequence(src, position + 1, U)
        if reues:
            return RegExpIdentifierPart_escape(src, Span(position, reues.span.after), ("\\", reues))
    return None


class DecimalEscape(Production):
    @cached_property
    def CapturingGroupNumber(self) -> int:
        # 21.2.1.2 Static Semantics: CapturingGroupNumber
        # DecimalEscape :: NonZeroDigit
        #   1. Return the MV of NonZeroDigit.
        # DecimalEscape :: NonZeroDigit DecimalDigits
        #   1. Let n be the number of code points in DecimalDigits.
        #   2. Return (the MV of NonZeroDigit × 10n) plus the MV of DecimalDigits.
        # The definitions of “the MV of NonZeroDigit” and “the MV of DecimalDigits” are in 11.8.3.
        assert len(self.children) > 0 and isinstance(self.children[0], str)
        return int(self.children[0], 10)


_DecimalEscape = regex.compile(r"[1-9][0-9]*")


def parse_DecimalEscape(src: str, position: int) -> Optional[DecimalEscape]:
    # Syntax:
    #   DecimalEscape::
    #       NonZeroDigit DecimalDigits[opt] [lookahead ∉ DecimalDigit]
    m = _DecimalEscape.match(src, pos=position)
    if m:
        return DecimalEscape(src, Span(*m.span()), (m.group(0),))
    return None


# 21.2.2.6.1 Runtime Semantics: WordCharacters ( )
@lru_cache
def WordCharacters(context: Context) -> str:
    # The abstract operation WordCharacters performs the following steps:
    #   1. Let A be a set of characters containing the sixty-three characters:
    #       a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   p   q   r   s   t   u   v   w   x   y   z
    #       A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S   T   U   V   W   X   Y   Z
    #       0   1   2   3   4   5   6   7   8   9   _
    #   2. Let U be an empty set.
    #   3. For each character c not in set A where Canonicalize(c) is in A, add c to U.
    #   4. Assert: Unless Unicode and IgnoreCase are both true, U is empty.
    #   5. Add the characters in set U to set A.
    #   6. Return A.
    A = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"
    assert len(A) == 63
    if context.Unicode and context.IgnoreCase:
        A += "".join(chr(x) for x in range(0x110000) if chr(x) not in A and Canonicalize(context, chr(x)) in A)
    return A


class CharacterClassEscape(Production):
    pass


class CharacterClassEscape_d(CharacterClassEscape):
    def __init__(self, src: str, position: int):
        self.src = src
        self.span = Span(position, position + 1)
        self.children = ("d",)

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.12 CharacterClassEscape
        # The production CharacterClassEscape::d evaluates as follows:
        #   1. Return the ten-element set of characters containing the characters 0 through 9 inclusive.
        return CharSet("0123456789")


class CharacterClassEscape_D(CharacterClassEscape):
    def __init__(self, src: str, position: int):
        self.src = src
        self.span = Span(position, position + 1)
        self.children = ("D",)

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.12 CharacterClassEscape
        # The production CharacterClassEscape::D evaluates as follows:
        #   1. Return the set of all characters not included in the set returned by CharacterClassEscape::d .
        return CharSet("0123456789", inverted=True)


_schars = "\t\n\x0b\x0c\r \xa0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u2028\u2029\u202f\u205f\u3000\ufeff"
# _schars = (
#     "".join(chr(x) for x in range(0x110000) if unicodedata.category(chr(x)) == "Zs")
#     + "\t\v\f \N{NBSP}\N{ZWNBSP}\n\r\N{LINE SEPARATOR}\N{PARAGRAPH SEPARATOR}"
# )


class CharacterClassEscape_s(CharacterClassEscape):
    def __init__(self, src: str, position: int):
        self.src = src
        self.span = Span(position, position + 1)
        self.children = ("s",)

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.12 CharacterClassEscape
        # The production CharacterClassEscape::s evaluates as follows:
        #   1. Return the set of characters containing the characters that are on the right-hand side of the WhiteSpace
        #      or LineTerminator productions.
        return CharSet(_schars)


class CharacterClassEscape_S(CharacterClassEscape):
    def __init__(self, src: str, position: int):
        self.src = src
        self.span = Span(position, position + 1)
        self.children = ("S",)

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.12 CharacterClassEscape
        # The production CharacterClassEscape::S evaluates as follows:
        #   1. Return the set of all characters not included in the set returned by CharacterClassEscape::s .
        return CharSet(_schars, inverted=True)


class CharacterClassEscape_w(CharacterClassEscape):
    def __init__(self, src: str, position: int):
        self.src = src
        self.span = Span(position, position + 1)
        self.children = ("w",)

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.12 CharacterClassEscape
        # The production CharacterClassEscape::w evaluates as follows:
        #   1. Return the set of all characters returned by WordCharacters().
        return CharSet(WordCharacters(context))


class CharacterClassEscape_W(CharacterClassEscape):
    def __init__(self, src: str, position: int):
        self.src = src
        self.span = Span(position, position + 1)
        self.children = ("W",)

    def evaluate(self, context: Context) -> CharSet:
        # 21.2.2.12 CharacterClassEscape
        # The production CharacterClassEscape::W evaluates as follows:
        #   1. Return the set of all characters not included in the set returned by CharacterClassEscape::w .
        return CharSet(WordCharacters(context), inverted=False)


class CharacterClassEscape_p(CharacterClassEscape):
    def __init__(self, src: str, upve: "UnicodePropertyValueExpression"):
        self.src = src
        self.span = Span(upve.span.start - 2, upve.span.after + 1)
        self.children = ("p{", upve, "}")
        self.unicode_property_value_expression = upve


class CharacterClassEscape_P(CharacterClassEscape):
    def __init__(self, src: str, upve: "UnicodePropertyValueExpression"):
        self.src = src
        self.span = Span(upve.span.start - 2, upve.span.after + 1)
        self.children = ("P{", upve, "}")
        self.unicode_property_value_expression = upve


def parse_CharacterClassEscape(src: str, position: int, U: bool) -> Optional[CharacterClassEscape]:
    # Syntax:
    #   CharacterClassEscape[U]::
    #       d
    #       D
    #       s
    #       S
    #       w
    #       W
    #       [+U] p { UnicodePropertyValueExpression }
    #       [+U] P { UnicodePropertyValueExpression }
    if position < len(src) and src[position] in "dDsSwW":
        return {
            "d": CharacterClassEscape_d,
            "D": CharacterClassEscape_D,
            "s": CharacterClassEscape_s,
            "S": CharacterClassEscape_S,
            "w": CharacterClassEscape_w,
            "W": CharacterClassEscape_W,
        }[src[position]](src, position)
    if U and position < len(src) - 1 and src[position] in "pP" and src[position + 1] == "{":
        upve = parse_UnicodePropertyValueExpression(src, position + 2)
        if upve and upve.span.after < len(src) and src[upve.span.after] == "}":
            return {"p": CharacterClassEscape_p, "P": CharacterClassEscape_P}[src[position]](src, upve)
    return None


class UnicodePropertyValueExpression(Production):
    pass


class UnicodePropertyValueExpression_lone(Production):
    @cached_property
    def NameStringValue(self) -> str:
        return self.children[0]

    @cached_property
    def earlyerrors(self) -> List[str]:
        # UnicodePropertyValueExpression :: LoneUnicodePropertyNameOrValue
        #   * It is a Syntax Error if the List of Unicode code points that is SourceText of
        #     LoneUnicodePropertyNameOrValue is not identical to a List of Unicode code points that is a Unicode
        #     general category or general category alias listed in the “Property value and aliases” column of Table 56,
        #     nor a binary property or binary property alias listed in the “Property name and aliases” column of Table
        #     55.
        return list(
            chain(
                filter(
                    None,
                    [
                        self.NameStringValue not in _binary_unicode_property_names_and_aliases
                        and self.NameStringValue not in _general_categories
                        and f"{self.src[self.span.start:self.span.after]} does not match a valid Unicode binary or general category property name"
                    ],
                ),
                super().earlyerrors,
            )
        )


class UnicodePropertyValueExpression_pair(Production):
    @cached_property
    def NameStringValue(self) -> str:
        return self.children[0]

    @cached_property
    def ValueStringValue(self) -> str:
        return self.children[1]

    @cached_property
    def earlyerrors(self) -> List[str]:
        # UnicodePropertyValueExpression :: UnicodePropertyName = UnicodePropertyValue
        #   * It is a Syntax Error if the List of Unicode code points that is SourceText of UnicodePropertyName is not
        #     identical to a List of Unicode code points that is a Unicode property name or property alias listed in
        #     the “Property name and aliases” column of Table 54.
        #   * It is a Syntax Error if the List of Unicode code points that is SourceText of UnicodePropertyValue is not
        #     identical to a List of Unicode code points that is a value or value alias for the Unicode property or
        #     property alias given by SourceText of UnicodePropertyName listed in the “Property value and aliases”
        #     column of the corresponding tables Table 56 or Table 57.
        return list(
            chain(
                filter(
                    None,
                    [
                        self.NameStringValue not in _nonbinary_unicode_property_names_and_aliases
                        and f"{self.NameStringValue} does not match a valid Unicode property name",
                        self.NameStringValue in _general_category_aliases
                        and self.ValueStringValue not in _general_categories
                        and f"{self.ValueStringValue} is not a valid Unicode general category",
                        self.NameStringValue in _script_aliases
                        and self.ValueStringValue not in _script_values
                        and f"{self.ValueStringValue} is not a valid Unicode script identifier",
                    ],
                ),
                super().earlyerrors,
            )
        )


_ControlLetter_re = r"([a-zA-Z])"
_UnicodePropertyNameCharacter_re = f"(_|{_ControlLetter_re})"
_UnicodePropertyValueCharacter_re = f"({_UnicodePropertyNameCharacter_re}|[0-9])"
_UnicodePropertyValueCharacters_re = f"({_UnicodePropertyValueCharacter_re}+)"
_LoneUnicodePropertyNameOrValue_re = f"(?P<lone>{_UnicodePropertyValueCharacters_re})"
_UnicodePropertyValue_re = f"(?P<value>{_UnicodePropertyValueCharacters_re})"
_UnicodePropertyNameCharacters_re = f"({_UnicodePropertyNameCharacter_re}+)"
_UnicodePropertyName_re = f"(?P<name>{_UnicodePropertyNameCharacters_re})"
_UnicodePropertyValueExpression_re = (
    f"(({_UnicodePropertyName_re}={_UnicodePropertyValue_re})|{_LoneUnicodePropertyNameOrValue_re})"
)
_UnicodePropertyValueExpression_pattern = regex.compile(_UnicodePropertyValueExpression_re)


def parse_UnicodePropertyValueExpression(src: str, position: int) -> Optional[UnicodePropertyValueExpression]:
    # Syntax:
    #   UnicodePropertyValueExpression::
    #       UnicodePropertyName = UnicodePropertyValue
    #       LoneUnicodePropertyNameOrValue
    m = _UnicodePropertyValueExpression_pattern.match(src, pos=position)
    if m:
        lone = m.group("lone")
        if lone:
            return UnicodePropertyValueExpression_lone(src, Span(*m.span()), (lone,))
        return UnicodePropertyValueExpression_pair(src, Span(*m.span()), (m.group("name"), m.group("value")))
    return None


class RegExpUnicodeEscapeSequence(Production):
    @cached_property
    def SV(self) -> str:
        return utf_16_encode(chr(self.CharacterValue))

    def CharacterValue(self) -> int:
        raise NotImplementedError("Must use a subclass")


class RegExpUnicodeEscapeSequence_4digits(RegExpUnicodeEscapeSequence):
    @cached_property
    def CharacterValue(self) -> int:
        return int(self.children[0][1:], 16)


class RegExpUnicodeEscapeSequence_curlies(RegExpUnicodeEscapeSequence):
    @cached_property
    def CharacterValue(self) -> int:
        return int(self.children[0][2:-1], 16)


class RegExpUnicodeEscapeSequence_2surrogates(RegExpUnicodeEscapeSequence):
    @cached_property
    def CharacterValue(self) -> int:
        lead = chr(int(self.children[0][1:5], 16))
        trail = chr(int(self.children[0][7:11], 16))
        return ord(utf_16_decode(lead + trail, throw=False))


_REUES_Hex4Digits_pattern = regex.compile(r"u(?P<digits>[0-9a-fA-F]{4})")
_REUES_CodePoint_pattern = regex.compile(r"u\{(?P<CodePoint>[0-9a-fA-F]+)\}")


def parse_RegExpUnicodeEscapeSequence(src: str, position: int, U: bool) -> Optional[RegExpUnicodeEscapeSequence]:
    # Syntax:
    #   RegExpUnicodeEscapeSequence[U]::
    #       [+U] u LeadSurrogate \u TrailSurrogate
    #       [+U] u LeadSurrogate
    #       [+U] u TrailSurrogate
    #       [+U] u NonSurrogate
    #       [~U] u Hex4Digits
    #       [+U] u{ CodePoint }
    if U:
        codepoint_match = _REUES_CodePoint_pattern.match(src, pos=position)
        if codepoint_match and int(codepoint_match.group("CodePoint"), 16) <= 0x10FFFF:
            return RegExpUnicodeEscapeSequence_curlies(
                src, Span(*codepoint_match.span()), (codepoint_match.group(0),)
            )
        four_digits_match = _REUES_Hex4Digits_pattern.match(src, pos=position)
        if four_digits_match:
            value = int(four_digits_match.group("digits"), 16)
            after = four_digits_match.span()[1]
            if after < len(src) and src[after] == "\\" and 0xD800 <= value <= 0xDBFF:
                next_digits_match = _REUES_Hex4Digits_pattern.match(src, pos=after + 1)
                if next_digits_match:
                    trail_value = int(next_digits_match.group("digits"), 16)
                    if 0xDC00 <= trail_value <= 0xDFFF:
                        return RegExpUnicodeEscapeSequence_2surrogates(
                            src,
                            Span(position, next_digits_match.span()[1]),
                            (four_digits_match.group(0) + "\\" + next_digits_match.group(0),),
                        )
            return RegExpUnicodeEscapeSequence_4digits(src, Span(position, after), (four_digits_match.group(0),))
        return None
    just_digits = _REUES_Hex4Digits_pattern.match(src, pos=position)
    if just_digits:
        return RegExpUnicodeEscapeSequence_4digits(src, Span(*just_digits.span()), (just_digits.group(0),))
    return None


class IdentityEscape(Production):
    @cached_property
    def CharacterValue(self) -> int:
        return ord(self.children[0])


_SyntaxSlash_pattern = regex.compile(r"[/^$\\.*+?()\[\]{}|]")
_NotContinue_pattern = regex.compile(r"[^\p{ID_Continue}]")


def parse_IdentityEscape(src: str, position: int, U: bool) -> Optional[IdentityEscape]:
    # Syntax:
    #   IdentityEscape[U]::
    #       [+U] SyntaxCharacter
    #       [+U] /
    #       [~U] SourceCharacter but not UnicodeIDContinue
    if U:
        ch = _SyntaxSlash_pattern.match(src, pos=position)
        if ch:
            return IdentityEscape(src, Span(*ch.span()), (ch.group(0),))
        return None
    chx = _NotContinue_pattern.match(src, pos=position)
    if chx:
        return IdentityEscape(src, Span(*chx.span()), (chx.group(0),))
    return None


# 21.2.2.8.2 Runtime Semantics: Canonicalize ( ch )
def Canonicalize(context: Context, ch: str) -> str:
    # The abstract operation Canonicalize takes a character parameter ch and performs the following steps:
    #   1. If IgnoreCase is false, return ch.
    #   2. If Unicode is true, then
    #       a. If the file CaseFolding.txt of the Unicode Character Database provides a simple or common case folding
    #          mapping for ch, return the result of applying that mapping to ch.
    #       b. Return ch.
    #   3. Else,
    #       a. Assert: ch is a UTF-16 code unit.
    #       b. Let s be the String value consisting of the single code unit ch.
    #       c. Let u be the same result produced as if by performing the algorithm for String.prototype.toUpperCase
    #          using s as the this value.
    #       d. Assert: Type(u) is String.
    #       e. If u does not consist of a single code unit, return ch.
    #       f. Let cu be u's single code unit element.
    #       g. If the numeric value of ch ≥ 128 and the numeric value of cu < 128, return ch.
    #       h. Return cu.
    if not context.IgnoreCase:
        return ch
    if context.Unicode:
        return chr(_casefold.get(ord(ch), ord(ch)))
    cu = _casefold.get(ord(ch), ord(ch))
    if ord(ch) >= 128 and cu < 128:
        return ch
    return chr(cu)


# NOTE 4    | In case-insignificant matches when Unicode is true, all characters are implicitly case-folded using the
#           | simple mapping provided by the Unicode standard immediately before they are compared. The simple mapping
#           | always maps to a single code point, so it does not map, for example, "ß" (U+00DF) to "SS". It may however
#           | map a code point outside the Basic Latin range to a character within, for example, "ſ" (U+017F) to "s".
#           | Such characters are not mapped if Unicode is false. This prevents Unicode code points such as U+017F and
#           | U+212A from matching regular expressions such as /[a-z]/i, but they will match /[a-z]/ui.


def Uncanonicalize(context: Context, ch: str) -> str:
    # Given a char, figure out the set of chars that canonicalize to it. (Might be more than one. Might be none.)
    if not context.IgnoreCase:
        return ch
    chval = ord(ch)
    result = ""
    if context.Unicode:
        for key, value in _casefold.items():
            if chval == value:
                result += chr(key)
        if chval not in _casefold:
            result += ch
        return result
    for key, value in _casefold.items():
        if chval == value and (chval >= 128 or key < 128):
            result += chr(key)
    if chval not in _casefold or chval >= 128 and _casefold[chval] < 128:
        result += ch
    return result


# 21.2.2.8.1 Runtime Semantics: CharacterSetMatcher ( A, invert, direction )
def CharacterSetMatcher(context: Context, A: CharSet, invert: bool, direction: int) -> Matcher:
    # The abstract operation CharacterSetMatcher takes three arguments, a CharSet A, a Boolean flag invert, and an
    # integer direction, and performs the following steps:
    #   1. Return an internal Matcher closure that takes two arguments, a State x and a Continuation c, and performs
    #      the following steps when evaluated:
    #       a. Let e be x's endIndex.
    #       b. Let f be e + direction.
    #       c. If f < 0 or f > InputLength, return failure.
    #       d. Let index be min(e, f).
    #       e. Let ch be the character Input[index].
    #       f. Let cc be Canonicalize(ch).
    #       g. If invert is false, then
    #           i. If there does not exist a member a of set A such that Canonicalize(a) is cc, return failure.
    #       h. Else,
    #           i. Assert: invert is true.
    #           ii. If there exists a member a of set A such that Canonicalize(a) is cc, return failure.
    #       i. Let cap be x's captures List.
    #       j. Let y be the State (f, cap).
    #       k. Call c(y) and return its result.
    def closure(x: State, c: Continuation) -> MatchResult:
        e = x.endIndex
        f = e + direction
        if f < 0 or f > context.InputLength:
            return FAILURE
        index = min(e, f)
        ch = context.Input[index]
        cc = Canonicalize(context, ch)
        aa = Uncanonicalize(context, cc)  # all the chars a where Canonicalize(a) is cc
        in_set = any(
            ch in A for ch in aa
        )  # True if there exists a member a of set A such that Canonicalize(a) is cc
        if (not invert and not in_set) or (invert and in_set):
            return FAILURE
        cap = x.captures
        y = State(f, cap)
        return c(y)

    return closure


class Atom(Production):
    def evaluate(self, context: Context, direction: int) -> Matcher:
        raise NotImplementedError()


class Atom_PatternCharacter(Atom):
    def __init__(self, src: str, pc: PatternCharacter):
        self.src = src
        self.span = pc.span
        self.pattern_character = pc
        self.children = (pc,)

    def evaluate(self, context: Context, direction: int) -> Matcher:
        # 21.2.2.8 Atom
        #   With parameter direction.
        # The production Atom::PatternCharacter evaluates as follows:
        #   1. Let ch be the character matched by PatternCharacter.
        #   2. Let A be a one-element CharSet containing the character ch.
        #   3. Call CharacterSetMatcher(A, false, direction) and return its Matcher result.
        ch = self.pattern_character.ch
        A = CharSet(ch)
        return CharacterSetMatcher(context, A, False, direction)


class Atom_escape(Atom):
    def __init__(self, src: str, span: Span, ae: AtomEscape):
        self.src = src
        self.span = span
        self.atom_escape = ae
        self.children = ("\\", ae)

    def evaluate(self, context: Context, direction: int) -> Matcher:
        # 21.2.2.8 Atom
        #   With parameter direction.
        # The production Atom::\AtomEscape evaluates as follows:
        #   1. Return the Matcher that is the result of evaluating AtomEscape with argument direction.
        return self.atom_escape.evaluate(context, direction)


class Atom_dot(Atom):
    def __init__(self, src: str, pos: int):
        self.src = src
        self.span = Span(pos, pos + 1)
        self.children = (".",)

    def evaluate(self, context: Context, direction: int) -> Matcher:
        # 21.2.2.8 Atom
        #   With parameter direction.
        raise NotImplementedError()  # @@@


class Atom_charclass(Atom):
    def __init__(self, src: str, cc: CharacterClass):
        self.src = src
        self.span = cc.span
        self.children = (cc,)
        self.character_class = cc

    def evaluate(self, context: Context, direction: int) -> Matcher:
        # 21.2.2.8 Atom
        #   With parameter direction.
        # The production Atom::CharacterClass evaluates as follows:
        #   1. Evaluate CharacterClass to obtain a CharSet A and a Boolean invert.
        #   2. Call CharacterSetMatcher(A, invert, direction) and return its Matcher result.
        A, invert = self.character_class.evaluate(context)
        return CharacterSetMatcher(context, A, invert, direction)


class Atom_group_dis(Atom):
    def __init__(self, src: str, span: Span, gs: GroupSpecifier, dis: "Disjunction"):
        self.src = src
        self.span = span
        self.group_specifier = gs
        self.disjunction = dis
        self.children = ("(", gs, dis, ")")

    @cached_property
    def NcapturingParens(self) -> int:
        assert len(self.children) >= 3 and isinstance(self.children[2], Production)
        return 1 + self.children[2].NcapturingParens

    @lru_cache
    def parens_left_of(self, target: Production) -> Tuple[int, bool]:
        count, found = super().parens_left_of(target)
        return (count + 1, found)

    def evaluate(self, context: Context, direction: int) -> Matcher:
        # 21.2.2.8 Atom
        #   With parameter direction.
        # The production Atom :: ( GroupSpecifier Disjunction ) evaluates as follows:
        #   1. Evaluate Disjunction with argument direction to obtain a Matcher m.
        #   2. Let parenIndex be the number of left-capturing parentheses in the entire regular expression that occur
        #      to the left of this Atom. This is the total number of Atom:: ( GroupSpecifier Disjunction ) Parse Nodes
        #      prior to or enclosing this Atom.
        #   3. Return an internal Matcher closure that takes two arguments, a State x and a Continuation c, and
        #      performs the following steps:
        #       a. Let d be an internal Continuation closure that takes one State argument y and performs the following
        #          steps:
        #           i. Let cap be a copy of y's captures List.
        #           ii. Let xe be x's endIndex.
        #           iii. Let ye be y's endIndex.
        #           iv. If direction is equal to +1, then
        #               1. Assert: xe ≤ ye.
        #               2. Let s be a new List whose elements are the characters of Input at indices xe (inclusive)
        #                  through ye (exclusive).
        #           v. Else,
        #               1. Assert: direction is equal to -1.
        #               2. Assert: ye ≤ xe.
        #               3. Let s be a new List whose elements are the characters of Input at indices ye (inclusive)
        #                  through xe (exclusive).
        #           vi. Set cap[parenIndex + 1] to s.
        #           vii. Let z be the State (ye, cap).
        #           viii. Call c(z) and return its result.
        #       b. Call m(x, d) and return its result.
        m = self.disjunction.evaluate(context, direction)
        parenIndex = context.pattern.parenIndex(self)

        def closure(x: State, c: Continuation) -> MatchResult:
            def d(y: State) -> MatchResult:
                cap = copy(y.captures)
                xe = x.endIndex
                ye = y.endIndex
                if direction > 0:
                    assert xe <= ye
                    s = context.Input[xe:ye]
                else:
                    assert ye <= xe
                    s = context.Input[ye:xe]
                cap[parenIndex] = s  # (Unlike the spec, our capture array is 0-indexed)
                z = State(ye, cap)
                return c(z)

            return m(x, d)

        return closure


class Atom_dis(Atom):
    def __init__(self, src: str, span: Span, dis: "Disjunction"):
        self.src = src
        self.span = span
        self.disjunction = dis
        self.children = ("(?:", dis, ")")

    def evaluate(self, context: Context, direction: int) -> Matcher:
        raise NotImplementedError()  # @@@


class Atom_lookahead(Atom):
    def __init__(self, src: str, dis: "Disjunction"):
        self.src = src
        self.span = Span(dis.span.start - 3, dis.span.after + 1)
        self.children = ("(?=", dis, ")")
        self.disjunction = dis

    def evaluate(self, context: Context) -> Matcher:
        # 21.2.2.6 Assertion
        # The production Assertion::(?=Disjunction) evaluates as follows:
        #   1. Evaluate Disjunction with +1 as its direction argument to obtain a Matcher m.
        #   2. Return an internal Matcher closure that takes two arguments, a State x and a Continuation c, and performs the following steps:
        #       a. Let d be a Continuation that always returns its State argument as a successful MatchResult.
        #       b. Call m(x, d) and let r be its result.
        #       c. If r is failure, return failure.
        #       d. Let y be r's State.
        #       e. Let cap be y's captures List.
        #       f. Let xe be x's endIndex.
        #       g. Let z be the State (xe, cap).
        #       h. Call c(z) and return its result.
        # Spec Error: These "lookaround" assertions should have been Atoms, not Assertions.
        m = self.disjunction.evaluate(context, 1)

        def closure(x: State) -> bool:
            def d(x: State) -> State:
                return x

            r = m(x, d)
            return r != FAILURE

        return closure


class Atom_not_lookahead(Atom):
    def __init__(self, src: str, dis: "Disjunction"):
        self.src = src
        self.span = Span(dis.span.start - 3, dis.span.after + 1)
        self.children = ("(?!", dis, ")")
        self.disjunction = dis

    def evaluate(self, context: Context) -> Matcher:
        # 21.2.2.6 Assertion
        raise NotImplementedError()  # @@@


class Atom_lookbehind(Atom):
    def __init__(self, src: str, dis: "Disjunction"):
        self.src = src
        self.span = Span(dis.span.start - 4, dis.span.after + 1)
        self.children = ("(?<=", dis, ")")
        self.disjunction = dis

    def evaluate(self, context: Context) -> Matcher:
        # 21.2.2.6 Assertion
        raise NotImplementedError()  # @@@


class Atom_not_lookbehind(Atom):
    def __init__(self, src: str, dis: "Disjunction"):
        self.src = src
        self.span = Span(dis.span.start - 4, dis.span.after + 1)
        self.children = ("(?<!", dis, ")")
        self.disjunction = dis

    def evaluate(self, context: Context) -> Matcher:
        # 21.2.2.6 Assertion
        raise NotImplementedError()  # @@@


def parse_Atom(src: str, position: int, U: bool, N: bool) -> Optional[Atom]:
    # Syntax:
    #   Atom[U, N]::
    #       PatternCharacter
    #       .
    #       \ AtomEscape[?U, ?N]
    #       CharacterClass[?U]
    #       ( GroupSpecifier[?U] Disjunction[?U, ?N] )
    #       ( ? : Disjunction[?U, ?N] )
    #       ( ? = Disjunction[?U, ?N] )
    #       ( ? ! Disjunction[?U, ?N] )
    #       ( ? < = Disjunction[?U, ?N] )
    #       ( ? < ! Disjunction[?U, ?N] )
    if position >= len(src):
        return None
    pc = parse_PatternCharacter(src, position)
    if pc:
        return Atom_PatternCharacter(src, pc)
    if src[position] == ".":
        return Atom_dot(src, position)
    if src[position] == "\\":
        ae = parse_AtomEscape(src, position + 1, U, N)
        if ae:
            return Atom_escape(src, Span(position, ae.span.after), ae)
        return None
    cc = parse_CharacterClass(src, position, U)
    if cc:
        return Atom_charclass(src, cc)
    if src[position] == "(":
        gs = parse_GroupSpecifier(src, position + 1, U)
        if gs:
            dis = parse_Disjunction(src, gs.span.after, U, N)
            if dis and dis.span.after < len(src) and src[dis.span.after] == ")":
                return Atom_group_dis(src, Span(position, dis.span.after + 1), gs, dis)
        if position + 3 < len(src) and src[position + 1 : position + 3] == "?:":
            dis = parse_Disjunction(src, position + 3, U, N)
            if dis and dis.span.after < len(src) and src[dis.span.after] == ")":
                return Atom_dis(src, Span(position, dis.span.after + 1), dis)
    lookarounds = (
        ("(?=", Atom_lookahead),
        ("(?!", Atom_not_lookahead),
        ("(?<=", Atom_lookbehind),
        ("(?<!", Atom_not_lookbehind),
    )
    for leader, ctor in lookarounds:
        l = len(leader)
        if src[position : position + l] == leader:
            disjunction = parse_Disjunction(src, position + l, U, N)
            if disjunction:
                after = disjunction.span.after
                if after < len(src) and src[after] == ")":
                    return ctor(src, disjunction)
            break
    return None


# 21.2.2.5.1 Runtime Semantics: RepeatMatcher ( m, min, max, greedy, x, c, parenIndex, parenCount )
def RepeatMatcher(
    m: Matcher, min: int, max: float, greedy: bool, x: State, c: Continuation, parenIndex: int, parenCount: int
) -> MatchResult:
    # The abstract operation RepeatMatcher takes eight parameters, a Matcher m, an integer min, an integer (or ∞)
    # max, a Boolean greedy, a State x, a Continuation c, an integer parenIndex, and an integer parenCount, and
    # performs the following steps:
    #   1. If max is zero, return c(x).
    #   2. Let d be an internal Continuation closure that takes one State argument y and performs the following
    #      steps when evaluated:
    #       a. If min is zero and y's endIndex is equal to x's endIndex, return failure.
    #       b. If min is zero, let min2 be zero; otherwise let min2 be min - 1.
    #       c. If max is ∞, let max2 be ∞; otherwise let max2 be max - 1.
    #       d. Call RepeatMatcher(m, min2, max2, greedy, y, c, parenIndex, parenCount) and return its result.
    #   3. Let cap be a copy of x's captures List.
    #   4. For each integer k that satisfies parenIndex < k and k ≤ parenIndex + parenCount, set cap[k] to
    #      undefined.
    #   5. Let e be x's endIndex.
    #   6. Let xr be the State (e, cap).
    #   7. If min is not zero, return m(xr, d).
    #   8. If greedy is false, then
    #       a. Call c(x) and let z be its result.
    #       b. If z is not failure, return z.
    #       c. Call m(xr, d) and return its result.
    #   9. Call m(xr, d) and let z be its result.
    #   10. If z is not failure, return z.
    #   11. Call c(x) and return its result.
    #
    # NOTE 1    | An Atom followed by a Quantifier is repeated the number of times specified by the Quantifier. A
    #           | Quantifier can be non-greedy, in which case the Atom pattern is repeated as few times as possible
    #           | while still matching the sequel, or it can be greedy, in which case the Atom pattern is repeated
    #           | as many times as possible while still matching the sequel. The Atom pattern is repeated rather
    #           | than the input character sequence that it matches, so different repetitions of the Atom can match
    #           | different input substrings.
    #
    # NOTE 2    | If the Atom and the sequel of the regular expression all have choice points, the Atom is first
    #           | matched as many (or as few, if non-greedy) times as possible. All choices in the sequel are tried
    #           | before moving on to the next choice in the last repetition of Atom. All choices in the last (nth)
    #           | repetition of Atom are tried before moving on to the next choice in the next-to-last (n - 1)st
    #           | repetition of Atom; at which point it may turn out that more or fewer repetitions of Atom are now
    #           | possible; these are exhausted (again, starting with either as few or as many as possible) before
    #           | moving on to the next choice in the (n - 1)st repetition of Atom and so on.
    #           |
    #           | Compare
    #           |       /a[a-z]{2,4}/.exec("abcdefghi")
    #           | which returns "abcde" with
    #           |       /a[a-z]{2,4}?/.exec("abcdefghi")
    #           | which returns "abc".
    #           |
    #           | Consider also
    #           |       /(aa|aabaac|ba|b|c)*/.exec("aabaac")
    #           | which, by the choice point ordering above, returns the array
    #           |       ["aaba", "ba"]
    #           | and not any of:
    #           |       ["aabaac", "aabaac"]
    #           |       ["aabaac", "c"]
    #           |
    #           | The above ordering of choice points can be used to write a regular expression that calculates the
    #           | greatest common divisor of two numbers (represented in unary notation). The following example
    #           | calculates the gcd of 10 and 15:
    #           |       "aaaaaaaaaa,aaaaaaaaaaaaaaa".replace(/^(a+)\1*,\1+$/, "$1")
    #           | which returns the gcd in unary notation "aaaaa".
    #
    # NOTE 3    | Step 4 of the RepeatMatcher clears Atom's captures each time Atom is repeated. We can see its
    #           | behaviour in the regular expression
    #           |       /(z)((a+)?(b+)?(c))*/.exec("zaacbbbcac")
    #           | which returns the array
    #           |       ["zaacbbbcac", "z", "ac", "a", undefined, "c"]
    #           | and not
    #           |       ["zaacbbbcac", "z", "ac", "a", "bbb", "c"]
    #           | because each iteration of the outermost * clears all captured Strings contained in the quantified
    #           | Atom, which in this case includes capture Strings numbered 2, 3, 4, and 5.
    #
    # NOTE 4    | Step 1 of the RepeatMatcher's d closure states that, once the minimum number of repetitions has
    #           | been satisfied, any more expansions of Atom that match the empty character sequence are not
    #           | considered for further repetitions. This prevents the regular expression engine from falling into
    #           | an infinite loop on patterns such as:
    #           |       /(a*)*/.exec("b")
    #           | or the slightly more complicated:
    #           |       /(a*)b\1+/.exec("baaaac")
    #           | which returns the array
    #           |       ["b", ""]
    if max == 0:
        return c(x)

    def d(y: State) -> MatchResult:
        if min == 0 and y.endIndex == x.endIndex:
            return FAILURE
        min2 = 0 if min == 0 else min - 1
        max2 = math.inf if max == math.inf else max - 1
        return RepeatMatcher(m, min2, max2, greedy, y, c, parenIndex, parenCount)

    cap = copy(x.captures)
    for k in range(parenIndex + 1, parenIndex + parenCount + 1):
        cap[k - 1] = None
    e = x.endIndex
    xr = State(e, cap)
    if min != 0:
        return m(xr, d)
    if not greedy:
        z = c(x)
        if z != FAILURE:
            return z
        return m(xr, d)
    z = m(xr, d)
    if z != FAILURE:
        return z
    return c(x)


class Term(Production):
    def evaluate(self, context: Context, direction: int) -> Matcher:
        raise NotImplementedError()


class Term_Assertion(Term):
    def __init__(self, src: str, ass: Assertion):
        self.src = src
        self.span = ass.span
        self.assertion = ass
        self.children = (ass,)

    def evaluate(self, context: Context, direction: int) -> Matcher:
        # 21.2.2.5 Term
        #   With parameter direction.
        #
        # The production Term::Assertion evaluates as follows:
        #   1. Return an internal Matcher closure that takes two arguments, a State x and a Continuation c, and
        #      performs the following steps when evaluated:
        #       a. Evaluate Assertion to obtain an AssertionTester t.
        #       b. Call t(x) and let r be the resulting Boolean value.
        #       c. If r is false, return failure.
        #       d. Call c(x) and return its result.
        # NOTE  | The AssertionTester is independent of direction.
        def matcher(x: State, c: Continuation) -> MatchResult:
            if not self.assertion.evaluate(context)(x):
                return FAILURE
            return c(x)

        return matcher


class Term_Atom(Term):
    def __init__(self, src: str, atom: Atom):
        self.src = src
        self.span = atom.span
        self.atom = atom
        self.children = (atom,)

    def evaluate(self, context: Context, direction: int) -> Matcher:
        # 21.2.2.5 Term
        #   With parameter direction.
        #
        # The production Term::Atom evaluates as follows:
        #   1. Return the Matcher that is the result of evaluating Atom with argument direction.
        return self.atom.evaluate(context, direction)


class Term_Atom_Quant(Term):
    def __init__(self, src: str, span: Span, atom: Atom, quant: Quantifier):
        self.src = src
        self.span = span
        self.atom = atom
        self.quantifier = quant
        self.children = (atom, quant)

    def evaluate(self, context: Context, direction: int) -> Matcher:
        # 21.2.2.5 Term
        #   With parameter direction.
        #
        # The production Term::AtomQuantifier evaluates as follows:
        #   1. Evaluate Atom with argument direction to obtain a Matcher m.
        #   2. Evaluate Quantifier to obtain the three results: an integer min, an integer (or ∞) max, and Boolean
        #      greedy.
        #   3. Assert: If max is finite, then max is not less than min.
        #   4. Let parenIndex be the number of left-capturing parentheses in the entire regular expression that occur
        #      to the left of this Term. This is the total number of Atom::(GroupSpecifier Disjunction) Parse Nodes
        #      prior to or enclosing this Term.
        #   5. Let parenCount be the number of left-capturing parentheses in Atom. This is the total number of
        #      Atom::(GroupSpecifier Disjunction) Parse Nodes enclosed by Atom.
        #   6. Return an internal Matcher closure that takes two arguments, a State x and a Continuation c, and
        #      performs the following steps when evaluated:
        #       a. Call RepeatMatcher(m, min, max, greedy, x, c, parenIndex, parenCount) and return its result.
        m = self.atom.evaluate(context, direction)
        min, max, greedy = self.quantifier.evaluate(context)
        assert max >= min
        parenIndex = context.pattern.parenIndex(self)
        parenCount = self.NcapturingParens

        def matcher(x: State, c: Continuation) -> MatchResult:
            return RepeatMatcher(m, min, max, greedy, x, c, parenIndex, parenCount)

        return matcher


def parse_Term(src: str, position: int, U: bool, N: bool) -> Optional[Term]:
    # Syntax:
    #   Term[U, N] ::
    #       Assertion[?U, ?N]
    #       Atom[?U, ?N]
    #       Atom[?U, ?N] Quantifier
    assertion = parse_Assertion(src, position, U, N)
    if assertion:
        return Term_Assertion(src, assertion)
    atom = parse_Atom(src, position, U, N)
    if atom:
        after = atom.span.after
        quantifier = parse_Quantifier(src, after)
        if not quantifier:
            return Term_Atom(src, atom)
        return Term_Atom_Quant(src, Span(position, quantifier.span.after), atom, quantifier)
    return None


class Alternative(Production):
    def __init__(self, src: str, span: Span, terms: Tuple[Term, ...]) -> None:
        self.src = src
        self.span = span
        self.children = terms
        self.terms = terms

    def evaluate(self, context: Context, direction: int) -> Matcher:
        # 21.2.2.4 Alternative
        #   With parameter direction.
        #
        # The production Alternative::[empty] evaluates as follows:
        #   1. Return a Matcher that takes two arguments, a State x and a Continuation c, and returns the result of
        #      calling c(x).
        #
        # The production Alternative::Alternative Term evaluates as follows:
        #   1. Evaluate Alternative with argument direction to obtain a Matcher m1.
        #   2. Evaluate Term with argument direction to obtain a Matcher m2.
        #   3. If direction is equal to +1, then
        #       a. Return an internal Matcher closure that takes two arguments, a State x and a Continuation c, and
        #          performs the following steps when evaluated:
        #           i. Let d be a Continuation that takes a State argument y and returns the result of calling
        #              m2(y, c).
        #           ii. Call m1(x, d) and return its result.
        #   4. Else,
        #       a. Assert: direction is equal to -1.
        #       b. Return an internal Matcher closure that takes two arguments, a State x and a Continuation c, and
        #          performs the following steps when evaluated:
        #           i. Let d be a Continuation that takes a State argument y and returns the result of calling
        #              m1(y, c).
        #           ii. Call m2(x, d) and return its result.
        # NOTE  | Consecutive Terms try to simultaneously match consecutive portions of Input. When direction is equal
        #       | to +1, if the left Alternative, the right Term, and the sequel of the regular expression all have
        #       | choice points, all choices in the sequel are tried before moving on to the next choice in the right
        #       | Term, and all choices in the right Term are tried before moving on to the next choice in the left
        #       | Alternative. When direction is equal to -1, the evaluation order of Alternative and Term are
        #       | reversed.
        #
        # So: I've gone through a number of by-hand simulations, and I just don't see how to do this without the
        # recursion that's actually implied here. So much for my parsing this into a flat list! Anyway, that's why
        # you see the odd subfunction here. We need to transform this list into a recursive function call system.
        def evaluate(terms: Tuple[Term, ...]) -> Matcher:
            if len(terms) == 0:
                return lambda x, c: c(x)
            m1 = evaluate(terms[:-1])
            m2 = terms[-1].evaluate(context, direction)
            if direction > 0:
                return lambda x, c: m1(x, lambda y: m2(y, c))
            return lambda x, c: m2(x, lambda y: m1(y, c))

        return evaluate(self.terms)


def parse_Alternative(src: str, position: int, U: bool, N: bool) -> Optional[Alternative]:
    # Syntax:
    #   Alternative[U, N] ::
    #       [empty]
    #       Alternative[?U, ?N] Term[?U, ?N]
    # (I.e.: Any number of non-empty terms or the empty string.)
    pos = position
    children: Tuple[Term, ...] = ()
    while 1:
        term = parse_Term(src, pos, U, N)
        if term:
            children += (term,)
            pos = term.span.after
            continue
        return Alternative(src, Span(position, pos), children)


class Disjunction(Production):
    def __init__(self, src: str, span: Span, alts: Tuple[Alternative, ...]) -> None:
        self.src = src
        self.span = span
        self.alternatives = alts
        self.children = tuple(join(alts, "|"))

    def evaluate(self, context: Context, direction: int) -> Matcher:
        # 21.2.2.3 Disjunction
        #   With parameter direction.
        #
        # The production Disjunction::Alternative evaluates as follows:
        #   1. Evaluate Alternative with argument direction to obtain a Matcher m.
        #   2. Return m.
        #
        # The production Disjunction::Alternative|Disjunction evaluates as follows:
        #   1. Evaluate Alternative with argument direction to obtain a Matcher m1.
        #   2. Evaluate Disjunction with argument direction to obtain a Matcher m2.
        #   3. Return an internal Matcher closure that takes two arguments, a State x and a Continuation c, and
        #      performs the following steps when evaluated:
        #       a. Call m1(x, c) and let r be its result.
        #       b. If r is not failure, return r.
        #       c. Call m2(x, c) and return its result.
        # NOTE  | The | regular expression operator separates two alternatives. The pattern first tries to match the
        #       | left Alternative (followed by the sequel of the regular expression); if it fails, it tries to match
        #       | the right Disjunction (followed by the sequel of the regular expression). If the left Alternative,
        #       | the right Disjunction, and the sequel all have choice points, all choices in the sequel are tried
        #       | before moving on to the next choice in the left Alternative. If choices in the left Alternative are
        #       | exhausted, the right Disjunction is tried instead of the left Alternative. Any capturing parentheses
        #       | inside a portion of the pattern skipped by | produce undefined values instead of Strings. Thus, for
        #       | example,
        #       |       /a|ab/.exec("abc")
        #       | returns the result "a" and not "ab". Moreover,
        #       |       /((a)|(ab))((c)|(bc))/.exec("abc")
        #       | returns the array
        #       |       ["abc", "a", "a", undefined, "bc", undefined, "bc"]
        #       | and not
        #       |       ["abc", "ab", undefined, "ab", "c", "c", undefined]
        #       | The order in which the two alternatives are tried is independent of the value of direction.
        assert len(self.alternatives) > 0
        if len(self.alternatives) == 1:
            return self.alternatives[0].evaluate(context, direction)
        matchers = (alternative.evaluate(context, direction) for alternative in self.alternatives)

        def closure(x: State, c: Continuation) -> MatchResult:
            return reduce(lambda acc, item: item(x, c) if acc == FAILURE else acc, matchers, FAILURE)

        return closure


def parse_Disjunction(src: str, position: int, U: bool, N: bool) -> Optional[Disjunction]:
    # Syntax:
    #   Disjunction[U, N] ::
    #       Alternative[?U, ?N]
    #       Alternative[?U, ?N] | Disjunction[?U, ?N]
    # (I.e.: Any number of Alternatives separated by the pipe character.)
    pos = position
    children: Tuple[Alternative, ...] = ()

    while 1:
        alternative = parse_Alternative(src, pos, U, N)
        if alternative:
            after = alternative.span.after
            children += (alternative,)
            if after < len(src) and src[after] == "|":
                pos = after + 1
                continue
            return Disjunction(src, Span(position, after), children)
        return None


class Pattern(Production):
    def __init__(self, src: str, dis: Disjunction):
        self.src = src
        self.span = dis.span
        self.disjunction = dis
        self.children = (dis,)

    @cached_property
    def group_name_clashes(self) -> List[str]:
        return [name for name, count in Counter(self.group_names).items() if count > 1]

    @cached_property
    def unresolved_group_names(self) -> List[str]:
        return list(set(self.group_references).difference(set(self.group_names)))

    @cached_property
    def bad_group_numbers(self) -> List[int]:
        return sorted(x for x in set(self.group_numbers) if x > self.NcapturingParens)

    @cached_property
    def earlyerrors(self) -> List[str]:
        # 21.2.1.1 Static Semantics: Early Errors
        # Pattern :: Disjunction
        #   * It is a Syntax Error if NcapturingParens ≥ 2^32 - 1.
        #   * It is a Syntax Error if Pattern contains multiple GroupSpecifiers whose enclosed RegExpIdentifierNames
        #     have the same StringValue.
        # AtomEscape :: k GroupName
        #   * It is a Syntax Error if the enclosing Pattern does not contain a GroupSpecifier with an enclosed
        #     RegExpIdentifierName whose StringValue equals the StringValue of the RegExpIdentifierName of this
        #     production's GroupName.
        # AtomEscape :: DecimalEscape
        #   * It is a Syntax Error if the CapturingGroupNumber of DecimalEscape is larger than NcapturingParens
        #     (21.2.2.1).
        return list(
            chain(
                filter(
                    None,
                    [
                        self.NcapturingParens >= 2 ** 32 - 1 and "Too many captures",
                        self.group_name_clashes
                        and f"Duplicate group names not allowed ({', '.join(f'{n!r}' for n in self.group_name_clashes)})",
                        self.unresolved_group_names
                        and f"Unresolved group references: {', '.join(f'{n!r}' for n in self.unresolved_group_names)}",
                        self.bad_group_numbers
                        and f"Group numbers too large: {', '.join(str(num) for num in self.bad_group_numbers)}",
                    ],
                ),
                super().earlyerrors,
            )
        )

    @lru_cache
    def parenIndex(self, target: Production) -> int:
        count, found = self.parens_left_of(target)
        assert found
        return count

    def evaluate(self, context: Context) -> Callable[[str, int], MatchResult]:
        # The production Pattern::Disjunction evaluates as follows:
        #   1. Evaluate Disjunction with +1 as its direction argument to obtain a Matcher m.
        #   2. Return an internal closure that takes two arguments, a String str and an integer index, and performs
        #      the following steps:
        #       a. Assert: index ≤ the length of str.
        #       b. If Unicode is true, let Input be a List consisting of the sequence of code points of str interpreted
        #          as a UTF-16 encoded (6.1.4) Unicode string. Otherwise, let Input be a List consisting of the
        #          sequence of code units that are the elements of str. Input will be used throughout the algorithms in
        #          21.2.2. Each element of Input is considered to be a character.
        #       c. Let InputLength be the number of characters contained in Input. This variable will be used
        #          throughout the algorithms in 21.2.2.
        #       d. Let listIndex be the index into Input of the character that was obtained from element index of str.
        #       e. Let c be a Continuation that always returns its State argument as a successful MatchResult.
        #       f. Let cap be a List of NcapturingParens undefined values, indexed 1 through NcapturingParens.
        #       g. Let x be the State (listIndex, cap).
        #       h. Call m(x, c) and return its result.
        # NOTE  | A Pattern evaluates (“compiles”) to an internal procedure value. RegExpBuiltinExec can then apply
        #       | this procedure to a String and an offset within the String to determine whether the pattern would
        #       | match starting at exactly that offset within the String, and, if it does match, what the values of
        #       | the capturing parentheses would be. The algorithms in 21.2.2 are designed so that compiling a pattern
        #       | may throw a SyntaxError exception; on the other hand, once the pattern is successfully compiled,
        #       | applying the resulting internal procedure to find a match in a String cannot throw an exception
        #       | (except for any host-defined exceptions that can occur anywhere such as out-of-memory).
        m = self.disjunction.evaluate(context, 1)

        def closure(string: str, index: int) -> Union[Failure, State]:
            assert index <= len(string)
            if context.Unicode:
                context.Input = utf_16_decode(string, throw=False)
                idx = 0
                listIndex = 0
                while idx < index:
                    if (
                        0xD800 <= ord(string[idx]) <= 0xDBFF
                        and idx + 1 < index
                        and 0xDC00 <= ord(string[idx + 1]) <= 0xDFFF
                    ):
                        idx += 2
                    else:
                        idx += 1
                    listIndex += 1
            else:
                context.Input = string
                listIndex = index
            context.InputLength = len(context.Input)
            c: Continuation = lambda state: state
            cap: List[Optional[str]] = [None] * context.NcapturingParens
            x = State(listIndex, cap)
            return m(x, c)

        return closure


def parse_Pattern(src: str, position: int, U: bool, N: bool) -> Optional[Pattern]:
    # Syntax:
    #   Pattern[U, N] ::
    #       Disjunction[?U, ?N]
    disjunction = parse_Disjunction(src, position, U, N)
    if disjunction:
        return Pattern(src, disjunction)
    return None


def matcher(pat: Pattern, pattern_chars: str, flags: str) -> Callable[[str, int], MatchResult]:
    context = Context(pat.NcapturingParens, "s" in flags, "i" in flags, "m" in flags, "u" in flags, pat)
    return pat.evaluate(context)


# Table 54
_general_category_aliases = ("General_Category", "gc")
_script_aliases = ("Script", "sc", "Script_Extensions", "scx")
_nonbinary_unicode_property_names_and_aliases = (*_general_category_aliases, *_script_aliases)

# Table 55
_binary_unicode_property_names_and_aliases = (
    "ASCII",
    "ASCII_Hex_Digit",
    "AHex",
    "Alphabetic",
    "Alpha",
    "Any",
    "Assigned",
    "Bidi_Control",
    "Bidi_C",
    "Bidi_Mirrored",
    "Bidi_M",
    "Case_Ignorable",
    "CI",
    "Cased",
    "Changes_When_Casefolded",
    "CWCF",
    "Changes_When_Casemapped",
    "CWCM",
    "Changes_When_Lowercased",
    "CWL",
    "Changes_When_NFKC_Casefolded",
    "CWKCF",
    "Changes_When_Titlecased",
    "CWT",
    "Changes_When_Uppercased",
    "CWU",
    "Dash",
    "Default_Ignorable_Code_Point",
    "DI",
    "Deprecated",
    "Dep",
    "Diacritic",
    "Dia",
    "Emoji",
    "Emoji_Component",
    "Emoji_Modifier",
    "Emoji_Modifier_Base",
    "Emoji_Presentation",
    "Extended_Pictographic",
    "Extender",
    "Ext",
    "Grapheme_Base",
    "Gr_Base",
    "Grapheme_Extend",
    "Gr_Ext",
    "Hex_Digit",
    "Hex",
    "IDS_Binary_Operator",
    "IDSB",
    "IDS_Trinary_Operator",
    "IDST",
    "ID_Continue",
    "IDC",
    "ID_Start",
    "IDS",
    "Ideographic",
    "Ideo",
    "Join_Control",
    "Join_C",
    "Logical_Order_Exception",
    "LOE",
    "Lowercase",
    "Lower",
    "Math",
    "Noncharacter_Code_Point",
    "NChar",
    "Pattern_Syntax",
    "Pat_Syn",
    "Pattern_White_Space",
    "Pat_WS",
    "Quotation_Mark",
    "QMark",
    "Radical",
    "Regional_Indicator",
    "RI",
    "Sentence_Terminal",
    "STerm",
    "Soft_Dotted",
    "SD",
    "Terminal_Punctuation",
    "Term",
    "Unified_Ideograph",
    "UIdeo",
    "Uppercase",
    "Upper",
    "Variation_Selector",
    "VS",
    "White_Space",
    "space",
    "XID_Continue",
    "XIDC",
    "XID_Start",
    "XIDS",
)

# Table 56
_general_categories = (
    "Cased_Letter",
    "LC",
    "Close_Punctuation",
    "Pe",
    "Connector_Punctuation",
    "Pc",
    "Control",
    "Cc",
    "cntrl",
    "Currency_Symbol",
    "Sc",
    "Dash_Punctuation",
    "Pd",
    "Decimal_Number",
    "Nd",
    "digit",
    "Enclosing_Mark",
    "Me",
    "Final_Punctuation",
    "Pf",
    "Format",
    "Cf",
    "Initial_Punctuation",
    "Pi",
    "Letter",
    "L",
    "Letter_Number",
    "Nl",
    "Line_Separator",
    "Zl",
    "Lowercase_Letter",
    "Ll",
    "Mark",
    "M",
    "Combining_Mark",
    "Math_Symbol",
    "Sm",
    "Modifier_Letter",
    "Lm",
    "Modifier_Symbol",
    "Sk",
    "Nonspacing_Mark",
    "Mn",
    "Number",
    "N",
    "Open_Punctuation",
    "Ps",
    "Other",
    "C",
    "Other_Letter",
    "Lo",
    "Other_Number",
    "No",
    "Other_Punctuation",
    "Po",
    "Other_Symbol",
    "So",
    "Paragraph_Separator",
    "Zp",
    "Private_Use",
    "Co",
    "Punctuation",
    "P",
    "punct",
    "Separator",
    "Z",
    "Space_Separator",
    "Zs",
    "Spacing_Mark",
    "Mc",
    "Surrogate",
    "Cs",
    "Symbol",
    "S",
    "Titlecase_Letter",
    "Lt",
    "Unassigned",
    "Cn",
    "Uppercase_Letter",
    "Lu",
)

# Table 57
_script_values = (
    "Adlam",
    "Adlm",
    "Ahom",
    "Anatolian_Hieroglyphs",
    "Hluw",
    "Arabic",
    "Arab",
    "Armenian",
    "Armn",
    "Avestan",
    "Avst",
    "Balinese",
    "Bali",
    "Bamum",
    "Bamu",
    "Bassa_Vah",
    "Bass",
    "Batak",
    "Batk",
    "Bengali",
    "Beng",
    "Bhaiksuki",
    "Bhks",
    "Bopomofo",
    "Bopo",
    "Brahmi",
    "Brah",
    "Braille",
    "Brai",
    "Buginese",
    "Bugi",
    "Buhid",
    "Buhd",
    "Canadian_Aboriginal",
    "Cans",
    "Carian",
    "Cari",
    "Caucasian_Albanian",
    "Aghb",
    "Chakma",
    "Cakm",
    "Cham",
    "Cherokee",
    "Cher",
    "Common",
    "Zyyy",
    "Coptic",
    "Copt",
    "Qaac",
    "Cuneiform",
    "Xsux",
    "Cypriot",
    "Cprt",
    "Cyrillic",
    "Cyrl",
    "Deseret",
    "Dsrt",
    "Devanagari",
    "Deva",
    "Dogra",
    "Dogr",
    "Duployan",
    "Dupl",
    "Egyptian_Hieroglyphs",
    "Egyp",
    "Elbasan",
    "Elba",
    "Ethiopic",
    "Ethi",
    "Georgian",
    "Geor",
    "Glagolitic",
    "Glag",
    "Gothic",
    "Goth",
    "Grantha",
    "Gran",
    "Greek",
    "Grek",
    "Gujarati",
    "Gujr",
    "Gunjala_Gondi",
    "Gong",
    "Gurmukhi",
    "Guru",
    "Han",
    "Hani",
    "Hangul",
    "Hang",
    "Hanifi_Rohingya",
    "Rohg",
    "Hanunoo",
    "Hano",
    "Hatran",
    "Hatr",
    "Hebrew",
    "Hebr",
    "Hiragana",
    "Hira",
    "Imperial_Aramaic",
    "Armi",
    "Inherited",
    "Zinh",
    "Qaai",
    "Inscriptional_Pahlavi",
    "Phli",
    "Inscriptional_Parthian",
    "Prti",
    "Javanese",
    "Java",
    "Kaithi",
    "Kthi",
    "Kannada",
    "Knda",
    "Katakana",
    "Kana",
    "Kayah_Li",
    "Kali",
    "Kharoshthi",
    "Khar",
    "Khmer",
    "Khmr",
    "Khojki",
    "Khoj",
    "Khudawadi",
    "Sind",
    "Lao",
    "Laoo",
    "Latin",
    "Latn",
    "Lepcha",
    "Lepc",
    "Limbu",
    "Limb",
    "Linear_A",
    "Lina",
    "Linear_B",
    "Linb",
    "Lisu",
    "Lycian",
    "Lyci",
    "Lydian",
    "Lydi",
    "Mahajani",
    "Mahj",
    "Makasar",
    "Maka",
    "Malayalam",
    "Mlym",
    "Mandaic",
    "Mand",
    "Manichaean",
    "Mani",
    "Marchen",
    "Marc",
    "Medefaidrin",
    "Medf",
    "Masaram_Gondi",
    "Gonm",
    "Meetei_Mayek",
    "Mtei",
    "Mende_Kikakui",
    "Mend",
    "Meroitic_Cursive",
    "Merc",
    "Meroitic_Hieroglyphs",
    "Mero",
    "Miao",
    "Plrd",
    "Modi",
    "Mongolian",
    "Mong",
    "Mro",
    "Mroo",
    "Multani",
    "Mult",
    "Myanmar",
    "Mymr",
    "Nabataean",
    "Nbat",
    "New_Tai_Lue",
    "Talu",
    "Newa",
    "Nko",
    "Nkoo",
    "Nushu",
    "Nshu",
    "Ogham",
    "Ogam",
    "Ol_Chiki",
    "Olck",
    "Old_Hungarian",
    "Hung",
    "Old_Italic",
    "Ital",
    "Old_North_Arabian",
    "Narb",
    "Old_Permic",
    "Perm",
    "Old_Persian",
    "Xpeo",
    "Old_Sogdian",
    "Sogo",
    "Old_South_Arabian",
    "Sarb",
    "Old_Turkic",
    "Orkh",
    "Oriya",
    "Orya",
    "Osage",
    "Osge",
    "Osmanya",
    "Osma",
    "Pahawh_Hmong",
    "Hmng",
    "Palmyrene",
    "Palm",
    "Pau_Cin_Hau",
    "Pauc",
    "Phags_Pa",
    "Phag",
    "Phoenician",
    "Phnx",
    "Psalter_Pahlavi",
    "Phlp",
    "Rejang",
    "Rjng",
    "Runic",
    "Runr",
    "Samaritan",
    "Samr",
    "Saurashtra",
    "Saur",
    "Sharada",
    "Shrd",
    "Shavian",
    "Shaw",
    "Siddham",
    "Sidd",
    "SignWriting",
    "Sgnw",
    "Sinhala",
    "Sinh",
    "Sogdian",
    "Sogd",
    "Sora_Sompeng",
    "Sora",
    "Soyombo",
    "Soyo",
    "Sundanese",
    "Sund",
    "Syloti_Nagri",
    "Sylo",
    "Syriac",
    "Syrc",
    "Tagalog",
    "Tglg",
    "Tagbanwa",
    "Tagb",
    "Tai_Le",
    "Tale",
    "Tai_Tham",
    "Lana",
    "Tai_Viet",
    "Tavt",
    "Takri",
    "Takr",
    "Tamil",
    "Taml",
    "Tangut",
    "Tang",
    "Telugu",
    "Telu",
    "Thaana",
    "Thaa",
    "Thai",
    "Tibetan",
    "Tibt",
    "Tifinagh",
    "Tfng",
    "Tirhuta",
    "Tirh",
    "Ugaritic",
    "Ugar",
    "Vai",
    "Vaii",
    "Warang_Citi",
    "Wara",
    "Yi",
    "Yiii",
    "Zanabazar_Square",
    "Zanb",
)

_casefold = {
    0x0041: 0x0061,
    0x0042: 0x0062,
    0x0043: 0x0063,
    0x0044: 0x0064,
    0x0045: 0x0065,
    0x0046: 0x0066,
    0x0047: 0x0067,
    0x0048: 0x0068,
    0x0049: 0x0069,
    0x004A: 0x006A,
    0x004B: 0x006B,
    0x004C: 0x006C,
    0x004D: 0x006D,
    0x004E: 0x006E,
    0x004F: 0x006F,
    0x0050: 0x0070,
    0x0051: 0x0071,
    0x0052: 0x0072,
    0x0053: 0x0073,
    0x0054: 0x0074,
    0x0055: 0x0075,
    0x0056: 0x0076,
    0x0057: 0x0077,
    0x0058: 0x0078,
    0x0059: 0x0079,
    0x005A: 0x007A,
    0x00B5: 0x03BC,
    0x00C0: 0x00E0,
    0x00C1: 0x00E1,
    0x00C2: 0x00E2,
    0x00C3: 0x00E3,
    0x00C4: 0x00E4,
    0x00C5: 0x00E5,
    0x00C6: 0x00E6,
    0x00C7: 0x00E7,
    0x00C8: 0x00E8,
    0x00C9: 0x00E9,
    0x00CA: 0x00EA,
    0x00CB: 0x00EB,
    0x00CC: 0x00EC,
    0x00CD: 0x00ED,
    0x00CE: 0x00EE,
    0x00CF: 0x00EF,
    0x00D0: 0x00F0,
    0x00D1: 0x00F1,
    0x00D2: 0x00F2,
    0x00D3: 0x00F3,
    0x00D4: 0x00F4,
    0x00D5: 0x00F5,
    0x00D6: 0x00F6,
    0x00D8: 0x00F8,
    0x00D9: 0x00F9,
    0x00DA: 0x00FA,
    0x00DB: 0x00FB,
    0x00DC: 0x00FC,
    0x00DD: 0x00FD,
    0x00DE: 0x00FE,
    0x0100: 0x0101,
    0x0102: 0x0103,
    0x0104: 0x0105,
    0x0106: 0x0107,
    0x0108: 0x0109,
    0x010A: 0x010B,
    0x010C: 0x010D,
    0x010E: 0x010F,
    0x0110: 0x0111,
    0x0112: 0x0113,
    0x0114: 0x0115,
    0x0116: 0x0117,
    0x0118: 0x0119,
    0x011A: 0x011B,
    0x011C: 0x011D,
    0x011E: 0x011F,
    0x0120: 0x0121,
    0x0122: 0x0123,
    0x0124: 0x0125,
    0x0126: 0x0127,
    0x0128: 0x0129,
    0x012A: 0x012B,
    0x012C: 0x012D,
    0x012E: 0x012F,
    0x0132: 0x0133,
    0x0134: 0x0135,
    0x0136: 0x0137,
    0x0139: 0x013A,
    0x013B: 0x013C,
    0x013D: 0x013E,
    0x013F: 0x0140,
    0x0141: 0x0142,
    0x0143: 0x0144,
    0x0145: 0x0146,
    0x0147: 0x0148,
    0x014A: 0x014B,
    0x014C: 0x014D,
    0x014E: 0x014F,
    0x0150: 0x0151,
    0x0152: 0x0153,
    0x0154: 0x0155,
    0x0156: 0x0157,
    0x0158: 0x0159,
    0x015A: 0x015B,
    0x015C: 0x015D,
    0x015E: 0x015F,
    0x0160: 0x0161,
    0x0162: 0x0163,
    0x0164: 0x0165,
    0x0166: 0x0167,
    0x0168: 0x0169,
    0x016A: 0x016B,
    0x016C: 0x016D,
    0x016E: 0x016F,
    0x0170: 0x0171,
    0x0172: 0x0173,
    0x0174: 0x0175,
    0x0176: 0x0177,
    0x0178: 0x00FF,
    0x0179: 0x017A,
    0x017B: 0x017C,
    0x017D: 0x017E,
    0x017F: 0x0073,
    0x0181: 0x0253,
    0x0182: 0x0183,
    0x0184: 0x0185,
    0x0186: 0x0254,
    0x0187: 0x0188,
    0x0189: 0x0256,
    0x018A: 0x0257,
    0x018B: 0x018C,
    0x018E: 0x01DD,
    0x018F: 0x0259,
    0x0190: 0x025B,
    0x0191: 0x0192,
    0x0193: 0x0260,
    0x0194: 0x0263,
    0x0196: 0x0269,
    0x0197: 0x0268,
    0x0198: 0x0199,
    0x019C: 0x026F,
    0x019D: 0x0272,
    0x019F: 0x0275,
    0x01A0: 0x01A1,
    0x01A2: 0x01A3,
    0x01A4: 0x01A5,
    0x01A6: 0x0280,
    0x01A7: 0x01A8,
    0x01A9: 0x0283,
    0x01AC: 0x01AD,
    0x01AE: 0x0288,
    0x01AF: 0x01B0,
    0x01B1: 0x028A,
    0x01B2: 0x028B,
    0x01B3: 0x01B4,
    0x01B5: 0x01B6,
    0x01B7: 0x0292,
    0x01B8: 0x01B9,
    0x01BC: 0x01BD,
    0x01C4: 0x01C6,
    0x01C5: 0x01C6,
    0x01C7: 0x01C9,
    0x01C8: 0x01C9,
    0x01CA: 0x01CC,
    0x01CB: 0x01CC,
    0x01CD: 0x01CE,
    0x01CF: 0x01D0,
    0x01D1: 0x01D2,
    0x01D3: 0x01D4,
    0x01D5: 0x01D6,
    0x01D7: 0x01D8,
    0x01D9: 0x01DA,
    0x01DB: 0x01DC,
    0x01DE: 0x01DF,
    0x01E0: 0x01E1,
    0x01E2: 0x01E3,
    0x01E4: 0x01E5,
    0x01E6: 0x01E7,
    0x01E8: 0x01E9,
    0x01EA: 0x01EB,
    0x01EC: 0x01ED,
    0x01EE: 0x01EF,
    0x01F1: 0x01F3,
    0x01F2: 0x01F3,
    0x01F4: 0x01F5,
    0x01F6: 0x0195,
    0x01F7: 0x01BF,
    0x01F8: 0x01F9,
    0x01FA: 0x01FB,
    0x01FC: 0x01FD,
    0x01FE: 0x01FF,
    0x0200: 0x0201,
    0x0202: 0x0203,
    0x0204: 0x0205,
    0x0206: 0x0207,
    0x0208: 0x0209,
    0x020A: 0x020B,
    0x020C: 0x020D,
    0x020E: 0x020F,
    0x0210: 0x0211,
    0x0212: 0x0213,
    0x0214: 0x0215,
    0x0216: 0x0217,
    0x0218: 0x0219,
    0x021A: 0x021B,
    0x021C: 0x021D,
    0x021E: 0x021F,
    0x0220: 0x019E,
    0x0222: 0x0223,
    0x0224: 0x0225,
    0x0226: 0x0227,
    0x0228: 0x0229,
    0x022A: 0x022B,
    0x022C: 0x022D,
    0x022E: 0x022F,
    0x0230: 0x0231,
    0x0232: 0x0233,
    0x023A: 0x2C65,
    0x023B: 0x023C,
    0x023D: 0x019A,
    0x023E: 0x2C66,
    0x0241: 0x0242,
    0x0243: 0x0180,
    0x0244: 0x0289,
    0x0245: 0x028C,
    0x0246: 0x0247,
    0x0248: 0x0249,
    0x024A: 0x024B,
    0x024C: 0x024D,
    0x024E: 0x024F,
    0x0345: 0x03B9,
    0x0370: 0x0371,
    0x0372: 0x0373,
    0x0376: 0x0377,
    0x037F: 0x03F3,
    0x0386: 0x03AC,
    0x0388: 0x03AD,
    0x0389: 0x03AE,
    0x038A: 0x03AF,
    0x038C: 0x03CC,
    0x038E: 0x03CD,
    0x038F: 0x03CE,
    0x0391: 0x03B1,
    0x0392: 0x03B2,
    0x0393: 0x03B3,
    0x0394: 0x03B4,
    0x0395: 0x03B5,
    0x0396: 0x03B6,
    0x0397: 0x03B7,
    0x0398: 0x03B8,
    0x0399: 0x03B9,
    0x039A: 0x03BA,
    0x039B: 0x03BB,
    0x039C: 0x03BC,
    0x039D: 0x03BD,
    0x039E: 0x03BE,
    0x039F: 0x03BF,
    0x03A0: 0x03C0,
    0x03A1: 0x03C1,
    0x03A3: 0x03C3,
    0x03A4: 0x03C4,
    0x03A5: 0x03C5,
    0x03A6: 0x03C6,
    0x03A7: 0x03C7,
    0x03A8: 0x03C8,
    0x03A9: 0x03C9,
    0x03AA: 0x03CA,
    0x03AB: 0x03CB,
    0x03C2: 0x03C3,
    0x03CF: 0x03D7,
    0x03D0: 0x03B2,
    0x03D1: 0x03B8,
    0x03D5: 0x03C6,
    0x03D6: 0x03C0,
    0x03D8: 0x03D9,
    0x03DA: 0x03DB,
    0x03DC: 0x03DD,
    0x03DE: 0x03DF,
    0x03E0: 0x03E1,
    0x03E2: 0x03E3,
    0x03E4: 0x03E5,
    0x03E6: 0x03E7,
    0x03E8: 0x03E9,
    0x03EA: 0x03EB,
    0x03EC: 0x03ED,
    0x03EE: 0x03EF,
    0x03F0: 0x03BA,
    0x03F1: 0x03C1,
    0x03F4: 0x03B8,
    0x03F5: 0x03B5,
    0x03F7: 0x03F8,
    0x03F9: 0x03F2,
    0x03FA: 0x03FB,
    0x03FD: 0x037B,
    0x03FE: 0x037C,
    0x03FF: 0x037D,
    0x0400: 0x0450,
    0x0401: 0x0451,
    0x0402: 0x0452,
    0x0403: 0x0453,
    0x0404: 0x0454,
    0x0405: 0x0455,
    0x0406: 0x0456,
    0x0407: 0x0457,
    0x0408: 0x0458,
    0x0409: 0x0459,
    0x040A: 0x045A,
    0x040B: 0x045B,
    0x040C: 0x045C,
    0x040D: 0x045D,
    0x040E: 0x045E,
    0x040F: 0x045F,
    0x0410: 0x0430,
    0x0411: 0x0431,
    0x0412: 0x0432,
    0x0413: 0x0433,
    0x0414: 0x0434,
    0x0415: 0x0435,
    0x0416: 0x0436,
    0x0417: 0x0437,
    0x0418: 0x0438,
    0x0419: 0x0439,
    0x041A: 0x043A,
    0x041B: 0x043B,
    0x041C: 0x043C,
    0x041D: 0x043D,
    0x041E: 0x043E,
    0x041F: 0x043F,
    0x0420: 0x0440,
    0x0421: 0x0441,
    0x0422: 0x0442,
    0x0423: 0x0443,
    0x0424: 0x0444,
    0x0425: 0x0445,
    0x0426: 0x0446,
    0x0427: 0x0447,
    0x0428: 0x0448,
    0x0429: 0x0449,
    0x042A: 0x044A,
    0x042B: 0x044B,
    0x042C: 0x044C,
    0x042D: 0x044D,
    0x042E: 0x044E,
    0x042F: 0x044F,
    0x0460: 0x0461,
    0x0462: 0x0463,
    0x0464: 0x0465,
    0x0466: 0x0467,
    0x0468: 0x0469,
    0x046A: 0x046B,
    0x046C: 0x046D,
    0x046E: 0x046F,
    0x0470: 0x0471,
    0x0472: 0x0473,
    0x0474: 0x0475,
    0x0476: 0x0477,
    0x0478: 0x0479,
    0x047A: 0x047B,
    0x047C: 0x047D,
    0x047E: 0x047F,
    0x0480: 0x0481,
    0x048A: 0x048B,
    0x048C: 0x048D,
    0x048E: 0x048F,
    0x0490: 0x0491,
    0x0492: 0x0493,
    0x0494: 0x0495,
    0x0496: 0x0497,
    0x0498: 0x0499,
    0x049A: 0x049B,
    0x049C: 0x049D,
    0x049E: 0x049F,
    0x04A0: 0x04A1,
    0x04A2: 0x04A3,
    0x04A4: 0x04A5,
    0x04A6: 0x04A7,
    0x04A8: 0x04A9,
    0x04AA: 0x04AB,
    0x04AC: 0x04AD,
    0x04AE: 0x04AF,
    0x04B0: 0x04B1,
    0x04B2: 0x04B3,
    0x04B4: 0x04B5,
    0x04B6: 0x04B7,
    0x04B8: 0x04B9,
    0x04BA: 0x04BB,
    0x04BC: 0x04BD,
    0x04BE: 0x04BF,
    0x04C0: 0x04CF,
    0x04C1: 0x04C2,
    0x04C3: 0x04C4,
    0x04C5: 0x04C6,
    0x04C7: 0x04C8,
    0x04C9: 0x04CA,
    0x04CB: 0x04CC,
    0x04CD: 0x04CE,
    0x04D0: 0x04D1,
    0x04D2: 0x04D3,
    0x04D4: 0x04D5,
    0x04D6: 0x04D7,
    0x04D8: 0x04D9,
    0x04DA: 0x04DB,
    0x04DC: 0x04DD,
    0x04DE: 0x04DF,
    0x04E0: 0x04E1,
    0x04E2: 0x04E3,
    0x04E4: 0x04E5,
    0x04E6: 0x04E7,
    0x04E8: 0x04E9,
    0x04EA: 0x04EB,
    0x04EC: 0x04ED,
    0x04EE: 0x04EF,
    0x04F0: 0x04F1,
    0x04F2: 0x04F3,
    0x04F4: 0x04F5,
    0x04F6: 0x04F7,
    0x04F8: 0x04F9,
    0x04FA: 0x04FB,
    0x04FC: 0x04FD,
    0x04FE: 0x04FF,
    0x0500: 0x0501,
    0x0502: 0x0503,
    0x0504: 0x0505,
    0x0506: 0x0507,
    0x0508: 0x0509,
    0x050A: 0x050B,
    0x050C: 0x050D,
    0x050E: 0x050F,
    0x0510: 0x0511,
    0x0512: 0x0513,
    0x0514: 0x0515,
    0x0516: 0x0517,
    0x0518: 0x0519,
    0x051A: 0x051B,
    0x051C: 0x051D,
    0x051E: 0x051F,
    0x0520: 0x0521,
    0x0522: 0x0523,
    0x0524: 0x0525,
    0x0526: 0x0527,
    0x0528: 0x0529,
    0x052A: 0x052B,
    0x052C: 0x052D,
    0x052E: 0x052F,
    0x0531: 0x0561,
    0x0532: 0x0562,
    0x0533: 0x0563,
    0x0534: 0x0564,
    0x0535: 0x0565,
    0x0536: 0x0566,
    0x0537: 0x0567,
    0x0538: 0x0568,
    0x0539: 0x0569,
    0x053A: 0x056A,
    0x053B: 0x056B,
    0x053C: 0x056C,
    0x053D: 0x056D,
    0x053E: 0x056E,
    0x053F: 0x056F,
    0x0540: 0x0570,
    0x0541: 0x0571,
    0x0542: 0x0572,
    0x0543: 0x0573,
    0x0544: 0x0574,
    0x0545: 0x0575,
    0x0546: 0x0576,
    0x0547: 0x0577,
    0x0548: 0x0578,
    0x0549: 0x0579,
    0x054A: 0x057A,
    0x054B: 0x057B,
    0x054C: 0x057C,
    0x054D: 0x057D,
    0x054E: 0x057E,
    0x054F: 0x057F,
    0x0550: 0x0580,
    0x0551: 0x0581,
    0x0552: 0x0582,
    0x0553: 0x0583,
    0x0554: 0x0584,
    0x0555: 0x0585,
    0x0556: 0x0586,
    0x10A0: 0x2D00,
    0x10A1: 0x2D01,
    0x10A2: 0x2D02,
    0x10A3: 0x2D03,
    0x10A4: 0x2D04,
    0x10A5: 0x2D05,
    0x10A6: 0x2D06,
    0x10A7: 0x2D07,
    0x10A8: 0x2D08,
    0x10A9: 0x2D09,
    0x10AA: 0x2D0A,
    0x10AB: 0x2D0B,
    0x10AC: 0x2D0C,
    0x10AD: 0x2D0D,
    0x10AE: 0x2D0E,
    0x10AF: 0x2D0F,
    0x10B0: 0x2D10,
    0x10B1: 0x2D11,
    0x10B2: 0x2D12,
    0x10B3: 0x2D13,
    0x10B4: 0x2D14,
    0x10B5: 0x2D15,
    0x10B6: 0x2D16,
    0x10B7: 0x2D17,
    0x10B8: 0x2D18,
    0x10B9: 0x2D19,
    0x10BA: 0x2D1A,
    0x10BB: 0x2D1B,
    0x10BC: 0x2D1C,
    0x10BD: 0x2D1D,
    0x10BE: 0x2D1E,
    0x10BF: 0x2D1F,
    0x10C0: 0x2D20,
    0x10C1: 0x2D21,
    0x10C2: 0x2D22,
    0x10C3: 0x2D23,
    0x10C4: 0x2D24,
    0x10C5: 0x2D25,
    0x10C7: 0x2D27,
    0x10CD: 0x2D2D,
    0x13F8: 0x13F0,
    0x13F9: 0x13F1,
    0x13FA: 0x13F2,
    0x13FB: 0x13F3,
    0x13FC: 0x13F4,
    0x13FD: 0x13F5,
    0x1C80: 0x0432,
    0x1C81: 0x0434,
    0x1C82: 0x043E,
    0x1C83: 0x0441,
    0x1C84: 0x0442,
    0x1C85: 0x0442,
    0x1C86: 0x044A,
    0x1C87: 0x0463,
    0x1C88: 0xA64B,
    0x1C90: 0x10D0,
    0x1C91: 0x10D1,
    0x1C92: 0x10D2,
    0x1C93: 0x10D3,
    0x1C94: 0x10D4,
    0x1C95: 0x10D5,
    0x1C96: 0x10D6,
    0x1C97: 0x10D7,
    0x1C98: 0x10D8,
    0x1C99: 0x10D9,
    0x1C9A: 0x10DA,
    0x1C9B: 0x10DB,
    0x1C9C: 0x10DC,
    0x1C9D: 0x10DD,
    0x1C9E: 0x10DE,
    0x1C9F: 0x10DF,
    0x1CA0: 0x10E0,
    0x1CA1: 0x10E1,
    0x1CA2: 0x10E2,
    0x1CA3: 0x10E3,
    0x1CA4: 0x10E4,
    0x1CA5: 0x10E5,
    0x1CA6: 0x10E6,
    0x1CA7: 0x10E7,
    0x1CA8: 0x10E8,
    0x1CA9: 0x10E9,
    0x1CAA: 0x10EA,
    0x1CAB: 0x10EB,
    0x1CAC: 0x10EC,
    0x1CAD: 0x10ED,
    0x1CAE: 0x10EE,
    0x1CAF: 0x10EF,
    0x1CB0: 0x10F0,
    0x1CB1: 0x10F1,
    0x1CB2: 0x10F2,
    0x1CB3: 0x10F3,
    0x1CB4: 0x10F4,
    0x1CB5: 0x10F5,
    0x1CB6: 0x10F6,
    0x1CB7: 0x10F7,
    0x1CB8: 0x10F8,
    0x1CB9: 0x10F9,
    0x1CBA: 0x10FA,
    0x1CBD: 0x10FD,
    0x1CBE: 0x10FE,
    0x1CBF: 0x10FF,
    0x1E00: 0x1E01,
    0x1E02: 0x1E03,
    0x1E04: 0x1E05,
    0x1E06: 0x1E07,
    0x1E08: 0x1E09,
    0x1E0A: 0x1E0B,
    0x1E0C: 0x1E0D,
    0x1E0E: 0x1E0F,
    0x1E10: 0x1E11,
    0x1E12: 0x1E13,
    0x1E14: 0x1E15,
    0x1E16: 0x1E17,
    0x1E18: 0x1E19,
    0x1E1A: 0x1E1B,
    0x1E1C: 0x1E1D,
    0x1E1E: 0x1E1F,
    0x1E20: 0x1E21,
    0x1E22: 0x1E23,
    0x1E24: 0x1E25,
    0x1E26: 0x1E27,
    0x1E28: 0x1E29,
    0x1E2A: 0x1E2B,
    0x1E2C: 0x1E2D,
    0x1E2E: 0x1E2F,
    0x1E30: 0x1E31,
    0x1E32: 0x1E33,
    0x1E34: 0x1E35,
    0x1E36: 0x1E37,
    0x1E38: 0x1E39,
    0x1E3A: 0x1E3B,
    0x1E3C: 0x1E3D,
    0x1E3E: 0x1E3F,
    0x1E40: 0x1E41,
    0x1E42: 0x1E43,
    0x1E44: 0x1E45,
    0x1E46: 0x1E47,
    0x1E48: 0x1E49,
    0x1E4A: 0x1E4B,
    0x1E4C: 0x1E4D,
    0x1E4E: 0x1E4F,
    0x1E50: 0x1E51,
    0x1E52: 0x1E53,
    0x1E54: 0x1E55,
    0x1E56: 0x1E57,
    0x1E58: 0x1E59,
    0x1E5A: 0x1E5B,
    0x1E5C: 0x1E5D,
    0x1E5E: 0x1E5F,
    0x1E60: 0x1E61,
    0x1E62: 0x1E63,
    0x1E64: 0x1E65,
    0x1E66: 0x1E67,
    0x1E68: 0x1E69,
    0x1E6A: 0x1E6B,
    0x1E6C: 0x1E6D,
    0x1E6E: 0x1E6F,
    0x1E70: 0x1E71,
    0x1E72: 0x1E73,
    0x1E74: 0x1E75,
    0x1E76: 0x1E77,
    0x1E78: 0x1E79,
    0x1E7A: 0x1E7B,
    0x1E7C: 0x1E7D,
    0x1E7E: 0x1E7F,
    0x1E80: 0x1E81,
    0x1E82: 0x1E83,
    0x1E84: 0x1E85,
    0x1E86: 0x1E87,
    0x1E88: 0x1E89,
    0x1E8A: 0x1E8B,
    0x1E8C: 0x1E8D,
    0x1E8E: 0x1E8F,
    0x1E90: 0x1E91,
    0x1E92: 0x1E93,
    0x1E94: 0x1E95,
    0x1E9B: 0x1E61,
    0x1E9E: 0x00DF,
    0x1EA0: 0x1EA1,
    0x1EA2: 0x1EA3,
    0x1EA4: 0x1EA5,
    0x1EA6: 0x1EA7,
    0x1EA8: 0x1EA9,
    0x1EAA: 0x1EAB,
    0x1EAC: 0x1EAD,
    0x1EAE: 0x1EAF,
    0x1EB0: 0x1EB1,
    0x1EB2: 0x1EB3,
    0x1EB4: 0x1EB5,
    0x1EB6: 0x1EB7,
    0x1EB8: 0x1EB9,
    0x1EBA: 0x1EBB,
    0x1EBC: 0x1EBD,
    0x1EBE: 0x1EBF,
    0x1EC0: 0x1EC1,
    0x1EC2: 0x1EC3,
    0x1EC4: 0x1EC5,
    0x1EC6: 0x1EC7,
    0x1EC8: 0x1EC9,
    0x1ECA: 0x1ECB,
    0x1ECC: 0x1ECD,
    0x1ECE: 0x1ECF,
    0x1ED0: 0x1ED1,
    0x1ED2: 0x1ED3,
    0x1ED4: 0x1ED5,
    0x1ED6: 0x1ED7,
    0x1ED8: 0x1ED9,
    0x1EDA: 0x1EDB,
    0x1EDC: 0x1EDD,
    0x1EDE: 0x1EDF,
    0x1EE0: 0x1EE1,
    0x1EE2: 0x1EE3,
    0x1EE4: 0x1EE5,
    0x1EE6: 0x1EE7,
    0x1EE8: 0x1EE9,
    0x1EEA: 0x1EEB,
    0x1EEC: 0x1EED,
    0x1EEE: 0x1EEF,
    0x1EF0: 0x1EF1,
    0x1EF2: 0x1EF3,
    0x1EF4: 0x1EF5,
    0x1EF6: 0x1EF7,
    0x1EF8: 0x1EF9,
    0x1EFA: 0x1EFB,
    0x1EFC: 0x1EFD,
    0x1EFE: 0x1EFF,
    0x1F08: 0x1F00,
    0x1F09: 0x1F01,
    0x1F0A: 0x1F02,
    0x1F0B: 0x1F03,
    0x1F0C: 0x1F04,
    0x1F0D: 0x1F05,
    0x1F0E: 0x1F06,
    0x1F0F: 0x1F07,
    0x1F18: 0x1F10,
    0x1F19: 0x1F11,
    0x1F1A: 0x1F12,
    0x1F1B: 0x1F13,
    0x1F1C: 0x1F14,
    0x1F1D: 0x1F15,
    0x1F28: 0x1F20,
    0x1F29: 0x1F21,
    0x1F2A: 0x1F22,
    0x1F2B: 0x1F23,
    0x1F2C: 0x1F24,
    0x1F2D: 0x1F25,
    0x1F2E: 0x1F26,
    0x1F2F: 0x1F27,
    0x1F38: 0x1F30,
    0x1F39: 0x1F31,
    0x1F3A: 0x1F32,
    0x1F3B: 0x1F33,
    0x1F3C: 0x1F34,
    0x1F3D: 0x1F35,
    0x1F3E: 0x1F36,
    0x1F3F: 0x1F37,
    0x1F48: 0x1F40,
    0x1F49: 0x1F41,
    0x1F4A: 0x1F42,
    0x1F4B: 0x1F43,
    0x1F4C: 0x1F44,
    0x1F4D: 0x1F45,
    0x1F59: 0x1F51,
    0x1F5B: 0x1F53,
    0x1F5D: 0x1F55,
    0x1F5F: 0x1F57,
    0x1F68: 0x1F60,
    0x1F69: 0x1F61,
    0x1F6A: 0x1F62,
    0x1F6B: 0x1F63,
    0x1F6C: 0x1F64,
    0x1F6D: 0x1F65,
    0x1F6E: 0x1F66,
    0x1F6F: 0x1F67,
    0x1F88: 0x1F80,
    0x1F89: 0x1F81,
    0x1F8A: 0x1F82,
    0x1F8B: 0x1F83,
    0x1F8C: 0x1F84,
    0x1F8D: 0x1F85,
    0x1F8E: 0x1F86,
    0x1F8F: 0x1F87,
    0x1F98: 0x1F90,
    0x1F99: 0x1F91,
    0x1F9A: 0x1F92,
    0x1F9B: 0x1F93,
    0x1F9C: 0x1F94,
    0x1F9D: 0x1F95,
    0x1F9E: 0x1F96,
    0x1F9F: 0x1F97,
    0x1FA8: 0x1FA0,
    0x1FA9: 0x1FA1,
    0x1FAA: 0x1FA2,
    0x1FAB: 0x1FA3,
    0x1FAC: 0x1FA4,
    0x1FAD: 0x1FA5,
    0x1FAE: 0x1FA6,
    0x1FAF: 0x1FA7,
    0x1FB8: 0x1FB0,
    0x1FB9: 0x1FB1,
    0x1FBA: 0x1F70,
    0x1FBB: 0x1F71,
    0x1FBC: 0x1FB3,
    0x1FBE: 0x03B9,
    0x1FC8: 0x1F72,
    0x1FC9: 0x1F73,
    0x1FCA: 0x1F74,
    0x1FCB: 0x1F75,
    0x1FCC: 0x1FC3,
    0x1FD8: 0x1FD0,
    0x1FD9: 0x1FD1,
    0x1FDA: 0x1F76,
    0x1FDB: 0x1F77,
    0x1FE8: 0x1FE0,
    0x1FE9: 0x1FE1,
    0x1FEA: 0x1F7A,
    0x1FEB: 0x1F7B,
    0x1FEC: 0x1FE5,
    0x1FF8: 0x1F78,
    0x1FF9: 0x1F79,
    0x1FFA: 0x1F7C,
    0x1FFB: 0x1F7D,
    0x1FFC: 0x1FF3,
    0x2126: 0x03C9,
    0x212A: 0x006B,
    0x212B: 0x00E5,
    0x2132: 0x214E,
    0x2160: 0x2170,
    0x2161: 0x2171,
    0x2162: 0x2172,
    0x2163: 0x2173,
    0x2164: 0x2174,
    0x2165: 0x2175,
    0x2166: 0x2176,
    0x2167: 0x2177,
    0x2168: 0x2178,
    0x2169: 0x2179,
    0x216A: 0x217A,
    0x216B: 0x217B,
    0x216C: 0x217C,
    0x216D: 0x217D,
    0x216E: 0x217E,
    0x216F: 0x217F,
    0x2183: 0x2184,
    0x24B6: 0x24D0,
    0x24B7: 0x24D1,
    0x24B8: 0x24D2,
    0x24B9: 0x24D3,
    0x24BA: 0x24D4,
    0x24BB: 0x24D5,
    0x24BC: 0x24D6,
    0x24BD: 0x24D7,
    0x24BE: 0x24D8,
    0x24BF: 0x24D9,
    0x24C0: 0x24DA,
    0x24C1: 0x24DB,
    0x24C2: 0x24DC,
    0x24C3: 0x24DD,
    0x24C4: 0x24DE,
    0x24C5: 0x24DF,
    0x24C6: 0x24E0,
    0x24C7: 0x24E1,
    0x24C8: 0x24E2,
    0x24C9: 0x24E3,
    0x24CA: 0x24E4,
    0x24CB: 0x24E5,
    0x24CC: 0x24E6,
    0x24CD: 0x24E7,
    0x24CE: 0x24E8,
    0x24CF: 0x24E9,
    0x2C00: 0x2C30,
    0x2C01: 0x2C31,
    0x2C02: 0x2C32,
    0x2C03: 0x2C33,
    0x2C04: 0x2C34,
    0x2C05: 0x2C35,
    0x2C06: 0x2C36,
    0x2C07: 0x2C37,
    0x2C08: 0x2C38,
    0x2C09: 0x2C39,
    0x2C0A: 0x2C3A,
    0x2C0B: 0x2C3B,
    0x2C0C: 0x2C3C,
    0x2C0D: 0x2C3D,
    0x2C0E: 0x2C3E,
    0x2C0F: 0x2C3F,
    0x2C10: 0x2C40,
    0x2C11: 0x2C41,
    0x2C12: 0x2C42,
    0x2C13: 0x2C43,
    0x2C14: 0x2C44,
    0x2C15: 0x2C45,
    0x2C16: 0x2C46,
    0x2C17: 0x2C47,
    0x2C18: 0x2C48,
    0x2C19: 0x2C49,
    0x2C1A: 0x2C4A,
    0x2C1B: 0x2C4B,
    0x2C1C: 0x2C4C,
    0x2C1D: 0x2C4D,
    0x2C1E: 0x2C4E,
    0x2C1F: 0x2C4F,
    0x2C20: 0x2C50,
    0x2C21: 0x2C51,
    0x2C22: 0x2C52,
    0x2C23: 0x2C53,
    0x2C24: 0x2C54,
    0x2C25: 0x2C55,
    0x2C26: 0x2C56,
    0x2C27: 0x2C57,
    0x2C28: 0x2C58,
    0x2C29: 0x2C59,
    0x2C2A: 0x2C5A,
    0x2C2B: 0x2C5B,
    0x2C2C: 0x2C5C,
    0x2C2D: 0x2C5D,
    0x2C2E: 0x2C5E,
    0x2C60: 0x2C61,
    0x2C62: 0x026B,
    0x2C63: 0x1D7D,
    0x2C64: 0x027D,
    0x2C67: 0x2C68,
    0x2C69: 0x2C6A,
    0x2C6B: 0x2C6C,
    0x2C6D: 0x0251,
    0x2C6E: 0x0271,
    0x2C6F: 0x0250,
    0x2C70: 0x0252,
    0x2C72: 0x2C73,
    0x2C75: 0x2C76,
    0x2C7E: 0x023F,
    0x2C7F: 0x0240,
    0x2C80: 0x2C81,
    0x2C82: 0x2C83,
    0x2C84: 0x2C85,
    0x2C86: 0x2C87,
    0x2C88: 0x2C89,
    0x2C8A: 0x2C8B,
    0x2C8C: 0x2C8D,
    0x2C8E: 0x2C8F,
    0x2C90: 0x2C91,
    0x2C92: 0x2C93,
    0x2C94: 0x2C95,
    0x2C96: 0x2C97,
    0x2C98: 0x2C99,
    0x2C9A: 0x2C9B,
    0x2C9C: 0x2C9D,
    0x2C9E: 0x2C9F,
    0x2CA0: 0x2CA1,
    0x2CA2: 0x2CA3,
    0x2CA4: 0x2CA5,
    0x2CA6: 0x2CA7,
    0x2CA8: 0x2CA9,
    0x2CAA: 0x2CAB,
    0x2CAC: 0x2CAD,
    0x2CAE: 0x2CAF,
    0x2CB0: 0x2CB1,
    0x2CB2: 0x2CB3,
    0x2CB4: 0x2CB5,
    0x2CB6: 0x2CB7,
    0x2CB8: 0x2CB9,
    0x2CBA: 0x2CBB,
    0x2CBC: 0x2CBD,
    0x2CBE: 0x2CBF,
    0x2CC0: 0x2CC1,
    0x2CC2: 0x2CC3,
    0x2CC4: 0x2CC5,
    0x2CC6: 0x2CC7,
    0x2CC8: 0x2CC9,
    0x2CCA: 0x2CCB,
    0x2CCC: 0x2CCD,
    0x2CCE: 0x2CCF,
    0x2CD0: 0x2CD1,
    0x2CD2: 0x2CD3,
    0x2CD4: 0x2CD5,
    0x2CD6: 0x2CD7,
    0x2CD8: 0x2CD9,
    0x2CDA: 0x2CDB,
    0x2CDC: 0x2CDD,
    0x2CDE: 0x2CDF,
    0x2CE0: 0x2CE1,
    0x2CE2: 0x2CE3,
    0x2CEB: 0x2CEC,
    0x2CED: 0x2CEE,
    0x2CF2: 0x2CF3,
    0xA640: 0xA641,
    0xA642: 0xA643,
    0xA644: 0xA645,
    0xA646: 0xA647,
    0xA648: 0xA649,
    0xA64A: 0xA64B,
    0xA64C: 0xA64D,
    0xA64E: 0xA64F,
    0xA650: 0xA651,
    0xA652: 0xA653,
    0xA654: 0xA655,
    0xA656: 0xA657,
    0xA658: 0xA659,
    0xA65A: 0xA65B,
    0xA65C: 0xA65D,
    0xA65E: 0xA65F,
    0xA660: 0xA661,
    0xA662: 0xA663,
    0xA664: 0xA665,
    0xA666: 0xA667,
    0xA668: 0xA669,
    0xA66A: 0xA66B,
    0xA66C: 0xA66D,
    0xA680: 0xA681,
    0xA682: 0xA683,
    0xA684: 0xA685,
    0xA686: 0xA687,
    0xA688: 0xA689,
    0xA68A: 0xA68B,
    0xA68C: 0xA68D,
    0xA68E: 0xA68F,
    0xA690: 0xA691,
    0xA692: 0xA693,
    0xA694: 0xA695,
    0xA696: 0xA697,
    0xA698: 0xA699,
    0xA69A: 0xA69B,
    0xA722: 0xA723,
    0xA724: 0xA725,
    0xA726: 0xA727,
    0xA728: 0xA729,
    0xA72A: 0xA72B,
    0xA72C: 0xA72D,
    0xA72E: 0xA72F,
    0xA732: 0xA733,
    0xA734: 0xA735,
    0xA736: 0xA737,
    0xA738: 0xA739,
    0xA73A: 0xA73B,
    0xA73C: 0xA73D,
    0xA73E: 0xA73F,
    0xA740: 0xA741,
    0xA742: 0xA743,
    0xA744: 0xA745,
    0xA746: 0xA747,
    0xA748: 0xA749,
    0xA74A: 0xA74B,
    0xA74C: 0xA74D,
    0xA74E: 0xA74F,
    0xA750: 0xA751,
    0xA752: 0xA753,
    0xA754: 0xA755,
    0xA756: 0xA757,
    0xA758: 0xA759,
    0xA75A: 0xA75B,
    0xA75C: 0xA75D,
    0xA75E: 0xA75F,
    0xA760: 0xA761,
    0xA762: 0xA763,
    0xA764: 0xA765,
    0xA766: 0xA767,
    0xA768: 0xA769,
    0xA76A: 0xA76B,
    0xA76C: 0xA76D,
    0xA76E: 0xA76F,
    0xA779: 0xA77A,
    0xA77B: 0xA77C,
    0xA77D: 0x1D79,
    0xA77E: 0xA77F,
    0xA780: 0xA781,
    0xA782: 0xA783,
    0xA784: 0xA785,
    0xA786: 0xA787,
    0xA78B: 0xA78C,
    0xA78D: 0x0265,
    0xA790: 0xA791,
    0xA792: 0xA793,
    0xA796: 0xA797,
    0xA798: 0xA799,
    0xA79A: 0xA79B,
    0xA79C: 0xA79D,
    0xA79E: 0xA79F,
    0xA7A0: 0xA7A1,
    0xA7A2: 0xA7A3,
    0xA7A4: 0xA7A5,
    0xA7A6: 0xA7A7,
    0xA7A8: 0xA7A9,
    0xA7AA: 0x0266,
    0xA7AB: 0x025C,
    0xA7AC: 0x0261,
    0xA7AD: 0x026C,
    0xA7AE: 0x026A,
    0xA7B0: 0x029E,
    0xA7B1: 0x0287,
    0xA7B2: 0x029D,
    0xA7B3: 0xAB53,
    0xA7B4: 0xA7B5,
    0xA7B6: 0xA7B7,
    0xA7B8: 0xA7B9,
    0xA7BA: 0xA7BB,
    0xA7BC: 0xA7BD,
    0xA7BE: 0xA7BF,
    0xA7C2: 0xA7C3,
    0xA7C4: 0xA794,
    0xA7C5: 0x0282,
    0xA7C6: 0x1D8E,
    0xAB70: 0x13A0,
    0xAB71: 0x13A1,
    0xAB72: 0x13A2,
    0xAB73: 0x13A3,
    0xAB74: 0x13A4,
    0xAB75: 0x13A5,
    0xAB76: 0x13A6,
    0xAB77: 0x13A7,
    0xAB78: 0x13A8,
    0xAB79: 0x13A9,
    0xAB7A: 0x13AA,
    0xAB7B: 0x13AB,
    0xAB7C: 0x13AC,
    0xAB7D: 0x13AD,
    0xAB7E: 0x13AE,
    0xAB7F: 0x13AF,
    0xAB80: 0x13B0,
    0xAB81: 0x13B1,
    0xAB82: 0x13B2,
    0xAB83: 0x13B3,
    0xAB84: 0x13B4,
    0xAB85: 0x13B5,
    0xAB86: 0x13B6,
    0xAB87: 0x13B7,
    0xAB88: 0x13B8,
    0xAB89: 0x13B9,
    0xAB8A: 0x13BA,
    0xAB8B: 0x13BB,
    0xAB8C: 0x13BC,
    0xAB8D: 0x13BD,
    0xAB8E: 0x13BE,
    0xAB8F: 0x13BF,
    0xAB90: 0x13C0,
    0xAB91: 0x13C1,
    0xAB92: 0x13C2,
    0xAB93: 0x13C3,
    0xAB94: 0x13C4,
    0xAB95: 0x13C5,
    0xAB96: 0x13C6,
    0xAB97: 0x13C7,
    0xAB98: 0x13C8,
    0xAB99: 0x13C9,
    0xAB9A: 0x13CA,
    0xAB9B: 0x13CB,
    0xAB9C: 0x13CC,
    0xAB9D: 0x13CD,
    0xAB9E: 0x13CE,
    0xAB9F: 0x13CF,
    0xABA0: 0x13D0,
    0xABA1: 0x13D1,
    0xABA2: 0x13D2,
    0xABA3: 0x13D3,
    0xABA4: 0x13D4,
    0xABA5: 0x13D5,
    0xABA6: 0x13D6,
    0xABA7: 0x13D7,
    0xABA8: 0x13D8,
    0xABA9: 0x13D9,
    0xABAA: 0x13DA,
    0xABAB: 0x13DB,
    0xABAC: 0x13DC,
    0xABAD: 0x13DD,
    0xABAE: 0x13DE,
    0xABAF: 0x13DF,
    0xABB0: 0x13E0,
    0xABB1: 0x13E1,
    0xABB2: 0x13E2,
    0xABB3: 0x13E3,
    0xABB4: 0x13E4,
    0xABB5: 0x13E5,
    0xABB6: 0x13E6,
    0xABB7: 0x13E7,
    0xABB8: 0x13E8,
    0xABB9: 0x13E9,
    0xABBA: 0x13EA,
    0xABBB: 0x13EB,
    0xABBC: 0x13EC,
    0xABBD: 0x13ED,
    0xABBE: 0x13EE,
    0xABBF: 0x13EF,
    0xFF21: 0xFF41,
    0xFF22: 0xFF42,
    0xFF23: 0xFF43,
    0xFF24: 0xFF44,
    0xFF25: 0xFF45,
    0xFF26: 0xFF46,
    0xFF27: 0xFF47,
    0xFF28: 0xFF48,
    0xFF29: 0xFF49,
    0xFF2A: 0xFF4A,
    0xFF2B: 0xFF4B,
    0xFF2C: 0xFF4C,
    0xFF2D: 0xFF4D,
    0xFF2E: 0xFF4E,
    0xFF2F: 0xFF4F,
    0xFF30: 0xFF50,
    0xFF31: 0xFF51,
    0xFF32: 0xFF52,
    0xFF33: 0xFF53,
    0xFF34: 0xFF54,
    0xFF35: 0xFF55,
    0xFF36: 0xFF56,
    0xFF37: 0xFF57,
    0xFF38: 0xFF58,
    0xFF39: 0xFF59,
    0xFF3A: 0xFF5A,
    0x10400: 0x10428,
    0x10401: 0x10429,
    0x10402: 0x1042A,
    0x10403: 0x1042B,
    0x10404: 0x1042C,
    0x10405: 0x1042D,
    0x10406: 0x1042E,
    0x10407: 0x1042F,
    0x10408: 0x10430,
    0x10409: 0x10431,
    0x1040A: 0x10432,
    0x1040B: 0x10433,
    0x1040C: 0x10434,
    0x1040D: 0x10435,
    0x1040E: 0x10436,
    0x1040F: 0x10437,
    0x10410: 0x10438,
    0x10411: 0x10439,
    0x10412: 0x1043A,
    0x10413: 0x1043B,
    0x10414: 0x1043C,
    0x10415: 0x1043D,
    0x10416: 0x1043E,
    0x10417: 0x1043F,
    0x10418: 0x10440,
    0x10419: 0x10441,
    0x1041A: 0x10442,
    0x1041B: 0x10443,
    0x1041C: 0x10444,
    0x1041D: 0x10445,
    0x1041E: 0x10446,
    0x1041F: 0x10447,
    0x10420: 0x10448,
    0x10421: 0x10449,
    0x10422: 0x1044A,
    0x10423: 0x1044B,
    0x10424: 0x1044C,
    0x10425: 0x1044D,
    0x10426: 0x1044E,
    0x10427: 0x1044F,
    0x104B0: 0x104D8,
    0x104B1: 0x104D9,
    0x104B2: 0x104DA,
    0x104B3: 0x104DB,
    0x104B4: 0x104DC,
    0x104B5: 0x104DD,
    0x104B6: 0x104DE,
    0x104B7: 0x104DF,
    0x104B8: 0x104E0,
    0x104B9: 0x104E1,
    0x104BA: 0x104E2,
    0x104BB: 0x104E3,
    0x104BC: 0x104E4,
    0x104BD: 0x104E5,
    0x104BE: 0x104E6,
    0x104BF: 0x104E7,
    0x104C0: 0x104E8,
    0x104C1: 0x104E9,
    0x104C2: 0x104EA,
    0x104C3: 0x104EB,
    0x104C4: 0x104EC,
    0x104C5: 0x104ED,
    0x104C6: 0x104EE,
    0x104C7: 0x104EF,
    0x104C8: 0x104F0,
    0x104C9: 0x104F1,
    0x104CA: 0x104F2,
    0x104CB: 0x104F3,
    0x104CC: 0x104F4,
    0x104CD: 0x104F5,
    0x104CE: 0x104F6,
    0x104CF: 0x104F7,
    0x104D0: 0x104F8,
    0x104D1: 0x104F9,
    0x104D2: 0x104FA,
    0x104D3: 0x104FB,
    0x10C80: 0x10CC0,
    0x10C81: 0x10CC1,
    0x10C82: 0x10CC2,
    0x10C83: 0x10CC3,
    0x10C84: 0x10CC4,
    0x10C85: 0x10CC5,
    0x10C86: 0x10CC6,
    0x10C87: 0x10CC7,
    0x10C88: 0x10CC8,
    0x10C89: 0x10CC9,
    0x10C8A: 0x10CCA,
    0x10C8B: 0x10CCB,
    0x10C8C: 0x10CCC,
    0x10C8D: 0x10CCD,
    0x10C8E: 0x10CCE,
    0x10C8F: 0x10CCF,
    0x10C90: 0x10CD0,
    0x10C91: 0x10CD1,
    0x10C92: 0x10CD2,
    0x10C93: 0x10CD3,
    0x10C94: 0x10CD4,
    0x10C95: 0x10CD5,
    0x10C96: 0x10CD6,
    0x10C97: 0x10CD7,
    0x10C98: 0x10CD8,
    0x10C99: 0x10CD9,
    0x10C9A: 0x10CDA,
    0x10C9B: 0x10CDB,
    0x10C9C: 0x10CDC,
    0x10C9D: 0x10CDD,
    0x10C9E: 0x10CDE,
    0x10C9F: 0x10CDF,
    0x10CA0: 0x10CE0,
    0x10CA1: 0x10CE1,
    0x10CA2: 0x10CE2,
    0x10CA3: 0x10CE3,
    0x10CA4: 0x10CE4,
    0x10CA5: 0x10CE5,
    0x10CA6: 0x10CE6,
    0x10CA7: 0x10CE7,
    0x10CA8: 0x10CE8,
    0x10CA9: 0x10CE9,
    0x10CAA: 0x10CEA,
    0x10CAB: 0x10CEB,
    0x10CAC: 0x10CEC,
    0x10CAD: 0x10CED,
    0x10CAE: 0x10CEE,
    0x10CAF: 0x10CEF,
    0x10CB0: 0x10CF0,
    0x10CB1: 0x10CF1,
    0x10CB2: 0x10CF2,
    0x118A0: 0x118C0,
    0x118A1: 0x118C1,
    0x118A2: 0x118C2,
    0x118A3: 0x118C3,
    0x118A4: 0x118C4,
    0x118A5: 0x118C5,
    0x118A6: 0x118C6,
    0x118A7: 0x118C7,
    0x118A8: 0x118C8,
    0x118A9: 0x118C9,
    0x118AA: 0x118CA,
    0x118AB: 0x118CB,
    0x118AC: 0x118CC,
    0x118AD: 0x118CD,
    0x118AE: 0x118CE,
    0x118AF: 0x118CF,
    0x118B0: 0x118D0,
    0x118B1: 0x118D1,
    0x118B2: 0x118D2,
    0x118B3: 0x118D3,
    0x118B4: 0x118D4,
    0x118B5: 0x118D5,
    0x118B6: 0x118D6,
    0x118B7: 0x118D7,
    0x118B8: 0x118D8,
    0x118B9: 0x118D9,
    0x118BA: 0x118DA,
    0x118BB: 0x118DB,
    0x118BC: 0x118DC,
    0x118BD: 0x118DD,
    0x118BE: 0x118DE,
    0x118BF: 0x118DF,
    0x16E40: 0x16E60,
    0x16E41: 0x16E61,
    0x16E42: 0x16E62,
    0x16E43: 0x16E63,
    0x16E44: 0x16E64,
    0x16E45: 0x16E65,
    0x16E46: 0x16E66,
    0x16E47: 0x16E67,
    0x16E48: 0x16E68,
    0x16E49: 0x16E69,
    0x16E4A: 0x16E6A,
    0x16E4B: 0x16E6B,
    0x16E4C: 0x16E6C,
    0x16E4D: 0x16E6D,
    0x16E4E: 0x16E6E,
    0x16E4F: 0x16E6F,
    0x16E50: 0x16E70,
    0x16E51: 0x16E71,
    0x16E52: 0x16E72,
    0x16E53: 0x16E73,
    0x16E54: 0x16E74,
    0x16E55: 0x16E75,
    0x16E56: 0x16E76,
    0x16E57: 0x16E77,
    0x16E58: 0x16E78,
    0x16E59: 0x16E79,
    0x16E5A: 0x16E7A,
    0x16E5B: 0x16E7B,
    0x16E5C: 0x16E7C,
    0x16E5D: 0x16E7D,
    0x16E5E: 0x16E7E,
    0x16E5F: 0x16E7F,
    0x1E900: 0x1E922,
    0x1E901: 0x1E923,
    0x1E902: 0x1E924,
    0x1E903: 0x1E925,
    0x1E904: 0x1E926,
    0x1E905: 0x1E927,
    0x1E906: 0x1E928,
    0x1E907: 0x1E929,
    0x1E908: 0x1E92A,
    0x1E909: 0x1E92B,
    0x1E90A: 0x1E92C,
    0x1E90B: 0x1E92D,
    0x1E90C: 0x1E92E,
    0x1E90D: 0x1E92F,
    0x1E90E: 0x1E930,
    0x1E90F: 0x1E931,
    0x1E910: 0x1E932,
    0x1E911: 0x1E933,
    0x1E912: 0x1E934,
    0x1E913: 0x1E935,
    0x1E914: 0x1E936,
    0x1E915: 0x1E937,
    0x1E916: 0x1E938,
    0x1E917: 0x1E939,
    0x1E918: 0x1E93A,
    0x1E919: 0x1E93B,
    0x1E91A: 0x1E93C,
    0x1E91B: 0x1E93D,
    0x1E91C: 0x1E93E,
    0x1E91D: 0x1E93F,
    0x1E91E: 0x1E940,
    0x1E91F: 0x1E941,
    0x1E920: 0x1E942,
    0x1E921: 0x1E943,
}

import pytest
import os
import sys
import re
import yaml
import glob
from itertools import chain
from operator import itemgetter
import regex
import gc

from ecmascript.ecmascript import (
    CreateAnnotatedFunctionObject,
    CreateFilledRealm,
    DefinePropertyOrThrow,
    DetachArrayBuffer,
    ESRangeError,
    ESReferenceError,
    ESSyntaxError,
    ESTypeError,
    Get,
    InitializeHostDefinedRealm,
    ObjectCreate,
    ParseScript,
    PropertyDescriptor,
    RunJobs,
    ScriptEvaluation,
    SetHostErrorCallback,
    surrounding_agent,
    ToString,
)

import snoop
from pprint import pprint

# if any(n.startswith("CIRCLE") for n in os.environ):
#    pytest.skip("Skipping Test-262 on Circle.CI", allow_module_level=True)
base_paths = tuple(
    p for p in ("/Users/scole/fun/test262", "/mnt/c/Users/scole/Documents/test262", "./test262") if os.path.exists(p)
)
if len(base_paths) != 1:
    pytest.skip("Skipping Test-262 because we don't know where the tests are", allow_module_level=True)


def add_to_global_object(realm):
    gl = realm.global_object

    def testprint(this_value, new_target, arg1=None, *_):
        print(ToString(arg1))  # we want something better in the future. But for now...

    testprint.length = 1
    testprint.name = "print"
    DefinePropertyOrThrow(
        gl,
        "print",
        PropertyDescriptor(
            value=CreateAnnotatedFunctionObject(realm, testprint), writable=True, enumerable=False, configurable=True
        ),
    )

    dollar262 = ObjectCreate(realm.intrinsics["%ObjectPrototype%"])

    def createRealm(this_value, new_target, *_):
        r = CreateFilledRealm(add_to_global_object)
        return Get(r.global_object, "$262")

    createRealm.length = 0
    createRealm.name = "createRealm"
    DefinePropertyOrThrow(
        dollar262,
        "createRealm",
        PropertyDescriptor(
            value=CreateAnnotatedFunctionObject(realm, createRealm),
            writable=True,
            enumerable=True,
            configurable=True,
        ),
    )

    def detachArrayBuffer(this_value, new_target, arrayBuffer=None, key=None, *_):
        return DetachArrayBuffer(arrayBuffer, key)

    detachArrayBuffer.length = 1
    detachArrayBuffer.name = "detachArrayBuffer"
    DefinePropertyOrThrow(
        dollar262,
        "detachArrayBuffer",
        PropertyDescriptor(
            value=CreateAnnotatedFunctionObject(realm, detachArrayBuffer),
            writable=True,
            enumerable=True,
            configurable=True,
        ),
    )

    def evalScript(this_value, new_target, sourceText=None, *_):
        st = ToString(sourceText)
        s = ParseScript(st, surrounding_agent.running_ec.realm, None)
        if isinstance(s, list):
            ctors = {
                "SyntaxError": ESSyntaxError,
                "TypeError": ESTypeError,
                "ReferenceError": ESReferenceError,
                "RangeError": ESRangeError,
            }
            err = s[0]
            ctor = ctors[Get(Get(err, "constructor"), "name")]
            raise ctor(err)
        return ScriptEvaluation(s)

    evalScript.length = 1
    evalScript.name = "evalScript"
    DefinePropertyOrThrow(
        dollar262,
        "evalScript",
        PropertyDescriptor(
            value=CreateAnnotatedFunctionObject(realm, evalScript), writable=True, enumerable=True, configurable=True
        ),
    )

    def collect_gc(this_value, new_target, *_):
        gc.collect()

    collect_gc.length = 0
    collect_gc.name = "gc"
    DefinePropertyOrThrow(
        dollar262,
        "gc",
        PropertyDescriptor(
            value=CreateAnnotatedFunctionObject(realm, collect_gc), writable=True, enumerable=True, configurable=True
        ),
    )

    DefinePropertyOrThrow(
        dollar262, "global", PropertyDescriptor(value=gl, writable=True, enumerable=True, configurable=True)
    )

    DefinePropertyOrThrow(
        dollar262, "agent", PropertyDescriptor(value=None, writable=True, enumerable=True, configurable=True)
    )

    DefinePropertyOrThrow(
        gl, "$262", PropertyDescriptor(value=dollar262, writable=True, enumerable=False, configurable=True)
    )


@pytest.fixture
def test262realm():
    InitializeHostDefinedRealm(add_to_global_object)
    yield surrounding_agent.running_ec.realm
    surrounding_agent.ec_stack = []
    surrounding_agent.running_ec = None


class test262_testcase:
    config_matcher = re.compile(r".*/\*---(.*)---\*/.*", re.MULTILINE | re.DOTALL)

    def __init__(self, filename, root):
        with open(filename, "r") as f:
            self.source = f.read()
        self.config = self.get_config()
        self.root = root

    def get_config(self):
        confyaml = self.config_matcher.match(self.source).group(1)
        conf = yaml.safe_load(confyaml)
        if "includes" not in conf:
            conf["includes"] = []
        if "flags" not in conf:
            conf["flags"] = []
        return conf

    def massaged(self, strict=False):
        result = ""
        if "raw" not in self.config["flags"]:
            if strict:
                result += '"use strict";\n'
            for file in chain(("assert.js", "sta.js"), self.config["includes"]):
                with open(os.path.join(self.root, "harness", file), "r") as f:
                    result += f.read() + "\n"
        result += self.source
        with open("test.js", "w") as f:
            print(result, file=f)
        return result

    def run_once(self, strict=False):
        massaged = self.massaged(strict)
        rv = RunJobs(scripts=[massaged], add_host_defined_globals=add_to_global_object)
        return rv


@pytest.fixture
def cleanup():
    yield None
    surrounding_agent.ec_stack = []
    surrounding_agent.running_ec = None


from pprint import pprint

lang_tests = (
    "arguments-object",
    "block-scope",
    "comments",
    "computed-property-names",
    "destructuring",
    "directive-prologue",
    "expressions",
    "function-code",
    "future-reserved-words",
    "global-code",
    "identifier-resolution",
    "identifiers",
    "keywords",
    "line-terminators",
    "literals",
    "punctuators",
    "reserved-words",
    "rest-parameters",
    "source-text",
    "statements",
    "types",
    "white-space",
)

passing = (
    # These paths have passed (or xfailed, or skipped) 100%. We shouldn't break them.
    "harness",
    "built-ins/Array/from",
    "built-ins/Array/prototype/concat",
    "built-ins/Array/prototype/forEach",
    "built-ins/Array/prototype/join",
    "built-ins/Array/prototype/map",
    "built-ins/Array/prototype/push",
    "built-ins/Array/prototype/reduce",
    "built-ins/Array/prototype/slice",
    "built-ins/Array/prototype/sort",
    "built-ins/Array/prototype/toString",
    "built-ins/Array/prototype/values",
    "built-ins/Boolean",
    "built-ins/Date",
    "built-ins/Error",
    "built-ins/Function",
    "built-ins/JSON",
    "built-ins/Map",
    "built-ins/Math",
    "built-ins/Object",
    "built-ins/RegExp",
    "built-ins/String",
    "built-ins/isFinite",
    "built-ins/isNaN",
    "built-ins/parseFloat",
    "built-ins/parseInt",
    "language/arguments-object",
    "language/asi",
    "language/block-scope",
    "language/comments",
    "language/computed-property-names",
    "language/destructuring",
    "language/directive-prologue",
    "language/eval-code",
    "language/identifier-resolution",
    "language/identifiers",
    "language/line-terminators",
    "language/literals/boolean",
    "language/literals/null",
    "language/literals/numeric",
    "language/literals/regexp",
    "language/literals/string",
    "language/statementList",
    "language/statements/block",
    "language/statements/break",
    "language/statements/const",
    "language/statements/continue",
    "language/statements/debugger",
    "language/statements/labeled",
    "language/types",
    "language/expressions/addition",
    "language/expressions/array",
    "language/expressions/assignment",
    "language/expressions/bitwise-and",
    "language/expressions/bitwise-not",
    "language/expressions/bitwise-or",
    "language/expressions/bitwise-xor",
    "language/expressions/call",
    "language/expressions/comma",
    "language/expressions/concatenation",
    "language/expressions/conditional",
    "language/expressions/delete",
    "language/expressions/division",
    "language/expressions/does-not-equals",
    "language/expressions/equals",
    "language/expressions/exponentiation",
    "language/expressions/greater-than",
    "language/expressions/greater-than-or-equal",
    "language/expressions/grouping",
    "language/expressions/in",
    "language/expressions/instanceof",
    "language/expressions/less-than",
    "language/expressions/less-than-or-equal",
    "language/expressions/logical-and",
    "language/expressions/logical-not",
    "language/expressions/logical-or",
    "language/expressions/multiplication",
    "language/expressions/new.target",
    "language/expressions/postfix-decrement",
    "language/expressions/postfix-increment",
    "language/expressions/prefix-decrement",
    "language/expressions/prefix-increment",
    "language/expressions/relational",
    "language/expressions/strict-does-not-equals",
    "language/expressions/strict-equals",
    "language/expressions/subtraction",
    "language/expressions/super",
    "language/expressions/void",
)

is_ci = any(n.startswith("CIRCLE") or n == "GITHUB_ACTIONS" for n in os.environ)

test_passing = True or is_ci
run_slow_tests = False and not is_ci
base_path = base_paths[0]
test_files = []
if test_passing:
    for tst in passing:
        if tst.endswith(".js"):
            test_files.append(f"{base_path}/test/{tst}")
        else:
            test_files.extend(glob.glob(f"{base_path}/test/{tst}/**/*.js", recursive=True))
# test_files.extend(glob.glob(f"{base_path}/test/harness/*.js"))
# for suite in lang_tests:
#     test_files.extend(glob.glob(f"{base_path}/test/language/{suite}/**/*.js", recursive=True))
# test_files.extend(glob.glob(f"{base_path}/test/built-ins/Array/**/*.js", recursive=True))
# test_files.extend(glob.glob(f"{base_path}/test/built-ins/Symbol/**/*.js", recursive=True)) # Needs Map
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/arrow-function/**/*.js", recursive=True)) # 109 tests failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/compound-assignment/**/*.js", recursive=True)) # 78 failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/class/**/*.js", recursive=True)) # 38 failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/function/**/*.js", recursive=True))  # 14 failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/left-shift/**/*.js", recursive=True))  # 8 failing (Hit python recursion limit)
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/modulus/**/*.js", recursive=True))  # 20 failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/new/**/*.js", recursive=True))  # 16 failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/object/**/*.js", recursive=True))  # 23 failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/property-accessors/**/*.js", recursive=True))  # 10 failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/right-shift/**/*.js", recursive=True)) #  8 failing (Hit python recursion limit)
for s in ():
    test_files.append(f"{base_path}{s}")
test_files = [fn for fn in test_files if not fn.endswith("_FIXTURE.js")]
test_files.sort()

features_to_avoid = (
    # won't ever support
    "caller",
    # don't support right now
    "async-iteration",
    "async-functions",
    "tail-call-optimization",
    "generators",
    "Proxy",
    # proposals not actually part of the spec yet
    "proxy-missing-checks",
    "Promise.allSettled",
    "hashbang",
    "Object.fromEntries",
    "BigInt",
    "class-fields-public",
    "class-fields-private",
    "class-static-fields-public",
    "class-static-fields-private",
    "class-static-methods-private",
    "class-methods-private",
    "dynamic-import",
    "Array.prototype.flat",
    "Array.prototype.flatMap",
    "numeric-separator-literal",
    "String.prototype.matchAll",
    "Symbol.matchAll",
    "Symbol.prototype.description",
    "well-formed-json-stringify",
    "Intl.ListFormat",
    "Intl.Locale",
    "Intl.RelativeTimeFormat",
    "Intl.Segmenter",
    "Intl.NumberFormat-unified",
    "Intl.DateTimeFormat-datetimestyle",
    "Intl.DateTimeFormat-formatRange",
    "Intl.DateTimeFormat-dayPeriod",
    "Intl.DateTimeFormat-quarter",
    "Intl.DateTimeFormat-fractionalSecondDigits",
    "globalThis",
    "export-star-as-namespace-from-module",
    "import.meta",
    "WeakRef",
    "FinalizationGroup",
    "optional-chaining",
    "top-level-await",
    "regexp-match-indices",
    "coalesce-expression",
    "Intl.DisplayNames",
    "Promise.any",
    "AggregateError",
    "String.prototype.replaceAll",
)

flags_to_avoid = ("module",)

slow_tests = (
    "/test/built-ins/parseFloat/S15.1.2.3_A6.js",
    "/test/built-ins/parseInt/S15.1.2.2_A8.js",
    "/test/language/comments/S7.4_A5.js",
    "/test/language/comments/S7.4_A6.js",
    "/test/built-ins/RegExp/CharacterClassEscapes/character-class-non-digit-class-escape-flags-u.js",
    "/test/built-ins/RegExp/CharacterClassEscapes/character-class-non-digit-class-escape-plus-quantifier-flags-u.js",
    "/test/built-ins/RegExp/CharacterClassEscapes/character-class-non-digit-class-escape-plus-quantifier.js",
    "/test/built-ins/RegExp/CharacterClassEscapes/character-class-non-digit-class-escape.js",
    "/test/built-ins/RegExp/CharacterClassEscapes/character-class-non-whitespace-class-escape-flags-u.js",
    "/test/built-ins/RegExp/CharacterClassEscapes/character-class-non-whitespace-class-escape-plus-quantifier-flags-u.js",
    "/test/built-ins/RegExp/CharacterClassEscapes/character-class-non-whitespace-class-escape-plus-quantifier.js",
    "/test/built-ins/RegExp/CharacterClassEscapes/character-class-non-whitespace-class-escape.js",
    "/test/built-ins/RegExp/CharacterClassEscapes/character-class-non-word-class-escape-flags-u.js",
    "/test/built-ins/RegExp/CharacterClassEscapes/character-class-non-word-class-escape-plus-quantifier-flags-u.js",
    "/test/built-ins/RegExp/CharacterClassEscapes/character-class-non-word-class-escape-plus-quantifier.js",
    "/test/built-ins/RegExp/CharacterClassEscapes/character-class-non-word-class-escape.js",
    "/test/built-ins/RegExp/property-escapes/generated/ASCII.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/ASCII_Hex_Digit.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Alphabetic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Any.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Assigned.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Bidi_Control.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Bidi_Mirrored.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Case_Ignorable.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Cased.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Changes_When_Casefolded.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Changes_When_Casemapped.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Changes_When_Lowercased.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Changes_When_NFKC_Casefolded.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Changes_When_Titlecased.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Changes_When_Uppercased.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Dash.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Default_Ignorable_Code_Point.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Deprecated.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Diacritic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Emoji.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Emoji_Component.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Emoji_Modifier.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Emoji_Modifier_Base.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Emoji_Presentation.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Extended_Pictographic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Extender.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Cased_Letter.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Close_Punctuation.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Connector_Punctuation.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Control.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Currency_Symbol.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Dash_Punctuation.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Decimal_Number.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Enclosing_Mark.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Final_Punctuation.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Format.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Initial_Punctuation.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Letter.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Letter_Number.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Line_Separator.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Lowercase_Letter.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Mark.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Math_Symbol.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Modifier_Letter.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Modifier_Symbol.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Nonspacing_Mark.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Number.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Open_Punctuation.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Other_Number.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Other_Punctuation.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Other_Symbol.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Paragraph_Separator.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Private_Use.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Punctuation.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Separator.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Space_Separator.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Spacing_Mark.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Surrogate.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Symbol.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Titlecase_Letter.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Unassigned.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Uppercase_Letter.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Grapheme_Base.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Grapheme_Extend.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Hex_Digit.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/IDS_Binary_Operator.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/IDS_Trinary_Operator.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/ID_Continue.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/ID_Start.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Ideographic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Join_Control.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Logical_Order_Exception.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Lowercase.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Math.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Noncharacter_Code_Point.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Pattern_Syntax.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Pattern_White_Space.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Quotation_Mark.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Radical.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Regional_Indicator.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Adlam.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Ahom.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Anatolian_Hieroglyphs.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Arabic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Armenian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Avestan.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Balinese.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Bamum.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Bassa_Vah.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Batak.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Bengali.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Bhaiksuki.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Bopomofo.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Brahmi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Braille.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Buginese.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Buhid.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Canadian_Aboriginal.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Carian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Caucasian_Albanian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Chakma.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Cham.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Cherokee.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Common.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Coptic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Cuneiform.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Cypriot.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Cyrillic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Deseret.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Devanagari.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Dogra.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Duployan.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Egyptian_Hieroglyphs.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Elbasan.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Elymaic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Ethiopic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Georgian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Glagolitic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Gothic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Grantha.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Greek.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Gujarati.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Gunjala_Gondi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Gurmukhi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Han.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Hangul.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Hanifi_Rohingya.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Hanunoo.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Hatran.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Hebrew.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Hiragana.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Imperial_Aramaic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Inherited.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Inscriptional_Pahlavi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Inscriptional_Parthian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Javanese.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Kaithi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Kannada.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Katakana.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Kayah_Li.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Kharoshthi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Khmer.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Khojki.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Khudawadi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Lao.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Latin.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Lepcha.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Limbu.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Linear_A.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Linear_B.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Lisu.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Lycian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Lydian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Mahajani.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Makasar.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Malayalam.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Mandaic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Manichaean.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Marchen.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Masaram_Gondi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Medefaidrin.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Meetei_Mayek.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Mende_Kikakui.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Meroitic_Cursive.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Meroitic_Hieroglyphs.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Miao.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Modi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Mongolian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Mro.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Multani.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Myanmar.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Nabataean.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Nandinagari.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_New_Tai_Lue.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Newa.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Nko.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Nushu.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Nyiakeng_Puachue_Hmong.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Ogham.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Ol_Chiki.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Old_Hungarian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Old_Italic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Old_North_Arabian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Old_Permic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Old_Persian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Old_Sogdian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Old_South_Arabian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Old_Turkic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Oriya.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Osage.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Osmanya.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Pahawh_Hmong.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Palmyrene.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Pau_Cin_Hau.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Phags_Pa.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Phoenician.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Psalter_Pahlavi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Rejang.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Runic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Samaritan.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Saurashtra.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Sharada.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Shavian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Siddham.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_SignWriting.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Sinhala.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Sogdian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Sora_Sompeng.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Soyombo.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Sundanese.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Syloti_Nagri.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Syriac.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Tagalog.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Tagbanwa.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Tai_Le.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Tai_Tham.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Tai_Viet.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Takri.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Tamil.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Tangut.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Telugu.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Thaana.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Thai.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Tibetan.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Tifinagh.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Tirhuta.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Ugaritic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Vai.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Wancho.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Warang_Citi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Yi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_-_Zanabazar_Square.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Adlam.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Ahom.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Anatolian_Hieroglyphs.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Arabic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Armenian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Avestan.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Balinese.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Bamum.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Bassa_Vah.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Batak.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Bengali.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Bhaiksuki.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Bopomofo.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Brahmi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Braille.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Buginese.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Buhid.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Canadian_Aboriginal.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Carian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Caucasian_Albanian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Chakma.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Cham.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Cherokee.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Common.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Coptic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Cuneiform.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Cypriot.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Cyrillic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Deseret.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Devanagari.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Dogra.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Duployan.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Egyptian_Hieroglyphs.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Elbasan.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Elymaic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Ethiopic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Georgian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Glagolitic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Gothic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Grantha.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Greek.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Gujarati.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Gunjala_Gondi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Gurmukhi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Han.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Hangul.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Hanifi_Rohingya.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Hanunoo.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Hatran.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Hebrew.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Hiragana.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Imperial_Aramaic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Inherited.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Inscriptional_Pahlavi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Inscriptional_Parthian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Javanese.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Kaithi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Kannada.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Katakana.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Kayah_Li.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Kharoshthi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Khmer.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Khojki.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Khudawadi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Lao.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Latin.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Lepcha.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Limbu.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Linear_A.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Linear_B.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Lisu.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Lycian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Lydian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Mahajani.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Makasar.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Malayalam.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Mandaic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Manichaean.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Marchen.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Masaram_Gondi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Medefaidrin.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Meetei_Mayek.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Mende_Kikakui.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Meroitic_Cursive.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Meroitic_Hieroglyphs.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Miao.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Modi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Mongolian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Mro.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Multani.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Myanmar.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Nabataean.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Nandinagari.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_New_Tai_Lue.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Newa.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Nko.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Nushu.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Nyiakeng_Puachue_Hmong.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Ogham.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Ol_Chiki.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Old_Hungarian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Old_Italic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Old_North_Arabian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Old_Permic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Old_Persian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Old_Sogdian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Old_South_Arabian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Old_Turkic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Oriya.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Osage.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Osmanya.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Pahawh_Hmong.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Palmyrene.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Pau_Cin_Hau.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Phags_Pa.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Phoenician.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Psalter_Pahlavi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Rejang.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Runic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Samaritan.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Saurashtra.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Sharada.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Shavian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Siddham.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_SignWriting.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Sinhala.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Sogdian.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Sora_Sompeng.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Soyombo.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Sundanese.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Syloti_Nagri.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Syriac.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Tagalog.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Tagbanwa.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Tai_Le.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Tai_Tham.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Tai_Viet.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Takri.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Tamil.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Tangut.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Telugu.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Thaana.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Thai.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Tibetan.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Tifinagh.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Tirhuta.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Ugaritic.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Vai.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Wancho.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Warang_Citi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Yi.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Script_Extensions_-_Zanabazar_Square.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Sentence_Terminal.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Soft_Dotted.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Terminal_Punctuation.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Unified_Ideograph.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Uppercase.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/Variation_Selector.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/White_Space.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/XID_Continue.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/generated/XID_Start.js",  # Not checked
)

skip_tests = (
    # Flaky tests that pass or fail, depending on things out of our control, like OS or geography
    "/test/built-ins/Date/S15.9.2.1_A2.js",  # Something to do with localtime
    "/test/built-ins/Date/parse/without-utc-offset.js",  # Needs time zone handling
    "/test/built-ins/Date/S15.9.3.1_A5_T1.js",  # Not sure
    "/test/built-ins/Date/S15.9.3.1_A5_T2.js",  # Not sure
    "/test/built-ins/Date/S15.9.3.1_A5_T3.js",  # Not sure
    "/test/built-ins/Date/S15.9.3.1_A5_T4.js",  # Not sure
    "/test/built-ins/Date/S15.9.3.1_A5_T5.js",  # Not sure
    "/test/built-ins/Date/S15.9.3.1_A5_T6.js",  # Not sure
)

xfail_tests = (
    "/test/harness/deepEqual-mapset.js",  # Needs Set
    "/test/harness/timer.js",  # Needs Promise
    "/test/built-ins/Array/from/elements-deleted-after.js",  # Needs Array.prototype.splice
    "/test/built-ins/Array/prototype/concat/Array.prototype.concat_large-typed-array.js",  # Needs TypedArrays to have a working length
    "/test/built-ins/Array/prototype/concat/Array.prototype.concat_small-typed-array.js",  # Needs TypedArrays to have a working length
    "/test/built-ins/Array/prototype/concat/create-proto-from-ctor-realm-array.js",  # Alternate Realm stuff
    "/test/built-ins/Array/prototype/map/create-proto-from-ctor-realm-array.js",  # Alternate Realm stuff
    "/test/built-ins/Array/prototype/slice/create-proto-from-ctor-realm-array.js",  # Alternate Realm stuff
    "/test/built-ins/Array/prototype/slice/length-exceeding-integer-limit-proxied-array.js",  # Needs Proxy
    "/test/built-ins/Array/prototype/sort/stability-2048-elements.js",  # Recursion Limit
    "/test/built-ins/Array/prototype/sort/stability-513-elements.js",  # Recursion Limit
    "/test/built-ins/Date/parse/zero.js",  # Wants a bit more flexible Date.parse
    "/test/built-ins/Date/prototype/getTimezoneOffset/this-value-valid-date.js",  # Overflow in LocalTZA
    "/test/built-ins/Date/prototype/setDate/new-value-time-clip.js",  # Overflow in LocalTZA
    "/test/built-ins/Date/prototype/setFullYear/arg-year-to-number.js",  # Overflow in LocalTZA
    "/test/built-ins/Date/prototype/setFullYear/new-value-time-clip.js",  # Overflow in LocalTZA
    "/test/built-ins/Date/prototype/setHours/new-value-time-clip.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/setHours/this-value-invalid-date.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/setMilliseconds/new-value-time-clip.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/setMilliseconds/this-value-invalid-date.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/setMinutes/new-value-time-clip.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/setMinutes/this-value-invalid-date.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/setMonth/new-value-time-clip.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/setMonth/this-value-invalid-date.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/setSeconds/new-value-time-clip.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/setSeconds/this-value-invalid-date.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/toDateString/negative-year.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/toISOString/15.9.5.43-0-10.js",  # Out of range for python dates
    "/test/built-ins/Date/prototype/toISOString/15.9.5.43-0-11.js",  # Out of range for python dates
    "/test/built-ins/Date/prototype/toISOString/15.9.5.43-0-12.js",  # Out of range for python dates
    "/test/built-ins/Date/prototype/toISOString/15.9.5.43-0-8.js",  # Out of range for python dates
    "/test/built-ins/Date/prototype/toISOString/15.9.5.43-0-9.js",  # Out of range for python dates
    "/test/built-ins/Date/prototype/toLocaleString/name.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/toString/negative-year.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/toUTCString/negative-year.js",  # Failed test run; error not investigated
    "/test/built-ins/Function/prototype/toString/built-in-function-object.js",  # Needs Generators
    "/test/built-ins/Function/prototype/toString/generator-function-expression.js",  # Needs Generators
    "/test/built-ins/Function/prototype/toString/method-computed-property-name.js",  # Needs better Function.prototype.toString
    "/test/built-ins/Function/prototype/toString/S15.3.4.2_A12.js",  # Needs better Function.prototype.toString
    "/test/built-ins/Function/prototype/toString/S15.3.4.2_A13.js",  # Needs better Function.prototype.toString
    "/test/built-ins/Function/prototype/toString/S15.3.4.2_A14.js",  # Needs better Function.prototype.toString
    "/test/built-ins/Function/prototype/toString/S15.3.4.2_A16.js",  # Needs better Function.prototype.toString
    "/test/built-ins/Function/prototype/toString/well-known-intrinsic-object-functions.js",  # Needs Generators
    "/test/built-ins/JSON/stringify/value-string-escape-ascii.js",  # Needs Array.prototype.reverse
    "/test/built-ins/Map/prototype/clear/context-is-set-object-throws.js",  # Needs Set
    "/test/built-ins/Map/prototype/clear/context-is-weakmap-object-throws.js",  # Needs WeakMap
    "/test/built-ins/Map/prototype/delete/context-is-set-object-throws.js",  # Needs Set
    "/test/built-ins/Map/prototype/delete/context-is-weakmap-object-throws.js",  # Needs WeakMap
    "/test/built-ins/Map/prototype/entries/does-not-have-mapdata-internal-slot-set.js",  # Needs Set
    "/test/built-ins/Map/prototype/entries/does-not-have-mapdata-internal-slot-weakmap.js",  # Needs WeakMap
    "/test/built-ins/Map/prototype/forEach/does-not-have-mapdata-internal-slot-set.js",  # Needs Set
    "/test/built-ins/Map/prototype/forEach/does-not-have-mapdata-internal-slot-weakmap.js",  # Needs WeakMap
    "/test/built-ins/Map/prototype/get/does-not-have-mapdata-internal-slot-set.js",  # Needs Set
    "/test/built-ins/Map/prototype/get/does-not-have-mapdata-internal-slot-weakmap.js",  # Needs WeakMap
    "/test/built-ins/Map/prototype/has/does-not-have-mapdata-internal-slot-set.js",  # Needs Set
    "/test/built-ins/Map/prototype/has/does-not-have-mapdata-internal-slot-weakmap.js",  # Needs WeakMap
    "/test/built-ins/Map/prototype/keys/does-not-have-mapdata-internal-slot-set.js",  # Needs Set
    "/test/built-ins/Map/prototype/keys/does-not-have-mapdata-internal-slot-weakmap.js",  # Needs WeakMap
    "/test/built-ins/Map/prototype/set/append-new-values.js",  # Needs Array.prototype.pop
    "/test/built-ins/Map/prototype/set/does-not-have-mapdata-internal-slot-set.js",  # Needs Set
    "/test/built-ins/Map/prototype/set/does-not-have-mapdata-internal-slot-weakmap.js",  # Needs WeakMap
    "/test/built-ins/Map/prototype/size/does-not-have-mapdata-internal-slot-set.js",  # Needs Set
    "/test/built-ins/Map/prototype/size/does-not-have-mapdata-internal-slot-weakmap.js",  # Needs WeakMap
    "/test/built-ins/Map/prototype/values/does-not-have-mapdata-internal-slot-set.js",  # Needs Set
    "/test/built-ins/Map/prototype/values/does-not-have-mapdata-internal-slot-weakmap.js",  # Needs WeakMap
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-612.js",  # Needs Array.prototype.indexOf
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-613.js",  # Needs Array.prototype.lastIndexOf
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-614.js",  # Needs Array.prototype.every
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-615.js",  # Needs Array.prototype.some
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-618.js",  # Needs Array.prototype.filter
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-620.js",  # Needs Array.prototype.reduceRight
    "/test/built-ins/Object/freeze/throws-when-false.js",  # Needs Proxy
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-10.js",  # Needs decodeURIComponent
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-11.js",  # Needs encodeURIComponent
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-42.js",  # Needs Array.prototype.reverse
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-47.js",  # Needs Array.prototype.pop
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-48.js",  # Needs Array.prototype.shift
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-49.js",  # Needs Array.prototype.unshift
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-50.js",  # Needs Array.prototype.splice
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-51.js",  # Needs Array.prototype.toLocaleString
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-52.js",  # Needs Array.prototype.indexOf
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-53.js",  # Needs Array.prototype.lastIndexOf
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-54.js",  # Needs Array.prototype.every
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-55.js",  # Needs Array.prototype.some
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-58.js",  # Needs Array.prototype.filter
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-60.js",  # Needs Array.prototype.reduceRight
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-9.js",  # Needs decodeURI
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-90.js",  # Needs Number.prototype.toLocaleString
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-91.js",  # Needs Number.prototype.toFixed
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-92.js",  # Needs Number.prototype.toExponential
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-93.js",  # Needs Number.prototype.toPrecision
    "/test/built-ins/Object/getOwnPropertyNames/15.2.3.4-4-1.js",  # Needs all the globals
    "/test/built-ins/Object/keys/proxy-keys.js",  # Needs Proxy
    "/test/built-ins/Object/preventExtensions/throws-when-false.js",  # Needs Proxy
    "/test/built-ins/Object/seal/throws-when-false.js",  # Needs Proxy
    "/test/built-ins/RegExp/S15.10.2.8_A3_T15.js",  # Recursion Limit
    "/test/built-ins/RegExp/S15.10.2.8_A3_T16.js",  # Recursion Limit
    "/test/built-ins/RegExp/S15.10.2.8_A3_T18.js",  # Recursion Limit
    "/test/built-ins/RegExp/S15.10.2_A1_T1.js",  # Recursion Limit
    "/test/built-ins/RegExp/character-class-escape-non-whitespace.js",  # Needs Unicode class tables
    "/test/built-ins/RegExp/lookBehind/sliced-strings.js",  # Needs String.prototype.substr
    "/test/built-ins/RegExp/property-escapes/character-class.js",  # Needs Unicode class tables
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Other.js",  # Recursion Limit
    "/test/built-ins/RegExp/property-escapes/generated/General_Category_-_Other_Letter.js",  # Recursion Limit
    "/test/built-ins/String/prototype/indexOf/S15.5.4.7_A1_T12.js",  # Needs Array.prototype.indexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A1_T12.js",  # Needs Array.prototype.lastIndexOf
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A3_T1.js",  # Needs Regex Character Class Escape
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A3_T2.js",  # Needs Regex Character Class Escape
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A3_T3.js",  # Needs Regex Character Class Escape
    "/test/built-ins/String/prototype/Symbol.iterator/length.js",  # Needs String.prototype.@@iterator
    "/test/built-ins/String/prototype/Symbol.iterator/name.js",  # Needs String.prototype.@@iterator
    "/test/built-ins/String/prototype/Symbol.iterator/prop-desc.js",  # Needs String.prototype.@@iterator
    "/test/built-ins/String/prototype/Symbol.iterator/this-val-to-str-err.js",  # Needs String.prototype.@@iterator
    "/test/language/computed-property-names/class/static/method-number.js",  # Wants default anonymous function naming
    "/test/language/computed-property-names/class/static/method-string.js",  # Wants default anonymous function naming
    "/test/language/computed-property-names/class/static/method-symbol.js",  # Wants default anonymous function naming
    "/test/language/expressions/assignment/dstr/array-elem-nested-array-invalid.js",  # Something is Wrong
    "/test/language/expressions/assignment/dstr/array-elem-nested-obj-invalid.js",  # Something is Wrong
    "/test/language/expressions/assignment/dstr/array-elem-nested-obj-yield-ident-valid.js",  # ???
    "/test/language/expressions/assignment/dstr/array-elem-target-simple-strict.js",  # An early error is failing
    "/test/language/expressions/assignment/dstr/array-elision-val-string.js",  # ???
    "/test/language/expressions/assignment/dstr/array-empty-val-string.js",  # ???
    "/test/language/expressions/assignment/dstr/array-rest-nested-array-invalid.js",  # ???
    "/test/language/expressions/assignment/dstr/array-rest-nested-obj-invalid.js",  # ???
    "/test/language/expressions/assignment/dstr/array-rest-nested-obj-yield-ident-valid.js",  # ???
    "/test/language/expressions/assignment/dstr/obj-id-init-assignment-missing.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-id-init-assignment-null.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-id-init-assignment-truthy.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-id-init-assignment-undef.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-id-init-evaluation.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-id-init-fn-name-arrow.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-id-init-fn-name-class.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-id-init-fn-name-cover.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-id-init-fn-name-fn.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-id-init-in.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-id-init-let.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-id-init-order.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-id-init-simple-no-strict.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-id-init-yield-ident-valid.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-id-simple-strict.js",  # ???
    "/test/language/expressions/assignment/dstr/obj-prop-nested-array-invalid.js",  # ???
    "/test/language/expressions/assignment/dstr/obj-prop-nested-array-null.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-prop-nested-array-undefined-own.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-prop-nested-array-undefined.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-prop-nested-array-yield-ident-valid.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-prop-nested-array.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-prop-nested-obj-invalid.js",  # ???
    "/test/language/expressions/assignment/dstr/obj-prop-nested-obj-null.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-prop-nested-obj-undefined-own.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-prop-nested-obj-undefined.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/obj-prop-nested-obj-yield-ident-valid.js",  # ???
    "/test/language/expressions/assignment/dstr/obj-prop-nested-obj.js",  # Fix pending
    "/test/language/expressions/assignment/fn-name-arrow.js",  # Fix pending
    "/test/language/expressions/assignment/fn-name-class.js",  # Fix pending
    "/test/language/expressions/assignment/fn-name-cover.js",  # Fix pending
    "/test/language/expressions/assignment/fn-name-fn.js",  # Fix pending
    "/test/language/expressions/assignment/fn-name-lhs-cover.js",  # Wants default anonymous function naming
    "/test/language/expressions/assignment/fn-name-lhs-member.js",  # Wants default anonymous function naming
    "/test/built-ins/RegExp/named-groups/groups-object-subclass-sans.js",  # Not checked
    "/test/built-ins/RegExp/named-groups/groups-object-subclass.js",  # Not checked
    "/test/built-ins/RegExp/named-groups/string-replace-nocaptures.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/grammar-extension-empty-negated.js",  # Not checked
    "/test/built-ins/RegExp/property-escapes/grammar-extension-empty.js",  # Not checked
    "/test/built-ins/RegExp/prototype/Symbol.match/exec-return-type-invalid.js",  # Not checked
    "/test/built-ins/RegExp/prototype/Symbol.replace/result-coerce-capture.js",  # Not checked
    "/test/built-ins/RegExp/prototype/Symbol.replace/result-coerce-matched.js",  # Not checked
    "/test/built-ins/RegExp/prototype/Symbol.replace/subst-after.js",  # Not checked
    "/test/built-ins/RegExp/prototype/Symbol.replace/subst-before.js",  # Not checked
    "/test/built-ins/RegExp/prototype/Symbol.replace/subst-capture-idx-1.js",  # Not checked
    "/test/built-ins/RegExp/prototype/Symbol.replace/subst-capture-idx-2.js",  # Not checked
    "/test/built-ins/RegExp/prototype/Symbol.replace/subst-matched.js",  # Not checked
    "/test/built-ins/RegExp/prototype/Symbol.search/cstm-exec-return-invalid.js",  # Not checked
    "/test/built-ins/RegExp/prototype/source/value-empty.js",  # Not checked
    "/test/built-ins/RegExp/prototype/source/value-line-terminator.js",  # Not checked
    "/test/built-ins/RegExp/prototype/source/value-slash.js",  # Not checked
    "/test/built-ins/RegExp/unicode_identity_escape.js",  # Not checked
    "/test/built-ins/RegExp/unicode_restricted_identity_escape.js",  # Not checked
    "/test/built-ins/RegExp/unicode_restricted_identity_escape_alpha.js",  # Not checked
    "/test/built-ins/RegExp/unicode_restricted_identity_escape_c.js",  # Not checked
    "/test/built-ins/RegExp/unicode_restricted_identity_escape_u.js",  # Not checked
    "/test/built-ins/RegExp/unicode_restricted_identity_escape_x.js",  # Not checked
    "/test/built-ins/RegExp/unicode_restricted_octal_escape.js",  # Not checked
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A3_T1.js",  # Not checked
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A3_T2.js",  # Not checked
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A3_T3.js",  # Not checked
    "/test/language/literals/regexp/S7.8.5_A1.1_T2.js",  # Not checked
    "/test/language/literals/regexp/S7.8.5_A1.4_T2.js",  # Not checked
    "/test/language/literals/regexp/S7.8.5_A2.1_T2.js",  # Not checked
    "/test/language/literals/regexp/S7.8.5_A2.4_T2.js",  # Not checked
    "/test/language/literals/regexp/named-groups/invalid-incomplete-groupname-2.js",  # Not checked
    "/test/language/literals/regexp/named-groups/invalid-incomplete-groupname-3.js",  # Not checked
    "/test/language/literals/regexp/named-groups/invalid-incomplete-groupname-4.js",  # Not checked
    "/test/language/literals/regexp/named-groups/invalid-incomplete-groupname-5.js",  # Not checked
    "/test/language/literals/regexp/named-groups/invalid-incomplete-groupname-6.js",  # Not checked
    "/test/language/literals/regexp/named-groups/invalid-incomplete-groupname.js",  # Not checked
)


def should_skip(tc, file_id):
    if file_id in skip_tests:
        return True
    if not run_slow_tests and file_id in slow_tests:
        return True
    test_features = tc.config.get("features", [])
    if any(feature in test_features for feature in features_to_avoid):
        return True
    test_flags = tc.config.get("flags", [])
    if any(flag in test_flags for flag in flags_to_avoid):
        return True
    return False


def should_xfail(file_id):
    return file_id in xfail_tests


params = []
for tf in test_files:
    tc = test262_testcase(tf, base_path)
    file_id = tf[len(base_path) :] if tf.startswith(base_path) else tf
    skipit = should_skip(tc, file_id)
    for strict in (
        val
        for val, _ in filter(
            itemgetter(1),
            (
                (False, "onlyStrict" not in tc.config["flags"]),
                (True, all(x not in tc.config["flags"] for x in ("noStrict", "raw"))),
            ),
        )
    ):
        params.append(
            pytest.param(
                tc,
                strict,
                tf,
                id=f"{file_id}: {tc.config['description'].strip().splitlines()[0].strip()} [{'strict' if strict else 'loose'}]",
                marks=pytest.mark.skip if skipit else (pytest.mark.xfail if should_xfail(file_id) else ()),
            )
        )

errtype_pattern = regex.compile(r"(?P<name>[^:]*).*")


@pytest.mark.parametrize("tc, strict, testfile", params)
def test_test262(cleanup, tc, strict, testfile):
    class x:
        def __init__(self):
            self.errors = []

    execution_errors = x()

    def gen_error_reporter(execution_errors):
        def error_reporter(lst):
            execution_errors.errors = [ToString(err) for err in lst]

        return error_reporter

    SetHostErrorCallback(gen_error_reporter(execution_errors))

    print("=========================================================================")
    print(f"Running test {testfile}:")
    print(f"    Description: {tc.config['description'].strip()}")
    print(f"    Strict: {strict}")
    if "info" in tc.config:
        print(f"{tc.config['info'].strip()}")
    print("=========================================================================")
    tc.run_once(strict=strict)
    if "negative" in tc.config:
        assert (
            len(execution_errors.errors) != 0
        ), f"Expected a {tc.config['negative']['type']} error, completed successfully instead"
        names = tuple(errtype_pattern.match(err).group("name") for err in execution_errors.errors)
        assert (
            tc.config["negative"]["type"] in names
        ), f"Expected a {tc.config['negative']['type']} error, got '{', '.join(names)}' instead"
    else:
        assert len(execution_errors.errors) == 0, "\n".join(execution_errors.errors)

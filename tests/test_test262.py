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
    "built-ins/Array/prototype/concat",
    "built-ins/Array/prototype/forEach",
    "built-ins/Array/prototype/join",
    "built-ins/Array/prototype/map",
    "built-ins/Array/prototype/push",
    "built-ins/Array/prototype/reduce",
    "built-ins/Array/prototype/slice",
    "built-ins/Array/prototype/toString",
    "built-ins/Array/prototype/values",
    "built-ins/Boolean",
    "built-ins/Date",
    "built-ins/Error",
    "built-ins/Function",
    "built-ins/JSON",
    "built-ins/Math",
    "built-ins/Object",
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
    "language/literals/string",
    "language/literas/numeric",
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

test_passing = False or is_ci
run_slow_tests = True and not is_ci
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
    "string-trimming",
    "String.prototype.trimEnd",
    "String.prototype.trimStart",
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
)

skip_tests = (
    # Flaky tests that pass or fail, depending on things out of our control, like OS or geography
    "/test/built-ins/Date/S15.9.2.1_A2.js",  # Something to do with localtime
    "/test/built-ins/Date/parse/without-utc-offset.js",  # Needs time zone handling
)

xfail_tests = (
    "/test/harness/deepEqual-array.js",  # Needs Map
    "/test/harness/deepEqual-circular.js",  # Needs Map
    "/test/harness/deepEqual-deep.js",  # Needs Map
    "/test/harness/deepEqual-mapset.js",  # Needs Set
    "/test/harness/deepEqual-object.js",  # Needs Map
    "/test/harness/deepEqual-primitives.js",  # Needs Map
    "/test/harness/timer.js",  # Needs Promise
    "/test/built-ins/Array/prototype/concat/Array.prototype.concat_large-typed-array.js",  # Needs TypedArrays to have a working length
    "/test/built-ins/Array/prototype/concat/Array.prototype.concat_small-typed-array.js",  # Needs TypedArrays to have a working length
    "/test/built-ins/Array/prototype/concat/create-proto-from-ctor-realm-array.js",  # Alternate Realm stuff
    "/test/built-ins/Array/prototype/map/create-proto-from-ctor-realm-array.js",  # Alternate Realm stuff
    "/test/built-ins/Array/prototype/slice/create-proto-from-ctor-realm-array.js",  # Alternate Realm stuff
    "/test/built-ins/Array/prototype/slice/length-exceeding-integer-limit-proxied-array.js",  # Needs Proxy
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
    "/test/built-ins/Date/prototype/toDateString/format.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/toDateString/negative-year.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/toISOString/15.9.5.43-0-10.js",  # Out of range for python dates
    "/test/built-ins/Date/prototype/toISOString/15.9.5.43-0-11.js",  # Out of range for python dates
    "/test/built-ins/Date/prototype/toISOString/15.9.5.43-0-12.js",  # Out of range for python dates
    "/test/built-ins/Date/prototype/toISOString/15.9.5.43-0-8.js",  # Out of range for python dates
    "/test/built-ins/Date/prototype/toISOString/15.9.5.43-0-9.js",  # Out of range for python dates
    "/test/built-ins/Date/prototype/toLocaleString/name.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/toString/format.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/toString/negative-year.js",  # Failed test run; error not investigated
    "/test/built-ins/Date/prototype/toTimeString/format.js",  # Failed test run; error not investigated
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
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-612.js",  # Needs Array.prototype.indexOf
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-613.js",  # Needs Array.prototype.lastIndexOf
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-614.js",  # Needs Array.prototype.every
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-615.js",  # Needs Array.prototype.some
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-618.js",  # Needs Array.prototype.filter
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-620.js",  # Needs Array.prototype.reduceRight
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-621.js",  # Needs String.prototype.trim
    "/test/built-ins/Object/freeze/throws-when-false.js",  # Needs Proxy
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-10.js",  # Needs decodeURIComponent
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-11.js",  # Needs encodeURIComponent
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-42.js",  # Needs Array.prototype.reverse
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-44.js",  # Needs Array.prototype.sort
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
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-67.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-68.js",  # Needs String.prototype.match
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-69.js",  # Needs String.prototype.replace
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-70.js",  # Needs String.prototype.search
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-77.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-79.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-80.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-81.js",  # Needs String.prototype.localeCompare
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-82.js",  # Needs String.prototype.trim
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-9.js",  # Needs decodeURI
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-90.js",  # Needs Number.prototype.toLocaleString
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-91.js",  # Needs Number.prototype.toFixed
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-92.js",  # Needs Number.prototype.toExponential
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-93.js",  # Needs Number.prototype.toPrecision
    "/test/built-ins/Object/getOwnPropertyNames/15.2.3.4-4-1.js",  # Needs all the globals
    "/test/built-ins/Object/keys/proxy-keys.js",  # Needs Proxy
    "/test/built-ins/Object/preventExtensions/throws-when-false.js",  # Needs Proxy
    "/test/built-ins/Object/prototype/toString/Object.prototype.toString.call-regexp.js",  # Needs functional regex
    "/test/built-ins/Object/prototype/toString/symbol-tag-override-instances.js",  # Needs functional regex
    "/test/built-ins/Object/seal/throws-when-false.js",  # Needs Proxy
    "/test/built-ins/String/prototype/indexOf/S15.5.4.7_A1_T12.js",  # Needs Array.prototype.indexOf
    "/test/built-ins/String/prototype/lastIndexOf/name.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A1_T1.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A1_T10.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A1_T12.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A1_T2.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A1_T4.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A1_T5.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A1_T6.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A1_T7.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A1_T8.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A1_T9.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A10.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A11.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A4_T1.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A4_T2.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A4_T3.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A4_T4.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A4_T5.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A6.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A8.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/S15.5.4.8_A9.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/lastIndexOf/this-value-not-obj-coercible.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/String/prototype/localeCompare/15.5.4.9_3.js",  # Needs String.prototype.localeCompare
    "/test/built-ins/String/prototype/localeCompare/15.5.4.9_CE.js",  # Needs String.prototype.charCodeAt
    "/test/built-ins/String/prototype/localeCompare/15.5.4.9_CE.js",  # Needs String.prototype.localeCompare
    "/test/built-ins/String/prototype/localeCompare/name.js",  # Needs String.prototype.localeCompare
    "/test/built-ins/String/prototype/localeCompare/S15.5.4.9_A1_T1.js",  # Needs String.prototype.localeCompare
    "/test/built-ins/String/prototype/localeCompare/S15.5.4.9_A1_T2.js",  # Needs String.prototype.localeCompare
    "/test/built-ins/String/prototype/localeCompare/S15.5.4.9_A10.js",  # Needs String.prototype.localeCompare
    "/test/built-ins/String/prototype/localeCompare/S15.5.4.9_A11.js",  # Needs String.prototype.localeCompare
    "/test/built-ins/String/prototype/localeCompare/S15.5.4.9_A6.js",  # Needs String.prototype.localeCompare
    "/test/built-ins/String/prototype/localeCompare/S15.5.4.9_A8.js",  # Needs String.prototype.localeCompare
    "/test/built-ins/String/prototype/localeCompare/S15.5.4.9_A9.js",  # Needs String.prototype.localeCompare
    "/test/built-ins/String/prototype/localeCompare/this-value-not-obj-coercible.js",  # Needs String.prototype.localeCompare
    "/test/built-ins/String/prototype/match/cstm-matcher-get-err.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/cstm-matcher-invocation.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/invoke-builtin-match.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/length.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/name.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A1_T10.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A1_T11.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A1_T12.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A1_T13.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A1_T14.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A1_T3.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A1_T4.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A1_T5.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A1_T6.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A1_T7.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A1_T8.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A1_T9.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T1.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T10.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T11.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T12.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T13.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T14.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T15.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T16.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T17.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T18.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T2.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T3.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T4.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T5.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T6.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T7.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T8.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A2_T9.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A6.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A8.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/S15.5.4.10_A9.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/this-val-bool.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/this-val-obj.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/match/this-value-not-obj-coercible.js",  # Needs String.prototype.match
    "/test/built-ins/String/prototype/normalize/form-is-not-valid-throws.js",  # Needs String.prototype.normalize
    "/test/built-ins/String/prototype/normalize/length.js",  # Needs String.prototype.normalize
    "/test/built-ins/String/prototype/normalize/name.js",  # Needs String.prototype.normalize
    "/test/built-ins/String/prototype/normalize/normalize.js",  # Needs String.prototype.normalize
    "/test/built-ins/String/prototype/normalize/return-abrupt-from-form.js",  # Needs String.prototype.normalize
    "/test/built-ins/String/prototype/normalize/return-abrupt-from-this.js",  # Needs String.prototype.normalize
    "/test/built-ins/String/prototype/normalize/return-normalized-string-from-coerced-form.js",  # Needs String.prototype.normalize
    "/test/built-ins/String/prototype/normalize/return-normalized-string-using-default-parameter.js",  # Needs String.prototype.normalize
    "/test/built-ins/String/prototype/normalize/return-normalized-string.js",  # Needs String.prototype.normalize
    "/test/built-ins/String/prototype/padEnd/exception-not-object-coercible.js",  # Needs String.prototype.padEnd
    "/test/built-ins/String/prototype/padEnd/fill-string-empty.js",  # Needs String.prototype.padEnd
    "/test/built-ins/String/prototype/padEnd/fill-string-non-strings.js",  # Needs String.prototype.padEnd
    "/test/built-ins/String/prototype/padEnd/fill-string-omitted.js",  # Needs String.prototype.padEnd
    "/test/built-ins/String/prototype/padEnd/function-length.js",  # Needs String.prototype.padEnd
    "/test/built-ins/String/prototype/padEnd/function-name.js",  # Needs String.prototype.padEnd
    "/test/built-ins/String/prototype/padEnd/function-property-descriptor.js",  # Needs String.prototype.padEnd
    "/test/built-ins/String/prototype/padEnd/max-length-not-greater-than-string.js",  # Needs String.prototype.padEnd
    "/test/built-ins/String/prototype/padEnd/normal-operation.js",  # Needs String.prototype.padEnd
    "/test/built-ins/String/prototype/padEnd/observable-operations.js",  # Needs String.prototype.padEnd
    "/test/built-ins/String/prototype/padStart/exception-not-object-coercible.js",  # Needs String.prototype.padStart
    "/test/built-ins/String/prototype/padStart/fill-string-empty.js",  # Needs String.prototype.padStart
    "/test/built-ins/String/prototype/padStart/fill-string-non-strings.js",  # Needs String.prototype.padStart
    "/test/built-ins/String/prototype/padStart/fill-string-omitted.js",  # Needs String.prototype.padStart
    "/test/built-ins/String/prototype/padStart/function-length.js",  # Needs String.prototype.padStart
    "/test/built-ins/String/prototype/padStart/function-name.js",  # Needs String.prototype.padStart
    "/test/built-ins/String/prototype/padStart/function-property-descriptor.js",  # Needs String.prototype.padStart
    "/test/built-ins/String/prototype/padStart/max-length-not-greater-than-string.js",  # Needs String.prototype.padStart
    "/test/built-ins/String/prototype/padStart/normal-operation.js",  # Needs String.prototype.padStart
    "/test/built-ins/String/prototype/padStart/observable-operations.js",  # Needs String.prototype.padStart
    "/test/built-ins/String/prototype/repeat/count-coerced-to-zero-returns-empty-string.js",  # Needs String.prototype.repeat
    "/test/built-ins/String/prototype/repeat/count-is-infinity-throws.js",  # Needs String.prototype.repeat
    "/test/built-ins/String/prototype/repeat/count-is-zero-returns-empty-string.js",  # Needs String.prototype.repeat
    "/test/built-ins/String/prototype/repeat/count-less-than-zero-throws.js",  # Needs String.prototype.repeat
    "/test/built-ins/String/prototype/repeat/empty-string-returns-empty.js",  # Needs String.prototype.repeat
    "/test/built-ins/String/prototype/repeat/length.js",  # Needs String.prototype.repeat
    "/test/built-ins/String/prototype/repeat/name.js",  # Needs String.prototype.repeat
    "/test/built-ins/String/prototype/repeat/repeat-string-n-times.js",  # Needs String.prototype.repeat
    "/test/built-ins/String/prototype/repeat/repeat.js",  # Needs String.prototype.repeat
    "/test/built-ins/String/prototype/repeat/return-abrupt-from-count.js",  # Needs String.prototype.repeat
    "/test/built-ins/String/prototype/repeat/return-abrupt-from-this.js",  # Needs String.prototype.repeat
    "/test/built-ins/String/prototype/replace/15.5.4.11-1.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/cstm-replace-get-err.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/cstm-replace-invocation.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/length.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/name.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/replaceValue-evaluation-order.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A1_T1.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A1_T10.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A1_T11.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A1_T12.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A1_T13.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A1_T14.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A1_T17.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A1_T2.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A1_T4.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A1_T5.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A1_T6.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A1_T7.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A1_T8.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A1_T9.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A12.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A2_T1.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A2_T10.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A2_T2.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A2_T3.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A2_T4.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A2_T5.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A2_T6.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A2_T7.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A2_T8.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A2_T9.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A3_T1.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A3_T2.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A3_T3.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A4_T1.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A4_T2.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A4_T3.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A4_T4.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A5_T1.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/S15.5.4.11_A6.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/replace/this-value-not-obj-coercible.js",  # Needs String.prototype.replace
    "/test/built-ins/String/prototype/search/cstm-search-get-err.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/cstm-search-invocation.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/invoke-builtin-search-searcher-undef.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/invoke-builtin-search.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/name.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A1.1_T1.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A10.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A11.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A1_T1.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A1_T10.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A1_T11.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A1_T12.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A1_T13.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A1_T14.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A1_T2.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A1_T4.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A1_T5.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A1_T6.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A1_T7.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A1_T8.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A1_T9.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A2_T1.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A2_T2.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A2_T3.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A2_T4.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A2_T5.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A2_T6.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A2_T7.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A3_T1.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A3_T2.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A6.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A8.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/S15.5.4.12_A9.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/search/this-value-not-obj-coercible.js",  # Needs String.prototype.search
    "/test/built-ins/String/prototype/split/S15.5.4.14_A4_T19.js",  # Needs functional regex
    "/test/built-ins/String/prototype/split/S15.5.4.14_A4_T20.js",  # Needs functional regex
    "/test/built-ins/String/prototype/split/S15.5.4.14_A4_T22.js",  # Needs functional regex
    "/test/built-ins/String/prototype/split/S15.5.4.14_A4_T23.js",  # Needs functional regex
    "/test/built-ins/String/prototype/startsWith/coerced-values-of-position.js",  # Needs String.prototype.startsWith
    "/test/built-ins/String/prototype/startsWith/length.js",  # Needs String.prototype.startsWith
    "/test/built-ins/String/prototype/startsWith/name.js",  # Needs String.prototype.startsWith
    "/test/built-ins/String/prototype/startsWith/out-of-bounds-position.js",  # Needs String.prototype.startsWith
    "/test/built-ins/String/prototype/startsWith/return-abrupt-from-position.js",  # Needs String.prototype.startsWith
    "/test/built-ins/String/prototype/startsWith/return-abrupt-from-searchstring-regexp-test.js",  # Needs String.prototype.startsWith
    "/test/built-ins/String/prototype/startsWith/return-abrupt-from-searchstring.js",  # Needs String.prototype.startsWith
    "/test/built-ins/String/prototype/startsWith/return-abrupt-from-this.js",  # Needs String.prototype.startsWith
    "/test/built-ins/String/prototype/startsWith/return-true-if-searchstring-is-empty.js",  # Needs String.prototype.startsWith
    "/test/built-ins/String/prototype/startsWith/searchstring-found-with-position.js",  # Needs String.prototype.startsWith
    "/test/built-ins/String/prototype/startsWith/searchstring-found-without-position.js",  # Needs String.prototype.startsWith
    "/test/built-ins/String/prototype/startsWith/searchstring-is-regexp-throws.js",  # Needs String.prototype.startsWith
    "/test/built-ins/String/prototype/startsWith/searchstring-not-found-with-position.js",  # Needs String.prototype.startsWith
    "/test/built-ins/String/prototype/startsWith/searchstring-not-found-without-position.js",  # Needs String.prototype.startsWith
    "/test/built-ins/String/prototype/startsWith/startsWith.js",  # Needs String.prototype.startsWith
    "/test/built-ins/String/prototype/Symbol.iterator/length.js",  # Needs String.prototype.@@iterator
    "/test/built-ins/String/prototype/Symbol.iterator/name.js",  # Needs String.prototype.@@iterator
    "/test/built-ins/String/prototype/Symbol.iterator/prop-desc.js",  # Needs String.prototype.@@iterator
    "/test/built-ins/String/prototype/Symbol.iterator/this-val-to-str-err.js",  # Needs String.prototype.@@iterator
    "/test/built-ins/String/prototype/toLocaleLowerCase/Final_Sigma_U180E.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/name.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A10.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A11.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A1_T1.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A1_T10.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A1_T11.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A1_T12.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A1_T13.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A1_T14.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A1_T2.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A1_T3.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A1_T4.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A1_T5.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A1_T6.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A1_T7.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A1_T8.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A1_T9.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A2_T1.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A6.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A8.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/S15.5.4.17_A9.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/special_casing.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/special_casing_conditional.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/supplementary_plane.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleLowerCase/this-value-not-obj-coercible.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/name.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A10.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A11.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A1_T1.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A1_T10.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A1_T11.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A1_T12.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A1_T13.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A1_T14.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A1_T2.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A1_T3.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A1_T4.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A1_T5.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A1_T6.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A1_T7.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A1_T8.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A1_T9.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A2_T1.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A6.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A8.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/S15.5.4.19_A9.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/special_casing.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/supplementary_plane.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toLocaleUpperCase/this-value-not-obj-coercible.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/String/prototype/toUpperCase/name.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A10.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A11.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A1_T1.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A1_T10.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A1_T11.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A1_T12.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A1_T13.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A1_T14.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A1_T2.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A1_T3.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A1_T4.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A1_T5.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A1_T6.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A1_T7.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A1_T8.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A1_T9.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A2_T1.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A6.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A8.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/S15.5.4.18_A9.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/special_casing.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/supplementary_plane.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/toUpperCase/this-value-not-obj-coercible.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/String/prototype/trim/15.5.4.20-0-1.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-0-2.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-1-3.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-1-4.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-1-5.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-1-6.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-1-7.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-1-8.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-1-9.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-1.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-10.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-11.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-12.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-13.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-14.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-15.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-16.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-17.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-18.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-19.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-2.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-20.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-21.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-22.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-23.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-24.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-25.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-26.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-27.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-28.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-29.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-3.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-30.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-31.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-32.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-33.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-34.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-35.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-36.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-37.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-38.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-39.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-4.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-40.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-41.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-42.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-43.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-44.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-45.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-46.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-47.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-49.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-5.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-50.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-51.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-6.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-7.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-8.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-2-9.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-3-1.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-3-10.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-3-11.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-3-12.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-3-13.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-3-14.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-3-2.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-3-3.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-3-4.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-3-5.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-3-6.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-3-7.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-3-8.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-3-9.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-1.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-10.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-11.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-12.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-13.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-14.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-16.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-18.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-19.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-2.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-20.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-21.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-22.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-24.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-27.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-28.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-29.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-3.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-30.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-32.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-34.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-35.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-36.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-37.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-38.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-39.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-4.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-40.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-41.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-42.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-43.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-44.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-45.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-46.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-47.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-48.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-49.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-5.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-50.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-51.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-52.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-53.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-54.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-55.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-56.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-57.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-58.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-59.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-6.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-60.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/15.5.4.20-4-8.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/name.js",  # Needs String.prototype.trim
    "/test/built-ins/String/prototype/trim/u180e.js",  # Needs String.prototype.trim
    "/test/language/computed-property-names/class/static/method-number.js",  # Wants default anonymous function naming
    "/test/language/computed-property-names/class/static/method-string.js",  # Wants default anonymous function naming
    "/test/language/computed-property-names/class/static/method-symbol.js",  # Wants default anonymous function naming
    "/test/language/expressions/assignment/dstr/array-elem-iter-thrw-close-err.js",  # Needs a functional IteratorClose
    "/test/language/expressions/assignment/dstr/array-elem-iter-thrw-close.js",  # Needs a functional IteratorClose
    "/test/language/expressions/assignment/dstr/array-elem-nested-array-invalid.js",  # Something is Wrong
    "/test/language/expressions/assignment/dstr/array-elem-nested-array-null.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-elem-nested-array-undefined-hole.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-elem-nested-array-undefined-own.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-elem-nested-array-undefined.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-elem-nested-array-yield-ident-valid.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-elem-nested-array.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-elem-nested-obj-invalid.js",  # Something is Wrong
    "/test/language/expressions/assignment/dstr/array-elem-nested-obj-null.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-elem-nested-obj-undefined-hole.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-elem-nested-obj-undefined-own.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-elem-nested-obj-undefined.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-elem-nested-obj-yield-ident-valid.js",  # ???
    "/test/language/expressions/assignment/dstr/array-elem-nested-obj.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-elem-target-simple-strict.js",  # An early error is failing
    "/test/language/expressions/assignment/dstr/array-elem-trlg-iter-list-thrw-close-err.js",  # Needs a functional IteratorClose
    "/test/language/expressions/assignment/dstr/array-elem-trlg-iter-list-thrw-close.js",  # Needs a functional IteratorClose
    "/test/language/expressions/assignment/dstr/array-elem-trlg-iter-rest-nrml-close-skip.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-elem-trlg-iter-rest-thrw-close-err.js",  # Needs a functional IteratorClose
    "/test/language/expressions/assignment/dstr/array-elem-trlg-iter-rest-thrw-close.js",  # Needs a functional IteratorClose
    "/test/language/expressions/assignment/dstr/array-elision-val-string.js",  # ???
    "/test/language/expressions/assignment/dstr/array-empty-val-string.js",  # ???
    "/test/language/expressions/assignment/dstr/array-rest-after-element.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-rest-after-elision.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-rest-elision.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-rest-iter-nrml-close-skip.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-rest-iter-thrw-close-err.js",  # IteratorClose
    "/test/language/expressions/assignment/dstr/array-rest-iter-thrw-close.js",  # IteratorClose
    "/test/language/expressions/assignment/dstr/array-rest-lref-err.js",  # IteratorClose
    "/test/language/expressions/assignment/dstr/array-rest-lref.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-rest-nested-array-invalid.js",  # ???
    "/test/language/expressions/assignment/dstr/array-rest-nested-array-iter-thrw-close-skip.js",  # IteratorDestructuringAssignmentEvaluation
    "/test/language/expressions/assignment/dstr/array-rest-nested-array-null.js",  # IteratorDestructuringAssignmentEvaluation
    "/test/language/expressions/assignment/dstr/array-rest-nested-array-undefined-hole.js",  # IteratorDestructuringAssignmentEvaluation
    "/test/language/expressions/assignment/dstr/array-rest-nested-array-undefined-own.js",  # IteratorDestructuringAssignmentEvaluation
    "/test/language/expressions/assignment/dstr/array-rest-nested-array-undefined.js",  # IteratorDestructuringAssignmentEvaluation
    "/test/language/expressions/assignment/dstr/array-rest-nested-array-yield-ident-valid.js",  # IteratorDestructuringAssignmentEvaluation
    "/test/language/expressions/assignment/dstr/array-rest-nested-array.js",  # ???
    "/test/language/expressions/assignment/dstr/array-rest-nested-obj-invalid.js",  # ???
    "/test/language/expressions/assignment/dstr/array-rest-nested-obj-null.js",  # IteratorDestructuringAssignmentEvaluation
    "/test/language/expressions/assignment/dstr/array-rest-nested-obj-undefined-hole.js",  # IteratorDestructuringAssignmentEvaluation
    "/test/language/expressions/assignment/dstr/array-rest-nested-obj-undefined-own.js",  # IteratorDestructuringAssignmentEvaluation
    "/test/language/expressions/assignment/dstr/array-rest-nested-obj-undefined.js",  # IteratorDestructuringAssignmentEvaluation
    "/test/language/expressions/assignment/dstr/array-rest-nested-obj-yield-ident-valid.js",  # ???
    "/test/language/expressions/assignment/dstr/array-rest-nested-obj.js",  # IteratorDestructuringAssignmentEvaluation
    "/test/language/expressions/assignment/dstr/array-rest-put-let.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-rest-put-prop-ref-no-get.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-rest-put-prop-ref-user-err-iter-close-skip.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-rest-put-prop-ref.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-rest-put-unresolvable-no-strict.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-rest-put-unresolvable-strict.js",  # Fix pending
    "/test/language/expressions/assignment/dstr/array-rest-yield-ident-valid.js",  # Fix pending
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

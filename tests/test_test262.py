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
        print(f"Loading Test {filename}")
        self.config = self.get_config()
        self.root = root

    def get_config(self):
        confyaml = self.config_matcher.match(self.source).group(1)
        conf = yaml.safe_load(confyaml)
        if "includes" not in conf:
            conf["includes"] = []
        if "flags" not in conf:
            conf["flags"] = []
        # pprint(conf)
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
    "built-ins/Boolean",
    "built-ins/Error",
    "built-ins/Function/prototype/bind",
    "built-ins/Math/Symbol.toStringTag.js",
    "built-ins/Object",
    "built-ins/String/prototype/charAt",
    "built-ins/String/prototype/concat",
    "built-ins/String/prototype/indexOf",
    "built-ins/String/prototype/slice",
    "built-ins/String/prototype/split",
    "built-ins/String/prototype/substring",
    "built-ins/String/prototype/toString",
    "built-ins/String/prototype/valueOf",
    "built-ins/isFinite",
    "built-ins/isNaN",
    "built-ins/parseFloat",
    "built-ins/parseInt",
    "language/arguments-object",
    "language/asi",
    "language/block-scope",
    "language/comments",
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
    "language/expressions/bitwise-and",
    "language/expressions/bitwise-not",
    "language/expressions/bitwise-or",
    "language/expressions/bitwise-xor",
    "language/expressions/call",
    "language/expressions/comma",
    "language/expressions/concatenation",
    "language/expressions/conditional",
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
    "language/expressions/void",
)

is_circle = any(n.startswith("CIRCLE") for n in os.environ)

test_passing = False or is_circle
run_slow_tests = True and not is_circle
base_path = base_paths[0]
test_files = []
if test_passing:
    for tst in passing:
        if tst.endswith(".js"):
            test_files.append(f"{base_path}/test/{tst}")
        else:
            test_files.extend(glob.glob(f"{base_path}/test/{tst}/**.js", recursive=True))
# test_files.extend(glob.glob(f"{base_path}/test/harness/*.js"))
# for suite in lang_tests:
#     test_files.extend(glob.glob(f"{base_path}/test/language/{suite}/**/*.js", recursive=True))
# test_files.extend(glob.glob(f"{base_path}/test/built-ins/String/**/*.js", recursive=True))
# test_files.extend(glob.glob(f"{base_path}/test/built-ins/Array/**/*.js", recursive=True))
# test_files.extend(glob.glob(f"{base_path}/test/built-ins/Symbol/**/*.js", recursive=True)) # Needs Map
# test_files.extend(glob.glob(f"{base_path}/test/language/computed-property-names/**/*.js", recursive=True)) # 20 failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/assignment/**/*.js", recursive=True)) # Needs Array.prototype.reduce
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/arrow-function/**/*.js", recursive=True)) # 109 tests failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/compound-assignment/**/*.js", recursive=True)) # 78 failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/delete/**/*.js", recursive=True)) # 4 failing (needs JSON)
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/class/**/*.js", recursive=True)) # 38 failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/function/**/*.js", recursive=True))  # 14 failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/left-shift/**/*.js", recursive=True))  # 8 failing (Hit python recursion limit)
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/modulus/**/*.js", recursive=True))  # 20 failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/new/**/*.js", recursive=True))  # 16 failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/object/**/*.js", recursive=True))  # 23 failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/property-accessors/**/*.js", recursive=True))  # 10 failing
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/right-shift/**/*.js", recursive=True)) #  8 failing (Hit python recursion limit)
# test_files.extend(glob.glob(f"{base_path}/test/language/expressions/super/**/*.js", recursive=True))  # 2 failing (Needs String.toLowerCase)
test_files = [fn for fn in test_files if not fn.endswith("_FIXTURE.js")]

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

xfail_tests = (
    "/test/built-ins/Object/prototype/toString/Object.prototype.toString.call-regexp.js",  # Needs functional regex
    "/test/built-ins/Object/prototype/toString/symbol-tag-override-instances.js",  # Needs functional regex
    "/test/built-ins/String/prototype/indexOf/S15.5.4.7_A1_T12.js",  # Needs Array.prototype.indexOf
    "/test/built-ins/String/prototype/split/S15.5.4.14_A4_T20.js",  # Needs functional regex
    "/test/built-ins/String/prototype/split/S15.5.4.14_A4_T19.js",  # Needs functional regex
    "/test/built-ins/String/prototype/split/S15.5.4.14_A4_T22.js",  # Needs functional regex
    "/test/built-ins/String/prototype/split/S15.5.4.14_A4_T23.js",  # Needs functional regex
    "/test/built-ins/Function/prototype/bind/15.3.4.5-2-7.js",  # Needs JSON object
    "/test/built-ins/Function/prototype/bind/S15.3.4.5_A5.js",  # Needs Array.prototype.concat
    "/test/built-ins/Object/create/15.2.3.5-4-120.js",  # Needs JSON object
    "/test/built-ins/Object/create/15.2.3.5-4-13.js",  # Needs JSON object
    "/test/built-ins/Object/create/15.2.3.5-4-145.js",  # Needs JSON object
    "/test/built-ins/Object/create/15.2.3.5-4-173.js",  # Needs JSON object
    "/test/built-ins/Object/create/15.2.3.5-4-199.js",  # Needs JSON object
    "/test/built-ins/Object/create/15.2.3.5-4-224.js",  # Needs JSON object
    "/test/built-ins/Object/create/15.2.3.5-4-252.js",  # Needs JSON object
    "/test/built-ins/Object/create/15.2.3.5-4-287.js",  # Needs JSON object
    "/test/built-ins/Object/create/15.2.3.5-4-36.js",  # Needs JSON object
    "/test/built-ins/Object/create/15.2.3.5-4-67.js",  # Needs JSON object
    "/test/built-ins/Object/create/15.2.3.5-4-92.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperties/15.2.3.7-2-14.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperties/15.2.3.7-5-a-15.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperties/15.2.3.7-5-b-105.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperties/15.2.3.7-5-b-133.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperties/15.2.3.7-5-b-159.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperties/15.2.3.7-5-b-184.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperties/15.2.3.7-5-b-212.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperties/15.2.3.7-5-b-247.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperties/15.2.3.7-5-b-27.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperties/15.2.3.7-5-b-52.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperties/15.2.3.7-5-b-80.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperties/15.2.3.7-6-a-20.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-3-119.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-3-147-1.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-3-147.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-3-173-1.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-3-173.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-3-198.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-3-226-1.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-3-226.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-3-256-1.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-3-256.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-3-41-1.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-3-41.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-3-66.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-3-94-1.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-3-94.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-41.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-410.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-586.js",  # Needs JSON object
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-612.js",  # Needs Array.prototype.indexOf
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-613.js",  # Needs Array.prototype.lastIndexOf
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-614.js",  # Needs Array.prototype.every
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-615.js",  # Needs Array.prototype.some
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-618.js",  # Needs Array.prototype.filter
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-619.js",  # Needs Array.prototype.reduce
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-620.js",  # Needs Array.prototype.reduceRight
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-621.js",  # Needs String.prototype.trim
    "/test/built-ins/Object/defineProperty/15.2.3.6-4-622.js",  # Needs Date.now
    "/test/built-ins/Object/freeze/throws-when-false.js",  # Needs Proxy
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-10.js",  # Needs decodeURIComponent
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-11.js",  # Needs encodeURIComponent
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-114.js",  # Needs Date.parse
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-115.js",  # Needs Date.UTC
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-116.js",  # Needs Date.prototype.constructor
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-163.js",  # Needs RegExp.prototype.constructor
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-168.js",  # Needs Error.prototype.constructor
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-170.js",  # Needs EvalError.prototype.constructor
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-171.js",  # Needs RangeError.prototype.constructor
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-172.js",  # Needs ReferenceError.prototype.constructor
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-173.js",  # Needs SyntaxError.prototype.constructor
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-174.js",  # Needs TypeError.prototype.constructor
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-175.js",  # Needs URIError.prototype.constructor
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-176.js",  # Needs JSON
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-177.js",  # Needs JSON
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-27.js",  # Needs Object.prototype.constructor
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-34.js",  # Needs Function.prototype.constructor
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-39.js",  # Needs Array.prototype.constructor
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-40.js",  # Needs Array.prototype.concat
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
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-59.js",  # Needs Array.prototype.reduce
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-60.js",  # Needs Array.prototype.reduceRight
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-62.js",  # Needs String.prototype.constructor
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-64.js",  # Needs String.prototype.charCodeAt
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-67.js",  # Needs String.prototype.lastIndexOf
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-68.js",  # Needs String.prototype.match
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-69.js",  # Needs String.prototype.replace
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-70.js",  # Needs String.prototype.search
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-75.js",  # Needs String.prototype.toLowerCase
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-77.js",  # Needs String.prototype.toUpperCase
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-79.js",  # Needs String.prototype.toLocaleLowerCase
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-80.js",  # Needs String.prototype.toLocaleUpperCase
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-81.js",  # Needs String.prototype.localeCompare
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-82.js",  # Needs String.prototype.trim
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-84.js",  # Needs Boolean.prototype.constructor
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-88.js",  # Needs Number.prototype.constructor
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-9.js",  # Needs decodeURI
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-90.js",  # Needs Number.prototype.toLocaleString
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-91.js",  # Needs Number.prototype.toFixed
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-92.js",  # Needs Number.prototype.toExponential
    "/test/built-ins/Object/getOwnPropertyDescriptor/15.2.3.3-4-93.js",  # Needs Number.prototype.toPrecision
    "/test/built-ins/Object/getOwnPropertyNames/15.2.3.4-4-1.js",  # Needs all the globals
    "/test/built-ins/Object/getPrototypeOf/15.2.3.2-2-18.js",  # Needs JSON
    "/test/built-ins/Object/isExtensible/15.2.3.13-2-12.js",  # Needs JSON
    "/test/built-ins/Object/isFrozen/15.2.3.12-3-27.js",  # Needs JSON
    "/test/built-ins/Object/isSealed/15.2.3.11-4-27.js",  # Needs JSON
    "/test/built-ins/Object/keys/proxy-keys.js",  # Needs Proxy
    "/test/built-ins/Object/preventExtensions/throws-when-false.js",  # Needs Proxy
    "/test/built-ins/Object/seal/throws-when-false.js",  # Needs Proxy
)


def should_skip(tc, file_id):
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

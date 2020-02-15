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
# test_files.extend(glob.glob(f"{base_path}/test/built-ins/Object/**/*.js", recursive=True)) # Needs Proxy
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
    "/test/built-ins/Function/prototype/bind/15.3.4.5-2-7.js",  # Needs JSON object
    "/test/built-ins/Function/prototype/bind/S15.3.4.5_A5.js",  # Needs Array.prototype.concat
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

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

if any(n.startswith("CIRCLE") for n in os.environ):
    pytest.skip("Skipping Test-262 on Circle.CI", allow_module_level=True)
base_paths = tuple(
    p for p in ("/Users/scole/fun/test262", "/mnt/c/Users/scole/Documents/test262") if os.path.exists(p)
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

base_path = base_paths[0]
test_files = []
test_files = glob.glob(f"{base_path}/test/harness/*.js")
# for suite in lang_tests:
#     test_files.extend(glob.glob(f"{base_path}/test/language/{suite}/**/*.js", recursive=True))
# test_files.extend(glob.glob(f"{base_path}/test/built-ins/String/**/*.js", recursive=True))
# test_files.extend(glob.glob(f"{base_path}/test/built-ins/Boolean/**/*.js", recursive=True))  # 100% passing !
# test_files.extend(glob.glob(f"{base_path}/test/built-ins/Array/**/*.js", recursive=True))
# test_files.extend(glob.glob(f"{base_path}/test/built-ins/parseInt/**/*.js", recursive=True))  # 100% passing !
# test_files.extend(glob.glob(f"{base_path}/test/built-ins/isNaN/**/*.js", recursive=True))  # 100% passing !
# test_files.extend(glob.glob(f"{base_path}/test/built-ins/isFinite/**/*.js", recursive=True))  # 100% passing !
# test_files.extend(glob.glob(f"{base_path}/test/language/types/boolean/**/*.js", recursive=True))  # 100% passing !
# test_files.extend(glob.glob(f"{base_path}/test/language/types/**/*.js", recursive=True))
# test_files.extend(glob.glob(f"{base_path}/test/language/eval-code/**/*.js", recursive=True))  # 100% passing !
# test_files.extend(glob.glob(f"{base_path}/test/language/statementList/**/*.js", recursive=True))  # 100% passing !
# test_files.extend(glob.glob(f"{base_path}/test/language/statements/labeled/**/*.js", recursive=True))  # 100% passing !
# test_files.extend(glob.glob(f"{base_path}/test/language/identifiers/**/*.js", recursive=True))  # 100% passing!
# test_files.extend(glob.glob(f"{base_path}/test/language/identifier-resolution/**/*.js", recursive=True))  # 100% passing!
# test_files.extend(glob.glob(f"{base_path}/test/language/line-terminators/**/*.js", recursive=True))  # 100% passing!
# test_files.extend(glob.glob(f"{base_path}/test/language/statements/break/**/*.js", recursive=True))  # 100% passing!
# test_files.extend(glob.glob(f"{base_path}/test/language/statements/continue/**/*.js", recursive=True))  # 100% passing!
test_files = [fn for fn in test_files if not fn.endswith("_FIXTURE.js")]

features_to_avoid = (
    # won't ever support
    "caller",
    # don't support right now
    "async-iteration",
    "async-functions",
    "tail-call-optimization",
    "class",
    "generators",
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

params = []
for tf in test_files:
    tc = test262_testcase(tf, base_path)
    file_id = tf[len(base_path) :] if tf.startswith(base_path) else tf
    if not any(feature in tc.config.get("features", []) for feature in features_to_avoid) and not any(
        flag in tc.config.get("flags", []) for flag in flags_to_avoid
    ):
        for strict in (
            val
            for val, _ in filter(
                itemgetter(1),
                ((False, "onlyStrict" not in tc.config["flags"]), (True, "noStrict" not in tc.config["flags"])),
            )
        ):
            params.append(
                pytest.param(
                    tc,
                    strict,
                    tf,
                    id=f"{file_id}: {tc.config['description'].strip().splitlines()[0].strip()} [{'strict' if strict else 'loose'}]",
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

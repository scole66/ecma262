import pytest
import os
import re
import yaml
import glob
from itertools import chain
from operator import itemgetter

from ecmascript.ecmascript import (
    surrounding_agent,
    RunJobs,
    SetHostErrorCallback,
    ToString,
    InitializeHostDefinedRealm,
    Get,
)

import snoop
from pprint import pprint

if any(n.startswith("CIRCLE") for n in os.environ):
    pytest.skip("Skipping Test-262 on Circle.CI", allow_module_level=True)
base_paths = tuple(p for p in ("/Users/scole/fun/test262", "/mnt/c/Users/scole/Documents/test262") if os.path.exists(p))
if len(base_paths) != 1:
    pytest.skip("Skipping Test-262 because we don't know where the tests are", allow_module_level=True)


@pytest.fixture
def test262realm():
    InitializeHostDefinedRealm()
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
        rv = RunJobs(scripts=[massaged])
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
# test_files = glob.glob(f"{base_path}/test/harness/*.js")
# for suite in lang_tests:
#     test_files.extend(glob.glob(f"{base_path}/test/language/{suite}/**/*.js", recursive=True))
# test_files.extend(glob.glob(f"{base_path}/test/built-ins/String/**/*.js", recursive=True))
# test_files.extend(glob.glob(f"{base_path}/test/built-ins/Boolean/**/*.js", recursive=True))
# test_files.extend(glob.glob(f"{base_path}/test/built-ins/Array/**/*.js", recursive=True))
test_files.extend(glob.glob(f"{base_path}/test/built-ins/parseInt/**/*.js", recursive=True))
test_files = [fn for fn in test_files if not fn.endswith("_FIXTURE.js")]

features_to_avoid = (
    # won't ever support
    "caller",
    # don't support right now
    "async-iteration",
    "async-functions",
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

params = []
for tf in test_files:
    tc = test262_testcase(tf, base_path)
    file_id = tf[len(base_path) :] if tf.startswith(base_path) else tf
    if not any(feature in tc.config.get("features", []) for feature in features_to_avoid):
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
        assert len(execution_errors.errors) == 1, (
            f"Expected a {tc.config['negative']['type']} error, "
            + ("completed successfully" if len(execution_errors.errors) == 0 else "got multiple errors")
            + " instead"
        )
        errobj = execution_errors.errors[0]
        constructor = Get(errobj, "constructor")
        name = Get(constructor, "name")
        assert (
            name == tc.config["negative"]["type"]
        ), f"Expected a {tc.config['negative']['type']} error, got '{execution_errors.errors[0]}' instead"
    else:
        assert len(execution_errors.errors) == 0, "\n".join(execution_errors.errors)

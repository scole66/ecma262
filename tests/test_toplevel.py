"""
This one is for top-level tests. I.e.: Send some JS into the parser and see what comes out after evaluation.
"""

import pytest
import math

# pylint: disable=unused-wildcard-import
from ecmascript.ecmascript import *


@pytest.fixture
def cleanup():
    yield None
    surrounding_agent.ec_stack = []
    surrounding_agent.running_ec = None


@pytest.mark.parametrize(
    "script, result",
    [
        ("", None),
        ("Infinity;", math.inf),
        ("bob = 3;", 3),
        ("bob = 3; bob *= 2;", 6),
        ("bob = 'fire'; bob += 'truck';", "firetruck"),
        ("7, 8;", 8),
        (
            """
    true ? 'truth value' : 'falsehood value';
    """,
            "truth value",
        ),
        (
            """
    false ? 'truth value' : 'falsehood value';
    """,
            "falsehood value",
        ),
        ("67 || 99;", 67),
        ("67 && 99;", 99),
        ("false || 102;", 102),
        ("null && 'thing';", JSNull.NULL),
        ("0x80 | 0x34;", 0b10110100),
        ("0x73 & 0x29;", 0x21),
        ("0xff ^ 0x156;", 0x1A9),
        ("'3' == 3;", True),
        ("'-0' == 0;", True),
        ("67 == 89;", False),
        ("'t1' == 't1';", True),
        ("'3' != 3;", False),
        ("'-0' != 0;", False),
        ("67 != 89;", True),
        ("'t1' != 't1';", False),
        ("'3' === 3;", False),
        ("3 === 3;", True),
        ("'3' !== 3;", True),
        ("3 !== 3;", False),
        ("3 < 4;", True),
        ("3 <= 4;", True),
        ("3 > 4;", False),
        ("3 >= 3;", True),
        ("'green' instanceof Number;", False),
        ("Number instanceof Object;", True),
        ("0xffffffff >> 16;", -1),
        ("0xffffffff >>> 16;", 0x0000FFFF),
        ("0xffffffff << 8;", -256),
        ("3 + 7;", 10),
        ("100 - 25;", 75),
        ("'th' + 'ing';", "thing"),
        ("-Infinity + -Infinity;", -math.inf),
        ("Infinity + Infinity;", math.inf),
        ("Infinity + 88;", math.inf),
        ("'prototype' in Object;", True),
        ("'yuccaplant' in Object;", False),
        ("{}", None),
        ("{ b=3; b*=6; }", 18),
        ("var x;", None),
        ("var xyz=67+99; xyz;", 67 + 99),
        ("1 * 1;", 1),
        ("121 / 11;", 11),
        ("67 % 16;", 3),
        ("4**16;", 4 ** 16),
        ("xyz=8881; delete xyz;", True),
        ("foo=NaN; typeof foo;", "number"),
        ("typeof missing;", "undefined"),
        ("typeof true;", "boolean"),
        ("typeof 'a';", "string"),
        ("typeof Number;", "function"),
        ("+89;", 89),
        ("-900;", -900),
        ("~0x80008000;", 0x7FFF7FFF),
        ("!true;", False),
        ("x=100; --x;", 99),
        ("x=100; x--;", 100),
        ("x=100; x--; x;", 99),
        ("x=100; ++x;", 101),
        ("x=100; x++;", 100),
        ("x=100; x++; x;", 101),
        ("new Number - 0;", 0),
        ("new Number(60) - 0;", 60),
        ("Number('55');", 55),
        ("      Boolean(60);", True),
        ("n = new Number(3); n['extra'] = 6; n['extra'];", 6),
        ("puppy = new Boolean(false); puppy['true'] = 888; puppy.true;", 888),
        ("bob = new Object(); bob.thing_1 = 99; bob.thing_2 = 102; bob.NUMBER = -1234;", -1234),
        ("((4+8)*(12-3)+1)/(13-3);", 10.9),
        ("if (true) { 'true value'; } else { 'false value'; };", "true value"),
        ("if (false) { 'true value'; } else { 'false value'; };", "false value"),
        ("if (true) { 'true value'; };", "true value"),
        ("if (false) { 'true value'; };", None),
        ("x=0; do x++; while (x<10); x;", 10),
        ("x=0; y=0; while (x++ < 10) y -= 2; y;", -20),
        ("y=''; for (x=0; x<10; x++) { y += x; }", "0123456789"),
        (
            "var mystr=''; for (var x=0, y=0; x<15; x++,y--) {if (x%2) {mystr += x;} else {mystr += y;}}; mystr;",
            "01-23-45-67-89-1011-1213-14",
        ),
        ("String.fromCharCode(65, 66, 67);", "ABC"),
        ("for (i=0, m=''; i<23; i++){ if (i%2 == 0) { continue; }; m += i + '-'; };", "1-3-5-7-9-11-13-15-17-19-21-"),
        ("m=''; for (let i=0; i<10; i++) { let i=2; m += i; };", "2222222222"),
        ("{let a=3;{let a=6;};};", None),
        ("idx=67; m=''; for (let idx=0; idx<10; idx++) { m += idx; }; m+idx;", "012345678967"),
        ("b={a:3};b.a;", 3),
        ("b={a:5,};b.a;", 5),
        ("b={a:99,bob:'purple'};b.bob+b.a;", "purple99"),
        ("a=55;b=67;c={a,b};c.a*c.b;", 55 * 67),
        ("b={a:3};c={...b,y:88};c.a*c.y;", 3 * 88),
        ("b=Array();'['+b+']';", "[]"),
        ("b=Array(10);'['+b+']';", "[,,,,,,,,,]"),
        ("b=Array(3, 6, 12); '['+b+']';", "[3,6,12]"),
        ("'[' + [] + ']';", "[]"),
        ("'[' + [1, 2, 3] + ']';", "[1,2,3]"),
        ("'[' + [1, , 3] + ']';", "[1,,3]"),
        ("'[' + [,,, 3] + ']';", "[,,,3]"),
        ("'[' + [,,,] + ']';", "[,,]"),
        ("'[' + [66,55,,] + ']';", "[66,55,]"),
        ("'['+[65,99,]+']';", "[65,99]"),
        (
            "a=[1, 2, 3, 44, 55];for (it=a.values(), ir=it.next(), s=''; ! ir.done; ir = it.next()) { s += ir.value + '-'; };",
            "1-2-3-44-55-",
        ),
        ("a=[1, 2, 3]; it = a.values(); ''+it;", "[object Array Iterator]"),
        (
            "let colors = [ 'red', 'green', 'blue' ]; let [ firstColor, secondColor ] = colors; firstColor + '-' + secondColor;",
            "red-green",
        ),
        ("let colors = [ 'red', 'green', 'blue' ]; let [ ,, thirdColor ] = colors; thirdColor;", "blue"),
        (
            "let colors = [ 'red', 'green', 'blue' ], firstColor = 'black', secondColor = 'purple'; [ firstColor, secondColor ] = colors; firstColor + '+' + secondColor;",
            "red+green",
        ),
        ("let a=1, b=2; [a, b] = [b, a]; ''+a+' '+b;", "2 1"),
        # ("[a, b] = undefined;", None), Need to find a way to test for errors :/
        (
            "let colors=['red']; [firstcolor, secondcolor] = colors; 'firstcolor=' + firstcolor + ', secondcolor=' + secondcolor;",
            "firstcolor=red, secondcolor=undefined",
        ),
        (
            "let colors=['red']; [firstcolor='pink', secondcolor='puce'] = colors; 'firstcolor=' + firstcolor + ', secondcolor=' + secondcolor;",
            "firstcolor=red, secondcolor=puce",
        ),
        (
            "let colors=['red', ['green', 'lightgreen'], 'blue']; let [ firstcolor, [ secondcolor ]] = colors; 'firstcolor=' + firstcolor + ', secondcolor=' + secondcolor;",
            "firstcolor=red, secondcolor=green",
        ),
        (
            "let colors=['red','green','blue'];let [firstcolor, ...restcolors]=colors;firstcolor+restcolors.length+restcolors[0]+restcolors[1];",
            "red2greenblue",
        ),
        (
            "let node={type:'Identifier',name:'foo',loc:{start:{line:1,column:1},end:{line:1,column:4}},range:[0,3]};let{loc:{start},range:[startIndex]}=node;''+start.line+', '+start.column+', '+startIndex;",
            "1, 1, 0",
        ),
        (
            "let obj={first:'a',second:'b',third:'c'};s='';for (h in obj){s+=h+'-';}",
            "first-second-third-",
        ),  # for ( LeftHandSideExpression in Expression ) Statement
        (
            "let obj={first:'a',second:'b',third:'c'};s='';for (var h in obj){s+=h+'-';}",
            "first-second-third-",
        ),  # for ( var ForBinding in Expression ) Statement
        (
            "let obj={first:'a',second:'b',third:'c'};s='';for (let h in obj){s+=h+'-';}",
            "first-second-third-",
        ),  # for ( ForDeclaration in Expression ) Statement
        (
            "let ary=['first','second','third'];s='';for (h of ary){s+=h+'-';}",
            "first-second-third-",
        ),  # for ( LeftHandSideExpression of Expression ) Statement
        (
            "let ary=['first','second','third'];s='';for (var h of ary){s+=h+'-';}",
            "first-second-third-",
        ),  # for ( var ForBinding of Expression ) Statement
        (
            "let ary=['first','second','third'];s='';for (let h of ary){s+=h+'-';}",
            "first-second-third-",
        ),  # for ( ForDeclaration of Expression ) Statement
        ("let ary=[[1,2],[3,4],[5,6]];let s='';for ([a,b] of ary){s+=(a*b)+'-';}", "2-12-30-"),
        ("let a={f:10};function b(o) {o.f=2;}; b(a); a.f;", 2),
        ("let a={f:1};function d(){a.f=100;}; d(); a.f;", 100),
        ("let s='';for(let ch of['a','b','c','d','e','f']){s+=ch;if(ch=='d'){break;}}s;", "abcd"),
        ("let s='',i=0;while(true){s+=i++;if(i>=10)break;}s;", "0123456789"),
        ("function mathme(a,b,c){return(a+b)*c;}mathme(1,2,3)*mathme(100,200,300);", 810000),
        (
            "let s=new String('hi');''+Object.defineProperty(s, '0', {value: 'h'});",
            "hi",
        ),  # Should exercise IsCompatiblePropertyDescriptor
        ("let get=3; get;", 3),  # Make sure 'get' works as an identifier.
        ("let set=4; set;", 4),  # Make sure 'set' works as an identifier.
        ("new Date(2016, 6).getMonth();", 6),  # first millisecond
        ("new Date(2016, 6, 0, 0, 0, 0, -1).getMonth();", 5),  # previous millisecond
        ("new Date(2016, 6, 31, 23, 59, 59, 999).getMonth();", 6),  # final millisecond
        ("new Date(2016, 6, 31, 23, 59, 59, 1000).getMonth();", 7),
        ("new Date(2016, 11, 31).getMonth();", 11),
        ("new Date(2016, 11, 0, 0, 0, 0, -1).getMonth();", 10),
        ("new Date(2016, 11, 31, 23, 59, 59, 999).getMonth();", 11),
        ("new Date(2016, 11, 31, 23, 59, 59, 1000).getMonth();", 0),
        ("1.0 + Number.EPSILON != 1.0 && 1.0 + (Number.EPSILON / 2) == 1.0;", True),
        (
            "Number.MAX_SAFE_INTEGER + 1 != Number.MAX_SAFE_INTEGER && Number.MAX_SAFE_INTEGER + 1 == Number.MAX_SAFE_INTEGER + 2;",
            True,
        ),
        ("Number.MAX_VALUE * (1 + Number.EPSILON) == Infinity;", True),
        (
            "Number.MIN_SAFE_INTEGER - 1 != Number.MIN_SAFE_INTEGER && Number.MIN_SAFE_INTEGER - 1 == Number.MIN_SAFE_INTEGER - 2;",
            True,
        ),
        ("Number.MIN_VALUE / 2 == 0;", True),
        ("Number.NEGATIVE_INFINITY;", -math.inf),
        ("Number.POSITIVE_INFINITY;", math.inf),
        (
            "let result = true; for (x of [NaN, Infinity, -Infinity, undefined]) { result = result && !Number.isFinite(x); } result;",
            True,
        ),
        ("Number.isFinite(67);", True),
        (
            "let result = true; for (x of [NaN, Infinity, -Infinity, undefined, 55.34]) { result = result && !Number.isInteger(x); } result;",
            True,
        ),
        ("Number.isInteger(99) && Number.isInteger(-100);", True),
        ("Number.isNaN();", False),
        ("Number.isNaN(Infinity);", False),
        ("Number.isNaN(NaN);", True),
        ("Number.isNaN(0);", False),
        ("Number.isSafeInteger();", False),
        ("Number.isSafeInteger(100);", True),
        ("!Number.isSafeInteger(2**100) && Number.isInteger(2**100);", True),
        (
            "let result = true; for (exp=50; exp<60; exp++) { num = 2**exp - 1; probe = Number.isSafeInteger(num) === num <= Number.MAX_SAFE_INTEGER; result = result && probe; } result;",
            True,
        ),
        (
            'function fart(literals, ...substitutions) { let result = ""; for (let i=0; i < substitutions.length; i++) { result += literals[i] + "~~"; result += "#" + substitutions[i] + "#"; } result += literals[literals.length-1]; return result; } fart`${100} items are $${100 * 2.50}.` + fart`loopy`;',
            "~~#100# items are $~~#250#.loopy",
        ),
        (
            'let name="bob"; String.raw`Hello.\\nIt looks like ${name} is here.\\n`;',
            "Hello.\\nIt looks like bob is here.\\n",
        ),
        ("String.raw({raw: []})", ""),
        ("String.raw({raw: ['a', 'b', 'c', 'd', 'e']}, 12, 123)", "a12b123cde"),
        (
            "a = [1, 2, 3]; a.forEach(function (v, i, o) {o[i] = v+10;}); let res=''; for (idx=0; idx<a.length; idx++) res += a[idx] + ', '; res;",
            "11, 12, 13, ",
        ),
        ("isFinite(Infinity)", False),
        ("isFinite(62)", True),
        ("isNaN(0/0)", True),
        ("isNaN(88)", False),
        ("switch(1) { case 1: 2; break; }", 2),
        ("1; switch(1) { case 1: }", None),
        ("delete eval.length; eval.length;", 0),
        ("isNaN(parseInt(true))", True),
        ("parseInt(true, 36)", 1389110),
        ("''-4", -4),
        ("this.eval('1')", 1),
        ("p = Object.getOwnPropertyDescriptor(this, 'NaN'); !p.writable && !p.enumerable && !p.configurable", True),
        (
            "p = Object.getOwnPropertyDescriptor(this, 'undefined'); !p.writable && !p.enumerable && !p.configurable",
            True,
        ),
        (
            "p = Object.getOwnPropertyDescriptor(this, 'Infinity'); !p.writable && !p.enumerable && !p.configurable",
            True,
        ),
        ("{ function f ( ) { } }", None),
    ],
)
def test_scripts_01(cleanup, script, result):
    rv = RunJobs(scripts=[script])
    assert rv == result


@pytest.mark.parametrize(
    "script", ["NaN + 3;", "78 + NaN;", "-Infinity + Infinity;", "Infinity + -Infinity;", "Number.NaN;"]
)
def test_scripts_02(cleanup, script):
    # Check for those expressions that return NaN.
    rv = RunJobs(scripts=[script])
    assert math.isnan(rv)

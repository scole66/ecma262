import pytest

import ecmascript.lexer2 as lexer2


class Test_utf_16_encode:
    @pytest.mark.parametrize(
        "input, expected",
        [
            ("", ""),
            ("simple", "simple"),
            ("funky\uffff!", "funky\uffff!"),
            ("big\U00014332gy", "big\ud810\udf32gy"),
            ("\U00010437\U00024b62foo", "\ud801\udc37\ud852\udf62foo"),
        ],
    )
    def test_func(self, input, expected):
        rv = lexer2.utf_16_encode(input)
        assert rv == expected


class Test_utf_16_decode:
    @pytest.mark.parametrize(
        "input, expected",
        [
            ("", ""),
            ("simple", "simple"),
            ("funky\uffff!", "funky\uffff!"),
            ("big\ud810\udf32gy", "big\U00014332gy"),
            ("\ud801\udc37\ud852\udf62foo", "\U00010437\U00024b62foo"),
        ],
    )
    def test_good(self, input, expected):
        rv = lexer2.utf_16_decode(input)
        assert rv == expected

    @pytest.mark.parametrize("input", ["big\ud810gy", "\ud801\udc37\ud852foo", "\ud810"])
    def test_failure(self, input):
        with pytest.raises(SyntaxError):
            lexer2.utf_16_decode(input)


class Test_identifier_name_string_value:
    @pytest.mark.parametrize(
        "input, expected",
        [
            ("main", "main"),
            (r"\u0068\u0061\u{70}\u{70}\u{79}", "happy"),
            (r"\u{14332}", "\ud810\udf32"),
            ("\U00014332", "\ud810\udf32"),
        ],
    )
    def test_func(self, input, expected):
        rv = lexer2.identifier_name_string_value(input)
        assert rv == expected

    @pytest.mark.parametrize("input", ["bad\\u{451345234513241}stuff"])
    def test_bad(self, input):
        with pytest.raises(SyntaxError):
            lexer2.identifier_name_string_value(input)


class Test_identifier_name_early_errors:
    # \u{1001d} is in ID_continue (also start), looks like a tie figher
    # Input is already in utf-16, no unicode escapes.

    @pytest.mark.parametrize(
        "input", ["valid", "\ud800\udc1d_mark", "mark_\ud800\udc1d", "thing_1", "$val\N{ZWJ}ue", "_va\N{ZWNJ}lue"]
    )
    def test_good(self, input):
        token = lexer2.Token(type="IDENTIFIER", src=input, value=input, span=lexer2.Span(0, len(input)), newlines=[])
        # All we're really doing here is watching for exceptions.
        lexer2.identifier_name_early_errors(token)

    @pytest.mark.parametrize("input", ["1_thing", "+evil", "\N{ZWJ}$", "\N{ZWNJ}foo"])
    def test_bad(self, input):
        token = lexer2.Token(type="IDENTIFIER", src=input, value=input, span=lexer2.Span(0, len(input)), newlines=[])
        # All we're really doing here is watching for exceptions.
        with pytest.raises(SyntaxError):
            lexer2.identifier_name_early_errors(token)


class Test_LexerCore:
    def test_LexerCore_init(self):
        lc = lexer2.LexerCore("I am a source string", "I am an error constructor")
        assert lc.src == "I am a source string"
        assert lc.syntax_error_ctor == "I am an error constructor"

    @pytest.mark.parametrize(
        "input, expected",
        [
            ('"vanilla"', "vanilla"),
            ('"is_\\x55gly"', "is_Ugly"),
            ("'is+\\u0055gly'", "is+Ugly"),
            ('"is(\\u{55}gly)"', "is(Ugly)"),
            (
                '"is\\0Null-\\b-Bell-\\t-Tab-\\n-Newline-\\v-vtab-\\f-formfeed-\\r-return"',
                "is\0Null-\b-Bell-\t-Tab-\n-Newline-\v-vtab-\f-formfeed-\r-return",
            ),
            ('"line1\\\nline2"', "line1line2"),
            ('"line1\\\r\nline2"', "line1line2"),
            ("'bull\\pucky'", "bullpucky"),
            ("'ends_on_an_escape\\n'", "ends_on_an_escape\n"),
        ],
    )
    def test_string_value(self, input, expected):
        lc = lexer2.LexerCore("", SyntaxError)
        rv = lc._string_value(input)
        assert rv == expected

    @pytest.mark.parametrize("input", ['"badness: \\u{7451980347895104359810451}"'])
    def test_string_value_errs(self, input):
        lc = lexer2.LexerCore("", SyntaxError)
        with pytest.raises(SyntaxError):
            lc._string_value(input)

    @pytest.mark.parametrize(
        "input_src, startpos, goal, expected",
        [
            ("bob", 0, "InputElementRegExp", lexer2.Token("IDENTIFIER", "bob", "bob", lexer2.Span(0, 3), [])),
            (
                "    bob   ",
                0,
                "InputElementRegExp",
                lexer2.Token("IDENTIFIER", "    bob   ", "bob", lexer2.Span(4, 7), []),
            ),
            (
                "    bob   ",
                4,
                "InputElementRegExp",
                lexer2.Token("IDENTIFIER", "    bob   ", "bob", lexer2.Span(4, 7), []),
            ),
            (
                "  \n\r\n  bob   ",
                0,
                "InputElementRegExp",
                lexer2.Token(
                    "IDENTIFIER", "  \n\r\n  bob   ", "bob", lexer2.Span(7, 10), [lexer2.Span(2, 3), lexer2.Span(3, 5)]
                ),
            ),
            (
                "// Happy?\n    bob   ",
                0,
                "InputElementRegExp",
                lexer2.Token("IDENTIFIER", "// Happy?\n    bob   ", "bob", lexer2.Span(14, 17), [lexer2.Span(9, 10)]),
            ),
            (
                "/* Happy?*/    bob   ",
                0,
                "InputElementRegExp",
                lexer2.Token("IDENTIFIER", "/* Happy?*/    bob   ", "bob", lexer2.Span(15, 18), []),
            ),
            (
                "/* Happy?\r\n*/    bob   ",
                0,
                "InputElementRegExp",
                lexer2.Token(
                    "IDENTIFIER", "/* Happy?\r\n*/    bob   ", "bob", lexer2.Span(17, 20), [lexer2.Span(9, 11)]
                ),
            ),
            ("======", 0, "InputElementRegExp", lexer2.Token("===", "======", "===", lexer2.Span(0, 3), [])),
            ("0b0011", 0, "InputElementRegExp", lexer2.Token("NUMERIC", "0b0011", 3, lexer2.Span(0, 6), [])),
            ("0o765", 0, "InputElementRegExp", lexer2.Token("NUMERIC", "0o765", 501, lexer2.Span(0, 5), [])),
            ("0xab", 0, "InputElementRegExp", lexer2.Token("NUMERIC", "0xab", 171, lexer2.Span(0, 4), [])),
            (
                "899.31e-5",
                0,
                "InputElementRegExp",
                lexer2.Token("NUMERIC", "899.31e-5", 0.0089931, lexer2.Span(0, 9), []),
            ),
            ('"string"', 0, "InputElementRegExp", lexer2.Token("STRING", '"string"', "string", lexer2.Span(0, 8), [])),
            ("", 0, "InputElementRegExp", None),
            ("}", 0, "InputElementDiv", lexer2.Token("}", "}", "}", lexer2.Span(0, 1), [])),
            ("/=", 0, "InputElementDiv", lexer2.Token("/=", "/=", "/=", lexer2.Span(0, 2), [])),
            (
                "/(?:)/",
                0,
                "InputElementRegExp",
                lexer2.Token("REGEXP", "/(?:)/", lexer2.RegExp("(?:)", ""), lexer2.Span(0, 6), []),
            ),
        ]
        + [
            (
                f"{start}{test}{finish}",
                0,
                "InputElementRegExpOrTemplateTail",
                lexer2.Token(
                    tokentype,
                    f"{start}{test}{finish}",
                    lexer2.Template(tv, trv),
                    lexer2.Span(0, len(f"{start}{test}{finish}")),
                    [],
                ),
            )
            for start, finish, tokentype in (
                ("`", "`", "NOSUBSTITUTIONTEMPLATE"),
                ("}", "${", "TEMPLATEMIDDLE"),
                ("}", "`", "TEMPLATETAIL"),
            )
            for test, tv, trv in (
                ("", "", ""),
                ("test", "test", "test"),
                ("t", "t", "t"),
                ("_\U00103324_", "_\udbcc\udf24_", "_\udbcc\udf24_"),
                ("$", "$", "$"),
                ("\\'", "'", "\\'"),
                ('\\"', '"', '\\"'),
                ("\\\\", "\\", "\\\\"),
                ("\\b", "\x08", "\\b"),
                ("\\f", "\x0c", "\\f"),
                ("\\n", "\x0a", "\\n"),
                ("\\r", "\x0d", "\\r"),
                ("\\t", "\x09", "\\t"),
                ("\\v", "\x0b", "\\v"),
                ("\\0", "\x00", "\\0"),
                ("\\x67", "\x67", "\\x67"),
                ("\\o", "o", "\\o"),
                ("\\u0041", "A", "\\u0041"),
                ("\\u{41}", "A", "\\u{41}"),
                ("\\u{839140674389}", None, "\\u{839140674389}"),
                ("\\u{ffffff}", None, "\\u{ffffff}"),
                ("a\r\nb", "a\nb", "a\nb"),
                ("abc\\\ndef", "abcdef", "abc\\\ndef"),
                ("\rboo", "\nboo", "\nboo"),
                ("\\07", None, "\\07"),
                ("\\xq", None, "\\xq"),
                ("\\up", None, "\\up"),
                ("\\u0p", None, "\\u0p"),
                ("\\u00p", None, "\\u00p"),
                ("\\u000p", None, "\\u000p"),
            )
        ],
    )
    def test_goodlexing(self, input_src, startpos, goal, expected):
        lex = lexer2.LexerCore(input_src, SyntaxError)
        rv = lex.token(startpos, getattr(lex, goal))
        assert rv == expected

    @pytest.mark.parametrize(
        "input, goal",
        [
            ('"', "InputElementRegExp"),
            ("'", "InputElementTemplateTail"),
            ("3in", "InputElementRegExp"),
            ("0b01uu", "InputElementRegExp"),
            ("0xabh", "InputElementRegExp"),
            ("0o334ttt", "InputElementRegExp"),
            ('"\\u8&&"', "InputElementRegExp"),
            ('"\\u{-128}"', "InputElementRegExp"),
            ('"\\xpp"', "InputElementRegExp"),
        ],
    )
    def test_badlexing(self, input, goal):
        lex = lexer2.LexerCore(input, SyntaxError)
        rv = lex.token(0, getattr(lex, goal))
        assert rv is None


class Test_Lexer:
    def test_Lexer_init(self):
        lex = lexer2.Lexer("I am source text", SyntaxError)
        assert lex.src == "I am source text"
        assert lex.pos == 0
        assert lex.syntax_error_ctor == SyntaxError
        assert isinstance(lex, lexer2.LexerCore)
        assert isinstance(lex, lexer2.Lexer)

    @pytest.mark.parametrize(
        "source_text, where, expected",
        [
            ("This is a long test with many words", 0, "Lexer[pos=0: 'This is a long test with ']"),
            ("This is a long test with many words", 3, "Lexer[pos=3: 's is a long test with man']"),
            ("Short", 3, "Lexer[pos=3: 'rt']"),
            ("line1\nline2\n", 0, "Lexer[pos=0: 'line1\\nline2\\n']"),
        ],
    )
    def test_Lexer_repr(self, source_text, where, expected):
        lex = lexer2.Lexer(source_text, SyntaxError)
        lex.reset_position(where)

        rv = f"{lex!r}"
        assert rv == expected

    def test_position(self):
        lex = lexer2.Lexer("I am source text", SyntaxError)
        assert lex.current_position() == 0
        lex.reset_position(5)
        assert lex.current_position() == 5

    @pytest.mark.parametrize(
        "count, expected",
        [
            (1, lexer2.Token("IDENTIFIER", "one two three", "one", lexer2.Span(0, 3), [])),
            (
                2,
                [
                    lexer2.Token("IDENTIFIER", "one two three", "one", lexer2.Span(0, 3), []),
                    lexer2.Token("IDENTIFIER", "one two three", "two", lexer2.Span(4, 7), []),
                ],
            ),
            (
                3,
                [
                    lexer2.Token("IDENTIFIER", "one two three", "one", lexer2.Span(0, 3), []),
                    lexer2.Token("IDENTIFIER", "one two three", "two", lexer2.Span(4, 7), []),
                    lexer2.Token("IDENTIFIER", "one two three", "three", lexer2.Span(8, 13), []),
                ],
            ),
            (
                4,
                [
                    lexer2.Token("IDENTIFIER", "one two three", "one", lexer2.Span(0, 3), []),
                    lexer2.Token("IDENTIFIER", "one two three", "two", lexer2.Span(4, 7), []),
                    lexer2.Token("IDENTIFIER", "one two three", "three", lexer2.Span(8, 13), []),
                    None,
                ],
            ),
        ],
    )
    def test_peek_token(self, count, expected):
        lex = lexer2.Lexer("one two three", SyntaxError)
        rv = lex.peek_token(count)
        assert rv == expected

    @pytest.mark.parametrize(
        "src, newpos, tok",
        [
            ("      /* Nothing Here */      ", 0, None),
            ("a b", 1, lexer2.Token("IDENTIFIER", "a b", "a", lexer2.Span(0, 1), [])),
        ],
    )
    def test_next_token(self, src, newpos, tok):
        lex = lexer2.Lexer(src, SyntaxError)
        token = lex.next_token()
        assert token == tok
        assert lex.current_position() == newpos

    @pytest.mark.parametrize(
        "src, check, newline, expected, newpos",
        [
            ("* bob = 3;", "*", True, lexer2.Token("*", "* bob = 3;", "*", lexer2.Span(0, 1), []), 1),
            ("* bob = 3;", "&", True, None, 0),
            (
                "\n\n* bob = 3;",
                "*",
                True,
                lexer2.Token("*", "\n\n* bob = 3;", "*", lexer2.Span(2, 3), [lexer2.Span(0, 1), lexer2.Span(1, 2)]),
                3,
            ),
            ("\n\n* bob = 3;", "*", False, None, 0),
        ],
    )
    def test_next_token_if(self, src, check, newline, expected, newpos):
        lex = lexer2.Lexer(src, SyntaxError)
        rv = lex.next_token_if(check, prior_newline_allowed=newline)
        assert rv == expected
        assert lex.current_position() == newpos

    @pytest.mark.parametrize(
        "src, check, newline, expected, newpos",
        [
            (
                "function bob = 3;",
                "function",
                True,
                lexer2.Token("IDENTIFIER", "function bob = 3;", "function", lexer2.Span(0, 8), []),
                8,
            ),
            ("function bob = 3;", "await", True, None, 0),
            (
                "\n\nfunction bob = 3;",
                "function",
                True,
                lexer2.Token(
                    "IDENTIFIER",
                    "\n\nfunction bob = 3;",
                    "function",
                    lexer2.Span(2, 10),
                    [lexer2.Span(0, 1), lexer2.Span(1, 2)],
                ),
                10,
            ),
            ("\n\nfunction bob = 3;", "function", False, None, 0),
        ],
    )
    def test_next_id_if(self, src, check, newline, expected, newpos):
        lex = lexer2.Lexer(src, SyntaxError)
        rv = lex.next_id_if(check, prior_newline_allowed=newline)
        assert rv == expected
        assert lex.current_position() == newpos

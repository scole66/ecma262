from functools import reduce
from itertools import chain
import pytest
from .helpers import *

import ecmascript.ecmascript as ecmascript


class Test_parse_ObjectAssignmentPattern:
    # Syntax
    #   ObjectAssignmentPattern[Yield, Await]:
    #       { }
    #       { AssignmentRestProperty[?Yield, ?Await] }
    #       { AssignmentPropertyList[?Yield, ?Await] }
    #       { AssignmentPropertyList[?Yield, ?Await] , AssignmentRestProperty[?Yield, ?Await]opt }

    OAP_Empty = ecmascript.P2_ObjectAssignmentPattern_Empty
    OAP_ARP = ecmascript.P2_ObjectAssignmentPattern_AssignmentRestProperty
    OAP_APL = ecmascript.P2_ObjectAssignmentPattern_AssignmentPropertyList
    OAP_APL_ARP = ecmascript.P2_ObjectAssignmentPattern_AssignmentPropertyList_AssignmentRestProperty
    productions = (
        (("{", "}"), OAP_Empty),
        (("{", "AssignmentRestProperty", "}"), OAP_ARP),
        (("{", "AssignmentPropertyList", "}"), OAP_APL),
        (("{", "AssignmentPropertyList", ",", "}"), OAP_APL),
        (("{", "AssignmentPropertyList", ",", "AssignmentRestProperty", "}"), OAP_APL_ARP),
    )

    @classmethod
    def setup_prod_mocks(cls, mocker, token_stream, lex_pos):
        return prod_mocks(
            mocker,
            token_stream,
            lex_pos,
            ecmascript.ParseNode2,
            set(p for p in chain.from_iterable(prod for prod, ecls in cls.productions) if p[0].isupper()),
        )

    @pytest.mark.parametrize("strict_flag", (False, True))
    @pytest.mark.parametrize("yield_flag", (False, True))
    @pytest.mark.parametrize("await_flag", (False, True))
    @pytest.mark.parametrize("lex_pos", (0, 10))
    @pytest.mark.parametrize("token_stream, expected_class, guard", prod_streams(productions))
    def test_ordinary(
        self, mocker, context, strict_flag, yield_flag, await_flag, token_stream, expected_class, lex_pos, guard
    ):
        lexer = lexer_mock(mocker, token_stream, lex_pos)
        productions = self.setup_prod_mocks(mocker, token_stream, lex_pos)

        oap = ecmascript.parse_ObjectAssignmentPattern(context, lexer, lex_pos, strict_flag, yield_flag, await_flag)

        assert isinstance(oap, expected_class)
        for name, mock in productions.items():
            if name in token_stream:
                mock.assert_called_with(context, lexer, mocker.ANY, strict_flag, yield_flag, await_flag)
        assert oap.strict == strict_flag
        assert len(oap.children) == len(token_stream)
        for idx, expected in enumerate(token_stream):
            assert oap.children[idx].value == expected

    @pytest.mark.parametrize("strict_flag", (False, True))
    @pytest.mark.parametrize("yield_flag", (False, True))
    @pytest.mark.parametrize("await_flag", (False, True))
    @pytest.mark.parametrize("lex_pos", (0, 10))
    @pytest.mark.parametrize("token_stream", synerror2_streams(prod for prod, ecls in productions))
    def test_syntax_errors(self, mocker, context, strict_flag, yield_flag, await_flag, token_stream, lex_pos):
        lexer = lexer_mock(mocker, token_stream, lex_pos)
        self.setup_prod_mocks(mocker, token_stream, lex_pos)

        oap = ecmascript.parse_ObjectAssignmentPattern(context, lexer, lex_pos, strict_flag, yield_flag, await_flag)

        assert oap is None

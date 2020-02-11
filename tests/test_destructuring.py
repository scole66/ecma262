from functools import reduce
from itertools import chain
import pytest
from .helpers import *

import ecmascript.ecmascript as ecmascript


class Test_parse_ObjectAssignmentPattern(parse_test):
    # Syntax
    #   ObjectAssignmentPattern[Yield, Await]:
    #       { }
    #       { AssignmentRestProperty[?Yield, ?Await] }
    #       { AssignmentPropertyList[?Yield, ?Await] }
    #       { AssignmentPropertyList[?Yield, ?Await] , AssignmentRestProperty[?Yield, ?Await]opt }
    target = staticmethod(ecmascript.parse_ObjectAssignmentPattern)
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
    target_argnames = ("Yield", "Await")
    called_argnames = {
        "AssignmentRestProperty": ("?Yield", "?Await"),
        "AssignmentPropertyList": ("?Yield", "?Await"),
    }

    @ordinary_test_params(target_argnames, productions)
    def test_ordinary(self, context, mocker, token_stream, expected_class, guard, lex_pos, strict_flag, prod_args):
        self.ordinary(mocker, context, token_stream, expected_class, guard, lex_pos, prod_args, strict_flag)

    @syntax_error_test_params(target_argnames, productions)
    def test_syntax_errors(self, mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class):
        self.syntax_errors(mocker, context, strict_flag, prod_args, token_stream, lex_pos, error_class)


####################################################################################
#
#  d888    .d8888b.       d888   888888888      888888888      888888888
# d8888   d88P  Y88b     d8888   888            888            888
#   888          888       888   888            888            888
#   888        .d88P       888   8888888b.      8888888b.      8888888b.
#   888    .od888P"        888        "Y88b          "Y88b          "Y88b
#   888   d88P"            888          888            888            888
#   888   888"       d8b   888   Y88b  d88P d8b Y88b  d88P d8b Y88b  d88P
# 8888888 888888888  Y8P 8888888  "Y8888P"  Y8P  "Y8888P"  Y8P  "Y8888P"
#
#
#
####################################################################################
# ECMAScript Language: Expressions | Assignment Operators | Destructuring Assignment
# 12.15.5.5 | Runtime Semantics: IteratorDestructuringAssignmentEvaluation
####################################################################################
class Test_DestructuringAssignment_IteratorDestructuringAssignmentEvaluation:
    # With parameter iteratorRecord.

    # Elision : ,
    #   1. If iteratorRecord.[[Done]] is false, then
    #       a. Let next be IteratorStep(iteratorRecord).
    #       b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
    #       c. ReturnIfAbrupt(next).
    #       d. If next is false, set iteratorRecord.[[Done]] to true.
    #   2. Return NormalCompletion(empty).
    # Test Cases:
    #   * IteratorRecord.Done == True
    #   - IteratorRecord.Done == False
    #     * IteratorStep(IteratorRecord) returns False
    #     * IteratorStep(IteratorRecord) returns something not False
    #     * IteratorStep(IteratorRecord) raises
    @strict_params
    def test_Elision_Comma_01(self, context, mocker, strict):
        # IteratorRecord.Done == True
        iterator_record = mocker.Mock(Done=True)
        elision = ecmascript.P2_Elision_COMMA(context, strict, [mocker.Mock()])
        rv = elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        assert rv == ecmascript.EMPTY
        assert iterator_record.Done is True

    @strict_params
    def test_Elision_Comma_02(self, context, mocker, strict):
        # IteratorRecord.Done == False; IteratorStep(IteratorRecord) returns something not False
        iterator_record = mocker.Mock(Done=False)
        istep = mocker.patch("ecmascript.ecmascript.IteratorStep", return_value=mocker.sentinel.iterobj)
        elision = ecmascript.P2_Elision_COMMA(context, strict, [mocker.Mock()])
        rv = elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        assert rv == ecmascript.EMPTY
        assert iterator_record.Done is False
        istep.assert_called_with(iterator_record)

    @strict_params
    def test_Elision_Comma_03(self, context, mocker, strict):
        # IteratorRecord.Done == False; IteratorStep(IteratorRecord) returns False
        iterator_record = mocker.Mock(Done=False)
        istep = mocker.patch("ecmascript.ecmascript.IteratorStep", return_value=False)
        elision = ecmascript.P2_Elision_COMMA(context, strict, [mocker.Mock()])
        rv = elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        assert rv == ecmascript.EMPTY
        assert iterator_record.Done is True
        istep.assert_called_with(iterator_record)

    @strict_params
    def test_Elision_Comma_04(self, context, mocker, strict):
        # IteratorRecord.Done == False; IteratorStep(IteratorRecord) raises
        iterator_record = mocker.Mock(Done=False)
        istep = mocker.patch("ecmascript.ecmascript.IteratorStep", side_effect=ecmascript.ESTypeError("Test Error"))
        elision = ecmascript.P2_Elision_COMMA(context, strict, [mocker.Mock()])
        with pytest.raises(ecmascript.ESTypeError, match="Test Error"):
            elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        assert iterator_record.Done is True
        istep.assert_called_with(iterator_record)

    # Elision : Elision ,
    #   1. Perform ? IteratorDestructuringAssignmentEvaluation of Elision with iteratorRecord as the argument.
    #   2. If iteratorRecord.[[Done]] is false, then
    #       a. Let next be IteratorStep(iteratorRecord).
    #       b. If next is an abrupt completion, set iteratorRecord.[[Done]] to true.
    #       c. ReturnIfAbrupt(next).
    #       d. If next is false, set iteratorRecord.[[Done]] to true.
    #   3. Return NormalCompletion(empty).
    # Test Cases:
    #   * After step 1, iteratorRecord.[[Done]] is true
    #   - After step 1, iteratorRecord.[[Done]] is false
    #       * IteratorStep(iteratorRecord) raises
    #       * IteratorStep(iteratorRecord) returns false
    #       * IteratorStep(iteratorRecord) returns anything but false
    @strict_params
    def test_Elision_Elision_Comma_01(self, context, mocker, strict):
        # After step 1, iteratorRecord.[[Done]] is true
        elision_child = mocker.Mock(
            **{"IteratorDestructuringAssignmentEvaluation.return_value": mocker.sentinel.child}
        )
        elision = ecmascript.P2_Elision_Elision_COMMA(context, strict, [elision_child, mocker.Mock()])
        iterator_record = mocker.Mock(Done=True)
        rv = elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        elision_child.IteratorDestructuringAssignmentEvaluation.assert_called_with(iterator_record)
        assert rv == ecmascript.EMPTY
        assert iterator_record.Done is True
        elision_child.IteratorDestructuringAssignmentEvaluation.assert_called_with(iterator_record)

    @strict_params
    def test_Elision_Elision_Comma_02(self, context, mocker, strict):
        # After step 1, iteratorRecord.[[Done]] is false; IteratorStep(iteratorRecord) raises
        elision_child = mocker.Mock(
            **{"IteratorDestructuringAssignmentEvaluation.return_value": mocker.sentinel.child}
        )
        elision = ecmascript.P2_Elision_Elision_COMMA(context, strict, [elision_child, mocker.Mock()])
        iterator_record = mocker.Mock(Done=False)
        istep = mocker.patch("ecmascript.ecmascript.IteratorStep", side_effect=ecmascript.ESTypeError("Test Error"))
        with pytest.raises(ecmascript.ESTypeError, match="Test Error"):
            elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        assert iterator_record.Done is True
        istep.assert_called_with(iterator_record)
        elision_child.IteratorDestructuringAssignmentEvaluation.assert_called_with(iterator_record)

    @strict_params
    def test_Elision_Elision_Comma_03(self, context, mocker, strict):
        # After step 1, iteratorRecord.[[Done]] is false; IteratorStep(iteratorRecord) returns false
        elision_child = mocker.Mock(
            **{"IteratorDestructuringAssignmentEvaluation.return_value": mocker.sentinel.child}
        )
        elision = ecmascript.P2_Elision_Elision_COMMA(context, strict, [elision_child, mocker.Mock()])
        iterator_record = mocker.Mock(Done=False)
        istep = mocker.patch("ecmascript.ecmascript.IteratorStep", return_value=False)
        rv = elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        assert rv == ecmascript.EMPTY
        assert iterator_record.Done is True
        istep.assert_called_with(iterator_record)
        elision_child.IteratorDestructuringAssignmentEvaluation.assert_called_with(iterator_record)

    @strict_params
    def test_Elision_Elision_Comma_04(self, context, mocker, strict):
        # After step 1, iteratorRecord.[[Done]] is false; IteratorStep(iteratorRecord) returns anything but false
        elision_child = mocker.Mock(
            **{"IteratorDestructuringAssignmentEvaluation.return_value": mocker.sentinel.child}
        )
        elision = ecmascript.P2_Elision_Elision_COMMA(context, strict, [elision_child, mocker.Mock()])
        iterator_record = mocker.Mock(Done=False)
        istep = mocker.patch("ecmascript.ecmascript.IteratorStep", return_value=mocker.sentinel.iterobj)
        rv = elision.IteratorDestructuringAssignmentEvaluation(iterator_record)
        assert rv == ecmascript.EMPTY
        assert iterator_record.Done is False
        istep.assert_called_with(iterator_record)
        elision_child.IteratorDestructuringAssignmentEvaluation.assert_called_with(iterator_record)

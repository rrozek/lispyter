import unittest
import logzero
from lispyter import *

logzero.loglevel(logzero.WARNING)

class TestLispy(unittest.TestCase):
    def setUp(self):
        self.lispy = Lispyter()

    def test_list_function_with_one_arg(self):
        self.assertEqual(self.lispy.eval("(list 1)"), [1])

    def test_list_function_with_multiple_args(self):
        self.assertEqual(self.lispy.eval("(list 1 2 3)"), [1, 2, 3])

    def test_list_evaluate_args(self):
        with self.assertRaises(Interpreter.UndefinedSymbolError):
            result = self.lispy.eval("(list 1 2 foo)")

    def test_int_logic(self):
        self.assertEqual(self.lispy.eval("(= 1 1)"), LispBooleanTrue)
        self.assertEqual(self.lispy.eval("(= 1 2)"), LispNil)

    def test_float_logic(self):
        self.assertEqual(self.lispy.eval("(= 1.0 1.0)"), LispBooleanTrue)
        self.assertEqual(self.lispy.eval("(= 1.0 1.1)"), LispNil)

    def test_string_logic(self):
        self.assertEqual(self.lispy.eval('(= "abc" "abc")'), LispBooleanTrue)
        self.assertEqual(self.lispy.eval('(= "abc" "def")'), LispNil)

    def test_sum_with_multiple_numbers(self):
        self.assertEqual(self.lispy.eval("(+ 1 2 3)"), 6)

    def test_sub_numbers(self):
        self.assertEqual(self.lispy.eval("(- 1 2)"), -1)

    def test_mul_with_two_numbers(self):
        self.assertEqual(self.lispy.eval("(* 2 3)"), 6)

    def test_div_with_two_numbers(self):
        self.assertEqual(self.lispy.eval("(/ 6 3)"), 2)

    def test_set_and_get_variable_values(self):
        self.assertEqual(self.lispy.eval("(setq foo 42)"), LispNil)
        self.assertEqual(self.lispy.eval("(get foo)"), 42)

    def test_let_without_instructions(self):
        self.assertEqual(self.lispy.eval("(let ((x 1)))"), LispNil)

    def test_let_with_multiple_instructions(self):
        self.assertEqual(self.lispy.eval("(let ((x 1) (y 2)) (+ x x) (+ x y))"), 3)

    def test_defun_return_variable(self):
        self.assertEqual(self.lispy.eval("(defun foo (x) x)"), LispSymbol("foo"))
        self.assertEqual(self.lispy.eval("(foo 2)"), 2)

    def test_defun_evaluate_expression(self):
        self.assertEqual(self.lispy.eval("(defun foo (x) (+ x 1))"), LispSymbol("foo"))
        self.assertEqual(self.lispy.eval("(foo 2)"), 3)

    def test_defun_does_not_leak_variables(self):
        self.lispy.eval("(defun foo (x y) (- x y))")
        self.assertEqual(self.lispy.eval("(foo 5 2)"), 3)
        with self.assertRaises(Interpreter.UndefinedSymbolError):
            self.lispy.eval("(+ x y)")

    def test_defun_evaluate_multiexpression(self):
        self.assertEqual(
            self.lispy.eval(
                "(defun foo (x) (setq res (+ x 1)) (setq res (+ res 1)) res)"
            ),
            LispSymbol("foo"),
        )
        self.assertEqual(4, self.lispy.eval("(foo 2)"))

    def test_adjoin_eval_simple_args(self):
        self.assertEqual(LispNil, self.lispy.eval("(setq someset ())"))
        self.assertEqual(LispNil, self.lispy.eval("(setq someset (adjoin 1 someset))"))
        self.assertEqual(LispList(LispInt(1)), self.lispy.eval("(get someset)"))
        self.assertEqual(LispNil, self.lispy.eval("(setq someset (adjoin 1 someset))"))
        self.assertEqual(LispList(LispInt(1)), self.lispy.eval("(get someset)"))
        self.assertEqual(LispNil, self.lispy.eval("(setq someset (adjoin 2 someset))"))
        self.assertEqual(
            LispList(LispInt(2), LispInt(1)), self.lispy.eval("(get someset)")
        )

    def test_adjoin_evals_args(self):
        self.assertEqual(LispNil, self.lispy.eval("(setq someset ())"))
        self.assertEqual(
            LispList(LispInt(1)), self.lispy.eval("(adjoin (- 2 1) someset)")
        )

    def test_loop_simple(self):
        # (loop for x in universe do ((lambda(val uni)
        self.assertEqual(LispNil, self.lispy.eval("(setq listcopy ())"))
        self.assertEqual(
            LispNil,
            self.lispy.eval("(setq somelist (cons 1 (cons 2 (cons 3 (cons 4 nil)))))"),
        )
        self.assertEqual(
            LispList(LispInt(1), LispInt(2), LispInt(3), LispInt(4)),
            self.lispy.eval("(get somelist)"),
        )
        self.lispy.eval("(loop for x in somelist do (setq listcopy (cons x listcopy)))")
        self.assertEqual(
            LispList(LispInt(4), LispInt(3), LispInt(2), LispInt(1)),
            self.lispy.eval("(get listcopy)"),
        )

    def test_function_evals_args(self):
        self.lispy.eval("(defun foo (x y) (+ x y))")
        self.assertEqual(self.lispy.eval("(foo (+ 2 3) (- 7 3))"), 9)

    def test_if_evaluate_condition_true(self):
        self.assertEqual(self.lispy.eval("(if (= (+ 1 2) 3) 1 2)"), 1)

    def test_if_evaluate_condition_false(self):
        self.assertEqual(self.lispy.eval("(if (= (+ 1 2) 4) 1 2)"), 2)

    def test_if_evaluate_true_result(self):
        self.assertEqual(self.lispy.eval("(if t (+ 1 2) 0)"), 3)

    def test_if_evaluate_false_result(self):
        self.assertEqual(self.lispy.eval("(if nil 0 (+ 1 2))"), 3)


class TestTypes(unittest.TestCase):
    def test_nil_value(self):
        self.assertEqual(LispNil, None)

    def test_string_value(self):
        self.assertEqual(LispStr("abc"), "abc")

    def test_symbol_value(self):
        self.assertTrue(LispSymbol("foo") == LispSymbol("foo"))
        self.assertTrue(LispSymbol("foo") != LispSymbol("bar"))


class TestParser(unittest.TestCase):
    def setUp(self):
        self.parser = Parser()

    def test_tokenize_with_single_word(self):
        self.assertEqual(self.parser.tokenize("abc"), ["abc"])

    def test_tokenize_returns_all_words(self):
        self.assertEqual(self.parser.tokenize("abc def"), ["abc", "def"])

    def test_tokenize_with_empty_list(self):
        self.assertEqual(self.parser.tokenize("()"), [])

    def test_tokenize_with_elements(self):
        self.assertEqual(self.parser.tokenize("(1 2)"), ["1", "2"])

    def test_nested_lists(self):
        self.assertEqual(
            self.parser.tokenize("((1 2) ((3 4) 5 6))"),
            [["1", "2"], [["3", "4"], "5", "6"]],
        )

    def test_nil(self):
        result = self.parser.parse(["foo", "nil"])[1]
        self.assertEqual(result, LispNil)

    def test_integer(self):
        result = self.parser.parse(["foo", "1"])[1]
        self.assertEqual(result, LispInt(1))

    def test_negative_integer(self):
        result = self.parser.parse(["foo", "-1"])[1]
        self.assertEqual(result, LispInt(-1))

    def test_float(self):
        result = self.parser.parse(["foo", "1.1"])[1]
        self.assertEqual(result, LispFloat(1.1))

    def test_negative_float(self):
        result = self.parser.parse(["foo", "-1.1"])[1]
        self.assertEqual(result, LispFloat(-1.1))

    def test_abc_as_string(self):
        result = self.parser.parse(["foo", '"abc"'])[1]
        self.assertEqual(result, LispStr("abc"))


class TestInterpreter(unittest.TestCase):
    def setUp(self):
        self.interpreter = Interpreter()

    def test_sum_multiple_ints_and_floats(self):
        self.assertEqual(
            self.interpreter.execute(
                LispList(
                    LispSymbol("+"),
                    LispInt(1),
                    LispFloat(2.5),
                    LispInt(-3),
                    LispFloat(-1.25),
                )
            ),
            LispFloat(-0.75),
        )


if __name__ == "__main__":
    unittest.main()

__version__ = "0.0.1"

import argparse
import re
from logzero import logger
import logzero

logzero.loglevel(logzero.WARNING)

class LispyterError(BaseException):
    pass


class Lispyter:
    def __init__(self):
        self.prompt = "lispyter>>> "
        self.sayhi = "hi! (press ctrl+d to exit)"
        self.saybye = "bye!"

        self.parser = Parser()
        self.interpreter = Interpreter()

    def eval(self, string):
        tokens = self.parser.tokenize(string)
        instruction = self.parser.parse(tokens)
        return self.interpreter.execute(instruction)

    def repl(self):
        print(self.sayhi)

        while True:
            try:
                string = input(self.prompt)
                output = self.eval(string)
                print(self._format_output(output))
            except LispyterError as e:
                print("ERROR: {}".format(str(e)))
            except (KeyboardInterrupt, EOFError):
                break

        print(f"\n{self.saybye}")

    def _format_output(self, output):
        return str(output)

    def run_program(self, filename):
        with open(filename) as fd:
            script_code = fd.read()

        open_brackets = 0
        prev_eval_index = 0
        for index, character in enumerate(script_code, start=1):
            if character == "(":
                open_brackets += 1
            elif character == ")":
                open_brackets -= 1
            if open_brackets == 0:
                to_eval = script_code[prev_eval_index:index]
                self.eval(to_eval)
                prev_eval_index = index


class LispList(list):
    def __init__(self, *elements):
        super().__init__(elements)


class LispSymbol(str):
    pass


LispStr = str
LispNil = None
LispBooleanTrue = True
LispInt = int
LispFloat = float
LispAtom = (LispSymbol, LispNil, LispBooleanTrue, LispInt, LispFloat)
LispType = (LispSymbol, LispNil, LispBooleanTrue, LispList, LispInt, LispFloat)


class Parser:
    class InvalidInputError(LispyterError):
        pass

    def __init__(self):
        self.type_parser = {
            "nil": {"regex": r"^(nil)$", "parser": lambda x: LispNil},
            "t": {"regex": r"^(t)$", "parser": lambda x: LispBooleanTrue},
            "integer": {
                "regex": r"^(-?[0-9]+)$",
                "parser": lambda x: LispInt(int(x)),
            },
            "float": {
                "regex": r"^(-?[0-9]+\.?[0-9]*)$",
                "parser": lambda x: LispFloat(float(x)),
            },
            "string": {"regex": r'^"(.*)"$', "parser": lambda x: LispStr(x)},
        }
        self.types = self.type_parser.keys()

    def tokenize(self, string):
        string = string.replace("\n", " ")

        if string[0] == "(":
            return self._tokenize_list(list(string))
        else:
            return self._tokenize_words(list(string))

    def parse(self, tokens):
        logger.debug(f'parse. tokens: {tokens}')
        if not tokens:
            return LispNil

        result = []

        for token in tokens:
            if type(token) == list:
                result.append(self.parse(token))
            else:
                result.append(self._parse_token(token))
        logger.debug(f"parse: {tokens} result: {result}")
        return LispList(*result)

    def _tokenize_list(self, chars):
        result = []
        _ = chars.pop(0)  # ignore first '('

        while chars:
            char = chars.pop(0)

            if char == ")":
                logger.debug(f'_tokenize_list ret {result}')
                return result
            elif char == "(":
                chars.insert(0, char)
                result.append(self._tokenize_list(chars))
            else:
                chars.insert(0, char)
                result += self._tokenize_words(chars)

        raise self.InvalidInputError(f'Invalid input "{"".join(chars)}"')

    def _tokenize_words(self, chars):
        words_delimiters = "()"
        words_ignores = " "
        result = []

        while chars:
            char = chars.pop(0)

            if char in words_delimiters:
                chars.insert(0, char)
                break
            elif char in words_ignores:
                continue
            else:
                chars.insert(0, char)
                result.append(self._tokenize_word(chars))

        logger.debug(f'_tokenize_words ret {result}')
        return result

    def _tokenize_word(self, chars):
        word_delimiters = " ()"
        result = []

        while chars:
            char = chars.pop(0)

            if char in word_delimiters:
                chars.insert(0, char)
                break
            else:
                result.append(char)

        logger.debug(f'_tokenize_word ret {result}')
        return "".join(result)

    def _parse_token(self, token):
        for type in self.types:
            regex = self.type_parser[type]["regex"]
            result = re.match(regex, token)

            if result:
                parser = self.type_parser[type]["parser"]
                return parser(result.group(1))

        return LispSymbol(token)


class Interpreter:
    class UndefinedSymbolError(LispyterError):
        pass

    class UndefinedFunctionError(LispyterError):
        pass

    class UndefinedVariableError(LispyterError):
        pass

    def __init__(self):
        self.global_variable_context = {}
        self.local_variable_contexts = []

        self.special_functions = {
            LispSymbol("defun"): self._defun,
            LispSymbol("if"): self._if,
            LispSymbol("let"): self._let,
            LispSymbol("setq"): self._setq,
            LispSymbol("get"): self._get,
            LispSymbol("adjoin"): self._adjoin,
            LispSymbol("subsetp"): self._subsetp,
            LispSymbol("loop"): self._loop,
            LispSymbol("write"): self._write,
            LispSymbol("prin1"): self._write_inline,
            LispSymbol("terpri"): self._add_linebreak,
        }
        self.regular_functions = {
            LispSymbol("list"): self._list,
            LispSymbol("="): self._equal,
            LispSymbol("+"): self._sum,
            LispSymbol("-"): self._sub,
            LispSymbol("*"): self._mul,
            LispSymbol("/"): self._div,
            LispSymbol("cons"): self._cons,
        }
        self.functions = {**self.special_functions, **self.regular_functions}

    def execute(self, instruction):
        if instruction.__class__ in [LispSymbol, LispInt, LispFloat, LispStr]:
            raise self.UndefinedSymbolError('Undefined symbol "{}"'.format(instruction))

        if instruction == LispNil:
            return LispNil

        if instruction.__class__ == LispList:
            function_name = instruction[0]
            args = instruction[1:]

            if function_name == LispNil:
                return LispNil

            if isinstance(function_name, LispBooleanTrue.__class__):
                return LispBooleanTrue

            if function_name in self.regular_functions:
                args = self._evaluate_elements(args)

            if function_name in self.functions:
                function = self.functions[function_name]
                result = function(*args)
                return result if result != None else LispNil

        raise self.UndefinedFunctionError(
            'Undefined function "{}"'.format(function_name)
        )

    def _evaluate_elements(self, elements):
        return [self._evaluate_element(element) for element in elements]

    def _evaluate_element(self, element):
        if element.__class__ == LispList:
            return self.execute(element)
        elif element.__class__ == LispSymbol:
            return (
                self._get_variable(element)
                if self._is_variable(element)
                else self.execute(element)
            )
        else:
            return element

    def _get_variable(self, name):
        local_variable_context = self._find_local_variable_context(name)
        if local_variable_context is not None:
            return local_variable_context[name]
        elif self._is_global_variable(name):
            return self._get_global_variable(name)
        raise self.UndefinedVariableError('Undefined variable "{}"'.format(name))

    def _is_variable(self, name):
        return self._is_global_variable(name) or self._is_local_variable(name)

    def _get_global_variable(self, name):
        return self.global_variable_context[name]

    def _set_global_variable(self, name, value):
        self.global_variable_context[name] = value

    def _is_global_variable(self, name):
        return name in self.global_variable_context

    def _get_local_variable(self, name):
        local_variable_context = self._find_local_variable_context(name)
        if local_variable_context is not None:
            return local_variable_context[name]
        raise self.UndefinedVariableError('Undefined local variable "{}"'.format(name))

    def _set_local_variable(self, name, value):
        self.local_variable_contexts[0][name] = value

    def _is_local_variable(self, name):
        return self._find_local_variable_context(name) is not None

    def _find_local_variable_context(self, name):
        for local_variable_context in self.local_variable_contexts:
            if name in local_variable_context:
                return local_variable_context
        return None

    def _create_local_variable_context(self):
        self.local_variable_contexts.insert(0, {})

    def _delete_local_variable_context(self):
        self.local_variable_contexts.pop(0)

    def _evaluate_if_list(self, param):
        return self.execute(param) if param.__class__ == LispList else param

    # Functions
    def _list(self, *args):
        if not len(args):
            return LispNil
        return LispList(*args)

    def _setq(self, name, value):
        self._set_global_variable(name, self._evaluate_if_list(value))

    def _cons(self, value, list):
        if not list:
            return LispList(value)
        return LispList(value, *list)

    def _get(self, name):
        return self._get_global_variable(name)

    def _equal(self, x, y):
        return LispBooleanTrue if x == y else LispNil

    def _sum(self, *args):
        return sum([a for a in args])

    def _sub(self, x, y=None):
        if y:
            return x - y
        else:
            return x * -1

    def _mul(self, *args):
        result = 1
        for arg in args:
            result *= arg
        return result

    def _div(self, x, y):
        if y == 0:
            raise ArithmeticError("just please, dont.")
        return LispFloat(x / y)

    def _let(self, var_defs, *instructions):
        self._create_local_variable_context()
        for name, value in var_defs:
            self._set_local_variable(name, value)

        result = LispNil
        for instruction in instructions:
            result = self._evaluate_element(instruction)
        self._delete_local_variable_context()

        return result

    def _defun(self, function_name, arg_names, *instructions):
        def function(*arg_values):
            values = [self._evaluate_if_list(a) for a in arg_values]
            var_defs = zip(arg_names, values) if arg_names else []
            return self._let(var_defs, *instructions)

        self.functions[function_name] = function
        return function_name

    def _if(self, condition, true_expr, false_expr=LispNil):
        condition_result = self._evaluate_if_list(condition)

        if condition_result != LispNil:
            result = self._evaluate_element(true_expr)
        else:
            result = self._evaluate_element(false_expr)

        return result

    def _write(self, arg):
        print(self._evaluate_if_list(arg), end="\n")
        return LispNil

    def _write_inline(self, arg):
        print(self._evaluate_if_list(arg), end="")
        return LispNil

    def _add_linebreak(self):
        return self._write("")

    def _adjoin(self, item, target):
        target_list = self._get_variable(target)
        result = self._evaluate_element(item)
        if isinstance(result, int):
            result = LispInt(result)
        if target_list:
            if result in target_list:
                return target_list
            return LispList(result, *target_list)
        return LispList(result)

    def _subsetp(self, list1, list2):
        l1 = self._evaluate_element(list1)
        while isinstance(l1, LispSymbol):
            l1 = self._evaluate_element(l1)
        l2 = self._evaluate_element(list2)
        while isinstance(l2, LispSymbol):
            l2 = self._evaluate_element(l2)
        if not l2:
            return LispNil
        result = set(l1).issubset(set(l2))
        return result if result else LispNil

    def _loop(self, _for, varname, _in, iteratee, _do, *instructions):
        self._create_local_variable_context()
        iteration_source = self._evaluate_element(iteratee)

        if isinstance(iteration_source, LispSymbol):
            iteration_source = self._get_variable(iteration_source)
        if isinstance(iteration_source, LispInt):
            iteration_source = range(iteration_source)
        if isinstance(iteration_source, LispList):
            iteration_source = iteration_source
        result = LispNil
        if not iteration_source:
            return result
        for i in iteration_source:
            self._set_local_variable(varname, i)

            for instruction in instructions:
                result = self._evaluate_element(instruction)
        self._delete_local_variable_context()
        return result


if __name__ == "__main__":
    arg_parser = argparse.ArgumentParser()
    group = arg_parser.add_mutually_exclusive_group(required=True)
    group.add_argument("-repl", action="store_true", help="Interactive LISP console")
    group.add_argument("-execute", help="Execute LISP script")
    args = arg_parser.parse_args()

    if args.execute:
        Lispyter().run_program(args.execute)
    else:
        Lispyter().repl()

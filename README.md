# lispyter

Superbasic LISP interpreter enabling Jupyter-alike LISP code evaluation along with LISP script execution.  
Supported set of functions guarantees Turing-completness.
LISP syntax basics taken from http://clhs.lisp.se/Front/StartPts.htm

Minimum set of operations required for an interpreter to be Turing-complete imperative language includes:  
* conditionals
* loops
* variables
* arithmetics

### Features  
list of supported functions: 
```python
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
```
Why this and not the other? For pure turing completness we just need some conditionals, variables and a loop.  
My implementation also contains ways to print the output from the script and define standalone functions. 
It also provies way to mimic pythons `set` container with adjoin/subsetp methods. 

### How to use it
Lispyter works in two modes. `-repl`
```
$ python lispyter.py -repl
hi! (press ctrl+d to exit)
lispyter>>> (+ 4 2)
6
lispyter>>> (* 8 8)
64
lispyter>>> 
bye!
```
 or in script running mode `-execute FILE`
```
$ python lispyter.py -execute 110.lisp
00000000000000000001
00000000000000000011
00000000000000000111
00000000000000001101
00000000000000011111
00000000000000110001
00000000000001110011
00000000000011010111
00000000000111111101
00000000001100000111
00000000011100001101
00000000110100011111
00000001111100110001
00000011000101110011
00000111001111010111
00001101011001111101
00011111111011000111
00110000001111001101
01110000011001011111
11010000111011110001
```
to see the equivalent in python:
```
$ python 110.py
00000000000000000001
00000000000000000011
00000000000000000111
00000000000000001101
00000000000000011111
00000000000000110001
00000000000001110011
00000000000011010111
00000000000111111101
00000000001100000111
00000000011100001101
00000000110100011111
00000001111100110001
00000011000101110011
00000111001111010111
00001101011001111101
00011111111011000111
00110000001111001101
01110000011001011111
11010000111011110001
```
in order to run tests type
```
$ python tests.py
........................................
----------------------------------------------------------------------
Ran 40 tests in 0.004s

OK
```

### Ok, fine, but why...
This project was created to fulfill requirement of writing Turing Complete LISP interpreter.  
A few days ago I had no idea what LISP syntax looks like, what are the LISP flavours and how to write code interpreters whatsoever.  
I also had only blurry idea of what Turing-completness actually means. Did some research and here's the brief summary.  
The best way to prove your language/interpreter is Turing complete is to implement Universal Turing Machine   
(  
https://www.youtube.com/watch?v=RPQD7-AOjMI  
https://www.youtube.com/watch?v=dNRDvLACg5Q  
)  
During my deepdive into wikipedia I found out, that there is something much simpler then Game of Life  
which is also Turing complete and yet still much more interesting and fun then Universal Machine. There are Elementary Cellular Automatons  
(https://rosettacode.org/wiki/Elementary_cellular_automaton https://en.wikipedia.org/wiki/Elementary_cellular_automaton#Rule_110)  
in various flavours, with one of them being particularly interesting - Rule110. It is mathematically proven,
that Rule110 in itself it Turing complete. So if i could create a parser which will read a script in LISP executing 
Rule110 automaton, that would clearly prove that my RROZEK-LISP flavour is Turing Complete.  
In order to make things easier I wanted to mimic the exact implementation from python (with the use of sets) to make it easier to compare python script with (extremely naive) LISP script equivalent.

### Hall of Fame
* Thank you to @CarlosLunaMota for https://github.com/CarlosLunaMota/Rule110
* Thank you to @matheusportela for https://github.com/matheusportela/lispy/
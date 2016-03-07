:- consult(assignment3).

% Test utilities
interpret(TokenList, N) :- parse(TokenList, AST), evaluate(AST, N).

printres(N, M) :- N =:= M, write('pass\n').
printres(_, _) :- write('FAIL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n').
printres(Ans, Got, Err) :- Low is Ans - Err, High is Ans + Err, Low < Got, Got < High, write('pass\n').
printres(_, _, _) :- printres(1, 2).

printtoks(Toks) :- write('Testing '), write(Toks), write(' ').
printge(Got,Expected) :- write('Got '), write(Got), write(' expected '), write(Expected), write(' ... '), printres(Got, Expected).
printge(Got,Expected,Err) :- write('Got '), write(Got), write(' expected '), write(Expected), write(' within '), write(Err), write(' ... '), printres(Got, Expected, Err).

i_testcase(Toks, Ans) :- interpret(Toks, N), printge(N, Ans).
i_testcase(_,_) :- write('FAIL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n').

pf_testcase(Toks) :- \+parse(Toks, _), write('pass\n').
pf_testcase(_) :- write('FAIL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n').

testcase(interpret, Toks, Ans) :- printtoks(Toks), i_testcase(Toks, Ans).
testcase(parsefail, Toks) :- printtoks(Toks), write('expecting parse fail ... '), pf_testcase(Toks).
testcase(evalfail, Toks) :- printtoks(Toks), write('expecting eval fail ... '), parse(Toks, AST), write('parse good ... '), \+evaluate(AST, _), write('pass\n').
testcase(evalfail, _) :- write('FAIL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n').
testcase(approx, Toks, Ans, Err) :- printtoks(Toks), interpret(Toks, Res), printge(Res, Ans, Err).

tc(A,B,C,D) :- testcase(A,B,C,D).
tc(A,B,C) :- testcase(A,B,C).
tc(A,B) :- testcase(A,B).

% Test some simple structures: Returning integers and floating points
test_simple :- write('\nTesting simples...\n'),
	       tc(interpret, ['return', 5, '.'], 5),
	       tc(interpret, ['return', 14.6, '.'], 14.6),
	       tc(interpret, ['return', '1', '.'], 1).

% Test setting variables and returning them.
test_var :- write('\nTesting var...\n'),
	    tc(interpret, ['var', 'foo', ';', 'foo', ':=', 6, ';', 'return', 'foo', '.'], 6),
	    tc(evalfail, ['foo', ':=', 6, ';', 'return', 'foo', '.']).

% Test order of operations and associativity
test_oop :- write('\nTesting operations and associativity...\n'),
	    tc(interpret,['var', 'foo', ';', 'foo', ':=', '(', 10, '/', 2, ')', ';', 'return', 'foo', '.'], 5),
	    tc(interpret,['var', 'foo', ';', 'foo', ':=', '(', 10, '-', 2, ')', ';', 'return', 'foo', '.'], 8),
	    tc(interpret,['var', 'foo', ';', 'foo', ':=', '(', 10, '/', 2, '/', 5, ')', ';', 'return', 'foo', '.'],1),
	    tc(interpret,['var', 'foo', ';', 'foo', ':=', '(', 10, '-', 2, '-', 5, ')', ';', 'return', 'foo', '.'], 3),
	    tc(interpret,['var', 'foo', ';', 'foo', ':=', '(', 10, '+', 2, '-', 3, ')', ';', 'return', 'foo', '.'], 9),
	    tc(interpret,['var', 'foo', ';', 'foo', ':=', '(', 10, '*', 2, '-', 3, ')', ';', 'return', 'foo', '.'], 17).

% Test conditionals
test_cond :- write('\nTesting conditionals...\n'),
	     tc(interpret,[var, x, ;, x, :=, 1, ;, if, '(', x, ==, 1, ')', then, x, :=, 2, '.', else, x, :=, 1, '.', endif, ;, return, x, '.'],2),
	     tc(interpret,[var, x, ;, x, :=, 1, ;, if, '(', x, ==, 1, ')', then, x, :=, 2, ;, x, :=, '(', x, +, 1, ')', ., else, x, :=, 1, '.', endif, ;, return, x, '.'],3),
	     tc(interpret,[var, x, ;, x, :=, 1, ;, if, '(', x, ==, 2, ')', then, x, :=, 2,  '.', else, x, :=, 1, ;, x, :=, 8, '.', endif, ;, return, x, '.'],8).


% Test loops
test_loop :- write('\nTesting loops...\n'),
	     tc(interpret,[var, x, ;, x, :=, 1, ;, var, n, ;, n, :=, 1, ;, while, '(', n, '<', 10, ')', do, n, :=, '(', n, '+', 1, ')', ;, x, :=, '(', x, '*', 2, ')', '.', done, ;, if, '(', x, ==, 1, ')', then, x, :=, 2, '.', else, x, :=, x, '.', endif, ;, return, x, '.'],512),
	     tc(interpret, [var, x, ;, x, :=, 1, ;, var, n, ;, n, :=, 1, ;, if, '(', n, '==', 1, ')', then, while, '(', n, '<', 10, ')', do, n, :=, '(', n, '+', 1, ')', ;, x, :=, '(', x, '*', 2, ')', '.', done, '.', else, x, :=, 0, '.', endif, ;, return, x, '.'], 512).

% Test functions
% TODO: Test functions with side effects within conditionals, args, etc.
test_func :- write('\nTesting functions...\n'),
	     tc(interpret, [function, fn, '(', x, ')', '{', return, x, '.', '}', ;, return, fn, '(', 5, ')', '.'], 5),
	     tc(interpret, [function, fn, '(', x, ')', '{', function, fn2, '(', y, ')', '{', return, '(', x, +, y, ')', '.', '}', ;, return, fn2, '(', '(', 3, +, x, ')', ')', '.', '}', ;, return, fn, '(', 5, ')', '.'], 13),
	     % Variable names same as function names
	     tc(interpret, [var, x, ;, x, :=, 1, ;, function, x, '(', x, ')', '{', function, x, '(', x, ')', '{', return, '(', x, +, 1, ')', '.', '}', ';', return, '(', x, '(', x, ')', +, 10, ')', '.', '}', ';', return, x, '(', x, ')', .], 12),
	     % Conditional scope influencing global scope
	     tc(interpret, [var, x, ;, var, y, ;, x, :=, 1, ;, function, f, '(', y, ')', '{', x, :=, '(', x, +, 1, ')', ;, return, y, '.', '}', ;, if, '(', f, '(', x, ')', ==, 2, ')', then, y, :=, 10, ., else, y, :=, 20, ., endif, ;, return, x, .], 2),
	     % Expression scope influencing global scope
	     tc(interpret, [var, x, ;, x, :=, 1, ;, function, f, '(', y, ')', '{', x, :=, '(', x, +, 1, ')', ;, return, x, '.', '}', ;, return, '(', f, '(', x, ')', +, f, '(', x, ')', +, f, '(', x, ')', ')', .], 9),
	     tc(interpret, [var, x, ;, x, :=, 1, ;, function, f, '(', y, ')', '{', x, :=, '(', x, +, 1, ')', ;, return, x, '.', '}', ;, return, '(', '(', f, '(', x, ')', +, f, '(', x, ')', +, f, '(', x, ')', ')', *, f, '(', x, ')', ')', .], 45),
	     tc(interpret, [var, x, ;, x, :=, 1, ;, var, i, ;, i, :=, 1, ;, function, f, '(', x, ')', '{', i, :=, '(', i, +, 1, ')', ;, return, i, ., '}', ;, while, '(', f, '(', i, ')', <, 10, ')', do, x, :=, '(', x, *, x, ')', ., done, ;, return, x, .], 1),
         tc(evalfail, [function, f, '(', x, ')', '{', return, x, ., '}', ;, function, f, '(', x,  ')','{', return, x, ., '}', ;, return, f,'(', 2, ')', '.']),
         tc(interpret, [function, f, '(', x, ')', '{', function, f, '(', x,  ')','{', return, x, ., '}',; , return, x, .,'}', ;, return, f,'(', 2, ')', '.'],2),
         tc(interpret, [function, f, '(', x, ')', '{', return, x, .,'}', ;, function, g, '(', x, ')','{', return, 3, ., '}',; ,return, f,'(',g,'(', 2, ')', ')', '.'],3),
         tc(interpret, ['function', 'fib', '(', 'x', ')', '{', 'var', 'y', ';', 'if', '(', 'x', '<=', 1, ')', 'then', 'y', ':=', 1, '.', 'else', 'y', ':=', '(', 'fib', '(', '(', 'x', '-', 1, ')', ')', '+', 'fib', '(', '(', 'x', '-', 2, ')', ')', ')', '.', 'endif', ';', 'return', 'y', '.', '}', ';', 'return', 'fib', '(', 5, ')', '.'], 8).

% Test scope
test_scope :- write('\nTesting scope...\n'),
	      tc(interpret, [var, n, ;, n, :=, 1, ;, var, y, ;, y, :=, 1, ;, while, '(', n, '<', 10, ')', do, var, y, ;, y, :=, 2, ;, n, :=, '(', n, '+', 1, ')', '.', done, ;, return, y, '.'], 1),
          tc(interpret, [var, y, ;, y, :=, 10, ;, function, f, '(', x, ')', '{', return, y, '.', '}', ;, function, g, '(', x, ')', '{', var, y, ;, y, :=, 5, ;, return, f, '(', 2, ')', '.', '}', ;, return, g, '(', 1, ')', '.'],10),
          tc(interpret, [function, f, '(', x, ')', '{', function, f, '(', x,  ')','{', return, 4, ., '}',; , return, f, '(', x, ')', .,'}', ;, return, f,'(', 2, ')', '.'],4),
          tc(interpret, [var, z, ;, z, :=, 11, ;, function, f, '(', x,  ')','{', return, 4, ., '}', ;, if, '(', 1, '>', 10, ')', then, z, :=, 3, ., else, z, :=, f, '(', z, ')', ., endif, ;, return, z, .],4).

% Some comprehensive tests
test_comp :- write('\nTesting comprehensive...\n'),
	     tc(interpret,['var', 'foo', ';',
			  'foo', ':=', '(', 1, '+', 2.5, ')', ';',
                          'return', '(', 'foo', '*', 5, ')', '.'], 17.5),
	     tc(interpret,['var', 'foo', ';', 'foo', ':=', '(', 1, '+', 2.5, ')', ';', 'return', 'foo', '.'], 3.5),
	     tc(interpret,['var', 'x', ';', 'x',':=', '(', 5, '*', 2, ')', ';', 'return', '(', 'x', '+', 1, ')', '.'], 11),
	     tc(interpret,['var', 'foo', ';', 
                           'var', 'bar', ';',
                           'var', 'lol', ';',
                           'foo', ':=', 1, ';',
                           'bar', ':=', 'foo', ';',
                           'lol', ':=', 'bar', ';',
                           'return', 'lol', '.'], 1),
	     tc(approx, ['function', 'newton_sqrt', '(', 'x', ')', '{', 'var', 'next', ';', 'next', ':=', 'x', ';', 'var', 'this', ';', 'this', ':=', 'x', ';', 'var', 'diff', ';', 'diff', ':=', 1.0, ';', 'while', '(', 'diff', '>', 0.01, ')', 'do', 'var', 'tmp', ';', 'tmp', ':=', '(', 'this', '-', '(', '(', 'this', '*', 'this', '-', 'x', ')', '/', '(', 2.0, '*', 'this', ')', ')', ')', ';', 'this', ':=', 'next', ';', 'next', ':=', 'tmp', ';', 'diff', ':=', '(', 'next', '*', 'next', '-', 'x', ')', '.', 'done', ';', 'return', 'next', '.', '}', ';', 'return', 'newton_sqrt', '(', 16.0, ')', '.'], 4, 0.001),
	     tc(interpret, ['var', 'mod_op', ';', 'function', 'mod', '(', 'a', ')', '{', 'while', '(', 'a', '>', 'mod_op', ')', 'do', 'a', ':=', '(', 'a', '-', 'mod_op', ')', '.', 'done', ';', 'return', 'a', '.', '}', ';', 'mod_op', ':=', 100, ';', 'return', 'mod', '(', 8745, ')', '.'], 45),
         %tc(interpret, ['var', 'mod_op', ';', 'function', 'mod', '(', 'a', ')', '{', 'while', '(', 'a', '>', 'mod_op', ')', 'do', 'a', ':=', '(', 'a', '-', 'mod_op', ')', '.', 'done', ';', 'return', 'a', '.', '}', ';', 'mod_op', ':=', 100, ';', 'function', 'f', '(', 'x', ')', '{', 'mod_op', ':=', 10, ';', 'return', 'mod', '(', 15, ')', '.', '}', ';', 'function', 'g', '(', 'x', ')', '{', 'var', 'mod_op', ';', 'mod_op', ':=', 10, ';', 'return', 'mod', '(', 15, ')', '.', '}', ';', 'return', 'g', '(', 1, ')', '.'], 15),
	     tc(interpret, ['var', 'mod_op', ';', 'function', 'mod', '(', 'a', ')', '{', 'while', '(', 'a', '>', 'mod_op', ')', 'do', 'a', ':=', '(', 'a', '-', 'mod_op', ')', '.', 'done', ';', 'return', 'a', '.', '}', ';', 'mod_op', ':=', 100, ';', 'function', 'f', '(', 'x', ')', '{', 'mod_op', ':=', 10, ';', 'return', 'mod', '(', 15, ')', '.', '}', ';', 'function', 'g', '(', 'x', ')', '{', 'var', 'mod_op', ';', 'mod_op', ':=', 10, ';', 'return', 'mod', '(', 15, ')', '.', '}', ';', 'return', 'f', '(', 1, ')', '.'], 5),
	     tc(interpret, ['var', 'a', ';', 'a', ':=', 5, ';', 'function', 'f', '(', 'y', ')', '{', 'return', '(', 'y', '*', 'a', ')', '.', '}', ';', 'function', 'g', '(', 'x', ')', '{', 'return', 'f', '(', 'x', ')', '.', '}', ';', 'return', 'g', '(', 3, ')', '.'], 15),
	     tc(approx, ['function', 'newton_sqrt', '(', 'x', ')', '{', 'var', 'next', ';', 'next', ':=', 'x', ';', 'var', 'this', ';', 'this', ':=', 'x', ';', 'var', 'diff', ';', 'diff', ':=', 1.0, ';', 'while', '(', 'diff', '>', 0.01, ')', 'do', 'var', 'tmp', ';', 'tmp', ':=', '(', 'this', '-', '(', '(', 'this', '*', 'this', '-', 'x', ')', '/', '(', 2.0, '*', 'this', ')', ')', ')', ';', 'this', ':=', 'next', ';', 'next', ':=', 'tmp', ';', 'diff', ':=', '(', 'next', '*', 'next', '-', 'x', ')', '.', 'done', ';', 'return', 'next', '.', '}', ';', 'var', 'mod_op', ';', 'function', 'mod', '(', 'a', ')', '{', 'while', '(', 'a', '>', 'mod_op', ')', 'do', 'a', ':=', '(', 'a', '-', 'mod_op', ')', '.', 'done', ';', 'return', 'a', '.', '}', ';', 'mod_op', ':=', 100, ';', 'function', 'iveclen', '(', 'v', ')', '{', 'var', 'x', ';', 'var', 'y', ';', 'var', 'z', ';', 'x', ':=', 'mod', '(', 'v', ')', ';', 'mod_op', ':=', 10000, ';', 'y', ':=', '(', '(', 'mod', '(', 'v', ')', '-', 'x', ')', '/', 100, ')', ';', 'z', ':=', '(', '(', 'v', '-', '(', 'y', '+', 'x', ')', ')', '/', 10000, ')', ';', 'return', 'newton_sqrt', '(', '(', '(', 'x', '*', 'x', ')', '+', '(', 'y', '*', 'y', ')', '+', '(', 'z', '*', 'z', ')', ')', ')', '.', '}', ';', 'return', 'iveclen', '(', 101010, ')', '.'], 17.377, 0.01).

% Source code for various functions:
/*
function newton_sqrt(x) {
  var next;
  next := x;
  var this;
  this := x;
  var diff;
  diff := 1.0;
  while (diff > 0.01) do
    var tmp;
    tmp := (this - ((this * this - x) / (2.0 * this)));
    this := next;
    next := tmp;
    diff := (next * next - x).
  done;
  return next.
};

var mod_op;
function mod(a) {
  while (a > mod_op) do
    a := (a - mod_op).
  done;
  return a.
};

function iveclen(v) {
  var x;
  var y;
  var z;
  x := mod(v);
  mod_op := 10000;
  y := ((mod(v) - x) / 100);
  z := ((v - (y + x)) / 10000);
  return newton_sqrt(((x*x) + (y*y) + (z*z))).
};
*/

% Check to make sure that we crash in certain illegal cases
test_reject :- write('\nTesting rejects...\n'),
	       tc(parsefail, ['return', 5]),
	       tc(parsefail, ['var', 'foo', ';', 'foo', ':=', 5, '+', 6, '.']),
	       tc(parsefail, ['return', 5, '+', 6, '.']),
	       tc(parsefail, ['var', 'foo', ';',
			      'foo', ':=', '5', '+', '6', ';',
			      'return', 'foo', '.']),
	       tc(parsefail, ['var', 'var', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', 'x', ';', 'return', 'var', '.']),
	       tc(parsefail, ['var', '+', ';', 'return', '+', '.']),
	       tc(evalfail, ['var', 'x', ';', 'y', ':=', 1, ';', 'return', 1, '.']),
	       tc(evalfail, ['var', 'x', ';', 'var', 'x', ';', 'return', 1, '.']),
	       tc(evalfail, [var, x, ;, x, :=, 1, ;, var, i, ;, i, :=, 1, ;, function, f, '(', x, ')', '{', i, :=, '(', i, +, 1, ')', ;, return, i, ., '}', ;, while, '(', f, '(', i, ')', <, 10, ')', do, var, y, ;, y, :=, 1, ;, x, :=, '(', x, *, x, ')', ., done, ;, return, y, .]),
	       tc(parsefail, [function, fn, '(', x, ')', '{', function, fn2, '(', y, ')', '{', return, '(', x, +, y, ')', '.', '}', ;, return, fn, '(', 3, +, x, ')', '.', '}', ;, return, f, '(', 5, ')', '.']),
	       tc(evalfail, ['function', 'f', '(', 'x', ')', '{', 'return', 'x', '.', '}', ';', 'function', 'f', '(', 'y', ')', '{', 'return', '(', 'y', '+', 1, ')', '.', '}', ';', 'return', 'f', '(', 5, ')', '.']),
	       tc(evalfail, ['var', 'a', ';', 'a', ':=', 5, ';', 'function', 'f', '(', 'y', ')', '{', 'var', 'a', ';', 'return', '(', 'y', '*', 'a', ')', '.', '}', ';', 'return', 'f', '(', 3, ')', '.']).

% Check to make sure that we crash in certain illegal cases
test_keywords :- write('\nTesting keywords...\n'),
	       tc(parsefail, ['var', 'var', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', 'function', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', ';', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', ':=', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '.', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '(', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', ')', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '[', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', ']', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '{', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '}', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', 'return', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', 'if', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', 'then', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', 'else', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', 'endif', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', 'while', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', 'do', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', 'done', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '+', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '-', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '*', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '/', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '==', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '!=', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '<', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '>', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '>=', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '<=', ';', 'return', 5, '.']),
	       tc(parsefail, ['var', 3, ';', 'return', 5, '.']),
	       tc(parsefail, ['var', '3', ';', 'return', 5, '.']),
               tc(parsefail, [function, 'var','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, 'function','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, ';','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, ':=','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '.','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '(','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, ')','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '[','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, ']','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '{','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '}','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, 'return','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, 'if','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, 'then','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, 'else','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, 'endif','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, 'while','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, 'do','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, 'done','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '+','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '-','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '*','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '/','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '==','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '!=','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '<','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '>','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '>=','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '<=','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, '3','(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
               tc(parsefail, [function, 3,'(', y, ')', '{', return, 1, '.', '}', ;, 'return', 5, '.']),
	       tc(parsefail, [var, a, ;, 3, :=, 3, ;, return, 5, '.']).


% The set of 'official' tests...
itestcases :- write('\nTesting the official tests...\n'),
	      tc(interpret, [return,1,'.'], 1),
	      tc(interpret, [return,1.0,'.'], 1),
	      tc(parsefail, [return,'(',1,'.']),
	      tc(parsefail, [return,1,')','.']),
	      tc(interpret, [return,'(',42,')','.'], 42),
	      tc(interpret, [return,'(','(',42,')',')','.'], 42),
	      tc(parsefail, [return,'(',42,'(',')',')','.']),
	      tc(interpret, [return,'(',1,+,1,')','.'], 2),
	      tc(interpret, [return,'(',2,*,2,')','.'], 4),
	      tc(interpret, [return,'(',1,+,1,+,1,')','.'], 3),
	      tc(interpret, [return,'(',2,*,4,*,6,')','.'], 48),
	      tc(interpret, [return,'(',1,-,1,')','.'], 0),
	      tc(interpret, [return,'(',2,/,2,')','.'], 1),
	      tc(interpret, [return,'(',1,-,1,+,1,')','.'], 1),
	      tc(interpret, [return,'(',64,/,2,/,8,')','.'], 4),
	      tc(interpret, [return,'(',1,+,'(',1,+,1,')',')','.'], 3),
	      tc(interpret, [return,'(',2,*,'(',2,/,2,')',')','.'], 2),
	      tc(interpret, [return,'(',4,-,'(',2,-,1,')',')','.'], 3),
	      tc(interpret, [return,'(',4,/,'(',2,-,-2,')',')','.'], 1),
	      tc(interpret, [var,x,;,x,:=,1,;,return,x,'.'], 1),
	      tc(evalfail,  [return,x,'.']),
	      tc(evalfail,  [var,x,;,return,x,'.']),
	      tc(evalfail,  [x,:=,1,;,return,x,'.']),
	      tc(evalfail,  [x,:=,1,;,return,1,'.']),
	      tc(interpret, [var,x,;,var,y,;,x,:=,1,;,y,:=,2,;,return,'(',x,+,y,')','.'], 3),
	      tc(evalfail,  [var,x,;,var,y,;,x,:=,1,;,return,'(',x,+,y,')','.']),
	      tc(interpret, [var,x,;,var,y,;,x,:=,'(',1,+,1,+,1,')',;,return,x,'.'], 3),
	      tc(interpret, [var,x,;,var,y,;,var,z,;,x,:=,1,;,y,:=,2,;,z,:=,'(',x,+,y,')',;,return,z,'.'], 3),
	      tc(interpret, [var,x,;,var,y,;,x,:=,1,;,y,:=,2,;,return,y,'.'], 2),
	      tc(interpret, [var,x,;,var,y,;,x,:=,1,;,x,:=,2,;,return,x,'.'], 2),
	      tc(interpret, [var,x,;,var,y,;,x,:=,1,;,y,:=,1000,;,x,:=,y,;,return,x,'.'], 1000),
	      tc(interpret, [var,x,;,var,y,;,x,:=,1,;,y,:=,1000,;,x,:=,y,;,y,:=,2,;,return,x,'.'], 1000),
	      tc(evalfail,  [var,x,;,var,y,;,x,:=,a,;,y,:=,1000,;,x,:=,y,;,return,x,'.']),
	      tc(evalfail,  [var,x,;,var,y,;,x,:=,1,;,y,:=,a,;,x,:=,y,;,return,y,'.']),
	      tc(interpret, [var,x,;,x,:=,'(','(',2,+,6,+,1,')',+,1,')',;,return,x,'.'] , 10),
	      tc(interpret, [var,x,;,x,:=,'(','(',2,+,6,+,1,')',*,2,*,2,')',;,return,x,'.'], 36),
	      tc(interpret, [var,a,;,var,b,;,a,:=,5,;,b,:=,'(',a,+,5,')',;,b,:=,'(',a,+,b,')',;,return,b,'.'], 15),
	      tc(evalfail,  [var,a,;,var,b,;,a,:=,b,;,b,:=,'37',;,return,a,'.']),
	      tc(interpret, [var,a,;,var,b,;,var,c,;,a,:=,5,;,b,:=,'(',a,+,5,')',;,c,:=,'(',10,+,a,+,b,')',;,return,c,'.'], 25),
	      tc(interpret, [var,a,;,var,b,;,var,c,;,a,:=,5,;,b,:=,'(',a,+,5,')',;,c,:=,'(',10,*,a,*,b,')',;,return,c,'.'], 500),
	      tc(interpret, [var,a,;,var,b,;,var,c,;,a,:=,5,;,b,:=,'(',a,+,5,')',;,c,:=,'(',10,-,a,-,b,')',;,return,c,'.'], -5),
	      tc(interpret, [var,a,;,var,b,;,var,c,;,a,:=,5,;,b,:=,'(',a,+,5,')',;,c,:=,'(',10,-,a,+,b,')',;,return,c,'.'], 15),
	      tc(interpret, [var,a,;,var,b,;,var,c,;,a,:=,5,;,b,:=,'(',a,+,5,')',;,c,:=,'(',100,/,a,/,b,')',;,return,c,'.'], 2),
	      tc(interpret, [var,a,;,var,b,;,var,c,;,a,:=,5,;,b,:=,'(',a,+,5,')',;,c,:=,'(',100,/,a,*,b,')',;,return,c,'.'], 200),
	      tc(interpret, [var,a,;,var,b,;,var,c,;,a,:=,5,;,b,:=,'(',a,+,5,')',;,c,:=,'(',100,+,a,*,b,-,200,')',;,return,c,'.'], -50),
	      tc(interpret, [var,a,;,var,b,;,var,c,;,a,:=,5,;,b,:=,'(',a,+,5,')',;,c,:=,'(','(',100,+,a,')',*,b,-,200,')',;,return,c,'.'], 850),
	      tc(interpret, [var,a,;,var,b,;,var,c,;,a,:=,5,;,b,:=,'(',a,+,5,')',;,c,:=,'(','(',10,+,a,')',*,'(',b,-,20,')',')',;,return,c,'.'], -150),
	      tc(interpret, [var,a,;,var,b,;,var,c,;,a,:=,5,;,b,:=,'(',a,+,5,')',;,c,:=,'(','(',10,+,a,')',+,'(',b,-,20,')',')',;,return,c,'.'], 5),
	      tc(interpret, [var,a,;,var,b,;,var,c,;,a,:=,5,;,b,:=,'(',a,+,5,')',;,c,:=,'(','(',10,+,a,')',-,'(',b,-,20,')',')',;,return,c,'.'], 25), 
	      tc(interpret, [var,a,;,var,b,;,var,c,;,a,:=,5,;,b,:=,'(',a,+,5,')',;,c,:=,'(','(',10,+,a,')',/,'(',b,-,20,')',')',;,return,c,'.'], -1.5),
	      tc(interpret, [var,a,;,var,b,;,var,c,;,a,:=,5,;,b,:=,'(',a,+,5,')',;,c,:=,'(','(',10,*,a,')',/,'(',b,-,15,+,10,')',')',;,return,c,'.'], 10),
	      tc(interpret, [var,a,;,var,b,;,var,c,;,a,:=,5,;,b,:=,'(',a,+,5,')',;,c,:=,'(','(',10,*,'(',a,+,1,')',')',/,'(','(',b,-,20,')',/,2,')',')',;,return,c,'.'], -12),
	      tc(parsefail, [return,'.']),
	      tc(parsefail, [return,;,return,1,'.']),
	      tc(parsefail, [return,1,'.',return,1,'.']),
	      tc(parsefail, [a,:=,1,;]),
	      tc(parsefail, [var,a,;,a,:=,1,;]),
	      tc(parsefail, [a,:=,1,;,return,a,'.',a,:=,0]),
	      tc(parsefail, [return,:=,1,;,return,return,'.']),
	      tc(parsefail, [:=,:=,1,;,return,:=,'.']),
	      tc(parsefail, [+,:=,1,;,return,+,'.']),
	      tc(parsefail, [*,:=,1,;,return,*,'.']),
	      tc(parsefail, [-,:=,1,;,return,-,'.']),
	      tc(parsefail, [/,:=,1,;,return,/,'.']).

% Pull it all together....
test_all :- test_simple, test_var, test_oop, test_comp, test_reject, test_keywords, test_cond, test_loop, test_func, test_scope, itestcases.

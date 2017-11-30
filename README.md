# InteractiveExpressionParser
This project focus on using functional programing to parse the expression, I use Haskell to develop an interactive expression parser using a lot of features of Haskell, you can use haskell compiler such as GHCI to run the program, Let's begin.

# Expression Tree

In Haskell, a famous data type called Maybe is used to represent “a value or nothing”. If an expression e has type Maybe a, then e is either Just a (just a value of type a) or Nothing, as demonstrated by the de nition below.
data Maybe a = Just a | Nothing
Maybe describes values with a context of possible failure attached. For example, if a function returns Maybe Int, it means the function may fail when calculating the result integer. We will make use of Maybe to deal with expression trees.
Let’s consider the following Expr data type for expression trees. data Binop = Add | Sub | Mul | Div | Mod
deriving (Eq, Show)
data Expr
= Bin Binop Expr Expr | Val Int
| Var String deriving (Eq, Show)
It might be a little bit di erent from the expression tree you have seen, because we are going to reuse Binop later. Binop stands for binary operations, including addition Add, subtraction Sub, multiplication Mul, division Div, modulo Mod. Expr contains all binary operations over expressions, together with integer literal Val and variable Var.
We use an environment Env to determine the values for variables:
type Env = [(String, Int)]
The library function lookup could be used for searching in an environment.
Problem 1. I implemented a function eval :: Env -> Expr -> Maybe Int to evaluate expression trees. eval should return Nothing if the divisor is 0 in the division and modulo cases. Also, if a variable cannot be found in the environment, Nothing should be returned.
2
running results:
* Main> eval [] (Bin Add (Val 2) (Val 3))
Just 5
* Main> eval [("x", 2)] (Bin Add (Var "x") (Val 3))
Just 5
* Main> eval [("x", 2)] (Bin Add (Var "y") (Val 3))
Nothing
* Main> eval [] (Bin Div (Val 4) (Val 2))
Just 2
* Main> eval [] (Bin Mod (Val 4) (Val 0))
Nothing


# Parsing Expressions

Then let’s write a parser for those expression trees. You may want to review Tutorial 4 and the lecture slides when doing this I implemented a function pExpr :: Parser Expr for parsing Exprs. The grammar is provided as below:
expr := term op_term
op_term := (′+′ | ′−′) term op_term | ε term := factor op_factor
op_factor := (′∗′ | ′/′ | ′%′) factor op_factor | ε factor := ′(′ expr ′)′ | integer | identifier
You can assume the identi ers start with a lower case letter, and may contain any alphabetic or numeric characters after the  rst one.
Notice:
• Use the token function in Parsing.hs to remove leading and trailing spaces.
• Your parser should re ect the left-associativity of the operators. See the second
example below.
running results:
* Main> parse pExpr "1 + 2"
[(Bin Add (Val 1) (Val 2),"")]
* Main> parse pExpr "1 + 2 + 3"
[(Bin Add (Bin Add (Val 1) (Val 2)) (Val 3),"")]
* Main> parse pExpr "1 + x"
[(Bin Add (Val 1) (Var "x"),"")]
* Main> parse pExpr "1 + x * 3"
[(Bin Add (Val 1) (Bin Mul (Var "x") (Val 3)),"")]
* Main> parse pExpr "1 + x * 3 / 5"
[(Bin Add (Val 1) (Bin Div (Bin Mul (Var "x") (Val 3)) (Val 5)),"")]


I implemented a function runParser :: Parser a -> String -> Maybe a. runParser runs a given parser to parse the full string and returns the  rst result. Maybe implys the parser may fail.
Notice:
• Return Nothing when the result list is empty.
• Return Nothing when the parser only consumes part of the input (the second
component of the pair is not empty, see the examples below).
running results:
* Main> runParser (char 'a') "a"
Just 'a'
* Main> runParser (char 'a') "ab"
Nothing
* Main> runParser pExpr "1+2"
Just (Bin Add (Val 1) (Val 2))
* Main> runParser pExpr "1++"
Nothing

#Compilation
Instead of de ning eval directly, we can give expressions meaning by compiling expres- sions onto a simple stack machine.
Consider that the stack machine supports the following instructions:
data Instr = IVal Int | IBin Binop | IVar String deriving (Eq, Show)
Instructions contains integer literal IVal, variable Var, and binary operations IBin. The evaluation of instructions are based on a simple stack, which is modeled as a list of integers:
type Stack = [Int]
Instruction IVal i will push i into the stack, and IVar x will push the value of variable x (if it is in the environment) into stack. IBin op will pop two values from the stack, and apply the binary operator to them, and then push the result into the stack.
A program is a list of instructions.
type Prog = [Instr]
I implemented a function runProg :: Prog -> Env -> Maybe Int. runProg runs a program with an empty stack at the beginning. It should end with exactly one number in the stack. In those cases, return Just value where value is the only number in the stack. Return Nothing if evaluation fails:
• Not enough numbers to apply for binary operations.
• A divisor is 0 in the division and modulo cases.
• A variable cannot be found in the environment.
• After evaluation, the stack has less or more than one number.
running results:
* Main> runProg [IVal 1, IVal 2, IBin Add] []
Just 3
6
* Main> runProg [IVal 2, IVal 4, IBin Div] []
Just 2
* Main> runProg [IVal 3, IBin Add] []
Nothing
* Main> runProg [IVal 2, IVal 4, IBin Mul, IVal 1] []
Nothing
* Main> runProg [IVal 2, IVar "x", IBin Sub] [("x", 3)]
Just 1

I implemented a function compile :: Expr -> Prog that compiles an expression into a program that can be run in the stack machine.
* Main> compile (Bin Sub (Val 2) (Val 1))
[IVal 1,IVal 2,IBin Sub]
* Main> compile (Bin Sub (Val 2) (Var "x"))
[IVar "x",IVal 2,IBin Sub]
* Main> compile (Bin Sub (Val 2) (Bin Add (Val 1) (Var "x")))
[IVar "x",IVal 1,IBin Add,IVal 2,IBin Sub]
With all those functions de ned so far, you should be able to verity that the two ap- proaches always give the same result:
• Direct evaluation over an expression
• First translate the expression into a program on a simple stack machine, and then
run the program.
* Main> let Just x = runParser pExpr "2-1" *Main> x
Bin Sub (Val 2) (Val 1)
* Main> eval [] x
Just 1
* Main> compile x
[IVal 1,IVal 2,IBin Sub]
* Main> runProg (compile x) []
Just 1

#Optimization
The compilation in practice can be very complicated. In order to produce e cient ma- chine programs, there are usually many optimization heuristics. One of the simplest heuristics is Constant folding, namely, calculation between constants is calculated di- rectly during compilation time instead of at runtime.
I implemented a function optimize :: Expr -> Maybe Expr that optimizes an expression according to the following rules:
• Multiplication between any expression e and 0 is simpli ed to 0. • Addition between any expression e and 0 is simpli ed to e.
• Subtraction an expression e by 0 simpli ed to e.
• Division or Modulo by 0 returns Nothing.
• Any evaluation between constants are calculated directly.
running results:
* Main> optimize $ Bin Add (Var "x") (Bin Sub (Val 2) (Val 1))
Just (Bin Add (Var "x") (Val 1))
* Main> optimize $ Bin Add (Val 3) (Bin Sub (Val 2) (Val 1))
Just (Val 4)
* Main> optimize $ Bin Add (Val 3) (Bin Mul (Var "x") (Val 0))
Just (Val 3)
* Main> optimize $ Bin Add (Var "x") (Val 0)
Just (Var "x")
* Main> optimize $ Bin Add (Var "x") (Val 1)
Just (Bin Add (Var "x") (Val 1))
* Main> optimize $ Bin Div (Val 3) (Val 0)
Nothing


#REPL: Read-Eval-Print Loop
Running Example 1:
Main> main
* > no such command
Error
* > quit 
* Main>

Running Example 2:
* Main> main
* > let a = 10 + 10 * 2
* a = 30
* > let b = a * 2
* b = 60
* > let b = 5 / 0
* Error
* > let d = c Error
* > let b = 10 b = 10
* > let e = b + 5 e = 15
* > quit 
* Main>

Running Example 3:
*Main> main
* > env
* > let a = 10 a = 10
* > let b = a / 2

* b=5
* > env a = 10 b=5
* > let a = 5 + 1 * 2 a=7
* > env a=7 b=5
* > quit *Main>

* > let a = 10 + 20
* a = 30 > var a
* a = 30 > var b
Error
* > let a = 3 * 4 a = 12
* > var a a = 12
* > quit *Main>


Running Example:5
* Main> main > let a = 10
* > var a a = 10
* > del a
Deleted a
*  > var a Error
* > del b Error
* > let b = 10 b = 10
* > quit


Running Example 6:
* Main> main >1+2+3+4
* ans = 10 >1+2*2
* ans = 5
* > let a = 10
* 13
* a = 10 >a-1-2
* ans = 7 >5/0
Error
> b+1 Error
> let b = 2 b=2
> a*b ans = 20
> quit

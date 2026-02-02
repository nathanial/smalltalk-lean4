/-
  Smalltalk parser tests.
-/
import Crucible
import Smalltalk

open Crucible
open Smalltalk

namespace ParserTests

testSuite "Smalltalk.Parse"

def parseMain (src : String) : IO (List Expr) := do
  match Smalltalk.parse src with
  | .ok program => pure program.main
  | .error e => throw (IO.userError s!"parse error: {e.message}")

def parseProgram (src : String) : IO Program := do
  match Smalltalk.parse src with
  | .ok program => pure program
  | .error e => throw (IO.userError s!"parse error: {e.message}")

def parseOne (src : String) : IO Expr := do
  match Smalltalk.parseExpr src with
  | .ok expr => pure expr
  | .error e => throw (IO.userError s!"parse error: {e.message}")

def parseError (src : String) : IO String := do
  match Smalltalk.parse src with
  | .ok _ => throw (IO.userError "expected parse error")
  | .error e => pure e.message

test "parse int literal" := do
  let exprs ← parseMain "42"
  exprs ≡ [Expr.lit (.int 42)]

test "parse radix int literal" := do
  let exprs ← parseMain "16rFF"
  exprs ≡ [Expr.lit (.int 255)]

test "parse float literal" := do
  let exprs ← parseMain "3.14"
  exprs ≡ [Expr.lit (.float 3.14)]

test "parse float exponent" := do
  let exprs ← parseMain "1e3"
  exprs ≡ [Expr.lit (.float 1000.0)]

test "parse scaled decimal" := do
  let exprs ← parseMain "1.23s2"
  exprs ≡ [Expr.lit (.scaled 123 2)]

test "parse string literal" := do
  let exprs ← parseMain "'hello''world'"
  exprs ≡ [Expr.lit (.str "hello'world")]

test "parse char literal" := do
  let exprs ← parseMain "$a"
  exprs ≡ [Expr.lit (.char 'a')]

test "parse symbol literal" := do
  let exprs ← parseMain "#at:put:"
  exprs ≡ [Expr.lit (.symbol "at:put:")]

test "parse quoted symbol literal" := do
  let exprs ← parseMain "#'foo bar'"
  exprs ≡ [Expr.lit (.symbol "foo bar")]

test "parse binary symbol literal" := do
  let exprs ← parseMain "#+"
  exprs ≡ [Expr.lit (.symbol "+")]

test "parse literal array" := do
  let exprs ← parseMain "#(1 'a' #foo $b)"
  exprs ≡ [Expr.lit (.array [.int 1, .str "a", .symbol "foo", .char 'b'])]

test "parse literal dictionary" := do
  let exprs ← parseMain "#{ #a -> 1 . #b -> 2 }"
  exprs ≡
    [Expr.lit (.dict [(.symbol "a", .int 1), (.symbol "b", .int 2)])]

test "parse byte array" := do
  let exprs ← parseMain "#[1 2 255]"
  exprs ≡ [Expr.lit (.byteArray [1, 2, 255])]

test "parse boolean literal" := do
  let exprs ← parseMain "true"
  exprs ≡ [Expr.lit (.bool true)]

test "parse nil literal" := do
  let exprs ← parseMain "nil"
  exprs ≡ [Expr.lit .nil]

test "parse variable" := do
  let exprs ← parseMain "counter"
  exprs ≡ [Expr.var "counter"]

test "parse assignment" := do
  let exprs ← parseMain "counter := 3"
  exprs ≡ [Expr.assign "counter" (Expr.lit (.int 3))]

test "parse underscore assignment" := do
  let exprs ← parseMain "counter _ 3"
  exprs ≡ [Expr.assign "counter" (Expr.lit (.int 3))]

test "parse unary send" := do
  let exprs ← parseMain "array size"
  exprs ≡ [Expr.send (Expr.var "array") "size" []]

test "parse binary send" := do
  let exprs ← parseMain "1 + 2"
  exprs ≡ [Expr.send (Expr.lit (.int 1)) "+" [Expr.lit (.int 2)]]

test "parse keyword send" := do
  let exprs ← parseMain "array at: 1 put: 2"
  exprs ≡
    [Expr.send (Expr.var "array") "at:put:" [Expr.lit (.int 1), Expr.lit (.int 2)]]

test "parse cascade" := do
  let exprs ← parseMain "obj foo; bar: 1; baz"
  exprs ≡
    [Expr.cascade (Expr.var "obj")
      [[("foo", [])], [("bar:", [Expr.lit (.int 1)])], [("baz", [])]]]

test "parse cascade with chained first message" := do
  let exprs ← parseMain "obj foo bar; baz"
  exprs ≡
    [Expr.cascade (Expr.var "obj")
      [[("foo", []), ("bar", [])], [("baz", [])]]]

test "parse dynamic array" := do
  let exprs ← parseMain "{ 1. 2 }"
  exprs ≡ [Expr.array [Expr.lit (.int 1), Expr.lit (.int 2)]]

test "parse chained unary sends" := do
  let exprs ← parseMain "foo bar baz"
  exprs ≡
    [Expr.send (Expr.send (Expr.var "foo") "bar" []) "baz" []]

test "parse precedence (binary before keyword)" := do
  let exprs ← parseMain "1 + 2 at: 3"
  exprs ≡
    [Expr.send (Expr.send (Expr.lit (.int 1)) "+" [Expr.lit (.int 2)]) "at:" [Expr.lit (.int 3)]]

test "parse block with params" := do
  let exprs ← parseMain "[ :x :y | x + y ]"
  exprs ≡
    [Expr.block ["x", "y"] []
      [Expr.send (Expr.var "x") "+" [Expr.var "y"]]]

test "parse block with temps" := do
  let exprs ← parseMain "[ | t1 t2 | t1 ]"
  exprs ≡
    [Expr.block [] ["t1", "t2"] [Expr.var "t1"]]

test "parse block with params and temps" := do
  let exprs ← parseMain "[ :x | | t | t := x ]"
  exprs ≡
    [Expr.block ["x"] ["t"] [Expr.assign "t" (Expr.var "x")]]

test "parse block without params" := do
  let exprs ← parseMain "[ 1 + 2 ]"
  exprs ≡
    [Expr.block [] [] [Expr.send (Expr.lit (.int 1)) "+" [Expr.lit (.int 2)]]]

test "parse top-level sequence" := do
  let exprs ← parseMain "a. b. c"
  exprs ≡ [Expr.var "a", Expr.var "b", Expr.var "c"]

test "parse parenthesized sequence" := do
  let exprs ← parseMain "(a. b)"
  exprs ≡ [Expr.seq [Expr.var "a", Expr.var "b"]]

test "parse parenthesized single" := do
  let exprs ← parseMain "(1)"
  exprs ≡ [Expr.lit (.int 1)]

test "parse return statement" := do
  let exprs ← parseMain "^ 1"
  exprs ≡ [Expr.return (Expr.lit (.int 1))]

test "parse comments" := do
  let exprs ← parseMain "\"hello\" 1 \"world\""
  exprs ≡ [Expr.lit (.int 1)]

test "parse expression entrypoint" := do
  let expr ← parseOne "1 + 2"
  expr ≡ Expr.send (Expr.lit (.int 1)) "+" [Expr.lit (.int 2)]

test "parse class definition" := do
  let program ← parseProgram "class Point < Object | x y | x ^ x ! end"
  program.classes.length ≡ 1
  let cls := program.classes.head!
  cls.name ≡ "Point"
  cls.super ≡ some "Object"
  cls.ivars ≡ ["x", "y"]
  cls.methods.length ≡ 1
  cls.methods.head!.selector ≡ "x"

test "parse class then main expressions" := do
  let program ← parseProgram "class Foo end 1 + 2"
  program.main ≡ [Expr.send (Expr.lit (.int 1)) "+" [Expr.lit (.int 2)]]

test "parse class-side methods" := do
  let program ← parseProgram "class Foo class answer ^ 42 ! end"
  let cls := program.classes.head!
  cls.methods.length ≡ 0
  cls.classMethods.length ≡ 1
  cls.classMethods.head!.selector ≡ "answer"

test "disallow ! as binary selector in expressions" := do
  let msg ← parseError "1 ! 2"
  shouldSatisfy (msg.startsWith "Parse error") "expected parse error"



end ParserTests

/-
  Smalltalk evaluator tests.
-/
import Crucible
import Smalltalk

open Crucible
open Smalltalk

def String.containsSubstr (s : String) (sub : String) : Bool :=
  (s.splitOn sub).length > 1

namespace EvalTests

testSuite "Smalltalk.Eval"

-- Helper to create a simple program from expressions
def mkProgram (exprs : List Expr) : Program :=
  { classes := [], main := exprs }

-- Helper to check evaluation result
def shouldEvalTo (expr : Expr) (expected : Value) : IO Unit := do
  let program := mkProgram [expr]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      let eStr := reprStr expected
      shouldSatisfy (vStr == eStr) s!"expected {eStr}, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

-- Helper to check that evaluation produces an error containing a substring
def shouldEvalError (expr : Expr) (substring : String) : IO Unit := do
  let program := mkProgram [expr]
  match Smalltalk.evalProgram program with
  | .ok v =>
      throw (IO.userError s!"expected error containing '{substring}', got value: {reprStr v}")
  | .error e =>
      shouldSatisfy (e.message.containsSubstr substring)
        s!"expected error containing '{substring}', got: {e.message}"

-- Helper to create a program with class definitions
def mkProgramWithClasses (classes : List ClassDef) (exprs : List Expr) : Program :=
  { classes := classes, main := exprs }

-- ============ Literal Tests ============

test "eval int literal" := do
  shouldEvalTo (.lit (.int 42)) (.int 42)

test "eval negative int literal" := do
  shouldEvalTo (.lit (.int (-17))) (.int (-17))

test "eval float literal" := do
  shouldEvalTo (.lit (.float 3.14)) (.float 3.14)

test "eval scaled decimal literal" := do
  -- 123s2 means 123 * 10^(-2) = 1.23
  shouldEvalTo (.lit (.scaled 123 2)) (.float 1.23)

test "eval string literal" := do
  shouldEvalTo (.lit (.str "hello")) (.str "hello")

test "eval empty string literal" := do
  shouldEvalTo (.lit (.str "")) (.str "")

test "eval bool true" := do
  shouldEvalTo (.lit (.bool true)) (.bool true)

test "eval bool false" := do
  shouldEvalTo (.lit (.bool false)) (.bool false)

test "eval nil literal" := do
  shouldEvalTo (.lit .nil) .nil

test "eval symbol literal" := do
  shouldEvalTo (.lit (.symbol "foo")) (.symbol "foo")

test "eval char literal" := do
  shouldEvalTo (.lit (.char 'x')) (.char 'x')

test "eval literal array" := do
  shouldEvalTo (.lit (.array [.int 1, .int 2, .int 3]))
    (.array [.int 1, .int 2, .int 3])

test "eval nested literal array" := do
  shouldEvalTo (.lit (.array [.int 1, .array [.int 2, .int 3]]))
    (.array [.int 1, .array [.int 2, .int 3]])

test "eval literal dict" := do
  shouldEvalTo (.lit (.dict [(.symbol "a", .int 1), (.symbol "b", .int 2)]))
    (.dict [(.symbol "a", .int 1), (.symbol "b", .int 2)])

test "eval byte array literal" := do
  shouldEvalTo (.lit (.byteArray [1, 2, 255]))
    (.array [.int 1, .int 2, .int 255])

-- ============ Variable Tests ============

test "eval undefined variable returns error" := do
  shouldEvalError (.var "x") "Undefined variable: x"

test "eval assignment returns value" := do
  shouldEvalTo (.assign "x" (.lit (.int 42))) (.int 42)

test "eval variable after assignment" := do
  let program := mkProgram [
    .assign "x" (.lit (.int 42)),
    .var "x"
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.int 42)) s!"expected 42, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

test "eval shadowing assignment" := do
  let program := mkProgram [
    .assign "x" (.lit (.int 1)),
    .assign "x" (.lit (.int 2)),
    .var "x"
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.int 2)) s!"expected 2, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

test "eval multiple variables" := do
  let program := mkProgram [
    .assign "x" (.lit (.int 1)),
    .assign "y" (.lit (.int 2)),
    .seq [.var "x", .var "y"]
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.int 2)) s!"expected 2 (last var), got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

-- ============ Sequence Tests ============

test "eval empty sequence returns nil" := do
  shouldEvalTo (.seq []) .nil

test "eval single expr sequence" := do
  shouldEvalTo (.seq [.lit (.int 42)]) (.int 42)

test "eval multi-expr sequence returns last" := do
  shouldEvalTo (.seq [.lit (.int 1), .lit (.int 2), .lit (.int 3)]) (.int 3)

test "eval sequence with side effects" := do
  -- x := 1. x := 2. x
  let program := mkProgram [
    .seq [
      .assign "x" (.lit (.int 1)),
      .assign "x" (.lit (.int 2)),
      .var "x"
    ]
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.int 2)) s!"expected 2, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

test "eval nested sequences" := do
  shouldEvalTo (.seq [.seq [.lit (.int 1)], .seq [.lit (.int 2)]]) (.int 2)

-- ============ Dynamic Array Tests ============

test "eval dynamic array" := do
  shouldEvalTo (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3)])
    (.array [.int 1, .int 2, .int 3])

test "eval empty dynamic array" := do
  shouldEvalTo (.array []) (.array [])

test "eval dynamic array with expressions" := do
  -- { x := 1. x }
  let program := mkProgram [
    .assign "x" (.lit (.int 5)),
    .array [.var "x", .lit (.int 10)]
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      let expected := reprStr (Value.array [.int 5, .int 10])
      shouldSatisfy (vStr == expected) s!"expected {expected}, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

-- ============ Empty Program Tests ============

test "eval empty program returns nil" := do
  let program : Program := { classes := [], main := [] }
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr Value.nil) s!"expected nil, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

-- ============ Integer Primitive Tests ============

test "int addition" := do
  shouldEvalTo (.send (.lit (.int 3)) "+" [.lit (.int 4)]) (.int 7)

test "int subtraction" := do
  shouldEvalTo (.send (.lit (.int 5)) "-" [.lit (.int 3)]) (.int 2)

test "int unary negation" := do
  shouldEvalTo (.send (.lit (.int 5)) "-" []) (.int (-5))

test "int negated" := do
  shouldEvalTo (.send (.lit (.int 5)) "negated" []) (.int (-5))

test "int multiplication" := do
  shouldEvalTo (.send (.lit (.int 3)) "*" [.lit (.int 4)]) (.int 12)

test "int division" := do
  shouldEvalTo (.send (.lit (.int 10)) "/" [.lit (.int 3)]) (.int 3)

test "int floor division" := do
  shouldEvalTo (.send (.lit (.int 10)) "//" [.lit (.int 3)]) (.int 3)

test "int modulo" := do
  shouldEvalTo (.send (.lit (.int 10)) "\\\\" [.lit (.int 3)]) (.int 1)

test "int abs positive" := do
  shouldEvalTo (.send (.lit (.int 5)) "abs" []) (.int 5)

test "int abs negative" := do
  shouldEvalTo (.send (.lit (.int (-5))) "abs" []) (.int 5)

test "int division by zero" := do
  shouldEvalError (.send (.lit (.int 5)) "/" [.lit (.int 0)]) "Division by zero"

-- ============ Integer Comparison Tests ============

test "int less than true" := do
  shouldEvalTo (.send (.lit (.int 3)) "<" [.lit (.int 5)]) (.bool true)

test "int less than false" := do
  shouldEvalTo (.send (.lit (.int 5)) "<" [.lit (.int 3)]) (.bool false)

test "int greater than true" := do
  shouldEvalTo (.send (.lit (.int 5)) ">" [.lit (.int 3)]) (.bool true)

test "int greater than false" := do
  shouldEvalTo (.send (.lit (.int 3)) ">" [.lit (.int 5)]) (.bool false)

test "int less than or equal true" := do
  shouldEvalTo (.send (.lit (.int 3)) "<=" [.lit (.int 5)]) (.bool true)

test "int less than or equal equal" := do
  shouldEvalTo (.send (.lit (.int 5)) "<=" [.lit (.int 5)]) (.bool true)

test "int greater than or equal true" := do
  shouldEvalTo (.send (.lit (.int 5)) ">=" [.lit (.int 3)]) (.bool true)

test "int greater than or equal equal" := do
  shouldEvalTo (.send (.lit (.int 5)) ">=" [.lit (.int 5)]) (.bool true)

test "int equal true" := do
  shouldEvalTo (.send (.lit (.int 5)) "=" [.lit (.int 5)]) (.bool true)

test "int equal false" := do
  shouldEvalTo (.send (.lit (.int 5)) "=" [.lit (.int 3)]) (.bool false)

test "int not equal true" := do
  shouldEvalTo (.send (.lit (.int 3)) "~=" [.lit (.int 5)]) (.bool true)

test "int not equal false" := do
  shouldEvalTo (.send (.lit (.int 5)) "~=" [.lit (.int 5)]) (.bool false)

-- ============ Integer-Float Mixed Operations ============

test "int plus float" := do
  shouldEvalTo (.send (.lit (.int 1)) "+" [.lit (.float 2.5)]) (.float 3.5)

test "int times float" := do
  shouldEvalTo (.send (.lit (.int 2)) "*" [.lit (.float 1.5)]) (.float 3.0)

test "int less than float" := do
  shouldEvalTo (.send (.lit (.int 2)) "<" [.lit (.float 2.5)]) (.bool true)

-- ============ Float Primitive Tests ============

test "float addition" := do
  shouldEvalTo (.send (.lit (.float 1.5)) "+" [.lit (.float 2.5)]) (.float 4.0)

test "float subtraction" := do
  shouldEvalTo (.send (.lit (.float 5.5)) "-" [.lit (.float 2.5)]) (.float 3.0)

test "float multiplication" := do
  shouldEvalTo (.send (.lit (.float 2.0)) "*" [.lit (.float 3.0)]) (.float 6.0)

test "float division" := do
  shouldEvalTo (.send (.lit (.float 6.0)) "/" [.lit (.float 2.0)]) (.float 3.0)

test "float negated" := do
  shouldEvalTo (.send (.lit (.float 3.5)) "negated" []) (.float (-3.5))

test "float abs" := do
  shouldEvalTo (.send (.lit (.float (-3.5))) "abs" []) (.float 3.5)

test "float floor" := do
  shouldEvalTo (.send (.lit (.float 3.7)) "floor" []) (.int 3)

test "float ceiling" := do
  shouldEvalTo (.send (.lit (.float 3.2)) "ceiling" []) (.int 4)

test "float rounded" := do
  shouldEvalTo (.send (.lit (.float 3.5)) "rounded" []) (.int 4)

test "float truncated positive" := do
  shouldEvalTo (.send (.lit (.float 3.7)) "truncated" []) (.int 3)

test "float truncated negative" := do
  shouldEvalTo (.send (.lit (.float (-3.7))) "truncated" []) (.int (-3))

test "float sqrt" := do
  shouldEvalTo (.send (.lit (.float 4.0)) "sqrt" []) (.float 2.0)

test "float sqrt negative error" := do
  shouldEvalError (.send (.lit (.float (-1.0))) "sqrt" []) "sqrt of negative"

-- ============ Float Comparison Tests ============

test "float less than true" := do
  shouldEvalTo (.send (.lit (.float 1.5)) "<" [.lit (.float 2.5)]) (.bool true)

test "float equal true" := do
  shouldEvalTo (.send (.lit (.float 3.0)) "=" [.lit (.float 3.0)]) (.bool true)

-- ============ String Primitive Tests ============

test "string concat" := do
  shouldEvalTo (.send (.lit (.str "hello")) "," [.lit (.str " world")]) (.str "hello world")

test "string size" := do
  shouldEvalTo (.send (.lit (.str "hello")) "size" []) (.int 5)

test "string at" := do
  shouldEvalTo (.send (.lit (.str "hello")) "at:" [.lit (.int 1)]) (.char 'h')

test "string at last" := do
  shouldEvalTo (.send (.lit (.str "hello")) "at:" [.lit (.int 5)]) (.char 'o')

test "string at out of bounds" := do
  shouldEvalError (.send (.lit (.str "hello")) "at:" [.lit (.int 6)]) "out of bounds"

test "string at zero" := do
  shouldEvalError (.send (.lit (.str "hello")) "at:" [.lit (.int 0)]) "out of bounds"

test "string equal true" := do
  shouldEvalTo (.send (.lit (.str "hello")) "=" [.lit (.str "hello")]) (.bool true)

test "string equal false" := do
  shouldEvalTo (.send (.lit (.str "hello")) "=" [.lit (.str "world")]) (.bool false)

test "string not equal" := do
  shouldEvalTo (.send (.lit (.str "hello")) "~=" [.lit (.str "world")]) (.bool true)

test "string isEmpty true" := do
  shouldEvalTo (.send (.lit (.str "")) "isEmpty" []) (.bool true)

test "string isEmpty false" := do
  shouldEvalTo (.send (.lit (.str "hello")) "isEmpty" []) (.bool false)

test "string asUppercase" := do
  shouldEvalTo (.send (.lit (.str "hello")) "asUppercase" []) (.str "HELLO")

test "string asLowercase" := do
  shouldEvalTo (.send (.lit (.str "HELLO")) "asLowercase" []) (.str "hello")

-- ============ Character Primitive Tests ============

test "char asInteger" := do
  shouldEvalTo (.send (.lit (.char 'A')) "asInteger" []) (.int 65)

test "char asString" := do
  shouldEvalTo (.send (.lit (.char 'x')) "asString" []) (.str "x")

test "char asUppercase" := do
  shouldEvalTo (.send (.lit (.char 'a')) "asUppercase" []) (.char 'A')

test "char asLowercase" := do
  shouldEvalTo (.send (.lit (.char 'A')) "asLowercase" []) (.char 'a')

test "char isLetter true" := do
  shouldEvalTo (.send (.lit (.char 'a')) "isLetter" []) (.bool true)

test "char isLetter false" := do
  shouldEvalTo (.send (.lit (.char '1')) "isLetter" []) (.bool false)

test "char isDigit true" := do
  shouldEvalTo (.send (.lit (.char '5')) "isDigit" []) (.bool true)

test "char equal" := do
  shouldEvalTo (.send (.lit (.char 'a')) "=" [.lit (.char 'a')]) (.bool true)

test "char less than" := do
  shouldEvalTo (.send (.lit (.char 'a')) "<" [.lit (.char 'b')]) (.bool true)

-- ============ Boolean Primitive Tests ============

test "bool and true" := do
  shouldEvalTo (.send (.lit (.bool true)) "&" [.lit (.bool true)]) (.bool true)

test "bool and false" := do
  shouldEvalTo (.send (.lit (.bool true)) "&" [.lit (.bool false)]) (.bool false)

test "bool or true" := do
  shouldEvalTo (.send (.lit (.bool true)) "|" [.lit (.bool false)]) (.bool true)

test "bool or false" := do
  shouldEvalTo (.send (.lit (.bool false)) "|" [.lit (.bool false)]) (.bool false)

test "bool not true" := do
  shouldEvalTo (.send (.lit (.bool true)) "not" []) (.bool false)

test "bool not false" := do
  shouldEvalTo (.send (.lit (.bool false)) "not" []) (.bool true)

test "bool and: keyword" := do
  shouldEvalTo (.send (.lit (.bool true)) "and:" [.lit (.bool true)]) (.bool true)

test "bool or: keyword" := do
  shouldEvalTo (.send (.lit (.bool false)) "or:" [.lit (.bool true)]) (.bool true)

test "bool xor: true" := do
  shouldEvalTo (.send (.lit (.bool true)) "xor:" [.lit (.bool false)]) (.bool true)

test "bool xor: false" := do
  shouldEvalTo (.send (.lit (.bool true)) "xor:" [.lit (.bool true)]) (.bool false)

test "bool equal true" := do
  shouldEvalTo (.send (.lit (.bool true)) "=" [.lit (.bool true)]) (.bool true)

test "bool equal false" := do
  shouldEvalTo (.send (.lit (.bool true)) "=" [.lit (.bool false)]) (.bool false)

-- ============ Nil Primitive Tests ============

test "nil isNil" := do
  shouldEvalTo (.send (.lit .nil) "isNil" []) (.bool true)

test "nil notNil" := do
  shouldEvalTo (.send (.lit .nil) "notNil" []) (.bool false)

test "nil equal nil" := do
  shouldEvalTo (.send (.lit .nil) "=" [.lit .nil]) (.bool true)

test "nil equal int" := do
  shouldEvalTo (.send (.lit .nil) "=" [.lit (.int 1)]) (.bool false)

test "nil not equal int" := do
  shouldEvalTo (.send (.lit .nil) "~=" [.lit (.int 1)]) (.bool true)

-- ============ Symbol Primitive Tests ============

test "symbol asString" := do
  shouldEvalTo (.send (.lit (.symbol "foo")) "asString" []) (.str "foo")

test "symbol size" := do
  shouldEvalTo (.send (.lit (.symbol "hello")) "size" []) (.int 5)

test "symbol equal true" := do
  shouldEvalTo (.send (.lit (.symbol "foo")) "=" [.lit (.symbol "foo")]) (.bool true)

test "symbol equal false" := do
  shouldEvalTo (.send (.lit (.symbol "foo")) "=" [.lit (.symbol "bar")]) (.bool false)

-- ============ Array Primitive Tests ============

test "array size" := do
  shouldEvalTo (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3)]) "size" []) (.int 3)

test "array isEmpty true" := do
  shouldEvalTo (.send (.array []) "isEmpty" []) (.bool true)

test "array isEmpty false" := do
  shouldEvalTo (.send (.array [.lit (.int 1)]) "isEmpty" []) (.bool false)

test "array at" := do
  shouldEvalTo (.send (.array [.lit (.int 10), .lit (.int 20), .lit (.int 30)]) "at:" [.lit (.int 2)]) (.int 20)

test "array at out of bounds" := do
  shouldEvalError (.send (.array [.lit (.int 1)]) "at:" [.lit (.int 5)]) "out of bounds"

test "array first" := do
  shouldEvalTo (.send (.array [.lit (.int 10), .lit (.int 20)]) "first" []) (.int 10)

test "array first empty error" := do
  shouldEvalError (.send (.array []) "first" []) "empty array"

test "array last" := do
  shouldEvalTo (.send (.array [.lit (.int 10), .lit (.int 20)]) "last" []) (.int 20)

test "array concat" := do
  shouldEvalTo (.send (.array [.lit (.int 1)]) "," [.array [.lit (.int 2), .lit (.int 3)]])
    (.array [.int 1, .int 2, .int 3])

-- ============ Dictionary Primitive Tests ============

test "dict size" := do
  shouldEvalTo (.send (.lit (.dict [(.symbol "a", .int 1), (.symbol "b", .int 2)])) "size" []) (.int 2)

test "dict isEmpty true" := do
  shouldEvalTo (.send (.lit (.dict [])) "isEmpty" []) (.bool true)

test "dict at" := do
  shouldEvalTo (.send (.lit (.dict [(.symbol "a", .int 42)])) "at:" [.lit (.symbol "a")]) (.int 42)

test "dict at missing key" := do
  shouldEvalError (.send (.lit (.dict [(.symbol "a", .int 42)])) "at:" [.lit (.symbol "b")]) "Key not found"

test "dict at:ifAbsent: found" := do
  shouldEvalTo (.send (.lit (.dict [(.symbol "a", .int 42)])) "at:ifAbsent:" [.lit (.symbol "a"), .lit (.int 0)]) (.int 42)

test "dict at:ifAbsent: missing" := do
  shouldEvalTo (.send (.lit (.dict [(.symbol "a", .int 42)])) "at:ifAbsent:" [.lit (.symbol "b"), .lit (.int 0)]) (.int 0)

test "dict includesKey true" := do
  shouldEvalTo (.send (.lit (.dict [(.symbol "a", .int 1)])) "includesKey:" [.lit (.symbol "a")]) (.bool true)

test "dict includesKey false" := do
  shouldEvalTo (.send (.lit (.dict [(.symbol "a", .int 1)])) "includesKey:" [.lit (.symbol "b")]) (.bool false)

test "dict keys" := do
  shouldEvalTo (.send (.lit (.dict [(.symbol "a", .int 1), (.symbol "b", .int 2)])) "keys" [])
    (.array [.symbol "a", .symbol "b"])

test "dict values" := do
  shouldEvalTo (.send (.lit (.dict [(.symbol "a", .int 1), (.symbol "b", .int 2)])) "values" [])
    (.array [.int 1, .int 2])

-- ============ Identity Tests ============

test "int identity same" := do
  shouldEvalTo (.send (.lit (.int 5)) "==" [.lit (.int 5)]) (.bool true)

test "int identity different" := do
  shouldEvalTo (.send (.lit (.int 5)) "==" [.lit (.int 3)]) (.bool false)

test "int identity different type" := do
  shouldEvalTo (.send (.lit (.int 5)) "==" [.lit (.str "5")]) (.bool false)

test "int not identity" := do
  shouldEvalTo (.send (.lit (.int 5)) "~~" [.lit (.str "5")]) (.bool true)

test "nil identity" := do
  shouldEvalTo (.send (.lit .nil) "==" [.lit .nil]) (.bool true)

-- ============ Type Error Tests ============

test "int add string error" := do
  shouldEvalError (.send (.lit (.int 5)) "+" [.lit (.str "x")]) "expected Integer or Float"

test "bool and int error" := do
  shouldEvalError (.send (.lit (.bool true)) "&" [.lit (.int 1)]) "expected Boolean"

test "string concat int error" := do
  shouldEvalError (.send (.lit (.str "hi")) "," [.lit (.int 1)]) "expected String"

test "unknown selector error" := do
  shouldEvalError (.send (.lit (.int 5)) "foo" []) "No primitive"

-- ============ Complex Expression Tests ============

test "chained arithmetic" := do
  -- (3 + 4) * 2 = 14
  let expr := .send
    (.send (.lit (.int 3)) "+" [.lit (.int 4)])
    "*" [.lit (.int 2)]
  shouldEvalTo expr (.int 14)

test "comparison chain" := do
  -- (5 > 3) & (2 < 4) = true
  let expr := .send
    (.send (.lit (.int 5)) ">" [.lit (.int 3)])
    "&" [.send (.lit (.int 2)) "<" [.lit (.int 4)]]
  shouldEvalTo expr (.bool true)

test "expression with variable" := do
  -- x := 5. x + 3
  let program := mkProgram [
    .assign "x" (.lit (.int 5)),
    .send (.var "x") "+" [.lit (.int 3)]
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.int 8)) s!"expected 8, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

-- ============ Block and Closure Tests ============

test "block with no params value" := do
  -- [5] value => 5
  shouldEvalTo (.send (.block [] [] [.lit (.int 5)]) "value" []) (.int 5)

test "block with one param" := do
  -- [:x | x + 1] value: 5 => 6
  let block := Expr.block ["x"] [] [.send (.var "x") "+" [.lit (.int 1)]]
  shouldEvalTo (.send block "value:" [.lit (.int 5)]) (.int 6)

test "block with two params" := do
  -- [:x :y | x + y] value: 3 value: 4 => 7
  let block := Expr.block ["x", "y"] [] [.send (.var "x") "+" [.var "y"]]
  shouldEvalTo (.send block "value:value:" [.lit (.int 3), .lit (.int 4)]) (.int 7)

test "block with temps" := do
  -- [| t | t := 10. t + 5] value => 15
  let block := Expr.block [] ["t"] [.assign "t" (.lit (.int 10)), .send (.var "t") "+" [.lit (.int 5)]]
  shouldEvalTo (.send block "value" []) (.int 15)

test "block captures environment" := do
  -- y := 10. [:x | x + y] value: 5 => 15
  let program := mkProgram [
    .assign "y" (.lit (.int 10)),
    .send (.block ["x"] [] [.send (.var "x") "+" [.var "y"]]) "value:" [.lit (.int 5)]
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 15)) s!"expected 15, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "block wrong arity error" := do
  -- [5] value: 1 => error
  shouldEvalError (.send (.block [] [] [.lit (.int 5)]) "value:" [.lit (.int 1)])
    "Block expects 0 arguments"

-- ============ Boolean Control Flow Tests ============

test "ifTrue: executes when true" := do
  -- true ifTrue: [42] => 42
  shouldEvalTo
    (.send (.lit (.bool true)) "ifTrue:" [.block [] [] [.lit (.int 42)]])
    (.int 42)

test "ifTrue: returns nil when false" := do
  -- false ifTrue: [42] => nil
  shouldEvalTo
    (.send (.lit (.bool false)) "ifTrue:" [.block [] [] [.lit (.int 42)]])
    .nil

test "ifFalse: executes when false" := do
  -- false ifFalse: [42] => 42
  shouldEvalTo
    (.send (.lit (.bool false)) "ifFalse:" [.block [] [] [.lit (.int 42)]])
    (.int 42)

test "ifFalse: returns nil when true" := do
  -- true ifFalse: [42] => nil
  shouldEvalTo
    (.send (.lit (.bool true)) "ifFalse:" [.block [] [] [.lit (.int 42)]])
    .nil

test "ifTrue:ifFalse: true branch" := do
  -- true ifTrue: [1] ifFalse: [2] => 1
  shouldEvalTo
    (.send (.lit (.bool true)) "ifTrue:ifFalse:"
      [.block [] [] [.lit (.int 1)], .block [] [] [.lit (.int 2)]])
    (.int 1)

test "ifTrue:ifFalse: false branch" := do
  -- false ifTrue: [1] ifFalse: [2] => 2
  shouldEvalTo
    (.send (.lit (.bool false)) "ifTrue:ifFalse:"
      [.block [] [] [.lit (.int 1)], .block [] [] [.lit (.int 2)]])
    (.int 2)

test "ifFalse:ifTrue: true branch" := do
  -- true ifFalse: [1] ifTrue: [2] => 2
  shouldEvalTo
    (.send (.lit (.bool true)) "ifFalse:ifTrue:"
      [.block [] [] [.lit (.int 1)], .block [] [] [.lit (.int 2)]])
    (.int 2)

-- ============ Loop Tests ============

test "timesRepeat: basic" := do
  -- | i | i := 0. 5 timesRepeat: [i := i + 1]. i => 5
  let program := mkProgram [
    .assign "i" (.lit (.int 0)),
    .send (.lit (.int 5)) "timesRepeat:" [.block [] [] [.assign "i" (.send (.var "i") "+" [.lit (.int 1)])]],
    .var "i"
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 5)) s!"expected 5, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "timesRepeat: zero times" := do
  -- 0 timesRepeat: [x] => nil
  shouldEvalTo
    (.send (.lit (.int 0)) "timesRepeat:" [.block [] [] [.lit (.int 42)]])
    .nil

test "whileTrue: loop" := do
  -- | i | i := 0. [i < 3] whileTrue: [i := i + 1]. i => 3
  let program := mkProgram [
    .assign "i" (.lit (.int 0)),
    .send
      (.block [] [] [.send (.var "i") "<" [.lit (.int 3)]])
      "whileTrue:"
      [.block [] [] [.assign "i" (.send (.var "i") "+" [.lit (.int 1)])]],
    .var "i"
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 3)) s!"expected 3, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "whileFalse: loop" := do
  -- | i | i := 0. [i >= 3] whileFalse: [i := i + 1]. i => 3
  let program := mkProgram [
    .assign "i" (.lit (.int 0)),
    .send
      (.block [] [] [.send (.var "i") ">=" [.lit (.int 3)]])
      "whileFalse:"
      [.block [] [] [.assign "i" (.send (.var "i") "+" [.lit (.int 1)])]],
    .var "i"
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 3)) s!"expected 3, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "to:do: loop" := do
  -- | sum | sum := 0. 1 to: 5 do: [:i | sum := sum + i]. sum => 15
  let program := mkProgram [
    .assign "sum" (.lit (.int 0)),
    .send (.lit (.int 1)) "to:do:"
      [.lit (.int 5), .block ["i"] [] [.assign "sum" (.send (.var "sum") "+" [.var "i"])]],
    .var "sum"
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 15)) s!"expected 15, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

-- ============ Non-Local Return Tests ============

test "non-local return from block in method" := do
  -- Object subclass with method that has block with return
  let testClass : ClassDef := {
    name := "TestReturn",
    super := some "Object",
    ivars := [],
    methods := [
      { selector := "earlyReturn", params := [], temps := [], pragmas := [],
        body := [
          .send (.lit (.bool true)) "ifTrue:" [.block [] [] [.return (.lit (.int 42))]],
          .lit (.int 99)  -- Should not reach here
        ] }
    ]
  }
  let program := mkProgramWithClasses [testClass] [
    .assign "t" (.send (.var "TestReturn") "new" []),
    .send (.var "t") "earlyReturn" []
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 42)) s!"expected 42, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "return outside block ends method" := do
  let testClass : ClassDef := {
    name := "TestReturn2",
    super := some "Object",
    ivars := [],
    methods := [
      { selector := "directReturn", params := [], temps := [], pragmas := [],
        body := [
          .return (.lit (.int 100)),
          .lit (.int 200)  -- Should not reach here
        ] }
    ]
  }
  let program := mkProgramWithClasses [testClass] [
    .assign "t" (.send (.var "TestReturn2") "new" []),
    .send (.var "t") "directReturn" []
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 100)) s!"expected 100, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

-- ============ Complex Block Tests ============

test "nested blocks" := do
  -- [[:x | x + 1] value: 5] value => 6
  let innerBlock := Expr.block ["x"] [] [.send (.var "x") "+" [.lit (.int 1)]]
  let outerBlock := Expr.block [] [] [.send innerBlock "value:" [.lit (.int 5)]]
  shouldEvalTo (.send outerBlock "value" []) (.int 6)

test "block modifies outer variable" := do
  -- | x | x := 0. [x := x + 10] value. x => 10
  let program := mkProgram [
    .assign "x" (.lit (.int 0)),
    .send (.block [] [] [.assign "x" (.send (.var "x") "+" [.lit (.int 10)])]) "value" [],
    .var "x"
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 10)) s!"expected 10, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "factorial with blocks" := do
  -- | fact n | n := 5. fact := 1. [n > 0] whileTrue: [fact := fact * n. n := n - 1]. fact => 120
  let program := mkProgram [
    .assign "n" (.lit (.int 5)),
    .assign "fact" (.lit (.int 1)),
    .send
      (.block [] [] [.send (.var "n") ">" [.lit (.int 0)]])
      "whileTrue:"
      [.block [] [] [
        .assign "fact" (.send (.var "fact") "*" [.var "n"]),
        .assign "n" (.send (.var "n") "-" [.lit (.int 1)])
      ]],
    .var "fact"
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 120)) s!"expected 120, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

-- ============ Cascade Tests ============

test "cascade single message returns receiver" := do
  -- 5 negated → sends negated to 5 (-5), but returns 5 (the receiver)
  shouldEvalTo (.cascade (.lit (.int 5)) [[("negated", [])]]) (.int 5)

test "cascade multiple messages returns receiver" := do
  -- 5 negated; abs → sends both to 5, returns 5
  shouldEvalTo (.cascade (.lit (.int 5)) [[("negated", [])], [("abs", [])]]) (.int 5)

test "cascade with string" := do
  -- "hello" size; isEmpty → sends size (5), isEmpty (false), returns "hello"
  shouldEvalTo (.cascade (.lit (.str "hello")) [[("size", [])], [("isEmpty", [])]])
    (.str "hello")

test "cascade with binary message" := do
  -- 5 + 1; - 2 → sends both to 5, returns 5
  shouldEvalTo (.cascade (.lit (.int 5)) [[("+", [.lit (.int 1)])], [("-", [.lit (.int 2)])]])
    (.int 5)

test "cascade error propagates" := do
  -- 5 foo → unknown selector error
  shouldEvalError (.cascade (.lit (.int 5)) [[("foo", [])]]) "No primitive"

test "cascade with variable receiver" := do
  let program := mkProgram [
    .assign "x" (.lit (.int 10)),
    .cascade (.var "x") [[("negated", [])], [("abs", [])]]
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 10)) "should return 10"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "cascade with side effect in args" := do
  -- Verify arguments are evaluated for each message
  let program := mkProgram [
    .assign "count" (.lit (.int 0)),
    .cascade (.lit (.int 5)) [
      [("+", [.assign "count" (.send (.var "count") "+" [.lit (.int 1)])])],
      [("+", [.assign "count" (.send (.var "count") "+" [.lit (.int 1)])])]
    ],
    .var "count"
  ]
  -- count should be 2 after both cascade messages evaluated their args
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 2)) "count should be 2"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

-- ============ Self Tests ============

test "self outside method errors" := do
  shouldEvalError (.var "self") "self used outside method context"

-- ============ Class and Object Tests ============

-- Test class definitions
def counterClass : ClassDef := {
  name := "Counter",
  super := some "Object",
  ivars := ["count"],
  methods := [
    { selector := "count", params := [], temps := [], pragmas := [],
      body := [.var "count"] },
    { selector := "count:", params := ["n"], temps := [], pragmas := [],
      body := [.assign "count" (.var "n")] },
    { selector := "increment", params := [], temps := [], pragmas := [],
      body := [.assign "count" (.send (.var "count") "+" [.lit (.int 1)])] }
  ]
}

def pointClass : ClassDef := {
  name := "Point",
  super := some "Object",
  ivars := ["x", "y"],
  methods := [
    { selector := "x", params := [], temps := [], pragmas := [],
      body := [.var "x"] },
    { selector := "y", params := [], temps := [], pragmas := [],
      body := [.var "y"] },
    { selector := "x:", params := ["val"], temps := [], pragmas := [],
      body := [.assign "x" (.var "val")] },
    { selector := "y:", params := ["val"], temps := [], pragmas := [],
      body := [.assign "y" (.var "val")] },
    { selector := "setX:y:", params := ["newX", "newY"], temps := [], pragmas := [],
      body := [.assign "x" (.var "newX"), .assign "y" (.var "newY")] }
  ]
}

def animalClass : ClassDef := {
  name := "Animal",
  super := some "Object",
  ivars := ["name"],
  methods := [
    { selector := "name", params := [], temps := [], pragmas := [],
      body := [.var "name"] },
    { selector := "speak", params := [], temps := [], pragmas := [],
      body := [.lit (.str "...")] }
  ]
}

def dogClass : ClassDef := {
  name := "Dog",
  super := some "Animal",
  ivars := ["breed"],
  methods := [
    { selector := "breed", params := [], temps := [], pragmas := [],
      body := [.var "breed"] },
    { selector := "speak", params := [], temps := [], pragmas := [],
      body := [.lit (.str "Woof!")] },
    { selector := "parentSpeak", params := [], temps := [], pragmas := [],
      body := [.send (.var "super") "speak" []] }
  ]
}

def factoryClass : ClassDef := {
  name := "Factory",
  super := some "Object",
  ivars := [],
  methods := [],
  classMethods := [
    { selector := "answer", params := [], temps := [], pragmas := [],
      body := [.lit (.int 42)] }
  ]
}

test "instantiate object with new" := do
  let program := mkProgramWithClasses [counterClass] [
    .send (.var "Counter") "new" []
  ]
  match Smalltalk.evalProgram program with
  | .ok (.object _ className fields) =>
      shouldSatisfy (className == "Counter") s!"expected Counter, got {className}"
      shouldSatisfy (fields.length == 1) s!"expected 1 field, got {fields.length}"
  | .ok v =>
      throw (IO.userError s!"expected object, got {reprStr v}")
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

test "class-side method dispatch" := do
  let program := mkProgramWithClasses [factoryClass] [
    .send (.var "Factory") "answer" []
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.int 42)) s!"expected 42, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

test "object identity uses ids" := do
  let program := mkProgramWithClasses [counterClass] [
    .assign "a" (.send (.var "Counter") "new" []),
    .assign "b" (.send (.var "Counter") "new" []),
    .send (.var "a") "==" [.var "b"]
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.bool false)) s!"expected false, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

test "instance variable initialized to nil" := do
  let program := mkProgramWithClasses [counterClass] [
    .assign "c" (.send (.var "Counter") "new" []),
    .send (.var "c") "count" []
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr Value.nil) s!"expected nil, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

test "set and get instance variable" := do
  let program := mkProgramWithClasses [counterClass] [
    .assign "c" (.send (.var "Counter") "new" []),
    .send (.var "c") "count:" [.lit (.int 10)],
    .send (.var "c") "count" []
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.int 10)) s!"expected 10, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

test "method modifies instance variable" := do
  let program := mkProgramWithClasses [counterClass] [
    .assign "c" (.send (.var "Counter") "new" []),
    .send (.var "c") "count:" [.lit (.int 5)],
    .send (.var "c") "increment" [],
    .send (.var "c") "count" []
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.int 6)) s!"expected 6, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

test "multiple instance variables" := do
  let program := mkProgramWithClasses [pointClass] [
    .assign "p" (.send (.var "Point") "new" []),
    .send (.var "p") "x:" [.lit (.int 3)],
    .send (.var "p") "y:" [.lit (.int 4)],
    .send (.send (.var "p") "x" []) "+" [.send (.var "p") "y" []]
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.int 7)) s!"expected 7, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

test "keyword method with multiple params" := do
  let program := mkProgramWithClasses [pointClass] [
    .assign "p" (.send (.var "Point") "new" []),
    .send (.var "p") "setX:y:" [.lit (.int 10), .lit (.int 20)],
    .send (.var "p") "y" []
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.int 20)) s!"expected 20, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

test "inheritance method lookup" := do
  let program := mkProgramWithClasses [animalClass, dogClass] [
    .assign "d" (.send (.var "Dog") "new" []),
    .send (.var "d") "speak" []
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.str "Woof!")) s!"expected Woof!, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

test "inherited method from superclass" := do
  let program := mkProgramWithClasses [animalClass, dogClass] [
    .assign "a" (.send (.var "Animal") "new" []),
    .send (.var "a") "speak" []
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.str "...")) s!"expected ..., got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

test "super call invokes superclass method" := do
  let program := mkProgramWithClasses [animalClass, dogClass] [
    .assign "d" (.send (.var "Dog") "new" []),
    .send (.var "d") "parentSpeak" []
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.str "...")) s!"expected ..., got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

test "super outside method errors" := do
  shouldEvalError (.var "super") "super used outside method context"

test "core class Object exists" := do
  let program := mkProgram [
    .send (.var "Object") "new" []
  ]
  match Smalltalk.evalProgram program with
  | .ok (.object _ className _) =>
      shouldSatisfy (className == "Object") s!"expected Object, got {className}"
  | .ok v =>
      throw (IO.userError s!"expected object, got {reprStr v}")
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

test "wrong arity error" := do
  let program := mkProgramWithClasses [counterClass] [
    .assign "c" (.send (.var "Counter") "new" []),
    .send (.var "c") "count:" []  -- missing argument
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      throw (IO.userError s!"expected error, got value: {reprStr v}")
  | .error e =>
      shouldSatisfy (e.message.containsSubstr "Wrong arity") s!"expected Wrong arity error, got: {e.message}"

test "method uses temporary variable" := do
  let tempClass : ClassDef := {
    name := "TempTest",
    super := some "Object",
    ivars := [],
    methods := [
      { selector := "compute", params := [], temps := ["temp"], pragmas := [],
        body := [
          .assign "temp" (.lit (.int 42)),
          .var "temp"
        ] }
    ]
  }
  let program := mkProgramWithClasses [tempClass] [
    .assign "t" (.send (.var "TempTest") "new" []),
    .send (.var "t") "compute" []
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.int 42)) s!"expected 42, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

-- ============ Primitive Method Lookup Tests ============

-- User-defined method on Integer class
def integerWithSquared : ClassDef := {
  name := "Integer",
  super := some "Object",
  ivars := [],
  methods := [
    { selector := "squared", params := [], temps := [], pragmas := [],
      body := [.send (.var "self") "*" [.var "self"]] }
  ]
}

test "user-defined method on Integer" := do
  let program := mkProgramWithClasses [integerWithSquared] [
    .send (.lit (.int 5)) "squared" []
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.int 25)) s!"expected 25, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

-- User-defined method on String class
def stringWithReverse : ClassDef := {
  name := "String",
  super := some "Object",
  ivars := [],
  methods := [
    { selector := "reversed", params := [], temps := [], pragmas := [],
      -- Return self for now since we don't have string reversal primitive
      body := [.var "self"] }
  ]
}

test "user-defined method on String" := do
  let program := mkProgramWithClasses [stringWithReverse] [
    .send (.lit (.str "hello")) "reversed" []
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.str "hello")) s!"expected hello, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

-- Test that primitives still work when no user method defined
test "primitives work without user methods" := do
  let program := mkProgram [
    .send (.lit (.int 5)) "+" [.lit (.int 3)]
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.int 8)) s!"expected 8, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

-- Test primitive pragma with fallback
def integerWithPrimitivePlus : ClassDef := {
  name := "Integer",
  super := some "Object",
  ivars := [],
  methods := [
    { selector := "+", params := ["n"], temps := [],
      pragmas := [{ selector := "primitive:", args := [.int 1] }],
      body := [.lit (.int 999)] }  -- Fallback if primitive fails
  ]
}

test "primitive pragma uses primitive first" := do
  let program := mkProgramWithClasses [integerWithPrimitivePlus] [
    .send (.lit (.int 5)) "+" [.lit (.int 3)]
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      -- Primitive should succeed, returning 8 (not the fallback 999)
      shouldSatisfy (vStr == reprStr (Value.int 8)) s!"expected 8, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

-- Test True/False class distinction
test "true and false have different classes" := do
  -- Define a method only on True
  let trueClass : ClassDef := {
    name := "True",
    super := some "Boolean",
    ivars := [],
    methods := [
      { selector := "isTrue", params := [], temps := [], pragmas := [],
        body := [.lit (.str "yes")] }
    ]
  }
  let falseClass : ClassDef := {
    name := "False",
    super := some "Boolean",
    ivars := [],
    methods := [
      { selector := "isTrue", params := [], temps := [], pragmas := [],
        body := [.lit (.str "no")] }
    ]
  }
  let program := mkProgramWithClasses [trueClass, falseClass] [
    .send (.lit (.bool true)) "isTrue" []
  ]
  match Smalltalk.evalProgram program with
  | .ok v =>
      let vStr := reprStr v
      shouldSatisfy (vStr == reprStr (Value.str "yes")) s!"expected yes, got {vStr}"
  | .error e =>
      throw (IO.userError s!"unexpected error: {e.message}")

-- ============ Collection Iteration Tests ============

-- Array do: tests
test "array do: iterates all elements" := do
  -- | sum | sum := 0. #(1 2 3) do: [:x | sum := sum + x]. sum => 6
  let program := mkProgram [
    .assign "sum" (.lit (.int 0)),
    .send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3)]) "do:"
      [.block ["x"] [] [.assign "sum" (.send (.var "sum") "+" [.var "x"])]],
    .var "sum"
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 6)) s!"expected 6, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "array do: returns nil" := do
  shouldEvalTo
    (.send (.array [.lit (.int 1), .lit (.int 2)]) "do:" [.block ["x"] [] [.var "x"]])
    .nil

test "array do: empty array" := do
  let program := mkProgram [
    .assign "count" (.lit (.int 0)),
    .send (.array []) "do:" [.block ["x"] [] [.assign "count" (.send (.var "count") "+" [.lit (.int 1)])]],
    .var "count"
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 0)) s!"expected 0, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

-- Array collect: tests
test "array collect: transforms elements" := do
  -- #(1 2 3) collect: [:x | x * 2] => #(2 4 6)
  shouldEvalTo
    (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3)]) "collect:"
      [.block ["x"] [] [.send (.var "x") "*" [.lit (.int 2)]]])
    (.array [.int 2, .int 4, .int 6])

test "array collect: empty array" := do
  shouldEvalTo
    (.send (.array []) "collect:" [.block ["x"] [] [.send (.var "x") "*" [.lit (.int 2)]]])
    (.array [])

test "array collect: type change" := do
  -- #(1 2 3) collect: [:x | x > 1] => #(false true true)
  shouldEvalTo
    (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3)]) "collect:"
      [.block ["x"] [] [.send (.var "x") ">" [.lit (.int 1)]]])
    (.array [.bool false, .bool true, .bool true])

-- Array select: tests
test "array select: filters elements" := do
  -- #(1 2 3 4 5) select: [:x | x > 2] => #(3 4 5)
  shouldEvalTo
    (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3), .lit (.int 4), .lit (.int 5)])
      "select:" [.block ["x"] [] [.send (.var "x") ">" [.lit (.int 2)]]])
    (.array [.int 3, .int 4, .int 5])

test "array select: empty result" := do
  -- #(1 2 3) select: [:x | x > 10] => #()
  shouldEvalTo
    (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3)])
      "select:" [.block ["x"] [] [.send (.var "x") ">" [.lit (.int 10)]]])
    (.array [])

test "array select: all elements" := do
  -- #(1 2 3) select: [:x | x > 0] => #(1 2 3)
  shouldEvalTo
    (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3)])
      "select:" [.block ["x"] [] [.send (.var "x") ">" [.lit (.int 0)]]])
    (.array [.int 1, .int 2, .int 3])

-- Array reject: tests
test "array reject: filters out elements" := do
  -- #(1 2 3 4 5) reject: [:x | x > 2] => #(1 2)
  shouldEvalTo
    (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3), .lit (.int 4), .lit (.int 5)])
      "reject:" [.block ["x"] [] [.send (.var "x") ">" [.lit (.int 2)]]])
    (.array [.int 1, .int 2])

test "array reject: keep all elements" := do
  -- #(1 2 3) reject: [:x | x > 10] => #(1 2 3)
  shouldEvalTo
    (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3)])
      "reject:" [.block ["x"] [] [.send (.var "x") ">" [.lit (.int 10)]]])
    (.array [.int 1, .int 2, .int 3])

-- Array detect: tests
test "array detect: finds first match" := do
  -- #(1 2 3 4 5) detect: [:x | x > 2] => 3
  shouldEvalTo
    (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3), .lit (.int 4), .lit (.int 5)])
      "detect:" [.block ["x"] [] [.send (.var "x") ">" [.lit (.int 2)]]])
    (.int 3)

test "array detect: no match error" := do
  shouldEvalError
    (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3)])
      "detect:" [.block ["x"] [] [.send (.var "x") ">" [.lit (.int 10)]]])
    "no element found"

-- Array detect:ifNone: tests
test "array detect:ifNone: finds match" := do
  -- #(1 2 3) detect: [:x | x > 2] ifNone: [0] => 3
  shouldEvalTo
    (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3)])
      "detect:ifNone:" [.block ["x"] [] [.send (.var "x") ">" [.lit (.int 2)]], .block [] [] [.lit (.int 0)]])
    (.int 3)

test "array detect:ifNone: returns none block result" := do
  -- #(1 2 3) detect: [:x | x > 10] ifNone: [99] => 99
  shouldEvalTo
    (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3)])
      "detect:ifNone:" [.block ["x"] [] [.send (.var "x") ">" [.lit (.int 10)]], .block [] [] [.lit (.int 99)]])
    (.int 99)

-- Array inject:into: tests
test "array inject:into: sums elements" := do
  -- #(1 2 3 4) inject: 0 into: [:sum :x | sum + x] => 10
  shouldEvalTo
    (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3), .lit (.int 4)])
      "inject:into:" [.lit (.int 0), .block ["sum", "x"] [] [.send (.var "sum") "+" [.var "x"]]])
    (.int 10)

test "array inject:into: product of elements" := do
  -- #(1 2 3 4) inject: 1 into: [:prod :x | prod * x] => 24
  shouldEvalTo
    (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3), .lit (.int 4)])
      "inject:into:" [.lit (.int 1), .block ["prod", "x"] [] [.send (.var "prod") "*" [.var "x"]]])
    (.int 24)

test "array inject:into: empty array" := do
  -- #() inject: 42 into: [:acc :x | acc + x] => 42
  shouldEvalTo
    (.send (.array [])
      "inject:into:" [.lit (.int 42), .block ["acc", "x"] [] [.send (.var "acc") "+" [.var "x"]]])
    (.int 42)

-- Array includes: tests
test "array includes: found" := do
  -- #(1 2 3) includes: 2 => true
  shouldEvalTo
    (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3)]) "includes:" [.lit (.int 2)])
    (.bool true)

test "array includes: not found" := do
  -- #(1 2 3) includes: 5 => false
  shouldEvalTo
    (.send (.array [.lit (.int 1), .lit (.int 2), .lit (.int 3)]) "includes:" [.lit (.int 5)])
    (.bool false)

-- Array indexOf: tests
test "array indexOf: found" := do
  -- #(10 20 30) indexOf: 20 => 2
  shouldEvalTo
    (.send (.array [.lit (.int 10), .lit (.int 20), .lit (.int 30)]) "indexOf:" [.lit (.int 20)])
    (.int 2)

test "array indexOf: not found" := do
  -- #(10 20 30) indexOf: 50 => 0
  shouldEvalTo
    (.send (.array [.lit (.int 10), .lit (.int 20), .lit (.int 30)]) "indexOf:" [.lit (.int 50)])
    (.int 0)

-- String iteration tests
test "string do: iterates characters" := do
  -- | count | count := 0. 'abc' do: [:c | count := count + 1]. count => 3
  let program := mkProgram [
    .assign "count" (.lit (.int 0)),
    .send (.lit (.str "abc")) "do:"
      [.block ["c"] [] [.assign "count" (.send (.var "count") "+" [.lit (.int 1)])]],
    .var "count"
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 3)) s!"expected 3, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "string collect: transforms to array" := do
  -- 'abc' collect: [:c | c asInteger] => #(97 98 99)
  shouldEvalTo
    (.send (.lit (.str "abc")) "collect:"
      [.block ["c"] [] [.send (.var "c") "asInteger" []]])
    (.array [.int 97, .int 98, .int 99])

test "string collect: char to char returns string" := do
  -- 'abc' collect: [:c | c asUppercase] => 'ABC'
  shouldEvalTo
    (.send (.lit (.str "abc")) "collect:"
      [.block ["c"] [] [.send (.var "c") "asUppercase" []]])
    (.str "ABC")

test "string select: filters characters" := do
  -- 'aAbBcC' select: [:c | c isLetter & (c asLowercase = c)] => 'abc'
  -- Simplified: select vowels from 'aeiou123'
  shouldEvalTo
    (.send (.lit (.str "a1b2c3")) "select:"
      [.block ["c"] [] [.send (.var "c") "isLetter" []]])
    (.str "abc")

test "string reject: filters out characters" := do
  -- 'a1b2c3' reject: [:c | c isDigit] => 'abc'
  shouldEvalTo
    (.send (.lit (.str "a1b2c3")) "reject:"
      [.block ["c"] [] [.send (.var "c") "isDigit" []]])
    (.str "abc")

test "string detect: finds first match" := do
  -- 'abc123' detect: [:c | c isDigit] => $1
  shouldEvalTo
    (.send (.lit (.str "abc123")) "detect:"
      [.block ["c"] [] [.send (.var "c") "isDigit" []]])
    (.char '1')

test "string detect:ifNone: not found" := do
  -- 'abc' detect: [:c | c isDigit] ifNone: [$x] => $x
  shouldEvalTo
    (.send (.lit (.str "abc")) "detect:ifNone:"
      [.block ["c"] [] [.send (.var "c") "isDigit" []], .block [] [] [.lit (.char 'x')]])
    (.char 'x')

test "string inject:into: concatenates" := do
  -- 'abc' inject: '' into: [:acc :c | acc , c asString] would build string
  -- Simpler: count characters
  let program := mkProgram [
    .send (.lit (.str "hello")) "inject:into:"
      [.lit (.int 0), .block ["acc", "c"] [] [.send (.var "acc") "+" [.lit (.int 1)]]]
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 5)) s!"expected 5, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "string includes: char found" := do
  shouldEvalTo
    (.send (.lit (.str "hello")) "includes:" [.lit (.char 'e')])
    (.bool true)

test "string includes: char not found" := do
  shouldEvalTo
    (.send (.lit (.str "hello")) "includes:" [.lit (.char 'x')])
    (.bool false)

test "string includes: substring found" := do
  shouldEvalTo
    (.send (.lit (.str "hello world")) "includes:" [.lit (.str "wor")])
    (.bool true)

test "string includes: substring not found" := do
  shouldEvalTo
    (.send (.lit (.str "hello")) "includes:" [.lit (.str "xyz")])
    (.bool false)

-- Dictionary iteration tests
test "dict do: iterates values" := do
  -- | sum | sum := 0. #{#a -> 1. #b -> 2. #c -> 3} do: [:v | sum := sum + v]. sum => 6
  let program := mkProgram [
    .assign "sum" (.lit (.int 0)),
    .send (.lit (.dict [(.symbol "a", .int 1), (.symbol "b", .int 2), (.symbol "c", .int 3)])) "do:"
      [.block ["v"] [] [.assign "sum" (.send (.var "sum") "+" [.var "v"])]],
    .var "sum"
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 6)) s!"expected 6, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "dict keysAndValuesDo: iterates pairs" := do
  -- | result | result := ''. #{#a -> 1} keysAndValuesDo: [:k :v | ...]. result
  let program := mkProgram [
    .assign "keyCount" (.lit (.int 0)),
    .assign "valueSum" (.lit (.int 0)),
    .send (.lit (.dict [(.symbol "a", .int 10), (.symbol "b", .int 20)])) "keysAndValuesDo:"
      [.block ["k", "v"] [] [
        .assign "keyCount" (.send (.var "keyCount") "+" [.lit (.int 1)]),
        .assign "valueSum" (.send (.var "valueSum") "+" [.var "v"])
      ]],
    .send (.var "keyCount") "+" [.var "valueSum"]
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 32)) s!"expected 32, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "dict collect: transforms values" := do
  -- #{#a -> 1. #b -> 2} collect: [:v | v * 2] => #(2 4)
  shouldEvalTo
    (.send (.lit (.dict [(.symbol "a", .int 1), (.symbol "b", .int 2)])) "collect:"
      [.block ["v"] [] [.send (.var "v") "*" [.lit (.int 2)]]])
    (.array [.int 2, .int 4])

test "dict select: filters entries" := do
  -- #{#a -> 1. #b -> 2. #c -> 3} select: [:v | v > 1] => #{#b -> 2. #c -> 3}
  shouldEvalTo
    (.send (.lit (.dict [(.symbol "a", .int 1), (.symbol "b", .int 2), (.symbol "c", .int 3)])) "select:"
      [.block ["v"] [] [.send (.var "v") ">" [.lit (.int 1)]]])
    (.dict [(.symbol "b", .int 2), (.symbol "c", .int 3)])

test "dict reject: filters out entries" := do
  -- #{#a -> 1. #b -> 2. #c -> 3} reject: [:v | v > 1] => #{#a -> 1}
  shouldEvalTo
    (.send (.lit (.dict [(.symbol "a", .int 1), (.symbol "b", .int 2), (.symbol "c", .int 3)])) "reject:"
      [.block ["v"] [] [.send (.var "v") ">" [.lit (.int 1)]]])
    (.dict [(.symbol "a", .int 1)])

test "dict detect: finds first value" := do
  -- #{#a -> 1. #b -> 2. #c -> 3} detect: [:v | v > 1] => 2
  shouldEvalTo
    (.send (.lit (.dict [(.symbol "a", .int 1), (.symbol "b", .int 2), (.symbol "c", .int 3)])) "detect:"
      [.block ["v"] [] [.send (.var "v") ">" [.lit (.int 1)]]])
    (.int 2)

test "dict detect:ifNone: not found" := do
  shouldEvalTo
    (.send (.lit (.dict [(.symbol "a", .int 1)])) "detect:ifNone:"
      [.block ["v"] [] [.send (.var "v") ">" [.lit (.int 10)]], .block [] [] [.lit (.int 99)]])
    (.int 99)

test "dict inject:into: accumulates values" := do
  -- #{#a -> 1. #b -> 2. #c -> 3} inject: 0 into: [:sum :v | sum + v] => 6
  shouldEvalTo
    (.send (.lit (.dict [(.symbol "a", .int 1), (.symbol "b", .int 2), (.symbol "c", .int 3)]))
      "inject:into:" [.lit (.int 0), .block ["sum", "v"] [] [.send (.var "sum") "+" [.var "v"]]])
    (.int 6)

-- Error handling tests for iteration
test "array select: non-boolean error" := do
  shouldEvalError
    (.send (.array [.lit (.int 1)]) "select:" [.block ["x"] [] [.var "x"]])
    "must return Boolean"

test "array reject: non-boolean error" := do
  shouldEvalError
    (.send (.array [.lit (.int 1)]) "reject:" [.block ["x"] [] [.var "x"]])
    "must return Boolean"

test "array detect: non-boolean error" := do
  shouldEvalError
    (.send (.array [.lit (.int 1)]) "detect:" [.block ["x"] [] [.var "x"]])
    "must return Boolean"

-- ============ Exception Handling Tests ============

-- Basic signal and catch
test "on:do: catches matching exception" := do
  -- [[Error signal: 'oops'] on: #Error do: [:ex | #caught]] => #caught
  let program := mkProgram [
    .send
      (.block [] [] [.send (.var "Error") "signal:" [.lit (.str "oops")]])
      "on:do:"
      [.lit (.symbol "Error"), .block ["ex"] [] [.lit (.symbol "caught")]]
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.symbol "caught")) s!"expected #caught, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "on:do: propagates non-matching exception" := do
  -- [[Error signal: 'oops'] on: #Warning do: [:ex | #caught]] => error (unhandled)
  let program := mkProgram [
    .send
      (.block [] [] [.send (.var "Error") "signal:" [.lit (.str "oops")]])
      "on:do:"
      [.lit (.symbol "Warning"), .block ["ex"] [] [.lit (.symbol "caught")]]
  ]
  match Smalltalk.evalProgram program with
  | .ok v => throw (IO.userError s!"expected unhandled exception, got {reprStr v}")
  | .error _ => pure ()  -- Expected - exception propagates

test "on:do: catches subclass exceptions" := do
  -- [[Error signal: 'oops'] on: #Exception do: [:ex | #caught]] => #caught
  let program := mkProgram [
    .send
      (.block [] [] [.send (.var "Error") "signal:" [.lit (.str "oops")]])
      "on:do:"
      [.lit (.symbol "Exception"), .block ["ex"] [] [.lit (.symbol "caught")]]
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.symbol "caught")) s!"expected #caught, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "on:do: no exception returns normally" := do
  -- [[42] on: #Error do: [:ex | 0]] => 42
  shouldEvalTo
    (.send
      (.block [] [] [.lit (.int 42)])
      "on:do:"
      [.lit (.symbol "Error"), .block ["ex"] [] [.lit (.int 0)]])
    (.int 42)

-- Ensure always runs
test "ensure: runs on success" := do
  -- | ran | ran := false. [[42] ensure: [ran := true]]. ran => true
  let program := mkProgram [
    .assign "ran" (.lit (.bool false)),
    .send
      (.block [] [] [.lit (.int 42)])
      "ensure:"
      [.block [] [] [.assign "ran" (.lit (.bool true))]],
    .var "ran"
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.bool true)) s!"expected true, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "ensure: runs on exception" := do
  -- | ran | ran := false. [[[Error signal: 'x'] ensure: [ran := true]] on: #Error do: [:e | nil]]. ran => true
  let program := mkProgram [
    .assign "ran" (.lit (.bool false)),
    .send
      (.block [] [] [
        .send
          (.block [] [] [.send (.var "Error") "signal:" [.lit (.str "x")]])
          "ensure:"
          [.block [] [] [.assign "ran" (.lit (.bool true))]]
      ])
      "on:do:"
      [.lit (.symbol "Error"), .block ["e"] [] [.lit .nil]],
    .var "ran"
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.bool true)) s!"expected true, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "ensure: returns original result" := do
  -- [[42] ensure: [99]] => 42 (ensure result is discarded)
  shouldEvalTo
    (.send
      (.block [] [] [.lit (.int 42)])
      "ensure:"
      [.block [] [] [.lit (.int 99)]])
    (.int 42)

-- ifCurtailed only runs on exception
test "ifCurtailed: does not run on success" := do
  -- | ran | ran := false. [[42] ifCurtailed: [ran := true]]. ran => false
  let program := mkProgram [
    .assign "ran" (.lit (.bool false)),
    .send
      (.block [] [] [.lit (.int 42)])
      "ifCurtailed:"
      [.block [] [] [.assign "ran" (.lit (.bool true))]],
    .var "ran"
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.bool false)) s!"expected false, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

test "ifCurtailed: runs on exception" := do
  -- | ran | ran := false. [[[Error signal: 'x'] ifCurtailed: [ran := true]] on: #Error do: [:e | nil]]. ran => true
  let program := mkProgram [
    .assign "ran" (.lit (.bool false)),
    .send
      (.block [] [] [
        .send
          (.block [] [] [.send (.var "Error") "signal:" [.lit (.str "x")]])
          "ifCurtailed:"
          [.block [] [] [.assign "ran" (.lit (.bool true))]]
      ])
      "on:do:"
      [.lit (.symbol "Error"), .block ["e"] [] [.lit .nil]],
    .var "ran"
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.bool true)) s!"expected true, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

-- Handler receives exception object
test "on:do: handler receives exception with messageText" := do
  -- [[Error signal: 'hello'] on: #Error do: [:ex | ex messageText]] => 'hello'
  let program := mkProgram [
    .send
      (.block [] [] [.send (.var "Error") "signal:" [.lit (.str "hello")]])
      "on:do:"
      [.lit (.symbol "Error"), .block ["ex"] [] [.send (.var "ex") "messageText" []]]
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.str "hello")) s!"expected 'hello', got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

-- Signal without message
test "exception object signal works" := do
  -- | ex | ex := Error new. [[ex signal] on: #Error do: [:e | #caught]] => #caught
  let program := mkProgram [
    .assign "ex" (.send (.var "Error") "new" []),
    .send
      (.block [] [] [.send (.var "ex") "signal" []])
      "on:do:"
      [.lit (.symbol "Error"), .block ["e"] [] [.lit (.symbol "caught")]]
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.symbol "caught")) s!"expected #caught, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

-- Nested exception handling
test "nested on:do: handlers" := do
  -- [[[Error signal: 'x'] on: #Warning do: [:e | 1]] on: #Error do: [:e | 2]] => 2
  let program := mkProgram [
    .send
      (.block [] [] [
        .send
          (.block [] [] [.send (.var "Error") "signal:" [.lit (.str "x")]])
          "on:do:"
          [.lit (.symbol "Warning"), .block ["e"] [] [.lit (.int 1)]]
      ])
      "on:do:"
      [.lit (.symbol "Error"), .block ["e"] [] [.lit (.int 2)]]
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.int 2)) s!"expected 2, got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

-- Warning exception class
test "Warning is an exception class" := do
  let program := mkProgram [
    .send
      (.block [] [] [.send (.var "Warning") "signal:" [.lit (.str "caution")]])
      "on:do:"
      [.lit (.symbol "Warning"), .block ["ex"] [] [.send (.var "ex") "messageText" []]]
  ]
  match Smalltalk.evalProgram program with
  | .ok v => shouldSatisfy (reprStr v == reprStr (Value.str "caution")) s!"expected 'caution', got {reprStr v}"
  | .error e => throw (IO.userError s!"unexpected error: {e.message}")

-- Non-exception class signal: should error
test "signal: on non-exception class errors" := do
  shouldEvalError
    (.send (.var "Object") "signal:" [.lit (.str "test")])
    "is not an exception class"

-- Unhandled exception propagates
test "unhandled exception propagates" := do
  let program := mkProgram [
    .send (.var "Error") "signal:" [.lit (.str "unhandled")]
  ]
  match Smalltalk.evalProgram program with
  | .ok v => throw (IO.userError s!"expected unhandled exception, got {reprStr v}")
  | .error e =>
      -- Should have an exception value
      shouldSatisfy e.exceptionValue.isSome "expected exception value to be set"

end EvalTests

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

-- ============ Not Yet Implemented Tests ============

test "eval block returns error" := do
  shouldEvalError (.block [] [] [.lit (.int 1)]) "Blocks not yet implemented"

test "eval return returns error" := do
  shouldEvalError (.return (.lit (.int 1))) "Return not yet implemented"

test "eval cascade returns error" := do
  shouldEvalError (.cascade (.lit (.int 1)) [[("foo", [])]]) "Cascades not yet implemented"

end EvalTests

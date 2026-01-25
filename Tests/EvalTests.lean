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

-- ============ Not Yet Implemented Tests ============

test "eval message send returns error" := do
  shouldEvalError (.send (.lit (.int 1)) "+" [.lit (.int 2)]) "Message sends not yet implemented"

test "eval block returns error" := do
  shouldEvalError (.block [] [] [.lit (.int 1)]) "Blocks not yet implemented"

test "eval return returns error" := do
  shouldEvalError (.return (.lit (.int 1))) "Return not yet implemented"

test "eval cascade returns error" := do
  shouldEvalError (.cascade (.lit (.int 1)) [[("foo", [])]]) "Cascades not yet implemented"

end EvalTests

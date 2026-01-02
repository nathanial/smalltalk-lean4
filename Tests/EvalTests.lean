/-
  Smalltalk evaluator tests.
-/
import Crucible
import Smalltalk

open Crucible
open Smalltalk

namespace EvalTests

testSuite "Smalltalk.Eval"

test "eval stub returns error" := do
  let program : Program := { classes := [], main := [Expr.lit (.int 1)] }
  match Smalltalk.evalProgram program with
  | .ok _ => throw (IO.userError "expected eval error")
  | .error e =>
    shouldSatisfy (e.message == "interpreter not implemented") "expected stub error"

#generate_tests

end EvalTests

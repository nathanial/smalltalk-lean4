import Smalltalk.AST
import Smalltalk.Runtime

namespace Smalltalk

/-- Evaluation errors for the interpreter. -/
structure EvalError where
  message : String
  deriving Repr, BEq, Inhabited

/-- Interpreter state carrying the current environment. -/
structure ExecState where
  env : Env := []
  deriving Repr, Inhabited

/-- Evaluate a single expression (stub). -/
def evalExpr (_state : ExecState) (_expr : Expr) : Except EvalError (ExecState × Value) :=
  .error { message := "interpreter not implemented" }

/-- Evaluate a whole program (stub). -/
def evalProgram (_program : Program) : Except EvalError Value :=
  .error { message := "interpreter not implemented" }

end Smalltalk

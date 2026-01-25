import Smalltalk.AST
import Smalltalk.Runtime
import Smalltalk.Primitives

namespace Smalltalk

/-- Evaluation errors for the interpreter. -/
structure EvalError where
  message : String
  deriving Repr, BEq, Inhabited

/-- Interpreter state carrying the current environment. -/
structure ExecState where
  env : Env := []
  deriving Repr, Inhabited

/-- Convert a literal AST node to a runtime value. -/
partial def literalToValue : Literal → Value
  | .int n => .int n
  | .float f => .float f
  | .scaled m s => .float (Float.ofInt m / Float.ofNat (10 ^ s))
  | .str s => .str s
  | .char c => .char c
  | .symbol sym => .symbol sym
  | .array elems => .array (elems.map literalToValue)
  | .dict entries => .dict (entries.map fun (k, v) => (literalToValue k, literalToValue v))
  | .byteArray bytes => .array (bytes.map fun b => .int b.toNat)
  | .bool b => .bool b
  | .nil => .nil

mutual
  /-- Evaluate a sequence of expressions, returning the last value. -/
  partial def evalSeq (state : ExecState) (exprs : List Expr) : Except EvalError (ExecState × Value) :=
    match exprs with
    | [] => .ok (state, .nil)
    | [e] => evalExpr state e
    | e :: rest => do
        let (state', _) ← evalExpr state e
        evalSeq state' rest

  /-- Evaluate a list of expressions, collecting all values. -/
  partial def evalExprs (state : ExecState) (exprs : List Expr) : Except EvalError (ExecState × List Value) :=
    match exprs with
    | [] => .ok (state, [])
    | e :: rest => do
        let (state', v) ← evalExpr state e
        let (state'', vs) ← evalExprs state' rest
        .ok (state'', v :: vs)

  /-- Evaluate a single expression. -/
  partial def evalExpr (state : ExecState) (expr : Expr) : Except EvalError (ExecState × Value) :=
    match expr with
    | .lit lit => .ok (state, literalToValue lit)
    | .var name =>
        match envLookup state.env name with
        | some v => .ok (state, v)
        | none => .error { message := s!"Undefined variable: {name}" }
    | .assign name valueExpr => do
        let (state', value) ← evalExpr state valueExpr
        let newEnv := envInsert state'.env name value
        .ok ({ state' with env := newEnv }, value)
    | .seq exprs => evalSeq state exprs
    | .array elems => do
        let (state', values) ← evalExprs state elems
        .ok (state', .array values)
    | .send recvExpr sel argsExpr => do
        let (state', recvVal) ← evalExpr state recvExpr
        let (state'', argVals) ← evalExprs state' argsExpr
        match evalPrimitive recvVal sel argVals with
        | .ok v => .ok (state'', v)
        | .error e => .error { message := e.message }
    | .block _ _ _ => .error { message := "Blocks not yet implemented" }
    | .return _ => .error { message := "Return not yet implemented" }
    | .cascade _ _ => .error { message := "Cascades not yet implemented" }
end

/-- Evaluate a whole program. -/
def evalProgram (program : Program) : Except EvalError Value :=
  match evalSeq { env := emptyEnv } program.main with
  | .ok (_, value) => .ok value
  | .error e => .error e

end Smalltalk

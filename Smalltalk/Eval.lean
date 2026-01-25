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
  self : Option Value := none
  classes : ClassRegistry := []
  currentClass : Option Symbol := none
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

/-- Look up a method in the class hierarchy. Returns (definingClassName, method). -/
partial def lookupMethod (registry : ClassRegistry) (className : Symbol) (selector : Symbol)
    : Option (Symbol × Method) :=
  match registryLookup registry className with
  | none => none
  | some classDef =>
      match classDef.methods.find? (fun m => m.selector == selector) with
      | some method => some (className, method)
      | none =>
          match classDef.super with
          | none => none
          | some superName => lookupMethod registry superName selector

mutual
  /-- Evaluate a method call on an object. -/
  partial def evalMethodCall (state : ExecState) (receiver : Value) (definingClass : Symbol)
      (method : Method) (args : List Value) : Except EvalError (ExecState × Value) := do
    if method.params.length != args.length then
      .error { message := s!"Wrong arity for {method.selector}: expected {method.params.length}, got {args.length}" }
    else
      let paramBindings := method.params.zip args
      let tempBindings := method.temps.map (fun t => (t, Value.nil))
      let methodEnv := paramBindings ++ tempBindings
      let methodState := { state with
        env := methodEnv,
        self := some receiver,
        currentClass := some definingClass
      }
      match evalSeq methodState method.body with
      | .ok (finalState, result) =>
          -- Return state with modified object in self field (caller may need to update variable)
          .ok ({ state with self := finalState.self, classes := finalState.classes }, result)
      | .error e => .error e

  /-- Evaluate a message send, optionally updating a receiver variable after method call. -/
  partial def evalSend (state : ExecState) (recvVal : Value) (sel : Symbol) (argVals : List Value)
      (recvVarName : Option Symbol) : Except EvalError (ExecState × Value) := do
    -- Handle object instantiation: ClassName new
    if sel == "new" && argVals.isEmpty then
      match recvVal with
      | .symbol className =>
          match registryLookup state.classes className with
          | some classDef =>
              let fields := classDef.ivars.map (fun iv => (iv, Value.nil))
              .ok (state, .object className fields)
          | none =>
              match evalPrimitive recvVal sel argVals with
              | .ok v => .ok (state, v)
              | .error e => .error { message := e.message }
      | _ =>
          evalSendToValue state recvVal sel argVals recvVarName
    else
      evalSendToValue state recvVal sel argVals recvVarName

  /-- Dispatch message to a value (object or primitive). -/
  partial def evalSendToValue (state : ExecState) (recvVal : Value) (sel : Symbol) (argVals : List Value)
      (recvVarName : Option Symbol) : Except EvalError (ExecState × Value) := do
    -- Get the class name for this value (works for both objects and built-in types)
    let className := match recvVal with
      | .object cn _ => cn
      | .int _ => "Integer"
      | .float _ => "Float"
      | .str _ => "String"
      | .char _ => "Character"
      | .symbol _ => "Symbol"
      | .bool true => "True"
      | .bool false => "False"
      | .nil => "UndefinedObject"
      | .array _ => "Array"
      | .dict _ => "Dictionary"
    -- Try method lookup first (allows user-defined methods on built-in types)
    match lookupMethod state.classes className sel with
    | some (defClass, method) =>
        -- Check for primitive pragma
        let hasPrimitive := method.pragmas.any (fun p => p.selector == "primitive:")
        if hasPrimitive then
          -- Try primitive first, fall back to method body if primitive fails
          match evalPrimitive recvVal sel argVals with
          | .ok v => .ok (state, v)
          | .error _ =>
              -- Primitive failed, try method body
              match evalMethodCall state recvVal defClass method argVals with
              | .ok (resultState, result) =>
                  match recvVarName, resultState.self with
                  | some varName, some updatedObj =>
                      let newEnv := envInsert resultState.env varName updatedObj
                      .ok ({ resultState with env := newEnv, self := state.self }, result)
                  | _, _ =>
                      .ok ({ resultState with self := state.self }, result)
              | .error e => .error e
        else
          -- No primitive, just run the method
          match evalMethodCall state recvVal defClass method argVals with
          | .ok (resultState, result) =>
              match recvVarName, resultState.self with
              | some varName, some updatedObj =>
                  let newEnv := envInsert resultState.env varName updatedObj
                  .ok ({ resultState with env := newEnv, self := state.self }, result)
              | _, _ =>
                  .ok ({ resultState with self := state.self }, result)
          | .error e => .error e
    | none =>
        -- No method found, try primitive
        match evalPrimitive recvVal sel argVals with
        | .ok v => .ok (state, v)
        | .error e => .error { message := e.message }

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
    | .var "self" =>
        match state.self with
        | some v => .ok (state, v)
        | none => .error { message := "self used outside method context" }
    | .var "super" =>
        match state.self with
        | some v => .ok (state, v)  -- super evaluates to self, dispatch handled in send
        | none => .error { message := "super used outside method context" }
    | .var name =>
        -- 1. Check local environment first
        match envLookup state.env name with
        | some v => .ok (state, v)
        | none =>
            -- 2. Check instance variables if we're in a method context
            match state.self with
            | some (.object _ fields) =>
                match fields.find? (fun (n, _) => n == name) with
                | some (_, v) => .ok (state, v)
                | none =>
                    -- 3. Check if it's a class name
                    if registryLookup state.classes name |>.isSome then
                      .ok (state, .symbol name)
                    else
                      .error { message := s!"Undefined variable: {name}" }
            | _ =>
                -- 3. Check if it's a class name
                if registryLookup state.classes name |>.isSome then
                  .ok (state, .symbol name)
                else
                  .error { message := s!"Undefined variable: {name}" }
    | .assign name valueExpr => do
        let (state', value) ← evalExpr state valueExpr
        -- Check if assigning to an instance variable
        match state'.self with
        | some (.object cn fields) =>
            if fields.any (fun (n, _) => n == name) then
              -- Update instance variable
              let newFields := fields.map (fun (n, v) => if n == name then (n, value) else (n, v))
              let newSelf := Value.object cn newFields
              .ok ({ state' with self := some newSelf }, value)
            else
              -- Normal variable assignment
              .ok ({ state' with env := envInsert state'.env name value }, value)
        | _ =>
            -- Normal variable assignment
            .ok ({ state' with env := envInsert state'.env name value }, value)
    | .seq exprs => evalSeq state exprs
    | .array elems => do
        let (state', values) ← evalExprs state elems
        .ok (state', .array values)
    -- Special case for super sends: start method lookup from superclass
    | .send (.var "super") sel argsExpr => do
        match state.self, state.currentClass with
        | some recvVal, some currentClassName =>
            let (state', argVals) ← evalExprs state argsExpr
            match registryLookup state'.classes currentClassName with
            | none => .error { message := s!"Internal error: current class {currentClassName} not found" }
            | some currentClass =>
                match currentClass.super with
                | none => .error { message := s!"No superclass for {currentClassName}" }
                | some superName =>
                    match lookupMethod state'.classes superName sel with
                    | some (defClass, method) =>
                        evalMethodCall state' recvVal defClass method argVals
                    | none =>
                        -- Try primitive as fallback
                        match evalPrimitive recvVal sel argVals with
                        | .ok v => .ok (state', v)
                        | .error e => .error { message := e.message }
        | _, _ => .error { message := "super used outside method context" }
    -- Special case for sends where receiver is a variable - update variable after method call
    | .send (.var varName) sel argsExpr => do
        if varName == "self" || varName == "super" then
          -- Handle self/super sends with regular dispatch
          let (state', recvVal) ← evalExpr state (.var varName)
          let (state'', argVals) ← evalExprs state' argsExpr
          evalSend state'' recvVal sel argVals none
        else
          let (state', recvVal) ← evalExpr state (.var varName)
          let (state'', argVals) ← evalExprs state' argsExpr
          evalSend state'' recvVal sel argVals (some varName)
    | .send recvExpr sel argsExpr => do
        let (state', recvVal) ← evalExpr state recvExpr
        let (state'', argVals) ← evalExprs state' argsExpr
        evalSend state'' recvVal sel argVals none
    | .block _ _ _ => .error { message := "Blocks not yet implemented" }
    | .return _ => .error { message := "Return not yet implemented" }
    | .cascade recvExpr chains => do
        -- Evaluate receiver once
        let (state', recvVal) ← evalExpr state recvExpr
        -- Apply each message chain to the receiver, discarding results
        let mut currentState := state'
        for chain in chains do
          for (sel, argsExpr) in chain do
            let (newState, argVals) ← evalExprs currentState argsExpr
            currentState := newState
            -- Try method dispatch for objects, else primitive
            match recvVal with
            | .object className _ =>
                match lookupMethod currentState.classes className sel with
                | some (defClass, method) =>
                    match evalMethodCall currentState recvVal defClass method argVals with
                    | .ok (s, _) => currentState := s
                    | .error e => throw e
                | none =>
                    match evalPrimitive recvVal sel argVals with
                    | .ok _ => pure ()
                    | .error e => throw { message := e.message }
            | _ =>
                match evalPrimitive recvVal sel argVals with
                | .ok _ => pure ()
                | .error e => throw { message := e.message }
        .ok (currentState, recvVal)  -- Return the original receiver
end

/-- Core classes always available. -/
def coreClasses : List ClassDef := [
  { name := "Object", super := none, ivars := [], methods := [] },
  { name := "UndefinedObject", super := some "Object", ivars := [], methods := [] },
  -- Built-in type classes (primitives are handled in evalPrimitive, but methods can be added)
  { name := "Integer", super := some "Object", ivars := [], methods := [] },
  { name := "Float", super := some "Object", ivars := [], methods := [] },
  { name := "String", super := some "Object", ivars := [], methods := [] },
  { name := "Character", super := some "Object", ivars := [], methods := [] },
  { name := "Symbol", super := some "Object", ivars := [], methods := [] },
  { name := "Boolean", super := some "Object", ivars := [], methods := [] },
  { name := "True", super := some "Boolean", ivars := [], methods := [] },
  { name := "False", super := some "Boolean", ivars := [], methods := [] },
  { name := "Array", super := some "Object", ivars := [], methods := [] },
  { name := "Dictionary", super := some "Object", ivars := [], methods := [] }
]

/-- Get the class name for a runtime value. -/
def classNameOf : Value → Symbol
  | .int _ => "Integer"
  | .float _ => "Float"
  | .str _ => "String"
  | .char _ => "Character"
  | .symbol _ => "Symbol"
  | .bool true => "True"
  | .bool false => "False"
  | .nil => "UndefinedObject"
  | .array _ => "Array"
  | .dict _ => "Dictionary"
  | .object cn _ => cn

/-- Evaluate a whole program. -/
def evalProgram (program : Program) : Except EvalError Value :=
  let userClasses := program.classes.map (fun c => (c.name, c))
  let coreRegistry := coreClasses.map (fun c => (c.name, c))
  let registry := userClasses ++ coreRegistry
  match evalSeq { env := emptyEnv, classes := registry } program.main with
  | .ok (_, value) => .ok value
  | .error e => .error e

end Smalltalk

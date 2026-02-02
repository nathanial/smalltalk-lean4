import Smalltalk.AST
import Smalltalk.Runtime
import Smalltalk.Primitives

namespace Smalltalk

/-- Check if a string contains a substring -/
def String.containsSubstr (s : String) (sub : String) : Bool :=
  (s.splitOn sub).length > 1

/-- Evaluation errors for the interpreter. -/
structure EvalError where
  message : String
  returnValue : Option Value := none  -- Non-local return value (if set, not a real error)
  exceptionValue : Option Value := none  -- Exception being signaled
  -- Env from cleanup blocks (ensure:/ifCurtailed:) - allows side effects to propagate
  cleanupEnv : Option Env := none
  deriving Repr, Inhabited

/-- Interpreter state carrying the current environment. -/
structure ExecState where
  env : Env := []
  self : Option Value := none
  classes : ClassRegistry := []
  currentClass : Option Symbol := none
  nextObjectId : Nat := 1
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

/-- Metaclass name for a class. -/
def metaName (name : Symbol) : Symbol :=
  name ++ " class"

/-- Check if a name is a metaclass. -/
def isMetaName (name : Symbol) : Bool :=
  name.endsWith " class"

/-- Build a metaclass definition from a class definition. -/
def toMetaclass (cls : ClassDef) : ClassDef :=
  let metaSuper :=
    match cls.super with
    | some superName => some (metaName superName)
    | none => some "Class"
  { name := metaName cls.name
    super := metaSuper
    ivars := []
    methods := cls.classMethods
    classMethods := [] }

/-- Expand classes with metaclasses, skipping already-expanded metaclasses. -/
def expandClasses (classes : List ClassDef) : List ClassDef :=
  classes.foldr
    (fun cls acc =>
      if isMetaName cls.name then
        cls :: acc
      else
        cls :: toMetaclass cls :: acc)
    []

/-- Build the class registry with metaclasses. -/
def buildRegistry (classes : List ClassDef) : ClassRegistry :=
  (expandClasses classes).map (fun c => (c.name, c))

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
  | .object _ cn _ => cn
  | .classObj name => metaName name
  | .block _ _ _ _ _ => "Block"

/-- Check if a class inherits from Exception -/
partial def isExceptionClass (registry : ClassRegistry) (className : Symbol) : Bool :=
  if className == "Exception" then true
  else match registryLookup registry className with
    | none => false
    | some classDef =>
        match classDef.super with
        | none => false
        | some superName => isExceptionClass registry superName

/-- Check if exception matches handler class (includes subclasses) -/
partial def exceptionMatches (registry : ClassRegistry) (exClass : Symbol) (handlerClass : Symbol) : Bool :=
  if exClass == handlerClass then true
  else match registryLookup registry exClass with
    | none => false
    | some classDef =>
        match classDef.super with
        | none => false
        | some superName => exceptionMatches registry superName handlerClass

/-- Collect instance variables from a class and its superclasses. -/
partial def collectIvars (registry : ClassRegistry) (className : Symbol) : List Symbol :=
  match registryLookup registry className with
  | none => []
  | some classDef =>
      let superIvars :=
        match classDef.super with
        | some superName => collectIvars registry superName
        | none => []
      superIvars ++ classDef.ivars

/-- Allocate a new object instance. -/
def allocObject (state : ExecState) (className : Symbol)
    : Except EvalError (ExecState × Value) := do
  match registryLookup state.classes className with
  | some _ =>
      let fields := collectIvars state.classes className |>.map (fun iv => (iv, Value.nil))
      let obj := Value.object state.nextObjectId className fields
      let nextState := { state with nextObjectId := state.nextObjectId + 1 }
      .ok (nextState, obj)
  | none => .error { message := s!"Unknown class: {className}" }

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
      | .error e =>
          -- Catch non-local returns at method boundary
          match e.returnValue with
          | some v =>
              -- Non-local return - return the value from the method
              .ok ({ state with classes := methodState.classes }, v)
          | none =>
              -- Real error - propagate it
              .error e

  /-- Evaluate a message send, optionally updating a receiver variable after method call. -/
  partial def evalSend (state : ExecState) (recvVal : Value) (sel : Symbol) (argVals : List Value)
      (recvVarName : Option Symbol) : Except EvalError (ExecState × Value) := do
    evalSendToValue state recvVal sel argVals recvVarName

  /-- Dispatch message to a value (object or primitive). -/
  partial def evalSendToValue (state : ExecState) (recvVal : Value) (sel : Symbol) (argVals : List Value)
      (recvVarName : Option Symbol) : Except EvalError (ExecState × Value) := do
    -- Handle identity messages for all values
    if sel == "==" || sel == "~~" then
      match argVals with
      | [other] =>
          let identical := valueIdentical recvVal other
          if sel == "==" then .ok (state, .bool identical)
          else .ok (state, .bool (!identical))
      | _ => .error { message := s!"{sel} expects exactly 1 argument" }
    else
    -- Handle class objects first
    match recvVal with
    | .classObj className =>
        match sel with
        | "new" =>
            if argVals.isEmpty then
              allocObject state className
            else
              .error { message := "new takes no arguments" }
        | "basicNew" =>
            if argVals.isEmpty then
              allocObject state className
            else
              .error { message := "basicNew takes no arguments" }
        | "name" =>
            if argVals.isEmpty then .ok (state, .symbol className)
            else .error { message := "name takes no arguments" }
        | "superclass" =>
            if argVals.isEmpty then
              match registryLookup state.classes className with
              | some classDef =>
                  match classDef.super with
                  | some superName => .ok (state, .classObj superName)
                  | none => .ok (state, .nil)
              | none => .error { message := s!"Unknown class: {className}" }
            else
              .error { message := "superclass takes no arguments" }
        | "class" =>
            if argVals.isEmpty then .ok (state, .classObj (classNameOf recvVal))
            else .error { message := "class takes no arguments" }
        | "signal:" =>
            match argVals with
            | [.str msg] =>
                if isExceptionClass state.classes className then
                  let (_, exVal) ← allocObject state className
                  let exVal := match exVal with
                    | .object id cn fields =>
                        let newFields :=
                          fields.map (fun (n, v) => if n == "messageText" then (n, .str msg) else (n, v))
                        .object id cn newFields
                    | other => other
                  .error { message := "", exceptionValue := some exVal }
                else
                  .error { message := s!"{className} is not an exception class" }
            | _ => .error { message := "signal: expects a string message" }
        | _ =>
            evalSendToValueFallback state recvVal sel argVals recvVarName
    -- Handle block-specific messages
    | .block _ _ _ _ _ =>
        match sel with
        | "class" =>
            if argVals.isEmpty then .ok (state, .classObj (classNameOf recvVal))
            else .error { message := "class takes no arguments" }
        | "value" =>
            if argVals.isEmpty then evalBlockValue state recvVal []
            else .error { message := "value takes no arguments" }
        | "value:" =>
            match argVals with
            | [arg] => evalBlockValue state recvVal [arg]
            | _ => .error { message := "value: expects exactly 1 argument" }
        | "value:value:" =>
            match argVals with
            | [a1, a2] => evalBlockValue state recvVal [a1, a2]
            | _ => .error { message := "value:value: expects exactly 2 arguments" }
        | "value:value:value:" =>
            match argVals with
            | [a1, a2, a3] => evalBlockValue state recvVal [a1, a2, a3]
            | _ => .error { message := "value:value:value: expects exactly 3 arguments" }
        | "whileTrue:" =>
            match argVals with
            | [bodyBlock] => evalWhileTrue state recvVal bodyBlock
            | _ => .error { message := "whileTrue: expects exactly 1 argument" }
        | "whileFalse:" =>
            match argVals with
            | [bodyBlock] => evalWhileFalse state recvVal bodyBlock
            | _ => .error { message := "whileFalse: expects exactly 1 argument" }
        | "whileTrue" =>
            -- [condition] whileTrue evaluates until condition is false
            evalWhileTrue state recvVal recvVal
        | "whileFalse" =>
            -- [condition] whileFalse evaluates until condition is true
            evalWhileFalse state recvVal recvVal
        | "on:do:" =>
            match argVals with
            | [.symbol exClass, handlerBlock] => evalOnDo state recvVal exClass handlerBlock
            | _ => .error { message := "on:do: expects exception class symbol and handler block" }
        | "ensure:" =>
            match argVals with
            | [ensureBlock] => evalEnsure state recvVal ensureBlock
            | _ => .error { message := "ensure: expects exactly 1 block" }
        | "ifCurtailed:" =>
            match argVals with
            | [curtailedBlock] => evalIfCurtailed state recvVal curtailedBlock
            | _ => .error { message := "ifCurtailed: expects exactly 1 block" }
        | _ => .error { message := s!"Block does not understand: {sel}" }
    | _ =>
    -- Handle class message for any receiver
    if sel == "class" && argVals.isEmpty then
      .ok (state, .classObj (classNameOf recvVal))
    else
    -- Handle boolean control flow messages
    match recvVal, sel with
    | .bool b, "ifTrue:" =>
        match argVals with
        | [trueBlock] =>
            if b then evalBlockValue state trueBlock []
            else .ok (state, .nil)
        | _ => .error { message := "ifTrue: expects exactly 1 argument" }
    | .bool b, "ifFalse:" =>
        match argVals with
        | [falseBlock] =>
            if b then .ok (state, .nil)
            else evalBlockValue state falseBlock []
        | _ => .error { message := "ifFalse: expects exactly 1 argument" }
    | .bool b, "ifTrue:ifFalse:" =>
        match argVals with
        | [trueBlock, falseBlock] =>
            if b then evalBlockValue state trueBlock []
            else evalBlockValue state falseBlock []
        | _ => .error { message := "ifTrue:ifFalse: expects exactly 2 arguments" }
    | .bool b, "ifFalse:ifTrue:" =>
        match argVals with
        | [falseBlock, trueBlock] =>
            if b then evalBlockValue state trueBlock []
            else evalBlockValue state falseBlock []
        | _ => .error { message := "ifFalse:ifTrue: expects exactly 2 arguments" }
    -- Handle integer timesRepeat:
    | .int n, "timesRepeat:" =>
        match argVals with
        | [blockVal] =>
            if n <= 0 then .ok (state, .nil)
            else evalTimesRepeat state n.toNat blockVal
        | _ => .error { message := "timesRepeat: expects exactly 1 argument" }
    | .int n, "to:do:" =>
        -- n to: m do: [:i | body] - evaluates block with i from n to m
        match argVals with
        | [.int m, blockVal] =>
            evalToDo state n m blockVal
        | _ => .error { message := "to:do: expects an integer and a block" }
    -- Array iteration methods
    | .array elems, "do:" =>
        match argVals with
        | [blockVal] => evalArrayDo state elems blockVal
        | _ => .error { message := "do: expects exactly 1 argument" }
    | .array elems, "collect:" =>
        match argVals with
        | [blockVal] => evalArrayCollect state elems blockVal
        | _ => .error { message := "collect: expects exactly 1 argument" }
    | .array elems, "select:" =>
        match argVals with
        | [blockVal] => evalArraySelect state elems blockVal
        | _ => .error { message := "select: expects exactly 1 argument" }
    | .array elems, "reject:" =>
        match argVals with
        | [blockVal] => evalArrayReject state elems blockVal
        | _ => .error { message := "reject: expects exactly 1 argument" }
    | .array elems, "detect:" =>
        match argVals with
        | [blockVal] => evalArrayDetect state elems blockVal
        | _ => .error { message := "detect: expects exactly 1 argument" }
    | .array elems, "detect:ifNone:" =>
        match argVals with
        | [blockVal, noneBlock] => evalArrayDetectIfNone state elems blockVal noneBlock
        | _ => .error { message := "detect:ifNone: expects exactly 2 arguments" }
    | .array elems, "inject:into:" =>
        match argVals with
        | [initial, blockVal] => evalArrayInject state elems initial blockVal
        | _ => .error { message := "inject:into: expects exactly 2 arguments" }
    | .array elems, "includes:" =>
        match argVals with
        | [val] => .ok (state, .bool (elems.any fun e => valueIdentical e val))
        | _ => .error { message := "includes: expects exactly 1 argument" }
    | .array elems, "indexOf:" =>
        match argVals with
        | [val] =>
            match elems.findIdx? (fun e => valueIdentical e val) with
            | some idx => .ok (state, .int (idx + 1))  -- 1-indexed
            | none => .ok (state, .int 0)  -- 0 means not found
        | _ => .error { message := "indexOf: expects exactly 1 argument" }
    -- String iteration methods
    | .str s, "do:" =>
        match argVals with
        | [blockVal] => evalStringDo state s blockVal
        | _ => .error { message := "do: expects exactly 1 argument" }
    | .str s, "collect:" =>
        match argVals with
        | [blockVal] => evalStringCollect state s blockVal
        | _ => .error { message := "collect: expects exactly 1 argument" }
    | .str s, "select:" =>
        match argVals with
        | [blockVal] => evalStringSelect state s blockVal
        | _ => .error { message := "select: expects exactly 1 argument" }
    | .str s, "reject:" =>
        match argVals with
        | [blockVal] => evalStringReject state s blockVal
        | _ => .error { message := "reject: expects exactly 1 argument" }
    | .str s, "detect:" =>
        match argVals with
        | [blockVal] => evalStringDetect state s blockVal
        | _ => .error { message := "detect: expects exactly 1 argument" }
    | .str s, "detect:ifNone:" =>
        match argVals with
        | [blockVal, noneBlock] => evalStringDetectIfNone state s blockVal noneBlock
        | _ => .error { message := "detect:ifNone: expects exactly 2 arguments" }
    | .str s, "inject:into:" =>
        match argVals with
        | [initial, blockVal] => evalStringInject state s initial blockVal
        | _ => .error { message := "inject:into: expects exactly 2 arguments" }
    | .str s, "includes:" =>
        match argVals with
        | [.char c] => .ok (state, .bool (s.any (· == c)))
        | [.str sub] => .ok (state, .bool (String.containsSubstr s sub))
        | _ => .error { message := "includes: expects a Character or String" }
    -- Dictionary iteration methods
    | .dict entries, "do:" =>
        match argVals with
        | [blockVal] => evalDictDo state entries blockVal
        | _ => .error { message := "do: expects exactly 1 argument" }
    | .dict entries, "keysAndValuesDo:" =>
        match argVals with
        | [blockVal] => evalDictKeysAndValuesDo state entries blockVal
        | _ => .error { message := "keysAndValuesDo: expects exactly 1 argument" }
    | .dict entries, "collect:" =>
        match argVals with
        | [blockVal] => evalDictCollect state entries blockVal
        | _ => .error { message := "collect: expects exactly 1 argument" }
    | .dict entries, "select:" =>
        match argVals with
        | [blockVal] => evalDictSelect state entries blockVal
        | _ => .error { message := "select: expects exactly 1 argument" }
    | .dict entries, "reject:" =>
        match argVals with
        | [blockVal] => evalDictReject state entries blockVal
        | _ => .error { message := "reject: expects exactly 1 argument" }
    | .dict entries, "detect:" =>
        match argVals with
        | [blockVal] => evalDictDetect state entries blockVal
        | _ => .error { message := "detect: expects exactly 1 argument" }
    | .dict entries, "detect:ifNone:" =>
        match argVals with
        | [blockVal, noneBlock] => evalDictDetectIfNone state entries blockVal noneBlock
        | _ => .error { message := "detect:ifNone: expects exactly 2 arguments" }
    | .dict entries, "inject:into:" =>
        match argVals with
        | [initial, blockVal] => evalDictInject state entries initial blockVal
        | _ => .error { message := "inject:into: expects exactly 2 arguments" }
    -- Exception signaling: anException signal
    | .object _ className _, "signal" =>
        if isExceptionClass state.classes className then
          .error { message := "", exceptionValue := some recvVal }
        else
          -- Fall through to method lookup (handled below)
          evalSendToValueFallback state recvVal sel argVals recvVarName
    -- Exception messageText accessor
    | .object _ className fields, "messageText" =>
        if isExceptionClass state.classes className then
          match fields.find? (fun (n, _) => n == "messageText") with
          | some (_, v) => .ok (state, v)
          | none => .ok (state, .nil)
        else
          evalSendToValueFallback state recvVal sel argVals recvVarName
    | _, _ =>
        evalSendToValueFallback state recvVal sel argVals recvVarName

  /-- Fallback method dispatch for non-exception-specific messages. -/
  partial def evalSendToValueFallback (state : ExecState) (recvVal : Value) (sel : Symbol)
      (argVals : List Value) (recvVarName : Option Symbol)
      : Except EvalError (ExecState × Value) := do
    -- Get the class name for this value (works for both objects and built-in types)
    let className := classNameOf recvVal
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

  /-- Merge environment changes from block execution back to outer scope.
      Variables that were modified inside the block (and exist in outer env) get updated. -/
  partial def mergeBlockEnvChanges (outerEnv : Env) (blockEnv : Env) (params : List Symbol) (temps : List Symbol) : Env :=
    -- For each binding in outerEnv, check if it was modified in blockEnv
    outerEnv.map fun (name, oldVal) =>
      -- Skip params and temps - they're block-local
      if params.contains name || temps.contains name then
        (name, oldVal)
      else
        -- Look for updated value in blockEnv
        match envLookup blockEnv name with
        | some newVal => (name, newVal)
        | none => (name, oldVal)

  /-- Evaluate a block with the given arguments. -/
  partial def evalBlockValue (state : ExecState) (blockVal : Value) (args : List Value)
      : Except EvalError (ExecState × Value) := do
    match blockVal with
    | .block params temps body capturedEnv capturedSelf =>
        if params.length != args.length then
          .error { message := s!"Block expects {params.length} arguments, got {args.length}" }
        else
          let paramBindings := params.zip args
          let tempBindings := temps.map (fun t => (t, Value.nil))
          -- Block env = params + temps + CURRENT outer env (not just captured)
          -- This allows blocks to see updates made by previous block evaluations
          let blockEnv := paramBindings ++ tempBindings ++ state.env
          let blockState := { state with env := blockEnv, self := capturedSelf.orElse (fun _ => state.self) }
          -- Evaluate block body, propagating non-local returns
          match evalSeq blockState body with
          | .ok (finalState, result) =>
              -- Merge env changes back to caller (excluding params/temps which are block-local)
              let mergedEnv := mergeBlockEnvChanges state.env finalState.env params temps
              .ok ({ state with env := mergedEnv, classes := finalState.classes }, result)
          | .error e =>
              -- Propagate returns up (they'll be caught by evalMethodCall)
              .error e
    | _ => .error { message := "value sent to non-block" }

  /-- Evaluate whileTrue: loop - receiver block is condition, arg is body. -/
  partial def evalWhileTrue (state : ExecState) (condBlock : Value) (bodyBlock : Value)
      : Except EvalError (ExecState × Value) := do
    match ← evalBlockValue state condBlock [] with
    | (state', .bool true) =>
        match ← evalBlockValue state' bodyBlock [] with
        | (state'', _) => evalWhileTrue state'' condBlock bodyBlock
    | (state', .bool false) => .ok (state', .nil)
    | (_, other) => .error { message := s!"whileTrue: condition must return Boolean, got {reprStr other}" }

  /-- Evaluate whileFalse: loop - receiver block is condition, arg is body. -/
  partial def evalWhileFalse (state : ExecState) (condBlock : Value) (bodyBlock : Value)
      : Except EvalError (ExecState × Value) := do
    match ← evalBlockValue state condBlock [] with
    | (state', .bool false) =>
        match ← evalBlockValue state' bodyBlock [] with
        | (state'', _) => evalWhileFalse state'' condBlock bodyBlock
    | (state', .bool true) => .ok (state', .nil)
    | (_, other) => .error { message := s!"whileFalse: condition must return Boolean, got {reprStr other}" }

  /-- Evaluate timesRepeat: - evaluates block n times. -/
  partial def evalTimesRepeat (state : ExecState) (n : Nat) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    if n == 0 then
      .ok (state, .nil)
    else
      let (state', _) ← evalBlockValue state blockVal []
      evalTimesRepeat state' (n - 1) blockVal

  /-- Evaluate to:do: - evaluates block with index from start to end. -/
  partial def evalToDo (state : ExecState) (start : Int) (stop : Int) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    if start > stop then
      .ok (state, .nil)
    else
      let (state', _) ← evalBlockValue state blockVal [.int start]
      evalToDo state' (start + 1) stop blockVal

  /-- Evaluate [protected] on: ExceptionClass do: [:ex | handler] -/
  partial def evalOnDo (state : ExecState) (protectedBlock : Value)
      (exClass : Symbol) (handlerBlock : Value) : Except EvalError (ExecState × Value) := do
    match evalBlockValue state protectedBlock [] with
    | .ok result => .ok result  -- No exception, return normally
    | .error e =>
        match e.exceptionValue with
        | some exVal =>
            -- Check if exception matches handler class
            let exClassName := match exVal with
              | .object _ cn _ => cn
              | _ => ""
            if exceptionMatches state.classes exClassName exClass then
              -- Handler catches this exception
              -- Use cleanupEnv if present (from ensure:/ifCurtailed: blocks)
              let handlerState := match e.cleanupEnv with
                | some env => { state with env := env }
                | none => state
              evalBlockValue handlerState handlerBlock [exVal]
            else
              -- Exception doesn't match, propagate
              .error e
        | none =>
            -- Not an exception (could be non-local return or real error)
            .error e

  /-- Evaluate [protected] ensure: [cleanup] -/
  partial def evalEnsure (state : ExecState) (protectedBlock : Value)
      (ensureBlock : Value) : Except EvalError (ExecState × Value) := do
    -- Always run cleanup, regardless of outcome
    match evalBlockValue state protectedBlock [] with
    | .ok (state', result) =>
        -- Success: run cleanup, return original result
        match evalBlockValue state' ensureBlock [] with
        | .ok (state'', _) => .ok (state'', result)
        | .error cleanupError => .error cleanupError  -- Cleanup error takes precedence
    | .error e =>
        -- Exception or return: run cleanup, then re-raise
        -- Use cleanupEnv from e if present (for nested ensure:/ifCurtailed:)
        let cleanupState := match e.cleanupEnv with
          | some env => { state with env := env }
          | none => state
        match evalBlockValue cleanupState ensureBlock [] with
        | .ok (state', _) =>
            -- Re-raise original error, but preserve cleanup env
            .error { e with cleanupEnv := some state'.env }
        | .error cleanupError =>
            -- Cleanup error takes precedence
            .error cleanupError

  /-- Evaluate [protected] ifCurtailed: [cleanup] -/
  partial def evalIfCurtailed (state : ExecState) (protectedBlock : Value)
      (curtailedBlock : Value) : Except EvalError (ExecState × Value) := do
    match evalBlockValue state protectedBlock [] with
    | .ok result => .ok result  -- Success: don't run cleanup
    | .error e =>
        -- Exception or non-local return: run cleanup, then re-raise
        -- Use cleanupEnv from e if present (for nested ensure:/ifCurtailed:)
        let cleanupState := match e.cleanupEnv with
          | some env => { state with env := env }
          | none => state
        match evalBlockValue cleanupState curtailedBlock [] with
        | .ok (state', _) =>
            -- Re-raise original error, but preserve cleanup env
            .error { e with cleanupEnv := some state'.env }
        | .error cleanupError => .error cleanupError

  /-- Array do: - evaluate block for each element, return nil -/
  partial def evalArrayDo (state : ExecState) (elems : List Value) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let mut currentState := state
    for elem in elems do
      let (newState, _) ← evalBlockValue currentState blockVal [elem]
      currentState := newState
    .ok (currentState, .nil)

  /-- Array collect: - transform each element, return new array -/
  partial def evalArrayCollect (state : ExecState) (elems : List Value) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let mut currentState := state
    let mut results : List Value := []
    for elem in elems do
      let (newState, result) ← evalBlockValue currentState blockVal [elem]
      currentState := newState
      results := results ++ [result]
    .ok (currentState, .array results)

  /-- Array select: - filter elements where block returns true -/
  partial def evalArraySelect (state : ExecState) (elems : List Value) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let mut currentState := state
    let mut results : List Value := []
    for elem in elems do
      let (newState, result) ← evalBlockValue currentState blockVal [elem]
      currentState := newState
      match result with
      | .bool true => results := results ++ [elem]
      | .bool false => pure ()
      | _ => throw { message := "select: block must return Boolean" }
    .ok (currentState, .array results)

  /-- Array reject: - filter elements where block returns false -/
  partial def evalArrayReject (state : ExecState) (elems : List Value) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let mut currentState := state
    let mut results : List Value := []
    for elem in elems do
      let (newState, result) ← evalBlockValue currentState blockVal [elem]
      currentState := newState
      match result with
      | .bool false => results := results ++ [elem]
      | .bool true => pure ()
      | _ => throw { message := "reject: block must return Boolean" }
    .ok (currentState, .array results)

  /-- Array detect: - find first element where block returns true -/
  partial def evalArrayDetect (state : ExecState) (elems : List Value) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let mut currentState := state
    for elem in elems do
      let (newState, result) ← evalBlockValue currentState blockVal [elem]
      currentState := newState
      match result with
      | .bool true => return (currentState, elem)
      | .bool false => pure ()
      | _ => throw { message := "detect: block must return Boolean" }
    .error { message := "detect: no element found" }

  /-- Array detect:ifNone: - find first or evaluate none block -/
  partial def evalArrayDetectIfNone (state : ExecState) (elems : List Value)
      (blockVal : Value) (noneBlock : Value) : Except EvalError (ExecState × Value) := do
    let mut currentState := state
    for elem in elems do
      let (newState, result) ← evalBlockValue currentState blockVal [elem]
      currentState := newState
      match result with
      | .bool true => return (currentState, elem)
      | .bool false => pure ()
      | _ => throw { message := "detect:ifNone: block must return Boolean" }
    evalBlockValue currentState noneBlock []

  /-- Array inject:into: - fold/reduce with accumulator -/
  partial def evalArrayInject (state : ExecState) (elems : List Value)
      (initial : Value) (blockVal : Value) : Except EvalError (ExecState × Value) := do
    let mut currentState := state
    let mut acc := initial
    for elem in elems do
      let (newState, result) ← evalBlockValue currentState blockVal [acc, elem]
      currentState := newState
      acc := result
    .ok (currentState, acc)

  /-- String do: - evaluate block for each character -/
  partial def evalStringDo (state : ExecState) (s : String) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let chars := s.toList
    let mut currentState := state
    for c in chars do
      let (newState, _) ← evalBlockValue currentState blockVal [.char c]
      currentState := newState
    .ok (currentState, .nil)

  /-- String collect: - transform each character -/
  partial def evalStringCollect (state : ExecState) (s : String) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let chars := s.toList
    let mut currentState := state
    let mut results : List Value := []
    for c in chars do
      let (newState, result) ← evalBlockValue currentState blockVal [.char c]
      currentState := newState
      results := results ++ [result]
    -- Try to produce a string if all results are characters
    let allChars := results.all fun v => match v with | .char _ => true | _ => false
    if allChars then
      let str := String.ofList (results.filterMap fun v => match v with | .char c => some c | _ => none)
      .ok (currentState, .str str)
    else
      .ok (currentState, .array results)

  /-- String select: - filter characters where block returns true -/
  partial def evalStringSelect (state : ExecState) (s : String) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let chars := s.toList
    let mut currentState := state
    let mut results : List Char := []
    for c in chars do
      let (newState, result) ← evalBlockValue currentState blockVal [.char c]
      currentState := newState
      match result with
      | .bool true => results := results ++ [c]
      | .bool false => pure ()
      | _ => throw { message := "select: block must return Boolean" }
    .ok (currentState, .str (String.ofList results))

  /-- String reject: - filter characters where block returns false -/
  partial def evalStringReject (state : ExecState) (s : String) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let chars := s.toList
    let mut currentState := state
    let mut results : List Char := []
    for c in chars do
      let (newState, result) ← evalBlockValue currentState blockVal [.char c]
      currentState := newState
      match result with
      | .bool false => results := results ++ [c]
      | .bool true => pure ()
      | _ => throw { message := "reject: block must return Boolean" }
    .ok (currentState, .str (String.ofList results))

  /-- String detect: - find first character where block returns true -/
  partial def evalStringDetect (state : ExecState) (s : String) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let chars := s.toList
    let mut currentState := state
    for c in chars do
      let (newState, result) ← evalBlockValue currentState blockVal [.char c]
      currentState := newState
      match result with
      | .bool true => return (currentState, .char c)
      | .bool false => pure ()
      | _ => throw { message := "detect: block must return Boolean" }
    .error { message := "detect: no element found" }

  /-- String detect:ifNone: - find first or evaluate none block -/
  partial def evalStringDetectIfNone (state : ExecState) (s : String)
      (blockVal : Value) (noneBlock : Value) : Except EvalError (ExecState × Value) := do
    let chars := s.toList
    let mut currentState := state
    for c in chars do
      let (newState, result) ← evalBlockValue currentState blockVal [.char c]
      currentState := newState
      match result with
      | .bool true => return (currentState, .char c)
      | .bool false => pure ()
      | _ => throw { message := "detect:ifNone: block must return Boolean" }
    evalBlockValue currentState noneBlock []

  /-- String inject:into: - fold/reduce with accumulator -/
  partial def evalStringInject (state : ExecState) (s : String)
      (initial : Value) (blockVal : Value) : Except EvalError (ExecState × Value) := do
    let chars := s.toList
    let mut currentState := state
    let mut acc := initial
    for c in chars do
      let (newState, result) ← evalBlockValue currentState blockVal [acc, .char c]
      currentState := newState
      acc := result
    .ok (currentState, acc)

  /-- Dictionary do: - evaluate block for each value -/
  partial def evalDictDo (state : ExecState) (entries : List (Value × Value)) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let mut currentState := state
    for (_, v) in entries do
      let (newState, _) ← evalBlockValue currentState blockVal [v]
      currentState := newState
    .ok (currentState, .nil)

  /-- Dictionary keysAndValuesDo: - evaluate block for each key-value pair -/
  partial def evalDictKeysAndValuesDo (state : ExecState) (entries : List (Value × Value)) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let mut currentState := state
    for (k, v) in entries do
      let (newState, _) ← evalBlockValue currentState blockVal [k, v]
      currentState := newState
    .ok (currentState, .nil)

  /-- Dictionary collect: - transform each value -/
  partial def evalDictCollect (state : ExecState) (entries : List (Value × Value)) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let mut currentState := state
    let mut results : List Value := []
    for (_, v) in entries do
      let (newState, result) ← evalBlockValue currentState blockVal [v]
      currentState := newState
      results := results ++ [result]
    .ok (currentState, .array results)

  /-- Dictionary select: - filter entries where block returns true (block receives value) -/
  partial def evalDictSelect (state : ExecState) (entries : List (Value × Value)) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let mut currentState := state
    let mut results : List (Value × Value) := []
    for (k, v) in entries do
      let (newState, result) ← evalBlockValue currentState blockVal [v]
      currentState := newState
      match result with
      | .bool true => results := results ++ [(k, v)]
      | .bool false => pure ()
      | _ => throw { message := "select: block must return Boolean" }
    .ok (currentState, .dict results)

  /-- Dictionary reject: - filter entries where block returns false (block receives value) -/
  partial def evalDictReject (state : ExecState) (entries : List (Value × Value)) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let mut currentState := state
    let mut results : List (Value × Value) := []
    for (k, v) in entries do
      let (newState, result) ← evalBlockValue currentState blockVal [v]
      currentState := newState
      match result with
      | .bool false => results := results ++ [(k, v)]
      | .bool true => pure ()
      | _ => throw { message := "reject: block must return Boolean" }
    .ok (currentState, .dict results)

  /-- Dictionary detect: - find first value where block returns true -/
  partial def evalDictDetect (state : ExecState) (entries : List (Value × Value)) (blockVal : Value)
      : Except EvalError (ExecState × Value) := do
    let mut currentState := state
    for (_, v) in entries do
      let (newState, result) ← evalBlockValue currentState blockVal [v]
      currentState := newState
      match result with
      | .bool true => return (currentState, v)
      | .bool false => pure ()
      | _ => throw { message := "detect: block must return Boolean" }
    .error { message := "detect: no element found" }

  /-- Dictionary detect:ifNone: - find first value or evaluate none block -/
  partial def evalDictDetectIfNone (state : ExecState) (entries : List (Value × Value))
      (blockVal : Value) (noneBlock : Value) : Except EvalError (ExecState × Value) := do
    let mut currentState := state
    for (_, v) in entries do
      let (newState, result) ← evalBlockValue currentState blockVal [v]
      currentState := newState
      match result with
      | .bool true => return (currentState, v)
      | .bool false => pure ()
      | _ => throw { message := "detect:ifNone: block must return Boolean" }
    evalBlockValue currentState noneBlock []

  /-- Dictionary inject:into: - fold/reduce with accumulator over values -/
  partial def evalDictInject (state : ExecState) (entries : List (Value × Value))
      (initial : Value) (blockVal : Value) : Except EvalError (ExecState × Value) := do
    let mut currentState := state
    let mut acc := initial
    for (_, v) in entries do
      let (newState, result) ← evalBlockValue currentState blockVal [acc, v]
      currentState := newState
      acc := result
    .ok (currentState, acc)

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
            | some (.object _ _ fields) =>
                match fields.find? (fun (n, _) => n == name) with
                | some (_, v) => .ok (state, v)
                | none =>
                    -- 3. Check if it's a class name
                    if registryLookup state.classes name |>.isSome then
                      .ok (state, .classObj name)
                    else
                      .error { message := s!"Undefined variable: {name}" }
            | _ =>
                -- 3. Check if it's a class name
                if registryLookup state.classes name |>.isSome then
                  .ok (state, .classObj name)
                else
                  .error { message := s!"Undefined variable: {name}" }
    | .assign name valueExpr => do
        let (state', value) ← evalExpr state valueExpr
        -- Check if assigning to an instance variable
        match state'.self with
        | some (.object id cn fields) =>
            if fields.any (fun (n, _) => n == name) then
              -- Update instance variable
              let newFields := fields.map (fun (n, v) => if n == name then (n, value) else (n, v))
              let newSelf := Value.object id cn newFields
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
    | .block params temps body =>
        -- Capture current environment and self at block creation time
        .ok (state, .block params temps body state.env state.self)
    | .return valueExpr => do
        -- Evaluate the return expression and signal non-local return
        let (_, value) ← evalExpr state valueExpr
        .error { message := "__return__", returnValue := some value }
    | .cascade recvExpr chains => do
        -- Evaluate receiver once
        let (state', recvVal) ← evalExpr state recvExpr
        -- Apply each message chain to the receiver, discarding results
        let mut currentState := state'
        for chain in chains do
          for (sel, argsExpr) in chain do
            let (newState, argVals) ← evalExprs currentState argsExpr
            currentState := newState
            let (s, _) ← evalSendToValue currentState recvVal sel argVals none
            currentState := s
        .ok (currentState, recvVal)  -- Return the original receiver
end

/-- Core classes always available. -/
def coreClasses : List ClassDef := [
  { name := "Object", super := none, ivars := [], methods := [] },
  { name := "Class", super := some "Object", ivars := [], methods := [] },
  { name := "Metaclass", super := some "Class", ivars := [], methods := [] },
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
  { name := "Dictionary", super := some "Object", ivars := [], methods := [] },
  { name := "Block", super := some "Object", ivars := [], methods := [] },
  -- Exception hierarchy
  { name := "Exception", super := some "Object", ivars := ["messageText"], methods := [] },
  { name := "Error", super := some "Exception", ivars := [], methods := [] },
  { name := "Warning", super := some "Exception", ivars := [], methods := [] }
]

/-- Evaluate a whole program. -/
def evalProgram (program : Program) : Except EvalError Value :=
  let registry := buildRegistry (program.classes ++ coreClasses)
  match evalSeq { env := emptyEnv, classes := registry } program.main with
  | .ok (_, value) => .ok value
  | .error e => .error e

end Smalltalk

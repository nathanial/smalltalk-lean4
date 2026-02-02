/-
  Smalltalk primitive operations for built-in types.
-/
import Smalltalk.Runtime

namespace Smalltalk

/-- Error type for primitive evaluation. -/
structure PrimitiveError where
  message : String
  deriving Repr, BEq, Inhabited

/-- Get a human-readable type name for a value. -/
def typeNameOf : Value → String
  | .int _ => "Integer"
  | .float _ => "Float"
  | .str _ => "String"
  | .char _ => "Character"
  | .symbol _ => "Symbol"
  | .bool _ => "Boolean"
  | .nil => "UndefinedObject"
  | .array _ => "Array"
  | .dict _ => "Dictionary"
  | .object _ className _ => className
  | .classObj _ => "Class"
  | .block _ _ _ _ _ => "Block"

/-- Convert Float to Int by truncating towards zero. -/
def floatToInt (f : Float) : Int :=
  if f >= 0.0 then
    Int.ofNat f.toUInt64.toNat
  else
    -Int.ofNat (-f).toUInt64.toNat

/-- Check if two values are identical (same type and value for primitives). -/
partial def valueIdentical : Value → Value → Bool
  | .int a, .int b => a == b
  | .float a, .float b => a == b
  | .str a, .str b => a == b
  | .char a, .char b => a == b
  | .symbol a, .symbol b => a == b
  | .bool a, .bool b => a == b
  | .nil, .nil => true
  | .array a, .array b => a.length == b.length && (a.zip b).all fun (x, y) => valueIdentical x y
  | .dict a, .dict b => a.length == b.length
  | .object id1 _ _, .object id2 _ _ => id1 == id2
  | .classObj n1, .classObj n2 => n1 == n2
  | .block _ _ _ _ _, .block _ _ _ _ _ => false  -- blocks are never identical (would need object IDs)
  | _, _ => false

/-- Evaluate integer primitives. -/
def evalIntPrimitive (n : Int) (sel : Symbol) (args : List Value)
    : Except PrimitiveError Value :=
  match sel, args with
  -- Arithmetic (int-int)
  | "+", [.int m] => .ok (.int (n + m))
  | "-", [.int m] => .ok (.int (n - m))
  | "*", [.int m] => .ok (.int (n * m))
  | "/", [.int m] =>
      if m == 0 then .error { message := "Division by zero" }
      else .ok (.int (n / m))
  | "//", [.int m] =>
      if m == 0 then .error { message := "Division by zero" }
      else .ok (.int (n / m))
  | "\\\\", [.int m] =>
      if m == 0 then .error { message := "Division by zero" }
      else .ok (.int (n % m))
  -- Arithmetic (int-float promotion)
  | "+", [.float f] => .ok (.float (Float.ofInt n + f))
  | "-", [.float f] => .ok (.float (Float.ofInt n - f))
  | "*", [.float f] => .ok (.float (Float.ofInt n * f))
  | "/", [.float f] =>
      if f == 0.0 then .error { message := "Division by zero" }
      else .ok (.float (Float.ofInt n / f))
  -- Unary
  | "-", [] => .ok (.int (-n))
  | "negated", [] => .ok (.int (-n))
  | "abs", [] => .ok (.int n.natAbs)
  -- Comparison (int-int)
  | "<", [.int m] => .ok (.bool (n < m))
  | ">", [.int m] => .ok (.bool (n > m))
  | "<=", [.int m] => .ok (.bool (n ≤ m))
  | ">=", [.int m] => .ok (.bool (n ≥ m))
  | "=", [.int m] => .ok (.bool (n == m))
  | "~=", [.int m] => .ok (.bool (n != m))
  -- Comparison (int-float promotion)
  | "<", [.float f] => .ok (.bool (Float.ofInt n < f))
  | ">", [.float f] => .ok (.bool (Float.ofInt n > f))
  | "<=", [.float f] => .ok (.bool (Float.ofInt n ≤ f))
  | ">=", [.float f] => .ok (.bool (Float.ofInt n ≥ f))
  | "=", [.float f] => .ok (.bool (Float.ofInt n == f))
  | "~=", [.float f] => .ok (.bool (Float.ofInt n != f))
  -- Identity
  | "==", [other] => .ok (.bool (valueIdentical (.int n) other))
  | "~~", [other] => .ok (.bool (!valueIdentical (.int n) other))
  -- Type errors
  | "+", [_] => .error { message := s!"+ expected Integer or Float, got {typeNameOf args.head!}" }
  | "-", [_] => .error { message := s!"- expected Integer or Float, got {typeNameOf args.head!}" }
  | "*", [_] => .error { message := s!"* expected Integer or Float, got {typeNameOf args.head!}" }
  | "/", [_] => .error { message := s!"/ expected Integer or Float, got {typeNameOf args.head!}" }
  | "//", [_] => .error { message := s!"// expected Integer, got {typeNameOf args.head!}" }
  | "\\\\", [_] => .error { message := s!"\\\\ expected Integer, got {typeNameOf args.head!}" }
  | "<", [_] => .error { message := s!"< expected Integer or Float, got {typeNameOf args.head!}" }
  | ">", [_] => .error { message := s!"> expected Integer or Float, got {typeNameOf args.head!}" }
  | "<=", [_] => .error { message := s!"<= expected Integer or Float, got {typeNameOf args.head!}" }
  | ">=", [_] => .error { message := s!">= expected Integer or Float, got {typeNameOf args.head!}" }
  | "=", [_] => .ok (.bool false)  -- Different types are never equal
  | "~=", [_] => .ok (.bool true)  -- Different types are always not equal
  | _, _ => .error { message := s!"No primitive '{sel}' for Integer" }

/-- Evaluate float primitives. -/
def evalFloatPrimitive (f : Float) (sel : Symbol) (args : List Value)
    : Except PrimitiveError Value :=
  match sel, args with
  -- Arithmetic (float-float)
  | "+", [.float g] => .ok (.float (f + g))
  | "-", [.float g] => .ok (.float (f - g))
  | "*", [.float g] => .ok (.float (f * g))
  | "/", [.float g] =>
      if g == 0.0 then .error { message := "Division by zero" }
      else .ok (.float (f / g))
  -- Arithmetic (float-int promotion)
  | "+", [.int m] => .ok (.float (f + Float.ofInt m))
  | "-", [.int m] => .ok (.float (f - Float.ofInt m))
  | "*", [.int m] => .ok (.float (f * Float.ofInt m))
  | "/", [.int m] =>
      if m == 0 then .error { message := "Division by zero" }
      else .ok (.float (f / Float.ofInt m))
  -- Unary
  | "-", [] => .ok (.float (-f))
  | "negated", [] => .ok (.float (-f))
  | "abs", [] => .ok (.float f.abs)
  | "sqrt", [] =>
      if f < 0.0 then .error { message := "sqrt of negative number" }
      else .ok (.float f.sqrt)
  | "sin", [] => .ok (.float f.sin)
  | "cos", [] => .ok (.float f.cos)
  | "tan", [] => .ok (.float f.tan)
  | "exp", [] => .ok (.float f.exp)
  | "log", [] =>
      if f <= 0.0 then .error { message := "log of non-positive number" }
      else .ok (.float f.log)
  | "floor", [] => .ok (.int (floatToInt f.floor))
  | "ceiling", [] => .ok (.int (floatToInt f.ceil))
  | "rounded", [] => .ok (.int (floatToInt f.round))
  | "truncated", [] => .ok (.int (floatToInt (if f >= 0.0 then f.floor else f.ceil)))
  -- Comparison (float-float)
  | "<", [.float g] => .ok (.bool (f < g))
  | ">", [.float g] => .ok (.bool (f > g))
  | "<=", [.float g] => .ok (.bool (f ≤ g))
  | ">=", [.float g] => .ok (.bool (f ≥ g))
  | "=", [.float g] => .ok (.bool (f == g))
  | "~=", [.float g] => .ok (.bool (f != g))
  -- Comparison (float-int promotion)
  | "<", [.int m] => .ok (.bool (f < Float.ofInt m))
  | ">", [.int m] => .ok (.bool (f > Float.ofInt m))
  | "<=", [.int m] => .ok (.bool (f ≤ Float.ofInt m))
  | ">=", [.int m] => .ok (.bool (f ≥ Float.ofInt m))
  | "=", [.int m] => .ok (.bool (f == Float.ofInt m))
  | "~=", [.int m] => .ok (.bool (f != Float.ofInt m))
  -- Identity
  | "==", [other] => .ok (.bool (valueIdentical (.float f) other))
  | "~~", [other] => .ok (.bool (!valueIdentical (.float f) other))
  -- Type errors
  | "+", [_] => .error { message := s!"+ expected Integer or Float, got {typeNameOf args.head!}" }
  | "-", [_] => .error { message := s!"- expected Integer or Float, got {typeNameOf args.head!}" }
  | "*", [_] => .error { message := s!"* expected Integer or Float, got {typeNameOf args.head!}" }
  | "/", [_] => .error { message := s!"/ expected Integer or Float, got {typeNameOf args.head!}" }
  | "<", [_] => .error { message := s!"< expected Integer or Float, got {typeNameOf args.head!}" }
  | ">", [_] => .error { message := s!"> expected Integer or Float, got {typeNameOf args.head!}" }
  | "<=", [_] => .error { message := s!"<= expected Integer or Float, got {typeNameOf args.head!}" }
  | ">=", [_] => .error { message := s!">= expected Integer or Float, got {typeNameOf args.head!}" }
  | "=", [_] => .ok (.bool false)
  | "~=", [_] => .ok (.bool true)
  | _, _ => .error { message := s!"No primitive '{sel}' for Float" }

/-- Evaluate string primitives. -/
def evalStringPrimitive (s : String) (sel : Symbol) (args : List Value)
    : Except PrimitiveError Value :=
  match sel, args with
  | ",", [.str s2] => .ok (.str (s ++ s2))
  | "size", [] => .ok (.int s.length)
  | "at:", [.int idx] =>
      let i := idx.toNat
      if i < 1 || i > s.length then
        .error { message := s!"String index {idx} out of bounds (1..{s.length})" }
      else
        let chars := s.toList
        match chars[i - 1]? with
        | some c => .ok (.char c)
        | none => .error { message := s!"String index {idx} out of bounds" }
  | "=", [.str s2] => .ok (.bool (s == s2))
  | "~=", [.str s2] => .ok (.bool (s != s2))
  | "isEmpty", [] => .ok (.bool s.isEmpty)
  | "asUppercase", [] => .ok (.str s.toUpper)
  | "asLowercase", [] => .ok (.str s.toLower)
  -- Identity
  | "==", [other] => .ok (.bool (valueIdentical (.str s) other))
  | "~~", [other] => .ok (.bool (!valueIdentical (.str s) other))
  -- Type errors
  | ",", [_] => .error { message := s!", expected String, got {typeNameOf args.head!}" }
  | "at:", [_] => .error { message := s!"at: expected Integer, got {typeNameOf args.head!}" }
  | "=", [_] => .ok (.bool false)
  | "~=", [_] => .ok (.bool true)
  | _, _ => .error { message := s!"No primitive '{sel}' for String" }

/-- Evaluate character primitives. -/
def evalCharPrimitive (c : Char) (sel : Symbol) (args : List Value)
    : Except PrimitiveError Value :=
  match sel, args with
  | "asInteger", [] => .ok (.int c.toNat)
  | "asString", [] => .ok (.str c.toString)
  | "asUppercase", [] => .ok (.char c.toUpper)
  | "asLowercase", [] => .ok (.char c.toLower)
  | "isLetter", [] => .ok (.bool c.isAlpha)
  | "isDigit", [] => .ok (.bool c.isDigit)
  | "isAlphaNumeric", [] => .ok (.bool c.isAlphanum)
  | "=", [.char c2] => .ok (.bool (c == c2))
  | "~=", [.char c2] => .ok (.bool (c != c2))
  | "<", [.char c2] => .ok (.bool (c < c2))
  | ">", [.char c2] => .ok (.bool (c > c2))
  | "<=", [.char c2] => .ok (.bool (c ≤ c2))
  | ">=", [.char c2] => .ok (.bool (c ≥ c2))
  -- Identity
  | "==", [other] => .ok (.bool (valueIdentical (.char c) other))
  | "~~", [other] => .ok (.bool (!valueIdentical (.char c) other))
  | "=", [_] => .ok (.bool false)
  | "~=", [_] => .ok (.bool true)
  | _, _ => .error { message := s!"No primitive '{sel}' for Character" }

/-- Evaluate symbol primitives. -/
def evalSymbolPrimitive (sym : Symbol) (sel : Symbol) (args : List Value)
    : Except PrimitiveError Value :=
  match sel, args with
  | "asString", [] => .ok (.str sym)
  | "size", [] => .ok (.int sym.length)
  | "=", [.symbol s2] => .ok (.bool (sym == s2))
  | "~=", [.symbol s2] => .ok (.bool (sym != s2))
  -- Identity
  | "==", [other] => .ok (.bool (valueIdentical (.symbol sym) other))
  | "~~", [other] => .ok (.bool (!valueIdentical (.symbol sym) other))
  | "=", [_] => .ok (.bool false)
  | "~=", [_] => .ok (.bool true)
  | _, _ => .error { message := s!"No primitive '{sel}' for Symbol" }

/-- Evaluate boolean primitives. -/
def evalBoolPrimitive (b : Bool) (sel : Symbol) (args : List Value)
    : Except PrimitiveError Value :=
  match sel, args with
  | "&", [.bool b2] => .ok (.bool (b && b2))
  | "|", [.bool b2] => .ok (.bool (b || b2))
  | "not", [] => .ok (.bool (!b))
  | "and:", [.bool b2] => .ok (.bool (b && b2))
  | "or:", [.bool b2] => .ok (.bool (b || b2))
  | "xor:", [.bool b2] => .ok (.bool (b != b2))
  | "=", [.bool b2] => .ok (.bool (b == b2))
  | "~=", [.bool b2] => .ok (.bool (b != b2))
  -- Identity
  | "==", [other] => .ok (.bool (valueIdentical (.bool b) other))
  | "~~", [other] => .ok (.bool (!valueIdentical (.bool b) other))
  -- Type errors
  | "&", [_] => .error { message := s!"& expected Boolean, got {typeNameOf args.head!}" }
  | "|", [_] => .error { message := s!"| expected Boolean, got {typeNameOf args.head!}" }
  | "and:", [_] => .error { message := s!"and: expected Boolean, got {typeNameOf args.head!}" }
  | "or:", [_] => .error { message := s!"or: expected Boolean, got {typeNameOf args.head!}" }
  | "xor:", [_] => .error { message := s!"xor: expected Boolean, got {typeNameOf args.head!}" }
  | "=", [_] => .ok (.bool false)
  | "~=", [_] => .ok (.bool true)
  | _, _ => .error { message := s!"No primitive '{sel}' for Boolean" }

/-- Evaluate nil primitives. -/
def evalNilPrimitive (sel : Symbol) (args : List Value)
    : Except PrimitiveError Value :=
  match sel, args with
  | "isNil", [] => .ok (.bool true)
  | "notNil", [] => .ok (.bool false)
  | "=", [.nil] => .ok (.bool true)
  | "=", [_] => .ok (.bool false)
  | "~=", [.nil] => .ok (.bool false)
  | "~=", [_] => .ok (.bool true)
  -- Identity
  | "==", [other] => .ok (.bool (valueIdentical .nil other))
  | "~~", [other] => .ok (.bool (!valueIdentical .nil other))
  | _, _ => .error { message := s!"No primitive '{sel}' for UndefinedObject" }

/-- Evaluate array primitives. -/
def evalArrayPrimitive (elems : List Value) (sel : Symbol) (args : List Value)
    : Except PrimitiveError Value :=
  match sel, args with
  | "size", [] => .ok (.int elems.length)
  | "isEmpty", [] => .ok (.bool elems.isEmpty)
  | "at:", [.int idx] =>
      let i := idx.toNat
      if i < 1 || i > elems.length then
        .error { message := s!"Array index {idx} out of bounds (1..{elems.length})" }
      else
        match elems[i - 1]? with
        | some v => .ok v
        | none => .error { message := s!"Array index {idx} out of bounds" }
  | "first", [] =>
      match elems.head? with
      | some v => .ok v
      | none => .error { message := "first called on empty array" }
  | "last", [] =>
      match elems.getLast? with
      | some v => .ok v
      | none => .error { message := "last called on empty array" }
  | ",", [.array elems2] => .ok (.array (elems ++ elems2))
  | "=", [.array elems2] =>
      .ok (.bool (elems.length == elems2.length &&
        (elems.zip elems2).all fun (a, b) => valueIdentical a b))
  | "~=", [.array elems2] =>
      .ok (.bool (elems.length != elems2.length ||
        !(elems.zip elems2).all fun (a, b) => valueIdentical a b))
  -- Identity
  | "==", [other] => .ok (.bool (valueIdentical (.array elems) other))
  | "~~", [other] => .ok (.bool (!valueIdentical (.array elems) other))
  -- Type errors
  | "at:", [_] => .error { message := s!"at: expected Integer, got {typeNameOf args.head!}" }
  | ",", [_] => .error { message := s!", expected Array, got {typeNameOf args.head!}" }
  | "=", [_] => .ok (.bool false)
  | "~=", [_] => .ok (.bool true)
  | _, _ => .error { message := s!"No primitive '{sel}' for Array" }

/-- Evaluate dictionary primitives. -/
def evalDictPrimitive (entries : List (Value × Value)) (sel : Symbol) (args : List Value)
    : Except PrimitiveError Value :=
  match sel, args with
  | "size", [] => .ok (.int entries.length)
  | "isEmpty", [] => .ok (.bool entries.isEmpty)
  | "at:", [key] =>
      match entries.find? fun (k, _) => valueIdentical k key with
      | some (_, v) => .ok v
      | none => .error { message := s!"Key not found in Dictionary" }
  | "at:ifAbsent:", [key, default] =>
      match entries.find? fun (k, _) => valueIdentical k key with
      | some (_, v) => .ok v
      | none => .ok default
  | "includesKey:", [key] =>
      .ok (.bool (entries.any fun (k, _) => valueIdentical k key))
  | "keys", [] => .ok (.array (entries.map Prod.fst))
  | "values", [] => .ok (.array (entries.map Prod.snd))
  -- Identity
  | "==", [other] => .ok (.bool (valueIdentical (.dict entries) other))
  | "~~", [other] => .ok (.bool (!valueIdentical (.dict entries) other))
  | "=", [_] => .ok (.bool false)
  | "~=", [_] => .ok (.bool true)
  | _, _ => .error { message := s!"No primitive '{sel}' for Dictionary" }

/-- Evaluate primitive operations on built-in values.
    Returns `.error` if no primitive exists for this receiver/selector combination. -/
def evalPrimitive (recv : Value) (sel : Symbol) (args : List Value)
    : Except PrimitiveError Value :=
  match recv with
  | .int n => evalIntPrimitive n sel args
  | .float f => evalFloatPrimitive f sel args
  | .str s => evalStringPrimitive s sel args
  | .char c => evalCharPrimitive c sel args
  | .symbol sym => evalSymbolPrimitive sym sel args
  | .bool b => evalBoolPrimitive b sel args
  | .nil => evalNilPrimitive sel args
  | .array elems => evalArrayPrimitive elems sel args
  | .dict entries => evalDictPrimitive entries sel args
  | .object _ _ _ => .error { message := s!"No primitive '{sel}' for {typeNameOf recv}" }
  | .classObj _ => .error { message := s!"No primitive '{sel}' for {typeNameOf recv}" }
  | .block _ _ _ _ _ => .error { message := s!"No primitive '{sel}' for Block" }

end Smalltalk

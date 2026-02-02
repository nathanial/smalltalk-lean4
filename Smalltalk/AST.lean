/-
  Smalltalk.AST - Core syntax tree for a Smalltalk subset.
-/
namespace Smalltalk

/-- Smalltalk identifier (selector, variable, class name). -/
abbrev Symbol := String

/-- Literal values that can appear directly in source. -/
inductive Literal
  | int (value : Int)
  | float (value : Float)
  | scaled (mantissa : Int) (scale : Nat)
  | str (value : String)
  | char (value : Char)
  | symbol (value : Symbol)
  | array (elements : List Literal)
  | dict (entries : List (Literal × Literal))
  | byteArray (elements : List UInt8)
  | bool (value : Bool)
  | nil
  deriving Repr, BEq, Inhabited

/-- Expression forms for a minimal Smalltalk. -/
inductive Expr
  | lit (lit : Literal)
  | var (name : Symbol)
  | assign (name : Symbol) (value : Expr)
  | send (recv : Expr) (selector : Symbol) (args : List Expr)
  | block (params : List Symbol) (temps : List Symbol) (body : List Expr)
  | return (value : Expr)
  | seq (exprs : List Expr)
  | cascade (receiver : Expr) (chains : List (List (Symbol × List Expr)))
  | array (elements : List Expr)
  deriving Repr, BEq, Inhabited

abbrev Message := Symbol × List Expr
abbrev MessageChain := List Message

/-- Method pragmas (e.g., <primitive: 1>). -/
structure Pragma where
  selector : Symbol
  args : List Literal
  deriving Repr, BEq, Inhabited

/-- Method definition attached to a class. -/
structure Method where
  selector : Symbol
  params : List Symbol
  temps : List Symbol := []
  pragmas : List Pragma := []
  body : List Expr
  deriving Repr, BEq, Inhabited

/-- Class definition with optional superclass. -/
structure ClassDef where
  name : Symbol
  super : Option Symbol := none
  ivars : List Symbol := []
  methods : List Method := []
  classMethods : List Method := []
  deriving Repr, BEq, Inhabited

/-- Whole program with class table and entry expressions. -/
structure Program where
  classes : List ClassDef
  main : List Expr
  deriving Repr, BEq, Inhabited

end Smalltalk

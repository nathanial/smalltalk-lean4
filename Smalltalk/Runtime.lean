import Smalltalk.AST

namespace Smalltalk

/-- Runtime values used by the interpreter. -/
inductive Value
  | int (value : Int)
  | float (value : Float)
  | str (value : String)
  | char (value : Char)
  | symbol (value : Symbol)
  | bool (value : Bool)
  | nil
  | array (elements : List Value)
  | dict (entries : List (Value × Value))
  | object (id : Nat) (className : Symbol) (fields : List (Symbol × Value))
  | classObj (name : Symbol)
  | block (params : List Symbol) (temps : List Symbol) (body : List Expr)
          (capturedEnv : List (Symbol × Value)) (capturedSelf : Option Value)
  deriving Repr, Inhabited

/-- Variable environment. -/
abbrev Env := List (Symbol × Value)

/-- Class registry mapping class names to definitions. -/
abbrev ClassRegistry := List (Symbol × ClassDef)

/-- Look up a class by name in the registry. -/
def registryLookup (reg : ClassRegistry) (name : Symbol) : Option ClassDef :=
  reg.find? (fun (n, _) => n == name) |>.map Prod.snd

/-- Empty environment. -/
def emptyEnv : Env := []

/-- Look up a name in the environment. -/
def envLookup : Env → Symbol → Option Value
  | [], _ => none
  | (key, value) :: rest, name =>
      if key == name then
        some value
      else
        envLookup rest name

/-- Insert or update a binding in the environment. -/
def envInsert (env : Env) (name : Symbol) (value : Value) : Env :=
  (name, value) :: env

end Smalltalk

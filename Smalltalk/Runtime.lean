import Smalltalk.AST

namespace Smalltalk

/-- Runtime values used by the interpreter. -/
inductive Value
  | int (value : Int)
  | str (value : String)
  | bool (value : Bool)
  | nil
  | object (className : Symbol) (fields : List (Symbol × Value))
  deriving Repr

/-- Variable environment. -/
abbrev Env := List (Symbol × Value)

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

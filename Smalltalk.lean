/-
  Smalltalk - Smalltalk interpreter scaffolding for Lean 4.
-/
import Smalltalk.AST
import Smalltalk.Runtime
import Smalltalk.Parse
import Smalltalk.Primitives
import Smalltalk.Eval
import Smalltalk.Image
import Smalltalk.Stdlib

namespace Smalltalk

/-- Convenience alias to parse source and return a program. -/
def parseProgram (input : String) : Except ParseError Program :=
  parse input

def parseExpression (input : String) : Except ParseError Expr :=
  parseExpr input

def parseMethodDef (input : String) : Except ParseError Method :=
  parseMethod input

end Smalltalk

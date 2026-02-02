import Smalltalk.AST
import Smalltalk.Parse

namespace Smalltalk
namespace Stdlib

def defaultPath : System.FilePath :=
  System.FilePath.mk "Stdlib.st"

def parseFromFile (path : System.FilePath) : IO (Except String Program) := do
  try
    let source ← IO.FS.readFile path
    match Smalltalk.parse source with
    | .ok program => pure (.ok program)
    | .error e => pure (.error e.message)
  catch e =>
    pure (.error s!"{e}")

def loadIfExists (path : System.FilePath) : IO (Except String (Option Program)) := do
  try
    let source ← IO.FS.readFile path
    match Smalltalk.parse source with
    | .ok program => pure (.ok (some program))
    | .error e => pure (.error e.message)
  catch _ =>
    pure (.ok none)

def mergePrograms (stdlib user : Program) : Program :=
  { classes := stdlib.classes ++ user.classes
    main := stdlib.main ++ user.main }

end Stdlib
end Smalltalk

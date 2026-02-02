/-
  Smalltalk test suite entrypoint.
-/
import Crucible
import Tests.ASTTests
import Tests.ParserTests
import Tests.EvalTests
import Tests.MethodTests
import Tests.ImageTests

open Crucible

def main : IO UInt32 := do
  IO.println "Smalltalk Tests"
  IO.println "==============="
  runAllSuites

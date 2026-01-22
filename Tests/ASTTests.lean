/-
  Smalltalk AST tests.
-/
import Crucible
import Smalltalk

open Crucible
open Smalltalk

namespace ASTTests

testSuite "Smalltalk.AST"

test "literal equality" := do
  let a : Literal := .int 7
  let b : Literal := .int 7
  shouldSatisfy (a == b) "literals should be equal"

test "message send construction" := do
  let expr := Expr.send (.var "self") "at:" [Expr.lit (.int 1)]
  let same := Expr.send (.var "self") "at:" [Expr.lit (.int 1)]
  shouldSatisfy (expr == same) "ast should roundtrip"

test "block temps construction" := do
  let expr := Expr.block ["x"] ["tmp"] [Expr.var "x"]
  let same := Expr.block ["x"] ["tmp"] [Expr.var "x"]
  shouldSatisfy (expr == same) "blocks should be equal"



end ASTTests

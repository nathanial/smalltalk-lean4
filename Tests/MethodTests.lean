/-
  Smalltalk method parser tests.
-/
import Crucible
import Smalltalk

open Crucible
open Smalltalk

namespace MethodTests

testSuite "Smalltalk.Method"

test "parse unary method" := do
  match Smalltalk.parseMethod "size ^ 1" with
  | .ok m =>
      let expected : Method :=
        { selector := "size"
          params := []
          temps := []
          body := [Expr.return (Expr.lit (.int 1))] }
      shouldSatisfy (m == expected) "unary method should parse"
  | .error e => throw (IO.userError s!"parse error: {e.message}")

test "parse binary method" := do
  match Smalltalk.parseMethod "+ aNumber ^ aNumber" with
  | .ok m =>
      let expected : Method :=
        { selector := "+"
          params := ["aNumber"]
          temps := []
          body := [Expr.return (Expr.var "aNumber")] }
      shouldSatisfy (m == expected) "binary method should parse"
  | .error e => throw (IO.userError s!"parse error: {e.message}")

test "parse keyword method with temps" := do
  let src := "at: index put: value | old | old := value. ^ old"
  match Smalltalk.parseMethod src with
  | .ok m =>
      let expected : Method :=
        { selector := "at:put:"
          params := ["index", "value"]
          temps := ["old"]
          body := [
            Expr.assign "old" (Expr.var "value"),
            Expr.return (Expr.var "old")
          ] }
      shouldSatisfy (m == expected) "keyword method should parse"
  | .error e => throw (IO.userError s!"parse error: {e.message}")

test "parse method pragmas" := do
  let src := "foo <primitive: 1> <tag: #bar> ^ 1"
  match Smalltalk.parseMethod src with
  | .ok m =>
      let expected : Method :=
        { selector := "foo"
          params := []
          temps := []
          pragmas := [
            { selector := "primitive:", args := [.int 1] },
            { selector := "tag:", args := [.symbol "bar"] }
          ]
          body := [Expr.return (Expr.lit (.int 1))] }
      shouldSatisfy (m == expected) "pragmas should parse"
  | .error e => throw (IO.userError s!"parse error: {e.message}")



end MethodTests

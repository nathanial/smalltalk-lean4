/-
  Smalltalk image persistence tests.
-/
import Crucible
import Smalltalk

open Crucible
open Smalltalk

namespace ImageTests

testSuite "Smalltalk.Image"

def sampleMethod : Method :=
  { selector := "initWithX:y:"
    params := ["x", "y"]
    temps := ["tmp"]
    pragmas := [{ selector := "primitive:", args := [.int 1] }]
    body := [
      .assign "tmp" (.array [.lit (.int 1), .lit (.int 2)]),
      .cascade (.var "self") [[("x:", [.lit (.int 3)]), ("y:", [.lit (.int 4)])]],
      .lit (.byteArray [1, 2, 3]),
      .lit (.scaled 123 2),
      .return (.var "tmp")
    ]
  }

def sampleClass : ClassDef :=
  { name := "Point"
    super := some "Object"
    ivars := ["x", "y"]
    methods := [sampleMethod]
  }

def sampleState : ExecState :=
  { env := [
      ("x", .int 42),
      ("f", .float 3.5),
      ("s", .str "hello"),
      ("c", .char 'Z'),
      ("sym", .symbol "foo"),
      ("arr", .array [.int 1, .bool true, .nil]),
      ("dict", .dict [(.symbol "a", .int 1), (.str "b", .float 2.5)]),
      ("blk",
        .block ["n"] ["tmp"] [.assign "tmp" (.var "n"), .return (.var "tmp")]
          [("capt", .int 9)] (some (.int 7)))
    ]
    self := some (.object 1 "Point" [("x", .int 1), ("y", .int 2)])
    classes := [("Point", sampleClass)]
    currentClass := some "Point"
    nextObjectId := 2
  }

test "image encode/decode roundtrip" := do
  let bytes := Smalltalk.Image.encode sampleState
  match Smalltalk.Image.decode bytes with
  | .ok state =>
      let expected := reprStr sampleState
      let actual := reprStr state
      shouldSatisfy (expected == actual) s!"expected {expected}, got {actual}"
  | .error e =>
      throw (IO.userError s!"decode failed: {e}")

end ImageTests

import Smalltalk.AST
import Smalltalk.Runtime
import Smalltalk.Eval

namespace Smalltalk

namespace Image

def imageMagic : ByteArray := ByteArray.mk #[83, 84, 73, 77] -- "STIM"
def imageVersion : UInt8 := 2

structure Reader where
  bytes : ByteArray
  pos : Nat

def readByte (r : Reader) : Except String (UInt8 × Reader) := do
  if r.pos < r.bytes.size then
    let b := r.bytes.get! r.pos
    .ok (b, { r with pos := r.pos + 1 })
  else
    .error "unexpected end of input"

def readBytes (r : Reader) (n : Nat) : Except String (ByteArray × Reader) := do
  let next := r.pos + n
  if next <= r.bytes.size then
    let chunk := r.bytes.extract r.pos next
    .ok (chunk, { r with pos := next })
  else
    .error "unexpected end of input"

def encodeByte (b : UInt8) : ByteArray :=
  ByteArray.empty.push b

partial def encodeNat (n : Nat) : ByteArray :=
  let rec go (n : Nat) (acc : ByteArray) : ByteArray :=
    let byte := n % 128
    let rest := n / 128
    let out := if rest == 0 then byte else byte + 128
    let acc := acc.push out.toUInt8
    if rest == 0 then acc else go rest acc
  go n ByteArray.empty

partial def readNat (r : Reader) : Except String (Nat × Reader) := do
  let rec loop (acc : Nat) (shift : Nat) (r : Reader) : Except String (Nat × Reader) := do
    if shift > 1024 then
      .error "varint too large"
    else
      let (b, r) ← readByte r
      let byte := b.toNat
      let chunk := byte % 128
      let acc := acc + Nat.shiftLeft chunk shift
      if byte < 128 then
        .ok (acc, r)
      else
        loop acc (shift + 7) r
  loop 0 0 r

def encodeBool (b : Bool) : ByteArray :=
  encodeByte (if b then 1 else 0)

def readBool (r : Reader) : Except String (Bool × Reader) := do
  let (b, r) ← readByte r
  match b.toNat with
  | 0 => .ok (false, r)
  | 1 => .ok (true, r)
  | _ => .error "invalid boolean tag"

def encodeInt (n : Int) : ByteArray :=
  let zigzag : Nat :=
    if n >= 0 then
      (Int.toNat n) * 2
    else
      (Int.toNat (-n)) * 2 - 1
  encodeNat zigzag

def readInt (r : Reader) : Except String (Int × Reader) := do
  let (n, r) ← readNat r
  if n % 2 == 0 then
    .ok (Int.ofNat (n / 2), r)
  else
    .ok (Int.negOfNat (n / 2 + 1), r)

partial def encodeUInt64 (n : UInt64) : ByteArray :=
  let v := n.toNat
  let rec go (i : Nat) (acc : ByteArray) : ByteArray :=
    if i == 8 then acc
    else
      let shift := (7 - i) * 8
      let byte := (Nat.shiftRight v shift) % 256
      go (i + 1) (acc.push byte.toUInt8)
  go 0 ByteArray.empty

partial def readUInt64 (r : Reader) : Except String (UInt64 × Reader) := do
  let rec loop (i : Nat) (acc : Nat) (r : Reader) : Except String (Nat × Reader) := do
    if i == 8 then
      .ok (acc, r)
    else
      let (b, r) ← readByte r
      let acc := Nat.shiftLeft acc 8 + b.toNat
      loop (i + 1) acc r
  let (n, r) ← loop 0 0 r
  .ok (UInt64.ofNat n, r)

def encodeFloat (f : Float) : ByteArray :=
  encodeUInt64 (Float.toBits f)

def readFloat (r : Reader) : Except String (Float × Reader) := do
  let (bits, r) ← readUInt64 r
  .ok (Float.ofBits bits, r)

def encodeBytes (bytes : ByteArray) : ByteArray :=
  encodeNat bytes.size ++ bytes

def readBytesWithLength (r : Reader) : Except String (ByteArray × Reader) := do
  let (len, r) ← readNat r
  readBytes r len

def encodeString (s : String) : ByteArray :=
  encodeBytes s.toUTF8

def readString (r : Reader) : Except String (String × Reader) := do
  let (bytes, r) ← readBytesWithLength r
  match String.fromUTF8? bytes with
  | some s => .ok (s, r)
  | none => .error "invalid UTF-8 string"

def encodeChar (c : Char) : ByteArray :=
  encodeNat c.toNat

def readChar (r : Reader) : Except String (Char × Reader) := do
  let (n, r) ← readNat r
  .ok (Char.ofNat n, r)

def encodeOption (encode : α → ByteArray) : Option α → ByteArray
  | none => encodeByte 0
  | some v => encodeByte 1 ++ encode v

def readOption (decode : Reader → Except String (α × Reader))
    (r : Reader) : Except String (Option α × Reader) := do
  let (tag, r) ← readByte r
  match tag.toNat with
  | 0 => .ok (none, r)
  | 1 =>
      let (v, r) ← decode r
      .ok (some v, r)
  | _ => .error "invalid option tag"

def encodeList (encode : α → ByteArray) (xs : List α) : ByteArray :=
  xs.foldl (fun acc x => acc ++ encode x) (encodeNat xs.length)

partial def readList (decode : Reader → Except String (α × Reader))
    (r : Reader) : Except String (List α × Reader) := do
  let (len, r) ← readNat r
  let rec loop (n : Nat) (acc : List α) (r : Reader) : Except String (List α × Reader) := do
    if n == 0 then
      .ok (acc.reverse, r)
    else
      let (v, r) ← decode r
      loop (n - 1) (v :: acc) r
  loop len [] r

def encodePair (encodeA : α → ByteArray) (encodeB : β → ByteArray) (pair : α × β) : ByteArray :=
  encodeA pair.1 ++ encodeB pair.2

def readPair (decodeA : Reader → Except String (α × Reader))
    (decodeB : Reader → Except String (β × Reader))
    (r : Reader) : Except String ((α × β) × Reader) := do
  let (a, r) ← decodeA r
  let (b, r) ← decodeB r
  .ok ((a, b), r)

partial def encodeLiteral : Literal → ByteArray
  | .int n => encodeByte 0 ++ encodeInt n
  | .float f => encodeByte 1 ++ encodeFloat f
  | .scaled m s => encodeByte 2 ++ encodeInt m ++ encodeNat s
  | .str s => encodeByte 3 ++ encodeString s
  | .char c => encodeByte 4 ++ encodeChar c
  | .symbol sym => encodeByte 5 ++ encodeString sym
  | .array elems => encodeByte 6 ++ encodeList encodeLiteral elems
  | .dict entries =>
      encodeByte 7 ++ encodeList (encodePair encodeLiteral encodeLiteral) entries
  | .byteArray bytes =>
      encodeByte 8 ++ encodeBytes (ByteArray.mk bytes.toArray)
  | .bool b => encodeByte 9 ++ encodeBool b
  | .nil => encodeByte 10

partial def readLiteral (r : Reader) : Except String (Literal × Reader) := do
  let (tag, r) ← readByte r
  match tag.toNat with
  | 0 =>
      let (n, r) ← readInt r
      .ok (.int n, r)
  | 1 =>
      let (f, r) ← readFloat r
      .ok (.float f, r)
  | 2 =>
      let (m, r) ← readInt r
      let (s, r) ← readNat r
      .ok (.scaled m s, r)
  | 3 =>
      let (s, r) ← readString r
      .ok (.str s, r)
  | 4 =>
      let (c, r) ← readChar r
      .ok (.char c, r)
  | 5 =>
      let (s, r) ← readString r
      .ok (.symbol s, r)
  | 6 =>
      let (elems, r) ← readList readLiteral r
      .ok (.array elems, r)
  | 7 =>
      let (entries, r) ← readList (readPair readLiteral readLiteral) r
      .ok (.dict entries, r)
  | 8 =>
      let (bytes, r) ← readBytesWithLength r
      .ok (.byteArray bytes.toList, r)
  | 9 =>
      let (b, r) ← readBool r
      .ok (.bool b, r)
  | 10 => .ok (.nil, r)
  | _ => .error "invalid literal tag"

partial def encodeExpr : Expr → ByteArray
  | .lit lit => encodeByte 0 ++ encodeLiteral lit
  | .var name => encodeByte 1 ++ encodeString name
  | .assign name value => encodeByte 2 ++ encodeString name ++ encodeExpr value
  | .send recv sel args =>
      encodeByte 3 ++ encodeExpr recv ++ encodeString sel ++ encodeList encodeExpr args
  | .block params temps body =>
      encodeByte 4
        ++ encodeList encodeString params
        ++ encodeList encodeString temps
        ++ encodeList encodeExpr body
  | .return value => encodeByte 5 ++ encodeExpr value
  | .seq exprs => encodeByte 6 ++ encodeList encodeExpr exprs
  | .cascade receiver chains =>
      encodeByte 7 ++ encodeExpr receiver ++ encodeList (encodeList encodeMessage) chains
  | .array elems => encodeByte 8 ++ encodeList encodeExpr elems
where
  encodeMessage (msg : Symbol × List Expr) : ByteArray :=
    encodeString msg.1 ++ encodeList encodeExpr msg.2

partial def readExpr (r : Reader) : Except String (Expr × Reader) := do
  let (tag, r) ← readByte r
  match tag.toNat with
  | 0 =>
      let (lit, r) ← readLiteral r
      .ok (.lit lit, r)
  | 1 =>
      let (name, r) ← readString r
      .ok (.var name, r)
  | 2 =>
      let (name, r) ← readString r
      let (value, r) ← readExpr r
      .ok (.assign name value, r)
  | 3 =>
      let (recv, r) ← readExpr r
      let (sel, r) ← readString r
      let (args, r) ← readList readExpr r
      .ok (.send recv sel args, r)
  | 4 =>
      let (params, r) ← readList readString r
      let (temps, r) ← readList readString r
      let (body, r) ← readList readExpr r
      .ok (.block params temps body, r)
  | 5 =>
      let (value, r) ← readExpr r
      .ok (.return value, r)
  | 6 =>
      let (exprs, r) ← readList readExpr r
      .ok (.seq exprs, r)
  | 7 =>
      let (receiver, r) ← readExpr r
      let (chains, r) ← readList (readList readMessage) r
      .ok (.cascade receiver chains, r)
  | 8 =>
      let (elems, r) ← readList readExpr r
      .ok (.array elems, r)
  | _ => .error "invalid expression tag"
where
  readMessage (r : Reader) : Except String ((Symbol × List Expr) × Reader) := do
    let (sel, r) ← readString r
    let (args, r) ← readList readExpr r
    .ok ((sel, args), r)

def encodePragma (p : Pragma) : ByteArray :=
  encodeString p.selector ++ encodeList encodeLiteral p.args

def readPragma (r : Reader) : Except String (Pragma × Reader) := do
  let (sel, r) ← readString r
  let (args, r) ← readList readLiteral r
  .ok ({ selector := sel, args := args }, r)

def encodeMethod (m : Method) : ByteArray :=
  encodeString m.selector
    ++ encodeList encodeString m.params
    ++ encodeList encodeString m.temps
    ++ encodeList encodePragma m.pragmas
    ++ encodeList encodeExpr m.body

def readMethod (r : Reader) : Except String (Method × Reader) := do
  let (sel, r) ← readString r
  let (params, r) ← readList readString r
  let (temps, r) ← readList readString r
  let (pragmas, r) ← readList readPragma r
  let (body, r) ← readList readExpr r
  .ok ({ selector := sel, params := params, temps := temps, pragmas := pragmas, body := body }, r)

def encodeClassDef (c : ClassDef) : ByteArray :=
  encodeString c.name
    ++ encodeOption encodeString c.super
    ++ encodeList encodeString c.ivars
    ++ encodeList encodeMethod c.methods
    ++ encodeList encodeMethod c.classMethods

def readClassDef (r : Reader) : Except String (ClassDef × Reader) := do
  let (name, r) ← readString r
  let (super, r) ← readOption readString r
  let (ivars, r) ← readList readString r
  let (methods, r) ← readList readMethod r
  let (classMethods, r) ← readList readMethod r
  .ok ({ name := name, super := super, ivars := ivars, methods := methods, classMethods := classMethods }, r)

partial def encodeValue : Value → ByteArray
  | .int n => encodeByte 0 ++ encodeInt n
  | .float f => encodeByte 1 ++ encodeFloat f
  | .str s => encodeByte 2 ++ encodeString s
  | .char c => encodeByte 3 ++ encodeChar c
  | .symbol sym => encodeByte 4 ++ encodeString sym
  | .bool b => encodeByte 5 ++ encodeBool b
  | .nil => encodeByte 6
  | .array elems => encodeByte 7 ++ encodeList encodeValue elems
  | .dict entries => encodeByte 8 ++ encodeList (encodePair encodeValue encodeValue) entries
  | .object id className fields =>
      encodeByte 9
        ++ encodeNat id
        ++ encodeString className
        ++ encodeList (encodePair encodeString encodeValue) fields
  | .block params temps body capturedEnv capturedSelf =>
      encodeByte 10
        ++ encodeList encodeString params
        ++ encodeList encodeString temps
        ++ encodeList encodeExpr body
        ++ encodeList (encodePair encodeString encodeValue) capturedEnv
        ++ encodeOption encodeValue capturedSelf
  | .classObj name =>
      encodeByte 11 ++ encodeString name

partial def readValue (r : Reader) : Except String (Value × Reader) := do
  let (tag, r) ← readByte r
  match tag.toNat with
  | 0 =>
      let (n, r) ← readInt r
      .ok (.int n, r)
  | 1 =>
      let (f, r) ← readFloat r
      .ok (.float f, r)
  | 2 =>
      let (s, r) ← readString r
      .ok (.str s, r)
  | 3 =>
      let (c, r) ← readChar r
      .ok (.char c, r)
  | 4 =>
      let (s, r) ← readString r
      .ok (.symbol s, r)
  | 5 =>
      let (b, r) ← readBool r
      .ok (.bool b, r)
  | 6 => .ok (.nil, r)
  | 7 =>
      let (elems, r) ← readList readValue r
      .ok (.array elems, r)
  | 8 =>
      let (entries, r) ← readList (readPair readValue readValue) r
      .ok (.dict entries, r)
  | 9 =>
      let (id, r) ← readNat r
      let (className, r) ← readString r
      let (fields, r) ← readList (readPair readString readValue) r
      .ok (.object id className fields, r)
  | 10 =>
      let (params, r) ← readList readString r
      let (temps, r) ← readList readString r
      let (body, r) ← readList readExpr r
      let (capturedEnv, r) ← readList (readPair readString readValue) r
      let (capturedSelf, r) ← readOption readValue r
      .ok (.block params temps body capturedEnv capturedSelf, r)
  | 11 =>
      let (name, r) ← readString r
      .ok (.classObj name, r)
  | _ => .error "invalid value tag"

def encodeExecState (state : ExecState) : ByteArray :=
  encodeList (encodePair encodeString encodeValue) state.env
    ++ encodeOption encodeValue state.self
    ++ encodeList (encodePair encodeString encodeClassDef) state.classes
    ++ encodeOption encodeString state.currentClass
    ++ encodeNat state.nextObjectId

def readExecState (r : Reader) : Except String (ExecState × Reader) := do
  let (env, r) ← readList (readPair readString readValue) r
  let (self, r) ← readOption readValue r
  let (classes, r) ← readList (readPair readString readClassDef) r
  let (currentClass, r) ← readOption readString r
  let (nextObjectId, r) ← readNat r
  .ok ({ env := env, self := self, classes := classes, currentClass := currentClass, nextObjectId := nextObjectId }, r)

def encode (state : ExecState) : ByteArray :=
  imageMagic ++ encodeByte imageVersion ++ encodeExecState state

def decode (bytes : ByteArray) : Except String ExecState := do
  let r := { bytes := bytes, pos := 0 }
  let (magic, r) ← readBytes r imageMagic.size
  if magic != imageMagic then
    .error "invalid image magic"
  else
    let (version, r) ← readByte r
    if version != imageVersion then
      .error "unsupported image version"
    else
      let (state, r) ← readExecState r
      if r.pos == r.bytes.size then
        .ok state
      else
        .error "trailing bytes after image"

def save (path : System.FilePath) (state : ExecState) : IO (Except String Unit) := do
  IO.FS.writeBinFile path (encode state)
  pure (.ok ())

def load (path : System.FilePath) : IO (Except String ExecState) := do
  let bytes ← IO.FS.readBinFile path
  pure (decode bytes)

end Image

end Smalltalk

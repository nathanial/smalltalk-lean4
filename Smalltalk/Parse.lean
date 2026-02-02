import Sift
import Smalltalk.AST

namespace Smalltalk

open Sift

/-- Parsing errors for source text. -/
structure ParseError where
  message : String
  deriving Repr, BEq, Inhabited

/-- Optional parser. -/
def opt (p : Parser Unit α) : Parser Unit (Option α) :=
  (some <$> attempt p) <|> pure none

/-- Skip whitespace and comments. -/
partial def ws : Parser Unit Unit := do
  let _ ← many (wsChar <|> attempt comment)
  pure ()
where
  wsChar : Parser Unit Unit := do
    let _ ← satisfy fun c => c == ' ' || c == '\t' || c == '\n' || c == '\r'
    pure ()

  comment : Parser Unit Unit := do
    let _ ← char '"'
    let _ ← many (attempt (string "\"\"" *> pure ()) <|>
      ((satisfy fun c => c != '"') *> pure ()))
    let _ ← char '"'
    pure ()

/-- Parse a fixed symbol string, skipping leading whitespace. -/
def symbol (s : String) : Parser Unit Unit := do
  ws
  let _ ← string s
  pure ()

/-- Parse an identifier without leading whitespace. -/
def identRaw : Parser Unit String := do
  let first ← satisfy fun c => c.isAlpha || c == '_'
  let rest ← manyChars (satisfy fun c => c.isAlphanum || c == '_')
  pure (first.toString ++ rest)

/-- Parse an identifier with leading whitespace. -/
def ident : Parser Unit String := do
  ws
  identRaw

/-- Parse a keyword selector part (e.g. "at:"). -/
def keywordPart : Parser Unit String := attempt do
  ws
  let name ← identRaw
  let _ ← char ':'
  pure (name ++ ":")

/-- Parse a unary selector (identifier not followed by ':'). -/
def unarySelector : Parser Unit String := attempt do
  ws
  let name ← identRaw
  let next ← peek
  match next with
  | some ':' => Parser.fail "keyword selector"
  | _ => pure name

/-- Parse a binary selector. -/
def binarySelector : Parser Unit String := attempt do
  ws
  let chars ← many1Chars (satisfy isBinaryChar)
  pure chars
  where
    isBinaryChar (c : Char) : Bool :=
      "+-*/\\=<>~&|,@%?".contains c

/-- Parse a string literal using single quotes ('' to escape '). -/
def stringLitCore (skipWs : Bool) : Parser Unit String := do
  if skipWs then ws else pure ()
  let _ ← char '\''
  let chars ← manyChars stringChar
  let _ ← char '\''
  pure chars
where
  stringChar : Parser Unit Char :=
    (attempt do
      let _ ← string "''"
      pure '\'') <|>
    satisfy (fun c => c != '\'')

/-- Parse a string literal with leading whitespace. -/
def stringLit : Parser Unit String :=
  stringLitCore true

/-- Parse an integer literal in base 10. -/
def decimalInt : Parser Unit Int := do
  ws
  let neg ← Sift.optional (attempt do
    let _ ← char '-'
    let next ← peek
    match next with
    | some c => if c.isDigit then pure () else Parser.fail "expected digit after '-'"
    | none => Parser.fail "expected digit after '-'")
  let digits ← many1Chars digit
  let value := digits.toNat!
  pure (if neg.isSome then -value else value)

/-- Parse a radix integer literal (e.g. 16rFF). -/
def radixInt : Parser Unit Int := attempt do
  ws
  let neg ← Sift.optional (attempt do
    let _ ← char '-'
    let next ← peek
    match next with
    | some c => if c.isDigit then pure () else Parser.fail "expected digit after '-'"
    | none => Parser.fail "expected digit after '-'"
  )
  let baseDigits ← many1Chars digit
  let base := baseDigits.toNat!
  if base < 2 || base > 36 then
    Parser.fail "invalid radix"
  let _ ← satisfy fun c => c == 'r' || c == 'R'
  let digits ← many1Chars (satisfy isRadixDigit)
  let value ← match radixValue base digits with
    | some v => pure v
    | none => Parser.fail "invalid radix digit"
  let intVal : Int := Int.ofNat value
  pure (if neg.isSome then -intVal else intVal)
where
  isRadixDigit (c : Char) : Bool :=
    c.isDigit || ('a' ≤ c && c ≤ 'z') || ('A' ≤ c && c ≤ 'Z')

  digitValue (c : Char) : Option Nat :=
    if '0' ≤ c && c ≤ '9' then some (c.toNat - '0'.toNat)
    else if 'a' ≤ c && c ≤ 'z' then some (c.toNat - 'a'.toNat + 10)
    else if 'A' ≤ c && c ≤ 'Z' then some (c.toNat - 'A'.toNat + 10)
    else none

  radixValue (base : Nat) (digits : String) : Option Nat :=
    digits.toList.foldl
      (fun acc c =>
        match acc, digitValue c with
        | some a, some v => if v < base then some (a * base + v) else none
        | _, _ => none)
      (some 0)

/-- Parse an integer literal. -/
def intLit : Parser Unit Int :=
  radixInt <|> decimalInt

/-- Parse a float literal. -/
def floatLit : Parser Unit Float := attempt do
  ws
  let sign ← Sift.optional (satisfy fun c => c == '-')
  let intPart ← many1Chars digit
  let hasDot ← opt (attempt (char '.'))
  match hasDot with
  | some _ =>
      let fracPart ← many1Chars digit
      let expPart ← opt exponentPart
      let numStr :=
        (if sign.isSome then "-" else "") ++ intPart ++ "." ++ fracPart ++
        (match expPart with | some e => e | none => "")
      pure (parseFloatString numStr)
  | none =>
      let exp ← exponentPart
      let numStr := (if sign.isSome then "-" else "") ++ intPart ++ exp
      pure (parseFloatString numStr)
where
  exponentPart : Parser Unit String := do
    let _ ← satisfy fun c => c == 'e' || c == 'E'
    let sign ← Sift.optional (satisfy fun c => c == '+' || c == '-')
    let digits ← many1Chars digit
    pure ("e" ++ (match sign with | some c => c.toString | none => "") ++ digits)

  parseFloatString (s : String) : Float :=
    let negative := s.startsWith "-"
    let s' := if negative then s.drop 1 else s
    let (mantissa, expPart) := match s'.splitOn "e" with
      | [m, e] => (m, some e)
      | _ => match s'.splitOn "E" with
        | [m, e] => (m, some e)
        | _ => (s', none)
    let (intStr, fracStr) := match mantissa.splitOn "." with
      | [i, f] => (i, f)
      | [i] => (i, "")
      | _ => ("0", "0")
    let intVal := (if intStr.isEmpty then 0 else intStr.toNat!).toFloat
    let fracVal :=
      if fracStr.isEmpty then 0.0
      else fracStr.toNat!.toFloat / Float.pow 10.0 fracStr.length.toFloat
    let mantissaVal := intVal + fracVal
    let expVal : Int := match expPart with
      | some e => e.toInt!
      | none => 0
    let multiplier := if expVal >= 0
      then Float.pow 10.0 expVal.natAbs.toFloat
      else 1.0 / Float.pow 10.0 expVal.natAbs.toFloat
    let result := mantissaVal * multiplier
    if negative then -result else result

/-- Parse a scaled decimal literal (e.g., 1s2, 1.23s2). -/
def scaledLit : Parser Unit Literal := attempt do
  ws
  let neg ← Sift.optional (char '-')
  let intPart ← many1Chars digit
  let fracPart ← opt (attempt do
    let _ ← char '.'
    many1Chars digit)
  let _ ← satisfy fun c => c == 's' || c == 'S'
  let scaleDigits ← many1Chars digit
  let scale := scaleDigits.toNat!
  let mantissaDigits := intPart ++ (match fracPart with | some f => f | none => "")
  let mantissaNat := mantissaDigits.toNat!
  let mantissa : Int := Int.ofNat mantissaNat
  let mantissa := if neg.isSome then -mantissa else mantissa
  pure (Literal.scaled mantissa scale)

/-- Parse a character literal ($a). -/
def charLit : Parser Unit Char := do
  ws
  let _ ← char '$'
  anyChar

/-- Parse a symbol literal (#foo, #at:put:, #'+'). -/
def symbolLit : Parser Unit String := attempt do
  ws
  let _ ← char '#'
  let next ← peek
  match next with
  | some '\'' =>
      let s ← stringLitCore false
      pure s
  | some c =>
      if "+-*/\\=<>~&|,@%!?".contains c then
        let chars ← many1Chars (satisfy fun ch => "+-*/\\=<>~&|,@%!?".contains ch)
        pure chars
      else
        let name ← identRaw
        let next2 ← peek
        match next2 with
        | some ':' =>
            let _ ← char ':'
            let rest ← many (attempt do
              let part ← identRaw
              let _ ← char ':'
              pure (part ++ ":"))
            let selector := (name ++ ":") ++ rest.toList.foldl (· ++ ·) ""
            pure selector
        | _ => pure name
  | none => Parser.fail "unexpected end of input"

mutual
  /-- Parse a literal array (#(...)). -/
  partial def literalArray : Parser Unit Literal := attempt do
    ws
    let _ ← string "#("
    let elems ← many (attempt literal)
    ws
    let _ ← char ')'
    pure (Literal.array elems.toList)

  /-- Parse a literal dictionary (#{ key -> value . ... }). -/
  partial def literalDict : Parser Unit Literal := attempt do
    ws
    let _ ← string "#{"
    let entries ← seqLiteralAssoc
    ws
    let _ ← char '}'
    pure (Literal.dict entries)
  where
    literalAssoc : Parser Unit (Literal × Literal) := do
      let key ← literal
      symbol "->"
      let value ← literal
      pure (key, value)

    seqLiteralAssoc : Parser Unit (List (Literal × Literal)) := do
      let first? ← opt literalAssoc
      match first? with
      | none => pure []
      | some first =>
        let rest ← many (attempt do
          symbol "."
          literalAssoc)
        let _ ← opt (attempt (symbol "."))
        pure (first :: rest.toList)

  /-- Parse a byte array (#[1 2 3]). -/
  partial def byteArray : Parser Unit Literal := attempt do
    ws
    let _ ← string "#["
    let elems ← many (attempt do
      let n ← intLit
      if n < 0 then
        Parser.fail "byte array element must be non-negative"
      let natVal := n.natAbs
      if natVal > 255 then
        Parser.fail "byte array element out of range"
      pure (UInt8.ofNat natVal))
    ws
    let _ ← char ']'
    pure (Literal.byteArray elems.toList)

  /-- Parse a literal value. -/
  partial def literal : Parser Unit Literal := do
    (attempt literalArray) <|>
    (attempt literalDict) <|>
    (attempt byteArray) <|>
    (attempt do pure (Literal.symbol (← symbolLit))) <|>
    (attempt do pure (Literal.char (← charLit))) <|>
    (attempt scaledLit) <|>
    (attempt do pure (Literal.float (← floatLit))) <|>
    (attempt do pure (Literal.int (← intLit))) <|>
    (attempt do pure (Literal.str (← stringLit))) <|>
    (attempt do
      let name ← ident
      if name == "true" then
        pure (Literal.bool true)
      else if name == "false" then
        pure (Literal.bool false)
      else if name == "nil" then
        pure Literal.nil
      else
        Parser.fail "expected literal")
end

/-- Parse a block parameter list and temps. -/
def blockParamsTemps : Parser Unit (List Symbol × List Symbol) := do
  ws
  let next ← peek
  match next with
  | some ':' =>
      let params ← many1 (attempt do
        let _ ← char ':'
        let name ← identRaw
        ws
        pure name)
      symbol "|"
      let temps ← blockTemps
      pure (params.toList, temps)
  | _ =>
      let temps ← blockTemps
      pure ([], temps)
where
  blockTemps : Parser Unit (List Symbol) :=
    (attempt do
      symbol "|"
      let temps ← many (attempt ident)
      symbol "|"
      pure temps.toList) <|>
    pure []

/-- Apply a list of messages to a receiver. -/
def applyMessages (recv : Expr) (msgs : List Message) : Expr :=
  msgs.foldl (fun acc msg => Expr.send acc msg.1 msg.2) recv

mutual
  /-- Parse a keyword message (selector + args). -/
  partial def keywordMessage : Parser Unit Message := attempt do
    let firstPart ← keywordPart
    let firstArg ← binaryExpr
    let rest ← many (attempt do
      let part ← keywordPart
      let arg ← binaryExpr
      pure (part, arg))
    let selector := (firstPart :: rest.toList.map (fun p => p.1)).foldl (· ++ ·) ""
    let args := firstArg :: rest.toList.map (fun p => p.2)
    pure (selector, args)

  /-- Parse a unary message. -/
  partial def unaryMessage : Parser Unit Message := do
    let sel ← unarySelector
    pure (sel, [])

  /-- Parse a binary message. -/
  partial def binaryMessage : Parser Unit Message := do
    let sel ← binarySelector
    let arg ← unaryExpr
    pure (sel, [arg])

  /-- Parse a full expression. -/
  partial def expr : Parser Unit Expr := do
    assignment <|> cascadeExpr

  /-- Parse assignment: name := expr or name _ expr. -/
  partial def assignment : Parser Unit Expr := attempt do
    let name ← ident
    ws
    let _ ← (string ":=" <|> string "_")
    let value ← expr
    pure (Expr.assign name value)

  /-- Parse cascades. -/
  partial def cascadeExpr : Parser Unit Expr := do
    let (recv, msgs) ← messageChain
    let firstExpr := applyMessages recv msgs
    let rest ← many (attempt do
      symbol ";"
      let msg ← cascadeMessage
      pure msg)
    if rest.isEmpty then
      pure firstExpr
    else
      if msgs.isEmpty then
        Parser.fail "cascade requires a message"
      else
        let chains : List MessageChain :=
          (msgs :: rest.toList.map (fun m => [m]))
        pure (Expr.cascade recv chains)

  /-- Parse a message chain (unary/binary/keyword). -/
  partial def messageChain : Parser Unit (Expr × List Message) := do
    let recv ← primary
    let unarySels ← many (attempt unarySelector)
    let unaryMsgs := unarySels.toList.map (fun s => (s, []))
    let binaryMsgs ← many (attempt do
      let sel ← binarySelector
      let arg ← unaryExpr
      pure (sel, [arg]))
    let keywordMsg? ← opt keywordMessage
    let msgs := unaryMsgs ++ binaryMsgs.toList ++ keywordMsg?.toList
    pure (recv, msgs)

  /-- Parse a unary expression (primary + unary messages). -/
  partial def unaryExpr : Parser Unit Expr := do
    let recv ← primary
    let sels ← many (attempt unarySelector)
    pure (sels.foldl (fun acc sel => Expr.send acc sel []) recv)

  /-- Parse a binary expression (left associative). -/
  partial def binaryExpr : Parser Unit Expr := do
    let recv ← unaryExpr
    let steps ← many (attempt do
      let sel ← binarySelector
      let arg ← unaryExpr
      pure (sel, arg))
    pure (steps.foldl (fun acc step => Expr.send acc step.1 [step.2]) recv)

  /-- Parse primary expressions. -/
  partial def primary : Parser Unit Expr := do
    (attempt do pure (Expr.lit (← literal))) <|>
    (attempt arrayExpr) <|>
    (attempt blockExpr) <|>
    (attempt parenExpr) <|>
    (do
      let name ← ident
      pure (Expr.var name))

  /-- Parse dynamic array expressions: { expr . expr }. -/
  partial def arrayExpr : Parser Unit Expr := do
    symbol "{"
    let elems ← seqExprs
    symbol "}"
    pure (Expr.array elems)

  /-- Parse block expressions: [ :x :y | | t1 t2 | exprs ] -/
  partial def blockExpr : Parser Unit Expr := do
    symbol "["
    let (params, temps) ← blockParamsTemps
    let body ← seqStatements
    symbol "]"
    pure (Expr.block params temps body)

  /-- Parse parenthesized expressions. -/
  partial def parenExpr : Parser Unit Expr := do
    symbol "("
    let exprs ← seqStatements
    symbol ")"
    match exprs with
    | [] => Parser.fail "empty parentheses"
    | [e] => pure e
    | es => pure (Expr.seq es)

  /-- Parse a statement (return or expression). -/
  partial def statement : Parser Unit Expr := do
    (attempt do
      symbol "^"
      let value ← expr
      pure (Expr.return value)) <|>
    expr

  /-- Parse a sequence of expressions separated by periods. -/
  partial def seqExprs : Parser Unit (List Expr) := do
    let first? ← opt expr
    match first? with
    | none => pure []
    | some first =>
      let rest ← many (attempt do
        symbol "."
        expr)
      let _ ← opt (attempt (symbol "."))
      pure (first :: rest.toList)

  /-- Parse a sequence of statements separated by periods. -/
  partial def seqStatements : Parser Unit (List Expr) := do
    let first? ← opt statement
    match first? with
    | none => pure []
    | some first =>
      let rest ← many (attempt do
        symbol "."
        statement)
      let _ ← opt (attempt (symbol "."))
      pure (first :: rest.toList)

  /-- Parse a sequence of statements until the end parser matches (end is not consumed). -/
  partial def seqStatementsUntil (endp : Parser Unit Unit) : Parser Unit (List Expr) := do
    let endAhead ← opt (attempt (lookAhead endp))
    match endAhead with
    | some _ => pure []
    | none =>
        let first ← statement
        let rest ← many (attempt do
          symbol "."
          let stopAhead ← opt (attempt (lookAhead endp))
          match stopAhead with
          | some _ => pure none
          | none => some <$> statement)
        let tail := rest.toList.filterMap (fun e => e)
        pure (first :: tail)

  /-- Parse a cascade message (unary/binary/keyword). -/
  partial def cascadeMessage : Parser Unit Message :=
    (attempt keywordMessage) <|>
    (attempt binaryMessage) <|>
    unaryMessage
end

/-- Parse a method header (selector + parameters). -/
def methodHeader : Parser Unit (Symbol × List Symbol) :=
  (attempt keywordHeader) <|> (attempt binaryHeader) <|> unaryHeader
where
  unaryHeader : Parser Unit (Symbol × List Symbol) := do
    let sel ← unarySelector
    pure (sel, [])

  binaryHeader : Parser Unit (Symbol × List Symbol) := do
    let sel ← binarySelector
    let name ← ident
    pure (sel, [name])

  keywordHeader : Parser Unit (Symbol × List Symbol) := do
    let firstPart ← keywordPart
    let firstParam ← ident
    let rest ← many (attempt do
      let part ← keywordPart
      let param ← ident
      pure (part, param))
    let selector := (firstPart :: rest.toList.map (fun p => p.1)).foldl (· ++ ·) ""
    let params := firstParam :: rest.toList.map (fun p => p.2)
    pure (selector, params)

/-- Parse method temporary variables. -/
def methodTemps : Parser Unit (List Symbol) :=
  (attempt do
    symbol "|"
    let temps ← many (attempt ident)
    symbol "|"
    pure temps.toList) <|>
  pure []

/-- Parse class instance variables. -/
def classIvars : Parser Unit (List Symbol) :=
  (attempt do
    symbol "|"
    let ivars ← many (attempt ident)
    symbol "|"
    pure ivars.toList) <|>
  pure []

/-- Parse a method pragma (e.g., <primitive: 1>). -/
def pragma : Parser Unit Pragma := attempt do
  symbol "<"
  let (selector, args) ← (attempt keywordPragma <|> unaryPragma)
  symbol ">"
  pure { selector := selector, args := args }
where
  unaryPragma : Parser Unit (Symbol × List Literal) := do
    let sel ← unarySelector
    pure (sel, [])

  keywordPragma : Parser Unit (Symbol × List Literal) := do
    let firstPart ← keywordPart
    let firstArg ← literal
    let rest ← many (attempt do
      let part ← keywordPart
      let arg ← literal
      pure (part, arg))
    let selector := (firstPart :: rest.toList.map (fun p => p.1)).foldl (· ++ ·) ""
    let args := firstArg :: rest.toList.map (fun p => p.2)
    pure (selector, args)

/-- End of input. -/
def eof : Parser Unit Unit := do
  if ← atEnd then pure ()
  else Parser.fail "expected end of input"

/-- Parse a method definition (selector + temps + body). -/
def methodParser : Parser Unit Method := do
  ws
  let (selector, params) ← methodHeader
  let temps ← methodTemps
  let pragmas ← many (attempt pragma)
  let body ← seqStatements
  ws
  eof
  pure { selector := selector, params := params, temps := temps, pragmas := pragmas.toList, body := body }

/-- Parse a method definition inside a class (terminated by ! or end). -/
def methodInClass : Parser Unit Method := do
  ws
  let (selector, params) ← methodHeader
  let temps ← methodTemps
  let pragmas ← many (attempt pragma)
  let body ← seqStatementsUntil (symbol "!" <|> symbol "end")
  ws
  pure { selector := selector, params := params, temps := temps, pragmas := pragmas.toList, body := body }

/-- Parse zero or more class methods until stop parser matches. -/
partial def classMethodsUntil (stop : Parser Unit Unit) : Parser Unit (List Method) := do
  let stopAhead ← opt (attempt (lookAhead stop))
  match stopAhead with
  | some _ => pure []
  | none =>
      let m ← methodInClass
      let _ ← opt (attempt (symbol "!"))
      let rest ← classMethodsUntil stop
      pure (m :: rest)

/-- Parse a class definition. -/
def classDef : Parser Unit ClassDef := do
  ws
  symbol "class"
  let name ← ident
  let super ← opt (attempt do
    symbol "<"
    ident)
  let ivars ← classIvars
  let methods ← classMethodsUntil (attempt (symbol "class") <|> symbol "end")
  let classMethods ←
    (attempt do
      symbol "class"
      classMethodsUntil (symbol "end")) <|>
    pure []
  symbol "end"
  pure { name := name, super := super, ivars := ivars, methods := methods, classMethods := classMethods }

/-- Parse Smalltalk source into a program (class definitions + expressions). -/
def programParser : Parser Unit Program := do
  ws
  let classes ← many (attempt classDef)
  let exprs ← seqStatements
  ws
  eof
  pure { classes := classes.toList, main := exprs }

/-- Parse Smalltalk source into a program. -/
def parse (input : String) : Except ParseError Program :=
  match Parser.run programParser input with
  | .ok program => .ok program
  | .error err => .error { message := s!"Parse error at {err.pos.line}:{err.pos.column}: {err.message}" }

/-- Parse a single expression. -/
def parseExpr (input : String) : Except ParseError Expr :=
  let parser : Parser Unit Expr := do
    ws
    let e ← expr
    ws
    eof
    pure e
  match Parser.run parser input with
  | .ok e => .ok e
  | .error err => .error { message := s!"Parse error at {err.pos.line}:{err.pos.column}: {err.message}" }

/-- Parse a method definition. -/
def parseMethod (input : String) : Except ParseError Method :=
  match Parser.run methodParser input with
  | .ok m => .ok m
  | .error err => .error { message := s!"Parse error at {err.pos.line}:{err.pos.column}: {err.message}" }

end Smalltalk

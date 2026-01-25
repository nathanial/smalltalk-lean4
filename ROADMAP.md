# Smalltalk Interpreter Roadmap

A Smalltalk interpreter implemented in Lean 4.

## Current Status

### Completed

**Parser & Lexer** (fully functional, 36 tests passing)
- Literals: integers (decimal/radix), floats, scaled decimals, strings, characters, symbols, arrays, dictionaries, byte arrays, booleans, nil
- Message sends: unary, binary, keyword with correct precedence
- Cascades with chained messages
- Blocks with parameters and temporaries
- Variable references and assignment
- Return statements
- Method definitions with pragmas
- Comments

**Runtime Infrastructure**
- AST definitions for all constructs
- Value types: int, str, bool, nil, object
- Environment structure for variable bindings

### Not Yet Implemented

- Expression evaluation
- Message dispatch
- Method lookup and invocation
- Object creation and state
- Block closures
- Standard library

---

## Phase 1: Core Evaluator

Implement basic expression evaluation in `Eval.lean`.

- [ ] Literal evaluation (integers, floats, strings, booleans, nil)
- [ ] Variable lookup from environment
- [ ] Variable assignment
- [ ] Sequence evaluation (statement lists)
- [ ] Parenthesized expressions
- [ ] Array and dictionary literal construction

**Tests:** Evaluate literals, assign/read variables, run sequences.

---

## Phase 2: Primitive Operations

Add built-in operations for core types.

- [ ] Integer arithmetic: `+`, `-`, `*`, `/`, `//`, `\\`, `negated`, `abs`
- [ ] Integer comparison: `<`, `>`, `<=`, `>=`, `=`, `~=`
- [ ] Float operations: same as integer plus `sqrt`, `sin`, `cos`
- [ ] String operations: `,` (concatenation), `size`, `at:`
- [ ] Boolean operations: `&`, `|`, `not`
- [ ] Object identity: `==`, `~~`

**Implementation:** Primitive dispatch table keyed by receiver type + selector.

---

## Phase 3: Message Dispatch

Implement the message send protocol.

- [ ] Unary message sends
- [ ] Binary message sends
- [ ] Keyword message sends
- [ ] Cascade evaluation
- [ ] `self` keyword (add to parser)
- [ ] Primitive method lookup

**Tests:** Message chains, cascades, self-sends.

---

## Phase 4: Object System

Implement classes and object creation.

- [ ] Class definitions with instance variables
- [ ] Object instantiation (`new`, `basicNew`)
- [ ] Instance variable access and storage
- [ ] Method dictionaries per class
- [ ] Method lookup with inheritance
- [ ] `super` keyword (add to parser)

**Core Classes:**
- `Object` - root class
- `UndefinedObject` - nil's class
- `Boolean`, `True`, `False`

---

## Phase 5: Blocks and Control Flow

Implement block closures and evaluation.

- [ ] Block closure creation (capture environment)
- [ ] `value`, `value:`, `value:value:` evaluation
- [ ] Block temporary variable scoping
- [ ] Non-local returns (`^` inside blocks)
- [ ] `ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:`
- [ ] `whileTrue:`, `whileFalse:`
- [ ] `timesRepeat:`

---

## Phase 6: Standard Library

Build out core collection and utility classes.

**Numeric:**
- `Integer`, `Float`, `Number`
- `SmallInteger`, `LargeInteger` (optional)

**Collections:**
- `Collection`, `SequenceableCollection`
- `Array`, `ByteArray`
- `String`, `Symbol`
- `Dictionary`, `Set`
- Iteration: `do:`, `select:`, `collect:`, `inject:into:`

**Streams:**
- `Stream`, `ReadStream`, `WriteStream`

---

## Future Enhancements

- **Reflection:** `class`, `respondsTo:`, method listing
- **Exceptions:** `on:do:`, `signal`, `ensure:`
- **Image persistence:** Save/restore interpreter state
- **Debugging:** Stack traces, breakpoints
- **Foreign function interface:** Call Lean from Smalltalk
- **Compiler optimizations:** Inline primitives, bytecode

---

## Architecture Notes

**Key Files:**
- `Smalltalk/AST.lean` - Syntax tree definitions
- `Smalltalk/Parse.lean` - Lexer and parser
- `Smalltalk/Runtime.lean` - Values and environment
- `Smalltalk/Eval.lean` - Evaluator (to be implemented)
- `Tests/` - Test suite using Crucible

**Design Decisions:**
- Tree-walking interpreter (no bytecode initially)
- Smalltalk-80 semantics where applicable
- Pragmas for primitive method binding

# Smalltalk

A Smalltalk interpreter in Lean 4 (WIP). This project provides the core AST and runtime scaffolding plus a test harness to grow the parser and evaluator.

## Status

- AST and runtime types
- Parser and evaluator stubs
- Crucible test suite

## Build & Test

```bash
cd util/smalltalk
lake build
lake test
```

## CLI

```bash
lake run smalltalk_app -- [--load image] [--save image] [--stdlib path] [--no-stdlib] <source-file>
```

- `--load image` loads an image (serialized interpreter state) before evaluation.
- `--save image` writes the updated interpreter state after evaluation.
- `--stdlib path` loads a Smalltalk standard library file before the source file.
- `--no-stdlib` disables loading the default `Stdlib.st`.

## Layout

```
Smalltalk/     # Core library modules
Smalltalk.lean # Public entry point
Tests/         # Crucible tests
```

## Class Syntax (Current)

```smalltalk
class Point < Object | x y |
  x
    ^ x.
!
class
  origin
    ^ self new.
!
end
```

Methods are separated by `!`. A class body ends with `end`. The parser currently treats
`class`/`end` as reserved words inside class bodies. `!` is reserved as a method delimiter
and is not available as a binary selector in expressions.

Class-side methods are defined inside a `class` section within the class body.

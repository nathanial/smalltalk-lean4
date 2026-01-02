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

## Layout

```
Smalltalk/     # Core library modules
Smalltalk.lean # Public entry point
Tests/         # Crucible tests
```

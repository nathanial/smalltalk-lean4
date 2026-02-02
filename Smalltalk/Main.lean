import Smalltalk

open Smalltalk

namespace Smalltalk.App

structure CliArgs where
  source : System.FilePath
  load? : Option System.FilePath := none
  save? : Option System.FilePath := none
  stdlib? : Option System.FilePath := none
  noStdlib : Bool := false

def usage : String :=
  "Usage: smalltalk_app [--load image] [--save image] [--stdlib path] [--no-stdlib] <source-file>"

def parseArgs (args : List String) : Except String CliArgs :=
  let rec loop (args : List String) (load? save? stdlib? : Option System.FilePath)
      (noStdlib : Bool)
      (source? : Option System.FilePath) : Except String CliArgs :=
    match args with
    | [] =>
        match source? with
        | some source =>
            .ok { source := source, load? := load?, save? := save?, stdlib? := stdlib?, noStdlib := noStdlib }
        | none => .error "missing source file"
    | ["--load"] => .error "missing value for --load"
    | "--load" :: path :: rest =>
        if load?.isSome then
          .error "duplicate --load flag"
        else
          loop rest (some (System.FilePath.mk path)) save? stdlib? noStdlib source?
    | ["--save"] => .error "missing value for --save"
    | "--save" :: path :: rest =>
        if save?.isSome then
          .error "duplicate --save flag"
        else
          loop rest load? (some (System.FilePath.mk path)) stdlib? noStdlib source?
    | ["--stdlib"] => .error "missing value for --stdlib"
    | "--stdlib" :: path :: rest =>
        if stdlib?.isSome then
          .error "duplicate --stdlib flag"
        else
          loop rest load? save? (some (System.FilePath.mk path)) noStdlib source?
    | "--no-stdlib" :: rest =>
        if noStdlib then
          .error "duplicate --no-stdlib flag"
        else
          loop rest load? save? stdlib? true source?
    | "--help" :: _ => .error usage
    | "-h" :: _ => .error usage
    | flag :: rest =>
        if flag.startsWith "-" then
          .error s!"unknown flag: {flag}"
        else
          match source? with
          | none => loop rest load? save? stdlib? noStdlib (some (System.FilePath.mk flag))
          | some _ => .error s!"unexpected argument: {flag}"
  loop args none none none false none

def withProgramClasses (state : ExecState) (program : Program) : ExecState :=
  let userRegistry := buildRegistry program.classes
  let baseRegistry := if state.classes.isEmpty then buildRegistry coreClasses else state.classes
  { state with classes := userRegistry ++ baseRegistry }

def run (cfg : CliArgs) : IO UInt32 := do
  let source ← IO.FS.readFile cfg.source
  let stdlibPath? :=
    if cfg.noStdlib then
      none
    else
      match cfg.stdlib? with
      | some path => some path
      | none =>
          if cfg.load?.isSome then none else some Smalltalk.Stdlib.defaultPath
  let stdlibProgram? ←
    match stdlibPath? with
    | none => pure (.ok none)
    | some path =>
        if cfg.stdlib?.isSome then
          match (← Smalltalk.Stdlib.parseFromFile path) with
          | .ok program => pure (.ok (some program))
          | .error err => pure (.error err)
        else
          Smalltalk.Stdlib.loadIfExists path
  match stdlibProgram? with
  | .error err =>
      IO.eprintln s!"stdlib error: {err}"
      return 1
  | .ok stdlibProgramOpt =>
  match Smalltalk.parseProgram source with
  | .error e =>
      IO.eprintln s!"parse error: {e.message}"
      return 1
  | .ok program =>
      let program := match stdlibProgramOpt with
        | none => program
        | some stdlibProgram => Smalltalk.Stdlib.mergePrograms stdlibProgram program
      let baseState ←
        match cfg.load? with
        | none => pure ({ } : ExecState)
        | some path =>
            match (← Smalltalk.Image.load path) with
            | .ok state => pure state
            | .error err =>
                IO.eprintln s!"load error: {err}"
                return 1
      let startState := withProgramClasses baseState program
      match Smalltalk.evalSeq startState program.main with
      | .ok (endState, value) =>
          IO.println (reprStr value)
          match cfg.save? with
          | none => return 0
          | some path =>
              match (← Smalltalk.Image.save path endState) with
              | .ok _ => return 0
              | .error err =>
                  IO.eprintln s!"save error: {err}"
                  return 1
      | .error e =>
          IO.eprintln s!"eval error: {e.message}"
          return 1

end Smalltalk.App

def main : IO UInt32 := do
  let args ← IO.getArgs
  match Smalltalk.App.parseArgs args.toList with
  | .ok cfg => Smalltalk.App.run cfg
  | .error msg =>
      IO.eprintln msg
      IO.eprintln Smalltalk.App.usage
      return 1

open Comptypes
open Clvm
open Compiler
open Frontend
open Codegen
open Runtypes

let emptyOpts =
  { includeDirs = []
  ; filename = "test.clvm"
  ; assemble = true
  ; stdenv = true
  ; inDefun = false
  ; startEnv = None
  ; compiler = None

  ; readNewFile =
      (fun _ _ name ->
         if name == "*macros*" then
           CompileOk (name, String.concat "\n" Macros.macros)
         else
           CompileError (Srcloc.start name, "include unimplemented for name " ^ name)
      )

  ; compileProgram =
      (fun opts program ->
         frontend opts [program]
         |> compBind (fun m -> codegen opts m)
      )
  }

let emptyCompile = { emptyOpts with assemble = true }

module type TestSpecT = sig
  type r
  type t

  val get_expected : t -> r
  val get_result : t -> r
  val show_failure : t -> r -> unit
end

module CompileSpecT = struct
  type r = string compileResult
  type t =
    { expected : r
    ; opts : compilerOpts
    ; input : string
    }

  let get_expected e = e.expected
  let show_failure e cr =
    Js.log "\nunexpected output:" ;
    Js.log cr ;
    Js.log "for input:" ;
    Js.log e.input ;
    Js.log "wanted:" ;
    Js.log e.expected
  let get_result e = compile_file e.opts e.input
end

module RunSpecT = struct
  type r = string runResult
  type t =
    { expected : string runResult
    ; input : string
    ; args : string
    }

  let get_expected e = e.expected
  let show_failure e cr =
    Js.log "\nunexpected output:" ;
    Js.log cr ;
    Js.log "for input:" ;
    Js.log @@ e.input ^ " on " ^ e.args ;
    Js.log "wanted:" ;
    Js.log e.expected

  let get_result e =
    parse_and_run "*clvm*" e.input e.args |> runMap Sexp.to_string
end

module FullSpecT = struct
  type r = string runResult
  type t =
    { expected : string runResult
    ; input : string
    ; opts : compilerOpts
    ; args : string
    }

  let get_expected e = e.expected
  let show_failure e cr =
    Js.log "\nunexpected output:" ;
    Js.log cr ;
    Js.log "for input: " ;
    Js.log e.input ;
    Js.log "with args: " ;
    Js.log e.args ;
    Js.log "wanted:" ;
    Js.log e.expected
  let get_result e =
    match compile_file e.opts e.input with
    | CompileOk compiled ->
      parse_and_run "*clvm*" compiled e.args |> runMap Sexp.to_string
    | CompileError (l,e) -> RunError (l,"compile: " ^ e)
end

module RunTest(Test : TestSpecT) = struct
  type t = Test.t
  let run failed i (e : t) =
    let cr = Test.get_result e in
    if cr <> Test.get_expected e then
      begin
        failed := true ;
        Test.show_failure e cr
      end
    else
      Printf.printf "\r%s\r" (string_of_int i)
end

module RunExecTest = RunTest(RunSpecT)
module RunCompileTest = RunTest(CompileSpecT)
module RunFullTest = RunTest(FullSpecT)

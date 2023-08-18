open Gillian
open Instantiation
module SMemory = Gillian.Monadic.MonadicSMemory.Lift (MonadicSMemory)

(*
module Lifter (Verification : Gillian.Abstraction.Verifier.S) =
  Gillian.Debugger.Lifter.Gil_lifter.Make (Verification) (SMemory)
    (ParserAndCompiler)
*)

module Lifter 
    (Verification : Gillian.Abstraction.Verifier.S
                      with type annot = ParserAndCompiler.Annot.t) =
struct
  include
    Gillian.Debugger.Lifter.Gil_lifter.Make (ParserAndCompiler) (Verification)
      (SMemory)

  let add_varables = MonadicSMemory.Lift.add_variables
end

(* For now, the parserandcompiler and external.m interfaces do not match *)
module CLI =
  Gillian.Command_line.Make (Gillian.General.Init_data.Dummy) (Cmemory) (SMemory)
    (ParserAndCompiler) 
    (External.M)
    (struct
      let runners = []
    end)
    (Lifter)

let () = CLI.main ()

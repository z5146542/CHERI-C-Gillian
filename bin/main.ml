open Gillian
open Instantiation
module CLI =
  Gillian.CommandLine.Make (Cmemory) (Symbolic.Dummy_memory) (General.External.Dummy)
    (ParserAndCompiler)
    (struct
      let runners = []
    end)
    (Debugger.Gil_to_tl_lifter.Default (Gillian.Symbolic.Dummy_memory) (ParserAndCompiler))

let () = CLI.main ()

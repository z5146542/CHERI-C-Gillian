module Annot = Gillian.Gil_syntax.Annot.Basic
module Gil_parsing = Gil_parsing.Make(Annot)
type init_data = unit
type tl_ast = unit

(* Because the compiler is a foreign program, we need to run externally *)
let run cmd =
  let inp = Unix.open_process_in cmd in
  let try_read () =
    try Some (input_line inp) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> acc ^ "\n" ^ (loop s)
   (*
    | Some s -> (match acc with
      | "" -> loop s
      | _  -> acc ^ "\n" ^ (loop s))
   *)
    | None   -> Unix.close_process_in inp; acc in
  let first = match try_read () with
    | Some s -> s ^ loop ""
    | None   -> Unix.close_process_in inp; "" in
  (* loop "" *)
  first

let includes = "import \"unops_common.gil\", \"binops_common.gil\", \"internals.gil\", \"cheri_purecap_compressed.gil\", \"cheri_purecap_uncompressed.gil\", \"stdlib_cap256.gil\", \"string_common.gil\", \"global.gil\";\n\n"
let esbmc_run = "bin/esbmc "
let cheri_settings = "--no-library --cheri purecap --cheri-uncompressed "
let sysroot = "--sysroot ~/Downloads/rootfs-mips64-purecap.cheribsd-headers "
let remove_misc = "tail -n +8 | grep -v \"^\\s\\s*$\" | grep -v \"^[ ]*skip;\" "

(* commands that add struct-related procedures *)
let parse_st_run = "ocaml -I +str str.cma bin/parseST.ml "

let l_emit_parse_tree = ref false
let l_emit_symbol_table = ref false
let l_warnings = ref false
let l_esbmc_args = ref []

module TargetLangOptions = struct
  open Cmdliner

  type t = {
    emit_parse_tree : bool;
    emit_symbol_table : bool;
    warnings : bool;
    esbmc_args : string list;
  }

  let term = 
    let docs = Manpage.s_common_options in
    
    let doc = "Write the parse tree emitted by ESBMC to a file." in
    let eptree = Arg.(value & flag & info [ "emit-ptree" ] ~docs ~doc) in

    let doc = "Write the symbol table emitted by ESBMC to a file." in
    let estable = Arg.(value & flag & info [ "emit-stable" ] ~docs ~doc) in

    let doc = "Silence ESBMC warnings." in
    let no_warnings = Arg.(value & flag & info [ "no-warnings" ] ~docs ~doc) in

    let doc = "Provide additional arguments to ESBMC." in
    let esbmc_arg = Arg.(value & opt_all string [] & info [ "esbmc-args" ] ~docs ~doc) in 

    

    let opt
        eptree
        estable
        no_warnings
        esbmc_arg =
      {
        emit_parse_tree = eptree;
        emit_symbol_table = estable;
        warnings = not no_warnings;
        esbmc_args = esbmc_arg;
      }
    in
    Term.(const opt $ eptree $ estable $ no_warnings $ esbmc_arg)
  let apply { emit_parse_tree; emit_symbol_table; warnings; esbmc_args; } = 
    l_emit_parse_tree := emit_parse_tree;
    l_emit_symbol_table := emit_symbol_table;
    l_warnings := warnings;
    l_esbmc_args := esbmc_args
  (*
      {
        emit_parse_tree;
        emit_symbol_table;
        warnings;
      } = 
  emit_parse_tree := emit_parse_tree;
  emit_symbol_table := emit_symbol_table;
  warnings := warnings
  *)
end

(*type tl_ast = unit ( * ESBMC (probs) does not emit an abstract syntax tree *)
type err = unit (* Deal with errors later *)

let pp_err _ _ =
  failwith
    "Please implement the compiling interface to use with the '-compile' \
     flag or test suites"
  
let parse_and_compile_files files =
  let open IncrementalAnalysis in
  let open Command_line.ParserAndCompiler in 
  let exec_mode = !Gillian.Utils.Config.current_exec_mode in
  let () =
    if !l_emit_parse_tree then
      let ept path =
      let pathptree = Filename.chop_extension path ^ ".c.ptree" in
      let oc = open_out pathptree in
      let output = run (esbmc_run ^ " --parse-tree-only " ^ path ^ " 2>&1") in
      let () = Printf.fprintf oc "%s" output in
      close_out oc in
      List.iter ept files
  in
  let () =
    if !l_emit_symbol_table then
      let est path =
      let pathstable = Filename.chop_extension path ^ ".c.stable" in
      let oc = open_out pathstable in
      let output = run (esbmc_run ^ " --symbol-table-only " ^ path) in
      let () = Printf.fprintf oc "%s" output in
      close_out oc in
      List.iter est files
  in
  let source_files = SourceFiles.make () in
  let files_string = String.concat " " files in
  let mainfile = run (esbmc_run ^ cheri_settings ^ sysroot ^ " --symbol-table-only " ^ files_string ^ " | grep -B 1 \"Base name\\.*: main\" | head -1 | sed \"s/Module\\.*:\ //g\"") in
  let is_mainpath file =
    try ignore (Str.search_forward (Str.regexp_string (mainfile ^ ".c")) file 0);
      true
    with
      Not_found -> false
  in
  let mainpath = List.find is_mainpath files in
  let pathgil = Filename.chop_extension mainpath ^ ".gil" in
  let oc = open_out pathgil in
    let pathtxt' = Filename.chop_extension mainpath ^ ".txt" in
    let pathtxt = "/tmp/" ^ Filename.basename pathtxt' in
    let output = includes ^
      (run ( esbmc_run ^ cheri_settings ^ sysroot
          ^ " --symbol-table-only " ^ files_string ^ " > " ^ pathtxt ^ "; " ^ parse_st_run
          ^ pathtxt ^  "; rm " ^ pathtxt  )) ^
      (run (esbmc_run ^ cheri_settings ^ sysroot
          ^ (if !l_esbmc_args = [] then "" else String.concat " " !l_esbmc_args)
          ^ " --goto-functions-only " ^ files_string ^ " | " ^ remove_misc ))
    in
  let () = Printf.fprintf oc "%s" output in
  let () = close_out oc in
  let trans_procs = (Gil_parsing.parse_eprog_from_string output).labeled_prog in
  let progs = [(pathgil, trans_procs)] in
    Ok ({ gil_progs = progs ; source_files; tl_ast = (); init_data = ()})

let other_imports = []
let env_var_import_path = Some "INSTANTIATION_RUNTIME_PATH"
let initialize _ = ()

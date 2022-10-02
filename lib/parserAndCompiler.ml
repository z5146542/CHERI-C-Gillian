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

let includes = "import \"unops_common.gil\", \"binops_common.gil\", \"internals.gil\", \"stdlib_cap256.gil\", \"cheri_purecap_uncompressed.gil\";\n\n"
let esbmc_run = "bin/esbmc "
let cheri_settings = "--no-library --cheri purecap --cheri-uncompressed "
let sysroot = "--sysroot ~/Downloads/rootfs-mips64-purecap.cheribsd-headers "
let remove_misc = "tail -n +8 | grep -v \"^\\s\\s*$\" | grep -v \"^\\ *skip;\" "

let l_emit_parse_tree = ref false
let l_emit_symbol_table = ref false
let l_warnings = ref false

module TargetLangOptions = struct
  open Cmdliner

  type t = {
    emit_parse_tree : bool;
    emit_symbol_table : bool;
    warnings : bool;
  }

  let term = 
    let docs = Manpage.s_common_options in

    let doc = "Write the parse tree emitted by ESBMC to a file." in
    let eptree = Arg.(value & flag & info [ "emit-ptree" ] ~docs ~doc) in

    let doc = "Write the symbol table emitted by ESBMC to a file." in
    let estable = Arg.(value & flag & info [ "emit-stable" ] ~docs ~doc) in

    let doc = "Silence ESBMC warnings." in
    let no_warnings = Arg.(value & flag & info [ "no-warnings" ] ~docs ~doc) in

    let opt
        eptree
        estable
        no_warnings =
      {
        emit_parse_tree = eptree;
        emit_symbol_table = estable;
        warnings = not no_warnings;
      }
    in
    Term.(const opt $ eptree $ estable $ no_warnings)
  let apply { emit_parse_tree; emit_symbol_table; warnings } = 
    l_emit_parse_tree := emit_parse_tree;
    l_emit_symbol_table := emit_symbol_table;
    l_warnings := warnings
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

type tl_ast = unit (* ESBMC (probs) does not emit an abstract syntax tree *)
type err = unit (* Deal with errors later *)

let pp_err _ _ =
  failwith
    "Please implement the compiling interface to use with the '-compile' \
     flag or test suites"

let parse_and_compile_file path =
  let () =
    if !l_emit_parse_tree then
      let pathptree = Filename.chop_extension path ^ ".c.ptree" in
      let oc = open_out pathptree in
      let output = run (esbmc_run ^ " --parse-tree-only " ^ path ^ " 2>&1") in
      let () = Printf.fprintf oc "%s" output in
      close_out oc
  in
  let () =
    if !l_emit_symbol_table then
      let pathstable = Filename.chop_extension path ^ ".c.stable" in
      let oc = open_out pathstable in
      let output = run (esbmc_run ^ " --symbol-table-only " ^ path) in
      let () = Printf.fprintf oc "%s" output in
      close_out oc
  in
  let pathgil = Filename.chop_extension path ^ ".gil" in
  (* let snd = Filename.chop_extension path ^ ".giltmp" in *)
  let oc = open_out pathgil in
  (* let od = open_out snd in *)
  let output = includes ^ (run (esbmc_run ^ cheri_settings ^ sysroot ^ "--goto-functions-only " ^ path ^ " | " ^ remove_misc)) in
  let () = Printf.fprintf oc "%s" output in
  (* let () = Printf.fprintf od "%s" output in *)
  let () = close_out oc in
  (* let () = close_out od in *)
  let trans_procs = Gil_parsing.parse_eprog_from_string output in
  (pathgil, trans_procs)
  

let parse_and_compile_files files =
  let open IncrementalAnalysis in
  let open CommandLine.ParserAndCompiler in 
  let exec_mode = !Gillian.Utils.Config.current_exec_mode in
  let source_files = SourceFiles.make () in
  let rec g filez = match filez with 
    | f :: fs -> SourceFiles.add_source_file source_files f; (parse_and_compile_file f) :: g fs
    | []      -> [] in
  let progs = g files in
  Ok ({ gil_progs = progs ; source_files; tl_ast = () })


let other_imports = []
let env_var_import_path = Some "INSTANTIATION_RUNTIME_PATH"
let initialize _ = ()

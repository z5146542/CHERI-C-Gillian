(* 
  README
  1. Compile: ocamlc str.cma -o parseST parseST.ml

  2. Generate symbol table: esbmc --symbol-table-only  [OPTIONS] sample.c >> sample.txt

  3. Run parser: ./parseST sample.txt

Note: The parser uses an extract.sh script for formatting the ESBMC output.
      It generates temp.txt and structDef.txt files. Make sure you don't have personal 
      files with these names in the folder.
*)

(* String helper functions *)
let contains str sub =
    let l1 = String.length str in
      let l2 = String.length sub in
        if l1 >= l2
          then  
            let re = Str.regexp_string sub in
              try ignore (Str.search_forward re str 0); true
              with Not_found -> false    
          else
            false
;;

let rec compare s1 s2 i =  (* compare first i characters *)
  if i < 0
    then true
    else
      if Char.equal (String.get s1 i) (String.get s2 i)
        then
          compare s1 s2 (i-1)
        else
          false
;;

let starts_with ~prefix (str:string) =
  let lp = String.length prefix in
    let ls = String.length str in
      if lp > ls 
        then
          false
        else
          compare prefix str (lp-1)
;;

let rec extract_from_1 str idx ch value =
  if idx = String.length str
    then "" (* Not found *)
    else 
      let c = String.get str idx in
        if Char.equal c ch
          then value ^ (Char.escaped c)
          else extract_from_1 str (idx+1) ch (value ^ Char.escaped c)
;;

let extract_from str idx ch = (* extract a substring from str starting at upto the first occurrence of ch *)
  extract_from_1 str idx ch ""
;;
(* End of string helper functions *)

let rec get_no_bits t i =
  if Char.equal (String.get t i) '('
    then 
      let fi = String.index_from t i ')' in
        match int_of_string_opt (String.sub t (i+1) (fi-i-1)) with
            None -> 0
          | Some n -> n
    else get_no_bits t (i+1)  (* recurse till we get '(' *)
;;

let sizeof (t:string) =
  match t with 
     "unsigned char"      -> 8
   | "signed char"        -> 8
   | "unsigned short int" -> 16
   | "signed short int"   -> 16
   | "unsigned int"       -> 32
   | "signed int"         -> 32
   | "unsigned long int"  -> 64
   | "signed long int"    -> 64
   | _                    -> if contains t "CAP$"
                               then 256
                               else if contains t "_ExtInt"
                                      then get_no_bits t 0
                                      else 1000

let compact_ty (t:string) = (* for proc generation *)
  match t with 
     "unsigned char"      -> "uint8"
   | "signed char"        -> "int8"
   | "unsigned short int" -> "uint16"
   | "signed short int"   -> "int16"
   | "unsigned int"       -> "uint32"
   | "signed int"         -> "int32"
   | "unsigned long int"  -> "uint64"
   | "signed long int"    -> "int64"
   | _                    -> if contains t "CAP$"
                               then "cap"
                               else "(compact_ty) err"

(* Symbol Table Structure *)
type tyField = 
    PField of string * string * int * int (* type, field name, sizeof type, field offset *)
  | SField of tyTree
and tyTree = 
    NilStruct
  | Struct of string * tyField list * int (*struct name, Fields, sizeof struct *) (* check for empty list *)

let getFldType (fld:tyField) =
  match fld with
     PField(t, _, _, _) -> t
   | _ -> "struct"
;;

let getFldName (fld:tyField) =
  match fld with
     PField(_, n, _, _) -> n
   | _ -> "struct name"
;;

let getFldSize (fld:tyField) =
  match fld with
     PField(_, _, n, _) -> n
   | _ -> 0
;;

let getFldOffset (fld:tyField) =
  match fld with
     PField(_, _, _, n) -> n
   | _ -> 0
;;

(* Pretty print tyTree *)
let rec print_fields (fields:tyField list) =
  match fields with
      [] -> ()
    | PField(ty,fn,sz,off)::tl -> begin
                                    Printf.printf " %s %s %d %d;\n" ty fn sz off;
                                    print_fields tl
                                  end
    | _ -> print_endline "unknown field"; 
;;

let print_struct (symTab:tyTree) = 
  match symTab with
      NilStruct         -> ()
    | Struct(sname, fl, fsz) -> begin
                                  Printf.printf "struct %s {\n" sname;
                                  print_fields fl;
                                  Printf.printf "}\n\n"
                                end
;;

let keywords = ["unsigned"; "signed"; "short"; "long"; "char"; "int"; "_ExtInt"; "CAP$"] (* TODO add struct *)
;;

let fname = ref "";;

let rec get_bits str i = (* str is expected to be of "(xx)yyy" format *)
  if Char.equal (String.get str i) ')'
    then
      String.sub str 0 (i+1)
    else
      get_bits str (i+1)
;;

let rec get_field_type (str:string) (subList:string list) =
  let len = String.length str in
    match subList with
        [] -> fname := str; "" (* str will contain field name *)
      (* check whether str has 'CAP$' -> string upto CAP$ will be field type, rest is field name *)
      | h::t when contains str "CAP$"                         -> let ftype = extract_from str 0 '$' in
                                                                   let tylen = String.length ftype in
                                                                     begin
                                                                       fname := String.sub str (tylen+1) (len - (tylen+1));
                                                                       ftype
                                                                     end                   
      | "unsigned"::t when starts_with ~prefix:"unsigned" str -> "unsigned" ^ (get_field_type (String.sub str 9 (len - 9) ) t )
      | "unsigned"::t                                         -> (get_field_type str t )
      | "signed"::t when starts_with ~prefix:"signed" str     -> "signed" ^ (get_field_type (String.sub str 7 (len - 7) ) t )
      | "signed"::t                                           -> (get_field_type str t )
      | "short"::t when starts_with ~prefix:"short" str       -> " short"    ^ (get_field_type (String.sub str 6 (len - 6) ) t )
      | "short"::t                                            -> (get_field_type str t )
      | "long"::t when starts_with ~prefix:"long" str         -> " long"     ^ (get_field_type (String.sub str 5 (len - 5) ) t )
      | "long"::t                                             -> (get_field_type str t )
      | "char"::t when starts_with ~prefix:"char" str         -> begin 
                                                                   fname := (String.sub str 5 (len -5)); 
                                                                   " char" 
                                                                 end
      | "char"::t                                             -> (get_field_type str t )
      | "int"::t when starts_with ~prefix:"int" str           -> begin 
                                                                   fname := (String.sub str 4 (len - 4));
                                                                   " int"
                                                                 end
      | "int"::t                                              -> (get_field_type str t )
      | "_ExtInt"::t when starts_with ~prefix:"_ExtInt" str   -> let bits = get_bits (String.sub str 7 (len - 7)) 0 in
                                                                   let bl = String.length bits in
                                                                     begin                                                                 
                                                                       fname := (String.sub str (8 + bl) (len - (8+bl)));
                                                                       " _ExtInt" ^ bits
                                                                     end
      | "CAP$"::t                                             -> (get_field_type str t )
      | h::t          -> "FIELD TYPE ERROR" (* TODO handle this *)
;;

let filename = Sys.argv.(1)

let cmd = "./extractStructs.sh " ^ Sys.argv.(1)
;;

let read_line chnl = 
  let line = try input_line chnl 
             with End_of_file -> begin
                                   print_endline "I'm quitting";
                                   exit 0 (* TODO a better handling? *)
                                 end
    in line
;;

let get_struct_name line:string =
  let len = String.length line in
    let name = String.sub line 7 (len-8) in (* String length "struct " = 7 *)
      name
;;

(* field list, field type, field name, field offset *)
let rec insert_field (field:tyField list) (ty:string) (fn:string) (off:int) = 
  match field with
       [] -> let n = sizeof ty in
               ([PField(ty, fn, n, off)], n) (* field, size *)
     | hd::tl -> let fld = insert_field tl ty fn off  in (* return pair of field list and size *)
                   match fld with
                       ([], sz)  -> ( [], getFldSize hd ) (* sz should be 0 *)
                     | (lst, sz) -> let hsz = getFldSize hd in
                                      (hd::lst, sz + hsz)
(*                     | _         -> ([], 0) (* error *) *)
;;

let rec populate_fields (bStruct:tyTree) chnl = 
  let line = input_line chnl in
    if contains line "__ENDSTRUCT__"
    then
      begin 
        fname:= ""; (* reset *)
        bStruct
      end
    else
      let typeFlist = get_field_type line keywords in (* line: a field declaration *) (* typeF: string list *)
        match bStruct with
            Struct(s, f, n) -> let fld = insert_field f typeFlist !fname n in (* current size of struct becomes offset for next field *)
                                 begin
                                   match fld with
                                     (fl, fsz) -> populate_fields (Struct(s, fl, fsz)) chnl (* f:tyField list *)
                                 end
          | _               -> begin 
                                 fname := "";
                                 print_endline "Invalid struct";
                                 NilStruct
                               end (* TODO ensure correct handling *)
;;

let rec populate_struct (symTable:tyTree list) (line:string) chnl =
  if contains line "__STRUCT__"
    then
      let nLine = read_line chnl in (* struct name is in next line *)
        let sName = get_struct_name nLine in
          let f = populate_fields (Struct(sName, [], 0)) chnl in
            populate_ST ( f::symTable ) chnl (* update the symbol table and continue populating the table *)
    else
      symTable
and populate_ST (symTable:tyTree list) chnl =
    let line = try input_line chnl with End_of_file -> "" in
      populate_struct symTable line chnl
;;

let rec print_ST (symTable:tyTree list) = 
  match symTable with
      [] -> print_endline "<< EOS >>"
    | hd::tl -> begin
                  print_struct hd;
                  print_ST tl
                end
;;

let gen_struct_sizeof_proc name size =
  begin
    Printf.printf "proc %s_sizeof (){\n" name ;
    Printf.printf "    ret := {{ \"uint64\", %di }};\n" (size/8);
    Printf.printf "    return\n};\n\n";
    ()
  end
;;

let gen_struct_declare_proc name fls = 
  let undefs = String.concat ", " (List.init fls (fun _ -> "undefined")) in
  begin
    Printf.printf "proc %s_declare (){ \n" name;
    Printf.printf "    ret := {{ %s }};\n" undefs;
    Printf.printf "    return\n};\n\n";
    ()
  end
;;

let gen_struct_declare_cap_proc name size =
  begin
    Printf.printf "proc %s_declare_cap (){ \n" name;
    Printf.printf "   len :=  \"i__length\"(%di);\n" (size/8);
    Printf.printf "   ret := [alloc]({{ \"uint64\", len }});\n";
    Printf.printf "   return\n};\n\n";
    ()
   end
;;

let gen_get_field_proc sn fn i = (* struct name, field name, field index *)
  begin
    Printf.printf "proc %s_get_%s (val) {\n" sn fn;
    Printf.printf "    ret := l-nth(val, %d.);\n" i;
    Printf.printf "    return\n};\n\n";
    ()
  end
;;

let rec print_set_entries arg1 arg2 i flLen idx = (* arg1, arg2, field index original field list length, iteration index *)
  if idx = flLen (* done with all fields *)
    then 
      ignore (Printf.printf " }};\n";) (* Printf.printf "%d %d}};\n" idx flLen *)
    else
      if i = idx (* field index = iteration index *)
        then
          begin
            if idx = (flLen-1)
              then begin Printf.printf "%s" arg2 end
              else begin Printf.printf "%s, " arg2 end;
            print_set_entries arg1 arg2 i flLen (idx+1)
          end
        else
          begin
            if idx = (flLen-1)
              then begin Printf.printf "l-nth(%s, %d.)" arg1 idx end
              else begin Printf.printf "l-nth(%s, %d.), " arg1 idx end;
            print_set_entries arg1 arg2 i flLen (idx+1)
          end
;;

let gen_set_field_proc sn fn i flLen = (* struct name, field name, field index, original field list length *)
  let v = "val" in
    let n = "new" in
      begin
        Printf.printf "proc %s_set_%s (%s, %s) {\n" sn fn v n;
        Printf.printf "    ret := {{ ";
        print_set_entries v n i flLen 0 ;
        Printf.printf "    return\n};\n\n"
      end
;;

let gen_load_field_proc sn fn ty off = (* struct name, field name, field type, field offset *)
  begin
    Printf.printf "proc %s_load_%s (val) {\n" sn fn;
    Printf.printf "    nval := \"i__unops_cast\"(1i, val);\n";
    Printf.printf "    nnval := \"i__binops_add\"(nval, {{ \"int32\", %di }});\n" (off/8);
    Printf.printf "    ret := \"i__load\"(nnval, \"%s\");\n" (compact_ty ty);
    Printf.printf "    return\n};\n\n"
  end
;;

let gen_store_field_proc sn fn off = (* struct name, field name, field offset *)
  begin
    Printf.printf "proc %s_store_%s (val, new) {\n" sn fn;
    Printf.printf "    nval := \"i__unops_cast\"(1i, val);\n";
    Printf.printf "    nnval := \"i__binops_add\"(nval, {{ \"int32\", %di }});\n" (off/8);
    Printf.printf "    ret := \"i__store\"(nnval, new);\n";
    Printf.printf "    return\n};\n\n"
  end
;;

let rec gen_fields_proc_1 sn fl flLen i = (* struct name, fields list, original fields list length, field index *)
  match fl with
      [] -> ()
    | PField(ty, fn, sz, off)::tl -> begin
                                  if contains fn "pad$" then
                                    gen_fields_proc_1 sn tl flLen (i+1)
                                  else 
                                    let () = gen_get_field_proc sn fn i in
                                      let () = gen_set_field_proc sn fn i flLen in
                                        let () = gen_load_field_proc sn fn ty off in
                                          let () = gen_store_field_proc sn fn off in
                                            gen_fields_proc_1 sn tl flLen (i+1)
                                end
    | _ -> ()
;;

let gen_fields_proc sn fl =
  gen_fields_proc_1 sn fl (List.length fl) 0
;;

let rec generate_procs (symTable:tyTree list) =
  match symTable with
      [] -> ()
    | hd::tl -> begin
                  match hd with
                      NilStruct ->  ()
                    | Struct(sn, fl, sz) -> let () = gen_struct_sizeof_proc sn sz in
                                            let () = gen_struct_declare_proc sn (List.length fl) in
                                            let () = gen_struct_declare_cap_proc sn sz in
                                              let () = gen_fields_proc sn fl in                                                
                                                generate_procs tl
                end
;;

(* symTable is a list of symbols *)
let main () =
  let _ = Sys.command ("bin/extractStructs.sh " ^ filename) in
    let in_ch = open_in "/tmp/structDef.txt" in
      let symTable = (populate_ST [] in_ch) in (* empty list of symbols *)
        generate_procs symTable 
        (* print_ST symTable *)
;;

if !Sys.interactive 
   then () 
   else
     begin
       main ();
       ignore (Sys.command ("rm /tmp/structDef.txt"))
     end
;;

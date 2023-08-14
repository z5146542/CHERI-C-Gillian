open Gil_syntax

module Core = struct
  let pred ga ins outs =
    let ga_name = LActions.str_ga ga in
    Asrt.GA (ga_name, ins, outs)
  (* TODO: for our case, we won't need permissions (we instead need tags) -- 
     so we'd have to remove the permissions somehow for now. *)
  let single ~loc ~ofs ~chunk ~sval =
    let chunk = Expr.Lit (String (ValueTranslation.chunk_to_string chunk)) in
    pred Single [ loc; ofs; chunk ] [ sval ]

  let array ~loc ~ofs ~chunk ~size ~sval_arr =
    let chunk = Expr.Lit (String (ValueTranslation.chunk_to_string chunk)) in
    pred Array [ loc; ofs; size; chunk ] [ sval_arr ]

  let hole ~loc ~low ~high =
    pred Hole [ loc; low; high ] [ ]

  let zeros ~loc ~low ~high =
    pred Zeros [ loc; low; high ] [ ]

  let bounds ~loc ~low ~high =
    let bounds = Expr.EList [ low; high ] in
    pred Bounds [ loc ] [ bounds ]

  let freed ~loc = pred Freed [ loc ] []
end

module Others = struct
  open CConstants

  let pred name params = Asrt.Pred (name, params)

  let malloced_abst ~ptr ~total_size =
    pred Internal_Predicates.malloced [ ptr; total_size ]

  let malloced ~ptr ~total_size =
    let loc, ofs = ptr in
    let size = Expr.int_z total_size in
    pred Internal_Predicates.malloced [ Expr.list [ loc; ofs ]; size ]

  let zeros_ptr_size ~ptr ~size =
    pred Internal_Predicates.zeros_ptr_size [ ptr; size ]

  let undefs_ptr_size ~ptr ~size =
    pred Internal_Predicates.undefs_ptr_size [ ptr; size ]

  let array_ptr ~ptr ~chunk ~size ~content =
    let chunk_str = Expr.string (Chunk.to_string chunk) in
    pred Internal_Predicates.array_ptr [ ptr; size; chunk_str; content ]

  let ptr_add ~ptr ~to_add ~res =
    pred Internal_Predicates.ptr_add [ ptr; to_add; res ]
end

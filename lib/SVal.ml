open Gil_syntax
open Gillian.Symbolic

let ( let* ) = Option.bind
let ( let+ ) o f = Option.map f o

(* Might as well use the values used in my original paper.
   The types are:
   - Uint8_v
   - Sint8_v
   - Uint16_v
   - Sint16_v
   - Uint32_v
   - Sint32_v
   - Uint64_v
   - Sint64_v
   - Cap_v
   - Cap_v_frag
   - Undef *)
type t =
  | SUndef
  | SUint8_v  of Expr.t
  | SSint8_v  of Expr.t
  | SUint16_v of Expr.t
  | SSint16_v of Expr.t
  | SUint32_v of Expr.t
  | SSint32_v of Expr.t
  | SUint64_v of Expr.t
  | SSint64_v of Expr.t
  | SCap_v of {
        block  : string;
        offset : Expr.t;
        base   : Expr.t;
        length : Expr.t;
        load   : Expr.t;
        cload  : Expr.t;
        store  : Expr.t;
        cstore : Expr.t;
        clstre : Expr.t;
        global : Expr.t;
        tag    : Expr.t;
    }
  | SCap_v_frag of {
        block  : string;
        offset : Expr.t;
        base   : Expr.t;
        length : Expr.t;
        load   : Expr.t;
        cload  : Expr.t;
        store  : Expr.t;
        cstore : Expr.t;
        clstre : Expr.t;
        global : Expr.t;
        tag    : Expr.t;
        nth    : Expr.t;
    }
[@@deriving yojson]

let typeof (v : t) : Chunk.t =
  match v with
  | SUint8_v _    -> Uint8
  | SSint8_v _    -> Sint8
  | SUint16_v _   -> Uint16
  | SSint16_v _   -> Sint16
  | SUint32_v _   -> Uint32
  | SSint32_v _   -> Sint32
  | SUint64_v _   -> Uint64
  | SSint64_v _   -> Sint64
  | SCap_v _      -> Cap
  | SCap_v_frag _ -> Uint8

let null_cap = SCap_v {
        block  = "0";
        offset = Expr.zero_i;
        base   = Expr.zero_i;
        length = Expr.zero_i;
        load   = (Expr.bool false);
        cload  = (Expr.bool false);
        store  = (Expr.bool false);
        cstore = (Expr.bool false);
        clstre = (Expr.bool false);
        global = (Expr.bool false);
        tag    = (Expr.bool false);
    }

let equal a b =
  match (a, b) with
  | SUndef, SUndef -> true
  | SUint8_v  a, SUint8_v  b when Expr.equal a b -> true
  | SSint8_v  a, SSint8_v  b when Expr.equal a b -> true
  | SUint16_v a, SUint16_v b when Expr.equal a b -> true
  | SSint16_v a, SSint16_v b when Expr.equal a b -> true
  | SUint32_v a, SUint32_v b when Expr.equal a b -> true
  | SSint32_v a, SSint32_v b when Expr.equal a b -> true
  | SUint64_v a, SUint64_v b when Expr.equal a b -> true
  | SSint64_v a, SSint64_v b when Expr.equal a b -> true
  | SCap_v a, SCap_v b
    when String.equal a.block b.block && Expr.equal a.offset b.offset &&
         Expr.equal a.base b.base     && Expr.equal a.length b.length &&
         Expr.equal a.load b.load     && Expr.equal a.cload b.cload &&
         Expr.equal a.store b.store   && Expr.equal a.cstore b.cstore &&
         Expr.equal a.clstre b.clstre && Expr.equal a.global b.global && Expr.equal a.tag b.tag ->
      true
  | SCap_v_frag a, SCap_v_frag b
    when String.equal a.block b.block && Expr.equal a.offset b.offset &&
         Expr.equal a.base b.base     && Expr.equal a.length b.length &&
         Expr.equal a.load b.load     && Expr.equal a.cload b.cload &&
         Expr.equal a.store b.store   && Expr.equal a.cstore b.cstore &&
         Expr.equal a.clstre b.clstre && Expr.equal a.global b.global &&
         Expr.equal a.tag b.tag       && Expr.equal a.nth b.nth ->
      true
  | _, _ -> false

(*
type typ =
  | Tint
  | Tfloat
  | Tlong
  | Tsingle
  | Tany32
  | Tany64
*)
(* let tptr = Compcert.AST.coq_Tptr *)

let is_loc gamma loc =
  let r_opt =
    let* loc_t = Type_env.get gamma loc in
    match loc_t with
    | Type.ObjectType -> Some true
    | _ -> Some false
  in
  Option.value ~default:false r_opt

let is_zero = function
  | SUint8_v (Lit (Int z)) when Z.equal z Z.zero -> true
  | SSint8_v (Lit (Int z)) when Z.equal z Z.zero -> true
  | SUint16_v (Lit (Int z)) when Z.equal z Z.zero -> true
  | SSint16_v (Lit (Int z)) when Z.equal z Z.zero -> true
  | SUint32_v (Lit (Int z)) when Z.equal z Z.zero -> true
  | SSint32_v (Lit (Int z)) when Z.equal z Z.zero -> true
  | SUint64_v (Lit (Int z)) when Z.equal z Z.zero -> true
  | SSint64_v (Lit (Int z)) when Z.equal z Z.zero -> true
  | SCap_v c when SCap_v c = null_cap -> true
  | _ -> false

(*
let zero_of_chunk chunk =
  match Chunk.type_of chunk with
  | Tany32 | Tint -> SVint Expr.zero_i
  | Tany64 | Tlong -> SVlong Expr.zero_i
  | Tsingle -> SVsingle (Lit (Num 0.))
  | Tfloat -> SVfloat (Lit (Num 0.))
*)

(* For capability, we need an isomorphism between a zero expression and null cap *)
let zero_of_chunk (chunk : Chunk.t) =
  match chunk with
  | Uint8 -> SUint8_v Expr.zero_i
  | Sint8 -> SSint8_v Expr.zero_i
  | Uint16 -> SUint16_v Expr.zero_i
  | Sint16 -> SSint16_v Expr.zero_i
  | Uint32 -> SUint32_v Expr.zero_i
  | Sint32 -> SSint32_v Expr.zero_i
  | Uint64 -> SUint64_v Expr.zero_i
  | Sint64 -> SSint64_v Expr.zero_i
  | Cap -> null_cap

let is_loc_ofs gamma loc ofs =
  let r_opt =
    let* loc_t = Type_env.get gamma loc in
    let* ofs_t = Type_env.get gamma ofs in
    match (loc_t, ofs_t) with
    | Type.ObjectType, Type.IntType -> Some true
    | _ -> Some false
  in
  Option.value ~default:false r_opt

let is_cap_types gamma block offset base length load cload store cstore clstre global tag offsiz =
  let r_opt =
    let* block_t  = Type_env.get gamma block  in
    let* offset_t = Type_env.get gamma offset in
    let* base_t   = Type_env.get gamma base   in
    let* length_t = Type_env.get gamma length in
    let* load_t   = Type_env.get gamma load   in
    let* cload_t  = Type_env.get gamma cload  in
    let* store_t  = Type_env.get gamma store  in
    let* cstore_t = Type_env.get gamma cstore in
    let* clstre_t = Type_env.get gamma clstre in
    let* global_t = Type_env.get gamma global in
    let* tag_t    = Type_env.get gamma tag    in
    let* offsiz_t = Type_env.get gamma offsiz in
    match (block_t, offset_t, base_t, length_t, load_t, cload_t, store_t, cstore_t, clstre_t, global_t, tag_t, offsiz_t) with
    | Type.ObjectType, Type.IntType, Type.IntType, Type.IntType, Type.BooleanType,
      Type.BooleanType, Type.BooleanType, Type.BooleanType, Type.BooleanType,
      Type.BooleanType, Type.BooleanType, Type.IntType -> Some true
    | _ -> Some false
  in
  Option.value ~default:false r_opt

let is_cap_frag_types gamma block offset base length load cload store cstore clstre global tag offsiz nth =
  let r_opt =
    let* block_t  = Type_env.get gamma block  in
    let* offset_t = Type_env.get gamma offset in
    let* base_t   = Type_env.get gamma base   in
    let* length_t = Type_env.get gamma length in
    let* load_t   = Type_env.get gamma load   in
    let* cload_t  = Type_env.get gamma cload  in
    let* store_t  = Type_env.get gamma store  in
    let* cstore_t = Type_env.get gamma cstore in
    let* clstre_t = Type_env.get gamma clstre in
    let* global_t = Type_env.get gamma global in
    let* tag_t    = Type_env.get gamma tag    in
    let* offsiz_t = Type_env.get gamma offsiz in
    let* nth_t    = Type_env.get gamma nth    in
    match (block_t, offset_t, base_t, length_t, load_t, cload_t, store_t, cstore_t, clstre_t, global_t, tag_t, offsiz_t, nth_t) with
    | Type.ObjectType, Type.IntType, Type.IntType, Type.IntType, Type.BooleanType,
      Type.BooleanType, Type.BooleanType, Type.BooleanType, Type.BooleanType,
      Type.BooleanType, Type.BooleanType, Type.IntType, Type.IntType -> Some true
    | _ -> Some false
  in
  Option.value ~default:false r_opt
(* TODO: somehow offsiz needs to be included *)
let of_gil_expr_almost_concrete ?(gamma = Type_env.init ()) gexpr =
  let open Expr in
  let open ValueTranslation.VTypes in
  match gexpr with
  | Lit Undefined -> Some (SUndef, [])
  (* For now, just do some simple pattern matching *)
  (** Case 1: capabilities **)
  | EList [ ALoc loc; offset; base; length; load; cload; store; cstore; clstre; global; tag; offsiz ]
  | EList [ Lit (Loc loc); offset; base; length; load; cload; store; cstore; clstre; global; tag; offsiz ] ->
      Some (SCap_v { block = loc; offset = offset; base = base; length = length;
                     load = load; cload = cload; store = store; cstore = cstore;
                     clstre = clstre; global = global; tag = tag
                   }, [])
  | EList [ LVar loc; Lit (Int offset); Lit (Int base); Lit (Int length); Lit (Bool load);
            Lit (Bool cload); Lit (Bool store); Lit (Bool cstore); Lit (Bool clstre);
            Lit (Bool global); Lit (Bool tag); Lit (Int offsiz)] ->
      let aloc = ALoc.alloc () in
      let new_pf = Formula.Eq (LVar loc, Expr.ALoc aloc) in
      Some (SCap_v { block = aloc; offset = Lit (Int offset); base = Lit (Int base);
                     length = Lit (Int length); load = Lit (Bool load); cload = Lit (Bool load);
                     store = Lit (Bool store); cstore = Lit (Bool cstore);
                     clstre = Lit (Bool clstre); global = Lit (Bool global); tag = Lit (Bool tag)
                   }, [ new_pf ])
  | EList [ LVar block; LVar offset; LVar base; LVar length; LVar load; LVar cload;
            LVar store; LVar cstore; LVar clstre; LVar global; LVar tag; LVar offsiz ]
      when is_cap_types gamma block offset base length load cload store cstore clstre global tag offsiz ->
      let aloc = ALoc.alloc () in
      let new_pf = Formula.Eq (LVar block, Expr.ALoc aloc) in
      Some (SCap_v { block = aloc; offset = LVar offset; base = LVar base; length = LVar length;
                     load = LVar load; cload = LVar cload; store = LVar store;
                     cstore = LVar cstore; clstre = LVar clstre; global = LVar global;
                     tag = LVar tag
                   }, [ new_pf ])
  (* Case 2: capability fragments *)
  | EList [ ALoc loc; offset; base; length; load; cload; store; cstore; clstre; global; tag; offsiz; nth ]
  | EList [ Lit (Loc loc); offset; base; length; load; cload; store; cstore; clstre; global; tag; offsiz; nth ] ->
      Some (SCap_v_frag { block = loc; offset = offset; base = base; length = length;
                     load = load; cload = cload; store = store; cstore = cstore;
                     clstre = clstre; global = global; tag = tag; nth = nth;
                   }, [])
  | EList [ LVar loc; Lit (Int offset); Lit (Int base); Lit (Int length); Lit (Bool load);
            Lit (Bool cload); Lit (Bool store); Lit (Bool cstore); Lit (Bool clstre);
            Lit (Bool global); Lit (Bool tag); Lit (Int offsiz); Lit (Int nth) ] ->
      let aloc = ALoc.alloc () in
      let new_pf = Formula.Eq (LVar loc, Expr.ALoc aloc) in
      Some (SCap_v_frag { block = aloc; offset = Lit (Int offset); base = Lit (Int base);
                     length = Lit (Int length); load = Lit (Bool load); cload = Lit (Bool load);
                     store = Lit (Bool store); cstore = Lit (Bool cstore);
                     clstre = Lit (Bool clstre); global = Lit (Bool global); tag = Lit (Bool tag);
                     nth = Lit (Int nth)
                   }, [ new_pf ])
  | EList [ LVar block; LVar offset; LVar base; LVar length; LVar load; LVar cload;
            LVar store; LVar cstore; LVar clstre; LVar global; LVar tag; LVar offsiz; LVar nth ]
      when is_cap_frag_types gamma block offset base length load cload store cstore clstre global tag offsiz nth ->
      let aloc = ALoc.alloc () in
      let new_pf = Formula.Eq (LVar block, Expr.ALoc aloc) in
      Some (SCap_v_frag { block = aloc; offset = LVar offset; base = LVar base; length = LVar length;
                     load = LVar load; cload = LVar cload; store = LVar store;
                     cstore = LVar cstore; clstre = LVar clstre; global = LVar global;
                     tag = LVar tag; nth = LVar nth
                   }, [ new_pf ])
  (* Case on integer types -- in this case, we also just match to the corresponding values *)
  | EList [ Lit (String typ); value ] when String.equal typ u8_type  -> Some (SUint8_v  value, [])
  | EList [ Lit (String typ); value ] when String.equal typ s8_type  -> Some (SSint8_v  value, [])
  | EList [ Lit (String typ); value ] when String.equal typ u16_type -> Some (SUint16_v value, [])
  | EList [ Lit (String typ); value ] when String.equal typ s16_type -> Some (SSint16_v value, [])
  | EList [ Lit (String typ); value ] when String.equal typ u32_type -> Some (SUint32_v value, [])
  | EList [ Lit (String typ); value ] when String.equal typ s32_type -> Some (SSint32_v value, [])
  | EList [ Lit (String typ); value ] when String.equal typ u64_type -> Some (SUint64_v value, [])
  | EList [ Lit (String typ); value ] when String.equal typ s64_type -> Some (SSint64_v value, [])
  | _ -> None

let of_gil_expr ?(pfs = Pure_context.init ()) ?(gamma = Type_env.init ()) sval_e
    =
  Logging.verbose (fun fmt -> fmt "OF_GIL_EXPR : %a" Expr.pp sval_e);
  let possible_exprs =
    sval_e :: FO_logic.Reduction.get_equal_expressions pfs sval_e
  in
  List.fold_left
    (fun ac exp ->
      Logging.verbose (fun fmt -> fmt "TRYING SUBSTITUTE EXPR : %a" Expr.pp exp);
      match ac with
      | None -> of_gil_expr_almost_concrete ~gamma exp
      | _ -> ac)
    None possible_exprs

let of_gil_expr_exn
    ?(pfs = Pure_context.init ())
    ?(gamma = Type_env.init ())
    gexp =
  match of_gil_expr ~pfs ~gamma gexp with
  | Some s -> s
  | None ->
      failwith
        (Format.asprintf
           "The following expression does not seem to correspond to any \
            CHERI-C value : %a"
           Expr.pp gexp)

let to_gil_expr gexpr =
  let open Expr in
  let open ValueTranslation.VTypes in
  match gexpr with
  | SUndef -> (Lit Undefined, [])
  | SCap_v c ->
      let loc = loc_from_loc_name c.block in
      (EList [ loc; c.offset; c.base; c.length; c.load; c.cload; c.store; c.cstore;
               c.clstre; c.global; c.tag; one_i ],
             [ (loc, Type.ObjectType); (c.offset, Type.IntType); (c.base, Type.IntType);
               (c.length, Type.IntType); (c.load, Type.BooleanType); (c.cload, Type.BooleanType);
               (c.store, Type.BooleanType); (c.cstore, Type.BooleanType);
               (c.clstre, Type.BooleanType); (c.global, Type.BooleanType); (c.tag, Type.BooleanType) ])
  | SCap_v_frag c ->
      let loc = loc_from_loc_name c.block in
      (EList [ loc; c.offset; c.base; c.length; c.load; c.cload; c.store; c.cstore;
               c.clstre; c.global; c.tag; one_i; c.nth ],
             [ (loc, Type.ObjectType); (c.offset, Type.IntType); (c.base, Type.IntType);
               (c.length, Type.IntType); (c.load, Type.BooleanType); (c.cload, Type.BooleanType);
               (c.store, Type.BooleanType); (c.cstore, Type.BooleanType);
               (c.clstre, Type.BooleanType); (c.global, Type.BooleanType); (c.tag, Type.BooleanType);
               (c.nth, Type.IntType) ])
  | SUint8_v n -> (EList [ Lit (String u8_type); n ], [ (n, Type.IntType) ])
  | SSint8_v n -> (EList [ Lit (String s8_type); n ], [ (n, Type.IntType) ])
  | SUint16_v n -> (EList [ Lit (String u16_type); n ], [ (n, Type.IntType) ])
  | SSint16_v n -> (EList [ Lit (String s16_type); n ], [ (n, Type.IntType) ])
  | SUint32_v n -> (EList [ Lit (String u32_type); n ], [ (n, Type.IntType) ])
  | SSint32_v n -> (EList [ Lit (String s32_type); n ], [ (n, Type.IntType) ])
  | SUint64_v n -> (EList [ Lit (String u64_type); n ], [ (n, Type.IntType) ])
  | SSint64_v n -> (EList [ Lit (String s64_type); n ], [ (n, Type.IntType) ])

(* Get all the logical variables given expressions within a symbolic value *)
(* For this, I may have to take the union of the variablesi in the Cap case *)
let lvars =
  let open Utils.Containers in
  function
  | SUndef -> SS.empty
  | SCap_v { block = _; offset = o; base = b; length = l; load = l1; cload = l2; store = s1; cstore = s2; clstre = s3; global = g; tag = t } ->
      List.fold_left SS.union SS.empty [ Expr.lvars o; Expr.lvars b; Expr.lvars l;
        Expr.lvars l1; Expr.lvars l2; Expr.lvars s1; Expr.lvars s2; Expr.lvars s3;
        Expr.lvars g; Expr.lvars t ]
  | SCap_v_frag { block = _; offset = o; base = b; length = l; load = l1; cload = l2; store = s1; cstore = s2; clstre = s3; global = g; tag = t; nth = n } ->
      List.fold_left SS.union SS.empty [ Expr.lvars o; Expr.lvars b; Expr.lvars l;
        Expr.lvars l1; Expr.lvars l2; Expr.lvars s1; Expr.lvars s2; Expr.lvars s3;
        Expr.lvars g; Expr.lvars t; Expr.lvars n ]
  | SUint8_v e  | SSint8_v e  | SUint16_v e | SSint16_v e
  | SUint32_v e | SSint32_v e | SUint64_v e | SSint64_v e ->
      Expr.lvars e

(* Get all the abstract locations given expressions within a symbolic value *)
(* Personally, I think this is extremely redundant *)
let alocs =
  let open Utils.Containers in
  function
  | SUndef -> SS.empty
  | SCap_v { block = bl; offset = o; base = b; length = l; load = l1; cload = l2; store = s1; cstore = s2; clstre = s3; global = g; tag = t } ->
      let alocs_union = List.fold_left SS.union SS.empty
        [ Expr.alocs o; Expr.alocs b; Expr.alocs l; Expr.alocs l1; Expr.alocs l2;
          Expr.alocs s1; Expr.alocs s2; Expr.alocs s3; Expr.alocs g; Expr.alocs t ] in
      if Utils.Names.is_aloc_name bl then SS.add bl alocs_union else alocs_union
  | SCap_v_frag { block = bl; offset = o; base = b; length = l; load = l1; cload = l2; store = s1; cstore = s2; clstre = s3; global = g; tag = t; nth = n }  ->
      let alocs_union = List.fold_left SS.union SS.empty
        [ Expr.alocs o; Expr.alocs b; Expr.alocs l; Expr.alocs l1; Expr.alocs l2;
          Expr.alocs s1; Expr.alocs s2; Expr.alocs s3; Expr.alocs g; Expr.alocs t;
          Expr.alocs n] in
      if Utils.Names.is_aloc_name bl then SS.add bl alocs_union else alocs_union
  | SUint8_v e  | SSint8_v e  | SUint16_v e | SSint16_v e
  | SUint32_v e | SSint32_v e | SUint64_v e | SSint64_v e ->
      Expr.alocs e

let pp fmt v =
  let se = Expr.pp in
  let f = Format.fprintf in
  match v with
  | SUndef -> f fmt "undefined"
  | SUint8_v i -> f fmt "uint8(%a)" se i
  | SSint8_v i -> f fmt "int8(%a)" se i
  | SUint16_v i -> f fmt "uint16(%a)" se i
  | SSint16_v i -> f fmt "int16(%a)" se i
  | SUint32_v i -> f fmt "uint32(%a)" se i
  | SSint32_v i -> f fmt "int32(%a)" se i
  | SUint64_v i -> f fmt "uint64(%a)" se i
  | SSint64_v i -> f fmt "int64(%a)" se i
  | SCap_v c -> f fmt "Cap(%s, %a, %a, %a, %a, %a, %a, %a, %a, %a, %a)"
      c.block se c.offset se c.base se c.length se c.load se c.cload se c.store
      se c.cstore se c.clstre se c.global se c.tag
  | SCap_v_frag c -> f fmt "CapF(%s, %a, %a, %a, %a, %a, %a, %a, %a, %a, %a, %a)"
      c.block se c.offset se c.base se c.length se c.load se c.cload se c.store
      se c.cstore se c.clstre se c.global se c.tag se c.nth

let substitution ~le_subst sv =
  match sv with
  | SUint8_v v -> SUint8_v (le_subst v)
  | SSint8_v v -> SSint8_v (le_subst v)
  | SUint16_v v -> SUint16_v (le_subst v)
  | SSint16_v v -> SSint16_v (le_subst v)
  | SUint32_v v -> SUint32_v (le_subst v)
  | SSint32_v v -> SSint32_v (le_subst v)
  | SUint64_v v -> SUint64_v (le_subst v)
  | SSint64_v v -> SSint64_v (le_subst v)
  | SUndef -> SUndef
  | SCap_v { block = loc; offset = o; base = b; length = l; load = l1; cload = l2; store = s1; cstore = s2; clstre = s3; global = g; tag = t } -> (
      let loc_e = Expr.loc_from_loc_name loc in
      match le_subst loc_e with
      | Expr.ALoc nloc | Lit (Loc nloc) ->
          SCap_v { block  = nloc;
                   offset = le_subst o;
                   base   = le_subst b;
                   length = le_subst l;
                   load   = le_subst l1;
                   cload  = le_subst l2;
                   store  = le_subst s1;
                   cstore = le_subst s2;
                   clstre = le_subst s3;
                   global = le_subst g;
                   tag    = le_subst t
                 }
      | e ->
          failwith
            (Format.asprintf "Heap substitution fail for loc: %a" Expr.pp e))
  | SCap_v_frag { block = loc; offset = o; base = b; length = l; load = l1; cload = l2; store = s1; cstore = s2; clstre = s3; global = g; tag = t; nth = n } -> (
      let loc_e = Expr.loc_from_loc_name loc in
      match le_subst loc_e with
      | Expr.ALoc nloc | Lit (Loc nloc) ->
          SCap_v_frag { block = nloc;
                        offset = le_subst o;
                        base   = le_subst b;
                        length = le_subst l;
                        load   = le_subst l1;
                        cload  = le_subst l2;
                        store  = le_subst s1;
                        cstore = le_subst s2;
                        clstre = le_subst s3;
                        global = le_subst g;
                        tag    = le_subst t;
                        nth    = le_subst n;
                 }
      | e ->
          failwith
            (Format.asprintf "Heap substitution fail for loc: %a" Expr.pp e))

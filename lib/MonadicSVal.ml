include SVal
open Gil_syntax
open Monadic
open Delayed.Syntax
module DO = Delayed_option
module DR = Delayed_result

exception NotACHERICValue of Expr.t

module Patterns = struct
  open Formula.Infix

  let integer e =
    let open Expr in
    (typeof e) #== (type_ IntType)

  let uint8_typ, sint8_typ, uint16_typ, sint16_typ, uint32_typ, sint32_typ,
      uint64_typ, sint64_typ =
    let open Expr in
    let open ValueTranslation.VTypes in
    let num_typ typ_str x =
      (typeof x) #== (type_ ListType)
      #&& ((list_length x) #== (int 2))
      #&& ((list_nth x 0) #== (string typ_str))
      #&& ((typeof (list_nth x 1)) #== (type_ IntType))
    in
    ( num_typ u8_type,
      num_typ s8_type,
      num_typ u16_type,
      num_typ s16_type,
      num_typ u32_type,
      num_typ s32_type,
      num_typ u64_type,
      num_typ s64_type )

  let undefined x = x #== (Expr.Lit Undefined)

  (* We need to consider both cap and cap_f *)
  let obj x =
    let open Expr in
    (typeof x) #== (type_ ListType)
    #&& ((list_length x) #== (int 12))
    #&& ((typeof (list_nth x 0)) #== (type_ ObjectType))
    #&& ((typeof (list_nth x 1)) #== (type_ IntType))
    #&& ((typeof (list_nth x 2)) #== (type_ IntType))
    #&& ((typeof (list_nth x 3)) #== (type_ IntType))
    #&& ((typeof (list_nth x 4)) #== (type_ BooleanType))
    #&& ((typeof (list_nth x 5)) #== (type_ BooleanType))
    #&& ((typeof (list_nth x 6)) #== (type_ BooleanType))
    #&& ((typeof (list_nth x 7)) #== (type_ BooleanType))
    #&& ((typeof (list_nth x 8)) #== (type_ BooleanType))
    #&& ((typeof (list_nth x 9)) #== (type_ BooleanType))
    #&& ((typeof (list_nth x 10)) #== (type_ BooleanType))
    #&& ((typeof (list_nth x 11)) #== (type_ IntType))
  
  let obj_f x =
    let open Expr in
    (typeof x) #== (type_ ListType)
    #&& ((list_length x) #== (int 13))
    #&& ((typeof (list_nth x 0)) #== (type_ ObjectType))
    #&& ((typeof (list_nth x 1)) #== (type_ IntType))
    #&& ((typeof (list_nth x 2)) #== (type_ IntType))
    #&& ((typeof (list_nth x 3)) #== (type_ IntType))
    #&& ((typeof (list_nth x 4)) #== (type_ BooleanType))
    #&& ((typeof (list_nth x 5)) #== (type_ BooleanType))
    #&& ((typeof (list_nth x 6)) #== (type_ BooleanType))
    #&& ((typeof (list_nth x 7)) #== (type_ BooleanType))
    #&& ((typeof (list_nth x 8)) #== (type_ BooleanType))
    #&& ((typeof (list_nth x 9)) #== (type_ BooleanType))
    #&& ((typeof (list_nth x 10)) #== (type_ BooleanType))
    #&& ((typeof (list_nth x 11)) #== (type_ IntType))
    #&& ((typeof (list_nth x 12)) #== (type_ IntType))
end

let of_chunk_and_expr chunk e =
  let open Chunk in
  let return = Delayed.return in
  let open Patterns in
  let* e = Delayed.reduce e in
  match e with
  | Expr.Lit Undefined -> return SUndef
  | _ -> (
      match chunk with
      | Cap -> (
          match%ent e with
          | obj -> (
              match e with
              | EList [ ALoc bl; o; b; l; l1; l2; s1; s2; s3; g; t; os ] -> 
                  return (SCap_v { block = bl; offset = o; base = b; length = l; load = l1; cload = l2; store = s1; cstore = s2; clstre = s3; global = g; tag = t })
              | _ ->
                  Fmt.failwith
                    "of_chunk_and_expr: Not a location, but should be: %a"
                    Expr.pp e)
        )
      | Uint8 -> (
          match%ent e with
          | uint8_typ -> 
              let open Formula.Infix in
              let i k = Expr.int k in
              let learned = [ (i 0) #<= e; e #<= (i 255) ]
              in
              return ~learned (SUint8_v e)
          | obj_f -> (
              match e with
              | EList [ ALoc bl; o; b; l; l1; l2; s1; s2; s3; g; t; n; os ] -> 
                  return (SCap_v_frag { block = bl; offset = o; base = b; length = l; load = l1; cload = l2; store = s1; cstore = s2; clstre = s3; global = g; tag = t; nth = n })
              | _ ->
                  Fmt.failwith
                    "of_chunk_and_expr: Not a location, but should be: %a"
                    Expr.pp e)
          )
      | Sint8 -> (
          match%ent e with
          | sint8_typ ->
              let open Formula.Infix in
              let i k = Expr.int k in
              let learned = [ (i (-128)) #<= e; e #<= (i 127) ]
              in
              return ~learned (SSint8_v e)
          | obj_f -> ( 
              match e with
              | EList [ ALoc bl; o; b; l; l1; l2; s1; s2; s3; g; t; n; os ] -> 
                  return (SCap_v_frag { block = bl; offset = o; base = b; length = l; load = l1; cload = l2; store = s1; cstore = s2; clstre = s3; global = g; tag = t; nth = n })
              | _ ->
                  Fmt.failwith
                    "of_chunk_and_expr: Not a location, but should be: %a"
                    Expr.pp e)
        )
      | Uint16 ->
          let open Formula.Infix in
          let i k = Expr.int k in
          let learned = [ (i 0) #<= e; e #<= (i 65535) ]
          in
          return ~learned (SUint16_v e)
      | Sint16 ->
          let open Formula.Infix in
          let i k = Expr.int k in
          let learned = [ (i (-32768)) #<= e; e #<= (i 32767) ]
          in
          return ~learned (SSint16_v e)
      | Uint32 ->
          let open Formula.Infix in
          let i k = Expr.int k in
          let learned = [ (i 0) #<= e; e #<= (i 4294967295) ]
          in
          return ~learned (SUint32_v e)
      | Sint32 ->
          let open Formula.Infix in
          let i k = Expr.int k in
          let learned = [ (i (-2147483648)) #<= e; e #<= (i 2147483647) ]
          in
          return ~learned (SSint32_v e)
      | Uint64 ->
          let open Formula.Infix in
          let i k = Expr.int k in
          let j k = Expr.int_z k in
          let learned = [ (i 0) #<= e; e #<= (j (Z.succ (Z.shift_left (Z.of_int64 Int64.max_int) 1))) ]
          in
          return ~learned (SUint64_v e)
      | Sint64 ->
          let open Formula.Infix in
          let i k = Expr.int_z (Z.of_int64 k) in
          let learned = [ (i (Int64.min_int)) #<= e; e #<= (i Int64.max_int) ]
          in
          return ~learned (SSint64_v e)
  )

let of_gil_expr sval_e =
  let open Formula.Infix in
  let open Patterns in
  Logging.verbose (fun fmt -> fmt "OF_GIL_EXPR : %a" Expr.pp sval_e);
  let* sval_e = Delayed.reduce sval_e in
  match%ent sval_e with
  | undefined -> DO.some SUndef
  | obj ->
      let loc_expr = Expr.list_nth sval_e 0 in
      let o  = Expr.list_nth sval_e 1  in
      let b  = Expr.list_nth sval_e 2  in
      let l  = Expr.list_nth sval_e 3  in
      let l1 = Expr.list_nth sval_e 4  in 
      let l2 = Expr.list_nth sval_e 5  in
      let s1 = Expr.list_nth sval_e 6  in
      let s2 = Expr.list_nth sval_e 7  in 
      let s3 = Expr.list_nth sval_e 8  in 
      let g  = Expr.list_nth sval_e 9  in
      let t  = Expr.list_nth sval_e 10 in
      let s  = Expr.list_nth sval_e 11 in
      let* s  = Delayed.reduce  s in
      let* t  = Delayed.reduce  t in
      let* g  = Delayed.reduce  g in
      let* s3 = Delayed.reduce s3 in
      let* s2 = Delayed.reduce s2 in
      let* s1 = Delayed.reduce s1 in
      let* l2 = Delayed.reduce l2 in
      let* l1 = Delayed.reduce l1 in
      let* l  = Delayed.reduce  l in
      let* b  = Delayed.reduce  b in
      let* o  = Delayed.reduce  o in
      let* loc_opt = Delayed.resolve_loc loc_expr in
      let bl, learned =
        match loc_opt with
        | Some l -> (l, [])
        | None ->
            let aloc = ALoc.alloc () in
            let learned = [ loc_expr #== (ALoc aloc) ] in
            (aloc, learned)
      in
      DO.some ~learned (SCap_v { block = bl ; offset = o ; base = b ; length = l ; load = l1 ; cload = l2 ; store = s1 ; cstore = s2 ; clstre = s3 ; global = g ; tag = t })
  | obj_f ->
      let loc_expr = Expr.list_nth sval_e 0 in
      let o  = Expr.list_nth sval_e 1  in
      let b  = Expr.list_nth sval_e 2  in
      let l  = Expr.list_nth sval_e 3  in
      let l1 = Expr.list_nth sval_e 4  in 
      let l2 = Expr.list_nth sval_e 5  in
      let s1 = Expr.list_nth sval_e 6  in
      let s2 = Expr.list_nth sval_e 7  in 
      let s3 = Expr.list_nth sval_e 8  in 
      let g  = Expr.list_nth sval_e 9  in
      let t  = Expr.list_nth sval_e 10 in
      let n  = Expr.list_nth sval_e 11 in
      let s  = Expr.list_nth sval_e 12 in
      let* s  = Delayed.reduce  s in
      let* n  = Delayed.reduce  n in
      let* t  = Delayed.reduce  t in
      let* g  = Delayed.reduce  g in
      let* s3 = Delayed.reduce s3 in
      let* s2 = Delayed.reduce s2 in
      let* s1 = Delayed.reduce s1 in
      let* l2 = Delayed.reduce l2 in
      let* l1 = Delayed.reduce l1 in
      let* l  = Delayed.reduce  l in
      let* b  = Delayed.reduce  b in
      let* o  = Delayed.reduce  o in
      let* loc_opt = Delayed.resolve_loc loc_expr in
      let bl, learned =
        match loc_opt with
        | Some l -> (l, [])
        | None ->
            let aloc = ALoc.alloc () in
            let learned = [ loc_expr #== (ALoc aloc) ] in
            (aloc, learned)
      in
      DO.some ~learned (SCap_v_frag { block = bl ; offset = o ; base = b ; length = l ; load = l1 ; cload = l2 ; store = s1 ; cstore = s2 ; clstre = s3 ; global = g ; tag = t ; nth = n })
  | uint8_typ -> DO.some (SUint8_v (Expr.list_nth sval_e 1))
  | sint8_typ -> DO.some (SSint8_v (Expr.list_nth sval_e 1))
  | uint16_typ -> DO.some (SUint16_v (Expr.list_nth sval_e 1))
  | sint16_typ -> DO.some (SSint16_v (Expr.list_nth sval_e 1))
  | uint32_typ -> DO.some (SUint32_v (Expr.list_nth sval_e 1))
  | sint32_typ -> DO.some (SSint32_v (Expr.list_nth sval_e 1))
  | uint64_typ -> DO.some (SUint64_v (Expr.list_nth sval_e 1))
  | sint64_typ -> DO.some (SSint64_v (Expr.list_nth sval_e 1))
  | _ -> DO.none ()

let of_gil_expr_exn sval_e =
  let* value_opt = of_gil_expr sval_e in
  match value_opt with
  | None -> raise (NotACHERICValue sval_e)
  | Some value -> Delayed.return value

let to_gil_expr_undelayed = to_gil_expr

let to_gil_expr sval =
  let exp, typings = to_gil_expr_undelayed sval in
  let typing_pfs =
    List.map
      (fun (e, t) ->
        let open Expr in
        let open Formula.Infix in
        (typeof e) #== (type_ t))
      typings
  in
  Delayed.return ~learned:typing_pfs exp

let sure_is_zero = function
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

module SVArray = struct
  type t =
    | Arr of Expr.t
        (** the parameter should be a list representing a *NON-EMPTY* list *)
    | AllUndef
    | AllZeros
  [@@deriving yojson]

  let reduce t =
    let open Delayed.Syntax in
    match t with
    | Arr e ->
        let+ reduced = Delayed.reduce e in
        Arr reduced
    | _ -> Delayed.return t

  let pp fmt = function
    | Arr e -> Expr.pp fmt e
    | AllUndef -> Fmt.string fmt "AllUndef"
    | AllZeros -> Fmt.string fmt "AllZeros"

  let empty = Arr (EList [])

  let is_empty =
    let open Formula.Infix in
    function
    | Arr e -> (Expr.list_length e) #== (Expr.int 0)
    | _ -> False

  let sure_is_all_zeros = function
    | Arr (EList l) ->
        List.for_all
          (function
            | Expr.Lit (Int z) when Z.equal z Z.zero -> true
            | _ -> false)
          l
    | AllZeros -> true
    | _ -> false

  let equal arr_a arr_b =
    match (arr_a, arr_b) with
    | Arr a, Arr b -> Expr.equal a b
    | AllUndef, AllUndef | AllZeros, AllZeros -> true
    | _ -> false

  let conc_to_abst_undelayed conc =
    let rev_l, gamma =
      List.fold_left
        (fun (acc, gamma) sval ->
          let new_el, new_gamma = SVal.to_gil_expr sval in
          (new_el :: acc, new_gamma @ gamma))
        ([], []) conc
    in
    let learned =
      List.map
        (let open Formula.Infix in
        fun (e, t) -> (Expr.typeof e) #== (Expr.type_ t))
        gamma
    in
    (Expr.EList (List.rev rev_l), learned)

  let conc_to_abst conc =
    let e, learned = conc_to_abst_undelayed conc in
    Delayed.return ~learned e

  let undefined_pf ?size arr_exp =
    let size =
      match size with
      | None -> Expr.list_length arr_exp
      | Some size -> size
    in
    let open Formula.Infix in
    let zero = Expr.int 0 in
    let size = Engine.Reduction.reduce_lexpr size in
    match size with
    | Lit (Int x) ->
        Logging.verbose (fun fmt ->
            fmt "Undefined pf: Concrete: %a" Expr.pp size);
        let undefs =
          Expr.Lit (LList (List.init (Z.to_int x) (fun _ -> Literal.Undefined)))
        in
        arr_exp #== undefs
    | _ ->
        Logging.verbose (fun fmt ->
            fmt "Undefined pf: not as concrete: %a" Expr.pp size);
        let i = LVar.alloc () in
        let i_e = Expr.LVar i in
        forall
          [ (i, Some IntType) ]
          zero #<= i_e #&& (i_e #< size)
          #=> ((Expr.list_nth_e arr_exp i_e) #== (Lit Undefined))

  let zeros_pf ?size arr_exp =
    let size =
      match size with
      | None -> Expr.list_length arr_exp
      | Some size -> size
    in
    let open Formula.Infix in
    let size = Engine.Reduction.reduce_lexpr size in
    match size with
    | Lit (Int x) ->
        Logging.verbose (fun fmt -> fmt "Zeros pf: Concrete: %a" Expr.pp size);
        let zeros =
          Expr.Lit
            (LList (List.init (Z.to_int x) (fun _ -> Literal.Int Z.zero)))
        in
        arr_exp #== zeros
    | _ ->
        Logging.verbose (fun fmt ->
            fmt "Zeros pf: not as concrete: %a" Expr.pp size);
        let is_zero e = e #== (Expr.int 0) in
        let i = LVar.alloc () in
        let i_e = Expr.LVar i in
        let zero = Expr.int 0 in
        forall
          [ (i, Some IntType) ]
          zero #<= i_e #&& (i_e #< size)
          #=> (is_zero (Expr.list_nth_e arr_exp i_e))

  let to_arr_with_size arr s =
    let open Formula.Infix in
    let allocate_array_lvar (descr : ?size:Expr.t -> Expr.t -> Formula.t) =
      let x = LVar.alloc () in
      let learned_types = [ (x, Gil_syntax.Type.ListType) ] in
      let x = Expr.LVar x in
      let learned = [ (Expr.list_length x) #== s; descr ~size:s x ] in
      Delayed.return ~learned ~learned_types x
    in
    match arr with
    | Arr e -> Delayed.return e
    | AllUndef -> allocate_array_lvar undefined_pf
    | AllZeros -> allocate_array_lvar zeros_pf

  let concat_knowing_size (left, left_size) (right, right_size) =
    let open Delayed in
    let open Delayed.Syntax in
    match (left, right) with
    | Arr a, Arr b -> return (Arr (Expr.list_cat a b))
    | AllUndef, AllUndef -> return AllUndef
    | AllZeros, AllZeros -> return AllZeros
    | left, right ->
        let* left = to_arr_with_size left left_size in
        let+ right = to_arr_with_size right right_size in
        Arr (Expr.list_cat left right)

  let concat left right =
    match (left, right) with
    | Arr a, Arr b -> Some (Arr (Expr.list_cat a b))
    | AllUndef, AllUndef -> Some AllUndef
    | AllZeros, AllZeros -> Some AllZeros
    | _ -> None

  (** This already assumes the value is a number and not a pointer *)
  let to_single_value ~chunk = function
    | Arr (EList [ a ]) ->
        let+ v = of_chunk_and_expr chunk a in
        Some v
    | AllZeros -> DO.some (zero_of_chunk chunk)
    | AllUndef -> DO.some SUndef
    | _ -> DO.none ()

  let singleton = function
    (* Assuming that the chunk is correct already *)
    | SUint8_v e  | SSint8_v e  | SUint16_v e | SSint16_v e 
    | SUint32_v e | SSint32_v e | SUint64_v e | SSint64_v e -> Arr (Expr.EList [ e ])
    | SCap_v _ as cap ->
        let e_cap, _ = to_gil_expr_undelayed cap in
        Arr (Expr.EList [ e_cap ])
    | SCap_v_frag _ as capf ->
        let e_capf, _ = to_gil_expr_undelayed capf in
        Arr (Expr.EList [ e_capf ])
    | SUndef -> AllUndef

  let array_sub arr o len : t =
    match arr with
    | AllZeros -> AllZeros
    | AllUndef -> AllUndef
    | Arr e -> Arr (Expr.list_sub ~lst:e ~start:o ~size:len)

  (** This assumes chunks are properly respected outside of the call of this function *)
  let array_cons (el : SVal.t) arr = concat (singleton el) arr

  let array_append arr el = concat arr (singleton el)

  let to_gil_expr_undelayed ~chunk ~range svarr =
    let chunk_size = Chunk.size_expr chunk in
    let size =
      let open Expr.Infix in
      let low, high = range in
      (high - low) / chunk_size
    in
    let f_of_all_same ~describing_pf ~concrete_single =
      match size with
      | Lit (Int n) ->
          (Expr.EList (Utils.List_utils.make (Z.to_int n) concrete_single), [])
      | _ ->
          let open Formula.Infix in
          let arr = LVar.alloc () in
          let arr_e = Expr.LVar arr in
          let learned =
            let open Expr in
            [
              (typeof arr_e) #== (type_ ListType);
              (list_length arr_e) #== size;
              describing_pf arr_e;
            ]
          in
          (arr_e, learned)
    in
    match svarr with
    | Arr e ->
        let open Formula.Infix in
        let learned =
          [
            (Expr.typeof e) #== (Expr.type_ ListType);
            (Expr.list_length e) #== size;
          ]
        in
        (e, learned)
    | AllZeros ->
        f_of_all_same ~concrete_single:Expr.zero_i ~describing_pf:zeros_pf
    | AllUndef ->
        f_of_all_same ~concrete_single:(Expr.Lit Undefined)
          ~describing_pf:undefined_pf

  let to_gil_expr ~chunk ~range (svarr : t) : Expr.t Delayed.t =
    let e, learned = to_gil_expr_undelayed ~chunk ~range svarr in
    Delayed.return ~learned e

  let of_gil_expr_exn expr = Arr expr

  (** Only call on Mint8Unsigned arrays *)
  let learn_chunk ~chunk ~size arr =
    let bounds =
      match chunk with
      | Chunk.Uint8 -> Some (0, 255)
      | _ -> None
      (* Should be completed later *)
    in
    let* size = Delayed.reduce size in
    match bounds with
    | None -> Delayed.return ()
    | Some (low, high) -> (
        match arr with
        | Arr (EList e) ->
            let i k = Expr.int k in
            let learned =
              List.concat_map
                (function
                  | Expr.Lit Undefined -> []
                  | x ->
                      let open Formula.Infix in
                      [ (i low) #<= x; x #<= (i high) ])
                e
            in
            Delayed.return ~learned ()
        | Arr e -> (
            match size with
            | Expr.Lit (Int n) ->
                let i k = Expr.int k in
                let learned =
                  List.concat
                    (List.init (Z.to_int n) (fun k ->
                         let x = Expr.list_nth e k in
                         let open Formula.Infix in
                         [ (i low) #<= x; x #<= (i high) ]))
                in
                Delayed.return ~learned ()
            | _ -> Delayed.return ())
        | _ -> Delayed.return ())

  (* type nonrec t = Conc of t list | Abst of Expr.t | AllUndef | AllZeros *)

  let subst ~le_subst t =
    match t with
    | Arr e ->
        let s = le_subst e in
        if s == e then t else Arr s
    | AllUndef -> AllUndef
    | AllZeros -> AllZeros
end

module Infix = struct
  let ( @: ) = SVArray.concat
  let ( ^: ) = SVArray.array_cons
  let ( ^:? ) a b = Option.bind b (fun b -> a ^: b)
end

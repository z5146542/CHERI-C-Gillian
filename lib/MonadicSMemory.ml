module GUtils = Gillian.Utils
module Result = Stdlib.Result
open Gillian.Monadic
open LActions
module DR = Delayed_result
module DO = Delayed_option
open Delayed.Syntax
open Gillian.Symbolic
open Gillian.Gil_syntax
module Logging = Gillian.Logging
module PFS = Gillian.Symbolic.Pure_context
module SS = GUtils.Containers.SS
module SVal = MonadicSVal
module Debugger = Gillian.Debugger
module Recovery_tactic = Gillian.General.Recovery_tactic

(* Some utils first *)

type init_data = unit

let resolve_or_create_loc_name (lvar_loc : Expr.t) : string Delayed.t =
  let open Delayed.Syntax in
  let* loc_name = Delayed.resolve_loc lvar_loc in
  match loc_name with
  | None ->
      let new_loc_name = ALoc.alloc () in
      let learned = [ Formula.Eq (ALoc new_loc_name, lvar_loc) ] in
      Logging.verbose (fun fmt ->
          fmt "Couldn't resolve loc %a, created %s" Expr.pp lvar_loc
            new_loc_name);
      Delayed.return ~learned new_loc_name
  | Some l ->
      Logging.verbose (fun fmt -> fmt "Resolved %a as %s" Expr.pp lvar_loc l);
      Delayed.return l

let expr_of_loc_name loc_name =
  if GUtils.Names.is_aloc_name loc_name then Expr.ALoc loc_name
  else Lit (Loc loc_name)

type vt = Values.t
type st = Subst.t

(* let ga = LActions.ga *)

type err_t =
  | InvalidLocation of Expr.t
  | MissingLocResource of {
      is_store : bool;
      action : LActions.ga;
      loc_name : string;
      ofs_opt : Expr.t option;
      chunk_opt : Chunk.t option;
    }
  | SHeapTreeErr of {
      at_locations : string list;
      sheaptree_err : SHeapTree.err;
    }
[@@deriving show]

let lift_sheaptree_err loc err =
  SHeapTreeErr { at_locations = [ loc ]; sheaptree_err = err }

let resolve_loc_result loc =
  Delayed_result.of_do ~none:(InvalidLocation loc) (Delayed.resolve_loc loc)

(* let make_err ~fixes ?(fc = Formula.False) ?(rvs = []) () =
   { failing_constraint = fc; recovery_values = rvs; fixes } *)

module Mem = struct
  open Delayed.Syntax

  module SMap = GUtils.Prelude.Map.Make (struct
    include String

    let of_yojson = function
      | `String s -> Ok s
      | _ -> Error "string_of_yojson: expected string"

    let to_yojson s = `String s
  end)

  module LastOp = struct
    (** This is used internally to keep track of the last operation done.
    If we're removing something, we check that it was the last thing that we got
    (which should always be the case) and we remove it directly without sat checks,
    since we kept track of paths.
    *)
    type t =
      | GetSingle of string * Expr.t * Chunk.t
      | GetArray of string * Expr.t * Expr.t * Chunk.t
      | GetHole of string * Expr.t * Expr.t
      | GetZeros of string * Expr.t * Expr.t
      | Other
  end

  type t = { map : SHeapTree.t SMap.t; last_op : LastOp.t }

  let of_yojson json =
    Result.map
      (fun map -> { map; last_op = Other })
      (SMap.of_yojson SHeapTree.of_yojson json)

  let to_yojson { map; _ } = SMap.to_yojson SHeapTree.to_yojson map
  let make ~last_op map = { last_op; map }
  let make_other = make ~last_op:Other
  let map_lift_err loc res = DR.map_error res (lift_sheaptree_err loc)
  let empty = { map = SMap.empty; last_op = Other }
  let copy x = x

  let get_tree_res ?(is_store = false) map loc_name ofs_opt chunk_opt =
    DR.of_option
      ~none:
        (MissingLocResource
           { is_store; action = Single; loc_name; ofs_opt; chunk_opt })
      (SMap.find_opt loc_name map)

  let get_or_create_tree map loc_name =
    match SMap.find_opt loc_name map with
    | Some t -> Delayed.return t
    | None -> Delayed.return SHeapTree.empty

  (***** Implementation of local actions *****)

  let alloc ({ map; _ } : t) low high : t * string =
    let loc = ALoc.alloc () in
    let tree = SHeapTree.alloc low high in
    (make_other @@ SMap.add loc tree map, loc)

  (*
  let weak_valid_pointer { map; _ } loc ofs =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    (* The corresponding missing error should be a missing bound *)
    let** tree = get_tree_res map loc_name (Some ofs) None in
    map_lift_err loc_name (SHeapTree.weak_valid_pointer tree ofs)

  let getcurperm { map; _ } loc ofs =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    (* The corresponding missing error should be a missing bound.
       Though getcurperm is only used for pointer validity checking,
       we should move that logic to something more fixable. *)
    let** tree = get_tree_res map loc_name (Some ofs) None in
    map_lift_err loc_name (SHeapTree.get_perm_at tree ofs)

  let drop_perm { map; _ } loc low high new_perm =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name None None in
    let++ new_tree =
      map_lift_err loc_name (SHeapTree.drop_perm tree low high new_perm)
    in
    make_other @@ SMap.add loc_name new_tree map

  *)
  let store { map; _ } loc chunk ofs value =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree =
      get_tree_res ~is_store:true map loc_name (Some ofs) (Some chunk)
    in
    Logging.verbose (fun m -> m "Got inside store -  %a" Expr.pp ofs);
    let++ new_tree =
      map_lift_err loc_name (SHeapTree.store tree chunk ofs value)
    in
    make_other @@ SMap.add loc_name new_tree map

  let load { map; _ } loc chunk ofs =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name (Some ofs) (Some chunk) in
    let++ value, new_tree =
      map_lift_err loc_name (SHeapTree.load tree chunk ofs)
    in
    (value, make_other @@ SMap.add loc_name new_tree map)

  let free { map; _ } loc low high =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name None None in
    let++ new_tree = map_lift_err loc_name (SHeapTree.free tree low high) in
    make_other @@ SMap.add loc_name new_tree map

  let get_single { map; _ } loc ofs chunk =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name (Some ofs) (Some chunk) in
    let++ sval, new_tree =
      map_lift_err loc_name (SHeapTree.get_single tree ofs chunk)
    in
    let last_op = LastOp.GetSingle (loc_name, ofs, chunk) in
    (make ~last_op @@ SMap.add loc_name new_tree map, loc_name, sval)

  let set_single { map; _ } loc ofs chunk sval =
    let open DR.Syntax in
    let* loc_name = resolve_or_create_loc_name loc in
    let* tree = get_or_create_tree map loc_name in
    let++ new_tree =
      map_lift_err loc_name (SHeapTree.set_single tree ofs chunk sval)
    in
    make_other @@ SMap.add loc_name new_tree map

  let rem_single { map; last_op } loc ofs chunk =
    let+ loc_name = Delayed.resolve_loc loc in
    match loc_name with
    | None -> failwith "Executing rem_single with an unkown loc name"
    | Some loc_name ->
        let () =
          match last_op with
          | GetSingle (l, o, c) when l = loc_name && o = ofs && c = chunk -> ()
          | _ ->
              failwith "Executing rem_single, but not after running get_single"
        in
        let new_tree = SHeapTree.rem_last_get (SMap.find loc_name map) in
        let map =
          if SHeapTree.is_empty new_tree then SMap.remove loc_name map
          else SMap.add loc_name new_tree map
        in
        make_other map

  let get_array { map; _ } loc ofs size chunk =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let last_op = LastOp.GetArray (loc_name, ofs, size, chunk) in
    let open Formula.Infix in
    if%sat size #<= (Expr.int 0) then
      DR.ok
        ( make ~last_op map,
          loc_name,
          MonadicSVal.SVArray.empty)
    else
      let** tree = get_tree_res map loc_name (Some ofs) (Some chunk) in
      let++ sarr, new_tree =
        map_lift_err loc_name (SHeapTree.get_array tree ofs size chunk)
      in
      (make ~last_op @@ SMap.add loc_name new_tree map, loc_name, sarr)

  let set_array { map; _ } loc ofs size chunk array =
    let open DR.Syntax in
    let open Formula.Infix in
    if%sat size #<= (Expr.int 0) then DR.ok (make_other map)
    else
      let* loc_name = resolve_or_create_loc_name loc in
      let* tree = get_or_create_tree map loc_name in
      let++ new_tree =
        map_lift_err loc_name
          (SHeapTree.set_array tree ofs size chunk array)
      in
      Logging.tmi (fun m -> m "created tree: %a" SHeapTree.pp new_tree);
      make_other @@ SMap.add loc_name new_tree map

  let rem_array { map; last_op } loc ofs size chunk =
    let+ loc_name = Delayed.resolve_loc loc in
    match loc_name with
    | None -> failwith "Executing rem_array with an unkown loc name"
    | Some loc_name ->
        let () =
          match last_op with
          | GetArray (l, o, s, c)
            when l = loc_name && o = ofs && c = chunk && s = size -> ()
          | _ -> failwith "Executing rem_array, but not after running get_array"
        in
        let new_tree = SHeapTree.rem_last_get (SMap.find loc_name map) in
        let map =
          if SHeapTree.is_empty new_tree then SMap.remove loc_name map
          else SMap.add loc_name new_tree map
        in
        make_other map

  let get_freed { map; _ } loc =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name None None in
    DR.of_result (SHeapTree.get_freed tree) |> map_lift_err loc_name

  let set_freed { map; _ } loc =
    let+ loc_name = resolve_or_create_loc_name loc in
    make_other @@ SMap.add loc_name SHeapTree.freed map

  let rem_freed { map; _ } loc =
    let open DR.Syntax in
    let* loc_name = Delayed.resolve_loc loc in
    match Option.bind loc_name (fun l -> SMap.find_opt l map) with
    | None -> DR.ok (make_other map)
    | Some tree ->
        let loc_name = Option.get loc_name in
        let++ () =
          map_lift_err loc_name (DR.of_result (SHeapTree.get_freed tree))
        in
        make_other @@ SMap.remove loc_name map

  let get_simple ~sheap_getter ~constr_last_op { map; _ } loc low high =
    let open DR.Syntax in
    let open Formula.Infix in
    let** loc_name = resolve_loc_result loc in
    let last_op = constr_last_op loc_name low high in
    if%sat high #<= low then
      DR.ok (make ~last_op map, loc_name)
    else
      let** tree = get_tree_res map loc_name None None in
      let++ new_tree =
        map_lift_err loc_name (sheap_getter tree low high)
      in
      (make ~last_op @@ SMap.add loc_name new_tree map, loc_name)

  let set_simple ~sheap_setter { map; _ } loc low high =
    let open DR.Syntax in
    let open Formula.Infix in
    if%sat high #<= low then DR.ok (make_other map)
    else
      let* loc_name = resolve_or_create_loc_name loc in
      let* tree = get_or_create_tree map loc_name in
      let++ new_tree =
        map_lift_err loc_name (sheap_setter tree low high)
      in
      make_other @@ SMap.add loc_name new_tree map

  let rem_simple ~check { map; last_op } loc low high =
    let+ loc_name = Delayed.resolve_loc loc in
    match loc_name with
    | None -> failwith "Executing rem_simple with an unkown loc name"
    | Some loc_name ->
        let () =
          if not (check last_op loc_name low high) then
            failwith "Executing rem_simple, but not after running get_simple"
        in
        let new_tree = SHeapTree.rem_last_get (SMap.find loc_name map) in
        let map =
          if SHeapTree.is_empty new_tree then SMap.remove loc_name map
          else SMap.add loc_name new_tree map
        in
        make_other map

  let get_hole =
    get_simple ~sheap_getter:SHeapTree.get_hole ~constr_last_op:(fun a b c ->
        GetHole (a, b, c))

  let set_hole = set_simple ~sheap_setter:SHeapTree.set_hole

  let allocate_function { map; _ } loc =
    let tree = SHeapTree.allocated_function in
    SMap.add loc tree map |> make_other

  let rem_hole =
    rem_simple ~check:(fun last_op loc low high ->
        match last_op with
        | LastOp.GetHole (l, b, h) -> l = loc && b = low && h = high
        | _ -> false)

  let get_zeros =
    get_simple ~sheap_getter:SHeapTree.get_zeros ~constr_last_op:(fun a b c ->
        GetZeros (a, b, c))

  let set_zeros = set_simple ~sheap_setter:SHeapTree.set_zeros

  let rem_zeros =
    rem_simple ~check:(fun last_op loc low high ->
        match last_op with
        | LastOp.GetZeros (l, b, h) -> l = loc && b = low && h = high
        | _ -> false)

  let get_bounds { map; _ } loc =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name None None in
    let++ bounds =
      map_lift_err loc_name (DR.of_result (SHeapTree.get_bounds tree))
    in
    (loc_name, bounds)

  let set_bounds { map; _ } loc bounds =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let* tree = get_or_create_tree map loc_name in
    let++ tree_set =
      map_lift_err loc_name (DR.of_result (SHeapTree.set_bounds tree bounds))
    in
    make_other @@ SMap.add loc_name tree_set map

  let rem_bounds { map; _ } loc =
    let open DR.Syntax in
    let** loc_name = resolve_loc_result loc in
    let** tree = get_tree_res map loc_name None None in
    let++ tree_rem =
      map_lift_err loc_name (DR.of_result (SHeapTree.rem_bounds tree))
    in
    make_other @@ SMap.add loc_name tree_rem map

  let move { map; _ } dst_loc dst_ofs src_loc src_ofs sz =
    let open DR.Syntax in
    let open Formula.Infix in
    if%sat sz #== (Expr.int 0) then DR.ok (make_other map)
    else
      let** dst_loc_name = resolve_loc_result dst_loc in
      let** src_loc_name = resolve_loc_result src_loc in
      let** dst_tree = get_tree_res map dst_loc_name (Some dst_ofs) None in
      let** src_tree = get_tree_res map src_loc_name (Some src_ofs) None in
      let++ new_dst_tree =
        DR.map_error (SHeapTree.move dst_tree dst_ofs src_tree src_ofs sz)
          (fun err ->
            SHeapTreeErr
              {
                at_locations = [ dst_loc_name; src_loc_name ];
                sheaptree_err = err;
              })
      in
      make_other @@ SMap.add dst_loc_name new_dst_tree map

  let lvars { map; _ } =
    let open Utils.Containers in
    SMap.fold
      (fun _ tree acc -> SS.union (SHeapTree.lvars tree) acc)
      map SS.empty

  let alocs { map; _ } =
    let open Utils.Containers in
    SMap.fold
      (fun _ tree acc -> SS.union (SHeapTree.alocs tree) acc)
      map SS.empty

  let assertions ~exclude { map; _ } =
    SMap.fold
      (fun loc tree acc ->
        if exclude loc then acc else SHeapTree.assertions ~loc tree @ acc)
      map []

  let pp_full ft mem =
    let open Fmt in
    (Dump.iter_bindings SMap.iter nop string SHeapTree.pp_full) ft mem

  let substitution subst ({ map; last_op } as mem) : (t, SHeapTree.err) DR.t =
    let open DR.Syntax in
    if Subst.domain subst None = Expr.Set.empty then DR.ok mem
    else
      let aloc_subst =
        Subst.fold subst
          (fun l r acc ->
            match l with
            | ALoc aloc -> (aloc, r) :: acc
            | _ -> acc)
          []
      in
      let le_subst = Subst.subst_in_expr subst ~partial:true in
      let sval_subst = SVal.substitution ~le_subst in
      let svarr_subst = SVal.SVArray.subst ~le_subst in
      let subst_tree =
        SHeapTree.substitution ~le_subst ~sval_subst ~svarr_subst
      in
      let substituted = SMap.map subst_tree map in
      let++ map =
        List.fold_left
          (fun acc (old_loc, new_loc) ->
            let** acc in
            Logging.verbose (fun fmt ->
                fmt "SHOULD Merge locs: %s --> %a" old_loc Expr.pp new_loc);
            Logging.tmi (fun fmt -> fmt "IN MEMORY: %a" pp_full acc);
            let new_loc =
              match new_loc with
              | Lit (Loc loc) | ALoc loc -> loc
              | _ ->
                  Fmt.failwith "Heap substitution failed for loc : %a" Expr.pp
                    new_loc
            in
            match SMap.find_opt new_loc acc with
            | Some new_tree -> (
                try
                  let old_tree = SMap.find old_loc acc in
                  let without_old = SMap.remove old_loc acc in
                  Logging.verbose (fun fmt -> fmt "Merging now.");
                  let++ merged = SHeapTree.merge ~new_tree ~old_tree in
                  Logging.verbose (fun fmt -> fmt "Done merging.");
                  SMap.add new_loc merged without_old
                with Not_found -> DR.ok acc)
            | None -> (
                try
                  let tree = SMap.find old_loc acc in
                  DR.ok (SMap.add new_loc tree (SMap.remove old_loc acc))
                with Not_found -> DR.ok acc))
          (DR.ok substituted) aloc_subst
      in
      { map; last_op }

  let pp_normal ~exclude ft mem =
    let is_first = ref true in
    SMap.iter
      (fun loc tree ->
        if not (exclude loc) then (
          if !is_first then is_first := false else Fmt.pf ft "@\n";
          Fmt.pf ft "%s -> @[<v 0>%a@]" loc SHeapTree.pp tree))
      mem

  let pp ~exclude fmt { map = t; _ } =
    if !Config.pp_full_tree then pp_full fmt t else pp_normal ~exclude fmt t
end

type t = { mem : Mem.t ref }

let to_yojson t =
  let { mem } = t in
  Mem.to_yojson !mem

let of_yojson m =
  Result.map
    (fun mem -> { mem = ref mem })
    (Mem.of_yojson m)

(* type action_ret = Success of (t * vt list) | Failure of err_t *)
type action_ret = (t * vt list, err_t) result

let make_branch ~heap ?(rets = []) () = (heap, rets)

(* Init *)

let just_functions genv =
  if !Config.allocated_functions then
    PMap.foldi
      (fun loc def acc ->
        match def with
        | Global_env.FunDef _ -> Mem.allocate_function acc loc
        | GlobVar _ -> acc)
      genv Mem.empty
  else Mem.empty

(*let init genv = { genv; mem = ref (just_functions genv) }*)
let init () = { mem = ref Mem.empty }
let clear _ = { mem = ref Mem.empty }
let copy h = { mem = ref (Mem.copy !(h.mem)) }

let pp_params fmt params =
  let rec aux fmtp = function
    | [] -> ()
    | [ a ] -> Format.fprintf fmt "%a" Expr.pp a
    | a :: r ->
        Format.fprintf fmt "%a, " Expr.pp a;
        aux fmtp r
  in
  Format.fprintf fmt "[%a]" aux params

let fail_ungracefully act_name params =
  failwith (Format.asprintf "Invalid call to %s : %a" act_name pp_params params)

(* Action execution *)

let execute_alloc heap params =
  let open Expr in
  match params with
  | [ EList [ typ; siz ] ] ->
    let mem, loc = Mem.alloc !(heap.mem) zero_i siz in
    (* loc refers to the abstract location of the capability; we fill the 
       rest in cap*)
    let cap = [ ALoc loc; zero_i; zero_i; siz; Lit (Bool true); Lit (Bool true);
                      Lit (Bool true); Lit (Bool true); Lit (Bool true); 
                      Lit (Bool false); Lit (Bool true); one_i ] in
    let result =
      make_branch ~heap:{ mem = ref mem } ~rets:cap ()
    in 
    DR.ok result
  | _ -> fail_ungracefully "alloc" params
(*
let execute_getcurperm heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs ] ->
      let** perm = Mem.getcurperm !(heap.mem) loc ofs in
      let perm_string =
        Expr.Lit (String (ValueTranslation.string_of_permission_opt perm))
      in
      let branch = make_branch ~heap ~rets:[ perm_string ] () in
      DR.ok branch
  | _ -> fail_ungracefully "getcurperm" params

let execute_weak_valid_pointer heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs ] ->
      let** bool = Mem.weak_valid_pointer !(heap.mem) loc ofs in
      let res = Expr.bool bool in
      let branch = make_branch ~heap ~rets:[ res ] () in
      DR.ok branch
  | _ -> fail_ungracefully "weakvalidpointer" params

let execute_drop_perm heap params =
  let open DR.Syntax in
  match params with
  | [ loc; low; high; Expr.Lit (String perm_string) ] ->
      let perm = ValueTranslation.permission_of_string perm_string in
      let++ mem = Mem.drop_perm !(heap.mem) loc low high perm in
      make_branch ~heap:{ mem = ref mem } ~rets:[] ()
  | _ -> fail_ungracefully "drop_perm" params
*)

(* Note to self: we only need the capability and the value in our case
   Note 2: the value also contains information of the type *)
let execute_store heap params =
  let open DR.Syntax in
  let open Formula.Infix in
  let open Expr.Infix in
  match params with
  | [ Expr.EList [ loc; off; base; len; l1; l2; s1; s2; s3; g; t; ofs ]; value ] ->
      (*let cap = Expr.EList [ loc; off; base; len; l1; l2; s1; s2; s3; g; t; ofs ] in*)
      let* sval = SVal.of_gil_expr_exn value in
      if%sat t #== (Expr.bool false)
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.C2Err TagViolation } )
      else if%sat s1 #== (Expr.bool false)
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.C2Err PermitStoreViolation } )
      else if%sat (match sval with | SCap_v cv -> Expr.BinOp (not s2, BAnd, cv.tag) | _ -> Expr.bool false) #== (Expr.bool true)
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.C2Err PermitStoreCapViolation})
      else if%sat (match sval with | SCap_v cv -> Expr.BinOp (Expr.BinOp (not s3, BAnd, cv.tag), BAnd, cv.global) | _ -> Expr.bool false) #== (Expr.bool true)
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.C2Err PermitStoreLocalCapViolation})
      else if%sat (base + len) #< (off + Expr.int (Chunk.size (SVal.typeof sval)))
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.C2Err LengthViolation})
      else if%sat off #< base
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.C2Err LengthViolation})
      else if%sat fnot ((Expr.BinOp (off, IMod, Expr.int (Chunk.align (SVal.typeof sval)))) #== (Expr.int 0)) 
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.C2Err BadAddressViolation})
      else if sval = SUndef
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.LogicErr (Unhandled "Value to be stored is undefined.")})
      else
      let chunk = SVal.typeof sval in
      let++ mem = Mem.store !(heap.mem) loc chunk off sval in
      make_branch ~heap:{ mem = ref mem } ~rets:[] ()
  | _ -> fail_ungracefully "store" params

(* Note: l2 does not throw an exception -- it nullifies the tag bit of the capability when loaded *)
let execute_load heap params =
  let open DR.Syntax in
  let open Formula.Infix in
  let open Expr.Infix in
  match params with
  | [ Expr.EList [ loc; off; base; len; l1; l2; s1; s2; s3; g; t; ofs ]; Expr.Lit (String chunk) ] ->
      (*
      let cap = Expr.EList [ loc; off; base; len; l1; l2; s1; s2; s3; g; t; ofs ] in
      let* cap_gil_value = SVal.to_gil_expr cap in
      *)
      let typ = ValueTranslation.vtype_to_smm_type chunk in
      if%sat t #== (Expr.bool false)
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.C2Err TagViolation } )
      else if%sat l1 #== (Expr.bool false)
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.C2Err PermitLoadViolation } )
      else if%sat (base + len) #< (off + Expr.int (Chunk.size typ))
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.C2Err LengthViolation})
      else if%sat off #< base
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.C2Err LengthViolation})
      else if%sat fnot ((Expr.BinOp (off, IMod, Expr.int (Chunk.size typ))) #== (Expr.int 0)) 
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.C2Err BadAddressViolation})
      else
      let** value_, mem = Mem.load !(heap.mem) loc typ off in
      let** value = match value_ with
        | SVal.SCap_v c -> if%sat l2 #== (Expr.bool false) then DR.ok (SVal.SCap_v { c with tag = Expr.bool false }) else DR.ok value_
        | _ -> DR.ok value_
      in
      let* gil_value = SVal.to_gil_expr value in
      let llist_to_list x =
        match x with
        | Expr.EList l -> l
        | _ -> [x]
      in
      let gil_ret_val = llist_to_list gil_value in
      DR.ok
        (make_branch ~heap:{ mem = ref mem } ~rets:gil_ret_val ())
  | _ -> fail_ungracefully "store" params

let execute_free heap params =
  let open DR.Syntax in
  let open Formula.Infix in
  let open Expr.Infix in
  match params with
  | [ Expr.EList [ loc; off; base; len; l1; l2; s1; s2; s3; g; t; ofs ] ] ->
      let cap = Expr.EList [ loc; off; base; len; l1; l2; s1; s2; s3; g; t; ofs ] in
      let* gil_cap = SVal.of_gil_expr_exn cap in
      if MonadicSVal.sure_is_zero gil_cap
        then let ret_cap = [ loc; off; base; len; l1; l2; s1; s2; s3; g; t; ofs ] in
        let () = Logging.tmi (fun fmt -> fmt "Attempting to free a null capability") in
        DR.ok (make_branch ~heap:{ mem = ref !(heap.mem) } ~rets:ret_cap ())
      else if%sat t #== (Expr.bool false) 
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.C2Err TagViolation } ) 
      else if%sat g #== (Expr.bool true)
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.LogicErr (Unhandled "Attempting to free a global capability!") } )
      else
      if%sat (base + len) #< off
        then DR.error (SHeapTreeErr { at_locations = [""]; sheaptree_err = SHeapTree.LogicErr (Unhandled "Capability offset exceeds top!") } )
      else
      let++ mem = Mem.free !(heap.mem) loc base len in
      let freed_cap = [ loc; off; base; len; l1; l2; s1; s2; s3; g; Lit (Bool false); ofs ] in
      make_branch ~heap:{ mem = ref mem } ~rets:freed_cap ()
  | _ -> fail_ungracefully "free" params

let execute_null heap params =
  let open DR.Syntax in
  match params with 
  | [] -> 
      let* null_cap_gil_val = SVal.to_gil_expr SVal.null_cap in
      let llist_to_list x = match x with
      | Expr.EList y -> y
      | _ -> [x]
      in
      let null_ret = llist_to_list null_cap_gil_val in
      DR.ok (make_branch ~heap:heap ~rets:null_ret ())
  | _ -> failwith "NULL has arguments, which should be impossible." 

let execute_copy heap params =
  let open DR.Syntax in
  match params with
  | [ Expr.EList [ dst_loc; dst_off; dst_base; dst_len; dst_l1; dst_l2; dst_s1;
                   dst_s2; dst_s3; dst_g; dst_t; dst_ofs ];
      Expr.EList [ src_loc; src_off; src_base; src_len; src_l1; src_l2; src_s1;
                   src_s2; src_s3; src_g; src_t; src_ofs ];
      Expr.EList [ _; size ] ] ->
      let++ mem = Mem.move !(heap.mem) dst_loc dst_off src_loc src_off size in
      make_branch ~heap:{ mem = ref mem } ~rets:[] ()
  | _ -> fail_ungracefully "wrong call to execute_move" params

let execute_cast heap params = 
  let open DR.Syntax in 
  match params with
  | [ Expr.Lit (String ctyp) ; Expr.EList [ Expr.Lit (String typ); Expr.Lit (Int siz) ]] ->
    let vtyp = ValueTranslation.c_to_vtypes ctyp in 
    (*TODO: add a function that actually does the casting. *)
    DR.ok (make_branch ~heap:heap ~rets:[ Expr.Lit (String vtyp); Expr.Lit (Int siz) ] ())
  | _ -> failwith "Cast parameters do not match."

let execute_get_single heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; Expr.Lit (String chunk_string) ] ->
      let chunk = ValueTranslation.string_to_chunk chunk_string in
      let** mem, loc_name, sval =
        Mem.get_single !(heap.mem) loc ofs chunk
      in
      let loc_e = expr_of_loc_name loc_name in
      let* sval_e = SVal.to_gil_expr sval in
      DR.ok
        (make_branch
           ~heap:{ mem = ref mem }
           ~rets:
             [
               loc_e;
               ofs;
               Expr.Lit (String chunk_string);
               sval_e;
             ]
           ())
  | _ -> fail_ungracefully "get_single" params

let execute_set_single heap params =
  let open DR.Syntax in
  match params with
  | [
   loc;
   ofs;
   Expr.Lit (String chunk_string);
   sval_e;
  ] ->
      let chunk = ValueTranslation.string_to_chunk chunk_string in
      let* sval = SVal.of_gil_expr_exn sval_e in
      let++ mem = Mem.set_single !(heap.mem) loc ofs chunk sval in
      make_branch ~heap:{ mem = ref mem } ~rets:[] ()
  | _ -> fail_ungracefully "set_single" params

let execute_rem_single heap params =
  let open Delayed.Syntax in
  match params with
  | [ loc; ofs; Expr.Lit (String chunk_string) ] ->
      let chunk = ValueTranslation.string_to_chunk chunk_string in
      let+ mem = Mem.rem_single !(heap.mem) loc ofs chunk in
      Ok (make_branch ~heap:{ mem = ref mem } ~rets:[] ())
  | _ -> fail_ungracefully "rem_single" params

let execute_get_array heap params =
  let open DR.Syntax in
  match params with
  | [ loc; ofs; size; Expr.Lit (String chunk_string) ] ->
      let chunk = ValueTranslation.string_to_chunk chunk_string in
      let** mem, loc_name, array =
        Mem.get_array !(heap.mem) loc ofs size chunk
      in
      let loc_e = expr_of_loc_name loc_name in
      let range = SHeapTree.Range.of_low_chunk_and_size ofs chunk size in
      let* array_e = MonadicSVal.SVArray.to_gil_expr ~chunk ~range array in
      DR.ok
        (make_branch
           ~heap:{ mem = ref mem }
           ~rets:
             [
               loc_e;
               ofs;
               size;
               Expr.Lit (String chunk_string);
               array_e;
             ]
           ())
  | _ -> fail_ungracefully "get_array" params

let execute_set_array heap params =
  let open DR.Syntax in
  match params with
  | [
   loc;
   ofs;
   size;
   Expr.Lit (String chunk_string);
   arr_e;
  ] ->
      let chunk = ValueTranslation.string_to_chunk chunk_string in
      let arr = MonadicSVal.SVArray.of_gil_expr_exn arr_e in
      let++ mem = Mem.set_array !(heap.mem) loc ofs size chunk arr in
      make_branch ~heap:{ mem = ref mem } ~rets:[] ()
  | _ -> fail_ungracefully "set_single" params

let execute_rem_array heap params =
  let open Delayed.Syntax in
  match params with
  | [ loc; ofs; size; Expr.Lit (String chunk_string) ] ->
      let chunk = ValueTranslation.string_to_chunk chunk_string in
      let+ mem = Mem.rem_array !(heap.mem) loc ofs size chunk in
      Ok (make_branch ~heap:{ mem = ref mem } ~rets:[] ())
  | _ -> fail_ungracefully "rem_single" params

let execute_get_simple ~mem_getter ~name heap params =
  let open DR.Syntax in
  match params with
  | [ loc; low; high ] ->
      let** mem, loc_name = mem_getter !(heap.mem) loc low high in
      let loc_e = expr_of_loc_name loc_name in
      DR.ok
        (make_branch
           ~heap:{ mem = ref mem }
           ~rets:[ loc_e; low; high ]
           ())
  | _ -> fail_ungracefully name params

let execute_set_simple ~mem_setter ~name heap params =
  let open DR.Syntax in
  match params with
  | [ loc; low; high; Expr.Lit (String perm_string) ] ->
      let++ mem = mem_setter !(heap.mem) loc low high in
      make_branch ~heap:{ mem = ref mem } ~rets:[] ()
  | _ -> fail_ungracefully name params

let execute_rem_simple ~mem_remover ~name heap params =
  match params with
  | [ loc; low; high ] ->
      let+ mem = mem_remover !(heap.mem) loc low high in
      Ok (make_branch ~heap:{ mem = ref mem } ~rets:[] ())
  | _ -> fail_ungracefully name params

let execute_get_hole =
  execute_get_simple ~mem_getter:Mem.get_hole ~name:"get_hole"
(*
let execute_set_hole =
  execute_set_simple ~mem_setter:Mem.set_hole ~name:"set_hole"

let execute_rem_hole =
  execute_rem_simple ~mem_remover:Mem.rem_hole ~name:"rem_hole"

let execute_get_zeros =
  execute_get_simple ~mem_getter:Mem.get_zeros ~name:"get_zeros"

let execute_set_zeros =
  execute_set_simple ~mem_setter:Mem.set_zeros ~name:"set_zeros"

let execute_rem_zeros =
  execute_rem_simple ~mem_remover:Mem.rem_zeros ~name:"rem_zeros"
*)
let execute_get_freed heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let++ () = Mem.get_freed !(heap.mem) loc in
      make_branch ~heap ~rets:[] ()
  | _ -> fail_ungracefully "get_freed" params

let execute_set_freed heap params =
  match params with
  | [ loc ] ->
      let+ mem = Mem.set_freed !(heap.mem) loc in
      Ok (make_branch ~heap:{ mem = ref mem } ~rets:[] ())
  | _ -> fail_ungracefully "set_freed" params

let execute_rem_freed heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let++ mem = Mem.rem_freed !(heap.mem) loc in
      make_branch ~heap:{ mem = ref mem } ~rets:[] ()
  | _ -> fail_ungracefully "rem_freed" params

let execute_get_bounds heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let++ loc_name, bounds = Mem.get_bounds !(heap.mem) loc in
      let bounds_e =
        match bounds with
        | None -> Expr.Lit Null
        | Some (low, high) -> Expr.EList [ low; high ]
      in
      let loc_e = expr_of_loc_name loc_name in
      make_branch ~heap ~rets:[ loc_e; bounds_e ] ()
  | _ -> fail_ungracefully "get_bounds" params

let execute_set_bounds heap params =
  let open DR.Syntax in
  match params with
  | [ loc; bounds_e ] ->
      let bounds =
        match bounds_e with
        | Expr.EList [ low; high ] -> Some (low, high)
        | Lit (LList [ low; high ]) -> Some (Lit low, Lit high)
        | Lit Null -> None
        | _ -> fail_ungracefully "set_bounds" params
      in
      let++ mem = Mem.set_bounds !(heap.mem) loc bounds in
      make_branch ~heap:{ mem = ref mem } ~rets:[] ()
  | _ -> fail_ungracefully "set_bounds" params

let execute_rem_bounds heap params =
  let open DR.Syntax in
  match params with
  | [ loc ] ->
      let++ mem = Mem.rem_bounds !(heap.mem) loc in
      make_branch ~heap:{ mem = ref mem } ~rets:[] ()
  | _ -> fail_ungracefully "rem_bounds" params
(*
let execute_genvgetdef heap params =
  match params with
  | [ Expr.Lit (Loc loc) ] ->
      let def = Global_env.find_def heap.genv loc in
      let v = Global_env.serialize_def def in
      DR.ok (make_branch ~heap ~rets:[ Expr.Lit (Loc loc); Expr.Lit v ] ())
  | _ -> fail_ungracefully "genv_getdef" params
*)
(* Complete fixes  *)

(* type c_fix_t *)
type c_fix_t =
  | AddSingle of { loc : string; ofs : Expr.t; value : Expr.t; chunk : Chunk.t }
  | AddUnitialized of {
      loc : string;
      low : Expr.t;
      high : Expr.t;
      chunk : Chunk.t;
    }

(* Pretty printing utils *)

let pp_c_fix fmt c_fix =
  match c_fix with
  | AddSingle { loc : string; ofs : Expr.t; value : Expr.t; chunk : Chunk.t } ->
      Fmt.pf fmt "AddSingle(%s, %a, %a, %a)" loc Expr.pp ofs Expr.pp value
        Chunk.pp chunk
  | AddUnitialized
      { loc : string; low : Expr.t; high : Expr.t; chunk : Chunk.t } ->
      Fmt.pf fmt "AddUnitialized(%s, %a, %a, %a)" loc Expr.pp low Expr.pp high
        Chunk.pp chunk

let pp_err fmt (e : err_t) =
  match e with
  | InvalidLocation loc ->
      Fmt.pf fmt "[InvalidLocation] '%a' cannot be resolved as a location"
        Expr.pp loc
  | MissingLocResource { is_store; action; loc_name; ofs_opt; chunk_opt } ->
      Fmt.pf fmt
        "[MissingLocResource] No block associated with location '%s'. \
         Associated data: \n\
        \ * is_store: '%B'\n\
        \ * location: '%s'\n\
        \ * core_pred: %s\n\
        \ * value: %a \n\
        \ * chunk: %a \n"
        loc_name is_store loc_name (LActions.str_ga action)
        (Fmt.Dump.option Expr.pp) ofs_opt (Fmt.Dump.option Chunk.pp) chunk_opt
  | SHeapTreeErr { at_locations; sheaptree_err } ->
      Fmt.pf fmt "[SHeapTreeErr] Tree at location%a raised: <%a>"
        (fun fmt l ->
          match l with
          | [ s ] -> Fmt.pf fmt " '%s'" s
          | l -> Fmt.pf fmt "s %a" (Fmt.Dump.list Fmt.string) l)
        at_locations SHeapTree.pp_err sheaptree_err

(* let str_of_err e = Format.asprintf "%a" pp_err e *)

let pp fmt h =
  let exclude loc = false in
  (*
  let exclude loc =
    try
      match PMap.find loc h.genv with
      | Global_env.FunDef _ -> true
      | _ -> false
    with Not_found -> false
  in
  *)
  Format.fprintf fmt "@[%a@]" (Mem.pp ~exclude) !(h.mem)

let pp_by_need (_ : SS.t) fmt h = pp fmt h
let get_print_info _ _ = (SS.empty, SS.empty)

(* let str_noheap _ = "NO HEAP PRINTED" *)

(*
let lift_res res =
  match res with
  | Ok a -> Success a
  | Error e -> Failure e
*)
let pp_branch fmt branch =
  let _, values = branch in
  Fmt.pf fmt "Returns: %a@.(Ignoring heap)" (Fmt.Dump.list Expr.pp) values

let lift_dr_and_log res =
  let pp_res = Fmt.Dump.result ~ok:pp_branch ~error:pp_err in
  Delayed.map res (fun res ->
      Logging.verbose (fun fmt -> fmt "Resulting in: %a" pp_res res);
      res)

(* Actual action execution *)
(*
let filter_errors dr =
  Delayed.bind dr (fun res ->
      match res with
      | Ok _ -> Delayed.return res
      | Error err ->
          Logging.tmi (fun m -> m "Filtering error branch: %a" pp_err err);
          Delayed.vanish ())
*)
let execute_action ~action_name heap params =
  Logging.verbose (fun fmt ->
      fmt "Executing action %s with params %a" action_name pp_params params);
  Logging.tmi (fun fmt -> fmt "Current heap : %a" pp heap);
  let open LActions in
  let a_ret =
    match str_to_mem_ac action_name with
    | Alloc -> execute_alloc heap params
    (*
    | AMem GetCurPerm -> execute_getcurperm heap params
    | AMem WeakValidPointer -> execute_weak_valid_pointer heap params
    | AMem DropPerm -> execute_drop_perm heap params
    *)
    | Store -> execute_store heap params
    | Load -> execute_load heap params
    | Free -> execute_free heap params
    | Null -> execute_null heap params
    | Copy -> execute_copy heap params
    | Cast -> execute_cast heap params
    (*
    | Move -> execute_move heap params
    | AMem GetSingle -> execute_get_single heap params
    | AMem SetSingle -> execute_set_single heap params |> filter_errors
    | AMem RemSingle -> execute_rem_single heap params
    | AMem GetArray -> execute_get_array heap params
    | AMem SetArray -> execute_set_array heap params |> filter_errors
    | AMem RemArray -> execute_rem_array heap params
    | AMem GetBounds -> execute_get_bounds heap params
    | AMem SetBounds -> execute_set_bounds heap params |> filter_errors
    | AMem RemBounds -> execute_rem_bounds heap params
    | AMem GetHole -> execute_get_hole heap params
    | AMem SetHole -> execute_set_hole heap params |> filter_errors
    | AMem RemHole -> execute_rem_hole heap params
    | AMem GetZeros -> execute_get_zeros heap params
    | AMem SetZeros -> execute_set_zeros heap params |> filter_errors
    | AMem RemZeros -> execute_rem_zeros heap params
    | AMem GetFreed -> execute_get_freed heap params
    | AMem SetFreed -> execute_set_freed heap params |> filter_errors
    | AMem RemFreed -> execute_rem_freed heap params
    | AGEnv GetDef -> execute_genvgetdef heap params
    *)
  in
  lift_dr_and_log a_ret

(* LActions static *)
let id = (fun x -> x)
let ga_to_setter = id
let ga_to_getter = id
let ga_to_deleter = id

(* Serialization and operations *)
let substitution_in_place subst heap =
  let open Delayed.Syntax in
  let () = Logging.verbose (fun fmt -> fmt "ASDFCurrent heap : %a" pp heap) in
  let { mem } = heap in
  let+ mem = Mem.substitution subst !mem in
  match mem with
  | Ok mem -> { mem = ref mem }
  | Error e -> Fmt.failwith "Error in substitution: %a" SHeapTree.pp_err e

let fresh_val _ = Expr.LVar (LVar.alloc ())

let clean_up ?(keep = Expr.Set.empty) _ : Expr.Set.t * Expr.Set.t =
  (Expr.Set.empty, keep)

let lvars heap = Mem.lvars !(heap.mem)
let alocs heap = Mem.alocs !(heap.mem)

let assertions ?to_keep:_ heap =
  let exclude loc = false
  (*
    try
      match PMap.find loc heap.genv with
      | Global_env.FunDef _ -> true
      | _ -> false
    with Not_found -> false
  *)
  in
  let mem_asrts = Mem.assertions ~exclude !(heap.mem) in
  mem_asrts

let mem_constraints _heap = []

let is_overlapping_asrt = (fun _ -> false)

module Lift = struct
  open Debugger.Utils

  let get_store_vars store =
    store
    |> List.map (fun (var, value) : Variable.t ->
           let value = Fmt.to_to_string (Fmt.hbox Expr.pp) value in
           Variable.create_leaf var value ())
    |> List.sort (fun (v : Variable.t) w -> Stdlib.compare v.name w.name)

  let make_node ~get_new_scope_id ~variables ~name ~value ?(children = []) () :
      Variable.t =
    let var_ref = get_new_scope_id () in
    let () =
      match children with
      | [] -> ()
      | _ -> Hashtbl.replace variables var_ref children
    in
    let type_ =
      match children with
      | [] -> None
      | _ -> Some "object"
    in
    { name; value; var_ref; type_ }

  let add_variables
      ~store
      ~(memory : t)
      ~is_gil_file:_
      ~get_new_scope_id
      variables =
    let mem = !(memory.mem) in
    (* Store first *)
    let store_id = get_new_scope_id () in
    let store_vars = get_store_vars store in
    Hashtbl.replace variables store_id store_vars;
    (* And finally heap*)
    let make_node = make_node ~get_new_scope_id ~variables in
    let heap_id = get_new_scope_id () in
    let heap_vars =
      Mem.SMap.to_seq mem.map
      |> Seq.map (fun (loc, tree) ->
             SHeapTree.Lift.get_variable ~make_node ~loc tree)
      |> List.of_seq
    in
    Debugger_log.log (fun fmt ->
        fmt "There are %d heap vars" (List.length heap_vars));
    Hashtbl.replace variables heap_id heap_vars;
    Variable.
    [ 
      { id = store_id; name = "Store" };
      { id = heap_id; name = "Heap" }
      (* { id = genv_id; name = "Global Environment" }; *);
    ]
end

(** Things defined for BiAbduction *)

(* let get_recovery_vals _ = failwith "not implemented yet" *)



let get_recovery_tactic _ err =
  let values =
    match err with
    | InvalidLocation e ->
        List.map (fun x -> Expr.LVar x) (SS.elements (Expr.lvars e))
    | MissingLocResource
        { is_store = _; action = _; loc_name; ofs_opt = _; chunk_opt = _ } ->
        [ Expr.loc_from_loc_name loc_name ]
    | SHeapTreeErr { at_locations; _ } ->
        List.map Expr.loc_from_loc_name at_locations
  in
  Recovery_tactic.try_unfold values

let get_failing_constraint _e =
  failwith "Bi-abduction TODO: implement function get_failing_constraint"
(*
let offset_by_chunk low chunk =
  let open Expr.Infix in
  let len = Expr.int (Chunk.size chunk) in
  low + len
*)
let get_fixes  _heap _pfs _gamma _err =
  failwith "Not ready for bi-abduction yet"
  (*
  Logging.verbose (fun m -> m "Getting fixes for error : %a" pp_err err);
  let get_fixes_h is_store loc ofs chunk =
    let open CConstants.VTypes in
    (* let fixes = [] in *)
    let fixes =
      match chunk with
      | Chunk.Mfloat32 ->
          let new_var = LVar.alloc () in
          let set = SS.singleton new_var in
          let new_var_e = Expr.LVar new_var in
          let value = Expr.EList [ Expr.string single_type; new_var_e ] in
          let vtypes = [ (new_var, Type.NumberType) ] in
          [ ([ AddSingle { loc; ofs; value; chunk } ], [], vtypes, set, []) ]
      | Mfloat64 ->
          let new_var = LVar.alloc () in
          let set = SS.singleton new_var in
          let new_var_e = Expr.LVar new_var in
          let value = Expr.EList [ Expr.string float_type; new_var_e ] in
          let vtypes = [ (new_var, Type.NumberType) ] in
          [ ([ AddSingle { loc; ofs; value; chunk } ], [], vtypes, set, []) ]
      | Mint64 ->
          let new_var = LVar.alloc () in
          let set = SS.singleton new_var in
          let new_var_e = Expr.LVar new_var in
          let value = Expr.EList [ Expr.string long_type; new_var_e ] in
          let vtypes = [ (new_var, Type.IntType) ] in
          [ ([ AddSingle { loc; ofs; value; chunk } ], [], vtypes, set, []) ]
      | Mptr ->
          let new_var1 = LVar.alloc () in
          let new_var_e1 = Expr.LVar new_var1 in
          let new_var2 = LVar.alloc () in
          let new_var_e2 = Expr.LVar new_var2 in
          let set = SS.add new_var2 (SS.singleton new_var1) in
          let value = Expr.EList [ new_var_e1; new_var_e2 ] in
          let null_typ =
            if Compcert.Archi.ptr64 then Expr.string long_type
            else Expr.string int_type
          in
          let null_ptr = Expr.EList [ null_typ; Expr.int 0 ] in
          let vtypes =
            [ (new_var1, Type.ObjectType); (new_var2, Type.IntType) ]
          in
          [
            ([ AddSingle { loc; ofs; value; chunk } ], [], vtypes, set, []);
            ( [ AddSingle { loc; ofs; value = null_ptr; chunk } ],
              [],
              [],
              SS.empty,
              [] );
          ]
      | _ ->
          let new_var = LVar.alloc () in
          let set = SS.singleton new_var in
          let new_var_e = Expr.LVar new_var in
          let value = Expr.EList [ Expr.string int_type; new_var_e ] in
          let vtypes = [ (new_var, Type.IntType) ] in
          [ ([ AddSingle { loc; ofs; value; chunk } ], [], vtypes, set, []) ]
    in
    (* Additional fix for store operation to handle case of unitialized memory *)
    let fixes =
      if is_store then
        ( [
            AddUnitialized
              { loc; low = ofs; high = offset_by_chunk ofs chunk; chunk };
          ],
          [],
          [],
          SS.empty,
          [] )
        :: fixes
      else fixes
    in
    fixes
  in
  match err with
  | MissingLocResource
      {
        is_store;
        action = Single;
        loc_name;
        ofs_opt = Some ofs;
        chunk_opt = Some chunk;
      } -> get_fixes_h is_store loc_name ofs chunk
  | InvalidLocation loc ->
      let new_loc = ALoc.alloc () in
      let new_expr = Expr.ALoc new_loc in
      [ ([], [ Formula.Eq (new_expr, loc) ], [], SS.empty, []) ]
  | SHeapTreeErr
      {
        at_locations;
        sheaptree_err = MissingResource (Fixable { is_store; low; chunk });
      } -> (
      match at_locations with
      | [ loc ] -> get_fixes_h is_store loc low chunk
      | _ ->
          Logging.verbose (fun m ->
              m "SHeapTreeErr: Unsupported for more than 1 location");
          [])
  | _ -> []
  *)
let apply_fix heap fix =
  failwith "Not ready for bi-abdcution"
(*
  let open DR.Syntax in
  match fix with
  | AddSingle { loc; ofs; value; chunk } ->
      Logging.verbose (fun m ->
          m
            "Applying AddSingle fix for error\n\
            \ * location: '%s'\n\
            \ * core_pred: %a\n\
            \ * value: %a \n\
            \ * chunk: %a \n"
            loc Expr.pp ofs Expr.pp value Chunk.pp chunk);
      let loc = Expr.loc_from_loc_name loc in
      let* sval = SVal.of_gil_expr_exn value in
      let++ mem = Mem.set_single !(heap.mem) loc ofs chunk sval in
      { mem = ref mem }
  | AddUnitialized { loc; low; high; chunk } ->
      Logging.verbose (fun m ->
          m
            "Applying AddUnitialized fix for error\n\
            \ * location: '%s'\n\
            \ * low: %a\n\
            \ * high: %a \n\
            \ * chunk: %a \n"
            loc Expr.pp low Expr.pp high Chunk.pp chunk);
      let loc = Expr.loc_from_loc_name loc in
      let++ mem = Mem.set_hole !(heap.mem) loc low high in
      { mem = ref mem }
*)

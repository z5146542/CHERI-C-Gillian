open Gillian.Concrete
module Literal = Gillian.Gil_syntax.Literal
module Expr = Gillian.Gil_syntax.Expr
open CHERI_C_Memory_Model
module CHERI = CHERI_C_Concrete_Memory_Model
open ValueTranslation
open LActions
(* ************** *)
(* First, we require memory action types *)
(*
type mem_ac =
  | Alloc
  | Free
  | Load
  | Store
  | Copy
  | Cast
  | Null

let mem_ac_to_str = function
  | Alloc -> "alloc"
  | Free  -> "free"
  | Load  -> "load"
  | Store -> "store"
  | Copy  -> "memcpy"
  | Cast  -> "cast_val"
  | Null  -> "NULL"

let str_to_mem_ac = function
  | "alloc" -> Alloc
  | "free"  -> Free
  | "load"  -> Load
  | "store" -> Store
  | "memcpy"-> Copy
  | "cast_val" -> Cast
  | "NULL"  -> Null
  | err     -> failwith ("Unknown memory action : " ^ err)
*)

(* Next, we need a way to convert GIL values and MSpec values *)


(* ************** *)

type vt = Values.t

type st = Subst.t
type err_t = CHERI.errtype
type init_data = unit

type t = { mem : unit CHERI.heap_ext }

let empty = { mem = CHERI.init_heap }

let init () = empty

let copy x = x
type action_ret = ASucc of (t * vt list) | AFail of err_t list

let execute_alloc heap params =
  let open Literal in
  let open VTypes in
  let open More_Word_Library in
    match params with
    | [ LList [ String typ ; Int siz ] ] ->
      let res = CHERI.alloc heap.mem true (Arith.nat_of_integer siz) in
      (match res with
      | Error e -> AFail [e]
      | Success (memout, cap) -> ASucc ({ mem = memout }, llist_to_list (mm_to_gil (Cap_v cap))))
    | _ -> failwith "FAIL: alloc arguments does not match"

let execute_free heap params =
  let open Literal in
  let open VTypes in
  let open More_Word_Library in
    match params with
    | [ LList [ Loc block_id ; Int offset ; Int base ; Int len ; Bool perm_load ;
                Bool perm_cap_load ; Bool perm_store ; Bool perm_cap_store ;
                Bool perm_cap_store_local ; Bool perm_global ; Bool tag ; Int offsiz ] ] ->
      let gil_cap = LList [ Loc block_id ; Int offset ; Int base ; Int len ;
                            Bool perm_load ; Bool perm_cap_load ; Bool perm_store;
                            Bool perm_cap_store ; Bool perm_cap_store_local ;
                            Bool perm_global ; Bool tag ; Int offsiz ] in
      let (Cap_v mm_cap) = gil_to_mm gil_cap in
      let res = CHERI.free heap.mem mm_cap in
      (match res with
      | Error e -> AFail [e]
      | Success (memout, cap) -> ASucc ({ mem = memout }, llist_to_list (mm_to_gil (Cap_v cap))))
    | _ -> failwith "FAIL: free params does not match capability."

let execute_load heap params =
  let open Literal in
  let open VTypes in
  let open More_Word_Library in
    match params with
    | [ LList [ Loc block_id ; Int offset ; Int base ; Int len ; Bool perm_load ;
                Bool perm_cap_load ; Bool perm_store ; Bool perm_cap_store ;
                Bool perm_cap_store_local ; Bool perm_global ; Bool tag ; Int offsiz ] ; String typ ] when
        String.equal typ u8_type
        || String.equal typ s8_type
        || String.equal typ u16_type
        || String.equal typ s16_type
        || String.equal typ u32_type
        || String.equal typ s32_type
        || String.equal typ u64_type
        || String.equal typ s64_type
        || String.equal typ cap_type ->
      let gil_cap = LList [ Loc block_id ; Int offset ; Int base ; Int len ;
                            Bool perm_load ; Bool perm_cap_load ; Bool perm_store ;
                            Bool perm_cap_store ; Bool perm_cap_store_local ;
                            Bool perm_global ; Bool tag ; Int offsiz ] in
      let (Cap_v mm_cap) = gil_to_mm gil_cap in
      let res = CHERI.load heap.mem mm_cap (vtype_to_mm_type typ) in
      (match res with
      | Error e -> AFail [e]
      | Success mval -> ASucc (heap, llist_to_list (mm_to_gil mval)))
    | _ -> failwith "FAIL: load params does not match."

let execute_store heap params =
  let open Literal in
  let open VTypes in
  let open More_Word_Library in
    match params with
    | [ LList [ Loc block_id ; Int offset ; Int base ; Int len ; Bool perm_load ;
                Bool perm_cap_load ; Bool perm_store ; Bool perm_cap_store ;
                Bool perm_cap_store_local ; Bool perm_global ; Bool tag ; Int offsiz ] ; LList value ] ->
      let gil_cap = LList [ Loc block_id ; Int offset ; Int base ; Int len ;
                            Bool perm_load ; Bool perm_cap_load ; Bool perm_store ;
                            Bool perm_cap_store ; Bool perm_cap_store_local ;
                            Bool perm_global ; Bool tag ; Int offsiz ] in
      let (Cap_v mm_cap) = gil_to_mm gil_cap in
      let mval = gil_to_mm (list_to_llist value) in
      let res = CHERI.store heap.mem mm_cap mval in
      (match res with
      | Error e -> AFail [e]
      | Success memout -> ASucc ({ mem = memout }, []))
    | _ -> failwith "FAIL : store params do not match."

let execute_copy heap params =
  let open Literal in
  let open VTypes in
  let open More_Word_Library in
  match params with
  | [ LList [ Loc block_id_1 ; Int offset_1 ; Int base_1 ; Int len_1 ; Bool perm_load_1 ;
                Bool perm_cap_load_1 ; Bool perm_store_1 ; Bool perm_cap_store_1 ;
                Bool perm_cap_store_local_1 ; Bool perm_global_1 ; Bool tag_1 ; Int offsiz_1 ] ;
      LList [ Loc block_id_2 ; Int offset_2 ; Int base_2 ; Int len_2 ; Bool perm_load_2 ;
                Bool perm_cap_load_2 ; Bool perm_store_2 ; Bool perm_cap_store_2 ;
                Bool perm_cap_store_local_2 ; Bool perm_global_2 ; Bool tag_2 ; Int offsiz_2 ] ;
      LList [ String typ ; Int n ] ] ->
    let gil_cap_1 = LList [ Loc block_id_1 ; Int offset_1 ; Int base_1 ; Int len_1 ; Bool perm_load_1 ;
                Bool perm_cap_load_1 ; Bool perm_store_1 ; Bool perm_cap_store_1 ;
                Bool perm_cap_store_local_1 ; Bool perm_global_1 ; Bool tag_1 ; Int offsiz_1 ] in
    let gil_cap_2 = LList [ Loc block_id_2 ; Int offset_2 ; Int base_2 ; Int len_2 ; Bool perm_load_2 ;
                Bool perm_cap_load_2 ; Bool perm_store_2 ; Bool perm_cap_store_2 ;
                Bool perm_cap_store_local_2 ; Bool perm_global_2 ; Bool tag_2 ; Int offsiz_2 ] in
    let (Cap_v mm_cap_dst) = gil_to_mm gil_cap_1 in
    let (Cap_v mm_cap_src) = gil_to_mm gil_cap_2 in
    let res = CHERI.memcpy heap.mem mm_cap_dst mm_cap_src (Arith.nat_of_integer n) in
    (match res with
    | Error e -> AFail [e]
    | Success memout -> ASucc ({ mem = memout }, [gil_cap_1]))
  | _ -> failwith "Fail : store params do not match." 

let execute_cast heap params =
  let open Literal in
  let open VTypes in
  let open More_Word_Library in
  match params with
  | [ String ctyp ; LList [ String typ ; Int siz ] ] ->
    let vtyp = c_to_vtypes ctyp in
    let res = cast_val vtyp siz in
    ASucc (heap, [ String vtyp ; Int res ])
  | _ -> failwith "FAIL: cast params do not match."

let execute_null heap params =
  match params with
  | [] -> ASucc (heap, llist_to_list (mm_to_gil (Cap_v CHERI.null_capability)))
  | _ -> failwith "FAIL: NULL has arguments. Check file.log"

let execute_action name heap params =
  let action = str_to_mem_ac name in
  match action with
  | Alloc -> execute_alloc heap params
  | Free  -> execute_free  heap params
  | Load  -> execute_load  heap params
  | Store -> execute_store heap params
  | Copy  -> execute_copy  heap params
  | Cast  -> execute_cast  heap params
  | Null  -> execute_null  heap params

let pp_mem fmt mem =
  let open CHERI in
  Format.fprintf fmt "{@[<v 2>@\nnext curr: %i\
                              @\nunfreed memory: %i Bytes\
                              @\nunfreed blocks: [%s]@\n@]@\n}"
    (Z.to_int (next_block mem)) 
    (Z.to_int (Arith.integer_of_nat (get_memory_leak_size mem (Arith.nat_of_integer (next_block mem)))))
    (String.concat ", " (List.map Z.to_string (List.rev (get_unfreed_blocks mem (Arith.nat_of_integer (next_block mem))))))

let pp fmt h =
  Format.fprintf fmt "Mem: @[%a@]" pp_mem h.mem

let pp_err fmt t =
  let open CHERI in
  Fmt.string fmt
    (match t with
    | C2Err ce ->
      (match ce with
      | TagViolation -> "Tag Violation"
      | PermitLoadViolation -> "Permit Load Violation"
      | PermitStoreViolation -> "Permit Store Violation"
      | PermitStoreCapViolation -> "Permit Store Capability Violation"
      | PermitStoreLocalCapViolation -> "Permit Store Local Capability Violation"
      | LengthViolation -> "Length Violation"
      | BadAddressViolation -> "Bad Address Violation")
    | LogicErr le ->
      (match le with
      | UseAfterFree -> "Use After Free"
      | BufferOverrun -> "Buffer Overrun"
      | MissingResource -> "Missing Resource"
      | WrongMemVal -> "Wrong Memory Value"
      | MemoryNotFreed -> "Memory Not Freed"
      | Unhandled str -> "Unhandled: " ^ str)
    )


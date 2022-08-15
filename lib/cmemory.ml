open Gillian.Concrete
module Literal = Gillian.Gil_syntax.Literal
module Expr = Gillian.Gil_syntax.Expr
open CHERI_Concrete_Memory_Model_Base
module CHERI = CHERI_Memory_Model_Base
open MM_value_convert
open ValueTranslation

(* ************** *)
(* First, we require memory action types *)

type mem_ac =
  | Alloc
  | Free
  | Load
  | Store

let mem_ac_to_str = function
  | Alloc -> "alloc"
  | Free  -> "free"
  | Load  -> "load"
  | Store -> "store"

let str_to_mem_ac = function
  | "alloc" -> Alloc
  | "free"  -> Free
  | "load"  -> Load
  | "store" -> Store
  | err     -> failwith ("Unknown memory action : " ^ err)


(* Next, we need a way to convert GIL values and MSpec values *)


(* ************** *)

type vt = Values.t

type st = Subst.t
type err_t = unit 

type t = { mem : unit CHERI.heap_ext }

let empty = { mem = CHERI.heap_init }

let init () = empty

let copy x = x
type action_ret = ASucc of (t * vt list) | AFail of err_t list

let execute_alloc heap params = 
  let open Literal in 
  let open VTypes in
  let open MSpec_Library in
    match params with
    | [ String typ ] when String.equal typ byte_type 
                          || String.equal typ int_type 
                          || String.equal typ cap_type ->
      let res = CHERI.allocate heap.mem (vtype_to_mm_type typ) in
      (match res with
      | None -> AFail []
      | Some (memout, cap) -> ASucc ({ mem = memout }, llist_to_list (mm_to_gil (Pv cap))))
    | _ -> failwith "FAIL: alloc type does not match"

let execute_free heap params = 
  let open Literal in 
  let open VTypes in
  let open MSpec_Library in
    match params with
    | [ LList [ Bool perm_load ; Bool perm_cap_load ; Bool perm_store ; Bool perm_cap_store ;
                Loc  base ; Int length ; Int address ; Bool tag ] ] -> 
      let gil_cap = LList [ Bool perm_load ; Bool perm_cap_load ; Bool perm_store ;
                            Bool perm_cap_store ; Loc base ; Int length ; Int address ;
                            Bool tag ] in
      let (Pv mm_cap) = gil_to_mm gil_cap in
      let res = CHERI.free heap.mem mm_cap I8 in
      (match res with
      | None -> AFail []
      | Some (memout, cap) -> ASucc ({ mem = memout }, llist_to_list (mm_to_gil (Pv cap))))
    | _ -> failwith "FAIL: free params does not match."

let execute_load heap params = 
  let open Literal in
  let open VTypes in
  let open MSpec_Library in
    match params with
    | [ LList [ Bool perm_load ; Bool perm_cap_load ; Bool perm_store ; Bool perm_cap_store ;
                Loc  base ; Int length ; Int address ; Bool tag ] ; String typ ] when 
        String.equal typ byte_type 
        || String.equal typ int_type 
        || String.equal typ cap_type ->
      let gil_cap = LList [ Bool perm_load ; Bool perm_cap_load ; Bool perm_store ;
                            Bool perm_cap_store ; Loc base ; Int length ; Int address ;
                            Bool tag ] in
      let (Pv mm_cap) = gil_to_mm gil_cap in
      let res = CHERI.load heap.mem mm_cap (vtype_to_mm_type typ) in
      (match res with
      | None -> AFail []
      | Some mval -> ASucc (heap, llist_to_list (mm_to_gil mval)))
    | _ -> failwith "FAIL: load params does not match."
    
let execute_store heap params =
  let open Literal in 
  let open VTypes in
  let open MSpec_Library in
    match params with
    | [ LList [ Bool perm_load ; Bool perm_cap_load ; Bool perm_store ; Bool perm_cap_store ;
                Loc  base ; Int length ; Int address ; Bool tag ] ; LList value ] ->
      let gil_cap = LList [ Bool perm_load ; Bool perm_cap_load ; Bool perm_store ;
                            Bool perm_cap_store ; Loc base ; Int length ; Int address ;
                            Bool tag ] in
      let (Pv mm_cap) = gil_to_mm gil_cap in
      let mval = gil_to_mm (list_to_llist value) in
      let res = CHERI.store heap.mem mm_cap mval in
      (match res with
      | None -> AFail []
      | Some memout -> ASucc ({ mem = memout }, []))
    | _ -> failwith "FAIL : store params do not match."

let execute_action name heap params = 
  let action = str_to_mem_ac name in
  match action with 
  | Alloc -> execute_alloc heap params
  | Free  -> execute_free  heap params 
  | Load  -> execute_load  heap params
  | Store -> execute_store heap params 
  
let pp_mem fmt mem = 
  let open CHERI in
  let open Fat_Capability in 
  Format.fprintf fmt "{@[<v 2>@\nnext curr: %i@]@\n}" (to_int (curr mem))

let pp fmt h = 
  Format.fprintf fmt "Mem: @[%a@]" pp_mem h.mem

let pp_err _ _ = ()

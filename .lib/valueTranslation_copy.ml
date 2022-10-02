module Literal = Gillian.Gil_syntax.Literal
open CHERI_Concrete_Memory_Model_Base
open MM_value_convert

module VTypes = struct 
  let byte_type = "uint8"
  let int_type = "uint64"
  let cap_type = "cap256"
end ;;

let rec vtype_to_mm_type str =
  let open VTypes in
  let open MSpec_Library in
  if str = VTypes.byte_type then I8
  else if str = VTypes.int_type then I64
  else if str = VTypes.cap_type then Ptr I64
  else failwith "FAIL: VType does not match any of the MM types."

let gil_to_mm gil_value = 
  let open Literal in
  let open MSpec_Library in
  let open Fat_Capability in
  let open CHERI_Memory_Model_Base in
  match gil_value with 
  | LList [ String typ ; Int byteval ] when String.equal typ VTypes.byte_type ->
      Bv (word8_of_integer byteval)
  | LList [ String typ ; Int intval ] when String.equal typ VTypes.int_type ->
      Iv (word64_of_integer intval)
  | LList [ Bool perm_load ;
            Bool perm_cap_load ;
            Bool perm_store ;
            Bool perm_cap_store ;
            Loc base ;
            Int length ;
            Int address ;
            Bool tag ] ->
      Pv (
          Pre_capability_ext (
              perm_load,
              perm_cap_load,
              perm_store,
              perm_cap_store,
              of_int (Z.to_int (Z.of_string base)),
              of_int (Z.to_int address),
          Capability_ext (
              of_int (Z.to_int length),
              tag,
          ()))
      )
  | _ -> failwith "gil_to_mm: no matches." ;;

let mm_to_gil mm_value = 
  let open Literal in
  let open MSpec_Library in
  let open Fat_Capability in
  let open CHERI_Memory_Model_Base in
  match mm_value with
  | Bv bv -> LList [ String VTypes.byte_type ; Int (integer_of_word8 bv) ]
  | Iv iv -> LList [ String VTypes.int_type  ; Int (integer_of_word64 iv) ]
  | Pv cv -> LList [ Bool (perm_load cv) ;
                     Bool (perm_cap_load cv) ;
                     Bool (perm_store cv) ;
                     Bool (perm_cap_store cv) ;
                     Loc  (Z.to_string (Z.of_int (to_int (base cv)))) ;
                     Int  (Z.of_int (to_int (len cv))) ;
                     Int  (Z.of_int (to_int (addr cv))) ;
                     Bool (tag cv) ] ;;

let llist_to_list llist =
  let open Literal in 
  match llist with
  | LList xs -> xs
  | _ -> [] ;;

let list_to_llist xs =
  Literal.LList xs ;;

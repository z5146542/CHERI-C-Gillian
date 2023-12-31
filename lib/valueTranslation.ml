module Literal = Gillian.Gil_syntax.Literal
open CHERI_C_Memory_Model

module VTypes = struct
  let u8_type = "uint8"
  let s8_type = "int8"
  let u16_type = "uint16"
  let s16_type = "int16"
  let u32_type = "uint32"
  let s32_type = "int32"
  let u64_type = "uint64"
  let s64_type = "int64"
  let cap_type = "cap"
end ;;

let chunk_to_string = Chunk.to_string
let string_to_chunk = Chunk.of_string

let c_to_vtypes s =
  let open VTypes in
  match s with
  | "unsigned char" -> u8_type
  | "signed char" -> s8_type
  | "unsigned short int" -> u16_type
  | "signed short int" -> s16_type
  | "unsigned int" -> u32_type
  | "signed int" -> s32_type
  | "unsigned long int" -> u64_type
  | "signed long int" -> s64_type
  | u8_type -> u8_type
  | s8_type -> s8_type
  | u16_type -> u16_type
  | s16_type -> s16_type
  | u32_type -> u32_type
  | s32_type -> s32_type
  | u64_type -> u64_type
  | s64_type -> s64_type
  | _ -> failwith "FAIL: conversion of c type to vtype failed."

let rec vtype_to_mm_type str =
  let open VTypes in
  let open Preliminary_Library in
  if str = u8_type then Uint8
  else if str = s8_type then Sint8
  else if str = u16_type then Uint16
  else if str = s16_type then Sint16
  else if str = u32_type then Uint32
  else if str = s32_type then Sint32
  else if str = u64_type then Uint64
  else if str = s64_type then Sint64
  else if str = cap_type then Cap
  else failwith "FAIL: VType does not match any of the MM types."

let rec vtype_to_smm_type str =
  let open VTypes in
  let open Chunk in
  if str = u8_type then Uint8
  else if str = s8_type then Sint8
  else if str = u16_type then Uint16
  else if str = s16_type then Sint16
  else if str = u32_type then Uint32
  else if str = s32_type then Sint32
  else if str = u64_type then Uint64
  else if str = s64_type then Sint64
  else if str = cap_type then Cap
  else failwith "FAIL: VType does not match any of the Symbolic MM types."

let gil_to_mm gil_value =
  let open Literal in
  let open Preliminary_Library in
  let open CHERI_C_Concrete_Memory_Model in
  match gil_value with
  | LList [ String typ ; Int ival ] when String.equal typ VTypes.u8_type ->
      Uint8_v (word8_of_integer ival)
  | LList [ String typ ; Int ival ] when String.equal typ VTypes.s8_type ->
      Sint8_v (sword8_of_integer ival)
  | LList [ String typ ; Int ival ] when String.equal typ VTypes.u16_type ->
      Uint16_v (word16_of_integer ival)
  | LList [ String typ ; Int ival ] when String.equal typ VTypes.s16_type ->
      Sint16_v (sword16_of_integer ival)
  | LList [ String typ ; Int ival ] when String.equal typ VTypes.u32_type ->
      Uint32_v (word32_of_integer ival)
  | LList [ String typ ; Int ival ] when String.equal typ VTypes.s32_type ->
      Sint32_v (sword32_of_integer ival)
  | LList [ String typ ; Int ival ] when String.equal typ VTypes.u64_type ->
      Uint64_v (word64_of_integer ival)
  | LList [ String typ ; Int ival ] when String.equal typ VTypes.s64_type ->
      Sint64_v (sword64_of_integer ival)
  | LList [ Loc block_id ;
            Int offset ;
            Int base ;
            Int len ;
            Bool perm_load ;
            Bool perm_cap_load ;
            Bool perm_store ;
            Bool perm_cap_store ;
            Bool perm_cap_store_local ;
            Bool perm_global ;
            Bool tag ;
            Int offsiz ] ->
      Cap_v (
         Mem_capability_ext (
            Z.of_string block_id,
            Arith.int_of_nat (Arith.nat_of_integer offset),
            Arith.nat_of_integer base,
            Arith.nat_of_integer len,
            perm_load,
            perm_cap_load,
            perm_store,
            perm_cap_store,
            perm_cap_store_local,
            perm_global,
         Capability_ext (
            tag,
            ()
         ))
      )
  | LList [ Loc block_id ;
            Int offset ;
            Int base ;
            Int len ;
            Bool perm_load ;
            Bool perm_cap_load ;
            Bool perm_store ;
            Bool perm_cap_store ;
            Bool perm_cap_store_local ;
            Bool perm_global ;
            Bool tag ;
            Int frag_nth ;
            Int offsiz ] ->
      Cap_v_frag (
         (Mem_capability_ext (
            Z.of_string block_id,
            Arith.int_of_nat (Arith.nat_of_integer offset),
            Arith.nat_of_integer base,
            Arith.nat_of_integer len,
            perm_load,
            perm_cap_load,
            perm_store,
            perm_cap_store,
            perm_cap_store_local,
            perm_global,
         Capability_ext (
            tag,
            ()
         )), Arith.nat_of_integer frag_nth)
      )
  | Undefined -> Undef
  | _ -> failwith "gil_to_mm: no matches." ;;

let mm_to_gil mm_value =
  let open Literal in
  let open Preliminary_Library in
  let open CHERI_C_Concrete_Memory_Model in
  match mm_value with
  | Uint8_v v -> LList [ String VTypes.u8_type ; Int (integer_of_word8 v) ]
  | Sint8_v v -> LList [ String VTypes.s8_type ; Int (integer_of_sword8 v) ]
  | Uint16_v v -> LList [ String VTypes.u16_type ; Int (integer_of_word16 v) ]
  | Sint16_v v -> LList [ String VTypes.s16_type ; Int (integer_of_sword16 v) ]
  | Uint32_v v -> LList [ String VTypes.u32_type ; Int (integer_of_word32 v) ]
  | Sint32_v v -> LList [ String VTypes.s32_type ; Int (integer_of_sword32 v) ]
  | Uint64_v v -> LList [ String VTypes.u64_type ; Int (integer_of_word64 v) ]
  | Sint64_v v -> LList [ String VTypes.s64_type ; Int (integer_of_sword64 v) ]
  | Cap_v v -> LList [ Loc (Z.to_string (block_id comp_countable_integer v)) ;
                       Int (Arith.integer_of_int (offset comp_countable_integer v)) ;
                       Int (Arith.integer_of_nat (base comp_countable_integer v)) ;
                       Int (Arith.integer_of_nat (len comp_countable_integer v)) ;
                       Bool (perm_load comp_countable_integer v) ;
                       Bool (perm_store comp_countable_integer v) ;
                       Bool (perm_cap_load comp_countable_integer v) ;
                       Bool (perm_cap_store comp_countable_integer v) ;
                       Bool (perm_cap_store_local comp_countable_integer v) ;
                       Bool (perm_global comp_countable_integer v) ;
                       Bool (tag comp_countable_integer v) ;
                       Int Z.one ]
  | Cap_v_frag (v, n) ->
               LList [ Loc (Z.to_string (block_id comp_countable_integer v)) ;
                       Int (Arith.integer_of_int (offset comp_countable_integer v)) ;
                       Int (Arith.integer_of_nat (base comp_countable_integer v)) ;
                       Int (Arith.integer_of_nat (len comp_countable_integer v)) ;
                       Bool (perm_load comp_countable_integer v) ;
                       Bool (perm_store comp_countable_integer v) ;
                       Bool (perm_cap_load comp_countable_integer v) ;
                       Bool (perm_cap_store comp_countable_integer v) ;
                       Bool (perm_cap_store_local comp_countable_integer v) ;
                       Bool (perm_global comp_countable_integer v) ;
                       Bool (tag comp_countable_integer v) ;
                       Int (Arith.integer_of_nat n) ;
                       Int Z.one ]
  | Undef -> Undefined ;;

let llist_to_list llist =
  let open Literal in
  match llist with
  | LList xs -> xs
  | _ -> [] ;;

let list_to_llist xs =
  Literal.LList xs ;;

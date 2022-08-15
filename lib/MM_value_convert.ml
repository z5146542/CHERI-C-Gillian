open CHERI_Concrete_Memory_Model_Base
open CHERI_Memory_Model_Base

(* technically, the following are to_nat. Should use Arith.int instead, but we don't have negative types yet. *)
let rec to_int = function 
  | Arith.Zero_nat -> 0
  | Arith.Suc n    -> succ (to_int n)

let rec to_int64 = function
  | Arith.Zero_nat -> 0L
  | Arith.Suc n    -> Int64.succ (to_int64 n)

let rec of_int n = 
  assert (n >= 0);
  if n = 0 then Arith.Zero_nat else Arith.Suc (of_int (pred n))
 
let rec of_int64 n =
  assert (n >= 0L);
  if n = 0L then Arith.Zero_nat else Arith.Suc (of_int64 (Int64.pred n))

let rec word_to_int (w : Numeral_Type.num1 
                         Numeral_Type.bit0 
                         Numeral_Type.bit0 
                         Numeral_Type.bit0 
                         Numeral_Type.bit0 
                         Numeral_Type.bit0 
                         Numeral_Type.bit0 Word.word) : int =
  to_int (Word.the_nat (Type_Length.len_bit0 
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0
                                 (Type_Length.len_bit0 Type_Length.len_num1))))))
                        w) ;;

let rec int_to_word (i : int) : Numeral_Type.num1
                                Numeral_Type.bit0
                                Numeral_Type.bit0
                                Numeral_Type.bit0
                                Numeral_Type.bit0
                                Numeral_Type.bit0
                                Numeral_Type.bit0 Word.word = 
  Word.of_nat (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0 Type_Length.len_num1)))))) (of_int i);;

let rec word_to_byte (b : Numeral_Type.num1
                          Numeral_Type.bit0
                          Numeral_Type.bit0
                          Numeral_Type.bit0 Word.word) : int = 
  to_int (Word.the_nat (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0 Type_Length.len_num1))) 
                        b) ;;

let rec byte_to_word (b : int) : Numeral_Type.num1
                                 Numeral_Type.bit0
                                 Numeral_Type.bit0
                                 Numeral_Type.bit0 Word.word =
  Word.of_nat (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1))) (of_int b);;

let rec word_to_z (w : Numeral_Type.num1
                       Numeral_Type.bit0
                       Numeral_Type.bit0
                       Numeral_Type.bit0
                       Numeral_Type.bit0
                       Numeral_Type.bit0
                       Numeral_Type.bit0 Word.word) : Z.t = 
  (* Z.of_int (word_to_int w);; *)
  integer_of_word64 w;;

let rec z_to_word (i : Z.t) : Numeral_Type.num1
                              Numeral_Type.bit0
                              Numeral_Type.bit0
                              Numeral_Type.bit0
                              Numeral_Type.bit0
                              Numeral_Type.bit0
                              Numeral_Type.bit0 Word.word =
  (* int_to_word (Z.to_int i);; *)
  word64_of_integer i;;

let rec byte_to_z (b : Numeral_Type.num1
                       Numeral_Type.bit0
                       Numeral_Type.bit0
                       Numeral_Type.bit0 Word.word) : Z.t =
  (* Z.of_int (word_to_byte b);; *)
  integer_of_word8 b;;

let rec z_to_byte (b : Z.t) : Numeral_Type.num1
                              Numeral_Type.bit0
                              Numeral_Type.bit0
                              Numeral_Type.bit0 Word.word =
  (* byte_to_word (Z.to_int b);; *)
  word8_of_integer b;;

(* Isabelle int <--> Zarith ints (should be isomorphic) *)

(* record-based capability types (easier to read and update)  *)

type capability = 
  { c_perm_load      : bool ;
    c_perm_cap_load  : bool ;
    c_perm_store     : bool ;
    c_perm_cap_store : bool ;
    c_base           : int  ;
    c_length         : int  ;
    c_address        : int  ;
    c_tag            : bool ;
  } ;; 

let rec to_cap_rec (isa_cap : unit Fat_Capability.capability_ext Fat_Capability.pre_capability_ext) = 
  let open Fat_Capability in
  { c_perm_load      = perm_load isa_cap      ;
    c_perm_cap_load  = perm_cap_load isa_cap  ;
    c_perm_store     = perm_store isa_cap     ;
    c_perm_cap_store = perm_cap_store isa_cap ;
    c_base           = to_int (base isa_cap)  ;
    c_length         = to_int (len isa_cap)   ;
    c_address        = to_int (addr isa_cap)  ;
    c_tag            = tag isa_cap            ;
  } ;;

let rec of_cap_rec (cml_cap : capability) =
  let open Fat_Capability in
  Pre_capability_ext (
    cml_cap.c_perm_load,
    cml_cap.c_perm_cap_load,
    cml_cap.c_perm_store,
    cml_cap.c_perm_cap_store,
    of_int (cml_cap.c_base),
    of_int (cml_cap.c_address),
  Capability_ext (
    of_int (cml_cap.c_length),
    cml_cap.c_tag,
    ())) ;;

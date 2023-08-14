module Prefix = struct
  let internal_preds = "i__"
end

module Internal_Predicates = struct
  let is_int = Prefix.internal_preds ^ "is_int"
  let is_ptr_to_0 = Prefix.internal_preds ^ "is_ptr_to_0"
  let is_ptr = Prefix.internal_preds ^ "is_ptr"
  let is_ptr_to_0_opt = Prefix.internal_preds ^ "is_ptr_to_0_opt"
  let is_ptr_opt = Prefix.internal_preds ^ "is_ptr_opt"
  let is_ptr_to_int_opt = Prefix.internal_preds ^ "is_ptr_to_int_opt"
  let is_ptr_to_float_opt = Prefix.internal_preds ^ "is_ptr_to_float_opt"
  let is_ptr_to_long_opt = Prefix.internal_preds ^ "is_ptr_to_long_opt"
  let is_ptr_to_single_opt = Prefix.internal_preds ^ "is_ptr_to_single_opt"
  let is_long = Prefix.internal_preds ^ "is_long"
  let is_single = Prefix.internal_preds ^ "is_single"
  let is_float = Prefix.internal_preds ^ "is_float"

  (** Internal value getters *)
  let ptr_to_0_get = Prefix.internal_preds ^ "ptr_to_0"

  let ptr_get = Prefix.internal_preds ^ "ptr"
  let int_get = Prefix.internal_preds ^ "int"
  let single_get = Prefix.internal_preds ^ "single"
  let long_get = Prefix.internal_preds ^ "long"
  let float_get = Prefix.internal_preds ^ "float"

  (* Arrays *)

  let malloced = Prefix.internal_preds ^ "malloced"
  let zeros_ptr_size = Prefix.internal_preds ^ "zeros_ptr_size"
  let undefs_ptr_size = Prefix.internal_preds ^ "undefs_ptr_size"
  let array_ptr = Prefix.internal_preds ^ "array_ptr"

  (* Pointer arithmetic *)

  let ptr_add = Prefix.internal_preds ^ "ptr_add"
end

module Symbolic_Constr = struct
  let symb_int = "symb_int"
  let symb_float = "symb_float"
  let symb_single = "symb_single"
  let symb_long = "symb_long"
end

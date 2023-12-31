(* ************** *)
(* First, we require memory action types *)

type mem_ac =
  | Alloc
  | Free
  | Load
  | Store
  | Copy
  | Cast
  | Null
  | GlobSet
  | LoadG
  | StoreG

let mem_ac_to_str = function
  | Alloc   -> "alloc"
  | Free    -> "free"
  | Load    -> "load"
  | Store   -> "store"
  | Copy    -> "memcpy"
  | Cast    -> "cast_val"
  | Null    -> "NULL"
  | GlobSet -> "set_glob_var"
  | LoadG   -> "loadgv"
  | StoreG  -> "storegv"

let str_to_mem_ac = function
  | "alloc"        -> Alloc
  | "free"         -> Free
  | "load"         -> Load
  | "store"        -> Store
  | "memcpy"       -> Copy
  | "cast_val"     -> Cast
  | "NULL"         -> Null
  | "set_glob_var" -> GlobSet
  | "loadgv"       -> LoadG
  | "storegv"      -> StoreG
  | err     -> failwith ("Unknown memory action : " ^ err)

(** Symbolic and Verification Extensions **)
(* For now, let's try to keep memory actions and in/out operations separate *)

type ga = Single | Array | Hole | Zeros | Bounds | Freed
[@@deriving yojson, show]

let str_ga = function
  | Single -> "mem_single"
  | Array -> "mem_array"
  | Hole -> "mem_hole"
  | Zeros -> "mem_zeros"
  | Bounds -> "mem_bounds"
  | Freed -> "mem_freed"

let ga_from_str = function
  | "mem_single" -> Single
  | "mem_array" -> Array
  | "mem_bounds" -> Bounds
  | "mem_zeros" -> Zeros
  | "mem_hole" -> Hole
  | "mem_freed" -> Freed
  | str -> failwith ("Unknown memory assertion : " ^ str)


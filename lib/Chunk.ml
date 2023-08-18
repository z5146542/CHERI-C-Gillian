(* Idea: get rid of all CompCert calls and replace tau with the types we use in
         the Gillian-CHERI-C memory model. *)
(* CompCert dependencies: CompCert.Archi : replace this with my own, or
   just remove this, as we are fixing on a 64-bit architecture. *)

(* Notes: of_compcert and to_compcert may not be needed *)
(*
type t =
  | Sint8
  | Uint8
  | Sint16
  | Uint16
  | Mint32
  | Mint64
  | Mfloat32
  | Mfloat64
  | Mptr
[@@deriving eq, yojson]
*)

type t =
  | Sint8
  | Uint8
  | Sint16
  | Uint16
  | Sint32
  | Uint32
  | Sint64
  | Uint64
  | Cap
[@@deriving eq, yojson]


(* Physical equality: values should be decoded the same way *)
(*
let phy_equal a b =
  match (a, b) with
  | Sint8, Sint8
  | Uint8, Uint8
  | Sint16, Sint16
  | Uint16, Uint16
  | Mint32, Mint32
  | Mint64, Mint64
  | Mfloat32, Mfloat32
  | Mfloat64, Mfloat64
  | Mptr, Mptr -> true
  | Mint32, Mptr | Mptr, Mint32 -> not Compcert.Archi.ptr64
  | Mint64, Mptr | Mptr, Mint64 -> Compcert.Archi.ptr64
  | _ -> false
*)

let phy_equal a b =
  match (a, b) with
  | Sint8, Sint8
  | Uint8, Uint8
  | Sint16, Sint16
  | Uint16, Uint16
  | Sint32, Sint32
  | Uint32, Uint32
  | Sint64, Sint64
  | Uint64, Uint64
  | Cap, Cap -> true
  | _ -> false

(*
let of_compcert : Compcert.AST.memory_chunk -> t = function
  | Sint8 -> Sint8
  | Uint8 -> Uint8
  | Sint16 -> Sint16
  | Uint16 -> Uint16
  | Mint32 -> Mint32
  | Mint64 -> Mint64
  | Mfloat32 -> Mfloat32
  | Mfloat64 -> Mfloat64
  | Many32 | Many64 -> failwith "Unsupported Concert Chunk Many32 or Many64"

let to_compcert : t -> Compcert.AST.memory_chunk = function
  | Sint8 -> Sint8
  | Uint8 -> Uint8
  | Sint16 -> Sint16
  | Uint16 -> Uint16
  | Mint32 -> Mint32
  | Mint64 -> Mint64
  | Mfloat32 -> Mfloat32
  | Mfloat64 -> Mfloat64
  | Mptr -> if Compcert.Archi.ptr64 then Mint64 else Mint32
*)

let of_string = function
  | "signed char" -> Sint8
  | "unsigned char" -> Uint8
  | "signed short" -> Sint16
  | "unsigned short" -> Uint16
  | "signed int" -> Sint32
  | "unsigned int" -> Uint32
  | "signed long int" -> Sint64
  | "unsigned long int" -> Uint64
  | "cap" -> Cap
  | str -> failwith ("unknown chunk : " ^ str)

let to_string = function
  | Sint8 -> "signed char"
  | Uint8 -> "unsigned char"
  | Sint16 -> "signed short"
  | Uint16 -> "unsigned short"
  | Sint32 -> "signed int"
  | Uint32 -> "unsigned int"
  | Sint64 -> "signed long int"
  | Uint64 -> "unsigned long int"
  | Cap -> "cap"

let pp fmt chunk = Fmt.pf fmt "%s" (to_string chunk)

(*
let type_of = function
  | Mint64 -> Compcert.AST.Tlong
  | Mfloat32 -> Compcert.AST.Tsingle
  | Mfloat64 -> Compcert.AST.Tfloat
  | Mptr ->
      if Compcert.Archi.ptr64 then Compcert.AST.Tlong else Compcert.AST.Tint
  | _ -> Tint
*)
(* size of the chunks ; might be best to just manually pattern match *)
let size chunk =
  match chunk with
  | Sint8 | Uint8 -> 1
  | Sint16 | Uint16 -> 2
  | Sint32 | Uint32 -> 4
  | Sint64 | Uint64 -> 8
  | Cap -> 32

let size_expr chunk = Gil_syntax.Expr.int (size chunk)

let align chunk =
  match chunk with
  | Sint8 | Uint8 -> 1
  | Sint16 | Uint16 -> 2
  | Sint32 | Uint32 -> 4
  | Sint64 | Uint64 -> 8
  | Cap -> 32

(*
let ptr = Mptr

let could_be_ptr = function
  | Mptr -> true
  | Mint64 when Compcert.Archi.ptr64 -> true
  | Mint32 when not Compcert.Archi.ptr64 -> true
  | _ -> false
*)

let cap = Cap
(* rename afterwards to could_be_cap *)
let could_be_cap = function
  | Cap -> true
  | _ -> false

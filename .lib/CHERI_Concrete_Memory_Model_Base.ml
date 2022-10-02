module HOL : sig
  type 'a equal = {equal : 'a -> 'a -> bool}
  val equal : 'a equal -> 'a -> 'a -> bool
  type 'a itself = Type
  val eq : 'a equal -> 'a -> 'a -> bool
end = struct

type 'a equal = {equal : 'a -> 'a -> bool};;
let equal _A = _A.equal;;

type 'a itself = Type;;

let rec eq _A a b = equal _A a b;;

end;; (*struct HOL*)

module Fun : sig
  val id : 'a -> 'a
  val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
  val fun_upd : 'a HOL.equal -> ('a -> 'b) -> 'a -> 'b -> 'a -> 'b
end = struct

let rec id x = (fun xa -> xa) x;;

let rec comp f g = (fun x -> f (g x));;

let rec fun_upd _A f a b = (fun x -> (if HOL.eq _A x a then b else f x));;

end;; (*struct Fun*)

module Map : sig
  val map_of : 'a HOL.equal -> ('a * 'b) list -> 'a -> 'b option
end = struct

let rec map_of _A
  x0 k = match x0, k with
    (l, v) :: ps, k -> (if HOL.eq _A l k then Some v else map_of _A ps k)
    | [], k -> None;;

end;; (*struct Map*)

module Product_Type : sig
  val apsnd : ('a -> 'b) -> 'c * 'a -> 'c * 'b
  val fst : 'a * 'b -> 'a
  val snd : 'a * 'b -> 'b
  val equal_bool : bool -> bool -> bool
end = struct

let rec apsnd f (x, y) = (x, f y);;

let rec fst (x1, x2) = x1;;

let rec snd (x1, x2) = x2;;

let rec equal_bool p pa = match p, pa with p, true -> p
                     | p, false -> not p
                     | true, p -> p
                     | false, p -> not p;;

end;; (*struct Product_Type*)

module Orderings : sig
  type 'a ord = {less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool}
  val less_eq : 'a ord -> 'a -> 'a -> bool
  val less : 'a ord -> 'a -> 'a -> bool
  type 'a preorder = {ord_preorder : 'a ord}
  type 'a order = {preorder_order : 'a preorder}
  type 'a linorder = {order_linorder : 'a order}
  val max : 'a ord -> 'a -> 'a -> 'a
end = struct

type 'a ord = {less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool};;
let less_eq _A = _A.less_eq;;
let less _A = _A.less;;

type 'a preorder = {ord_preorder : 'a ord};;

type 'a order = {preorder_order : 'a preorder};;

type 'a linorder = {order_linorder : 'a order};;

let rec max _A a b = (if less_eq _A a b then b else a);;

end;; (*struct Orderings*)

module Arith : sig
  type num = One | Bit0 of num | Bit1 of num
  type int = Zero_int | Pos of num | Neg of num
  val equal_inta : int -> int -> bool
  val equal_int : int HOL.equal
  val times_inta : int -> int -> int
  type 'a times = {times : 'a -> 'a -> 'a}
  val times : 'a times -> 'a -> 'a -> 'a
  type 'a dvd
  val one_inta : int
  type 'a one = {one : 'a}
  val one : 'a one -> 'a
  val uminus_inta : int -> int
  val minus_inta : int -> int -> int
  val plus_inta : int -> int -> int
  type 'a uminus = {uminus : 'a -> 'a}
  val uminus : 'a uminus -> 'a -> 'a
  type 'a minus = {minus : 'a -> 'a -> 'a}
  val minus : 'a minus -> 'a -> 'a -> 'a
  type 'a zero = {zero : 'a}
  val zero : 'a zero -> 'a
  type 'a plus = {plus : 'a -> 'a -> 'a}
  val plus : 'a plus -> 'a -> 'a -> 'a
  type 'a semigroup_add = {plus_semigroup_add : 'a plus}
  type 'a cancel_semigroup_add
  type 'a ab_semigroup_add
  type 'a cancel_ab_semigroup_add
  type 'a monoid_add =
    {semigroup_add_monoid_add : 'a semigroup_add; zero_monoid_add : 'a zero}
  type 'a comm_monoid_add =
    {ab_semigroup_add_comm_monoid_add : 'a ab_semigroup_add;
      monoid_add_comm_monoid_add : 'a monoid_add}
  type 'a cancel_comm_monoid_add
  type 'a mult_zero = {times_mult_zero : 'a times; zero_mult_zero : 'a zero}
  type 'a semigroup_mult
  type 'a semiring
  type 'a semiring_0 =
    {comm_monoid_add_semiring_0 : 'a comm_monoid_add;
      mult_zero_semiring_0 : 'a mult_zero; semiring_semiring_0 : 'a semiring}
  type 'a semiring_0_cancel
  type 'a group_add =
    {cancel_semigroup_add_group_add : 'a cancel_semigroup_add;
      minus_group_add : 'a minus; monoid_add_group_add : 'a monoid_add;
      uminus_group_add : 'a uminus}
  type 'a ab_group_add
  type 'a ring
  type 'a numeral =
    {one_numeral : 'a one; semigroup_add_numeral : 'a semigroup_add}
  type 'a power
  val power_int : int power
  type 'a zero_neq_one
  val of_bool : 'a zero_neq_one -> bool -> 'a
  val zero_neq_one_int : int zero_neq_one
  val divide_inta : int -> int -> int
  type 'a divide = {divide : 'a -> 'a -> 'a}
  val divide : 'a divide -> 'a -> 'a -> 'a
  val modulo_inta : int -> int -> int
  type 'a modulo =
    {divide_modulo : 'a divide; dvd_modulo : 'a dvd; modulo : 'a -> 'a -> 'a}
  val modulo : 'a modulo -> 'a -> 'a -> 'a
  type 'a monoid_mult
  type 'a semiring_numeral
  type 'a semiring_1
  type 'a semiring_1_cancel
  type 'a neg_numeral =
    {group_add_neg_numeral : 'a group_add; numeral_neg_numeral : 'a numeral}
  type 'a ring_1 =
    {neg_numeral_ring_1 : 'a neg_numeral; ring_ring_1 : 'a ring;
      semiring_1_cancel_ring_1 : 'a semiring_1_cancel}
  val semiring_1_int : int semiring_1
  val ord_int : int Orderings.ord
  type 'a ab_semigroup_mult
  type 'a comm_semiring
  type 'a comm_semiring_0 =
    {comm_semiring_comm_semiring_0 : 'a comm_semiring;
      semiring_0_comm_semiring_0 : 'a semiring_0}
  type 'a comm_semiring_0_cancel
  type 'a comm_monoid_mult
  type 'a comm_semiring_1
  type 'a comm_semiring_1_cancel
  val comm_semiring_0_int : int comm_semiring_0
  type 'a comm_ring
  type 'a comm_ring_1 =
    {comm_ring_comm_ring_1 : 'a comm_ring;
      comm_semiring_1_cancel_comm_ring_1 : 'a comm_semiring_1_cancel;
      ring_1_comm_ring_1 : 'a ring_1}
  type 'a semiring_modulo
  type 'a semiring_parity
  type 'a ring_parity =
    {semiring_parity_ring_parity : 'a semiring_parity;
      comm_ring_1_ring_parity : 'a comm_ring_1}
  val semiring_parity_int : int semiring_parity
  val ring_parity_int : int ring_parity
  type 'a algebraic_semidom
  type 'a semidom_modulo
  val semidom_modulo_int : int semidom_modulo
  type nat = Zero_nat | Suc of nat
  val equal_nata : nat -> nat -> bool
  val equal_nat : nat HOL.equal
  val less_nat : nat -> nat -> bool
  val linorder_nat : nat Orderings.linorder
  val plus_nat : nat -> nat -> nat
  val one_nat : nat
  val nat_of_num : num -> nat
  val nat : int -> nat
  val minus_nat : nat -> nat -> nat
  val dvd : 'a HOL.equal * 'a semidom_modulo -> 'a -> 'a -> bool
  val power : 'a power -> 'a -> nat -> 'a
  val abs_int : int -> int
  val of_nat : 'a semiring_1 -> nat -> 'a
  val times_nat : nat -> nat -> nat
  val int_of_integer : Z.t -> int
  val integer_of_int : int -> Z.t
  val modulo_nat : nat -> nat -> nat
end = struct

type num = One | Bit0 of num | Bit1 of num;;

let rec equal_num x0 x1 = match x0, x1 with Bit0 x2, Bit1 x3 -> false
                    | Bit1 x3, Bit0 x2 -> false
                    | One, Bit1 x3 -> false
                    | Bit1 x3, One -> false
                    | One, Bit0 x2 -> false
                    | Bit0 x2, One -> false
                    | Bit1 x3, Bit1 y3 -> equal_num x3 y3
                    | Bit0 x2, Bit0 y2 -> equal_num x2 y2
                    | One, One -> true;;

type int = Zero_int | Pos of num | Neg of num;;

let rec equal_inta x0 x1 = match x0, x1 with Neg k, Neg l -> equal_num k l
                     | Neg k, Pos l -> false
                     | Neg k, Zero_int -> false
                     | Pos k, Neg l -> false
                     | Pos k, Pos l -> equal_num k l
                     | Pos k, Zero_int -> false
                     | Zero_int, Neg l -> false
                     | Zero_int, Pos l -> false
                     | Zero_int, Zero_int -> true;;

let equal_int = ({HOL.equal = equal_inta} : int HOL.equal);;

let rec plus_num
  x0 x1 = match x0, x1 with Bit1 m, Bit1 n -> Bit0 (plus_num (plus_num m n) One)
    | Bit1 m, Bit0 n -> Bit1 (plus_num m n)
    | Bit1 m, One -> Bit0 (plus_num m One)
    | Bit0 m, Bit1 n -> Bit1 (plus_num m n)
    | Bit0 m, Bit0 n -> Bit0 (plus_num m n)
    | Bit0 m, One -> Bit1 m
    | One, Bit1 n -> Bit0 (plus_num n One)
    | One, Bit0 n -> Bit1 n
    | One, One -> Bit0 One;;

let rec times_num
  m n = match m, n with
    Bit1 m, Bit1 n -> Bit1 (plus_num (plus_num m n) (Bit0 (times_num m n)))
    | Bit1 m, Bit0 n -> Bit0 (times_num (Bit1 m) n)
    | Bit0 m, Bit1 n -> Bit0 (times_num m (Bit1 n))
    | Bit0 m, Bit0 n -> Bit0 (Bit0 (times_num m n))
    | One, n -> n
    | m, One -> m;;

let rec times_inta k l = match k, l with Neg m, Neg n -> Pos (times_num m n)
                     | Neg m, Pos n -> Neg (times_num m n)
                     | Pos m, Neg n -> Neg (times_num m n)
                     | Pos m, Pos n -> Pos (times_num m n)
                     | Zero_int, l -> Zero_int
                     | k, Zero_int -> Zero_int;;

type 'a times = {times : 'a -> 'a -> 'a};;
let times _A = _A.times;;

type 'a dvd = {times_dvd : 'a times};;

let times_int = ({times = times_inta} : int times);;

let dvd_int = ({times_dvd = times_int} : int dvd);;

let one_inta : int = Pos One;;

type 'a one = {one : 'a};;
let one _A = _A.one;;

let one_int = ({one = one_inta} : int one);;

let rec uminus_inta = function Neg m -> Pos m
                      | Pos m -> Neg m
                      | Zero_int -> Zero_int;;

let rec bitM = function One -> One
               | Bit0 n -> Bit1 (bitM n)
               | Bit1 n -> Bit1 (Bit0 n);;

let rec dup = function Neg n -> Neg (Bit0 n)
              | Pos n -> Pos (Bit0 n)
              | Zero_int -> Zero_int;;

let rec minus_inta k l = match k, l with Neg m, Neg n -> sub n m
                     | Neg m, Pos n -> Neg (plus_num m n)
                     | Pos m, Neg n -> Pos (plus_num m n)
                     | Pos m, Pos n -> sub m n
                     | Zero_int, l -> uminus_inta l
                     | k, Zero_int -> k
and sub
  x0 x1 = match x0, x1 with
    Bit0 m, Bit1 n -> minus_inta (dup (sub m n)) one_inta
    | Bit1 m, Bit0 n -> plus_inta (dup (sub m n)) one_inta
    | Bit1 m, Bit1 n -> dup (sub m n)
    | Bit0 m, Bit0 n -> dup (sub m n)
    | One, Bit1 n -> Neg (Bit0 n)
    | One, Bit0 n -> Neg (bitM n)
    | Bit1 m, One -> Pos (Bit0 m)
    | Bit0 m, One -> Pos (bitM m)
    | One, One -> Zero_int
and plus_inta k l = match k, l with Neg m, Neg n -> Neg (plus_num m n)
                | Neg m, Pos n -> sub n m
                | Pos m, Neg n -> sub m n
                | Pos m, Pos n -> Pos (plus_num m n)
                | Zero_int, l -> l
                | k, Zero_int -> k;;

type 'a uminus = {uminus : 'a -> 'a};;
let uminus _A = _A.uminus;;

type 'a minus = {minus : 'a -> 'a -> 'a};;
let minus _A = _A.minus;;

type 'a zero = {zero : 'a};;
let zero _A = _A.zero;;

type 'a plus = {plus : 'a -> 'a -> 'a};;
let plus _A = _A.plus;;

type 'a semigroup_add = {plus_semigroup_add : 'a plus};;

type 'a cancel_semigroup_add =
  {semigroup_add_cancel_semigroup_add : 'a semigroup_add};;

type 'a ab_semigroup_add = {semigroup_add_ab_semigroup_add : 'a semigroup_add};;

type 'a cancel_ab_semigroup_add =
  {ab_semigroup_add_cancel_ab_semigroup_add : 'a ab_semigroup_add;
    cancel_semigroup_add_cancel_ab_semigroup_add : 'a cancel_semigroup_add;
    minus_cancel_ab_semigroup_add : 'a minus};;

type 'a monoid_add =
  {semigroup_add_monoid_add : 'a semigroup_add; zero_monoid_add : 'a zero};;

type 'a comm_monoid_add =
  {ab_semigroup_add_comm_monoid_add : 'a ab_semigroup_add;
    monoid_add_comm_monoid_add : 'a monoid_add};;

type 'a cancel_comm_monoid_add =
  {cancel_ab_semigroup_add_cancel_comm_monoid_add : 'a cancel_ab_semigroup_add;
    comm_monoid_add_cancel_comm_monoid_add : 'a comm_monoid_add};;

type 'a mult_zero = {times_mult_zero : 'a times; zero_mult_zero : 'a zero};;

type 'a semigroup_mult = {times_semigroup_mult : 'a times};;

type 'a semiring =
  {ab_semigroup_add_semiring : 'a ab_semigroup_add;
    semigroup_mult_semiring : 'a semigroup_mult};;

type 'a semiring_0 =
  {comm_monoid_add_semiring_0 : 'a comm_monoid_add;
    mult_zero_semiring_0 : 'a mult_zero; semiring_semiring_0 : 'a semiring};;

type 'a semiring_0_cancel =
  {cancel_comm_monoid_add_semiring_0_cancel : 'a cancel_comm_monoid_add;
    semiring_0_semiring_0_cancel : 'a semiring_0};;

type 'a group_add =
  {cancel_semigroup_add_group_add : 'a cancel_semigroup_add;
    minus_group_add : 'a minus; monoid_add_group_add : 'a monoid_add;
    uminus_group_add : 'a uminus};;

type 'a ab_group_add =
  {cancel_comm_monoid_add_ab_group_add : 'a cancel_comm_monoid_add;
    group_add_ab_group_add : 'a group_add};;

type 'a ring =
  {ab_group_add_ring : 'a ab_group_add;
    semiring_0_cancel_ring : 'a semiring_0_cancel};;

let plus_int = ({plus = plus_inta} : int plus);;

let semigroup_add_int = ({plus_semigroup_add = plus_int} : int semigroup_add);;

let cancel_semigroup_add_int =
  ({semigroup_add_cancel_semigroup_add = semigroup_add_int} :
    int cancel_semigroup_add);;

let ab_semigroup_add_int =
  ({semigroup_add_ab_semigroup_add = semigroup_add_int} :
    int ab_semigroup_add);;

let minus_int = ({minus = minus_inta} : int minus);;

let cancel_ab_semigroup_add_int =
  ({ab_semigroup_add_cancel_ab_semigroup_add = ab_semigroup_add_int;
     cancel_semigroup_add_cancel_ab_semigroup_add = cancel_semigroup_add_int;
     minus_cancel_ab_semigroup_add = minus_int}
    : int cancel_ab_semigroup_add);;

let zero_int = ({zero = Zero_int} : int zero);;

let monoid_add_int =
  ({semigroup_add_monoid_add = semigroup_add_int; zero_monoid_add = zero_int} :
    int monoid_add);;

let comm_monoid_add_int =
  ({ab_semigroup_add_comm_monoid_add = ab_semigroup_add_int;
     monoid_add_comm_monoid_add = monoid_add_int}
    : int comm_monoid_add);;

let cancel_comm_monoid_add_int =
  ({cancel_ab_semigroup_add_cancel_comm_monoid_add =
      cancel_ab_semigroup_add_int;
     comm_monoid_add_cancel_comm_monoid_add = comm_monoid_add_int}
    : int cancel_comm_monoid_add);;

let mult_zero_int =
  ({times_mult_zero = times_int; zero_mult_zero = zero_int} : int mult_zero);;

let semigroup_mult_int =
  ({times_semigroup_mult = times_int} : int semigroup_mult);;

let semiring_int =
  ({ab_semigroup_add_semiring = ab_semigroup_add_int;
     semigroup_mult_semiring = semigroup_mult_int}
    : int semiring);;

let semiring_0_int =
  ({comm_monoid_add_semiring_0 = comm_monoid_add_int;
     mult_zero_semiring_0 = mult_zero_int; semiring_semiring_0 = semiring_int}
    : int semiring_0);;

let semiring_0_cancel_int =
  ({cancel_comm_monoid_add_semiring_0_cancel = cancel_comm_monoid_add_int;
     semiring_0_semiring_0_cancel = semiring_0_int}
    : int semiring_0_cancel);;

let uminus_int = ({uminus = uminus_inta} : int uminus);;

let group_add_int =
  ({cancel_semigroup_add_group_add = cancel_semigroup_add_int;
     minus_group_add = minus_int; monoid_add_group_add = monoid_add_int;
     uminus_group_add = uminus_int}
    : int group_add);;

let ab_group_add_int =
  ({cancel_comm_monoid_add_ab_group_add = cancel_comm_monoid_add_int;
     group_add_ab_group_add = group_add_int}
    : int ab_group_add);;

let ring_int =
  ({ab_group_add_ring = ab_group_add_int;
     semiring_0_cancel_ring = semiring_0_cancel_int}
    : int ring);;

type 'a numeral =
  {one_numeral : 'a one; semigroup_add_numeral : 'a semigroup_add};;

let numeral_int =
  ({one_numeral = one_int; semigroup_add_numeral = semigroup_add_int} :
    int numeral);;

type 'a power = {one_power : 'a one; times_power : 'a times};;

let power_int = ({one_power = one_int; times_power = times_int} : int power);;

let rec less_eq_num x0 n = match x0, n with Bit1 m, Bit0 n -> less_num m n
                      | Bit1 m, Bit1 n -> less_eq_num m n
                      | Bit0 m, Bit1 n -> less_eq_num m n
                      | Bit0 m, Bit0 n -> less_eq_num m n
                      | Bit1 m, One -> false
                      | Bit0 m, One -> false
                      | One, n -> true
and less_num m x1 = match m, x1 with Bit1 m, Bit0 n -> less_num m n
               | Bit1 m, Bit1 n -> less_num m n
               | Bit0 m, Bit1 n -> less_eq_num m n
               | Bit0 m, Bit0 n -> less_num m n
               | One, Bit1 n -> true
               | One, Bit0 n -> true
               | m, One -> false;;

let rec less_eq_int x0 x1 = match x0, x1 with Neg k, Neg l -> less_eq_num l k
                      | Neg k, Pos l -> true
                      | Neg k, Zero_int -> true
                      | Pos k, Neg l -> false
                      | Pos k, Pos l -> less_eq_num k l
                      | Pos k, Zero_int -> false
                      | Zero_int, Neg l -> false
                      | Zero_int, Pos l -> true
                      | Zero_int, Zero_int -> true;;

let rec divmod_step_int
  l (q, r) =
    (if less_eq_int (Pos l) r
      then (plus_inta (times_inta (Pos (Bit0 One)) q) one_inta,
             minus_inta r (Pos l))
      else (times_inta (Pos (Bit0 One)) q, r));;

let rec divmod_int
  m x1 = match m, x1 with
    Bit1 m, Bit1 n ->
      (if less_num m n then (Zero_int, Pos (Bit1 m))
        else divmod_step_int (Bit1 n) (divmod_int (Bit1 m) (Bit0 (Bit1 n))))
    | Bit0 m, Bit1 n ->
        (if less_eq_num m n then (Zero_int, Pos (Bit0 m))
          else divmod_step_int (Bit1 n) (divmod_int (Bit0 m) (Bit0 (Bit1 n))))
    | Bit1 m, Bit0 n ->
        (let (q, r) = divmod_int m n in
          (q, plus_inta (times_inta (Pos (Bit0 One)) r) one_inta))
    | Bit0 m, Bit0 n ->
        (let (q, r) = divmod_int m n in (q, times_inta (Pos (Bit0 One)) r))
    | One, Bit1 n -> (Zero_int, Pos One)
    | One, Bit0 n -> (Zero_int, Pos One)
    | m, One -> (Pos m, Zero_int);;

type 'a zero_neq_one =
  {one_zero_neq_one : 'a one; zero_zero_neq_one : 'a zero};;

let rec of_bool _A = function true -> one _A.one_zero_neq_one
                     | false -> zero _A.zero_zero_neq_one;;

let zero_neq_one_int =
  ({one_zero_neq_one = one_int; zero_zero_neq_one = zero_int} :
    int zero_neq_one);;

let rec adjust_div
  (q, r) =
    plus_inta q (of_bool zero_neq_one_int (not (equal_inta r Zero_int)));;

let rec divide_inta
  k ka = match k, ka with Neg m, Neg n -> Product_Type.fst (divmod_int m n)
    | Pos m, Neg n -> uminus_inta (adjust_div (divmod_int m n))
    | Neg m, Pos n -> uminus_inta (adjust_div (divmod_int m n))
    | Pos m, Pos n -> Product_Type.fst (divmod_int m n)
    | k, Neg One -> uminus_inta k
    | k, Pos One -> k
    | Zero_int, k -> Zero_int
    | k, Zero_int -> Zero_int;;

type 'a divide = {divide : 'a -> 'a -> 'a};;
let divide _A = _A.divide;;

let divide_int = ({divide = divide_inta} : int divide);;

let rec adjust_mod
  l r = (if equal_inta r Zero_int then Zero_int else minus_inta l r);;

let rec modulo_inta
  k ka = match k, ka with
    Neg m, Neg n -> uminus_inta (Product_Type.snd (divmod_int m n))
    | Pos m, Neg n ->
        uminus_inta (adjust_mod (Pos n) (Product_Type.snd (divmod_int m n)))
    | Neg m, Pos n -> adjust_mod (Pos n) (Product_Type.snd (divmod_int m n))
    | Pos m, Pos n -> Product_Type.snd (divmod_int m n)
    | k, Neg One -> Zero_int
    | k, Pos One -> Zero_int
    | Zero_int, k -> Zero_int
    | k, Zero_int -> k;;

type 'a modulo =
  {divide_modulo : 'a divide; dvd_modulo : 'a dvd; modulo : 'a -> 'a -> 'a};;
let modulo _A = _A.modulo;;

let modulo_int =
  ({divide_modulo = divide_int; dvd_modulo = dvd_int; modulo = modulo_inta} :
    int modulo);;

type 'a monoid_mult =
  {semigroup_mult_monoid_mult : 'a semigroup_mult;
    power_monoid_mult : 'a power};;

type 'a semiring_numeral =
  {monoid_mult_semiring_numeral : 'a monoid_mult;
    numeral_semiring_numeral : 'a numeral;
    semiring_semiring_numeral : 'a semiring};;

type 'a semiring_1 =
  {semiring_numeral_semiring_1 : 'a semiring_numeral;
    semiring_0_semiring_1 : 'a semiring_0;
    zero_neq_one_semiring_1 : 'a zero_neq_one};;

type 'a semiring_1_cancel =
  {semiring_0_cancel_semiring_1_cancel : 'a semiring_0_cancel;
    semiring_1_semiring_1_cancel : 'a semiring_1};;

type 'a neg_numeral =
  {group_add_neg_numeral : 'a group_add; numeral_neg_numeral : 'a numeral};;

type 'a ring_1 =
  {neg_numeral_ring_1 : 'a neg_numeral; ring_ring_1 : 'a ring;
    semiring_1_cancel_ring_1 : 'a semiring_1_cancel};;

let monoid_mult_int =
  ({semigroup_mult_monoid_mult = semigroup_mult_int;
     power_monoid_mult = power_int}
    : int monoid_mult);;

let semiring_numeral_int =
  ({monoid_mult_semiring_numeral = monoid_mult_int;
     numeral_semiring_numeral = numeral_int;
     semiring_semiring_numeral = semiring_int}
    : int semiring_numeral);;

let semiring_1_int =
  ({semiring_numeral_semiring_1 = semiring_numeral_int;
     semiring_0_semiring_1 = semiring_0_int;
     zero_neq_one_semiring_1 = zero_neq_one_int}
    : int semiring_1);;

let semiring_1_cancel_int =
  ({semiring_0_cancel_semiring_1_cancel = semiring_0_cancel_int;
     semiring_1_semiring_1_cancel = semiring_1_int}
    : int semiring_1_cancel);;

let neg_numeral_int =
  ({group_add_neg_numeral = group_add_int; numeral_neg_numeral = numeral_int} :
    int neg_numeral);;

let ring_1_int =
  ({neg_numeral_ring_1 = neg_numeral_int; ring_ring_1 = ring_int;
     semiring_1_cancel_ring_1 = semiring_1_cancel_int}
    : int ring_1);;

let rec less_int x0 x1 = match x0, x1 with Neg k, Neg l -> less_num l k
                   | Neg k, Pos l -> true
                   | Neg k, Zero_int -> true
                   | Pos k, Neg l -> false
                   | Pos k, Pos l -> less_num k l
                   | Pos k, Zero_int -> false
                   | Zero_int, Neg l -> false
                   | Zero_int, Pos l -> true
                   | Zero_int, Zero_int -> false;;

let ord_int =
  ({Orderings.less_eq = less_eq_int; Orderings.less = less_int} :
    int Orderings.ord);;

type 'a semiring_no_zero_divisors =
  {semiring_0_semiring_no_zero_divisors : 'a semiring_0};;

type 'a semiring_1_no_zero_divisors =
  {semiring_1_semiring_1_no_zero_divisors : 'a semiring_1;
    semiring_no_zero_divisors_semiring_1_no_zero_divisors :
      'a semiring_no_zero_divisors};;

type 'a ab_semigroup_mult =
  {semigroup_mult_ab_semigroup_mult : 'a semigroup_mult};;

type 'a comm_semiring =
  {ab_semigroup_mult_comm_semiring : 'a ab_semigroup_mult;
    semiring_comm_semiring : 'a semiring};;

type 'a comm_semiring_0 =
  {comm_semiring_comm_semiring_0 : 'a comm_semiring;
    semiring_0_comm_semiring_0 : 'a semiring_0};;

type 'a comm_semiring_0_cancel =
  {comm_semiring_0_comm_semiring_0_cancel : 'a comm_semiring_0;
    semiring_0_cancel_comm_semiring_0_cancel : 'a semiring_0_cancel};;

type 'a comm_monoid_mult =
  {ab_semigroup_mult_comm_monoid_mult : 'a ab_semigroup_mult;
    monoid_mult_comm_monoid_mult : 'a monoid_mult;
    dvd_comm_monoid_mult : 'a dvd};;

type 'a comm_semiring_1 =
  {comm_monoid_mult_comm_semiring_1 : 'a comm_monoid_mult;
    comm_semiring_0_comm_semiring_1 : 'a comm_semiring_0;
    semiring_1_comm_semiring_1 : 'a semiring_1};;

type 'a comm_semiring_1_cancel =
  {comm_semiring_0_cancel_comm_semiring_1_cancel : 'a comm_semiring_0_cancel;
    comm_semiring_1_comm_semiring_1_cancel : 'a comm_semiring_1;
    semiring_1_cancel_comm_semiring_1_cancel : 'a semiring_1_cancel};;

type 'a semidom =
  {comm_semiring_1_cancel_semidom : 'a comm_semiring_1_cancel;
    semiring_1_no_zero_divisors_semidom : 'a semiring_1_no_zero_divisors};;

let semiring_no_zero_divisors_int =
  ({semiring_0_semiring_no_zero_divisors = semiring_0_int} :
    int semiring_no_zero_divisors);;

let semiring_1_no_zero_divisors_int =
  ({semiring_1_semiring_1_no_zero_divisors = semiring_1_int;
     semiring_no_zero_divisors_semiring_1_no_zero_divisors =
       semiring_no_zero_divisors_int}
    : int semiring_1_no_zero_divisors);;

let ab_semigroup_mult_int =
  ({semigroup_mult_ab_semigroup_mult = semigroup_mult_int} :
    int ab_semigroup_mult);;

let comm_semiring_int =
  ({ab_semigroup_mult_comm_semiring = ab_semigroup_mult_int;
     semiring_comm_semiring = semiring_int}
    : int comm_semiring);;

let comm_semiring_0_int =
  ({comm_semiring_comm_semiring_0 = comm_semiring_int;
     semiring_0_comm_semiring_0 = semiring_0_int}
    : int comm_semiring_0);;

let comm_semiring_0_cancel_int =
  ({comm_semiring_0_comm_semiring_0_cancel = comm_semiring_0_int;
     semiring_0_cancel_comm_semiring_0_cancel = semiring_0_cancel_int}
    : int comm_semiring_0_cancel);;

let comm_monoid_mult_int =
  ({ab_semigroup_mult_comm_monoid_mult = ab_semigroup_mult_int;
     monoid_mult_comm_monoid_mult = monoid_mult_int;
     dvd_comm_monoid_mult = dvd_int}
    : int comm_monoid_mult);;

let comm_semiring_1_int =
  ({comm_monoid_mult_comm_semiring_1 = comm_monoid_mult_int;
     comm_semiring_0_comm_semiring_1 = comm_semiring_0_int;
     semiring_1_comm_semiring_1 = semiring_1_int}
    : int comm_semiring_1);;

let comm_semiring_1_cancel_int =
  ({comm_semiring_0_cancel_comm_semiring_1_cancel = comm_semiring_0_cancel_int;
     comm_semiring_1_comm_semiring_1_cancel = comm_semiring_1_int;
     semiring_1_cancel_comm_semiring_1_cancel = semiring_1_cancel_int}
    : int comm_semiring_1_cancel);;

let semidom_int =
  ({comm_semiring_1_cancel_semidom = comm_semiring_1_cancel_int;
     semiring_1_no_zero_divisors_semidom = semiring_1_no_zero_divisors_int}
    : int semidom);;

type 'a comm_ring =
  {comm_semiring_0_cancel_comm_ring : 'a comm_semiring_0_cancel;
    ring_comm_ring : 'a ring};;

let comm_ring_int =
  ({comm_semiring_0_cancel_comm_ring = comm_semiring_0_cancel_int;
     ring_comm_ring = ring_int}
    : int comm_ring);;

type 'a comm_ring_1 =
  {comm_ring_comm_ring_1 : 'a comm_ring;
    comm_semiring_1_cancel_comm_ring_1 : 'a comm_semiring_1_cancel;
    ring_1_comm_ring_1 : 'a ring_1};;

let comm_ring_1_int =
  ({comm_ring_comm_ring_1 = comm_ring_int;
     comm_semiring_1_cancel_comm_ring_1 = comm_semiring_1_cancel_int;
     ring_1_comm_ring_1 = ring_1_int}
    : int comm_ring_1);;

type 'a semiring_modulo =
  {comm_semiring_1_cancel_semiring_modulo : 'a comm_semiring_1_cancel;
    modulo_semiring_modulo : 'a modulo};;

type 'a semiring_parity =
  {semiring_modulo_semiring_parity : 'a semiring_modulo};;

type 'a ring_parity =
  {semiring_parity_ring_parity : 'a semiring_parity;
    comm_ring_1_ring_parity : 'a comm_ring_1};;

let semiring_modulo_int =
  ({comm_semiring_1_cancel_semiring_modulo = comm_semiring_1_cancel_int;
     modulo_semiring_modulo = modulo_int}
    : int semiring_modulo);;

let semiring_parity_int =
  ({semiring_modulo_semiring_parity = semiring_modulo_int} :
    int semiring_parity);;

let ring_parity_int =
  ({semiring_parity_ring_parity = semiring_parity_int;
     comm_ring_1_ring_parity = comm_ring_1_int}
    : int ring_parity);;

type 'a semiring_no_zero_divisors_cancel =
  {semiring_no_zero_divisors_semiring_no_zero_divisors_cancel :
     'a semiring_no_zero_divisors};;

type 'a semidom_divide =
  {divide_semidom_divide : 'a divide; semidom_semidom_divide : 'a semidom;
    semiring_no_zero_divisors_cancel_semidom_divide :
      'a semiring_no_zero_divisors_cancel};;

let semiring_no_zero_divisors_cancel_int =
  ({semiring_no_zero_divisors_semiring_no_zero_divisors_cancel =
      semiring_no_zero_divisors_int}
    : int semiring_no_zero_divisors_cancel);;

let semidom_divide_int =
  ({divide_semidom_divide = divide_int; semidom_semidom_divide = semidom_int;
     semiring_no_zero_divisors_cancel_semidom_divide =
       semiring_no_zero_divisors_cancel_int}
    : int semidom_divide);;

type 'a algebraic_semidom =
  {semidom_divide_algebraic_semidom : 'a semidom_divide};;

type 'a semidom_modulo =
  {algebraic_semidom_semidom_modulo : 'a algebraic_semidom;
    semiring_modulo_semidom_modulo : 'a semiring_modulo};;

let algebraic_semidom_int =
  ({semidom_divide_algebraic_semidom = semidom_divide_int} :
    int algebraic_semidom);;

let semidom_modulo_int =
  ({algebraic_semidom_semidom_modulo = algebraic_semidom_int;
     semiring_modulo_semidom_modulo = semiring_modulo_int}
    : int semidom_modulo);;

type nat = Zero_nat | Suc of nat;;

let rec equal_nata x0 x1 = match x0, x1 with Zero_nat, Suc x2 -> false
                     | Suc x2, Zero_nat -> false
                     | Suc x2, Suc y2 -> equal_nata x2 y2
                     | Zero_nat, Zero_nat -> true;;

let equal_nat = ({HOL.equal = equal_nata} : nat HOL.equal);;

let rec less_eq_nat x0 n = match x0, n with Suc m, n -> less_nat m n
                      | Zero_nat, n -> true
and less_nat m x1 = match m, x1 with m, Suc n -> less_eq_nat m n
               | n, Zero_nat -> false;;

let ord_nat =
  ({Orderings.less_eq = less_eq_nat; Orderings.less = less_nat} :
    nat Orderings.ord);;

let preorder_nat =
  ({Orderings.ord_preorder = ord_nat} : nat Orderings.preorder);;

let order_nat =
  ({Orderings.preorder_order = preorder_nat} : nat Orderings.order);;

let linorder_nat =
  ({Orderings.order_linorder = order_nat} : nat Orderings.linorder);;

let rec plus_nat x0 n = match x0, n with Suc m, n -> plus_nat m (Suc n)
                   | Zero_nat, n -> n;;

let one_nat : nat = Suc Zero_nat;;

let rec nat_of_num
  = function Bit1 n -> (let m = nat_of_num n in Suc (plus_nat m m))
    | Bit0 n -> (let m = nat_of_num n in plus_nat m m)
    | One -> one_nat;;

let rec nat = function Pos k -> nat_of_num k
              | Zero_int -> Zero_nat
              | Neg k -> Zero_nat;;

let rec minus_nat m n = match m, n with Suc m, Suc n -> minus_nat m n
                    | Zero_nat, n -> Zero_nat
                    | m, Zero_nat -> m;;

let rec divmod_nat
  m n = (if equal_nata n Zero_nat || less_nat m n then (Zero_nat, m)
          else (let a = divmod_nat (minus_nat m n) n in
                let (q, aa) = a in
                 (Suc q, aa)));;

let rec dvd (_A1, _A2)
  a b = HOL.eq _A1
          (modulo _A2.semiring_modulo_semidom_modulo.modulo_semiring_modulo b a)
          (zero _A2.algebraic_semidom_semidom_modulo.semidom_divide_algebraic_semidom.semidom_semidom_divide.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero);;

let rec power _A a x1 = match a, x1 with a, Zero_nat -> one _A.one_power
                   | a, Suc n -> times _A.times_power a (power _A a n);;

let rec abs_int i = (if less_int i Zero_int then uminus_inta i else i);;

let rec divmod_integer
  k l = (if Z.equal k Z.zero then (Z.zero, Z.zero)
          else (if Z.lt Z.zero l
                 then (if Z.lt Z.zero k
                        then (fun k l -> if Z.equal Z.zero l then
                               (Z.zero, l) else Z.div_rem (Z.abs k) (Z.abs l))
                               k l
                        else (let (r, s) =
                                (fun k l -> if Z.equal Z.zero l then
                                  (Z.zero, l) else Z.div_rem (Z.abs k)
                                  (Z.abs l))
                                  k l
                                in
                               (if Z.equal s Z.zero then (Z.neg r, Z.zero)
                                 else (Z.sub (Z.neg r) (Z.of_int 1),
Z.sub l s))))
                 else (if Z.equal l Z.zero then (Z.zero, k)
                        else Product_Type.apsnd Z.neg
                               (if Z.lt k Z.zero
                                 then (fun k l -> if Z.equal Z.zero l then
(Z.zero, l) else Z.div_rem (Z.abs k) (Z.abs l))
k l
                                 else (let (r, s) =
 (fun k l -> if Z.equal Z.zero l then (Z.zero, l) else Z.div_rem (Z.abs k)
   (Z.abs l))
   k l
 in
(if Z.equal s Z.zero then (Z.neg r, Z.zero)
  else (Z.sub (Z.neg r) (Z.of_int 1), Z.sub (Z.neg l) s)))))));;

let rec of_nat_aux _A inc x1 i = match inc, x1, i with inc, Zero_nat, i -> i
                        | inc, Suc n, i -> of_nat_aux _A inc n (inc i);;

let rec of_nat _A
  n = of_nat_aux _A
        (fun i ->
          plus _A.semiring_numeral_semiring_1.numeral_semiring_numeral.semigroup_add_numeral.plus_semigroup_add
            i (one _A.semiring_numeral_semiring_1.numeral_semiring_numeral.one_numeral))
        n (zero _A.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero);;

let rec times_nat x0 n = match x0, n with Zero_nat, n -> Zero_nat
                    | Suc m, n -> plus_nat n (times_nat m n);;

let rec int_of_integer
  k = (if Z.lt k Z.zero then uminus_inta (int_of_integer (Z.neg k))
        else (if Z.equal k Z.zero then Zero_int
               else (let (l, j) = divmod_integer k (Z.of_int 2) in
                     let la = times_inta (Pos (Bit0 One)) (int_of_integer l) in
                      (if Z.equal j Z.zero then la
                        else plus_inta la one_inta))));;

let rec integer_of_int
  k = (if less_int k Zero_int then Z.neg (integer_of_int (uminus_inta k))
        else (if equal_inta k Zero_int then Z.zero
               else (let l =
                       Z.mul (Z.of_int 2)
                         (integer_of_int (divide_inta k (Pos (Bit0 One))))
                       in
                     let j = modulo_inta k (Pos (Bit0 One)) in
                      (if equal_inta j Zero_int then l
                        else Z.add l (Z.of_int 1)))));;

let rec modulo_nat m n = Product_Type.snd (divmod_nat m n);;

end;; (*struct Arith*)

module Bit_Operations : sig
  val and_int : Arith.int -> Arith.int -> Arith.int
  val take_bit_int : Arith.nat -> Arith.int -> Arith.int
  val drop_bit_int : Arith.nat -> Arith.int -> Arith.int
  type 'a semiring_bit_operations
  type 'a ring_bit_operations
  val ring_bit_operations_int : Arith.int ring_bit_operations
  val bin_rsplit : Arith.nat -> Arith.nat * Arith.int -> Arith.int list
  val concat_bit : Arith.nat -> Arith.int -> Arith.int -> Arith.int
  val signed_take_bit : 'a ring_bit_operations -> Arith.nat -> 'a -> 'a
end = struct

let rec bit_int
  k n = not (Arith.dvd (Arith.equal_int, Arith.semidom_modulo_int)
              (Arith.Pos (Arith.Bit0 Arith.One))
              (Arith.divide_inta k
                (Arith.power Arith.power_int (Arith.Pos (Arith.Bit0 Arith.One))
                  n)));;

type 'a semiring_bits =
  {semiring_parity_semiring_bits : 'a Arith.semiring_parity;
    bit : 'a -> Arith.nat -> bool};;
let bit _A = _A.bit;;

let semiring_bits_int =
  ({semiring_parity_semiring_bits = Arith.semiring_parity_int; bit = bit_int} :
    Arith.int semiring_bits);;

let rec push_bit_int
  n k = Arith.times_inta k
          (Arith.power Arith.power_int (Arith.Pos (Arith.Bit0 Arith.One)) n);;

let rec and_int
  k l = (if Arith.equal_inta k Arith.Zero_int ||
              Arith.equal_inta l Arith.Zero_int
          then Arith.Zero_int
          else (if Arith.equal_inta k (Arith.uminus_inta Arith.one_inta) then l
                 else (if Arith.equal_inta l (Arith.uminus_inta Arith.one_inta)
                        then k
                        else Arith.plus_inta
                               (Arith.times_inta
                                 (Arith.modulo_inta k
                                   (Arith.Pos (Arith.Bit0 Arith.One)))
                                 (Arith.modulo_inta l
                                   (Arith.Pos (Arith.Bit0 Arith.One))))
                               (Arith.times_inta
                                 (Arith.Pos (Arith.Bit0 Arith.One))
                                 (and_int
                                   (Arith.divide_inta k
                                     (Arith.Pos (Arith.Bit0 Arith.One)))
                                   (Arith.divide_inta l
                                     (Arith.Pos (Arith.Bit0 Arith.One))))))));;

let rec not_int k = Arith.minus_inta (Arith.uminus_inta k) Arith.one_inta;;

let rec unset_bit_int
  n k = and_int k (not_int (push_bit_int n Arith.one_inta));;

let rec take_bit_int
  n k = Arith.modulo_inta k
          (Arith.power Arith.power_int (Arith.Pos (Arith.Bit0 Arith.One)) n);;

let rec xor_int
  k l = (if Arith.equal_inta k (Arith.uminus_inta Arith.one_inta) then not_int l
          else (if Arith.equal_inta l (Arith.uminus_inta Arith.one_inta)
                 then not_int k
                 else (if Arith.equal_inta k Arith.Zero_int then l
                        else (if Arith.equal_inta l Arith.Zero_int then k
                               else Arith.plus_inta
                                      (Arith.abs_int
(Arith.minus_inta (Arith.modulo_inta k (Arith.Pos (Arith.Bit0 Arith.One)))
  (Arith.modulo_inta l (Arith.Pos (Arith.Bit0 Arith.One)))))
                                      (Arith.times_inta
(Arith.Pos (Arith.Bit0 Arith.One))
(xor_int (Arith.divide_inta k (Arith.Pos (Arith.Bit0 Arith.One)))
  (Arith.divide_inta l (Arith.Pos (Arith.Bit0 Arith.One)))))))));;

let rec flip_bit_int n k = xor_int k (push_bit_int n Arith.one_inta);;

let rec drop_bit_int
  n k = Arith.divide_inta k
          (Arith.power Arith.power_int (Arith.Pos (Arith.Bit0 Arith.One)) n);;

let rec or_int
  k l = (if Arith.equal_inta k (Arith.uminus_inta Arith.one_inta) ||
              Arith.equal_inta l (Arith.uminus_inta Arith.one_inta)
          then Arith.uminus_inta Arith.one_inta
          else (if Arith.equal_inta k Arith.Zero_int then l
                 else (if Arith.equal_inta l Arith.Zero_int then k
                        else Arith.plus_inta
                               (Orderings.max Arith.ord_int
                                 (Arith.modulo_inta k
                                   (Arith.Pos (Arith.Bit0 Arith.One)))
                                 (Arith.modulo_inta l
                                   (Arith.Pos (Arith.Bit0 Arith.One))))
                               (Arith.times_inta
                                 (Arith.Pos (Arith.Bit0 Arith.One))
                                 (or_int
                                   (Arith.divide_inta k
                                     (Arith.Pos (Arith.Bit0 Arith.One)))
                                   (Arith.divide_inta l
                                     (Arith.Pos (Arith.Bit0 Arith.One))))))));;

let rec set_bit_int n k = or_int k (push_bit_int n Arith.one_inta);;

let rec mask_int
  n = Arith.minus_inta
        (Arith.power Arith.power_int (Arith.Pos (Arith.Bit0 Arith.One)) n)
        Arith.one_inta;;

type 'a semiring_bit_operations =
  {semiring_bits_semiring_bit_operations : 'a semiring_bits;
    anda : 'a -> 'a -> 'a; ora : 'a -> 'a -> 'a; xor : 'a -> 'a -> 'a;
    mask : Arith.nat -> 'a; set_bit : Arith.nat -> 'a -> 'a;
    unset_bit : Arith.nat -> 'a -> 'a; flip_bit : Arith.nat -> 'a -> 'a;
    push_bit : Arith.nat -> 'a -> 'a; drop_bit : Arith.nat -> 'a -> 'a;
    take_bit : Arith.nat -> 'a -> 'a};;
let anda _A = _A.anda;;
let ora _A = _A.ora;;
let xor _A = _A.xor;;
let mask _A = _A.mask;;
let set_bit _A = _A.set_bit;;
let unset_bit _A = _A.unset_bit;;
let flip_bit _A = _A.flip_bit;;
let push_bit _A = _A.push_bit;;
let drop_bit _A = _A.drop_bit;;
let take_bit _A = _A.take_bit;;

type 'a ring_bit_operations =
  {semiring_bit_operations_ring_bit_operations : 'a semiring_bit_operations;
    ring_parity_ring_bit_operations : 'a Arith.ring_parity; nota : 'a -> 'a};;
let nota _A = _A.nota;;

let semiring_bit_operations_int =
  ({semiring_bits_semiring_bit_operations = semiring_bits_int; anda = and_int;
     ora = or_int; xor = xor_int; mask = mask_int; set_bit = set_bit_int;
     unset_bit = unset_bit_int; flip_bit = flip_bit_int;
     push_bit = push_bit_int; drop_bit = drop_bit_int; take_bit = take_bit_int}
    : Arith.int semiring_bit_operations);;

let ring_bit_operations_int =
  ({semiring_bit_operations_ring_bit_operations = semiring_bit_operations_int;
     ring_parity_ring_bit_operations = Arith.ring_parity_int; nota = not_int}
    : Arith.int ring_bit_operations);;

let rec bin_split
  x0 w = match x0, w with Arith.Zero_nat, w -> (w, Arith.Zero_int)
    | Arith.Suc n, w ->
        (let (w1, w2) =
           bin_split n (Arith.divide_inta w (Arith.Pos (Arith.Bit0 Arith.One)))
           in
          (w1, Arith.plus_inta
                 (Arith.of_bool Arith.zero_neq_one_int
                   (not (Arith.dvd (Arith.equal_int, Arith.semidom_modulo_int)
                          (Arith.Pos (Arith.Bit0 Arith.One)) w)))
                 (Arith.times_inta (Arith.Pos (Arith.Bit0 Arith.One)) w2)));;

let rec bin_rsplit_aux
  n m c bs =
    (if Arith.equal_nata m Arith.Zero_nat || Arith.equal_nata n Arith.Zero_nat
      then bs
      else (let a = bin_split n c in
            let (aa, b) = a in
             bin_rsplit_aux n (Arith.minus_nat m n) aa (b :: bs)));;

let rec bin_rsplit
  n w = bin_rsplit_aux n (Product_Type.fst w) (Product_Type.snd w) [];;

let rec concat_bit n k l = or_int (take_bit_int n k) (push_bit_int n l);;

let rec signed_take_bit _A
  n a = (let l =
           take_bit _A.semiring_bit_operations_ring_bit_operations (Arith.Suc n)
             a
           in
          (if bit _A.semiring_bit_operations_ring_bit_operations.semiring_bits_semiring_bit_operations
                l n
            then Arith.plus
                   _A.ring_parity_ring_bit_operations.Arith.comm_ring_1_ring_parity.Arith.ring_1_comm_ring_1.Arith.neg_numeral_ring_1.Arith.numeral_neg_numeral.Arith.semigroup_add_numeral.Arith.plus_semigroup_add
                   l (push_bit _A.semiring_bit_operations_ring_bit_operations
                       (Arith.Suc n)
                       (Arith.uminus
                         _A.ring_parity_ring_bit_operations.Arith.comm_ring_1_ring_parity.Arith.ring_1_comm_ring_1.Arith.neg_numeral_ring_1.Arith.group_add_neg_numeral.Arith.uminus_group_add
                         (Arith.one
                           _A.ring_parity_ring_bit_operations.Arith.comm_ring_1_ring_parity.Arith.ring_1_comm_ring_1.Arith.neg_numeral_ring_1.Arith.numeral_neg_numeral.Arith.one_numeral)))
            else l));;

end;; (*struct Bit_Operations*)

module Countable : sig
  type 'a countable = unit
end = struct

type 'a countable = unit;;

end;; (*struct Countable*)

module Finite_Set : sig
  type 'a finite
end = struct

type 'a finite = {countable_finite : 'a Countable.countable};;

end;; (*struct Finite_Set*)

module Numeral_Type : sig
  type 'a bit0
  type 'a bit1
  type num1
end = struct

type 'a bit0 = Abs_bit0 of Arith.int;;

type 'a bit1 = Abs_bit1 of Arith.int;;

type num1 = One_num1;;

end;; (*struct Numeral_Type*)

module Type_Length : sig
  type 'a len0 = {len_of : 'a HOL.itself -> Arith.nat}
  val len_of : 'a len0 -> 'a HOL.itself -> Arith.nat
  type 'a len = {len0_len : 'a len0}
  val len_bit0 : 'a len -> 'a Numeral_Type.bit0 len
  val len0_bit1 : 'a len0 -> 'a Numeral_Type.bit1 len0
  val len_bit1 : 'a len0 -> 'a Numeral_Type.bit1 len
  val len0_num1 : Numeral_Type.num1 len0
  val len_num1 : Numeral_Type.num1 len
end = struct

type 'a len0 = {len_of : 'a HOL.itself -> Arith.nat};;
let len_of _A = _A.len_of;;

let rec len_of_bit0 _A
  uu = Arith.times_nat (Arith.nat_of_num (Arith.Bit0 Arith.One))
         (len_of _A HOL.Type);;

type 'a len = {len0_len : 'a len0};;

let rec len0_bit0 _A = ({len_of = len_of_bit0 _A} : 'a Numeral_Type.bit0 len0);;

let rec len_bit0 _A =
  ({len0_len = (len0_bit0 _A.len0_len)} : 'a Numeral_Type.bit0 len);;

let rec len_of_bit1 _A
  uu = Arith.plus_nat
         (Arith.times_nat (Arith.nat_of_num (Arith.Bit0 Arith.One))
           (len_of _A HOL.Type))
         Arith.one_nat;;

let rec len0_bit1 _A = ({len_of = len_of_bit1 _A} : 'a Numeral_Type.bit1 len0);;

let rec len_bit1 _A = ({len0_len = (len0_bit1 _A)} : 'a Numeral_Type.bit1 len);;

let rec len_of_num1 uu = Arith.one_nat;;

let len0_num1 = ({len_of = len_of_num1} : Numeral_Type.num1 len0);;

let len_num1 = ({len0_len = len0_num1} : Numeral_Type.num1 len);;

end;; (*struct Type_Length*)

module Lista : sig
  val rev : 'a list -> 'a list
  val foldr : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val hd : 'a list -> 'a
  val map : ('a -> 'b) -> 'a list -> 'b list
end = struct

let rec fold f x1 s = match f, x1, s with f, x :: xs, s -> fold f xs (f x s)
               | f, [], s -> s;;

let rec rev xs = fold (fun a b -> a :: b) xs [];;

let rec foldr f x1 = match f, x1 with f, [] -> Fun.id
                | f, x :: xs -> Fun.comp (f x) (foldr f xs);;

let rec hd (x21 :: x22) = x21;;

let rec map f x1 = match f, x1 with f, [] -> []
              | f, x21 :: x22 -> f x21 :: map f x22;;

end;; (*struct Lista*)

module Groups_List : sig
  val horner_sum : 'b Arith.comm_semiring_0 -> ('a -> 'b) -> 'b -> 'a list -> 'b
end = struct

let rec horner_sum _B
  f a xs =
    Lista.foldr
      (fun x b ->
        Arith.plus
          _B.Arith.semiring_0_comm_semiring_0.Arith.comm_monoid_add_semiring_0.Arith.monoid_add_comm_monoid_add.Arith.semigroup_add_monoid_add.Arith.plus_semigroup_add
          (f x)
          (Arith.times
            _B.Arith.semiring_0_comm_semiring_0.Arith.mult_zero_semiring_0.Arith.times_mult_zero
            a b))
      xs (Arith.zero
           _B.Arith.semiring_0_comm_semiring_0.Arith.mult_zero_semiring_0.Arith.zero_mult_zero);;

end;; (*struct Groups_List*)

module Word : sig
  type 'a word
  val the_int : 'a Type_Length.len -> 'a word -> Arith.int
  val of_int : 'a Type_Length.len -> Arith.int -> 'a word
  val of_nat : 'a Type_Length.len -> Arith.nat -> 'a word
  val the_nat : 'a Type_Length.len -> 'a word -> Arith.nat
  val word_cat :
    'a Type_Length.len -> 'b Type_Length.len -> 'c Type_Length.len ->
      'a word -> 'b word -> 'c word
  val word_rcat :
    'a Type_Length.len -> 'b Type_Length.len -> 'a word list -> 'b word
  val word_split :
    'a Type_Length.len -> 'b Type_Length.len -> 'c Type_Length.len ->
      'a word -> 'b word * 'c word
  val the_signed_int : 'a Type_Length.len -> 'a word -> Arith.int
  val one_word : 'a Type_Length.len -> 'a word
  val zero_word : 'a Type_Length.len -> 'a word
  val equal_word : 'a Type_Length.len -> 'a word -> 'a word -> bool
  val and_word : 'a Type_Length.len -> 'a word -> 'a word -> 'a word
end = struct

type 'a word = Word of Arith.int;;

let rec the_int _A (Word x) = x;;

let rec cast _B _A
  w = Word (Bit_Operations.take_bit_int
             (Type_Length.len_of _A.Type_Length.len0_len HOL.Type)
             (the_int _B w));;

let rec of_int _A
  k = Word (Bit_Operations.take_bit_int
             (Type_Length.len_of _A.Type_Length.len0_len HOL.Type) k);;

let rec of_nat _A
  n = Word (Bit_Operations.take_bit_int
             (Type_Length.len_of _A.Type_Length.len0_len HOL.Type)
             (Arith.of_nat Arith.semiring_1_int n));;

let rec the_nat _A w = Arith.nat (the_int _A w);;

let rec word_cat _A _B _C
  a b = of_int _C
          (Bit_Operations.concat_bit
            (Type_Length.len_of _B.Type_Length.len0_len HOL.Type) (the_int _B b)
            (the_int _A a));;

let rec word_rcat _A _B
  = Fun.comp
      (Fun.comp (of_int _B)
        (Groups_List.horner_sum Arith.comm_semiring_0_int (the_int _A)
          (Arith.power Arith.power_int (Arith.Pos (Arith.Bit0 Arith.One))
            (Type_Length.len_of _A.Type_Length.len0_len HOL.Type))))
      Lista.rev;;

let rec drop_bit_word _A
  n w = Word (Bit_Operations.drop_bit_int n (the_int _A w));;

let rec word_split _A _B _C
  w = (cast _A _B
         (drop_bit_word _A (Type_Length.len_of _C.Type_Length.len0_len HOL.Type)
           w),
        cast _A _C w);;

let rec the_signed_int _A
  w = Bit_Operations.signed_take_bit Bit_Operations.ring_bit_operations_int
        (Arith.minus_nat (Type_Length.len_of _A.Type_Length.len0_len HOL.Type)
          (Arith.Suc Arith.Zero_nat))
        (the_int _A w);;

let rec one_word _A = Word Arith.one_inta;;

let rec zero_word _A = Word Arith.Zero_int;;

let rec equal_word _A v w = Arith.equal_inta (the_int _A v) (the_int _A w);;

let rec and_word _A
  v w = Word (Bit_Operations.and_int (the_int _A v) (the_int _A w));;

end;; (*struct Word*)

module AList : sig
  val update : 'a HOL.equal -> 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
end = struct

let rec update _A
  k v x2 = match k, v, x2 with k, v, [] -> [(k, v)]
    | k, v, p :: ps ->
        (if HOL.eq _A (Product_Type.fst p) k then (k, v) :: ps
          else p :: update _A k v ps);;

end;; (*struct AList*)

module DAList : sig
  type ('b, 'a) alist = Alist of ('b * 'a) list
  val empty : ('a, 'b) alist
  val impl_of : ('b, 'a) alist -> ('b * 'a) list
  val lookup : 'a HOL.equal -> ('a, 'b) alist -> 'a -> 'b option
  val update : 'a HOL.equal -> 'a -> 'b -> ('a, 'b) alist -> ('a, 'b) alist
end = struct

type ('b, 'a) alist = Alist of ('b * 'a) list;;

let empty : ('a, 'b) alist = Alist [];;

let rec impl_of (Alist x) = x;;

let rec lookup _A xa = Map.map_of _A (impl_of xa);;

let rec update _A xc xd xe = Alist (AList.update _A xc xd (impl_of xe));;

end;; (*struct DAList*)

module Option : sig
  val is_none : 'a option -> bool
  val the : 'a option -> 'a
end = struct

let rec is_none = function Some x -> false
                  | None -> true;;

let rec the (Some x2) = x2;;

end;; (*struct Option*)

module Rsplit : sig
  val word_rsplit :
    'a Type_Length.len -> 'b Type_Length.len ->
      'a Word.word -> 'b Word.word list
end = struct

let rec word_rsplit _A _B
  w = Lista.map (Word.of_int _B)
        (Bit_Operations.bin_rsplit
          (Type_Length.len_of _B.Type_Length.len0_len HOL.Type)
          (Type_Length.len_of _A.Type_Length.len0_len HOL.Type,
            Word.the_int _A w));;

end;; (*struct Rsplit*)

module Comparator : sig
  type order = Eq | Lt | Gt
  val comparator_of : 'a HOL.equal * 'a Orderings.linorder -> 'a -> 'a -> order
end = struct

type order = Eq | Lt | Gt;;

let rec comparator_of (_A1, _A2)
  x y = (if Orderings.less
              _A2.Orderings.order_linorder.Orderings.preorder_order.Orderings.ord_preorder
              x y
          then Lt else (if HOL.eq _A1 x y then Eq else Gt));;

end;; (*struct Comparator*)

module Compare_Instances : sig
  val compare_nat : Arith.nat -> Arith.nat -> Comparator.order
end = struct

let rec compare_nat
  x = Comparator.comparator_of (Arith.equal_nat, Arith.linorder_nat) x;;

end;; (*struct Compare_Instances*)

module Collection_Order : sig
  type 'a ccompare = {ccompare : ('a -> 'a -> Comparator.order) option}
  val ccompare : 'a ccompare -> ('a -> 'a -> Comparator.order) option
  val ccompare_nat : Arith.nat ccompare
end = struct

let ccompare_nata : (Arith.nat -> Arith.nat -> Comparator.order) option
  = Some Compare_Instances.compare_nat;;

type 'a ccompare = {ccompare : ('a -> 'a -> Comparator.order) option};;
let ccompare _A = _A.ccompare;;

let ccompare_nat = ({ccompare = ccompare_nata} : Arith.nat ccompare);;

end;; (*struct Collection_Order*)

module RBT_Impl : sig
  type color = R | B
  type ('a, 'b) rbt = Empty |
    Branch of color * ('a, 'b) rbt * 'a * 'b * ('a, 'b) rbt
  val paint : color -> ('a, 'b) rbt -> ('a, 'b) rbt
  val balance : ('a, 'b) rbt -> 'a -> 'b -> ('a, 'b) rbt -> ('a, 'b) rbt
  val balance_left : ('a, 'b) rbt -> 'a -> 'b -> ('a, 'b) rbt -> ('a, 'b) rbt
  val combine : ('a, 'b) rbt -> ('a, 'b) rbt -> ('a, 'b) rbt
  val balance_right : ('a, 'b) rbt -> 'a -> 'b -> ('a, 'b) rbt -> ('a, 'b) rbt
end = struct

type color = R | B;;

type ('a, 'b) rbt = Empty |
  Branch of color * ('a, 'b) rbt * 'a * 'b * ('a, 'b) rbt;;

let rec paint c x1 = match c, x1 with c, Empty -> Empty
                | c, Branch (uu, l, k, v, r) -> Branch (c, l, k, v, r);;

let rec balance
  x0 s t x3 = match x0, s, t, x3 with
    Branch (R, a, w, x, b), s, t, Branch (R, c, y, z, d) ->
      Branch (R, Branch (B, a, w, x, b), s, t, Branch (B, c, y, z, d))
    | Branch (R, Branch (R, a, w, x, b), s, t, c), y, z, Empty ->
        Branch (R, Branch (B, a, w, x, b), s, t, Branch (B, c, y, z, Empty))
    | Branch (R, Branch (R, a, w, x, b), s, t, c), y, z,
        Branch (B, va, vb, vc, vd)
        -> Branch
             (R, Branch (B, a, w, x, b), s, t,
               Branch (B, c, y, z, Branch (B, va, vb, vc, vd)))
    | Branch (R, Empty, w, x, Branch (R, b, s, t, c)), y, z, Empty ->
        Branch (R, Branch (B, Empty, w, x, b), s, t, Branch (B, c, y, z, Empty))
    | Branch (R, Branch (B, va, vb, vc, vd), w, x, Branch (R, b, s, t, c)), y,
        z, Empty
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), w, x, b), s, t,
               Branch (B, c, y, z, Empty))
    | Branch (R, Empty, w, x, Branch (R, b, s, t, c)), y, z,
        Branch (B, va, vb, vc, vd)
        -> Branch
             (R, Branch (B, Empty, w, x, b), s, t,
               Branch (B, c, y, z, Branch (B, va, vb, vc, vd)))
    | Branch (R, Branch (B, ve, vf, vg, vh), w, x, Branch (R, b, s, t, c)), y,
        z, Branch (B, va, vb, vc, vd)
        -> Branch
             (R, Branch (B, Branch (B, ve, vf, vg, vh), w, x, b), s, t,
               Branch (B, c, y, z, Branch (B, va, vb, vc, vd)))
    | Empty, w, x, Branch (R, b, s, t, Branch (R, c, y, z, d)) ->
        Branch (R, Branch (B, Empty, w, x, b), s, t, Branch (B, c, y, z, d))
    | Branch (B, va, vb, vc, vd), w, x,
        Branch (R, b, s, t, Branch (R, c, y, z, d))
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), w, x, b), s, t,
               Branch (B, c, y, z, d))
    | Empty, w, x, Branch (R, Branch (R, b, s, t, c), y, z, Empty) ->
        Branch (R, Branch (B, Empty, w, x, b), s, t, Branch (B, c, y, z, Empty))
    | Empty, w, x,
        Branch (R, Branch (R, b, s, t, c), y, z, Branch (B, va, vb, vc, vd))
        -> Branch
             (R, Branch (B, Empty, w, x, b), s, t,
               Branch (B, c, y, z, Branch (B, va, vb, vc, vd)))
    | Branch (B, va, vb, vc, vd), w, x,
        Branch (R, Branch (R, b, s, t, c), y, z, Empty)
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), w, x, b), s, t,
               Branch (B, c, y, z, Empty))
    | Branch (B, va, vb, vc, vd), w, x,
        Branch (R, Branch (R, b, s, t, c), y, z, Branch (B, ve, vf, vg, vh))
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), w, x, b), s, t,
               Branch (B, c, y, z, Branch (B, ve, vf, vg, vh)))
    | Empty, s, t, Empty -> Branch (B, Empty, s, t, Empty)
    | Empty, s, t, Branch (B, va, vb, vc, vd) ->
        Branch (B, Empty, s, t, Branch (B, va, vb, vc, vd))
    | Empty, s, t, Branch (v, Empty, vb, vc, Empty) ->
        Branch (B, Empty, s, t, Branch (v, Empty, vb, vc, Empty))
    | Empty, s, t, Branch (v, Branch (B, ve, vf, vg, vh), vb, vc, Empty) ->
        Branch
          (B, Empty, s, t,
            Branch (v, Branch (B, ve, vf, vg, vh), vb, vc, Empty))
    | Empty, s, t, Branch (v, Empty, vb, vc, Branch (B, vf, vg, vh, vi)) ->
        Branch
          (B, Empty, s, t,
            Branch (v, Empty, vb, vc, Branch (B, vf, vg, vh, vi)))
    | Empty, s, t,
        Branch
          (v, Branch (B, ve, vj, vk, vl), vb, vc, Branch (B, vf, vg, vh, vi))
        -> Branch
             (B, Empty, s, t,
               Branch
                 (v, Branch (B, ve, vj, vk, vl), vb, vc,
                   Branch (B, vf, vg, vh, vi)))
    | Branch (B, va, vb, vc, vd), s, t, Empty ->
        Branch (B, Branch (B, va, vb, vc, vd), s, t, Empty)
    | Branch (B, va, vb, vc, vd), s, t, Branch (B, ve, vf, vg, vh) ->
        Branch (B, Branch (B, va, vb, vc, vd), s, t, Branch (B, ve, vf, vg, vh))
    | Branch (B, va, vb, vc, vd), s, t, Branch (v, Empty, vf, vg, Empty) ->
        Branch
          (B, Branch (B, va, vb, vc, vd), s, t,
            Branch (v, Empty, vf, vg, Empty))
    | Branch (B, va, vb, vc, vd), s, t,
        Branch (v, Branch (B, vi, vj, vk, vl), vf, vg, Empty)
        -> Branch
             (B, Branch (B, va, vb, vc, vd), s, t,
               Branch (v, Branch (B, vi, vj, vk, vl), vf, vg, Empty))
    | Branch (B, va, vb, vc, vd), s, t,
        Branch (v, Empty, vf, vg, Branch (B, vj, vk, vl, vm))
        -> Branch
             (B, Branch (B, va, vb, vc, vd), s, t,
               Branch (v, Empty, vf, vg, Branch (B, vj, vk, vl, vm)))
    | Branch (B, va, vb, vc, vd), s, t,
        Branch
          (v, Branch (B, vi, vn, vo, vp), vf, vg, Branch (B, vj, vk, vl, vm))
        -> Branch
             (B, Branch (B, va, vb, vc, vd), s, t,
               Branch
                 (v, Branch (B, vi, vn, vo, vp), vf, vg,
                   Branch (B, vj, vk, vl, vm)))
    | Branch (v, Empty, vb, vc, Empty), s, t, Empty ->
        Branch (B, Branch (v, Empty, vb, vc, Empty), s, t, Empty)
    | Branch (v, Empty, vb, vc, Branch (B, ve, vf, vg, vh)), s, t, Empty ->
        Branch
          (B, Branch (v, Empty, vb, vc, Branch (B, ve, vf, vg, vh)), s, t,
            Empty)
    | Branch (v, Branch (B, vf, vg, vh, vi), vb, vc, Empty), s, t, Empty ->
        Branch
          (B, Branch (v, Branch (B, vf, vg, vh, vi), vb, vc, Empty), s, t,
            Empty)
    | Branch
        (v, Branch (B, vf, vg, vh, vi), vb, vc, Branch (B, ve, vj, vk, vl)),
        s, t, Empty
        -> Branch
             (B, Branch
                   (v, Branch (B, vf, vg, vh, vi), vb, vc,
                     Branch (B, ve, vj, vk, vl)),
               s, t, Empty)
    | Branch (v, Empty, vf, vg, Empty), s, t, Branch (B, va, vb, vc, vd) ->
        Branch
          (B, Branch (v, Empty, vf, vg, Empty), s, t,
            Branch (B, va, vb, vc, vd))
    | Branch (v, Empty, vf, vg, Branch (B, vi, vj, vk, vl)), s, t,
        Branch (B, va, vb, vc, vd)
        -> Branch
             (B, Branch (v, Empty, vf, vg, Branch (B, vi, vj, vk, vl)), s, t,
               Branch (B, va, vb, vc, vd))
    | Branch (v, Branch (B, vj, vk, vl, vm), vf, vg, Empty), s, t,
        Branch (B, va, vb, vc, vd)
        -> Branch
             (B, Branch (v, Branch (B, vj, vk, vl, vm), vf, vg, Empty), s, t,
               Branch (B, va, vb, vc, vd))
    | Branch
        (v, Branch (B, vj, vk, vl, vm), vf, vg, Branch (B, vi, vn, vo, vp)),
        s, t, Branch (B, va, vb, vc, vd)
        -> Branch
             (B, Branch
                   (v, Branch (B, vj, vk, vl, vm), vf, vg,
                     Branch (B, vi, vn, vo, vp)),
               s, t, Branch (B, va, vb, vc, vd));;

let rec balance_left
  x0 s y c = match x0, s, y, c with
    Branch (R, a, k, x, b), s, y, c ->
      Branch (R, Branch (B, a, k, x, b), s, y, c)
    | Empty, k, x, Branch (B, a, s, y, b) ->
        balance Empty k x (Branch (R, a, s, y, b))
    | Branch (B, va, vb, vc, vd), k, x, Branch (B, a, s, y, b) ->
        balance (Branch (B, va, vb, vc, vd)) k x (Branch (R, a, s, y, b))
    | Empty, k, x, Branch (R, Branch (B, a, s, y, b), t, z, c) ->
        Branch (R, Branch (B, Empty, k, x, a), s, y, balance b t z (paint R c))
    | Branch (B, va, vb, vc, vd), k, x,
        Branch (R, Branch (B, a, s, y, b), t, z, c)
        -> Branch
             (R, Branch (B, Branch (B, va, vb, vc, vd), k, x, a), s, y,
               balance b t z (paint R c))
    | Empty, k, x, Empty -> Empty
    | Empty, k, x, Branch (R, Empty, vb, vc, vd) -> Empty
    | Empty, k, x, Branch (R, Branch (R, ve, vf, vg, vh), vb, vc, vd) -> Empty
    | Branch (B, va, vb, vc, vd), k, x, Empty -> Empty
    | Branch (B, va, vb, vc, vd), k, x, Branch (R, Empty, vf, vg, vh) -> Empty
    | Branch (B, va, vb, vc, vd), k, x,
        Branch (R, Branch (R, vi, vj, vk, vl), vf, vg, vh)
        -> Empty;;

let rec combine
  xa0 x = match xa0, x with Empty, x -> x
    | Branch (v, va, vb, vc, vd), Empty -> Branch (v, va, vb, vc, vd)
    | Branch (R, a, k, x, b), Branch (R, c, s, y, d) ->
        (match combine b c
          with Empty -> Branch (R, a, k, x, Branch (R, Empty, s, y, d))
          | Branch (R, b2, t, z, c2) ->
            Branch (R, Branch (R, a, k, x, b2), t, z, Branch (R, c2, s, y, d))
          | Branch (B, b2, t, z, c2) ->
            Branch (R, a, k, x, Branch (R, Branch (B, b2, t, z, c2), s, y, d)))
    | Branch (B, a, k, x, b), Branch (B, c, s, y, d) ->
        (match combine b c
          with Empty -> balance_left a k x (Branch (B, Empty, s, y, d))
          | Branch (R, b2, t, z, c2) ->
            Branch (R, Branch (B, a, k, x, b2), t, z, Branch (B, c2, s, y, d))
          | Branch (B, b2, t, z, c2) ->
            balance_left a k x (Branch (B, Branch (B, b2, t, z, c2), s, y, d)))
    | Branch (B, va, vb, vc, vd), Branch (R, b, k, x, c) ->
        Branch (R, combine (Branch (B, va, vb, vc, vd)) b, k, x, c)
    | Branch (R, a, k, x, b), Branch (B, va, vb, vc, vd) ->
        Branch (R, a, k, x, combine b (Branch (B, va, vb, vc, vd)));;

let rec balance_right
  a k x xa3 = match a, k, x, xa3 with
    a, k, x, Branch (R, b, s, y, c) ->
      Branch (R, a, k, x, Branch (B, b, s, y, c))
    | Branch (B, a, k, x, b), s, y, Empty ->
        balance (Branch (R, a, k, x, b)) s y Empty
    | Branch (B, a, k, x, b), s, y, Branch (B, va, vb, vc, vd) ->
        balance (Branch (R, a, k, x, b)) s y (Branch (B, va, vb, vc, vd))
    | Branch (R, a, k, x, Branch (B, b, s, y, c)), t, z, Empty ->
        Branch (R, balance (paint R a) k x b, s, y, Branch (B, c, t, z, Empty))
    | Branch (R, a, k, x, Branch (B, b, s, y, c)), t, z,
        Branch (B, va, vb, vc, vd)
        -> Branch
             (R, balance (paint R a) k x b, s, y,
               Branch (B, c, t, z, Branch (B, va, vb, vc, vd)))
    | Empty, k, x, Empty -> Empty
    | Branch (R, va, vb, vc, Empty), k, x, Empty -> Empty
    | Branch (R, va, vb, vc, Branch (R, ve, vf, vg, vh)), k, x, Empty -> Empty
    | Empty, k, x, Branch (B, va, vb, vc, vd) -> Empty
    | Branch (R, ve, vf, vg, Empty), k, x, Branch (B, va, vb, vc, vd) -> Empty
    | Branch (R, ve, vf, vg, Branch (R, vi, vj, vk, vl)), k, x,
        Branch (B, va, vb, vc, vd)
        -> Empty;;

end;; (*struct RBT_Impl*)

module RBT_Comparator_Impl : sig
  val rbt_comp_delete :
    ('a -> 'a -> Comparator.order) ->
      'a -> ('a, 'b) RBT_Impl.rbt -> ('a, 'b) RBT_Impl.rbt
  val rbt_comp_insert :
    ('a -> 'a -> Comparator.order) ->
      'a -> 'b -> ('a, 'b) RBT_Impl.rbt -> ('a, 'b) RBT_Impl.rbt
  val rbt_comp_lookup :
    ('a -> 'a -> Comparator.order) -> ('a, 'b) RBT_Impl.rbt -> 'a -> 'b option
end = struct

let rec rbt_comp_del
  c x xa2 = match c, x, xa2 with c, x, RBT_Impl.Empty -> RBT_Impl.Empty
    | c, x, RBT_Impl.Branch (uu, a, y, s, b) ->
        (match c x y with Comparator.Eq -> RBT_Impl.combine a b
          | Comparator.Lt -> rbt_comp_del_from_left c x a y s b
          | Comparator.Gt -> rbt_comp_del_from_right c x a y s b)
and rbt_comp_del_from_left
  c x xa2 y s b = match c, x, xa2, y, s, b with
    c, x, RBT_Impl.Branch (RBT_Impl.B, lt, z, v, rt), y, s, b ->
      RBT_Impl.balance_left
        (rbt_comp_del c x (RBT_Impl.Branch (RBT_Impl.B, lt, z, v, rt))) y s b
    | c, x, RBT_Impl.Empty, y, s, b ->
        RBT_Impl.Branch (RBT_Impl.R, rbt_comp_del c x RBT_Impl.Empty, y, s, b)
    | c, x, RBT_Impl.Branch (RBT_Impl.R, va, vb, vc, vd), y, s, b ->
        RBT_Impl.Branch
          (RBT_Impl.R,
            rbt_comp_del c x (RBT_Impl.Branch (RBT_Impl.R, va, vb, vc, vd)), y,
            s, b)
and rbt_comp_del_from_right
  c x a y s xa5 = match c, x, a, y, s, xa5 with
    c, x, a, y, s, RBT_Impl.Branch (RBT_Impl.B, lt, z, v, rt) ->
      RBT_Impl.balance_right a y s
        (rbt_comp_del c x (RBT_Impl.Branch (RBT_Impl.B, lt, z, v, rt)))
    | c, x, a, y, s, RBT_Impl.Empty ->
        RBT_Impl.Branch (RBT_Impl.R, a, y, s, rbt_comp_del c x RBT_Impl.Empty)
    | c, x, a, y, s, RBT_Impl.Branch (RBT_Impl.R, va, vb, vc, vd) ->
        RBT_Impl.Branch
          (RBT_Impl.R, a, y, s,
            rbt_comp_del c x (RBT_Impl.Branch (RBT_Impl.R, va, vb, vc, vd)));;

let rec rbt_comp_ins
  c f k v x4 = match c, f, k, v, x4 with
    c, f, k, v, RBT_Impl.Empty ->
      RBT_Impl.Branch (RBT_Impl.R, RBT_Impl.Empty, k, v, RBT_Impl.Empty)
    | c, f, k, v, RBT_Impl.Branch (RBT_Impl.B, l, x, y, r) ->
        (match c k x
          with Comparator.Eq -> RBT_Impl.Branch (RBT_Impl.B, l, x, f k y v, r)
          | Comparator.Lt -> RBT_Impl.balance (rbt_comp_ins c f k v l) x y r
          | Comparator.Gt -> RBT_Impl.balance l x y (rbt_comp_ins c f k v r))
    | c, f, k, v, RBT_Impl.Branch (RBT_Impl.R, l, x, y, r) ->
        (match c k x
          with Comparator.Eq -> RBT_Impl.Branch (RBT_Impl.R, l, x, f k y v, r)
          | Comparator.Lt ->
            RBT_Impl.Branch (RBT_Impl.R, rbt_comp_ins c f k v l, x, y, r)
          | Comparator.Gt ->
            RBT_Impl.Branch (RBT_Impl.R, l, x, y, rbt_comp_ins c f k v r));;

let rec rbt_comp_delete c k t = RBT_Impl.paint RBT_Impl.B (rbt_comp_del c k t);;

let rec rbt_comp_insert_with_key
  c f k v t = RBT_Impl.paint RBT_Impl.B (rbt_comp_ins c f k v t);;

let rec rbt_comp_insert c = rbt_comp_insert_with_key c (fun _ _ nv -> nv);;

let rec rbt_comp_lookup
  c x1 k = match c, x1, k with c, RBT_Impl.Empty, k -> None
    | c, RBT_Impl.Branch (uu, l, x, y, r), k ->
        (match c k x with Comparator.Eq -> Some y
          | Comparator.Lt -> rbt_comp_lookup c l k
          | Comparator.Gt -> rbt_comp_lookup c r k);;

end;; (*struct RBT_Comparator_Impl*)

module RBT_Mapping2 : sig
  type ('b, 'a) mapping_rbt
  val empty : 'a Collection_Order.ccompare -> ('a, 'b) mapping_rbt
  val delete :
    'a Collection_Order.ccompare ->
      'a -> ('a, 'b) mapping_rbt -> ('a, 'b) mapping_rbt
  val insert :
    'a Collection_Order.ccompare ->
      'a -> 'b -> ('a, 'b) mapping_rbt -> ('a, 'b) mapping_rbt
  val lookup :
    'a Collection_Order.ccompare -> ('a, 'b) mapping_rbt -> 'a -> 'b option
end = struct

type ('b, 'a) mapping_rbt = Mapping_RBT of ('b, 'a) RBT_Impl.rbt;;

let rec empty _A = Mapping_RBT RBT_Impl.Empty;;

let rec impl_of _B (Mapping_RBT x) = x;;

let rec delete _A
  xb xc =
    Mapping_RBT
      (RBT_Comparator_Impl.rbt_comp_delete
        (Option.the (Collection_Order.ccompare _A)) xb (impl_of _A xc));;

let rec insert _A
  xc xd xe =
    Mapping_RBT
      (RBT_Comparator_Impl.rbt_comp_insert
        (Option.the (Collection_Order.ccompare _A)) xc xd (impl_of _A xe));;

let rec lookup _A
  xa = RBT_Comparator_Impl.rbt_comp_lookup
         (Option.the (Collection_Order.ccompare _A)) (impl_of _A xa);;

end;; (*struct RBT_Mapping2*)

module AssocList : sig
  val delete :
    'a HOL.equal -> 'a -> ('a, 'b) DAList.alist -> ('a, 'b) DAList.alist
end = struct

let rec delete_aux _A
  k x1 = match k, x1 with k, [] -> []
    | ka, (k, v) :: xs ->
        (if HOL.eq _A ka k then xs else (k, v) :: delete_aux _A ka xs);;

let rec delete _A xb xc = DAList.Alist (delete_aux _A xb (DAList.impl_of xc));;

end;; (*struct AssocList*)

module Mapping : sig
  type ('a, 'b) mapping = Assoc_List_Mapping of ('a, 'b) DAList.alist |
    RBT_Mapping of ('a, 'b) RBT_Mapping2.mapping_rbt |
    Mapping of ('a -> 'b option)
  val delete :
    'a Collection_Order.ccompare * 'a HOL.equal ->
      'a -> ('a, 'b) mapping -> ('a, 'b) mapping
  val lookup :
    'a Collection_Order.ccompare * 'a HOL.equal ->
      ('a, 'b) mapping -> 'a -> 'b option
  val update :
    'a Collection_Order.ccompare * 'a HOL.equal ->
      'a -> 'b -> ('a, 'b) mapping -> ('a, 'b) mapping
end = struct

type ('a, 'b) mapping = Assoc_List_Mapping of ('a, 'b) DAList.alist |
  RBT_Mapping of ('a, 'b) RBT_Mapping2.mapping_rbt |
  Mapping of ('a -> 'b option);;

let rec delete (_A1, _A2)
  k x1 = match k, x1 with
    k, RBT_Mapping t ->
      (match Collection_Order.ccompare _A1
        with None ->
          failwith "delete RBT_Mapping: ccompare = None"
            (fun _ -> delete (_A1, _A2) k (RBT_Mapping t))
        | Some _ -> RBT_Mapping (RBT_Mapping2.delete _A1 k t))
    | k, Assoc_List_Mapping al -> Assoc_List_Mapping (AssocList.delete _A2 k al)
    | k, Mapping m -> Mapping (Fun.fun_upd _A2 m k None);;

let rec lookup (_A1, _A2)
  = function RBT_Mapping t -> RBT_Mapping2.lookup _A1 t
    | Assoc_List_Mapping al -> DAList.lookup _A2 al;;

let rec update (_A1, _A2)
  k v x2 = match k, v, x2 with
    k, v, RBT_Mapping t ->
      (match Collection_Order.ccompare _A1
        with None ->
          failwith "update RBT_Mapping: ccompare = None"
            (fun _ -> update (_A1, _A2) k v (RBT_Mapping t))
        | Some _ -> RBT_Mapping (RBT_Mapping2.insert _A1 k v t))
    | k, v, Assoc_List_Mapping al ->
        Assoc_List_Mapping (DAList.update _A2 k v al)
    | k, v, Mapping m -> Mapping (Fun.fun_upd _A2 m k (Some v));;

end;; (*struct Mapping*)

module Phantom_Type : sig
  type ('a, 'b) phantom = Phantom of 'b
  val of_phantom : ('a, 'b) phantom -> 'b
end = struct

type ('a, 'b) phantom = Phantom of 'b;;

let rec of_phantom (Phantom x) = x;;

end;; (*struct Phantom_Type*)

module Mapping_Impl : sig
  type mapping_impl
  val mapping_empty :
    'a Collection_Order.ccompare -> mapping_impl -> ('a, 'b) Mapping.mapping
  val mapping_impl_nat : (Arith.nat, mapping_impl) Phantom_Type.phantom
end = struct

type mapping_impl = Mapping_Choose | Mapping_Assoc_List | Mapping_RBT |
  Mapping_Mapping;;

let rec mapping_empty_choose _A
  = (match Collection_Order.ccompare _A
      with None -> Mapping.Assoc_List_Mapping DAList.empty
      | Some _ -> Mapping.RBT_Mapping (RBT_Mapping2.empty _A));;

let rec mapping_empty _A
  = function Mapping_RBT -> Mapping.RBT_Mapping (RBT_Mapping2.empty _A)
    | Mapping_Assoc_List -> Mapping.Assoc_List_Mapping DAList.empty
    | Mapping_Mapping -> Mapping.Mapping (fun _ -> None)
    | Mapping_Choose -> mapping_empty_choose _A;;

let mapping_impl_nat : (Arith.nat, mapping_impl) Phantom_Type.phantom
  = Phantom_Type.Phantom Mapping_RBT;;

end;; (*struct Mapping_Impl*)

module MSpec_Library : sig
  type 'a mval =
    Bv of Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0
            Numeral_Type.bit0
            Word.word
    | Iv of Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0
              Numeral_Type.bit0
              Numeral_Type.bit0
              Numeral_Type.bit0
              Numeral_Type.bit0
              Word.word
    | Pv of 'a
  type mtype = I8 | I64 | Ptr of mtype
  val val_type : 'a mval -> mtype
end = struct

type 'a mval =
  Bv of Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
          Word.word
  | Iv of Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0
            Numeral_Type.bit0
            Numeral_Type.bit0
            Numeral_Type.bit0
            Numeral_Type.bit0
            Word.word
  | Pv of 'a;;

type mtype = I8 | I64 | Ptr of mtype;;

let rec val_type v = (match v with Bv _ -> I8 | Iv _ -> I64 | Pv _ -> Ptr I64);;

end;; (*struct MSpec_Library*)

module Fat_Capability : sig
  type 'a capability_ext = Capability_ext of Arith.nat * bool * 'a
  type 'a pre_capability_ext =
    Pre_capability_ext of bool * bool * bool * bool * Arith.nat * Arith.nat * 'a
  val decode_cap :
    bool ->
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Word.word ->
        unit capability_ext pre_capability_ext
  val perm_cap_store : 'a pre_capability_ext -> bool
  val perm_cap_load : 'a pre_capability_ext -> bool
  val perm_store : 'a pre_capability_ext -> bool
  val perm_load : 'a pre_capability_ext -> bool
  val base : 'a pre_capability_ext -> Arith.nat
  val addr : 'a pre_capability_ext -> Arith.nat
  val len : 'a capability_ext pre_capability_ext -> Arith.nat
  val encode_cap :
    unit capability_ext pre_capability_ext ->
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Word.word
  val tag : 'a capability_ext pre_capability_ext -> bool
  val tag_update :
    (bool -> bool) ->
      'a capability_ext pre_capability_ext ->
        'a capability_ext pre_capability_ext
end = struct

type 'a capability_ext = Capability_ext of Arith.nat * bool * 'a;;

type 'a pre_capability_ext =
  Pre_capability_ext of bool * bool * bool * bool * Arith.nat * Arith.nat * 'a;;

let rec word_cap_perm_cap_store
  wc = (if Word.equal_word
             (Type_Length.len_bit0
               (Type_Length.len_bit0
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0 Type_Length.len_num1))))))
             (Word.and_word
               (Type_Length.len_bit0
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0 Type_Length.len_num1))))))
               (Word.one_word
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0 Type_Length.len_num1)))))))
               (Product_Type.fst
                 (Word.word_split
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0 Type_Length.len_num1)))))))
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0 Type_Length.len_num1))))))
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0 Type_Length.len_num1))))))
                   (Product_Type.fst
                     (Word.word_split
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0
                                 (Type_Length.len_bit0
                                   (Type_Length.len_bit0
                                     (Type_Length.len_bit0
                                       Type_Length.len_num1))))))))
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0
                                 (Type_Length.len_bit0
                                   (Type_Length.len_bit0
                                     Type_Length.len_num1)))))))
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0
                                 (Type_Length.len_bit0
                                   (Type_Length.len_bit0
                                     Type_Length.len_num1)))))))
                       wc)))))
             (Word.zero_word
               (Type_Length.len_bit0
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0 Type_Length.len_num1)))))))
         then false else true);;

let rec word_cap_perm_cap_load
  wc = (if Word.equal_word
             (Type_Length.len_bit0
               (Type_Length.len_bit0
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0 Type_Length.len_num1))))))
             (Word.and_word
               (Type_Length.len_bit0
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0 Type_Length.len_num1))))))
               (Word.of_int
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0 Type_Length.len_num1))))))
                 (Arith.Pos (Arith.Bit0 (Arith.Bit0 Arith.One))))
               (Product_Type.fst
                 (Word.word_split
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0 Type_Length.len_num1)))))))
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0 Type_Length.len_num1))))))
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0 Type_Length.len_num1))))))
                   (Product_Type.fst
                     (Word.word_split
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0
                                 (Type_Length.len_bit0
                                   (Type_Length.len_bit0
                                     (Type_Length.len_bit0
                                       Type_Length.len_num1))))))))
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0
                                 (Type_Length.len_bit0
                                   (Type_Length.len_bit0
                                     Type_Length.len_num1)))))))
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0
                                 (Type_Length.len_bit0
                                   (Type_Length.len_bit0
                                     Type_Length.len_num1)))))))
                       wc)))))
             (Word.zero_word
               (Type_Length.len_bit0
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0 Type_Length.len_num1)))))))
         then false else true);;

let rec word_cap_perm_store
  wc = (if Word.equal_word
             (Type_Length.len_bit0
               (Type_Length.len_bit0
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0 Type_Length.len_num1))))))
             (Word.and_word
               (Type_Length.len_bit0
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0 Type_Length.len_num1))))))
               (Word.of_int
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0 Type_Length.len_num1))))))
                 (Arith.Pos (Arith.Bit0 Arith.One)))
               (Product_Type.fst
                 (Word.word_split
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0 Type_Length.len_num1)))))))
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0 Type_Length.len_num1))))))
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0 Type_Length.len_num1))))))
                   (Product_Type.fst
                     (Word.word_split
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0
                                 (Type_Length.len_bit0
                                   (Type_Length.len_bit0
                                     (Type_Length.len_bit0
                                       Type_Length.len_num1))))))))
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0
                                 (Type_Length.len_bit0
                                   (Type_Length.len_bit0
                                     Type_Length.len_num1)))))))
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0
                                 (Type_Length.len_bit0
                                   (Type_Length.len_bit0
                                     Type_Length.len_num1)))))))
                       wc)))))
             (Word.zero_word
               (Type_Length.len_bit0
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0 Type_Length.len_num1)))))))
         then false else true);;

let rec word_cap_perm_load
  wc = (if Word.equal_word
             (Type_Length.len_bit0
               (Type_Length.len_bit0
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0 Type_Length.len_num1))))))
             (Word.and_word
               (Type_Length.len_bit0
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0 Type_Length.len_num1))))))
               (Word.of_int
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0 Type_Length.len_num1))))))
                 (Arith.Pos (Arith.Bit0 (Arith.Bit0 (Arith.Bit0 Arith.One)))))
               (Product_Type.fst
                 (Word.word_split
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0 Type_Length.len_num1)))))))
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0 Type_Length.len_num1))))))
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0 Type_Length.len_num1))))))
                   (Product_Type.fst
                     (Word.word_split
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0
                                 (Type_Length.len_bit0
                                   (Type_Length.len_bit0
                                     (Type_Length.len_bit0
                                       Type_Length.len_num1))))))))
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0
                                 (Type_Length.len_bit0
                                   (Type_Length.len_bit0
                                     Type_Length.len_num1)))))))
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0
                                 (Type_Length.len_bit0
                                   (Type_Length.len_bit0
                                     Type_Length.len_num1)))))))
                       wc)))))
             (Word.zero_word
               (Type_Length.len_bit0
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0 Type_Length.len_num1)))))))
         then false else true);;

let rec word_cap_base
  w = Product_Type.fst
        (Word.word_split
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1)))))))
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0 Type_Length.len_num1))))))
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0 Type_Length.len_num1))))))
          (Product_Type.snd
            (Word.word_split
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0
                            (Type_Length.len_bit0 Type_Length.len_num1))))))))
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0 Type_Length.len_num1)))))))
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0 Type_Length.len_num1)))))))
              w)));;

let rec word_cap_addr
  w = Product_Type.snd
        (Word.word_split
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1)))))))
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0 Type_Length.len_num1))))))
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0 Type_Length.len_num1))))))
          (Product_Type.snd
            (Word.word_split
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0
                            (Type_Length.len_bit0 Type_Length.len_num1))))))))
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0 Type_Length.len_num1)))))))
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0 Type_Length.len_num1)))))))
              w)));;

let rec word_cap_len
  w = Product_Type.snd
        (Word.word_split
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1)))))))
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0 Type_Length.len_num1))))))
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0 Type_Length.len_num1))))))
          (Product_Type.fst
            (Word.word_split
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0
                            (Type_Length.len_bit0 Type_Length.len_num1))))))))
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0 Type_Length.len_num1)))))))
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0 Type_Length.len_num1)))))))
              w)));;

let rec decode_cap
  t c = Pre_capability_ext
          (word_cap_perm_load c, word_cap_perm_cap_load c,
            word_cap_perm_store c, word_cap_perm_cap_store c,
            Word.the_nat
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0 Type_Length.len_num1))))))
              (word_cap_base c),
            Word.the_nat
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0 Type_Length.len_num1))))))
              (word_cap_addr c),
            Capability_ext
              (Word.the_nat
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0 Type_Length.len_num1))))))
                 (word_cap_len c),
                t, ()));;

let rec perm_cap_store
  (Pre_capability_ext
    (perm_load, perm_cap_load, perm_store, perm_cap_store, base, addr, more))
    = perm_cap_store;;

let rec abs_cap_perm_cap_store
  c = (if perm_cap_store c then Word.one_word Type_Length.len_num1
        else Word.zero_word Type_Length.len_num1);;

let rec perm_cap_load
  (Pre_capability_ext
    (perm_load, perm_cap_load, perm_store, perm_cap_store, base, addr, more))
    = perm_cap_load;;

let rec abs_cap_perm_cap_load
  c = (if perm_cap_load c then Word.one_word Type_Length.len_num1
        else Word.zero_word Type_Length.len_num1);;

let rec perm_store
  (Pre_capability_ext
    (perm_load, perm_cap_load, perm_store, perm_cap_store, base, addr, more))
    = perm_store;;

let rec abs_cap_perm_store
  c = (if perm_store c then Word.one_word Type_Length.len_num1
        else Word.zero_word Type_Length.len_num1);;

let rec perm_load
  (Pre_capability_ext
    (perm_load, perm_cap_load, perm_store, perm_cap_store, base, addr, more))
    = perm_load;;

let rec abs_cap_perm_load
  c = (if perm_load c then Word.one_word Type_Length.len_num1
        else Word.zero_word Type_Length.len_num1);;

let abs_cap_empty :
  Numeral_Type.num1 Numeral_Type.bit1 Numeral_Type.bit1 Numeral_Type.bit1
    Numeral_Type.bit0
    Numeral_Type.bit0
    Word.word
  = Word.zero_word
      (Type_Length.len_bit0
        (Type_Length.len_bit0
          (Type_Length.len_bit1
            (Type_Length.len0_bit1
              (Type_Length.len0_bit1 Type_Length.len0_num1)))));;

let rec abs_cap_metadata
  c = Word.word_cat
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit1
              (Type_Length.len0_bit1
                (Type_Length.len0_bit1 Type_Length.len0_num1)))))
        (Type_Length.len_bit0 (Type_Length.len_bit0 Type_Length.len_num1))
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1))))))
        abs_cap_empty
        (Word.word_rcat Type_Length.len_num1
          (Type_Length.len_bit0 (Type_Length.len_bit0 Type_Length.len_num1))
          [abs_cap_perm_load c; abs_cap_perm_cap_load c; abs_cap_perm_store c;
            abs_cap_perm_cap_store c]);;

let rec base
  (Pre_capability_ext
    (perm_load, perm_cap_load, perm_store, perm_cap_store, base, addr, more))
    = base;;

let rec abs_cap_base
  c = Word.of_nat
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1))))))
        (base c);;

let rec addr
  (Pre_capability_ext
    (perm_load, perm_cap_load, perm_store, perm_cap_store, base, addr, more))
    = addr;;

let rec abs_cap_addr
  c = Word.of_nat
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1))))))
        (addr c);;

let rec len
  (Pre_capability_ext
    (perm_load, perm_cap_load, perm_store, perm_cap_store, base, addr,
      Capability_ext (len, tag, more)))
    = len;;

let rec abs_cap_len
  c = Word.of_nat
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1))))))
        (len c);;

let rec encode_cap
  c = Word.word_rcat
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0 Type_Length.len_num1)))))))
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1))))))))
        [Word.word_rcat
           (Type_Length.len_bit0
             (Type_Length.len_bit0
               (Type_Length.len_bit0
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0 Type_Length.len_num1))))))
           (Type_Length.len_bit0
             (Type_Length.len_bit0
               (Type_Length.len_bit0
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0 Type_Length.len_num1)))))))
           [abs_cap_metadata c; abs_cap_len c];
          Word.word_rcat
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1))))))
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0 Type_Length.len_num1)))))))
            [abs_cap_base c; abs_cap_addr c]];;

let rec tag
  (Pre_capability_ext
    (perm_load, perm_cap_load, perm_store, perm_cap_store, base, addr,
      Capability_ext (len, tag, more)))
    = tag;;

let rec tag_update
  taga (Pre_capability_ext
         (perm_load, perm_cap_load, perm_store, perm_cap_store, base, addr,
           Capability_ext (len, tag, more)))
    = Pre_capability_ext
        (perm_load, perm_cap_load, perm_store, perm_cap_store, base, addr,
          Capability_ext (len, taga tag, more));;

end;; (*struct Fat_Capability*)

module CHERI_Memory_Model_Base : sig
  type 'a heap_ext
  val free :
    unit heap_ext ->
      unit Fat_Capability.capability_ext Fat_Capability.pre_capability_ext ->
        MSpec_Library.mtype ->
          (unit heap_ext *
            unit Fat_Capability.capability_ext
              Fat_Capability.pre_capability_ext) option
  val load :
    unit heap_ext ->
      unit Fat_Capability.capability_ext Fat_Capability.pre_capability_ext ->
        MSpec_Library.mtype ->
          unit Fat_Capability.capability_ext Fat_Capability.pre_capability_ext
            MSpec_Library.mval option
  val store :
    unit heap_ext ->
      unit Fat_Capability.capability_ext Fat_Capability.pre_capability_ext ->
        unit Fat_Capability.capability_ext Fat_Capability.pre_capability_ext
          MSpec_Library.mval ->
          unit heap_ext option
  val curr : 'a heap_ext -> Arith.nat
  val allocate :
    unit heap_ext ->
      MSpec_Library.mtype ->
        (unit heap_ext *
          unit Fat_Capability.capability_ext
            Fat_Capability.pre_capability_ext) option
  val heap_init : unit heap_ext
  val integer_of_word8 :
    Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
      Word.word ->
      Z.t
  val word8_of_integer :
    Z.t ->
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Word.word
  val integer_of_word64 :
    Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
      Numeral_Type.bit0
      Numeral_Type.bit0
      Numeral_Type.bit0
      Word.word ->
      Z.t
  val word64_of_integer :
    Z.t ->
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Word.word
end = struct

type 'a heap_ext =
  Heap_ext of
    Arith.nat *
      (Arith.nat,
        Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
          Word.word)
        Mapping.mapping *
      (Arith.nat, bool) Mapping.mapping * 'a;;

let rec gamma_update
  gammaa (Heap_ext (curr, gamma, pi, more)) =
    Heap_ext (curr, gammaa gamma, pi, more);;

let rec gamma (Heap_ext (curr, gamma, pi, more)) = gamma;;

let rec allocd_region_rec
  uu uv x2 uw = match uu, uv, x2, uw with uu, uv, Arith.Zero_nat, uw -> true
    | h, p, Arith.Suc n, b ->
        Product_Type.equal_bool b
          (not (Option.is_none
                 (Mapping.lookup
                   (Collection_Order.ccompare_nat, Arith.equal_nat) (gamma h)
                   (Arith.plus_nat p n)))) &&
          allocd_region_rec h p n b;;

let rec allocd_region_prim
  h lo hi b =
    Arith.less_nat lo hi && allocd_region_rec h lo (Arith.minus_nat hi lo) b;;

let rec allocd_region h lo hi b = allocd_region_prim h lo hi b;;

let rec map_free
  m p x2 = match m, p, x2 with m, p, Arith.Zero_nat -> m
    | m, p, Arith.Suc n ->
        Mapping.delete (Collection_Order.ccompare_nat, Arith.equal_nat)
          (Arith.plus_nat p n) (map_free m p n);;

let rec free
  h cp uu =
    (if Product_Type.equal_bool (Fat_Capability.tag cp) false ||
          not (allocd_region h (Fat_Capability.base cp)
                (Arith.plus_nat (Fat_Capability.base cp)
                  (Fat_Capability.len cp))
                true)
      then None
      else Some (gamma_update
                   (fun _ ->
                     map_free (gamma h) (Fat_Capability.base cp)
                       (Fat_Capability.len cp))
                   h,
                  Fat_Capability.tag_update (fun _ -> false) cp));;

let rec retrieve_val
  uu uv x2 = match uu, uv, x2 with uu, uv, Arith.Zero_nat -> []
    | f, pt, Arith.Suc n ->
        Option.the
          (Mapping.lookup (Collection_Order.ccompare_nat, Arith.equal_nat) f
            pt) ::
          retrieve_val f (Arith.Suc pt) n;;

let rec pi (Heap_ext (curr, gamma, pi, more)) = pi;;

let rec retrieve_tval
  h cp t =
    (match t
      with MSpec_Library.I8 ->
        MSpec_Library.Bv
          (Lista.hd
            (retrieve_val (gamma h) (Fat_Capability.addr cp) Arith.one_nat))
      | MSpec_Library.I64 ->
        MSpec_Library.Iv
          (Word.word_rcat
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0 Type_Length.len_num1)))
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1))))))
            (retrieve_val (gamma h) (Fat_Capability.addr cp)
              (Arith.nat_of_num
                (Arith.Bit0 (Arith.Bit0 (Arith.Bit0 Arith.One))))))
      | MSpec_Library.Ptr _ ->
        MSpec_Library.Pv
          (Fat_Capability.decode_cap
            (if Fat_Capability.perm_cap_load cp
              then Option.the
                     (Mapping.lookup
                       (Collection_Order.ccompare_nat, Arith.equal_nat) (pi h)
                       (Fat_Capability.addr cp))
              else false)
            (Word.word_rcat
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1)))
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0
                            (Type_Length.len_bit0 Type_Length.len_num1))))))))
              (retrieve_val (gamma h) (Fat_Capability.addr cp)
                (Arith.nat_of_num
                  (Arith.Bit0
                    (Arith.Bit0
                      (Arith.Bit0 (Arith.Bit0 (Arith.Bit0 Arith.One))))))))));;

let rec sizeof
  t = (match t with MSpec_Library.I8 -> Arith.one_nat
        | MSpec_Library.I64 ->
          Arith.nat_of_num (Arith.Bit0 (Arith.Bit0 (Arith.Bit0 Arith.One)))
        | MSpec_Library.Ptr _ ->
          Arith.nat_of_num
            (Arith.Bit0
              (Arith.Bit0 (Arith.Bit0 (Arith.Bit0 (Arith.Bit0 Arith.One))))));;

let rec load
  h cp t =
    (if Product_Type.equal_bool (Fat_Capability.tag cp) false then None
      else (if Product_Type.equal_bool (Fat_Capability.perm_load cp) false
             then None
             else (if Arith.less_nat
                        (Arith.plus_nat (Fat_Capability.base cp)
                          (Fat_Capability.len cp))
                        (Arith.plus_nat (Fat_Capability.addr cp) (sizeof t))
                    then None
                    else (if Arith.less_nat (Fat_Capability.addr cp)
                               (Fat_Capability.base cp)
                           then None
                           else (if not (Arith.equal_nata
  (Arith.modulo_nat (Fat_Capability.addr cp) (sizeof t)) Arith.Zero_nat)
                                  then None
                                  else Some (retrieve_tval h cp t))))));;

let rec pi_update
  pia (Heap_ext (curr, gamma, pi, more)) =
    Heap_ext (curr, gamma, pia pi, more);;

let rec store_val
  f uu x2 = match f, uu, x2 with f, uu, [] -> f
    | f, pt, v :: vs ->
        store_val
          (Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat) pt v
            f)
          (Arith.plus_nat pt Arith.one_nat) vs;;

let rec store_tval
  h cp v =
    (match v
      with MSpec_Library.Bv b ->
        pi_update
          (fun _ ->
            Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat)
              (if Arith.equal_nata
                    (Arith.modulo_nat (Fat_Capability.addr cp)
                      (Arith.nat_of_num
                        (Arith.Bit0
                          (Arith.Bit0
                            (Arith.Bit0 (Arith.Bit0 (Arith.Bit0 Arith.One)))))))
                    Arith.Zero_nat
                then Fat_Capability.addr cp
                else Arith.minus_nat (Fat_Capability.addr cp)
                       (Arith.modulo_nat (Fat_Capability.addr cp)
                         (Arith.nat_of_num
                           (Arith.Bit0
                             (Arith.Bit0
                               (Arith.Bit0
                                 (Arith.Bit0 (Arith.Bit0 Arith.One))))))))
              false (pi h))
          (gamma_update
            (fun _ -> store_val (gamma h) (Fat_Capability.addr cp) [b]) h)
      | MSpec_Library.Iv i ->
        pi_update
          (fun _ ->
            Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat)
              (if Arith.equal_nata
                    (Arith.modulo_nat (Fat_Capability.addr cp)
                      (Arith.nat_of_num
                        (Arith.Bit0
                          (Arith.Bit0
                            (Arith.Bit0 (Arith.Bit0 (Arith.Bit0 Arith.One)))))))
                    Arith.Zero_nat
                then Fat_Capability.addr cp
                else Arith.minus_nat (Fat_Capability.addr cp)
                       (Arith.modulo_nat (Fat_Capability.addr cp)
                         (Arith.nat_of_num
                           (Arith.Bit0
                             (Arith.Bit0
                               (Arith.Bit0
                                 (Arith.Bit0 (Arith.Bit0 Arith.One))))))))
              false (pi h))
          (gamma_update
            (fun _ ->
              store_val (gamma h) (Fat_Capability.addr cp)
                (Rsplit.word_rsplit
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0
                            (Type_Length.len_bit0 Type_Length.len_num1))))))
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1)))
                  i))
            h)
      | MSpec_Library.Pv p ->
        pi_update
          (fun _ ->
            Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat)
              (Fat_Capability.addr cp) (Fat_Capability.tag p) (pi h))
          (gamma_update
            (fun _ ->
              store_val (gamma h) (Fat_Capability.addr cp)
                (Rsplit.word_rsplit
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0
                            (Type_Length.len_bit0
                              (Type_Length.len_bit0
                                (Type_Length.len_bit0
                                  Type_Length.len_num1))))))))
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1)))
                  (Fat_Capability.encode_cap p)))
            h));;

let rec store
  h cap vala =
    (if Product_Type.equal_bool (Fat_Capability.tag cap) false then None
      else (if Product_Type.equal_bool (Fat_Capability.perm_store cap) false
             then None
             else (if (match vala with MSpec_Library.Bv _ -> false
                        | MSpec_Library.Iv _ -> false
                        | MSpec_Library.Pv p ->
                          not (Fat_Capability.perm_cap_store cap) &&
                            Fat_Capability.tag p)
                    then None
                    else (if Arith.less_nat
                               (Arith.plus_nat (Fat_Capability.base cap)
                                 (Fat_Capability.len cap))
                               (Arith.plus_nat (Fat_Capability.addr cap)
                                 (sizeof (MSpec_Library.val_type vala)))
                           then None
                           else (if Arith.less_nat (Fat_Capability.addr cap)
                                      (Fat_Capability.base cap)
                                  then None
                                  else (if not
     (Arith.equal_nata
       (Arith.modulo_nat (Fat_Capability.addr cap)
         (sizeof (MSpec_Library.val_type vala)))
       Arith.Zero_nat)
 then None else Some (store_tval h cap vala)))))));;

let rec curr_update
  curra (Heap_ext (curr, gamma, pi, more)) =
    Heap_ext (curra curr, gamma, pi, more);;

let rec typ_offset
  t a = Arith.modulo_nat
          (Arith.minus_nat (sizeof t) (Arith.modulo_nat a (sizeof t)))
          (sizeof t);;

let rec map_alloc
  m p x2 = match m, p, x2 with m, p, Arith.Zero_nat -> m
    | m, p, Arith.Suc n ->
        Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat)
          (Arith.plus_nat p n)
          (Word.zero_word
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0 Type_Length.len_num1))))
          (map_alloc m p n);;

let rec curr (Heap_ext (curr, gamma, pi, more)) = curr;;

let rec allocate
  h t = (let b =
           (match t with MSpec_Library.I8 -> false | MSpec_Library.I64 -> false
             | MSpec_Library.Ptr _ -> true)
           in
          (if not (allocd_region h (curr h)
                    (Arith.plus_nat (Arith.plus_nat (curr h) (sizeof t))
                      (typ_offset t (curr h)))
                    false)
            then None
            else Some (gamma_update
                         (fun _ ->
                           map_alloc (gamma h)
                             (Arith.plus_nat (curr h) (typ_offset t (curr h)))
                             (sizeof t))
                         (curr_update
                           (fun _ ->
                             Arith.plus_nat (Arith.plus_nat (curr h) (sizeof t))
                               (typ_offset t (curr h)))
                           h),
                        Fat_Capability.Pre_capability_ext
                          (true, b, true, b,
                            Arith.plus_nat (curr h) (typ_offset t (curr h)),
                            Arith.plus_nat (curr h) (typ_offset t (curr h)),
                            Fat_Capability.Capability_ext
                              (sizeof t, true, ())))));;

let heap_init : unit heap_ext
  = Heap_ext
      (Arith.Zero_nat,
        Mapping_Impl.mapping_empty Collection_Order.ccompare_nat
          (Phantom_Type.of_phantom Mapping_Impl.mapping_impl_nat),
        Mapping_Impl.mapping_empty Collection_Order.ccompare_nat
          (Phantom_Type.of_phantom Mapping_Impl.mapping_impl_nat),
        ());;

let rec integer_of_word _A x = Arith.integer_of_int (Word.the_signed_int _A x);;

let rec word_of_integer _A x = Word.of_int _A (Arith.int_of_integer x);;

let rec integer_of_word8
  x = integer_of_word
        (Type_Length.len_bit0
          (Type_Length.len_bit0 (Type_Length.len_bit0 Type_Length.len_num1)))
        x;;

let rec word8_of_integer
  x = word_of_integer
        (Type_Length.len_bit0
          (Type_Length.len_bit0 (Type_Length.len_bit0 Type_Length.len_num1)))
        x;;

let rec integer_of_word64
  x = integer_of_word
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1))))))
        x;;

let rec word64_of_integer
  x = word_of_integer
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1))))))
        x;;

end;; (*struct CHERI_Memory_Model_Base*)


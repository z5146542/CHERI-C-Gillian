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
  val equal_unit : unit HOL.equal
  val apsnd : ('a -> 'b) -> 'c * 'a -> 'c * 'b
  val fst : 'a * 'b -> 'a
  val snd : 'a * 'b -> 'b
  val equal_bool : bool -> bool -> bool
  val equal_prod : 'a HOL.equal -> 'b HOL.equal -> 'a * 'b -> 'a * 'b -> bool
end = struct

let rec equal_unita u v = true;;

let equal_unit = ({HOL.equal = equal_unita} : unit HOL.equal);;

let rec apsnd f (x, y) = (x, f y);;

let rec fst (x1, x2) = x1;;

let rec snd (x1, x2) = x2;;

let rec equal_bool p pa = match p, pa with p, true -> p
                     | p, false -> not p
                     | true, p -> p
                     | false, p -> not p;;

let rec equal_prod _A _B
  (x1, x2) (y1, y2) = HOL.eq _A x1 y1 && HOL.eq _B x2 y2;;

end;; (*struct Product_Type*)

module Orderings : sig
  type 'a ord = {less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool}
  val less_eq : 'a ord -> 'a -> 'a -> bool
  val less : 'a ord -> 'a -> 'a -> bool
  type 'a preorder = {ord_preorder : 'a ord}
  type 'a order = {preorder_order : 'a preorder}
  type 'a linorder = {order_linorder : 'a order}
  val max : 'a ord -> 'a -> 'a -> 'a
  val min : 'a ord -> 'a -> 'a -> 'a
end = struct

type 'a ord = {less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool};;
let less_eq _A = _A.less_eq;;
let less _A = _A.less;;

type 'a preorder = {ord_preorder : 'a ord};;

type 'a order = {preorder_order : 'a preorder};;

type 'a linorder = {order_linorder : 'a order};;

let rec max _A a b = (if less_eq _A a b then b else a);;

let rec min _A a b = (if less_eq _A a b then a else b);;

end;; (*struct Orderings*)

module Arith : sig
  type int = Int_of_integer of Z.t
  val integer_of_int : int -> Z.t
  val equal_inta : int -> int -> bool
  val equal_int : int HOL.equal
  val times_inta : int -> int -> int
  type 'a times = {times : 'a -> 'a -> 'a}
  val times : 'a times -> 'a -> 'a -> 'a
  type 'a dvd
  type num = One | Bit0 of num | Bit1 of num
  val one_inta : int
  type 'a one = {one : 'a}
  val one : 'a one -> 'a
  val uminus_inta : int -> int
  val minus_inta : int -> int -> int
  val zero_inta : int
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
  val divide_inta : int -> int -> int
  type 'a divide = {divide : 'a -> 'a -> 'a}
  val divide : 'a divide -> 'a -> 'a -> 'a
  val modulo_inta : int -> int -> int
  type 'a modulo =
    {divide_modulo : 'a divide; dvd_modulo : 'a dvd; modulo : 'a -> 'a -> 'a}
  val modulo : 'a modulo -> 'a -> 'a -> 'a
  type 'a monoid_mult
  type 'a semiring_numeral
  type 'a zero_neq_one
  type 'a semiring_1
  type 'a semiring_1_cancel
  type 'a neg_numeral =
    {group_add_neg_numeral : 'a group_add; numeral_neg_numeral : 'a numeral}
  type 'a ring_1 =
    {neg_numeral_ring_1 : 'a neg_numeral; ring_ring_1 : 'a ring;
      semiring_1_cancel_ring_1 : 'a semiring_1_cancel}
  val zero_neq_one_int : int zero_neq_one
  val less_eq_int : int -> int -> bool
  val less_int : int -> int -> bool
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
  type nat
  val integer_of_nat : nat -> Z.t
  val equal_nata : nat -> nat -> bool
  val equal_nat : nat HOL.equal
  val less_nat : nat -> nat -> bool
  val ord_nat : nat Orderings.ord
  val linorder_nat : nat Orderings.linorder
  val equal_integer : Z.t HOL.equal
  val zero_integer : Z.t zero
  val ord_integer : Z.t Orderings.ord
  val linorder_integer : Z.t Orderings.linorder
  val nat : int -> nat
  val plus_nat : nat -> nat -> nat
  val one_nat : nat
  val suc : nat -> nat
  val dvd : 'a HOL.equal * 'a semidom_modulo -> 'a -> 'a -> bool
  val minus_nat : nat -> nat -> nat
  val zero_nat : nat
  val power : 'a power -> 'a -> nat -> 'a
  val abs_int : int -> int
  val int_of_nat : nat -> int
  val nat_of_integer : Z.t -> nat
  val times_nat : nat -> nat -> nat
  val of_bool : 'a zero_neq_one -> bool -> 'a
  val modulo_nat : nat -> nat -> nat
end = struct

type int = Int_of_integer of Z.t;;

let rec integer_of_int (Int_of_integer k) = k;;

let rec equal_inta k l = Z.equal (integer_of_int k) (integer_of_int l);;

let equal_int = ({HOL.equal = equal_inta} : int HOL.equal);;

let rec times_inta
  k l = Int_of_integer (Z.mul (integer_of_int k) (integer_of_int l));;

type 'a times = {times : 'a -> 'a -> 'a};;
let times _A = _A.times;;

type 'a dvd = {times_dvd : 'a times};;

let times_int = ({times = times_inta} : int times);;

let dvd_int = ({times_dvd = times_int} : int dvd);;

type num = One | Bit0 of num | Bit1 of num;;

let one_inta : int = Int_of_integer (Z.of_int 1);;

type 'a one = {one : 'a};;
let one _A = _A.one;;

let one_int = ({one = one_inta} : int one);;

let rec uminus_inta k = Int_of_integer (Z.neg (integer_of_int k));;

let rec minus_inta
  k l = Int_of_integer (Z.sub (integer_of_int k) (integer_of_int l));;

let zero_inta : int = Int_of_integer Z.zero;;

let rec plus_inta
  k l = Int_of_integer (Z.add (integer_of_int k) (integer_of_int l));;

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

let zero_int = ({zero = zero_inta} : int zero);;

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

let rec divide_integer k l = Product_Type.fst (divmod_integer k l);;

let rec divide_inta
  k l = Int_of_integer (divide_integer (integer_of_int k) (integer_of_int l));;

type 'a divide = {divide : 'a -> 'a -> 'a};;
let divide _A = _A.divide;;

let divide_int = ({divide = divide_inta} : int divide);;

let rec modulo_integer k l = Product_Type.snd (divmod_integer k l);;

let rec modulo_inta
  k l = Int_of_integer (modulo_integer (integer_of_int k) (integer_of_int l));;

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

type 'a zero_neq_one =
  {one_zero_neq_one : 'a one; zero_zero_neq_one : 'a zero};;

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

let zero_neq_one_int =
  ({one_zero_neq_one = one_int; zero_zero_neq_one = zero_int} :
    int zero_neq_one);;

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

let rec less_eq_int k l = Z.leq (integer_of_int k) (integer_of_int l);;

let rec less_int k l = Z.lt (integer_of_int k) (integer_of_int l);;

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

type nat = Nat of Z.t;;

let rec integer_of_nat (Nat x) = x;;

let rec equal_nata m n = Z.equal (integer_of_nat m) (integer_of_nat n);;

let equal_nat = ({HOL.equal = equal_nata} : nat HOL.equal);;

let rec less_eq_nat m n = Z.leq (integer_of_nat m) (integer_of_nat n);;

let rec less_nat m n = Z.lt (integer_of_nat m) (integer_of_nat n);;

let ord_nat =
  ({Orderings.less_eq = less_eq_nat; Orderings.less = less_nat} :
    nat Orderings.ord);;

let preorder_nat =
  ({Orderings.ord_preorder = ord_nat} : nat Orderings.preorder);;

let order_nat =
  ({Orderings.preorder_order = preorder_nat} : nat Orderings.order);;

let linorder_nat =
  ({Orderings.order_linorder = order_nat} : nat Orderings.linorder);;

let equal_integer = ({HOL.equal = Z.equal} : Z.t HOL.equal);;

let zero_integer = ({zero = Z.zero} : Z.t zero);;

let ord_integer =
  ({Orderings.less_eq = Z.leq; Orderings.less = Z.lt} : Z.t Orderings.ord);;

let preorder_integer =
  ({Orderings.ord_preorder = ord_integer} : Z.t Orderings.preorder);;

let order_integer =
  ({Orderings.preorder_order = preorder_integer} : Z.t Orderings.order);;

let linorder_integer =
  ({Orderings.order_linorder = order_integer} : Z.t Orderings.linorder);;

let rec nat k = Nat (Orderings.max ord_integer Z.zero (integer_of_int k));;

let rec plus_nat m n = Nat (Z.add (integer_of_nat m) (integer_of_nat n));;

let one_nat : nat = Nat (Z.of_int 1);;

let rec suc n = plus_nat n one_nat;;

let rec dvd (_A1, _A2)
  a b = HOL.eq _A1
          (modulo _A2.semiring_modulo_semidom_modulo.modulo_semiring_modulo b a)
          (zero _A2.algebraic_semidom_semidom_modulo.semidom_divide_algebraic_semidom.semidom_semidom_divide.comm_semiring_1_cancel_semidom.comm_semiring_1_comm_semiring_1_cancel.semiring_1_comm_semiring_1.semiring_0_semiring_1.mult_zero_semiring_0.zero_mult_zero);;

let rec minus_nat
  m n = Nat (Orderings.max ord_integer Z.zero
              (Z.sub (integer_of_nat m) (integer_of_nat n)));;

let zero_nat : nat = Nat Z.zero;;

let rec power _A
  a n = (if equal_nata n zero_nat then one _A.one_power
          else times _A.times_power a (power _A a (minus_nat n one_nat)));;

let rec abs_int i = (if less_int i zero_inta then uminus_inta i else i);;

let rec int_of_nat n = Int_of_integer (integer_of_nat n);;

let rec nat_of_integer k = Nat (Orderings.max ord_integer Z.zero k);;

let rec times_nat m n = Nat (Z.mul (integer_of_nat m) (integer_of_nat n));;

let rec of_bool _A = function true -> one _A.one_zero_neq_one
                     | false -> zero _A.zero_zero_neq_one;;

let rec modulo_nat
  m n = Nat (modulo_integer (integer_of_nat m) (integer_of_nat n));;

end;; (*struct Arith*)

module Bit_Operations : sig
  val take_bit_int : Arith.nat -> Arith.int -> Arith.int
  type 'a semiring_bit_operations
  type 'a ring_bit_operations
  val ring_bit_operations_int : Arith.int ring_bit_operations
  val bin_rsplit : Arith.nat -> Arith.nat * Arith.int -> Arith.int list
  val signed_take_bit : 'a ring_bit_operations -> Arith.nat -> 'a -> 'a
end = struct

let rec bit_int
  k n = not (Arith.dvd (Arith.equal_int, Arith.semidom_modulo_int)
              (Arith.Int_of_integer (Z.of_int 2))
              (Arith.divide_inta k
                (Arith.power Arith.power_int (Arith.Int_of_integer (Z.of_int 2))
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
          (Arith.power Arith.power_int (Arith.Int_of_integer (Z.of_int 2)) n);;

let rec and_int
  k l = (if Arith.equal_inta k Arith.zero_inta ||
              Arith.equal_inta l Arith.zero_inta
          then Arith.zero_inta
          else (if Arith.equal_inta k (Arith.uminus_inta Arith.one_inta) then l
                 else (if Arith.equal_inta l (Arith.uminus_inta Arith.one_inta)
                        then k
                        else Arith.plus_inta
                               (Arith.times_inta
                                 (Arith.modulo_inta k
                                   (Arith.Int_of_integer (Z.of_int 2)))
                                 (Arith.modulo_inta l
                                   (Arith.Int_of_integer (Z.of_int 2))))
                               (Arith.times_inta
                                 (Arith.Int_of_integer (Z.of_int 2))
                                 (and_int
                                   (Arith.divide_inta k
                                     (Arith.Int_of_integer (Z.of_int 2)))
                                   (Arith.divide_inta l
                                     (Arith.Int_of_integer (Z.of_int 2))))))));;

let rec not_int k = Arith.minus_inta (Arith.uminus_inta k) Arith.one_inta;;

let rec unset_bit_int
  n k = and_int k (not_int (push_bit_int n Arith.one_inta));;

let rec take_bit_int
  n k = Arith.modulo_inta k
          (Arith.power Arith.power_int (Arith.Int_of_integer (Z.of_int 2)) n);;

let rec xor_int
  k l = (if Arith.equal_inta k (Arith.uminus_inta Arith.one_inta) then not_int l
          else (if Arith.equal_inta l (Arith.uminus_inta Arith.one_inta)
                 then not_int k
                 else (if Arith.equal_inta k Arith.zero_inta then l
                        else (if Arith.equal_inta l Arith.zero_inta then k
                               else Arith.plus_inta
                                      (Arith.abs_int
(Arith.minus_inta (Arith.modulo_inta k (Arith.Int_of_integer (Z.of_int 2)))
  (Arith.modulo_inta l (Arith.Int_of_integer (Z.of_int 2)))))
                                      (Arith.times_inta
(Arith.Int_of_integer (Z.of_int 2))
(xor_int (Arith.divide_inta k (Arith.Int_of_integer (Z.of_int 2)))
  (Arith.divide_inta l (Arith.Int_of_integer (Z.of_int 2)))))))));;

let rec flip_bit_int n k = xor_int k (push_bit_int n Arith.one_inta);;

let rec drop_bit_int
  n k = Arith.divide_inta k
          (Arith.power Arith.power_int (Arith.Int_of_integer (Z.of_int 2)) n);;

let rec or_int
  k l = (if Arith.equal_inta k (Arith.uminus_inta Arith.one_inta) ||
              Arith.equal_inta l (Arith.uminus_inta Arith.one_inta)
          then Arith.uminus_inta Arith.one_inta
          else (if Arith.equal_inta k Arith.zero_inta then l
                 else (if Arith.equal_inta l Arith.zero_inta then k
                        else Arith.plus_inta
                               (Orderings.max Arith.ord_int
                                 (Arith.modulo_inta k
                                   (Arith.Int_of_integer (Z.of_int 2)))
                                 (Arith.modulo_inta l
                                   (Arith.Int_of_integer (Z.of_int 2))))
                               (Arith.times_inta
                                 (Arith.Int_of_integer (Z.of_int 2))
                                 (or_int
                                   (Arith.divide_inta k
                                     (Arith.Int_of_integer (Z.of_int 2)))
                                   (Arith.divide_inta l
                                     (Arith.Int_of_integer (Z.of_int 2))))))));;

let rec set_bit_int n k = or_int k (push_bit_int n Arith.one_inta);;

let rec mask_int
  n = Arith.minus_inta
        (Arith.power Arith.power_int (Arith.Int_of_integer (Z.of_int 2)) n)
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
  n w = (if Arith.equal_nata n Arith.zero_nat then (w, Arith.zero_inta)
          else (let (w1, w2) =
                  bin_split (Arith.minus_nat n Arith.one_nat)
                    (Arith.divide_inta w (Arith.Int_of_integer (Z.of_int 2)))
                  in
                 (w1, Arith.plus_inta
                        (Arith.of_bool Arith.zero_neq_one_int
                          (not (Arith.dvd
                                 (Arith.equal_int, Arith.semidom_modulo_int)
                                 (Arith.Int_of_integer (Z.of_int 2)) w)))
                        (Arith.times_inta (Arith.Int_of_integer (Z.of_int 2))
                          w2))));;

let rec bin_rsplit_aux
  n m c bs =
    (if Arith.equal_nata m Arith.zero_nat || Arith.equal_nata n Arith.zero_nat
      then bs
      else (let a = bin_split n c in
            let (aa, b) = a in
             bin_rsplit_aux n (Arith.minus_nat m n) aa (b :: bs)));;

let rec bin_rsplit
  n w = bin_rsplit_aux n (Product_Type.fst w) (Product_Type.snd w) [];;

let rec signed_take_bit _A
  n a = (let l =
           take_bit _A.semiring_bit_operations_ring_bit_operations (Arith.suc n)
             a
           in
          (if bit _A.semiring_bit_operations_ring_bit_operations.semiring_bits_semiring_bit_operations
                l n
            then Arith.plus
                   _A.ring_parity_ring_bit_operations.Arith.comm_ring_1_ring_parity.Arith.ring_1_comm_ring_1.Arith.neg_numeral_ring_1.Arith.numeral_neg_numeral.Arith.semigroup_add_numeral.Arith.plus_semigroup_add
                   l (push_bit _A.semiring_bit_operations_ring_bit_operations
                       (Arith.suc n)
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
  type num1
end = struct

type 'a bit0 = Abs_bit0 of Arith.int;;

type num1 = One_num1;;

end;; (*struct Numeral_Type*)

module Type_Length : sig
  type 'a len0 = {len_of : 'a HOL.itself -> Arith.nat}
  val len_of : 'a len0 -> 'a HOL.itself -> Arith.nat
  type 'a len = {len0_len : 'a len0}
  val len_bit0 : 'a len -> 'a Numeral_Type.bit0 len
  val len_num1 : Numeral_Type.num1 len
end = struct

type 'a len0 = {len_of : 'a HOL.itself -> Arith.nat};;
let len_of _A = _A.len_of;;

let rec len_of_bit0 _A
  uu = Arith.times_nat (Arith.nat_of_integer (Z.of_int 2))
         (len_of _A HOL.Type);;

type 'a len = {len0_len : 'a len0};;

let rec len0_bit0 _A = ({len_of = len_of_bit0 _A} : 'a Numeral_Type.bit0 len0);;

let rec len_bit0 _A =
  ({len0_len = (len0_bit0 _A.len0_len)} : 'a Numeral_Type.bit0 len);;

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
  val cast : 'b Type_Length.len -> 'a Type_Length.len -> 'b word -> 'a word
  val of_int : 'a Type_Length.len -> Arith.int -> 'a word
  val word_rcat :
    'a Type_Length.len -> 'b Type_Length.len -> 'a word list -> 'b word
  val the_signed_int : 'a Type_Length.len -> 'a word -> Arith.int
  val signed_cast :
    'b Type_Length.len -> 'a Type_Length.len -> 'b word -> 'a word
  val zero_word : 'a Type_Length.len -> 'a word
  val equal_word : 'a Type_Length.len -> 'a word -> 'a word -> bool
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

let rec word_rcat _A _B
  = Fun.comp
      (Fun.comp (of_int _B)
        (Groups_List.horner_sum Arith.comm_semiring_0_int (the_int _A)
          (Arith.power Arith.power_int (Arith.Int_of_integer (Z.of_int 2))
            (Type_Length.len_of _A.Type_Length.len0_len HOL.Type))))
      Lista.rev;;

let rec the_signed_int _A
  w = Bit_Operations.signed_take_bit Bit_Operations.ring_bit_operations_int
        (Arith.minus_nat (Type_Length.len_of _A.Type_Length.len0_len HOL.Type)
          (Arith.suc Arith.zero_nat))
        (the_int _A w);;

let rec signed_cast _B _A
  w = Word (Bit_Operations.take_bit_int
             (Type_Length.len_of _A.Type_Length.len0_len HOL.Type)
             (the_signed_int _B w));;

let rec zero_word _A = Word Arith.zero_inta;;

let rec equal_word _A v w = Arith.equal_inta (the_int _A v) (the_int _A w);;

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
  type ('b, 'a) alist
  val empty : ('a, 'b) alist
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
  val equal_option : 'a HOL.equal -> 'a option -> 'a option -> bool
end = struct

let rec is_none = function Some x -> false
                  | None -> true;;

let rec the (Some x2) = x2;;

let rec equal_option _A x0 x1 = match x0, x1 with None, Some x2 -> false
                          | Some x2, None -> false
                          | Some x2, Some y2 -> HOL.eq _A x2 y2
                          | None, None -> true;;

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
  val compare_integer : Z.t -> Z.t -> Comparator.order
end = struct

let rec compare_nat
  x = Comparator.comparator_of (Arith.equal_nat, Arith.linorder_nat) x;;

let rec compare_integer
  x = Comparator.comparator_of (Arith.equal_integer, Arith.linorder_integer) x;;

end;; (*struct Compare_Instances*)

module Stringa : sig
  val equal_literal : string HOL.equal
  val linorder_literal : string Orderings.linorder
end = struct

let equal_literal =
  ({HOL.equal = (fun a b -> ((a : string) = b))} : string HOL.equal);;

let ord_literal =
  ({Orderings.less_eq = (fun a b -> ((a : string) <= b));
     Orderings.less = (fun a b -> ((a : string) < b))}
    : string Orderings.ord);;

let preorder_literal =
  ({Orderings.ord_preorder = ord_literal} : string Orderings.preorder);;

let order_literal =
  ({Orderings.preorder_order = preorder_literal} : string Orderings.order);;

let linorder_literal =
  ({Orderings.order_linorder = order_literal} : string Orderings.linorder);;

end;; (*struct Stringa*)

module Collection_Order : sig
  type 'a ccompare = {ccompare : ('a -> 'a -> Comparator.order) option}
  val ccompare : 'a ccompare -> ('a -> 'a -> Comparator.order) option
  val ccompare_nat : Arith.nat ccompare
  val ccompare_literal : string ccompare
  val ccompare_integer : Z.t ccompare
end = struct

let ccompare_nata : (Arith.nat -> Arith.nat -> Comparator.order) option
  = Some Compare_Instances.compare_nat;;

type 'a ccompare = {ccompare : ('a -> 'a -> Comparator.order) option};;
let ccompare _A = _A.ccompare;;

let ccompare_nat = ({ccompare = ccompare_nata} : Arith.nat ccompare);;

let rec compare_literal
  x = Comparator.comparator_of (Stringa.equal_literal, Stringa.linorder_literal)
        x;;

let ccompare_literala : (string -> string -> Comparator.order) option
  = Some compare_literal;;

let ccompare_literal = ({ccompare = ccompare_literala} : string ccompare);;

let ccompare_integera : (Z.t -> Z.t -> Comparator.order) option
  = Some Compare_Instances.compare_integer;;

let ccompare_integer = ({ccompare = ccompare_integera} : Z.t ccompare);;

end;; (*struct Collection_Order*)

module RBT_Impl : sig
  type color = R | B
  type ('a, 'b) rbt = Empty |
    Branch of color * ('a, 'b) rbt * 'a * 'b * ('a, 'b) rbt
  val paint : color -> ('a, 'b) rbt -> ('a, 'b) rbt
  val balance : ('a, 'b) rbt -> 'a -> 'b -> ('a, 'b) rbt -> ('a, 'b) rbt
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

end;; (*struct RBT_Impl*)

module RBT_Comparator_Impl : sig
  val rbt_comp_insert :
    ('a -> 'a -> Comparator.order) ->
      'a -> 'b -> ('a, 'b) RBT_Impl.rbt -> ('a, 'b) RBT_Impl.rbt
  val rbt_comp_lookup :
    ('a -> 'a -> Comparator.order) -> ('a, 'b) RBT_Impl.rbt -> 'a -> 'b option
end = struct

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
  val insert :
    'a Collection_Order.ccompare ->
      'a -> 'b -> ('a, 'b) mapping_rbt -> ('a, 'b) mapping_rbt
  val lookup :
    'a Collection_Order.ccompare -> ('a, 'b) mapping_rbt -> 'a -> 'b option
end = struct

type ('b, 'a) mapping_rbt = Mapping_RBT of ('b, 'a) RBT_Impl.rbt;;

let rec empty _A = Mapping_RBT RBT_Impl.Empty;;

let rec impl_of _B (Mapping_RBT x) = x;;

let rec insert _A
  xc xd xe =
    Mapping_RBT
      (RBT_Comparator_Impl.rbt_comp_insert
        (Option.the (Collection_Order.ccompare _A)) xc xd (impl_of _A xe));;

let rec lookup _A
  xa = RBT_Comparator_Impl.rbt_comp_lookup
         (Option.the (Collection_Order.ccompare _A)) (impl_of _A xa);;

end;; (*struct RBT_Mapping2*)

module Mapping : sig
  type ('a, 'b) mapping = Assoc_List_Mapping of ('a, 'b) DAList.alist |
    RBT_Mapping of ('a, 'b) RBT_Mapping2.mapping_rbt |
    Mapping of ('a -> 'b option)
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
  val mapping_impl_integer : (Z.t, mapping_impl) Phantom_Type.phantom
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

let mapping_impl_integer : (Z.t, mapping_impl) Phantom_Type.phantom
  = Phantom_Type.Phantom Mapping_RBT;;

end;; (*struct Mapping_Impl*)

module Signed_Words : sig
  type 'a signed
  val len_signed : 'a Type_Length.len -> 'a signed Type_Length.len
end = struct

type 'a signed = EMPTY__;;

let rec len_of_signed _A x = Type_Length.len_of _A HOL.Type;;

let rec len0_signed _A =
  ({Type_Length.len_of = len_of_signed _A} : 'a signed Type_Length.len0);;

let rec len_signed _A =
  ({Type_Length.len0_len = (len0_signed _A.Type_Length.len0_len)} :
    'a signed Type_Length.len);;

end;; (*struct Signed_Words*)

module More_Word_Library : sig
  type ('a, 'b) mem_capability_ext =
    Mem_capability_ext of
      'a * Arith.int * Arith.nat * Arith.nat * bool * bool * bool * bool *
        bool * bool * 'b
  type 'a capability_ext = Capability_ext of bool * 'a
  type 'a ccval =
    Uint8_v of
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Word.word
    | Sint8_v of
        Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
          Signed_Words.signed
          Word.word
    | Uint16_v of
        Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
          Numeral_Type.bit0
          Word.word
    | Sint16_v of
        Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
          Numeral_Type.bit0
          Signed_Words.signed
          Word.word
    | Uint32_v of
        Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
          Numeral_Type.bit0
          Numeral_Type.bit0
          Word.word
    | Sint32_v of
        Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
          Numeral_Type.bit0
          Numeral_Type.bit0
          Signed_Words.signed
          Word.word
    | Uint64_v of
        Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
          Numeral_Type.bit0
          Numeral_Type.bit0
          Numeral_Type.bit0
          Word.word
    | Sint64_v of
        Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
          Numeral_Type.bit0
          Numeral_Type.bit0
          Numeral_Type.bit0
          Signed_Words.signed
          Word.word
    | Cap_v of ('a, unit capability_ext) mem_capability_ext |
    Cap_v_frag of ('a, unit capability_ext) mem_capability_ext * Arith.nat |
    Undef
  type 'a comp_countable =
    {countable_comp_countable : 'a Countable.countable;
      zero_comp_countable : 'a Arith.zero;
      ord_comp_countable : 'a Orderings.ord}
  val equal_mem_capability_exta :
    'a HOL.equal * 'a comp_countable -> 'b HOL.equal ->
      ('a, 'b) mem_capability_ext -> ('a, 'b) mem_capability_ext -> bool
  val equal_capability_ext : 'a HOL.equal -> 'a capability_ext HOL.equal
  val equal_ccvala :
    'a HOL.equal * 'a comp_countable -> 'a ccval -> 'a ccval -> bool
  val equal_ccval : 'a HOL.equal * 'a comp_countable -> 'a ccval HOL.equal
  type cctype = Uint8 | Sint8 | Uint16 | Sint16 | Uint32 | Sint32 | Uint64 |
    Sint64 | Cap
  val sword64_of_integer :
    Z.t ->
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Signed_Words.signed
        Word.word
  val sword32_of_integer :
    Z.t ->
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Signed_Words.signed
        Word.word
  val sword16_of_integer :
    Z.t ->
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Numeral_Type.bit0
        Signed_Words.signed
        Word.word
  val integer_of_sword64 :
    Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
      Numeral_Type.bit0
      Numeral_Type.bit0
      Numeral_Type.bit0
      Signed_Words.signed
      Word.word ->
      Z.t
  val integer_of_sword32 :
    Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
      Numeral_Type.bit0
      Numeral_Type.bit0
      Signed_Words.signed
      Word.word ->
      Z.t
  val integer_of_sword16 :
    Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
      Numeral_Type.bit0
      Signed_Words.signed
      Word.word ->
      Z.t
  val word64_of_integer :
    Z.t ->
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Word.word
  val word32_of_integer :
    Z.t ->
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Word.word
  val word16_of_integer :
    Z.t ->
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Numeral_Type.bit0
        Word.word
  val sword8_of_integer :
    Z.t ->
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Signed_Words.signed
        Word.word
  val integer_of_word64 :
    Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
      Numeral_Type.bit0
      Numeral_Type.bit0
      Numeral_Type.bit0
      Word.word ->
      Z.t
  val integer_of_word32 :
    Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
      Numeral_Type.bit0
      Numeral_Type.bit0
      Word.word ->
      Z.t
  val integer_of_word16 :
    Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
      Numeral_Type.bit0
      Word.word ->
      Z.t
  val integer_of_sword8 :
    Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
      Signed_Words.signed
      Word.word ->
      Z.t
  val word8_of_integer :
    Z.t ->
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Word.word
  val integer_of_word8 :
    Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
      Word.word ->
      Z.t
  val cast_val : string -> Z.t -> Z.t
  val memval_type : 'a ccval -> cctype
  val tag :
    'a comp_countable -> ('a, 'b capability_ext) mem_capability_ext -> bool
  val len : 'a comp_countable -> ('a, 'b) mem_capability_ext -> Arith.nat
  val base : 'a comp_countable -> ('a, 'b) mem_capability_ext -> Arith.nat
  val tag_update :
    'a comp_countable ->
      (bool -> bool) ->
        ('a, 'b capability_ext) mem_capability_ext ->
          ('a, 'b capability_ext) mem_capability_ext
  val perm_cap_store_local :
    'a comp_countable -> ('a, 'b) mem_capability_ext -> bool
  val perm_cap_store : 'a comp_countable -> ('a, 'b) mem_capability_ext -> bool
  val perm_cap_load : 'a comp_countable -> ('a, 'b) mem_capability_ext -> bool
  val perm_global : 'a comp_countable -> ('a, 'b) mem_capability_ext -> bool
  val perm_store : 'a comp_countable -> ('a, 'b) mem_capability_ext -> bool
  val perm_load : 'a comp_countable -> ('a, 'b) mem_capability_ext -> bool
  val block_id : 'a comp_countable -> ('a, 'b) mem_capability_ext -> 'a
  val offset : 'a comp_countable -> ('a, 'b) mem_capability_ext -> Arith.int
  val extend :
    'a comp_countable ->
      ('a, unit) mem_capability_ext -> 'b -> ('a, 'b) mem_capability_ext
  val truncate :
    'a comp_countable ->
      ('a, 'b) mem_capability_ext -> ('a, unit) mem_capability_ext
  val offset_update :
    'a comp_countable ->
      (Arith.int -> Arith.int) ->
        ('a, 'b) mem_capability_ext -> ('a, 'b) mem_capability_ext
  val perm_global_update :
    'a comp_countable ->
      (bool -> bool) ->
        ('a, 'b) mem_capability_ext -> ('a, 'b) mem_capability_ext
end = struct

type ('a, 'b) mem_capability_ext =
  Mem_capability_ext of
    'a * Arith.int * Arith.nat * Arith.nat * bool * bool * bool * bool * bool *
      bool * 'b;;

type 'a capability_ext = Capability_ext of bool * 'a;;

type 'a ccval =
  Uint8_v of
    Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
      Word.word
  | Sint8_v of
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Signed_Words.signed
        Word.word
  | Uint16_v of
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Numeral_Type.bit0
        Word.word
  | Sint16_v of
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Numeral_Type.bit0
        Signed_Words.signed
        Word.word
  | Uint32_v of
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Word.word
  | Sint32_v of
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Signed_Words.signed
        Word.word
  | Uint64_v of
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Word.word
  | Sint64_v of
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Numeral_Type.bit0
        Signed_Words.signed
        Word.word
  | Cap_v of ('a, unit capability_ext) mem_capability_ext |
  Cap_v_frag of ('a, unit capability_ext) mem_capability_ext * Arith.nat |
  Undef;;

type 'a comp_countable =
  {countable_comp_countable : 'a Countable.countable;
    zero_comp_countable : 'a Arith.zero;
    ord_comp_countable : 'a Orderings.ord};;

let rec equal_mem_capability_exta (_A1, _A2) _B
  (Mem_capability_ext
    (block_ida, offseta, basea, lena, perm_loada, perm_cap_loada, perm_storea,
      perm_cap_storea, perm_cap_store_locala, perm_globala, morea))
    (Mem_capability_ext
      (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
        perm_cap_store, perm_cap_store_local, perm_global, more))
    = HOL.eq _A1 block_ida block_id &&
        (Arith.equal_inta offseta offset &&
          (Arith.equal_nata basea base &&
            (Arith.equal_nata lena len &&
              (Product_Type.equal_bool perm_loada perm_load &&
                (Product_Type.equal_bool perm_cap_loada perm_cap_load &&
                  (Product_Type.equal_bool perm_storea perm_store &&
                    (Product_Type.equal_bool perm_cap_storea perm_cap_store &&
                      (Product_Type.equal_bool perm_cap_store_locala
                         perm_cap_store_local &&
                        (Product_Type.equal_bool perm_globala perm_global &&
                          HOL.eq _B morea more)))))))));;

let rec equal_mem_capability_ext (_A1, _A2) _B =
  ({HOL.equal = equal_mem_capability_exta (_A1, _A2) _B} :
    ('a, 'b) mem_capability_ext HOL.equal);;

let rec equal_capability_exta _A
  (Capability_ext (taga, morea)) (Capability_ext (tag, more)) =
    Product_Type.equal_bool taga tag && HOL.eq _A morea more;;

let rec equal_capability_ext _A =
  ({HOL.equal = equal_capability_exta _A} : 'a capability_ext HOL.equal);;

let rec equal_ccvala (_A1, _A2)
  x0 x1 = match x0, x1 with Cap_v_frag (x101, x102), Undef -> false
    | Undef, Cap_v_frag (x101, x102) -> false
    | Cap_v x9, Undef -> false
    | Undef, Cap_v x9 -> false
    | Cap_v x9, Cap_v_frag (x101, x102) -> false
    | Cap_v_frag (x101, x102), Cap_v x9 -> false
    | Sint64_v x8, Undef -> false
    | Undef, Sint64_v x8 -> false
    | Sint64_v x8, Cap_v_frag (x101, x102) -> false
    | Cap_v_frag (x101, x102), Sint64_v x8 -> false
    | Sint64_v x8, Cap_v x9 -> false
    | Cap_v x9, Sint64_v x8 -> false
    | Uint64_v x7, Undef -> false
    | Undef, Uint64_v x7 -> false
    | Uint64_v x7, Cap_v_frag (x101, x102) -> false
    | Cap_v_frag (x101, x102), Uint64_v x7 -> false
    | Uint64_v x7, Cap_v x9 -> false
    | Cap_v x9, Uint64_v x7 -> false
    | Uint64_v x7, Sint64_v x8 -> false
    | Sint64_v x8, Uint64_v x7 -> false
    | Sint32_v x6, Undef -> false
    | Undef, Sint32_v x6 -> false
    | Sint32_v x6, Cap_v_frag (x101, x102) -> false
    | Cap_v_frag (x101, x102), Sint32_v x6 -> false
    | Sint32_v x6, Cap_v x9 -> false
    | Cap_v x9, Sint32_v x6 -> false
    | Sint32_v x6, Sint64_v x8 -> false
    | Sint64_v x8, Sint32_v x6 -> false
    | Sint32_v x6, Uint64_v x7 -> false
    | Uint64_v x7, Sint32_v x6 -> false
    | Uint32_v x5, Undef -> false
    | Undef, Uint32_v x5 -> false
    | Uint32_v x5, Cap_v_frag (x101, x102) -> false
    | Cap_v_frag (x101, x102), Uint32_v x5 -> false
    | Uint32_v x5, Cap_v x9 -> false
    | Cap_v x9, Uint32_v x5 -> false
    | Uint32_v x5, Sint64_v x8 -> false
    | Sint64_v x8, Uint32_v x5 -> false
    | Uint32_v x5, Uint64_v x7 -> false
    | Uint64_v x7, Uint32_v x5 -> false
    | Uint32_v x5, Sint32_v x6 -> false
    | Sint32_v x6, Uint32_v x5 -> false
    | Sint16_v x4, Undef -> false
    | Undef, Sint16_v x4 -> false
    | Sint16_v x4, Cap_v_frag (x101, x102) -> false
    | Cap_v_frag (x101, x102), Sint16_v x4 -> false
    | Sint16_v x4, Cap_v x9 -> false
    | Cap_v x9, Sint16_v x4 -> false
    | Sint16_v x4, Sint64_v x8 -> false
    | Sint64_v x8, Sint16_v x4 -> false
    | Sint16_v x4, Uint64_v x7 -> false
    | Uint64_v x7, Sint16_v x4 -> false
    | Sint16_v x4, Sint32_v x6 -> false
    | Sint32_v x6, Sint16_v x4 -> false
    | Sint16_v x4, Uint32_v x5 -> false
    | Uint32_v x5, Sint16_v x4 -> false
    | Uint16_v x3, Undef -> false
    | Undef, Uint16_v x3 -> false
    | Uint16_v x3, Cap_v_frag (x101, x102) -> false
    | Cap_v_frag (x101, x102), Uint16_v x3 -> false
    | Uint16_v x3, Cap_v x9 -> false
    | Cap_v x9, Uint16_v x3 -> false
    | Uint16_v x3, Sint64_v x8 -> false
    | Sint64_v x8, Uint16_v x3 -> false
    | Uint16_v x3, Uint64_v x7 -> false
    | Uint64_v x7, Uint16_v x3 -> false
    | Uint16_v x3, Sint32_v x6 -> false
    | Sint32_v x6, Uint16_v x3 -> false
    | Uint16_v x3, Uint32_v x5 -> false
    | Uint32_v x5, Uint16_v x3 -> false
    | Uint16_v x3, Sint16_v x4 -> false
    | Sint16_v x4, Uint16_v x3 -> false
    | Sint8_v x2, Undef -> false
    | Undef, Sint8_v x2 -> false
    | Sint8_v x2, Cap_v_frag (x101, x102) -> false
    | Cap_v_frag (x101, x102), Sint8_v x2 -> false
    | Sint8_v x2, Cap_v x9 -> false
    | Cap_v x9, Sint8_v x2 -> false
    | Sint8_v x2, Sint64_v x8 -> false
    | Sint64_v x8, Sint8_v x2 -> false
    | Sint8_v x2, Uint64_v x7 -> false
    | Uint64_v x7, Sint8_v x2 -> false
    | Sint8_v x2, Sint32_v x6 -> false
    | Sint32_v x6, Sint8_v x2 -> false
    | Sint8_v x2, Uint32_v x5 -> false
    | Uint32_v x5, Sint8_v x2 -> false
    | Sint8_v x2, Sint16_v x4 -> false
    | Sint16_v x4, Sint8_v x2 -> false
    | Sint8_v x2, Uint16_v x3 -> false
    | Uint16_v x3, Sint8_v x2 -> false
    | Uint8_v x1, Undef -> false
    | Undef, Uint8_v x1 -> false
    | Uint8_v x1, Cap_v_frag (x101, x102) -> false
    | Cap_v_frag (x101, x102), Uint8_v x1 -> false
    | Uint8_v x1, Cap_v x9 -> false
    | Cap_v x9, Uint8_v x1 -> false
    | Uint8_v x1, Sint64_v x8 -> false
    | Sint64_v x8, Uint8_v x1 -> false
    | Uint8_v x1, Uint64_v x7 -> false
    | Uint64_v x7, Uint8_v x1 -> false
    | Uint8_v x1, Sint32_v x6 -> false
    | Sint32_v x6, Uint8_v x1 -> false
    | Uint8_v x1, Uint32_v x5 -> false
    | Uint32_v x5, Uint8_v x1 -> false
    | Uint8_v x1, Sint16_v x4 -> false
    | Sint16_v x4, Uint8_v x1 -> false
    | Uint8_v x1, Uint16_v x3 -> false
    | Uint16_v x3, Uint8_v x1 -> false
    | Uint8_v x1, Sint8_v x2 -> false
    | Sint8_v x2, Uint8_v x1 -> false
    | Cap_v_frag (x101, x102), Cap_v_frag (y101, y102) ->
        HOL.eq
          (equal_mem_capability_ext (_A1, _A2)
            (equal_capability_ext Product_Type.equal_unit))
          x101 y101 &&
          Arith.equal_nata x102 y102
    | Cap_v x9, Cap_v y9 ->
        HOL.eq
          (equal_mem_capability_ext (_A1, _A2)
            (equal_capability_ext Product_Type.equal_unit))
          x9 y9
    | Sint64_v x8, Sint64_v y8 ->
        Word.equal_word
          (Signed_Words.len_signed
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1)))))))
          x8 y8
    | Uint64_v x7, Uint64_v y7 ->
        Word.equal_word
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0 Type_Length.len_num1))))))
          x7 y7
    | Sint32_v x6, Sint32_v y6 ->
        Word.equal_word
          (Signed_Words.len_signed
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0 Type_Length.len_num1))))))
          x6 y6
    | Uint32_v x5, Uint32_v y5 ->
        Word.equal_word
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1)))))
          x5 y5
    | Sint16_v x4, Sint16_v y4 ->
        Word.equal_word
          (Signed_Words.len_signed
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1)))))
          x4 y4
    | Uint16_v x3, Uint16_v y3 ->
        Word.equal_word
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0 Type_Length.len_num1))))
          x3 y3
    | Sint8_v x2, Sint8_v y2 ->
        Word.equal_word
          (Signed_Words.len_signed
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0 Type_Length.len_num1))))
          x2 y2
    | Uint8_v x1, Uint8_v y1 ->
        Word.equal_word
          (Type_Length.len_bit0
            (Type_Length.len_bit0 (Type_Length.len_bit0 Type_Length.len_num1)))
          x1 y1
    | Undef, Undef -> true;;

let rec equal_ccval (_A1, _A2) =
  ({HOL.equal = equal_ccvala (_A1, _A2)} : 'a ccval HOL.equal);;

type cctype = Uint8 | Sint8 | Uint16 | Sint16 | Uint32 | Sint32 | Uint64 |
  Sint64 | Cap;;

let rec sword_of_integer _A
  x = Word.of_int (Signed_Words.len_signed _A) (Arith.Int_of_integer x);;

let rec sword64_of_integer
  x = sword_of_integer
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1))))))
        x;;

let rec sword32_of_integer
  x = sword_of_integer
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0 Type_Length.len_num1)))))
        x;;

let rec sword16_of_integer
  x = sword_of_integer
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0 (Type_Length.len_bit0 Type_Length.len_num1))))
        x;;

let rec integer_of_sword _A
  x = Arith.integer_of_int
        (Word.the_signed_int (Signed_Words.len_signed _A) x);;

let rec integer_of_sword64
  x = integer_of_sword
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1))))))
        x;;

let rec integer_of_sword32
  x = integer_of_sword
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0 Type_Length.len_num1)))))
        x;;

let rec integer_of_sword16
  x = integer_of_sword
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0 (Type_Length.len_bit0 Type_Length.len_num1))))
        x;;

let rec word_of_integer _A x = Word.of_int _A (Arith.Int_of_integer x);;

let rec word64_of_integer
  x = word_of_integer
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1))))))
        x;;

let rec word32_of_integer
  x = word_of_integer
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0 Type_Length.len_num1)))))
        x;;

let rec word16_of_integer
  x = word_of_integer
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0 (Type_Length.len_bit0 Type_Length.len_num1))))
        x;;

let rec sword8_of_integer
  x = sword_of_integer
        (Type_Length.len_bit0
          (Type_Length.len_bit0 (Type_Length.len_bit0 Type_Length.len_num1)))
        x;;

let rec integer_of_word _A x = Arith.integer_of_int (Word.the_int _A x);;

let rec integer_of_word64
  x = integer_of_word
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1))))))
        x;;

let rec integer_of_word32
  x = integer_of_word
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0
              (Type_Length.len_bit0
                (Type_Length.len_bit0 Type_Length.len_num1)))))
        x;;

let rec integer_of_word16
  x = integer_of_word
        (Type_Length.len_bit0
          (Type_Length.len_bit0
            (Type_Length.len_bit0 (Type_Length.len_bit0 Type_Length.len_num1))))
        x;;

let rec integer_of_sword8
  x = integer_of_sword
        (Type_Length.len_bit0
          (Type_Length.len_bit0 (Type_Length.len_bit0 Type_Length.len_num1)))
        x;;

let rec word8_of_integer
  x = word_of_integer
        (Type_Length.len_bit0
          (Type_Length.len_bit0 (Type_Length.len_bit0 Type_Length.len_num1)))
        x;;

let rec integer_of_word8
  x = integer_of_word
        (Type_Length.len_bit0
          (Type_Length.len_bit0 (Type_Length.len_bit0 Type_Length.len_num1)))
        x;;

let rec cast_val
  s i = (if ((s : string) = "uint8") then integer_of_word8 (word8_of_integer i)
          else (if ((s : string) = "int8")
                 then integer_of_sword8 (sword8_of_integer i)
                 else (if ((s : string) = "uint16")
                        then integer_of_word16 (word16_of_integer i)
                        else (if ((s : string) = "int16")
                               then integer_of_sword16 (sword16_of_integer i)
                               else (if ((s : string) = "uint32")
                                      then integer_of_word32
     (word32_of_integer i)
                                      else (if ((s : string) = "int32")
     then integer_of_sword32 (sword32_of_integer i)
     else (if ((s : string) = "uint64")
            then integer_of_word64 (word64_of_integer i)
            else (if ((s : string) = "int64")
                   then integer_of_sword64 (sword64_of_integer i)
                   else i))))))));;

let rec memval_type
  v = (match v with Uint8_v _ -> Uint8 | Sint8_v _ -> Sint8
        | Uint16_v _ -> Uint16 | Sint16_v _ -> Sint16 | Uint32_v _ -> Uint32
        | Sint32_v _ -> Sint32 | Uint64_v _ -> Uint64 | Sint64_v _ -> Sint64
        | Cap_v _ -> Cap | Cap_v_frag (_, _) -> Uint8);;

let rec tag _A
  (Mem_capability_ext
    (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
      perm_cap_store, perm_cap_store_local, perm_global,
      Capability_ext (tag, more)))
    = tag;;

let rec len _A
  (Mem_capability_ext
    (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
      perm_cap_store, perm_cap_store_local, perm_global, more))
    = len;;

let rec base _A
  (Mem_capability_ext
    (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
      perm_cap_store, perm_cap_store_local, perm_global, more))
    = base;;

let rec tag_update _A
  taga (Mem_capability_ext
         (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
           perm_cap_store, perm_cap_store_local, perm_global,
           Capability_ext (tag, more)))
    = Mem_capability_ext
        (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
          perm_cap_store, perm_cap_store_local, perm_global,
          Capability_ext (taga tag, more));;

let rec perm_cap_store_local _A
  (Mem_capability_ext
    (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
      perm_cap_store, perm_cap_store_local, perm_global, more))
    = perm_cap_store_local;;

let rec perm_cap_store _A
  (Mem_capability_ext
    (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
      perm_cap_store, perm_cap_store_local, perm_global, more))
    = perm_cap_store;;

let rec perm_cap_load _A
  (Mem_capability_ext
    (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
      perm_cap_store, perm_cap_store_local, perm_global, more))
    = perm_cap_load;;

let rec perm_global _A
  (Mem_capability_ext
    (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
      perm_cap_store, perm_cap_store_local, perm_global, more))
    = perm_global;;

let rec perm_store _A
  (Mem_capability_ext
    (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
      perm_cap_store, perm_cap_store_local, perm_global, more))
    = perm_store;;

let rec perm_load _A
  (Mem_capability_ext
    (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
      perm_cap_store, perm_cap_store_local, perm_global, more))
    = perm_load;;

let rec block_id _A
  (Mem_capability_ext
    (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
      perm_cap_store, perm_cap_store_local, perm_global, more))
    = block_id;;

let rec offset _A
  (Mem_capability_ext
    (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
      perm_cap_store, perm_cap_store_local, perm_global, more))
    = offset;;

let rec extend _A
  r more =
    Mem_capability_ext
      (block_id _A r, offset _A r, base _A r, len _A r, perm_load _A r,
        perm_cap_load _A r, perm_store _A r, perm_cap_store _A r,
        perm_cap_store_local _A r, perm_global _A r, more);;

let rec truncate _A
  r = Mem_capability_ext
        (block_id _A r, offset _A r, base _A r, len _A r, perm_load _A r,
          perm_cap_load _A r, perm_store _A r, perm_cap_store _A r,
          perm_cap_store_local _A r, perm_global _A r, ());;

let rec offset_update _A
  offseta
    (Mem_capability_ext
      (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
        perm_cap_store, perm_cap_store_local, perm_global, more))
    = Mem_capability_ext
        (block_id, offseta offset, base, len, perm_load, perm_cap_load,
          perm_store, perm_cap_store, perm_cap_store_local, perm_global, more);;

let rec perm_global_update _A
  perm_globala
    (Mem_capability_ext
      (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
        perm_cap_store, perm_cap_store_local, perm_global, more))
    = Mem_capability_ext
        (block_id, offset, base, len, perm_load, perm_cap_load, perm_store,
          perm_cap_store, perm_cap_store_local, perm_globala perm_global,
          more);;

end;; (*struct More_Word_Library*)

module Separation_Algebra : sig
  type 'a pre_sep_algebra =
    {plus_pre_sep_algebra : 'a Arith.plus; zero_pre_sep_algebra : 'a Arith.zero;
      sep_disj : 'a -> 'a -> bool}
  val sep_disj : 'a pre_sep_algebra -> 'a -> 'a -> bool
  type 'a sep_algebra = {pre_sep_algebra_sep_algebra : 'a pre_sep_algebra}
  type 'a cancellative_sep_algebra =
    {sep_algebra_cancellative_sep_algebra : 'a sep_algebra}
end = struct

type 'a pre_sep_algebra =
  {plus_pre_sep_algebra : 'a Arith.plus; zero_pre_sep_algebra : 'a Arith.zero;
    sep_disj : 'a -> 'a -> bool};;
let sep_disj _A = _A.sep_disj;;

type 'a sep_algebra = {pre_sep_algebra_sep_algebra : 'a pre_sep_algebra};;

type 'a cancellative_sep_algebra =
  {sep_algebra_cancellative_sep_algebra : 'a sep_algebra};;

end;; (*struct Separation_Algebra*)

module CHERI_C_Concrete_Memory_Model : sig
  val comp_countable_integer : Z.t More_Word_Library.comp_countable
  type memval
  type 'a object_ext
  type t
  type logicerrtype = UseAfterFree | BufferOverrun | MissingResource |
    WrongMemVal | MemoryNotFreed | Unhandled of string
  type c2errtype = TagViolation | PermitLoadViolation | PermitStoreViolation |
    PermitStoreCapViolation | PermitStoreLocalCapViolation | LengthViolation |
    BadAddressViolation
  type errtype = C2Err of c2errtype | LogicErr of logicerrtype
  type 'a result = Success of 'a | Error of errtype
  type 'a heap_ext
  val null_capability :
    (Z.t, unit More_Word_Library.capability_ext)
      More_Word_Library.mem_capability_ext
  val free :
    unit heap_ext ->
      (Z.t, unit More_Word_Library.capability_ext)
        More_Word_Library.mem_capability_ext ->
        (unit heap_ext *
          (Z.t, unit More_Word_Library.capability_ext)
            More_Word_Library.mem_capability_ext)
          result
  val load :
    unit heap_ext ->
      (Z.t, unit More_Word_Library.capability_ext)
        More_Word_Library.mem_capability_ext ->
        More_Word_Library.cctype -> Z.t More_Word_Library.ccval result
  val next_block : 'a heap_ext -> Z.t
  val alloc :
    unit heap_ext ->
      bool ->
        Arith.nat ->
          (unit heap_ext *
            (Z.t, unit More_Word_Library.capability_ext)
              More_Word_Library.mem_capability_ext)
            result
  val store :
    unit heap_ext ->
      (Z.t, unit More_Word_Library.capability_ext)
        More_Word_Library.mem_capability_ext ->
        Z.t More_Word_Library.ccval -> unit heap_ext result
  val res : 'a result -> 'a
  val memset :
    unit heap_ext ->
      (Z.t, unit More_Word_Library.capability_ext)
        More_Word_Library.mem_capability_ext ->
        Z.t More_Word_Library.ccval -> Arith.nat -> unit heap_ext result
  val calloc :
    unit heap_ext ->
      Arith.nat ->
        (unit heap_ext *
          (Z.t, unit More_Word_Library.capability_ext)
            More_Word_Library.mem_capability_ext)
          result
  val malloc :
    unit heap_ext ->
      Arith.nat ->
        (unit heap_ext *
          (Z.t, unit More_Word_Library.capability_ext)
            More_Word_Library.mem_capability_ext)
          result
  val memcmp :
    unit heap_ext ->
      (Z.t, unit More_Word_Library.capability_ext)
        More_Word_Library.mem_capability_ext ->
        (Z.t, unit More_Word_Library.capability_ext)
          More_Word_Library.mem_capability_ext ->
          Arith.nat -> bool result
  val memcpy :
    unit heap_ext ->
      (Z.t, unit More_Word_Library.capability_ext)
        More_Word_Library.mem_capability_ext ->
        (Z.t, unit More_Word_Library.capability_ext)
          More_Word_Library.mem_capability_ext ->
          Arith.nat -> unit heap_ext result
  val memmove :
    unit heap_ext ->
      (Z.t, unit More_Word_Library.capability_ext)
        More_Word_Library.mem_capability_ext ->
        (Z.t, unit More_Word_Library.capability_ext)
          More_Word_Library.mem_capability_ext ->
          Arith.nat -> unit heap_ext result
  val realloc :
    unit heap_ext ->
      (Z.t, unit More_Word_Library.capability_ext)
        More_Word_Library.mem_capability_ext ->
        Arith.nat ->
          (unit heap_ext *
            (Z.t, unit More_Word_Library.capability_ext)
              More_Word_Library.mem_capability_ext)
            result
  val init_heap : unit heap_ext
  val get_unfreed_blocks : unit heap_ext -> Arith.nat -> Z.t list
  val get_memory_leak_size : unit heap_ext -> Arith.nat -> Arith.nat
end = struct

let rec plus_unita u1 u2 = ();;

let plus_unit = ({Arith.plus = plus_unita} : unit Arith.plus);;

let zero_unita : unit = ();;

let zero_unit = ({Arith.zero = zero_unita} : unit Arith.zero);;

let rec sep_disj_unit u1 u2 = true;;

let pre_sep_algebra_unit =
  ({Separation_Algebra.plus_pre_sep_algebra = plus_unit;
     Separation_Algebra.zero_pre_sep_algebra = zero_unit;
     Separation_Algebra.sep_disj = sep_disj_unit}
    : unit Separation_Algebra.pre_sep_algebra);;

let sep_algebra_unit =
  ({Separation_Algebra.pre_sep_algebra_sep_algebra = pre_sep_algebra_unit} :
    unit Separation_Algebra.sep_algebra);;

let cancellative_sep_algebra_unit =
  ({Separation_Algebra.sep_algebra_cancellative_sep_algebra = sep_algebra_unit}
    : unit Separation_Algebra.cancellative_sep_algebra);;

let countable_integer = (() : Z.t Countable.countable);;

let comp_countable_integer =
  ({More_Word_Library.countable_comp_countable = countable_integer;
     More_Word_Library.zero_comp_countable = Arith.zero_integer;
     More_Word_Library.ord_comp_countable = Arith.ord_integer}
    : Z.t More_Word_Library.comp_countable);;

type memval =
  Byte of
    Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
      Word.word
  | ACap of (Z.t, unit) More_Word_Library.mem_capability_ext * Arith.nat;;

let rec equal_memvala
  x0 x1 = match x0, x1 with Byte x1, ACap (x21, x22) -> false
    | ACap (x21, x22), Byte x1 -> false
    | ACap (x21, x22), ACap (y21, y22) ->
        More_Word_Library.equal_mem_capability_exta
          (Arith.equal_integer, comp_countable_integer) Product_Type.equal_unit
          x21 y21 &&
          Arith.equal_nata x22 y22
    | Byte x1, Byte y1 ->
        Word.equal_word
          (Type_Length.len_bit0
            (Type_Length.len_bit0 (Type_Length.len_bit0 Type_Length.len_num1)))
          x1 y1;;

let equal_memval = ({HOL.equal = equal_memvala} : memval HOL.equal);;

let rec zero_capability_exta _A
  = More_Word_Library.Capability_ext (false, Arith.zero _A);;

let rec zero_capability_ext _A =
  ({Arith.zero = zero_capability_exta _A} :
    'a More_Word_Library.capability_ext Arith.zero);;

type 'a object_ext =
  Object_ext of
    (Arith.nat * Arith.nat) * (Arith.nat, memval) Mapping.mapping *
      (Arith.nat, bool) Mapping.mapping * 'a;;

type t = Freed | Map of unit object_ext;;

type logicerrtype = UseAfterFree | BufferOverrun | MissingResource | WrongMemVal
  | MemoryNotFreed | Unhandled of string;;

type c2errtype = TagViolation | PermitLoadViolation | PermitStoreViolation |
  PermitStoreCapViolation | PermitStoreLocalCapViolation | LengthViolation |
  BadAddressViolation;;

type errtype = C2Err of c2errtype | LogicErr of logicerrtype;;

type 'a result = Success of 'a | Error of errtype;;

type 'a heap_ext = Heap_ext of Z.t * (Z.t, t) Mapping.mapping * 'a;;

let rec heap_map_update
  heap_mapa (Heap_ext (next_block, heap_map, more)) =
    Heap_ext (next_block, heap_mapa heap_map, more);;

let rec zero_mem_capability_ext _A _B
  = More_Word_Library.Mem_capability_ext
      (Arith.zero _A.More_Word_Library.zero_comp_countable, Arith.zero_inta,
        Arith.zero_nat, Arith.zero_nat, false, false, false, false, false,
        false, Arith.zero _B);;

let null_capability :
  (Z.t, unit More_Word_Library.capability_ext)
    More_Word_Library.mem_capability_ext
  = zero_mem_capability_ext comp_countable_integer
      (zero_capability_ext zero_unit);;

let rec bounds (Object_ext (bounds, content, tags, more)) = bounds;;

let rec heap_map (Heap_ext (next_block, heap_map, more)) = heap_map;;

let rec free
  h c = (if More_Word_Library.equal_mem_capability_exta
              (Arith.equal_integer, comp_countable_integer)
              (More_Word_Library.equal_capability_ext Product_Type.equal_unit) c
              null_capability
          then Success (h, c)
          else (if Product_Type.equal_bool
                     (More_Word_Library.tag comp_countable_integer c) false
                 then Error (C2Err TagViolation)
                 else (if Product_Type.equal_bool
                            (More_Word_Library.perm_global
                              comp_countable_integer c)
                            true
                        then Error (LogicErr (Unhandled ""))
                        else (match
                               Mapping.lookup
                                 (Collection_Order.ccompare_integer,
                                   Arith.equal_integer)
                                 (heap_map h)
                                 (More_Word_Library.block_id
                                   comp_countable_integer c)
                               with None -> Error (LogicErr MissingResource)
                               | Some Freed -> Error (LogicErr UseAfterFree)
                               | Some (Map m) ->
                                 (if not (Arith.equal_inta
   (More_Word_Library.offset comp_countable_integer c) Arith.zero_inta)
                                   then Error (LogicErr (Unhandled ""))
                                   else (if Arith.less_int
      (Arith.int_of_nat
        (Arith.plus_nat (More_Word_Library.base comp_countable_integer c)
          (More_Word_Library.len comp_countable_integer c)))
      (More_Word_Library.offset comp_countable_integer c)
  then Error (LogicErr (Unhandled ""))
  else (let cap_bound =
          (More_Word_Library.base comp_countable_integer c,
            Arith.plus_nat (More_Word_Library.base comp_countable_integer c)
              (More_Word_Library.len comp_countable_integer c))
          in
         (if not (Product_Type.equal_prod Arith.equal_nat Arith.equal_nat
                   cap_bound (bounds m))
           then Error (LogicErr (Unhandled ""))
           else (let ha =
                   heap_map_update
                     (fun _ ->
                       Mapping.update
                         (Collection_Order.ccompare_integer,
                           Arith.equal_integer)
                         (More_Word_Library.block_id comp_countable_integer c)
                         Freed (heap_map h))
                     h
                   in
                 let cap =
                   More_Word_Library.tag_update comp_countable_integer
                     (fun _ -> false) c
                   in
                  Success (ha, cap))))))))));;

let rec is_contiguous_zeros_prim
  uu uv siz =
    (if Arith.equal_nata siz Arith.zero_nat then true
      else Option.equal_option equal_memval
             (Mapping.lookup (Collection_Order.ccompare_nat, Arith.equal_nat) uu
               uv)
             (Some (Byte (Word.zero_word
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0
                                 Type_Length.len_num1)))))) &&
             is_contiguous_zeros_prim uu (Arith.suc uv)
               (Arith.minus_nat siz Arith.one_nat));;

let rec is_contiguous_zeros m off siz = is_contiguous_zeros_prim m off siz;;

let rec memval_is_byte
  m = (match m with Byte _ -> true | ACap (_, _) -> false);;

let rec is_contiguous_bytes
  uu uv siz =
    (if Arith.equal_nata siz Arith.zero_nat then true
      else not (Option.is_none
                 (Mapping.lookup
                   (Collection_Order.ccompare_nat, Arith.equal_nat) uu uv)) &&
             (memval_is_byte
                (Option.the
                  (Mapping.lookup
                    (Collection_Order.ccompare_nat, Arith.equal_nat) uu uv)) &&
               is_contiguous_bytes uu (Arith.suc uv)
                 (Arith.minus_nat siz Arith.one_nat)));;

let rec of_nth (ACap (x21, x22)) = x22;;

let rec of_cap (ACap (x21, x22)) = x21;;

let rec is_contiguous_cap
  uu uv uw siz =
    (if Arith.equal_nata siz Arith.zero_nat then true
      else not (Option.is_none
                 (Mapping.lookup
                   (Collection_Order.ccompare_nat, Arith.equal_nat) uu uw)) &&
             (not (memval_is_byte
                    (Option.the
                      (Mapping.lookup
                        (Collection_Order.ccompare_nat, Arith.equal_nat) uu
                        uw))) &&
               (More_Word_Library.equal_mem_capability_exta
                  (Arith.equal_integer, comp_countable_integer)
                  Product_Type.equal_unit
                  (of_cap
                    (Option.the
                      (Mapping.lookup
                        (Collection_Order.ccompare_nat, Arith.equal_nat) uu
                        uw)))
                  uv &&
                 (Arith.equal_nata
                    (of_nth
                      (Option.the
                        (Mapping.lookup
                          (Collection_Order.ccompare_nat, Arith.equal_nat) uu
                          uw)))
                    (Arith.minus_nat siz Arith.one_nat) &&
                   is_contiguous_cap uu uv (Arith.suc uw)
                     (Arith.minus_nat siz Arith.one_nat)))));;

let rec of_byte (Byte x1) = x1;;

let rec retrieve_bytes
  m uu siz =
    (if Arith.equal_nata siz Arith.zero_nat then []
      else of_byte
             (Option.the
               (Mapping.lookup (Collection_Order.ccompare_nat, Arith.equal_nat)
                 m uu)) ::
             retrieve_bytes m (Arith.suc uu)
               (Arith.minus_nat siz Arith.one_nat));;

let rec content (Object_ext (bounds, content, tags, more)) = content;;

let rec tags (Object_ext (bounds, content, tags, more)) = tags;;

let rec get_cap
  m off =
    of_cap
      (Option.the
        (Mapping.lookup (Collection_Order.ccompare_nat, Arith.equal_nat) m
          off));;

let rec sizeof
  tau = (match tau with More_Word_Library.Uint8 -> Arith.one_nat
          | More_Word_Library.Sint8 -> Arith.one_nat
          | More_Word_Library.Uint16 -> Arith.nat_of_integer (Z.of_int 2)
          | More_Word_Library.Sint16 -> Arith.nat_of_integer (Z.of_int 2)
          | More_Word_Library.Uint32 -> Arith.nat_of_integer (Z.of_int 4)
          | More_Word_Library.Sint32 -> Arith.nat_of_integer (Z.of_int 4)
          | More_Word_Library.Uint64 -> Arith.nat_of_integer (Z.of_int 8)
          | More_Word_Library.Sint64 -> Arith.nat_of_integer (Z.of_int 8)
          | More_Word_Library.Cap -> Arith.nat_of_integer (Z.of_int 32));;

let rec is_cap
  m off =
    not (Option.is_none
          (Mapping.lookup (Collection_Order.ccompare_nat, Arith.equal_nat) m
            off)) &&
      not (memval_is_byte
            (Option.the
              (Mapping.lookup (Collection_Order.ccompare_nat, Arith.equal_nat) m
                off)));;

let rec retrieve_tval
  obj off typ pcl =
    (if is_contiguous_bytes (content obj) off (sizeof typ)
      then (match typ
             with More_Word_Library.Uint8 ->
               More_Word_Library.Uint8_v
                 (Lista.hd (retrieve_bytes (content obj) off (sizeof typ)))
             | More_Word_Library.Sint8 ->
               More_Word_Library.Sint8_v
                 (Word.cast
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0 Type_Length.len_num1)))
                   (Signed_Words.len_signed
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0 Type_Length.len_num1))))
                   (Lista.hd (retrieve_bytes (content obj) off (sizeof typ))))
             | More_Word_Library.Uint16 ->
               More_Word_Library.Uint16_v
                 (Word.word_rcat
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0 Type_Length.len_num1)))
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0 Type_Length.len_num1))))
                   (retrieve_bytes (content obj) off (sizeof typ)))
             | More_Word_Library.Sint16 ->
               More_Word_Library.Sint16_v
                 (Word.word_rcat
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0 Type_Length.len_num1)))
                   (Signed_Words.len_signed
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0 Type_Length.len_num1)))))
                   (retrieve_bytes (content obj) off (sizeof typ)))
             | More_Word_Library.Uint32 ->
               More_Word_Library.Uint32_v
                 (Word.word_rcat
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0 Type_Length.len_num1)))
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0 Type_Length.len_num1)))))
                   (retrieve_bytes (content obj) off (sizeof typ)))
             | More_Word_Library.Sint32 ->
               More_Word_Library.Sint32_v
                 (Word.word_rcat
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0 Type_Length.len_num1)))
                   (Signed_Words.len_signed
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0 Type_Length.len_num1))))))
                   (retrieve_bytes (content obj) off (sizeof typ)))
             | More_Word_Library.Uint64 ->
               More_Word_Library.Uint64_v
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
                   (retrieve_bytes (content obj) off (sizeof typ)))
             | More_Word_Library.Sint64 ->
               More_Word_Library.Sint64_v
                 (Word.word_rcat
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0 Type_Length.len_num1)))
                   (Signed_Words.len_signed
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0
                           (Type_Length.len_bit0
                             (Type_Length.len_bit0
                               (Type_Length.len_bit0 Type_Length.len_num1)))))))
                   (retrieve_bytes (content obj) off (sizeof typ)))
             | More_Word_Library.Cap ->
               (if is_contiguous_zeros (content obj) off (sizeof typ)
                 then More_Word_Library.Cap_v null_capability
                 else More_Word_Library.Undef))
      else (if is_cap (content obj) off
             then (let cap = get_cap (content obj) off in
                   let tv =
                     Option.the
                       (Mapping.lookup
                         (Collection_Order.ccompare_nat, Arith.equal_nat)
                         (tags obj)
                         (if Arith.equal_nata
                               (Arith.modulo_nat off
                                 (sizeof More_Word_Library.Cap))
                               Arith.zero_nat
                           then off
                           else Arith.minus_nat off
                                  (Arith.modulo_nat off
                                    (sizeof More_Word_Library.Cap))))
                     in
                   let t = (match pcl with true -> tv | false -> false) in
                   let cv =
                     More_Word_Library.extend comp_countable_integer cap
                       (More_Word_Library.Capability_ext (t, ()))
                     in
                   let nth_frag =
                     of_nth
                       (Option.the
                         (Mapping.lookup
                           (Collection_Order.ccompare_nat, Arith.equal_nat)
                           (content obj) off))
                     in
                    (match typ
                      with More_Word_Library.Uint8 ->
                        More_Word_Library.Cap_v_frag (cv, nth_frag)
                      | More_Word_Library.Sint8 ->
                        More_Word_Library.Cap_v_frag (cv, nth_frag)
                      | More_Word_Library.Uint16 -> More_Word_Library.Undef
                      | More_Word_Library.Sint16 -> More_Word_Library.Undef
                      | More_Word_Library.Uint32 -> More_Word_Library.Undef
                      | More_Word_Library.Sint32 -> More_Word_Library.Undef
                      | More_Word_Library.Uint64 -> More_Word_Library.Undef
                      | More_Word_Library.Sint64 -> More_Word_Library.Undef
                      | More_Word_Library.Cap ->
                        (if is_contiguous_cap (content obj) cap off (sizeof typ)
                          then More_Word_Library.Cap_v cv
                          else More_Word_Library.Undef)))
             else More_Word_Library.Undef));;

let rec load
  h c t =
    (if Product_Type.equal_bool (More_Word_Library.tag comp_countable_integer c)
          false
      then Error (C2Err TagViolation)
      else (if Product_Type.equal_bool
                 (More_Word_Library.perm_load comp_countable_integer c) false
             then Error (C2Err PermitLoadViolation)
             else (if Arith.less_int
                        (Arith.int_of_nat
                          (Arith.plus_nat
                            (More_Word_Library.base comp_countable_integer c)
                            (More_Word_Library.len comp_countable_integer c)))
                        (Arith.plus_inta
                          (More_Word_Library.offset comp_countable_integer c)
                          (Arith.int_of_nat (sizeof t)))
                    then Error (C2Err LengthViolation)
                    else (if Arith.less_int
                               (More_Word_Library.offset comp_countable_integer
                                 c)
                               (Arith.int_of_nat
                                 (More_Word_Library.base comp_countable_integer
                                   c))
                           then Error (C2Err LengthViolation)
                           else (if not (Arith.equal_inta
  (Arith.modulo_inta (More_Word_Library.offset comp_countable_integer c)
    (Arith.int_of_nat (sizeof t)))
  Arith.zero_inta)
                                  then Error (C2Err BadAddressViolation)
                                  else (match
 Mapping.lookup (Collection_Order.ccompare_integer, Arith.equal_integer)
   (heap_map h) (More_Word_Library.block_id comp_countable_integer c)
 with None -> Error (LogicErr MissingResource)
 | Some Freed -> Error (LogicErr UseAfterFree)
 | Some (Map m) ->
   (if Arith.less_int (More_Word_Library.offset comp_countable_integer c)
         (Arith.int_of_nat (Product_Type.fst (bounds m))) ||
         Arith.less_int (Arith.int_of_nat (Product_Type.snd (bounds m)))
           (Arith.plus_inta (More_Word_Library.offset comp_countable_integer c)
             (Arith.int_of_nat (sizeof t)))
     then Error (LogicErr BufferOverrun)
     else Success
            (retrieve_tval m
              (Arith.nat (More_Word_Library.offset comp_countable_integer c)) t
              (More_Word_Library.perm_cap_load comp_countable_integer
                c)))))))));;

let rec next_block_update
  next_blocka (Heap_ext (next_block, heap_map, more)) =
    Heap_ext (next_blocka next_block, heap_map, more);;

let rec next_block (Heap_ext (next_block, heap_map, more)) = next_block;;

let rec alloc
  h c s =
    (let cap =
       More_Word_Library.Mem_capability_ext
         (next_block h, Arith.zero_inta, Arith.zero_nat, s, true, c, true, c, c,
           false, More_Word_Library.Capability_ext (true, ()))
       in
     let ha =
       heap_map_update
         (fun _ ->
           Mapping.update
             (Collection_Order.ccompare_integer, Arith.equal_integer)
             (next_block h)
             (Map (Object_ext
                    ((Arith.zero_nat, s),
                      Mapping_Impl.mapping_empty Collection_Order.ccompare_nat
                        (Phantom_Type.of_phantom Mapping_Impl.mapping_impl_nat),
                      Mapping_Impl.mapping_empty Collection_Order.ccompare_nat
                        (Phantom_Type.of_phantom Mapping_Impl.mapping_impl_nat),
                      ())))
             (heap_map h))
         (next_block_update (fun _ -> Z.add (next_block h) (Z.of_int 1)) h)
       in
      Success (ha, cap));;

let rec content_update
  contenta (Object_ext (bounds, content, tags, more)) =
    Object_ext (bounds, contenta content, tags, more);;

let rec tags_update
  tagsa (Object_ext (bounds, content, tags, more)) =
    Object_ext (bounds, content, tagsa tags, more);;

let rec store_bytes
  obj uu x2 = match obj, uu, x2 with obj, uu, [] -> obj
    | obj, off, v :: vs ->
        store_bytes
          (Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat) off
            (Byte v) obj)
          (Arith.suc off) vs;;

let rec store_cap
  obj uu uv n =
    (if Arith.equal_nata n Arith.zero_nat then obj
      else store_cap
             (Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat) uu
               (ACap (More_Word_Library.truncate comp_countable_integer uv,
                       Arith.minus_nat n Arith.one_nat))
               obj)
             (Arith.suc uu) uv (Arith.minus_nat n Arith.one_nat));;

let rec store_tval
  obj off vala =
    (match vala
      with More_Word_Library.Uint8_v v ->
        tags_update
          (fun _ ->
            Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat)
              (if Arith.equal_nata
                    (Arith.modulo_nat off (sizeof More_Word_Library.Cap))
                    Arith.zero_nat
                then off
                else Arith.minus_nat off
                       (Arith.modulo_nat off (sizeof More_Word_Library.Cap)))
              false (tags obj))
          (content_update (fun _ -> store_bytes (content obj) off [v]) obj)
      | More_Word_Library.Sint8_v v ->
        tags_update
          (fun _ ->
            Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat)
              (if Arith.equal_nata
                    (Arith.modulo_nat off (sizeof More_Word_Library.Cap))
                    Arith.zero_nat
                then off
                else Arith.minus_nat off
                       (Arith.modulo_nat off (sizeof More_Word_Library.Cap)))
              false (tags obj))
          (content_update
            (fun _ ->
              store_bytes (content obj) off
                [Word.signed_cast
                   (Signed_Words.len_signed
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0
                         (Type_Length.len_bit0 Type_Length.len_num1))))
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0
                       (Type_Length.len_bit0 Type_Length.len_num1)))
                   v])
            obj)
      | More_Word_Library.Uint16_v v ->
        tags_update
          (fun _ ->
            Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat)
              (if Arith.equal_nata
                    (Arith.modulo_nat off (sizeof More_Word_Library.Cap))
                    Arith.zero_nat
                then off
                else Arith.minus_nat off
                       (Arith.modulo_nat off (sizeof More_Word_Library.Cap)))
              false (tags obj))
          (content_update
            (fun _ ->
              store_bytes (content obj) off
                (Rsplit.word_rsplit
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0 Type_Length.len_num1))))
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1)))
                  v))
            obj)
      | More_Word_Library.Sint16_v v ->
        tags_update
          (fun _ ->
            Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat)
              (if Arith.equal_nata
                    (Arith.modulo_nat off (sizeof More_Word_Library.Cap))
                    Arith.zero_nat
                then off
                else Arith.minus_nat off
                       (Arith.modulo_nat off (sizeof More_Word_Library.Cap)))
              false (tags obj))
          (content_update
            (fun _ ->
              store_bytes (content obj) off
                (Rsplit.word_rsplit
                  (Signed_Words.len_signed
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0 Type_Length.len_num1)))))
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1)))
                  v))
            obj)
      | More_Word_Library.Uint32_v v ->
        tags_update
          (fun _ ->
            Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat)
              (if Arith.equal_nata
                    (Arith.modulo_nat off (sizeof More_Word_Library.Cap))
                    Arith.zero_nat
                then off
                else Arith.minus_nat off
                       (Arith.modulo_nat off (sizeof More_Word_Library.Cap)))
              false (tags obj))
          (content_update
            (fun _ ->
              store_bytes (content obj) off
                (Rsplit.word_rsplit
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0 Type_Length.len_num1)))))
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1)))
                  v))
            obj)
      | More_Word_Library.Sint32_v v ->
        tags_update
          (fun _ ->
            Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat)
              (if Arith.equal_nata
                    (Arith.modulo_nat off (sizeof More_Word_Library.Cap))
                    Arith.zero_nat
                then off
                else Arith.minus_nat off
                       (Arith.modulo_nat off (sizeof More_Word_Library.Cap)))
              false (tags obj))
          (content_update
            (fun _ ->
              store_bytes (content obj) off
                (Rsplit.word_rsplit
                  (Signed_Words.len_signed
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0
                            (Type_Length.len_bit0 Type_Length.len_num1))))))
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1)))
                  v))
            obj)
      | More_Word_Library.Uint64_v v ->
        tags_update
          (fun _ ->
            Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat)
              (if Arith.equal_nata
                    (Arith.modulo_nat off (sizeof More_Word_Library.Cap))
                    Arith.zero_nat
                then off
                else Arith.minus_nat off
                       (Arith.modulo_nat off (sizeof More_Word_Library.Cap)))
              false (tags obj))
          (content_update
            (fun _ ->
              store_bytes (content obj) off
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
                  v))
            obj)
      | More_Word_Library.Sint64_v v ->
        tags_update
          (fun _ ->
            Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat)
              (if Arith.equal_nata
                    (Arith.modulo_nat off (sizeof More_Word_Library.Cap))
                    Arith.zero_nat
                then off
                else Arith.minus_nat off
                       (Arith.modulo_nat off (sizeof More_Word_Library.Cap)))
              false (tags obj))
          (content_update
            (fun _ ->
              store_bytes (content obj) off
                (Rsplit.word_rsplit
                  (Signed_Words.len_signed
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0
                            (Type_Length.len_bit0
                              (Type_Length.len_bit0 Type_Length.len_num1)))))))
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1)))
                  v))
            obj)
      | More_Word_Library.Cap_v c ->
        tags_update
          (fun _ ->
            Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat)
              (if Arith.equal_nata
                    (Arith.modulo_nat off (sizeof More_Word_Library.Cap))
                    Arith.zero_nat
                then off
                else Arith.minus_nat off
                       (Arith.modulo_nat off (sizeof More_Word_Library.Cap)))
              (More_Word_Library.tag comp_countable_integer c) (tags obj))
          (content_update
            (fun _ ->
              store_cap (content obj) off c (sizeof More_Word_Library.Cap))
            obj)
      | More_Word_Library.Cap_v_frag (c, n) ->
        tags_update
          (fun _ ->
            Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat)
              (if Arith.equal_nata
                    (Arith.modulo_nat off (sizeof More_Word_Library.Cap))
                    Arith.zero_nat
                then off
                else Arith.minus_nat off
                       (Arith.modulo_nat off (sizeof More_Word_Library.Cap)))
              false (tags obj))
          (content_update
            (fun _ ->
              Mapping.update (Collection_Order.ccompare_nat, Arith.equal_nat)
                off (ACap (More_Word_Library.truncate comp_countable_integer c,
                            n))
                (content obj))
            obj));;

let rec store
  h c v =
    (if Product_Type.equal_bool (More_Word_Library.tag comp_countable_integer c)
          false
      then Error (C2Err TagViolation)
      else (if Product_Type.equal_bool
                 (More_Word_Library.perm_store comp_countable_integer c) false
             then Error (C2Err PermitStoreViolation)
             else (if (match v with More_Word_Library.Uint8_v _ -> false
                        | More_Word_Library.Sint8_v _ -> false
                        | More_Word_Library.Uint16_v _ -> false
                        | More_Word_Library.Sint16_v _ -> false
                        | More_Word_Library.Uint32_v _ -> false
                        | More_Word_Library.Sint32_v _ -> false
                        | More_Word_Library.Uint64_v _ -> false
                        | More_Word_Library.Sint64_v _ -> false
                        | More_Word_Library.Cap_v cv ->
                          not (More_Word_Library.perm_cap_store
                                comp_countable_integer c) &&
                            More_Word_Library.tag comp_countable_integer cv
                        | More_Word_Library.Cap_v_frag (_, _) -> false
                        | More_Word_Library.Undef -> false)
                    then Error (C2Err PermitStoreCapViolation)
                    else (if (match v with More_Word_Library.Uint8_v _ -> false
                               | More_Word_Library.Sint8_v _ -> false
                               | More_Word_Library.Uint16_v _ -> false
                               | More_Word_Library.Sint16_v _ -> false
                               | More_Word_Library.Uint32_v _ -> false
                               | More_Word_Library.Sint32_v _ -> false
                               | More_Word_Library.Uint64_v _ -> false
                               | More_Word_Library.Sint64_v _ -> false
                               | More_Word_Library.Cap_v cv ->
                                 not (More_Word_Library.perm_cap_store_local
                                       comp_countable_integer c) &&
                                   (More_Word_Library.tag comp_countable_integer
                                      cv &&
                                     not (More_Word_Library.perm_global
   comp_countable_integer cv))
                               | More_Word_Library.Cap_v_frag (_, _) -> false
                               | More_Word_Library.Undef -> false)
                           then Error (C2Err PermitStoreLocalCapViolation)
                           else (if Arith.less_int
                                      (Arith.int_of_nat
(Arith.plus_nat (More_Word_Library.base comp_countable_integer c)
  (More_Word_Library.len comp_countable_integer c)))
                                      (Arith.plus_inta
(More_Word_Library.offset comp_countable_integer c)
(Arith.int_of_nat (sizeof (More_Word_Library.memval_type v))))
                                  then Error (C2Err LengthViolation)
                                  else (if Arith.less_int
     (More_Word_Library.offset comp_countable_integer c)
     (Arith.int_of_nat (More_Word_Library.base comp_countable_integer c))
 then Error (C2Err LengthViolation)
 else (if not (Arith.equal_inta
                (Arith.modulo_inta
                  (More_Word_Library.offset comp_countable_integer c)
                  (Arith.int_of_nat (sizeof (More_Word_Library.memval_type v))))
                Arith.zero_inta)
        then Error (C2Err BadAddressViolation)
        else (if More_Word_Library.equal_ccvala
                   (Arith.equal_integer, comp_countable_integer) v
                   More_Word_Library.Undef
               then Error (LogicErr (Unhandled ""))
               else (match
                      Mapping.lookup
                        (Collection_Order.ccompare_integer, Arith.equal_integer)
                        (heap_map h)
                        (More_Word_Library.block_id comp_countable_integer c)
                      with None -> Error (LogicErr MissingResource)
                      | Some Freed -> Error (LogicErr UseAfterFree)
                      | Some (Map m) ->
                        (if Arith.less_int
                              (More_Word_Library.offset comp_countable_integer
                                c)
                              (Arith.int_of_nat
                                (Product_Type.fst (bounds m))) ||
                              Arith.less_int
                                (Arith.int_of_nat (Product_Type.snd (bounds m)))
                                (Arith.plus_inta
                                  (More_Word_Library.offset
                                    comp_countable_integer c)
                                  (Arith.int_of_nat
                                    (sizeof (More_Word_Library.memval_type v))))
                          then Error (LogicErr BufferOverrun)
                          else Success
                                 (heap_map_update
                                   (fun _ ->
                                     Mapping.update
                                       (Collection_Order.ccompare_integer,
 Arith.equal_integer)
                                       (More_Word_Library.block_id
 comp_countable_integer c)
                                       (Map
 (store_tval m (Arith.nat (More_Word_Library.offset comp_countable_integer c))
   v))
                                       (heap_map h))
                                   h)))))))))));;

let rec res (Success x1) = x1;;

let rec is_Success = function Success x1 -> true
                     | Error x2 -> false;;

let rec memset_prim
  h uu uv n =
    (if Arith.equal_nata n Arith.zero_nat then Success h
      else (let hs = store h uu uv in
             (if not (is_Success hs) then hs
               else memset_prim (res hs)
                      (More_Word_Library.offset_update comp_countable_integer
                        (fun _ ->
                          Arith.plus_inta
                            (More_Word_Library.offset comp_countable_integer uu)
                            (Arith.int_of_nat
                              (sizeof (More_Word_Library.memval_type uv))))
                        uu)
                      uv (Arith.minus_nat n Arith.one_nat))));;

let rec u8_cast
  v = (match v with More_Word_Library.Uint8_v a -> More_Word_Library.Uint8_v a
        | More_Word_Library.Sint8_v va ->
          More_Word_Library.Uint8_v
            (Word.cast
              (Signed_Words.len_signed
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0 Type_Length.len_num1))))
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1)))
              va)
        | More_Word_Library.Uint16_v va ->
          More_Word_Library.Uint8_v
            (Word.cast
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0 Type_Length.len_num1))))
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1)))
              va)
        | More_Word_Library.Sint16_v va ->
          More_Word_Library.Uint8_v
            (Word.cast
              (Signed_Words.len_signed
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1)))))
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1)))
              va)
        | More_Word_Library.Uint32_v va ->
          More_Word_Library.Uint8_v
            (Word.cast
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0 Type_Length.len_num1)))))
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1)))
              va)
        | More_Word_Library.Sint32_v va ->
          More_Word_Library.Uint8_v
            (Word.cast
              (Signed_Words.len_signed
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0 Type_Length.len_num1))))))
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1)))
              va)
        | More_Word_Library.Uint64_v va ->
          More_Word_Library.Uint8_v
            (Word.cast
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0 Type_Length.len_num1))))))
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1)))
              va)
        | More_Word_Library.Sint64_v va ->
          More_Word_Library.Uint8_v
            (Word.cast
              (Signed_Words.len_signed
                (Type_Length.len_bit0
                  (Type_Length.len_bit0
                    (Type_Length.len_bit0
                      (Type_Length.len_bit0
                        (Type_Length.len_bit0
                          (Type_Length.len_bit0 Type_Length.len_num1)))))))
              (Type_Length.len_bit0
                (Type_Length.len_bit0
                  (Type_Length.len_bit0 Type_Length.len_num1)))
              va)
        | More_Word_Library.Cap_v va ->
          More_Word_Library.Cap_v_frag (va, Arith.nat_of_integer (Z.of_int 31))
        | More_Word_Library.Cap_v_frag (a, b) ->
          More_Word_Library.Cap_v_frag (a, b)
        | More_Word_Library.Undef -> More_Word_Library.Undef);;

let rec memset h c v n = memset_prim h c (u8_cast v) n;;

let rec calloc
  h n = (let hres = res (alloc h true n) in
         let ha =
           memset (Product_Type.fst hres) (Product_Type.snd hres)
             (More_Word_Library.Uint8_v
               (Word.zero_word
                 (Type_Length.len_bit0
                   (Type_Length.len_bit0
                     (Type_Length.len_bit0 Type_Length.len_num1)))))
             n
           in
          Success (res ha, Product_Type.snd hres));;

let rec malloc h n = alloc h true n;;

let rec equal_logicerrtype
  x0 x1 = match x0, x1 with MemoryNotFreed, Unhandled x6 -> false
    | Unhandled x6, MemoryNotFreed -> false
    | WrongMemVal, Unhandled x6 -> false
    | Unhandled x6, WrongMemVal -> false
    | WrongMemVal, MemoryNotFreed -> false
    | MemoryNotFreed, WrongMemVal -> false
    | MissingResource, Unhandled x6 -> false
    | Unhandled x6, MissingResource -> false
    | MissingResource, MemoryNotFreed -> false
    | MemoryNotFreed, MissingResource -> false
    | MissingResource, WrongMemVal -> false
    | WrongMemVal, MissingResource -> false
    | BufferOverrun, Unhandled x6 -> false
    | Unhandled x6, BufferOverrun -> false
    | BufferOverrun, MemoryNotFreed -> false
    | MemoryNotFreed, BufferOverrun -> false
    | BufferOverrun, WrongMemVal -> false
    | WrongMemVal, BufferOverrun -> false
    | BufferOverrun, MissingResource -> false
    | MissingResource, BufferOverrun -> false
    | UseAfterFree, Unhandled x6 -> false
    | Unhandled x6, UseAfterFree -> false
    | UseAfterFree, MemoryNotFreed -> false
    | MemoryNotFreed, UseAfterFree -> false
    | UseAfterFree, WrongMemVal -> false
    | WrongMemVal, UseAfterFree -> false
    | UseAfterFree, MissingResource -> false
    | MissingResource, UseAfterFree -> false
    | UseAfterFree, BufferOverrun -> false
    | BufferOverrun, UseAfterFree -> false
    | Unhandled x6, Unhandled y6 -> ((x6 : string) = y6)
    | MemoryNotFreed, MemoryNotFreed -> true
    | WrongMemVal, WrongMemVal -> true
    | MissingResource, MissingResource -> true
    | BufferOverrun, BufferOverrun -> true
    | UseAfterFree, UseAfterFree -> true;;

let rec equal_c2errtype
  x0 x1 = match x0, x1 with LengthViolation, BadAddressViolation -> false
    | BadAddressViolation, LengthViolation -> false
    | PermitStoreLocalCapViolation, BadAddressViolation -> false
    | BadAddressViolation, PermitStoreLocalCapViolation -> false
    | PermitStoreLocalCapViolation, LengthViolation -> false
    | LengthViolation, PermitStoreLocalCapViolation -> false
    | PermitStoreCapViolation, BadAddressViolation -> false
    | BadAddressViolation, PermitStoreCapViolation -> false
    | PermitStoreCapViolation, LengthViolation -> false
    | LengthViolation, PermitStoreCapViolation -> false
    | PermitStoreCapViolation, PermitStoreLocalCapViolation -> false
    | PermitStoreLocalCapViolation, PermitStoreCapViolation -> false
    | PermitStoreViolation, BadAddressViolation -> false
    | BadAddressViolation, PermitStoreViolation -> false
    | PermitStoreViolation, LengthViolation -> false
    | LengthViolation, PermitStoreViolation -> false
    | PermitStoreViolation, PermitStoreLocalCapViolation -> false
    | PermitStoreLocalCapViolation, PermitStoreViolation -> false
    | PermitStoreViolation, PermitStoreCapViolation -> false
    | PermitStoreCapViolation, PermitStoreViolation -> false
    | PermitLoadViolation, BadAddressViolation -> false
    | BadAddressViolation, PermitLoadViolation -> false
    | PermitLoadViolation, LengthViolation -> false
    | LengthViolation, PermitLoadViolation -> false
    | PermitLoadViolation, PermitStoreLocalCapViolation -> false
    | PermitStoreLocalCapViolation, PermitLoadViolation -> false
    | PermitLoadViolation, PermitStoreCapViolation -> false
    | PermitStoreCapViolation, PermitLoadViolation -> false
    | PermitLoadViolation, PermitStoreViolation -> false
    | PermitStoreViolation, PermitLoadViolation -> false
    | TagViolation, BadAddressViolation -> false
    | BadAddressViolation, TagViolation -> false
    | TagViolation, LengthViolation -> false
    | LengthViolation, TagViolation -> false
    | TagViolation, PermitStoreLocalCapViolation -> false
    | PermitStoreLocalCapViolation, TagViolation -> false
    | TagViolation, PermitStoreCapViolation -> false
    | PermitStoreCapViolation, TagViolation -> false
    | TagViolation, PermitStoreViolation -> false
    | PermitStoreViolation, TagViolation -> false
    | TagViolation, PermitLoadViolation -> false
    | PermitLoadViolation, TagViolation -> false
    | BadAddressViolation, BadAddressViolation -> true
    | LengthViolation, LengthViolation -> true
    | PermitStoreLocalCapViolation, PermitStoreLocalCapViolation -> true
    | PermitStoreCapViolation, PermitStoreCapViolation -> true
    | PermitStoreViolation, PermitStoreViolation -> true
    | PermitLoadViolation, PermitLoadViolation -> true
    | TagViolation, TagViolation -> true;;

let rec equal_errtype x0 x1 = match x0, x1 with C2Err x1, LogicErr x2 -> false
                        | LogicErr x2, C2Err x1 -> false
                        | LogicErr x2, LogicErr y2 -> equal_logicerrtype x2 y2
                        | C2Err x1, C2Err y1 -> equal_c2errtype x1 y1;;

let rec equal_result _A
  x0 x1 = match x0, x1 with Success x1, Error x2 -> false
    | Error x2, Success x1 -> false
    | Error x2, Error y2 -> equal_errtype x2 y2
    | Success x1, Success y1 -> HOL.eq _A x1 y1;;

let rec err (Error x2) = x2;;

let rec memcmp
  h s1 s2 n =
    (if Arith.equal_nata n Arith.zero_nat then Success true
      else (let v1 = load h s1 More_Word_Library.Uint8 in
            let v2 = load h s2 More_Word_Library.Uint8 in
             (if not (is_Success v1) then Error (err v1)
               else (if not (is_Success v2) then Error (err v2)
                      else (if equal_result
                                 (More_Word_Library.equal_ccval
                                   (Arith.equal_integer,
                                     comp_countable_integer))
                                 v1 (Success More_Word_Library.Undef) ||
                                 equal_result
                                   (More_Word_Library.equal_ccval
                                     (Arith.equal_integer,
                                       comp_countable_integer))
                                   v2 (Success More_Word_Library.Undef)
                             then Error (LogicErr WrongMemVal)
                             else (if not
(equal_result
  (More_Word_Library.equal_ccval (Arith.equal_integer, comp_countable_integer))
  v1 v2)
                                    then Success false
                                    else memcmp h s1 s2
   (Arith.minus_nat n Arith.one_nat)))))));;

let rec memcpy_cap
  h uw ux n =
    (if Arith.equal_nata n Arith.zero_nat then Success h
      else (if Arith.less_nat (Arith.suc (Arith.minus_nat n Arith.one_nat))
                 (sizeof More_Word_Library.Cap)
             then memcpy_prim h uw ux
                    (Arith.suc (Arith.minus_nat n Arith.one_nat))
             else (let x = load h ux More_Word_Library.Cap in
                    (if not (is_Success x)
                      then memcpy_prim h uw ux
                             (Arith.suc (Arith.minus_nat n Arith.one_nat))
                      else (let xs = res x in
                             (if More_Word_Library.equal_ccvala
                                   (Arith.equal_integer, comp_countable_integer)
                                   xs More_Word_Library.Undef
                               then memcpy_prim h uw ux
                                      (Arith.suc
(Arith.minus_nat n Arith.one_nat))
                               else (let y = store h uw xs in
                                      (if not (is_Success y)
then memcpy_prim h uw ux (Arith.suc (Arith.minus_nat n Arith.one_nat))
else (let ys = res y in
       memcpy_cap ys
         (More_Word_Library.offset_update comp_countable_integer
           (fun _ ->
             Arith.plus_inta
               (More_Word_Library.offset comp_countable_integer uw)
               (Arith.int_of_nat (sizeof More_Word_Library.Cap)))
           uw)
         (More_Word_Library.offset_update comp_countable_integer
           (fun _ ->
             Arith.plus_inta
               (More_Word_Library.offset comp_countable_integer ux)
               (Arith.int_of_nat (sizeof More_Word_Library.Cap)))
           ux)
         (Arith.minus_nat (Arith.suc (Arith.minus_nat n Arith.one_nat))
           (sizeof More_Word_Library.Cap)))))))))))
and memcpy_prim
  h uu uv n =
    (if Arith.equal_nata n Arith.zero_nat then Success h
      else (let x = load h uv More_Word_Library.Uint8 in
             (if not (is_Success x) then Error (err x)
               else (let xs = res x in
                      (if More_Word_Library.equal_ccvala
                            (Arith.equal_integer, comp_countable_integer) xs
                            More_Word_Library.Undef
                        then Error (LogicErr (Unhandled ""))
                        else (let y = store h uu xs in
                               (if not (is_Success y) then Error (err y)
                                 else (let ys = res y in
memcpy_cap ys
  (More_Word_Library.offset_update comp_countable_integer
    (fun _ ->
      Arith.plus_inta (More_Word_Library.offset comp_countable_integer uu)
        Arith.one_inta)
    uu)
  (More_Word_Library.offset_update comp_countable_integer
    (fun _ ->
      Arith.plus_inta (More_Word_Library.offset comp_countable_integer uv)
        Arith.one_inta)
    uv)
  (Arith.minus_nat n Arith.one_nat)))))))));;

let rec memcpy
  h dst src n =
    (if Arith.equal_nata n Arith.zero_nat then Success h
      else (if Z.equal (More_Word_Library.block_id comp_countable_integer dst)
                 (More_Word_Library.block_id comp_countable_integer src) &&
                 (Arith.less_eq_int
                    (More_Word_Library.offset comp_countable_integer dst)
                    (More_Word_Library.offset comp_countable_integer src) &&
                    Arith.less_int
                      (More_Word_Library.offset comp_countable_integer src)
                      (Arith.plus_inta
                        (More_Word_Library.offset comp_countable_integer dst)
                        (Arith.int_of_nat n)) ||
                   Arith.less_eq_int
                     (More_Word_Library.offset comp_countable_integer src)
                     (More_Word_Library.offset comp_countable_integer dst) &&
                     Arith.less_int
                       (More_Word_Library.offset comp_countable_integer dst)
                       (Arith.plus_inta
                         (More_Word_Library.offset comp_countable_integer src)
                         (Arith.int_of_nat n)))
             then Error (LogicErr (Unhandled ""))
             else memcpy_cap h dst src n));;

let rec memmove
  h dst src n = (let (h1, tmp) = res (alloc h true n) in
                 let h2 = res (memcpy h1 tmp src n) in
                 let h3 = res (memcpy h2 dst tmp n) in
                 let (h4, _) = res (free h3 tmp) in
                  Success h4);;

let rec realloc
  h cap n =
    (if More_Word_Library.equal_mem_capability_exta
          (Arith.equal_integer, comp_countable_integer)
          (More_Word_Library.equal_capability_ext Product_Type.equal_unit) cap
          null_capability
      then alloc h true n
      else (let (h1, capa) = res (alloc h true n) in
            let h2 =
              res (memcpy h1 capa cap
                    (Orderings.min Arith.ord_nat n
                      (More_Word_Library.len comp_countable_integer cap)))
              in
            let (h3, _) = res (free h2 cap) in
             Success (h3, capa)));;

let rec zero_heap_ext _A
  = Heap_ext
      (Z.zero,
        Mapping_Impl.mapping_empty Collection_Order.ccompare_integer
          (Phantom_Type.of_phantom Mapping_Impl.mapping_impl_integer),
        Arith.zero
          _A.Separation_Algebra.sep_algebra_cancellative_sep_algebra.Separation_Algebra.pre_sep_algebra_sep_algebra.Separation_Algebra.zero_pre_sep_algebra);;

let init_heap : unit heap_ext
  = next_block_update (fun _ -> (Z.of_int 1))
      (zero_heap_ext cancellative_sep_algebra_unit);;

let rec the_map (Map x2) = x2;;

let rec get_block_size
  h b = (match
          Mapping.lookup
            (Collection_Order.ccompare_integer, Arith.equal_integer)
            (heap_map h) b
          with None -> None
          | Some m ->
            (match m with Freed -> None
              | Map _ -> Some (Product_Type.snd (bounds (the_map m)))));;

let rec get_unfreed_blocks
  uu n =
    (if Arith.equal_nata n Arith.zero_nat then []
      else (match
             Mapping.lookup
               (Collection_Order.ccompare_integer, Arith.equal_integer)
               (heap_map uu)
               (Arith.integer_of_nat
                 (Arith.suc (Arith.minus_nat n Arith.one_nat)))
             with None ->
               get_unfreed_blocks uu (Arith.minus_nat n Arith.one_nat)
             | Some Freed ->
               get_unfreed_blocks uu (Arith.minus_nat n Arith.one_nat)
             | Some (Map _) ->
               Arith.integer_of_nat
                 (Arith.suc (Arith.minus_nat n Arith.one_nat)) ::
                 get_unfreed_blocks uu (Arith.minus_nat n Arith.one_nat)));;

let rec get_memory_leak_size
  uu n =
    (if Arith.equal_nata n Arith.zero_nat then Arith.zero_nat
      else Arith.plus_nat
             (get_memory_leak_size uu (Arith.minus_nat n Arith.one_nat))
             (match
               get_block_size uu
                 (Arith.integer_of_nat
                   (Arith.suc (Arith.minus_nat n Arith.one_nat)))
               with None -> Arith.zero_nat | Some na -> na));;

end;; (*struct CHERI_C_Concrete_Memory_Model*)

module CHERI_C_Global_Environment : sig
  val set_glob_var :
    unit CHERI_C_Concrete_Memory_Model.heap_ext ->
      bool ->
        Arith.nat ->
          string ->
            (string,
              (Z.t, unit More_Word_Library.capability_ext)
                More_Word_Library.mem_capability_ext)
              Mapping.mapping ->
              (unit CHERI_C_Concrete_Memory_Model.heap_ext *
                ((Z.t, unit More_Word_Library.capability_ext)
                   More_Word_Library.mem_capability_ext *
                  (string,
                    (Z.t, unit More_Word_Library.capability_ext)
                      More_Word_Library.mem_capability_ext)
                    Mapping.mapping))
                CHERI_C_Concrete_Memory_Model.result
end = struct

let rec alloc_glob_var
  h c s =
    (let ha = CHERI_C_Concrete_Memory_Model.alloc h c s in
      CHERI_C_Concrete_Memory_Model.Success
        (Product_Type.fst (CHERI_C_Concrete_Memory_Model.res ha),
          More_Word_Library.perm_global_update
            CHERI_C_Concrete_Memory_Model.comp_countable_integer (fun _ -> true)
            (Product_Type.snd (CHERI_C_Concrete_Memory_Model.res ha))));;

let rec set_glob_var
  h c s v g =
    (let (ha, cap) = CHERI_C_Concrete_Memory_Model.res (alloc_glob_var h c s) in
     let ga =
       Mapping.update (Collection_Order.ccompare_literal, Stringa.equal_literal)
         v cap g
       in
      CHERI_C_Concrete_Memory_Model.Success (ha, (cap, ga)));;

end;; (*struct CHERI_C_Global_Environment*)

theory Fat_Capability
  imports Main "HOL-Library.Extended_Nat" "Word_Lib.Word_Lib_Sumo" "HOL-Eisbach.Eisbach"
begin

record pre_capability =
  perm_load :: bool
  perm_cap_load :: bool
  perm_store :: bool
  perm_cap_store :: bool
  base :: nat
  addr :: nat

(* Root capability used as initial capability for *threads* *)
record root_capability = pre_capability + 
  len :: enat


(* Actual abstract capabilities capable of conversion b/w concrete capabilities *)
record capability = pre_capability +
  len :: nat
  (* tag bits only in abstract cap and not concrete *)
  tag :: bool

instantiation pre_capability_ext :: (type) ord
begin
definition "less_eq_pre_capability_ext (c\<^sub>1 :: 'a pre_capability_scheme) (c\<^sub>2 :: 'a pre_capability_scheme) \<equiv> addr c\<^sub>1 \<le> addr c\<^sub>2"
definition "less_pre_capability_ext (c\<^sub>1 :: 'a pre_capability_scheme) (c\<^sub>2 :: 'a pre_capability_scheme) \<equiv> addr c\<^sub>1 < addr c\<^sub>2"
instance ..
end

abbreviation cap_plus :: "capability \<Rightarrow> nat \<Rightarrow> capability" (infix "+\<^sub>c" 65)
  where
  "cap_plus c1 n \<equiv> c1 \<lparr> addr := addr c1 + n \<rparr>"

definition root_cap_init :: "root_capability"
  where
  "root_cap_init \<equiv> \<lparr> perm_load           = True,
                     perm_cap_load       = True,
                     perm_store          = True,
                     perm_cap_store      = True,
                     base                = 0,
                     addr                = 0,
                     root_capability.len = \<infinity> \<rparr>"

definition nth_bit :: "nat \<Rightarrow> ('a :: len) word \<Rightarrow> bool"
  where
  "nth_bit n w \<equiv> if and (2 ^ n) w = 0 then False else True"

definition addr_to_word :: "nat \<Rightarrow> 64 word"
  where
  "addr_to_word a \<equiv> word_of_nat a"

definition word_to_addr :: "64 word \<Rightarrow> nat"
  where
  "word_to_addr w \<equiv> unat w"

definition abs_cap_empty :: "60 word"
  where
  "abs_cap_empty \<equiv> 0"

definition abs_cap_perm_load :: "capability \<Rightarrow> 1 word"
  where
  "abs_cap_perm_load c \<equiv> if perm_load c then 1 else 0"

definition word_cap_perm_load :: "256 word \<Rightarrow> bool"
  where
  "word_cap_perm_load wc \<equiv> if 8 && fst ((word_split :: 128 word \<Rightarrow> 64 word \<times> 64 word) 
                                     (fst ((word_split :: 256 word \<Rightarrow> 128 word \<times> 128 word) wc))) = 0 
                           then False else True"
          
definition abs_cap_perm_cap_load :: "capability \<Rightarrow> 1 word"
  where
  "abs_cap_perm_cap_load c \<equiv> if perm_cap_load c then 1 else 0"

definition word_cap_perm_cap_load :: "256 word \<Rightarrow> bool"
  where
  "word_cap_perm_cap_load wc \<equiv> if 4 && fst ((word_split :: 128 word \<Rightarrow> 64 word \<times> 64 word) 
                                     (fst ((word_split :: 256 word \<Rightarrow> 128 word \<times> 128 word) wc))) = 0 
                           then False else True"

definition abs_cap_perm_store :: "capability \<Rightarrow> 1 word"
  where
  "abs_cap_perm_store c \<equiv> if perm_store c then 1 else 0"

definition word_cap_perm_store :: "256 word \<Rightarrow> bool"
  where
  "word_cap_perm_store wc \<equiv> if 2 && fst ((word_split :: 128 word \<Rightarrow> 64 word \<times> 64 word) 
                                     (fst ((word_split :: 256 word \<Rightarrow> 128 word \<times> 128 word) wc))) = 0 
                           then False else True"

definition abs_cap_perm_cap_store :: "capability \<Rightarrow> 1 word"
  where
  "abs_cap_perm_cap_store c \<equiv> if perm_cap_store c then 1 else 0"

definition word_cap_perm_cap_store :: "256 word \<Rightarrow> bool"
  where
  "word_cap_perm_cap_store wc \<equiv> if 1 && fst ((word_split :: 128 word \<Rightarrow> 64 word \<times> 64 word) 
                                     (fst ((word_split :: 256 word \<Rightarrow> 128 word \<times> 128 word) wc))) = 0 
                           then False else True"

definition abs_cap_metadata :: "capability \<Rightarrow> 64 word"
  where
  "abs_cap_metadata c \<equiv> (word_cat :: 60 word \<Rightarrow> 4 word \<Rightarrow> 64 word) 
                         abs_cap_empty 
                         (word_rcat [
                            abs_cap_perm_load c, 
                            abs_cap_perm_cap_load c, 
                            abs_cap_perm_store c, 
                            abs_cap_perm_cap_store c])"

lemma abs_cap_metadata_size[simp]: 
  "size (abs_cap_metadata c) = 64"
  by (simp add: wsst_TYs(3))

definition abs_cap_len :: "capability \<Rightarrow> 64 word"
  where
  "abs_cap_len c \<equiv> word_of_nat (len c)"

lemma abs_cap_len_size[simp]: 
  "size (abs_cap_len c) = 64"
  by (simp add: wsst_TYs(3))

definition word_cap_len :: "256 word \<Rightarrow> 64 word"
  where
  "word_cap_len w \<equiv> snd ((word_split :: 128 word \<Rightarrow> 64 word \<times> 64 word) 
                       (fst ((word_split :: 256 word \<Rightarrow> 128 word \<times> 128 word) w)))"

definition abs_cap_base :: "capability \<Rightarrow> 64 word"
  where
  "abs_cap_base c \<equiv> word_of_nat (base c)"

lemma abs_cap_base_size[simp]: 
  "size (abs_cap_base c) = 64"
  by (simp add: wsst_TYs(3))

definition word_cap_base :: "256 word \<Rightarrow> 64 word"
  where
  "word_cap_base w \<equiv> fst ((word_split :: 128 word \<Rightarrow> 64 word \<times> 64 word) 
                        (snd ((word_split :: 256 word \<Rightarrow> 128 word \<times> 128 word) w)))"

definition abs_cap_addr :: "capability \<Rightarrow> 64 word"
  where
  "abs_cap_addr c \<equiv> word_of_nat (addr c)"

lemma abs_cap_addr_size[simp]: 
  "size (abs_cap_addr c) = 64"
  by (simp add: wsst_TYs(3))

definition word_cap_addr :: "256 word \<Rightarrow> 64 word"
  where
  "word_cap_addr w \<equiv> snd ((word_split :: 128 word \<Rightarrow> 64 word \<times> 64 word) 
                        (snd ((word_split :: 256 word \<Rightarrow> 128 word \<times> 128 word) w)))"

(* Because tags do not reside within a concrete capability, bool is sufficient *)
abbreviation abs_cap_tag :: "capability \<Rightarrow> bool"
  where 
  "abs_cap_tag c \<equiv> tag c"

definition encode_cap :: "capability \<Rightarrow> 256 word"
  where
  "encode_cap c \<equiv> (word_rcat :: 128 word list \<Rightarrow> 256 word) [
                      (word_rcat :: 64 word list \<Rightarrow> 128 word) [abs_cap_metadata c, abs_cap_len c], 
                      (word_rcat :: 64 word list \<Rightarrow> 128 word) [abs_cap_base c, abs_cap_addr c]]"

lemma abs_cap_encoded_size[simp]: 
  "size (encode_cap c) = 256"
  by (simp add: wsst_TYs(3))

definition decode_cap :: "bool \<Rightarrow> 256 word \<Rightarrow> capability"
  where
  "decode_cap t c \<equiv> \<lparr> perm_load      = word_cap_perm_load c,
                      perm_cap_load  = word_cap_perm_cap_load c,
                      perm_store     = word_cap_perm_store c, 
                      perm_cap_store = word_cap_perm_cap_store c,
                      base           = unat (word_cap_base c),
                      addr           = unat (word_cap_addr c),
                      len            = unat (word_cap_len c),
                      tag            = t \<rparr>"

context
  notes encode_cap_def[simp] abs_cap_metadata_def[simp] abs_cap_perm_load_def[simp] 
  abs_cap_perm_cap_load_def[simp] abs_cap_perm_store_def[simp] abs_cap_perm_cap_store_def[simp] 
  abs_cap_empty_def[simp] abs_cap_len_def[simp] abs_cap_base_def[simp] abs_cap_addr_def[simp] 
  decode_cap_def[simp] word_cap_perm_load_def[simp] word_cap_perm_store_def[simp] 
  word_cap_perm_cap_load_def[simp] word_cap_perm_cap_store_def[simp] word_cap_base_def[simp] 
  word_cap_len_def[simp] word_cap_addr_def[simp] nth_bit_def[simp]
begin

lemma word_split_cat_eq_tup:
  assumes "w = word_cat w1 w2"
    and "size w1 + size w2 = size w"
  shows "(word_split w :: ('a :: len) word \<times> ('b :: len) word) = (w1, w2)"
  using assms word_split_cat_alt Orderings.order_eq_iff 
  by blast

lemma word_split_cat_eq_fst:
  fixes w1 :: "('a :: len) word"
    and w2 :: "('b :: len) word"
  assumes "w = word_cat w1 w2"
    and "size w1 + size w2 = size w"
  shows "fst (word_split w :: 'a word \<times> 'b word) = w1"
  using word_split_cat_eq_tup assms 
  by fastforce

lemma word_split_cat_eq_snd:
  fixes w1 :: "('a :: len) word"
    and w2 :: "('b :: len) word"
  assumes "w = word_cat w1 w2"
    and "size w1 + size w2 = size w"
  shows "snd (word_split w :: 'a word \<times> 'b word) = w2"
  using word_split_cat_eq_tup assms
  by fastforce

lemmas word_split_cat_eq = word_split_cat_eq_tup word_split_cat_eq_fst word_split_cat_eq_snd

lemma word_rcat_cat_eq:
  fixes w1 w2 :: "('a :: len) word"
  shows "word_rcat [w1, w2] = word_cat w1 w2"
  unfolding word_cat_def word_rcat_def
  by (simp add: push_bit_eq_mult)

lemma word_split_rcat_eq_tup:
  fixes w1 w2:: "('a :: len) word"
  assumes "w = word_rcat [w1, w2]"
    and "size w1 + size w2 = size w"
  shows "(word_split w :: 'a word \<times> 'a word) = (w1, w2)"
  using word_rcat_cat_eq word_split_cat_eq(1) assms
  by blast

lemma word_split_rcat_eq_fst:
  fixes w1 w2:: "('a :: len) word"
  assumes "w = word_rcat [w1, w2]"
    and "size w1 + size w2 = size w"
  shows "fst (word_split w :: 'a word \<times> 'a word) = w1"
  using word_split_rcat_eq_tup assms
  by fastforce

lemma word_split_rcat_eq_snd:
  fixes w1 w2:: "('a :: len) word"
  assumes "w = word_rcat [w1, w2]"
    and "size w1 + size w2 = size w"
  shows "snd (word_split w :: 'a word \<times> 'a word) = w2"
  using word_split_rcat_eq_tup assms
  by fastforce

lemmas word_split_rcat_eq = word_split_rcat_eq_tup word_split_rcat_eq_fst word_split_rcat_eq_snd

lemma 
  fixes w1 w2 :: "('a :: len) word"
  fixes w :: "('b :: len) word"
  assumes "LENGTH('b) = 2 * LENGTH('a)"
  shows "size ((word_rcat [w1, w2]) :: 'b word) = size w1 + size w2"
proof -
  have "size w1 = LENGTH('a)"
    by (simp add: size_word.rep_eq)
  also have "size w2 = LENGTH('a)"
    by (simp add: size_word.rep_eq)
  ultimately obtain "size w1 + size w2 = LENGTH('b)"
    using assms
    by auto
  thus ?thesis 
    by (simp add: word_size)
qed

(* Due to issues with the absence of dependent types, we need to explicitly provide the type
   signatures, namely so that we have exact sizes. *)

lemma cap_split: "(word_split :: 256 word \<Rightarrow> 128 word \<times> 128 word)
                       ((word_rcat :: 128 word list \<Rightarrow> 256 word)
                         [(word_rcat :: 64 word list \<Rightarrow> 128 word) [(word_cat :: 60 word \<Rightarrow> 4 word \<Rightarrow> 64 word) 0 ((word_rcat :: 1 word list \<Rightarrow> 4 word) ls), word_of_nat (len c)],
                          (word_rcat :: 64 word list \<Rightarrow> 128 word) [word_of_nat (base c), word_of_nat (addr c)]]) = 
                    ((word_rcat :: 64 word list \<Rightarrow> 128 word) [(word_cat :: 60 word \<Rightarrow> 4 word \<Rightarrow> 64 word) 0 ((word_rcat :: 1 word list \<Rightarrow> 4 word) ls), word_of_nat (len c)],
                          (word_rcat :: 64 word list \<Rightarrow> 128 word) [word_of_nat (base c), word_of_nat (addr c)])"
  by (simp add: word_rcat_cat_eq word_split_cat_eq wsst_TYs(3))

lemma cap_hi_split: "(word_split :: 128 word \<Rightarrow> 64 word \<times> 64 word) (
                        (word_rcat :: 64 word list \<Rightarrow> 128 word) [(word_cat :: 60 word \<Rightarrow> 4 word \<Rightarrow> 64 word) 0 ((word_rcat :: 1 word list \<Rightarrow> 4 word) ls), word_of_nat (len c)])
                    = ((word_cat :: 60 word \<Rightarrow> 4 word \<Rightarrow> 64 word) 0 ((word_rcat :: 1 word list \<Rightarrow> 4 word) ls), word_of_nat (len c))"
  by (simp add: word_rcat_cat_eq word_split_cat_eq wsst_TYs(3))

lemma cap_lo_split: "(word_split :: 128 word \<Rightarrow> 64 word \<times> 64 word) (
                       (word_rcat :: 64 word list \<Rightarrow> 128 word) [word_of_nat (base c), word_of_nat (addr c)])
                   = (word_of_nat (base c), word_of_nat (addr c))"
  by (simp add: word_rcat_cat_eq word_split_cat_eq wsst_TYs(3))

lemma cap_abs_conc_addr:
  assumes "addr c \<le> 2 ^ LENGTH(64) - 1"
  shows "addr c = unat (word_cap_addr (encode_cap c))"
  by (clarsimp; safe; simp_all add: cap_split cap_lo_split; insert assms; simp add: unat_of_nat_len)+

lemma cap_abs_conc_base:
  assumes "base c \<le> 2 ^ LENGTH(64) - 1"
  shows "base c = unat (word_cap_base (encode_cap c))"
  by (clarsimp; safe; simp_all add: cap_split cap_lo_split; insert assms; simp add: unat_of_nat_len)+

lemma cap_abs_conc_len:
  assumes "len c \<le> 2 ^ LENGTH(64) - 1"
  shows "len c = unat (word_cap_len (encode_cap c))"
  by (clarsimp; safe; simp_all add: cap_split cap_hi_split; insert assms; simp add: unat_of_nat_len)+

lemma bit_exhaustive:
  fixes b :: "1 word"
  shows "b = 0 \<or> b = 1"
proof (cases "b")
  case (1 n)
  then show ?thesis 
    using degenerate_word len_num1 by blast 
qed

method word_4_cases =
  (case_tac "n = 0",
   case_tac "na = 0",
   case_tac "nb = 0",
   simp,
   subgoal_tac "nb = 1",
   simp,
   simp,
   subgoal_tac "na = 1",
   case_tac "nb = 0",
   simp,
   subgoal_tac "nb = 1",
   simp,
   simp,
   simp,
   subgoal_tac "n = 1",
   case_tac "na = 0",
   case_tac "nb = 0",
   simp,
   subgoal_tac "nb = 1",
   simp,
   simp,
   subgoal_tac "na = 1",
   case_tac "nb = 0",
   simp,
   subgoal_tac "nb = 1",
   simp,
   simp,
   simp,
   simp)

lemma cast_same:
  "unat (x :: 4 word) = unat (UCAST(4 \<rightarrow> 64) x)"
  apply (cases x, clarsimp)
  apply (case_tac "n = 0", simp)
  apply (case_tac "n = 1", simp)
  apply (case_tac "n = 2", simp)
  apply (case_tac "n = 3", simp)
  apply (case_tac "n = 4", simp)
  apply (case_tac "n = 5", simp)
  apply (case_tac "n = 6", simp)
  apply (case_tac "n = 7", simp)
  apply (case_tac "n = 8", simp)
  apply (case_tac "n = 9", simp)
  apply (case_tac "n = 10", simp)
  apply (case_tac "n = 11", simp)
  apply (case_tac "n = 12", simp)
  apply (case_tac "n = 13", simp)
  apply (case_tac "n = 14", simp)
  apply (case_tac "n = 15", simp) 
  apply presburger
  done

lemma same_val: 
  "unat ((word_rcat :: 1 word list \<Rightarrow> 4 word) [b1, b2, b3 ,b4]) = unat w \<longleftrightarrow>
   unat ((word_cat :: 60 word \<Rightarrow> 4 word \<Rightarrow> 64 word) abs_cap_empty ((word_rcat :: 1 word list \<Rightarrow> 4 word) [b1, b2, b3, b4])) = unat w"
  unfolding word_rcat_def word_cat_def
  using cast_same 
  by simp

lemma perm_case_1:
  assumes "b1 = 0"
  shows "8 && word_cat abs_cap_empty ((word_rcat :: 1 word list \<Rightarrow> 4 word) [b1, b2, b3, b4]) = 0"
  unfolding word_rcat_def abs_cap_empty_def word_cat_def
  apply (insert assms)
  apply (cases b2; clarsimp)
  apply (cases b3; clarsimp)
  apply (cases b4; clarsimp)
  apply (word_4_cases)
  done

lemma perm_case_2:
  assumes "b1 = 1"
  shows "8 && (word_cat :: 60 word \<Rightarrow> 4 word \<Rightarrow> 64 word) abs_cap_empty ((word_rcat :: 1 word list \<Rightarrow> 4 word) [b1, b2, b3, b4]) \<noteq> 0"
  unfolding word_rcat_def abs_cap_empty_def word_cat_def
  apply (insert assms)
  apply (cases b2; clarsimp)
  apply (cases b3; clarsimp)
  apply (cases b4; clarsimp)
  apply (word_4_cases)
  done

lemma perm_case_3:
  assumes "b2 = 0"
  shows "4 && word_cat abs_cap_empty ((word_rcat :: 1 word list \<Rightarrow> 4 word) [b1, b2, b3, b4]) = 0"
  unfolding word_rcat_def abs_cap_empty_def word_cat_def
  apply (insert assms)
  apply (cases b1; clarsimp)
  apply (cases b3; clarsimp)
  apply (cases b4; clarsimp)
  apply (word_4_cases)
  done

lemma perm_case_4:
  assumes "b2 = 1"
  shows "4 && (word_cat :: 60 word \<Rightarrow> 4 word \<Rightarrow> 64 word) abs_cap_empty ((word_rcat :: 1 word list \<Rightarrow> 4 word) [b1, b2, b3, b4]) \<noteq> 0"
  unfolding word_rcat_def abs_cap_empty_def word_cat_def
  apply (insert assms)
  apply (cases b1; clarsimp)
  apply (cases b3; clarsimp)
  apply (cases b4; clarsimp)
  apply (word_4_cases)
  done

lemma perm_case_5:
  assumes "b3 = 0"
  shows "2 && word_cat abs_cap_empty ((word_rcat :: 1 word list \<Rightarrow> 4 word) [b1, b2, b3, b4]) = 0"
  unfolding word_rcat_def abs_cap_empty_def word_cat_def
  apply (insert assms)
  apply (cases b1; clarsimp)
  apply (cases b2; clarsimp)
  apply (cases b4; clarsimp)
  apply (word_4_cases)
  done

lemma perm_case_6:
  assumes "b3 = 1"
  shows "2 && (word_cat :: 60 word \<Rightarrow> 4 word \<Rightarrow> 64 word) abs_cap_empty ((word_rcat :: 1 word list \<Rightarrow> 4 word) [b1, b2, b3, b4]) \<noteq> 0"
  unfolding word_rcat_def abs_cap_empty_def word_cat_def
  apply (insert assms)
  apply (cases b1; clarsimp)
  apply (cases b2; clarsimp)
  apply (cases b4; clarsimp)
  apply (word_4_cases)
  done

lemma perm_case_7:
  assumes "b4 = 0"
  shows "1 && word_cat abs_cap_empty ((word_rcat :: 1 word list \<Rightarrow> 4 word) [b1, b2, b3, b4]) = 0"
  unfolding word_rcat_def abs_cap_empty_def word_cat_def
  apply (insert assms)
  apply (cases b1; clarsimp)
  apply (cases b2; clarsimp)
  apply (cases b3; clarsimp)
  apply (word_4_cases)
  done

lemma perm_case_8:
  assumes "b4 = 1"
  shows "1 && (word_cat :: 60 word \<Rightarrow> 4 word \<Rightarrow> 64 word) abs_cap_empty ((word_rcat :: 1 word list \<Rightarrow> 4 word) [b1, b2, b3, b4]) \<noteq> 0"
  unfolding word_rcat_def abs_cap_empty_def word_cat_def
  apply (insert assms)
  apply (cases b1; clarsimp)
  apply (cases b2; clarsimp)
  apply (cases b3; clarsimp)
  apply (word_4_cases)
  done

lemmas perm_cases = 
  perm_case_1 perm_case_2 perm_case_3 perm_case_4 perm_case_5 perm_case_6 perm_case_7 perm_case_8

lemma cap_abs_conc_perm_load:
  "perm_load c = word_cap_perm_load (encode_cap c)"
  by (insert perm_cases, clarsimp, safe, simp_all add: cap_split cap_hi_split, blast+)

lemma cap_abs_conc_perm_cap_load:
  "perm_cap_load c = word_cap_perm_cap_load (encode_cap c)"
  by (insert perm_cases, clarsimp, safe, simp_all add: cap_split cap_hi_split, fastforce+)

lemma cap_abs_conc_perm_store:
  "perm_store c = word_cap_perm_store (encode_cap c)"
  by (insert perm_cases, clarsimp, safe, simp_all add: cap_split cap_hi_split, fastforce+)

lemma cap_abs_conc_perm_cap_store:
  "perm_cap_store c = word_cap_perm_cap_store (encode_cap c)"
  by (insert perm_cases, clarsimp, safe, simp_all add: cap_split cap_hi_split, fastforce+)

lemmas cap_abs_conc_perms =
  cap_abs_conc_perm_load
  cap_abs_conc_perm_cap_load
  cap_abs_conc_perm_store
  cap_abs_conc_perm_cap_store

lemma decode_encode_iso:
  fixes c :: "capability"
  assumes "addr c \<le> 2 ^ LENGTH(64) - 1"
  assumes "base c \<le> 2 ^ LENGTH(64) - 1"
  assumes "len c \<le> 2 ^ LENGTH(64) - 1"
  assumes "abs_cap_tag c = t"
  shows "c = decode_cap t (encode_cap c)"
  unfolding decode_cap_def
  by (cases rule: capability.cases, smt (z3) assms cap_abs_conc_addr cap_abs_conc_base 
      cap_abs_conc_len cap_abs_conc_perm_cap_load cap_abs_conc_perm_cap_store cap_abs_conc_perm_load 
      cap_abs_conc_perm_store capability.surjective old.unit.exhaust)

(* We define weak equivalence between concrete capabilities to ensure only relevant regions are compared *)

definition cap_canon_eq :: "256 word \<Rightarrow> 256 word \<Rightarrow> bool" (infix "\<simeq>\<^sub>C" 60)
  where
  "cap_canon_eq c1 c2 \<equiv> word_cap_perm_load c1 = word_cap_perm_load c2
                      \<and> word_cap_perm_cap_load c1 = word_cap_perm_cap_load c2
                      \<and> word_cap_perm_store c1 = word_cap_perm_store c2
                      \<and> word_cap_perm_cap_store c1 = word_cap_perm_cap_store c2
                      \<and> word_cap_len c1 = word_cap_len c2
                      \<and> word_cap_base c1 = word_cap_base c2
                      \<and> word_cap_addr c1 = word_cap_addr c2"

(* Equivalence proof *)

lemma cap_canon_eq_refl: 
  "reflp (\<simeq>\<^sub>C)"
  unfolding reflp_def cap_canon_eq_def
  by blast

lemma cap_canon_eq_symm:
   "symp (\<simeq>\<^sub>C)"
  unfolding symp_def cap_canon_eq_def
  by presburger

lemma cap_canon_eq_trans:
  "transp (\<simeq>\<^sub>C)"
  unfolding transp_def cap_canon_eq_def 
  by presburger

corollary cap_canon_eq_equiv_rel: 
  "equivp (\<simeq>\<^sub>C)"
  using cap_canon_eq_refl cap_canon_eq_symm cap_canon_eq_trans equivpI
  by blast

lemma encode_decode_iso:
  "c \<simeq>\<^sub>C encode_cap (decode_cap t c)"
  by (smt (z3) abs_cap_addr_def abs_cap_base_def abs_cap_empty_def abs_cap_len_def 
      abs_cap_metadata_def cap_abs_conc_perm_cap_load cap_abs_conc_perm_cap_store 
      cap_abs_conc_perm_load cap_abs_conc_perm_store cap_canon_eq_def cap_hi_split cap_lo_split 
      cap_split capability.select_convs(1) decode_cap_def encode_cap_def fst_conv 
      pre_capability.select_convs(1) pre_capability.select_convs(2) pre_capability.select_convs(3) 
      pre_capability.select_convs(4) pre_capability.select_convs(5) pre_capability.select_convs(6) 
      snd_conv word_cap_addr_def word_cap_base_def word_cap_len_def word_unat.Rep_inverse)


(* Finally, we convert between 256 word and 8 word list for storage purposes *)
abbreviation encode_cap_store :: "256 word \<Rightarrow> 8 word list"
  where
  "encode_cap_store c \<equiv> (word_rsplit :: 256 word \<Rightarrow> 8 word list) c"

abbreviation decode_cap_store :: "8 word list \<Rightarrow> 256 word"
  where
  "decode_cap_store c \<equiv> (word_rcat :: 8 word list \<Rightarrow> 256 word) c"

lemma rsplit_rcat_eq:
  assumes "LENGTH(('b::len)) mod LENGTH(('a::len)) = 0"
    and "length w = LENGTH('b) div LENGTH('a)"
  shows "(word_rsplit :: 'b word \<Rightarrow> 'a word list) ((word_rcat :: 'a word list \<Rightarrow> 'b word) w) = w"
  by (simp add: assms(1) assms(2) mod_0_imp_dvd size_word.rep_eq word_rsplit_rcat_size)

lemma encode_decode_cap_store_eq:
  assumes "length w = 32"
  shows "w = encode_cap_store (decode_cap_store w)"
  using assms rsplit_rcat_eq[symmetric]
  by force

lemma decode_encode_cap_store_eq:
  "decode_cap_store (encode_cap_store c) = c"
  by (blast intro: word_rcat_rsplit)

lemma decode_encode_cap_and_store_eq:
  fixes c :: "capability"
  assumes "addr c \<le> 2 ^ LENGTH(64) - 1"
  assumes "base c \<le> 2 ^ LENGTH(64) - 1"
  assumes "len c \<le> 2 ^ LENGTH(64) - 1"
  assumes "abs_cap_tag c = t"
  shows "decode_cap t (decode_cap_store (encode_cap_store (encode_cap c))) = c"
  using assms 
  by (metis decode_encode_cap_store_eq decode_encode_iso)

end

end
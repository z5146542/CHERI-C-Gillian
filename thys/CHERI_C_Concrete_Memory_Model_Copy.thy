theory CHERI_C_Concrete_Memory_Model
  imports "More_Word_Library"
          "Separation_Algebra.Separation_Algebra"
          "Containers.Containers"
          "HOL-Library.Mapping"
          "HOL-Library.Code_Target_Numeral"
          (*"HOL-Imperative_HOL.Imperative_HOL"*)
begin

\<comment> \<open>These are coprocessor 2 excessptions thrown by the hardware. 
    BadAddressViolation is not a coprocessor 2 exception but remains one given by the hardware. \<close>
datatype c2errtype = 
  TagViolation
  | PermitLoadViolation
  | PermitStoreViolation
  | PermitStoreCapViolation
  | PermitStoreLocalCapViolation
  | LengthViolation
  | BadAddressViolation

\<comment> \<open>These are logical errors produced by the language. In practice, Some of these errors would never
    be caught due to the inherent spatial safety guarantees given by capabilities. \<close>
datatype logicerrtype =
  UseAfterFree
  | BufferOverrun
  | MissingResource
  | WrongMemVal
  | MemoryNotFreed
  | Unhandled "String.literal"

datatype errtype = 
  C2Err c2errtype
  | LogicErr logicerrtype

datatype 'a result =
  Success (res: 'a)
  | Error (err: errtype)

text \<open>In this theory, we concretise the notion of blocks\<close>
\<comment> \<open>While we can use int as blocks, integer makes it more efficient for code execution\<close>
type_synonym block = integer
type_synonym memcap = "block mem_capability"
type_synonym cap = "block capability"

text \<open>Because sizeof depends on the architeture, it shall be given via the memory model\<close>
definition sizeof :: "cctype \<Rightarrow> nat" ("|_|\<^sub>\<tau>")
  where
  "sizeof \<tau> \<equiv> case \<tau> of
     Uint8  \<Rightarrow> 1
   | Sint8  \<Rightarrow> 1
   | Uint16 \<Rightarrow> 2
   | Sint16 \<Rightarrow> 2
   | Uint32 \<Rightarrow> 4
   | Sint32 \<Rightarrow> 4
   | Uint64 \<Rightarrow> 8
   | Sint64 \<Rightarrow> 8
   | Cap \<Rightarrow> 32"

lemma size_type_align:
  "|t|\<^sub>\<tau> = x \<Longrightarrow> \<exists> n. 2 ^ n = x"
  apply (simp add: sizeof_def split: cctype.split_asm)
          apply fastforce+
        apply (rule_tac x=1 in exI, fastforce)
       apply (rule_tac x=1 in exI, fastforce)
      apply (rule_tac x=2 in exI, fastforce)
     apply (rule_tac x=2 in exI, fastforce)
    apply (rule_tac x=3 in exI, fastforce)
   apply (rule_tac x=3 in exI, fastforce)
  apply (rule_tac x=5 in exI, fastforce)
  done

lemma memval_size_u8:
  "|memval_type (Uint8_v v)|\<^sub>\<tau> = 1"
  unfolding sizeof_def 
  by fastforce

lemma memval_size_s8:
  "|memval_type (Sint8_v v)|\<^sub>\<tau> = 1"
  unfolding sizeof_def 
  by fastforce

lemma memval_size_u16:
  "|memval_type (Uint16_v v)|\<^sub>\<tau> = 2"
  unfolding sizeof_def 
  by fastforce

lemma memval_size_s16:
  "|memval_type (Sint16_v v)|\<^sub>\<tau> = 2"
  unfolding sizeof_def 
  by fastforce

lemma memval_size_u32:
  "|memval_type (Uint32_v v)|\<^sub>\<tau> = 4"
  unfolding sizeof_def 
  by fastforce

lemma memval_size_s32:
  "|memval_type (Sint32_v v)|\<^sub>\<tau> = 4"
  unfolding sizeof_def 
  by fastforce

lemma memval_size_u64:
  "|memval_type (Uint64_v v)|\<^sub>\<tau> = 8"
  unfolding sizeof_def 
  by fastforce

lemma memval_size_s64:
  "|memval_type (Sint64_v v)|\<^sub>\<tau> = 8"
  unfolding sizeof_def 
  by fastforce

lemma memval_size_cap:
  "|memval_type (Cap_v v)|\<^sub>\<tau> = 32"
  unfolding sizeof_def 
  by fastforce

corollary memval_size_u16_eq_word_split_len:
  assumes "val = Uint16_v v"
  shows "|memval_type val|\<^sub>\<tau> = length (u16_split v)"
  using assms memval_size_u16 u16_split_length 
  by force

corollary memval_size_s16_eq_word_split_len:
  assumes "val = Sint16_v v"
  shows "|memval_type val|\<^sub>\<tau> = length (s16_split v)"
  using assms memval_size_s16 flatten_s16_length 
  by force

corollary memval_size_u32_eq_word_split_len:
  assumes "val = Uint32_v v"
  shows "|memval_type val|\<^sub>\<tau> = length (flatten_u32 v)"
  using assms memval_size_u32 flatten_u32_length
  by force

corollary memval_size_s32_eq_word_split_len:
  assumes "val = Sint32_v v"
  shows "|memval_type val|\<^sub>\<tau> = length (flatten_s32 v)"
  using assms memval_size_s32 flatten_s32_length 
  by force

corollary memval_size_u64_eq_word_split_len:
  assumes "val = Uint64_v v"
  shows "|memval_type val|\<^sub>\<tau> = length (flatten_u64 v)"
  using assms memval_size_u64 flatten_u64_length
  by force

corollary memval_size_s64_eq_word_split_len:
  assumes "val = Sint64_v v"
  shows "|memval_type val|\<^sub>\<tau> = length (flatten_s64 v)"
  using assms memval_size_s64 flatten_s64_length 
  by force

lemma sizeof_nonzero:
  "|t|\<^sub>\<tau> > 0"
  by (simp add: sizeof_def split: cctype.split)

instance int :: comp_countable ..

lemma integer_encode_eq: "(int_encode \<circ> int_of_integer) x = (int_encode \<circ> int_of_integer) y \<longleftrightarrow> x = y"
  using int_encode_eq integer_eq_iff 
  by auto

instance integer :: countable
  by (rule countable_classI[of "int_encode \<circ> int_of_integer"]) (simp only: integer_encode_eq)

instance integer :: comp_countable ..

datatype memval =
  Byte (of_byte: "8 word")
  | ACap (of_cap: "memcap") (of_nth: "nat")

definition memval_is_byte :: "memval \<Rightarrow> bool"
  where
  "memval_is_byte m \<equiv> case m of Byte _ \<Rightarrow> True | ACap _ _ \<Rightarrow> False"

abbreviation memval_is_cap :: "memval \<Rightarrow> bool"
  where
  "memval_is_cap m \<equiv> \<not> memval_is_byte m"

lemma memval_byte:
  "memval_is_byte m \<Longrightarrow> \<exists> b. m = Byte b"
  by (simp add: memval_is_byte_def split: memval.split_asm)

lemma memval_byte_not_memcap:
  "memval_is_byte m \<Longrightarrow> m \<noteq> ACap c n"
  by (simp add: memval_is_byte_def split: memval.split_asm)

lemma memval_memcap:
  "memval_is_cap m \<Longrightarrow> \<exists> c n. m = ACap c n"
  by (simp add: memval_is_byte_def split: memval.split_asm)

lemma memval_memcap_not_byte:
  "memval_is_cap m \<Longrightarrow> m \<noteq> Byte b"
  by (simp add: memval_is_byte_def split: memval.split_asm)

record object =
  bounds :: "nat \<times> nat"
  content :: "(nat, memval) mapping"
  tags :: "(nat, bool) mapping"

datatype t = 
  Freed
  | Map (the_map: "object")

record heap =
  next_block :: "block"
  heap_map :: "(block, t) mapping"

section \<open>Proving heap is an instance of a separation algebra\<close>

instantiation unit :: cancellative_sep_algebra
begin
definition "0 \<equiv> ()"
definition "u1 + u2 = ()"
definition "(u1::unit) ## u2 \<equiv> True"
instance 
  by (standard; (blast | simp add: sep_disj_unit_def))
end

instantiation nat :: cancellative_sep_algebra
begin
definition "(n1::nat) ## n2 \<equiv> True"
instance 
  by (standard; (blast | simp add: sep_disj_nat_def))
end

instantiation mapping :: (type, type) cancellative_sep_algebra
begin

definition zero_map_def: "0 \<equiv> Mapping.empty"
definition plus_map_def: "m1 + m2 \<equiv> Mapping ((Mapping.lookup m1) ++ (Mapping.lookup m2))"
definition sep_disj_map_def: "m1 ## m2 \<equiv> Mapping.keys m1 \<inter> Mapping.keys m2 = {}"

instance
  apply standard
         apply (simp add: sep_disj_map_def Mapping.keys_def zero_map_def) 
         apply (metis Mapping.keys.rep_eq Mapping.keys_empty inf_bot_right)
        apply (simp add: sep_disj_map_def Mapping.keys_def zero_map_def)
        apply (simp add: inf_commute)
       apply (simp add: sep_disj_map_def Mapping.keys_def zero_map_def plus_map_def) 
       apply (metis Mapping.empty_def Mapping.lookup.abs_eq map_add_empty rep_inverse)
      apply (simp add: sep_disj_map_def Mapping.keys_def zero_map_def plus_map_def Mapping.lookup_def map_add_comm)
      apply (fastforce dest: map_add_comm)
     apply (simp add: sep_disj_map_def Mapping.keys_def zero_map_def plus_map_def Mapping.lookup_def map_add_comm)
     apply (simp add: Mapping_inverse)
    apply (simp add: sep_disj_map_def Mapping.keys_def zero_map_def plus_map_def Mapping.lookup_def map_add_comm) 
    apply (metis (no_types, opaque_lifting) Mapping.keys.abs_eq Mapping.keys.rep_eq disjoint_iff domIff map_add_dom_app_simps(3))
   apply (simp add: sep_disj_map_def Mapping.keys_def zero_map_def plus_map_def Mapping.lookup_def map_add_comm)
  apply (simp add: Mapping_inverse inf_commute inf_sup_distrib1)
  apply (simp add: plus_map_def sep_disj_map_def)
  apply (metis (mono_tags, opaque_lifting) Mapping.keys.abs_eq Mapping.lookup.abs_eq disjoint_iff domIff map_add_dom_app_simps(3) mapping_eqI)
  done
end

instantiation heap_ext :: (cancellative_sep_algebra) cancellative_sep_algebra 
begin
definition "0 :: 'a heap_scheme \<equiv> \<lparr> next_block = 0, heap_map = Mapping.empty, \<dots> = 0 \<rparr>"
definition "(m1 :: 'a heap_scheme) + (m2 :: 'a heap_scheme) \<equiv> 
              \<lparr> next_block = next_block m1 + next_block m2,
                heap_map = Mapping ((Mapping.lookup (heap_map m1)) ++ (Mapping.lookup (heap_map m2))), 
                \<dots> = heap.more m1 + heap.more m2 \<rparr>" 
definition "(m1 :: 'a heap_scheme) ## (m2 :: 'a heap_scheme) \<equiv> 
              Mapping.keys (heap_map m1) \<inter> Mapping.keys (heap_map m2) = {}
              \<and> heap.more m1 ## heap.more m2" 
instance 
  apply standard
         apply (unfold plus_heap_ext_def sep_disj_heap_ext_def zero_heap_ext_def)
         apply force 
        apply (simp add: inf_commute sep_disj_commute)
       apply (simp add: Mapping.empty_def Mapping.lookup.abs_eq, simp add: Mapping.lookup.rep_eq rep_inverse)
      apply (metis add.commute keys_dom_lookup map_add_comm sep_add_commute)
     apply (simp add: Mapping.lookup.abs_eq sep_add_assoc)
    apply (metis heap.select_convs(2) heap.select_convs(3) plus_map_def sep_disj_addD sep_disj_map_def)
   apply (simp add: Mapping.lookup.abs_eq disjoint_iff keys_dom_lookup sep_disj_addI1)
  apply (metis add_right_cancel heap.equality heap.ext_inject plus_map_def sep_add_cancel sep_disj_map_def)
  done
end

instantiation mem_capability_ext :: (comp_countable, zero) zero
begin
definition "0 :: ('a, 'b) mem_capability_scheme \<equiv> 
          \<lparr> block_id = 0, 
            offset = 0,
            base = 0, 
            len = 0, 
            perm_load = False, 
            perm_cap_load = False, 
            perm_store = False,
            perm_cap_store = False,
            perm_cap_store_local = False,
            perm_global = False,
            \<dots> =  0\<rparr>"
instance ..
end

subclass (in comp_countable) zero .

instantiation capability_ext :: (zero) zero
begin
definition "0 \<equiv> \<lparr> tag = False, \<dots> = 0\<rparr>"
instance ..
end

\<comment> \<open>Section 4.5 of CHERI C/C++ Programming Guide defines what a NULL capability is.\<close>
definition null_capability :: "cap" ("NULL")
  where
  "NULL \<equiv> 0"

context 
  notes null_capability_def[simp]
begin

lemma null_capability_block_id[simp]: 
  "block_id NULL = 0"
  by (simp add: zero_mem_capability_ext_def) 

lemma null_capability_offset[simp]:
  "offset NULL = 0"
  by (simp add: zero_mem_capability_ext_def)

lemma null_capability_base[simp]:
  "base NULL = 0"
  by (simp add: zero_mem_capability_ext_def)

lemma null_capability_len[simp]:
  "len NULL = 0"
  by (simp add: zero_mem_capability_ext_def)

lemma null_capability_perm_load[simp]:
  "perm_load NULL = False"
  by (simp add: zero_mem_capability_ext_def)

lemma null_capability_perm_cap_load[simp]:
  "perm_cap_load NULL = False"
  by (simp add: zero_mem_capability_ext_def)

lemma null_capability_perm_store[simp]:
  "perm_store NULL = False"
  by (simp add: zero_mem_capability_ext_def)

lemma null_capability_perm_cap_store[simp]:
  "perm_cap_store NULL = False"
  by (simp add: zero_mem_capability_ext_def)

lemma null_capability_perm_cap_store_local[simp]:
  "perm_cap_store_local NULL = False"
  by (simp add: zero_mem_capability_ext_def)

lemma null_capability_tag[simp]:
  "tag NULL = False"
  by (simp add: zero_capability_ext_def zero_mem_capability_ext_def)

end

\<comment> \<open>Note that the starting block is 1, as 0 loosely refers to the null capability\<close>
definition init_heap :: "heap"
  where
  "init_heap \<equiv> 0 \<lparr> next_block := 1 \<rparr>"

definition alloc :: "heap \<Rightarrow> bool \<Rightarrow> nat \<Rightarrow> (heap \<times> cap) result"
  where
  "alloc h c s \<equiv> 
     let cap = \<lparr> block_id = (next_block h),
                 offset = 0,
                 base = 0,
                 len = s,
                 perm_load = True,
                 perm_cap_load = c,
                 perm_store = True,
                 perm_cap_store = c,
                 perm_cap_store_local = c,
                 perm_global = False,
                 tag = True
               \<rparr> in
     let h' = h \<lparr> next_block := (next_block h) + 1,
                  heap_map := Mapping.update 
                                (next_block h) 
                                (Map \<lparr> bounds = (0, s), 
                                       content = Mapping.empty, 
                                       tags = Mapping.empty 
                                     \<rparr>
                                 ) (heap_map h)
                \<rparr> in
     Success (h', cap)"

lemma alloc_always_success:
  "\<exists>! res. alloc h c s = Success res"
  by (simp add: alloc_def)

schematic_goal alloc_updated_heap_and_cap:
  "alloc h c s = Success (?h', ?cap)"
  by (fastforce simp add: alloc_def)

lemma alloc_never_fails:
  "alloc h c s = Error e \<Longrightarrow> False"
  by (simp add: alloc_def)

\<comment> \<open>In practice, malloc may actually return NULL when allocation fails. However, this still complies
    with The C Standard.\<close>
lemma alloc_no_null_ret:
  assumes "alloc h c s = Success (h', cap)"
  shows "cap \<noteq> NULL"
proof -
  have "perm_load cap"
    using assms alloc_def
    by force
  moreover have "\<not> perm_load NULL"
    unfolding null_capability_def zero_capability_ext_def zero_mem_capability_ext_def
    by force
  ultimately show ?thesis 
    by blast
qed

lemma alloc_correct:
  assumes "alloc h c s = Success (h', cap)"
  shows "next_block h' = next_block h + 1"
    and "Mapping.lookup (heap_map h') (next_block h) 
         = Some (Map \<lparr> bounds = (0, s), content = Mapping.empty, tags = Mapping.empty\<rparr>)"
  using assms alloc_def
  by auto

definition free :: "heap \<Rightarrow> cap \<Rightarrow> (heap \<times> cap) result"
  where
  "free h c \<equiv>
     if c = NULL then Success (h, c) else
     if tag c = False then Error (C2Err (TagViolation)) else
     if perm_global c = True then Error (LogicErr (Unhandled 0)) else
     let obj = Mapping.lookup (heap_map h) (block_id c) in
     (case obj of None      \<Rightarrow> Error (LogicErr (MissingResource))
               | Some cobj \<Rightarrow>
       (case cobj of Freed \<Rightarrow> Error (LogicErr (UseAfterFree))
                  | Map m \<Rightarrow>
         if offset c \<noteq> 0 then Error (LogicErr (Unhandled 0)) 
         else
       let cap_bound = (base c, base c + len c) in
       if cap_bound \<noteq> bounds m then Error (LogicErr (Unhandled 0)) else
       let h' = h \<lparr> heap_map := Mapping.update (block_id c) Freed (heap_map h) \<rparr> in 
       let cap = c \<lparr> tag := False \<rparr> in
       Success (h', cap)))"

\<comment> \<open>Section 7.20.3.2 of The C Standard states free(NULL) results in no action occuring.\<close>
lemma free_null:
  "free h NULL = Success (h, NULL)"
  by (simp add: free_def)

lemma free_false_tag:
  assumes "c \<noteq> NULL"
    and "tag c = False"
  shows "free h c = Error (C2Err (TagViolation))"
  by (presburger add: assms free_def)

lemma free_global_cap:
  assumes "c \<noteq> NULL"
    and "tag c = True"
    and "perm_global c = True"
  shows "free h c = Error (LogicErr (Unhandled 0))"
  by (presburger add: assms free_def)

lemma free_nonexistant_obj:
  assumes "c \<noteq> NULL"
    and "tag c = True"
    and "perm_global c = False"
    and "Mapping.lookup (heap_map h) (block_id c) = None"
  shows "free h c = Error (LogicErr (MissingResource))"
  using assms free_def
  by auto

text \<open>This case may arise if there are copies of the same capability, where only one was freed.
      It is worth noting that due to this, temporal safety is not guaranteed.\<close>
lemma free_double_free:
  assumes "c \<noteq> NULL"
    and "tag c = True"
    and "perm_global c = False"
    and "Mapping.lookup (heap_map h) (block_id c) = Some Freed"
  shows "free h c = Error (LogicErr (UseAfterFree))"
  using free_def assms
  by force

\<comment> \<open>An incorrect offset implies the actual ptr value is not that returned by alloc.
    Section 7.20.3.2 of The C Standard states this leads to undefined behaviour.
    Clang, in practice, however, terminates the C program with an invalid pointer error. \<close>
lemma free_incorrect_cap_offset:
  assumes "c \<noteq> NULL"
    and "tag c = True"
    and "perm_global c = False"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "offset c \<noteq> 0"
  shows "free h c = Error (LogicErr (Unhandled 0))"
  using free_def assms
  by force

(* This I don't know if the check is necessary. *)
lemma free_incorrect_bounds:
  assumes "c \<noteq> NULL"
    and "tag c = True"
    and "perm_global c = False"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "offset c = 0"
    and "bounds m \<noteq> (base c, base c + len c)"
  shows "free h c = Error (LogicErr (Unhandled 0))"
  unfolding free_def
  using assms 
  by force

lemma free_non_null_correct:
  assumes "c \<noteq> NULL"
    and valid_tag: "tag c = True"
    and "perm_global c = False"
    and map_has_contents: "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and offset_correct: "offset c = 0"
    and bounds_correct: "bounds m = (base c, base c + len c)"
  shows "free h c = Success (h \<lparr> heap_map := Mapping.update (block_id c) Freed (heap_map h) \<rparr>, 
                             c \<lparr> tag := False \<rparr>)"
  unfolding free_def 
  using assms
  by simp

lemma free_cond:
  assumes "free h c = Success (h', cap)"
  shows "c \<noteq> NULL \<Longrightarrow> tag c = True"
    and "c \<noteq> NULL \<Longrightarrow> perm_global c = False"
    and "c \<noteq> NULL \<Longrightarrow> offset c = 0"
    and "c \<noteq> NULL \<Longrightarrow> \<exists> m. Mapping.lookup (heap_map h) (block_id c) = Some (Map m) \<and> 
              bounds m = (base c, base c + len c)"
    and "c \<noteq> NULL \<Longrightarrow> Mapping.lookup (heap_map h') (block_id c) = Some Freed"
    and "c \<noteq> NULL \<Longrightarrow> cap = c \<lparr> tag := False \<rparr>"
    and "c = NULL \<Longrightarrow> (h, c) = (h', cap)"
proof -
  assume "c \<noteq> NULL"
  thus "tag c = True"
    using assms unfolding free_def
    by (meson result.simps(4))
next
  assume "c \<noteq> NULL"
  thus "perm_global c = False"
    using assms unfolding free_def
    by (meson result.simps(4))
next
  assume "c \<noteq> NULL"
  thus "offset c = 0"
    using assms unfolding free_def
    by (smt (verit, ccfv_SIG) not_None_eq option.simps(4) option.simps(5) 
        result.distinct(1) t.exhaust t.simps(4) t.simps(5))
next
  assume "c \<noteq> NULL"
  thus "\<exists> m. Mapping.lookup (heap_map h) (block_id c) = Some (Map m) \<and> 
             bounds m = (base c, base c + len c)"
    using assms unfolding free_def
    by (metis assms free_double_free free_incorrect_bounds free_incorrect_cap_offset 
        free_nonexistant_obj not_Some_eq result.distinct(1) t.exhaust)
next 
  assume "c \<noteq> NULL"
  hence "h' = h \<lparr> heap_map := Mapping.update (block_id c) Freed (heap_map h) \<rparr>"
    using assms unfolding free_def
    by (smt (verit, ccfv_SIG) free_nonexistant_obj not_Some_eq option.simps(4) option.simps(5) 
        prod.inject result.distinct(1) result.exhaust result.inject(1) t.exhaust t.simps(4) t.simps(5))
  thus "Mapping.lookup (heap_map h') (block_id c) = Some Freed"
    by fastforce
next
  assume "c \<noteq> NULL"
  thus "cap = c \<lparr> tag := False \<rparr>"
    using assms unfolding free_def
    by (smt (verit, ccfv_SIG) not_Some_eq option.simps(4) option.simps(5) prod.inject 
        result.distinct(1) result.inject(1) t.exhaust t.simps(4) t.simps(5))
next
  assume "c = NULL"
  thus "(h, c) = (h', cap)"
    using free_null assms 
    by force
qed

lemma double_free:
  assumes "free h c = Success (h', cap)"
    and "cap \<noteq> NULL"
  shows "free h' cap = Error (C2Err TagViolation)"
proof -
  have "cap = c \<lparr> tag := False \<rparr> \<Longrightarrow> tag cap = False"
    by fastforce
  thus ?thesis
    using assms free_cond(6)[where ?h=h and ?c=c and ?h'=h' and ?cap=cap] 
      free_false_tag[where ?c=cap and ?h=h'] free_cond(7)[where ?h=h and ?c=c and ?h'=h' and ?cap=cap]
    by blast
qed

lemma alloc_free:
  assumes "alloc h c s = Success (h', cap)"
  shows "\<exists>! ret. free h' cap = Success ret"
  using alloc_def assms free_non_null_correct alloc_no_null_ret
  by force

primrec is_memval_defined :: "(nat, memval) mapping \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> bool"
  where
  "is_memval_defined _ _ 0 = True"
| "is_memval_defined m off (Suc siz) = ((off \<in> Mapping.keys m) \<and> is_memval_defined m (Suc off) siz)"

primrec is_contiguous_bytes :: "(nat, memval) mapping \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> bool"
  where
  "is_contiguous_bytes _ _ 0 = True"
| "is_contiguous_bytes m off (Suc siz) = ((off \<in> Mapping.keys m) 
                                         \<and> memval_is_byte (the (Mapping.lookup m off))
                                         \<and> is_contiguous_bytes m (Suc off) siz)"

definition get_cap :: "(nat, memval) mapping \<Rightarrow> nat \<Rightarrow> memcap"
  where
  "get_cap m off = of_cap (the (Mapping.lookup m off))"

fun is_cap :: "(nat, memval) mapping \<Rightarrow> nat \<Rightarrow> bool"
  where
  "is_cap m off = (off \<in> Mapping.keys m \<and> memval_is_cap (the (Mapping.lookup m off)))"

primrec is_contiguous_cap :: "(nat, memval) mapping \<Rightarrow> memcap \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> bool"
  where
  "is_contiguous_cap _ _ _ 0 = True"
| "is_contiguous_cap m c off (Suc siz) = ((off \<in> Mapping.keys m)
                                         \<and> memval_is_cap (the (Mapping.lookup m off))
                                         \<and> of_cap (the (Mapping.lookup m off)) = c
                                         \<and> of_nth (the (Mapping.lookup m off)) = siz
                                         \<and> is_contiguous_cap m c (Suc off) siz)"

primrec is_contiguous_zeros_prim :: "(nat, memval) mapping \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> bool"
  where
  "is_contiguous_zeros_prim _ _ 0 = True"
| "is_contiguous_zeros_prim m off (Suc siz) = (Mapping.lookup m off = Some (Byte 0)
                                              \<and> is_contiguous_zeros_prim m (Suc off) siz)"

definition is_contiguous_zeros :: "(nat, memval) mapping \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> bool"
  where
  "is_contiguous_zeros m off siz \<equiv> \<forall> ofs \<ge> off. ofs < off + siz \<longrightarrow> Mapping.lookup m ofs = Some (Byte 0)"

lemma is_contiguous_zeros_code[code]:
  "is_contiguous_zeros m off siz = is_contiguous_zeros_prim m off siz"
proof safe
  show "is_contiguous_zeros m off siz \<Longrightarrow> is_contiguous_zeros_prim m off siz"
    unfolding is_contiguous_zeros_def
  proof (induct siz arbitrary: off)
    case 0
    thus ?case by simp
  next
    case (Suc siz)
    thus ?case
      by fastforce
  qed
next
  show "is_contiguous_zeros_prim m off siz \<Longrightarrow> is_contiguous_zeros m off siz"
    unfolding is_contiguous_zeros_def
  proof (induct siz arbitrary: off)
    case 0
    thus ?case
      by auto
  next
    case (Suc siz)
    have alt: "is_contiguous_zeros_prim m (Suc off) siz"
      using Suc(2) is_contiguous_zeros_prim.simps(2)[where ?m=m and ?off=off and ?siz=siz]
      by blast
    have add_simp: "Suc off + siz = off + Suc siz" 
      by simp
    show ?case 
      using Suc(1)[where ?off="Suc off", OF alt, simplified add_simp le_eq_less_or_eq Suc_le_eq] 
        Suc(2) Suc_le_eq le_eq_less_or_eq 
      by auto
  qed
qed

  

primrec retrieve_bytes :: "(nat, memval) mapping \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> 8 word list"
  where
  "retrieve_bytes m _ 0 = []"
| "retrieve_bytes m off (Suc siz) = of_byte (the (Mapping.lookup m off)) # retrieve_bytes m (Suc off) siz"

primrec is_same_cap :: "(nat, memval) mapping \<Rightarrow> memcap \<Rightarrow> nat \<Rightarrow> nat \<Rightarrow> bool"
  where
  "is_same_cap _ _ _ 0 = True"
| "is_same_cap m c off (Suc siz) = (of_cap (the (Mapping.lookup m off)) = c \<and> is_same_cap m c (Suc off) siz)"

abbreviation cap_offset :: "nat \<Rightarrow> nat"
  where
  "cap_offset p \<equiv> if p mod |Cap|\<^sub>\<tau> = 0 then p else p - p mod |Cap|\<^sub>\<tau>"

(* tag retrieval must be based on offset now *)
definition retrieve_tval :: "object \<Rightarrow> nat \<Rightarrow> cctype \<Rightarrow> bool \<Rightarrow> block ccval"
  where
  "retrieve_tval obj off typ pcl \<equiv> 
     if is_contiguous_bytes (content obj) off |typ|\<^sub>\<tau> then
       (case typ of 
          Uint8  \<Rightarrow> Uint8_v  (decode_u8_list (retrieve_bytes (content obj) off |typ|\<^sub>\<tau>))
        | Sint8  \<Rightarrow> Sint8_v  (decode_s8_list (retrieve_bytes (content obj) off |typ|\<^sub>\<tau>))
        | Uint16 \<Rightarrow> Uint16_v (cat_u16 (retrieve_bytes (content obj) off |typ|\<^sub>\<tau>))
        | Sint16 \<Rightarrow> Sint16_v (cat_s16 (retrieve_bytes (content obj) off |typ|\<^sub>\<tau>))
        | Uint32 \<Rightarrow> Uint32_v (cat_u32 (retrieve_bytes (content obj) off |typ|\<^sub>\<tau>))
        | Sint32 \<Rightarrow> Sint32_v (cat_s32 (retrieve_bytes (content obj) off |typ|\<^sub>\<tau>))
        | Uint64 \<Rightarrow> Uint64_v (cat_u64 (retrieve_bytes (content obj) off |typ|\<^sub>\<tau>))
        | Sint64 \<Rightarrow> Sint64_v (cat_s64 (retrieve_bytes (content obj) off |typ|\<^sub>\<tau>))
        | Cap    \<Rightarrow> if is_contiguous_zeros (content obj) off |typ|\<^sub>\<tau> then Cap_v NULL else Undef)
     else if is_cap (content obj) off then
       let cap = get_cap (content obj) off in
       let tv = the (Mapping.lookup (tags obj) (cap_offset off)) in
       let t = (case pcl of False \<Rightarrow> False | True \<Rightarrow> tv) in
       let cv = mem_capability.extend cap \<lparr> tag = t \<rparr> in 
       let nth_frag = of_nth (the (Mapping.lookup (content obj) off)) in 
       (case typ of 
          Uint8 \<Rightarrow> Cap_v_frag cv nth_frag
        | Sint8 \<Rightarrow> Cap_v_frag cv nth_frag
        | Cap   \<Rightarrow> if is_contiguous_cap (content obj) cap off |typ|\<^sub>\<tau> then Cap_v cv else Undef
        | _     \<Rightarrow> Undef)
     else Undef"

text \<open>How load works:
      The hardware would perform a CL[C] operation on the given capability first.
      An invalid capability for load would be caught by the hardware.
      Once all the hardware checks are performed, we then proceed to the logical checks.\<close>
definition load :: "heap \<Rightarrow> cap \<Rightarrow> cctype \<Rightarrow> block ccval result"
  where
  "load h c t \<equiv> 
     if tag c = False then
       Error (C2Err TagViolation)
     else if perm_load c = False then 
       Error (C2Err PermitLoadViolation)
     else if offset c + |t|\<^sub>\<tau> > base c + len c then
       Error (C2Err LengthViolation)
     else if offset c < base c then
       Error (C2Err LengthViolation)
     else if offset c mod |t|\<^sub>\<tau> \<noteq> 0 then
       Error (C2Err BadAddressViolation)
     else
       let obj = Mapping.lookup (heap_map h) (block_id c) in
      (case obj of None      \<Rightarrow> Error (LogicErr (MissingResource))
                 | Some cobj \<Rightarrow>
        (case cobj of Freed \<Rightarrow> Error (LogicErr (UseAfterFree))
                    | Map m \<Rightarrow> Success (retrieve_tval m (offset c) t (perm_cap_load c))))"

lemma load_null_error:
  "load h NULL t = Error (C2Err TagViolation)" 
  unfolding load_def
  by simp

lemma load_false_tag:
  assumes "tag c = False"
  shows "load h c t = Error (C2Err TagViolation)"
  unfolding load_def
  using assms
  by presburger

lemma load_false_perm_load:
  assumes "tag c = True"
    and "perm_load c = False"
  shows "load h c t = Error (C2Err PermitLoadViolation)"
  unfolding load_def
  using assms 
  by presburger

lemma load_bound_over:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> > base c + len c"
  shows "load h c t = Error (C2Err LengthViolation)"
  unfolding load_def
  using assms 
  by presburger

lemma load_bound_under:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c < base c"
  shows "load h c t = Error (C2Err LengthViolation)"
  unfolding load_def
  using assms 
  by presburger

lemma load_misaligned:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> \<noteq> 0"
  shows "load h c t = Error (C2Err BadAddressViolation)"
  unfolding load_def
  using assms 
  by force

lemma load_nonexistant_obj:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = None"
  shows "load h c t = Error (LogicErr MissingResource)"
  unfolding load_def
  using assms
  by auto

lemma load_load_after_free:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some Freed"
  shows "load h c t = Error (LogicErr UseAfterFree)"
  unfolding load_def
  using assms
  by fastforce

lemma load_cap_on_membytes_fail:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "t = Cap"
    and "\<not> is_contiguous_zeros (content m) (offset c) |t|\<^sub>\<tau>"
  shows "load h c t = Success Undef"
  unfolding load_def retrieve_tval_def 
  using assms
  by fastforce

lemma load_null_cap_on_membytes:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "t = Cap"
    and "is_contiguous_zeros (content m) (offset c) |t|\<^sub>\<tau>"
  shows "load h c t = Success (Cap_v NULL)"
  unfolding load_def retrieve_tval_def 
  using assms
  by fastforce

lemma load_u8_on_membytes:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "t = Uint8"
  shows "load h c t = Success (Uint8_v (decode_u8_list (retrieve_bytes (content m) (offset c) |t|\<^sub>\<tau>)))"
  unfolding load_def retrieve_tval_def 
  using assms
  by fastforce

lemma load_s8_on_membytes:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "t = Sint8"
  shows "load h c t = Success (Sint8_v (decode_s8_list (retrieve_bytes (content m) (offset c) |t|\<^sub>\<tau>)))"
  unfolding load_def retrieve_tval_def 
  using assms
  by fastforce

lemma load_u16_on_membytes:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "t = Uint16"
  shows "load h c t = Success (Uint16_v (cat_u16 (retrieve_bytes (content m) (offset c) |t|\<^sub>\<tau>)))"
  unfolding load_def retrieve_tval_def 
  using assms
  by fastforce

lemma load_s16_on_membytes:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "t = Sint16"
  shows "load h c t = Success (Sint16_v (cat_s16 (retrieve_bytes (content m) (offset c) |t|\<^sub>\<tau>)))"
  unfolding load_def retrieve_tval_def 
  using assms
  by fastforce

lemma load_u32_on_membytes:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "t = Uint32"
  shows "load h c t = Success (Uint32_v (cat_u32 (retrieve_bytes (content m) (offset c) |t|\<^sub>\<tau>)))"
  unfolding load_def retrieve_tval_def 
  using assms
  by fastforce

lemma load_s32_on_membytes:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "t = Sint32"
  shows "load h c t = Success (Sint32_v (cat_s32 (retrieve_bytes (content m) (offset c) |t|\<^sub>\<tau>)))"
  unfolding load_def retrieve_tval_def 
  using assms
  by fastforce

lemma load_u64_on_membytes:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "t = Uint64"
  shows "load h c t = Success (Uint64_v (cat_u64 (retrieve_bytes (content m) (offset c) |t|\<^sub>\<tau>)))"
  unfolding load_def retrieve_tval_def 
  using assms
  by fastforce

lemma load_s64_on_membytes:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "t = Sint64"
  shows "load h c t = Success (Sint64_v (cat_s64 (retrieve_bytes (content m) (offset c) |t|\<^sub>\<tau>)))"
  unfolding load_def retrieve_tval_def 
  using assms
  by fastforce

lemma load_not_cap_in_mem:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "\<not> is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "\<not> is_cap (content m) (offset c)"
  shows "load h c t = Success Undef"
  unfolding load_def retrieve_tval_def 
  using assms
  by fastforce

lemma load_not_contiguous_cap_in_mem:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "\<not> is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "is_cap (content m) (offset c)"
    and "mc = get_cap (content m) (offset c)"
    and "\<not> is_contiguous_cap (content m) mc (offset c) |t|\<^sub>\<tau>"
    and "t \<noteq> Uint8"
    and "t \<noteq> Sint8"
  shows "load h c t = Success Undef"
  unfolding load_def retrieve_tval_def Let_def
  using assms
  by (clarsimp split: cctype.split)

lemma load_cap_frag_u8:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "\<not> is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "is_cap (content m) (offset c)"
    and "mc = get_cap (content m) (offset c)"
    and "t = Uint8"
    and "tagval = the (Mapping.lookup (tags m) (cap_offset (offset c)))"
    and "tg = (case perm_cap_load c of False \<Rightarrow> False | True \<Rightarrow> tagval)"
    and "nth_frag = of_nth (the (Mapping.lookup (content m) (offset c)))"
  shows "load h c t = Success (Cap_v_frag (mem_capability.extend mc \<lparr> tag = tg \<rparr>) nth_frag)"
  unfolding load_def retrieve_tval_def Let_def
  using assms
  by (clarsimp simp add: sizeof_def split: cctype.split)

lemma load_cap_frag_s8:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "\<not> is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "is_cap (content m) (offset c)"
    and "mc = get_cap (content m) (offset c)"
    and "\<not> is_contiguous_cap (content m) mc (offset c) |t|\<^sub>\<tau>"
    and "t = Sint8"
    and "tagval = the (Mapping.lookup (tags m) (cap_offset (offset c)))"
    and "tg = (case perm_cap_load c of False \<Rightarrow> False | True \<Rightarrow> tagval)"
    and "nth_frag = of_nth (the (Mapping.lookup (content m) (offset c)))"
  shows "load h c t = Success (Cap_v_frag (mem_capability.extend mc \<lparr> tag = tg \<rparr>) nth_frag)"
  unfolding load_def retrieve_tval_def Let_def
  using assms
  by (clarsimp simp add: sizeof_def split: cctype.split)

lemma load_bytes_on_capbytes_fail:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "\<not> is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "is_cap (content m) (offset c)"
    and "mc = get_cap (content m) (offset c)"
    and "is_contiguous_cap (content m) mc (offset c) |t|\<^sub>\<tau>"
    and "t \<noteq> Cap"
    and "t \<noteq> Uint8"
    and "t \<noteq> Sint8"
  shows "load h c t = Success Undef"
  unfolding load_def retrieve_tval_def Let_def
  using assms 
  by (clarsimp split: cctype.split)

lemma load_cap_on_capbytes:
  assumes "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
    and "\<not> is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
    and "is_cap (content m) (offset c)"
    and "mc = get_cap (content m) (offset c)"
    and "is_contiguous_cap (content m) mc (offset c) |t|\<^sub>\<tau>"
    and "t = Cap"
    and "tagval = the (Mapping.lookup (tags m) (offset c))"
    and "tg = (case perm_cap_load c of False \<Rightarrow> False | True \<Rightarrow> tagval)"
  shows "load h c t = Success (Cap_v (mem_capability.extend mc \<lparr> tag = tg \<rparr>))"
  unfolding load_def retrieve_tval_def 
  using assms 
  by (clarsimp split: cctype.split)

lemma load_after_alloc:
  assumes "alloc h c s = Success (h', cap)"
    and "|t|\<^sub>\<tau> \<le> s"
  shows "load h' cap t = Success Undef"
proof -
  let ?m = "\<lparr>bounds = (0, s), content = Mapping.empty, tags = Mapping.empty\<rparr>"
  have "tag cap = True"
    using assms(1) alloc_def 
    by fastforce
  moreover have "perm_load cap = True"
    using assms(1) alloc_def
    by fastforce
  moreover have "offset cap + |t|\<^sub>\<tau> \<le> base cap + len cap"
    using assms alloc_def 
    by fastforce
  moreover have "offset cap \<ge> base cap"
    using assms alloc_def
    by fastforce
  moreover have "offset cap mod |t|\<^sub>\<tau> = 0"
    using assms alloc_def
    by fastforce
  moreover have "Mapping.lookup (heap_map h') (block_id cap) = Some (Map ?m)"
    using assms alloc_def
    by fastforce
  moreover have "\<not> is_contiguous_bytes (content ?m) (offset cap) |t|\<^sub>\<tau>"
  proof -
    have "\<exists> n. |t|\<^sub>\<tau> = Suc n"
      using not0_implies_Suc sizeof_nonzero 
      by force
    thus ?thesis 
      using assms alloc_def
      by fastforce
  qed
  moreover have "\<not> is_cap (content ?m) (offset cap)"
    by simp
  ultimately show ?thesis
    using load_not_cap_in_mem
    by presburger
qed

lemma load_after_alloc_size_fail:
  assumes "alloc h c s = Success (h', cap)"
    and "|t|\<^sub>\<tau> > s"
  shows "load h' cap t = Error (C2Err LengthViolation)"
proof -
  have "tag cap = True"
    using assms alloc_def
    by auto
  moreover have "perm_load cap = True"
    using assms alloc_def
    by force
  moreover have "base cap = 0"
    using assms alloc_def
    by fastforce
  moreover have "len cap = s"
    using assms alloc_def 
    by auto
  ultimately show ?thesis 
    using assms load_def by auto
qed

lemma load_after_free:
  assumes "free h c = Success (h', cap)"
  shows "load h cap t = Error (C2Err TagViolation)"
proof -
  consider (null) "c = NULL" | (non_null) "c \<noteq> NULL" by blast
  then show ?thesis
  proof (cases)
    case null
    moreover hence "c = cap"
      using assms free_null
      by force
    ultimately show ?thesis
      using load_null_error assms
      by blast
  next
    case non_null
    hence "cap = c \<lparr> tag := False \<rparr>"
      using assms free_cond(6)[where ?h=h and ?c=c and ?h'=h' and ?cap=cap] 
      by presburger
    moreover hence "tag cap = False"
      using assms
      by force
    ultimately show ?thesis using load_false_tag
      by blast
  qed
qed

lemma load_cond_hard_cap:
  assumes "load h c t = Success ret"
  shows "tag c = True"
    and "perm_load c = True"
    and "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |t|\<^sub>\<tau> = 0"
proof -
  show "tag c = True"
    using assms result.distinct(1) 
    unfolding load_def
    by metis
next
  show "perm_load c = True"
    using assms result.distinct(1) 
    unfolding load_def
    by metis
next
  show "offset c + |t|\<^sub>\<tau> \<le> base c + len c"
    using assms result.distinct(1) linorder_not_le
    unfolding load_def 
    by metis
next 
  show "offset c \<ge> base c"
    using assms result.distinct(1) linorder_not_le
    unfolding load_def 
    by metis
next
  show "offset c mod |t|\<^sub>\<tau> = 0"
    using assms result.distinct(1)
    unfolding load_def 
    by metis
qed

lemma load_cond_bytes:
  assumes "load h c t = Success ret"
    and "ret \<noteq> Undef"
    and "\<forall> x. ret \<noteq> Cap_v x"
    and "\<forall> x n . ret \<noteq> Cap_v_frag x n"
  shows "\<exists> m. Mapping.lookup (heap_map h) (block_id c) = Some (Map m)
            \<and> is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>"
proof (cases ret)
  case (Cap_v x9)
  thus ?thesis 
    using assms(3)
    by blast
next
  case (Cap_v_frag x101 x102)
  thus ?thesis
    using assms(4)
    by blast
next
  case Undef
  thus ?thesis
    using assms(2)
    by simp
(* WARNING: takes quite some time to prove the remaining cases *)
qed (insert assms(1) load_cond_hard_cap[where ?h=h and ?c=c and ?t=t and ?ret=ret], clarsimp, 
    unfold load_def retrieve_tval_def, clarsimp split: option.split_asm t.split_asm, 
    smt (z3) assms(2) assms(3) assms(4) cctype.exhaust cctype.simps(73) cctype.simps(74) 
    cctype.simps(75) cctype.simps(76) cctype.simps(77) cctype.simps(78) cctype.simps(79) 
    cctype.simps(80) cctype.simps(81))+

lemma load_cond_cap:
  assumes "load h c t = Success ret"
    and "\<exists> x. ret = Cap_v x"
  shows "\<exists> m mc tagval tg. 
              Mapping.lookup (heap_map h) (block_id c) = Some (Map m) \<and>
              (is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau> \<longrightarrow> 
               is_contiguous_zeros (content m) (offset c) |t|\<^sub>\<tau> \<and>
               ret = Cap_v NULL) \<and>
              (\<not> is_contiguous_bytes (content m) (offset c) |t|\<^sub>\<tau>  \<longrightarrow>
               is_cap (content m) (offset c) \<and>
               mc = get_cap (content m) (offset c) \<and>
               is_contiguous_cap (content m) mc (offset c) |t|\<^sub>\<tau> \<and>
               t = Cap \<and>
               tagval = the (Mapping.lookup (tags m) (offset c)) \<and> 
               tg = (case perm_cap_load c of False \<Rightarrow> False | True \<Rightarrow> tagval))"
  using assms(2)
proof (cases ret)
  case (Cap_v ca)
  show ?thesis
    by (insert assms load_cond_hard_cap[where ?h=h and ?c=c and ?t=t and ?ret=ret], clarsimp,
        unfold load_def retrieve_tval_def Let_def, clarsimp split: option.split_asm, 
        clarsimp split: t.split_asm cctype.split_asm, safe; force?)
      (metis ccval.distinct(105) ccval.distinct(107) ccval.inject(9) is_cap.elims(2))+
qed blast+

primrec store_bytes :: "(nat, memval) mapping \<Rightarrow> nat \<Rightarrow> 8 word list \<Rightarrow> (nat, memval) mapping"
  where
  "store_bytes obj _ [] = obj"
| "store_bytes obj off (v # vs) = store_bytes (Mapping.update off (Byte v) obj) (Suc off) vs"

primrec store_cap :: "(nat, memval) mapping \<Rightarrow> nat \<Rightarrow> cap \<Rightarrow> nat \<Rightarrow> (nat, memval) mapping"
  where
  "store_cap obj _ _ 0 = obj"
| "store_cap obj off cap (Suc n) = store_cap (Mapping.update off (ACap (mem_capability.truncate cap) n) obj) (Suc off) cap n"

abbreviation store_tag :: "(nat, bool) mapping \<Rightarrow> nat \<Rightarrow> bool \<Rightarrow> (nat, bool) mapping"
  where
  "store_tag obj off tg \<equiv> Mapping.update off tg obj"
                                                             
definition store_tval :: "object \<Rightarrow> nat \<Rightarrow> block ccval \<Rightarrow> object"
  where
  "store_tval obj off val \<equiv> 
     case val of Uint8_v  v     \<Rightarrow> obj \<lparr> content := store_bytes (content obj) off (encode_u8_list v), 
                                         tags := store_tag (tags obj) (cap_offset off) False \<rparr>
               | Sint8_v  v     \<Rightarrow> obj \<lparr> content := store_bytes (content obj) off (encode_s8_list v), 
                                         tags := store_tag (tags obj) (cap_offset off) False \<rparr>
               | Uint16_v v     \<Rightarrow> obj \<lparr> content := store_bytes (content obj) off (u16_split v), 
                                         tags := store_tag (tags obj) (cap_offset off) False \<rparr>
               | Sint16_v v     \<Rightarrow> obj \<lparr> content := store_bytes (content obj) off (s16_split v), 
                                         tags := store_tag (tags obj) (cap_offset off) False \<rparr>
               | Uint32_v v     \<Rightarrow> obj \<lparr> content := store_bytes (content obj) off (flatten_u32 v), 
                                         tags := store_tag (tags obj) (cap_offset off) False \<rparr>
               | Sint32_v v     \<Rightarrow> obj \<lparr> content := store_bytes (content obj) off (flatten_s32 v), 
                                         tags := store_tag (tags obj) (cap_offset off) False \<rparr>
               | Uint64_v v     \<Rightarrow> obj \<lparr> content := store_bytes (content obj) off (flatten_u64 v), 
                                         tags := store_tag (tags obj) (cap_offset off) False \<rparr>
               | Sint64_v v     \<Rightarrow> obj \<lparr> content := store_bytes (content obj) off (flatten_s64 v), 
                                         tags := store_tag (tags obj) (cap_offset off) False \<rparr>
               | Cap_v    c     \<Rightarrow> obj \<lparr> content := store_cap (content obj) off c |Cap|\<^sub>\<tau>, 
                                         tags := store_tag (tags obj) (cap_offset off) (tag c) \<rparr>
               | Cap_v_frag c n \<Rightarrow> obj \<lparr> content := Mapping.update off (ACap (mem_capability.truncate c) n) (content obj),
                                         tags := if off mod |Cap|\<^sub>\<tau> = |Cap|\<^sub>\<tau> - 1 - n then
                                                   if |Cap|\<^sub>\<tau> - 1 - n = 0 then
                                                     store_tag (tags obj) (cap_offset off) (tag c)
                                                   else if (cap_offset off) \<in> Mapping.keys (tags obj) then 
                                                     tags obj
                                                   else
                                                     store_tag (tags obj) (cap_offset off) False
                                                 else
                                                   store_tag (tags obj) (cap_offset off) False\<rparr>"

lemma stored_bytes_prev:
  assumes "x < off"
  shows "Mapping.lookup (store_bytes obj off vs) x = Mapping.lookup obj x"
  using assms 
  by (induct vs arbitrary: obj off) fastforce+

lemma stored_cap_prev:
  assumes "x < off"
  shows "Mapping.lookup (store_cap obj off cap siz) x = Mapping.lookup obj x"
  using assms 
  by (induct siz arbitrary: obj off) fastforce+

lemma stored_bytes_instant_correctness:
  "Mapping.lookup (store_bytes obj off (v # vs)) off = Some (Byte v)"
proof (induct vs arbitrary: obj off)
  case Nil
  thus ?case by force
next 
  case (Cons a vs)
  thus ?case using stored_bytes_prev Suc_eq_plus1 lessI store_bytes.simps(2)
    by metis
qed

lemma stored_cap_instant_correctness:
  "Mapping.lookup (store_cap obj off cap (Suc siz)) off = Some (ACap (mem_capability.truncate cap) siz)"
proof (induct siz arbitrary: obj off)
  case 0
  thus ?case by force
next 
  case (Suc siz)
  thus ?case using stored_cap_prev Suc_eq_plus1 lessI store_cap.simps(2) lookup_update
    by metis
qed

lemma numeral_4_eq_4: "4 = Suc (Suc (Suc (Suc 0)))"
  by (simp add: eval_nat_numeral)

lemma numeral_5_eq_5: "5 = Suc (Suc (Suc (Suc (Suc 0))))"
  by (simp add: eval_nat_numeral)

lemma numeral_6_eq_6: "6 = Suc (Suc (Suc (Suc (Suc (Suc 0)))))"
  by (simp add: eval_nat_numeral)

lemma numeral_7_eq_7: "7 = Suc (Suc (Suc (Suc (Suc (Suc (Suc 0))))))"
  by (simp add: eval_nat_numeral)

lemma numeral_8_eq_8: "8 = Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc 0)))))))"
  by (simp add: eval_nat_numeral)

lemma list_length_2_realise:
  "length ls = 2 \<Longrightarrow> \<exists> n0 n1. ls = [n0, n1]"
  by (metis One_nat_def Suc_length_conv add_diff_cancel_right' len_gt_0 len_of_finite_2_def 
      list.size(4) list_exhaust_size_eq0 list_exhaust_size_gt0 one_add_one)

lemma list_length_4_realise: 
  "length ls = 4 \<Longrightarrow> \<exists> n0 n1 n2 n3. ls = [n0, n1, n2, n3]"
  by (metis list_exhaust_size_eq0 list_exhaust_size_gt0 numeral_4_eq_4 size_Cons_lem_eq zero_less_Suc)

lemma list_length_8_realise:
  "length ls = 8 \<Longrightarrow> \<exists> n0 n1 n2 n3 n4 n5 n6 n7. ls = [n0, n1, n2, n3, n4, n5, n6, n7]"
  using list_exhaust_size_eq0 list_exhaust_size_gt0 numeral_8_eq_8 size_Cons_lem_eq zero_less_Suc
  by smt

lemma u16_split_realise:
  "\<exists> b0 b1. u16_split v = [b0, b1]" 
  using list_length_2_realise[where ?ls="u16_split v", OF u16_split_length[where ?vs=v]]
  by assumption

lemma s16_split_realise:
  "\<exists> b0 b1. s16_split v = [b0, b1]"
  using list_length_2_realise[where ?ls="s16_split v", OF flatten_s16_length[where ?vs=v]]
  by assumption

lemma u32_split_realise:
  "\<exists> b0 b1 b2 b3. flatten_u32 v = [b0, b1, b2, b3]"
  using list_length_4_realise[where ?ls="flatten_u32 v", OF flatten_u32_length[where ?vs=v]]
  by assumption

lemma s32_split_realise:
  "\<exists> b0 b1 b2 b3. flatten_s32 v = [b0, b1, b2, b3]"
  using list_length_4_realise[where ?ls="flatten_s32 v", OF flatten_s32_length[where ?vs=v]]
  by assumption

lemma u64_split_realise:
  "\<exists> b0 b1 b2 b3 b4 b5 b6 b7. flatten_u64 v = [b0, b1, b2, b3, b4, b5, b6, b7]"
  using list_length_8_realise[where ?ls="flatten_u64 v", OF flatten_u64_length[where ?vs=v]]
  by assumption

lemma s64_split_realise:
  "\<exists> b0 b1 b2 b3 b4 b5 b6 b7. flatten_s64 v = [b0, b1, b2, b3, b4, b5, b6, b7]"
  using list_length_8_realise[where ?ls="flatten_s64 v", OF flatten_s64_length[where ?vs=v]]
  by assumption

lemma store_bytes_u16:
  shows "off \<in> Mapping.keys (store_bytes m off (u16_split v))"
    and "Suc off \<in> Mapping.keys (store_bytes m off (u16_split v))"
    and "\<exists> b0. Mapping.lookup (store_bytes m off (u16_split v)) off = Some (Byte b0)"
    and "\<exists> b1. Mapping.lookup (store_bytes m off (u16_split v)) (Suc off) = Some (Byte b1)" 
proof -
  show "off \<in> Mapping.keys (store_bytes m off (u16_split v))"
    by (metis (no_types, opaque_lifting) domIff u16_split_realise handy_if_lemma keys_dom_lookup 
        stored_bytes_instant_correctness)
next
  show "Suc off \<in> Mapping.keys (store_bytes m off (u16_split v))"
    by (metis (mono_tags, opaque_lifting) domIff u16_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b0. Mapping.lookup (store_bytes m off (u16_split v)) off = Some (Byte b0)"
    by (metis u16_split_realise stored_bytes_instant_correctness)
next
  show "\<exists> b1. Mapping.lookup (store_bytes m off (u16_split v)) (Suc off) = Some (Byte b1)"
    by (metis u16_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
qed

lemma store_bytes_s16:
  shows "off \<in> Mapping.keys (store_bytes m off (s16_split v))"
    and "Suc off \<in> Mapping.keys (store_bytes m off (s16_split v))"
    and "\<exists> b0. Mapping.lookup (store_bytes m off (s16_split v)) off = Some (Byte b0)"
    and "\<exists> b1. Mapping.lookup (store_bytes m off (s16_split v)) (Suc off) = Some (Byte b1)" 
proof -
  show "off \<in> Mapping.keys (store_bytes m off (s16_split v))"
    by (metis (no_types, opaque_lifting) domIff s16_split_realise handy_if_lemma keys_dom_lookup 
        stored_bytes_instant_correctness)
next
  show "Suc off \<in> Mapping.keys (store_bytes m off (s16_split v))"
    by (metis (mono_tags, opaque_lifting) domIff s16_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b0. Mapping.lookup (store_bytes m off (s16_split v)) off = Some (Byte b0)"
    by (metis s16_split_realise stored_bytes_instant_correctness)
next
  show "\<exists> b1. Mapping.lookup (store_bytes m off (s16_split v)) (Suc off) = Some (Byte b1)"
    by (metis s16_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
qed

lemma store_bytes_u32:
  shows "off \<in> Mapping.keys (store_bytes m off (flatten_u32 v))"
    and "Suc off \<in> Mapping.keys (store_bytes m off (flatten_u32 v))"
    and "Suc (Suc off) \<in> Mapping.keys (store_bytes m off (flatten_u32 v))"
    and "Suc (Suc (Suc off)) \<in> Mapping.keys (store_bytes m off (flatten_u32 v))"
    and "\<exists> b0. Mapping.lookup (store_bytes m off (flatten_u32 v)) off = Some (Byte b0)"
    and "\<exists> b1. Mapping.lookup (store_bytes m off (flatten_u32 v)) (Suc off) = Some (Byte b1)"
    and "\<exists> b2. Mapping.lookup (store_bytes m off (flatten_u32 v)) (Suc (Suc off)) = Some (Byte b2)"
    and "\<exists> b3. Mapping.lookup (store_bytes m off (flatten_u32 v)) (Suc (Suc (Suc off))) = Some (Byte b3)" 
proof -
  show "off \<in> Mapping.keys (store_bytes m off (flatten_u32 v))" 
    by (metis (no_types, opaque_lifting) domIff handy_if_lemma keys_dom_lookup 
        stored_bytes_instant_correctness u32_split_realise)
next
  show "Suc off \<in> Mapping.keys (store_bytes m off (flatten_u32 v))" 
    by (metis (mono_tags, opaque_lifting) domIff u32_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc off) \<in> Mapping.keys (store_bytes m off (flatten_u32 v))"
    by (metis (mono_tags, opaque_lifting) domIff u32_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc (Suc off)) \<in> Mapping.keys (store_bytes m off (flatten_u32 v))"
    by (metis (mono_tags, opaque_lifting) domIff u32_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b0. Mapping.lookup (store_bytes m off (flatten_u32 v)) off = Some (Byte b0)"
    by (metis u32_split_realise stored_bytes_instant_correctness)
next
  show "\<exists> b1. Mapping.lookup (store_bytes m off (flatten_u32 v)) (Suc off) = Some (Byte b1)"
    by (metis u32_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b2. Mapping.lookup (store_bytes m off (flatten_u32 v)) (Suc (Suc off)) = Some (Byte b2)"
    by (metis u32_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b3. Mapping.lookup (store_bytes m off (flatten_u32 v)) (Suc (Suc (Suc off))) = Some (Byte b3)" 
    by (metis u32_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
qed

lemma store_bytes_s32:
  shows "off \<in> Mapping.keys (store_bytes m off (flatten_s32 v))"
    and "Suc off \<in> Mapping.keys (store_bytes m off (flatten_s32 v))"
    and "Suc (Suc off) \<in> Mapping.keys (store_bytes m off (flatten_s32 v))"
    and "Suc (Suc (Suc off)) \<in> Mapping.keys (store_bytes m off (flatten_s32 v))"
    and "\<exists> b0. Mapping.lookup (store_bytes m off (flatten_s32 v)) off = Some (Byte b0)"
    and "\<exists> b1. Mapping.lookup (store_bytes m off (flatten_s32 v)) (Suc off) = Some (Byte b1)"
    and "\<exists> b2. Mapping.lookup (store_bytes m off (flatten_s32 v)) (Suc (Suc off)) = Some (Byte b2)"
    and "\<exists> b3. Mapping.lookup (store_bytes m off (flatten_s32 v)) (Suc (Suc (Suc off))) = Some (Byte b3)" 
proof -
  show "off \<in> Mapping.keys (store_bytes m off (flatten_s32 v))" 
    by (metis (no_types, opaque_lifting) domIff handy_if_lemma keys_dom_lookup 
        stored_bytes_instant_correctness s32_split_realise)
next
  show "Suc off \<in> Mapping.keys (store_bytes m off (flatten_s32 v))" 
    by (metis (mono_tags, opaque_lifting) domIff s32_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc off) \<in> Mapping.keys (store_bytes m off (flatten_s32 v))"
    by (metis (mono_tags, opaque_lifting) domIff s32_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc (Suc off)) \<in> Mapping.keys (store_bytes m off (flatten_s32 v))"
    by (metis (mono_tags, opaque_lifting) domIff s32_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b0. Mapping.lookup (store_bytes m off (flatten_s32 v)) off = Some (Byte b0)"
    by (metis s32_split_realise stored_bytes_instant_correctness)
next
  show "\<exists> b1. Mapping.lookup (store_bytes m off (flatten_s32 v)) (Suc off) = Some (Byte b1)"
    by (metis s32_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b2. Mapping.lookup (store_bytes m off (flatten_s32 v)) (Suc (Suc off)) = Some (Byte b2)"
    by (metis s32_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b3. Mapping.lookup (store_bytes m off (flatten_s32 v)) (Suc (Suc (Suc off))) = Some (Byte b3)" 
    by (metis s32_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
qed

lemma store_bytes_u64:
  shows "off \<in> Mapping.keys (store_bytes m off (flatten_u64 v))"
    and "Suc off \<in> Mapping.keys (store_bytes m off (flatten_u64 v))"
    and "Suc (Suc off) \<in> Mapping.keys (store_bytes m off (flatten_u64 v))"
    and "Suc (Suc (Suc off)) \<in> Mapping.keys (store_bytes m off (flatten_u64 v))"
    and "Suc (Suc (Suc (Suc off))) \<in> Mapping.keys (store_bytes m off (flatten_u64 v))"
    and "Suc (Suc (Suc (Suc (Suc off)))) \<in> Mapping.keys (store_bytes m off (flatten_u64 v))"
    and "Suc (Suc (Suc (Suc (Suc (Suc off))))) \<in> Mapping.keys (store_bytes m off (flatten_u64 v))"
    and "Suc (Suc (Suc (Suc (Suc (Suc (Suc off)))))) \<in> Mapping.keys (store_bytes m off (flatten_u64 v))"
    and "\<exists> b0. Mapping.lookup (store_bytes m off (flatten_u64 v)) off = Some (Byte b0)"
    and "\<exists> b1. Mapping.lookup (store_bytes m off (flatten_u64 v)) (Suc off) = Some (Byte b1)"
    and "\<exists> b2. Mapping.lookup (store_bytes m off (flatten_u64 v)) (Suc (Suc off)) = Some (Byte b2)"
    and "\<exists> b3. Mapping.lookup (store_bytes m off (flatten_u64 v)) (Suc (Suc (Suc off))) = Some (Byte b3)"
    and "\<exists> b0. Mapping.lookup (store_bytes m off (flatten_u64 v)) (Suc (Suc (Suc (Suc off)))) = Some (Byte b0)"
    and "\<exists> b1. Mapping.lookup (store_bytes m off (flatten_u64 v)) (Suc (Suc (Suc (Suc (Suc off))))) = Some (Byte b1)"
    and "\<exists> b2. Mapping.lookup (store_bytes m off (flatten_u64 v)) (Suc (Suc (Suc (Suc (Suc (Suc off)))))) = Some (Byte b2)"
    and "\<exists> b3. Mapping.lookup (store_bytes m off (flatten_u64 v)) (Suc (Suc (Suc (Suc (Suc (Suc (Suc off))))))) = Some (Byte b3)"
proof -
  show "off \<in> Mapping.keys (store_bytes m off (flatten_u64 v))"
    by (metis (no_types, opaque_lifting) domIff handy_if_lemma keys_dom_lookup 
        stored_bytes_instant_correctness u64_split_realise)
next
  show "Suc off \<in> Mapping.keys (store_bytes m off (flatten_u64 v))" 
    by (metis (mono_tags, opaque_lifting) domIff u64_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc off) \<in> Mapping.keys (store_bytes m off (flatten_u64 v))"
    by (metis (mono_tags, opaque_lifting) domIff u64_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc (Suc off)) \<in> Mapping.keys (store_bytes m off (flatten_u64 v))"
    by (metis (mono_tags, opaque_lifting) domIff u64_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc (Suc (Suc off))) \<in> Mapping.keys (store_bytes m off (flatten_u64 v))"
    by (metis (mono_tags, opaque_lifting) domIff u64_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc (Suc (Suc (Suc off)))) \<in> Mapping.keys (store_bytes m off (flatten_u64 v))"
    by (metis (mono_tags, opaque_lifting) domIff u64_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc (Suc (Suc (Suc (Suc off))))) \<in> Mapping.keys (store_bytes m off (flatten_u64 v))"
    by (metis (mono_tags, opaque_lifting) domIff u64_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc (Suc (Suc (Suc (Suc (Suc off)))))) \<in> Mapping.keys (store_bytes m off (flatten_u64 v))"
    by (metis (mono_tags, opaque_lifting) domIff u64_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b0. Mapping.lookup (store_bytes m off (flatten_u64 v)) off = Some (Byte b0)"
    by (metis u64_split_realise stored_bytes_instant_correctness)
next
  show "\<exists> b1. Mapping.lookup (store_bytes m off (flatten_u64 v)) (Suc off) = Some (Byte b1)"
    by (metis u64_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b2. Mapping.lookup (store_bytes m off (flatten_u64 v)) (Suc (Suc off)) = Some (Byte b2)"
    by (metis u64_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b3. Mapping.lookup (store_bytes m off (flatten_u64 v)) (Suc (Suc (Suc off))) = Some (Byte b3)" 
    by (metis u64_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b0. Mapping.lookup (store_bytes m off (flatten_u64 v)) (Suc (Suc (Suc (Suc off)))) = Some (Byte b0)"
    by (metis u64_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next 
  show"\<exists> b1. Mapping.lookup (store_bytes m off (flatten_u64 v)) (Suc (Suc (Suc (Suc (Suc off))))) = Some (Byte b1)"
    by (metis u64_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b2. Mapping.lookup (store_bytes m off (flatten_u64 v)) (Suc (Suc (Suc (Suc (Suc (Suc off)))))) = Some (Byte b2)"
    by (metis u64_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next 
  show "\<exists> b3. Mapping.lookup (store_bytes m off (flatten_u64 v)) (Suc (Suc (Suc (Suc (Suc (Suc (Suc off))))))) = Some (Byte b3)"
    by (metis u64_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
qed

lemma store_bytes_s64:
  shows "off \<in> Mapping.keys (store_bytes m off (flatten_s64 v))"
    and "Suc off \<in> Mapping.keys (store_bytes m off (flatten_s64 v))"
    and "Suc (Suc off) \<in> Mapping.keys (store_bytes m off (flatten_s64 v))"
    and "Suc (Suc (Suc off)) \<in> Mapping.keys (store_bytes m off (flatten_s64 v))"
    and "Suc (Suc (Suc (Suc off))) \<in> Mapping.keys (store_bytes m off (flatten_s64 v))"
    and "Suc (Suc (Suc (Suc (Suc off)))) \<in> Mapping.keys (store_bytes m off (flatten_s64 v))"
    and "Suc (Suc (Suc (Suc (Suc (Suc off))))) \<in> Mapping.keys (store_bytes m off (flatten_s64 v))"
    and "Suc (Suc (Suc (Suc (Suc (Suc (Suc off)))))) \<in> Mapping.keys (store_bytes m off (flatten_s64 v))"
    and "\<exists> b0. Mapping.lookup (store_bytes m off (flatten_s64 v)) off = Some (Byte b0)"
    and "\<exists> b1. Mapping.lookup (store_bytes m off (flatten_s64 v)) (Suc off) = Some (Byte b1)"
    and "\<exists> b2. Mapping.lookup (store_bytes m off (flatten_s64 v)) (Suc (Suc off)) = Some (Byte b2)"
    and "\<exists> b3. Mapping.lookup (store_bytes m off (flatten_s64 v)) (Suc (Suc (Suc off))) = Some (Byte b3)"
    and "\<exists> b0. Mapping.lookup (store_bytes m off (flatten_s64 v)) (Suc (Suc (Suc (Suc off)))) = Some (Byte b0)"
    and "\<exists> b1. Mapping.lookup (store_bytes m off (flatten_s64 v)) (Suc (Suc (Suc (Suc (Suc off))))) = Some (Byte b1)"
    and "\<exists> b2. Mapping.lookup (store_bytes m off (flatten_s64 v)) (Suc (Suc (Suc (Suc (Suc (Suc off)))))) = Some (Byte b2)"
    and "\<exists> b3. Mapping.lookup (store_bytes m off (flatten_s64 v)) (Suc (Suc (Suc (Suc (Suc (Suc (Suc off))))))) = Some (Byte b3)"
proof -
  show "off \<in> Mapping.keys (store_bytes m off (flatten_s64 v))"
    by (metis (no_types, opaque_lifting) domIff handy_if_lemma keys_dom_lookup 
        stored_bytes_instant_correctness s64_split_realise)
next
  show "Suc off \<in> Mapping.keys (store_bytes m off (flatten_s64 v))" 
    by (metis (mono_tags, opaque_lifting) domIff s64_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc off) \<in> Mapping.keys (store_bytes m off (flatten_s64 v))"
    by (metis (mono_tags, opaque_lifting) domIff s64_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc (Suc off)) \<in> Mapping.keys (store_bytes m off (flatten_s64 v))"
    by (metis (mono_tags, opaque_lifting) domIff s64_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc (Suc (Suc off))) \<in> Mapping.keys (store_bytes m off (flatten_s64 v))"
    by (metis (mono_tags, opaque_lifting) domIff s64_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc (Suc (Suc (Suc off)))) \<in> Mapping.keys (store_bytes m off (flatten_s64 v))"
    by (metis (mono_tags, opaque_lifting) domIff s64_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc (Suc (Suc (Suc (Suc off))))) \<in> Mapping.keys (store_bytes m off (flatten_s64 v))"
    by (metis (mono_tags, opaque_lifting) domIff s64_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "Suc (Suc (Suc (Suc (Suc (Suc (Suc off)))))) \<in> Mapping.keys (store_bytes m off (flatten_s64 v))"
    by (metis (mono_tags, opaque_lifting) domIff s64_split_realise handy_if_lemma keys_dom_lookup 
        store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b0. Mapping.lookup (store_bytes m off (flatten_s64 v)) off = Some (Byte b0)"
    by (metis s64_split_realise stored_bytes_instant_correctness)
next
  show "\<exists> b1. Mapping.lookup (store_bytes m off (flatten_s64 v)) (Suc off) = Some (Byte b1)"
    by (metis s64_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b2. Mapping.lookup (store_bytes m off (flatten_s64 v)) (Suc (Suc off)) = Some (Byte b2)"
    by (metis s64_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b3. Mapping.lookup (store_bytes m off (flatten_s64 v)) (Suc (Suc (Suc off))) = Some (Byte b3)" 
    by (metis s64_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b0. Mapping.lookup (store_bytes m off (flatten_s64 v)) (Suc (Suc (Suc (Suc off)))) = Some (Byte b0)"
    by (metis s64_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next 
  show"\<exists> b1. Mapping.lookup (store_bytes m off (flatten_s64 v)) (Suc (Suc (Suc (Suc (Suc off))))) = Some (Byte b1)"
    by (metis s64_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next
  show "\<exists> b2. Mapping.lookup (store_bytes m off (flatten_s64 v)) (Suc (Suc (Suc (Suc (Suc (Suc off)))))) = Some (Byte b2)"
    by (metis s64_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
next 
  show "\<exists> b3. Mapping.lookup (store_bytes m off (flatten_s64 v)) (Suc (Suc (Suc (Suc (Suc (Suc (Suc off))))))) = Some (Byte b3)"
    by (metis s64_split_realise store_bytes.simps(2) stored_bytes_instant_correctness)
qed

corollary u16_store_bytes_imp_is_contiguous_bytes:
  "is_contiguous_bytes (store_bytes m off (u16_split v)) off 2"
  by (metis One_nat_def Suc_1 is_contiguous_bytes.simps(1) is_contiguous_bytes.simps(2) 
      memval_memcap_not_byte option.sel store_bytes_u16)

corollary s16_store_bytes_imp_is_contiguous_bytes:
  "is_contiguous_bytes (store_bytes m off (s16_split v)) off 2"
  by (metis One_nat_def Suc_1 is_contiguous_bytes.simps(1) is_contiguous_bytes.simps(2) 
      memval_memcap_not_byte option.sel store_bytes_s16)

corollary u32_store_bytes_imp_is_contiguous_bytes:
  "is_contiguous_bytes (store_bytes m off (flatten_u32 v)) off 4" 
  by(simp add: numeral_4_eq_4, safe) 
    (simp add: store_bytes_u32, metis memval_memcap_not_byte option.sel store_bytes_u32)+ 

corollary s32_store_bytes_imp_is_contiguous_bytes:
  "is_contiguous_bytes (store_bytes m off (flatten_s32 v)) off 4" 
  by(simp add: numeral_4_eq_4, safe) 
    (simp add: store_bytes_s32, metis memval_memcap_not_byte option.sel store_bytes_s32)+

corollary u64_store_bytes_imp_is_contiguous_bytes:
  "is_contiguous_bytes (store_bytes m off (flatten_u64 v)) off 8" 
  by(simp add: numeral_8_eq_8, safe) 
    (simp add: store_bytes_u64, metis memval_memcap_not_byte option.sel store_bytes_u64)+ 

corollary s64_store_bytes_imp_is_contiguous_bytes:
  "is_contiguous_bytes (store_bytes m off (flatten_s64 v)) off 8" 
  by(simp add: numeral_8_eq_8, safe) 
    (simp add: store_bytes_s64, metis memval_memcap_not_byte option.sel store_bytes_s64)+ 

lemma stored_tval_contiguous_bytes:
  assumes "val \<noteq> Undef"
    and "\<forall> v. val \<noteq> Cap_v v"
    and "\<forall> v n. val \<noteq> Cap_v_frag v n"
  shows "is_contiguous_bytes (content (store_tval obj off val)) off |memval_type val|\<^sub>\<tau>"
  unfolding sizeof_def
  by (simp add: assms store_tval_def memval_is_byte_def split: ccval.split) (presburger add: 
      s16_store_bytes_imp_is_contiguous_bytes s32_store_bytes_imp_is_contiguous_bytes 
      s64_store_bytes_imp_is_contiguous_bytes u16_store_bytes_imp_is_contiguous_bytes 
      u32_store_bytes_imp_is_contiguous_bytes u64_store_bytes_imp_is_contiguous_bytes)

lemma suc_of_32: 
  "32 = Suc 31"
  by simp

lemma store_cap_correct_dom:
  shows "off      \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 1  \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 2  \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 3  \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 4  \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 5  \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 6  \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 7  \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 8  \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 9  \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 10 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 11 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 12 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 13 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 14 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 15 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 16 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 17 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 18 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 19 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 20 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 21 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 22 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 23 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 24 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 25 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 26 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 27 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 28 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 29 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 30 \<in> Mapping.keys (store_cap m off cap 32)"
    and "off + 31 \<in> Mapping.keys (store_cap m off cap 32)" 
proof - qed (simp add: suc_of_32 domIff eval_nat_numeral(3) numeral_Bit0)+

lemma store_cap_correct_val:
  shows "Mapping.lookup (store_cap m off cap 32) off = 
         Some (ACap (mem_capability.truncate cap) 31)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 1) = 
         Some (ACap (mem_capability.truncate cap) 30)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 2) = 
         Some (ACap (mem_capability.truncate cap) 29)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 3) = 
         Some (ACap (mem_capability.truncate cap) 28)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 4) = 
         Some (ACap (mem_capability.truncate cap) 27)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 5) = 
         Some (ACap (mem_capability.truncate cap) 26)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 6) = 
         Some (ACap (mem_capability.truncate cap) 25)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 7) = 
         Some (ACap (mem_capability.truncate cap) 24)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 8) = 
         Some (ACap (mem_capability.truncate cap) 23)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 9) = 
         Some (ACap (mem_capability.truncate cap) 22)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 10) = 
         Some (ACap (mem_capability.truncate cap) 21)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 11) = 
         Some (ACap (mem_capability.truncate cap) 20)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 12) = 
         Some (ACap (mem_capability.truncate cap) 19)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 13) = 
         Some (ACap (mem_capability.truncate cap) 18)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 14) = 
         Some (ACap (mem_capability.truncate cap) 17)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 15) = 
         Some (ACap (mem_capability.truncate cap) 16)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 16) = 
         Some (ACap (mem_capability.truncate cap) 15)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 17) = 
         Some (ACap (mem_capability.truncate cap) 14)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 18) = 
         Some (ACap (mem_capability.truncate cap) 13)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 19) = 
         Some (ACap (mem_capability.truncate cap) 12)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 20) = 
         Some (ACap (mem_capability.truncate cap) 11)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 21) = 
         Some (ACap (mem_capability.truncate cap) 10)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 22) = 
         Some (ACap (mem_capability.truncate cap) 9)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 23) = 
         Some (ACap (mem_capability.truncate cap) 8)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 24) = 
         Some (ACap (mem_capability.truncate cap) 7)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 25) = 
         Some (ACap (mem_capability.truncate cap) 6)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 26) = 
         Some (ACap (mem_capability.truncate cap) 5)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 27) = 
         Some (ACap (mem_capability.truncate cap) 4)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 28) = 
         Some (ACap (mem_capability.truncate cap) 3)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 29) = 
         Some (ACap (mem_capability.truncate cap) 2)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 30) = 
         Some (ACap (mem_capability.truncate cap) 1)"
    and "Mapping.lookup (store_cap m off cap 32) (off + 31) = 
         Some (ACap (mem_capability.truncate cap) 0)"
proof - qed (simp add: stored_cap_instant_correctness suc_of_32 eval_nat_numeral(3) numeral_Bit0)+

corollary store_cap_imp_is_contiguous_cap:
  "is_contiguous_cap (store_cap m off cap 32) (mem_capability.truncate cap) off 32"
  by (simp add: eval_nat_numeral(3) numeral_Bit0, insert memval_byte_not_memcap, blast)

lemma stored_tval_is_cap:
  assumes "\<exists> v. val = Cap_v v"
  shows "is_cap (content (store_tval obj off val)) off"
  apply (simp add: assms store_tval_def split: ccval.split)
  apply (safe; ((insert assms, blast)+)?)
   apply (metis domIff keys_dom_lookup less_imp_Suc_add option.discI sizeof_nonzero 
      stored_cap_instant_correctness)
  apply (metis memval_byte_not_memcap not0_implies_Suc not_less_eq option.sel sizeof_nonzero 
      stored_cap_instant_correctness zero_less_Suc)
   apply (simp add: sizeof_def store_cap_correct_dom(1))
  apply (metis Some_to_the cctype.simps(81) memval_byte_not_memcap sizeof_def store_cap_correct_val(1))
  done

lemma stored_tval_contiguous_cap:
  assumes "val = Cap_v cap"
  shows "is_contiguous_cap (content (store_tval obj off val)) (mem_capability.truncate cap) off |memval_type val|\<^sub>\<tau>"
  using assms store_tval_def
  by (simp add: sizeof_def store_cap_imp_is_contiguous_cap)

lemma decode_encoded_u16_in_mem:
  "cat_u16 (retrieve_bytes (content (store_tval obj off (Uint16_v x3))) off |Uint16|\<^sub>\<tau>) = x3"
  apply (clarsimp simp add: sizeof_def store_tval_def eval_nat_numeral(3))
  apply (clarsimp simp add: numeral_Bit0)
  apply (subgoal_tac "of_byte (the (Mapping.lookup (store_bytes (content obj) off (u16_split x3)) off)) = (u16_split x3) ! 0 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (u16_split x3)) (Suc off))) = (u16_split x3) ! 1")
   apply (metis cat_flatten_u16_eq list_length_2_realise memval.sel(1) option.sel store_bytes.simps(2) 
      stored_bytes_instant_correctness u16_split_length)
  apply safe
   apply (smt (verit, best) Some_to_the length_nth_simps(3) memval.sel(1) 
      stored_bytes_instant_correctness u16_split_realise)
  apply (metis One_nat_def length_nth_simps(3) memval.sel(1) nth_Cons_Suc option.sel 
      store_bytes.simps(2) stored_bytes_instant_correctness u16_split_realise)
  done

lemma decode_encoded_s16_in_mem: 
  "cat_s16 (retrieve_bytes (content (store_tval obj off (Sint16_v x4))) off |Sint16|\<^sub>\<tau>) = x4"
  apply (clarsimp simp add: sizeof_def store_tval_def eval_nat_numeral(3))
  apply (clarsimp simp add: numeral_Bit0)
  apply (subgoal_tac "of_byte (the (Mapping.lookup (store_bytes (content obj) off (s16_split x4)) off)) = (s16_split x4) ! 0 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (s16_split x4)) (Suc off))) = (s16_split x4) ! 1")
   apply (metis cat_flatten_s16_eq list_length_2_realise memval.sel(1) option.sel store_bytes.simps(2) 
      stored_bytes_instant_correctness flatten_s16_length)
  apply safe
   apply (smt (verit, best) Some_to_the length_nth_simps(3) memval.sel(1) 
      stored_bytes_instant_correctness s16_split_realise)
  apply (metis One_nat_def flatten_s16_length list_length_2_realise memval.sel(1) nth_Cons_0 
      nth_Cons_Suc option.sel store_bytes.simps(2) stored_bytes_instant_correctness)
  done

lemma decode_encoded_u32_in_mem:
  "cat_u32 (retrieve_bytes (content (store_tval obj off (Uint32_v x5))) off |Uint32|\<^sub>\<tau>) = x5"
  apply (clarsimp simp add: sizeof_def store_tval_def eval_nat_numeral(3))
  apply (clarsimp simp add: numeral_Bit0)
  apply (subgoal_tac "of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_u32 x5)) off)) = (flatten_u32 x5) ! 0 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_u32 x5)) (Suc off))) = (flatten_u32 x5) ! 1 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_u32 x5)) (Suc (Suc off)))) = (flatten_u32 x5) ! 2 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_u32 x5)) (Suc (Suc (Suc off))))) = (flatten_u32 x5) ! 3")
   apply (smt (verit, del_insts) One_nat_def Suc_1 eval_nat_numeral(3) length_nth_simps(3) 
      length_nth_simps(4) u32_split_realise word_rcat_rsplit)
  apply safe
     apply (metis length_nth_simps(3) memval.sel(1) option.sel stored_bytes_instant_correctness 
      u32_split_realise)
    apply (metis One_nat_def length_nth_simps(3) length_nth_simps(4) memval.sel(1) option.sel 
      store_bytes.simps(2) stored_bytes_instant_correctness u32_split_realise)
   apply (metis One_nat_def Suc_1 length_nth_simps(3) length_nth_simps(4) memval.sel(1) option.sel 
      store_bytes.simps(2) stored_bytes_instant_correctness u32_split_realise)
  apply (metis Some_to_the length_nth_simps(3) length_nth_simps(4) memval.sel(1) numeral_3_eq_3 
      store_bytes.simps(2) stored_bytes_instant_correctness u32_split_realise)
  done

lemma decode_encoded_s32_in_mem:
  "cat_s32 (retrieve_bytes (content (store_tval obj off (Sint32_v x6))) off |Sint32|\<^sub>\<tau>) = x6"
  apply (clarsimp simp add: sizeof_def store_tval_def eval_nat_numeral(3))
  apply (clarsimp simp add: numeral_Bit0)
  apply (subgoal_tac "of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_s32 x6)) off)) = (flatten_s32 x6) ! 0 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_s32 x6)) (Suc off))) = (flatten_s32 x6) ! 1 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_s32 x6)) (Suc (Suc off)))) = (flatten_s32 x6) ! 2 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_s32 x6)) (Suc (Suc (Suc off))))) = (flatten_s32 x6) ! 3")
   apply (smt (verit, del_insts) One_nat_def Suc_1 eval_nat_numeral(3) length_nth_simps(3) 
      length_nth_simps(4) s32_split_realise word_rcat_rsplit)
  apply safe
     apply (metis length_nth_simps(3) memval.sel(1) option.sel stored_bytes_instant_correctness 
      s32_split_realise)
    apply (metis One_nat_def length_nth_simps(3) length_nth_simps(4) memval.sel(1) option.sel 
      store_bytes.simps(2) stored_bytes_instant_correctness s32_split_realise)
   apply (metis One_nat_def Suc_1 length_nth_simps(3) length_nth_simps(4) memval.sel(1) option.sel 
      store_bytes.simps(2) stored_bytes_instant_correctness s32_split_realise)
  apply (metis Some_to_the length_nth_simps(3) length_nth_simps(4) memval.sel(1) numeral_3_eq_3 
      store_bytes.simps(2) stored_bytes_instant_correctness s32_split_realise)
  done

lemma cat_flatten_u64_contents_eq:
  "cat_u64 [flatten_u64 vs ! 0, flatten_u64 vs ! 1, flatten_u64 vs ! 2, flatten_u64 vs ! 3,
            flatten_u64 vs ! 4, flatten_u64 vs ! 5, flatten_u64 vs ! 6, flatten_u64 vs ! 7] = vs"
  apply clarsimp
  apply (insert u64_split_realise[where?v=vs])
  apply safe
  apply (smt (verit, best) One_nat_def Suc_1 add.commute add_Suc_right eval_nat_numeral(3) 
      length_nth_simps(3) length_nth_simps(4) numeral_4_eq_4 numeral_Bit0 word_rcat_rsplit)
  done

lemma cat_flatten_s64_contents_eq:
  "cat_s64 [flatten_s64 vs ! 0, flatten_s64 vs ! 1, flatten_s64 vs ! 2, flatten_s64 vs ! 3,
            flatten_s64 vs ! 4, flatten_s64 vs ! 5, flatten_s64 vs ! 6, flatten_s64 vs ! 7] = vs"
  apply clarsimp
  apply (insert s64_split_realise[where?v=vs])
  apply safe
  apply (smt (verit, best) One_nat_def Suc_1 add.commute add_Suc_right eval_nat_numeral(3) 
      length_nth_simps(3) length_nth_simps(4) numeral_4_eq_4 numeral_Bit0 word_rcat_rsplit)
  done


lemma decode_encoded_u64_in_mem:
  "cat_u64 (retrieve_bytes (content (store_tval obj off (Uint64_v x7))) off |Uint64|\<^sub>\<tau>) = x7"
  apply (clarsimp simp add: sizeof_def store_tval_def eval_nat_numeral(3))
  apply (clarsimp simp add: numeral_Bit0)
  apply (subgoal_tac "of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_u64 x7)) off)) = (flatten_u64 x7) ! 0 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_u64 x7)) (Suc off))) = (flatten_u64 x7) ! 1 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_u64 x7)) (Suc (Suc off)))) = (flatten_u64 x7) ! 2 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_u64 x7)) (Suc (Suc (Suc off))))) = (flatten_u64 x7) ! 3 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_u64 x7)) (Suc (Suc (Suc (Suc off)))))) = (flatten_u64 x7) ! 4 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_u64 x7)) (Suc (Suc (Suc (Suc (Suc off))))))) = (flatten_u64 x7) ! 5 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_u64 x7)) (Suc (Suc (Suc (Suc (Suc (Suc off)))))))) = (flatten_u64 x7) ! 6 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_u64 x7)) (Suc (Suc (Suc (Suc (Suc (Suc (Suc off))))))))) = (flatten_u64 x7) ! 7")
   apply (presburger add: cat_flatten_u64_contents_eq)
  apply (smt (verit, best) length_nth_simps(3) length_nth_simps(4) memval.sel(1) option.sel One_nat_def
      numeral_2_eq_2 numeral_3_eq_3 numeral_4_eq_4 numeral_5_eq_5 numeral_6_eq_6 numeral_7_eq_7
      store_bytes.simps(2) stored_bytes_instant_correctness u64_split_realise)
  done

lemma decode_encoded_s64_in_mem:
  "cat_s64 (retrieve_bytes (content (store_tval obj off (Sint64_v x8))) off |Sint64|\<^sub>\<tau>) = x8"
  apply (clarsimp simp add: sizeof_def store_tval_def eval_nat_numeral(3))
  apply (clarsimp simp add: numeral_Bit0)
  apply (subgoal_tac "of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_s64 x8)) off)) = (flatten_s64 x8) ! 0 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_s64 x8)) (Suc off))) = (flatten_s64 x8) ! 1 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_s64 x8)) (Suc (Suc off)))) = (flatten_s64 x8) ! 2 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_s64 x8)) (Suc (Suc (Suc off))))) = (flatten_s64 x8) ! 3 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_s64 x8)) (Suc (Suc (Suc (Suc off)))))) = (flatten_s64 x8) ! 4 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_s64 x8)) (Suc (Suc (Suc (Suc (Suc off))))))) = (flatten_s64 x8) ! 5 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_s64 x8)) (Suc (Suc (Suc (Suc (Suc (Suc off)))))))) = (flatten_s64 x8) ! 6 \<and>
                      of_byte (the (Mapping.lookup (store_bytes (content obj) off (flatten_s64 x8)) (Suc (Suc (Suc (Suc (Suc (Suc (Suc off))))))))) = (flatten_s64 x8) ! 7")
   apply (presburger add: cat_flatten_s64_contents_eq)
  apply (smt (verit, best) length_nth_simps(3) length_nth_simps(4) memval.sel(1) option.sel One_nat_def
      numeral_2_eq_2 numeral_3_eq_3 numeral_4_eq_4 numeral_5_eq_5 numeral_6_eq_6 numeral_7_eq_7
      store_bytes.simps(2) stored_bytes_instant_correctness s64_split_realise)
  done

lemma retrieve_stored_tval_cap:
  assumes "val = Cap_v v"
  shows "retrieve_tval (store_tval obj off val) off (memval_type val) True = val"
  apply (clarsimp simp add: assms)
  apply (unfold retrieve_tval_def; clarsimp simp add: stored_tval_contiguous_cap; safe)
                     apply (metis is_contiguous_bytes.simps(2) less_numeral_extra(3) not0_implies_Suc sizeof_nonzero)
                    apply (subgoal_tac "is_contiguous_cap (content (store_tval obj off (Cap_v v))) 
                        (get_cap (content (store_tval obj off (Cap_v v))) off) off |Cap|\<^sub>\<tau>")
                     apply clarsimp 
                     apply (unfold store_tval_def get_cap_def sizeof_def)[1]
                     apply clarsimp 
                     apply (subst suc_of_32) 
                     apply (simp only: stored_cap_instant_correctness)
                     apply simp
                     apply (unfold mem_capability.extend_def mem_capability.truncate_def, clarsimp)[1]
                    apply (metis cctype.simps(81) get_cap_def is_contiguous_cap.simps(2) memval_size_cap sizeof_def 
      stored_tval_contiguous_cap suc_of_32)
                   apply (insert stored_tval_is_cap, force)[1]
                  apply (insert stored_tval_is_cap, force)[1]
                 apply (insert stored_tval_is_cap, force)[1]
                apply (insert stored_tval_is_cap, force)[1]
               apply (metis is_contiguous_bytes.simps(2) less_numeral_extra(3) not0_implies_Suc sizeof_nonzero)
              apply (subgoal_tac "is_contiguous_cap (content (store_tval obj off (Cap_v v))) 
                        (get_cap (content (store_tval obj off (Cap_v v))) off) off |Cap|\<^sub>\<tau>")
               apply clarsimp 
               apply (unfold store_tval_def get_cap_def sizeof_def)[1]
               apply clarsimp 
               apply (subst suc_of_32) 
               apply (simp only: stored_cap_instant_correctness)
               apply simp
               apply (unfold mem_capability.extend_def mem_capability.truncate_def, clarsimp)[1]
              apply (metis cctype.simps(81) get_cap_def is_contiguous_cap.simps(2) memval_size_cap sizeof_def 
      stored_tval_contiguous_cap suc_of_32)
             apply (insert stored_tval_is_cap, force)[1]
            apply (insert stored_tval_is_cap, force)[1]
           apply (metis cctype.simps(81) is_contiguous_bytes.simps(2) sizeof_def suc_of_32)
          apply (subgoal_tac "is_contiguous_cap (content (store_tval obj off (Cap_v v))) 
                        (get_cap (content (store_tval obj off (Cap_v v))) off) off |Cap|\<^sub>\<tau>")
           apply clarsimp 
           apply (unfold store_tval_def get_cap_def sizeof_def)[1]
           apply clarsimp 
           apply (subst suc_of_32) 
           apply (simp only: stored_cap_instant_correctness)
           apply simp
           apply (unfold mem_capability.extend_def mem_capability.truncate_def, clarsimp)[1]
          apply (metis cctype.simps(81) get_cap_def is_contiguous_cap.simps(2) memval_size_cap sizeof_def 
      stored_tval_contiguous_cap suc_of_32)
         apply (insert stored_tval_is_cap, force)[1]
        apply (insert stored_tval_is_cap, force)[1]
       apply (insert stored_tval_is_cap, force)[1]
      apply (insert stored_tval_is_cap, force)[1]
     apply (metis gr_implies_not_zero is_contiguous_bytes.simps(2) old.nat.exhaust sizeof_nonzero)
    apply (subgoal_tac "is_contiguous_cap (content (store_tval obj off (Cap_v v))) 
                        (get_cap (content (store_tval obj off (Cap_v v))) off) off |Cap|\<^sub>\<tau>")
     apply clarsimp 
     apply (unfold store_tval_def get_cap_def sizeof_def)[1]
     apply clarsimp 
     apply (subst suc_of_32) 
     apply (simp only: stored_cap_instant_correctness)
     apply simp
     apply (unfold mem_capability.extend_def mem_capability.truncate_def, clarsimp)[1]
    apply (metis cctype.simps(81) get_cap_def is_contiguous_cap.simps(2) memval_size_cap sizeof_def 
      stored_tval_contiguous_cap suc_of_32)
   apply (insert stored_tval_is_cap, force)[1]
  apply (insert stored_tval_is_cap, force)[1]
  done

lemma retrieve_stored_tval_cap_no_perm_cap_load:
  assumes "val = Cap_v v"
  shows "retrieve_tval (store_tval obj off val) off (memval_type val) False = (Cap_v (v \<lparr> tag := False \<rparr>))"
apply (clarsimp simp add: assms)
  apply (unfold retrieve_tval_def; clarsimp simp add: stored_tval_contiguous_cap; safe)
           apply (metis is_contiguous_bytes.simps(2) less_numeral_extra(3) not0_implies_Suc sizeof_nonzero)
          apply (subgoal_tac "is_contiguous_cap (content (store_tval obj off (Cap_v v))) 
                        (get_cap (content (store_tval obj off (Cap_v v))) off) off |Cap|\<^sub>\<tau>")
           apply clarsimp 
           apply (unfold store_tval_def get_cap_def sizeof_def)[1]
           apply clarsimp 
           apply (subst suc_of_32) 
           apply (simp only: stored_cap_instant_correctness)
           apply simp
           apply (unfold mem_capability.extend_def mem_capability.truncate_def, clarsimp)[1]
          apply (metis cctype.simps(81) get_cap_def is_contiguous_cap.simps(2) memval_size_cap sizeof_def 
      stored_tval_contiguous_cap suc_of_32)
         apply (insert stored_tval_is_cap, force)[1]
        apply (insert stored_tval_is_cap, force)[1]
       apply (insert stored_tval_is_cap, force)[1]
      apply (insert stored_tval_is_cap, force)[1]
     apply (metis is_contiguous_bytes.simps(2) less_numeral_extra(3) not0_implies_Suc sizeof_nonzero)
    apply (subgoal_tac "is_contiguous_cap (content (store_tval obj off (Cap_v v))) 
                        (get_cap (content (store_tval obj off (Cap_v v))) off) off |Cap|\<^sub>\<tau>")
     apply clarsimp 
     apply (unfold store_tval_def get_cap_def sizeof_def)[1]
     apply clarsimp 
     apply (subst suc_of_32) 
     apply (simp only: stored_cap_instant_correctness)
     apply simp
     apply (unfold mem_capability.extend_def mem_capability.truncate_def, clarsimp)[1]
    apply (metis cctype.simps(81) get_cap_def is_contiguous_cap.simps(2) memval_size_cap sizeof_def 
      stored_tval_contiguous_cap suc_of_32)
   apply (insert stored_tval_is_cap, force)[1]
  apply (insert stored_tval_is_cap, force)[1]
  done

lemma retrieve_stored_tval_u8:
  assumes "val = Uint8_v v"
  shows "retrieve_tval (store_tval obj off val) off (memval_type val) b = val"
  apply (clarsimp simp add: assms)
  apply (unfold retrieve_tval_def; clarsimp simp add: stored_tval_contiguous_bytes; safe)
       apply (simp add: sizeof_def) 
      apply (metis One_nat_def ccval.distinct(15) ccval.distinct(17) ccval.distinct(19) 
      is_contiguous_bytes.simps(2) memval_size_u8 stored_tval_contiguous_bytes)
     apply (simp add: sizeof_def)
    apply (metis cctype.simps(73) ccval.distinct(15) ccval.distinct(17) ccval.distinct(19) 
      memval_size_u8 sizeof_def stored_tval_contiguous_bytes)
   apply (clarsimp simp add: sizeof_def store_tval_def)
  apply (metis cctype.simps(73) ccval.distinct(15) ccval.distinct(17) ccval.distinct(19) 
      memval_size_u8 sizeof_def stored_tval_contiguous_bytes)
  done

lemma retrieve_stored_tval_s8:
  assumes "val = Sint8_v v"
  shows "retrieve_tval (store_tval obj off val) off (memval_type val) b = val"
  apply (clarsimp simp add: assms)
  apply (unfold retrieve_tval_def; clarsimp simp add: stored_tval_contiguous_bytes; safe)
       apply (simp add: sizeof_def) 
      apply (metis One_nat_def ccval.distinct(33) ccval.distinct(35) ccval.distinct(37) 
      is_contiguous_bytes.simps(2) memval_size_s8 stored_tval_contiguous_bytes)
     apply (simp add: sizeof_def)
    apply (metis cctype.simps(74) ccval.distinct(33) ccval.distinct(35) ccval.distinct(37) 
      memval_size_s8 sizeof_def stored_tval_contiguous_bytes)
   apply (clarsimp simp add: sizeof_def store_tval_def)
  apply (metis cctype.simps(74) ccval.distinct(33) ccval.distinct(35) ccval.distinct(37) 
      memval_size_s8 sizeof_def stored_tval_contiguous_bytes)
  done

lemma retrieve_stored_tval_u16:
  assumes "val = Uint16_v v"
  shows "retrieve_tval (store_tval obj off val) off (memval_type val) b = val"
  apply (clarsimp simp add: assms)
  apply (unfold retrieve_tval_def; clarsimp simp add: stored_tval_contiguous_bytes; safe) 
       apply (presburger add: decode_encoded_u16_in_mem)
      apply (metis ccval.distinct(49) ccval.distinct(51) ccval.distinct(53) 
      is_contiguous_bytes.simps(2) memval_size_u16 numeral_2_eq_2 stored_tval_contiguous_bytes)
     apply (presburger add: decode_encoded_u16_in_mem)
    apply (metis Suc_1 ccval.distinct(49) ccval.distinct(51) ccval.distinct(53) 
      is_contiguous_bytes.simps(2) memval_size_u16 stored_tval_contiguous_bytes)
   apply (presburger add: decode_encoded_u16_in_mem)
  apply (metis cctype.simps(75) ccval.distinct(49) ccval.distinct(51) ccval.distinct(53) 
      memval_size_u16 sizeof_def stored_tval_contiguous_bytes)
  done

lemma retrieve_stored_tval_s16:
  assumes "val = Sint16_v v"
  shows "retrieve_tval (store_tval obj off val) off (memval_type val) b = val"
  apply (clarsimp simp add: assms)
  apply (unfold retrieve_tval_def; clarsimp simp add: stored_tval_contiguous_bytes; safe) 
       apply (presburger add: decode_encoded_s16_in_mem)
      apply (metis ccval.distinct(63) ccval.distinct(65) ccval.distinct(67) is_contiguous_bytes.simps(2) 
      memval_size_s16 numeral_2_eq_2 stored_tval_contiguous_bytes)
     apply (presburger add: decode_encoded_s16_in_mem)
    apply (metis Suc_1 ccval.distinct(63) ccval.distinct(65) ccval.distinct(67) 
      is_contiguous_bytes.simps(2) memval_size_s16 stored_tval_contiguous_bytes)
   apply (presburger add: decode_encoded_s16_in_mem)
  apply (metis cctype.simps(76) ccval.distinct(63) ccval.distinct(65) ccval.distinct(67) 
      memval_size_s16 sizeof_def stored_tval_contiguous_bytes)
  done

lemma retrieve_stored_tval_u32:
  assumes "val = Uint32_v v"
  shows "retrieve_tval (store_tval obj off val) off (memval_type val) b = val"
  apply (clarsimp simp add: assms)
  apply (unfold retrieve_tval_def; clarsimp simp add: stored_tval_contiguous_bytes; safe) 
       apply (presburger add: decode_encoded_u32_in_mem)
      apply (metis ccval.distinct(75) ccval.distinct(77) ccval.distinct(79) is_contiguous_bytes.simps(2) 
      memval_size_u32 numeral_4_eq_4 stored_tval_contiguous_bytes)
     apply (presburger add: decode_encoded_u32_in_mem)
    apply (metis ccval.distinct(77) ccval.distinct(79) is_cap.elims(2) is_contiguous_bytes.simps(2) 
      memval_size_u32 numeral_4_eq_4 stored_tval_contiguous_bytes stored_tval_is_cap)
   apply (presburger add: decode_encoded_u32_in_mem)
  apply (metis cctype.simps(77) ccval.distinct(75) ccval.distinct(77) ccval.distinct(79) 
      memval_size_u32 sizeof_def stored_tval_contiguous_bytes)
  done

lemma retrieve_stored_tval_s32:
  assumes "val = Sint32_v v"
  shows "retrieve_tval (store_tval obj off val) off (memval_type val) b = val"
  apply (clarsimp simp add: assms)
  apply (unfold retrieve_tval_def; clarsimp simp add: stored_tval_contiguous_bytes; safe) 
       apply (presburger add: decode_encoded_s32_in_mem)
      apply (metis cctype.simps(78) ccval.distinct(85) ccval.distinct(87) ccval.distinct(89) 
      flatten_s32_length memval_size_s32_eq_word_split_len sizeof_def stored_tval_contiguous_bytes)
     apply (presburger add: decode_encoded_s32_in_mem)
    apply (metis ccval.distinct(85) ccval.distinct(87) ccval.distinct(89) is_contiguous_bytes.simps(2) 
      less_numeral_extra(3) not0_implies_Suc sizeof_nonzero stored_tval_contiguous_bytes) 
   apply (presburger add: decode_encoded_s32_in_mem)
  apply (metis cctype.simps(78) ccval.distinct(85) ccval.distinct(87) ccval.simps(100) 
      flatten_s32_length memval_size_s32_eq_word_split_len sizeof_def stored_tval_contiguous_bytes)
  done

lemma retrieve_stored_tval_u64:
  assumes "val = Uint64_v v"
  shows "retrieve_tval (store_tval obj off val) off (memval_type val) b = val"
  apply (clarsimp simp add: assms)
  apply (unfold retrieve_tval_def; clarsimp simp add: stored_tval_contiguous_bytes; safe) 
       apply (presburger add: decode_encoded_u64_in_mem)
      apply (metis cctype.simps(79) ccval.distinct(93) ccval.distinct(95) ccval.distinct(97) 
      memval_size_u64 sizeof_def stored_tval_contiguous_bytes)
     apply (presburger add: decode_encoded_u64_in_mem)
    apply (metis cctype.simps(79) ccval.distinct(95) ccval.distinct(97) is_cap.elims(1) 
      memval_size_u64 sizeof_def stored_tval_contiguous_bytes stored_tval_is_cap)
   apply (presburger add: decode_encoded_u64_in_mem)
  apply (metis cctype.simps(79) ccval.distinct(93) ccval.distinct(95) ccval.distinct(97) 
      memval_size_u64 sizeof_def stored_tval_contiguous_bytes) 
  done

lemma retrieve_stored_tval_s64:
  assumes "val = Sint64_v v"
  shows "retrieve_tval (store_tval obj off val) off (memval_type val) b = val"
  apply (clarsimp simp add: assms)
  apply (unfold retrieve_tval_def; clarsimp simp add: stored_tval_contiguous_bytes; safe) 
       apply (presburger add: decode_encoded_s64_in_mem)
      apply (metis cctype.simps(80) ccval.distinct(101) ccval.distinct(103) ccval.distinct(99) 
      memval_size_s64 sizeof_def stored_tval_contiguous_bytes)
     apply (presburger add: decode_encoded_s64_in_mem)
    apply (metis bot_nat_0.not_eq_extremum ccval.distinct(101) ccval.distinct(103) ccval.distinct(99) 
      is_contiguous_bytes.simps(2) list_decode.cases sizeof_nonzero stored_tval_contiguous_bytes) 
   apply (presburger add: decode_encoded_s64_in_mem)
  apply (metis cctype.simps(80) ccval.distinct(101) ccval.distinct(103) ccval.distinct(99) 
      memval_size_s64 sizeof_def stored_tval_contiguous_bytes)
  done

lemma memcap_truncate_extend_equiv:
  "mem_capability.extend (mem_capability.truncate c) \<lparr> tag = tag c \<rparr> = c"
  by (simp add: mem_capability.extend_def mem_capability.truncate_def)

corollary Acap_truncate_extend_equiv:
  "mem_capability.extend (of_cap (ACap (mem_capability.truncate c) n)) \<lparr> tag = tag c \<rparr> = c"
  by clarsimp (blast intro: memcap_truncate_extend_equiv)

lemma memcap_truncate_extend_gen:
  "mem_capability.extend (mem_capability.truncate c) \<lparr> tag = b \<rparr> = c \<lparr> tag := b \<rparr>"
  by (simp add: mem_capability.extend_def mem_capability.truncate_def)

corollary Acap_truncate_extend_gen:
  "mem_capability.extend (of_cap (ACap (mem_capability.truncate c) n)) \<lparr> tag = b \<rparr> = c \<lparr> tag := b \<rparr>"
  by clarsimp (blast intro: memcap_truncate_extend_gen)

lemma retrieve_stored_tval_cap_frag_1:
  assumes "val = Cap_v_frag c n"
    and aligned: "off mod |Cap|\<^sub>\<tau> = |Cap|\<^sub>\<tau> - 1 - n"
    and first: "off mod |Cap|\<^sub>\<tau> = 0"
  shows "retrieve_tval (store_tval obj off val) off (memval_type val) b = 
         Cap_v_frag (c \<lparr> tag := if b then tag c else False \<rparr>) n"
  by (clarsimp simp add: assms retrieve_tval_def store_tval_def) 
    (metis Acap_truncate_extend_gen Mapping.lookup_update One_nat_def Some_to_the aligned 
      cctype.simps(73) diff_diff_left diff_is_0_eq first get_cap_def is_contiguous_bytes.simps(2) 
      memcap_truncate_extend_equiv memval.sel(2) memval_byte_not_memcap plus_1_eq_Suc sizeof_def)

lemma retrieve_stored_tval_cap_frag_2:
  assumes misaligned: "off mod |Cap|\<^sub>\<tau> \<noteq> |Cap|\<^sub>\<tau> - 1 - n"
  shows "retrieve_tval (store_tval obj off (Cap_v_frag c n)) off (memval_type (Cap_v_frag c n)) b = 
         Cap_v_frag (c \<lparr> tag := False \<rparr>) n"
  apply (clarsimp simp add: misaligned)
  apply (simp add: retrieve_tval_def)
  apply safe
           apply (simp add: sizeof_def)
          apply (simp add: store_tval_def)
          apply (cases b; simp add: store_tval_def)
           apply safe
            apply (insert misaligned, force)[1]
           apply (simp add: get_cap_def memcap_truncate_extend_gen)
          apply (simp add: get_cap_def memcap_truncate_extend_gen)
         apply (simp add: store_tval_def)
        apply (simp add: store_tval_def)
       apply (simp add: store_tval_def memval_is_byte_def)
      apply (simp add: store_tval_def sizeof_def)
     apply (cases b; simp add: store_tval_def)
      apply safe
         apply (simp add: get_cap_def memcap_truncate_extend_gen)
        apply (presburger add: diff_Suc_eq_diff_pred misaligned)
       apply (simp add: get_cap_def memcap_truncate_extend_gen)
      apply (simp add: get_cap_def memcap_truncate_extend_gen)
     apply (simp add: get_cap_def memcap_truncate_extend_gen)
    apply (simp add: store_tval_def)
   apply (simp add: store_tval_def)
  apply (simp add: store_tval_def memval_is_byte_def)
  done

lemma retrieve_stored_tval_cap_frag_3:
  assumes aligned: "off mod |Cap|\<^sub>\<tau> = |Cap|\<^sub>\<tau> - 1 - n"
    and not_first: "|Cap|\<^sub>\<tau> - 1 - n \<noteq> 0"
    and unmapped_tag: "cap_offset off \<notin> Mapping.keys (tags obj)"
  shows "retrieve_tval (store_tval obj off (Cap_v_frag c n)) off (memval_type (Cap_v_frag c n)) b = 
         Cap_v_frag (c \<lparr> tag := False \<rparr>) n"
  apply (clarsimp simp add: aligned)
  apply (simp add: retrieve_tval_def)
  apply safe
           apply (simp add: sizeof_def)
          apply (cases b; simp add: store_tval_def get_cap_def)
           apply safe 
            apply (insert not_first, linarith)[1]
           apply (blast intro: memcap_truncate_extend_gen)
          apply (blast intro: memcap_truncate_extend_gen)
         apply (simp add: store_tval_def)
        apply (simp add: store_tval_def)
       apply (simp add: store_tval_def memval_is_byte_def)
      apply (simp add: store_tval_def)
      apply (simp add: sizeof_def)
     apply (cases b; simp add: store_tval_def get_cap_def)
      apply safe 
         apply (insert not_first, linarith)[1] 
        apply (insert unmapped_tag store_tval_def, force)[1]
       apply (blast intro: memcap_truncate_extend_gen)
      apply (blast intro: memcap_truncate_extend_gen)
      apply (blast intro: memcap_truncate_extend_gen)
    apply (simp add: store_tval_def)
   apply (simp add: store_tval_def)
  apply (simp add: store_tval_def memval_is_byte_def)
  done

lemma retrieve_stored_tval_cap_frag_4:
  assumes aligned: "off mod |Cap|\<^sub>\<tau> = |Cap|\<^sub>\<tau> - 1 - n"
    and not_first: "|Cap|\<^sub>\<tau> - 1 - n \<noteq> 0"
    and mapped_tag: "Mapping.lookup (tags obj) (cap_offset off) = Some b'"
  shows "retrieve_tval (store_tval obj off (Cap_v_frag c n)) off (memval_type (Cap_v_frag c n)) b = 
         Cap_v_frag (c \<lparr> tag := if b then b' else False \<rparr>) n"
  apply (clarsimp simp add: aligned)
  apply (simp add: retrieve_tval_def)
  apply safe
               apply (simp add: sizeof_def)
              apply (cases b; simp add: store_tval_def get_cap_def)
              apply safe 
               apply (insert not_first, linarith)[1] 
              apply (insert aligned, fastforce)[1]
             apply (insert aligned not_first, fastforce)[1]
            apply (simp add: store_tval_def get_cap_def memcap_truncate_extend_gen)
           apply (simp add: store_tval_def)
          apply (simp add: store_tval_def memval_is_byte_def)
         apply (simp add: store_tval_def memval_is_byte_def)
        apply (simp add: store_tval_def memval_is_byte_def) 
        apply (simp add: sizeof_def memval_byte_not_memcap memval_is_byte_def)
       apply (simp add: store_tval_def get_cap_def)
       apply safe 
          apply (insert not_first, linarith)[1] 
         apply (insert mapped_tag, force simp add: store_tval_def memcap_truncate_extend_gen)[1]
        apply (fastforce simp add: aligned)
       apply (smt (verit, ccfv_SIG) Nat.diff_diff_right Suc_diff_eq_diff_pred aligned diff_is_0_eq' 
      domIff keys_dom_lookup mapped_tag nat_le_linear not_first option.simps(3) sizeof_nonzero)
       apply (insert assms, simp add: store_tval_def memcap_truncate_extend_gen)[1]
      apply (insert assms, simp add: store_tval_def)[1] 
     apply (simp add: get_cap_def store_tval_def memcap_truncate_extend_gen)
    apply (simp add: store_tval_def memval_is_byte_def) 
   apply (simp add: store_tval_def)
  apply (simp add: store_tval_def memval_is_byte_def) 
  done

lemmas retrieve_stored_tval_prim = retrieve_stored_tval_u8 retrieve_stored_tval_s8
retrieve_stored_tval_u16 retrieve_stored_tval_s16
retrieve_stored_tval_u32 retrieve_stored_tval_s32
retrieve_stored_tval_u64 retrieve_stored_tval_s64

lemma retrieve_stored_tval_any_perm:
  assumes "val \<noteq> Undef"
    and "\<forall> v. val \<noteq> Cap_v v"
    and "\<forall> v n. val \<noteq> Cap_v_frag v n"
  shows "retrieve_tval (store_tval obj off val) off (memval_type val) b = val"
  by (clarsimp simp add: assms split: ccval.split) 
    (insert retrieve_stored_tval_prim[where ?obj=obj and ?off=off and ?val=val and ?b=b], fastforce)

lemma retrieve_stored_tval_with_perm_cap_load:
  assumes "val \<noteq> Undef"
    and "\<forall> v n. val \<noteq> Cap_v_frag v n"
  shows "retrieve_tval (store_tval obj off val) off (memval_type val) True = val"
  by (clarsimp simp add: assms split: ccval.split) 
    (insert retrieve_stored_tval_prim[where ?obj=obj and ?off=off and ?val=val and ?b=True]
      retrieve_stored_tval_cap[where ?obj=obj and ?off=off and ?val=val], simp)

definition store :: "heap \<Rightarrow> cap \<Rightarrow> block ccval \<Rightarrow> heap result"
  where
  "store h c v \<equiv> 
     if tag c = False then 
       Error (C2Err TagViolation)
     else if perm_store c = False then 
       Error (C2Err PermitStoreViolation)
     else if (case v of Cap_v cv \<Rightarrow> \<not> perm_cap_store c \<and> tag cv | _ \<Rightarrow> False) then 
       Error (C2Err PermitStoreCapViolation)
     else if (case v of Cap_v cv \<Rightarrow> \<not> perm_cap_store_local c \<and> tag cv \<and> \<not> perm_global cv | _ \<Rightarrow> False) then
       Error (C2Err PermitStoreLocalCapViolation)
     else if offset c + |memval_type v|\<^sub>\<tau> > base c + len c then 
       Error (C2Err LengthViolation)
     else if offset c < base c then 
       Error (C2Err LengthViolation)
     else if offset c mod |memval_type v|\<^sub>\<tau> \<noteq> 0 then 
       Error (C2Err BadAddressViolation)
     else if v = Undef then
       Error (LogicErr (Unhandled 0))
     else
       let obj = Mapping.lookup (heap_map h) (block_id c) in
      (case obj of None      \<Rightarrow> Error (LogicErr (MissingResource))
                 | Some cobj \<Rightarrow>
        (case cobj of Freed \<Rightarrow> Error (LogicErr (UseAfterFree))
                    | Map m \<Rightarrow> Success (h \<lparr> heap_map := Mapping.update 
                                                         (block_id c) 
                                                         (Map (store_tval m (offset c) v)) 
                                                         (heap_map h) \<rparr>)))"

lemma store_null_error:
  "store h NULL v = Error (C2Err TagViolation)" 
  unfolding store_def
  by simp

lemma store_false_tag:
  assumes "tag c = False"
  shows "store h c v = Error (C2Err TagViolation)"
  unfolding store_def
  using assms
  by presburger

lemma store_false_perm_store:
  assumes "tag c = True"
    and "perm_store c = False"
  shows "store h c v = Error (C2Err PermitStoreViolation)"
  unfolding store_def
  using assms 
  by presburger

lemma store_cap_false_perm_cap_store:
  assumes "tag c = True"
    and "perm_store c = True"
    and "perm_cap_store c = False"
    and "\<exists> cv. v = Cap_v cv \<and> tag cv = True"
  shows "store h c v = Error (C2Err PermitStoreCapViolation)"
  unfolding store_def  
  using assms
  by force

lemma store_cap_false_perm_cap_store_local:
    assumes "tag c = True"
    and "perm_store c = True"
    and "perm_cap_store c = True"
    and "perm_cap_store_local c = False"
    and "\<exists> cv. v = Cap_v cv \<and> tag cv = True \<and> perm_global cv = False"
  shows "store h c v = Error (C2Err PermitStoreLocalCapViolation)"
  unfolding store_def
  using assms
  by force
  
lemma store_bound_over:
  assumes "tag c = True"
    and "perm_store c = True"
    and "\<And> cv. \<lbrakk> v = Cap_v cv; tag cv \<rbrakk> \<Longrightarrow> perm_cap_store c \<and> (perm_cap_store_local c \<or> perm_global cv)"
    and "offset c + |memval_type v|\<^sub>\<tau> > base c + len c"
  shows "store h c v = Error (C2Err LengthViolation)"
  unfolding store_def
  using assms 
  by (clarsimp split: ccval.split) 

lemma store_bound_under:
  assumes "tag c = True"
    and "perm_store c = True"
    and "\<And> cv. \<lbrakk> v = Cap_v cv; tag cv \<rbrakk> \<Longrightarrow> perm_cap_store c \<and> (perm_cap_store_local c \<or> perm_global cv)"
    and "offset c + |memval_type v|\<^sub>\<tau> \<le> base c + len c"
    and "offset c < base c"
  shows "store h c v = Error (C2Err LengthViolation)"
  unfolding store_def
  using assms 
  by (clarsimp split: ccval.split)

lemma store_misaligned:
  assumes "tag c = True"
    and "perm_store c = True"
    and "\<And> cv. \<lbrakk> v = Cap_v cv; tag cv \<rbrakk> \<Longrightarrow> perm_cap_store c \<and> (perm_cap_store_local c \<or> perm_global cv)"
    and "offset c + |memval_type v|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |memval_type v|\<^sub>\<tau> \<noteq> 0"
  shows "store h c v = Error (C2Err BadAddressViolation)"
  unfolding store_def
  using assms
  by (clarsimp split: ccval.split)

lemma store_undef_val:
  assumes "tag c = True"
    and "perm_store c = True"
    and "\<And> cv. \<lbrakk> v = Cap_v cv; tag cv \<rbrakk> \<Longrightarrow> perm_cap_store c \<and> (perm_cap_store_local c \<or> perm_global cv)"
    and "offset c + |memval_type v|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |memval_type v|\<^sub>\<tau> = 0"
    and "v = Undef"
  shows "store h c v = Error (LogicErr (Unhandled 0))"
  unfolding store_def 
  using assms
  by auto

lemma store_nonexistant_obj:
  assumes "tag c = True"
    and "perm_store c = True"
    and "\<And> cv. \<lbrakk> v = Cap_v cv; tag cv \<rbrakk> \<Longrightarrow> perm_cap_store c \<and> (perm_cap_store_local c \<or> perm_global cv)"
    and "offset c + |memval_type v|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |memval_type v|\<^sub>\<tau> = 0"
    and "v \<noteq> Undef"
    and "Mapping.lookup (heap_map h) (block_id c) = None"
  shows "store h c v = Error (LogicErr MissingResource)"
  unfolding store_def
  using assms 
  by (clarsimp split: ccval.split)

lemma store_store_after_free:
  assumes "tag c = True"
    and "perm_store c = True"
    and "\<And> cv. \<lbrakk> v = Cap_v cv; tag cv \<rbrakk> \<Longrightarrow> perm_cap_store c \<and> (perm_cap_store_local c \<or> perm_global cv)"
    and "offset c + |memval_type v|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |memval_type v|\<^sub>\<tau> = 0"
    and "v \<noteq> Undef"
    and "Mapping.lookup (heap_map h) (block_id c) = Some Freed"
  shows "store h c v = Error (LogicErr UseAfterFree)"
  unfolding store_def
  using assms
  by (clarsimp split: ccval.split)

lemma store_success:
  assumes "tag c = True"
    and "perm_store c = True"
    and "\<And> cv. \<lbrakk> v = Cap_v cv; tag cv \<rbrakk> \<Longrightarrow> perm_cap_store c \<and> (perm_cap_store_local c \<or> perm_global cv)"
    and "offset c + |memval_type v|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |memval_type v|\<^sub>\<tau> = 0"
    and "v \<noteq> Undef"
    and "Mapping.lookup (heap_map h) (block_id c) = Some (Map m)"
  shows "\<exists> ret. store h c v = Success ret \<and>
                next_block ret = next_block h \<and>
                heap_map ret = Mapping.update (block_id c) (Map (store_tval m (offset c) v)) (heap_map h)"
  unfolding store_def
  using assms 
  by (clarsimp split: ccval.split)
  
lemma store_cond_hard_cap:
  assumes "store h c v = Success ret"
  shows "tag c = True"
    and "perm_store c = True"
    and "\<And> cv. \<lbrakk> v = Cap_v cv; tag cv \<rbrakk> \<Longrightarrow> perm_cap_store c \<and> (perm_cap_store_local c \<or> perm_global cv)"
    and "offset c + |memval_type v|\<^sub>\<tau> \<le> base c + len c"
    and "offset c \<ge> base c"
    and "offset c mod |memval_type v|\<^sub>\<tau> = 0"
proof -
  show "tag c = True"
    using assms unfolding store_def
    by (meson result.simps(4))
next
  show "perm_store c = True"
    using assms unfolding store_def
    by (meson result.simps(4))
next
  show "\<And> cv. \<lbrakk> v = Cap_v cv; tag cv \<rbrakk> \<Longrightarrow> perm_cap_store c \<and> (perm_cap_store_local c \<or> perm_global cv)"
    using assms unfolding store_def
    by (metis (no_types, lifting) assms result.simps(4) store_cap_false_perm_cap_store 
        store_cap_false_perm_cap_store_local)
next
  show "offset c + |memval_type v|\<^sub>\<tau> \<le> base c + len c"
    using assms unfolding store_def
    by (meson le_def result.simps(4))
next 
  show "offset c \<ge> base c"
    using assms unfolding store_def
    by (meson le_def result.simps(4))
next 
  show "offset c mod |memval_type v|\<^sub>\<tau> = 0"
    using assms unfolding store_def
    by (meson le_def result.simps(4))
qed

lemma store_cond_bytes:
  assumes "store h c val = Success h'"
    and "\<forall> x. val \<noteq> Cap_v x"
  shows "val \<noteq> Undef"
    and "\<exists> m. Mapping.lookup (heap_map h') (block_id c) = Some (Map m)"
  using store_cond_hard_cap[where ?h=h and ?c=c and ?v=val and ?ret=h', OF assms(1)] assms
  unfolding store_def
  by (simp split: ccval.split_asm; simp split: option.split_asm t.split_asm, fastforce?)+

lemma store_cond_cap:
  assumes "store h c val = Success h'"
    and "val = Cap_v v"
  shows "\<exists> m. Mapping.lookup (heap_map h') (block_id c) = Some (Map m)"
  using store_cond_hard_cap(1)[where ?h=h and ?c=c and ?v=val and ?ret=h', OF assms(1)]
    store_cond_hard_cap(2)[where ?h=h and ?c=c and ?v=val and ?ret=h', OF assms(1)]
    store_cond_hard_cap(3)[where ?h=h and ?c=c and ?v=val and ?ret=h' and ?cv=v, OF assms(1)]
    store_cond_hard_cap(4)[where ?h=h and ?c=c and ?v=val and ?ret=h', OF assms(1)]
    store_cond_hard_cap(5)[where ?h=h and ?c=c and ?v=val and ?ret=h', OF assms(1)]
    store_cond_hard_cap(6)[where ?h=h and ?c=c and ?v=val and ?ret=h', OF assms(1)]
    assms
  apply (simp split: ccval.split)
  apply (unfold store_def)
  apply clarsimp
  apply (subgoal_tac "\<not>(\<not> perm_cap_store c \<and> tag v) \<and> 
                      \<not>(\<not> perm_cap_store_local c \<and> tag v \<and> \<not> perm_global v)"; blast?)
   apply clarsimp
   apply (simp split: option.split_asm t.split_asm)
   apply force
  done

lemma store_cond_cap_frag:
  assumes "store h c val = Success h'"
    and "val = Cap_v_frag v n"
  shows "\<exists> m. Mapping.lookup (heap_map h') (block_id c) = Some (Map m)"
  using store_cond_hard_cap[where ?h=h and ?c=c and ?v=val and ?ret=h', OF assms(1)] assms
  unfolding store_def
  by (simp split: ccval.split_asm; simp split: option.split_asm t.split_asm, fastforce?)+

lemma load_after_store_prim:
  assumes "store h c val = Success h'"
    and "val \<noteq> Undef"
    and "\<forall> v. val \<noteq> Cap_v v"
    and "\<forall> v n. val \<noteq> Cap_v_frag v n"
    and "perm_load c = True"
  shows "load h' c (memval_type val) = Success val"
  using assms(1) store_cond_hard_cap[where ?h=h and ?c=c and ?v=val and ?ret=h', OF assms(1)]
  by (clarsimp, simp split: ccval.split add: assms; safe; clarsimp)
    (simp add: load_def assms split: option.split t.split; safe, (insert store_cond_bytes(2))[1], 
      blast, metis assms(3) option.sel store_cond_bytes(2) t.distinct(1),
      (insert assms(1), unfold store_def, simp split: option.split_asm t.split_asm, clarsimp)[1],
      (insert retrieve_stored_tval_prim)[1], simp)+

lemma load_after_store_cap:
  assumes "store h c (Cap_v v) = Success h'"
    and "perm_load c = True"
  shows "load h' c (memval_type (Cap_v v)) = Success (Cap_v (v \<lparr> tag := case perm_cap_load c of False => False | True => tag v \<rparr>))"
 using store_cond_hard_cap(1)[where ?h=h and ?c=c and ?v="Cap_v v" and ?ret=h', OF assms(1)]
    store_cond_hard_cap(2)[where ?h=h and ?c=c and ?v="Cap_v v" and ?ret=h', OF assms(1)] 
    store_cond_hard_cap(3)[where ?h=h and ?c=c and ?v="Cap_v v" and ?ret=h' and ?cv=v, OF assms(1) refl]
    store_cond_hard_cap(4)[where ?h=h and ?c=c and ?v="Cap_v v" and ?ret=h', OF assms(1)]
    store_cond_hard_cap(5)[where ?h=h and ?c=c and ?v="Cap_v v" and ?ret=h', OF assms(1)]
    store_cond_hard_cap(6)[where ?h=h and ?c=c and ?v="Cap_v v" and ?ret=h', OF assms(1)]
    assms
  apply (clarsimp, simp split: ccval.split; safe; clarsimp)
  apply (unfold load_def; clarsimp split: option.split)
  apply (simp split: t.split, safe)
  apply (blast dest: store_cond_cap)
   apply (metis option.sel store_cond_cap t.distinct(1))
  apply (unfold store_def, clarsimp)
  apply (subgoal_tac "\<not>(\<not> perm_cap_store c \<and> tag v) \<and> 
                      \<not>(\<not> perm_cap_store_local c \<and> tag v \<and> \<not> perm_global v)"; presburger?)
   apply clarsimp
   apply (simp split: option.split_asm t.split_asm)
  apply clarsimp
  apply (cases "perm_cap_load c")
   apply clarsimp
  apply (smt (z3) cctype.exhaust cctype.simps(73) cctype.simps(74) cctype.simps(75) cctype.simps(76) 
     cctype.simps(77) cctype.simps(78) cctype.simps(79) cctype.simps(80) ccval.distinct(105) 
     ccval.distinct(107) is_cap.elims(2) is_contiguous_bytes.simps(2) memval_size_cap 
     retrieve_stored_tval_cap retrieve_tval_def stored_tval_is_cap suc_of_32)
  apply clarsimp 
  apply (insert retrieve_stored_tval_cap_no_perm_cap_load, force)
  done

lemma load_after_store_cap_frag_1:
  assumes "store h c (Cap_v_frag c' n) = Success h'"
    and "perm_load c"
    and aligned: "(offset c) mod |Cap|\<^sub>\<tau> = |Cap|\<^sub>\<tau> - 1 - n"
    and first: "offset c  mod |Cap|\<^sub>\<tau> = 0"
  shows "load h' c (memval_type (Cap_v_frag c' n)) = Success (Cap_v_frag (c' \<lparr> tag := if perm_cap_load c then tag c' else False \<rparr>) n)"
  using assms(1) store_cond_hard_cap[where ?h=h and ?c=c and ?v="Cap_v_frag c' n" and ?ret=h', OF assms(1)]
  unfolding store_def
  apply (simp split:option.split_asm t.split_asm add: load_def assms(2), safe; simp)
  using retrieve_stored_tval_cap_frag_1[where ?val="Cap_v_frag c' n" and ?c=c' and ?n=n 
      and ?off="offset c" and ?b="True", OF refl aligned first, simplified] 
   apply fast
  using retrieve_stored_tval_cap_frag_1[where ?val="Cap_v_frag c' n" and ?c=c' and ?n=n 
      and ?off="offset c" and ?b="False", OF refl aligned first, simplified] 
  apply fast
  done

lemma load_after_store_cap_frag_2:
  assumes "store h c (Cap_v_frag c' n) = Success h'"
    and "perm_load c"
    and misaligned: "offset c mod |Cap|\<^sub>\<tau> \<noteq> |Cap|\<^sub>\<tau> - 1 - n"
  shows "load h' c (memval_type (Cap_v_frag c' n)) = Success (Cap_v_frag (c' \<lparr> tag := False \<rparr>) n)"
  using assms(1) store_cond_hard_cap[where ?h=h and ?c=c and ?v="Cap_v_frag c' n" and ?ret=h', OF assms(1)]
  unfolding store_def
  apply (simp split:option.split_asm t.split_asm add: load_def assms(2), safe; simp)
  using retrieve_stored_tval_cap_frag_2[where ?off="offset c" and ?c=c' and ?n=n 
      and ?b="perm_cap_load c", OF misaligned, simplified] 
  apply blast
  done

lemma load_after_store_cap_frag_3:
  assumes "store h c (Cap_v_frag c' n) = Success h'"
    and "perm_load c"
    and aligned: "offset c mod |Cap|\<^sub>\<tau> = |Cap|\<^sub>\<tau> - 1 - n"
    and not_first: "|Cap|\<^sub>\<tau> - 1 - n \<noteq> 0"
    and unmapped_tag: "cap_offset (offset c) \<notin> Mapping.keys (tags (the_map (the (Mapping.lookup (heap_map h) (block_id c)))))"
  shows "load h' c (memval_type (Cap_v_frag c' n)) = Success (Cap_v_frag (c' \<lparr> tag := False \<rparr>) n)"
  using assms(1) store_cond_hard_cap[where ?h=h and ?c=c and ?v="Cap_v_frag c' n" and ?ret=h', OF assms(1)]
    store_cond_cap_frag[where ?h=h and ?c=c and ?val="Cap_v_frag c' n" and ?h'=h' and ?v=c' and ?n=n, OF assms(1) refl]
  unfolding store_def
  apply (simp split: option.split_asm t.split_asm add: load_def assms(2))
  using retrieve_stored_tval_cap_frag_3[where ?off="offset c" and ?c=c' and ?n=n 
        and ?b="perm_cap_load c" and ?obj="the_map (the (Mapping.lookup (heap_map h) (block_id c)))", 
        OF aligned not_first unmapped_tag, simplified]
  apply fastforce
  done

lemma load_after_store_cap_frag_4:
  assumes "store h c (Cap_v_frag c' n) = Success h'"
    and "perm_load c"
    and aligned: "offset c mod |Cap|\<^sub>\<tau> = |Cap|\<^sub>\<tau> - 1 - n"
    and not_first: "|Cap|\<^sub>\<tau> - 1 - n \<noteq> 0"
    and mapped_tag: "Mapping.lookup (tags (the_map (the (Mapping.lookup (heap_map h) (block_id c))))) (cap_offset (offset c)) = Some b'"
  shows "load h' c (memval_type (Cap_v_frag c' n)) = Success (Cap_v_frag (c' \<lparr> tag := if perm_cap_load c then b' else False \<rparr>) n)"
  using assms(1) store_cond_hard_cap[where ?h=h and ?c=c and ?v="Cap_v_frag c' n" and ?ret=h', OF assms(1)]
    store_cond_cap_frag[where ?h=h and ?c=c and ?val="Cap_v_frag c' n" and ?h'=h' and ?v=c' and ?n=n, OF assms(1) refl]
  unfolding store_def
  apply (simp split: option.split_asm t.split_asm add: load_def assms(2) split del: if_split) 
  using retrieve_stored_tval_cap_frag_4[where ?off="offset c" and ?c=c' and ?n=n 
        and ?b="perm_cap_load c" and ?obj="the_map (the (Mapping.lookup (heap_map h) (block_id c)))"
        and ?b'=b', OF aligned not_first mapped_tag, simplified]
  apply fastforce
  done

lemma store_undef_false:
  assumes "store h c Undef = Success ret"
  shows "False"
  using store_cond_hard_cap[where ?h=h and ?c=c and ?v="Undef" and ?ret=ret, OF assms] assms
  unfolding store_def
  by simp

end
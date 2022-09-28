import tactic
import data.real.basic
import topology.algebra.affine
import probability.martingale.basic

#check mul_one 

-- a ≤ b ↔  a 
#check le_iff_lt_or_eq
-- A ∩ B 
#check set.inter_subset_left 

#check set.inter_subset_right 

#check set.subset_union_left

#check set.subset_union_right

-- subset and union form a Lattice    A ⊆ A ∪ B
-- G group, subgroup G
-- so A, B : subgroup G then A ⊆ B ?? Not for Lean ( ⊆ is for set)
-- so LEan use ≤ for subgroups


example (G: Type) [group G]
  (A B : subgroup G) : Prop := 
begin 
--  A ≤ B,   -- Ok,    but   A ⊆ B -- Error!!! 
  sorry,
end 
  -- also A ∩ B not working  for subgroup but we can use:
--   A ⊓ B -- \ + meet  (+inf) for subgroup 
  -- apply subgroup.mem_inf,

 --  A ⊔ B -- \ + join (+ sup) for group/subgroup (but not like the Union in sets)


  eq.symm  -- (dot works with implication) -- a = b → b = a
  eq_comm  -- a = b ↔ b = a  (commutativity)

  -- rwa = rewrite and assumption tactics in a single one

  le_of_lt -- a ⋖ b → a ≤ b 
  #check has_lt.lt.le --  same things but with a different appraoch / alias

  example (X: Type)(s : set X) :    -- s is a subset of X
    sᶜᶜ = s :=                    -- s + \ + ^ + c = sᶜ 
  begin
    simp only [compl_compl]       -- only is to reduce the simp search
  end





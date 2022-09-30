import tactic

-- A → B → C (A → B) → C       
-- A, B finite set
-- | A 'to B | = bᵃ (b^a) .   with |A| = a and |B| = b
-- (c^b)^a or (cᵇ)ᵃ = c^(axb)    
-- P => (Q=>R) ↔ (P ∧ Q) => R
-- A → (B → C) ≃ A x B → C

-- Currying 

variables (A B C : Type)

example : (A → B → C) ≃ (A × B → C) := 
{ 
  to_fun := λ f ab, f ab.fst ab.snd,  -- or ab.1 ab.2 
  inv_fun := λ g a b, g ⟨a, b⟩,    -- a ⨯ b could be ⟨a, b⟩ or with normal braket (only for prod): g (a, b))
  left_inv := λ x, rfl,
  right_inv := begin
          intro y, 
          dsimp,
          ext ab,   -- in Lean4 refl would work cause eta reduction is "by definition"
          cases ab,
          refl,
        end
}


-- division by zero and infinite sum

#eval 36/4  -- = 4
#eval 37/4  -- = 4 (cause ∈ ℕ )

#check has_div.div 

#eval 1/0 -- 0 !!! again cause ∈ ℕ 

-- in proof mode you won't do a / b
-- but sensible_div a b hb ,   where hb is the proof that b ≠ 0

-- 1 + 1 + 1 + 1 + ....   = 0  !!!!  Infinite sum that doesn▸ converge returns 0




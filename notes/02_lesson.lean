import tactic
import data.real.basic
import topology.algebra.affine
import probability.martingale.basic


-- rw vs apply

example (p q : Prop) (hp : p)(hpq : p ↔ q) : q :=
begin
  -- have new_thing := hpq(hp),
  -- apply hpq,    -- apply only works 1) with Prop (implies) 2) goal is the result of the Prop

  -- rw hpq,           -- rw only works with h in the form A=B or A↔B @) the goal contains A     (A=B ≠ B=A syntactically)
  sorry
end 

#check eq.symm   -- eq.symm : ?M_2 = ?M_3 → ?M_3 = ?M_2
-- or (to have better reading)
#check @eq.symm  -- eq.symm : ∀ {α : Sort u_1} {a b : α}, a = b → b = a

-- A = B ≠ B = A
example (a b : ℕ)(h: a = b) : b = a :=
begin
  -- apply eq.symm h,
  rw h,
end

example (A B C : Type) :
(A × B → C) ≃ (A → B → C) := sorry  -- equivalence/bijective between AxB→C to A→B→C:w

-- set_option pp.notation false  SWITCH OFF notation (Lean syntactic sugar)
#print notation + -- check the real implementation of the symbol (notation): in this case of +
#print notation -
#print notation ^  -- 2 number indicating the precedence and how they associate 80 79 means associate from the right

example (p q r : Prop)(h: p → q → r) : r :=
begin 
  sorry,
end

-- DOT Notation
-- instead using apply with some function (lib.function) we can use 
-- refine with _ 
-- le_of_eq -- less or equal of equality 

#check mul_one -- x * 1 = x
#check one_mul -- 1 * x = x
#check le_iff_lt_or_eq

--example (a b : ℕ)(h : b = a): b  ≤ a :=
--  h.simm.le

-- structure
#check ℝ 

-- define our own complex 
@[ext] structure complex1 : Type :=
(re : ℝ)
(im : ℝ)

notation `ℂ1` := complex1

--let z be a conplex numvber
variable (z: complex1)

#check complex1.re
#check complex1.im

--make 3 + 4i
def z₁ : ℂ1 := complex1.mk 3 4     -- make, constructor for my structure
def z₂ : ℂ1 := ⟨3, 4⟩ 

example : z₁ = z₂ :=
begin
  refl,
end 

def complex1.zero : ℂ1 := ⟨0, 0⟩
def complex1.one : ℂ1 := ⟨1, 0⟩
def complex1.I : ℂ1 := ⟨0, 1⟩

def complex1.add : ℂ1 → ℂ1 → ℂ1 :=
λ z w, ⟨complex1.re z + complex1.re w, 
        complex1.im z + complex1.im w ⟩  

-- make instance for + notation
instance : has_add ℂ1 := ⟨ complex1.add ⟩ 

def z₃ := z₁ + z₂   -- without instance has _add we won't be able to do it

lemma complex1.re_add (z w : ℂ1) :
  (z + w).re = z.re + w.re :=
    rfl

lemma complex1.re_im (z w : ℂ1) :
  (z + w).im = z.im + w.im := 
    rfl

example (a b : ℝ) : (a+b)^2 = 
a^2+2*a*b+b^2 := by ring   -- ring does NOT have access to hypothesis

theorem complex1.add_comm (z w : ℂ1) :
  z + w = w + z := 
  begin
    ext,   -- needed to add@ [ext] notation to the structure (existential)
    {
      -- rw complex1.re_add,
      -- rw complex1.re_add,
      -- apply add_comm,
      simp [add_comm]   -- ATT. also symp has NO access to hypothesis
    },
    {
      simp [add_comm]
    }

  end

-- high power tactics (use hint) 

-- linear_combination
-- linarith
-- nlinarith
-- simp,
-- ring


#exit
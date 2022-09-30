import tactic
import topology.algebra.affine
import data.complex.basic   -- vector spaces

-- bijections 

def injective {α β : Type} (f : α → β) :=
  ∀ α1 α2 : α, f α1 = f α2 → α1 = α2

def surjective {α β : Type} (f : α → β) :=
  ∀ b : β, ∃ a : α, f a = b

def bijective {α β : Type} (f : α → β) :=
injective f ∧ surjective f 

structure myequiv (α β : Type) := 
(to_fun : α → β )
(inv_fun : β  → α )
(left_inv : ∀ a : α, inv_fun (to_fun a) = a)
(righ_inv : ∀ b : β, to_fun (inv_fun b) = b)

-- example (α β : Type) (e: myequiv α β ) : 
--   bijective e.to_fun := 
-- begin 
--   refine ⟨_ , _⟩ 
--   { cases e with f g hfg hgf,
--     dsimp only,
--     intros α1 α2 h,
--     rw [← hfg a1, h, hfg a2], },
--   { cases e with f g hfg hgf,
--     intro b,
--     use (g b),
--     exact gf b, } ,
-- end
-- 
-- def inverse (α β  : Type) (f : α → β )
--   (hf: bijective f) : β → α :=
--   λ b,
--   begin
--     cases hf with hfinj hfsurj 
--     unfold surjective at hfsurj,
--     specialize hfsurj b,
--     exact hfsurj.some,
--   end
-- 

notation X `≅ ` Y := myequiv X Y   -- my isomorphic notation \ + iso 

variables (X Y : Type)(e : X ≅ Y) (x : X)

-- we want e : X → Y , but we don't have it but we can do like that:
#check e.to_fun x

-- we can instead use a term as a function (using instance)
-- @[instance] def foo(X Y : Type) : ... 
instance (X Y : Type) :
  has_coe_to_fun (X ≅ Y) := 
{ F := λ e, X → Y,
    coe := λ e, e.to_fun }


@[derive decidable_eq]
inductive int : Type 
| of nat : nat → int 
| neg_succ_of_nat : nat → int 



-- (up arrow) e = term regarded as a function (use has_coe_to_fun) (see before)
-- e = term regarded as a different term (has_coe)
-- e = tetrm regarded as a (diff) type (has_coe_to_sort) 
def n : ℕ := 5 

#check n
#check (n : ℤ)  -- different type
#check (n : ℚ)
-- #check ((((n: ℤ) ℚ) :ℝ) :ℂ) 

example (G: Type) [group G]
  (H : subgroup G)(a b: H) :  -- h has a up arrow cause subgroup is not a type is a term
  a * (a * b) = (a * a) * b := 
begin 
  cases a,
  have foo : a = a_val = b := sorry,  -- b is type up arrow H  (↥H)
  sorry,
end


example : has_coe_t ℤ ℝ := -- coercion on type
begin 
  apply_instance,
end


-- Vector spaces
-- if R is a ring, then a module over R is just 
-- (write down vector space axioms!)

-- V group under addition, where v + w ∈ V and 0 ∈ V (no division or inverse mentioned in Vector space axioms)

-- let k be a field
-- let V be k-vector space
variables (k : Type) [field k]
(V : Type)[add_comm_group V]
[module k V]

def mynat := ℕ 
variables (a b : ℕ )
#check a + b . -- all ok 

variables (c d : mynat)
#check c + d . -- don't kbow how to add them

-- I can change def to notation and it will work
notation `mynat2` := ℕ 
variables (q w : mynat2)
#check q + w  -- works !!!


-- What is a basis ??
-- V = ℝ² → W = ℝ³ 
-- we need ordered so basis cound not be a Set 
-- In Lean a basis is a function  B : L → V   (L = iota)

variables (k : Type)[field k]
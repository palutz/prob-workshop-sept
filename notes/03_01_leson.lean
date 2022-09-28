import tactic

-- trying to write our own And 
structure my_and (a b : Prop) : Prop := 
intro :: 
(left: a)
(right: b)

#check my_and
--#check my_and.mk   -- if without intro
#check my_and.intro -- my_and.mk : ?M_1 → ?M_2 → my_and ?M_1 ?M_2
                  -- get a Prop of M_1 and a Prop of M_2 and return an and of M_1 and M_2

-- how to define OR (with inductive)

example (X: Type)(a b : X)(h :a = b ∧ a ≠ b) : false := 
begin 
  -- cases h with heq hne,   -- with x y, naming the branch x and y
  rcases h with ⟨heq, hne⟩, 
  -- rw heq at hne,
  subst heq,  -- define b to the a and remove b substituting with a
  sorry 
end

-- other way 
example (X: Type)(a b : X)(h :a = b ∧ a ≠ b) : false := 
begin 
  rcases h with ⟨rfl, hne⟩, -- rfl instead naming and then rw
  delta ne at hne,   -- unfold super carefully. we can use unfold 
  delta not at hne, 
  apply hne,
  refl
end
-- compresaed and cleaned way of previous
example (X: Type)(a b : X)(h :a = b ∧ a ≠ b) : false := 
begin 
  rcases h with ⟨rfl, hne⟩, -- rfl instead naming and then rw
  exact hne rfl,
end

-- adding another prop
example (X: Type)(a b : X)(h :a = b ∧ a ≠ b ∧ 2 + 2 = 4) : false := 
begin 
  rcases h with ⟨heq, hne, hobvious⟩ , 
  sorry 
end

#check nat 


inductive mynat
  | zero : mynat
  | succ (n: mynat)  : mynat

-- mynat : Type
-- mynat.zero : mynat
-- mynat.succ : mynat → mynat
-- mynat.rec   -- principle of induction (using motive, more recursion)
-- and then Lean auto-generates some theorems about mynat

def mynat.pred : mynat → mynat 
| mynat.zero := mynat.zero
| (mynat.succ n) := n

lemma foo (m n : mynat) (h : m.succ = n.succ) : m = n :=
begin
  apply_fun mynat.pred at h,
  exact h,
end

-- pred defined with recursive
def mynat.pred2 : mynat → mynat := 
@mynat.rec (λ n, mynat) (mynat.zero)
(λ n m, n) 

-- #print prefix mynat

-- motive: mynat → Type
-- motive(n) = mynat
-- motive(n) is a true/false statement    (the idea is to prove the motive)
-- we want a function (recursive) with 
-- input n and
-- output term of type motive(n)
-- using the CALCULUS of NATURAL INDUCTION

inductive myand (p q : Prop)
| intro (hp: p)(hq : q): myand 

example (n :ℕ) : false := 
begin
  cases n,   -- 2 hypo nat.zero (false), nat.succ (true) 
  { sorry, },
  { sorry, },
end

-- !!! Induction is the one applying the recursion !!! 

#exit
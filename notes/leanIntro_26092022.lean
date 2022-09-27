import tactic 
import group
import data.real.basic
-- Everything is one of:
-- a Universe
-- a type (= sets) (Set like ℕ, ℝ  or subgroup G, prop like 2+2 = 4 or 2 + 2 = 5)
                    -- prop (can vbe True or False)
def type1: Prop := 2+2=4
def type2: Prop := 2+2=5

-- a term (= elements)

-- x € S in set theory. In Lean x : T (is of type T)
-- 37 : ℝ  or H : Subgroup G

def term1: type1 := 
begin 
  -- unfold, dunflod, delta
  delta type1,
  refl,
end 

def term2: type1 :=
begin 
  delta type1
  norm_num,
end

example : term1 = term2 :=  -- prooving that term1 and term2 are the same proof 
begin  
  refl,
end

-- group example
variables (G: Type)(i: group G)
variables (x y : G)

#check x * y
example : x * y = x * y := sorry    
-- changing the var to [group G] instead of (i: group G) it will work

def foo (G: Type) [group G]
  (H: subgroup G) : Type := H

print foo 

def x: ℝ := 37

#check x  -- x: ℝ 

def y: ℝ := (37 : ℕ)  -- we can't even do it in LEan (whilst we can do in Maths and having a disjoin set)

example : x = x :=
begin 
  sorry
end

-- X: Type 
-- Y: Type
-- f: X → Y then X → Y : Type and is a set of ALL the function X → Y
-- f: Hom (X, Y) 
--    Map (X, Y)


⟨digits, Group, MDSlash, Num⟩ ← •Import "../../utils/utils.bqn"
schema ← >•FLines "./test2.txt"

dm ← (⊑∊⟜digits)¨schema
sm ← (⊑∊⟜('.'∾digits))¨schema
S ← 1‿0‿¯1⊸(⌽⌜)·1‿0‿¯1⊸(⌽˘¨)<
pm ← ∧´∧˘˝S sm
F ← {𝕩=1∧𝕨≠0?𝕨;𝕩}
mask ← 2= F`∘(F`⌾⌽)˘ dm+dm∧¬pm

# Part 1
•Show +´ Num¨ mask Group○⥊ schema

# Part 2
g1 ← '*'=schema
igm ← +´·+˘˝S {(≢𝕩)⥊{𝕩×+`𝕩}⥊𝕩}g1
gm ← +´·+˘˝S g1
ppm ← ¬∧´∧˘˝S ¬ 1<g1∧ +´+˘˝S dm ∧ gm
#+˘˝1‿0‿¯1⊸(⌽˘¨)<mmask
mmask ← 2= F`∘(F`⌾⌽)˘ dm+dm∧ppm
imask ← F`∘(F`⌾⌽)˘ igm {𝕩=0 ? 0; 𝕨+𝕩}¨ mmask
cogMasks ← {imask=𝕩}¨1+↕⌈´⥊imask
•Show ns ← ⟨⟩⊸≢¨⊸/ {Num¨ 𝕩 Group○⥊ schema}¨ cogMasks
rs ← 1⊸<∘≠¨⊸/ ns
•Show +´×´¨rs

# TODO: Finish optimising part 2
l ← >{𝕩 ∾ (3⥊⟨0‿0‿1‿1‿1⟩) ∾𝕩} (3⥊<(5⥊0))
(3‿3⥊MDSlash˜)⊸⊑ l
x← mmask + 2×{(≢𝕩)⥊{𝕩×+`𝕩}⥊𝕩}g1
MDSlash⟜(=⟜2) x

relSq ← -⟜1¨↕3‿3

GetSq ← {𝕩 ⊑˜ 𝕨⊸+¨relSq}

GetSq⟜dm¨ MDSlash˜ g1
{+´{𝕩≡1‿0‿1?2;⊑∨´𝕩}˘𝕩}¨⌾(GetSq⟜dm¨MDSlash˜) g1
⊑¨⌾(MDSlash˜) g1
(MDSlash˜) g1

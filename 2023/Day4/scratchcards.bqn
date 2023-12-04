⟨Num, Split⟩ ← •Import "../../utils/utils.bqn"
i ← {Num¨¨' 'Split¨'|'Split 1⊑':'Split𝕩}¨•FLines "i"
score ← +´∘∊´¨i
# Part 1
•Show +´ ⌊2⋆¯1+score
# Part 2
•Show +´ (1¨i) {(𝕨⊑𝕩)⊸+⌾((𝕨⊑score)↑(1+𝕨)↓⊢)𝕩}´ ⌽↕≠i

⟨SInts⟩ ← •Import "../../utils/utils.bqn"
i ← SInts¨ •FLines "i"

Next ← ¯1⊑{S𝕩: {∧´0=1↓𝕩? 𝕩∾0; (⊑𝕩)∾S 1↓𝕩}⌾(+`⁼) 𝕩}

# Part 1
•Show +´ Next¨ i
# Part 2
•Show +´ Next∘⌽¨ i

⟨Num, Ints, Split⟩ ← •Import "../../utils/utils.bqn"
i ← {1⊑':'Split 𝕩}¨ •FLines "i"
_quad ← {t‿d←𝕩⋄2÷˜t𝔽√(t⋆2)-4×d}
Solve ← 1⊸+·⌊+_quad--_quad
# Part 1
•Show ×´ Solve¨ ∾¨´ Ints¨ i
# Part 2
•Show Solve Num∘(∊⟜digits⊸/)¨i

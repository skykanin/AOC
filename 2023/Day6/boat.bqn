⟨Num, Ints, Split⟩ ← •Import "../../utils/utils.bqn"
i ← {1⊑':'Split 𝕩}¨ •FLines "i"
Solve ← {t‿d←𝕩⋄+´(d<⊢×t-⊢)1+↕t}
# Part 1
•Show ×´ Solve¨ ∾¨´ Ints¨ i
# Part 2
•Show Solve Num∘(∊⟜digits⊸/)¨i

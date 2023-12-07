⟨Num, Split⟩ ← •Import "../../utils/utils.bqn"
c‿v ← Num¨⌾(1⊸⊑) <˘⍉>' '⊸Split¨ •FLines "t"

Rank ← {⊑⟨1‿1‿1‿1‿1,2‿1‿1‿1,2‿2‿1,3‿1‿1,3‿2,4‿1,⟨5⟩⟩⊐<𝕩}
ranks ← ((Rank·∨·/⁼⊐)∾"23456789TJQKA"⊸⊐)¨ c
Score ← {+´v × 1+⍋⍋𝕩}

# Part 1
•Show Score ranks

jranks ← {
  k ← ∨/⁼⊐ 'J'⊸≠⊸/ 𝕩
  jk ← (5-+´k)⊸+⌾⊑ (1⌈≠)⊸↑ k # redirect jokers to the most common card
  (Rank jk)∾"J23456789TQKA"⊐𝕩
}¨ c

# Part 2
•Show Score jranks

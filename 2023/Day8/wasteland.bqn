l ← •FLines "t"
k‿lv‿rv ← <˘⍉> {⟨3↑𝕩, 3↑7↓𝕩, 3↑12↓𝕩⟩}¨ 2↓l
lr ← k ⊐ >lv‿rv
r ← 'R'=⊑l
Loop ← { c←𝕨
  ⊑{⟨1+𝕨, ⟨((≠r)|𝕨)⊑r, 𝕩⟩⊑lr⟩}´•_while_{c⊑˜1⊑𝕩} ⟨0, ⊑k⊐<𝕩⟩
}
# Part 1
•Show ("ZZZ"⊸≢¨ k) Loop "AAA"
# Part 2
•Show •math.LCM´ (('Z'≢2⊑⊢)¨ k)⊸Loop¨ ('A'=2⊑¨⊢)⊸/ k

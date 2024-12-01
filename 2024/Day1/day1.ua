# T ← &fras "test.txt"
I ← &fras "input.txt"

Parse ← ≡°$"_   _" ⊜□ ⊸≠"\n"

Day₁ ← /+ ⌵- ∩(⊏⊸⍏⋕) Parse

&p "--- Day1 ---"
&p Day₁ I

Day₂ ← (
  /+ × ⟜≡(/+=) ⊙¤ ∩⋕ Parse
)

&p "--- Day2 ---"
&p Day₂ I

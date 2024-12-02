# T ← &fras "test.txt"
I ← &fras "input.txt"

Parse ← ≡°$"_   _" ⊜∘ ⊸≠"\n"

Part₁ ← /+ ⌵- ∩(⊏⊸⍏⋕) Parse

&p "--- Part1 ---"
&p Part₁ I

Part₂ ← /+ × ⟜≡(/+=) ⊙¤ ∩⋕ Parse

&p "--- Part2 ---"
&p Part₂ I

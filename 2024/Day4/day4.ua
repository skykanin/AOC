Parse ← ⊜∘⊸≠@\n
I     ← Parse &fras $ i
TI₁   ← Parse &fras $ t
TI₂   ← Parse &fras $ t2

Part₁ ← (
  {⊃(♭₂ ⧈(⌝⤸0_0) ⊟.4   # Diag from top left
    | ♭₂⧈(⌝⤸0_0) ⊟.4≡⇌ # Diag from top right
    | ♭₂≡⧈∘4           # Rows
    | ♭₂≡⧈∘4⍉          # Columns
    )
  }
  /+≡◇(+∩(/+∈¤"XMAS")≡⇌.)
)

⍤⤙≍ 18 Part₁ TI₁
⍤⤙≍ 2571 Part₁ I

Mas   ← /↧ ≡/↥≡(≡(≍"MAS")⊟⊸⇌) [∩⊡] ⊙, ⊸⍜(≡⊣)⇌ ≡⊟.⇡3
Part₂ ← /+≡(Mas)♭₃ ⧈∘3_3

⍤⤙≍ 9 Part₂ TI₂
⍤⤙≍ 1992 Part₂ I

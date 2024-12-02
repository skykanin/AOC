I ← &fras "t"

Parse ← ⊜(□ ⊜⋕ ⊸≠@ ) ⊸≠@\n

IsSafe ← (
  ≡(/-)◫2
  +∩(/↧)⊃(>0)(<0).
  ↧/↧×⊃(≥¯3)(≤3)⌵:
)

Part₁ ← /+ ≡⍚IsSafe Parse

&p "--- Part 1 ---"
&p Part₁ I

MadeSafe ← /↥ ≡IsSafe ≡▽ ⊙¤ ⊞≠.⇡ ⧻.
Part₂    ← /+ ≡⍚(↥⊃MadeSafe IsSafe) Parse

&p "--- Part 2 ---"
&p Part₂ I

I ← &fras "t"

Parse ← ⊜(□ ⊜⋕ ⊸≠@ )⊸≠@\n

IsSafe ← (
  ≡/-◫₂
  +∩/↧⊃>₀<₀.
  ↧/↧×⊃≥₋₃≤₃⌵:
)

Part₁ ← /◇+ ≡⍚IsSafe Parse

&p "--- Part 1 ---"
⍤⤙≍ 2 ⊸&p Part₁ I

MadeSafe ← /↥ ≡IsSafe ≡▽ ⊙¤ ⊞≠.⇡⊸⧻
Part₂    ← /◇+ ≡⍚(↥⊃MadeSafe IsSafe) Parse

&p "--- Part 2 ---"
⍤⤙≍ 4 ⊸&p Part₂ I

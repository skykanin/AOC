# test

test ← >•file.Lines "test_input.txt"
data ← >•file.Lines "input.txt"

# this is a comment
F ← {+´'#'= 𝕨⊸{(¯1⊑𝕩)⊸|⌾(¯1⊸⊑)¨ 𝕨⊸×¨↕⊑𝕩}∘≢⊸⊑ 𝕩}
P1 ← 1‿3⊸F
P2 ← F

P1 test
P1 data

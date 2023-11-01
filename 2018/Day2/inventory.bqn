lines ← •FLines "./example2.txt"

Freq ← (3⊸=+2⊸=)⊸/·⍷·≠¨·⊔˜⊐
Cnt ← ≠=/⊢

# Part 1
×´(2⊸Cnt∾3⊸Cnt)∾Freq¨lines

# Part 2
⥊˘lines
+´¨=⌜˜lines

data←⊃⎕nget'input.txt'1
ranges←data(⍎¨⊣⊆⍨⊢∘(('-'⍷⊢)⍱','⍷⊢))¨data

Contains←{a b c d←⍵ ⋄ ((c≥a)∧b≥d) ∨ (a≥c)∧d≥b}
Overlaps←{a b c d←⍵ ⋄ (d≥a)∧b≥c}

+/Contains¨ ranges ⍝ Part 1
+/Overlaps¨ ranges ⍝ Part 2

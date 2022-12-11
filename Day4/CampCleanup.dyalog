data←⊃⎕nget'input.txt'1
ranges←data(⍎¨⊣⊆⍨⊢∘(('-'⍷⊢)⍱','⍷⊢))¨data

Contains←{a b c d←⍵ ⋄ 0≥(a-c)×b-d}
Overlaps←{a b c d←⍵ ⋄ 0≥(a-d)×b-c}

+/Contains¨ ranges ⍝ Part 1
+/Overlaps¨ ranges ⍝ Part 2

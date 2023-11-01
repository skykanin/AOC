p←⊃⊃⎕nget'input.txt'1
f←{¯1+⍵+⊃⍸∧/↑≠¨⍵,/p}

f 4 ⍝ Part 1
f 14 ⍝ Part 2

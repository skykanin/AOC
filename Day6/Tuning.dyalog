data←⊃⎕nget'input.txt'

⍝ train spotting

⍝ explicit function
solve←{(⍺-1)+(≠¨⍺,/⍵)⍳⊂⊢1⍨¨⍳⍺}

f← ≠¨,/

solveWorks2←{¯1+⍺+(⍺ f ⍵) ⍳ ⊂1⍨¨⍳⍺}

g←⊂⊢1⍨¨⍳

solveWorks3←{¯1+⍺+ (⍺ f ⍵) ⍳ g ⍺}

solveWorks4←{¯1+⍺+ ((⍺ f ⍵) ⍳ g) ⍺}

solveWorks5←{¯1+⍺+ (g⊢ ⍳⍨ ⍺ f ⍵) ⍺}

4 solve data ⍝ Part 1
14 solve data ⍝ Part 2

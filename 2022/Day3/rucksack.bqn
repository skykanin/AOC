input ← •FLines "./input.txt"

Split ← <˘(2÷˜≠)⊸(↑≍↓)
alpha ← ∾{𝕩+↕26}¨ "aA"
Chunk ← ⌊∘÷˜⟜(↕≠)⊔⊢

Priority ← 1+·⊑alpha⊐·⊑(∊/⊣)´

•Show +´ Priority∘Split¨ input # Part 1
•Show +´ Priority¨ 3 Chunk input # Part 2

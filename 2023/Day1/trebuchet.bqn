lines ← •Flines "./input.txt"

# Part 1
•Show +´•ParseFloat¨ 0‿¯1⊸⊏¨ ∊⟜('0'+↕10)⊸/¨ lines

names ← (⋈¨'0'+↕10)≍@‿"one"‿"two"‿"three"‿"four"‿"five"‿"six"‿"seven"‿"eight"‿"nine"

# Part 2
•Show +´•ParseFloat¨ {'0'+⥊/˘ (0‿¯1⊏·/∨˝)⊸⊏⟜⍉ >+˝((≠𝕩)↑⍷⟜𝕩)¨ names}¨ lines

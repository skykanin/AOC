⟨Num, Split⟩ ← •Import "../../utils/utils.bqn"
games ← •FLines "./input.txt"
colors ← "red"‿"green"‿"blue"

games {
  {
  [n, s] ← ⍉>{Num⌾⊑ ' ' Split 𝕩}¨ ',' Split 1↓𝕩
  n⌾((colors⊐s)⊸⊏) 3⥊0
  }¨ ';' Split 1⊑':' Split 𝕩
}¨ ↩

# Part 1
•Show +´1+/(∧´(∧´12‿13‿14⊸≥)¨)¨ games
#Part 2
•Show +´(×´⌈´)¨ games

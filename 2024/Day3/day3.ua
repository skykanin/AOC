┌─╴Solution
  Part₁ ← /+≡/×⋕≡↘₁regex $ mul\((\d{1,3}),(\d{1,3})\)
  Part₂ ← Part₁▽⬚1\(↥₀↧₁+)+⊙¯∩⌕"do()","don't()".
└─╴

┌─╴test
  &p "--- Part 1 ---"
  ⍤⤙≍ 161 ⊸&p Solution~Part₁ &fras $ t1

  &p "--- Part 2 ---"
  ⍤⤙≍ 48 ⊸&p Solution~Part₂ &fras $ t2
└─╴

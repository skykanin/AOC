Input ← $ vJrwpWtwJgWrhcsFMMfFFhFp
        $ jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
        $ PmmdzqPrVvPwwTWBwg
        $ wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
        $ ttgJtRGJQctTZtZT
        $ CrZsJsPPZsGzwwsLwLmpwMDw

InputTwo ← &fras "/home/skykanin/Projects/AOC2022/Day3/input.txt"

Alphabet ← $ abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ

# ⊡ /↧ ≡(⊗ 1) ◳0_1⌕ ⊙."vJrwpWtwJgWr" "hcsFMMfFFhFp"

Priority ← + 1 ⊗ ∶ Alphabet ⊡ /↧ ≡(⊗ 1) ⍚0_1⌕ ⊙.⊃↙↘ ÷2 ⧻.

# Priority "vJrwpWtwJgWrhcsFMMfFFhFp"

/+ ∵'Priority⊔ ⊜□ ≠@\n . Input

/+ ∵'Priority⊔ ⊜□ ≠@\n . InputTwo

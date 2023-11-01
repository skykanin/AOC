data ← •ParseFloat∘⋈¨˘ >•file.Lines "input.txt"

_count ← {(⊑⊢⊐𝔽´)¨+˝0‿1⊸=¨𝕩}
Bin ← {+⟜(2⊸×)´⌽𝕩}

P1 ← Bin∘(⌈_count)×Bin∘(⌊_count)

P1 data

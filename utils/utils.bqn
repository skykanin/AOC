digits ⇐ '0'+↕10
Group ⇐ (¯1+⊢×·+`»⊸<)⊸⊔
Num ⇐ •ParseFloat
Split ⇐ {𝕩 ⊔˜ (¯1+⊢×·+`»⊸<)¬𝕩=𝕨}
ReIndex ⇐ {<˘𝕨|⎉1⌊𝕩÷⌜1«×`⌾⌽𝕨} # convert indices into a shape-⟨×´𝕨⟩ array into corresponding indices into a shape-𝕨 one
MDSlash ⇐ {(0.13×≠⥊𝕩)>+´⥊𝕩? (≢𝕩) ReIndex /⥊𝕩; /○⥊⟜↕⟜≢ 𝕩}

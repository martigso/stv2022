library(tidyverse)
library(tidytext)
library(stm)

load("./data/virksommeord_id.rda")
load("./data/virksommeord_dfm.rda")



# Estimere LDA-modell
virkord_lda <- stm(virkord_dfm,       # dfm objekt
                   data = virkord,    # legger til metadata
                   K = 20,            # antall emner man ønsker
                   init.type = "LDA") # initialiseringstype, her LDA

save(virkord_lda, file = "./data/virkord_lda.rda")
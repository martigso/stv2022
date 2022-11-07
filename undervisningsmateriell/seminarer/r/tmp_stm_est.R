library(tidyverse)
library(tidytext)
library(stm)

load("./data/virksommeord_id.rda")
load("./data/virksommeord_dfm.rda")


virkord_stm <- stm(virkord_dfm,                  
                   prevalence = ~ factor(gender), 
                   data = virkord,               
                   K = 20,                       
                   init.type = "Spectral")       

save(virkord_stm, file = "./data/virkord_stm.rda")
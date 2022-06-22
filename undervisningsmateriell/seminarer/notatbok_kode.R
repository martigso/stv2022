## ----setup, echo=FALSE, message=FALSE, error=FALSE----------------------------------

# set.wd("./undervisningsmateriell/seminarer")
library(knitr)
library(formatR)
library(rmdformats)

## Global options
# options(max.print = "75")
knitr::opts_chunk$set(
  message = FALSE, warning = FALSE, highlight = TRUE
)
#opts_knit$set(width = 75)

library(stringr)
library(dplyr) 

# if("tidytext" %in% installed.packages() == FALSE) install.packages("tidytext")



## ----laste_data_data----------------------------------------------------------------

saker <- stortingscrape::cases$root

saker %>% 
  select(id, document_group, status, title_short) %>% 
  mutate(title_short = str_sub(title_short, 1, 30)) %>% 
  tail()



## ----rda_save, eval=FALSE-----------------------------------------------------------
## 
## save(saker, file = "./data/saker.rda")
## 


## ----rda_load-----------------------------------------------------------------------

load("./data/saker.rda")



## ----csv----------------------------------------------------------------------------

library(readr)

saker <- read_csv("./data/saker.csv", show_col_types = FALSE)



## ----msw_zip------------------------------------------------------------------------

unzip("data/ba_thesis.docx", exdir = "data/wordfiles")

list.files("data/wordfiles/")



## ----read_msw_feil, warning=TRUE----------------------------------------------------

readLines("./data/ba_thesis.docx", n = 2)




## ----docx---------------------------------------------------------------------------

library(textreadr)

ba_docx <- read_docx("./data/ba_thesis.docx")

ba_docx[43:46]



## ----read_pdf-----------------------------------------------------------------------

ba_pdf <- read_pdf("./data/ba_thesis.pdf")

ba_pdf <- ba_pdf$text[4] %>% 
  strsplit("\\n") %>% 
  unlist()

ba_pdf[11:14]



## ----bow, tidy=TRUE, results='markup'-----------------------------------------------

regndans <- readLines("./data/regndans.txt")

bow <- regndans %>% str_split("\\s") %>% unlist()

set.seed(984301)

cat(bow[sample(1:length(bow))])



## ----dragepust----------------------------------------------------------------------

regndans[which(str_detect(regndans, "dragepust"))]



## ----regndans_full, echo=FALSE------------------------------------------------------
cat(paste(regndans, collapse = "\n"))


## ----writeManifest, echo=FALSE, eval=FALSE------------------------------------------
## installed.packages()
## # Oppdaterer manifest -- jeg tror vi må kjøre denne manuelt hver
## # gang før vi pusher til github. uio-serveren er hvertfall ikke fornøyd
## # når jeg ikke gjør det.
## rsconnect::writeManifest("./")
## 
## # For å trekke ut koden fra hele boka til en R-fil
## knitr::purl("./notatbok.Rmd",
##             output = "./notatbok_kode.R",
##             documentation = 1)


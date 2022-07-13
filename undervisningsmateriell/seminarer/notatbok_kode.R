## ----setup, echo=FALSE, message=FALSE, error=FALSE--------------
devtools::install_github("martigso/stortingscrape")

# setwd("./undervisningsmateriell/seminarer")
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
library(ggplot2)
# if("tidytext" %in% installed.packages() == FALSE) install.packages("tidytext")



## ----child = 'oppgavebygging.Rmd'-------------------------------




## ----laste_data_data--------------------------------------------
# install.packages("stortingscrape")

saker <- stortingscrape::cases$root

saker %>% 
  select(id, document_group, status, title_short) %>% 
  mutate(title_short = str_sub(title_short, 1, 30)) %>% 
  tail()



## ----rda_save, eval=FALSE---------------------------------------
## 
## save(saker, file = "./data/saker.rda")
## 


## ----rda_load---------------------------------------------------

load("./data/saker.rda")



## ----csv--------------------------------------------------------

library(readr)

saker <- read_csv("./data/saker.csv", show_col_types = FALSE)



## ----sav--------------------------------------------------------
library(haven)
saker <- read_sav("./data/saker.sav")



## ----dta--------------------------------------------------------

saker <- read_dta("./data/saker.dta")



## ----txt--------------------------------------------------------

filer <- list.files("./data/txt", pattern = ".txt", full.names = TRUE)
filer

titler <- lapply(filer, readLines)
class(titler)

# Første tekst
titler[[1]]



## ----txt2-------------------------------------------------------

names(titler) <- str_extract(filer, "[0-9]+")
names(titler)



## ----txt3-------------------------------------------------------
saker_txt <- data.frame(titler = unlist(titler),
                        id = names(titler))


## ----txt4-------------------------------------------------------
saker_merge <- left_join(saker_txt, saker[, c("id", "title")], by = "id")

saker_merge$titler == saker_merge$title



## ----msw_zip----------------------------------------------------

unzip("data/ba_thesis.docx", exdir = "data/wordfiles")

list.files("data/wordfiles/")



## ----read_msw_feil, warning=TRUE--------------------------------

readLines("./data/ba_thesis.docx", n = 2)




## ----docx-------------------------------------------------------

library(textreadr)

ba_docx <- read_docx("./data/ba_thesis.docx")

ba_docx[43:46]



## ----read_pdf---------------------------------------------------

ba_pdf <- read_pdf("./data/ba_thesis.pdf")

ba_pdf <- ba_pdf$text[4] %>% 
  strsplit("\\n") %>% 
  unlist()

ba_pdf[11:14]



## ----bow, tidy=TRUE, results='markup'---------------------------

regndans <- readLines("./data/regndans.txt")

bow <- regndans %>% str_split("\\s") %>% unlist()

set.seed(984301)

cat(bow[sample(1:length(bow))])



## ----dragepust--------------------------------------------------

regndans[which(str_detect(regndans, "dragepust"))]



## ----regndans_full, echo=FALSE----------------------------------
cat(paste(regndans, collapse = "\n"))


## ----norsentlex_hidden, echo=FALSE------------------------------
load("./data/nor_fullform_sent.rda")
load("./data/nor_lemma_sent.rda")


## ----norsentlex_load, eval=FALSE--------------------------------
## # devtools::install_github("martigso/NorSentLex")
## 
## # library(NorSentLex)


## ----norsentlex_general-----------------------------------------

# Ordbøker i fullform
names(nor_fullform_sent)

# Ordbøker for lemma med PoS-tags
names(nor_lemma_sent)



## ----pos_fullform, echo=-1--------------------------------------
set.seed(58493)
nor_fullform_sent$positive %>% head()
nor_fullform_sent$positive %>% tail()
nor_fullform_sent$positive %>% sample(., 6)



## ----pos_subst, echo=-1-----------------------------------------
set.seed(8943)
nor_lemma_sent$lemma_noun_positive %>% sample(., 6)



## ----regndans_sent_setup----------------------------------------

library(tidytext)

load("./data/no4.rda")

no4 <- no4 %>% 
  group_by(titler) %>% 
  unnest_tokens(ord, tekst)



## ----regndans_sent----------------------------------------------


no4$pos_sent <- ifelse(no4$ord %in% nor_fullform_sent$positive, 1, 0)
no4$neg_sent <- ifelse(no4$ord %in% nor_fullform_sent$negative, 1, 0)

table(no4$pos_sent, 
      no4$neg_sent, 
      dnn = c("positiv", "negativ"))



## ----no4_sent_sum-----------------------------------------------

no4_sent <- no4 %>% 
  group_by(titler) %>% 
  summarize(pos_sent = mean(pos_sent),
            neg_sent = mean(neg_sent)) %>% 
  mutate(sent = pos_sent - neg_sent)

no4_sent


## ----no4_vis----------------------------------------------------

no4_sent %>% 
  mutate(neg_sent = neg_sent * -1) %>% 
  ggplot(., aes(x = str_c(sprintf("%02d", 1:12),
                          ". ",
                          str_sub(titler, 1, 7),
                          "[...]"))) +
  geom_point(aes(y = neg_sent, color = "Negativ")) +
  geom_point(aes(y = pos_sent, color = "Positiv")) +
  geom_point(aes(y = sent, color = "Snitt")) +
  geom_linerange(aes(ymin = neg_sent, ymax = pos_sent), color = "gray40") +
  scale_color_manual(values = c("red", "cyan", "gray70")) +
  labs(x = NULL, y = "Sentiment", color = NULL) +
  ggdark::dark_theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = .25, hjust = 0))



## ----writeManifest, echo=FALSE, eval=FALSE----------------------
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


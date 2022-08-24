## ----setup, echo=FALSE, message=FALSE, error=FALSE, warning=FALSE------------------------------------------------------

# setwd("./undervisningsmateriell/seminarer")
library(knitr)
library(formatR)
library(rmdformats)

## Global options
# options(max.print = "75")
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  highlight = TRUE
)
#opts_knit$set(width = 75)

library(stringr)
library(dplyr) 
library(ggplot2)


# if("tidytext" %in% installed.packages() == FALSE) install.packages("tidytext")

load("./data/saker.rda")


## ----siste_commit, echo=FALSE------------------------------------------------------------------------------------------

gh_com <- "https://github.com/martigso/stv2022/commits/master.atom" %>% 
  rvest::read_html(.)


gh_com_title_latest <- gh_com %>% 
  rvest::html_elements("title") %>% 
  rvest::html_text() %>% 
  .[2] %>% 
  str_trim()

gh_com_date_latest <- gh_com %>% 
  rvest::html_elements("updated") %>% 
  rvest::html_text() %>% 
  .[2] %>% 
  as.Date()

cat(paste0(gh_com_title_latest, " (", gh_com_date_latest, ")"))


## ----laste_data_data, eval=-1------------------------------------------------------------------------------------------
 
library(stortingscrape)
#saker <- cases$root

saker %>% 
  select(id, document_group, status, title_short) %>% 
  mutate(title_short = str_sub(title_short, 1, 30)) %>% 
  tail()



## ----rda_save, eval=FALSE----------------------------------------------------------------------------------------------
## 
## save(saker, file = "./data/saker.rda")
## 


## ----rda_load----------------------------------------------------------------------------------------------------------

load("./data/saker.rda")



## ----csv---------------------------------------------------------------------------------------------------------------

library(readr)

saker <- read_csv("./data/saker.csv", show_col_types = FALSE)



## ----sav---------------------------------------------------------------------------------------------------------------
library(haven)
saker <- read_sav("./data/saker.sav")



## ----dta---------------------------------------------------------------------------------------------------------------

saker <- read_dta("./data/saker.dta")



## ----txt---------------------------------------------------------------------------------------------------------------

filer <- list.files("./data/txt", pattern = ".txt", full.names = TRUE)
filer

titler <- lapply(filer, readLines)
class(titler)

# Første tekst
titler[[1]]



## ----txt2--------------------------------------------------------------------------------------------------------------

names(titler) <- str_extract(filer, "[0-9]+")
names(titler)



## ----txt3--------------------------------------------------------------------------------------------------------------
saker_txt <- data.frame(titler = unlist(titler),
                        id = names(titler))


## ----txt4--------------------------------------------------------------------------------------------------------------
saker_merge <- left_join(saker_txt, saker[, c("id", "title")], by = "id")

saker_merge$titler == saker_merge$title



## ----msw_zip-----------------------------------------------------------------------------------------------------------

unzip("data/ba_thesis.docx", exdir = "data/wordfiles")

list.files("data/wordfiles/")



## ----read_msw_feil, warning=TRUE---------------------------------------------------------------------------------------

readLines("./data/ba_thesis.docx", n = 2)




## ----docx--------------------------------------------------------------------------------------------------------------

library(textreadr)

ba_docx <- read_docx("./data/ba_thesis.docx")

ba_docx[43:46]



## ----read_pdf----------------------------------------------------------------------------------------------------------

ba_pdf <- read_pdf("./data/ba_thesis.pdf")

ba_pdf <- ba_pdf$text[4] %>% 
  strsplit("\\n") %>% 
  unlist()

ba_pdf[11:14]



## ----wiki_bilde, out.width="100%", echo = FALSE------------------------------------------------------------------------
knitr::include_graphics("./figurer/wikipedia.png")


## ----wiki_bilde2, out.width="100%", echo = FALSE-----------------------------------------------------------------------
knitr::include_graphics("./figurer/wikipedia2.png")


## ----wiki_bilde3, out.width="50%", fig.align="center", echo = FALSE----------------------------------------------------
knitr::include_graphics("./figurer/wikipedia3.png")


## ----laste_rvest-------------------------------------------------------------------------------------------------------
library(rvest)


## ----laste_ned_appelsin, eval=FALSE------------------------------------------------------------------------------------
## 
## download.file("https://en.wikipedia.org/wiki/Orange_(fruit)", # Last ned en html-fil ...
##                 destfile = "./data/links/Oranges.html") # ... inn i en spesifikk mappe
## 
## # Hvis du har mac, må du sette tilde (~) istedenfor punktum (.)
## # Husk å være oppmerksom på hvor du har working directory, sjekk med getwd() og sett nytt working directory med setwd()
## 


## ----lese_appelsin, warning=FALSE, error=FALSE, message=FALSE, eval=-4-------------------------------------------------

library(rvest)

read_html("https://en.wikipedia.org/wiki/Orange_(fruit)") # Les inn direkte fra nettside

read_html("./data/links/Oranges.html") # Les inn fra din nedlastede fil



## ----appelsin_bilde, out.width="100%", echo = FALSE--------------------------------------------------------------------
knitr::include_graphics("./figurer/oranges1.png")


## ----appelsin_bilde2, out.width="100%", echo = FALSE-------------------------------------------------------------------
knitr::include_graphics("./figurer/oranges2.png")


## ----strukturere_appelsin----------------------------------------------------------------------------------------------

read_html("./data/links/Oranges.html") %>%
  html_node("#mw-content-text > div.mw-parser-output > p:nth-child(9)") %>%
  html_text(trim = TRUE)



## ----appelsin_bilde3, out.width="100%", echo = FALSE-------------------------------------------------------------------
knitr::include_graphics("./figurer/oranges3.png")


## ----appelsintabell----------------------------------------------------------------------------------------------------

read_html("./data/links/Oranges.html") %>%
  html_node("#mw-content-text > div.mw-parser-output > table.infobox.nowrap") %>%
  html_table()



## ----appelsintabell2---------------------------------------------------------------------------------------------------

read_html("./data/links/Oranges.html") %>%
  html_node("#mw-content-text > div.mw-parser-output > table.infobox.nowrap") %>%
  html_table() %>%
  na_if("") %>% # Erstatter "" med NA (missing)
  na.omit() # Fjerner alle NA



## ----trekke_ut_lenker, eval = FALSE------------------------------------------------------------------------------------
## 
## read_html("./data/links/Oranges.html") %>%
##   html_node("#mw-content-text > div.mw-parser-output > p:nth-child(9)") %>%
##   html_elements("a") %>%
##   html_attr("href")
## 


## ----trekke_ut_lenker2-------------------------------------------------------------------------------------------------

links <- read_html("./data/links/Oranges.html") %>%
  html_node("#mw-content-text > div.mw-parser-output > p:nth-child(9)") %>%
  html_elements("a") %>%
  html_attr("href") %>%
  str_extract("/wiki.*") %>% # Samle bare de URL-ene som starter med "/wiki", fulgt av hva som helst (.*)
  na.omit() %>% # Alle andre strenger blir NA, vi fjerner disse
  str_c("https://en.wikipedia.org/", .) # str_c limer sammen to strenger, vi limer på første halvdel av URL-en.



## ----laste_ned_flere_sider, eval=FALSE---------------------------------------------------------------------------------
## 
## linkstopic <- str_remove(links, "https://en.wikipedia.org//wiki/")
## 
## for(i in 1:length(links)) { # For alle lenkene...
## 
##   download.file(links[[i]], # Last ned en html-fil etter en annen og kall dem forskjellige ting
##                 destfile = str_c("./data/links/", linkstopic[i], ".html"))
## }
## 


## ----laste_inn_flere_filer---------------------------------------------------------------------------------------------

fruit_files <- list.files("./data/links", full.names = TRUE) # Liste med filene vi har lastet ned

info <- list() # Lag et liste-objekt hvor du kan putte output fra løkken

for (i in 1:length(fruit_files)) { # For hver enhet (i) som finnes i links, fra plass 1 til sisteplass i objektet (gitt med length(links))...
  
  page <- read_html(fruit_files[i]) # ... les html-filen for hver i
  
  page <- page %>% # Bruk denne siden
    html_elements("p") %>% # Og få tak i avsnittene
    html_text() # Deretter, hent ut teksten fra disse avsnittene
  
  info[[i]] <- page # Plasser teksten inn på sin respektive plass i info-objektet
  
}

# Info-objektet inneholder nå blant annet:

info[[1]][3]
info[[2]][3]
info[[3]][2]



## ----swapi_intro, echo=1:5, eval=6-------------------------------------------------------------------------------------

person1_url <- "https://swapi.dev/api/people/1/"

readLines(person1_url)

readLines("./data/swapi/person1.json")



## ----swapi_luke--------------------------------------------------------------------------------------------------------

library(jsonlite)

person1 <- read_json("./data/swapi/person1.json")

names(person1)
class(person1)

person1$name
person1$starships



## ----swapi_long_ex, file="r/swapi_ex.R", eval=FALSE, class.source="fold-hide"------------------------------------------
## 


## ----entur_dl, eval=FALSE----------------------------------------------------------------------------------------------
## 
## if(file.exists("./data/ruter.xml") == FALSE){
##   download.file(url = "https://api.entur.io/realtime/v1/rest/et?datasetId=RUT",
##                 destfile = "./data/ruter.xml")
## }
## 


## # Dette er en Unix-command som gjør -xml filer litt finere når vi printer dem i console

## xmllint --encode utf8 --format data/ruter.xml | sed -n 1185,1247p

## 


## ----entur_les, eval=FALSE---------------------------------------------------------------------------------------------
## 
## library(rvest)
## 
## ruter <- read_html("./data/ruter.xml")
## 


## ----entur_jukselitt, echo=FALSE, eval=TRUE----------------------------------------------------------------------------
load("./data/entur_alle_stop.rda")


## ----entur_strukt, eval=FALSE, echo=TRUE-------------------------------------------------------------------------------
## # Deler opp .xml-dokumentet i hver del som er innenfor
## # <recordedcall> . . . </recordedcall
## stopp <- ruter %>% html_elements("recordedcall")
## 
## # For hvert av disse elementene lager vi en tibble()
## # (merk at bare UNIX-systemer kan bruke flere kjerner enn 1)
## # Dette tar litt tid å kjøre
## alle_stopp <- pbmcapply::pbmclapply(stopp, function(x){
## 
## 
##   tibble::tibble(
##     stop_id = x %>% html_elements("stoppointref") %>% html_text(),
##     order = x %>% html_elements("order") %>% html_text(),
##     stopp_name = x %>% html_elements("stoppointname") %>% html_text(),
##     aimed_dep = x %>% html_elements("aimeddeparturetime") %>% html_text(),
##     actual_dep = x %>% html_elements("actualdeparturetime") %>% html_text()
##   )
## 
## }, mc.cores = parallel::detectCores()-1)
## 
## alle_stopp <- bind_rows(alle_stopp)
## 


## ----entur_wordcloud---------------------------------------------------------------------------------------------------

# Viser data
head(alle_stopp)

# Lager nytt datasett der ... 
stop_name_count <- alle_stopp %>% 
  count(stopp_name) %>%             # vi teller stoppnavn
  arrange(desc(n)) %>%              # sorterer data etter # linjer
  filter(nchar(stopp_name) > 3) %>% # tar bort korte stoppnavn
  slice_max(n = 30, order_by = n)   # tar med bare de 30 mest brukte stoppene


library(ggwordcloud)

# Setter opp tilfeldige farger
cols <- sample(colors(),
               size = nrow(stop_name_count),
               replace = TRUE)

# Lager plot
stop_name_count %>% 
  ggplot(., aes(label = stopp_name, 
                size = n,  
                color = cols)) +
  geom_text_wordcloud_area()+
  scale_size_area(max_size = 10) +
  ggdark::dark_theme_void()



## ----kravling, eval=FALSE----------------------------------------------------------------------------------------------
## # Laster inn pakke for kravling
## library(Rcrawler)
## 
## Rcrawler("http://virksommeord.no/", # Nettsiden vi skal kravle
##          DIR = "./crawl",           # mappen vi lagrer filene i
##          no_cores = 4,              # kjerner for å prosessere data
##          dataUrlfilter = "/tale/",  # subset filter for kravling
##          RequestsDelay = 2 + abs(rnorm(1)))


## ----bow, tidy=TRUE, results='markup'----------------------------------------------------------------------------------

regndans <- readLines("./data/regndans.txt")

bow <- regndans %>% str_split("\\s") %>% unlist()

set.seed(984301)

cat(bow[sample(1:length(bow))])



## ----dragepust---------------------------------------------------------------------------------------------------------

regndans[which(str_detect(regndans, "dragepust"))]



## ----regndans_full, echo=FALSE-----------------------------------------------------------------------------------------
cat(paste(regndans, collapse = "\n"))


## ----zipf, echo=TRUE, eval=TRUE, warning=FALSE-------------------------------------------------------------------------

library(janeaustenr)
library(dplyr)
library(tidytext)
library(ggplot2)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(line = row_number()) %>%
  ungroup()


tidy_books <- original_books %>%
  unnest_tokens(word, text) %>% 
  count(word) %>% 
  arrange(desc(n))

tidy_books %>% head(300) %>% 
  ggplot(., aes(x = 1:300, y = n)) +
  geom_point() +
  geom_line(aes(group = 1)) +
  scale_y_continuous(trans = "log") +
  scale_x_continuous(trans = "log") +
  geom_smooth(method = "lm", se = FALSE) +
  ggrepel::geom_label_repel(aes(label = word)) +
  ggdark::dark_theme_classic() +
  labs(x = "Rangering (log)", y = "Frekvens (log)", title = "Zipf's lov illustrasjon")



## ----quanteda_stopwords_nor, fold.output=FALSE-------------------------------------------------------------------------

quanteda::stopwords("no")



## ----no4_stoppordliste-------------------------------------------------------------------------------------------------
library(tidytext)

load("./data/no4.rda")

no4_tokens <- no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst) %>%
  count(token)

# Med stoppord
no4_tokens %>%
  slice_max(order_by = n,
            n = 2,
            with_ties = FALSE)

# Uten stoppord
no4_tokens %>%
  filter(token %in% quanteda::stopwords("no") == FALSE) %>% 
  slice_max(order_by = n,
            n = 2,
            with_ties = FALSE)




## ----no4_idf_stop------------------------------------------------------------------------------------------------------

idf_stop <- no4_tokens %>%
  bind_tf_idf(token, titler, n) %>% 
  ungroup() %>% 
  select(token, idf) %>% 
  unique() %>% 
  arrange(idf)

idf_stop



## ----no4_idf_stop_gone-------------------------------------------------------------------------------------------------

idf_stop <- idf_stop %>% 
  filter(idf < 1)

no4_tokens %>%
  filter(token %in% idf_stop$token == FALSE) %>% 
  slice_max(order_by = n,
            n = 2,
            with_ties = FALSE)




## ----fjerne_puktsetting------------------------------------------------------------------------------------------------

no4_tokens <- no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst) 

table(str_detect(no4_tokens$token, "[[:punct:]]"))

no4_tokens$token %>% 
  .[which(str_detect(., "[[:punct:]]"))]



## ----ikke_fjerne_puktsetting-------------------------------------------------------------------------------------------

no4_tokens <- no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst,
                strip_punct = FALSE) 

table(str_detect(no4_tokens$token, "[[:punct:]]"))



## ----fjerne_tall-------------------------------------------------------------------------------------------------------
no4_tokens <- no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst,
                strip_numeric = TRUE) 

table(str_detect(no4_tokens$token, "[0-9]"))



## ----stemming----------------------------------------------------------------------------------------------------------

stem1 <- tokenizers::tokenize_words("det satt to katter på et bord") %>% 
  unlist() %>% 
  quanteda::char_wordstem(., language = "no")

stem2 <- tokenizers::tokenize_words("det satt en katt på et bordet") %>% 
  unlist() %>% 
  quanteda::char_wordstem(., language = "no")

cbind(stem1, stem2, samme = stem1 == stem2)



## ----uregel_stemming---------------------------------------------------------------------------------------------------

stem1 <- tokenizers::tokenize_words("jeg har én god fot og én dårlig hånd") %>% 
  unlist() %>% 
  quanteda::char_wordstem(., language = "no")

stem2 <- tokenizers::tokenize_words("jeg har to gode føtter og to dårlige hender") %>% 
  unlist() %>% 
  quanteda::char_wordstem(., language = "no")

cbind(stem1, stem2, samme = stem1 == stem2)



## ----spacyr_ex, eval = FALSE, echo = TRUE, warning=FALSE---------------------------------------------------------------
## 
## library(spacyr)
## spacy_initialize("nb_core_news_lg")
## 
## spacy_eksempel <- spacy_parse(c("jeg har én god fot og én dårlig hånd",
##                                 "jeg har to gode føtter og to dårlige hender"))
## 
## 
## spacy_eksempel


## ----spacy_juksechunk, echo=FALSE--------------------------------------------------------------------------------------

load("./data/spacy_eksempel.rda")

spacy_eksempel



## ----merke_subverb-----------------------------------------------------------------------------------------------------

tekst2 <- stortingscrape::read_obt("./data/lemmatisering/tekst2_tag.txt")

tekst2



## ----pos_eks, warning=FALSE, eval=FALSE--------------------------------------------------------------------------------
## 
## grei1 <- "den snegler seg fremover"
## grei2 <- "det er mange snegler her"
## 
## grei <- spacy_parse(c(grei1, grei2)) %>%
##   tibble() %>%
##   select(doc_id, token, pos) %>%
##   filter(str_detect(token, "snegl")) %>%
##   mutate(token_pos = str_c(token, ":", pos))
## 
## grei


## ----spacy_juksechunk2, echo=FALSE-------------------------------------------------------------------------------------

load("./data/spacy_eksempel2.rda")

grei



## ----tokenizing_fig, out.width="100%", echo = FALSE--------------------------------------------------------------------
knitr::include_graphics("./figurer/tokenizing.jpeg")


## ----unigram-----------------------------------------------------------------------------------------------------------
no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst,
                token = "words")


## ----bigram------------------------------------------------------------------------------------------------------------
no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst,
                token = "ngrams",
                n = 2) 


## ----we_strukturere----------------------------------------------------------------------------------------------------

stoppord <- stopwords::stopwords("Norwegian") # Finner stoppord fra den norske bokmålslista til "stopwords" pakken

stoppord_boundary <- str_c("\\b", stoppord, "\\b", # Lager en vektor med "word boundary" for å ta ut ord fra en streng
                           collapse = "|") # Setter | mellom hver ord for å skille dem fra hverandre med "eller"-operator

no4_prepped <- no4 %>%
  mutate(tekst = str_to_lower(tekst), # Setter all tekst til liten bokstav
         tekst = str_replace_all(tekst, "[0-9]+", ""), # Fjerner tall fra teksten
         tekst = str_squish(tekst), # Fjerner whitespace
         tekst = str_replace_all(tekst, "\\b\\w{1,1}\\b", ""), # Fjerner enkeltbokstaver
         tekst = str_replace_all(tekst, stoppord_boundary, ""), # Fjerner stoppord
         tekst = str_replace_all(tekst, "[:punct:]", "")) # Fjerner all punktsetting



## ----we_tmpfile--------------------------------------------------------------------------------------------------------
no4_tekster <- tempfile() # Oppretter en midlertidig fil på PCen
writeLines(text = no4_prepped %>% pull(tekst), con = no4_tekster) # I denne filen skriver vi inn teksten fra datasettet. 


## ----fasttextr_train_test----------------------------------------------------------------------------------------------

library(fastTextR)

ft_cbow <- ft_train(no4_tekster, 
                    type = "cbow", # Velger cbow modell
                    control = ft_control(window_size = 5L)) # Setter kontekstvinduet til 5

ft_skipgram <- ft_train(no4_tekster, 
                        type = "skipgram", # Velger skipgram modell
                        control = ft_control(window_size = 5L))



## ----we_features-------------------------------------------------------------------------------------------------------
ft_word_vectors(ft_cbow, c("fordi", "himmel"))


## ----we_nearest_neighbors----------------------------------------------------------------------------------------------
ft_nearest_neighbors(ft_cbow, "himmel", k = 5L)


## ----norsentlex_hidden, echo=FALSE-------------------------------------------------------------------------------------
load("./data/nor_fullform_sent.rda")
load("./data/nor_lemma_sent.rda")


## ----norsentlex_load, eval=FALSE---------------------------------------------------------------------------------------
## # devtools::install_github("martigso/NorSentLex")
## 
## # library(NorSentLex)


## ----norsentlex_general------------------------------------------------------------------------------------------------

# Ordbøker i fullform
names(nor_fullform_sent)

# Ordbøker for lemma med PoS-tags
names(nor_lemma_sent)



## ----pos_fullform, echo=-1---------------------------------------------------------------------------------------------
set.seed(58493)
nor_fullform_sent$positive %>% head()
nor_fullform_sent$positive %>% tail()
nor_fullform_sent$positive %>% sample(., 6)



## ----pos_subst, echo=-1------------------------------------------------------------------------------------------------
set.seed(8943)
nor_lemma_sent$lemma_noun_positive %>% sample(., 6)



## ----regndans_sent_setup-----------------------------------------------------------------------------------------------

library(tidytext)

load("./data/no4.rda")

no4 <- no4 %>% 
  group_by(titler) %>% 
  unnest_tokens(ord, tekst)



## ----regndans_sent-----------------------------------------------------------------------------------------------------


no4$pos_sent <- ifelse(no4$ord %in% nor_fullform_sent$positive, 1, 0)
no4$neg_sent <- ifelse(no4$ord %in% nor_fullform_sent$negative, 1, 0)

table(no4$pos_sent, 
      no4$neg_sent, 
      dnn = c("positiv", "negativ"))



## ----no4_sent_sum------------------------------------------------------------------------------------------------------

no4_sent <- no4 %>% 
  group_by(titler) %>% 
  summarize(pos_sent = mean(pos_sent),
            neg_sent = mean(neg_sent)) %>% 
  mutate(sent = pos_sent - neg_sent)

no4_sent


## ----no4_vis-----------------------------------------------------------------------------------------------------------

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



## ----writeManifest, echo=FALSE, eval=FALSE-----------------------------------------------------------------------------
## 
## # Oppdaterer manifest -- jeg tror vi må kjøre denne manuelt hver
## # gang før vi pusher til github. uio-serveren er hvertfall ikke fornøyd
## # når jeg ikke gjør det.
## rsconnect::writeManifest("./")
## 
## # For å trekke ut koden fra hele boka til en R-fil
## knitr::purl("./notatbok.Rmd",
##             output = "./notatbok_kode.R",
##             documentation = 1)


## ----setup, echo=FALSE, message=FALSE, error=FALSE, warning=FALSE---------------------------

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


## ----siste_commit, echo=FALSE---------------------------------------------------------------

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


## ----laste_data_data, eval=-1---------------------------------------------------------------
 
library(stortingscrape)
#saker <- cases$root

saker %>% 
  select(id, document_group, status, title_short) %>% 
  mutate(title_short = str_sub(title_short, 1, 30)) %>% 
  tail()



## ----rda_save, eval=FALSE-------------------------------------------------------------------
## 
## save(saker, file = "./data/saker.rda")
## 


## ----rda_load-------------------------------------------------------------------------------

load("./data/saker.rda")



## ----csv------------------------------------------------------------------------------------

library(readr)

saker <- read_csv("./data/saker.csv", show_col_types = FALSE)



## ----sav------------------------------------------------------------------------------------
library(haven)
saker <- read_sav("./data/saker.sav")



## ----dta------------------------------------------------------------------------------------

saker <- read_dta("./data/saker.dta")



## ----txt------------------------------------------------------------------------------------

filer <- list.files("./data/txt", pattern = ".txt", full.names = TRUE)
filer

titler <- lapply(filer, readLines)
class(titler)

# Første tekst
titler[[1]]



## ----txt2-----------------------------------------------------------------------------------

names(titler) <- str_extract(filer, "[0-9]+")
names(titler)



## ----txt3-----------------------------------------------------------------------------------
saker_txt <- data.frame(titler = unlist(titler),
                        id = names(titler))


## ----txt4-----------------------------------------------------------------------------------
saker_merge <- left_join(saker_txt, saker[, c("id", "title")], by = "id")

saker_merge$titler == saker_merge$title



## ----msw_zip--------------------------------------------------------------------------------

unzip("data/ba_thesis.docx", exdir = "data/wordfiles")

list.files("data/wordfiles/")



## ----read_msw_feil, warning=TRUE------------------------------------------------------------

readLines("./data/ba_thesis.docx", n = 2)




## ----docx, eval=FALSE-----------------------------------------------------------------------
## 
## # library(textreadr)
## 
## ba_docx <- read_docx("./data/ba_thesis.docx")
## 
## ba_docx[43:46]
## 


## ----read_pdf, eval=FALSE-------------------------------------------------------------------
## 
## ba_pdf <- read_pdf("./data/ba_thesis.pdf")
## 
## ba_pdf <- ba_pdf$text[4] %>%
##   strsplit("\\n") %>%
##   unlist()
## 
## ba_pdf[11:14]
## 


## ----wiki_bilde, out.width="100%", echo = FALSE---------------------------------------------
knitr::include_graphics("./figurer/wikipedia.png")


## ----wiki_bilde2, out.width="100%", echo = FALSE--------------------------------------------
knitr::include_graphics("./figurer/wikipedia2.png")


## ----wiki_bilde3, out.width="50%", fig.align="center", echo = FALSE-------------------------
knitr::include_graphics("./figurer/wikipedia3.png")


## ----laste_rvest----------------------------------------------------------------------------
library(rvest)


## ----laste_ned_appelsin, eval=FALSE---------------------------------------------------------
## 
## download.file("https://en.wikipedia.org/wiki/Orange_(fruit)", # Last ned en html-fil ...
##                 destfile = "./data/links/Oranges.html") # ... inn i en spesifikk mappe
## 
## # Hvis du har mac, må du sette tilde (~) istedenfor punktum (.)
## # Husk å være oppmerksom på hvor du har working directory, sjekk med getwd() og sett nytt working directory med setwd()
## 


## ----lese_appelsin, warning=FALSE, error=FALSE, message=FALSE, eval=-4----------------------

library(rvest)

read_html("https://en.wikipedia.org/wiki/Orange_(fruit)") # Les inn direkte fra nettside

read_html("./data/links/Oranges.html") # Les inn fra din nedlastede fil



## ----appelsin_bilde, out.width="100%", echo = FALSE-----------------------------------------
knitr::include_graphics("./figurer/oranges1.png")


## ----appelsin_bilde2, out.width="100%", echo = FALSE----------------------------------------
knitr::include_graphics("./figurer/oranges2.png")


## ----strukturere_appelsin-------------------------------------------------------------------

read_html("./data/links/Oranges.html") %>%
  html_element("#mw-content-text > div.mw-parser-output > p:nth-child(9)") %>%
  html_text(trim = TRUE)



## ----appelsin_bilde3, out.width="100%", echo = FALSE----------------------------------------
knitr::include_graphics("./figurer/oranges3.png")


## ----appelsintabell-------------------------------------------------------------------------

read_html("./data/links/Oranges.html") %>%
  html_element("#mw-content-text > div.mw-parser-output > table.infobox.nowrap") %>%
  html_table()



## ----appelsintabell2------------------------------------------------------------------------

read_html("./data/links/Oranges.html") %>%
  html_element("#mw-content-text > div.mw-parser-output > table.infobox.nowrap") %>%
  html_table() %>%
  na_if("") %>% # Erstatter "" med NA (missing)
  na.omit() # Fjerner alle NA



## ----trekke_ut_lenker, eval = FALSE---------------------------------------------------------
## 
## read_html("./data/links/Oranges.html") %>%
##   html_elements("#mw-content-text > div.mw-parser-output > p:nth-child(9) > a") %>%
##   html_attr("href")
## 


## ----trekke_ut_lenker2----------------------------------------------------------------------

links <- read_html("./data/links/Oranges.html") %>%
  html_elements("#mw-content-text > div.mw-parser-output > p:nth-child(9) > a") %>%
  html_attr("href") %>%
  str_extract("/wiki.*") %>% # Samle bare de URL-ene som starter med "/wiki", fulgt av hva som helst (.*)
  na.omit() %>% # Alle andre strenger blir NA, vi fjerner disse
  str_c("https://en.wikipedia.org/", .) # str_c limer sammen to strenger, vi limer på første halvdel av URL-en.



## ----laste_ned_flere_sider, eval=FALSE------------------------------------------------------
## 
## linkstopic <- str_remove(links, "https://en.wikipedia.org//wiki/")
## 
## for(i in 1:length(links)) { # For alle lenkene...
## 
##   download.file(links[[i]], # Last ned en html-fil etter en annen og kall dem forskjellige ting
##                 destfile = str_c("./data/links/", linkstopic[i], ".html"))
## }
## 


## ----laste_inn_flere_filer------------------------------------------------------------------

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



## ----swapi_intro, echo=1:5, eval=6----------------------------------------------------------

person1_url <- "https://swapi.dev/api/people/1/"

readLines(person1_url)

readLines("./data/swapi/person1.json")



## ----swapi_luke-----------------------------------------------------------------------------

library(jsonlite)

person1 <- read_json("./data/swapi/person1.json")

names(person1)
class(person1)

person1$name
person1$starships



## ----swapi_long_ex, file="r/swapi_ex.R", eval=FALSE, class.source="fold-hide"---------------
## 


## ----entur_dl, eval=FALSE-------------------------------------------------------------------
## 
## if(file.exists("./data/ruter.xml") == FALSE){
##   download.file(url = "https://api.entur.io/realtime/v1/rest/et?datasetId=RUT",
##                 destfile = "./data/ruter.xml")
## }
## 


## # Dette er en Unix-command som gjør -xml filer litt finere når vi printer dem i console

## xmllint --encode utf8 --format data/ruter.xml | sed -n 1185,1247p

## 


## ----entur_les, eval=FALSE------------------------------------------------------------------
## 
## library(rvest)
## 
## ruter <- read_html("./data/ruter.xml")
## 


## ----entur_jukselitt, echo=FALSE, eval=TRUE-------------------------------------------------
load("./data/entur_alle_stop.rda")


## ----entur_strukt, eval=FALSE, echo=TRUE----------------------------------------------------
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


## ----entur_wordcloud------------------------------------------------------------------------

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



## ----kravling, eval=FALSE-------------------------------------------------------------------
## # Laster inn pakke for kravling
## library(Rcrawler)
## 
## Rcrawler("http://virksommeord.no/", # Nettsiden vi skal kravle
##          DIR = "./crawl",           # mappen vi lagrer filene i
##          no_cores = 4,              # kjerner for å prosessere data
##          dataUrlfilter = "/tale/",  # subset filter for kravling
##          RequestsDelay = 2 + abs(rnorm(1)))


## ----bow, tidy=TRUE, results='markup'-------------------------------------------------------

regndans <- readLines("./data/regndans.txt")

bow <- regndans %>% str_split("\\s") %>% unlist()

set.seed(984301)

cat(bow[sample(1:length(bow))])



## ----dragepust------------------------------------------------------------------------------

regndans[which(str_detect(regndans, "dragepust"))]



## ----regndans_full, echo=FALSE--------------------------------------------------------------
cat(paste(regndans, collapse = "\n"))


## ----zipf, echo=TRUE, eval=TRUE, warning=FALSE----------------------------------------------

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



## ----quanteda_stopwords_nor, fold.output=FALSE----------------------------------------------

quanteda::stopwords("no")



## ----no4_stoppordliste----------------------------------------------------------------------
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




## ----no4_idf_stop---------------------------------------------------------------------------

idf_stop <- no4_tokens %>%
  bind_tf_idf(token, titler, n) %>% 
  ungroup() %>% 
  select(token, idf) %>% 
  unique() %>% 
  arrange(idf)

idf_stop



## ----no4_idf_stop_gone----------------------------------------------------------------------

idf_stop <- idf_stop %>% 
  filter(idf < 1)

no4_tokens %>%
  filter(token %in% idf_stop$token == FALSE) %>% 
  slice_max(order_by = n,
            n = 2,
            with_ties = FALSE)




## ----fjerne_puktsetting---------------------------------------------------------------------

no4_tokens <- no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst) 

table(str_detect(no4_tokens$token, "[[:punct:]]"))

no4_tokens$token %>% 
  .[which(str_detect(., "[[:punct:]]"))]



## ----ikke_fjerne_puktsetting----------------------------------------------------------------

no4_tokens <- no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst,
                strip_punct = FALSE) 

table(str_detect(no4_tokens$token, "[[:punct:]]"))



## ----fjerne_tall----------------------------------------------------------------------------
no4_tokens <- no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst,
                strip_numeric = TRUE) 

table(str_detect(no4_tokens$token, "[0-9]"))



## ----stemming-------------------------------------------------------------------------------

stem1 <- tokenizers::tokenize_words("det satt to katter på et bord") %>% 
  unlist() %>% 
  quanteda::char_wordstem(., language = "no")

stem2 <- tokenizers::tokenize_words("det satt en katt på et bordet") %>% 
  unlist() %>% 
  quanteda::char_wordstem(., language = "no")

cbind(stem1, stem2, samme = stem1 == stem2)



## ----uregel_stemming------------------------------------------------------------------------

stem1 <- tokenizers::tokenize_words("jeg har én god fot og én dårlig hånd") %>% 
  unlist() %>% 
  quanteda::char_wordstem(., language = "no")

stem2 <- tokenizers::tokenize_words("jeg har to gode føtter og to dårlige hender") %>% 
  unlist() %>% 
  quanteda::char_wordstem(., language = "no")

cbind(stem1, stem2, samme = stem1 == stem2)



## ----spacyr_ex, eval = FALSE, echo = TRUE, warning=FALSE------------------------------------
## 
## library(spacyr)
## spacy_initialize("nb_core_news_lg")
## 
## spacy_eksempel <- spacy_parse(c("jeg har én god fot og én dårlig hånd",
##                                 "jeg har to gode føtter og to dårlige hender"))
## 
## 
## spacy_eksempel


## ----spacy_juksechunk, echo=FALSE-----------------------------------------------------------

load("./data/spacy_eksempel.rda")

spacy_eksempel



## ----merke_subverb--------------------------------------------------------------------------

tekst2 <- stortingscrape::read_obt("./data/lemmatisering/tekst2_tag.txt")

tekst2



## ----pos_eks, warning=FALSE, eval=FALSE-----------------------------------------------------
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


## ----spacy_juksechunk2, echo=FALSE----------------------------------------------------------

load("./data/spacy_eksempel2.rda")

grei



## ----tokenizing_fig, out.width="100%", echo = FALSE-----------------------------------------
knitr::include_graphics("./figurer/tokenizing.jpeg")


## ----unigram--------------------------------------------------------------------------------
no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst,
                token = "words")


## ----bigram---------------------------------------------------------------------------------
no4 %>%
  group_by(spor, titler) %>%
  unnest_tokens(output = token,
                input = tekst,
                token = "ngrams",
                n = 2) 


## ----we_strukturere-------------------------------------------------------------------------

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



## ----we_tmpfile-----------------------------------------------------------------------------
no4_tekster <- tempfile() # Oppretter en midlertidig fil på PCen
writeLines(text = no4_prepped %>% pull(tekst), con = no4_tekster) # I denne filen skriver vi inn teksten fra datasettet. 


## ----fasttextr_train_test-------------------------------------------------------------------

library(fastTextR)

ft_cbow <- ft_train(no4_tekster, 
                    type = "cbow", # Velger cbow modell
                    control = ft_control(window_size = 5L)) # Setter kontekstvinduet til 5

ft_skipgram <- ft_train(no4_tekster, 
                        type = "skipgram", # Velger skipgram modell
                        control = ft_control(window_size = 5L))



## ----we_features----------------------------------------------------------------------------
ft_word_vectors(ft_cbow, c("fordi", "himmel"))


## ----we_nearest_neighbors-------------------------------------------------------------------
ft_nearest_neighbors(ft_cbow, "himmel", k = 5L)


## ----governing_with_words, out.width="40%", fig.align='center', echo = FALSE----------------
knitr::include_graphics("./figurer/governing_with_words.jfif")


## ---- message=FALSE, warning = FALSE, error = FALSE-----------------------------------------

library(tidyverse)
library(jsonlite)
library(plotly)

tedtalks <- read_csv("./data/teds.csv") %>% # Leser inn data fra en .csv-fil
  janitor::clean_names() # Bruker funksjonen "clean_names" fra pakken "janitor" for å gjøre variabelnavnene litt penere

tedtalks_subset <- tedtalks %>% # Fra objektet tedtalks
  rename(doc_id = id) %>% # Endrer navn på variabelen "id" til å hete "doc_id" isteden
  select(doc_id, transcript, topics) %>% # Henter ut variablene doc_id, transcript og topics
  mutate(topics = map(topics, # Variabelen topics ligger inne i json-format, jeg bruker "map" for å gå over hver rad 
                      ~ fromJSON(.) %>% # og gjør om fra json-format slik at vi får to variabler "id" på topic og "name" på topic
                        as_tibble())) %>% # Gjør om til en vanlig tibble (en R dataframe)
  unnest(cols = c(topics)) %>% # Hver talk har flere topics, når jeg bruker unnest får hver topic sin egen rad
  select(-id) # Fjerner variabelen som indikerer topicet sin id

tedtalks_subset %>%
  count(name) %>% # Teller opp hvor mange ganger hver topic dukker opp
  plot_ly(x = ~n, # Plotter med plotly-pakken (kunne også brukt ggplot, men da blir det ikke interaktivt)
          y = ~name, # Setter antall ganger topic dukker opp på x-aksen og navnet på topicet på y-aksen
          text = ~name, # Når man hovrer over søylene, skal man få opp navn på topic
          type = "bar")  %>% # Lager et stolpediagram (bar chart)
  layout(xaxis = list(title = ""), # Ønsker ikke at det skal være tekst på y-eller x-aksen
         yaxis = list(title = "", categoryorder = "total ascending")) # Sorter søylene i synkende rekkefølge ift. y-aksen (altså antallet)



## ---- message=FALSE, warning = FALSE, error = FALSE-----------------------------------------

tedtalks_subset <- tedtalks_subset %>%
  filter(name %in% c("politics", "international relations")) %>% # Hent ut de talene som har politics og/eller international relations som tema
  mutate(tema = "politikk") %>% # Lag en variabel "topic" med verdien "politikk".
  select(doc_id, tema) %>% # Hent ut id på TED-talken og dens topic
  unique() # Fjern duplikater

ted <- tedtalks_subset %>%
  full_join(tedtalks %>% # Sett sammen datasettet som bare inneholder talks som handlet om politikk, med det fulle datasettet...
              select(id, transcript, topics), # ...der jeg plukker ut kun variablene "id", "transcript" og "topics"
            by = c("doc_id" = "id")) %>% # Endre navn på id til doc_id
  mutate(tema = replace_na(tema, "annet")) %>% # Variabel "tema" har missingverdien for de talkene som ikke handler om politikk, jeg bytter ut missing med "annet"
  select(transcript, tema) %>% # Hent ut to kolonner, kolonnen med tekst (transcript) og kolonnen med merke (tema)
  rename(text = transcript) %>% # Endre navn på transcript-variabelen til "text" (for at senere kode skal fungere)
  drop_na(text) # Fjern missingverdier fra variabelen "text"



## -------------------------------------------------------------------------------------------

ted %>% 
  count(tema) # Tell opp hvor mange rader som har hver sin verdi på variabelen "tema". 



## ----overfitting, out.width="50%", fig.align='center', echo = FALSE-------------------------
knitr::include_graphics("./figurer/overfitting.png")


## ----testtreningvalideringssett, out.width="50%", fig.align='center', echo = FALSE----------
knitr::include_graphics("./figurer/split_data2.png")


## ---- message=FALSE, warning = FALSE, error = FALSE-----------------------------------------

library(tidymodels)

set.seed(930) # Denne koden gjør at R gir tilsvarende resultater hver gang man kjører samme kode (dvs. splitter på samme sted)

ted_splitt <- initial_split(ted, #  Del datasettet i to
                            prop = 0.8, # 80 prosent av data (dvs. 80 prosen av radene/talksene) skal gå inn i treningsdata, resten blir testdata
                            strata = tema) # Passer på at Y, tema, er godt representert i både treningsdatasett og testdatasett

ted_trening <- training(ted_splitt) # Lager treningsdatasett
ted_test <- testing(ted_splitt) # Lager testdatasett

ted_trening %>% head()
ted_test %>% head()

ted_folds <- vfold_cv(ted_trening, # Splitt treningsdatasettet inn i valideringsdatasett 
                      strata = tema, # Passer på at Y, tema, er godt representert i både valideringsdatasett og treningsdatasett
                      v = 5) # Splitter opp fem ganger (slik at vi får "5-fold cross validation")

ted_folds



## ---- message=FALSE, warning = FALSE, error = FALSE-----------------------------------------

library(textrecipes)
library(quanteda)
library(tm)

ted_oppskrift <- recipe(tema ~ ., data = ted_trening) %>% # Modellen jeg ønsker å kjøre - jeg vil estimere Y ved å bruke resten av dataene
  step_mutate(text = str_to_lower(text)) %>% # Setter alle til liten bokstav
  step_mutate(text = removeNumbers(text)) %>%  # Fjerner tall
  step_mutate(text = removePunctuation(text)) %>% # Fjerner punktsetting
  step_tokenize(text) %>% # Tokeniserer teksten
  step_stem(text) %>% # Lager ordstammer
  step_stopwords(text, custom_stopword_source = stopwords("en")) %>% # Fjerner stoppord
  step_tokenfilter(text, max_tokens = 1000, min_times = 2) %>% # Beholder tokens som dukker opp maks 1000 ganger, fjerner de som dukker opp mindre enn 2 ganger
  step_tfidf(text) # Vektoriserer teksten med TF-IDF

prep(ted_oppskrift) %>% # Iverksetter preprosesseringsstegene slik beskrevet i oppskriften over
  bake(new_data = NULL) %>% # Ser på hvordan oppskrifts-objektet ser ut
  head(5) %>% select(1:5) # Henter ut de fem første radene, og de fem første kolonnene



## -------------------------------------------------------------------------------------------
contrl_preds <- control_resamples(save_pred = TRUE) # Velger å lagre prediksjonene etter at modellen har kjørt


## ---- eval = FALSE--------------------------------------------------------------------------
## 
## glmn_spec <-
##   logistic_reg(penalty = 0.001, # Setter et par argumenter for å forhinde modeller fra å overtilpasse seg
##                mixture = 0.5) %>% # Dette er typisk noe man går fram og tilbake med (kalt å "tune" modellen)
##   set_engine("glmnet") %>% # Logistisk modell får vi ved å spesifisere "glmnet"
##   set_mode("classification") # Vi ønsker klassifisering, ikke regresjon
## 
## glm_wf <- workflow(ted_oppskrift, # Datasettet vårt etter preprosessering
##                    glmn_spec) # Modellen som spesifisert over, altså logitisk
## 
## glm_rs <- fit_resamples( # Passer modellen ved å bruke testdata og valideringsdata i sekvens fem ganger
##   glm_wf, # Dette objektet forteller hva som er data og hva som er modellen
##   resamples = ted_folds, # Spesifiserer hva valideringsdataene er
##   control = contrl_preds # Legger valgene som jeg lagret over
## )
## 


## ---- echo = FALSE--------------------------------------------------------------------------
load("./data/glm_rs.rda")


## ---- eval = FALSE--------------------------------------------------------------------------
## 
## rf_spec <-
##   rand_forest(trees = 500) %>% # Spesifiserer valg for å prøve å sørge for at modellen er best mulig tilpasset data
##   set_mode("classification")
## 
## rf_wf <- workflow(ted_oppskrift,
##                   rf_spec)
## 
## ranger_rs <- fit_resamples(
##   rf_wf,
##   resamples = ted_folds,
##   control = contrl_preds
## )
## 


## ---- echo = FALSE--------------------------------------------------------------------------
load("./data/ranger_rs.rda")


## ---- eval = FALSE--------------------------------------------------------------------------
## 
## svm_spec <-
##   svm_rbf() %>%
##   set_engine("kernlab", scaled = FALSE) %>% # Valg for å tune modellen
##   set_mode("classification")
## 
## svm_wf <- workflow(ted_oppskrift,
##                    svm_spec)
## 
## svm_rs <- fit_resamples(
##   svm_wf,
##   resamples = ted_folds,
##   control = contrl_preds,
## )
## 


## ---- echo = FALSE--------------------------------------------------------------------------

svm_spec <-
  svm_rbf() %>%
  set_engine("kernlab", scaled = FALSE) %>% # Valg for å tune modellen
  set_mode("classification") 

svm_wf <- workflow(ted_oppskrift, 
                   svm_spec)

load("./data/svm_rs.rda")


## ----klasse1, out.width="80%", fig.align='center', echo = FALSE-----------------------------
knitr::include_graphics("./figurer/klass1.PNG")


## ----klasse2, out.width="80%", fig.align='center', echo = FALSE-----------------------------
knitr::include_graphics("./figurer/klass2.PNG")


## -------------------------------------------------------------------------------------------

collect_metrics(glm_rs) # Accuracy til den logistiske modellen er på 95 prosent. Den klassifiserer 95 prosent av observasjonene riktig.
collect_metrics(ranger_rs) # Random forest klassifiserer 95,2 prosent av observasjonene riktig
collect_metrics(svm_rs) # SVM klassifiserer 95,9 prosent riktig.



## -------------------------------------------------------------------------------------------
# Lager forvirringsmatriser for hver av modellene

metrikk_glm <- collect_predictions(glm_rs)
metrikk_glm %>%
  conf_mat(truth = tema, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

metrikk_rf <- collect_predictions(ranger_rs) 
metrikk_rf %>%
  conf_mat(truth = tema, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

metrikk_svm <- collect_predictions(svm_rs) 
metrikk_svm %>%
  conf_mat(truth = tema, estimate = .pred_class) %>%
  autoplot(type = "heatmap")



## -------------------------------------------------------------------------------------------
# Undersøker recall for hver av modellene
spec(metrikk_rf, metrikk_rf$.pred_class, metrikk_rf$tema)
spec(metrikk_glm, metrikk_glm$.pred_class, metrikk_glm$tema)
spec(metrikk_svm, metrikk_svm$.pred_class, metrikk_svm$tema)


## -------------------------------------------------------------------------------------------

final_fitted <- last_fit(svm_wf, ted_splitt) # Passer SVM-modellen til testdatasettet

collect_metrics(final_fitted) # Sjekker hvor bra modellen gjorde det (accuracy og ROC-kurve)



## ---- eval = FALSE--------------------------------------------------------------------------
## 
## library(tidytext)
## library(quanteda)
## library(rainette)
## 
## tedtalks_tfidf <- tedtalks %>%
##   group_by(id) %>%
##   unnest_tokens(input = transcript,
##                 output = token,
##                 strip_punct = TRUE,
##                 strip_numeric = TRUE,
##                 token = "words") %>%
##   filter(!token %in% stopwords("en")) %>%
##   count(token) %>%
##   bind_tf_idf(token, id, n) %>%
##   na.omit()
## 
## tedtalks_dfm <- tedtalks_tfidf %>%
##   cast_dfm(id, token, tf_idf) %>%
##   dfm_trim(min_termfreq = 2, max_termfreq = 1000)
## 
## rainette_cluster <- rainette(
##   tedtalks_dfm,
##   k = 8)
## 


## ---- echo = FALSE--------------------------------------------------------------------------
load("./data/rainette_cluster.rda")
load("./data/tedtalks_dfm.rda")
library(rainette)


## -------------------------------------------------------------------------------------------
rainette_plot(rainette_cluster, tedtalks_dfm, k = 6)


## ---- out.width="70%", fig.align="center", echo = FALSE-------------------------------------
knitr::include_graphics("./figurer/rulebased.PNG")


## -------------------------------------------------------------------------------------------

ordbok <- tibble(positiv = c("glad", "beste", "fantastisk"), # Lag en dataframe (også kalt 'tibble') med variabler positiv og negativ
                 negativ = c("dårlig", "elendig", "forferdelig")) # Legg inn som verdier ordene som regnes som positive og negative

ordbok



## -------------------------------------------------------------------------------------------

library(dplyr)
library(tidytext)

# Legger tekstene til Bennie og Bjarne inn i en ny dataframe "tekster"
tekster <- tibble(person = c("Bennie", "Bjarne"),
                  tekst = c("Dette har vært en fantastisk ferie. Jeg blir så glad av å se på bildene, selv om de har litt dårlig kvalitet.",
"Det er helt forferdelig at de har glemt bagasjen min, det var der jeg hadde mine beste bukser. For en elendig service."))

tekster %>%
  unnest_tokens(input = tekst, # Hent ut bag of words ved bruk av unnest_tokens, hent ut ord fra variabelen "tekst"
                output = ord) %>% # Kall den nye variabelen "ord"
  mutate(positiv = ifelse(ord %in% ordbok$positiv, 1, 0), # Lag en ny variabel "positiv" og gi den verdi 1 hver gang variabelen "ord" har samme verdi som variabelen "positiv" i ordbok-datasettet vi lagde over
         negativ = ifelse(ord %in% ordbok$negativ, 1, 0)) %>% # Gjør samme operasjon for negative ord
  group_by(person) %>% # Grupper på person
  summarise(positiv = sum(positiv), # Legg sammen hvor mange ganger hver person hadde positive ord i teksten sin
            negativ = sum(negativ)) %>% # Og hvor mange ganger hver person hadde negative ord i teksten sin
  mutate(sentiment = positiv - negativ) # Lag en ny variabel "sentiment" der jeg trekker summen av positive ord fra summen av negative ord



## -------------------------------------------------------------------------------------------

library(tidyverse) # Pakke som brukes for veldig mange funksjoner
library(tidytext) # Pakke som brukes for veldig mange tekst-funksjoner
library(quanteda.textstats) # Pakke som har mange funksjoner for å finne tekststatistikk

tedtalks <- read_csv("./data/teds.csv") %>% # Leser inn data fra en .csv-fil
  janitor::clean_names() # Bruker funksjonen clean_names() fra janitor-pakken for å gjøre om variabelnavnene til små bokstaver med understrek



## -------------------------------------------------------------------------------------------

tedtalks_token <- tedtalks %>%
  group_by(title) %>%
  unnest_tokens(input = transcript, # Gjør om tekstene (variabel med tekstene er kalt "transcript")
                output = token, # Gjør dem om til enkeltord kalt "token"
                token = "words", # Skal ha unigram (ett og ett ord)
                strip_punct = TRUE, # Fjern punktsetting
                strip_numeric = TRUE) %>% # Fjern tall
  filter(!token %in% quanteda::stopwords("en")) # Fjern stoppord (engelsk siden TED-talksene er på engelsk)



## -------------------------------------------------------------------------------------------

tedtalks_tfidf <- tedtalks_token %>%
  count(token, name = "antall") %>% # Tell opp hvor ofte hver token dukker opp (dette gir oss sekk med ord), kaller variabelen "antall"
  bind_tf_idf(token, title, antall) # Regn om frekvensen av ord til TF-IDF - må supplere med variabel som inneholder ord (token), variabel med navn på dokument (title) og variabel med ordfrekvensen (antall)



## -------------------------------------------------------------------------------------------

tedtalks_dfm <- tedtalks_tfidf %>%
  cast_dfm(document = title, # Lag en dokument-frekevens-matrise med funksjonen cast_dfm(), fortell hva som er dokumentnavnene
           term = token, # Hva som er navnet på variabelen som inneholder tokens
           value = tf_idf) # Hva som skal bli verdien til hver observasjon på hver variabel (TF-IDF i dette tilfellet)



## -------------------------------------------------------------------------------------------

simil_tf_idf_ted <- textstat_simil(tedtalks_dfm, method = "cosine") # Funksjon for å regne ut cosine-likhet



## -------------------------------------------------------------------------------------------

simil_tf_idf_ted %>% # Går inn i cosine-likhet objektet som ble lagd over
  as.data.frame(., diag = FALSE) %>% # Gjør om objektet til en dataframe der diagonalen er 0 (tekster likhet med seg selv er uinteressant)
  arrange(desc(cosine)) %>% # Sorter observasjonene i synkende rekkefølge etter cosine-variabelen
  slice_head(n = 20) # Hent ut bare de 20 øverste observasjonene



## -------------------------------------------------------------------------------------------

tedtalks %>% 
  filter(title %in% c("A dance in a hurricane of paper, wind and light", # Hent ut observasjonene som hadde følgende verdier på variabelen "tittel"
                      "An 11-year-old prodigy performs old-school jazz")) %>% 
  select(title, transcript) # Hent ut variabelene title og transcript (altså tittel på talen og teksten på talen)



## ---- eval = FALSE--------------------------------------------------------------------------
## 
## tedtalks <- tedtalks %>%
##   filter(!title %in% c("A dance in a hurricane of paper, wind and light", # Filter med utropstegn foran betyr "ikke"
##                        "An 11-year-old prodigy performs old-school jazz")) # Altså henter vi ut alle observasjoner unntatt de med følgende verdier på variabelen "title"
## 


## -------------------------------------------------------------------------------------------

tedtalks %>% 
  filter(title %in% c("Questions no one knows the answers to",
                      "How many universes are there?")) %>% 
  select(title, transcript) 



## ---- eval = FALSE--------------------------------------------------------------------------
## textstat_dist(tedtalks_dfm, method = "euclidean")


## ---- eval = FALSE--------------------------------------------------------------------------
## textstat_dist(tedtalks_dfm, method = "manhattan")


## ---- eval = FALSE--------------------------------------------------------------------------
## textstat_dist(tedtalks_dfm, method = "minkowski")


## -------------------------------------------------------------------------------------------

dist_tf_idf_ted <- textstat_dist(tedtalks_dfm, method = "euclidean") # Regn ut euclidean-distanse mellom tekstene

dist_tf_idf_ted %>% 
  as.data.frame(., diag = FALSE) %>% # Lag en dataframe og sett diagnonalen til 0 (teksters ulikhet med seg selv er uinteressant)
  arrange(desc(euclidean)) %>% # Sorter observasjonene i synkende rekkefølge etter euclidean-variabelen
  slice_head(n = 10) # Hent ut de 10 øverste radene



## -------------------------------------------------------------------------------------------
tedtalks %>%
  filter(title == "How I turned my Tourette's tics into art") %>%
  select(title, transcript)


## -------------------------------------------------------------------------------------------

set.seed(721) # Dette gjør at vi henter ut de samme 20 tilfeldige observasjonene hver gang vi kjører koden

dist_tf_idf_ted %>% 
  as.data.frame(., diag = FALSE) %>% # Lager om distanse-målet til en dataframe
  filter(document1 %in% c("Your body was forged in the spectacular death of stars",
                          "What coronavirus means for the global economy",
                          "What reality are you creating for yourself?",
                          "What playing Monopoly with real money taught me about my kids--and humanity",
                          "What makes the Great Wall of China so extraordinary",
                          "What it's like to be a transgender dad")) %>%
  filter(document2 %in% c("Your body was forged in the spectacular death of stars",
                          "What coronavirus means for the global economy",
                          "What reality are you creating for yourself?",
                          "What playing Monopoly with real money taught me about my kids--and humanity",
                          "What makes the Great Wall of China so extraordinary",
                          "What it's like to be a transgender dad")) %>%
  ggplot(aes(x = document1, y = document2, fill = euclidean)) + # Plotter dokumentene mot hverandre med distansemålet som verdi
  geom_tile(stat = "identity") + # geom_tile gir et varmediagram
  geom_text(aes(label = ifelse(euclidean > .001, round(euclidean, 2), ""))) + # Legger tekst inn i cellene
  scale_fill_gradient2(low = "green", mid = "white", high = "blue", midpoint = 0) + # Bestemmer fargene i cellene
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) + # Vil ikke ha noe informasjon på aksene
  theme_minimal() + # Hvit plottebakgrunn
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) # Bestemmer vinkel og plassering på teksten på x-aksen



## ----norsentlex_hidden, echo=FALSE----------------------------------------------------------
load("./data/nor_fullform_sent.rda")
load("./data/nor_lemma_sent.rda")


## ----norsentlex_load, eval=FALSE------------------------------------------------------------
## # devtools::install_github("martigso/NorSentLex")
## 
## # library(NorSentLex)


## ----norsentlex_general---------------------------------------------------------------------

# Ordbøker i fullform
names(nor_fullform_sent)

# Ordbøker for lemma med PoS-tags
names(nor_lemma_sent)



## ----pos_fullform, echo=-1------------------------------------------------------------------
set.seed(58493)
nor_fullform_sent$positive %>% head()
nor_fullform_sent$positive %>% tail()
nor_fullform_sent$positive %>% sample(., 6)



## ----pos_subst, echo=-1---------------------------------------------------------------------
set.seed(8943)
nor_lemma_sent$lemma_noun_positive %>% sample(., 6)



## ----regndans_sent_setup--------------------------------------------------------------------

library(tidytext)

load("./data/no4.rda")

no4 <- no4 %>% 
  group_by(titler) %>% 
  unnest_tokens(ord, tekst)



## ----regndans_sent--------------------------------------------------------------------------


no4$pos_sent <- ifelse(no4$ord %in% nor_fullform_sent$positive, 1, 0)
no4$neg_sent <- ifelse(no4$ord %in% nor_fullform_sent$negative, 1, 0)

table(no4$pos_sent, 
      no4$neg_sent, 
      dnn = c("positiv", "negativ"))



## ----no4_sent_sum---------------------------------------------------------------------------

no4_sent <- no4 %>% 
  group_by(titler) %>% 
  summarize(pos_sent = mean(pos_sent),
            neg_sent = mean(neg_sent)) %>% 
  mutate(sent = pos_sent - neg_sent)

no4_sent


## ----no4_vis--------------------------------------------------------------------------------

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



## ---- eval=T--------------------------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(tm)

brexit <- readRDS("./data/brexit.rds") # Leser inn datasettet



## ---- eval=T--------------------------------------------------------------------------------

set.seed(201) # Sår et frø sånn at vi splitter opp på de samme radene i trening-, validering- og testsett hver gang

brexit_classify <- brexit %>% 
  filter(sentiment != "Not Rated") %>% # Fjerner kategori "Not Rated", ettersom vi ikke er interessert i å kategorisere denne
  drop_na(influencer) %>% # Fjern missingverdier fra influencer-variabelen 
  select(influencer, hit_sentence, sentiment) %>% # Hent ut variablene influener (den som skrev tweeten), hit_sentence (teksten i tweeten) og sentiment (hva slags sentiment tweeten er klassifisert til)
  rename(text = hit_sentence) %>% # Endre navn på tekst-variabelen til "text" for å gjøre preprosessering mer oversiktlig
  slice_sample(n = 1000) # Henter ut 1000 tilfeldige rader (dvs. observasjoner) for illustrasjonsformål.

brexit_classify %>%
  slice_head(n = 5) # Viser de fem første radene i datasettet 




## ---- eval=T--------------------------------------------------------------------------------

brexit_classify %>%
  janitor::tabyl(sentiment) # Ser hvor mange observasjoner som har hver sin klasse



## ---- eval=T--------------------------------------------------------------------------------

brexit_classify_split <- initial_split(brexit_classify, # Splitt opp datasettet
                                       strata = sentiment, # Balanser etter utfallsvariabel, sentiment
                                       prop = 0.8) # 80 prosent av radene i datasettet går i treningssettet

brexit_trening <- training(brexit_classify_split) # Hent ut treningssettet fra splitten
brexit_validering <- vfold_cv(brexit_trening, v = 5) # Lag valideringssett av treningsdataene, vi lager fem valideringssett

brexit_testing <- testing(brexit_classify_split) # Hent ut testsettet fra splitten



## ---- eval=T--------------------------------------------------------------------------------

library(textrecipes)

brexit_oppskrift <- recipe(sentiment ~ text + influencer, data = brexit_trening) %>% 
  # Forteller hva som er modellen - sentiment er avhengig variabel mens teksten er uavhengig variabel
  update_role(influencer, new_role = "id variable") %>% # La med influencer over, her spesifiserer jeg at dette er id-variabel slik at den ikke brukes som en uavhengig variabel i analysen
  step_text_normalization(text) %>% # Normaliserer teksten, fjerner unicode-støy
  step_mutate(text = str_to_lower(text)) %>% # Setter alle bokstaver til liten bokstav
  step_mutate(text = removeNumbers(text)) %>% # Fjerner tall
  step_mutate(text = removePunctuation(text)) %>% # Fjerner punktsetting
  step_tokenize(text) %>% # Tokeniserer teksten til unigrams
  step_stopwords(text, custom_stopword_source = stopwords("en")) %>% # Fjerner stoppord
  step_tokenfilter(text, max_tokens = tune()) %>% # Setter en tuningsvariabel som vil finne det optimale antallet tokens å fjerne for å få best mulig treffsikkerhet i modellen senere
  step_stem(text) %>% # Tar stammen av tokensene
  step_tfidf(text) # Vektoriserer ved å hente ut tf-idf 



## ---- eval=T--------------------------------------------------------------------------------

brexit_modell <- svm_rbf(cost = 3, 
                         rbf_sigma = NULL) %>% # Setter noen "tuning-argumenter". Dere kan lære mer om dette i andre maskinlæringskurs.
  set_engine("kernlab") %>% # Måten SVM estimerer på
  set_mode("classification") # Vi ømsker klassifisering, ikke regresjon

# Lager en workflow-oppskrift
brexit_wf <- workflow() %>% 
  add_recipe(brexit_oppskrift) %>%
  add_model(brexit_modell)

# Tuner modellen
tune_res <- tune_grid(
  brexit_wf, # Setter inn workflow-objektet slik det ble lagd over
  resamples = brexit_validering, # Trener med valideringssettet som sjekk
  grid = 5, # Kjører over fem ganger (bør økes i egne analyser) 
  metrics = metric_set(accuracy, sens, spec), # Henter ut treffsikkerhetsmålene "accuracy", "sensitivity" og "specificity" - noen mål er bedre enn andre for å si noe om hvor god modellen er avhengig av om man er mest opptatt av sanne positive, falske positive, falske negative eller sanne negative.
  control = control_grid(verbose = FALSE, # Ikke la modellen forklare oss hva den driver med mens den kjører
                         save_pred = TRUE) # Lagre prediksjonene
)

tune_res %>%
  autoplot(type = "marginals") # Her ser vi et plott som viser noe om hvordan modellen har valgt hva som er optimalt antall tokens gitt accuracy, sensitivity og specificity. 



## ---- eval=T--------------------------------------------------------------------------------

collect_metrics(tune_res) %>% 
  arrange(desc(mean))



## ---- eval=T--------------------------------------------------------------------------------

best_accuracy <- select_best(tune_res, "accuracy")



## ---- eval=T--------------------------------------------------------------------------------

final_wf <- finalize_workflow( # Ferdigstiller workflow med modellen som gir best accuracy gitt antall tokens
  brexit_wf,
  best_accuracy
)

final_res <- final_wf %>%
  last_fit(brexit_classify_split) # Kjører modellen på testsettet

final_res %>%
  collect_predictions() %>% # Samler prediksjonene fra modellen
  conf_mat(truth = sentiment,  # Plotter de sanne verdiene
           estimate = .pred_class) %>% # Opp mot de predikerte verdiene
  autoplot("heatmap") # Lager et varmediagram (heatmap) som gir oss forvirringsmatrisen



## ---- include = FALSE-----------------------------------------------------------------------
library(Rttf2pt1)
library(extrafontdb)


## -------------------------------------------------------------------------------------------

library(tidyverse) # Vanlig pakke for databehandling i R
library(manifestoR) # Pakken som gjør at vi kan hente data fra Manifesto sitt API
library(tidytext) # Vanlig pakke for tekstbehandling i R
library(quanteda) # Pakke med mange nyttige funksjoner rundt tekstmodellering i R
library(quanteda.textmodels) # Spesifikke funksjoner for tekstmodellering (det er her wordscore og wordfish-funksjonene finnes)
library(quanteda.textplots) # Pakke for å plotte modeller og output fra quanteda



## ---- eval = FALSE--------------------------------------------------------------------------
## 
## mp_setapikey("manifesto_api_key_sb.txt") # Sett API-nøkkel
## 
## valgprogram_corpus <- mp_corpus(countryname %in% c("Sweden", "Denmark", "Norway") & # Ønsker disse landene
##                                   edate > as.Date("2017-01-01")) # Alle partiprogrammer fra etter 2017
## 


## ---- eval = TRUE, include = FALSE----------------------------------------------------------
valgprogram_corpus <- readRDS("./data/valgprogram_corpus.rds")


## -------------------------------------------------------------------------------------------

populist_precoded <- readRDS("./data/populist_precoded.rds")

populist_precoded %>%
  kableExtra::kable(row.names = F) %>%
  kableExtra::kable_styling()



## ----til-og-fra-tidy, out.width="100%", echo = FALSE----------------------------------------
knitr::include_graphics("./figurer/tmwr_0501.png")


## -------------------------------------------------------------------------------------------

valgprogram_tekst <- valgprogram_corpus %>%
  tidy() %>% # Gjør om fra et korpus til en vanlig dataframe
  mutate(aar = str_sub(date, 1, 4)) %>% # Henter ut de fire første tegnene i "date"-variabelen, som er året
  mutate(land = ifelse(str_detect(id, "^12"), "Norge", # I koden "id" indikerer de to første tallene hvilket land det er snakk om
                       ifelse(str_detect(id, "^11"), "Sverige", # Derfor, hvis "id" starter med 11 skal variabelen "land" være Sverige
                                         ifelse(str_detect(id, "^13"), "Danmark", NA)))) %>% # Også videre
  mutate(parti = case_when(party == "12110" ~ "Green Party", # Her oppretter jeg en variabel "parti" og gir den navn avhengig av "id-koden
                           party == "12221" ~ "Socialist Left Party",
                           party == "12230" ~ "Red Party",
                           party == "12320" ~ "Labour Party",
                           party == "12420" ~ "Liberal Party",
                           party == "12520" ~ "Christian People Party",
                           party == "12620" ~ "Conservative Party",
                           party == "12810" ~ "Centre Party",
                           party == "12951" ~ "Progress Party",
                           party == "11110" ~ "Greens",
                           party == "11220" ~ "Left Party",
                           party == "11320" ~ "Social Democrats",
                           party == "11420" ~ "People’s Party",
                           party == "11520" ~ "Christian Democrats",
                           party == "11620" ~ "Right Party",
                           party == "11710" ~ "Sweden Democrats",
                           party == "11810" ~ "Centre Party",
                           party == "13001" ~ "Liberal Alliance",
                           party == "13110" ~ "Alternativ",
                           party == "13320" ~ "Social Democratic Party",
                           party == "13229" ~ "Red-Green Unity List",
                           party == "13230" ~ "Socialist People’s Party",
                           party == "13410" ~ "Danish Social-Liberal Party",
                           party == "13420" ~ "Liberals",
                           party == "13620" ~ "Conservative People’s Party",
                           party == "13720" ~ "Danish People Party",
                           party == "13730" ~ "The New Right")) %>%
  mutate(land_parti = str_c(land, parti, sep = "_"), # Setter sammen variabelen land og parti for lettere å plotte senere
         land_parti = str_replace_all(land_parti, " ", "_")) %>% # Erstatter mellomrom med understrek
  select(land_parti, aar, title, text) # Henter ut disse variabelene til videre analyse



## -------------------------------------------------------------------------------------------

valgprogram_tekst <- valgprogram_tekst %>% # Koder inn score på populisme som allerede er supplert av ekspertkoderne
  mutate(ref_score = ifelse(land_parti == "Sverige_Sweden_Democrats", 4,
                            ifelse(land_parti == "Sverige_Greens", 1,
                                   ifelse(land_parti == "Sverige_Social_Democrats", 1,
                                          ifelse(land_parti == "Sverige_Christian_Democrats", 4,
                                                 ifelse(land_parti == "Svergie_Right_Party", 2,
                                                        ifelse(land_parti == "Sverige_Left_Party", 1,
                                                               ifelse(land_parti == "Danmark_Liberal_Alliance", 3,
                                                                      ifelse(land_parti == "Danmark_Social_Democrats", 3,
                                                                             ifelse(land_parti == "Danmark_Danish_People_Party", 4,
                                                                                    ifelse(land_parti == "Danmark_Liberals", 1,
                                                                                           ifelse(land_parti == "Danmark_Red-Green_Unity_List", 3,
                                                                                                  ifelse(land_parti == "Norge_Progress_Party", 4,
                                                                                                         ifelse(land_parti == "Norge_Labour_Party", 1,
                                                                                                                ifelse(land_parti == "Norge_Conservative_Party", 1,
                                                                                                                       ifelse(land_parti == "Norge_Christian_People_Party", 2, 
                                                                                                                              ifelse(land_parti == "Norge_Socialist_Left_Party", 3, NA))))))))))))))))) %>%
  select(land_parti, text, ref_score) # Henter ut disse variablene




## -------------------------------------------------------------------------------------------

wordscore_corpus <- corpus(valgprogram_tekst) # Lager om objektet til et korpus igjen
docvars(wordscore_corpus, field = "land_parti") <- valgprogram_tekst %>% pull(land_parti) # Legger inn en dokumentvariabel "land-parti"
docvars(wordscore_corpus, field = "ref_score") <- valgprogram_tekst %>% pull(ref_score) # Legger inn en dokumentvariabel om populismeverdien, "ref_score"

summary(wordscore_corpus)



## -------------------------------------------------------------------------------------------

valgprogram_tokens <- wordscore_corpus %>%
  tidy() %>% # Gjør om fra korpus til dataframe igjen
  rowid_to_column() %>% # Setter radens tall som kolonne (for å matche de samme radene senere)
  mutate(text = str_remove_all(text, "[0-9]+")) %>% # Fjern tall fra teksten, også der tall dukker opp inni andre ord
  select(rowid, land_parti, text) %>% # Hent ut disse variablene
  unnest_tokens(input = text, # Tokeniser teksten fra variabelen "text"
                output = token, # Kall den nye variabelen for "token"
                token = "words", # Gjør om til unigram
                strip_punct = TRUE, # Fjern punktsetting
                strip_numeric = TRUE) %>% # Fjern tall
  filter(!token %in% quanteda::stopwords("no")) %>% # Fjern stoppord
  mutate(token = quanteda::char_wordstem(token, language = "no")) %>% # Hent ut stammen av ordene
  group_by(rowid, land_parti, token) %>% # Grupper på disse variablene
  count() %>% # Tell opp antall tokens per variablene overfor
  ungroup() # Fjern gruppering for å ikke skape krøll senere

valgprogram_dfm <- valgprogram_tokens %>% # Opprett en dokument-trekk-matrise
  cast_dfm(document = land_parti, # Enhetene, altså radene, skal være dokumentene - som er parti i ulike land
           term = token, # Plasser token som variabel i matrisen (dette blir kolonnene)
           value = n) # Fyll cellene med sekk av ord, altså frekvensen av ord per dokument

valgprogram_dfm$land_parti <- docvars(wordscore_corpus, field = "land_parti") # Plasser de samme variablene som variablene i dokument-trekk-matrisen
valgprogram_dfm$ref_score <- docvars(wordscore_corpus, field = "ref_score") # som ble gjort med korpuset



## -------------------------------------------------------------------------------------------

valgprogram_ws <- textmodel_wordscores(x = valgprogram_dfm, # Dokument-trekk-matrise for tekstene
                                       y = valgprogram_tekst$ref_score, # Referansescore på referansetekstene
                                       smooth = 0) # Denne må være 0 for å kjøre Laver, Benoit og Garry (2003) sin skaleringsmetode



## -------------------------------------------------------------------------------------------

valgprogram_pred <- predict(valgprogram_ws, 
                            se.fit = TRUE, # TRUE for å få usikkerhetsmål på estimatene
                            rescaling = "none", # Sett denne til "lbg" for å transformere jomfrutekstene i takt med Laver, Benoit & Garry (2003)
                            newdata = valgprogram_dfm)

textplot_scale1d(valgprogram_pred) + theme_dark()# Plotter predikert populismescore til de ulike partiprogrammene



## -------------------------------------------------------------------------------------------

ord <- attributes(valgprogram_ws$wordscores) %>% # Henter ut ordene fra wordscores-objektet inne i valgprogram-modellen
  unlist() %>% as.character() # Tar ut fra liste og gjør om til en vanlig character-vektor

tibble(ord = ord, # Lager en dataframe (tibble) med ord som en variabel, fyll med ordene vi fant over
       wordscore = round(valgprogram_ws$wordscores, 2)) %>% # Variabelen "wordscore" skal inneholde wordscorene avrundet til to desimaler
  arrange(desc(wordscore)) %>% # Sorter dem i synkende rekkefølge
  DT::datatable(rownames = FALSE) # Lag interaktiv tabell av dem, og ikke ta med radnummer



## ---- eval = FALSE--------------------------------------------------------------------------
## 
## valgprogram_corpus <- mp_corpus(countryname == "Sweden" & # Henter ut Sverige
##                                   party == 11710 & # Partikode for Sverigedemokratene
##                                   edate > as.Date("2000-01-01")) # Ønsker alle partiprogram etter 2000
## 
## valgprogram_tekst <- valgprogram_corpus %>%
##   tidy() %>% # Gjør om til en dataframe
##   mutate(aar = str_sub(date, 1, 4)) %>% # Henter ut de fire første tegnene i "date"-variabelen, som er året
##   select(aar, title, text) # Årgang på partiprogrammet er enhet i dette tilfellet
## 


## ---- include = FALSE-----------------------------------------------------------------------
valgprogram_tekst <- readRDS("./data/valgprogram_tekst2.rds")


## -------------------------------------------------------------------------------------------

valgprogram_tokens <- valgprogram_tekst %>%
  mutate(text = str_remove_all(text, "[0-9]+")) %>% # Fjern tall fra teksten, også der tall dukker opp inni andre ord
  select(aar, text) %>% # Hent ut disse variablene
  unnest_tokens(input = text, # Tokeniser teksten fra variabelen "text"
                output = token, # Kall den nye variabelen for "token"
                token = "words", # Gjør om til unigram
                strip_punct = TRUE, # Fjern punktsetting
                strip_numeric = TRUE) %>% # Fjern tall
  filter(!token %in% quanteda::stopwords("no")) %>% # Fjern stoppord
  mutate(token = quanteda::char_wordstem(token, language = "no")) %>% # Hent ut stammen av ordene
  group_by(aar, token) %>% # Grupper på disse variablene
  count() %>% # Tell opp antall tokens per variablene overfor
  ungroup() # Fjern gruppering for å ikke skape krøll senere

valgprogram_dfm <- valgprogram_tokens %>% # Opprett en dokument-trekk-matrise
  cast_dfm(document = aar, # Enhetene, altså radene, skal være dokumentene - som er parti i ulike land
           term = token, # Plasser token som variabel i matrisen (dette blir kolonnene)
           value = n) # Fyll cellene med sekk av ord, altså frekvensen av ord per dokument



## -------------------------------------------------------------------------------------------

valgprogram_wf <- textmodel_wordfish(valgprogram_dfm, dir = c(1,3)) # Kjører en wordfish-modell
summary(valgprogram_wf)



## -------------------------------------------------------------------------------------------

textplot_scale1d(valgprogram_wf) +  # Plotter modelle
  theme_dark()



## -------------------------------------------------------------------------------------------

ord <- valgprogram_wf$features # Henter ut ordene fra "features" i wordfish-modellen

tibble(ord = ord, # Legger inn ordene i en dataframe
       wordfish = round(valgprogram_wf$beta, 2)) %>% # Henter ut scoren på ordene og runder av til to desimaler
  arrange(desc(wordfish)) %>% # Sorterer i synkende rekkefølge
  DT::datatable(rownames = FALSE) # Lager interaktiv tabell uten radnummer



## -------------------------------------------------------------------------------------------

textplot_scale1d(valgprogram_wf, # Plotter modellen
                 margin = "features", # Setter ord inn i plottet
                 highlighted = c("barn", "välja", # Gjør akkurat disse ordene tydeligere
                                 "aggressiv", "genomför", 
                                 "upprör"))



## ----writeManifest, echo=FALSE, eval=FALSE--------------------------------------------------
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


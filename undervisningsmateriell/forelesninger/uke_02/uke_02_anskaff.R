## ----setup, include=FALSE,warning=FALSE,message=FALSE------------------------------------------------------------------
# setwd("./undervisningsmateriell/forelesninger/uke_02/")
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(class.source = "code-bg")
refs <- bibtex::read.bib("../../../referanser/stv2022.bib")

library(rvest)
library(tidyverse)
library(tweetrmd)


## ----struct_ex, echo=-1------------------------------------------------------------------------------------------------
set.seed(984984)
tibble(stortingscrape::cases$root) %>%      # Henter ut data fra stortingscrape
  select(id, status, type, title_short) %>% # Trekker ut relevante variabler
  slice_sample(n = 5)                       # Viser 5 tilfeldige enheter



## xmllint --encode utf8 --format xml_eks.xml


## ----tweet_regjeringen, out.width=".75\textwidth"----------------------------------------------------------------------
tweet_screenshot(
  tweet_url("Regjeringen", "1526207283872641025"),
  theme = "dark",
  align = "center",
  hide_media = TRUE
)


## ----nou_base, eval=-c(9,10), echo=TRUE--------------------------------------------------------------------------------

library(rvest)
library(stringr)

# rot-url for regjeringen.no
base_url <- "https://www.regjeringen.no"

# Laster ned siden med alle NOUer fra UD (atm 7 stk)
str_c(base_url, "no/dokument/nou-ar/id1767/?ownerid=833") %>% 
  download.file(., destfile = "./scrape/base.html")

# Trekker ut linkene til hver NOU
nou_links <- read_html("./scrape/base.html") %>% 
  html_elements("li > h2 > a[data-hitid]") %>% 
  html_attr("href")

paste0(base_url, nou_links)


## ----nou_forside, eval=FALSE, echo=TRUE--------------------------------------------------------------------------------
## 
## # Laster ned forsiden til hver NOU
## for(i in nou_links){
## 
##   # Trekker ut dokument-id
##   tmp_id <- str_extract(i, "id[0-9]+")
## 
##   # Laster ned forsiden til NOU i
##   str_c(base_url, i) %>%
##     download.file(., destfile = str_c( "./scrape/nou_forside/",
##                                        tmp_id,
##                                        ".html"))
## 
##   # Legger til litt tilfeldig søvn
##   Sys.sleep(2 + abs(rnorm(1, 0)))
## 
##   # Printer en beskjed til console om at i nå er ferdig
##   message(str_c(
##     "Ferdig med: ",
##     tmp_id,
##     "\n"
##   ))
## 
## }
## 


## ----sjekke_at_filer_er_der, echo=TRUE, eval=TRUE----------------------------------------------------------------------

# Lister opp forsidene vi har lagret
list.files("./scrape/nou_forside/")

# Les inn hver forsidefil med read_html()
nou_forsider <- lapply(
  list.files("./scrape/nou_forside/", full.names = TRUE), 
  read_html
)

# Viser første listeelement
nou_forsider[[1]]



## ----nou_pdf_links, echo=TRUE, eval=TRUE-------------------------------------------------------------------------------

# Prosesserer hvert listeelement
nou_pdf_links <- sapply(nou_forsider, function(x){
  
  tmp_content_link <- x %>%       # Trekk ut listeelement x
    html_elements("a[title]") %>% # Trekk ut html nodene "a" som har egenskapen "title"
    html_attr("href")             # Trekk ut teksten til egenskapen "href"
  
  # Trekk ut bare de linkene som inneholder ".pdf" på slutten av strengen
  tmp_content_link %>% 
    magrittr::extract(str_detect(., "\\.pdf$"))
  
  
})

# Vis linkene
nou_pdf_links


## ----nou_pdf_dl, echo=TRUE, eval=FALSE---------------------------------------------------------------------------------
## # Laster ned pdf til hver NOU
## for(i in nou_pdf_links){
## 
##   # Trekker ut dokument-id
##   tmp_id <- str_extract(i, "nou(.*?)\\.pdf$")
## 
##   # Laster ned forsiden til NOU i
##   str_c(base_url, i) %>%
##     download.file(., destfile = str_c("./scrape/nou_pdf/",
##                                       tmp_id))
##   # Legger til litt tilfeldig søvn
##   Sys.sleep(2 + abs(rnorm(1, 0)))
## 
##   # Printer en beskjed til console om at i nå er ferdig
##   message(str_c(
##     "Ferdig med: ",
##     tmp_id,
##     "\n"
##   ))
## 
## }
## 
## # Lister opp filene
## list.files("./scrape/nou_pdf/")


## ----juksechunk, echo=FALSE, eval=TRUE---------------------------------------------------------------------------------
list.files("./scrape/nou_pdf/")


## ----nou_lese_pdf, eval=TRUE, echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------
library(textreadr)

# Lager objekt for filbane til alle pdfene
nou_pdfer <-  list.files("./scrape/nou_pdf/", full.names = TRUE)

# Gå gjennom hver fil og...
nou_tekst <- lapply(nou_pdfer, function(x){
  
  # ... les pdfen og gjør det om til en tibble
  read_pdf(x) %>% tibble()
  
})

# Viser de første 4 radene på listeelement 1
nou_tekst[[1]] %>% 
  head(., 4)


## ----nou_txt, eval=FALSE, echo=TRUE------------------------------------------------------------------------------------
## 
## # Fra 1 til 7 (antall NOUer) ...
## lapply(1:length(nou_tekst), function(x){
## 
##   tmp_tekst <- nou_tekst[[x]] %>%            # ...trekk ut listeelement x (1:7)
##     summarize(tekst = str_c(text,            # ...og slå sammen teksten til én tekstbolk
##                             collapse = " "))
## 
## 
##   txt_file_out <- nou_pdfer[x] %>%           # Trekk ut filbane for x
##     str_extract("nou[0-9]+(.*?)\\.pdf") %>%  # Trekk ut bare filnavn for filbanen til x
##     str_remove("\\.pdf")                     # Fjern ".pdf" fra filbanen til x
## 
## 
##   writeLines(tmp_tekst$tekst,            # Skriv teksten til en .txt fil
##              str_c("./scrape/nou_txt/",  # som skal lagres i mappen ./scrape/nou_txt/
##                    txt_file_out,         # med navnet vi laget over
##                    ".txt"))              # og .txt suffix
## 
## })


## ----nou_analyse, echo=FALSE, error=FALSE, warning=FALSE, message=FALSE------------------------------------------------
library(tidytext)

# Hentet fra: https://no.wikipedia.org/wiki/Liste_over_land_etter_areal
countries <- rvest::read_html("./scrape/countries.html") %>% 
  html_elements("table") %>% # Trekker ut bare tabell
  html_table() %>%           # Gjør om til en tibble-liste
  bind_rows() %>%            # og gjør om fra liste til bare tibble
  pull(Land) %>%             # Trekker ut variabelen "Land", bare
  tolower()                  # Gjør om til små bokstaver

# Lager en vektor med filbane til alle .txt-filene
txt_filer <- list.files("./scrape/nou_txt/", full.names = TRUE)

# Leser inn alle .txt-filene
nouer <- lapply(txt_filer, function(x){
  readLines(x) %>%         # Leser fil x
    str_c(collapse = " ")  # Kollapser alle linjer til én bolk med tekst
})

# Lager endelig datasett...
nou_data <- tibble(id = str_extract(txt_filer, "nou[0-9]+(.*?)pdf[as]"),
                   tekst = unlist(nouer)) %>%       # ...der id er nou-id
  unnest_tokens(token, tekst) %>%                   # ...teksten splittes opp i enkeltord
  filter(token %in% countries) %>%                  # ...tar med bare ord som er i landlista
  filter(token %in% c("norge", "man") == FALSE) %>% # ...fjerner norge og "man"
  filter(str_detect(token, "^eu$") == FALSE) %>%    # ...fjerner EU
  count(token)                                      # ...og teller antall ganger land er nevnt


# Lager en vektor for skandinaviske land
skandinavia <- c("danmark",
                 "sverige",
                 "færøyene",
                 "finland",
                 "island")

# Lager ny variabel som tar verdien "ja" hvis et land er skandinavisk
# og "nei" hvis ikke
nou_data$skandinavia <- ifelse(nou_data$token %in% skandinavia, "ja", "nei") %>% 
  factor(., levels = c("nei", "ja"))

library(ggwordcloud)

nou_data %>% 
  ggplot(., aes(label = token,                         # Lager plot med token som label
                size = n,                              # n som størrelse
                color = skandinavia)) +                # og ja/nei på skand. som farge
  geom_text_wordcloud_area()+                          # Lager ordsky geom
  scale_size_area(max_size = 40) +                     # Justerer størrelse på ordskyen
  scale_color_manual(values = c("darkcyan", "red")) +  # Setter farge for ja/nei på skand.land
  ggdark::dark_theme_void()                            # Skifter til mørkt tema




## ----skand_reg, echo=FALSE, results='asis'-----------------------------------------------------------------------------
(lm(n ~ skandinavia, data = nou_data)) %>%                   # Lager en regresjon med n som AV og skand. som UV
  stargazer::stargazer(.,                                    # Lager tabell av regresjonen
                       type = "html",                        # som skal komme ut i .html
                       keep.stat = c("n", "adj.rsq"),        # Tar vare på ant. enheter og rsq av statistikker
                       covariate.labels = c("Skand. land",   # Skifter navn på koeffisientene i regresjonen
                                            "Konstantledd"))


## ----en_kravler, eval=FALSE, echo=TRUE---------------------------------------------------------------------------------
## 
## # Laster inn pakke for kravling
## library(Rcrawler)
## 
## 
## Rcrawler("http://virksommeord.no/", # Nettsiden vi skal kravle
##          DIR = "./crawl",           # mappen vi lagrer filene i
##          no_cores = 4,              # kjerner for å prosessere data
##          dataUrlfilter = "/tale/",  # subset filter for kravling
##          RequestsDelay = 2 + abs(rnorm(1)))
## 
## 


## ----juksechunk2, eval=TRUE, echo=TRUE---------------------------------------------------------------------------------
# Lager en vektor med alle filnavn
virkord_filer <- list.files("./crawl/virksommeord.no-101413", 
                            full.names = TRUE)

# Viser de første filene i vektoren...
head(virkord_filer)

# Og lengden på vektoren
length(virkord_filer)



## ---- eval=FALSE, echo = TRUE------------------------------------------------------------------------------------------
## 
## # Leser inn alle filene fra kravlingen
## virkord_html <- lapply(virkord_filer, rvest::read_html)
## 
## # Går gjennom alle filene med preprosessering
## virkord_data <- lapply(1:length(virkord_html), function(x){
## 
##   tmp_tekst <- virkord_html[[x]] %>%
##     html_elements("div[class='document'] > p") %>% # Trekker ut elementene <div class="document"> etterfulgt av <p>
##     html_text()                                    # Konverterer til tekst
## 
##   if(identical(character(), tmp_tekst)){           # Hvis teksten er tom...
##     tmp_tekst <- virkord_html[[x]] %>%             #
##       html_elements("tr[valign='top']") %>%        # ...trekker jeg ut <tr valign="top>
##       html_text()                                  # og gjør om til tekst
##   }
## 
##   if(identical(character(), tmp_tekst)){              # Hvis teksten fortsatt er tom...
##     tmp_tekst <- virkord_html[[x]] %>%                #
##       html_elements("div[class='document'] > h3") %>% # ...trekker jeg ut elementene <div class="document"> etterfulgt av <h3>
##       html_text() %>%                                 # ...gjør om til tekst
##       str_split(., "\\n") %>%                         # ...splitter opp i linjer
##       unlist()                                        # ... og konverterer fra liste til vektor
##   }
## 
##   # Trekker ut forfatternavn
##   tmp_forfatter <- virkord_html[[x]] %>%
##     html_elements("div[class='tale-header'] > ul[class='byline'] > li > a") %>%
##     html_text() %>%
##     str_replace_all(., "\\s+", " ") %>%
##     .[1]
## 
##   # Trekker ut link til forfatterside
##   tmp_forfatter_link <- virkord_html[[x]] %>%
##     html_elements("div[class='tale-header'] > ul[class='byline'] > li > a") %>%
##     html_attr("href") %>%
##     .[which(str_detect(., "person"))]
## 
##   # Trekker ut tittel
##   tmp_tittel <- virkord_html[[x]] %>%
##     html_elements("div[class='tale-header'] > h1") %>%
##     html_text() %>%
##     str_replace_all(., "\\s+", " ")
## 
##   # Setter alt sammen til en tibble
##   tmp_data <- tibble(
##     tittel = tmp_tittel,
##     forfatter = tmp_forfatter,
##     forfatter_link = tmp_forfatter_link,
##     avsnitt = 1:length(tmp_tekst),
##     tekst = tmp_tekst
##   )
## 
##   return(tmp_data)
## })
## 
## # Binder så sammen alle tibbles i listen "virkord_data"
## # Til å være én tibble
## virkord <- bind_rows(virkord_data)
## 
## 
## # save(virkord, file = "./crawl/virksommeord.rda")


## ----load_virkdata, echo = FALSE, eval=TRUE----------------------------------------------------------------------------
load("./crawl/virksommeord.rda")


## ----vise_virkdata, echo=TRUE, eval=TRUE-------------------------------------------------------------------------------
# Viser de 6 øverste radene i datasettet
virkord %>% sample_n(6)


## ----robots, eval=-1---------------------------------------------------------------------------------------------------
download.file("https://vg.no/robots.txt", destfile = "./scrape/vg_robots.txt")

readLines("./scrape/vg_robots.txt")



## convert -density 300x300 -shave 10x100;

##   not_noisy.png not_noisy_clean1.png

## 

## convert not_noisy_clean1.png -normalize ;

##   not_noisy_clean2.png

## 

## convert not_noisy_clean2.png ;

##   -connected-components 4 -threshold 0 -negate;

##   not_noisy_clean3.png

## 

## convert not_noisy_clean2.png;

##   -define connected-components:area-threshold=15;

##   -connected-components 4 -threshold 0 -negate;

##   not_noisy_clean4.png

## 

## convert not_noisy_clean3.png not_noisy_clean4.png;

##   -compose minus -composite;

##   not_noisy_clean5.png

## 

## convert not_noisy_clean2.png;

##   \( -clone 0 -negate -fill white -colorize 100% \) ;

##   not_noisy_clean5.png -compose Blend -composite;

##   not_noisy_clean6.png

## 

## convert not_noisy_clean6.png;

##   -fill black -opaque "#FF00FF" -morphology Erode Disk:0.5;

##   not_noisy_cleaned.png


## # https://github.com/tesseract-ocr/tesseract

## tesseract not_noisy_cleaned.png tekst

## head -n 10 tekst.txt


## ----show_ocr_text, echo = FALSE, eval = TRUE, warning = FALSE---------------------------------------------------------
readLines("./tekst.txt", n = 7)


## ----show_noisy_ocr_text, echo = FALSE, eval = TRUE, warning = FALSE---------------------------------------------------
readLines("./tekst_noisy.txt", n = 5)


## ----bow_ex, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE-------------------------------------------------------

samp_tekst <- read_csv("st_debates_meta.csv", show_col_types = FALSE)

set.seed(4093)

samp_tekst[sample(1:nrow(samp_tekst), 6), c("speech_id", "name_fixed", "speech")]


## ----bow_ex2, echo=FALSE, eval=TRUE, warning=FALSE, message=FALSE------------------------------------------------------
samp_tekst <- samp_tekst[sample(1:nrow(samp_tekst), 100), ]

samp_tekst <- samp_tekst %>%
  unnest_tokens(output = "token", input = "speech") %>%
  filter(grepl("[0-9]", token) == FALSE) %>% 
  group_by(speech_id) %>% count(token) %>% 
  cast_dfm(., document = speech_id, term = token, value = n)

as.matrix(samp_tekst[1:6, c("det", "har", "jord", "lærere", "behandling", "park")])



## ----writeScript, echo=FALSE, eval=FALSE-------------------------------------------------------------------------------
## knitr::purl("./uke_02_anskaff.Rmd", "./uke_02_anskaff.R", documentation = 1)


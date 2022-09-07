## ----setup, include=FALSE,warning=FALSE,message=FALSE------------------------------------------------------------------
# setwd("./undervisningsmateriell/forelesninger/uke_03/")
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(class.source = "code-bg")
refs <- bibtex::read.bib("../../../referanser/stv2022.bib")

library(tidyverse);library(tidytext)

klossmajor <- readLines("./data/klossmajor/Skjønte ikke du var borte.txt")
# toenes <- readLines("./data/saan_av_salve.txt")

str_break <- function(x, width = 7L) {
  x <- unlist(quanteda::tokenize_fastestword(x))
  n <- length(x)
  if (n <= width) return(x)
  n1 <- seq(1L, n, by = width)
  n2 <- seq(width, n, by = width)
  if (n %% width != 0) n2 = c(n2, n)
  
  lines <- character()
  for(i in 1:length(n1)){
    lines[i] <- paste(x[n1[i]:n2[i]], collapse = " ")
  }
 return(lines) 
}
library(spacyr);spacy_initialize("nb_core_news_lg")


## ----regexeks1, eval=TRUE, echo=-1-------------------------------------------------------------------------------------
eks_streng <- "Solveig er ca. 20 år og har bedre forelesninger enn Martin"
eks_streng %>% cat()


## ----regexeks2, eval=TRUE, echo=TRUE-----------------------------------------------------------------------------------
str_extract_all(eks_streng, "[A-ZØÆØÅ][a-zæøå]+")



## ----regexeks2b, eval=TRUE, echo=TRUE----------------------------------------------------------------------------------
str_extract_all(eks_streng, "[0-9]+")


## ----regexeks3, eval=TRUE, echo=TRUE-----------------------------------------------------------------------------------
str_extract(eks_streng, "[0-9]{1}")
str_extract_all(eks_streng, "[0-9]{1}")
str_detect(eks_streng, "\\@")


## ----regexeks4, eval=TRUE, echo=TRUE-----------------------------------------------------------------------------------
str_extract_all(eks_streng, "[aeiouyæøå]")

str_replace_all(eks_streng, c("Martin" = "Bjørn", 
                              "Solveig" = "Martin",
                              "20" = "60"))



## ----regexeks5, eval=TRUE, echo=TRUE-----------------------------------------------------------------------------------
to_mailadresser <- c("martin.soyland@stv.uio.no",
                     "solveig_har_en_mail@høhø.com")

str_extract(to_mailadresser, 
           "([a-z]+\\.*)+\\@")

str_extract(to_mailadresser, 
           "([a-z]+[[:punct:]]*)+\\@")


## ----regexeks6, eval=TRUE, echo=TRUE-----------------------------------------------------------------------------------
str_extract(to_mailadresser, 
           "^([a-z]+[[:punct:]]*)+\\@([a-z]+[[:punct:]]*)+$")


## ----regexeks7, eval=TRUE, echo=TRUE-----------------------------------------------------------------------------------
str_extract(to_mailadresser, 
           "(([a-zæøå]+[[:punct:]]*)+\\@*)+")


## ----isv_liste, echo=TRUE, eval=FALSE, file="./r/regex_eksempler.R"----------------------------------------------------
## 
## 


## ----bow_ex, echo=FALSE------------------------------------------------------------------------------------------------

set.seed(8954558)

bow_ex <- klossmajor %>% 
  str_c(collapse = " ") %>% 
  tibble(text = .) %>% 
  unnest_tokens(token, text) %>% 
  pull(token) %>% 
  sample(., size = length(.)) %>% 
  str_c(collapse = " ")

bow_ex %>% 
  str_break(., width = 10) %>% 
  str_c(., "\n") %>% 
  cat()



## ----bow_faktisk, echo=FALSE-------------------------------------------------------------------------------------------
klossmajor %>% str_replace(., "faen", "f*en")



## ----storting_eks------------------------------------------------------------------------------------------------------

stortingscrape::interp0203 %>% 
  head(., 10) %>% 
  mutate(title = str_sub(title, 4, 34)) %>% 
  select(question_from_id, qustion_to_id, title)




## ----laste_inn_klossmajor, eval=TRUE, echo=TRUE------------------------------------------------------------------------

klossmajor_filer <- list.files("./data/klossmajor", 
                               full.names = TRUE)

klossmajor <- lapply(klossmajor_filer, function(x){
  
  tmp <- readLines(x) %>% 
    tibble(tekst = .)
  
  tmp$tittel <- x %>% 
    str_remove_all("./data/klossmajor/|.txt")  
  
  tmp$linje <- 1:nrow(tmp)
  
  tmp <- tmp %>% dplyr::select(tittel, linje, tekst)
  
  return(tmp)
})

klossmajor <- klossmajor %>% bind_rows()





## ----tekst_i_linjer, echo=-1, eval=TRUE--------------------------------------------------------------------------------
set.seed(677)

klossmajor %>% head(3)


## ----tokenizing, echo=TRUE---------------------------------------------------------------------------------------------
library(tokenizers)
eks_streng <- "Her er en streng vi \n kan bruke som eksempelet vårt"

tokenize_characters(eks_streng, simplify = TRUE)



## ----tokenizing2, echo=TRUE--------------------------------------------------------------------------------------------
tokenize_words(eks_streng, simplify = TRUE)


## ----tokenizing3, echo=TRUE--------------------------------------------------------------------------------------------
tokenize_lines(eks_streng, simplify = TRUE)


## ----lowercaseeks, echo=TRUE-------------------------------------------------------------------------------------------
"Fjell kan være høye, sier Kari Fjell" %>% 
  tokenizers::tokenize_words(.,
                             lowercase = TRUE,
                             stopwords = NULL,
                             strip_punct = FALSE,
                             simplify = TRUE)



## ----talleks, echo=TRUE------------------------------------------------------------------------------------------------
"Lov om endring i skattebetalingsloven, §5-7, endres 1.januar 2023" %>% 
  tokenizers::tokenize_words(.,
                             lowercase = TRUE,
                             stopwords = NULL,
                             strip_punct = TRUE,
                             strip_numeric = TRUE,
                             simplify = TRUE)



## ----stoppordeks, echo=TRUE--------------------------------------------------------------------------------------------
"Etter jeg hadde forelesning, var jeg ikke fornøyd" %>% 
  tokenizers::tokenize_words(.,
                             lowercase = TRUE,
                             stopwords = quanteda::stopwords("no"),
                             strip_punct = TRUE,
                             strip_numeric = TRUE,
                             simplify = TRUE)



## ----stemming----------------------------------------------------------------------------------------------------------
message("Før stemming:")
"Mange fine fugler fløy på himmelen, men én fugl var langt bak" %>% 
  str_c(., collapse = " ") %>% 
  tolower() %>% 
  tokenize_words(., 
                 simplify = TRUE) %>% 
  cat()

message("Etter stemming:")
"Mange fine fugler fløy på himmelen, men én fugl var langt bak" %>% 
  str_c(., collapse = " ") %>% 
  tolower() %>% 
  tokenize_word_stems(., 
                      language = "no",
                      simplify = TRUE) %>% 
  cat()


## ----stemming2---------------------------------------------------------------------------------------------------------
message("Før stemming:")
"fot, foten, føtter"

message("Etter stemming:")
"fot, foten, føtter" %>% 
  str_c(., collapse = " ") %>% 
  tolower() %>% 
  tokenize_word_stems(.,
                      language = "no",
                      simplify = TRUE)



## ----lemmatisering, warning=TRUE, message=FALSE------------------------------------------------------------------------

spacy_parse("fot, foten, føtter",
            pos = FALSE)



## ----klossmajor_counts, message=FALSE, echo=TRUE-----------------------------------------------------------------------

km_count <- klossmajor %>% 
  group_by(tittel) %>% 
  unnest_tokens(., token, tekst) %>% 
  count(token)

km_count %>% tail(., n = 3)



## ----km_dfm, echo=TRUE-------------------------------------------------------------------------------------------------
km_dfm <- km_count %>% 
  cast_dfm(., tittel, token, n)

km_dfm



## ----km_dfm_reduksjon, echo=-19----------------------------------------------------------------------------------------
km_dfm <- klossmajor %>% 
  group_by(tittel) %>% 
  unnest_tokens(., 
                output = token, 
                input = tekst,
                token = "words",
                to_lower = TRUE,
                stopwords = quanteda::stopwords("no"),
                strip_punct = TRUE,
                strip_numeric = TRUE) %>% 
  mutate(token = quanteda::char_wordstem(token, language = "norwegian")) %>% 
  count(token) %>% 
  cast_dfm(., document = tittel, term = token, value = n)


## ----juksechunk_dfm----------------------------------------------------------------------------------------------------
km_dfm


## ----kloss_cos, eval=TRUE, warning=FALSE, error=FALSE, message=FALSE, fig.width=6.85-----------------------------------
library(quanteda.textstats)

km_simil <- textstat_simil(km_dfm, method = "cosine") %>% 
  as.matrix()

diag(km_simil) <- NA

km_simil <- km_simil %>% 
  as_tibble() 

km_simil$Tittel <- names(km_simil)

km_simil <- km_simil %>% 
  pivot_longer(cols = 1:9)


ggplot(km_simil, aes(x = Tittel, y = name, fill = value)) +
  geom_tile(stat = "identity") +
  geom_text(aes(label = ifelse(value > .15, round(value, 2), ""))) +
  scale_fill_gradient2(low = "#FF6E00", mid = "black", high = "#00E5FF", midpoint = 0,
                       limit = c(-0.31, 0.31)) +
  ggdark::dark_mode() +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



## ----ngrams_km1--------------------------------------------------------------------------------------------------------
klossmajor$tekst[90] %>% 
  tokenize_words(., simplify = TRUE)


## ----ngrams_km2--------------------------------------------------------------------------------------------------------
klossmajor$tekst[90] %>% 
  tokenize_ngrams(., n = 2, simplify = TRUE)


## ----ngrams_km3--------------------------------------------------------------------------------------------------------
klossmajor$tekst[90] %>% 
  tokenize_ngrams(., n = 3, simplify = TRUE)


## ----tfidf_km1---------------------------------------------------------------------------------------------------------

km_tfidf <- km_count %>% 
  bind_tf_idf(., 
              term = token,
              document = tittel,
              n = n) %>% 
  filter(token %in% quanteda::stopwords("norwegian") == FALSE)

km_tfidf %>% 
  group_by(tittel) %>% 
  slice_max(., 
            n = 1, 
            order_by = n, 
            with_ties = FALSE) %>% 
  .[c(3:4, 6, 9), ]


## ----tfidf_km2---------------------------------------------------------------------------------------------------------
km_tfidf %>% 
  group_by(tittel) %>% 
  slice_max(., 
            n = 1, 
            order_by = tf_idf, 
            with_ties = FALSE) %>% 
  .[c(3:4, 6, 9), ]



## ----tfidf_plotsimil, message=FALSE, warning=FALSE---------------------------------------------------------------------

km_simil <- textstat_simil(quanteda::dfm_tfidf(km_dfm)) %>% 
  as.matrix()

diag(km_simil) <- NA

km_simil <- km_simil %>% 
  as_tibble() 

km_simil$Tittel <- names(km_simil)

km_simil <- km_simil %>% 
  pivot_longer(cols = 1:9)


ggplot(km_simil, aes(x = Tittel, y = name, fill = value)) +
  geom_tile(stat = "identity") +
  geom_text(aes(label = ifelse(value > .15, round(value, 2), ""))) +
  scale_fill_gradient2(low = "#FF6E00", mid = "black", high = "#00E5FF", midpoint = 0,
                       limit = c(-0.15, 0.15)) +
  ggdark::dark_mode() +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


## ----pos_ex, eval=-1---------------------------------------------------------------------------------------------------
library(spacyr);spacy_initialize("nb_core_news_lg")

en_sang <- str_c(klossmajor$tekst[209:232], collapse = " ")

en_sang_pos <- spacy_parse(en_sang,
                           entity = FALSE,
                           lemma = FALSE)
en_sang_pos %>% tail()



## ----skrft_spm_ner-----------------------------------------------------------------------------------------------------

q <- stortingscrape::get_question("55041")

# Fjerner html tags
q <- str_remove_all(q$question_text, "\\<(.*?)\\>")

q_ner <- spacy_parse(q,
                     lemma = FALSE)

q_ner %>% 
  filter(entity != "")


## ----all_kode, file="r/scrape_klossmajor.R", echo=TRUE, eval=FALSE-----------------------------------------------------
## 


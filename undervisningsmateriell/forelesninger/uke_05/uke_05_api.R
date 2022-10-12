
library(tidytext);library(quanteda);library(tidyverse);library(tidytext);library(rvest)


load("data/wq1920.rda")
load("data/wq2021.rda")
load("data/covid.rda")
load("data/covid_stm.rda")


## ----------------------------------------------------------------------------------------------------------------------
if(file.exists("./data/xml/sesjoner.xml") == FALSE){
  download.file("https://data.stortinget.no/eksport/sesjoner",
              destfile = "./data/xml/sesjoner.xml")
}


## xmllint --encode utf8 --format ./data/xml/sesjoner.xml


## ----readxmleks1, echo=-1, eval=-4-------------------------------------------------------------------------------------
sesjoner <- read_html("./data/xml/sesjoner.xml")

# Leser rå xml-fil
sesjoner <- read_html("https://data.stortinget.no/eksport/sesjoner")

sesjoner


## ----readxmleks2, echo=TRUE--------------------------------------------------------------------------------------------
# Trekker ut alle "id"-nodene
sesjoner %>% html_elements("id") %>% head()


## ----readxmleks3, echo=TRUE--------------------------------------------------------------------------------------------
# Trekker ut sesjonstart
sesjoner %>% 
  html_elements("sesjoner_liste > sesjon > fra") %>% 
  html_text() %>% 
  as.Date() %>% 
  head()

# Trekker ut sesjonslutt
sesjoner %>% 
  html_elements("sesjoner_liste > sesjon > til") %>% 
  html_text() %>% 
  as.Date() %>% 
  head()



## ----struktxml, echo=TRUE----------------------------------------------------------------------------------------------


sesjoner_df <- tibble(
  from = sesjoner %>% 
    html_elements("sesjoner_liste > sesjon > fra") %>% 
    html_text() %>% 
    as.Date(), 
  id = sesjoner %>% 
    html_elements("sesjoner_liste > sesjon > id") %>% 
    html_text(), 
  to = sesjoner %>% 
    html_elements("sesjoner_liste > sesjon > til") %>% 
    html_text() %>% 
    as.Date()
)

sesjoner_df %>% 
  filter(from < as.Date("2022-01-01")) %>% 
  head()



## ----avail_data, echo=FALSE--------------------------------------------------------------------------------------------

library(rvest)
doc <- read_html("./doc_help.html")

cat((doc %>% html_elements("div > a") %>% html_text())[7:44], sep = "\n")



## ---- eval=FALSE-------------------------------------------------------------------------------------------------------
## get_mp_pic("GHB", size = "stort", destfile = "gro.png")
## get_mp_pic("EIGE", size = "stort", destfile = "einar.png")
## get_mp_pic("K_AWI", size = "stort", destfile = "kåre.png")


## ----get_wq, echo=TRUE, eval=FALSE-------------------------------------------------------------------------------------
## library(stortingscrape)
## 
## # Henter alle høringer i sesjonene 2019-2020 og 2020-2021
## wq1920 <- get_session_questions("2019-2020",
##                                 q_type = "skriftligesporsmal",
##                                 good_manners = 2)
## 
## wq2021 <- get_session_questions("2020-2021",
##                                 q_type = "skriftligesporsmal",
##                                 good_manners = 2)
## 
## # Binder sammen de to sesjonene til ett objekt
## wqs <- bind_rows(wq1920, wq2021)
## 
## # Filtrerer ut de som nevner "CoV", "koron[..]", "smittevern" eller "vaksine
## covid <- wqs %>%
##   filter(str_detect(title, "[Cc]o[Vv]|[Kk]oron|[Ss]mittevern|[Mm]unnbind|[Vv]aksine") == TRUE)
## 


## ----show_title_data, echo=FALSE, eval=TRUE----------------------------------------------------------------------------
covid %>% 
  select(question_from_id, qustion_to_minister_title, title) %>% 
  mutate(qustion_to_minister_title = str_remove(qustion_to_minister_title, "ministeren"),
         title = str_c(substr(title, 1, 15), "[...]")) %>% 
  tail(., n = 4)


## ---- eval=FALSE, echo=TRUE--------------------------------------------------------------------------------------------
## 
## covid$justification <- NA
## 
## for(i in 1:nrow(covid)){
##   tmp_question <- get_question(covid$id[i], good_manners = 1)
##   covid$justification[i] <- tmp_question$justification
## }
## 
## 


## ----------------------------------------------------------------------------------------------------------------------
str_sub(covid$justification, 1, 65) %>% 
  head() %>% 
  print()


## ----tekst_til_tall, echo=TRUE-----------------------------------------------------------------------------------------

covid_tokens <- covid %>%
  mutate(sendt_date = as.Date(sendt_date)) %>% 
  group_by(id, sendt_date) %>% 
  unnest_tokens(., 
                output = token, 
                input = justification) %>% 
  filter(str_detect(token, "[0-9]") == FALSE) %>% 
  count(token) %>% 
  bind_tf_idf(., 
              term = token,
              document = id,
              n = n)

covid_tokens %>% head(., 4)



## ----stopordliste, echo=TRUE-------------------------------------------------------------------------------------------
stop_words <- covid_tokens %>% 
  ungroup() %>% 
  group_by(token) %>% 
  summarize(token = unique(token),
            idf = unique(idf)) %>% 
  arrange(idf) %>% 
  filter(idf < 1) %>% 
  pull(token)

stop_words

covid_tokens <- covid_tokens %>% 
  filter(token %in% stop_words == FALSE) 



## ----top_tfidf, echo=FALSE, warning=FALSE, error=FALSE, message=FALSE--------------------------------------------------
covid_nokkelord <- c("kriseturnus", "smitteverntiltakene",
                     "koronaviruset", "rehabilitering",
                     "sykehuset", "senskader",
                     "antistoffer", "influensa",
                     "smittevernlegens", "vaksinedose",
                     "covax")

covid_tokens$color <- ifelse(covid_tokens$token %in% covid_nokkelord,
                                       "yes",
                                       "no")

covid_tokens %>%
  filter(tf_idf > 0.15) %>% 
  slice_max(., 
            order_by = tf_idf, 
            n = 1, 
            with_ties = FALSE) %>% 
  ggplot(., aes(label = token, size = tf_idf, color = color)) +
  ggwordcloud::geom_text_wordcloud() +
  scale_size_area(max_size = 10) +
  scale_color_manual(values = c("gray70", "cyan")) +
  ggdark::dark_theme_void()




## ----------------------------------------------------------------------------------------------------------------------

library(NorSentLex)

covid_tokens_sentiment <- covid_tokens %>% 
  mutate(sentiment = ifelse(
    token %in% nor_fullform_sent$positive, 1,
    ifelse(
      token %in% nor_fullform_sent$negative, -1, 0)))




ggplot(covid_tokens_sentiment, aes(x = sendt_date, y = sentiment)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2)) +
  ggdark::dark_theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray50",
                                          linetype = "dashed")) +
  labs(x = "Dato spørsmål ble sendt",
       y = "Sentiment")



## ---- message=FALSE, eval=FALSE----------------------------------------------------------------------------------------
## library(stm)
## 
## 
## q_dfm <- wqs %>%
##   group_by(id) %>%
##   unnest_tokens(.,
##                 output = token,
##                 input = title) %>%
##   filter(token %in% quanteda::stopwords(language = "no") == FALSE) %>%
##   filter(str_detect(token, "[0-9]") == FALSE) %>%
##   filter(str_detect(token, "[[:punct:]]") == FALSE) %>%
##   count(token) %>%
##   cast_dfm(.,
##            document = id,
##            term = token,
##            value = n)
## 
## covid_stm <- stm(q_dfm, K = 0, init.type = "Spectral")
## 


## ---- echo=TRUE, message=FALSE, warning=FALSE--------------------------------------------------------------------------
stm::findTopic(covid_stm, "covid", type = "frex")

covid_topics <- stm::labelTopics(covid_stm, n = 12)
covid_topics$frex[c(34, 43, 87), ] %>% t()



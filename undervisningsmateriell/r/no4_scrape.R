
rm(list = ls())

library(rvest)
library(tidyverse)

url <- "https://genius.com/albums/No-4/No-4"

test <- read_html(url)

test %>% html_elements("album-tracklist-row")

titler <- test %>% 
  html_elements("div.chart_row-content > a > h3") %>% 
  html_text() %>% 
  str_trim() %>% 
  str_remove_all("\\s+Lyrics")


track_urls <- sapply(test %>% html_elements("div.chart_row-content > a") %>% html_attrs(), "[[", 1)


text <- lapply(1:length(track_urls), function(x){
  
  t <- read_html(track_urls[x])
  
  lol <- t %>% html_elements("div.Lyrics__Container-sc-1ynbvzw-6.jYfhrf") %>% 
    html_text2() %>% 
    str_split("\\n") %>% 
    unlist() %>% 
    str_c(collapse = " ") %>% 
    str_remove_all("\\[(.*?)\\]") %>% 
    str_replace_all("\\s+", " ") %>% 
    str_trim()
  
  
  Sys.sleep(2+rnorm(1, 3))
  return(lol)
})

no4 <- tibble(spor = 1:length(titler),
              titler, 
              tekst = unlist(text))


save(no4, file = "./undervisningsmateriell/data/no4.rda")


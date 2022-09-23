
library(tidyverse)
library(rvest)

setwd("C:/Users/solvebjo/OneDrive - Universitetet i Oslo/Teaching/STV2022/stv2022/undervisningsmateriell/forelesninger/uke_04")

download.file("https://en.wikipedia.org/wiki/Marshmallow",
              destfile = "./marshmallow.html")

# Steg 1: Les in html-filen med read_html()

html <- read_html("./marshmallow.html")

# Steg 2: Finn noden du vil hente informasjon fra, og hent ut denne noden med html_node (eller html_nodes() om du vil ha flere instanser)

html2 <- html %>%
  html_node("#mw-content-text > div.mw-parser-output > p:nth-child(7)")

# Steg 3: Hent ut det du trenger fra noden. f. eks. tekst (html_text()), tabeller (html_table()) eller lenker (html_attr())

html3 <- html2 %>%
  html_text(html2)

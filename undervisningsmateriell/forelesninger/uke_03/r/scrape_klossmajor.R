# Laster inn bibliotek
library(rvest)
library(stringr)

# Url for albumforside
base_url <- "https://genius.com/albums/Klossmajor/Ordner-seg-for-snille-jenter"

# Trekker ut liste av sanger
track_list <- read_html(base_url)

# Trekker ut linkene til hver sang
track_list_links <- track_list %>% 
  html_elements("div.chart_row-content > a") %>% 
  html_attr("href")

# For hver sanglink...
for(i in track_list_links){
  
  # Les inn siden
  tmp <- read_html(i)
  
  
  lyrics <- tmp %>% 
    html_elements(xpath = '//*[@id="lyrics-root"]/div[1]') %>% # ...trekk ut denne "xpath"
    html_text2() %>%                                           # ...gjør om til tekst (med \\n tags)
    str_split("\\n") %>%                                       # ...split opp teksten i linjer
    unlist()                                                   # ...gjør om fra liste til vektor
  
  # Trekk ut tittel på sangen
  title <- tmp %>% 
    html_elements("h1 > span") %>% 
    html_text()
  
  # Skriv sangen til fil
  writeLines(lyrics, paste0("./data/klossmajor/", title, ".txt"))
  
  # Sov litt
  Sys.sleep(2)
}


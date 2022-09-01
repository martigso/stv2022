#####################
## Regex eksempler ##
#####################

rm(list = ls())

library(rvest)
library(stringr)
library(ggplot2)

# Link til alle ISV-ansatte
isv_url <- str_c("https://www.sv.uio.no/isv/personer/?page=", 1:6, "&u-page=", 1:6)

# Laster ned alle sidene
for(i in 1:length(isv_url)){
  
  # Hvis siden allerede er lagret lokalt, hopper vi over den
  if(file.exists(str_c("./data/isv_folk/", i, ".html"))) next
  
  # Hvis siden ikke er lagret, laster vi ned og lagrer
  download.file(isv_url[i], destfile = str_c("./data/isv_folk/", i, ".html"))
  
  # Sover litt
  Sys.sleep(2)
}


# Leser inn alle filene og strukturer dem til datasett i en liste
folk_df <- lapply(list.files("./data/isv_folk", full.names = TRUE), function(x){
  
  tmp_folk <- read_html(x)
  
  tmp_folk <- tmp_folk %>% 
    html_elements("table") %>% 
    html_table() %>% 
    .[[1]] %>% 
    tibble()
  
  return(tmp_folk)
  
})

# Binder sammen alle datasettene i lista
folk_df <- bind_rows(folk_df)

# Snur navnene for å showboate litt
# Originalt: [Etternavn, Fornavn]
# Ønsket resultat: [Fornavn Etternavn]
snudd_navn <- lapply(folk_df$Navn, function(x){
  
  # Spitter ut navn og tittel, som er i samme kolonne
  tmp <- x %>% str_split(., "\\n") %>% unlist()
  
  # Det første elementet etter split er [Etternavn, Fornavn]
  tmp_navn <- tmp[1]
  
  # Splitter navnet på komma + mellomrom (", ")
  tmp_navn <- tmp_navn %>% str_split(., ", ") %>% unlist()
  
  # Limer sammen del 2 av navnet med del 1
  # altså: str_c(fornavn, etternavn) 
  tmp_navn <- str_c(tmp_navn[2], tmp_navn[1], sep = " ")
  
  # Bytter ut alle sekvenser med mellomrom med bare ett mellomrom
  tmp_navn <- str_replace(tmp_navn, "\\s+", " ")
  
  # Gir resultatet tilbake til "snudd_navn"
  return(tmp_navn)
  
})

# Basically det samme som med navn, bare nå med tittel
tittel <- lapply(folk_df$Navn, function(x){
  
  # Splitter navn på "\n"
  tmp <- x %>% str_split(., "\\n") %>% unlist()
  
  # hvis lengden på tmp er større enn 1 ...
  if(length(tmp) > 1){
    tmp_tittel <- tmp[2] #...skal tittelen være element 2 i tmp
  } else {
    tmp_tittel <- NA # ...hvis ikke, har ikke personen tittel (NA)
  }
  
  # Trimmer white space i tittel-strengen
  tmp_tittel <- tmp_tittel %>% 
    str_trim()
  
  # returnerer resultatet til "tittel
  return(tmp_tittel)
})

# Setter inn snudd navn og tittel i tibble
folk_df$snudd_navn <- snudd_navn %>% unlist()
folk_df$tittel <- tittel %>% unlist()

# Fjerner de gamle variablene og endrer litt på rekkefølge
folk_df <- folk_df %>% 
  select(snudd_navn, tittel,
         `E-post`, Telefon,
         Emneord)

# Trekker ut bare de som har "Komparativ politikk" som emneord
folk_kp <- folk_df %>% 
  filter(str_detect(Emneord, "Komparativ politikk"))

# Skal telle hvor mange ganger emneord blir brukt
# Her splitter jeg emneordene på både komma og \n
telle_emner <- folk_df$Emneord %>% 
  str_split(., "\\,|\\n")

# Den resulterende listen går vi gjennom med lapply og ...
telle_emner <- lapply(telle_emner, function(x){
  
  # ...trekker ut bare de elementene som inneholder tekst
  # og trimmer strengene
  tmp_emner <- x[which(str_detect(x,"[a-zæøå]+"))] %>% 
    str_trim()
  
  # hvis tmp_emner er tom, returnerer vi med NA
  if(identical(character(), tmp_emner)){
    return(NA)
  }
  
  # Hvis den ikke er tom, returnerer vi emnene
  return(tmp_emner)
  
})

telle_emner <- telle_emner %>% 
  unlist() %>% 
  table() %>% 
  .[which(. > 3)] %>% 
  as_tibble()

telle_emner$emneord <- telle_emner$.

telle_emner %>% arrange(n)


ggplot(telle_emner, aes(x = emneord, y = n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = .5,
                                   hjust = 1))


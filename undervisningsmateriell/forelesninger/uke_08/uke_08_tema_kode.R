rm(list = ls())

# Laster pakker
library(readr)      # Read data through C++
library(dplyr)      # Parse manipulation of data through C++
library(pbmcapply)  # Parallelization with progress bar
library(quanteda)
library(tidyverse)
library(tidytext)
library(rvest)
library(stm)



# Laster data
load("./data/reps_preproc_bigram_pos.rda")


# Viser metadata
meta[2:5, c("id", "rep_id", "text")]



# Viser DFM
lemma_counts[2:5, 110:113]


# Estimerer LDA emnemodel med 20 emner
oil_lda <- stm(lemma_counts,
               K = 20,
               data = meta,
               init.type = "LDA",
               max.em.its = 50)
# OBS: "max.em.its" Skal være mye høyere, men det tar lang tid å kjøre
# Bruk "default value" fra ?stm

# Her har jeg lagret og lastet inn modellen fra fil, så jeg
# slipper å estimere hele modellen hver gang jeg kompilerer
# presentasjonen til forelesningen
# save(oil_lda, file = "./data/oil_lda.rda")
# load("./data/oil_lda.rda")


# Her er base-plot fra stm-pakken 
# Se ?plot.STM for hvordan det kan redigeres
plot(oil_lda, labeltype = "frex")


# Her er det samme plotet med ggplot -- det krever litt mer forståelse
# For de forskjellige delene av stm
top_ord <- labelTopics(oil_lda, n = 2)


tibble(exp_prop = apply(oil_lda$theta, 2, mean), 
       topic = paste("Emne: ", 1:20),
       top_ord = apply(top_ord$frex, 1, function(x) str_c(x, collapse = " og "))) %>% 
  ggplot(., aes(x = fct_reorder(topic, exp_prop), y = exp_prop)) +
  geom_col(fill = "#004B4F") +
  geom_text(aes(label = top_ord),
            color = c(rep("white", 6), "orange", rep("white", 13))) +
  coord_flip() +
  labs(x = "", y = "Forventet proporsjon") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.10))+
  ggdark::dark_mode() +
  theme(axis.text.y = element_text(hjust = 0))



# Her trekker jeg ut de to øverste top ord til hvert topic 
top_ord <- labelTopics(oil_lda, n = 2)

# og viser dem i console
top_ord


# Her søker jeg etter et spesifikt ord for å finne ut
# om noen emner inneholder det ordet
findTopic(oil_lda, "utslipp:subst", type = "frex")

# Printe topord på spesifikt emne
labelTopics(oil_lda, topics = 7, n = 5)

# Vise de tekstene som lader høyest på et emne
findThoughts(oil_lda, texts = meta$text, topics = 7)



#### STM med satt antall emner
oil_stm <- stm(lemma_counts,
               K = 20,
               prevalence = ~ factor(party_id),
               data = meta,
               init.type = "Spectral",
               max.em.its = 50)

# save(oil_stm, file = "./data/oil_stm.rda")
# load("./data/oil_stm.rda")

# Samme som over -- kan enten plotte direkte
plot(oil_stm, labeltype = "frex")


# Eller lage plot selv
top_ord <- labelTopics(oil_stm, n = 2)

tibble(exp_prop = apply(oil_stm$theta, 2, mean), 
           topic = paste("Emne: ", 1:20),
           top_ord = apply(top_ord$frex, 1, function(x) str_c(x, collapse = " og "))) %>% 
  ggplot(., aes(x = fct_reorder(topic, exp_prop), y = exp_prop)) +
  geom_col(fill = "darkcyan") +
  geom_text(aes(y = 0.05, label = top_ord),
            color = c(rep("white", 10), "darkorange", rep("white", 9)))  +
  coord_flip() +
  labs(x = "", y = "Forventet proporsjon") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.16))+
  ggdark::dark_mode() +
  theme(axis.text.y = element_text(hjust = 0))



# Her trekker jeg ut de 30 viktigste FREX-ordene
# til topic 10
labelTopics(oil_stm, n = 30)$frex[10, ]


# Lager plot av forventet emneandel på et tilfeldig innlegg fra hvert parti
# (denne tar litt tid å kjøre)
oil_eff <- estimateEffect(~ factor(party_id), oil_stm, metadata = meta)

# Også her kan du lage plot direkte
plot(oil_eff, covariate = "party_id", topics = 11)


# eller lage det selv
eff_plot_data <- plot(oil_eff, covariate = "party_id", topics = 11, omit.plot = TRUE)

tibble(p = eff_plot_data$uvals,
       topic = eff_plot_data$topics,
       m = eff_plot_data$means[[1]],
       upr = eff_plot_data$cis[[1]][1,],
       lwr = eff_plot_data$cis[[1]][2,]) %>% 
  ggplot(., aes(x = fct_reorder(p, m), y = m)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  coord_flip() +
  labs(y = "Forventet andel av innlegg", x = "Parti") +
  ggdark::dark_mode()


# Estimere modell med "optimal" K
oil_stm_optimal <- stm(lemma_counts,
                       K = 0,
                       prevalence = ~ factor(party_id),
                       data = meta,
                       init.type = "Spectral",
                       max.em.its = 50)

# save(oil_stm_optimal, file = "./data/oil_stm_optimal.rda")
#load("./data/oil_stm_optimal.rda")

# Sjekke antall emne
oil_stm_optimal$settings$dim$K

# Leter etter emne om klima på top 50 frex-ord
klima <- findTopic(oil_stm_optimal, 
                   list = c("klima:subst"), 
                   type = "frex", 
                   n = 50)

# Trekker ut alle toppord for emenene som har "klima"
# i sine topp 50 ord
labelTopics(oil_stm_optimal, n = 5,
            topics = klima)$frex[klima, ] %>% 
  t() # Denne er bare for å få mer forståelig output (se ?t)


# Lager plot av forventet emneandel på et tilfeldig innlegg fra hvert parti
oil_eff <- estimateEffect(~ factor(party_id), oil_stm_optimal, metadata = meta)

# Default plot
plot(oil_eff, covariate = "party_id", topics = klima)


# Eller eget plot
eff_plot_data <- plot(oil_eff, covariate = "party_id", topics = klima, omit.plot = TRUE)

tibble(p = rep(eff_plot_data$uvals, 2),
       t = rep(c("Grønne skiftet (49)", "Klimamål (77)"), each = length(eff_plot_data$uvals)),
       m = unlist(eff_plot_data$means),
       upr = unlist(lapply(eff_plot_data$cis, function(x) x[2, ])),
       lwr = unlist(lapply(eff_plot_data$cis, function(x) x[1, ]))) %>% 
  ggplot(., aes(x = fct_reorder(p, m), y = m, color = factor(t))) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position = position_dodge(width = 0.2)) +
  coord_flip() +
  labs(y = "Forventet andel av innlegg", x = "Parti", color = "Emne") +
  ggdark::dark_mode()


# Når vi skal leke med koherens og eksklusivitet
# må vi konvertere dfm til et stm-objekt
stm_counts <- convert(lemma_counts, "stm")

# Her legger jeg koherens og eksklusivitet inn i en dataframe
oil_sem_coh <- tibble(
  sem_coh = semanticCoherence(model = oil_stm_optimal, documents = stm_counts$documents),
  exclu = exclusivity(oil_stm_optimal),
  emne = str_c("Emne: ", 1:98)
)

# Lager en label for plotting
oil_sem_coh$lab <- ifelse(str_detect(oil_sem_coh$emne, "49|77"), oil_sem_coh$emne, "")

ggplot(oil_sem_coh, aes(x = sem_coh, y = exclu)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = lab), max.overlaps = 200) +
  stat_density2d() +
  labs(x = "Semantisk sammenheng", y = "Eksklusivitet") +
  ggdark::dark_mode()


### Serch K modell

# Setter opp hvilke K jeg vil estimere
K <- c(2, 5, 10, 30, 50, 70, 100, 150, 200)

set.seed(4685)

# Kjører modell på alle K
k_results <- searchK(documents = stm_counts$documents,
                     vocab = stm_counts$vocab, K = K,
                     data = stm_counts$meta)

# save(k_results, file = "./data/search_k_results.rda")
# load("./data/search_k_results.rda")


# Default plot
plot(k_results)

# Et annet alternativ
k_result_plotdata <- tibble(Exclusivity = unlist(k_results$results$exclus),
                            Coherence = unlist(k_results$results$semcoh),
                            Heldout = unlist(k_results$results$heldout),
                            Residual = unlist(k_results$results$residual),
                            K = unlist(k_results$results$K)) %>% 
  pivot_longer(., cols = c("Exclusivity", "Coherence", "Heldout", "Residual"))


ggplot(k_result_plotdata, aes(x = K, y = value)) +
  facet_wrap(~name, scales = "free_y") +
  geom_point() +
  geom_line() +
  theme_classic() +
  labs(y = NULL, x = "Antall emner") +
  ggdark::dark_mode()


# Prøver å "gjette" hvilket emne teksten fra
# Carl I. Hagen modellen vår tror passer best i
cih <- stortingscrape::read_obt("./cih_obt.txt")

#
lemmas_all <- cih %>% 
  filter(grepl("[[:punct:]]|\\–|^AV$", lemma) == FALSE) %>%
  group_by(sentence) %>% 
  mutate(lemma = tolower(lemma),                                        # Lowercasing all lemma
         lemma = ifelse(lemma %in% stopwords("norwegian"), NA, lemma),  # Removing stopwords
         lemma_pos = paste(lemma, pos, sep = ":"),
         next_lemma_pos = lead(lemma_pos),                                      # Leading lemma for bigram construction
         lemma_pos_bigram = ifelse(grepl("^NA\\:|\\:NA$", lemma_pos) | grepl("^NA\\:|\\:NA$", next_lemma_pos),        # Constructing bibrams
                                   NA, paste(lemma_pos, next_lemma_pos)),
         lemma_pos = ifelse(grepl("^NA\\:|\\:NA$", lemma_pos), NA, lemma_pos)) %>%
  filter((is.na(lemma_pos) == FALSE | is.na(lemma_pos_bigram) == FALSE))

lemmas_all <- lemmas_all %>% 
  mutate(lemma_pos_bigram = ifelse(grepl("\\sNA$", lemma_pos_bigram) == FALSE, lemma_pos_bigram, NA)) %>% 
  filter(is.na(lemma_pos) == FALSE | is.na(lemma_pos_bigram) == FALSE)

# Counting lemma bigrams
lemma_unigrams_pos <- lemmas_all %>%
  filter(is.na(lemma_pos) == FALSE) %>%
  count(lemma_pos)

# Counting lemma bigrams
lemma_bigrams_pos <- lemmas_all %>%
  filter(is.na(lemma_pos_bigram) == FALSE) %>%
  count(lemma_pos_bigram)

names(lemma_bigrams_pos)[2] <- "lemma_pos"

lemma_counts <- bind_rows(lemma_unigrams_pos, lemma_bigrams_pos) %>%
  mutate(id = "cih") %>% 
  group_by(id) %>% 
  filter(is.na(lemma_pos) == FALSE) %>%                              
  count(lemma_pos) %>%                                               
  cast_dfm(document = id, term = lemma_pos, n)

cih_counts <- convert(lemma_counts, "stm")



cih_pred <- fitNewDocuments(oil_stm_optimal, 
                            documents = cih_counts$documents,
                            newData = tibble(party_id = "FrP"),
                            origData = meta)


cih_theta <- cih_pred$theta[1, ] %>% round(., 3) 
names(cih_theta) <- str_c("Emne: ", 1:oil_stm_optimal$settings$dim$K)

cih_theta %>% sort(., decreasing = TRUE) %>% .[1:5]

labelTopics(oil_stm_optimal, topics = c(6, 18, 59, 47, 1), n = 4)

# Oppsummert: mye jobb for noe som gir lite mening.


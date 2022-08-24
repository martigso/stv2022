library(tidyverse);library(tidytext);library(stringr)
set.seed(8954558)

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

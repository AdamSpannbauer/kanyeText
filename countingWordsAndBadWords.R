rm(list=ls())
library(tidyverse)
library(tidytext)
library(stringr)
library(hunspell)

lyrics_df <- read_csv("data/kanye_lyrics.csv")
fcc_let_me_be <- read_csv("data/wirty_dords.csv") %>% 
  .[[1]]

# removes things like this* with spaces    *"[Intro: DeRay Davis]"
lyrics_df_no_brack <- lyrics_df %>% 
  mutate(lyrics = str_replace_all(lyrics, "\\[([^]]+)", ''))

# custom hunspell stemmer for use in mutate
my_hunspell_stem <- function(token) {
  stem_token <- hunspell_stem(token)[[1]]
  if (length(stem_token) == 0) return(token) else return(stem_token[1])
}
vec_hunspell_stem <- Vectorize(my_hunspell_stem, "token")

lyric_tokens_df <- lyrics_df_no_brack %>% 
  unnest_tokens(tokens, lyrics)

lyric_hunspell_df <- lyric_tokens_df %>% 
  mutate(tokens = vec_hunspell_stem(tokens)) %>%
  filter(!(tokens %in% stop_words$word))

######################################################################################
# counting words #####################################################################
######################################################################################
#count all words... boring
most_used_words <- lyric_hunspell_df %>% 
  group_by(tokens) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  arrange(desc(n))

# function to take tbl with token col and replace detected bad_words with "stemmed" bad_word
  # removes non-nsfw rows from tbl
make_nsfw_df <- function(tbl, word_col, bad_words) {
  word_col_str <- substitute(word_col)
  # extract words to check if bad
  check_words <- tbl %>% 
    select_(word_col_str) %>% 
    .[[1]]
  
  #sort bad_words by length (will replace matches with shortest)
  bad_sort <- bad_words[order(nchar(bad_words))]
  
  #detect bad words
  suppressWarnings({
    bad_word_root_ind <- map_dbl(check_words, ~str_detect(.x, fixed(bad_sort)) %>% 
                                   which() %>% 
                                   min())
  })
  
  #correct for min(integer(0))... occurs when no bad detected
  bad_word_root_ind <- replace(bad_word_root_ind, 
                               is.infinite(bad_word_root_ind), 
                               NA)
  
  tbl %>% 
    mutate(bad_word_ind = bad_word_root_ind) %>% 
    filter(!is.na(bad_word_ind)) %>% #filter out non nsfw words
    mutate(bad_word_root = bad_sort[bad_word_ind]) %>% #supply root bad word
    select(-bad_word_ind) #drop bad inds
}

#create nsfw tbls
nsfw_tokens <- lyric_hunspell_df %>% 
  make_nsfw_df(tokens, fcc_let_me_be)

most_used_nsfw <- nsfw_tokens %>% 
  group_by(bad_word_root) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count))



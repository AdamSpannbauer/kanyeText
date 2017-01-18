rm(list=ls())
library(tidyverse)
library(tidytext)
library(stringr)
library(hunspell)
library(magrittr)

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

# tokenize song lyrics
lyric_tokens_df <- lyrics_df_no_brack %>% 
  unnest_tokens(tokens, lyrics)

# stem words w/hunspell and rm stop words
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

# A tibble: 6,757 × 2
#     tokens     n
#       <chr> <int>
#   1      la   224
#   2    shit   215
#   3    love   212
#   4    feel   163
#   5  niggas   153

#why is la top 'word'
lyric_hunspell_df %>% 
  filter(tokens=="la") %>% 
  group_by(song) %>% 
  summarise(count_la = n()) %>% 
  ungroup() %>% 
  arrange(desc(count_la))

# A tibble: 10 × 2
#                           song count_la
#                           <chr>    <int>
# 1                  Crack Music      132
# 2        Can't Tell Me Nothing       72
# 3                      RoboCop        8
# 4                Touch the Sky        4
# 5        No More Parties in LA        3

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
    bad_word_root_ind <- map_dbl(check_words, 
                                 ~str_detect(.x, fixed(bad_sort)) %>% 
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

#find most used nsfw words
most_used_nsfw <- nsfw_tokens %>% 
  group_by(tokens) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count))

# A tibble: 102 × 2
#             tokens count
#              <chr> <int>
#   1           shit   215
#   2         niggas   153
#   3           fuck   151
#   4          nigga   136
#   5           dick    26

#find most used nsfw root words
most_used_nsfw_root <- nsfw_tokens %>% 
  group_by(bad_word_root) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count))

# A tibble: 15 × 2
#      bad_word_root count
#              <chr> <int>
#   1          nigga   305
#   2           fuck   266
#   3           shit   265
#   4           dick    32
#   5          pussy    26

#summarise nsfw by song
nsfw_by_song <- nsfw_tokens %>% 
  group_by(year, type, project, track_num, song) %>% 
  summarise(curse_count = n()) %>% 
  ungroup() %>% 
  arrange(desc(curse_count))

nsfw_by_song %>% 
  select(project, song, curse_count)
# A tibble: 116 × 3
#                                project                  song curse_count
#                                  <chr>                 <chr>       <int>
#   1  My Beautiful Dark Twisted Fantasy               Monster          44
#   2  My Beautiful Dark Twisted Fantasy            Blame Game          43
#   3  My Beautiful Dark Twisted Fantasy           So Appalled          37
#   4                The College Dropout             Last Call          36
#   5                             Yeezus            New Slaves          27

#histogram of nsfw count by song
ggplot(data=nsfw_by_song, aes(x=curse_count)) +
  geom_histogram() +
  xlab("Number of FCC Banned Words by Song") +
  ylab("Count")

ggplot(nsfw_by_song, aes(x=track_num, y=curse_count, group=project, colour=project)) +
  geom_line() +
  xlab("Track Number") +
  ylab("Count FCC Banned")

#summarise nsfw by album
nsfw_by_album <- nsfw_by_song %>% 
  group_by(year,type,project) %>% 
  summarise(curse_count = sum(curse_count)) %>% 
  ungroup() %>% 
  arrange(desc(curse_count))

# A tibble: 10 × 4
#       year    type                           project curse_count
#      <dbl>   <chr>                             <chr>       <int>
#   1   2010   album My Beautiful Dark Twisted Fantasy         196
#   2   2004   album               The College Dropout         151
#   3   2016   album                 The Life of Pablo         134
#   4   2013   album                            Yeezus          98
#   5   2005   album                 Late Registration          94
#   6   2003 mixtape                  Get Well Soon...          85
#   7   2007   album                        Graduation          77
#   8   2003 mixtape                          I'm Good          70
#   9   2007 mixtape             Can't Tell Me Nothing          51
#   10  2008   album                 808s & Heartbreak           1

#bar plot curse_count per album
ggplot(data=nsfw_by_album, aes(x=project, y=curse_count)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  ylab("Count FCC Banned Words") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

# summarise nsfw by year
nsfw_by_year <- nsfw_by_album %>% 
  group_by(year) %>% 
  summarise(project = paste(project, collapse=", "),
            curse_count = sum(curse_count)) %>% 
  ungroup() %>% 
  arrange(desc(curse_count))

ggplot(nsfw_by_year, aes(x=year, y=curse_count)) +
  geom_line() +
  xlab("Year") +
  ylab("Count FCC Banned Words")




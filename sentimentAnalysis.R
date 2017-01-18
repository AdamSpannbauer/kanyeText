rm(list=ls())
library(tidyverse)
library(tidytext)
library(stringr)
library(hunspell)
library(magrittr)

lyrics_df <- read_csv("data/kanye_lyrics.csv")

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

# stem words w/hunspell
lyric_hunspell_df <- lyric_tokens_df %>% 
  mutate(tokens = vec_hunspell_stem(tokens))

######################################################################################
# sentiment analysis #################################################################
######################################################################################
sentiment_df <- get_sentiments("afinn") %>% 
  mutate(word = vec_hunspell_stem(word)) %>% 
  bind_rows(get_sentiments("afinn"))

#inner join to sentiments
lyrics_sentiment_df <- lyric_hunspell_df %>% 
  inner_join(sentiment_df, by=c("tokens"="word"))

#sum sent for each song
sentiment_by_song <- lyrics_sentiment_df %>% 
  group_by(year, type, project, track_num, song) %>% 
  summarise(score = sum(score)) %>% 
  ungroup() %>% 
  arrange(desc(score))

#most positive songs
sentiment_by_song %>% 
  select(project, song, score)

# A tibble: 147 × 3
#                                project              song score
#                                  <chr>             <chr> <int>
#   1                  808s & Heartbreak           Amazing   460
#   2  My Beautiful Dark Twisted Fantasy All of the Lights   267
#   3                  The Life of Pablo   Ultralight Beam   171
#   4                         Graduation   Flashing Lights   138
#   5                         Graduation         Good Life   134

#most negative songs
sentiment_by_song %>% 
  select(project, song, score) %>% 
  arrange(score)

# A tibble: 146 × 3
#                                project                song score
#                                  <chr>               <chr> <int>
#   1  My Beautiful Dark Twisted Fantasy             Monster  -610
#   2  My Beautiful Dark Twisted Fantasy         So Appalled  -560
#   3  My Beautiful Dark Twisted Fantasy          Blame Game  -419
#   4                   Get Well Soon...           Champions  -323
#   5                             Yeezus          Send It Up  -301

# is there a pattern to sentiment by track number?
ggplot(sentiment_by_song, aes(x=track_num, y=score, group=project, colour=project)) +
  geom_line() +
  xlab("Track Number") +
  ylab("Score")

#sentiment by album
sentiment_by_album <- sentiment_by_song %>%
  group_by(year, type, project) %>% 
  summarise(score = sum(score)) %>% 
  ungroup() %>% 
  arrange(desc(score))

# A tibble: 10 × 4
#       year    type                           project score
#      <dbl>   <chr>                             <chr> <int>
#   1   2008   album                 808s & Heartbreak   338
#   2   2007   album                        Graduation   -44
#   3   2007 mixtape             Can't Tell Me Nothing  -249
#   4   2005   album                 Late Registration  -369
#   5   2003 mixtape                          I'm Good  -429
#   6   2004   album               The College Dropout  -710
#   7   2003 mixtape                  Get Well Soon...  -895
#   8   2016   album                 The Life of Pablo  -949
#   9   2013   album                            Yeezus -1521
#   10  2010   album My Beautiful Dark Twisted Fantasy -2051

#sentiment by year
sentiment_by_year <- sentiment_by_album %>% 
  group_by(year) %>% 
  summarise(project = paste(project, collapse=", "),
            score   = sum(score)) %>% 
  ungroup() %>% 
  arrange(desc(score))

# A tibble: 8 × 3
#      year                           project score
#     <dbl>                             <chr> <int>
#   1  2008                 808s & Heartbreak   338
#   2  2007 Graduation, Can't Tell Me Nothing  -293
#   3  2005                 Late Registration  -369
#   4  2004               The College Dropout  -710
#   5  2016                 The Life of Pablo  -949
#   6  2003        I'm Good, Get Well Soon... -1324
#   7  2013                            Yeezus -1521
#   8  2010 My Beautiful Dark Twisted Fantasy -2051

ggplot(sentiment_by_year, aes(x=year, y=score)) + 
  geom_line() +
  geom_point()

# how does removing fcc banned words change sentiment







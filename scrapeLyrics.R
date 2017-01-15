rm(list=ls())
library(tidyverse)
library(rvest)
library(xml2)
library(stringr)

######################################################################################
# scrape album & mixtape titles ######################################################
######################################################################################
discography_wiki_url <- "https://en.wikipedia.org/wiki/Kanye_West_discography"

#function to scrape tables from wiki discography page
get_album_title_and_year <- function(wiki_url, xpath, type) {
  #get table of just title and album details columns
  title_details_df <- wiki_url %>% 
    read_html() %>% 
    html_nodes(xpath=xpath) %>% 
    html_table(fill=TRUE) %>% 
    .[[1]] %>% #desired table first element of resulting list
    .[,1:2] %>% #desired columns title and album details are first 2 cols
    filter(Title != "Title") %>% #remove duplicated title row
    filter(str_sub(Title, end=3) != "\"—\"") %>% #remove footnote row
    set_names(c("project", "details"))
  
  #extract year from album details column
  title_year_df <- title_details_df %>% 
    mutate(date = str_match(details, pattern=regex("Released: ([^\\[]+)"))[,2]) %>% #extract full date (differing formats)
    mutate(year = str_sub(date, -4)) %>% #diff level of date info but all have year in last 4 chars
    mutate(year = as.numeric(year)) %>% 
    select(-details, -date)
  
  #add type and rearrange cols
  res_df <- title_year_df %>% 
    mutate(type = type) %>% 
    select(year, type, project)
}

#get album and mixtape titles
album_df <- get_album_title_and_year(discography_wiki_url,
                                     xpath='//*[@id="mw-content-text"]/table[2]',
                                     type="album")

mixtape_df <- get_album_title_and_year(discography_wiki_url,
                                 xpath='//*[@id="mw-content-text"]/table[6]',
                                 type="mixtape")

#remove "(with A-Trak)" from welcome to kanyes soul mixtape
mixtape_df <- mixtape_df %>% 
  mutate(project = str_replace_all(project, 
                                   pattern=fixed("(with A-Trak)"), 
                                   replacement=""))

# create kanye discography and save to csv
bind_rows(album_df, mixtape_df) %>% 
  arrange(year) %>% 
  write_csv("data/kanye_discography.csv")
  
######################################################################################
# scrape song titles #################################################################
######################################################################################
rm(list=ls())
#read in discography data
disc_df <- read_csv("data/kanye_discography.csv")

genius_base_url <- "https://genius.com/albums/Kanye-west/"

#function to scrape genius.com for an albums song titles
get_album_songs <- function(genius_base_url, album_name, verbose=TRUE) {
  if (verbose) cat("getting songs for", album_name, "\n")
  #clean album name with genius's rules (replace punct with space; replace \\s+ with "-")
  album_name <- album_name %>% 
    str_replace_all("[[:punct:]]", " ") %>% 
    trimws() %>% 
    str_replace_all(regex("\\s+"), "-")
  
  #create album full url
  album_url <- genius_base_url %>% 
    paste0(album_name)
  
  #scrape song titles and strip html
  songs <- album_url %>% 
    read_html() %>% 
    html_nodes(css= ".song_title") %>% 
    str_match(regex("\\>(.+)\\<")) %>% 
    .[,2]
  
  songs %>% 
    str_replace_all(pattern=fixed("&amp;"), replacement="&")
}

#scrape song titles for discography and unnest
songs_df <- disc_df %>% 
  mutate(song = map(project, ~get_album_songs(genius_base_url, .x))) %>% 
  unnest()

#save full song list
write_csv(songs_df, "data/kanye_full_song_list.csv")

######################################################################################
# scrape song lyrics #################################################################
######################################################################################
rm(list=ls())
#read in full song list
songs_df <- read_csv("data/kanye_full_song_list.csv")

genius_base_url_lyrics <- "https://genius.com/Kanye-west-"

#function to scrape song lyrics
get_song_lyrics <- function(genius_base_url, song_name, verbose=TRUE) {
  if (verbose) cat("getting lyrics for", song_name, "\n")
  
  #clean song names the same as album titles
  clean_song_title <- song_name %>% 
    str_replace_all("[[:punct:]]", " ") %>% 
    trimws() %>% 
    str_replace_all(regex("\\s+"), "-")
  
  #create full song lyric url
  song_url <- genius_base_url_lyrics %>% 
    paste0(clean_song_title, "-lyrics")
  
  #scrape and extract song lyrics
  lyrics <- song_url %>% 
    read_html() %>% 
    html_nodes(css= "p") %>% 
    html_text() %>% 
    .[1]
  
  lyrics
}
#safe version for map
safe_get_lyrics <- failwith(default = NA_character_, get_song_lyrics)

#perform lyrics scrape
lyrics_df <- songs_df %>% 
  mutate(lyrics = map(song, ~safe_get_lyrics(genius_base_url, .x))) %>% 
  unnest()

#after inspection of failed cases main causes of failure are:
#   1) songs from colloboration albums where ye wasnt artist
#   2) digital booklets listed as songs
lyrics_df %>% 
  group_by(project) %>% 
  summarise(NA_count = sum(is.na(lyrics)),
            NA_proportion = sum(is.na(lyrics)/n()))

#filter out lyricless cases and save
lyrics_df <- lyrics_df %>% 
  filter(!is.na(lyrics)) %>% 
  filter(trimws(lyrics) != "")

write_csv(lyrics_df, "data/kanye_lyrics.csv")


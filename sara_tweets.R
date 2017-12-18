# Sara's tweets
# A.Bailey December 18th 2017

library(tidytext)
library(tidyverse)
library(streamR)
library(stringr)

# Get tweets -------------------------------------------------------------------
# My credentials
load("credentials/oauth_token.Rdata")

## Handy functions created by Pablo Barbera ------------------------------------
# You need JSONIO installed to use these functions
source("functions.R")

# Get tweets from timeline -----------------------------------------------------
# Here grab 1000 tweets from the New York Times and then save them
# as a json file
getTimeline(filename="tweets_sara.json", screen_name="ArchaeologistSP", 
            n=6000, oauth=my_oauth, trim_user="false")

getTimeline(filename="tweets_uoya.json", screen_name="UoYArchaeology", 
            n=6000, oauth=my_oauth, trim_user="false")


# Read the json file back in as a tibble ---------------------------------------
tweets <- as_tibble(parseTweets("tweets_sara.json"))

uouya_tweets <- as_tibble(parseTweets("tweets_uoya.json"))

# Fix the date for analysis ----------------------------------------------------
tweets <- tweets %>% 
  # Mutate tweets to add time stamp
  # Use regex to get rid of the milliseconds.
  # And then reformat the date
  mutate(timestamp = as.Date(strptime(gsub("\\+\\d+\\s",""
                                           ,tweets$created_at, perl = T),
                                      "%a %b %d %H:%M:%S %Y")))

# Tidy the tweets --------------------------------------------------------------
# From http://tidytextmining.com/twitter.html
# regex to deal with urls and retweets,
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
# regex to for tokenising tweets to words
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

# Retweets ---------------------------------------------------------------------
tidy_retweets <- tweets %>% 
  filter(str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

tidy_retweets %>% group_by(name, id_str) %>% 
  summarise(rts = sum(retweet_count)) %>% 
  group_by(name) %>% summarise(total_rts = sum(rts))


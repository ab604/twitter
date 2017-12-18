# Twitter scraper outline
# A.Bailey 13th 2017

# Load packages ----------------------------------------------------------------
library(streamR)
library(ROAuth)
library(tidytext)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

# Set-up connection to twitter API ---------------------------------------------
## From:
## https://github.com/pablobarbera/social-media-workshop/blob/master/01-twitter-data-collection.r
## Step 1: go to apps.twitter.com and sign in
## Step 2: click on "Create New App"
## Step 3: fill name, description, and website (it can be anything, even google.com)
##			(make sure you leave 'Callback URL' empty)
## Step 4: Agree to user conditions
## Step 5: copy consumer key and consumer secret and paste below

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "OcD3Cpx9T9Q5WHUZeojwGC8uJ"
consumerSecret <- "IgnDCjeRVCG1ihDK43w3vQ7kthlaWKlCCwmCnLTxi7xennK0hv"

# Create authorisation
my_oauth <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret, 
                             requestURL=requestURL,
                             accessURL=accessURL, 
                             authURL=authURL)

# Run this line and go to the URL that appears on screen
my_oauth$handshake(cainfo = system.file("CurlSSL", 
                                        "cacert.pem", package = "RCurl"))

# Save oauth token for use in future sessions with twitteR or streamR
save(my_oauth, file="oauth_token.Rdata")

## Handy functions created by Pablo Barbera ------------------------------------
# You need JSONIO installed to use these functions
source("functions.R")

# Get tweets from timeline -----------------------------------------------------
# Here grab 1000 tweets from the New York Times and then save them
# as a json file
getTimeline(filename="tweets_sara.json", screen_name="ArchaeologistSP", 
            n=6000, oauth=my_oauth, trim_user="false")

# Friends ----------------------------------------------------------------------
# downloading friends of a user
library(tweetscores)
user <- "ArchaeologistSP"
friends <- getFriends(screen_name=user, oauth_folder="credentials")


# Read the json file back in as a tibble ---------------------------------------
tweets <- as_tibble(parseTweets("tweets_sara.json"))

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

# Get all heritage jam tweets --------------------------------------------------
filterStream("tweets_world.json", 
                             locations = c(-175, -85, 179, 85), 
             track=c("heritage", "digital media", "anthropology",
                     "archaeology","Çatalhöyük"),
                             timeout = 30,
                             oauth = my_oauth)

tweets_filter <- as_tibble(parseTweets("tweets_world.json"))

glimpse(tweets_filter)

library(grid)
map.data <- map_data("world")
points <- data.frame(x = as.numeric(tweets_filter$lon), y = as.numeric(tweets_filter$lat))
points <- points[points$y > 25, ]
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
                            color = "grey20", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                 aes(x = x, y = y), size = 1, alpha = 1/5, color = "darkblue")

# Sentiment analysis -----------------------------------------------------------
# Calculate sentiment score
tweets_sent <- tidy_tweets %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sort = TRUE, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)

# Plot sentiments
tweets_sent  %>%
        top_n(10, abs(sentiment)) %>%
        mutate(word = reorder(word, sentiment)) %>%
        ggplot(aes(word, sentiment, fill = sentiment > 0)) +
        geom_col(show.legend = FALSE) +
        coord_flip()

# Plot activity ----------------------------------------------------------------
ggplot(tweets, aes(x = timestamp)) +
        geom_histogram(position = "identity", bins = 7, show.legend = FALSE) 

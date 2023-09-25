
# Function to read anything from the manifest
read_archive_json <- function(path) {
  lines <- brio::read_lines(path)
  lines[1] <- sub("^[^{[]+([{[])", "\\1", lines[1])
  
  jsonlite::fromJSON(
    txt = lines,
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
}

# Get the manifest
manifest <- read_archive_json("data/manifest.js")

#get the tweets! 
tweets <- read_twitter_data(manifest, "tweets")

# Get one tweet to test functions
tweet <- read_twitter_data(manifest, "tweets")[[1]][[105]]$tweet

str(tweet, max.level = 2)

tidy_tweet_raw <- function(tweet_raw) {
  basic_items <- c(
    "created_at",
    "favorite_count",
    "retweet_count",
    "full_text",
    "id",
    "lang",
    "source"
    
  )
  
  # start with a few basic items
  tweet <- tweet_raw[basic_items]
  
  # and collapse a few nested items into a single string
  tweet$user_mentions <- tweet_raw |>
    purrr::pluck("entities", "user_mentions") |>
    purrr::map_chr("screen_name") |>
    paste(collapse = ",")
  
  tweet$hashtags <- tweet_raw |>
    purrr::pluck("entities", "hashtags") |>
    purrr::map_chr("text") |>
    paste(collapse = ",")
  
  # tweet$favorited <- tweet_raw |>
  #   purrr::pluck("entities", "favorite_count")
    # purrr::map_chr("text")
  tweet
}

tidyHenner <- read_twitter_data(manifest, "tweets") |>
  simplify_twitter_data(tidy_tweet_raw) %>% 
  mutate(
    across(contains("_count"), as.integer),
    retweet = str_detect(full_text, "^RT @"),
    reply = str_detect(full_text, "^@"),
    type = case_when(
      retweet ~ "retweet",
      reply ~ "reply",
      TRUE ~ "tweet"
    ),
    created_at = strptime(created_at, "%a %b %d %T %z %Y"),
    # hour = hour(created_at),
    # day = wday(created_at, label = TRUE, abbr = TRUE, week_start = 1),
    # month = month(created_at, label = TRUE, abbr = FALSE),
    # day_of_month = day(created_at),
    # year = year(created_at), 
    containsURL=grepl("http:|https:", full_text), 
    linkInTweet=str_extract(full_text, "http[s].*"), 
    justLink=str_extract(linkInTweet, "^\\S*"), 
    lngthLink=str_length(justLink), 
    linkFromTweet=case_when(
      lngthLink==23 ~ justLink, 
      lngthLink>23 ~ str_sub(justLink, start=1L, end=23L),
      is.na(lngthLink) ~ NA, 
      .default="ERROR"), 
    # Create a link to the original tweet
    linkToTweet=
      paste0("https://twitter.com/jmhenner/status/",id)
  ) %>% 
  mutate(created_at=anytime::anytime(created_at)) %>% 
  select(-source, -retweet, -reply
         ,-linkInTweet, -justLink, -lngthLink
         ) %>% 
  relocate(type,.after=created_at) %>% 
  relocate(id) %>% 
  left_join(mediaList %>% rename(id=tweetID) %>% select(id, gitUrl)) %>% 
  mutate(hasImage=!is.na(gitUrl)) %>% 
  select(
    id, created_at,type, lang,full_text,favorite_count, retweet_count, user_mentions, hashtags, 
    linkToTweet, containsURL, linkFromTweet, hasImage, gitUrl
  )




# googlesheets4::gs4_create(sheets=tidyHenner %>% slice_max(favorite_count, n=500))



library(tidyverse)
top500 <- tidyHenner %>% slice_max(favorite_count, n=500)
start <- Sys.time()
for(row in 1:nrow(top500)){
  tweetrmd::tweet_screenshot( tweet_url=paste(top500[row,10]),
    file=paste0("./output/",paste(top500[row,1]),".png"),
    scale=1
  )
}
end <- Sys.time()
end-start


tweetrmd::tweet_screenshot("https://twitter.com/jmhenner/status/1065021620224516098", 
                           file="test2.png", scale=2,
                           hide_thread=F, widget_type="video")

# readr::write_csv(tidyHenner, "tidyHenner.csv")
library(tidyverse)

mediaFolder <- "/Users/christopherwenz/Code/hennerTweets/data/tweets_media"

# https://github.com/cawenz/hennerTweets/blob/main/data/tweets_media/1001608265682374657-DeZtZsIX4AAIDQG.mp4


mp4url <- "https://raw.githubusercontent.com/cawenz/hennerTweets/main/data/tweets_media/"


# mp4url <- "https://raw.githubusercontent.com/cawenz/hennerTweets/374d9bf282d8c82f312a2ae7000f715f3a38f4cb/data/tweets_media/"
# jpgurl <- "https://github.com/cawenz/hennerTweets/blob/374d9bf282d8c82f312a2ae7000f715f3a38f4cb/data/tweets_media/"

mediaList <- data.frame(
                        fileName=list.files(path=mediaFolder, full.names=F),
                        kb=file.size(list.files(path=mediaFolder, full.names=T))/1000,
                        mb=round(file.size(list.files(path=mediaFolder, full.names=T))/10^6,digits=3)
) %>% 
  mutate(fileType=
           str_extract(fileName, "\\..*"), 
         gitUrl=
           paste0(mp4url, fileName), 
         tweetID=
           str_extract(fileName, "[^-]+"), 
         fileCode=
           str_extract(fileName, "\\-.*"), 
         tweetURL=paste0("https://twitter.com/jmhenner/status/", tweetID)
  ) %>% 
  select(-fileName)

# the df "mediaList" now has a link to every image/video 

write.csv(mediaList, "git.csv")



#*****************************************************************************************
# How to get a image screenshot of any tweet:
#*****************************************************************************************

tweetrmd::tweet_screenshot("https://twitter.com/jmhenner/status/1065021620224516098", 
                           file="test2.png", scale=2,
                           hide_thread=F, widget_type="video")
                           

# Configure certificate
library(RCurl) 
# Set SSL certs globally
#options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

# Create a new Twitter App
# https://apps.twitter.com/

rm(list=ls())

library("twitteR")
library("ROAuth")

# app: testing-twitteR
consumer_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
consumer_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
req_URL <- "https://api.twitter.com/oauth/request_token"
access_URL <- "https://api.twitter.com/oauth/access_token"
auth_URL <- "http://api.twitter.com/oauth/authorize"

# generate access token
access_token <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
access_secret <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)
# [1] "Using direct authentication"
# Use a local file to cache OAuth access credentials between R sessions?
# 1: Yes
# 2: No
# Choose 2

tweets_7JRes <- searchTwitter("#7JRes", n=1500, retryOnRateLimit=1)

################################################################################
# Getting user profile information and timeline
################################################################################
twit_user <- getUser("pablohaya")
twit_user$getFollowersCount()
twit_user$getFollowers()

tweets_user <- userTimeline(twit_user, n = 1000, includeRts = T)
df <- do.call("rbind", lapply(tweets_user, as.data.frame))

################################################################################
# Listen to the Twitter Stream.
################################################################################
library(streamR)

# To capture 10 minutes of Tweets containing the hashtag ‘#7JRes’, you have to
# provide a file.name where the Tweets are saved to, your search term, the time
# you want to record the stream in seconds and the name of the object of your
# credentials/authentication loaded above. The tweets are saved in the file in
# JSON format, which can be converted with one line into a data frame again.
filterStream(file.name="tweets_keyword.json", track=c("#7JRes"),
             timeout=600, oauth=twit_cred)
dim(tweets_stream)
names(tweets_stream)
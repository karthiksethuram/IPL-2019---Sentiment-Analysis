library(twitteR)
library(dplyr)
library("SnowballC")
library("tm")
library("syuzhet")
library(ggplot2)
library(data.table)




# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- "RnVrfcV3Bd9j0FeCeqDr48xnr"
consumer_secret <- "tDImNp07JZNSuIiM0rDY7NJqQvuK1D9ejOfXn0JTkJvww5LYLu"
access_token <- "2240235625-yHGNjLvPJ9pfhn59epEHDR1Me0OnxpiZ4nOzgxf"
access_secret <- "RE5JPTECJfXmNpsfs9ZjLTPxI4PqadkyPy11TprytDCSW"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)




tw_1 = twitteR::searchTwitter('#IPL', n = 1000,lang="en",since = '2019-04-25',until='2019-04-26')
tw_2 = twitteR::searchTwitter('#IPL', n = 1000,lang="en",since = '2019-04-26',until='2019-04-27')
tw_3 = twitteR::searchTwitter('#IPL', n = 1000,lang="en",since = '2019-04-27',until='2019-04-28')
tw_4 = twitteR::searchTwitter('#IPL', n = 1000,lang="en",since = '2019-04-28',until='2019-04-29')
tw_5 = twitteR::searchTwitter('#IPL', n = 1000,lang="en",since = '2019-04-29',until='2019-04-30')
tw_6 = twitteR::searchTwitter('#IPL', n = 1000,lang="en",since = '2019-04-30',until='2019-05-01')
tw_7 = twitteR::searchTwitter('#IPL', n = 1000,lang="en",since = '2019-05-01',until='2019-05-02')

tw_n<-append(tw_1,tw_2)
tw_n<-append(tw_n,tw_3)
tw_n<-append(tw_n,tw_4)
tw_n<-append(tw_n,tw_5)
tw_n<-append(tw_n,tw_6)
tw_n<-append(tw_n,tw_7)

df<- twListToDF(tw_n)

setwd("C:/Users/karth/Documents/Rutgers/Course Materials/Spring 2019/Data Wrangling/Project/Data")
write.csv(df,"tweet.csv")



#Twitter Data For Mankading
tw_mankad = twitteR::searchTwitter('mankad', n =5000,lang="en",since = '2019-03-25')
df_mankad<- twListToDF(tw_mankad)

setwd("C:/Users/karth/Documents/Rutgers/Course Materials/Spring 2019/Data Wrangling/Project/Data")
write.csv(df_mankad,"mankad.csv")



tw_mankad = twitteR::searchTwitter('#mankad + #IPL', n = 000,lang="en",since = '2019-03-25')



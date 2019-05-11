## DATE - MAY 05 2019									




############################################################################################################################
##                                  PULLING THE TWEET DATA FOR IPL                                                        ##

## This code pulls the tweet data with #IPL and #mankad for the specified time period                                     ##
							
## TIME PERIOD USED IN THE CODE - 25APR 2019 TO 02MAY 2019							

############################################################################################################################




############################################################################################################################
##                                      PULLING #IPL TWEETS                                               				  ##												
############################################################################################################################



#IMPORTING THE LIBRARIES REQUIRED
library(twitteR)
library(dplyr)
library("SnowballC")
library("tm")
library("syuzhet")
library(ggplot2)
library(data.table)


# CHANGE THE NEXT FOUR LINES BASED ON YOUR OWN CONSUMER_KEY, CONSUME_SECRET, ACCESS_TOKEN, AND ACCESS_SECRET. 
consumer_key <- "RnVrfcV3Bd9j0FeCeqDr48xnr"
consumer_secret <- "tDImNp07JZNSuIiM0rDY7NJqQvuK1D9ejOfXn0JTkJvww5LYLu"
access_token <- "2240235625-yHGNjLvPJ9pfhn59epEHDR1Me0OnxpiZ4nOzgxf"
access_secret <- "RE5JPTECJfXmNpsfs9ZjLTPxI4PqadkyPy11TprytDCSW"

#SETTING UP ACCESS TP TWITTER
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)



#BELOW CODE PULLS 1000 TWEETS PER DAY FROM 25 APR TO 02 MAY 2018 WITH # IPL AND LANAGAUGE - ENGLISH
tw_1 = twitteR::searchTwitter('#IPL', n = 1000,lang="en",since = '2019-04-25',until='2019-04-26')
tw_2 = twitteR::searchTwitter('#IPL', n = 1000,lang="en",since = '2019-04-26',until='2019-04-27')
tw_3 = twitteR::searchTwitter('#IPL', n = 1000,lang="en",since = '2019-04-27',until='2019-04-28')
tw_4 = twitteR::searchTwitter('#IPL', n = 1000,lang="en",since = '2019-04-28',until='2019-04-29')
tw_5 = twitteR::searchTwitter('#IPL', n = 1000,lang="en",since = '2019-04-29',until='2019-04-30')
tw_6 = twitteR::searchTwitter('#IPL', n = 1000,lang="en",since = '2019-04-30',until='2019-05-01')
tw_7 = twitteR::searchTwitter('#IPL', n = 1000,lang="en",since = '2019-05-01',until='2019-05-02')

#APPENDING THE ABOVE 7 LISTS AND CONVERTING IN TO DATAFRAME
tw_n<-append(tw_1,tw_2)
tw_n<-append(tw_n,tw_3)
tw_n<-append(tw_n,tw_4)
tw_n<-append(tw_n,tw_5)
tw_n<-append(tw_n,tw_6)
tw_n<-append(tw_n,tw_7)

df<- twListToDF(tw_n)


#WRITING THE ABOVE DATAFRAME AS A CSV FILE AND STORING IT IN OUR LOCAL SYSTEM
setwd("C:/Users/karth/Documents/Rutgers/Course Materials/Spring 2019/Data Wrangling/Project/Data")
write.csv(df,"tweet.csv")

############################################################################################################################
##                                      PULLING #mankad TWEETS                                               				  ##												
############################################################################################################################


#BELOW CODE PULLS 1000 TWEETS SINCE MAR 25, 2019 WITH HASTAG - #mankad AND LANAGAUGE - ENGLISH

#tw_mankad = twitteR::searchTwitter('#mankad + #IPL', n = 000,lang="en",since = '2019-03-25')
tw_mankad = twitteR::searchTwitter('mankad', n =1000,lang="en",since = '2019-03-25')
df_mankad<- twListToDF(tw_mankad)

#WRITING THE ABOVE DATAFRAME AS A CSV FILE AND STORING IT IN OUR LOCAL SYSTEM
setwd("C:/Users/karth/Documents/Rutgers/Course Materials/Spring 2019/Data Wrangling/Project/Data")
write.csv(df_mankad,"mankad.csv")






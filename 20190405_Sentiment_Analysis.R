

############################################################################################################################
##                                  SENTIMENT ANALYSIS OF IPL DATA                                                        ##

## This code does the sentiment analysis of the tweets.csv and the manakad. csv file and plots the results                ##
												 
############################################################################################################################




############################################################################################################################
##                                PART -1 SENTIMENT ANALYSIS OF #IPL TWEETS                                               ##												
############################################################################################################################




#IMPORTING THE LIBRARIES REQUIRED
library(stringr)
library(twitteR)
library(dplyr)
library("SnowballC")
library("tm")
library("syuzhet")
library(ggplot2)
library(data.table)

#SET THE WORKING DIRECTORY AND LOADING THE TWEET.CSV
setwd("C:/Users/karth/Documents/Rutgers/Course Materials/Spring 2019/Data Wrangling/Project")
tweets<-read.csv("tweet.csv",stringsAsFactors = FALSE)


#CLEAN UP ANY DUPLICATE TWEETS FROM THE DATA FRAME USING #DPLYR::DISTINCT
tweets<-distinct(tweets)
#tweets$text<-str(tweets$text)

#CLEAN SPECIAL CHARACTERS FROM THE TWEET TEXT
tweets.df <- gsub("http.*","",tweets$text)
tweets.df <- gsub("http.*","",tweets.df)
tweets.df <- gsub("http.*","",tweets.df)
tweets.df <- gsub("http.*","",tweets.df)
tweets$text=tweets.df


#FINDING THE MOST COMMON WORDS IN THE TWEET TEXT - USING DPLYR AND FILTERING OUT STOP WORDS AND SOME COMON WORDS
Words <- tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%  filter(!word %in% c("rt", "ipl","ipl2019","match","0001f49c","season","20e3","0001f64c",
                                                 "wins","win","6","4",
                                                 "0001f981","vivoipl","overs","runs","balls","game","special","team")) 

#PLOTTING THE MOST COMMON WORDS IN THE TWEET TEXT - USING DPLYR AND FILTERING OUT STOP WORDS AND SOME COMON WORDS
Words %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")


#JOIN SENTIMENT CLASSIFICATION TO THE TWEET WORDS
Senti_word_counts <- Words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#PLOTTING THE SENTIMENT CLASSIFICATION TO THE TWEET WORDS
Senti_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Positive VS Negative Sentiment - Words Contribution",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()



#GETTING THE SENTIMENT SCORE RESULTS
tweets.df <- as.vector(tweets.df)
emotion.df <- get_nrc_sentiment(tweets.df)
results <- cbind(tweets.df, emotion.df) 
head(results)

#PLOTTING THE OVERALL SENTIMENTS - EMOTIONS AND THE POSITIVE VS NEGATIVE
overall=t(emotion.df)
overall <- data.frame(rowSums(overall))
names(overall)[1] <- "count"
overall <- cbind("sentiment" = rownames(overall), overall)
rownames(overall) <- NULL
qplot(sentiment, data=overall[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("Overall Sentiments")
qplot(sentiment, data=overall[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Overall Sentiments")


#TEAM WISE TWEETS COUNT - FOR CSK
results_CSK=results[results$tweets.df %like% "CSK", -1]
results_CSK <- data.frame(colMeans(results_CSK))
results_CSK=as.data.frame(t(results_CSK))
results_CSK$team="CSK"
results_teamwise<-results_CSK


#FUNCTION TO APPEND TEAM WISE TWEETS COUNT - FOR OTHER TEAMS
teamwise_score= function(team){
  print(team)
  results_t=results[results$tweets.df %like% team, -1]
  results_t <- data.frame(colMeans(results_t))
  results_t=as.data.frame(t(results_t))
  results_t$team=team
  return (results_t)
}

results_teamwise<-rbind(results_teamwise,teamwise_score("DC"))
results_teamwise<-rbind(results_teamwise,teamwise_score("MI"))
results_teamwise<-rbind(results_teamwise,teamwise_score("SRH"))
results_teamwise<-rbind(results_teamwise,teamwise_score("KKR"))
results_teamwise<-rbind(results_teamwise,teamwise_score("RR"))
results_teamwise<-rbind(results_teamwise,teamwise_score("RCB"))
results_teamwise<-rbind(results_teamwise,teamwise_score("KXIP"))
rownames(results_teamwise)<-NULL


#PLOTS FOR THE TEAM WISE SENTIMENT ANALYSIS - 8 EMOTIONS AND POSTIVE VS NEGATIVE OF NRC LEXICON
qplot(team, data=results_teamwise, weight=positive, geom="bar",fill=team)+ggtitle("Teamwise - Positive Sentiments")
qplot(team, data=results_teamwise, weight=negative, geom="bar",fill=team)+ggtitle("Teamwise - Negative Sentiments")
qplot(team, data=results_teamwise, weight=trust, geom="bar",fill=team)+ggtitle("Teamwise - Trust")
qplot(team, data=results_teamwise, weight=anticipation, geom="bar",fill=team)+ggtitle("Teamwise - Anticipation")
qplot(team, data=results_teamwise, weight=anger, geom="bar",fill=team)+ggtitle("Teamwise - Anger")
qplot(team, data=results_teamwise, weight=disgust, geom="bar",fill=team)+ggtitle("Teamwise - Disgust")
qplot(team, data=results_teamwise, weight=surprise, geom="bar",fill=team)+ggtitle("Teamwise - Surprise")
qplot(team, data=results_teamwise, weight=sadness, geom="bar",fill=team)+ggtitle("Teamwise - Sadness")
qplot(team, data=results_teamwise, weight=joy, geom="bar",fill=team)+ggtitle("Teamwise - Joy")
qplot(team, data=results_teamwise, weight=fear, geom="bar",fill=team)+ggtitle("Teamwise - Fear")


############################################################################################################################
##                                PART -2 SENTIMENT ANALYSIS OF MANKADING RELATED TWEETS                                  ##												
############################################################################################################################


#SET THE WORKING DIRECTORY AND LOADING THE MANKAD.CSV AND TAKING ONLY THE NECESSARY COLUMNS
setwd("C:/Users/karth/Documents/Rutgers/Course Materials/Spring 2019/Data Wrangling/Project")
tweets_mankad<-read.csv("mankad.csv",stringsAsFactors = FALSE)
tweets_mankad=tweets_mankad[,1:2]

# FUNCTION TO GET VARIOUS SENTIMENT SCORES, USING THE SYUZHET PACKAGE - DIFFERENT METHODS ARE USED
scoreSentiment = function(tab)
{
  tab$syuzhet = get_sentiment(tab$text, method="syuzhet")
  tab$bing = get_sentiment(tab$text, method="bing")
  tab$afinn = get_sentiment(tab$text, method="afinn")
  tab$nrc = get_sentiment(tab$text, method="nrc")
  emotions = get_nrc_sentiment(tab$text)
  n = names(emotions)
  for (nn in n) tab[, nn] = emotions[nn]
  return(tab)
}

# GET THE SENTIMENT SCORES FOR THE TWEETS USING THE ABOVE FUNCTION
tweets_mankad = scoreSentiment(tweets_mankad)

#CLEANING THE TWEET TEXT DATA BY REMOVING ALL SPECIAL CHARACTERS 
tweets.mankad <- gsub("http.*","",tweets_mankad$text)
tweets.mankad <- gsub("http.*","",tweets.mankad)
tweets.mankad <- gsub("http.*","",tweets.mankad)
tweets.mankad <- gsub("http.*","",tweets.mankad)
tweets_mankad$text=tweets.mankad

#FINDING THE MOST COMMON WORDS IN THE TWEET TEXT------- FOR MANKADING AFTER REMOVING STOP WORDS AND SOME MOST COMMON WORDS
Words_mankad <- tweets_mankad %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%  filter(!word %in% c("rt", "ipl","ipl2019","match","0001f49c","season","20e3","0001f64c",
                                                 "wins","win","6","4",
                                                 "0001f981","vivoipl","overs","runs","balls","game","special","team")) 

#PLOTTING THE MOST COMMON WORDS IN THE TWEET TEXT------- FOR MANKADING AFTER REMOVING STOP WORDS AND SOME MOST COMMON WORDS
Words_mankad %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")


#JOIN SENTIMENT CLASSIFICATION TO THE TWEET WORDS
Senti_word_counts_man <- Words_mankad %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
## Joining, by = "word"

#PLOTTING THE SENTIMENT CLASSIFICATION TO THE TWEET WORDS
Senti_word_counts_man %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Positive VS Negative Sentiment - Words Contribution",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()



#PLOTTING NRC ALGO SENTIMENT RESULTS - EMOTION WISE AND POSITIVE VS NEGATIVE
mankad_nrc_results<-tweets_mankad[,7:16]
mankad_nrc_results=t(mankad_nrc_results)
mankad_nrc_results <- data.frame(rowSums(mankad_nrc_results))
names(mankad_nrc_results)[1] <- "count"
mankad_nrc_results <- cbind("sentiment" = rownames(mankad_nrc_results), mankad_nrc_results)
rownames(mankad_nrc_results) <- NULL
qplot(sentiment, data=mankad_nrc_results[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("Overall Sentiments")
qplot(sentiment, data=mankad_nrc_results[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Overall Sentiments")


# COMPARING RESULTS OF OTHER ALGORITHMS
mankad_other_results<-tweets_mankad[,3:6]
mankad_other_results=t(mankad_other_results)
mankad_other_results <- data.frame(rowMeans(mankad_other_results))
names(mankad_other_results)[1] <- "Mean"
mankad_other_results <- cbind("sentiment" = rownames(mankad_other_results), mankad_other_results)
rownames(mankad_other_results) <- NULL

# PLOT TO COMPARE RESULTS OF VARIOUS ALGORITHMS - AVERAGE SENTIMENT SCORE
qplot(sentiment, data=mankad_other_results[1:4,], weight=Mean, geom="bar",fill=sentiment)+ggtitle("Average Sentiments across Methods")


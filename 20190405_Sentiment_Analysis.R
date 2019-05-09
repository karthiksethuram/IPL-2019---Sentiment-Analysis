library(stringr)



setwd("C:/Users/karth/Documents/Rutgers/Course Materials/Spring 2019/Data Wrangling/Project")
tweets<-read.csv("tweet.csv",stringsAsFactors = FALSE)

#clean up any duplicate tweets from the data frame using #dplyr::distinct
tweets<-distinct(tweets)
#tweets$text<-str(tweets$text)

tweets.df <- gsub("http.*","",tweets$text)
tweets.df <- gsub("http.*","",tweets.df)
tweets.df <- gsub("http.*","",tweets.df)
tweets.df <- gsub("http.*","",tweets.df)
tweets$text=tweets.df


#FINDING THE MOST COMMON WORDS IN THE TWEET TEXT
Words <- tweets %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%  filter(!word %in% c("rt", "ipl","ipl2019","match","0001f49c","season","20e3","0001f64c",
                                                 "wins","win","6","4",
                                                 "0001f981","vivoipl","overs","runs","balls","game","special","team")) 


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


# join sentiment classification to the tweet words
Senti_word_counts <- Words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
## Joining, by = "word"

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

## Selecting by n


#GETTING THE SENTIMENT SCORE RESULTS
tweets.df <- as.vector(tweets.df)
emotion.df <- get_nrc_sentiment(tweets.df)
results <- cbind(tweets.df, emotion.df) 
head(results)

#Plotting the overall Sentiments
overall=t(emotion.df)
overall <- data.frame(rowSums(overall))
names(overall)[1] <- "count"
overall <- cbind("sentiment" = rownames(overall), overall)
rownames(overall) <- NULL
qplot(sentiment, data=overall[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("Overall Sentiments")
qplot(sentiment, data=overall[9:10,], weight=count, geom="bar",fill=sentiment)+ggtitle("Overall Sentiments")


#Team wise tweets count - for CSK
results_CSK=results[results$tweets.df %like% "CSK", -1]
results_CSK <- data.frame(colSums(results_CSK))
results_CSK=as.data.frame(t(results_CSK))
results_CSK$team="CSK"
results_teamwise<-results_CSK


#Team wise tweets count - for Other Teams
teamwise_score= function(team){
  print(team)
  results_t=results[results$tweets.df %like% team, -1]
  results_t <- data.frame(colSums(results_t))
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


#########################################Mankading Incident##########################################################

setwd("C:/Users/karth/Documents/Rutgers/Course Materials/Spring 2019/Data Wrangling/Project")
tweets_mankad<-read.csv("mankad.csv",stringsAsFactors = FALSE)

tweets_mankad=tweets_mankad[,1:2]

# function to get various sentiment scores, using the syuzhet package
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

# get the sentiment scores for the tweets
tweets_mankad = scoreSentiment(tweets_mankad)




#FINDING THE MOST COMMON WORDS IN THE TWEET TEXT------- FOR MANKADING


tweets.mankad <- gsub("http.*","",tweets_mankad$text)
tweets.mankad <- gsub("http.*","",tweets.mankad)
tweets.mankad <- gsub("http.*","",tweets.mankad)
tweets.mankad <- gsub("http.*","",tweets.mankad)
tweets_mankad$text=tweets.mankad


Words_mankad <- tweets_mankad %>%
  dplyr::select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%  filter(!word %in% c("rt", "ipl","ipl2019","match","0001f49c","season","20e3","0001f64c",
                                                 "wins","win","6","4",
                                                 "0001f981","vivoipl","overs","runs","balls","game","special","team")) 


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


# join sentiment classification to the tweet words
Senti_word_counts_man <- Words_mankad %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
## Joining, by = "word"

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





#Plotting NRC algo Sentiment results
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
qplot(sentiment, data=mankad_other_results[1:4,], weight=Mean, geom="bar",fill=sentiment)+ggtitle("Average Sentiments across Methods")


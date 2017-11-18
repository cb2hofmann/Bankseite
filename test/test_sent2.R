require(ggplot2)
library(tidyr)
library(tidytext)
library(dplyr)
library(lubridate)

splittedDataframe1<-read.csv("C:/Users/Christian/Documents/textmining/R-projekt/BeckerSeminar2/Testing/Daten2012usa.csv")
datan3<- data.frame(splittedDataframe1)
datan3$Tweets<-as.character(datan3$Tweets)
tidy_daten2012_word <- datan3 %>% unnest_tokens(word, Tweets)
class(tidy_daten2012_word$Month)
#entferne stopwords
tidy_2012_ohne_stopwords <- tidy_daten2012_word %>% anti_join(stop_words)
#join bing

nrcjoy <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

#zähle positive wörter
tidy_2012_ohne_stopwords  %>%
  inner_join(nrcjoy) %>%
  count(sentiment, sort = TRUE)
a<-tidy_2012_ohne_stopwords[grep("Jan",tidy_2012_ohne_stopwords),]<-
#nach month gruppieren und spalte erzeugen positive -negativ
datplot<-tidy_2012_ohne_stopwords  %>%
  inner_join(nrcjoy) %>%
  group_by(Month)%>%
count(sentiment) %>%
  as.Date(t)
  spread(sentiment, n)%>%
mutate(sentiment = positive - negative)
class(post_u_nega$Month)
ymd(post_u_nega$Month,format="%b")
a<-as.Date(post_u_nega$Month)
strptime(post_u_nega$Month,format="%b")
#nach month gruppieren und zählen positive negative
post_u_nega<-tidy_2012_ohne_stopwords  %>%
  inner_join(nrcjoy) %>%
  group_by(Month)%>%
  count(sentiment) 

ggplot(post_u_nega, aes(x = Month, fill = sentiment)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~sentiment, ncol = 23)

ggplot(data=post_u_nega, aes(Month)) + geom_histogram(bins=30)+
  facet_wrap(~sentiment, ncol = 2, scales = "free_x")


ggplot(data=datplot, aes(x=sentiment, y=n)) +
  geom_bar(stat="identity")
datum <- strptime(tidy_2012_ohne_stopwords$Month,format="%b")
datplot<-tidy_2012_ohne_stopwords  %>%
  inner_join(nrcjoy) %>%
  group_by(Month)%>%
  count(sentiment) 

ggplot(data=datplot, aes(x=Month, y=n, fill=sentiment)) + geom_col(show.legend = FALSE)+
  geom_bar(stat="identity") + facet_wrap(~sentiment, ncol = 2, scales = "free_x")


ggplot(datplot, aes(Month, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, ncol = 2, scales = "free_x")
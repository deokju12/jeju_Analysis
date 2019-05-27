
library(KoNLP)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(KoNLP)

place<-read.table("koReviw_place.txt")
place<-table(place)


wordcloud( words = names(place), freq = place,
           min.freq = 2, max.words = 200,
           random.order = FALSE, rot.per = 0.1,
           scale= c(5,0.3),
           colors = brewer.pal(8, "Dark2"))

dtm <-place
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col ="lightblue", 
        main ="jeju_place Best10 words(ko)", ylab = "Word frequencies")

place_en<-read.table("enReview_place.txt")
place_en<-table(place_en)


wordcloud( words = names(place_en), freq = place_en,
           min.freq = 2, max.words = 200,
           random.order = FALSE, rot.per = 0.1,
           scale= c(5,0.3),
           colors = brewer.pal(8, "Dark2"))

dtm <-place_en
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col ="lightblue", main ="jeju_place Best10 words(en)", ylab = "Word frequencies")

food<-read.table("koReview_food.txt")
food<-table(food)
set.seed(1004)
s
wordcloud( words = names(food), freq = food,
           min.freq = 2, max.words = 200,
           random.order = FALSE, rot.per = 0.1,
           scale= c(5,0.3),
           colors = brewer.pal(8, "Dark2"))
dtm <-food
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col ="lightblue", main ="jeju_restaurents Best10 words(ko)", ylab = "Word frequencies")


food_en<-read.table("enReview_food.txt")
food_en<-table(food_en)
set.seed(1004)

wordcloud( words = names(food_en), freq = food_en,
           min.freq = 2, max.words = 200,
           random.order = FALSE, rot.per = 0.1,
           scale= c(5,0.3),
           colors = brewer.pal(8, "Dark2"))
dtm <-food_en
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col ="lightblue", main ="jeju_restaurents Best10 words(en)", ylab = "Word frequencies")


language<-c("en","en","ko","ko")
sentiment<-c("positive","negative","positive","negative")
score<-c(2880,669,1390,228)
foodResult<-data.frame(language,sentiment,score)
library(ggplot2)
ggplot(foodResult, aes(x=language, y=score, fill=sentiment)) + 
  geom_bar(stat="identity", position="fill", colour="black") +  
  scale_fill_brewer(palette=1) + 
  ggtitle("Compare Restaurent Review")

language<-c("en","en","ko","ko")
sentiment<-c("positive","negative","positive","negative")
score<- c(923,355,138,49)
placeResult<-data.frame(language,sentiment,score)
placeResult

library(ggplot2)
ggplot(placeResult, aes(x=language, y=score, fill=sentiment)) + 
  geom_bar(stat="identity", position="fill", colour="black") +  
  scale_fill_brewer(palette=1) + 
  ggtitle("Compare Place Review")

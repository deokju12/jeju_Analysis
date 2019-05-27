url <- "https://github.com/park1200656/KnuSentiLex/blob/master/SentiWord_Dict.txt"
download.file(url, "SentiWord_Dict.html", mode="w")
setwd("C:/Users/deokju.DESKTOP-SSNCOJV/Desktop/presentation")
KO_lexicon <- readLines("SentiWord_Dict.html")
Encoding(KO_lexicon) <- "UTF-8"
KO_lexicon
require(RCurl)
require(XML)
html.parsed <-htmlParse(KO_lexicon)
class(html.parsed)
lexicon2 <- xpathSApply(html.parsed,"//td[@class='blob-code blob-code-inner js-file-line']",xmlValue)
lexicon2
lexicon2 <- gsub("\n", "",lexicon2)
lexicon2 <- strsplit(lexicon2,"\t")

#lexicon2<- data.franame =lexicon2[1], score=lexicon2[2],stringsAsFactors = FALSE)

lexicon2<-do.call(rbind,lexicon2)
?do.call
text<-lexicon2[,1]
score<-lexicon2[,2]
score <- as.numeric(score)
lexicon2 <- data.frame(text=text,score=score, stringsAsFactors = FALSE)

install.packages("car")
lexicon2 <- cbind(sentiment = "a", lexicon2, stringsAsFactors=FALSE)
lexicon2

library(car)


for (i in 1:nrow(lexicon2))
{
  if(lexicon2$score[i]>=1){lexicon2$sentiment[i] <- recode(lexicon2$sentiment[i], "'a'='positive'")} 
  else if(lexicon2$score[i]==0){lexicon2$sentiment[i] <- recode(lexicon2$sentiment[i], "'a'='neutral'")}
  else {lexicon2$sentiment[i] <- recode(lexicon2$sentiment[i], "'a'='negative'")}
}

lexicon2

write.table(lexicon2, "lexicontxt.txt", sep="")


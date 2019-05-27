# ÇÑ±¹¾î À½½Ä ¸®ºä »Ì±â
koReview_food<-function(baseurl, n=NULL ){
  library(RCurl)
  library(stringr)
  library(XML)
  library(bitops)
  
  
  html <-getURL(baseurl,ssl.verifypeer=FALSE,ssl.verifyhost=FALSE)
  html.parsed <- htmlParse(html)
  
  inner <- xpathSApply(html.parsed,"//div[@class='title']/a",xmlGetAttr,"href")
  innerurl <- paste0("https://www.tripadvisor.co.kr",inner)
  koReview.food <- data.frame()
  Sys.setlocale("LC_TIME","C")
  for(i in  innerurl){
    
    html2<-getURL(i,verifypeer=FALSE,ssl.verifyhost=FALSE)
    html2.parsed <- htmlParse(html2)
    nexturl <- xpathSApply(html2.parsed,"//div[@class='quote']/a",xmlGetAttr, "href")
    url <-paste0("https://www.tripadvisor.co.kr",nexturl[1])
    
    if(is.null(n)){
      html3 <-getURL(url,ssl.verifypeer=FALSE,ssl.verifyhost=FALSE)
      html3.parsed <- htmlParse(html3)
      total.pages <- xpathSApply(html3.parsed,"//a[@class='pageNum last taLnk ']",xmlValue)
      total.pages <- as.numeric(gsub(",","",total.pages))[1]
      n<- total.pages
    }
    
    
    for (i in c(1:n)) {
      if (i == 1) {url2<- url}
      else {
        
        nexturl2 <- xpathSApply(html4.parsed,"//a[@class='nav next taLnk ']",xmlGetAttr,"href")
        url2 <- paste0("https://www.tripadvisor.co.kr",nexturl2)
      }
      html4 <-getURL(url2,ssl.verifypeer=FALSE,ssl.verifyhost=FALSE)
      html4.parsed <- htmlParse(html4)
      text <- xpathSApply(html4.parsed, "//p[@class='partial_entry']",xmlValue)
      koReview <- data.frame(text =text,stringsAsFactors=FALSE)
      koReview.food <- rbind(koReview.food,koReview)
    }
  }
  Sys.setlocale()
  
  
  return(koReview.food)
}

baseurl <- "https://www.tripadvisor.co.kr/Restaurants-g983296-Jeju_Island.html"
koReview.food<- koReview_food(baseurl= baseurl, n=20 )

koReview.food
library(xlsx)

write.xlsx(koReview.food, file = "AllReview_food.xlsx",
           sheetName = "koreaFood", append = FALSE)
library(rJava)
library(KoNLP)
library(tidyverse)
vignette("KoNLP-API") 
useNIADic()
#¸®ºä ´Ü¾î·Î ºÐ¸®
text<-SimplePos09(koReview.food$text)
text<-str_match_all(text,"([°¡-ÆR]+)/[NPM]")
text<-sapply(text, function(x) x[,2][str_length(x[,2]) >=2])
text
text<-unlist(text)
write.xlsx(text, file = "AllReview_word_food.xlsx",
           sheetName = "koreaFood", append = FALSE)
write(text,"koReview_food.txt")
text<-data.frame(text= text, stringsAsFactors=FALSE)
text
#°¨Á¤»çÀü ºÒ·¯¿À±â
sentiment <- read.table("lexicontxt.txt")
sentiment
#merge
result<- merge(text, sentiment, by='text', all=FALSE)
result
#positive, negative Á¡¼ö
positive_score<-length(which(result$sentiment=="positive"))
negative_score<-length(which(result$sentiment=="negative"))
score<- positive_score - negative_score


resultscore<- data.frame(positive=positive_score,negative=negative_score,sentiment=score,stringsAsFactors = FALSE)
resultscore
#´Ü¾îº°ºóµµ¼ö
resultscore2<-table(result$text)
resultscore2<-data.frame(resultscore2)
names(resultscore2)<- c("text","n")
resultscore2<- merge(result, resultscore2,by='text', all=FALSE)
resultscore2<-subset(resultscore2, select=c("sentiment","text","n"))
resultscore2<-unique(resultscore2)
resultscore2<-filter(resultscore2,n>=5)
resultscore2



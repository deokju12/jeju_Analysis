# 영어 음식 리뷰
enReview_food<-function(baseurl, n=NULL ){
  library(RCurl)
  library(stringr)
  library(XML)
  
  
  
  html <-getURL(baseurl,ssl.verifypeer=FALSE,ssl.verifyhost=FALSE)
  html.parsed <- htmlParse(html)
  
  inner <- xpathSApply(html.parsed,"//div[@class='title']/a",xmlGetAttr,"href")
  innerurl <- paste0("https://www.tripadvisor.com",inner)
  enReview.food <- data.frame()
  Sys.setlocale("LC_TIME","C")
  for(i in  innerurl){
    
    html2<-getURL(i,verifypeer=FALSE,ssl.verifyhost=FALSE)
    html2.parsed <- htmlParse(html2)
    nexturl <- xpathSApply(html2.parsed,"//div[@class='quote']/a",xmlGetAttr, "href")
    url <-paste0("https://www.tripadvisor.com",nexturl[1])
    
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
        url2 <- paste0("https://www.tripadvisor.com",nexturl2)
      }
      html4 <-getURL(url2,ssl.verifypeer=FALSE,ssl.verifyhost=FALSE)
      html4.parsed <- htmlParse(html4)
      text <- xpathSApply(html4.parsed, "//p[@class='partial_entry']",xmlValue)
      enReview <- data.frame(text =text,stringsAsFactors=FALSE)
      enReview.food <- rbind(enReview.food,enReview)
    }
  }
  Sys.setlocale()
  
  
  return(enReview.food)
}

baseurl <- "https://www.tripadvisor.com/Restaurants-g983296-Jeju_Island.html"
enReview.food<- enReview_food(baseurl= baseurl, n=20 )
enReview.food
library(xlsx)

write.xlsx(enReview.food, file = "AllReview_food_en.xlsx",
           sheetName = "Englishfood", append = FALSE)

library(tidytext)
bing <-get_sentiments(lexicon = "bing")
bing
library(tidyverse)
enReview.food.words <- unnest_tokens(tbl = enReview.food, output=word, input=text, token="words") %>%
  anti_join(stop_words, by="word") %>%
  
  as_tibble(.)
enReview.food.words
write.xlsx(enReview.food.words, file = "AllReview_word_food_en.xlsx",
           sheetName = "EnglisFood", append = FALSE)
write.table(enReview.food.words,"enReview_food.txt")
enReview.food.sent<- enReview.food.words %>%
  inner_join(bing,by="word")%>%
  count(sentiment) %>%
  spread(sentiment, n, fill=0)%>%
  mutate(sentiment= positive- negative)%>% 
  ungroup()
enReview.food.sent

enReview.food.sent2 <- enReview.food.words %>%
  inner_join(bing,by="word")%>%
  count(sentiment,word) %>%
  ungroup()%>%
  mutate(nsign= ifelse(sentiment =="negtive", -n, n))
enReview.place.sent2<- filter(enReview.food.sent2, n>=5)
enReview.place.sent2

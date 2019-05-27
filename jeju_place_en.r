# ¿µ¾î Àå¼Ò ¸®ºä»Ì±â
enReview_place<-function(baseurl, n=NULL ){
  library(RCurl)
  library(stringr)
  library(XML)
  library(bitops)
  
  
  html <-getURL(baseurl,ssl.verifypeer=FALSE,ssl.verifyhost=FALSE)
  html.parsed <- htmlParse(html)
  
  inner <- xpathSApply(html.parsed,"//div[@class='listing_title ']/a",xmlGetAttr,"href")
  innerurl <- paste0("https://www.tripadvisor.com",inner)
  enReview.place <- data.frame()
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
      enReview.place <- rbind(enReview.place,enReview)
    }
  }
  Sys.setlocale()
  
  
  return(enReview.place)
}

baseurl <- "https://www.tripadvisor.com/Attractions-g983296-Activities-Jeju_Island.html"
enReview.place<- enReview_place(baseurl= baseurl ,n=20)

enReview.place
library(xlsx)

write.xlsx(enReview.place, file = "AllReview_En.xlsx",
           sheetName = "EnglishPlace", append = FALSE)
library(tidytext)
bing <-get_sentiments(lexicon = "bing")
bing
library(tidyverse)
enReview.place.words <- unnest_tokens(tbl = enReview.place, output=word, input=text, token="words") %>%
  anti_join(stop_words, by="word") %>%
  
  as_tibble(.)
enReview.place.words
write.xlsx(enReview.place.words, file = "AllReview_word_En.xlsx",
           sheetName = "EnglishPlace", append = FALSE)
write.table(enReview.place.words,"enReview_place.txt", sep="")
enReview.place.sent<- enReview.place.words %>%
  inner_join(bing,by="word")%>%
  count(sentiment) %>%
  spread(sentiment, n, fill=0)%>%
  mutate(sentiment= positive- negative)%>% 
  ungroup()
enReview.place.sent

enReview.place.sent2 <- enReview.place.words %>%
  inner_join(bing,by="word")%>%
  count(sentiment,word) %>%
  ungroup()%>%
  mutate(nsign= ifelse(sentiment =="negtive", -n, n))
enReview.place.sent2<- filter(enReview.place.sent2, n>=5)
enReview.place.sent2

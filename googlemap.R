install.packages("ggmap")
gapi.key <- "your key"
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
library(ggmap)
library(devtools)
library(ggplot2)
register_google(key = gapi.key) 


jeju.attraction <- read.csv("tripadv.place.csv", stringsAsFactors = F)

jeju.attraction <- head(jeju.attraction, n=70)

jeju.attraction$place <- enc2utf8(jeju.attraction$place)

jeju_attraction <- mutate_geocode(jeju.attraction, place, source = 'google')
jeju_attraction

map.data <- get_googlemap(center=c(126.542671,33.364805), zoom=10, maptype="roadmap")
map.data
ggmap(map.data) +
  geom_point(data = jeju_attraction, aes(lon, lat), size = 2, colour='red', 
             position = "jitter", alpha = .6) +
  ggtitle("Jeju Island Attraction")



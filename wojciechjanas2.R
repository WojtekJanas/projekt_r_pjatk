install.packages("httr")
install.packages("jsonlite")

library(httr)
library(jsonlite)

endpoint<-"https://www.omdbapi.com/?apikey=e380c2d6&type=movie&t=12+Angry+Men&y=1957&plot=full&r=json"
getshortlist<-GET(endpoint)
shortlistText<-content(getshortlist,"text")
View(shortlistText)
shortlistJson<-fromJSON(shortlistText, flatten = TRUE)
shortlistDF<-as.data.frame(shortlistJson)
View(shortlistDF)
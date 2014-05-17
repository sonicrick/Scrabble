### clean up rating.csv as scraped by Python

setwd("~/GitHub/Scrabble")

RatingsFull <- read.csv("rating.csv",stringsAsFactors=FALSE)
RatingsFull <- subset(RatingsFull, !(is.na(Rating))) #remove all unrated tourneys

RatingsFull <- within(RatingsFull, {
  Date <- strptime(Date, "%d %b %Y")
  Division <- gsub(" of", "", Division)
  Placement <- gsub("[a-z ]*","", Placement)
  Wins <- gsub("½", ".5", Wins)  
  GamesFirst <- gsub(" -", "", GamesFirst)
  #separate norm from norm progress
  NormProgress <- gsub("[A-Z]+", "", Norm)
  Norm <- gsub("\\*+", "", Norm)
    })

RatingsFull <- RatingsFull[order(RatingsFull$Date),]

write.csv(RatingsFull, file="cleanedrating.csv", row.names=FALSE)
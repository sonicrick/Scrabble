library(plyr)

setwd("~/GitHub/Scrabble/Nationals 2014")
touname <- "NATL2014B.TOU"
tourney <- "Nationals 2014 B"
statsfile <- paste(tourney, "stats.csv")
bingofile <- paste(tourney, "bingos.csv")
playerfile <- paste(tourney, "participants.csv")

source('~/GitHub/Scrabble/readTou.R')
tou <- readTou(touname)

tiles <- read.csv(statsfile, stringsAsFactors=FALSE)
bingos <- read.csv(bingofile, stringsAsFactors=FALSE)
names(tiles) <- tolower(names(tiles))
names(bingos) <- tolower(names(bingos))
bingos <- bingos[nchar(bingos$bingo)>3, ]  # eliminate NAs, _NAs, etc
bingos$phoney <- grepl("\\*", bingos$bingo)
bingos$bingo <- gsub("\\*", "", bingos$bingo)
bingos$length <- nchar(bingos$bingo)
b.sum <- ddply(bingos, .(player, round), summarise,
                   bingo.count = length(bingo),
                   bingo.points = sum(score, na.rm=TRUE))

#standardize names
#e.g.
#tou$games$player[tou$games$player == "Hubert Wee Ming Hui"] <- "Hubert Wee"
#levels(tou$games$opp)[levels(tou$games$opp)=="Hubert Wee Ming Hui"] <- "Hubert Wee"

tiles$power.tiles <- with(tiles, blank+s+j+q+x+z)
stats <- join(tou$games, tiles, by=c("player", "round"))
stats <- join(stats, b.sum, by=c("player", "round"))
#replace NAs with 0s when there are no bingos
stats$bingo.count[is.na(stats$bingo.count)] <- 0
stats$bingo.points[is.na(stats$bingo.points)] <- 0

source('~/GitHub/Scrabble/bingoPlayability.R')
bingos <- bingoPlayability(bingos)
####
#manual append for bingos longer than 8 letter, probability first then playability
####
longbing<- c("AEROLITES", 16, 8)
dim(longbing) <- c(3, length(longbing)/3)
lbdf <- data.frame(bingo=as.character(longbing[1,]),
                   probability=as.numeric(longbing[2,]),
                   playability=as.numeric(longbing[3,]))
names(lbdf) <- c("bingo", "probability", "playability")
for (lb in 1:nrow(lbdf)) {
  idx <- bingos$bingo == lbdf$bingo[lb]
  bingos$probability[idx] <- as.numeric(lbdf$probability[lb])
  bingos$playability[idx] <- as.numeric(lbdf$playability[lb])
}

#remove byes
stats <- stats[stats$bye=="N",]

ratings <- read.csv(playerfile, stringsAsFactors=FALSE)
#ratings <- ratings[, -c(4,5)] #remove SA and MSA name
names(ratings)[names(ratings)=="display.name"] <- "player"

b.indiv <- ddply(bingos, .(player), summarise, bingos=length(bingo),
                     bingo.ave.score=mean(score, na.rm=TRUE))
#replace NAs with 0s when there are no bingos
b.indiv$bingos[is.na(b.indiv$bingos)] <- 0
b.indiv$bingo.ave.score[is.na(b.indiv$bingo.ave.score)] <- 0

indiv <- ddply(stats, .(player), summarise,
                   vp=sum(vp, na.rm=TRUE),
                   spread=sum(margin, na.rm=TRUE),
                   ave.score=mean(score, na.rm=TRUE),
                   ave.opp.score=mean(opp.score, na.rm=TRUE),
                   ave.spread=mean(margin, na.rm=TRUE),
                   j=sum(j, na.rm=TRUE), q=sum(q, na.rm=TRUE),
                   x=sum(x, na.rm=TRUE), z=sum(z, na.rm=TRUE),
                   blank=sum(blank, na.rm=TRUE), s=sum(s, na.rm=TRUE),
                   power.tiles=sum(power.tiles, na.rm=TRUE),
                   overtime=sum(overtime=="Y", na.rm=TRUE),
                   seconds.left=mean((minute*60 +second)*(overtime!="Y"), na.rm=TRUE)
                   )

indiv <- join(indiv, b.indiv, by="player")
indiv <- join(indiv, ratings, by="player")

write.csv(indiv, file=paste(tourney, "individual stats summary.csv"), row.names=FALSE)
write.csv(stats, file=paste(tourney, "game stats.csv"), row.names=FALSE)
write.csv(bingos, file=paste(tourney, "bingo records.csv"), row.names=FALSE)
write.csv(b.indiv, file=paste(tourney, "bingo stats summary.csv"), row.names=FALSE)

#vppred <- lm(vp ~ blank+s+j+q+x+z+overtime+seconds.left+bingos+bingo.ave.score
#             +exp.wespa.win, data=indiv)

#gamepred <- lm(vp ~ blank+s+j+q+x+z+overtime+minute+bingo.count+I(bingo.points/bingo.count),
#               data=stats)

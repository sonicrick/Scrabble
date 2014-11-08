setwd("~/GitHub/Scrabble/Causeway 2014")
touname <- "CWAY2014.TOU"

source('~/GitHub/Scrabble/readTou.R')
csc.tou <- readTou(touname)

csc.tiles <- read.csv("CSC14 stats.csv", stringsAsFactors=FALSE)
csc.bingos <- read.csv("CSC14 Bingos.csv", stringsAsFactors=FALSE)
names(csc.tiles) <- tolower(names(csc.tiles))
names(csc.bingos) <- tolower(names(csc.bingos))
csc.bingos$phoney <- grepl("\\*", csc.bingos$bingo)
csc.bingos$bingo <- gsub("\\*", "", csc.bingos$bingo)
csc.bingos$length <- nchar(csc.bingos$bingo)
csc.b.sum <- ddply(csc.bingos, .(player, round), summarise,
                   bingo.count = length(bingo),
                   bingo.points = sum(score, na.rm=TRUE))

#standardize names
csc.tou$games$player[csc.tou$games$player == "Hubert Wee Ming Hui"] <- "Hubert Wee"
levels(csc.tou$games$opp)[levels(csc.tou$games$opp)=="Hubert Wee Ming Hui"] <- "Hubert Wee"

csc.tiles$power.tiles <- with(csc.tiles, blank+s+j+q+x+z)
csc <- join(csc.tou$games, csc.tiles, by=c("player", "round"))
csc <- join(csc, csc.b.sum, by=c("player", "round"))
#replace NAs with 0s when there are no bingos
csc$bingo.count[is.na(csc$bingo.count)] <- 0
csc$bingo.points[is.na(csc$bingo.points)] <- 0

source('~/GitHub/Scrabble/bingoPlayability.R')
csc.bingos <- bingoPlayability(csc.bingos)
#manual append for bingos longer than 8 letter
longbing<- c("PLATINISE", 1864, 3018,
             "RESORTING", 1333, 9875,
             "ANABIOSIS", 17519, 15037,
             "MULTiPART", 21225, 26272,
             "DEVASTATED", 19723, 7729,
             "UNBROiLED", 2032, 14332)
dim(longbing) <- c(3, 6)
lbdf <- data.frame(bingo=as.character(longbing[1,]),
                   probability=as.numeric(longbing[2,]),
                   playability=as.numeric(longbing[3,]))
names(lbdf) <- c("bingo", "probability", "playability")
for (lb in 1:nrow(lbdf)) {
  idx <- csc.bingos$bingo == lbdf$bingo[lb]
  csc.bingos$probability[idx] <- as.numeric(lbdf$probability[lb])
  csc.bingos$playability[idx] <- as.numeric(lbdf$playability[lb])
}

#remove byes
csc <- csc[csc$bye=="N",]

csc.roster <- read.csv("causeway 2014 participants.csv", stringsAsFactors=FALSE)
csc.roster <- csc.roster[, -c(4,5)] #remove SA and MSA name
names(csc.roster)[names(csc.roster)=="wespa.name"] <- "player"

csc.b.indiv <- ddply(csc.bingos, .(player), summarise, bingos=length(bingo),
                     bingo.ave.score=mean(score, na.rm=TRUE))
#replace NAs with 0s when there are no bingos
csc.b.indiv$bingos[is.na(csc.b.indiv$bingos)] <- 0
csc.b.indiv$bingo.ave.score[is.na(csc.b.indiv$bingo.ave.score)] <- 0

csc.indiv <- ddply(csc, .(player), summarise,
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

csc.indiv <- join(csc.indiv, csc.b.indiv, by="player")
csc.indiv <- join(csc.indiv, csc.roster, by="player")

write.csv(csc.indiv, file="CSC individual stats summary.csv", row.names=FALSE)
write.csv(csc, file="CSC game stats.csv", row.names=FALSE)
write.csv(csc.bingos, file="CSC bingo records.csv", row.names=FALSE)


#boxplot(score~player, data=csc.bingos)
#plot(table(csc.bingos$length))

vppred <- lm(vp ~ blank+s+j+q+x+z+overtime+seconds.left+bingos+bingo.ave.score
             +exp.wespa.win, data=csc.indiv)

gamepred <- lm(vp ~ blank+s+j+q+x+z+overtime+minute+bingo.count+I(bingo.points/bingo.count),
               data=csc)

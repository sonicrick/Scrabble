### convert csv from online to .TOU


setwd("~/GitHub/Scrabble/SSSg")

library(gdata)  # for fixed width output

raw.gdoc <- read.csv("SSSg.csv", stringsAsFactors = F)
raw.gdoc[, 3] <- gsub(" - .*", "", raw.gdoc[, 3])  #strip email from player 1 name
raw.gdoc[, 5] <- gsub(" - .*", "", raw.gdoc[, 5])  #strip email from player 2 name

### extract all bingos
raw.bingo <- raw.gdoc[, grepl("Bingo", names(raw.gdoc))]

### TODO: assign player names to bingos

### extract stats
raw.stats <- raw.gdoc[, !grepl("Bingo", names(raw.gdoc))]
raw.stats <- raw.stats[, -1]  #remove timestamp field
names(raw.stats) <- c("game", "player", "player.score", "opp", "opp.score", "start", "J", "X", "Q", "Z",
                      "S", "opp.S", "blank", "opp.blank", "time", "opp.time")
flipfields <- c("start", "J", "X", "Q", "Z")
raw.stats[,flipfields] <- raw.stats[,flipfields]=="Player 1"

#replicate to swap player 2 and player 1
raw.stats2 <- raw.stats
raw.stats2[, c("player", "player.score", "opp", "opp.score", "S", "opp.S", "blank", "opp.blank", "time", "opp.time")] <-
  raw.stats2 [, c("opp", "opp.score", "player", "player.score", "opp.S", "S", "opp.blank", "blank", "opp.time", "time")]

raw.stats2[, flipfields] <- !raw.stats2[, flipfields]
raw.stats <- rbind(raw.stats, raw.stats2)

######################
### prepare .tou file
tou <- raw.stats[order(raw.stats$player, raw.stats$game), 1:6]
maxgames <- max(tou$game)
tou <- within(tou, {
  player <- factor(player)
  player.score <- player.score +
       ifelse(player.score > opp.score, 2000, ifelse(player.score == opp.score, 1000, 0))
  start <- ifelse(start, "+", "")
  opp <- formatC(paste0(start, as.numeric(factor(opp))), format="s", width="4")
       })
tou <- tou[, 1:4]  # rearrange to sequence as per .TOU output

widestats <- reshape(tou, idvar="player", timevar="game", direction="wide")
opp.col <- which(substr(names(widestats), 1, 3) == "opp")
score.col <- which(substr(names(widestats), 1, 12) == "player.score")

for (x in opp.col) {
  nongame <- is.na(widestats[[x]])
  widestats[[x]][nongame] <- formatC(widestats[[1]][nongame], format="s", width="4")
}

for (x in score.col) {
  nongame <- is.na(widestats[[x]])
  widestats[[x]][nongame] <- 100
}

widestats[is.na(widestats)] <- ""

tou.name <- "SSSg2014.TOU"
tourney.name <- "SSSg 2014 QF"
tourney.date <- "2014-10-22"  # YYYY-MM-DD
div.name <- "Quarterfinals"

cat(paste0("*M", tourney.date, " ", tourney.name),
    paste0("*", div.name),
    paste0(paste0(rep(" ", 38), collapse=""), "0"),
    file=tou.name, sep="\n", fill=TRUE)

colwidth <- c(20, rep(c(5, 4), maxgames))

write.fwf(widestats, file=tou.name, append=TRUE, sep="", colnames=FALSE, width=colwidth)

cat("*** END OF FILE ***", file=tou.name, append=TRUE)


######################
### TODO: output stats and bingos file

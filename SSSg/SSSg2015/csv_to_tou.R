### convert csv from online to .TOU


setwd("~/GitHub/Scrabble/SSSg/SSSg2015")

require(gdata)  # for fixed width output
require(plyr)
require(dplyr)
require(tidyr)
require(stringr)

raw.gdoc <- read.csv("SSSg2015_Ro16.csv", stringsAsFactors = F)
# raw.gdoc[, 3] <- gsub(" - .*", "", raw.gdoc[, 3])  #strip email from player 1 name
# raw.gdoc[, 5] <- gsub(" - .*", "", raw.gdoc[, 5])  #strip email from player 2 name

### extract all bingos
raw.bingo <- raw.gdoc[, grepl("Bingo", names(raw.gdoc))]

### TODO: assign player names to bingos

### extract stats
raw.stats <- raw.gdoc[, !grepl("Bingo", names(raw.gdoc))]  # drop all fields on bingos
raw.stats <- raw.stats[, -1]  #remove timestamp field
field.names <-
  c("match.id", "game", "name", "opp.name", "score", "opp.score", "start", "opp.start",
    "J", "Q", "X", "Z", "S", "blank",
    "opp.J", "opp.Q", "opp.X", "opp.Z", "opp.S", "opp.blank")
names(raw.stats) <- field.names

# flipfields <- c("start", "J", "Q", "X", "Z")
# raw.stats[,flipfields] <- raw.stats[,flipfields]=="Player A"
swap.fields <- field.names[grep("opp.*", field.names)]
swap.fields2 <- gsub("opp\\.", "", swap.fields)

#replicate to swap player 2 and player 1
raw.stats2 <- raw.stats
#swap the fields
raw.stats2[, c(swap.fields, swap.fields2)] <- raw.stats2[, c(swap.fields2, swap.fields)]

raw.stats <- rbind(raw.stats, raw.stats2)

######################
### prepare .tou file
# tou <- raw.stats[order(raw.stats$name, raw.stats$game), 1:6]
tou <- raw.stats[order(raw.stats$name, raw.stats$game), 2:7]  #game number, players, scores, who starts
maxgames <- max(tou$game)
tou <- within(tou, {
  name <- factor(name)
#   player.score <- player.score +
#        ifelse(player.score > opp.score, 2000, ifelse(player.score == opp.score, 1000, 0))
#   start <- ifelse(start, "+", "")
#   opp <- formatC(paste0(start, as.numeric(factor(opp))), format="s", width="4")
  score <- score +
    ifelse(score > opp.score, 2000, ifelse(score == opp.score, 1000, 0))
  start <- ifelse(start=="*", "+", "")
  opp.name <- formatC(paste0(start, as.numeric(factor(opp.name))), format="s", width="4")
  
       })
# tou <- tou[, 1:4]  # rearrange to sequence as per .TOU output
tou <- tou[, c(1, 2, 4, 3)]  # rearrange to sequence as per .TOU output


widestats <- reshape(tou, idvar="name", timevar="game", direction="wide")
opp.col <- which(substr(names(widestats), 1, 3) == "opp")
score.col <- which(substr(names(widestats), 1, 5) == "score")

for (x in opp.col) {
  nongame <- is.na(widestats[[x]])
  widestats[[x]][nongame] <- formatC(widestats[[1]][nongame], format="s", width="4")
}

for (x in score.col) {
  nongame <- is.na(widestats[[x]])
  widestats[[x]][nongame] <- 100
}

widestats[is.na(widestats)] <- ""

tou.name <- "Ro16.TOU"
tourney.name <- "SSSg 2015 Round of 16"
tourney.date <- "2015-10-22"  # YYYY-MM-DD
div.name <- "Round of 16"

cat(paste0("*M", tourney.date, " ", tourney.name),
    paste0("*", div.name),
    paste0(paste0(rep(" ", 38), collapse=""), "0"),
    file=tou.name, sep="\n", fill=TRUE)

colwidth <- c(20, rep(c(5, 4), maxgames))

write.fwf(widestats, file=tou.name, append=TRUE, sep="", colnames=FALSE, width=colwidth)

cat("*** END OF FILE ***", file=tou.name, append=TRUE)


######################
### TODO: output stats and bingos file

### create bingo stats
# extract bingos of each players, both players A and B
b.raw <- data.frame(name=raw.stats$name,
                    match.id=raw.stats$match.id,
                    game=raw.stats$game,
                    bingos=c(raw.bingo$A.Bingos, raw.bingo$B.Bingos))

# format into one bingo per line
# assumption is no one has more than 7 bingos in a game
bingos <- b.raw %>% separate(bingos, paste0("bingo", 1:7), ",") %>%
  gather(key=no, value=bingo, bingo1:bingo7) %>%
  filter(bingo!="") %>%
  mutate(score = as.numeric(str_extract(bingo, "[:digit:]+")),
         bingo = gsub("[ [:digit:]]", "", bingo))

# derive bingo stats per game
b.stats <- bingos %>% group_by(name, match.id, game) %>%
  summarize(bingo.num= n(),
            ave.bingo.score = mean(score),
            max.bingo.score = max(score)
            )

# merge back with full raw.stats
raw.stats <- left_join(raw.stats, b.stats)

sum.stats <- raw.stats %>% group_by(name) %>%
  summarize(ave.score = mean(score),
            ave.opp.score = mean(opp.score),
            J = sum(J)/n(),
            Q = sum(Q)/n(),
            X = sum(X)/n(),
            Z = sum(Z)/n(),
            S = sum(S)/n(),
            blank = sum(blank)/n(),
            hiscore = max(score),
            total.bingo = sum(bingo.num, na.rm = T),
            total.bingo.score = sum(ave.bingo.score*bingo.num, na.rm=T),
            ave.bingo = total.bingo/n(),
            ave.bingo.score = total.bingo.score/total.bingo,
            max.bingo.score = max(max.bingo.score, na.rm=T)
            )


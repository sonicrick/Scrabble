##########
# generate log of simulated pure-KOTH tourney
# based on random number of top 100 WESPA player
##########


setwd("~/GitHub/Scrabble/Grand CSC/blog 20150131")


source('~/GitHub/Scrabble/lib/WESPARatingLib.R')
require(dplyr)  # must require this after plyr (called in WESPARatingLib)

#top100 <- read.csv("WESPA Top 100 (20150118).csv", stringsAsFactors=FALSE)
top300 <- read.csv("WESPA Top 300 (20150118).csv", stringsAsFactors=FALSE)

set.seed(12345)

# below to select same set of players for all subsequent rounds
### COMMENT THIS OUT and uncomment in loop if 
players <- top300[sort(sample(101:300, numplay)), ]

numiter <- 1000
numplay <- 100
numround <- 45



sim_matchup <- function(standing) {
#  pairing <- standing[order(standing$cum.win*1000000+standing$cum.spread), ]
  pairing <- standing[order(standing$rank), ]
  player1 <- pairing[seq(1, numplay, 2), ]
  player2 <- pairing[seq(2, numplay, 2), ]
  # TODO: handle odd number of numplay with byes
  prob <- calcWESPAExpWin(player1$rating, player2$rating)
  p1.win <- rbinom(length(prob), 1, prob)
  p1.spread <- ifelse(p1.win==1, 1, -1)*abs(ceiling(rnorm(length(p1.win), 0, 1)*100))
  p1.win[p1.spread==0] <- .5
  return(data.frame(player1=player1$name, player2=player2$name, p1.prob=prob,
                    p1.spread=p1.spread, p1.win=p1.win,
                    stringsAsFactors=FALSE))
}

update_standing <- function(full.standing, result, rnd) {
  standing <- filter(full.standing, round==(rnd-1))
  standing <- standing[order(standing$rank), ]
  standing$round <- rnd
  p1 <- standing$name %in% result$player1
  p2 <- standing$name %in% result$player2
  standing$opp[p1] <- result$player2
  standing$opp[p2] <- result$player1
  standing$cum.win[p1] <- standing$cum.win[p1] + result$p1.win
  standing$cum.spread[p1] <- standing$cum.spread[p1] + result$p1.spread
  standing$cum.win[p2] <- standing$cum.win[p2] +1 - result$p1.win
  standing$cum.spread[p2] <- standing$cum.spread[p2] - result$p1.spread
  standing$rank[order(standing$cum.win*1000000+standing$cum.spread)] <- numplay:1
  return(standing)
}

for (iter in 1:numiter) {
  #players <- top100[sort(c(1, sample(2:100, numplay-1))), ]
  ### use this if we randomize player all the time
  #players <- top300[sort(sample(101:300, numplay)), ]
  
  tourney <- data.frame(name = players$Name, rating = players$Rating, round=0, rank=seq(1, numplay),
                        opp=character(numplay), cum.win=numeric(numplay), cum.spread=integer(numplay),
                        stringsAsFactors=FALSE)
  
  matchup <- data.frame(player1 = character(0), player2 = character(0),
                        p1.prob=numeric(0), p1.spread=numeric(0), p1.win=numeric(0), round=integer(0),
                        stringsAsFactors=FALSE)
  
  for (rnd in 1:numround) {
    pairing <- sim_matchup(filter(tourney, round==(rnd-1)))
    matchup <- rbind(matchup, cbind(pairing, rnd))
    tourney <- rbind(tourney, update_standing(tourney, pairing, rnd))
  }
  if (iter==1) {
    matchup.log <- cbind(matchup, iter)
    tourney.log <- cbind(tourney, iter)
  } 
  else {
      matchup.log <- rbind(matchup.log, cbind(matchup, iter))
      tourney.log <- rbind(tourney.log, cbind(tourney, iter))
  }
}

write.csv(matchup.log, "matchup_log_100_same.csv", row.names=FALSE)
write.csv(tourney.log, "standing_log_100_same.csv", row.names=FALSE)

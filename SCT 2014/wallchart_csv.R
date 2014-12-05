require(plyr)
require(reshape2)

streakCount <- function(x, streakval) {
  series <- rle(x)
  return(max(series$lengths[series$values==streakval]))
}

lastStreak <- function(x) {
  series <- rle(x)
  return(series$lengths[length(series$lengths)])
}


setwd("~/R/Scrabble")
a <- read.csv("a.csv", stringsAsFactors=FALSE)
a <- a[, -(3:5)]
player <- a[, 1:3]

#remove Nyman
a <- a[a$name != "Nyman, Mark", ]


pairing <- a[, c(1, which(substring(names(a), 1, 7)=="pairing"))]
score <- a[, c(1, which(substring(names(a), 1, 5)=="score"))]

mp <- melt(pairing, id.vars="name", variable.name="round", value.name="opp")
mp$round <- as.numeric(gsub("pairing", "", mp$round))
ms <- melt(score, id.vars="name", variable.name="round", value.name="score")
ms$round <- as.numeric(gsub("score", "", ms$round))

f <- join(player, join(mp, ms, type="right"))
#remove NAs i.e. Mark Nyman; can't remove earlier without messing players index
f <- f[complete.cases(f), ]
f$opp[f$opp != 0] <- player$name[f$opp[f$opp != 0]]
f$opp[f$opp == 0] <- "Bye"  # byes changed as playing himself


opp <- data.frame(name=f$opp, round=f$round)
opp <- join(opp,ms)


f$opp.score <- opp$score
f$opp.score[f$opp == "Bye"] <- 0  # give 75 wins to bye

f <- ddply(f, .(name), mutate,
           spread = score - opp.score,
           win = ifelse(spread>0, 1, ifelse(spread<0, 0, .5)),
           cumwin = cumsum(win),
           cumspread = cumsum(spread),
           ave.score = cumsum(score)/round,
           hi.score = cummax(score),
           low.score = cummin(score),
           si = cumwin*100000 + cumspread,
           lose.streak = streakCount(win, 0),
           win.streak = streakCount(win, 1),
         )

f <- ddply(f, .(round), mutate,
           rank = rank(-si, ties.method="min")  # negative sign to force descending rank
           )


write.csv(f, file="wallchart_tableau.csv")
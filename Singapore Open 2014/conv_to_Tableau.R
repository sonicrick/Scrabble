source('~/GitHub/Scrabble/lib/aupair_lib.R')
source('~/GitHub/Scrabble/lib/stat_lib.R')
setwd("~/GitHub/Scrabble/Singapore Open 2014")

touname <- "NATL2014.TOU"
tou <- readTou(touname)

f <- tou$games

f <- ddply(f, .(player), mutate,
#           spread = score - opp.score,
#           win = ifelse(spread>0, 1, ifelse(spread<0, 0, .5)),
           cumwin = cumsum(vp),
           cumspread = cumsum(margin),
           ave.score = cumsum(score)/round,
           hi.score = cummax(score),
           low.score = cummin(score),
           si = cumwin*100000 + cumspread,
            lose.streak = streakCount(vp, 0),
            win.streak = streakCount(vp, 1)
)

f <- ddply(f, .(round), mutate,
           rank = rank(-si, ties.method="min")  # negative sign to force descending rank
)

write.csv(f, file="SingOpen14Tableau.csv")
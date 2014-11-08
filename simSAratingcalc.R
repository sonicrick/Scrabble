library(plyr)

calcExpWin <- function(myrating, opprating) {
  logit <- 172
  expWin <- 1 / (1 + exp((opprating - myrating)/logit))
  return(expWin)
}

calcNewRating <- function(curRat, expWin, actWin) {
  bands <- c(-Inf, 1000, 1800, 2000, Inf)
  mult <- c(20, 20, 16, 10, 1)
  #to-do: check actual band for below 1000 i.e. first band
  
  return(curRat + (actWin-expWin)*mult[cut(curRat, breaks=bands, labels=FALSE)])
  #to-do: refractive multipliers where people cross new band halfway
  
  #to-do: iterative provisional calculation
}

tou <- read.csv("WYSCBen31May2014.csv", stringsAsFactors = FALSE)
names(tou) <- c("Name", "Opp", "ScoreFor", "ScoreAgainst", "Round")
tou <- within(tou, {Difference <- ScoreFor - ScoreAgainst
                    Win <- factor(ifelse(Difference==0, "D", ifelse(Difference<0, "L",  "W")))
})


#replicate the record for Opps
swap.tou <- tou
swap.tou <- within(swap.tou, {Name <- Opp
                              Opp <- tou$Name
                              ScoreFor <- ScoreAgainst
                              ScoreAgainst <- tou$ScoreFor
                              Difference <- -Difference
                              Win <- factor(ifelse(Difference==0, "D", ifelse(Difference<0, "L",  "W")))
})

tou <- rbind(tou, swap.tou)


#read ratings
ratings <- read.csv("WYSCBen31May2014rating.csv", stringsAsFactors=FALSE)

tou.Name <- data.frame(tou$Name)
tou.Opp <- data.frame(tou$Opp)
names(tou.Name) <- "Player"
names(tou.Opp) <- "Player"
tou.Name <- join(tou.Name, ratings[,-3])
tou.Opp <- join(tou.Opp, ratings[,-3])
tou$Rating <- tou.Name$pre.rating
tou$Opp.Rating <- tou.Opp$pre.rating
rm("tou.Name")
rm("tou.Opp")

tou$exp.win <- calcExpWin(tou$Rating, tou$Opp.Rating)

tou.sum <- ddply(tou, .(Name), summarize, 
                 pre.rating=Rating[1],
                 exp.win=sum(exp.win),
                 actual.win=sum(Win=="W") + sum(Win=="D")*0.5
                )

tou.sum$post.rating <- with(tou.sum, calcNewRating(pre.rating, exp.win, actual.win))

toucompare <- merge(tou.sum, ratings[,-2], by.y="Player", by.x="Name")

touerr <- with(toucompare, sum(abs(post.rating.y - post.rating.x)))

##########
### read and clean up head-to-head webdump from WESPA
##########

library(plyr)


cleanWESPAh2h <- function(filename="WESPA Head-to-Head.csv") {
# read from initial WESPA dump file
  h2h <- read.csv(filename, stringsAsFactors = FALSE)
  
  h2h <- within(h2h, {Date <- as.Date(substr(Tournament, 1, 10), "%d.%m.%Y")
                      Difference <- ScoreFor - ScoreAgainst
                      Win <- factor(ifelse(Difference==0, "D", ifelse(Difference<0, "L",  "W")))
                      })
  h2h <- h2h[,!(names(h2h) %in% c("Loss", "Margin"))]
  
  #replicate the record for Opps
  swap.h2h <- h2h
  swap.h2h <- within(swap.h2h, {Name <- Opp
                                Opp <- h2h$Name
                                ScoreFor <- ScoreAgainst
                                ScoreAgainst <- h2h$ScoreFor
                                Difference <- -Difference
                                Win <- factor(ifelse(Difference==0, "D", ifelse(Difference<0, "L",  "W")))
                                })
  
  h2h <- rbind(h2h, swap.h2h)
  rm(swap.h2h)
  
  return(h2h)
}


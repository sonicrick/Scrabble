##########
### read and clean up head-to-head webdump from SA
##########

library(plyr)


cleanSAh2h <- function(filename="SA Head-to-Head.csv") {

  # read from initial SA dump file
  h2h <- read.csv(filename, stringsAsFactors = FALSE)
  
  h2h <- within(h2h, {Date <- as.Date(Date, "%d %b %Y")
                      ScoreFor <- as.numeric(substr(ScoreFor, 1, 3))
                      Win <- factor(ifelse(Difference==0, "D", ifelse(Difference<0, "L",  "W")))
                      })
  h2h <- h2h[,!(names(h2h) %in% c("Loss", "Margin", "Round"))]
  # Round above dropped for now as WESPA h2h has not retrieved round yet
  

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


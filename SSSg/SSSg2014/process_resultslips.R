########
# Read full-record stats csv saved from online google form
# output one file on player features by round and bingo features
########

require(lubridate)
require(tidyr)
require(dplyr)

src <- "SSSg2014_full.csv"

#cutoff dates for SSSG2014; use the start date, and toucanet rating date + 1 of each stage
milestones <- dmy(c("21-9-2014", "6-10-2014", "23-10-2014", "2-11-2014", "23-11-2014"))
stages <- c("Round of 16", "Quarterfinal" ,"Semifinal", "Final")

raw <- read.csv(src, stringsAsFactors = FALSE)
raw$Player.1. <- gsub(" - .*", "", raw$Player.1.)
raw$Player.2. <- gsub(" - .*", "", raw$Player.2.)
raw$round <- cut(mdy(substr(raw$Timestamp, 1, 10)), milestones, stages)
  
#### rename columns
#names(raw) <- c("Date", "Game", "Player"


#create bingos file
b.raw <- raw[, c(1:3, 5, 18:42)]
names(b.raw)[3:4] <- c("P1", "P2")  # remove other column names starting with "Player", for next step
bingos <- b.raw %>% gather(key, value, starts_with("Player")) %>% separate(key, c("player", "property"), 9)
bingos$property <- gsub("Bingo\\.", "", bingos$property)  # change content for ease of separation below
bingos$property <- gsub("^(.)\\.$", "\\1\\.word", bingos$property)
bingos <- bingos %>% separate(property, c("bingo.no", "property")) %>%
  spread(property, value) %>%
  filter(score>0)
bingos$player <- with(bingos, ifelse(player=="Player.1.", P1, P2))
bingos <- bingos[, !names(bingos) %in% c("P1", "P2")]

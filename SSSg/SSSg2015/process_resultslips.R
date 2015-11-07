########
# Read full-record stats csv saved from online google form
# output one file on player features by round and bingo features
########

require(tidyr)
require(dplyr)

src <- "SSSg2014_full.csv"

raw <- read.csv(src, stringsAsFactors = FALSE)
raw$Player.1. <- gsub(" - .*", "", raw$Player.1.)
raw$Player.2. <- gsub(" - .*", "", raw$Player.2.)

#### rename columns
#names(raw) <- c("Date", "Game", "Player"


#create bingos file
b.raw <- raw[, c(1:3, 5, 18:41)]
bingos <- b.raw %>% gather(key, value, starts_with("Player")) %>% separate(key, c("player", "property"), 9)
bingos$property[bingos$property == ""] <- "name"
bingos$property <- gsub("\\.[[:digit:]]\\.", "", bingos$property)

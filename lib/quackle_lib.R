##################
# functions to handle Quackle (.gcg)
##################

library(dplyr)
library(tidyr)
library(stringr)

# path.data <- '~/GitHub/Scrabble/common_data'
# file.name <- 'Rd 07 Rod Talbot vs RP.gcg'

# path.data <- "C:/Users/Ricky/Dropbox/scrabble/quackle/PISC 2013"
# path.data <- "C:/Users/Ricky/Documents/GitHub/Scrabble/common_data/QvQsims/games_Speedy_Player_16.03_01.04.40"
path.data <- "C:/Users/Ricky/Documents/GitHub/Scrabble/common_data/QvQsims/gcg_20160301"
file.name <- "Speedy_Player-game-10018.gcg"
# file.name <- "Rd 11 Russell Honeybun vs RP.gcg"

file.name <- file.path(path.data, file.name)

####### functions

# create helper function to substract string y from string x, to find leave etc
findDiffTile <- function(x, y) {
#   #debug
#   cat(paste(x, y, '\n'))
  # check different chars in x and y, y being the subsetC:\Users\Ricky\Documents\GitHub\Scrabble\common_data
  xl <- unlist(strsplit(x, "")) # split into char vector for comparison
  yl <- unlist(strsplit(y, ""))

  # remove NAs since below can't have -NA
  pm <- pmatch(yl, xl)
  pm[is.na(pm)] <- 0
  # l <- xl[-pmatch(yl, xl)]
  l <- xl[-pm]
  
  # convert blanks to "" to avoid creating a list when findDiffTile is lapplied
  return(ifelse(length(l)==0, "", paste(l, collapse="")))
}

####### Main

extractGcg <- function(file.name) {
  gcg <- readLines(file.name)
  cat(paste0(file.name), "\n")
  
  prag.idx <- grep("^#", gcg)
  pragma <- gcg[prag.idx]
  
  ev.idx <- grep("^>", gcg)
  events <- gcg[ev.idx]
  
  # check for stray lines not defined by pragma
  # TODO: handle turn comments
  unk <- gcg[-c(prag.idx, ev.idx)]
  stopifnot(length(unk)==0)
  
  # get player names in pragma
  pm <- pragma %>% str_extract_all("^#player.+") %>% unlist %>%
    str_replace("^#player. ", "") %>%  # cleanup
    str_split(" ", 2)   # keep max 2 splits: id and name (which may include space)
  players.map <- do.call(rbind, pm)   # create as matrix for lookup later
  
  # set play type and the various syntaxes
  play.types <- c(
    "Play","Pass", "Exchange", "Challenged off", "Challenge bonus",
    "Countback bonus", "Countback penalty", "Time penalty")
  # set up syntax for the various play types
  grep.pattern <- c(
    " .+ .+ .+ .+ .+", " - ", " -[[:alpha:]]+", "--", "\\(challenge\\)",
    ": +\\([\\?[:upper:]]{1,7}", "\\([\\?[:upper:]]{1,7}\\)[ \\+]+-", "\\(time\\)")
  
  
  # create matrix of play types for each line
  pt <- sapply(grep.pattern, function(x) grepl(x, events), USE.NAMES = F)
  move.type <- factor(
    play.types[unlist(apply(pt, 1, which))],
    levels=play.types)
  
  play.score <- as.integer(str_extract(events, "[-+][[:digit:]]+"))
  cume.score <- as.integer(str_extract(events, "[[:digit:]]+$"))
  player <- events %>% str_extract(">.+:") %>%  # extract text
    str_replace_all("[>:]", "")  # cleanup to match with players.map
  player <- factor(      # convert to factor
    players.map[match(player, players.map[, 1]), 2],   #players.map column 1 for id, column 2 for name
    levels = players.map[, 2]
  )   
  
  
  # derive turn number for each player (including sequential e.g failed challenge)
  turn.seq <- rle(as.numeric(player))$length   # as.numeric because rle can't handle factor
  l <- length(turn.seq)
  tr <- rep(1:l, each=2, len=l)
  turn <- rep(tr, turn.seq)
  
  rack <- events %>% str_extract(": \\?*[[:upper:]]+") %>%
    str_extract("\\?*[[:upper:]]+")
  
  # identifying tiles used, and extract them
  # get list of all used tiles
  ut <- events %>% str_extract("[\\.[:alpha:]]+ [-+][:digit:]+") %>%
    str_extract_all("[:alpha:]+")
  # convert to char and clean up
  used.tiles <- sapply(ut, function(x) {
    # merge split chars in list e.g. play of B(A)BY is recorded as "B" and "BY", below merges it to "BBY"
    l <- lapply(x, function(y) {y[is.na(y)] <- ""; y })
    do.call(paste0, l)
  })
  # extract blank designation
  blk <- used.tiles %>% str_extract_all("[:lower:]")
  idx <- which(sapply(blk, length)>0)  # find non-empty blank designation
  blanks <- character(length(blk))  # pre designate placeholder
  blanks[idx] <- sapply(blk[idx], paste0, collapse="")  # paste to handle two blanks in a move
  
  # replace used.tiles designation to "?"
  used.tiles <- used.tiles %>% str_replace_all("[:lower:]", "?")
  
  leave <- mapply(findDiffTile, rack, used.tiles, USE.NAMES = F)
  
  picked.tiles <- "dummy"
  # if failed challenge, no picked.tiles
  

    
  # get position information
  h <- str_extract(events, "[:digit:]{1,2}[A-O]")
  v <- str_extract(events, "[A-O][:digit:]{1,2}")
  # v.idx <- is.na(h)
  # flag, true for horizontal
  horizontal <- !is.na(h)
  play.loc <- ifelse(is.na(h), ifelse(is.na(v), NA, v), h)
  play.row <- as.integer(str_extract(play.loc, "[:digit:]+"))
  play.col <- factor(str_extract(play.loc, "[A-O]"), levels=LETTERS[1:15])
  
  moves <- data.frame(player, turn, move.type, play.score, cume.score,
                      rack, used.tiles, leave, picked.tiles, blanks,
                      horizontal, play.row, play.col)

  # add challenge bonus points
  cb <- moves %>% filter(move.type == "Challenge bonus") %>% select(player, turn, play.score, cume.score)
  # identify in main list
  cb.idx <- which(!is.na(match(paste(moves$player, moves$turn, moves$move.type),
                               paste(cb$player, cb$turn, "Play"))))
  #create new field for challenge bonus
  moves$challenge.bonus <- 0
  moves$challenge.bonus[cb.idx] <- cb$play.score
  moves$cume.score[cb.idx] <- cb$cume.score
  # remove the challenge bonuses
  moves <- moves %>% filter(move.type != "Challenge bonus")
  
  # add field for challenged off moves
  co <- moves %>% filter(move.type == "Challenged off") %>% select(player, turn, cume.score)
  # identify in main list
  co.idx <- which(!is.na(match(paste(moves$player, moves$turn, moves$move.type),
                               paste(co$player, co$turn, "Play"))))
  #create new field for challenge bonus
  moves$challenged.off <- logical(nrow(moves))
  moves$challenged.off[co.idx] <- T
  moves$cume.score[co.idx] <- co$cume.score
  # remove the challenged off lines
  moves <- moves %>% filter(move.type != "Challenged off")
  
  game.data <- list(
    pragma=pragma, moves=moves, events=events
  )
  
}

# get all gcgs in path data
gcgs <- list.files(path.data)
gcgs <- gcgs[str_detect(gcgs, "gcg$")]
# to test, use just some
gcgs <- gcgs[1:100]


# gcg.data <- lapply(file.path(path.data, gcgs), extractGcg)
setwd(path.data)
gcg.data <- lapply(gcgs, extractGcg)
# gcg.data <- extractGcg(file.path(path.data, file.name))
bingoPlayability <- function(bingos) {
  require(plyr)
  bingoDir <- "~/GitHub/Scrabble"
  bingo.list <- read.table(paste0(bingoDir, "/7-8 words prob-play.txt"))
  bingos$cap <- toupper(bingos$bingo)
  names(bingo.list) <- c("cap", "probability", "playability")
  bingos <- join(bingos, bingo.list, by="cap")
  bingos <- bingos[, names(bingos)!="cap"]
}


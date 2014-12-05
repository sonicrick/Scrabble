require(plyr)

readTou <- function(touname, bye.style="SA") {
  txt <- readLines(touname)
  tourney <- substr(txt[1], 3, nchar(txt[1]))
  division <- substr(txt[2], 2, nchar(txt[2]))
  #TO-DO: handle multiple divisions in single .TOU file?
  txt <- txt[4:(length(txt)-1)]
  #ensure all names have no spaces, then read all separated by white space as a column
  tou.records <- read.table(text = gsub(" +([[:alpha:]]+)", "_\\1", txt),
                    colClasses="character", header=FALSE, strip.white=TRUE,
                    stringsAsFactors=FALSE)
  #TO-DO: handle multiple div, else error will occur here
  
  #reinsert space into player names
  tou.records[[1]] <- gsub("_", " ", tou.records[[1]])
  players <- data.frame(name=tou.records[[1]], no=seq(1:nrow(tou.records)))
  
  rounds <- (length(tou.records)-1)/2
  names(tou.records) <- c("player",
                  paste(rep(c("score", "opp"), rounds), rep(1:rounds, each=2), sep="."))
  games <- reshape(tou.records, direction="long", sep=".", varying=2:length(tou.records),
                      timevar="round", idvar="player.no.")
  
  games <- within(games, {
    start <- ifelse(substr(opp, 1, 1)=="+", "Y", "N")
    opp <- as.numeric(gsub("+", "", opp))
    score <- as.numeric(score)
    result <- ifelse(score>2000, "Win", ifelse(score>1000, "Draw", "Lose"))
    vp <- (score %/% 1000)/2
    #TO-DO: handle default treatment of score 1350 as a winning bye instead of draw
    #http://www.scrabbleplayers.org/w/Directing_SOWPODS_tournaments#Explanation_of_.TOU_file_format
    score <- score %% 1000
    bye <- ifelse(opp==player.no., "Y", "N")
  })
  
  names(games)[names(games)=="opp"] <- "no"
  games <- join(games, players, by="no")
  names(games)[names(games)=="no"] <- "opp.no."
  names(games)[names(games)=="name"] <- "opp"
  
  tmpopp <- games[, c("round", "player.no.", "score")]
  names(tmpopp) <- c("round", "opp.no.", "opp.score")
  games <- join(games, tmpopp)
  games$margin <- games$score - games$opp.score
  
  # assumes treatment of bye is 150-100 a-la Singapore SA
  if (bye.style == "SA") {
    games <- within(games, {
      bye <- ifelse(((score == 150) & (opp.score == 100))
                    | ((score == 100) & (opp.score == 150)), "Y", bye)
    })  
  }
  
  return(list(name=tourney, division=division, rounds=rounds, games=games))
}

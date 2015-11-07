##########
# count position of letters in all CSW words
##########

require(dplyr)

#load the word list keeping just the words 4-8 letters
csw <- read.table("~/GitHub/Scrabble/wordlists/CSW12 2-9 probability.txt",
                  stringsAsFactors=FALSE)
csw$length <- nchar(csw[, 1])
l.par <- 4:8  # included word lengths
csw <- csw[csw$length %in% l.par, -2]  # drop the probability column

freq <- data.frame(NULL)

for (l in l.par) {
  # generate position for all letters
  raw.pos <- lapply(as.list(LETTERS), function(x)
    unlist(gregexpr(x, csw[csw$length==l, 1])))
  names(raw.pos) <- LETTERS
  
  # generate count of location for all letters
  raw.count <- unlist(lapply(raw.pos, function(x) table(x[x>0])))
  
  # extract letter and position from column name
  lc <- do.call(rbind.data.frame, strsplit(names(raw.count), "\\."))
  
  # construct statistic table
  fr <- data.frame(letter=lc[, 1], length=l, pos=lc[, 2], count=raw.count)
  rownames(fr) <- NULL 
  
  freq <- rbind(freq, fr)
}

# calculate percentage within letter+length combination
freq <- freq %>% group_by(letter, length) %>% mutate(percent=count/sum(count))
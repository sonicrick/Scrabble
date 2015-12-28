setwd("~/GitHub/Scrabble/WESPA")

require(rvest)

url.WESPA <- "http://www.wespa.org/aardvark/cgi-bin/rating.cgi"

rankings <- html(url.WESPA) %>%
  html_nodes("table") %>%
  .[[2]] %>%   # the second table element in the page
  html_table()

names(rankings) <- rankings[1, ]
rankings <- rankings[-1, ]
# 
# rank.date <- html(url.WESPA) %>%
#   html_nodes("h2")

write.csv(rankings, file="WESPA_ratings_20151108.csv")

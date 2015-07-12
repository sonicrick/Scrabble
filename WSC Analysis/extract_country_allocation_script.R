setwd("~/GitHub/Scrabble/WSC Analysis")

require(stringr)

#1999
# source: http://www.nottinghamnomads.com/TLW/TLW65.pdf
# typed directly into excel from pdf
# note that US slot is one more than it should be, because WSC winner slot is lumped in. to separate out.

#2001
#source: http://www.scrabbleplayers.org/tourneys/2001/wsc/index.html
tmp <- readClipboard()
tmp <- gsub("[[:digit:]]/", "", tmp)
tmps <- gsub("(.+)\\((.+)\\)", "\\1\\2", tmp)
sp_loc <- str_locate(tmps, "[[:digit:]]+")
country <- str_trim(substr(tmps, 1, sp_loc[, 1]-1))
spots <- as.numeric(substr(tmps, sp_loc[, 1], sp_loc[, 2]))
alloc2001 <- data.frame(country=country, spots=spots, year=2001)
wsc.alloc <- rbind(wsc.alloc, alloc2001)

#2003
# to modify directly in excel, using 2001 numbers and
# source as reference http://stp.lingfil.uu.se/pipermail/skrabbel/2003-January/001001.html
# later manually add "Indonesia" which participated the first time

#2005
# source to copy into clipboard: http://www.scrabbleplayers.org/tourneys/2005/wsc/bulletin-1.html
tmp <- readClipboard()
tmp1 <- substr(tmp, 1, 20)
tmp2 <- substr(tmp, 21, 40)
tmps <- str_trim(c(tmp1, tmp2))
tmps <- tmps[-length(tmps)]  # throw last empty cell
sp_loc <- str_locate(tmps, "[[:digit:]]+")
country <- str_trim(substr(tmps, 1, sp_loc[, 1]-1))
spots <- as.numeric(substr(tmps, sp_loc[, 1], sp_loc[, 2]))
alloc2005 <- data.frame(country=country, spots=spots, year=2005)
wsc.alloc <- rbind(wsc.alloc, alloc2005)

write.csv(wsc.alloc, "wsc_alloc.csv")


#2007
# source  to copy into clipboard https://www.google.com.sg/url?sa=t&rct=j&q=&esrc=s&source=web&cd=4&cad=rja&uact=8&ved=0CC0QFjAD&url=http%3A%2F%2Fwww.scrabble.org.au%2Fwespa%2Fmattelwsc.doc&ei=HjizVKDiDI_auQS49YEw&usg=AFQjCNHMFWSx_KghNaGD4aqdbTntm0a7VA&sig2=WjQd9LfPOpAll3nQ0zbaUQ&bvm=bv.83339334,d.c2E
tmp <- readClipboard()
tmps <- str_split_fixed(tmp, "\\t", 6)
alloc <- data.frame(country=c(tmps[, 1], tmps[, 3], tmps[, 5]),
                    spots=as.numeric(c(tmps[, 2], tmps[, 4], tmps[, 6])),
                    year=2007)
alloc2007 <- alloc[1:(nrow(alloc)-2), ]
wsc.alloc <- rbind(wsc.alloc, alloc2007)

write.csv(wsc.alloc, "wsc_alloc.csv")


#2009
# source (cached from wscgames.com) http://webcache.googleusercontent.com/search?q=cache:OUqHHZ6jbLwJ:www.wscgames.com/2009/register.html+&cd=3&hl=en&ct=clnk&gl=sg
tmp <- readClipboard()
tmps <- gsub("\\t*\\[flag\\]", "", tmp)
tmps <- tmps[nchar(tmps)>0]  # remove empty rows
alloc2009 <- data.frame(country=str_sub(tmps, 1, str_locate(tmps, " \\(")[, 1]-1), 
                        spots=as.numeric(str_extract(tmps, "[[:digit:]]+")), year=2009)
wsc.alloc <- rbind(wsc.alloc, alloc2009)
#### Extra slots for WSC champ, runner up, and 1 WYSC to be added manually directly to Excel


#2011
# source (cached from wscgames.com) http://webcache.googleusercontent.com/search?q=cache:SzJrup234NYJ:www.wscgames.com/2011/register.html+&cd=5&hl=en&ct=clnk&gl=sg
# later manually add "Czech Republic", which somehow is not in homepage above
tmp <- readClipboard()
tmps <- gsub("\\t*\\[flag\\]", "", tmp)
tmps <- tmps[nchar(tmps)>0]  # remove empty rows
alloc2011 <- data.frame(country=str_sub(tmps, 1, str_locate(tmps, " \\(")[, 1]-1), 
                        spots=as.numeric(str_extract(tmps, "[[:digit:]]+")), year=2011)
wsc.alloc <- rbind(wsc.alloc, alloc2011)
#### Extra slots for WSC champ, runner up, and 1 WYSC to be added manually directly to Excel

write.csv(wsc.alloc, "wsc_alloc.csv")



#2013
# source (cached from wscgames.com) http://webcache.googleusercontent.com/search?q=cache:BUdWVajrRewJ:www.wscgames.com/2013/register.html+&cd=4&hl=en&ct=clnk&gl=sg
# later manually add "Czech Republic" (host) which is listed separately from above
tmp <- readClipboard()
tmps <- gsub("\\t*\\[flag\\]", "", tmp)
tmps <- tmps[nchar(tmps)>0]
alloc2013 <- data.frame(country=str_sub(tmps, 1, str_locate(tmps, " \\(")[, 1]-1), 
                        spots=as.numeric(str_extract(tmps, "[[:digit:]]+")), year=2013)
wsc.alloc <- rbind(wsc.alloc, alloc2013)
#### Extra slots for WSC champ, runner up, and 1 WYSC, and 5 LCQ to be added manually directly to Excel


write.csv(wsc.alloc, "wsc_alloc.csv")


#2015
# source http://scrabble.org.au/events/15WSC/inv_quotas.html
tmp <- readClipboard()
tmps <- tmp[tmp!="\t"]
alloc2015 <- data.frame(country=str_trim(str_extract(tmps, "^.+\\t")),
                        spots=as.numeric(str_extract(tmps, "[[:digit:]]+$")),
                        year=2015)
wsc.alloc <- rbind(wsc.alloc, alloc2015)
#### Extra slots for WSC/SCT champs (2), runner ups (2), and WYSC (2), and 5 LCQ to be added manually directly to Excel

write.csv(wsc.alloc, "wsc_alloc.csv")

##########
# WORK IN PROGRESS
# summarise from features
##########

setwd("~/GitHub/Scrabble/blog/20150524 new CSW15 words")

load("features.RData")

####################
# are words mainly from us / uk? below words not searched in US and/or UK
nonus <- which(tf$us.idx==0)
nonuk <- which(tf$uk.idx==0)
#answer: US
nonusuk <- nonus[which(nonus %in% nonuk)]

# regions with top search
top.ct <- table(tf$topreg)
top.ct <- top.ct[order(top.ct, decreasing=TRUE)]

####################
# words which are commonly searched throughout
zsum <- with(tf, (zero.pct1 + zero.pct2 + zero.pct3))
common.idx <- which(zsum==0)

# words which become common in 3rd period but not first
z3 <- with(tf, which((zero.pct3 == 0) & (zero.pct1 * zero.pct2 != 0)))


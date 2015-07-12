##########
# plot world source of words
##########

require(googleVis)

setwd("~/GitHub/Scrabble/blog/20150524 new CSW15 words")
load("features.RData")

# count countries and number of times a word is top spot there
top.ct <- table(tf$topreg)
top.ct <- top.ct[order(top.ct, decreasing=TRUE)]
world <- data.frame(region=names(top.ct), Count=top.ct,
                    stringsAsFactors=FALSE)
row.names(world) <- NULL

# count as percentage
world$pct <- with(world, (Count/sum(Count) * 100))
# create list of words occuring as top trends there
words <- paste0(world$region, ":\\n",
                sapply(world$region, function(x)
                  do.call(paste, as.list(tf$word[tf$topreg==x])))
                )
words <- sapply(as.list(words), function(x)
  do.call("paste", c(as.list(strwrap(x, 60)), sep="\\n")))
world <- cbind(world, words)

# remove the one with 0 region name, which throws off google map
CSW15Searches <- world[world$region!="0", ]

########
# create googlevis map


#######
# GeoMap version superceded, due to slow loading time
#M <- gvisGeoMap(CSW15Searches, locationvar="region", numvar="Count", hovervar="words",
#                options=list(region="world", dataMode="markers",
#                             width=741, height=463))
#
#plot(M)
#print(M, 'chart', file="CSW15-word-map.html")

C <- gvisGeoChart(CSW15Searches, locationvar="region", colorvar="Count",
                  hovervar="words",
                  options=list(displayMode="markers", width=834, height=521,
                               tooltip="{isHtml: 'true'}", # to ensure hover wraps
                               sizeAxis="{minValue:0, maxSize: 24}"
                               )
                  )
plot(C)
#export embeddable version
print(C, 'chart', file="CSW15-word-mapchart.html")

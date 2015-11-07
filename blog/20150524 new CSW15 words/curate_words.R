##########
### analyse when words in CSW15 pick up trend
##########

setwd("~/GitHub/Scrabble/blog/20150524 new CSW15 words")

# build the dictionary with data from CSW Initiation kit

cdef <- rbind(read.csv("CSW15 new 3s.csv", header=FALSE, stringsAsFactors=FALSE),
              read.csv("CSW15 new 4s.csv", header=FALSE, stringsAsFactors=FALSE),
              read.csv("CSW15 new 5s.csv", header=FALSE, stringsAsFactors=FALSE),
              read.csv("CSW15 new 7s 4 vowels or more.csv", header=FALSE, stringsAsFactors=FALSE),
              read.csv("CSW15 new 8s 4 vowels or more.csv", header=FALSE, stringsAsFactors=FALSE))
cdef$V1 <- gsub(" ", "", cdef$V1)
cdef <- cdef[cdef$V1 != "", ]


# top 10000, require clean up
c7 <- read.csv("CSW15 new 7s top 10000 (raw).csv", header=FALSE, stringsAsFactors=FALSE)
c7 <- unlist(c7, use.names=FALSE)
c7 <- c7[!grepl("[a-z]", c7)]
c7 <- c7[c7!=""]
c7 <- strsplit(c7, " ")
c7 <- sapply(c7, function(x) x[2])

c8 <- read.csv("CSW15 new 8s top 10000 (raw).csv", header=FALSE, stringsAsFactors=FALSE)
c8 <- unlist(c8[, c(2, 4, 6)], use.names=FALSE)
c8 <- c8[!grepl("[a-z]", c8)]
c8 <- substr(c8[c8!=""], 1, 8)

top10k <- c(c7, c8)

#filter out those already in cdef
in.cdef <- sapply(sapply(top10k, grep, cdef$V1), length)
keep10k <- top10k[which(in.cdef==0)]

# reverse filter for stems
### NOTE below are script commented out on how I shortlisted candidates for removal
### the hand-curated list of those to remove are retained uncommented
#c47 <- cdef$V1
#c47 <- c47[nchar(c47) > 3 & nchar(c47) < 8]
#stemword <- sapply(sapply(c47, grep, keep10k), length)
#stemf <- names(stemword)[stemword > 0]
#k <- unlist(sapply(stemf, grep, keep10k))
rem <- c("PONENTS", "ONOMAST", "PERCINES", "CRUDIEST", "SHENAIS")
keep10k <- keep10k[!(keep10k %in% rem)]

# clean up words with definition (cdef)
# key to note clean up is only ater filtering of top10k words, lest inflections are retained in keep10k
cdef$V3 <- gsub(" ", "", cdef$V3)
cfull <- c(cdef$V1, keep10k)


# remove plurals where stem exist in new word list

##### OLD
#plurals <- cdef[cdef$V3=="pl", 1]
#singulars <- substr(plurals, 1, nchar(plurals)-1)
#dup.sing <- unlist(sapply(singulars, function(x) grep(paste0("^",x,"$"), cfull)))
#cfull <- cfull[!(cfull %in% names(dup.sing))]

# find all which is simple S or ED extension
ccull <- sapply(cfull, function(x) grep(paste0("^", x, "S$|^", x, "ED$"), cfull))
#ccull.c <- sapply(ccull, length)
celim <- cfull[unlist(ccull)]
# take out from full list
cfull <- cfull[!(cfull %in% celim)]

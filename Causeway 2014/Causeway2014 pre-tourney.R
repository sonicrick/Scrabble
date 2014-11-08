###############
### Script for data for Causeway 2014
###############

source('cleanSAH2H.R')
source('cleanWESPAH2H.R')
source('SARatingLib.R')
source('WESPARatingLib.R')

library(plyr)

SAfile <- 'Head-to-Head.csv'
WESPAfile <- 'WESPA Head-to-Head.csv'
tournfile <- 'causeway 2014 participants.csv'
participants <- read.csv(tournfile, stringsAsFactors=FALSE)
WESPAname <- participants[,names(participants) %in% c("wespa.name", "sa.h2h.name")]

# convert SA name to standardize with WESPA name, for correct summarizing
SAh2h <- cleanSAh2h(SAfile)
SAh2h.Name <- data.frame(SAh2h$Name)
SAh2h.Opp <- data.frame(SAh2h$Opp)
names(SAh2h.Name) <- "sa.h2h.name"
names(SAh2h.Opp) <- "sa.h2h.name"
SAh2h.Name <- join(SAh2h.Name, WESPAname)
SAh2h.Opp <- join(SAh2h.Opp, WESPAname)
SAh2h$Name <- SAh2h.Name$wespa.name
SAh2h$Opp <- SAh2h.Opp$wespa.name
rm("WESPAname")
rm("SAh2h.Name")
rm("SAh2h.Opp")

h2h <- rbind(SAh2h, cleanWESPAh2h(WESPAfile))


#build h2h for each player from a certain cut-off date
startdate <- as.Date("01/01/1990", "%d/%m/%Y")
#re-sort by date to allow proper summarizing
h2h <- h2h[order(h2h$Date),]
h2h.sum <- ddply(subset(h2h, startdate <= Date), .(Name, Opp), summarize,
                 W=sum(Win=="W"), L=sum(Win=="L"), D=sum(Win=="D"),
                 Played=W+L+D, Margin=sum(Difference),
                 FirstGame=Date[1], LastGame=Date[Played]
)


#build expected wins info from ratings
sg <- subset(participants, country=="Singapore")
my <- subset(participants, country=="Malaysia")
sgprof <- sg[,c(1:3, 10)]
names(sgprof) <- c("P1.country", "P1.country.rank", "Player1", "P1.wespa.rating")
myprof <- my[,c(1:3, 10)]
names(myprof) <- c("P2.country", "P2.country.rank", "Player2", "P2.wespa.rating")

wespa.rat <- expand.grid(x=sg$wespa.rating, y=my$wespa.rating)
exp.wespa.win <- calcWESPAExpWin(wespa.rat$x, wespa.rat$y)
wespa.g <- expand.grid(x=sg$wespa.games, y=my$wespa.games)
wespa.games <- rowSums(wespa.g)

sa.rat <- expand.grid(x=sg$sa.rating, y=my$sa.rating)
exp.sa.win <- calcSAExpWin(sa.rat$x, sa.rat$y)
sa.g <- expand.grid(x=sg$sa.games, y=my$sa.games)
sa.games <- rowSums(sa.g)

msa.rat <- expand.grid(x=sg$msa.rating, y=my$msa.rating)
exp.msa.win <- calcSAExpWin(msa.rat$x, msa.rat$y)
msa.g <- expand.grid(x=sg$msa.games, y=my$msa.games)
msa.games <- rowSums(msa.g)

matchup <- expand.grid(Player1=sg$wespa.name, Player2=my$wespa.name)
matchup <- join(join(matchup, sgprof), myprof)

matchup <- cbind(matchup, exp.wespa.win, wespa.games,
                 exp.sa.win, sa.games, exp.msa.win, msa.games)

# combine all
final <- merge(matchup, h2h.sum, by.x=c("Player1", "Player2"),
               by.y=c("Name", "Opp"), all.x=TRUE)
final[,9:19][is.na(final[,9:19])] <- 0  #remove all NAs except dates
final$rated.games <- with(final, wespa.games + sa.games + msa.games)
final$mixed.exp.win <- with(final, (exp.wespa.win*wespa.games + exp.sa.win*sa.games +
                                    exp.msa.win*msa.games)/rated.games)


#sgwin <- ddply(final, .(Player1), summarize,
#               exp.mixed.win=2*sum(mixed.exp.win),
#               exp.wespa.win=2*sum(exp.wespa.win),
#               exp.sa.win=2*sum(exp.sa.win),
#               exp.msa.win=2*sum(exp.msa.win))

#mywin <- ddply(final, .(Player2), summarize,
#               exp.mixed.win=20-2*sum(mixed.exp.win),
#               exp.wespa.win=20-2*sum(exp.wespa.win),
#               exp.sa.win=20-2*sum(exp.sa.win),
#               exp.msa.win=20-2*sum(exp.msa.win))


#names(sgwin)[1] <- "wespa.name"
#names(mywin)[1] <- "wespa.name"
#participants <- join(participants, rbind(sgwin, mywin))
#write.csv(participants, file=tournfile, row.names=FALSE, na="")

#write.csv(participants[participants$country=="Singapore",],
#          file="Singapore team Causeway 2014.csv", row.names=FALSE, na="")
#write.csv(participants[participants$country=="Malaysia",],
#          file="Malaysia team Causeway 2014.csv", row.names=FALSE, na="")

#build a "flip" where P1 are Malaysians
final.m <- final
final.m[, c(1, 3, 4, 5, 15)] <- final.m[, c(2, 6, 7, 8, 16)]
final.m[, c(2, 6, 7, 8, 16)] <- final[, c(1, 3, 4, 5, 15)]
final.m[, c(9, 11, 13, 23)] <- 1 - final.m[, c(9, 11, 13, 23)]
final.m[, 19] <- -final.m[, 19]

final<-rbind(final, final.m)
#re-order so WESPA ratings are last two columns, for Tableau
final <- final[, c(1:4, 6, 7, 9:23, 5, 8)]

write.csv(final, file="Causeway 2014 matchup.csv", row.names=FALSE)

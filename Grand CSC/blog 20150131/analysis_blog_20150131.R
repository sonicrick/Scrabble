##########
# analyse log of simulated pure-KOTH tourney
##########

setwd("~/GitHub/Scrabble/Grand CSC/blog 20150131")
require(plyr)
require(dplyr)
require(stringr)
require(tidyr)
require(ggplot2)
require(treemap)

#### read both premier division and 2nd div

tourney.log <- read.csv("standing_log.csv", stringsAsFactors=FALSE)
matchup.log <- read.csv("matchup_log.csv", stringsAsFactors=FALSE)
tourney.log <- cbind(tourney.log, div="Premier Division")
matchup.log <- cbind(matchup.log, div="Premier Division")
#matchup.log <- matchup.log[, -1]  #remove running number at the beginning
#tourney.log <- tourney.log[, -1]  #remove running number at the beginning

idx <- "50v100"
#idx <- "50v100_same"

tourney.file <- "standing_log_100.csv"
matchup.file <- "matchup_log_100.csv"
#tourney.file <- "standing_log_100_same.csv"
#matchup.file <- "matchup_log_100_same.csv"

tourney.log <- rbind(tourney.log, 
                     cbind(read.csv(tourney.file, stringsAsFactors=FALSE), div="Second Division"))
matchup.log <- rbind(matchup.log,
                     cbind(read.csv(matchup.file, stringsAsFactors=FALSE), div="Second Division"))

#reduce space
matchup.log <- matchup.log[, -1]  #remove running number at the beginning
tourney.log <- tourney.log[, -1]  #remove running number at the beginning
tourney.log$div <- factor(tourney.log$div)
matchup.log$div <- factor(matchup.log$div)


numround <- max(tourney.log$round)   # assumed all tourney same rounds , TODO to vectorize
numplay <- 100  # TODO: make variable, length(unique(tourney.log$name)) was wrong

#### stats on winners
winners <- tourney.log %>% filter(round==numround, rank==1)
winners.roll <- winners %>% group_by(name, div) %>% summarise(won=n())
winners.roll$times.played <- sapply(winners.roll$name, function(x) sum(tourney.log$name==x)/(numround+1))
winners.roll <- mutate(winners.roll, pct=won/times.played)
runners.up <- tourney.log %>% filter(round==numround, rank==2)

win.margin <- winners$cum.win - runners.up$cum.win


#### stats on repeats by tourneys
h2h <- tourney.log %>% filter(opp!="") %>% group_by(name, opp, iter, div) %>% summarise(played=n())
iter.stat <- h2h %>% group_by(iter, div, played) %>% summarise(repfreq = n()/2)
iter.max <- iter.stat %>% group_by(iter, div) %>% summarise(maxrep = max(played))

iter2 <- iter.max %>% group_by(maxrep, div) %>% summarise(freq=n())
prem.big <- iter2 %>% group_by(maxrep) %>% summarise(m=max(freq))
iter2 <- iter2 %>% left_join(prem.big, by="maxrep") %>% mutate(above=freq==m)

iter2$pos <- c(2, -1)[iter2$above + 1]  #set position for label later


iter2.plot <- ggplot(iter2, aes(x=maxrep, y=freq, colour=div)) +
  geom_text(aes(label=freq, vjust=pos), show_guide=FALSE) +
  geom_line() + geom_point() +
  labs(x="Highest number of repetition in a tournament", y="Number of tournaments",
       colour="Division") + 
  scale_x_continuous(breaks=5:27, minor_breaks=NULL)

ggsave(paste0(idx, "Max repetitions (all tourney).png"), iter2.plot)

iter.2ndmax <- iter.stat %>% filter(played < max(played)) %>% group_by(iter) %>%
  summarise(maxrep=max(played))


#### stats on players by iters (expected wins, repeats etc)
h2h.stat <- h2h %>%
  mutate(id=paste(name, str_pad(iter, 3, pad="0"))) %>% group_by(id, div) %>%
  summarize(opp=n(), avg.repeats=mean(played),
            med.repeats=median(played), most.repeats=max(played))

ph.stat <- rbind_list(matchup.log,
                      # create info on player2s
                      matchup.log %>% mutate(player1=player2, p1.prob=1-p1.prob,
                                         p1.spread=-p1.spread, p1.win=1-p1.win,
                                         rnd=rnd, iter=iter)) %>% 
  mutate(id=paste(player1, str_pad(iter, 3, pad="0"))) %>% group_by(id, div) %>%
  summarize(exp.win=sum(p1.prob), act.win=sum(p1.win))

seed <- tourney.log %>% filter(round==0) %>%
  mutate(id=paste(name, str_pad(iter, 3, pad="0"))) %>%
  mutate(seed=rank) %>%
  select(id, div, seed)

t.stat <- tourney.log %>% filter(round!=0) %>% 
  mutate(id=paste(name, str_pad(iter, 3, pad="0"))) %>% group_by(id, div) %>%
  summarize(avg.rank=mean(rank), med.rank=median(rank),
            final.pos=rank[numround], freq_top=sum(rank==1))

p.stat <- join(h2h.stat, ph.stat) %>% join(t.stat) %>% join(seed)

###################
### plot
###################

### plot expected wins vs max repeats
maxrep <- max(p.stat$most.repeats)
rep.v.exp.win <- ggplot(data=p.stat, aes(y=most.repeats, x=exp.win)) +
  geom_point() + scale_y_continuous(breaks=seq(5, numround+5, 5),
                                    limits=c(0, maxrep+5)) +
  labs(y="Highest number of repeats for player in a tournament",
       x="Player's expected wins in the tournament") +
  facet_grid(.~div)

ggsave(paste0(idx, "Max repeats vs expected wins.png"), rep.v.exp.win)


### plot median rank vs max repeats
psp <- p.stat %>% group_by(most.repeats, med.rank, div) %>% summarize(freq=n()) %>%
  mutate(band=cut(freq, breaks=c(0, 1, 2, 5, 10, 50, 100, 1000),
                  labels=c("1", "2", "3-5", "6-10", "11-50", "51-100", ">100")),
         freq.band=cut(freq, breaks=seq(0, 600, 100),
                       labels=c("<100", "101-200", "201-300", "301-400", "401-500", ">500")))
rep.v.med.rank <- ggplot(data=psp, aes(y=most.repeats, x=med.rank)) +
  geom_point(aes(size=freq.band, colour=band)) +
  scale_x_continuous(breaks=c(1, seq(10, numplay, 10))) +
  scale_size_discrete(range=c(2, 4)) +
  scale_y_continuous(breaks=c(1, seq(5, numround, 5))) +
  scale_colour_brewer(palette="YlOrRd") +
  labs(y="Highest number of repeats for player in a tournament",
       x="Median of repeat player's rank in the tournament",
       colour="Frequency", size="Frequency (bands of 100)") +
  theme(legend.position="bottom") + 
  facet_grid(.~div, scale="free_x", space="free_x")
  

ggsave(paste0(idx, "Max repeats vs median rank.png"), rep.v.med.rank)


### plot seeding vs max repeats
pss <- p.stat %>% group_by(most.repeats, seed, div) %>% summarize(freq=n()) %>%
  mutate(band=cut(freq, breaks=c(0, 1, 2, 5, 10, 50, 100, 1000),
                  labels=c("1", "2", "3-5", "6-10", "11-50", "51-100", ">100")),
         freq.band=cut(freq, breaks=seq(0, 600, 100),
                       labels=c("<100", "101-200", "201-300", "301-400", "401-500", ">500")))
rep.v.seed <- ggplot(data=pss, aes(y=most.repeats, x=seed)) +
  geom_point(aes(size=freq.band, colour=band)) +
  scale_x_continuous(breaks=c(1, seq(5, numplay, 5))) +
  scale_size_discrete(range=c(2,4)) +
  scale_y_continuous(breaks=c(1, seq(5, numround, 5))) +
  scale_colour_brewer(palette="YlOrRd") +
  labs(y="Highest number of repeats for player in a tournament",
       x="Repeat player's seeding in the tournament",
       colour="Frequency", size="Frequency (bands of 100)") +
  theme(legend.position="bottom") +
  facet_grid(.~div, scale="free_x", space="free_x")

ggsave(paste0(idx, "Max repeats vs seeding.png"), rep.v.seed)


### plot expected wins vs number of opps
minopp <- min(p.stat$opp)
maxopp <- max(p.stat$opp)
opp.v.exp.win <- ggplot(data=p.stat, aes(y=opp, x=exp.win)) +
  geom_point() + scale_y_continuous(breaks=seq(5, numround+5, 5),
                                    limits=c(minopp-5, maxopp+5)) +
  labs(y="Number of different opponents in the tournament",
       x="Player's expected wins in the tournament") +
  facet_grid(.~div)

ggsave(paste0(idx, "Number of opponents vs expected wins.png"), opp.v.exp.win)


### plot median rank vs number of opps
osp <- p.stat %>% group_by(opp, med.rank, div) %>% summarize(freq=n()) %>%
  mutate(band=cut(freq, breaks=c(0, 1, 2, 5, 10, 50, 100, 1000),
                  labels=c("1", "2", "3-5", "6-10", "11-50", "51-100", ">100")),
         freq.band=cut(freq, breaks=seq(0, 600, 100),
                       labels=c("<100", "101-200", "201-300", "301-400", "401-500", ">500")))

opp.v.med.rank <- ggplot(data=osp, aes(y=opp, x=med.rank)) +
  geom_point(aes(size=freq.band, colour=band)) +
  scale_x_continuous(breaks=c(1, seq(5, numplay, 5))) +
  scale_size_discrete(range=c(2,4)) +
  scale_y_continuous(breaks=c(1, seq(5, numround, 5))) +
  scale_colour_brewer(palette="YlOrRd") +
  labs(y="Number of different opponents in the tournament",
       x="Median of player's rank in the tournament",
       colour="Frequency", size="Frequency (bands of 100)") +
  theme(legend.position="bottom") +
  facet_grid(.~div, scale="free_x", space="free_x")

ggsave(paste0(idx, "Number of opponents vs median rank.png"), opp.v.med.rank)


### plot seeding vs number of opps
oss <- p.stat %>% group_by(opp, seed, div) %>% summarize(freq=n()) %>%
  mutate(band=cut(freq, breaks=c(0, 1, 2, 5, 10, 50, 100, 1000),
                  labels=c("1", "2", "3-5", "6-10", "11-50", "51-100", ">100")),
         freq.band=cut(freq, breaks=seq(0, 600, 100),
                       labels=c("<100", "101-200", "201-300", "301-400", "401-500", ">500")))
opp.v.seed <- ggplot(data=oss, aes(y=opp, x=seed)) +
  geom_point(aes(size=freq.band, colour=band)) +
  scale_x_continuous(breaks=c(1, seq(5, numplay, 5))) +
  scale_size_discrete(range=c(2,4)) +
  scale_y_continuous(breaks=c(1, seq(5, numround, 5))) +
  scale_colour_brewer(palette="YlOrRd") +
  labs(y="Number of different opponents in the tournament",
       x="Repeat player's seeding in the tournament",
       colour="Frequency", size="Frequency (bands of 100)") +
  theme(legend.position="bottom") +
  facet_grid(.~div, scale="free_x", space="free_x")

ggsave(paste0(idx, "Number of opponents vs seeding.png"), opp.v.seed)


#### div 2 winners

nigel <- tourney.log %>% filter(name=="Nigel Richards", round==45)

wr <- winners.roll %>% filter(div=="Second Division") %>%
  mutate(pct=pct*100,
         display=paste0(name, " - ", won, " (", sprintf("%2.1f", pct), "%)")) %>%
  select(display, won, pct)

wrplot <- treemap(wr, "display", vSize="won", vColor="pct", type="value",
                  title = "Number of wins", title.legend = "% of wins",
                  fontsize.title=28, fontsize.labels=20, fontsize.legend=24)
dev.print(png, filename=paste0(idx, "Winners Treemap.png"), width=1007, height=643)
dev.off()


#explore anomaly of 13 matchups in second div
h2h13 <- h2h %>% filter(played==13, div=="Second Division")
h2hn <- unique(paste(h2h13$name, str_pad(h2h13$iter, 3, pad="0")))
h2hn <- p.stat[p.stat$id %in% h2hn, ]


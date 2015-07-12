##########
# analyse log of simulated pure-KOTH tourney
##########

setwd("~/GitHub/Scrabble/Grand CSC")
require(plyr)
require(dplyr)
require(stringr)
require(tidyr)
require(ggplot2)
require(treemap)

tourney.file <- "standing_log.csv"
matchup.file <- "matchup_log.csv"

tourney.log <- read.csv(tourney.file, stringsAsFactors=FALSE)
matchup.log <- read.csv(matchup.file, stringsAsFactors=FALSE)

numround <- max(tourney.log$round)   # assumed all tourney same rounds , TODO to vectorize
numplay <- 45  # TODO: make variable, length(unique(tourney.log$name)) was wrong

#### stats on winners
winners <- tourney.log %>% filter(round==numround, rank==1)
winners.roll <- winners %>% select(name, rating) %>% group_by(name) %>% summarise(won=n())
winners.roll$times.played <- sapply(winners.roll$name, function(x) sum(tourney.log$name==x)/(numround+1))
winners.roll <- mutate(winners.roll, pct=won/times.played)
runners.up <- tourney.log %>% filter(round==numround, rank==2)

win.margin <- winners$cum.win - runners.up$cum.win


#### stats on repeats by tourneys
h2h <- tourney.log %>% filter(opp!="") %>% group_by(name, opp, iter) %>% summarise(played=n())
iter.stat <- h2h %>% group_by(iter, played) %>% summarise(repfreq = n()/2)
iter.max <- iter.stat %>% group_by(iter) %>% summarise(maxrep = max(played))

iterrep.plot <- ggplot(iter.max, aes(maxrep)) +
  geom_histogram(binwidth=1, origin=.5, aes(fill=..count..)) +
  stat_bin(geom="text", aes(label=..count.., vjust=-1)) +
  labs(x="Highest number of repetition in a tournament", y="Number of tournaments") + 
  coord_cartesian(xlim=c(5, 27)) + scale_x_discrete(breaks=5:27)  # TODO: change constant to variable

ggsave("Max repetitions (all tourney).png", iterrep.plot)

iter.2ndmax <- iter.stat %>% filter(played < max(played)) %>% group_by(iter) %>%
  summarise(maxrep=max(played))


#### stats on players by iters (expected wins, repeats etc)
h2h.stat <- h2h %>%
  mutate(id=paste(name, str_pad(iter, 3, pad="0"))) %>% group_by(id) %>%
  summarize(opp=n(), avg.repeats=mean(played),
            med.repeats=median(played), most.repeats=max(played))

ph.stat <- rbind_list(matchup.log,
                      # create info on player2s
                      matchup.log %>% mutate(player1=player2, p1.prob=1-p1.prob,
                                         p1.spread=-p1.spread, p1.win=1-p1.win,
                                         rnd=rnd, iter=iter)) %>% 
  mutate(id=paste(player1, str_pad(iter, 3, pad="0"))) %>% group_by(id) %>%
  summarize(exp.win=sum(p1.prob), act.win=sum(p1.win))

seed <- tourney.log %>% filter(round==0) %>%
  mutate(id=paste(name, str_pad(iter, 3, pad="0"))) %>%
  mutate(seed=rank) %>%
  select(id, seed)

t.stat <- tourney.log %>% filter(round!=0) %>% 
  mutate(id=paste(name, str_pad(iter, 3, pad="0"))) %>% group_by(id) %>%
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
       x="Player's expected wins in the tournament")

ggsave("Max repeats vs expected wins.png", rep.v.exp.win)

# with Nigel colour
nig.p.stat <- p.stat %>%
  mutate(Player=ifelse(substr(id, 1, 5)=="Nigel", "Nigel Richards", "Everyone else")) %>%
  select(exp.win, most.repeats, Player)
nig.rvew <- ggplot(data=nig.p.stat, aes(y=most.repeats, x=exp.win, colour=Player)) +
  geom_point() + scale_y_continuous(breaks=seq(5, numround+5, 5),
                                    limits=c(0, maxrep+5)) +
  scale_color_manual(values=c("#000000", "#E69F00")) +
  labs(y="Highest number of repeats for player in a tournament",
       x="Player's expected wins in the tournament")

ggsave("Nigel - Max repeats vs expected wins.png", nig.rvew)

### plot median rank vs max repeats
psp <- p.stat %>% group_by(most.repeats, med.rank) %>% summarize(freq=n()) %>%
  mutate(band=cut(freq, breaks=c(0, 1, 2, 5, 10, 50, 100, 1000),
                  labels=c("1", "2", "3-5", "6-10", "11-50", "51-100", ">100")),
         freq.band=cut(freq, breaks=seq(0, 600, 100),
                       labels=c("<100", "101-200", "201-300", "301-400", "401-500", ">500")))
rep.v.med.rank <- ggplot(data=psp, aes(y=most.repeats, x=med.rank)) +
  geom_point(aes(size=freq.band, colour=band)) +
  scale_x_continuous(breaks=c(1, seq(5, numplay, 5))) +
  scale_size_discrete(range=c(3, 6)) +
  scale_y_continuous(breaks=c(1, seq(5, numround, 5))) +
  scale_colour_brewer(palette="YlOrRd") +
  labs(y="Highest number of repeats for player in a tournament",
       x="Median of repeat player's rank in the tournament",
       colour="Frequency", size="Frequency (bands of 100)")

ggsave("Max repeats vs median rank.png", rep.v.med.rank)


### plot seeding vs max repeats
pss <- p.stat %>% group_by(most.repeats, seed) %>% summarize(freq=n()) %>%
  mutate(band=cut(freq, breaks=c(0, 1, 2, 5, 10, 50, 100, 1000),
                  labels=c("1", "2", "3-5", "6-10", "11-50", "51-100", ">100")),
         freq.band=cut(freq, breaks=seq(0, 600, 100),
                       labels=c("<100", "101-200", "201-300", "301-400", "401-500", ">500")))
rep.v.seed <- ggplot(data=pss, aes(y=most.repeats, x=seed)) +
  geom_point(aes(size=freq.band, colour=band)) +
  scale_x_continuous(breaks=c(1, seq(5, numplay, 5))) +
  scale_size_discrete(range=c(3,6)) +
  scale_y_continuous(breaks=c(1, seq(5, numround, 5))) +
  scale_colour_brewer(palette="YlOrRd") +
  labs(y="Highest number of repeats for player in a tournament",
       x="Repeat player's seeding in the tournament",
       colour="Frequency", size="Frequency (bands of 100)")

ggsave("Max repeats vs seeding.png", rep.v.seed)


### plot expected wins vs number of opps
minopp <- min(p.stat$opp)
maxopp <- max(p.stat$opp)
opp.v.exp.win <- ggplot(data=p.stat, aes(y=opp, x=exp.win)) +
  geom_point() + scale_y_continuous(breaks=seq(5, numround+5, 5),
                                    limits=c(minopp-5, maxopp+5)) +
  labs(y="Number of different opponents in the tournament",
       x="Player's expected wins in the tournament")

ggsave("Number of opponents vs expected wins.png", opp.v.exp.win)


### plot median rank vs number of opps
osp <- p.stat %>% group_by(opp, med.rank) %>% summarize(freq=n()) %>%
  mutate(band=cut(freq, breaks=c(0, 1, 2, 5, 10, 50, 100, 1000),
                  labels=c("1", "2", "3-5", "6-10", "11-50", "51-100", ">100")),
         freq.band=cut(freq, breaks=seq(0, 600, 100),
                       labels=c("<100", "101-200", "201-300", "301-400", "401-500", ">500")))

opp.v.med.rank <- ggplot(data=osp, aes(y=opp, x=med.rank)) +
  geom_point(aes(size=freq.band, colour=band)) +
  scale_x_continuous(breaks=c(1, seq(5, numplay, 5))) +
  scale_size_discrete(range=c(3,6)) +
  scale_y_continuous(breaks=c(1, seq(5, numround, 5))) +
  scale_colour_brewer(palette="YlOrRd") +
  labs(y="Number of different opponents in the tournament",
       x="Median of player's rank in the tournament",
       colour="Frequency", size="Frequency (bands of 100)")

ggsave("Number of opponents vs median rank.png", opp.v.med.rank)


### plot seeding vs number of opps
oss <- p.stat %>% group_by(opp, seed) %>% summarize(freq=n()) %>%
  mutate(band=cut(freq, breaks=c(0, 1, 2, 5, 10, 50, 100, 1000),
                  labels=c("1", "2", "3-5", "6-10", "11-50", "51-100", ">100")),
         freq.band=cut(freq, breaks=seq(0, 600, 100),
                       labels=c("<100", "101-200", "201-300", "301-400", "401-500", ">500")))
opp.v.seed <- ggplot(data=oss, aes(y=opp, x=seed)) +
  geom_point(aes(size=freq.band, colour=band)) +
  scale_x_continuous(breaks=c(1, seq(5, numplay, 5))) +
  scale_size_discrete(range=c(3,6)) +
  scale_y_continuous(breaks=c(1, seq(5, numround, 5))) +
  scale_colour_brewer(palette="YlOrRd") +
  labs(y="Number of different opponents in the tournament",
       x="Repeat player's seeding in the tournament",
       colour="Frequency", size="Frequency (bands of 100)")

ggsave("Number of opponents vs seeding.png", opp.v.seed)


### plot number of opps vs frequency by seeding
oss.f <- oss %>% select(opp, seed, freq) %>%
  mutate(seed.col=cut(seed, breaks=c(0, 1, 5, 10, 45, 50),
                      labels=c("Top", "Rest of top 5", "Rest of top 10",
                      "The rest", "Bottom 5")))
of.b.seed<- ggplot(data=oss.f, aes(x=opp, y=freq, colour=as.factor(seed))) +
  labs(x="Number of different opponents in a tournament",
       y="Number of occurences", colour="Seed") +
  guides(col=guide_legend(ncol=3)) +
  geom_line()

ggsave("Number of opponents vs frequency by seeding.png", of.b.seed)


#### nigel stats

nigel <- tourney.log %>% filter(name=="Nigel Richards", round==45)

wr <- winners.roll %>%
  mutate(pct=pct*100,
         display=paste0(name, " - ", won, " (", sprintf("%2.1f", pct), "%)")) %>%
  select(display, won, pct)

wrplot <- treemap(wr, "display", vSize="won", vColor="pct", type="value",
                  title = "Number of wins", title.legend = "% of wins",
                  fontsize.title=28, fontsize.labels=20, fontsize.legend=24)
dev.print(png, filename="Winners Treemap.png", width=1007, height=643)
dev.off()

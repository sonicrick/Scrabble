##########
# create charts on letter positions
# assume freq data frame is available
##########

require(dplyr)
require(scales)

# if data not loaded yet, un-comment and execute below
#source('~/GitHub/Scrabble/blog/20150502 letters position/letter_pos.R')

# count percentages of each letter by position
ds <- freq %>% mutate(gperc = as.numeric(pos)/length) %>%  # position as percentage
  group_by(letter, gperc) %>% summarise(n=sum(count)) %>%
  mutate(nperc=n/sum(n)) %>%  # percentage of times letter is in this position
  mutate(intervals = gperc - lag(gperc)) %>%  # for ggplot stack bar
  mutate(rev.letter = factor(letter, levels=rev(levels(letter))))  # for ggplot reverse scale

# fix NA intervals:
ds$intervals[is.na(ds$intervals)] <- ds$gperc[is.na(ds$intervals)]


# heatmap style percentage in positions
# ds <- ds %>% mutate(intervals = gperc - lag(gperc))
g.stack <- ggplot(ds, aes(x=rev.letter)) +  # reverse axis sequence
  geom_bar(aes(y=intervals, fill=nperc), stat="identity") +
  coord_flip() +  # NOTICE: reversal of x and y axes
  theme_bw() + theme(aspect.ratio=2) +
  scale_fill_continuous(low="white", high="red", guide="legend",
                        breaks=seq(.1, 1, .1), labels=percent) +
  scale_y_continuous(labels=percent) +
  ylab("Letter position with respect to word length") + xlab("") +
  labs(fill="Frequency of\noccurrence")

ggsave("Position of tile by letters.PNG", g.stack)


#tile rack heatmap
wl_label <- function(variable, value) {  # function to change facet label
  return(paste0(value, "-letter words"))
}

dt <- freq %>% mutate(rev.letter = factor(letter, levels=rev(levels(letter))))  # for ggplot reverse scale

g.tile <- ggplot(dt, aes(x=pos)) +
  theme_bw() + theme(aspect.ratio=30) +
  facet_grid(~ length, scales="free_x", space="free_x", labeller=wl_label) +
  scale_fill_continuous(low="white", high="red", guide="legend",
                        breaks=seq(.1, 1, .1), labels=percent) +
  geom_tile(aes(y=rev.letter, fill=percent)) +  # rev.letter to have A-Z vertical down
  xlab("Tile position") + ylab("") +
  labs(fill="Frequency of\noccurrence") +
  theme(panel.grid.major = element_line(size=5))

ggsave("Position of tile by letters and word length.PNG", g.tile)

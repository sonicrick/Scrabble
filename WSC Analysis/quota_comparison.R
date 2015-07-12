##########
# compare WSC quota vs actual participation
##########

require(xlsx)
require(dplyr)
require(tidyr)
require(stringr)

setwd("~/GitHub/Scrabble/WSC Analysis")
xls <- "WSC Participations until 2013.xlsx"

alloc <- read.xlsx(xls, sheetName="by country allocations")
players <- read.xlsx(xls, sheetName="player list")
names(players)[names(players) == "WSC.Year"] <- "WSC"  # to standardise with alloc

### needed just for comparison with c.spread summary from below; where same, use c.spread
#country.rep <- read.xlsx(xls, sheetName="by country representatives")
##drop everything after comma in country e.g. "Germany, Federal Republic of"
#country.rep$Country <- gsub(",.*", "", country.rep$Country)
##ensure every word capitalized
#country.rep$Country <- str_replace_all(country.rep$Country,
#                                       perl("( )([[:alpha:]])"), "\\1\\U\\2")


# filter only those where allocation information are available
min.year <- min(alloc$WSC)
players <- players[players$WSC >= min.year, ]


countries.years <- players %>% group_by(Entity, WSC) %>% summarise(players=n())
c.spread <- countries.years %>% spread(WSC, players)

wsc.teams <- full_join(alloc, countries.years)

# load libraries
require(fastLink)
require(dplyr)
require(tidyr)
require(data.table)
# load in clean dataset

setwd("~/Desktop/3rdYrPaper/Code/Data/FakeData/")
load("x_data.RData")
load("~/Desktop/3rdYrPaper/Code/Data/FakeData/y_raw_data.RData")
y_data <- y_raw_data

# first do one-to-one matches
prl <- fastLink(x_data, y_data, varnames = c("first","last", "year", "month", "day"),
                stringdist.match = c("first","last"),
                numeric.match = c("year", "month","day"))

unique_links<- cbind(x_data[prl$matches[,1],c("id_x", "x1","x_name","x_bday")], y_data[prl$matches[,2],c("id_y", "y","name","bday")])
matchStatus <- unique_links %>% mutate(trueMatch = (id_x == id_y)) %>% View()

# multiple matches
multi_prl <- fastLink(x_data, y_data, varnames = c("first","last", "year", "month", "day"),
                stringdist.match = c("first","last"),
                numeric.match = c("year", "month","day"), dedupe.matches = FALSE, threshold.match = 0.95 )

multi_links <- data.frame(i = multi_prl$matches$inds.a, j = multi_prl$matches$inds.b)
multi_links<- multi_links[order(multi_links$i),]
link_dict <- split(multi_links$i,multi_links$j)

# data frame / table way
# link_dict2 <- setDT(multi_links)[,.(links= paste(j, collapse = " ")), by=i] library (plyr)
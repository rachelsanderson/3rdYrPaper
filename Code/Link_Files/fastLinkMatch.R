# load libraries
require(fastLink)
require(dplyr)
require(tidyr)
require(data.table)
# load in clean dataset

setwd("~/Desktop/3rdYrPaper/Code/Data/FakeData/")
load("gold_data_aug.RData")
load("x_data.RData")
load("y_raw_data.RData")

setwd("~/Desktop/3rdYrPaper/Code/Data/MatchedData/")
data_gold <- gold_data_aug[complete.cases(gold_data_aug),]
save(data_gold, file = "data_gold.RData")

# Code for doing PRL with one-to-one matching -- saved in unique_links
y_data <- y_raw_data
prl <- fastLink(x_data, y_data, varnames = c("first","last", "year", "month", "day"),
                strsingdist.match = c("first","last"),
                numeric.match = c("year", "month","day"))

unique_links<- cbind(x_data[prl$matches[,1],c("id_x", "x1","x_name","x_bday")], y_data[prl$matches[,2],c("id_y", "y","name","bday")])
unique_links <- unique_links %>% mutate(trueMatch = (id_x == id_y)) 

# Analyze goodness of matching
disp(paste0("fastLink gets ", sum(unique_links$trueMatch == TRUE)/nrow(x_data), " of the possible matches correct"))
disp(paste0("fastLink has a false match rate of ", sum(unique_links$trueMatch == FALSE)/nrow(unique_links), " of all matches made"))
plot(data_gold$x1, data_gold$y, col="black", type='p')
points(unique_links$x1, unique_links$y, col="red")

# save for analysis file 
data_prl_singleMatch <- select(unique_links, c("x1", "y", "trueMatch"))
save(data_prl_singleMatch, file = "data_prl_singleMatch.RData")

# Code for doing PRL with multiple matches -- multi_links
x_data <- x_data[order(x_data$id),]
multi_prl <- fastLink(x_data, y_data, varnames = c("first","last", "year", "month", "day"),
                stringdist.match = c("first","last"),
                numeric.match = c("year", "month","day"), dedupe.matches = FALSE, threshold.match = 0.95 )
multi_links <- data.frame(i = multi_prl$matches$inds.a, j = multi_prl$matches$inds.b)
multi_links<- multi_links[order(multi_links$i),]
multi_link_data <- cbind(x_data[multi_links$i, ], y_data[multi_links$j, ]) %>%
                    mutate(trueMatch = (id_x == id_y)) %>%
                    add_count(id)

# Analyze goodness of matching
# disp(paste0("fastLink gets ", sum(unique_links$trueMatch == TRUE)/nrow(x_data), " of the possible matches correct"))
disp(paste0("fastLink w/multi matches has a false match rate of ", sum(multi_link_data$trueMatch == FALSE)/nrow(multi_link_data), " of all matches made"))
plot(data_gold$x1, data_gold$y, col="black", type='p')
points(multi_link_data$x1, multi_link_data$y, col="red") # adds so much noise!!!


# save for analysis file 
data_prl_multiMatch <- select(multi_link_data, c("id", "x1", "y", "trueMatch", "n"))
save(data_prl_multiMatch, file = "data_prl_multiMatch.RData")

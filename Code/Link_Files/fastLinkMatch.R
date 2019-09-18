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
y_data <- y_raw_data

### SET PARAMETERS HERE
thresh = 0.4

# Code for doing PRL with one-to-one matching -- saved in unique_links

prl <- fastLink(x_data, y_data, varnames = c("first","last", "year", "month", "day"),
                stringdist.match = c("first","last"),
                numeric.match = c("year", "month","day"), threshold.match = thresh, return.all=FALSE)

# threshold is somehow different? these produce different things and idk why
prl.matches <- getMatches(x_data,y_data, prl, threshold.match = thresh) %>% mutate(trueMatch = (id_x == id_y))
prl.matches.user <- cbind(x_data[prl$matches$inds.a, c("id_x", "x1", "x_name")], y_data[prl$matches$inds.b,c("id_y","y","name")])%>% mutate(trueMatch = (id_x == id_y)) 

# why don't prl.matches equal prl.matches.user   
print(nrow(prl.matches) == nrow(prl.matches.user))
print(sum(prl.matches$trueMatch)==sum(prl.matches.user$trueMatch))

# Analyze goodness of matching
print(paste0("fastLink gets ", sum(prl.matches$trueMatch == TRUE)/nrow(x_data), " of the possible matches correct"))
print(paste0("fastLink has a false match rate of ", sum(prl.matches$trueMatch == FALSE)/nrow(prl.matches), " of all matches made"))
plot(data_gold$x1, data_gold$y, col="black", type='p')
points(prl.matches$x1, prl.matches$y, col="red")

# save for analysis file 
data_prl_singleMatch <- select(unique_links, c("x1", "y", "trueMatch"))
save(data_prl_singleMatch, file = "data_prl_singleMatch.RData")

# Code for doing PRL with multiple matches -- multi_links
x_data <- x_data[order(x_data$id),]
multi_prl <- fastLink(x_data, y_data, varnames = c("first","last", "year", "month", "day"),
                stringdist.match = c("first","last"),
                numeric.match = c("year", "month","day"), dedupe.matches = FALSE, threshold.match = thresh)
multi_links <- data.frame(i = multi_prl$matches$inds.a, j = multi_prl$matches$inds.b)
# multi_links<- multi_links[order(multi_links$i),]
multi_link_data_raw <- cbind(x_data[multi_links$i, cbind("id_x", "x1", "x_name","x_bday")], y_data[multi_links$j,cbind("id_y", "y", "name","bday")]) %>%
                    mutate(trueMatch = (id_x == id_y),
                           posterior =  multi_prl$posterior) %>%
                    add_count(id_x) %>%
                    arrange(id_x, desc(posterior))

truncate_links <- function(df, thresh){
  df.truncated <- df %>% select(-n) %>% filter(posterior >= thresh ) %>% add_count(id_x)
  return(df.truncated)
}

multi_link_data <- truncate_links(multi_link_data_raw, thresh)

# Analyze goodness of matching
# disp(paste0("fastLink gets ", sum(unique_links$trueMatch == TRUE)/nrow(x_data), " of the possible matches correct"))
print(paste0("fastLink w/multi matches has a false match rate of ", sum(multi_link_data$trueMatch == FALSE)/nrow(multi_link_data), " of all matches made"))
plot(data_gold$x1, data_gold$y, col="black", type='p')
points(multi_link_data$x1, multi_link_data$y, col="red") # adds much noise!!!

# save for analysis file 
data_prl_multiMatch <- select(multi_link_data, c("id_x", "id_y", "x1", "y", "trueMatch", "n", "posterior"))
save(data_prl_multiMatch, file = "data_prl_multiMatch.RData")

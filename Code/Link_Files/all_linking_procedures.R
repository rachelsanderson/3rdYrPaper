require(dplyr)
require(fastLink)
require(phonics)
require(ggplot2)
require(gridExtra)
source("~/Desktop/3rdYrPaper/Code/Link_Files/link_files.R")
source("~/Desktop/3rdYrPaper/Code/Link_Files/abe_matching.R")
source("~/Desktop/3rdYrPaper/Code/Link_Files/prl_match.R")

############################ TO DO: Fix the data sets ##############
### LOAD IN GOLD, X AND Y DATA SETS ###################################
load("~/Desktop/3rdYrPaper/Code/Data/FakeData/gold_data_aug.RData")
load("~/Desktop/3rdYrPaper/Code/Data/FakeData/x_data.RData")
load("~/Desktop/3rdYrPaper/Code/Data/FakeData/y_data.RData")

### STANDARDIZE NAMES #########################################
x.df <- x_data %>% mutate(f_name_nysiis = nysiis(first, modified=TRUE), l_name_nysiis = nysiis(last, modified=TRUE)) %>% 
    select(id_x, x1, x2, f_name_nysiis, l_name_nysiis, year)
y.df <- y_data %>% mutate(f_name_nysiis = nysiis(first, modified=TRUE), l_name_nysiis =nysiis(last, modified=TRUE)) %>% 
  select(id_y, y, f_name_nysiis, l_name_nysiis, year)

#### Declare names of variables #####################
name_vars <- c("f_name_nysiis", "l_name_nysiis")
num_vars <- c("year")
x_vars <- c("id_x", "x1", "x2")
y_vars <- c("id_y", "y")
outDir <- "~/Desktop/3rdYrPaper/Code/Data/MatchedData/"

#### FOR SOME REASON WE ARE GETTING DOUBLE OBS FOR ABE MULTI --> generating prob in abe single!!
### Master linking function returns list of matched datasets #######
match_list <- link_files(x.df, y.df, x_vars, y_vars, name_vars, num_vars, 
                       age_band=2, twoway=TRUE, thresh=0.85, outputDir=outDir, saveOut=TRUE)

###  Plot output of all matchings 
par(mfrow = c(2,2))
for (i in seq_along(match_list)){
  m <- match_list[[i]]
  name <- names(match_list)[i]
  plot(gold_data_aug$x2, gold_data_aug$y, col="black", type='p', main=name)
  points(m$x2, m$y, col="red") 
  abline(lm(m$y ~ m$x2), col="blue")
}

# need to make true match for the prl as well
names <- c("abe_single", "abe_multi", "prl_single", "prl_multi")
t <- NULL
for (i in 1:length(match_list)){
  data <- match_list[[i]]
  t <- rbind(t, data.frame(method = as.character(names[i]), 
                           nMatches = nrow(data), 
                           pCorrect = round(sum(data$true_match)/nrow(data),3),
                           nUniqueX = nrow(distinct(data,id_x))))
}

# 
# plot_matches <- function(df){
#   p <- ggplot(df, mapping = aes(x = x2, y = y, colour = x1)) + geom_point() +
#     geom_smooth(method=lm, formula = y~x, se=false) + 
#     labs(title=deparse(substitute(df)))
#   return(p)
# }
# 
# grid.arrange(grobs=lapply(match_list, plot_matches),ncol=2)
# 

require(dplyr)
require(fastLink)
source("~/Desktop/3rdYrPaper/Code/Link_Files/link_files.R")
source("~/Desktop/3rdYrPaper/Code/Link_Files/abe_matching.R")
source("~/Desktop/3rdYrPaper/Code/Link_Files/prl_match.R")

### LOAD IN STANDARDIZED DATA FRAMES #######
load("~/Desktop/3rdYrPaper/Code/Data/FakeData/gold_data_aug.RData")
x.df <- read.dta("~/Desktop/3rdYrPaper/Code/Data/FakeData/x_data_ready2link.dta")
x.df <- select(x.df, x1, id_x, year, f_name_nysiis, l_name_nysiis)
y.df <- read.dta("~/Desktop/3rdYrPaper/Code/Data/FakeData/y_data_ready2link.dta")
y.df <- select(y.df, y, id_y, year, f_name_nysiis, l_name_nysiis)

### Declare names of variables #######
name_vars <- c("f_name_nysiis", "l_name_nysiis")
num_vars <- c("year")
x_vars <- c("id_x", "x1")
y_vars <- c("id_y", "y")
outDir <- "~/Desktop/3rdYrPaper/Code/Data/MatchedData/"

### Master linking function returns list of matched datasets #######
match_list <- link_files(x.df, y.df, x_vars, y_vars, name_vars, num_vars, 
                       age_band=2, twoway=TRUE, thresh=0.85, outputDir=outDir, saveOut=TRUE)

###  Plot output of all matchings 
par(mfrow = c(2,2))
for (i in seq_along(match_list)){
  m <- match_list[[i]]
  name <- names(match_list)[i]
  plot(gold_data_aug$x1, gold_data_aug$y, col="black", type='p', main=name)
  points(m$x1, m$y, col="red") 
  abline(lm(m$y ~ m$x1), col="blue")
}


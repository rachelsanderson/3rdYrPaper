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

## MAKE THIS INTO A FUNCTION?
par(mfrow = c(2,2))
plot(gold_data_aug$x1, gold_data_aug$y, col="black", type='p', main="abe single")
points(abe_single$x1, abe_single$y, col="red") 
abline(lm(abe_single$y ~ abe_single$x1), col="blue")

plot(gold_data_aug$x1, gold_data_aug$y, col="black", type='p', main="abe multi")
points(abe_multi$x1, abe_multi$y, col="red") 
abline(lm(abe_multi$y ~ abe_multi$x1), col="blue")

plot(gold_data_aug$x1, gold_data_aug$y, col="black", type='p', main="prl single")
points(prl_single$x1, prl_single$y, col="red") 
abline(lm(prl_single$y ~ prl_single$x1), col="blue")

plot(gold_data_aug$x1, gold_data_aug$y, col="black", type='p', main="prl multi")
points(prl_multi$x1, prl_multi$y, col=alpha("red",1))
abline(lm(prl_multi$y ~ prl_multi$x1), col="blue")
       

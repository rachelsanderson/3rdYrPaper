require(dplyr)
require(scales)
source("~/Desktop/3rdYrPaper/Code/Link_Files/abe_matching.R")
source("~/Desktop/3rdYrPaper/Code/Link_Files/prl_match.R")


load("~/Desktop/3rdYrPaper/Code/Data/FakeData/gold_data_aug.RData")
x.df <- read.dta("~/Desktop/3rdYrPaper/Code/Data/FakeData/x_data_ready2link.dta")
x.df <- select(x.df, x1, id_x, year, f_name_nysiis, l_name_nysiis)
y.df <- read.dta("~/Desktop/3rdYrPaper/Code/Data/FakeData/y_data_ready2link.dta")
y.df <- select(y.df, y, id_y, year, f_name_nysiis, l_name_nysiis)

name_vars <- c("f_name_nysiis", "l_name_nysiis")
num_vars <- c("year")
xVars <- c("x1")
yVars <- c("y")

abe_single <-  abe_match(x.df, y.df, name_vars, xVars, yVars, age_band = 2, unique = TRUE, twoway=TRUE)
abe_multi <- abe_match(x.df, y.df, name_vars, xVars, yVars, age_band = 2, unique = FALSE, twoway=TRUE)
print(paste0("Switching from multiple to single ABE matching drops ", nrow(abe_multi) - nrow(abe_single), " observations"))

prl_single <- prl_match(x.df, y.df, name_vars, num_vars, unique = TRUE, thresh = 0.85)
prl_multi <-  prl_match(x.df, y.df, name_vars, num_vars, unique = FALSE, thresh = 0.85)


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
       

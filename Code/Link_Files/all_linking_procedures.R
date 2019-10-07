require(tidyverse)
require(fastLink)
require(phonics)
require(stargazer)
require(xtable)
require(gridExtra)
source("~/Desktop/3rdYrPaper/Code/Link_Files/link_files.R")
source("~/Desktop/3rdYrPaper/Code/Link_Files/abe_matching.R")
source("~/Desktop/3rdYrPaper/Code/Link_Files/prl_match.R")

######################################################
###   Part 1.  Perform linking  #############
######################################################

### LOAD IN GOLD, X AND Y DATA SETS ###################################
load("~/Desktop/3rdYrPaper/Code/Data/FakeData/gold_data_aug.RData")
load("~/Desktop/3rdYrPaper/Code/Data/FakeData/x_data_g.RData")
load("~/Desktop/3rdYrPaper/Code/Data/FakeData/y_data.RData")
load("~/Desktop/3rdYrPaper/Code/Data/MatchedData/first_best_data.RData")
y_data <- y_raw_data

### STANDARDIZE NAMES #########################################
x.df <- x_data %>% mutate(f_name_nysiis = nysiis(first, modified=TRUE), l_name_nysiis = nysiis(last, modified=TRUE)) %>% 
    select(id_x, x1, x2, f_name_nysiis, l_name_nysiis, year, g)
y.df <- y_data %>% mutate(f_name_nysiis = nysiis(first, modified=TRUE), l_name_nysiis =nysiis(last, modified=TRUE)) %>% 
  select(id_y, y, f_name_nysiis, l_name_nysiis, year)

#### Declare names of variables #####################
name_vars <- c("f_name_nysiis", "l_name_nysiis")
num_vars <- c("year")
x_vars <- c("id_x", "x1", "x2", "g")
y_vars <- c("id_y", "y")
outDir <- "~/Desktop/3rdYrPaper/Code/Data/MatchedData/"

### Master linking function returns list of matched datasets #######
match_list <- link_files(x.df, y.df, x_vars, y_vars, name_vars, num_vars, 
                       age_band=2, twoway=TRUE, thresh=0.6, outputDir=outDir, saveOut=TRUE)

## Save match list for analysis 
save(match_list, file=paste0(outDir,"match_list.RData"))

######################################################
###   Part 2.  Analyze quality of matches #############
######################################################

outDir <- "~/Desktop/3rdYrPaper/Figures/"
dataList <- c("ABE (Single)", "ABE (Multi)", "PRL (Single)", "PRL (Multi)")

# Make match rate table
match_rates <- NULL
for (i in 1:length(match_list)){
  data <- match_list[[i]]
  match_rates <- rbind(match_rates, data.frame(method = dataList[i], 
                           nMatches = nrow(data), 
                           pCorrect = round(sum(data$true_match)/nrow(data),3),
                           nUniqueX = nrow(distinct(data,id_x))))
}

writeLines(capture.output(print(xtable(match_rates), floating = FALSE, booktabs=TRUE, include.rownames = FALSE)), paste0(outDir,"match_rates.tex"))

# Make number of matches (L_i) histogram'
pdf(paste0(outDir, "match_hist.pdf"))
par(mfrow=c(1,1))
hist(match_list$abe_multi$n, col=rgb(1,0,0,0.5), 
     xlab="Number of matches", xlim=c(1,3), main="")
hist(match_list$prl_multi$n, col=rgb(0,0,1,0.5), add=T)
legend("topright", legend=c("ABE (Multi)", "PRL (Multi)"), fill=c(rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
box()
dev.off()

#### Compare naive OLS regressions #### #### #### #### #### 
lmObjects <- lapply(match_list, FUN = function(x) lm(y~x1+x2, data=x))
ols.best <- lm(y~x1+x2, data=first_best)

writeLines(capture.output(stargazer(ols.best, lmObjects, omit.stat=c("f", "ser", "adj.rsq"), 
                                    float = FALSE, 
                                    column.labels = c("First Best", dataList),
                                    covariate.labels = c("$\\beta_0$", "$\\beta_1$", "$\\beta_2$"),
                                    model.numbers=FALSE,
                                    intercept.bottom=FALSE)), paste0(outDir,"naive_ols.tex"))

###  Plot output of all matchings #### #### #### #### #### #### #### 
pdf(paste0(outDir, "match_plots.pdf"))
par(oma = c(2, 0, 0, 0))
par(mfrow=c(2,2))
for (i in seq_along(match_list)){
  m <- match_list[[i]]
  plot(first_best$x2, first_best$y, col=rgb(1,0,0,0.6), pch=16,cex=0.5, main=dataList[i], xlab="X2", ylab="y")
  points(m$x2, m$y, col=rgb(0,0,1,0.6), pch=16,cex=0.5) 
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", c("first best", "automated method"), xpd = TRUE, horiz = TRUE, 
       inset = c(0, 0), bty = "n", fill = c(rgb(1,0,0,0.6),rgb(0,0,1,0.6)))
dev.off()

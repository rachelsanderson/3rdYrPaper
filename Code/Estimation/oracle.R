library(stargazer)
load("~/Desktop/3rdYrPaper/Code/Data/MatchedData/first_best_data.RData")
load("~/Desktop/3rdYrPaper/Code/Data/MatchedData/match_list.RData")
outDir <- "~/Desktop/3rdYrPaper/Figures/"
dataList <- c("First Best", "ABE (Single)", "ABE (Multi)", "PRL (Single)", "PRL (Multi)")
lmObjects <- lapply(match_list, FUN = function(x) lm(y~x1+x2, data=x))
ols.best <- lm(y~x1+x2, data=first_best)

writeLines(capture.output(stargazer(ols.best, lmObjects, omit.stat=c("f", "ser"), 
                                    float = FALSE, 
                                    column.labels = dataList,
                                    model.numbers=FALSE)), paste0(outDir,"naive_ols.tex"))


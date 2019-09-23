library(tidyverse)
library(dplyr)
library(modelr)
source("~/Desktop/3rdYrPaper/Code/Estimation/ahl19.R")
source("~/Desktop/3rdYrPaper/Code/Estimation/lahiri_larsen.R")

dataDir <- "~/Desktop/3rdYrPaper/Code/Data/MatchedData/"

############ Import data and fix variable names ###############
setwd(dataDir)
myFiles <- list.files(pattern=".*csv")
for (f in myFiles){
  data <- read.csv(f) %>% select(-X)
  ############ Fix variable names ###############
  names(data) <- sapply(strsplit(names(data), split=".", fixed=TRUE), function(x)(x[2]))
  if ("n" %in% names(data)){  
    names(data)[names(data)=="n"] <- "L" 
  }else{ 
    data$L <- 1
  }
  ############ Run estimation procedures ###############
  ahl <- run_ahl(data)
  ll <- lahiri_larsen(data)
}

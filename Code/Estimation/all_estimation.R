library(tidyverse)
library(dplyr)
library(modelr)
source("~/Desktop/3rdYrPaper/Code/Estimation/ahl19.R")
source("~/Desktop/3rdYrPaper/Code/Estimation/lahiri_larsen.R")

dataDir <- "~/Desktop/3rdYrPaper/Code/Data/MatchedData/"

############ Import data and fix variable names ###############
setwd(dataDir)
myFiles <- list.files(pattern=".*csv")
estimates <- NULL
for (f in myFiles){
  data <- read.csv(f) %>% select(-X)
  ############ Fix variable names ###############
  names(data) <- sapply(strsplit(names(data), split=".", fixed=TRUE), function(x)(x[2]))
  if ("n" %in% names(data)){  
    names(data)[names(data)=="n"] <- "L" 
  }else{ 
    data$L <- 1
  }
  ############ Run estimation procedures, save in df ###############
  estOut <- estimate_everything(data)
  names(estOut) <- c("est_method", "dataset", "beta1", "se")
  estimates <- estimates %>% rbind(estOut)
  rownames(estimates) <- NULL
}

##########################################################################################
############ Helper function for running all estimation procedures ###############
##########################################################################################
estimate_everything <- function(data){
  estOut <- NULL
  fileName <- strsplit(f, '.csv')[1]
  ############### AHL (2019) ###############
  ahl <- run_ahl(data)
  estOut <- rbind(estOut, data.frame(est_method = "ahl", dataset = fileName, beta1 = ahl$coefficients[2], se = 0))
  ############### Larsen (2005) ###############
  ll <- lahiri_larsen(data)
  estOut <- rbind(estOut, data.frame(est_method = "ols", dataset = fileName, beta1 = ll$naive_ols[2], se = 0))
  estOut <- rbind(estOut, data.frame(est_method = "sw", dataset = fileName, beta1 = ll$sw_ols[2], se=0))
  estOut <- rbind(estOut, data.frame(est_method = "ll", dataset = fileName, beta1 = ll$ll_ols[2], se=0))
  ############### Goldstein ###############
  
  
  return(estOut)
}


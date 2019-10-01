library(tidyverse)
library(dplyr)
library(modelr)
source("~/Desktop/3rdYrPaper/Code/Estimation/ahl19.R")
source("~/Desktop/3rdYrPaper/Code/Estimation/lahiri_larsen.R")

dataDir <- "~/Desktop/3rdYrPaper/Code/Data/MatchedData/"

############ Import data and fix variable names ###############
setwd(dataDir)
myFiles <- list.files(pattern=".*csv")
estimates <- data.frame()
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
  estimates <- rbind(estimates, estimate_everything(data))
  rownames(estimates) <- NULL
}

##########################################################################################
############ Helper function for running all estimation procedures ###############
##########################################################################################
estimate_everything <- function(data){
  estOut <- data.frame()
  fileName <- as.character(strsplit(f, '.csv')[1])
  ############### AHL (2019) ###############
  ahl <- run_ahl(data)
  # estOut <- rbind(estOut, data.frame(est_method = "ahl", dataset = fileName, beta1 = ahl$coefficients[2], se = 0))
  ############### Larsen (2005) ###############
  ll <- lahiri_larsen(data)
  estOut <- rbind(estOut, make_rows("ols", fileName, ll$beta_n$beta, ll$beta_n$se))
  estOut <- rbind(estOut, make_rows("sw", fileName, ll$beta_sw$beta, ll$beta_sw$se))
  estOut <- rbind(estOut,  make_rows("ll", fileName, ll$beta_ll$beta, ll$beta_ll$se))
  ############### Goldstein ###############
  
  
  return(estOut)
}

make_rows <- function(method_name, file, betas, se){
  rows <- data.frame()
  for (i in 1:length(betas)){
    rows <- rbind(rows, data.frame(est_method = method_name, 
                           matching = file, 
                           param = paste0("beta", i), 
                           value = betas[i], 
                           se=se[i]))
  }
  return(rows)
}



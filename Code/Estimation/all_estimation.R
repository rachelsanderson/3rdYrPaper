library(tidyverse)
library(dplyr)
library(modelr)
source("~/Desktop/3rdYrPaper/Code/Estimation/ahl19.R")
source("~/Desktop/3rdYrPaper/Code/Estimation/lahiri_larsen.R")

dataDir <- "~/Desktop/3rdYrPaper/Code/Data/MatchedData/"
outDir <- "~/Desktop/3rdYrPaper/Figures/"
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

############ Make a nice table  ###############

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- c(paste0("& \\multicolumn{4}{c}{AHL} & \\multicolumn{4}{c}{SW} & \\multicolumn{4}{c}{LL} \\\\\n
                             \\cmidrule(lr){2-5}\\cmidrule(lr){6-9}\\cmidrule(lr){10-13} Parameter",
                    paste(rep("& ABE-M & ABE-S & PRL-M & PRL-S",3), sep="",collapse=""), "\\\\\n"))

writeLines(capture.output(print(xtable(make_table(estimates)), add.to.row=addtorow, floating=FALSE, include.colnames=FALSE, 
                                include.rownames=FALSE)),
           paste0(outDir,"estimates.tex"))

##########################################################################################
############ Helper function for running all estimation procedures ###############
##########################################################################################
estimate_everything <- function(data){
  estOut <- data.frame()
  fileName <- as.character(strsplit(f, '.csv')[1])
  ############### AHL (2019) ###############
  ahl <- run_ahl(data)
  estOut <- rbind(estOut, make_rows("ahl", fileName, ahl$betas, ahl$se))
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

##########################################################################################
############ Helper functions for printing estimates          ###############
##########################################################################################

make_table <- function(estimates){
  methodList <- as.matrix(c("ahl", "sw", "ll"))
  cols <- apply(methodList, 1, make_table_row)
  table <- do.call("cbind", cols)
  table <- cbind(c("$\\beta_1$", "", "$\\beta_2$", "", "$\\beta_3", ""),table)
  return(table)
}
make_table_row <- function(m){
  params <- as.matrix(c("beta1", "beta2", "beta3"))
  l <- apply(params, 1, make_table_entry, m)
  rows <- do.call("rbind",l) 
  return(rows[,2:5])
}
make_table_entry <- function(x,method){
  entry <- data.frame()
  entry <- rbind(entry, estimates %>% filter(est_method == method & param == x) %>% select(-se) %>% spread(matching, value))
  entry <- rbind(entry, estimates %>% filter(est_method == method & param == x) %>% select(-value) %>% mutate(param="se") %>% spread(matching, se))
  colnames(entry)[2] <- method
  entry <- entry %>% select(-est_method)
  return(entry)
}


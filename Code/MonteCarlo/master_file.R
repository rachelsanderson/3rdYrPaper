require(tidyverse)
require(fastLink)
require(phonics)
require(stringr)
source("~/Desktop/3rdYrPaper/Code/MonteCarlo/make_datasets.R")
source("~/Desktop/3rdYrPaper/Code/Link_Files/link_files.R")
source("~/Desktop/3rdYrPaper/Code/Link_Files/abe_matching.R")
source("~/Desktop/3rdYrPaper/Code/Link_Files/prl_match.R")
source("~/Desktop/3rdYrPaper/Code/Estimation/estimate_g.R")

## Setup
set.seed(1989)
nDatasets <- 10
nObs <- 1000
pX1 <- 0.5
pX <- 0.5 
numX <- nObs*pX
pErrors <- c(0.15, 0.3, 0.5)
betas <- c(2,0.5,1)
sig2 <- 2

dataDir = "~/Desktop/3rdYrPaper/Code/MonteCarlo/Datasets/"
matchDataDir <- "~/Desktop/3rdYrPaper/Code/Data/MatchedData/"
firstNameDict =  "~/Desktop/3rdYrPaper/Code/Simulate_Data/Dictionaries/first_names.txt"
lastNameDict = "~/Desktop/3rdYrPaper/Code/Simulate_Data/Dictionaries/last_names_short.txt"

name_vars <- c("f_name_nysiis", "l_name_nysiis")
num_vars <- c("year")
x_vars <- c("id_x", "x1", "x2", "g")
y_vars <- c("id_y", "y")
num_neighbor <- 10

est_g <- function (x.df, y.df){return(mean(y.df$y))}

# Step 1. Make datasets 
make_datasets(nDatasets, nObs, pX1, pX, dataDir, firstNameDict, lastNameDict, betas, sig2)

# Step 2. Link datasets and evaluate links 
evaluate_links <- as.data.frame()

for (i in 1:nDatasets){
  x.df <- read.csv(file=paste0(outputDir, "x_data_", i,".csv"))
  y.df <- read.csv(file=paste0(outputDir, "y_data_", i,".csv"))
  
  # standardize data
  x.df <- x.df %>% mutate(f_name_nysiis = nysiis(first, modified=TRUE), l_name_nysiis = nysiis(last, modified=TRUE))
  y.df <- y.df %>% mutate(f_name_nysiis = nysiis(first, modified=TRUE), l_name_nysiis =nysiis(last, modified=TRUE)) 
  
  # non parametric g(w) for AHL procedure
  x.df$g <- est_g(x.df, y.df)
  
  # TODO: tune these parameters
  match_list <- link_files(x.df, y.df, x_vars, y_vars, name_vars, num_vars, 
                           age_band=2, twoway=TRUE, thresh=0.7)
  
  # save the matchings 
  save(match_list, file=paste0(matchDataDir,"match_list_", i, ".RData"))
  
  # evaluate the links and add row to evaluate_links dataframe
  
} 

# Step 3. Estimate using linked datasets
estimate_datasets()

evaluate_links <- function(match_list, numX){
  for (i in 1:length(match_list)){
    
    # create metadata row for monte carlo iteration
    data <- match_list[[i]]
    data <- data %>% group_by(id_x) %>% mutate(contains_true = (sum(true_match)==1))
    row <- data.frame(method = names(match_list)[i], 
                      pMatchedX = length(unique(data$id_x))/numX,
                      nMatches = nrow(data),
                      type_i = sum(!data$true_match)/nrow(data),
                      type_ii = 1 - pMatchedX,
                      contains_true = data$contains_true)
    
    # if method returns multiple matches, evaluate also for each value of L
    if (grepl("multi",names(match_list)[2])){
      multi_row = data.frame(method=names(match_list)[i])
      # do for each value of L
    } else { 
      multi_row = NULL 
    }
  }
  return(row = row_generic, multi_row = multi_row)
}
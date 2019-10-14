########################################################################
## Import Libraries
########################################################################

require(tidyverse)
require(fastLink)
require(phonics)
require(stringr)
source("~/Desktop/3rdYrPaper/Code/MonteCarlo/make_datasets.R")
source("~/Desktop/3rdYrPaper/Code/MonteCarlo/match_datasets.R")
source("~/Desktop/3rdYrPaper/Code/MonteCarlo/do_estimation.R")
source("~/Desktop/3rdYrPaper/Code/MonteCarlo/make_perfect_data.R")

source("~/Desktop/3rdYrPaper/Code/Link_Files/link_files.R")
source("~/Desktop/3rdYrPaper/Code/Link_Files/abe_matching.R")
source("~/Desktop/3rdYrPaper/Code/Link_Files/prl_match.R")
source("~/Desktop/3rdYrPaper/Code/Estimation/estimate_g.R")
source("~/Desktop/3rdYrPaper/Code/Estimation/sw93.R")
source("~/Desktop/3rdYrPaper/Code/Estimation/ahl19.R")

########################################################################
## Set Directories
## dataDir = where to store monte carlo datasets (.csv files)
## matchDataDir = where to store matched datasets (.RData files)
## first/lastNameDict = text files with dictionary of names
########################################################################

dataDir = "~/Desktop/3rdYrPaper/Code/MonteCarlo/Datasets/"
matchDataDir <- "~/Desktop/3rdYrPaper/Code/MonteCarlo/Linked_Datasets/"
metaDataDir <-  "~/Desktop/3rdYrPaper/Code/MonteCarlo/MetaData/"
perfectDataDir <- "~/Desktop/3rdYrPaper/Code/MonteCarlo/GoldData/"
firstNameDict =  "~/Desktop/3rdYrPaper/Code/Simulate_Data/Dictionaries/first_names.txt"
lastNameDict = "~/Desktop/3rdYrPaper/Code/Simulate_Data/Dictionaries/last_names_short.txt"

########################################################################
## Set parameters for DGP and Monte Carlo
########################################################################

set.seed(1989)
nDatasets <- 1000
nObs <- 1000
pX1 <- 0.5
pX <- 0.5 
numX <- nObs*pX
pErrors <- c(0.15, 0.3, 0.5)
betas <- c(2,0.5,1)
sig2 <- 2

########################################################################
## Set parameters for matching step -- may need tuning 
########################################################################

name_vars <- c("f_name_nysiis", "l_name_nysiis")
num_vars <- c("year")
x_vars <- c("id_x", "x1", "x2", "g")
y_vars <- c("id_y", "y")
num_neighbor <- 10
thresh <- 0.7
age_band <- 2

########################################################################
## Set parameters for estimation step 
########################################################################

est_g <- function (x.df, y.df){return(mean(y.df$y))}

########################################################################
## Monte Carlo steps 
########################################################################

ptm <- proc.time()

# Step 1. Make nDatasets using DGP and name dictionaries
make_datasets(nDatasets, nObs, pX1, pX, dataDir, firstNameDict, lastNameDict, betas, sig2)

# Step 2. Link datasets and evaluate quality of links

matching.metadata <- match_datasets(est_g, nDatasets, dataDir, matchDataDir, thresh, age_band) 
save(matching.metadata, file = paste0(metaDataDir,"matching_metadata.RData"))

# Step 2.5 Create also the ground truth linked dataset
make_perfect_data(dataDir, perfectDataDir, nDatasets)

# Step 3. Estimate using linked datasets

estimates <- do_estimation(matchDataDir)
save(estimates, file = paste0(metaDataDir, "estimates.RData"))

# Step 4. Analyze the estimates (median absolute deviations, etc.)

proc.time() - ptm

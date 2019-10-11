#####################################################################
### make_datasets. R                                          
### Function for generating datasets for Monte Carlo
### Gold data set contains (wFirst, wLast, wYear) and (y, x1, x2) generated according to
###         y = b0 + b1 * x1 + b2 * x2 + e
###         eps ~ rnorm()
#####################################################################
### inputs are nDatasets = number of MC datasets to generate
###            nObs = number of observations per dataset
###            pX1 = prob(x1 = 1)
###            pX = prop. of obs in X dataset
###            pErrors = vector of error probabilities (pFirst, pLast, pYear)
###            outDir = where to save the datasets
###            fNameDict = dictionary of first names .txt
###            lNameDict = dictionary of last names .txt
###            betas = c(b0, b1, b2)    
###            sig2 = var(e)
###
### outputs are goldData_id, x_data_id, y_data_id 
### which are saved in outDir
#####################################################################

make_datasets <- function(nDatasets, nObs, pX1, pX, outDir, fNameDict, lNameDict, betas, sig2){
  firstNames <- read.delim(fNameDict, header=FALSE,sep="\n")
  lastNames <- read.delim(lNameDict, header=FALSE,sep="\n")
  for (i in 1:nDatasets){
    gold_data <- make_gold_data(nObs, pX1, firstNames, lastNames, betas, sig2)

    y_data <- make_y(gold_data)
    x_data <- make_x(gold_data, pX, pErrors)
    
    # save all datasets 
    write.csv(x_data, file=paste0(outDir,"x_data_",i,".csv"))
    write.csv(y_data, file=paste0(outDir,"y_data_",i,".csv"))
  }
}
 
### Below are helper functions for generating gold data and x- and y- datasets
make_gold_data <- function(nObs, pX1, fNames, lNames, beta, sig2){
  ids <- 1:nObs
  x1 <- rbernoulli(nObs, p=pX1)
  x2 <- sqrt(sig2)*rnorm(nObs)
  eps <- sqrt(sig2)*rnorm(nObs)
  y <- beta[1] + beta[2]*x1 + beta[3]*x2 + eps
  wFirst <- as.character(fNames[sample(nrow(fNames), nObs, replace=T),])
  wLast <- as.character(lNames[sample(nrow(lNames), nObs, replace=T),])
  birth_dates <- r_date_of_births(nObs, start = as.Date("1900-01-01"), end = as.Date("1925-12-31"))
  wYear <- as.numeric(format(birth_dates, format = "%Y"))
  gold_data <- data.frame(id = ids, x1 = x1, x2= x2, y = y, 
                          first = as.character(wFirst), 
                          last = as.character(wLast), 
                          year = wYear)
  return(gold_data)
}
make_bad_data <- function(gold_data, pX, pErrors){
  y_data <- make_y(gold_data)
  x_data <- make_x(gold_data)
  return(list(x_data = x_data, y_data = y_data))
}
make_y <- function(gold_data){
  y_data <- select(gold_data, -c(x1, x2))
  y_data$id_y <- y_data$id
  y_data <- select(y_data, -id)
  return(y_data)
}
make_x <- function(gold_data, pX, pErrors){
  # select pX proportion of observations for X datafile 
  numX <- pX*nrow(gold_data)
  x_data <- gold_data[sample(nrow(gold_data), numX, replace=F),] %>% select(-y)
  x_data$id_x <- x_data$id
  x_data <- select(x_data, -id)
                   
  # introduce errors     
  x_data$first <- perturb_name(x_data$first, pErrors[1])
  x_data$last <- perturb_name(x_data$last, pErrors[2])
  x_data$year <- perturb_year(x_data$year, pErrors[3])
  
  return(x_data)
}
perturb_name <- function(names, pError){
  vowels<- c("a","e","i","o","u","y")
  n <- length(names)
  badNames <- as.character(names)
  
  # half of errors are typos
  ind <- rbinom(n,1, prob = pError/2)
  badNames[ind == 1] <- str_replace(as.character(names)[ind == 1], "[aeiou]", sample(vowels,1))
  badNames[ind == 1] <- str_replace(badNames[ind == 1], "ll", "l")
  badNames[ind == 1] <- str_replace(badNames[ind == 1], "rr", "r")
  badNames[ind == 1] <- str_replace(badNames[ind == 1], "C", "K")
  badNames[ind == 1] <- str_replace(badNames[ind == 1], "ph", "f")
  badNames[ind == 1] <- str_replace(badNames[ind == 1], "e", "ee")

  # half of errors are missing letters 
  ind <- rbinom(n,1, prob = pError/2)
  charNames <- strsplit(badNames[ind==1], "")
  letters_to_drop <- lapply(charNames, sample, 1)
  badNames[ind == 1] <- str_replace(badNames[ind == 1], unlist(letters_to_drop), "")
  return(badNames)
}
perturb_year <- function(years, pError){
  n <- length(years)
  fake_years <- years + rbinom(n,1, prob = pError)*round(sqrt(2.5)*rnorm(n))
  return(fake_years)
}

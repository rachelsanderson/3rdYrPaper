# load libraries
require(dplyr)
require(lubridate)
require(stringr)
require(pracma)

# load in clean dataset
setwd("~/Desktop/3rdYrPaper/Code/")
load("gold_data.RData")

##############################
### USER SET PARAMS HERE ######

# choose favorite random seed 
set.seed(1989)

# set size of X dataset to include
propX <- 0.7  

# prob of normally distributed error
pErrorDay <- 0.02
pErrorMonth <- 0.02
pErrorYear <- 0.5

# prob of first/last name typographical error
pTypo <- 0.05
pDrop <- 0.03

##############################

numObs <- max(gold_data$id)
numX <- propX*numObs

# proportion of individuals to mess up
x_raw_data <- select(gold_data, -y)
y_raw_data <- select(gold_data, -x)
save(y_raw_data, file="y_raw_data.RData")
save(x_data_raw, file="x_data_raw.RData")     
            
# select random subset of x data
x_data <- x_raw_data[sample(1:numObs, numX, replace=F),]

# introduce random typos for each variable
# note that typos follow conditional independence assumption

fake_dates <- gen_fake_dates(x_data$bday, pErrorDay, pErrorMonth, pErrorYear)

# check your work
compare_dates <- data.frame(true = as.Date(x_data$bday), false = fake_dates)
compare_dates$match <- (compare_dates$true == compare_dates$false)
View(compare_dates)

# rewrite the bad dates in x dataset
x_data$day <- as.numeric(format(fake_dates, format = "%Y"))
x_data$month <- as.numeric(format(fake_dates, format = "%m"))
x_data$year <- as.numeric(format(fake_dates, format = "%d"))
x_data <- select(x_data,-bday)

# Introduce random typos in names
newNames <- gen_fake_names(x_data$first, x_data$last, pTypo, pDrop) 
compare_names <- data.frame(true = paste(x_data$first, x_data$last), false = paste(newNames$fNew, newNames$lNew))
compare_names$match <- (as.character(compare_names$true) == as.character(compare_names$false))
View(compare_names)
x_data$first <-newNames$fNew
x_data$last <- newNames$lNew
# x_data is now corrupt!

# TO DO: make augmented gold data, shows the typos as well 

save(x_data, file = "x_data.RData")

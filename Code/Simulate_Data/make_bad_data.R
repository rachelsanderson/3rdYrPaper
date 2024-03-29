# load libraries
require(tidyverse)
require(lubridate)
require(stringr)
library(stargazer)
library(gridExtra)
require(pracma)
require(foreign)
source("~/Desktop/3rdYrPaper/Code/Simulate_Data/gen_fake_dates.R")
source("~/Desktop/3rdYrPaper/Code/Simulate_Data/gen_fake_names.R")

# load in clean dataset
load("~/Desktop/3rdYrPaper/Code/Data/FakeData/gold_data.RData")

##############################
### USER SET PARAMS HERE ######
##############################

outputDir = "~/Desktop/3rdYrPaper/Code/Data/FakeData/"
figDir = "~/Desktop/3rdYrPaper/Figures/"
# choose favorite random seed 
set.seed(1989)

# set size of X dataset to include
propX <- 0.4

# prob of normally distributed error
pErrorDay <- 0.02
pErrorMonth <- 0.02
pErrorYear <- 0.5

# prob of first/last name typographical error
# change to first and last names! 
pFirst <- 0.07  # ABEFP (2019) say 7-14% of first names have 1+ char diff
pLast <- 0.17   # ABEFP (2019) say 17-32% of last names have 1 + char diff
pTypo <- 0.05
pDrop <- 0.03

##############################

numObs <- max(gold_data$id)
numX <- propX*numObs

# split gold data into x and y datasets
y_raw_data <- select(gold_data, -c(x1, x2))
y_raw_data$first <- as.character(y_raw_data$first)
y_raw_data$last <- as.character(y_raw_data$last)
y_raw_data$name <- paste(y_raw_data$first,y_raw_data$last )
y_raw_data$id_y <- y_raw_data$id
y_data <- y_raw_data %>% select(-id)
save(y_raw_data, file=paste0(outputDir, "y_data.RData"))

# select random subset of x data
indices <- sample(nrow(gold_data), numX, replace=F) 
first_best <- gold_data[sample(nrow(gold_data), numX, replace=F),]
save(first_best, file="~/Desktop/3rdYrPaper/Code/Data/MatchedData/first_best_data.RData")     
x_raw_data <- select(first_best, -y)
save(x_raw_data, file=paste0(outputDir,"x_data_raw.RData"))     
x_data <- select(x_raw_data, x2, x1)
x_data$id_x <- x_raw_data$id

# introduce random typos for each variable
# note that typos follow conditional independence assumption

# rewrite the bad dates in x dataset
fake_dates <- gen_fake_dates(x_raw_data$bday, pErrorDay, pErrorMonth, pErrorYear)
x_data$year <- as.numeric(format(fake_dates, format = "%Y"))
x_data$month <- as.numeric(format(fake_dates, format = "%m"))
x_data$day <- as.numeric(format(fake_dates, format = "%d"))
x_data$x_bday <- fake_dates

# Introduce random typos in names
newNames <- gen_fake_names(x_raw_data$first, x_raw_data$last, pTypo, pDrop) 
x_data$first <-newNames$fNew
x_data$last <- newNames$lNew
x_data$x_name <- paste(newNames$fNew,newNames$lNew)

# save corrupted x_data in R and STATA
save(x_data, file = paste0(outputDir,"x_data.RData"))
write.dta(x_data, paste0(outputDir, "x_data.dta"))

# check your work
compare_vars <- data.frame(true_date = as.Date(x_raw_data$bday), 
                             false_date = as.Date(x_data$x_bday),
                             true_name = paste(x_raw_data$first, x_raw_data$last),
                             false_name = x_data$x_name)
compare_vars$date_match <- (compare_vars$true_date == compare_vars$false_date)
compare_vars$name_match <- (as.character(compare_vars$true_name) == as.character(compare_vars$false_name))
View(compare_vars)
  
# save also gold data with augmented info
gold_data_aug <- gold_data %>% mutate(name = paste(first, last)) %>% 
                full_join(x_data[, c("id", "x_bday", "x_name")], by="id") %>%
                select(-c("first","last", "month","day","year"))

save(gold_data_aug, file = paste0(outputDir, "gold_data_aug.RData"))

gold.plot <- ggplot(data = gold_data, mapping=aes(x2, y, group=x1, colour=x1)) + 
  geom_point() + 
  scale_colour_discrete(name = "x1", labels=c("0","1")) + 
  labs(title = "Full 'gold' dataset", x = "x2", y = "y") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.8, 0.2))
first_best.plot <- ggplot(data = first_best, mapping=aes(x2, y, group=x1, colour=x1)) + 
  geom_point() + 
  scale_colour_discrete(name = "x1", labels=c("0","1")) + 
  labs(title = "First best 'gold' dataset", x = "x2", y = "y") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.8, 0.2))
l <- grid.arrange(gold.plot,first_best.plot, nrow=1)
ggsave(paste0(figDir,"gold_data_compare.pdf"), plot = l)

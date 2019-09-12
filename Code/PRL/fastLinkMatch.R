# load libraries
require(fastLink)
require(dplyr)
# load in clean dataset
setwd("~/Desktop/3rdYrPaper/Code/Simulated_Data/")

load("x_data.RData")
load("y_raw_data.RData")
y_data <- y_raw_data

prl <- fastLink(x_data, y_data, varnames = c("first","last","year","month","day"),
         stringdist.match = c("first", "last"),
         numeric.match = c("year","month","day"))

links<-cbind(x_data[prl$matches[,1],c("x","x_name","x_bday")], y_data[prl$matches[,2],c("y","name","bday")])


                    
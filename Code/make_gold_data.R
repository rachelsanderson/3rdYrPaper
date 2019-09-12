# import libraries
library(generator)
# set up local directories, dictionaries, etc. here
workingDir = "~/Desktop/3rdYrPaper/Code/"
firstNameDict =  "first_names.csv"
lastNameDict = "last_names.txt"

# read in dictionary of male/female names
firstNames <-read.csv(paste0(workingDir,firstNameDict), header=FALSE)
# firstNames <- read.delim(paste0(workingDir,firstNameDict), header=FALSE, sep="\n")
lastNames <- read.delim(paste0(workingDir,lastNameDict), header=FALSE,sep="\n")

# set params for generating data
numObs <- 500
beta <- 2 

# simulate ground truth data
ids <- 1:numObs
x <- 2*rnorm(numObs)
eps <- rnorm(numObs)
y <- x*beta + eps

# create random w_i 
wFirst <- as.character(firstNames[round(nrow(firstNames)*runif(numObs))+1,])
wLast <- as.character(lastNames[round(nrow(lastNames)*runif(numObs))+1,])
birth_dates <- r_date_of_births(numObs, start = as.Date("1900-01-01"), end = as.Date("1925-12-31"))
wYear <- as.numeric(format(birth_dates, format = "%Y"))
wMonth <- as.numeric(format(birth_dates, format = "%m"))
wDay <- as.numeric(format(birth_dates, format = "%d"))

# ground truth data
gold_data <- data.frame(id = ids, x = x, y = y, 
                        first = as.character(wFirst), 
                        last = as.character(wLast), 
                        year = wYear, 
                        month = wMonth, 
                        day = wDay, 
                        bday = as.character(birth_dates))

# plot the true data points
plot(gold_data$x, gold_data$y, type='p')

save(gold_data, file = "gold_data.RData")

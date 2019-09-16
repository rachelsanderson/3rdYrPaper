# import libraries
library(generator)
library(foreign)

# set up local directories, dictionaries, etc. here
outputDir = "~/Desktop/3rdYrPaper/Code/Data/FakeData/"
firstNameDict =  "~/Desktop/3rdYrPaper/Code/Simulate_Data/Dictionaries/first_names_short.csv"
lastNameDict = "~/Desktop/3rdYrPaper/Code/Simulate_Data/Dictionaries/last_names_short.txt"

# read in dictionary of male/female names
firstNames <-read.csv(firstNameDict, header=FALSE)
# firstNames <- read.delim(paste0(workingDir,firstNameDict), header=FALSE, sep="\n")
lastNames <- read.delim(lastNameDict, header=FALSE,sep="\n")

# set params for generating data
numObs <- 500
beta <- 2 

# simulate ground truth data
ids <- 1:numObs
x1 <- 2*rnorm(numObs)
eps <- rnorm(numObs)
y <- x1*beta + eps

# create random w_i 
wFirst <- as.character(firstNames[round(nrow(firstNames)*runif(numObs))+1,])
wLast <- as.character(lastNames[round(nrow(lastNames)*runif(numObs))+1,])
birth_dates <- r_date_of_births(numObs, start = as.Date("1900-01-01"), end = as.Date("1925-12-31"))
wYear <- as.numeric(format(birth_dates, format = "%Y"))
wMonth <- as.numeric(format(birth_dates, format = "%m"))
wDay <- as.numeric(format(birth_dates, format = "%d"))

# ground truth data
gold_data <- data.frame(id = ids, x1 = x1, y = y, 
                        first = as.character(wFirst), 
                        last = as.character(wLast), 
                        year = wYear, 
                        month = wMonth, 
                        day = wDay, 
                        bday = as.character(birth_dates))

# plot the true data points
plot(gold_data$x1, gold_data$y, type='p')

save(gold_data, file = paste0(outputDir, "gold_data.RData"))
write.dta(gold_data, paste0(outputDir, "gold_data.dta"))

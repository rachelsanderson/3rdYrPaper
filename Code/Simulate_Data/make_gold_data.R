# import libraries
library(generator)
library(foreign)
library(ggplot2)

# set up local directories, dictionaries, etc. here
outputDir = "~/Desktop/3rdYrPaper/Code/Data/FakeData/"
firstNameDict =  "~/Desktop/3rdYrPaper/Code/Simulate_Data/Dictionaries/first_names_short.csv"
lastNameDict = "~/Desktop/3rdYrPaper/Code/Simulate_Data/Dictionaries/last_names_short.txt"

# read in dictionary of male/female names
firstNames <-read.csv(firstNameDict, header=FALSE)
# firstNames.male <- as.character(firstNames[1:10,])
# firstNames.female <- as.character(firstNames[20:41,])

# read in dictionary of last names
lastNames <- read.delim(lastNameDict, header=FALSE,sep="\n")

# set params for generating data
set.seed(1454)
numObs <- 500
pDummy <- 0.5
beta <- c(2, 0.5, 1)

# simulate ground truth data
ids <- 1:numObs
male <- rbernoulli(numObs, p=pMale)
x2 <- sqrt(2)*rnorm(numObs)
eps <- sqrt(2)*rnorm(numObs)
y <- beta[1] + beta[2]*x1 + beta[3]*x2 + eps
ols <- lm(y~ x1 + x2)
summary(ols)

# create random w_i 
# generate first names based on value of x1 (in this case, gender)
# wFirst <- rep("", numObs)
# wFirst[x1] <- firstNames.male[sample(1:length(firstNames.male), sum(x1), replace=TRUE)]
# wFirst[!x1] <- firstNames.female[sample(1:length(firstNames.female), sum(!x1), replace=TRUE)]

wFirst <- as.character(firstNames[round(nrow(firstNames)*runif(numObs))+1,])
wLast <- as.character(lastNames[round(nrow(lastNames)*runif(numObs))+1,])
birth_dates <- r_date_of_births(numObs, start = as.Date("1900-01-01"), end = as.Date("1925-12-31"))
wYear <- as.numeric(format(birth_dates, format = "%Y"))
wMonth <- as.numeric(format(biDecerth_dates, format = "%m"))
wDay <- as.numeric(format(birth_dates, format = "%d"))

# ground truth data
gold_data <- data.frame(id = ids, x1 = x1, x2= x2, y = y, 
                        first = as.character(wFirst), 
                        last = as.character(wLast), 
                        year = wYear, 
                        month = wMonth, 
                        day = wDay, 
                        bday = as.character(birth_dates))

# plot the true data points
gold.plot <- ggplot(data = gold_data, mapping=aes(x2, y, group=x1, colour=x1)) + 
  geom_point() + 
  scale_colour_discrete(name = "x1", labels=c("0","1")) + 
  labs(title = "Full 'gold' dataset", x = "x2", y = "y") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.8, 0.2))
gold.plot
ggsave(paste0(outputDir,"gold_data.png"), plot = last_plot())
  
save(gold_data, file = paste0(outputDir, "gold_data.RData"))
write.dta(gold_data, paste0(outputDir, "gold_data.dta"))


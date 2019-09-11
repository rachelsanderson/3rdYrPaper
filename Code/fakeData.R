# import libraries

# set up local directories, dictionaries, etc. here
workingDir = "~/Desktop/3rdYrPaper/Code/"
firstNameDict =  "first-names.txt"
lastNameDict = "middle-names.txt"

# read in dictionary of male/female names
firstNames <- read.delim(paste0(workingDir,firstNameDict), header=FALSE, sep="\n")
lastNames <- read.delim(paste0(workingDir,lastNameDict), header=FALSE,sep="\n")

# set params for generating data
numObs <- 500
beta <- 2 

# simulate ground truth data
ids <- 1:numObs
x <- 2*rnorm(numObs)
eps <- rnorm(numObs)
y <- x*beta + eps
wFirst <- firstNames[round(nrow(firstNames)*runif(numObs)),]
wLast <- lastNames[round(nrow(lastNames)*runif(numObs)),]


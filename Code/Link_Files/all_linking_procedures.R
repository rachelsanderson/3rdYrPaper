require(dplyr)
source("~/Desktop/3rdYrPaper/Code/Link_Files/abe_matching.R")
source("~/Desktop/3rdYrPaper/Code/Link_Files/prl_match.R")

x.df <- read.dta("~/Desktop/3rdYrPaper/Code/Data/FakeData/x_data_ready2link.dta")
x.df <- select(x.df, x1, id_x, year, f_name_nysiis, l_name_nysiis)
y.df <- read.dta("~/Desktop/3rdYrPaper/Code/Data/FakeData/y_data_ready2link.dta")
y.df <- select(y.df, y, id_y, year, f_name_nysiis, l_name_nysiis)

abe_single <-  abe_match(x.df, y.df, age_band = 2, unique = TRUE, twoway=TRUE)
abe_multi <- abe_match(x.df, y.df, age_band = 2, unique = FALSE, twoway=TRUE)
print(paste0("Switching from multiple to single ABE matching drops ", nrow(abe_multi) - nrow(abe_single), " observations"))

prl_single <- prl_match(x.df, y.df, unique = TRUE, thresh = 0.85)
prl_multi <-  prl_match(x.df, y.df, unique = FALSE, thresh = 0.85)

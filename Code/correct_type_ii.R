require(tidyverse)
require(scales)
require(knitr)
require(kableExtra)
require(magrittr)
require(qwraps2)

matchDataDir <- "~/Desktop/3rdYrPaper/Code/MonteCarlo/Linked_Datasets/"
metaDataDir <- "~/Desktop/3rdYrPaper/Code/MonteCarlo/MetaData/"
figureDir <- "~/Desktop/3rdYrPaper/Figures/"
load(paste0(metaDataDir,"matching_metadata.RData"))
options(qwraps2_markup = "latex")

calc_type_ii <- function(matchDataDir){
  type_ii <- NULL
  files <- list.files(matchDataDir)
  for (f in files){
    load(paste0(matchDataDir,f)) # loads in matched datasets in variable called "match_list"
    mc_id <- parse_number(strsplit(f, split ="_")[[1]][3])
    type_ii <- rbind(type_ii, helper_type_ii(match_list, mc_id))
  }
  type_ii <- type_ii %>% arrange(mc_id)
  return(type_ii)
}

helper_type_ii <- function(match_list, mc_id){
  rows <- NULL

  for (i in 1:length(match_list)){
    
    # create metadata row for monte carlo iteration
    data <- match_list[[i]]
    data <- data %>% group_by(id_x) %>% mutate(contains_true = (sum(true_match)==1)) 
    data <- data %>% distinct(id_x, contains_true) %>% filter(contains_true == TRUE)
    type_ii <- (500 - nrow(data))/500
    
    rows <- rbind(rows, data.frame(mc_id = mc_id,
                                   method = names(match_list)[i], 
                                   type_ii = type_ii))
  }
  return(rows)
}

true_type_ii <- calc_type_ii(matchDataDir)
save(true_type_ii, file=paste0(metaDataDir, "true_type_ii.RData"))

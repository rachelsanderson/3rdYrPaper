####################################################################################
# match_files.R                                                                 ####
####################################################################################
# Code for linking datasets according to ABE and PRL procedures
# gives both multi and single matches
# takes as input x.df, y.df, which are dataframes with standardized variables
# x_vars = (id_x, all other x variables for regression)
# y_vars = (id_y, all other y variables for regression)
# name_vars = string variables for matching
# num_vars = numeric variables for matching
# age_band = +- search band for ABE algorithm; default is 2 yrs
# twoway indicates whether twoway matching is desired for ABE (default is true)
# thresh is posterior probability cutoff for designating pair as match in fastLink
# save flag will save all matchings to outputDir if provided
####################################################################################

match_files <- function(x.df, y.df, x_vars, y_vars, name_vars, num_vars, 
                       age_band=2, twoway=TRUE, thresh=0.85, outputDir = NULL, saveOut=FALSE){
  
  abe_single <-  abe_match(x.df, y.df, name_vars, x_vars, y_vars, age_band, unique = TRUE, twoway)
  abe_multi <- abe_match(x.df, y.df, name_vars, x_vars, y_vars, age_band, unique = FALSE, twoway)
  print(paste0("Switching from multiple to single ABE matching drops ", nrow(abe_multi) - nrow(abe_single), " observations"))
  prl_single <- prl_match(x.df, y.df, name_vars, num_vars, unique = TRUE, thresh = thresh)
  prl_multi <-  prl_match(x.df, y.df, name_vars, num_vars, unique = FALSE, thresh = thresh)
  print(paste0("Switching from multiple to single PRL matching drops ", nrow(prl_multi) - nrow(prl_single), " observations"))
  
  match_list <- list(abe_single = abe_single, 
                     abe_multi = abe_multi, 
                     prl_single = prl_single,
                     prl_multi = prl_multi)
  
  if (saveOut==TRUE){
    if (is.null(outputDir)){print("You must provide an output directory to save the file")}
    else{
      for (i in 1:length(match_list)){
        print(names(match_list)[i])
        write.csv(match_list[i], file = paste0(outputDir,names(match_list)[i],".csv"))
      }
    }
  }
  return(match_list)
}


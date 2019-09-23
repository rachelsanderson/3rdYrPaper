run_ahl <- function(data){
  
  ### Make factor vars for nonparam. reg
  data$f_name <- factor(data$f_name_nysiis, levels=unique(data$f_name_nysiis))
  data$l_name <- factor(data$l_name_nysiis, levels=unique(data$l_name_nysiis))
  
  # use nonparam regression to smooth Y values
  nonparam <- lm(data$y~data$f_name + data$l_name)
  
  newData <- data %>% group_by(id_x) %>% 
    add_predictions(nonparam, var="gHat") %>%
    mutate(sumY = sum(y),
           smoothY = sumY - (L-1)*gHat) %>%
    distinct(sumY, id_x, .keep_all = TRUE)
  
  # run diff regressions
  ahl <- lm(smoothY ~ x1, data = newData)
  # trueMatches <- lm(y ~ x1, data = data %>% filter(true_match==TRUE))
  # need to fix the standard errors
  return(ahl)
}









prl_match <- function(x.df, y.df, name_vars, num_vars, unique=TRUE, thresh=0.85){
  if (unique){ 
      # uses year only, but using month and day as well creates much better results
      prl <- fastLink(x.df, y.df, varnames = c(name_vars, num_vars),
                  stringdist.match = name_vars,
                  numeric.match = num_vars, threshold.match = thresh, return.all=FALSE)
  
      matches <- getMatches(x.df,y.df, prl, threshold.match = thresh) %>% mutate(true_match = (id_x == id_y))
  }else{ # return multiple matches, exceeding the threshold
  multi_prl <- fastLink(x.df, y.df, varnames = c(name_vars, num_vars),
                        stringdist.match = name_vars,
                        numeric.match = num_vars, dedupe.matches = FALSE, threshold.match = thresh)

  matches <- data.frame(cbind(x.df[multi_prl$matches$inds.a,], y.df[multi_prl$matches$inds.b,]), check.names = TRUE) %>%
    mutate(true_match = (id_x == id_y),
           posterior =  multi_prl$posterior) %>%
    filter(posterior >= thresh ) %>% add_count(id_x)
  }
  
  colnames(matches) <- gsub('.','_', colnames(matches), fixed=TRUE)

  return(matches)
}

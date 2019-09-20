require(dplyr)
require(fastLink)
prl_match <- function(x.df, y.df, unique=TRUE, thresh=0.85){
  if (unique){ 
      # uses year only, but using month and day as well creates much better results
      prl <- fastLink(x.df, y.df, varnames = c("f_name_nysiis","l_name_nysiis", "year"),
                  stringdist.match = c("f_name_nysiis","l_name_nysiis"),
                  numeric.match = c("year"), threshold.match = thresh, return.all=FALSE)
  
      matches <- getMatches(x.df,y.df, prl, threshold.match = thresh) %>% mutate(trueMatch = (id_x == id_y))
  }else{ # return multiple matches, exceeding the threshold
  multi_prl <- fastLink(x.df, y.df, varnames = c("f_name_nysiis","l_name_nysiis", "year"),
                        stringdist.match = c("f_name_nysiis","l_name_nysiis"),
                        numeric.match = c("year"), dedupe.matches = FALSE, threshold.match = thresh)

  matches <- data.frame(cbind(x.df[multi_prl$matches$inds.a,], y.df[multi_prl$matches$inds.b,]), check.names = TRUE) %>%
    mutate(trueMatch = (id_x == id_y),
           posterior =  multi_prl$posterior) %>%
    filter(posterior >= thresh ) %>% add_count(id_x)
  }
  return(matches)
}

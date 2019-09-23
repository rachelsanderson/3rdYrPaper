require(dplyr)
# input are name_vars <-- all string variables for matching
# x/yVar is vector of variables of interest in x/yFile 

abe_match <- function(x.df, y.df, name_vars, xVar, yVar, age_band=2, unique=TRUE, twoway=TRUE){
  
  id_x <- xVar[1]
  id_y <- yVar[1]
  match_vars <- c(name_vars, 'year')
  
  # de duplicate files in file A 
  x_unique <- x.df %>% distinct_(.dots = c(match_vars, xVar), .keep_all = TRUE)
  
  # look for exact matches w/in +- age band in file B 
  x_matches <- x_unique %>%  inner_join(y.df, by = name_vars) %>%
    mutate(age_diff = abs(year.x - year.y)) %>% 
    filter(age_diff <= age_band) %>% 
    group_by(id_x, age_diff) %>% 
    add_count()
  
  # if unique matching desired, drop non-unique matches
  if (unique) { 
    x_matches <- x_matches %>% group_by(id_x) %>% filter(n == 1 & age_diff == min(age_diff))
  }
  
  if (twoway){
    y_unique <- y.df %>% distinct_(.dots = c(match_vars, yVar), .keep_all = TRUE)
    y_matches <- y_unique %>%  inner_join(x.df, by = name_vars) %>%
      mutate(age_diff = abs(year.x - year.y)) %>% 
      filter(age_diff <= age_band) %>% 
      group_by(id_y, age_diff) %>% 
      add_count() 
    if(unique){
      y_matches <- y_matches %>% group_by(id_y) %>% filter(n == 1 & age_diff == min(age_diff))
    }
  matches <- inner_join(x_matches, y_matches, by = c(xVar, yVar, name_vars, "id_x", "id_y", "age_diff")) %>%
      select(-c("n.x", "n.y"))
  }
  matches <- matches %>% group_by(id_x) %>% add_count() %>% mutate(posterior = 1/n) %>% 
      mutate(true_match = (id_x == id_y))
  return(matches)
}

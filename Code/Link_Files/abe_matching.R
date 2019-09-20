require(dplyr)

abe_match <- function(x.df, y.df, age_band=2, unique=TRUE, twoway=TRUE){
  
  # de duplicate files in file A 
  x_unique <- x.df %>% distinct(f_name_nysiis, l_name_nysiis, year, x1, .keep_all = TRUE)
  
  # look for exact matches w/in +- age band in file B 
  x_matches <- x_unique %>%  inner_join(y.df, by = c("f_name_nysiis", "l_name_nysiis")) %>%
    mutate(age_diff = abs(year.x - year.y)) %>% 
    filter(age_diff <= age_band) %>% 
    group_by(id_x, age_diff) %>% 
    add_count()
  
  # if unique matching desired, drop non-unique matches
  if (unique) { 
    x_matches <- x_matches %>% group_by(id_x) %>% filter(n == 1 & age_diff == min(age_diff))
  }
  
  if (twoway){
    y_unique <- y.df %>% distinct(f_name_nysiis, l_name_nysiis, year, y, .keep_all = TRUE)
    y_matches <- y_unique %>%  inner_join(x.df, by = c("f_name_nysiis", "l_name_nysiis")) %>%
      mutate(age_diff = abs(year.x - year.y)) %>% 
      filter(age_diff <= age_band) %>% 
      group_by(id_y, age_diff) %>% 
      add_count() 
    if(unique){
      y_matches <- y_name_matches %>% group_by(id_y) %>% filter(n == 1 & age_diff == min(age_diff))
    }
  matches <- inner_join(x_matches, y_matches, by = c("x1", "y", "f_name_nysiis", "l_name_nysiis", "id_x", "id_y", "age_diff")) %>%
      select(-c("n.x", "n.y"))
  }
  matches <- matches %>% group_by(id_x) %>% add_count() %>% mutate(posterior = 1/n)
  return(matches)
}
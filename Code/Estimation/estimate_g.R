estimate_g <- function(x.df, y.df, name_vars, num_vars, num_neighbors){
  g <- apply(x.df, 1, nearest_neighbors, y = y.df, name_vars = name_vars, num_vars = num_vars, num_neighbors = num_neighbors)
  return(g)
}

nearest_neighbors <- function(x, y, name_vars, num_vars, num_neighbors){
  compare.mat <- do.call("rbind", apply(y, 1, calc_distance, x=x, name_vars = name_vars, num_vars=num_vars)) # compare to every obs in the y dataset
  compare.mat$y <- y$y
  norm_data <- apply(compare.mat[,3:5], 2, normalize)
  compare.mat[c('f_name', 'l_name','num_dist')] <- norm_data
  compare.mat <- compare.mat %>% mutate(sim_score = (1-f_name) + (1-l_name) + num_dist) %>%
                arrange(desc(sim_score)) %>% slice(1:num_neighbors)
  return(mean(compare.mat$y))
}

normalize <- function(x){
  num <- x - min(x)
  denom <- max(x) - min(x)
  return(num/denom)
}

calc_distance <- function(x, y, name_vars, num_vars){
  x.names <- x[name_vars]
  y.names <- y[name_vars]
  x.num <- x[num_vars]
  y.num <- y[num_vars]
  
  name.dist <- stringdist::stringdist(x.names, y.names, method = c("jw"))
  num.dist <- dist(rbind(x.num,y.num), method = "euclidean")
  return(data.frame(id_x = x['id_x'], id_y = y['id_y'], f_name = name.dist[1], l_name = name.dist[2], num_dist = as.numeric(num.dist)))
}

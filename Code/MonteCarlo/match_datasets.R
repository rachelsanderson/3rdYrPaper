match_datasets <- function(est_g, nDatasets, dataDir, matchDataDir, age_band = 2, thresh=0.7){
  
  matching_meta_data <- NULL
  multi_matching_meta_data <- NULL
  
  for (i in 1:nDatasets){
    x.df <- read.csv(file=paste0(dataDir, "x_data_", i,".csv"))
    y.df <- read.csv(file=paste0(dataDir, "y_data_", i,".csv"))
    
    # standardize data
    x.df <- x.df %>% mutate(f_name_nysiis = nysiis(first, modified=TRUE), l_name_nysiis = nysiis(last, modified=TRUE))
    y.df <- y.df %>% mutate(f_name_nysiis = nysiis(first, modified=TRUE), l_name_nysiis =nysiis(last, modified=TRUE)) 
    
    # non parametric g(w) for AHL procedure
    x.df$g <- est_g(x.df, y.df)
    
    # TODO: tune these parameters
    match_list <- link_files(x.df, y.df, x_vars, y_vars, name_vars, num_vars, 
                             age_band=2, twoway=TRUE, thresh=0.7)
    
    # save the matchings 
    save(match_list, file=paste0(matchDataDir,"match_list_", i, ".RData"))
    
    # evaluate the links and add row to evaluate_links dataframe
    meta.data <- evaluate_links(match_list, nrow(x.df), i)
    matching_meta_data <- rbind(matching_meta_data, meta.data$rows)
    multi_matching_meta_data <- rbind(multi_matching_meta_data, meta.data$multi_rows)
    
  }
  return(list(full_matching_meta = matching_meta_data, multi_matching_meta = multi_matching_meta_data))
}

evaluate_links <- function(match_list, numX, mc_id){
  rows <- NULL
  multi_rows <- NULL
  
  for (i in 1:length(match_list)){
    
    # create metadata row for monte carlo iteration
    data <- match_list[[i]]
    data <- data %>% group_by(id_x) %>% mutate(contains_true = (sum(true_match)==1))
    rows <- rbind(rows, data.frame(mc_id = mc_id,
                                   method = names(match_list)[i], 
                                   pMatchedX = length(unique(data$id_x))/numX,
                                   nMatches = nrow(data),
                                   type_i = sum(!data$true_match)/nrow(data),
                                   type_ii = 1-(length(unique(data$id_x))/numX) - mean(!data$contains_true),
                                   pContainsTrue = mean(data$contains_true)))
    
    # if method returns multiple matches, evaluate also for each value of L
    if (grepl("multi",names(match_list)[i])){
      
      df_summ <- data %>% group_by(n) %>%
        summarize(pContainsTrue = mean(contains_true))
      
      counts <- data %>% group_by(n) %>% add_count() %>% distinct(n, nn)
      
      df_summ$method = rep(names(match_list)[i], nrow(df_summ))
      df_summ$mc_id = rep(mc_id, nrow(df_summ))
      df_summ$counts = counts$nn
      names(df_summ)[1] <- "L"
      multi_rows <- rbind(multi_rows, df_summ)
    } 
  }
  return(list(rows = rows, multi_rows = multi_rows))
}
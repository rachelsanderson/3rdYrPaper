
do_estimation <- function(matchDataDir){
  estimates <- NULL
  files <- list.files(matchDataDir)
  for (f in files){
    load(paste0(matchDataDir,f)) # loads in matched datasets in variable called "match_list"
    mc_id <- parse_number(strsplit(f, split ="_")[[1]][3])
    estimates <- rbind(estimates, estimate_everything(match_list,mc_id))
  }
  estimates <- estimates %>% arrange(mc_id)
  return(estimates)
}


estimate_everything <-function(match_list,mc_id){
  estOut <- NULL
  for (i in 1:length(match_list)){
    data <- match_list[[i]]
    fileName <- names(match_list)[i]
    
    # fix names for AHL procedure
    if ("n" %in% names(data)){  
      names(data)[names(data)=="n"] <- "L" 
    }else{ 
      data$L <- 1
    }
    
    estOut <- rbind(estOut, make_rows("ahl", fileName, run_ahl(data), mc_id))
    estOut <- rbind(estOut, make_rows("sw", fileName, calc_beta_sw(data),mc_id))
    estOut <- rbind(estOut, make_rows("ols_naive", fileName, do_ols(data),mc_id))
    estOut <- rbind(estOut, make_rows("ols_true", fileName, do_ols(data %>% filter(true_match == 1)),mc_id))
    estOut <- rbind(estOut, make_rows("ols_L1", fileName, do_ols(data %>% filter(L == 1)),mc_id))
  }
  
  rownames(estOut) <- NULL
  return(estOut)
}

do_ols <- function(df){
  ols <- lm(y ~ x1 + x2, data = df)
  return(list(beta = ols$coefficients, se=sqrt(diag(vcov(ols)))))
}

make_rows <- function(method_name, file, beta_obj, mc_id){
  rows <- data.frame()
  betas <- beta_obj$beta
  se <- beta_obj$se
  for (i in 1:length(betas)){
    rows <- rbind(rows, data.frame(mc_id = mc_id,
                                   est_method = method_name, 
                                   matching = file, 
                                   param = paste0("beta", i), 
                                   value = betas[i], 
                                   se=se[i]))
  }
  return(rows)
}


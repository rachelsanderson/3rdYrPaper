make_perfect_data <- function(dataDir, nDatasets){
  
}

for (i in 1:nDatasets){
  x.df <- read.csv(file=paste0(dataDir, "x_data_", i,".csv"))
  y.df <- read.csv(file=paste0(dataDir, "y_data_", i,".csv"))
  x.df$g <- est_g(x.df, y.df)
  x.df$id <- x.df$id_x
  y.df$id <- y.df$id_y
  linked.df <- left_join(x.df,y.df, by="id")
  if(sum(complete.cases(linked.df)) != 500){
    print("houston we have a problem") 
  }
  save(linked.df, file=paste0("perfect_match_")
}


  # ols.opt <- lm(y ~ x1 + x2, data=linked.df)
  # betas <- ols.opt$coefficients
  # se <- sqrt(diag(vcov(ols.opt)))
  # for (b in 1:3){
  #   beta_opt <- rbind(beta_opt, data.frame(mc_id = i,
  #                                          est_method = "beta_opt",
  #                                          matching = "perfect",
  #                                          param = paste0("beta",b-1),
  #                                          value = betas[b],
  #                                          se = se[b],
  #                                          ))
  # }
}
# rownames(beta_opt) <- NULL
# save(beta_opt, file=paste0(metaDataDir, "beta_opt.RData"))

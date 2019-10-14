make_perfect_data <- function(dataDir, perfectDataDir, nDatasets){
  betaOpt <- NULL
  for (i in 1:nDatasets){
    x.df <- read.csv(file=paste0(dataDir, "x_data_", i,".csv"))
    y.df <- read.csv(file=paste0(dataDir, "y_data_", i,".csv"))
    x.df$id <- x.df$id_x
    y.df$id <- y.df$id_y
    linked.df <- left_join(x.df,y.df, by="id")
    if(sum(complete.cases(linked.df)) != 500){
      print("houston we have a problem") 
    }
    betaOpt <- rbind(betaOpt, make_rows("ols_gold", fileName, do_ols(data), mc_id))
  }
  save(betaOpt, paste0(perfectDataDir, "betaOpt.RData"))
}
do_ols <- function(df){
  ols <- lm(y ~ x1 + x2, data = df)
  return(list(beta = ols$coefficients, se=sqrt(diag(vcov(ols)))))
}



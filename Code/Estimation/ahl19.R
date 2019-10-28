run_ahl <- function(data){
  
  # g(w) is included in data, estimated in previous step 
  newData <- data %>% group_by(id_x) %>%
    mutate(sumY = sum(y),
           smoothY = sumY - (L-1)*g) %>%
    distinct(sumY, id_x, .keep_all = TRUE)
  
  N <- nrow(newData)
  
  # run first stage to estimate var(eps)
  first.stage <- lm(smoothY ~ x1 + x2, data=newData)
  sig2.eps <- sum(first.stage$residuals^2)/(N-3)
  sig2.v <- var(data$y - data$g)
  
  # run diff regressions
  newData <- newData %>% mutate(sigX = sig2.eps + (L-1)*sig2.v)
  wls <- lm(smoothY ~ x1+x2, weights = 1/sigX, data=newData)
  se <- sqrt(diag(vcov(wls)))
  return(list(beta = wls$coefficients, se = se))
}


new_ahl <- function(data){
  
  # instead of g we sort by L 
  data <- data %>% group_by(id_x) %>% mutate (sumY = sum(y))
  data_L1 <- data %>% filter(L == 1) 
  data_L2 <- data %>% filter(L == 2) %>% distinct(id_x, .keep_all=TRUE)
  data_L3 <- data %>% filter(L == 3) %>% distinct(id_x, .keep_all=TRUE)
  
  gmm <- optim(c(1,1,1), new_ahl_moments, data_L1 = data_L1,
                                  data_L2 = data_L2, data_L3 = data_L3)

  
  
  return(list(beta = wls$coefficients, se = se))
}


new_ahl_moments <- function(data_L1, data_L2, data_L3, beta){
  m1 <- 2*calc_moment(data_L2, beta) - calc_moment(data_L3, beta)
  m2 <- calc_moment(data_L1,beta)
  m <- rbind(m1,m2) 
  return(t(m)%*%m)
}

opt_weights <- function(data_L1, data_L2, data_L3, beta){
  rbind(2*calc_moment_i(data_L1, beta) - calc_moment_i(data_L2, beta),
        calc_moment_i(data_L3, beta))
}

calc_moment <- function(df, beta){
  n <- nrow(df)
  return(t(cbind(rep(1, n), df$x1, df$x2)) %*% 
           as.matrix(df$sumY - beta[1]*rep(1,n) - beta[2]*df$x1
                     - beta[3]*df$x2)/n)
}

calc_moment_i <- function(df, beta){
  n <- nrow(df)
  m11 <- (df$sumY - beta[1]*rep(1,n) - beta[2]*df$x1 - beta[3]*df$x2)
  m12 <- (df$sumY - beta[1]*rep(1,n) - beta[2]*df$x1 - beta[3]*df$x2)*df$x1
  m13 <- (df$sumY - beta[1]*rep(1,n) - beta[2]*df$x1 - beta[3]*df$x2)*df$x2
  return(cbind(m11/n, m12/n, m13/n))
}











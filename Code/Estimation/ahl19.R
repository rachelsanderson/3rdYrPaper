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










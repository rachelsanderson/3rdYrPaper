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
  # X <- cbind(rep(1, nrow(newData)), newData$x1/(newData$sigX), newData$x2/sqrt(newData$sigX))
  # y <- newData$smoothY/sqrt(newData$sigX)
  # beta.ahl <- solve(t(X)%*%X)%*%t(X)%*%y
  # sig <- t(y-X%*%beta.ahl)%*%(y-X%*%beta.ahl)/(N-5)
  
  wls <- lm(smoothY ~ x1+x2, weights = sigX, data=newData)
  se <- sqrt(diag(vcov(wls)))
  # trueMatches <- lm(y ~ x1, data = data %>% filter(true_match==TRUE))
  # need to fix the standard errors
  return(list(betas = first.stage$coefficients, se = sqrt(diag(vcov(first.stage)))))
}










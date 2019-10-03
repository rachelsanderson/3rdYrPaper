run_ahl <- function(data){
  
  ### Make factor vars for nonparam. reg
  data$f_name <- factor(data$f_name_nysiis, levels=unique(data$f_name_nysiis))
  data$l_name <- factor(data$l_name_nysiis, levels=unique(data$l_name_nysiis))
  
  # use nonparam regression to smooth Y values
  nonparam <- lm(data$y~data$f_name + data$l_name)
  sig2.v <- sum(nonparam$residuals^2)/(N-3)
  
  newData <- data %>% group_by(id_x) %>% 
    add_predictions(nonparam, var="gHat") %>%
    mutate(sumY = sum(y),
           smoothY = sumY - (L-1)*gHat) %>%
    distinct(sumY, id_x, .keep_all = TRUE)
  
  N <- nrow(newData)
  
  # run first stage to estimate var(eps)
  first.stage <- lm(smoothY ~ x1 + x2, data=newData)
  sig2.eps <- sum(first.stage$residuals^2)/(N-3)
  
  # run diff regressions
  newData <- newData %>% mutate(sigX = sig2.eps + (L-1)*sig2.v)
  X <- cbind(rep(1, nrow(newData)), newData$x1/(newData$sigX), newData$x2/sqrt(newData$sigX))
  y <- newData$smoothY/sqrt(newData$sigX)
  beta.ahl <- solve(t(X)%*%X)%*%t(X)%*%y
  sig <- t(y-X%*%beta.ahl)%*%(y-X%*%beta.ahl)/(N-5)
  
  wls <- lm(smoothY ~ x1+x2, weights = sigX, data=newData)
  se <- sqrt(diag(vcov(wls)))
  # trueMatches <- lm(y ~ x1, data = data %>% filter(true_match==TRUE))
  # need to fix the standard errors
  return(list(betas = wls$coefficients, se = se))
}









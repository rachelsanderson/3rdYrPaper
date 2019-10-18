N<-10000
mu<-1
kappa<-6
sig1 <- 2
sig2 <- 3
pi <- 0.7
Z1 <- sqrt(sig1)*rnorm(N) + mu
Z2 <- sqrt(sig2)*rnorm(N) + kappa

gen_data <- function(pi){
  ind <- (runif(N) < pi)
  
  X1 <- rep(0,N)
  X1[ind] <- Z1[ind]
  X1[!ind] <- Z2[!ind]
  
  X2<- rep(0,N)
  X2[ind] <- Z2[ind]
  X2[!ind] <- Z1[!ind]
  return(list(x1 = X1, x2 = X2))
}

dta <- gen_data(0.5)
X1 <- dta$x1
X2 <- dta$x2

varX1 <- function(pi){
  return(pi*sig1 + (1-pi)*sig2 + pi*(1-pi)*(mu-kappa)^2)
}

varX2 <- function(pi){
  return((1-pi)*sig1 + (pi)*sig2 + pi*(1-pi)*(mu-kappa)^2)
}
covX12 <- function(pi){
  return((1-pi^2-(1-pi)^2)*mu*kappa - pi*(1-pi)*(mu^2+ kappa^2))
}

optD <- function(pi){
  num <- var(X2)/(1-pi)^2 - cov(X1,X2)/(pi*(1-pi))
  denom <- var(X1)/pi^2 + var(X2)/(1-pi)^2 - 2*cov(X1,X2)/(pi*(1-pi))
  return(num/denom)
}

mu1 <- function(pi){
  return(mean(X1/pi) - ((1-pi)/pi)*kappa)
}

mu2 <- function(pi){
  return(mean(X2/(1-pi)) - (pi/(1-pi))*kappa)
}

optMu <- function(pi){
  dStar <- optD(pi)
  return(dStar*mu1(pi) + (1-dStar)*mu2(pi))
}

trueP <- 0.7
beliefs <- seq(0.1, 0.9, by=0.05)
mu_1 <- sapply(beliefs, mu1)
mu_2 <- sapply(beliefs, mu2)
optMus <- sapply(beliefs, optMu)

mse <- function(pi){
  dStar <- optD(pi)
  muHat <- optMu(pi)
  bias <- (mu - muHat)^2
  vmu1 <- var(X1)/(pi^2)
  vmu2 <- var(X2)/((1-pi)^2)
  vmu12 <- cov(X1,X2)/(pi*(1-pi))
  var <- (dStar^2)*vmu1 + ((1-dStar)^2)*vmu2 + dStar*(1-dStar)*vmu12
  return(list(mse = bias + var, bias = bias, var = var))
}

mseout<- sapply(beliefs,mse)
plot(x=beliefs, y=mseout[1,], type='l', ylim=c(0,10))
lines(x=beliefs, y=mseout[2,], col='green')
lines(x=beliefs, y=mseout[3,], col='blue')
legend("bottomleft",legend=c("mse","bias","var"),col=c('black','green','blue'),lty=1, cex=0.8)

     
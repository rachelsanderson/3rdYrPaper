sig2 <- 2
omeg2 <- 2
mu <- 0
kappa <- 1
pi <- seq(0,1,0.1)

var_mu <- function(sig2,omeg2,mu,kap, pi){
  x <- pi*sig2 + (1-pi)*omeg2 + pi*(1-pi)*(mu-kap)^2
  return(x/(pi^2))
}


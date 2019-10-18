sig2 <- 2
omeg2 <- 2
mu <- 0
kappa <- 1
pi <- seq(0,1,0.1)

var_mu <- function(sig2,omeg2,mu,kap, pi){
  x <- pi*sig2 + (1-pi)*omeg2 + pi*(1-pi)*(mu-kap)^2
  return(x/(pi^2))
}

var1 <- var_mu(sig2, omeg2, mu, kappa, pi)
var2 <- var_mu(sig2, omeg2, mu, kappa, 1-pi)

# equal variances
plot(y=var1, x= pi, type='l')
lines(y=var2, x=pi)

# sig2 ≠ omeg2 just increases the y axis
sig2 <- 4
omeg2 <- 1
var1 <- var_mu(sig2, omeg2, mu, kappa, pi)
var2 <- var_mu(sig2, omeg2, mu, kappa, 1-pi)
plot(y=var1, x= pi, type='l')
lines(y=var2, x=pi)
# always cross at pi = 0.5; just see the y axis inc with sig2
# difference between means enters symmetrically!!! 
sig2 <- 2
omeg2 <- 2
mu <- 100
kap <- 0
var1 <- var_mu(sig2, omeg2, mu, kappa, pi)
var2 <- var_mu(sig2, omeg2, mu, kappa, 1-pi)
plot(y=var1, x= pi, type='l')
lines(y=var2, x=pi)

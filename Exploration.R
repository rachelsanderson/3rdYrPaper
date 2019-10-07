N <- 10000
pi <- 0.8
y1.is.true <- rbernoulli(N,pi)
mu <- 0
sig2 <- 1
kappa <- 1
nu2 <- 2

trueY <- rnorm(N, mu,sig2)
fakeY <- rnorm(N, kappa, nu2)
y1 <- y1.is.true*trueY + (1-y1.is.true)*fakeY
y2 <- (1-y1.is.true)*trueY + (y1.is.true)*fakeY
var.y1 <- var(y1)
var.y2 <- var(y2)
cov.y12 <- cov(y1,y2)

avar <- function(a1){
  t.1 <- (a1^2)*var.y1 
  t.2 <- ((1-a1*pi)/(1-pi))^2*var.y2
  t.12 <- ((2*a1*(1-a1*pi))/(1-pi))*cov.y12
  return(t.1+t.2-t.12)
}

get_a2 <- function(a1){
  return((1-a1*pi)/(1-pi))
}

get_a3 <- function(a1){
  return((a1-2*a1*pi + pi)/(1-pi))
}

a1.init <- 10
opt <- optim(a1.init, avar)
avar <- opt$val
a1.opt <- opt$par
a2.opt <-get_a2(a1.opt)
a3.opt <- get_a3(a1.opt)

a.opt <- c(a1.opt,a2.opt,a3.opt)
round(a.opt,3)

num <- pi*var.y2 + cov.y12
denom <- (1-pi)*var.y1+ (pi^2)*var.y2 + 2*pi*cov.y12
num/denom

calc_bias <- function(a){
  return(a[1]*(pi*mu + (1-pi)*kappa) + a[2]*((1-pi)*mu+pi*kappa) - a[3]*kappa)
}

bias <- calc_mu(a.opt)

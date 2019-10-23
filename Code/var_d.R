library(purrr)
set_pars <- function(parArray){
  return(list(pi = parArray[1], sig2=parArray[2], omeg2 = parArray[3], mu=parArray[4], kappa=parArray[5]))
}

### TESTING DATA TO CHECK FORMULAS
par <- set_pars(0.5, 1, 1, 0, 1)
N<-10000
trueX <- sqrt(par$sig2)*rnorm(N) + par$mu
fakeX <- sqrt(par$omeg2)*rnorm(N) + par$kappa
x1.is.true <- rbernoulli(N,p=par$pi)
x1 <- x1.is.true*trueX + (1-x1.is.true)*fakeX
x2 <- (1-x1.is.true)*trueX + (x1.is.true)*fakeX
var.x1 <- var(x1)
var.x2 <- var(x2)
cov.x12 <- cov(x1,x2)
v_mu1_r <- var.x1/(par$pi^2)
v_mu2_r <- var.x2/(1-par$pi)^2
cov_mu12_r <- cov.x12/(par$pi*(1-par$pi))

calc_d_star <- function(par){
  var_mu <- function(x) {(x*par$sig2 + (1-x)*par$omeg2 + x*(1-x)*(par$mu-par$kappa)^2)/(x^2)}
  v_mu1 <- var_mu(par$pi)
  v_mu2 <- var_mu(1-par$pi)
  cov_x12 <- (1-par$pi^2 - (1-par$pi)^2)*par$mu*par$kappa - par$pi*(1-par$pi)*(par$mu^2 + par$kappa^2)
  cov_mu12 <- cov_x12/(par$pi*(1-par$pi))
  d_star <- (v_mu2 - cov_mu12)/(v_mu1 + v_mu2 - 2*cov_mu12)
  return(d_star)
}

# see how d_star varies with parameters?

gen_title <- function(pars){
  return(bquote(sigma^2 == .(pars$sig2) ~ ", " ~
         omega^2 == .(pars$omeg2) ~", "~
         mu == .(pars$mu) ~ ", "~
         kappa == .(pars$mu)))
}

parCombos <- list(c(1,1,0,1),
                c(1,20,0,1),
                c(20,1,0,1),
                c(1,20,0,10),
                c(20,1,10,0))
piList <- seq(0.01, 0.5, by = 0.01)

get_dList <- function(parCombo){
  sapply(piList, function(x){ calc_d_star(set_pars(c(x, parCombo))) } )
}

d_combos <- lapply(parCombos, get_dList)

plot(x=piList, y=d_combos[[1]], type = 'l', col = 'blue', ylab=bquote(d^"*"), xlab=bquote(pi==~"Pr(" ~ X[1]~ "is drawn from correct distribution)"))

sapply(1:length(d_combos), FUN = function(i) { lines(x=piList, y = d_combos[[i]], type='l',col=i)})

lines(x=piList, y =  d_pi_sig2_large, type='l', col='green')
lines(x=piList, y =  d_pi_omeg2_kappa_large, type='l', col='orange')
title(main = "Optimal dstar as a function of pi \n and different parameter combos")
legend('topleft', legend = (eval(gen_title(set_pars(0.5, 1,1,0,1 )))), col = c("blue"), lty=1, lwd=2)


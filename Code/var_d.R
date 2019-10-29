library(purrr)
library(dplyr)
library(tidyverse)
library(knitr)
library(kableExtra)

outDir = "Desktop/3rdYrPaper/Figures/"

set_pars <- function(parArray){
  return(list(pi = parArray[1], 
              mu=parArray[2], 
              sig2=parArray[3],  
              kappa=parArray[4], 
              omeg2 = parArray[5]))
}

### TESTING DATA TO CHECK FORMULAS
par <- set_pars(c(0.4, 1, 20, 0, 1))
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

sig2 <- 2
beta <- c(2,0.5,1)
x1 <- rbernoulli(N, 0.5)
x2 <- sqrt(2)*rnorm(N)
x0 <- rep(1, N)
eps <- sqrt(2)*rnorm(N)
y = beta[1]*rep(1,N) + beta[2]*x1 + beta[3]*x2 + eps
var(y)

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
  return(as.expression(bquote(mu == .(pars[1]) ~ ", "~
                                sigma^2 == .(pars[2]) ~ ", " ~
                                kappa == .(pars[3]) ~ "," ~
                                omega^2 == .(pars[4]))))
}


# (sig2, omeg2, mu, kappa)
parCombos <- list(c(0,1,1,1), #equivariance
                  c(0,1,1,2),
                  c(0,4,1,2),
                  c(0,1,4,2),
                  c(0,1,1,10))
piList <- seq(0.01, 0.5, by = 0.01)

get_dList <- function(parCombo){
  return(sapply(piList, function(x){ calc_d_star(set_pars(c(x, parCombo))) } ))
}

d_combos <- lapply(parCombos, get_dList)
cols <- rainbow(length(parCombos))

pdf(paste0(outDir, "dStar.pdf"))
par(mfrow=c(1,1))
plot(x=piList, y=d_combos[[1]], type = 'l', col = 'blue', ylab=bquote(d^"*"), xlab=bquote(pi==~"Pr(" ~ X[1]~ "is drawn from correct distribution)"))
sapply(1:length(d_combos), FUN = function(i) { lines(x=piList, y = d_combos[[i]], type='l',col=cols[i], lwd=2)})
# title(main=bquote("Optimal"~d^"*"~"as a function of "~pi[0]~"and different parameters"))
legend('topleft', legend = sapply(parCombos, gen_title),
       col = cols, lty=1, lwd=2, pt.cex =1, cex=1.2)
dev.off()

################################################################################################################################
#### Now we consider thought experiment.  Suppose we have guess piHat, but it's wrong.  
#### Calculate optimal weights and get mse of estimator getting it wrong
################################################################################################################################
### 1.  Bias :  Helper functions for bias calculation
#############################################
calc_bias <- function(piHat, par){
  d <- calc_d_star(set_pars(c(piHat, unlist(par[-1]))))
  eX <- function(x) { x*par$mu + (1-x)*par$kappa }
  eMu1 <- eX(par$pi)/piHat - ((1-piHat)/(piHat))*par$kappa
  eMu2 <- eX(1-par$pi)/(1-piHat) - (piHat)/(1-piHat)*par$kappa
  eMu <- d*eMu1 + (1-d)*eMu2
  return(abs(eMu - par$mu))
}
get_bias_list <- function(truePi,parCombo){
  sapply(piList, function(x){ calc_bias(x, set_pars(c(truePi, parCombo))) } )
}
plot_bias <- function(truePi, parCombos){
  bias_list <- lapply(parCombos, get_bias_list, truePi=truePi)
  plot(x=piList, y=bias_list[[1]], type = 'l', col = 'black', 
       ylab="Bias", 
       xlab=bquote(hat(pi)),
       ylim = c(min(unlist(bias_list)), max(unlist(bias_list))))
  sapply(1:length(bias_list), FUN = function(i) { lines(x=piList, y = bias_list[[i]], type='l',col=cols[i])})
  title(main = bquote(pi[0]==.(truePi)))
}

# Plot bias for different values of truePi
truePiList <- seq(0.1, 0.5, by = 0.1)
pdf(paste0(outDir,"bias_plot.pdf"))
par(mfrow=c(3,2), mai = c(0.6, 0.8, 0.3, 0.4))
for (truePi in truePiList){
  plot_bias(truePi, parCombos)
}
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend('center', xpd=TRUE, legend = sapply(parCombos, gen_title),
       col = cols, lty=1, lwd=2, pt.cex =1, cex=1.3)
dev.off()

#################################################################################################
### 2.  Variance :  Helper functions for variance calculation
################################################################################################

calc_variance <- function(piHat, par){
  d <- calc_d_star(set_pars(c(piHat, unlist(par[-1]))))
  var_x <- function(x) {(x*par$sig2 + (1-x)*par$omeg2 + x*(1-x)*(par$mu-par$kappa)^2)}
  var_mu1 <- var_x(par$pi)/(piHat^2) # true variance/false beliefs 
  var_mu2 <- var_x(1-par$pi)/((1-piHat)^2)
  cov_x12 <- (1-par$pi^2 - (1-par$pi)^2)*par$mu*par$kappa - par$pi*(1-par$pi)*(par$mu^2 + par$kappa^2)
  cov_mu12 <- cov_x12/(piHat*(1-piHat)) #true covariance/false beliefs
  var_mu <- (d^2)*var_mu1 + ((1-d)^2)*var_mu2 +2*d*(1-d)*cov_mu12
  return(var_mu)
}

get_var_list <- function(parCombo, truePi){
  sapply(piList, function(x){ calc_variance(x, set_pars(c(truePi, parCombo))) } )
}
plot_variance <- function(truePi, parCombos){
  var_list <- lapply(parCombos, get_var_list, truePi=truePi)
  plot(x=piList, y=var_list[[1]], type = 'l', col = 'black', 
       ylab="Bias", 
       xlab=bquote(hat(pi)),
       ylim = c(min(unlist(var_list)), max(unlist(var_list))))
  sapply(1:length(var_list), FUN = function(i) { lines(x=piList, y = var_list[[i]], type='l',col=cols[i])})
  title(main = bquote(pi[0]==.(truePi)))
}


# Plot variance of one obs for different values of truePi
pdf(paste0(outDir,"var_plot.pdf"))
par(mfrow=c(3,2), mai = c(0.6, 0.8, 0.3, 0.2))
for (truePi in truePiList){
  plot_variance(truePi, parCombos)
}
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend('center', xpd=TRUE, legend = sapply(parCombos, gen_title),
       col = cols, lty=1, lwd=2, pt.cex =1, cex=1.3)
dev.off()

#################################################################################################
### 3.  MSE :  Helper functions for MSE calculation
################################################################################################

calc_mse <- function(bias, var, N){
  return(bias^2 + var/N)
}

plot_mse <- function(truePi, parCombos, n){
  mse_list <- lapply(parCombos, function(x){ calc_mse(get_bias_list(x, truePi), get_var_list(x, truePi), N=n)})
  plot(x=piList, y=mse_list[[1]], type = 'l', col = 'black', 
       ylab="MSE", 
       xlab=bquote(hat(pi)),
       ylim = c(min(unlist(mse_list)), max(unlist(mse_list))))
  sapply(1:length(mse_list), FUN = function(i) { lines(x=piList, y = mse_list[[i]], type='l',col=i)})
  title(main = bquote(pi[0]==.(truePi)))
}

nList <- c(1,100,1000)
for (n in nList){
  png(paste0(outDir,"mse_n",n,".png"))
  par(mfrow=c(3,2), mai = c(0.6, 0.8, 0.3, 0.2))
  for (truePi in truePiList){
    plot_mse(truePi, parCombos[-c(3:5)],n)
  }
  plot(1, type="n", axes=FALSE, xlab="", ylab="")
  legend('center', xpd=TRUE, legend = sapply(parCombos[-5], gen_title),
         col = 1:length(parCombos), lty=1, lwd=2, pt.cex =1, cex=1.3)
  dev.off()
}

#################################################################################################
### 4. Compare this all to the esitmator that takes d = 0.5, piHat = 0.5 (i.e. AHL estimator)
################################################################################################

calc_var_ahl <- function(par){
  # print(unlist(par))
  var_x <- function(x) {(x*par$sig2 + (1-x)*par$omeg2 + x*(1-x)*(par$mu-par$kappa)^2)}
  # print(paste0("vX1: ", var_x(par$pi), " vX2: ", var_x(1-par$pi)))
  cov_x12 <- (1-par$pi^2 - (1-par$pi)^2)*par$mu*par$kappa - par$pi*(1-par$pi)*(par$mu^2 + par$kappa^2)
  # print(paste0("cov: ", cov_x12))
  return(var_x(par$pi) + var_x(1-par$pi) + 2*cov_x12)
}
# 
# try <- function(x){
#   v1 <- (x*par$sig2 + (1-x)*par$omeg2 + x*(1-x)*(par$mu-par$kappa)^2)
#   v2 <- (1-x)*par$sig2 + x*par$omeg2 + x*(1-x)*(par$mu-par$kappa)^2
#   cov_x12 <- (1-x^2 - (1-x)^2)*par$mu*par$kappa - x*(1-x)*(par$mu^2 + par$kappa^2)
#   print(paste0("v1: ",v1, " v2: ", v2, " cov_x12: ", cov_x12))
#   return(v1 + v2 + 2*cov_x12)
# }


plot_var_ahl <- function(parCombo){
  var_list_ahl <- sapply(truePiList, function(x){ calc_var_ahl(set_pars(c(x, parCombo)))})
  plot(x=truePiList, y=var_list_ahl, type = 'l', col = 'black', 
       ylab="Variance/MSE", 
       xlab=bquote(pi[0]),
       ylim = c(min(unlist(var_list_ahl)), max(unlist(var_list_ahl))))
}

par(mfrow=c(3,2), mai = c(0.6, 0.8, 0.3, 0.2))
for (combo in parCombos){
  plot_var_ahl(combo)
}

par(mfrow=c(3,2), mai = c(0.6, 0.8, 0.3, 0.2))

plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend('center', xpd=TRUE, legend = sapply(parCombos, gen_title),
       col = 1:length(parCombos), lty=1, lwd=2, pt.cex =1, cex=1.3)



############


# df$bias <- apply(df, 1, FUN=function(x){x$set_pars(c(df$truePi,parCombos[[4]])))

make_df <- function(parCombo, N){
  
  # Intialize DF
  df <- data.frame(truePi=rep(seq(0.1,0.9, by=0.1), each=5),
                   piHat=rep(seq(0.05,0.5, by=0.05), 9))
  
  # Calculate bias/var of optimal thing with bad beliefs
  df$bias <- apply(df[,c('truePi','piHat')], 1, 
                   FUN = function(x) { calc_bias(x[2],set_pars(c(x[1],parCombo)))})
  df$var <- apply(df[,c('truePi','piHat')], 1, 
                  FUN = function(x) { calc_variance(x[2],set_pars(c(x[1],parCombo)))})    
  df$mse <- (df$bias)^2 + df$var/N
  df$var_ahl <- sapply(df$truePi, FUN = function(x){calc_var_ahl(set_pars(c(x, parCombo)))})
  df$mse_ahl <- df$var_ahl/N
  df$ratio <- (unlist(df$mse_ahl))/df$mse
  df$N <- N
  df <- df %>% select(c(piHat, truePi, ratio)) %>%
    spread(key = piHat, value = ratio) 
  return(df)
}

df_1 <- make_df(parCombos[[2]], 1)
df_2 <- make_df(parCombos[[2]], 1000)

par(mfrow=c(1,1))
plot(x = df$truePi-df$piHat, df_1$ratio, type = 'p')
hist(df_1$ratio)

title_1 = "MSE ratio for $\\hat{\\mu}^*$ and $\\hat{\\mu}^{AHL}$ for $N=1,000$ and $(\\mu, \\sigma^2, \\kappa, \\omega^2) = ($"

for (i in 1:length(parCombos)){
  pars <- parCombos[[i]]
  df <- round(make_df(pars, 1000),3)
  colnames(df)[1] <- "$\\pi$"
  title <- paste0(title_1, pars[1],",", pars[2], ",", pars[3], ",", pars[4], ")")
  kab <- kable(df,"latex", booktabs=T, 
               escape=FALSE,
               caption=title,
               linesep = "", align=c("c")) %>% kable_styling() %>%
          add_header_above(c(" " =1, "$\\\\hat{\\\\pi}}$" = 5), escape = FALSE)
  writeLines(kab, paste0(outDir, "compare_",i,".tex"))
}


title_1 = "MSE ratio for $\\hat{\\mu}^*$ and $\\hat{\\mu}^{AHL}$ for $N="
title_2 = "$ and $(\\mu, \\sigma^2, \\kappa, \\omega^2) = ($"

nList <- c(10,100,1000)
for (n in 1:length(nList)){
  pars <- parCombos[[2]]
  df <- round(make_df(pars, nList[n]),3)
  colnames(df)[1] <- "$\\pi$"
  title <- paste0(title_1, nList[n], title_2, pars[1],",", pars[2], ",", pars[3], ",", pars[4], ")")
  kab <- kable(df,"latex", booktabs=T, 
               escape=FALSE,
               caption=title,
               linesep = "", align=c("c")) %>% kable_styling() %>%
    add_header_above(c(" " =1, "$\\\\hat{\\\\pi}}$" = 5), escape = FALSE)
  writeLines(kab, paste0(outDir, "compare_",nList[n],".tex"))
}


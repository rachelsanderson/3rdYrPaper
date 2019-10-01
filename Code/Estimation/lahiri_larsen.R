####################################################################################
# lahiri_larsen.R                                                               ####
####################################################################################
# Code for implementing LL (2005) unbiased GLM estimator
# Model is for y = (y1, .. yN)'
#         y_i = x_i'Beta + eps_i
# But (y_i, x_i) not observed; instead researcher has access to (z_i, x_i), with
#         z_i = y_i with prob q_ii
#             = y_j with prob q_ij
# Naive OLS estimator is biased
#         Beta_n = (X'X)^(-1)X'z
# Use instead the Lahiri/Larsen unbiased estimator
#         Beta_LL = (W'W)^(-1)W'z 
# where W = (w1, w2, ... wN), w_i = q_i'X = sum_j q_{ij} x_j' 
#
# Function takes as input a df with (x_i, y_i1) (x_i, y_i2) ... (x_i, y_iL) entries
# each with an associated posterior prob p_iL (not necessarily normalized to sum to 1)
# 
####################################################################################

lahiri_larsen <- function(df){
  
  # normalize probabilities
  df <- df %>% group_by(id_x) %>%
    mutate(q = posterior/sum(posterior)) 
  
  return(list(beta_n = calc_beta_naive(df), beta_sw = calc_beta_sw(df, beta_n$beta), beta_ll = calc_beta_ll(df)))
}

calc_beta_naive <- function(df){
  ## Naive OLS does not account for fact that some observations are duplicated, merely regresses y~x1+x2 
  N <- nrow(df)
  X <- cbind(rep(1, nrow(df)), df$x1, df$x2)
  y <- df$y
  XX <- solve(t(X)%*%X)
  beta_naive <-XX%*%t(X)%*%y
  sig2 <- t(y-X%*%beta_naive)%*%(y-X%*%beta_naive)/(N-3)
  se_naive <- diag(as.numeric(sig2)*XX)
  return(list(beta = beta_naive, se = se_naive))
}

calc_beta_sw <- function(df, beta_naive){
  # B = (q_ij1 - 1) z_j1 + q_ij2 z_j2
  
  # Code for constructing B
  qij <- df %>% group_by(id_x) %>% arrange(id_x, desc(q)) %>% slice(1:2) %>% select(id_x, q)
  zij <- df %>% group_by(id_x) %>% arrange(id_x,desc(q)) %>% slice(1:2) %>% select(id_x, y)
  Bhat <- data.frame(id_x = qij$id_x, q = qij$q, Bval = (qij$q-1) * zij$y) %>% group_by(id_x) %>%
    mutate(Bhat = sum(Bval)) %>% 
    distinct(id_x, Bhat) %>% 
    ungroup() %>% select(Bhat)
  
  # Code for selecting highest posterior prob. match
  z <- make_z(df)
  
  # Code for calculating beta_sw and its standard error
  x_vals <- arrange(df,id_x) %>% distinct(id_x, x1, x2)
  X <- cbind(rep(1, nrow(x_vals)), x_vals$x1, x_vals$x2)
  XX <- solve(t(X)%*%X)
  B <- as.matrix(Bhat)
  z <- as.matrix(z$y)
  N <- nrow(df)
  beta_sw <- beta_naive - XX %*% t(X) %*% B
  sig2 <- (t(z)%*%z - 2*t(B)%*%z + t(B)%*%B - t(beta_sw)%*%t(X)%*%X%*%beta_sw)/(N-3)
  se_sw <- diag(as.numeric(sig2) * XX)
  return(list(beta = beta_sw, se = se_sw))
}

make_z <- function(df){
  z <- df %>% group_by(id_x) %>% 
    arrange(id_x) %>% 
    filter(q == max(q)) %>%
    sample_n(size=1) %>% ungroup %>% select(y)
  return(as.matrix(z))
}

calc_beta_ll <- function(df){
 # need to alter
  
  z <- df %>% group_by(id_x) %>% 
    arrange(id_x) %>% 
    filter(q == max(q)) %>%
    sample_n(size=1) %>% ungroup %>% select(y, id_y, id_x, q)
  
  qij <- df %>% group_by(id_x) %>% arrange(id_x, desc(q)) %>% slice(1:2) %>% select(id_x, id_y, q)
  
  x12 <- df %>% group_by(id_y) %>% 
    mutate(qY = posterior/sum(posterior)) %>% 
    arrange(id_y, desc(qY)) %>% 
    slice(1) %>% 
    select(id_y, x1, x2, qY) 
  
  merged <- qij %>% left_join(x12, by = "id_y") %>% 
    arrange(id_x) %>%
    group_by(id_x) %>%
    mutate(w1 = sum(q*x1), w2=sum(q*x2)) %>%
    distinct(id_x, w1, w2)
  
  W <- as.matrix(cbind(rep(1, nrow(merged)), merged$w1, merged$w2))
  WW <- solve(t(W)%*%W)
  beta_ll <-WW%*%t(W)%*%as.matrix(z$y)
  
  return(list(beta = beta_ll))
}

calc_var_bU <- function(){
  Sigma <- calc_sigma(W)
  WW <- solve(t(W)%*%W)
  var_bU <- WW %*% t(W) %*% Sigma %*% W %*% WW
}

calc_sigma <- function(){
  
}

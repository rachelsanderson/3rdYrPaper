####################################################################################
# sw93.R
# B = (q_ij1 - 1) z_j1 + q_ij2 z_j2
####################################################################################

calc_beta_sw <- function(df){
  # normalize probabilities
  df <- df %>% group_by(id_x) %>%
    mutate(q = posterior/sum(posterior)) 
  
  # Code for selecting highest posterior prob. match
  z <- make_z(df)
  
  temp_df <- distinct(df,id_x, .keep_all = TRUE) %>% arrange(id_x)
  temp_df$z <- z
  
  beta <- lm(z ~ x1+ x2, data = temp_df)$coefficients
  
  # Code for constructing B
  qij <- df %>% group_by(id_x) %>% arrange(id_x, desc(q)) %>% slice(1:2) %>% select(id_x, q)
  zij <- df %>% group_by(id_x) %>% arrange(id_x,desc(q)) %>% slice(1:2) %>% select(id_x, y)
  Bhat <- data.frame(id_x = qij$id_x, q = qij$q, Bval = (qij$q-1) * zij$y) %>% group_by(id_x) %>%
    mutate(Bhat = sum(Bval)) %>% 
    distinct(id_x, Bhat) %>% 
    ungroup() %>% select(Bhat)
  
  # make X data matrix
  x_vals <- arrange(df,id_x) %>% distinct(id_x, x1, x2)
  X <- cbind(rep(1, nrow(x_vals)), x_vals$x1, x_vals$x2)
  XX <- solve(t(X)%*%X)
  
  B <- as.matrix(Bhat)
  N <- nrow(df)
  beta_sw <- beta - XX %*% t(X) %*% B
  sig2 <- (t(z)%*%z - 2*t(B)%*%z + t(B)%*%B - t(beta_sw)%*%t(X)%*%X%*%beta_sw)/(N-3)
  se_sw <- diag(as.numeric(sig2) * XX)
  
  return(list(beta=beta_sw, se= se_sw))
}

make_z <- function(df){
  z <- df %>% group_by(id_x) %>% 
    arrange(id_x) %>% 
    filter(q == max(q)) %>%
    sample_n(size=1) %>% ungroup() %>% select(y)
  return(as.matrix(z))
}

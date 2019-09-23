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
  
  return(list(naive_ols = calc_beta_naive(df), sw_ols = calc_beta_sw(df, naive_ols), ll_ols = calc_beta_ll(df)))
}

calc_beta_naive <- function(df){
  df_naive <- df %>% group_by(id_x) %>% 
    filter(q == max(q)) %>%
    sample_n(size=1) 
  beta_n <- lm(y ~ x1, data = df_naive)
  return(beta_n$coefficients)
}

calc_beta_sw <- function(df, naive_ols){
  qij <- df %>% group_by(id_x) %>% arrange(id_x, desc(q)) %>% slice(1:2) %>% select(id_x, q)
  z <- df %>% group_by(id_x) %>% arrange(id_x,desc(q)) %>% slice(1:2) %>% select(id_x, y)
  Bhat <- data.frame(id_x = qij$id_x, z = z$y, q = qij$q, Bval = (qij$q-1) * z$y) %>% group_by(id_x) %>%
    mutate(Bhat = sum(Bval)) %>% 
    distinct(id_x, Bhat) %>% 
    ungroup() %>% select(Bhat)
  x_vals <- arrange(df,id_x) %>% distinct(id_x, x1)
  X <- cbind(rep(1, nrow(x_vals)), x_vals$x1)
  beta_sw <- naive_ols - solve(t(X)%*%X) %*% t(X) %*% as.matrix(Bhat) 
  return(beta_sw)
}


calc_beta_ll <- function(df){
  z <- df %>% group_by(id_x) %>% 
    arrange(id_x) %>% 
    filter(q == max(q)) %>%
    sample_n(size=1) %>% ungroup %>% select(y)
  
  qij <- df %>% group_by(id_x) %>% arrange(id_x, desc(q)) %>% slice(1:2) %>% select(id_x, id_y, q)
  x12 <- df %>% group_by(id_y) %>%
    mutate(qY = posterior/sum(posterior)) %>% 
    arrange(id_y, desc(qY)) %>% slice(1) %>% 
    select(id_y, x1) 
  W <- inner_join(qij, x12, by = "id_y") %>% group_by(id_x) %>%
        mutate(W = sum(q*x1)) %>% distinct(id_x, W) %>% ungroup() %>% select(W)

  beta_ll <- lm(as.matrix(z)~as.matrix(W))
  return(beta_ll$coefficients)
}

# make_What <- function(df){
#   # I THINK THERE PAPER ASSUMES N X AND N Y BUT THAT DOESNT NECESSARILY HAPPEN
#   qij <- df %>% group_by(id_x) %>% arrange(id_x, desc(q)) %>% slice(1:2) %>% select(id_x, id_y, q)
#   x12 <- df %>% group_by(id_y) %>%
#     mutate(qY = posterior/sum(posterior)) %>% 
#     arrange(id_y, desc(qY)) %>% slice(1:2) %>% 
#     select(id_y, x1) %>% add_count()
#   full_join(x=qij, y=x12, by="id_y") %>% filter_all(any_vars(is.na(.)))
#   xj2 <- df %>% group_by(id_y) %>% arrange(id_y, desc(q)) %>% slice(2) %>% select(id_y, id_x, x1)
#   # What = qij*yj2 + 
#   
# }


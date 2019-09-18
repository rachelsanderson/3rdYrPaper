
load("~/Desktop/3rdYrPaper/Code/Data/MatchedData/data_prl_multiMatch.RData")
load("~/Desktop/3rdYrPaper/Code/Data/MatchedData/data_prl_singleMatch.RData")

prl.multi <- data_prl_multiMatch %>% group_by(id_x) %>% 
            mutate(q = posterior/sum(posterior))

N <- length(unique(prl.multi$id_x))
X <- cbind(rep(1,N), unique(prl.multi$x1))
z <- make_z(prl.multi)
 
make_z <- function(df){
  if("q" %in% names(df)){
    # select max posterior as the match 
    df <- df %>% select(-n) %>% group_by(id_x) %>% 
          filter(q == max(q)) %>% 
          add_count() 
    
    single_z <- df %>% filter(n==1) 
    random_z <- df %>% filter(n>1)
    
  }else{
    
  }
}





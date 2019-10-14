require(tidyverse)
require(scales)
require(knitr)
require(kableExtra)
require(magrittr)
require(qwraps2)

metaDataDir <- "~/Desktop/3rdYrPaper/Code/MonteCarlo/MetaData/"
figureDir <- "~/Desktop/3rdYrPaper/Figures/"
load(paste0(metaDataDir,"estimates.RData"))
options(qwraps2_markup = "latex")

# Code for plotting the histogram of the estimators
levels(estimates$matching) <- c("ABE~Single","ABE~Multi","PRL~Single","PRL~Multi")
levels(estimates$param) <- c(expression(beta[1]==2), expression(beta[2]==0.5), expression(beta[3]==1))
levels(estimates$est_method) <- c("AHL", "SW", "NaiveOLS", "OLSTrue","OLS(L=1)")
est_methods <- levels(estimates$est_method)
plot_estimate_hist <- function(method){
  
  results <- estimates %>% filter(est_method == method)
  
  if (method == "SW"){
    results <- results %>% filter(matching == "ABE~Multi" | matching == "PRL~Multi")
  }
  
  mean_betas <- results %>% group_by(matching, param) %>% 
        summarise(mean_beta = mean(value))
  
  
  temp <- ggplot(results, aes(x=value, fill=matching, color=matching)) + 
    geom_histogram(position = "identity", alpha=0.5, show.legend=FALSE, bins = 50) + 
    geom_vline(aes(xintercept = mean_beta), linetype="dashed",
    data=mean_betas,
    show.legend=FALSE) + 
    facet_grid(rows = vars(matching), cols=vars(param), 
               labeller=label_parsed,
               scales="free") +
    labs(x = "Parameter Estimate", y = "Frequency", title = paste0(method, " Estimator")) +
    labs(caption = "*Based on 1,000 simulations. Vertical line indicates the sample mean.") +
    theme(plot.caption =element_text(hjust=0),
          plot.title = element_text(hjust=0.5))
  ggsave(paste0(figureDir,method,"_hist.pdf"), plot=temp)
}
lapply(est_methods, plot_estimate_hist)

# Code for making table of meta information about the estimators
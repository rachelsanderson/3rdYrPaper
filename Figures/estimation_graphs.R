require(tidyverse)
require(scales)
require(knitr)
require(kableExtra)
require(magrittr)
require(qwraps2)

metaDataDir <- "~/Desktop/3rdYrPaper/Code/MonteCarlo/MetaData/"
figureDir <- "~/Desktop/3rdYrPaper/Figures/"
load(paste0(metaDataDir,"estimates.RData"))
load(paste0(metaDataDir,"beta_opt.RData"))
options(qwraps2_markup = "latex")

# Code for plotting the histogram of the estimators
levels(estimates$matching) <- c("ABE~Single","ABE~Multi","PRL~Single","PRL~Multi")
levels(estimates$param) <- c(expression(beta[0]==2), expression(beta[1]==0.5), expression(beta[2]==1))
levels(estimates$est_method) <- c("AHL", "SW", "OLS (All)", "OLS (True)","OLS (Single)")
est_methods <- levels(estimates$est_method)

results <- estimates %>% filter(est_method == "SW" | est_method == "AHL" )
results <- results %>% filter(matching == "ABE~Multi" | matching == "PRL~Multi")

temp <- ggplot(results, aes(x=value, fill=est_method, color=est_method)) + 
    geom_density(alpha=0.3) + 
    facet_grid(rows = vars(matching), cols=vars(param), 
               labeller=label_parsed,
               scales="free") +
    labs(x = "Parameter Estimate", y = "Density", fill = "", 
         color = "") +
    theme(plot.caption =element_text(size=12),
          plot.title = element_text(hjust=0.5),
          legend.position = "bottom")


ggsave(paste0(figureDir,"ahl_sw.pdf"), temp)

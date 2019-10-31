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
levels(estimates$matching) <- c("Deterministic~(Single)",
                                "Deterministic~(Multi)",
                                "Probabilistic~(Single)",
                                "Probabilistic~(Multi)")
levels(estimates$param) <- c(expression(beta[0]==2), expression(beta[1]==0.5), expression(beta[2]==1))
levels(estimates$est_method) <- c("AHL", "SW", "OLS (All)", "OLS (True)","OLS (Single)")
est_methods <- levels(estimates$est_method)

plot_estimate_hist <- function(method){
  
  results <- estimates %>% filter(est_method == method)
  
  if (method == "SW"){
    results <- results %>% filter(matching == "ABE~Multi" | matching == "PRL~Multi")
  }
  
  mean_betas <- results %>% group_by(matching, param) %>% 
    summarise(mean_beta = mean(value))
  
  
  temp <- ggplot(results, aes(x=value, fill=matching, color=matching)) + 
    geom_density(alpha=0.3, show.legend=FALSE) + 
    # geom_vline(aes(xintercept = mean_beta), linetype="dashed",
    #            data=mean_betas,
    #            show.legend=FALSE) + 
    facet_grid(rows = vars(matching), cols=vars(param), 
               labeller=label_parsed,
               scales="free") +
    labs(x = "Parameter Estimate", y = "Frequency") +
    labs(caption = "*Based on 1,000 simulations.") +
    theme(plot.caption =element_text(hjust=0, size=12),
          plot.title = element_text(hjust=0.5))
  ggsave(paste0(figureDir,method,"_hist.pdf"), plot=temp)
}

lapply(est_methods, plot_estimate_hist)

# Plot comparing (L=1) vs. True Matches 
t <- estimates %>% filter(est_method=="OLS (True)" | est_method == "OLS (All)" |
                            est_method=="OLS (Single)")
tPlot <- ggplot(t, aes(x=value, fill=est_method, color=est_method)) +
  geom_density(alpha=0.3) + 
  facet_grid(rows = vars(matching), cols = vars(param),
             labeller=label_parsed,
             scales="free") +
  labs(x = "Parameter Estimate", y = "Frequency", 
       fill = "", color="") +
  theme(legend.position="bottom")

ggsave(paste0(figureDir,"compare.pdf"), plot=tPlot)

# Code for making table of meta information about the estimators
est_tab <- estimates %>% group_by(est_method, param, matching) %>% 
  summarise(mad = mad(value))

est_tab <- spread(est_tab, key = est_method, value = mad) %>% arrange(matching)
levels(est_tab$param) <- c("$\\beta_0$", "$\\beta_1$", "$\\beta_2$")
names(est_tab)[1] <- "Parameter"
est_tab[, 3:ncol(est_tab)] <- round(est_tab[, 3:ncol(est_tab)], 3)

est_tab_out <- kable(est_tab[,-2], "latex", booktabs = T, 
                     align=c("l", rep("c", ncol(est_tab)-1)),
                     escape=FALSE) %>%
  pack_rows("Deterministic (Single)", 1,3) %>%
  pack_rows("Deterministic (Multiple)", 4,6) %>%
  pack_rows("Probabilistic (Single)", 7, 9) %>%
  pack_rows("Probabilistic (Multiple)", 10, 12) %>%
  column_spec(1, width = "5cm")

writeLines(est_tab_out,paste0(figureDir,"est_tab.tex"))

# Benchmark stuff
beta_opt %>% group_by(param) %>% summarise(mad=mad(val),
                                           mean = mean(val),
                                           sd = sqrt(var(val)))
ggplot(beta_opt, aes(x=val,color=param,fill=param)) +
  geom_histogram(position = "identity", alpha=0.5, show.legend=FALSE, bins = 50) +
  facet_grid(rows = vars(param), scales="free") 


# View by dataset instead and overlay densities
plot_densities <- function(method){
  
  results <- estimates %>% filter(matching == method)
  names <- est_methods
  if (grepl("Single", method)){
    results <- results %>% filter(est_method == "OLS (All)"
                                  | est_method == "OLS (True)")
    names <- est_methods[-c(1,2,5)]
  }
  
  if (grepl("Multi", method)){
    results <- results %>% filter(est_method != "OLS (True)")
    names <- est_methods[-4]
  }
  
  temp <- ggplot(results, aes(x=value,color=est_method, fill =est_method)) + 
    geom_density(aes(x=value, color = est_method, fill=est_method), alpha=0.1) +
    # stat_density(aes(x=value, color= est_method), geom="line", position="identity") +
    facet_grid(rows = vars(param), 
               labeller=label_parsed,
               scales="free") +
    labs(x = "Parameter Estimate", y = "Density", title = method) +
    theme(legend.position ="bottom") +
    scale_color_discrete(name = "", labels = names) + 
    scale_fill_discrete(name = "", labels = names) + 
    labs(caption = "*Based on 1,000 simulations. Vertical line indicates the sample mean.") +
    theme(plot.caption =element_text(hjust=0),
          plot.title = element_text(hjust=0.5))
  temp
  ggsave(paste0(figureDir,method,"_hist.pdf"), plot=temp)
}

levels(estimates$matching) <- c("ABE Single","ABE Multi","PRL Single","PRL Multi")
match_methods <- levels(estimates$matching)

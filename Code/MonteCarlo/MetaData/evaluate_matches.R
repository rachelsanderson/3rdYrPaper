require(tidyverse)
require(scales)
require(knitr)
require(kableExtra)
require(magrittr)
require(qwraps2)

metaDataDir <- "~/Desktop/3rdYrPaper/Code/MonteCarlo/MetaData/"
figureDir <- "~/Desktop/3rdYrPaper/Figures/"
load(paste0(metaDataDir,"matching_metadata.RData"))
options(qwraps2_markup = "latex")


row_names <- c("ABE (Single)","ABE (Multi)", "PRL (Single)", "PRL (Multi)")
method_names <- c(
  'abe_single' = "ABE (Single)",
  'abe_multi' = "ABE (Multi)", 
  'prl_single' = "PRL (Single)", 
  'prl_multi' = "PRL (Multi)"
)

mean.sd <- function(x){
  return(mean_sd(x, denote_sd="paren"))
}

metaMatch <- matching.metadata$full_matching_meta   
metaMultiMatch <- matching.metadata$multi_matching_meta

# Make Matching Summary Table
table1 <- metaMatch   
table1 <- table1 %>% group_by(method) %>% 
                     summarise_all(mean.sd) %>% 
                     select(-mc_id)
table1$method <- row_names
colnames(table1) <- c("Method", "Match Rate", 
                      "# Matches", "Type I", "Type II",
                      "P(Contains True)")
table1 <- round(table1[,-1], 3)

k <- kable(table1, "latex", booktabs=T,align=("c")) 
writeLines(k,paste0(figureDir,"match_rates.tex"))

### Match Rate histogram
match_rates <- ggplot(metaMatch, aes(x=pMatchedX, fill=method, color=method)) + 
  geom_histogram(position = "identity", alpha=0.5, bins=50, show.legend=FALSE) +
  facet_grid(method ~ ., labeller=as_labeller(method_labeller)) + 
  labs(x = "Match Rate", y = "Frequency") +
  labs(caption = "*Based on 1,000 replications")

ggsave(paste0(figureDir,"match_rate.pdf"), plot = match_rates)

### Multi Match Table

table2 %>% group_by(method, L) %>% summarise(pContainsTrue=mean(pContainsTrue),
                                             meanCounts = mean(counts))

### Num Match histogram 
ggplot(metaMultiMatch, aes(x=L, fill=method, color=method)) + 
  geom_histogram(position = "identity", alpha=0.5,bins=8, show.legend=FALSE) +
  facet_grid(method ~ .) + 
  labs(x = "Match Rate", y = "Frequency") +
  labs(caption = "*Based on 1,000 replications")

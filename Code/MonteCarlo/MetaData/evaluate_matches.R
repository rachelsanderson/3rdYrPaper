require(tidyverse)
require(scales)
require(knitr)
require(kableExtra)
require(magrittr)
require(qwraps2)

metaDataDir <- "~/Desktop/3rdYrPaper/Code/MonteCarlo/MetaData/"
matchDataDir <- "~/Desktop/3rdYrPaper/Code/MonteCarlo/Linked_Datasets/"
figureDir <- "~/Desktop/3rdYrPaper/Figures/"
load(paste0(metaDataDir,"matching_metadata.RData"))
load(paste0(metaDataDir, "true_type_ii.RData"))
options(qwraps2_markup = "latex")

source("~/Desktop/3rdYrPaper/Code/correct_type_ii.R")

row_names <- c("ABE Single","ABE Multi", "PRL Single", "PRL Multi")
method_names <- c(
  'abe_single' = "ABE Single",
  'abe_multi' = "ABE Multi", 
  'prl_single' = "PRL Single", 
  'prl_multi' = "PRL Multi"
)

footnote_text = "Based on 1,000 simulations. Standard deviations are reported in parentheses."

mean.sd <- function(x){
  return(mean_sd(x, denote_sd="paren"))
}

metaMatch <- matching.metadata$full_matching_meta   
metaMultiMatch <- matching.metadata$multi_matching_meta

metaMatch$type_ii <- NULL
true_type_ii <- calc_type_ii(matchDataDir)
metaMatch <- inner_join(metaMatch, true_type_ii, by=c("mc_id", "method"))


# Make Matching Summary Table
table1 <- metaMatch   
table1 <- table1[,c(1:5, 7,6)]
table1 <- table1 %>% group_by(method) %>% 
                     summarise_all(mean.sd) %>% 
                     select(-mc_id)
table1$method <- row_names
colnames(table1) <- c("Method", "Match Rate", 
                      "# Matches", "Type I", "Type II",
                      "P(Contains True)")
# table1 <- round(table1[,-1], 3)

k <- kable(table1, "latex", booktabs=T,align=("c")) %>% 
  footnote(general = footnote_text,
           footnote_as_chunk = T)
writeLines(k,paste0(figureDir,"match_rates.tex"))

### Match Rate histogram
match_rate_mean <- metaMatch %>% group_by(method) %>% summarise(mean_match = mean(pMatchedX))

match_rates<- ggplot(metaMatch, aes(x=pMatchedX, color =method, fill=method)) + geom_density(alpha=0.3) +
  geom_vline(aes(xintercept = mean_match, color=method), linetype = "dashed", data=match_rate_mean, show.legend=FALSE) +
  labs(x="Match Rate", y="Frequency", fill="Method", color = "Method") +
  theme(legend.position ="bottom") +
  scale_fill_discrete(name = "", labels = row_names)+
  scale_color_discrete(name = "", labels = row_names) + 
  labs(caption = "*Based on 1,000 simulations. Vertical line indicates the sample mean.") +
  theme(plot.caption =element_text(hjust=0,size=12),
        legend.text=element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12))

ggsave(paste0(figureDir,"match_rate.pdf"), plot = match_rates)


match_rates <- ggplot(metaMatch, aes(x=pMatchedX, fill=method, color=method)) + 
  geom_histogram(position = "identity", alpha=0.5, bins=50, show.legend=FALSE) +
  geom_vline(aes(xintercept = mean_match), linetype="dashed",
             data=match_rate_mean,
             show.legend=FALSE) +
  facet_grid(method ~ ., labeller=as_labeller(method_names)) + 
  labs(x = "Match Rate", y = "Frequency") +
  labs(caption = "*Based on 1,000 simulations. Vertical line indicates the sample mean.") +
  theme_grey(base_size=12)
  # theme(plot.caption =element_text(hjust=0))


### Multi Match Table
table2 <- metaMultiMatch %>% group_by(method,mc_id) %>% mutate(totMatches = sum(counts),
                                                               propL = counts/totMatches)

## merge L = 6,7,8 for ease of exposition
ind <- (table2$L >= 6)
table2[ind,]$L <- rep("6+", sum(ind))

t <- table2 %>% group_by(method, L) %>% select(pContainsTrue, propL)%>% 
  summarise_all(mean.sd)

t1 <- t %>% select(-propL) %>% spread(key = L, value = pContainsTrue) %>%
  add_column(var = "Pr(Contains True)", .after="method")
t2 <- t %>% select(-pContainsTrue) %>% spread(key = L, value = propL, fill="0.00") %>%
  add_column(var = "Pr(L=$\\ell$)", .after="method")

tab2 <- rbind(t1,t2) %>% arrange(method)
tab2 <- tab2[, -1]
colnames(tab2)[1] <- c("L")

multi_tab <- kable(tab2, "latex", booktabs = T, 
                   align=c("l", rep("c", ncol(tab2)-1)),
                   escape=FALSE) %>%
  pack_rows("ABE Multi", 1,2) %>%
  pack_rows("PRL Multi", 3, 4) %>%
  footnote(general = footnote_text,
           footnote_as_chunk = T)

writeLines(multi_tab,paste0(figureDir,"multi_tab.tex"))

### Num Match histogram 
ggplot(table2, aes(x=L, y=propL, fill=method, color=method)) +
  geom_bar(stat="identity", alpha=0.5, show.legend=FALSE) + 
  facet_grid(method ~ ., labeller=as_labeller(method_names[c(2,4)])) 

  # labs(x = "L", y = "Proportion") +
  # labs(caption = "*Based on 1,000 replications")

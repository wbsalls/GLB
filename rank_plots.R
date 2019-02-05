library(ggplot2)

setwd("O:/PRIV/NERL_ORD_CYAN/Salls_working/GLB/Analysis/RF/out/current")

ranks <- read.csv("var_rank_hiElevHiLat_CI_sp90th_tmedian_2018-11-14.csv", stringsAsFactors = FALSE) # ***** select *****
#ranks <- read.csv("var_rank_lowElev_CI_sp90th_tmedian_2018-11-14.csv", stringsAsFactors = FALSE) # ***** select *****

# take top 25 only, if desired
ranks <- ranks[ranks$cum_rank <= 25, ]

# convert wide rank table to long
ranks <- ranks[, 2:(which(colnames(ranks) == "rank_sum") - 1)]


ranks_long <- data.frame()

for (c in 2:ncol(ranks)) {
  ranks_long <- rbind(ranks_long, data.frame(var = ranks$var, 
                                             run = colnames(ranks)[c], 
                                             rank = ranks[, c]))
}

# reorder factor levels of vars
#levels(ranks_long$var) <- as.character(ranks$var)

# plot
ggplot(ranks_long, aes(factor(var, levels = ranks$var), rank)) + 
  geom_violin() + 
  stat_summary(fun.y="mean", geom="point", color = "red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("variable") + 
  ylab("ranks") + 
  #scale_y_reverse() +
  scale_y_continuous(breaks = seq(min(ranks_long$rank), max(ranks_long$rank), by = 4),  
                     trans = 'reverse') + 
  ggtitle("High Elevation and Latitude") # ***** select *****
  #ggtitle("Low Elevation and Latitude") # ***** select *****

#*** make variable importance metric tables

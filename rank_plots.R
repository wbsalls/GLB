library(ggplot2)

setwd("O:/PRIV/NERL_ORD_CYAN/Salls_working/GLB/Analysis/RF/out/current")

# ***** select one *****
ranks_all <- read.csv("var_rank_all_CI_sp90th_tmedian_2018-12-19.csv", stringsAsFactors = FALSE) 
ranks_high <- read.csv("var_rank_hiElevHiLat_CI_sp90th_tmedian_2018-11-14.csv", stringsAsFactors = FALSE)
ranks_low <- read.csv("var_rank_lowElev_CI_sp90th_tmedian_2018-11-14.csv", stringsAsFactors = FALSE)

subset <- "all" # all, high, low

if (subset == "all") {
  ranks <- ranks_all
  title <- "All Lakes"
} else if (subset == "high") {
  ranks <- ranks_high
  title <- "High Elevation and Latitude"
} else if (subset == "low") {
  ranks <- ranks_low
  title <- "Low Elevation and Latitude"
}

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

# box plot ---------------------------- # 900 x 700
ggplot(ranks_long, aes(factor(var, levels = ranks$var), rank), base_family = "TT Arial") + 
  geom_boxplot() + 
  stat_summary(fun.y="mean", geom="point", color = "red") + 
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle = 45, hjust = 1), plot.margin= unit(c(0.5,0.5,0.5,1.5), "cm") #comma to include background
        #panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # comment out to include background
        #panel.background = element_blank(), axis.line = element_line(colour = "black") # comment out to include background
        ) +
  xlab("variable") + 
  ylab("ranks") + 
  #scale_y_reverse() +
  scale_y_continuous(breaks = seq(min(ranks_long$rank), max(ranks_long$rank), by = 4),  
                     trans = 'reverse') + 
  ggtitle(title)

'
# violint plot, vertical ----------------------------
ggplot(ranks_long, aes(factor(var, levels = ranks$var), rank)) + 
  geom_violin() + 
  stat_summary(fun.y="mean", geom="point", color = "red") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("variable") + 
  ylab("ranks") + 
  #scale_y_reverse() +
  scale_y_continuous(breaks = seq(min(ranks_long$rank), max(ranks_long$rank), by = 4),  
                     trans = "reverse") + 
  ggtitle("All Lakes") # ***** select *****
  #ggtitle("High Elevation and Latitude") # ***** select *****
  #ggtitle("Low Elevation and Latitude") # ***** select *****
'
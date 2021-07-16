library(ggplot2)

setwd("xxx/xxx/xxx/.../out") #                                 <<<< USER INPUT  **************
setwd("O:/PRIV/NERL_ORD_CYAN/Salls_working/GLB/Analysis/Publication/out")

# ***** select one *****
ranks_all <- read.csv("var_rank_CI_sp90th_tmedian_all_2021-03-26.csv", stringsAsFactors = FALSE) 
ranks_high <- read.csv("var_rank_CI_sp90th_tmedian_hiElevHiLat_2021-03-26.csv", stringsAsFactors = FALSE)
ranks_low <- read.csv("var_rank_CI_sp90th_tmedian_lowElev_2021-03-26.csv", stringsAsFactors = FALSE)

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

# number of variables to show (decided to show top 25 for paper)
#nvar <- length(unique(ranks_all$var)) # 88 variables total
nvar <- 25 # <<<< subset here <<<<
ranks <- ranks[ranks$cuml_rank <= nvar, ]

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

# box plot ---------------------------- # 900 x 700 (1800 x 1200 for all 88)
ggplot(ranks_long, aes(factor(var, levels = ranks$var), rank), base_family = "TT Arial") + 
  geom_boxplot() + 
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle = 45, hjust = 1), plot.margin= unit(c(0.5,0.5,0.5,1.5), "cm"), #comma if excluding bg
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # eliminates background
        panel.background = element_blank(), axis.line = element_line(colour = "black") # eliminates background
        ) +
  xlab("variable") + 
  ylab("ranks") + 
  #scale_y_reverse() +
  scale_y_continuous(breaks = seq(0, max(ranks_long$rank), by = 5),  
                     trans = 'reverse') + 
  ggtitle(title) +
  geom_vline(xintercept = seq(1.5, (nvar + 0.5), 1), linetype = "dotted", color = "black") + # adds vertical lines
  geom_hline(yintercept = seq(0, (floor(max(ranks_long$rank) / 5) * 5), 5), linetype = "dotted", color = "black") + # adds horizontal lines
  geom_boxplot() +
  stat_summary(fun.y="mean", geom="point", color = "red")

#

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
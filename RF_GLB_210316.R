library(corrplot)
library(randomForest)
library(ggplot2)

### setup ----------------------------------------------------------------------

# set working directory to location of input files
#setwd("xxx/xxx/xxx...") #                               <<<< USER INPUT  **************
#setwd("O:/PRIV/NERL_ORD_CYAN/Salls_working/GLB/Analysis/Publication") # REMOVE IN FINAL VERSION ******XXXXXXXXXX*******
setwd("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/LandscapeAnalysis/Publication")

# load data
lake_data_all <- read.csv("LandscapeAnalysis_data_2021.csv")

# load variable file to use for naming
variable_file <- read.csv("LandscapeAnalysis_variables_2021.csv", stringsAsFactors = FALSE)


# select which region to run
lakes_region <- "all" # "all", "hiElevHiLat", "lowElev" #  <<<< USER INPUT **************

if (lakes_region == "all") {
  lake_data <- lake_data_all
} else {
  lake_data <- lake_data_all[which(lake_data_all$Ecoregion_L2_highelev_lat == lakes_region),]
}

# select whether to analyze correlations
run_cors <- FALSE # TRUE or FALSE #                       <<<< USER INPUT  **************

# select whether to remove correlated predictors
rm_corr <- TRUE # TRUE or FALSE #                        <<<< USER INPUT  **************


## response

# select response
resp_var <- "CI_sp90th_tmedian"

# check for NAs in response - should be 0
sum(is.na(lake_data[, which(colnames(lake_data) == resp_var)]))

# remove NAs, if any
lake_data <- lake_data[!is.na(lake_data[, resp_var]), ]



## predictors

# select predictor variables
pred_vars_all <- colnames(lake_data)[(which(colnames(lake_data) == "CI_sp90th_tmax") + 1):
                                       (which(colnames(lake_data) == "Ecoregion_L2_code") - 1)]

# see # NAs in predictors - should all be 0
for (p in (which(colnames(lake_data) == "CI_sp90th_tmax") + 1):
     (which(colnames(lake_data) == "Ecoregion_L2_code") - 1)) {
  #print(paste0(sum(is.na(lake_data[, p])), " : ", colnames(lake_data)[p]))
}

# remove selected predictors - determined to be redundant or not of interest
pred_vars <- pred_vars_all[-which(pred_vars_all %in% c("ShorelineL",
                                                       "ShorelineD",
                                                       "MaxLength",
                                                       "MaxWidth",
                                                       "MeanWidth",
                                                       "FetchN",
                                                       "FetchNE",
                                                       "FetchE",
                                                       "FetchSE",
                                                       "MaxDepthCo",
                                                       "rNI90",
                                                       "Q"))]

# remove correlated predictors with r > 0.9 (determined below)
if (isTRUE(rm_corr)) {
  pred_vars <- pred_vars[-which(pred_vars %in% c("DRNN",
                                                 "DRNP",
                                                 "L1_AMP",
                                                 "longitude",
                                                 "PctCrop2011Ws",
                                                 "PctDecid2011Ws",
                                                 "PctImp2011Ws",
                                                 "PctUrbLoMd2011Ws_SUM",
                                                 "PermWs",
                                                 "PopDen2010Ws",
                                                 "SandWs",
                                                 "YON",
                                                 "YP"))]
}

# remove records with NA predictors, if any
index_na_pred <- which(apply(lake_data[, pred_vars], 1, FUN = function(x) {sum(is.na(x))}) > 0)
if (length(index_na_pred) > 0) {
  lake_data <- lake_data[-index_na_pred]
}



## set response and predictor data
resp_data <- lake_data[, resp_var]
pred_data <- lake_data[, pred_vars]



### correlations ---------------------------------------------------------------

if (isTRUE(run_cors)) {
  
  data_cor_all <- lake_data
  
  data_cor_preds <- data_cor_all[, which(colnames(data_cor_all) %in% pred_vars)]
  cor_preds <- cor(data_cor_preds, use = "pairwise.complete.obs")
  
  n_corrs <- ((ncol(cor_preds) ^ 2) - ncol(cor_preds)) / 2 # total num correlations
  
  (sum(cor_preds > 0.7) - ncol(cor_preds)) / 2 # how many correlations exceed 0.7? 86
  ((sum(cor_preds > 0.7) - ncol(cor_preds)) / 2) / n_corrs # portion
  
  sum(cor_preds < 0.7 & cor_preds > 0.3) / 2 # how many correlations in 0.3 - 0.7?
  (sum(cor_preds < 0.7 & cor_preds > 0.3) / 2) / n_corrs # portion
  
  (sum(cor_preds < 0.3)) / 2 # how many correlations are below 0.3? 3397
  ((sum(cor_preds < 0.3)) / 2) / n_corrs # portion
  
  
  # including response (to use for directionality of each effect)
  data_cor <- cbind(data_cor_all[, which(colnames(data_cor_all) == resp_var),], 
                    data_cor_all[, which(colnames(data_cor_all) %in% pred_vars)])
  colnames(data_cor)[1] <- resp_var
  cor_vars <- cor(data_cor, use = "pairwise.complete.obs")
  
  corrplot(cor_vars, main = lakes_region)
  
  # write correlation matrix to CSV
  if (isFALSE(dir.exists("out"))) {
    print("creating 'out' folder and saving there")
    dir.create("out")
  } else {
    print("saving in 'out' folder")
  }
  
  write.csv(cor_vars, sprintf("out/correlations_%s_%s.csv", lakes_region, Sys.Date()))
  
  
  ## identify predictor pairs with |r| > a threshold (0.9)
  
  rthresh <- 0.9
  cor_preds[abs(cor_preds) >= rthresh & cor_preds != 1]
  
  prior_ranks <- read.csv("out/var_ranks_SUMMARY_CI_sp90th_tmedian_2021-03-29.csv")
  
  cor9df <- data.frame()
  
  for (r in 1:nrow(cor_preds)) {
    for (c in 1:ncol(cor_preds)) {
      cor_rc <- cor_preds[r, c]
      if (abs(cor_rc) >= rthresh & cor_rc != 1) {
        prow <- rownames(cor_preds)[r]
        pcol <- colnames(cor_preds)[c]
        rank_prow <- prior_ranks$rank_all[which(prior_ranks$var == prow)]
        rank_pcol <- prior_ranks$rank_all[which(prior_ranks$var == pcol)]
        
        if (rank_prow < rank_pcol) {
          cor9df <- rbind(cor9df, data.frame(
            cor = cor_rc,
            rank_stronger = prior_ranks$rank_all[which(prior_ranks$var == prow)],
            pred_stronger = prow,
            rank_weaker = prior_ranks$rank_all[which(prior_ranks$var == pcol)],
            pred_weaker = pcol))
        } else {
          cor9df <- rbind(cor9df, data.frame(
            cor = cor_rc,
            rank_stronger = prior_ranks$rank_all[which(prior_ranks$var == pcol)],
            pred_stronger = pcol,
            rank_weaker = prior_ranks$rank_all[which(prior_ranks$var == prow)],
            pred_weaker = prow))
        }
      }
    }
  }
  
  cor9df <- cor9df[order(abs(cor9df$cor), decreasing = TRUE), ]
  cor9df <- cor9df[-which(duplicated(cor9df)), ]
  row.names(cor9df) <- NULL
  
  cor9df$conflict <- ""
  cor9df$conflict[cor9df$pred_stronger %in% cor9df$pred_weaker] <- sprintf("%sst in %swk", "", "")
  cor9df$conflict[cor9df$pred_weaker %in% cor9df$pred_stronger] <- sprintf("%swk in %sst", "", "")
  cor9df$conflict[cor9df$pred_stronger %in% cor9df$pred_weaker & 
                    cor9df$pred_weaker %in% cor9df$pred_stronger] <- "both"
  
  write.csv(cor9df, sprintf("out/correlated_pairs_%s_%s_%s.csv", round(rthresh, 2) * 100, lakes_region, Sys.Date()))
  
}



### run random forests ---------------------------------------------------------

nruns <- 100 # number of RF runs

# set name of importance metric
if (is.numeric(resp_data)) {
  imp_metric_label <- "%IncMSE" # if regression (IncNodePurity if type 2)
} else {
  imp_metric_label <- "Accuracy Increase?" # if classification (MeanDecreaseGini if type 2)
}

# initialize tables that will be written to
tree_eval_df <- data.frame()
rf_eval_df <- data.frame()
var_imp_df <- data.frame(var = as.character(pred_vars), stringsAsFactors = FALSE)
var_rank_df <- data.frame(var = as.character(pred_vars), stringsAsFactors = FALSE)
rf_list <- list() # initialize list for storing RF objects, for querying if desired


Sys.time()

set.seed(1) # set pseudo-randomization - to obtain consistent results

# for each model run...
for (j in 1:nruns) {
  
  # print status update
  print(sprintf("RF on %s, %s: run #%s of %s (%s rows, %s predictors) at %s", 
                resp_var, lakes_region, j, nruns, nrow(pred_data), ncol(pred_data), Sys.time()))
  
  # run rf function
  rf_i <- randomForest(x = pred_data, y = resp_data, 
                       na.action = na.omit, 
                       #mtry = floor(sqrt(ncol(pred_data))), # default for regression is p/3
                       importance=TRUE,
                       ntree = 500)
  
  # append this RF to list of RFs
  rf_list[[j]] <- rf_i
  
  # append MSE and rsq of all trees to tree_eval_df table
  tree_eval_df <- rbind(tree_eval_df, data.frame(run = j, tree = 1:rf_i$ntree, mse = rf_i$mse, rsq = rf_i$rsq))
  
  # append average MSE ad rsq for forest to rf_eval_df table
  rf_eval_df <- rbind(rf_eval_df, data.frame(run = j,
                                             n = nrow(pred_data),
                                             meanCI = mean(resp_data),
                                             medianCI = median(resp_data),
                                             minCI = min(resp_data),
                                             maxCI = max(resp_data),
                                             sdCI = sd(resp_data),
                                             OOB_mse_mean = mean(rf_i$mse),
                                             OOB_rsq_mean = mean(rf_i$rsq)))
  
  ## variable importance -----------
  
  # make importance dataframe for this run
  # (for each variable: increase in node purity for regression; Gini index for classification)
  
  importance_i <- importance(rf_i, type = 1, scale = F)
  
  var_imp_df_i <- data.frame(var = rownames(importance_i), 
                             importance_metr = importance_i)
  
  # merge this importance df to existing one
  var_imp_df <- merge(var_imp_df, var_imp_df_i)
  
  colnames(var_imp_df)[j + 1] <- paste0("run", j, "_", imp_metric_label)
  
  # variable ranks
  imp_ordered <- var_imp_df_i[order(-var_imp_df_i[, 2]), ]
  rownames(imp_ordered) <- 1:nrow(imp_ordered)
  
  ranks <- c()
  for (v in 1:length(var_rank_df$var)) {
    ranks <- c(ranks, as.numeric(rownames(imp_ordered)[which(imp_ordered$var == var_rank_df$var[v])]))
  }
  
  var_rank_df <- cbind(var_rank_df, ranks)
  colnames(var_rank_df)[j + 1] <- paste0("run", j)
}


## summarize importance metrics across runs
# sum importance metric (Increase in Node Purity or Mean Decrease in Gini Index)
var_imp_df$imp_value_sum <- apply(var_imp_df[, 2:(nruns + 1)], 1, sum)

# sort var_imp_df by imp_value_sum
var_imp_df <- var_imp_df[order(var_imp_df$imp_value_sum, decreasing = TRUE), ]

# append overall ranks
var_imp_df$imp_value_rank <- 1:nrow(var_imp_df)

# summarize var_imp_df
for (v in 1:nrow(var_imp_df)) {
  var_imp_df$mean[v] <- mean(as.numeric(var_imp_df[v, 2:(nruns + 1)]))
  var_imp_df$median[v] <- median(as.numeric(var_imp_df[v, 2:(nruns + 1)]))
  var_imp_df$min[v] <- min(as.numeric(var_imp_df[v, 2:(nruns + 1)]))
  var_imp_df$max[v] <- max(as.numeric(var_imp_df[v, 2:(nruns + 1)]))
  var_imp_df$range[v] <- max(as.numeric(var_imp_df[v, 2:(nruns + 1)])) - min(as.numeric(var_imp_df[v, 2:(nruns + 1)]))
  var_imp_df$sd[v] <- sd(as.numeric(var_imp_df[v, 2:(nruns + 1)]))
  var_imp_df$var_label[v] <- variable_file$Label[which(variable_file$Variable == var_imp_df$var[v])]
}


## summarize ranks across runs
# sum ranks to create rank score; order based on rank sum; assign cumulative ranks
var_rank_df$rank_sum <- apply(var_rank_df[, 2:(nruns + 1)], 1, sum)
var_rank_df <- var_rank_df[order(var_rank_df$rank_sum), ]
var_rank_df$cuml_rank <- 1:nrow(var_rank_df)

# summarize var_rank_df
for (v in 1:nrow(var_rank_df)) {
  var_rank_df$mean[v] <- mean(as.numeric(var_rank_df[v, 2:(nruns + 1)]))
  var_rank_df$median[v] <- median(as.numeric(var_rank_df[v, 2:(nruns + 1)]))
  var_rank_df$min[v] <- min(as.numeric(var_rank_df[v, 2:(nruns + 1)]))
  var_rank_df$max[v] <- max(as.numeric(var_rank_df[v, 2:(nruns + 1)]))
  var_rank_df$range[v] <- max(as.numeric(var_rank_df[v, 2:(nruns + 1)])) - min(as.numeric(var_rank_df[v, 2:(nruns + 1)]))
  var_rank_df$sd[v] <- sd(as.numeric(var_rank_df[v, 2:(nruns + 1)]))
  var_rank_df$var_label[v] <- variable_file$Label[which(variable_file$Variable == var_rank_df$var[v])]
}



## write tables
if (isFALSE(dir.exists("out"))) {
  print("creating 'out' folder and saving there")
  dir.create("out")
} else {
  print("saving in 'out' folder")
}

write.csv(tree_eval_df, sprintf("out/tree_eval_df_%s_%s.csv", resp_var, lakes_region, Sys.Date()))
write.csv(rf_eval_df, sprintf("out/rf_eval_%s_%s_%s.csv", resp_var, lakes_region, Sys.Date()))
write.csv(var_imp_df, sprintf("out/var_imp_df_%s_%s_%s.csv", resp_var, lakes_region, Sys.Date()))
write.csv(var_rank_df, sprintf("out/var_rank_%s_%s_%s.csv", resp_var, lakes_region, Sys.Date()))




## plot rank boxplots ----------------------------------------------------------

# set title for plot
if (lakes_region == "all") {
  title <- "All Lakes"
} else if (lakes_region == "hiElevHiLat") {
  title <- "High Elevation and Latitude"
} else if (lakes_region == "lowElev") {
  title <- "Low Elevation and Latitude"
}

# set number of variables to show (showing top 25 for paper)
nvar <- 25 # 25 (top 25) OR length(unique(var_rank_df$var)) (all) <<<< subset if desired *****
ranks <- var_rank_df[var_rank_df$cuml_rank <= nvar, ]


# convert wide rank table to long
ranks <- ranks[, c(1, which(colnames(ranks) == "var_label"), 
                   2:(which(colnames(ranks) == "rank_sum") - 1))]


ranks_long <- data.frame()

for (c in 3:ncol(ranks)) {
  ranks_long <- rbind(ranks_long, data.frame(var = ranks$var,
                                             var_label = ranks$var_label,
                                             run = colnames(ranks)[c], 
                                             rank = ranks[, c]))
}

# reorder factor levels of vars
#levels(ranks_long$var) <- as.character(ranks$var)


# plot box plot   # 900 x 700 (1800 x 1200 for all 88)
jpeg(sprintf("out/box_RF_%s_%s_%s_%s.jpg", lakes_region, ncol(pred_data), nvar, Sys.Date()), 
     res = 300, width = 9, height = 7, units = "in") #width = 900*0.8, height = 700*0.8, 

# for variable labels, can change XX in factor(XX, levels = XX) to: var OR var_label
ggplot(ranks_long, aes(factor(var_label, levels = ranks$var_label), rank), base_family = "TT Arial") + 
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
  stat_summary(fun="mean", geom="point", color = "red")

dev.off()



### partial dependence plots ---------------------------------------------------

# to be run for each region

# select forest with min error
forest_best <- which(rf_eval_df$OOB_mse_mean == min(rf_eval_df$OOB_mse_mean))
rf_best <- rf_list[[forest_best]]

#opar <- par()
#par(opar)

## show only top 10, sorted by rank
preds_sorted <- var_rank_df[1:10, "var"]

jpeg(sprintf("out/partialDependence_rf_best_%s_%s_top10_%s.jpg",lakes_region, ncol(pred_data), Sys.Date()), 
     res = 300, width = 10, height = 4, units = "in") #width = 400*2.2, height = 150*2.2
par(mfrow = c(2, 5), mar = rep(2, 4))
for (r in seq_along(preds_sorted)) {
  ptitle <- variable_file$Label[which(variable_file$Variable == preds_sorted[r])]
  partialPlot(rf_best, pred_data, preds_sorted[r], main = ptitle,
              ylim = c(0, 0.01))
}
dev.off()


'
## show all 88

# sort by raw predictor variables name
#preds_sorted <- sort(pred_vars) 

# sort by variable name (label)
variable_file_preds <- variable_file[variable_file$Variable %in% pred_vars, ]
preds_sorted <- variable_file_preds[order(variable_file_preds$Label), "Variable"]

jpeg(sprintf("out/partialDependence_rf_best_%s_%s_%s.jpg",lakes_region, ncol(pred_data), Sys.Date()), 
     width = 2000*0.8, height = 1500*0.8)
par(mfrow = c(8, 11), mar = rep(2, 4))
for (r in seq_along(preds_sorted)) {
  ptitle <- variable_file$Label[which(variable_file$Variable == preds_sorted[r])]
  partialPlot(rf_best, pred_data, preds_sorted[r], main = ptitle)
}
dev.off()
'


### ----------------------------------------------------------------------------



### now, to run on the regional subsets, change lakes_region to another (all, lo, hi) and run entire code block again


###                                                                       /\
###                                                                      //\\                
###                                                                       ||
###                                              _________________________||
###                                              __________________________|


### once run for all regions (all, lo, hi), summarize ranks and evals ----------

## summarize ranking tables across regions (all, hi, lo)
rank_files <- list.files("out", pattern = "var_rank_")

# initiate rank summary table with first rank file
rank_file1 <- read.csv(file.path("out", rank_files[1]), stringsAsFactors = FALSE)
rank_summary <- rank_file1[, c("var", "var_label", "cuml_rank")]
colnames(rank_summary)[3] <- paste0("rank_", strsplit(rank_files[1], split = "_")[[1]][6])

# for each rank file: read csv, pull var and rank columns, merge to rank_summary df, rename column
for (f in 2:length(rank_files)) {
  fname <- rank_files[f]
  rank_csv <- read.csv(file.path("out", fname), stringsAsFactors = FALSE)
  ranks_f <- rank_csv[, which(colnames(rank_csv) %in% c("var", "cuml_rank"))]
  
  rank_summary <- merge(rank_summary, ranks_f, by = "var", all.y = TRUE)
  colnames(rank_summary)[f + 2] <- paste0("rank_", strsplit(fname, split = "_")[[1]][6])
}

write.csv(rank_summary, sprintf("out/var_ranks_SUMMARY_%s_%s.csv", resp_var, Sys.Date()))



## summarize eval tables across regions (all, hi, lo)
eval_files <- list.files("out", pattern = "rf_eval_")

eval_summary <- data.frame()

for (f in 1:length(eval_files)) {
  fname <- eval_files[f]
  eval_csv <- read.csv(file.path("out", fname), stringsAsFactors = FALSE)
  colmeans <- as.data.frame(t(apply(eval_csv[, c("n", "OOB_mse_mean", "OOB_rsq_mean", "meanCI", "medianCI", "minCI", "maxCI", "sdCI")], 
                                    2, mean)))
  
  eval_summary <- rbind(eval_summary, data.frame(name = sub("rf_eval_CI_sp90th_tmedian_", "", fname), colmeans))
}

write.csv(eval_summary, sprintf("out/rf_evals_SUMMARY_%s_%s.csv", resp_var, Sys.Date()))

### ----------------------------------------------------------------------------


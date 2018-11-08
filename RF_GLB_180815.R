# version on GitHub


library(randomForest)
#library(party) # cforest
#library(caret)
#library(h2o)

script_dir <- "C:/Users/WSalls/Desktop/Git/GLB"
#script_dir <- "/Users/wilsonsalls/Desktop/Git/GLB"

source(file.path(script_dir, "RF_functions_180530.R")) # calls version from same folder (in this case, Git)

dir_analysis <- "O:\\PRIV\\NERL_ORD_CYAN\\Salls_working\\GLB\\Analysis"
#dir_analysis <- "/Users/wilsonsalls/Desktop/EPA/GLB/Analysis/"

setwd(dir_analysis)

# specify variable file to use for naming
variable_file <- read.csv("GLB_LandscapeAnalysis_variables_2018-11-06.csv", stringsAsFactors = FALSE)

# read in data
lake_data <- read.csv("GLB_LandscapeAnalysis_data_2018-11-06.csv")
lake_data <- lake_data[, -which(colnames(lake_data) == "X")] # remove X column

# select model type
model_type <- "randomForest" # "randomForest" or "conditionalForest"


## subset - ecoregion ------------

# set variable to subset by (or "all)
#colnames(lake_data)[which(grepl("Ecoregion", colnames(lake_data)))]

#subset_var <- "all"
#subset_var <- "Ecoregion_L1_code"
#subset_var <- "Ecoregion_L2_code"
#subset_var <- "Ecoregion_L2_elev_lat"
#subset_var <- "Ecoregion_L2_elev"
subset_var <- "Ecoregion_L2_highelev_lat" # use this one ***

# select names of classes/regions with at least 25 observations
if (subset_var == "all") {
  data_subsets <- "all"
} else {
  subset_table <- table(lake_data[, which(colnames(lake_data) == subset_var)])
  data_subsets <- names(subset_table[subset_table >= 0]) # set minimum group size
}



## responses ----------

# check for NAs in response
sum(is.na(lake_data$CI_sp90th_tmedian))

# add binary bloom/no variable
lake_data$ci_median_bloom <- NA
lake_data$ci_median_bloom[which(lake_data$CI_sp90th_tmedian == 0.0001)] <- "no bloom"
lake_data$ci_median_bloom[which(lake_data$CI_sp90th_tmedian != 0.0001)] <- "bloom"
lake_data$ci_median_bloom <- as.factor(lake_data$ci_median_bloom)

# add variable with ND removed
lake_data$CI_sp90th_tmedian_rmND <- lake_data$CI_sp90th_tmedian
lake_data$CI_sp90th_tmedian_rmND[which(lake_data$CI_sp90th_tmedian_rmND == 0.0001)] <- NA

# select response
#responses <- c("CI_sp90th_tmedian", "CI_sp90th_tmax", "ci_median_WHO", "ci_max_WHO", "ci_median_Ross", "ci_max_Ross", "ci_median_bloom", "CI_sp90th_tmedian_rmND")
#responses <- c("CI_sp90th_tmedian", "ci_median_WHO", "ci_median_Ross", "ci_median_bloom", "CI_sp90th_tmedian_rmND")
responses <- c("CI_sp90th_tmedian")

# function to reorder classification factor levels
reorder_levels <- function(input_dat) {
  if (length(levels(input_dat)) == 5) {
    output_dat <- factor(input_dat, levels(input_dat)[c(4, 2, 3, 1, 5)])
  } else if (length(levels(input_dat)) == 4) {
    output_dat <- factor(input_dat, levels(input_dat)[c(4, 2, 3, 1)])
  } else if (length(levels(input_dat)) == 3) {
    output_dat <- factor(input_dat, levels(input_dat)[c(2, 3, 1)])
  } else {
    output_dat <- input_dat
  }
  return(output_dat)
}

#

## predictors ---------

# predictors
pred_vars_all <- colnames(lake_data)[4:(which(colnames(lake_data) == "Ecoregion_L2_code") - 1)]

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
                                                       "Q"
))] # remove preds

# predictors - see NAs
#for (p in 4:(which(colnames(lake_data) == "Ecoregion_L1") - 1)) {print(paste0(sum(is.na(lake_data[, p])), " : ", colnames(lake_data)[p]))}

#

# ----------------------------------

Sys.time()

# for each subset (ecoregion)...
for (d in 1:length(data_subsets)) {
  
  if (data_subsets == "all") {
    lake_data_use <- lake_data
  } else {
    lake_data_use <- lake_data[which(lake_data[, which(colnames(lake_data) == subset_var)] == data_subsets[d]), ]
  }
  
  # for each response variable...
  for (nr in 1:length(responses)) {
    resp <- responses[nr]
    
    dataset_name <- paste0(data_subsets[d], "_", resp)
    
    resp_data <- lake_data_use[, resp]
    pred_data <- lake_data_use[, pred_vars]
    
    # reorder classification factor levels
    if (is.factor(resp_data)) {
      resp_data <- reorder_levels(resp_data)
    }
    
    # remove records with NA response
    pred_data <- pred_data[!is.na(resp_data), ] # this has to go before resp, so don't change order!
    resp_data <- resp_data[!is.na(resp_data)]
    
    
    # remove records with NA predictors, if any
    index_na_pred <- which(apply(pred_data, 1, FUN = function(x) {sum(is.na(x))}) > 0)
    if (length(index_na_pred) > 0) {
      resp_data <- resp_data[-index_na_pred]
      pred_data <- pred_data[-index_na_pred, ]
    }
    
    
    
    # multi RF runs -----------------------------------------------------------------------------------------------
    setwd(file.path(dir_analysis, "RF/out/"))
    
    rf_eval_df <- data.frame()
    var_imp_df <- data.frame(var = as.character(pred_vars), stringsAsFactors = FALSE)
    var_rank_df <- data.frame(var = as.character(pred_vars), stringsAsFactors = FALSE)
    
    if (is.numeric(resp_data)) {
      imp_metric_label <- "IncNodePurity"
    } else {
      imp_metric_label <- "MeanDecreaseGini"
    }
    
    set.seed(1)
    
    nruns <- 5
    
    # for each model run...
    for (j in 1:nruns) {
      print(sprintf("   *** %s: run #%s of %s ***   ", dataset_name, j, nruns))
      
      # run rf function
      rf_i <- randomForest(x = pred_data, y = resp_data, na.action = na.omit) # randomForest
      
      
      ## evaluate -----------
      
      # plot error rate
      jpeg(sprintf('runs/ErrorRate_%s.jpg', paste0(dataset_name, "_run", j)), width = 1000, height = 700)
      plot(rf_i)
      dev.off()
      
      if (is.numeric(resp_data)) {
        # predict using this rf model
        preds <- predict(rf_i)
        
        # run evaluation function, updating
        rf_eval_df_i <- eval_model(resp_data, preds, run_name = paste0(dataset_name, "_run", j), 
                                   plot_fit = FALSE, save_plot = TRUE, save_dir = "runs")
        rf_eval_df <- rbind(rf_eval_df, cbind(rf_eval_df_i, 
                                              OOB_mse_mean = mean(rf_i$mse),
                                              OOB_rsq_mean = mean(rf_i$rsq)))
      } else {
        # run confusion matrix; round classication.error to 3 places
        conf <- (rf_i$confusion)
        conf[, ncol(conf)] <- round(conf[, ncol(conf)], 3)
        
        # calculate commission error for each column
        comerr_vect <- c()
        for (c in 1:(ncol(conf) - 1)) {
          comerr_vect <- c(comerr_vect, round((sum(conf[, c]) - conf[c, c]) / sum(conf[, c]), 3))
        }
        
        # subset to only values (not error rates) for summing
        conf_vals <- conf[1:(nrow(conf) - 1), 1:(ncol(conf) - 1)]
        
        # calculate overall error, append to bottom row
        overall_err <- round((sum(conf_vals) - sum(diag(conf_vals))) / sum(conf_vals), 3)
        comerr_vect <- c(comerr_vect, overall_err)
        
        # append commission error 
        conf <- rbind(conf, comerr_vect)
        
        # update names; print; export to csv
        rownames(conf)[nrow(conf)] <- "commission error"
        colnames(conf)[nrow(conf)] <- "ommission error"
        #print(conf)
        write.csv(conf, sprintf("runs/confusionMatrix_%s_run%s.csv", dataset_name, j))
        
        # error rate
        rf_eval_df <- rbind(rf_eval_df, cbind(run = j, 
                                              OOB_err.rate.est = rf_i$err.rate[nrow(rf_i$err.rate), 1],
                                              confusion.error = conf[nrow(conf), ncol(conf)]))
      }
      
      
      ## variable importance -----------
      
      # make importance dataframe for this run
      var_imp_df_i <- data.frame(var = rownames(rf_i$importance), 
                                 importance_metr = rf_i$importance)
      
      # merge this importance df to existing one
      var_imp_df <- merge(var_imp_df, var_imp_df_i)
      
      colnames(var_imp_df)[j + 1] <- paste0("run", j, "_", imp_metric_label)
      
      # plot variable importance
      var_import_plot(rf_output = rf_i, var_file = variable_file, response_name = paste0(dataset_name, "_run", j), nvar_show = 20, 
                      save_plot = TRUE, save_dir = "runs", return_df = FALSE)
      
      # variable ranks
      imp_ordered <- var_imp_df_i[order(-var_imp_df_i[, 2]), ]
      rownames(imp_ordered) <- 1:nrow(imp_ordered)
      
      ranks <- c()
      for (v in 1:length(var_rank_df$var)) {
        ranks <- c(ranks, as.numeric(rownames(imp_ordered)[which(imp_ordered$var == var_rank_df$var[v])]))
      }
      
      var_rank_df <- cbind(var_rank_df, ranks)
      colnames(var_rank_df)[j + 1] <- paste0("run", j, "_", dataset_name)
    }
    
    
    # sum importance metric (Increase in Node Purity or Mean Decrease in Gini Index)
    var_imp_df$imp_value_sum <- apply(var_imp_df[, 2:(nruns + 1)], 1, sum)
    var_imp_df <- var_imp_df[order(var_imp_df$imp_value_sum, decreasing = TRUE), ]
    var_imp_df$imp_value_rank <- 1:nrow(var_imp_df)
    plot(var_imp_df$imp_value_rank, var_imp_df$imp_value_sum, xlab = "Overall Importance Rank", ylab = "Score (Sum of Importance Metric)", 
         main = sprintf("Distribution of Variable Importance Metric Values (%s RF runs)", nruns))
    
    # summarize var_imp_df
    for (v in 1:nrow(var_imp_df)) {
      var_imp_df$mean[v] <- mean(as.numeric(var_imp_df[v, 2:(nruns + 1)]))
      var_imp_df$median[v] <- median(as.numeric(var_imp_df[v, 2:(nruns + 1)]))
      var_imp_df$min[v] <- min(as.numeric(var_imp_df[v, 2:(nruns + 1)]))
      var_imp_df$max[v] <- max(as.numeric(var_imp_df[v, 2:(nruns + 1)]))
      var_imp_df$range[v] <- max(as.numeric(var_imp_df[v, 2:(nruns + 1)])) - min(as.numeric(var_imp_df[v, 2:(nruns + 1)]))
      var_imp_df$sd[v] <- sd(as.numeric(var_imp_df[v, 2:(nruns + 1)]))
      var_imp_df$var[v] <- variable_file$Label[which(variable_file$Variable == var_imp_df$var[v])]
    }
    
    
    # var_rank_df
    # sum ranks to create rank score; order based on rank sum; assign cumulative ranks; plot rank score distribution
    var_rank_df$rank_sum <- apply(var_rank_df[, 2:(nruns + 1)], 1, sum)
    var_rank_df <- var_rank_df[order(var_rank_df$rank_sum), ]
    var_rank_df$cum_rank <- 1:nrow(var_rank_df)
    plot(var_rank_df$cum_rank, var_rank_df$rank_sum, xlab = "Overall Rank", ylab = "Score (Sum of Ranks)", 
         main = sprintf("Distribution of Variable Rank Scores (%s RF runs)", nruns))
    
    # summarize var_rank_df
    for (v in 1:nrow(var_rank_df)) {
      var_rank_df$mean[v] <- mean(as.numeric(var_rank_df[v, 2:(nruns + 1)]))
      var_rank_df$median[v] <- median(as.numeric(var_rank_df[v, 2:(nruns + 1)]))
      var_rank_df$min[v] <- min(as.numeric(var_rank_df[v, 2:(nruns + 1)]))
      var_rank_df$max[v] <- max(as.numeric(var_rank_df[v, 2:(nruns + 1)]))
      var_rank_df$range[v] <- max(as.numeric(var_rank_df[v, 2:(nruns + 1)])) - min(as.numeric(var_rank_df[v, 2:(nruns + 1)]))
      var_rank_df$sd[v] <- sd(as.numeric(var_rank_df[v, 2:(nruns + 1)]))
      var_rank_df$var[v] <- variable_file$Label[which(variable_file$Variable == var_rank_df$var[v])]
    }
    
    # write tables
    write.csv(rf_eval_df, sprintf("rf_eval_%s_%s.csv", dataset_name, Sys.Date()))
    #write.csv(var_imp_df, sprintf("var_imp_df_%s_%s.csv", dataset_name, Sys.Date()))
    write.csv(var_rank_df, sprintf("var_rank_%s_%s.csv", dataset_name, Sys.Date()))
  }
}



setwd("O:/PRIV/NERL_ORD_CYAN/Salls_working/GLB/Analysis/RF/out")

## aggregate ranking tables
rank_files <- list.files(".", pattern = "var_rank_")

rank_summary <- data.frame()

for (f in 1:length(rank_files)) {
  fname <- rank_files[f]
  rank_csv <- read.csv(fname, stringsAsFactors = FALSE)
  ranks_f <- rank_csv[, which(colnames(rank_csv) %in% c("var", "cum_rank"))]
  
  rank_summary <- merge(rank_summary, ranks_f,
                        by = "var", all.y = TRUE)
  colnames(rank_summary)[f + 1] <- substr(fname, 1, (regexpr("_CI_sp90th_tmedian_2018-11-06.csv", fname) - 1))
}

write.csv(rank_summary, sprintf("rank_summary_%s_%s.csv", subset_var, Sys.Date()))


## aggregate eval tables
eval_files <- list.files(".", pattern = "rf_eval_")

eval_summary <- data.frame()

for (f in 1:length(eval_files)) {
  fname <- eval_files[f]
  eval_csv <- read.csv(fname, stringsAsFactors = FALSE)
  colmeans <- as.data.frame(t(apply(eval_csv[, 3:11], 2, mean)))
  
  eval_summary <- rbind(eval_summary, data.frame(name = sub("rf_eval_CI_sp90th_tmedian_", "", fname), colmeans))
}

write.csv(eval_summary, sprintf("rf_evals_summary_%s.csv", Sys.Date()))


### ----------------------

##

# individual

'
resp <- responses[1]
resp_data <- lake_data[, resp]
rf <- randomForest(x = pred_data, y = resp_data, na.action = na.omit)
importance(rf, type = 2)
'
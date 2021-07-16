# use after running setup (top) section of RF_GLB_180815.R

library(party)

options(stringsAsFactors = FALSE)



#

nruns <- 100 # number of forest runs

# run CIF
set.seed(1)

cf_list <- list()

var_imp_df <- data.frame(var = colnames(pred_data))

for (f in 1:nruns) {
  print(sprintf("#%s at %s", f, Sys.time()))
  start <- Sys.time()
  
  cf <- cforest(formula = resp_data ~ ., data = data.frame(resp_data, pred_data), 
                controls = cforest_unbiased(mtry = floor(ncol(pred_data) / 3)))
  
  cf_list[[f]] <- cf
  
  varimp_cf <- varimp(cf, conditional = TRUE)
  
  vi_f <- data.frame(var = names(varimp_cf), score = (varimp_cf))
  rownames(vi_f) <- NULL
  colnames(vi_f)[2] <- paste0("score_", f)
  
  var_imp_df <- merge(var_imp_df, vi_f, by = "var")
  
  print(sprintf("Done after %s", Sys.time() - start))
}


var_rank_df <- data.frame(var = pred_vars)

for (f in 2:(nruns + 1)) {
  f_scores <- var_imp_df[, c(1, f)]
  f_scores <- f_scores[order(f_scores[, 2], decreasing = TRUE), ]
  f_scores$rank <- 1:nrow(f_scores)
  
  var_rank_df <- merge(var_rank_df, f_scores[, c(1, 3)], by = "var")
  colnames(var_rank_df)[f] <- paste0("rank_", f - 1)
}


### <<<<< now run the code to summarize importance metrics and ranks across runs >>>>>

setwd("C:/Users/WSALLS/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/LandscapeAnalysis/Publication")
write.csv(var_imp_df, sprintf("conditional_importance_%s_%s_%s.csv", lakes_region, ncol(pred_data), Sys.Date()))
write.csv(var_rank_df, sprintf("conditional_ranks_%s_%s_%s.csv", lakes_region, ncol(pred_data), Sys.Date()))


###


# error?

cf_eval <- data.frame()

for (c in 1:length(cf_list)) {
  cf_f_metrics <- caret:::cforestStats(cf_list[[c]])
  cf_eval <- rbind(cf_eval, data.frame(tree = f,
                                       RMSE = cf_f_metrics[1],
                                       Rsquared = cf_f_metrics[2],
                                       MAE = cf_f_metrics[3]))
}

cf_eval[nrow(cf_eval) + 1, ] <- apply(cf_eval, 2, mean)
cf_eval$tree[nrow(cf_eval)] <- "mean"
rownames(cf_eval) <- NULL

write.csv(cf_eval, sprintf("eval_CF_%s_%s_%s.jpg",lakes_region, ncol(pred_data), Sys.Date()))

# use after running setup (top) section of RF_GLB_180815.R

options(stringsAsFactors = FALSE)



#

#rf <- randomForest(x = pred_data, y = resp_data, na.action = na.omit)

# run CIF
set.seed(1)

cf_imp_list <- c()

for (f in 1:20) {
  print(sprintf("#%s at %s", f, Sys.time()))
  start <- Sys.time()
  
  cf <- cforest(formula = resp_data ~ ., data = data.frame(resp_data, pred_data), 
                controls = cforest_unbiased(mtry = floor(ncol(pred_data) / 3)))
  
  
  
  varimp_cf <- varimp(cf, conditional = TRUE)
  
  vi2 <- data.frame(var = names(varimp_cf), score = (varimp_cf))
  
  vi_sort <- vi2[order(vi2$score, decreasing = TRUE), ]
  rownames(vi_sort) <- NULL
  
  cf_imp_list <- c(cf_imp_list, vi_sort)
  
  print(sprintf("Done after %s", Sys.time() - start))
}


# format list into rank table

imp_scores <- data.frame(variable = cf_imp_list[[1]],  score1 = cf_imp_list[[2]])

imp_scores1 <- imp_scores
imp_scores1$rank1 <- 1:ncol(pred_data)
imp_ranks <- imp_scores1[, -which(colnames(imp_scores1) =="score1")]

for (i in 2:(length(cf_imp_list) / 2)) {
  df_i <- data.frame(variable = cf_imp_list[[(i * 2) - 1]],  score = cf_imp_list[[i * 2]])
  
  ranks_i <- df_i
  ranks_i$rank <- 1:ncol(pred_data)
  ranks_i <- ranks_i[, -which(colnames(imp_scores) =="score")]
  
  colnames(df_i)[2] <- paste0("score", i)
  colnames(ranks_i)[2] <- paste0("rank", i)
  
  imp_scores <- merge(imp_scores, df_i, by = "variable")
  imp_ranks <- merge(imp_ranks, ranks_i, by = "variable")
}



rf_results <- read.csv("/Users/wilsonsalls/Desktop/EPA/GLB/Analysis/RF/out_completed/180604_rF/var_rank_df_CI_sp90th_tmedian_2018-06-04.csv")

cf_results <- read.csv("")

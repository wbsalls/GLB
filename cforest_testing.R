# use after running setup (top) section of RF_GLB_180815.R


resp <- "CI_sp90th_tmedian"
resp_data <- lake_data[, resp]

#

#rf <- randomForest(x = pred_data, y = resp_data, na.action = na.omit)

# run CIF
set.seed(1)

cf_imp_list <- c()

for (f in 1:20) {
  print(sprintf("#%s at %s", f, Sys.time()))
  start <- Sys.time()
  
  cf <- cforest(formula = resp_data ~ ., data = data.frame(resp_data, pred_data), 
                controls = cforest_unbiased(mtry = floor(sqrt(ncol(pred_data)))))
  
  
  
  varimp_cf <- varimp(cf, conditional = TRUE)
  
  vi2 <- data.frame(var = names(varimp_cf), score = (varimp_cf))
  
  vi_sort <- vi2[order(vi2$score), ]
  rownames(vi_sort) <- NULL
  
  cf_imp_list <- c(cf_imp_list, vi_sort)
  
  print(sprintf("Done after %s", Sys.time() - start))
}


# format list into rank table

imp_scores <- data.frame(variable = cf_imp_list[[1]],  score1 = cf_imp_list[[2]])

imp_scores1 <- imp_scores
imp_scores1$rank1 <- 75:1
imp_ranks <- imp_scores1[, -which(colnames(imp_scores1) =="score1")]

for (i in 2:(length(cf_imp_list) / 2)) {
  df_i <- data.frame(variable = cf_imp_list[[(i * 2) - 1]],  score = cf_imp_list[[i * 2]])
  
  ranks_i <- df_i
  ranks_i$rank <- 75:1
  ranks_i <- ranks_i[, -which(colnames(imp_scores1) =="score")]
  
  colnames(df_i)[2] <- paste0("score", i)
  colnames(ranks_i)[2] <- paste0("rank", i)
  
  imp_scores <- merge(imp_scores, df_i, by = "variable")
  imp_ranks <- merge(imp_ranks, ranks_i, by = "variable")
}




# rf_run, eval_model, var_importance, var_import_plot

library(randomForest)
library(scales) # alpha


# random forest function --------------------------

rf_run_impute <- function(predictors, response) {
  
  # remove records with NA response
  predictors_clean <- predictors[which(!is.na(response)), ]
  response_clean <- response[which(!is.na(response))]
  
  # fill in NA predictor values by imputation, if necessary
  if (sum(is.na(predictors)) > 0) {
    imputed <- rfImpute(x = predictors_clean, y = response_clean)
  } else {
    imputed <- cbind(response_clean, predictors_clean)
  }
  
  # run random forest
  rf <- randomForest(x = imputed[, 2:ncol(imputed)], 
                     y = imputed[, 1], 
                     na.action = na.omit)
  
  return(rf)
}


# evaluate model function ------------------------------
# for continuous response only

eval_model <- function(actual, predicted, run_name, plot_fit = TRUE, save_plot = FALSE, save_dir = getwd()) {
  # combine actual and predicted into one df; calculate error, then rmse and mae
  preds_df <- as.data.frame(cbind(actual, predicted))
  preds_df$error <- preds_df$actual - preds_df$predicted
  rmse <- sqrt(mean((preds_df$error)^2, na.rm = TRUE))
  mae <- mean(abs(preds_df$error), na.rm = TRUE)
  
  # run regression model to generate regression statistics
  model_c <- lm(preds_df[, 2] ~ preds_df[, 1])
  
  # append statistics to data frame
  eval_df_row <- data.frame(run = run_name, 
                            RMSE = rmse, 
                            MAE = mae, 
                            Rsq = summary(model_c)$r.squared, 
                            adjRsq = summary(model_c)$adj.r.squared, 
                            slope = model_c$coefficients[2], 
                            intercept = model_c$coefficients[1], 
                            n = nrow(preds_df))
  rownames(eval_df_row) <- NULL
  
  # plot regression and save to file
  if (plot_fit == TRUE) {
    if (save_plot == TRUE) {jpeg(sprintf('%s/pred_vs_actual_%s.jpg', save_dir, run_name))}
    
    plot(x = preds_df[, 1], y = preds_df[, 2], 
         xlim = c(0, max(preds_df[, 1:2], na.rm = TRUE)), ylim = c(0, max(preds_df[, 1:2], na.rm = TRUE)), 
         xlab = "CI Observed", ylab = "CI Predicted", main = run_name, cex.lab = 1.4, cex.axis = 1.2, 
         pch = 20, col = alpha("black", 0.3)) # family = "serif"
    abline(0,1)
    text(x = 0.05 * max(preds_df[, 1:2]), y = 0.9 * max(preds_df[, 1:2]), family = "serif", 
         labels = paste("RMSE = ", round(rmse, 3), "\n", 
                        "MAE = ", round(mae, 3), "\n", 
                        "adj. R-sq. = ", round(summary(model_c)$adj.r.squared, 3), sep = ""), 
         pos = 4)
    
    if (save_plot == TRUE) {dev.off()}
  }
  
  return(eval_df_row)
}


# variable importance ---------------------------------------------------
var_importance <- function(rf_output) {
  # make importance object into data frame
  imp <- data.frame(var = rownames(rf_output$importance), 
                    importance_measure = rf_output$importance)
  rownames(imp) <- NULL
  
  # order importance data frame
  importance_ordered <- imp[order(-imp[, 2]), ]
  rownames(importance_ordered) <- 1:nrow(importance_ordered)
  
  return(importance_ordered)
}




# variable importance plotting function ------------------------------------
# rf object, string with path and filename, string - name for output file, integer, logical, logical

var_import_plot <- function(rf_output, var_file, response_name, nvar_show = 20, save_plot = FALSE, save_dir = getwd(), return_df = FALSE) {
  
  #create ordered variable importance dataframe
  imp_ordered <- var_importance(rf_output)
  
  # add formatted variable names to importance data frame to later apply to plot
  for (v in 1:nrow(imp_ordered)) {
    imp_ordered$var_full[v] <- var_file$Label[which(var_file$Variable == imp_ordered$var[v])]
  }
  
  # get vector of formatted variable names to apply to plot
  var_labels <- imp_ordered$var_full
  
  # variable importance dot chart for regression or classification; save to file if specified in call
  if (rf_output$type == "regression") {
    if (save_plot == TRUE) {jpeg(sprintf('%s/VarImpPlot_%s.jpg', save_dir, response_name), width = 1000, height = 700)}
    
    dotchart(imp_ordered[nvar_show:1, 2], labels = var_labels[nvar_show:1], 
             xlim = c(0, max(imp_ordered[, 2])),
             xlab = "Increase in Node Purity", family = "sans", cex = 1.2, 
             main = response_name)
    
    if (save_plot == TRUE) {dev.off()}
  }
  
  if (rf_output$type == "classification") {
    if (save_plot == TRUE) {jpeg(sprintf('%s/VarImpPlot_%s.jpg', save_dir, response_name), width = 1000, height = 700)}
    
    dotchart(imp_ordered[nvar_show:1, 2], labels = var_labels[nvar_show:1], 
             xlim = c(0, max(imp_ordered[, 2])),
             xlab = "Mean Decrease in Gini Index", family = "sans", cex = 1.2, 
             main = response_name)
    
    if (save_plot == TRUE) {dev.off()}
  }
  
  # return importance df, if specified
  if (return_df == TRUE) {
    return(imp_ordered[, c(1, 3, 2)])
  }
}

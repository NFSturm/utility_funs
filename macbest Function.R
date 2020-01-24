# Function to compare binary classifiers

# Preprocessing

mac_list <- list(probit = stargazer_mod22, boosting = enterprise_mod53, svmlin = voyager_mod8, svmrad = borgcube_mod60, dectree = defiant_mod9)

# This function returns the test matrix as well as results of the McNemar test in a list.

# model1, model2: models to be compared
# data: data, which contains true value labels
# dep: name of dependent variable (as character string)
# s: decision threshold (0.5 by default)
# y: character string specifying factor level name for positive class

# Attention: Dplyr Dependency for "bind_cols"

macbest <- function(model1, model2, data, dep, s = 0.5, y){
  
  data <- as.data.frame(data)
  model1_preds <- predict(model1, data, type = "prob")
  model2_preds <- predict(model2, data, type = "prob")
  
  h1_preds <- ifelse(model1_preds[y] > s, 1,0)
  
  h2_preds <- ifelse(model2_preds[y] > s, 1, 0)
  
  true <- data[dep]
  true_values <- ifelse(true == y, 1, 0)
  
  comp_df <- bind_cols(h1preds = h1_preds, h2preds = h2_preds, truth = true_values)
  
  comp_df$h1comp <- ifelse(comp_df$h1preds == comp_df$truth, 1, 0)
  
  comp_df$h2comp <- ifelse(comp_df$h2preds == comp_df$truth, 1,0)
  
  h1h2_both_right <- sum(ifelse(comp_df$h1comp == 1 & comp_df$h2comp == 1, 1,0))
  h1h2_both_wrong <- sum(ifelse(comp_df$h1comp == 0 & comp_df$h2comp == 0, 1,0))
  h1h2_first_right <- sum(ifelse(comp_df$h1comp == 1 & comp_df$h2comp == 0, 1,0))
  h1h2_second_right <- sum(ifelse(comp_df$h1comp == 0 & comp_df$h2comp == 1, 1,0))
  
  test_mat <- matrix(data = c(h1h2_both_wrong, h1h2_second_right, h1h2_first_right, h1h2_both_right), nrow = 2)
  if(test_mat[1,2] == 0 & test_mat[2,1] == 0) stop("Contigency tables contains null values")  
  test_res <- mcnemar.test(test_mat)
  
  res <- list(test_res, test_mat)
  
  res
  
}








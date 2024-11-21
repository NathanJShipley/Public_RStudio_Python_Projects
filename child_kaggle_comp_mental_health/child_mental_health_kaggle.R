#==============================================================================
#                              AP Vote Prediction Analysis                   #      
#==============================================================================

# Final model got a .299 which was a pretty low score, but got really bored with this one. Really digs more into missing data than anything else. Not that interested
# https://www.kaggle.com/code/drtendy/main-submission/edit/run/208746669

#=========================================== =
# Load Required Libraries                 #
#=========================================== =

library(tidyverse, quietly = TRUE)     # Contains dplyr and ggplot2 for data manipulation and visualization
library(caret, quietly = TRUE)         # Machine learning pipeline and models
library(recipes, quietly = TRUE)       # Set up recipes for ML and frameworks
library(jtools, quietly = TRUE)        # Working with linear models
library(mice, quietly = TRUE)          # Multiple imputation
library(earth, quietly = TRUE)         # Multivariate adaptive regression splines (MARS)
library(mgcv, quietly = TRUE)          # Generalized additive models (GAM)
library(psych, quietly = TRUE)         # Correlation visualizations (e.g., pairs.panels)
library(FactoMineR, quietly = TRUE)    # Principal Component Analysis (PCA)
library(factoextra, quietly = TRUE)    # Visualization of PCA results
library(ranger, quietly = TRUE)        # Random forest models
library(tictoc, quietly = TRUE)        # Timing models


# Also lets set a seed for reproducibility of results
set.seed(123)


#===========================================
# LOAD DATA & Clean up                      #
#===========================================

train_dat <- read.csv("data/train.csv")
test_dat <- read.csv("data/test.csv")

# Lets just remove where DV is NA
# Also lets do some data manipulation, convert blank to NAs and then convert character to factor
train_dat <- train_dat %>%
  filter(!is.na(sii)) %>%  # Remove rows where the dependent variable is NA
  mutate(across(where(is.character), ~ na_if(., ""))) %>%  # Replace blank strings with NA
  mutate(across(where(is.character), as.factor)) %>%
  select(where(~ mean(is.na(.)) <= 0.70)) %>% # Drop columns with > 80% missing values
  filter(rowMeans(is.na(.)) <= 0.40) %>%
  janitor::clean_names() %>%
  mutate(sii = as.factor(sii)) %>%
  select(-c(id, 'pciat_pciat_total','fitness_endurance_season', 'sds_sds_total_raw') ) # Remove specific columns

# Load in the test data also and apply similar functions
test_dat <- test_dat %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%  # Replace blank strings with NA
  mutate(across(where(is.character), as.factor)) %>%
  janitor::clean_names() %>%
  select(-c( 'fitness_endurance_season', 'sds_sds_total_raw') ) # Remove specific columns

test_dat$sii <- NA # add this to the test data so the other transformations can work with it 
test_dat$sii <- as.factor(test_dat$sii)


# Lets drop any variable missing from the test data, which does look like some of the 
common_columns <- intersect(colnames(train_dat), colnames(test_dat)) # Just find the matching column names
#common_columns <- union(common_columns, "sii") #include sii outcome in train
train_dat_filtered <- train_dat[, common_columns]


# # # # # Looked at MISSING DATA and Aliased variables
# Removed anything greater NA than 70%

# na_percent_filter <- colMeans(is.na(train_dat)) * 100 
# na_percent_filter_sorted <- sort(na_percent_filter, decreasing = TRUE)
# na_percent_filter_sorted_df <- data.frame(Variable = names(na_percent_filter_sorted), 
#                                           Missing_Percentage = na_percent_filter_sorted)


# Dropped PCIAT.PCIAT_Total,Fitness_Endurance.Season due to aliasing issues



#===========================================
# Assess Impact of Missing, Imputing, and Factoring               #
#===========================================
# 
# # Very important step here, lets dummy code all. Come back and 
# train_dat_numeric <- train_dat_filtered %>%
#   mutate(across(where(is.factor), as.numeric)) %>%
#   mutate(across(where(is.character), as.numeric))
# 
# #train_dat_imputed <- mice(train_dat_numeric, method = "pmm", m = 5)  # pmm = predictive mean matching
# 
# 
# 
# # BIA Variables
# # So lets just select the BIA data
# BIA_numeric <- train_dat_numeric %>% select(starts_with("BIA"))
# # Set up the PCA
# bia_pca <- PCA(BIA_numeric, scale.unit = TRUE, graph = TRUE)
# # Get the output, looks like 5 variables probably are good
# summary(bia_pca)
# fviz_screeplot(bia_pca, addlabels = TRUE)
# fviz_pca_var(bia_pca, col.var = "contrib", gradient.cols = c("blue", "green", "red"))


# Now do the same thing for the imputed data, make sure 5 variables still work 
# 
# BIA_imputed_list <- lapply(1:5, function(i) {
#   # Extract the ith imputed dataset
#   imputed_data <- complete(train_dat_imputed, action = i)
#   
#   # Select only the columns that start with "BIA"
#   imputed_data_bia <- imputed_data %>% select(starts_with("BIA"))
#   
#   # Return the filtered dataset
#   return(imputed_data_bia)
# })
# 
# BIA_imputed_combined <- bind_rows(BIA_imputed_list)
# bia_pca_imputed <- PCA(BIA_imputed_combined, scale.unit = TRUE, graph = TRUE)
# summary(bia_pca_imputed)
# fviz_screeplot(bia_pca_imputed, addlabels = TRUE)
# fviz_pca_var(bia_pca_imputed, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# 
# # So, BIA looks good to impute and get 5 components via PCA
# 
# 
# # FGC Variables
# FGC_numeric <- train_dat_numeric %>% select(starts_with("FGC"))
# # Set up the PCA
# FGC_pca <- PCA(FGC_numeric, scale.unit = TRUE, graph = TRUE)
# # Get the output, looks like 5 variables probably are good
# summary(FGC_pca)
# fviz_screeplot(FGC_pca, addlabels = TRUE)
# fviz_pca_var(FGC_pca, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# 
# 
# # Now do the same thing for the imputed data, make sure 5 variables still work 
# 
# FGC_imputed_list <- lapply(1:5, function(i) {
#   # Extract the ith imputed dataset
#   imputed_data <- complete(train_dat_imputed, action = i)
#   
#   # Select only the columns that start with "FGC"
#   imputed_data_bia <- imputed_data %>% select(starts_with("FGC"))
#   
#   # Return the filtered dataset
#   return(imputed_data_bia)
# })
# 
# FGC_imputed_combined <- bind_rows(FGC_imputed_list)
# FGC_pca_imputed <- PCA(FGC_imputed_combined, scale.unit = TRUE, graph = TRUE)
# summary(FGC_pca_imputed)
# fviz_screeplot(FGC_pca_imputed, addlabels = TRUE)
# fviz_pca_var(FGC_pca_imputed, col.var = "contrib", gradient.cols = c("blue", "green", "red"))

# 7 looks good for this one also! 




# PCIAT
# PCIAT_numeric <- train_dat_numeric %>% select(starts_with("PCIAT"))
# # Set up the PCA
# PCIAT_pca <- PCA(PCIAT_numeric, scale.unit = TRUE, graph = TRUE)
# # Get the output, looks like 5 variables probably are good
# summary(PCIAT_pca)
# fviz_screeplot(PCIAT_pca, addlabels = TRUE)
# fviz_pca_var(PCIAT_pca, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# 
# 
# # Now do the same thing for the imputed data, make sure 5 variables still work 
# 
# PCIAT_imputed_list <- lapply(1:5, function(i) {
#   # Extract the ith imputed dataset
#   imputed_data <- complete(train_dat_imputed, action = i)
#   
#   # Select only the columns that start with "PCIAT"
#   imputed_data_bia <- imputed_data %>% select(starts_with("PCIAT"))
#   
#   # Return the filtered dataset
#   return(imputed_data_bia)
# })
# 
# PCIAT_imputed_combined <- bind_rows(PCIAT_imputed_list)
# PCIAT_pca_imputed <- PCA(PCIAT_imputed_combined, scale.unit = TRUE, graph = TRUE)
# summary(PCIAT_pca_imputed)
# fviz_screeplot(PCIAT_pca_imputed, addlabels = TRUE)
# fviz_pca_var(PCIAT_pca_imputed, col.var = "contrib", gradient.cols = c("blue", "green", "red"))


# 4 looks decent





# # Physical
# Physical_numeric <- train_dat_numeric %>% select(starts_with("Physical"))
# # Set up the PCA
# Physical_pca <- PCA(Physical_numeric, scale.unit = TRUE, graph = TRUE)
# # Get the output, looks like 5 variables probably are good
# summary(Physical_pca)
# fviz_screeplot(Physical_pca, addlabels = TRUE)
# fviz_pca_var(Physical_pca, col.var = "contrib", gradient.cols = c("blue", "green", "red"))
# 
# 
# # Now do the same thing for the imputed data, make sure 5 variables still work 
# 
# Physical_imputed_list <- lapply(1:5, function(i) {
#   # Extract the ith imputed dataset
#   imputed_data <- complete(train_dat_imputed, action = i)
#   
#   # Select only the columns that start with "Physical"
#   imputed_data_bia <- imputed_data %>% select(starts_with("Physical"))
#   
#   # Return the filtered dataset
#   return(imputed_data_bia)
# })
# 
# Physical_imputed_combined <- bind_rows(Physical_imputed_list)
# Physical_pca_imputed <- PCA(Physical_imputed_combined, scale.unit = TRUE, graph = TRUE)
# summary(Physical_pca_imputed)
# fviz_screeplot(Physical_pca_imputed, addlabels = TRUE)
# fviz_pca_var(Physical_pca_imputed, col.var = "contrib", gradient.cols = c("blue", "green", "red"))


# 4 variables


#===========================================
#  Set up final recipe to run with Caret   #
#===========================================

train_recipie <- recipes::recipe(sii ~ ., data = train_dat_filtered) %>%
                    update_role(sii, new_role = "outcome") %>%
                    step_impute_median(all_numeric()) %>%           # Median imputation for numeric variables
                    step_impute_mode(all_nominal()) %>%             # Mode imputation for nominal (character and factors )
                    step_dummy(all_nominal(), -all_outcomes()) %>%                   # Dummy code for nominal (character and factors )
                    # PCA on Key Variables
                    step_pca(starts_with("BIA"), num_comp = 5, prefix = "BIA_PC_") %>%  # Apply PCA on variables that start with "BIA" with 5 components
                    step_pca(starts_with("FGC"), num_comp = 7, prefix = "FGC_PC_") %>% # Apply PCA on variables that start with "FGC" with 7 components
                    step_pca(starts_with("Physical"), num_comp = 4, prefix = "Physical_PC_")


# Lets check to make sure the recipe is generally working, but will not use in the final caret training models 
# Prepare the recipe
prepared_train_recipie <- prep(train_recipie, training = train_dat_filtered)

# Apply the recipe to the training data
train_dat_transformed <- bake(prepared_train_recipie, new_data = train_dat_filtered)

# Now I want to do a final analysis of VIFs before moving this recipe onto more detailed ML analysis
# 
# multinom_fit <- nnet::multinom(sii ~., data = train_dat_transformed)
# multinom_fit
# 
# 
# train_dat_transformed$pred_sii <- predict(multinom_fit, newdata = train_dat_transformed)
# conf_matrix <- caret::confusionMatrix(train_dat_transformed$pred_sii, train_dat_transformed$sii)
# print(conf_matrix)



# test_dat_transformed <- bake(prepared_train_recipie, new_data = test_dat)
# # test_dat_transformed$predicted_sii <- predict(multinom_fit, newdata = test_dat_transformed)
# 
# 
# 
# # test_dat_transformed$predicted_sii <- predict(multinom_fit, newdata = test_dat_transformed)
# test_dat_transformed$id <- test_dat$id
# test_dat_transformed$id
# test_dat_transformed$predicted_sii
# 
# 
# result <- data.frame(id = test_dat_transformed$id,
#                      sii = test_dat_transformed$predicted_sii)


# ############################################################################ # # # # #
# #########################=========================================== #
# ######################### Time for more refined analysis with smaller data set   #
# #########################=========================================== #
# ############################################################################ # # # # #

########## VERSION 1 model build out 
# 
# train_control <- caret::trainControl(method = "repeatedcv",
#                                      number = 10, # Change back to 10 and set tuneLength to a higher number when running the initial pre-tuned models
#                                      repeats = 5)
# 
# cl <- parallel::makePSOCKcluster(10)
# doParallel::registerDoParallel(cl)
# 
# tictoc::tic()
# xgb.fit.v1 <- caret::train(sii ~., data = train_dat_transformed, method = "xgbTree", trControl = train_control, tuneLength = 5)
# tictoc::toc() ### 1671.81 sec elapsed
# 
# # tictoc::tic()
# # svm.fit.v1 <- caret::train(sii ~., data = train_dat_transformed, method = "svmRadial", trControl = train_control, tuneLength = 5)
# # tictoc::toc() ### 25.97 sec elapsed
# # 
# # tictoc::tic()
# # nnet.fit.v1 <- caret::train(sii ~., data = train_dat_transformed, method = "nnet", trControl = train_control)
# # tictoc::toc() ### 6.31 sec elapsed
# 
# tictoc::tic()
# gbm.fit.v1 <- caret::train(sii ~., data = train_dat_transformed, method = "gbm", trControl = train_control, tuneLength = 5)
# tictoc::toc() ### 57.98 sec elapsed
# 
# # tictoc::tic()
# # earth.fit.v1 <- caret::train(sii ~., data = train_dat_transformed, method = "earth", trControl = train_control, tuneLength = 5)
# # tictoc::toc() ### 10.97 sec elapsed
# # 
# # tictoc::tic()
# # bagearth.fit.v1 <- caret::train(sii ~., data = train_dat_transformed, method = "bagEarth", trControl = train_control, tuneLength = 5)
# # tictoc::toc() ### 447.2 sec elapsed
# 
# tictoc::tic()
# ranger.fit.v1 <- caret::train(sii ~., data = train_dat_transformed, method = "ranger", trControl = train_control, tuneLength = 5)
# tictoc::toc() ###102.41 sec elapsed
# 
# # tictoc::tic()
# # glmnet.fit.v1 <- caret::train(sii ~., data = train_dat_transformed, method = "glmnet", trControl = train_control, tuneLength = 5)
# # tictoc::toc() ### 5.92 sec elapsed
# 
# tictoc::tic()
# knn.fit.v1 <- caret::train(sii ~., data = train_dat_transformed, method = "knn", trControl = train_control, tuneLength = 5)
# tictoc::toc() ### 1.27 sec elapsed
# 
# # tictoc::tic()
# # lightgbm.fit.v1 <- caret::train(sii ~., data = train_dat_transformed, method = "lightgbm", trControl = train_control, tuneLength = 5)
# # tictoc::toc() ### 
# # 
# # tictoc::tic()
# # catboost.fit.v1 <- caret::train(sii ~., data = train_dat_transformed, method = "catboost", trControl = train_control, tuneLength = 5)
# # tictoc::toc() ### 
# 
# 
# 
# parallel::stopCluster(cl)
# 
# 
# 
# xgb.fit.v1$bestTune
# #nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# #     50         2 0.3     0              0.6                1         1
# train_dat_transformed$xgb_sii <- predict(xgb.fit.v1, newdata = train_dat_transformed)
# print(caret::confusionMatrix(train_dat_transformed$xgb_sii, train_dat_transformed$sii)) #Accuracy : 0.71
# 
# # svm.fit.v1$bestTune
# # train_dat_transformed$svm_sii <- predict(svm.fit.v1, newdata = train_dat_transformed)
# # print(caret::confusionMatrix(train_dat_transformed$svm_sii, train_dat_transformed$sii)) #Accuracy : 0.63 LOL
# 
# gbm.fit.v1$bestTune
# #n.trees interaction.depth shrinkage n.minobsinnode
# #     50                 2       0.1             10
# train_dat_transformed$gbm_sii <- predict(gbm.fit.v1, newdata = train_dat_transformed)
# print(caret::confusionMatrix(train_dat_transformed$gbm_sii, train_dat_transformed$sii)) #Accuracy : 0.67
# 
# # earth.fit.v1$bestTune
# # train_dat_transformed$earth_sii <- predict(earth.fit.v1, newdata = train_dat_transformed)
# # print(caret::confusionMatrix(train_dat_transformed$earth_sii, train_dat_transformed$sii)) #Accuracy : 0.62
# 
# # bagearth.fit.v1$bestTune
# # train_dat_transformed$bearth_sii <- predict(bagearth.fit.v1, newdata = train_dat_transformed)
# # print(caret::confusionMatrix(train_dat_transformed$bearth_sii, train_dat_transformed$sii)) #Accuracy : 0.64
# 
# ranger.fit.v1$bestTune
# #mtry splitrule min.node.size
# #  10      gini             1
# train_dat_transformed$ranger_sii <- predict(ranger.fit.v1, newdata = train_dat_transformed)
# print(caret::confusionMatrix(train_dat_transformed$ranger_sii, train_dat_transformed$sii)) #Accuracy : 1??? Okay, something strange going on there
# 
# # glmnet.fit.v1$bestTune
# # train_dat_transformed$glm_sii <- predict(glmnet.fit.v1, newdata = train_dat_transformed)
# # print(caret::confusionMatrix(train_dat_transformed$glm_sii, train_dat_transformed$sii)) #Accuracy : 0.61
# 
# knn.fit.v1$bestTune
# #k
# #11
# train_dat_transformed$knn_sii <- predict(knn.fit.v1, newdata = train_dat_transformed)
# print(caret::confusionMatrix(train_dat_transformed$knn_sii, train_dat_transformed$sii)) #Accuracy : 0.64








# ############################################################################ # # # # #
# #########################=========================================== #
# ######################### Time for more refined analysis with smaller data set   #
# #########################=========================================== #
# ############################################################################ # # # # #


# train_control <- caret::trainControl(method = "repeatedcv",
#                                      number = 10, # Change back to 10 and set tuneLength to a higher number when running the initial pre-tuned models
#                                      repeats = 5)
# 
# expand_grid_xgb <- expand.grid(
#   nrounds = c(50, 100, 200),
#   max_depth = c(4, 6, 8),
#   eta = c(0.01, 0.1, 0.3),
#   gamma = c(0, 0.1, 1),
#   colsample_bytree = c(0.6, 0.8),
#   min_child_weight = c(1, 3, 5),
#   subsample = c(0.7, 0.9)
# )


# expand_gbm_grid <- expand.grid(
#   n.trees = c(50, 100, 150, 200, 300),
#   interaction.depth = c(1, 2, 3, 4),
#   shrinkage = c(0.01, 0.05, 0.1, 0.2),
#   n.minobsinnode = c(5, 10, 15, 20)
# )
# 
# 
# expand_grid_ranger <- expand.grid(
#   mtry = c(3, 5, 6, 7),
#   splitrule = c("gini"),
#   min.node.size = c(4, 5, 6, 7)
# )


# cl <- parallel::makePSOCKcluster(10)
# doParallel::registerDoParallel(cl)
# 
# 
# tictoc::tic()
# xgb.fit.v2 <- caret::train(sii ~., data = train_dat_transformed, method = "xgbTree", trControl = train_control, tuneGrid = expand_grid_xgb )
# tictoc::toc() ### 
# 
# # tictoc::tic()
# # gbm.fit.v2 <- caret::train(sii ~., data = train_dat_transformed, method = "gbm", trControl = train_control, tuneGrid = expand_gbm_grid)
# # tictoc::toc() ### 
# # 
# # tictoc::tic()
# # ranger.fit.v2 <- caret::train(sii ~., data = train_dat_transformed, method = "ranger", trControl = train_control, tuneGrid = expand_grid_ranger)
# # tictoc::toc() ### 73.43 sec elapsed
# 
# parallel::stopCluster(cl)
# 
# 
# xgb.fit.v2
# plot(xgb.fit.v2)
# xgb.fit.v2$bestTune
#nrounds max_depth  eta gamma colsample_bytree min_child_weight subsample
#    50         4 0.01     1              0.8                1       0.9


# gbm.fit.v2
# plot(gbm.fit.v2)
# gbm.fit.v2$bestTune
# #n.trees interaction.depth shrinkage n.minobsinnode
# #   100                 4      0.01              5
# 
# 
# ranger.fit.v2
# plot(ranger.fit.v2)
# ranger.fit.v2$bestTune
# #mtry splitrule min.node.size
# #  6      gini             4






# train_dat_transformed$xgb_sii <- predict(xgb.fit.v2, train_dat_transformed)
# train_dat_transformed$gbm_sii <- predict(gbm.fit.v2, train_dat_transformed)
# train_dat_transformed$ranger_sii <- predict(ranger.fit.v2, train_dat_transformed)
# 
# print(caret::confusionMatrix(train_dat_transformed$xgb_sii, train_dat_transformed$sii))
# print(caret::confusionMatrix(train_dat_transformed$gbm_sii, train_dat_transformed$sii))
# print(caret::confusionMatrix(train_dat_transformed$ranger_sii, train_dat_transformed$sii))
# 
# 
# 
# # test_dat_transformed$predicted_sii <- predict(multinom_fit, newdata = test_dat_transformed)
# # test_dat_transformed$gbm_sii <- predict(gbm.fit.v2, newdata = test_dat_transformed)
# # test_dat_transformed$ranger_sii <- predict(ranger.fit.v2, newdata = test_dat_transformed)
# 
# test_dat_transformed$id <- test_dat$id
# test_dat_transformed$id
# test_dat_transformed$predicted_sii
# 
# print(caret::confusionMatrix(train_dat_transformed$ranger_sii, train_dat_transformed$sii))
# 
# 
# 
# 
# result <- data.frame(id = test_dat_transformed$id,
#                      sii = test_dat_transformed$predicted_sii)










# train_recipie_scale <- recipes::recipe(sii ~ ., data = train_dat_filtered) %>%
#   update_role(sii, new_role = "outcome") %>%
#   step_impute_median(all_numeric()) %>%           # Median imputation for numeric variables
#   step_impute_mode(all_nominal()) %>%             # Mode imputation for nominal (character and factors )
#   step_dummy(all_nominal(), -all_outcomes()) %>%                   # Dummy code for nominal (character and factors )
#   # PCA on Key Variables
#   step_pca(starts_with("BIA"), num_comp = 5, prefix = "BIA_PC_") %>%  # Apply PCA on variables that start with "BIA" with 5 components
#   step_pca(starts_with("FGC"), num_comp = 7, prefix = "FGC_PC_") %>% # Apply PCA on variables that start with "FGC" with 7 components
#   step_pca(starts_with("Physical"), num_comp = 4, prefix = "Physical_PC_")
# 
# 
# # Lets check to make sure the recipe is generally working, but will not use in the final caret training models 
# # Prepare the recipe
# prepared_train_recipie <- prep(train_recipie, training = train_dat_filtered)
# 
# # Apply the recipe to the training data
# train_dat_transformed <- bake(prepared_train_recipie, new_data = train_dat_filtered)












###################################################################################################### #
########################################################## # # # #
########################################################## # # # #
########################################################## # # # #   Round 4 - okay, maybe now lets run some final models 
########################################################## # # # #
########################################################## # # # #
###################################################################################################### #


final_train_control <- trainControl(method = "none")

tune_grid_xgb_final <- expand.grid(
  nrounds = 50,
  max_depth = 4,
  eta = 0.01,
  gamma = .1,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.9
)

tune_grid_gbm_final <- expand.grid(
  n.trees = 100,
  interaction.depth = 4,
  shrinkage = 0.01,
  n.minobsinnode = 5
)

tune_grid_ranger_final <- expand.grid(
  mtry = 6,
  splitrule = "gini",
  min.node.size = 4
)

multinom_fit <- nnet::multinom(sii ~., data = train_dat_transformed)

xgb.fit_v4 <- caret::train(sii ~., data = train_dat_transformed, method = "xgbTree", trControl = final_train_control, tuneGrid = tune_grid_xgb_final)

gbm.fit.v4 <- caret::train(sii ~., data = train_dat_transformed, method = "gbm", trControl = final_train_control, tuneGrid = tune_grid_gbm_final, verbose = FALSE)

ranger.fit.v4 <- caret::train(sii ~., data = train_dat_transformed, method = "ranger", trControl = final_train_control, tuneGrid = tune_grid_ranger_final)


train_dat_transformed$multino_pred <- predict(multinom_fit, newdata = train_dat_transformed)
train_dat_transformed$xgb_pred <- predict(xgb.fit_v4, newdata = train_dat_transformed)
train_dat_transformed$gbm_pred <- predict(gbm.fit.v4, newdata = train_dat_transformed)
train_dat_transformed$ranger_pred <- predict(ranger.fit.v4, newdata = train_dat_transformed)



#### Ensamble
train_control <- caret::trainControl(method = "repeatedcv",
                                     number = 10, # Change back to 10 and set tuneLength to a higher number when running the initial pre-tuned models
                                     repeats = 5)

cl <- parallel::makePSOCKcluster(10)
doParallel::registerDoParallel(cl)

multinom_fit.vfinal <- nnet::multinom(sii ~., data = train_dat_transformed)

tictoc::tic()
xgb.fit.vfinal <- caret::train(sii ~., data = train_dat_transformed, method = "xgbTree", trControl = train_control, tuneLength = 5)
tictoc::toc() ### 821.95 sec elapsed

tictoc::tic()
gbm.fit.vfinal <- caret::train(sii ~., data = train_dat_transformed, method = "gbm", trControl = train_control, tuneLength = 5)
tictoc::toc() ### 70.75 sec elapsed

tictoc::tic()
ranger.fit.vfinal <- caret::train(sii ~., data = train_dat_transformed, method = "ranger", trControl = train_control, tuneLength = 5)
tictoc::toc() ### 22.48 sec elapsed

parallel::stopCluster(cl)

# xgb.fit.vfinal$bestTune
# #nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# #    50         1 0.3     0              0.6                1       0.5
# 
# gbm.fit.vfinal$bestTune
# #n.trees interaction.depth shrinkage n.minobsinnode
# #    50                 1       0.1             10
# 
# ranger.fit.vfinal$bestTune
# #mtry splitrule min.node.size
# # 13      gini             1






gbm.fit.vfinal$bestTune
plot(gbm.fit.vfinal)


#train_dat_transformed$multino_final <- predict(multinom_fit.vfinal, newdata = train_dat_transformed)
#train_dat_transformed$xgb_final <- predict(xgb.fit.vfinal, newdata = train_dat_transformed)
#train_dat_transformed$gbm_final <- predict(gbm.fit.vfinal, newdata = train_dat_transformed)
#train_dat_transformed$ranger_final <- predict(ranger.fit.vfinal, newdata = train_dat_transformed)

#print(caret::confusionMatrix(train_dat_transformed$multino_final, train_dat_transformed$sii))
#print(caret::confusionMatrix(train_dat_transformed$xgb_final, train_dat_transformed$sii))
#print(caret::confusionMatrix(train_dat_transformed$gbm_final, train_dat_transformed$sii))
#print(caret::confusionMatrix(train_dat_transformed$ranger_final, train_dat_transformed$sii))








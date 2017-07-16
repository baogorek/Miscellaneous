#################################################
# Cross Validation to estimate ensemble weights
#################################################

library(glmnet)
library(sampling)
library(pROC)
library(xgboost)
library(nnet)
library(randomForest)

source('fns.R')

explore_df <- read.csv('data/explore_6.csv')

# Ad-hoc correction after the contest
names(explore_df)[2] <- "segment"

explore_df$is_subj1 <- as.numeric(explore_df$user == 1)
explore_df$is_subj2 <- as.numeric(explore_df$user == 2)

data_sets <- create_train_val_split(explore_df, n = 1000)
train_df <- data_sets[["train"]]
val_df <- data_sets[["val"]]

## Train on subset
cv_fit_glmnet_1 <- train_glmnet(train_df, alpha = 1)
cv_fit_glmnet_p5 <- train_glmnet(train_df, alpha = .5)
cv_fit_glmnet_0 <- train_glmnet(train_df, alpha = 0)

cv_fit_unit_glmnet_1 <- train_unit_glmnet(train_df, alpha = 1)
cv_fit_unit_glmnet_p5 <- train_unit_glmnet(train_df, alpha = .5)
cv_fit_unit_glmnet_0 <- train_unit_glmnet(train_df, alpha = 0)

cv_xgboost <- train_xgboost(train_df, nrounds = 500)
cv_nnets <- train_nnets(train_df, m = 100, size = 20)
cv_rf <- train_rf(train_df)

# Validate on subset
cv_pred_glmnet_1 <- predict_glmnet(val_df, cv_fit_glmnet_1)
cv_pred_glmnet_p5 <- predict_glmnet(val_df, cv_fit_glmnet_p5)
cv_pred_glmnet_0 <- predict_glmnet(val_df, cv_fit_glmnet_0)

cv_pred_unit_glmnet_1 <- predict_unit_glmnet(val_df, cv_fit_unit_glmnet_1)
cv_pred_unit_glmnet_p5 <- predict_unit_glmnet(val_df, cv_fit_unit_glmnet_p5)
cv_pred_unit_glmnet_0 <- predict_unit_glmnet(val_df, cv_fit_unit_glmnet_0)

cv_pred_xgboost <- predict_xgboost(val_df, cv_xgboost)
cv_pred_nnets <- predict_nnets(val_df, cv_nnets)
cv_pred_rf <- predict_rf(val_df, cv_rf)

auc(val_df$preictal, cv_pred_glmnet_1)
auc(val_df$preictal, cv_pred_glmnet_p5)
auc(val_df$preictal, cv_pred_glmnet_0)
auc(val_df$preictal, cv_pred_unit_glmnet_1)
auc(val_df$preictal, cv_pred_unit_glmnet_p5)
auc(val_df$preictal, cv_pred_unit_glmnet_0)
auc(val_df$preictal, cv_pred_xgboost)
auc(val_df$preictal, cv_pred_nnets)
auc(val_df$preictal, cv_pred_rf)

auc(val_df$preictal, get_ensemble_pred(rep(1, 9)))

my_optim <- optim(rep(1, 9), get_ensemble_auc, gen_candidate, method = "SANN",
                  control = list(fnscale = -1, REPORT = 500, trace = 1))
my_optim$value

overall_w <- my_optim$par
overall_pred <- get_ensemble_pred(overall_w)
auc(val_df$preictal, overall_pred) 

my_optim <- optim(overall_w, get_user1_auc, gen_candidate, method = "SANN",
                  control = list(fnscale = -1, REPORT = 500, trace = 1))

user1_w <- my_optim$par

my_optim <- optim(overall_w, get_user2_auc, gen_candidate, method = "SANN",
                  control = list(fnscale = -1, REPORT = 500, trace = 1))

user2_w <- my_optim$par

my_optim <- optim(overall_w, get_user3_auc, gen_candidate, method = "SANN",
                  control = list(fnscale = -1, REPORT = 500, trace = 1))

user3_w <- my_optim$par 
user_ensemble_pred <- pred_combined_user_ensemble(val_df, user1_w, user2_w, user3_w)
auc(val_df$preictal, user_ensemble_pred)

#save(list = c("overall_w", "user1_w", "user2_w", "user3_w"),
#     file = 'data/cv_parms_6.RData')

###################################################################
#### Training on whole data set using weights from cross validation
###################################################################

# Keep only the ensemble weights, the outputs of the CV above
rm(list = setdiff(ls(), c("overall_w", "user1_w", "user2_w", "user3_w")))

library(glmnet)
library(xgboost)
library(nnet)
library(randomForest)

source('fns.R')

explore_df <- read.csv('data/explore_6.csv')
explore_df$is_subj1 <- as.numeric(explore_df$user == 1)
explore_df$is_subj2 <- as.numeric(explore_df$user == 2)

fit_glmnet_1 <- train_glmnet(explore_df, alpha = 1)
fit_glmnet_p5 <- train_glmnet(explore_df, alpha = .5)
fit_glmnet_0 <- train_glmnet(explore_df, alpha = 0)

fit_unit_glmnet_1 <- train_unit_glmnet(explore_df, alpha = 1)
fit_unit_glmnet_p5 <- train_unit_glmnet(explore_df, alpha = .5)
fit_unit_glmnet_0 <- train_unit_glmnet(explore_df, alpha = 0)

fit_xgboost <- train_xgboost(explore_df, nrounds = 500)
fit_nnets <- train_nnets(explore_df, m = 100, size = 20)
fit_rf <- train_rf(explore_df)

#########################################
##### Scoring on Test data set
#########################################

test_df <- read.csv('data/test_6.csv')

user <- as.integer(gsub("new_(\\d+)_(\\d+).mat", "\\1", test_df$File))
segment <- as.integer(gsub("new_(\\d+)_(\\d+).mat", "\\2", test_df$File))

#bringing test_df to parity with explore_df
test_df <- cbind(data.frame(user = user, segment = segment), test_df)
test_df$is_subj1 <- as.numeric(test_df$user == 1)
test_df$is_subj2 <- as.numeric(test_df$user == 2)

# Note: These next 9 variable names are hard-coded into fns.R
pred_glmnet_1 <- predict_glmnet(test_df, fit_glmnet_1)
pred_glmnet_p5 <- predict_glmnet(test_df, fit_glmnet_p5)
pred_glmnet_0 <- predict_glmnet(test_df, fit_glmnet_0)

pred_unit_glmnet_1 <- predict_unit_glmnet(test_df, fit_unit_glmnet_1)
pred_unit_glmnet_p5 <- predict_unit_glmnet(test_df, fit_unit_glmnet_p5)
pred_unit_glmnet_0 <- predict_unit_glmnet(test_df, fit_unit_glmnet_0)

pred_xgboost <- predict_xgboost(test_df, fit_xgboost)
pred_nnets <- predict_nnets(test_df, fit_nnets)
pred_rf <- predict_rf(test_df, fit_rf)

#load('data/parms/parms_6.RData')  # if you're skipping the cv portion

overall_pred <- get_ensemble_pred(overall_w)
user_adj_pred <- pred_combined_user_ensemble(test_df, user1_w, user2_w, user3_w)
plot(overall_pred ~ user_adj_pred) # shows user model departure from overall 

test_df$Class <- format(user_adj_pred, scientific = FALSE)
#test_df$Class <- format(overall_pred, scientific = FALSE)

test_df <- test_df[order(test_df$user, test_df$segment), ]

scorable_df <- read.csv('data/sample_submission.csv')
missing_entries <- get_missing_entries(scorable_df, test_df, default_class = 0)

aug_scoring_df <- rbind(test_df[, c("File", "Class", "user", "segment")],
                        missing_entries)

# Sorting: Something I forgot to do in the original submission
aug_scoring_df <- aug_scoring_df[order(aug_scoring_df$user,
                                       aug_scoring_df$segment), ]

aug_scoring_df <- aug_scoring_df[, c("File", "Class")]

write.csv(aug_scoring_df, 'test_predictions.csv', row.names = F, quote = F)

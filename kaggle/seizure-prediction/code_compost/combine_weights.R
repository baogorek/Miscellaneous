# This file combines the ensemble weightings produced by the annealing over
# multiple test / train splits. In the contest, this didn't help much and
# it is optional

estimate_weights <- function() {
  # This was a quick and dirty solution to estimate the ensemble weights
  # a number of times. It didn't help much.
  number <- round(10000 * runif(1))
  outfile_name <-
    paste0('c:/devl/kaggle/seizure-prediction/data/parms/cv_parms_',
           number, '.RData')
  print(outfile_name)
  explore_df <- read.csv('c:/devl/kaggle/seizure-prediction/data/explore_6.csv')
  explore_df$is_subj1 <- as.numeric(explore_df$user == 1)
  explore_df$is_subj2 <- as.numeric(explore_df$user == 2)

  data_sets <- create_train_val_split(explore_df, n = 1000)
  train_df <<- data_sets[["train"]]
  val_df <<- data_sets[["val"]]


  ## Train on subset

  cv_fit_glmnet_1 <<- train_glmnet(train_df, alpha = 1)
  cv_fit_glmnet_p5 <<- train_glmnet(train_df, alpha = .5)
  cv_fit_glmnet_0 <<- train_glmnet(train_df, alpha = 0)

  cv_fit_unit_glmnet_1 <<- train_unit_glmnet(train_df, alpha = 1)
  cv_fit_unit_glmnet_p5 <<- train_unit_glmnet(train_df, alpha = .5)
  cv_fit_unit_glmnet_0 <<- train_unit_glmnet(train_df, alpha = 0)

  cv_xgboost <- train_xgboost(train_df, nrounds = 500)
  cv_nnets <- train_nnets(train_df, m = 100, size = 20)
  cv_rf <- train_rf(train_df)

  # Validate on subset
  cv_pred_glmnet_1 <<- predict_glmnet(val_df, cv_fit_glmnet_1)
  cv_pred_glmnet_p5 <<- predict_glmnet(val_df, cv_fit_glmnet_p5)
  cv_pred_glmnet_0 <<- predict_glmnet(val_df, cv_fit_glmnet_0)

  cv_pred_unit_glmnet_1 <<- predict_unit_glmnet(val_df, cv_fit_unit_glmnet_1)
  cv_pred_unit_glmnet_p5 <<- predict_unit_glmnet(val_df, cv_fit_unit_glmnet_p5)
  cv_pred_unit_glmnet_0 <<- predict_unit_glmnet(val_df, cv_fit_unit_glmnet_0)

  cv_pred_xgboost <<- predict_xgboost(val_df, cv_xgboost)
  cv_pred_nnets <<- predict_nnets(val_df, cv_nnets)
  cv_pred_rf <<- predict_rf(val_df, cv_rf)


  my_optim <- optim(rep(1, 9), get_ensemble_auc, gen_candidate, method = "SANN",
                    control = list(fnscale = -1, REPORT = 500, trace = 1))
  my_optim$value

  overall_w <<- my_optim$par
  overall_pred <- get_ensemble_pred(overall_w)
  auc(val_df$preictal, overall_pred)

  my_optim <- optim(overall_w, get_user1_auc, gen_candidate, method = "SANN",
                    control = list(fnscale = -1, REPORT = 500, trace = 1))

  user1_w <<- my_optim$par

  my_optim <- optim(overall_w, get_user2_auc, gen_candidate, method = "SANN",
                    control = list(fnscale = -1, REPORT = 500, trace = 1))

  user2_w <<- my_optim$par

  my_optim <- optim(overall_w, get_user3_auc, gen_candidate, method = "SANN",
                    control = list(fnscale = -1, REPORT = 500, trace = 1))

  user3_w <<- my_optim$par
  user_ensemble_pred <- pred_combined_user_ensemble(val_df, user1_w,
                                                    user2_w, user3_w)
  auc(val_df$preictal, user_ensemble_pred)

  save(list = c("overall_w", "user1_w", "user2_w", "user3_w"),
       file = outfile_name)
}


parms_dir <- 'c:/devl/kaggle/seizure-prediction/data/parms'
files <- list.files(parms_dir)

overall_list <- list()
user1_list <- list()
user2_list <- list()
user3_list <- list()
m <- length(files)
for(i in 1:m) {
  file_i <- files[i]
  load(file.path(parms_dir, file_i))
  
  overall_list[[i]] <- overall_w
  user1_list[[i]] <- user1_w
  user2_list[[i]] <- user2_w
  user3_list[[i]] <- user3_w
  
  rm(list = c("overall_w", "user1_w", "user2_w", "user3_w"))
}


avg_overall_w <- Reduce("+", overall_list) / m
avg_user1_w <- Reduce("+", user1_list) / m
avg_user2_w <- Reduce("+", user2_list) / m
avg_user3_w <- Reduce("+", user3_list) / m

save(list = c("avg_overall_w", "avg_user1_w", "avg_user2_w", "avg_user3_w"),
       file = 'c:/devl/kaggle/seizure-prediction/data/avg_parms6.RData')


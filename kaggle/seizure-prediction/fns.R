
get_fitted_models_from_backup <- function(backup_image, trained_models_image) {
  load(backup_image) # e.g., data/backup_6.RData'
  save(list = c("cv_fit_glmnet_1", "cv_fit_glmnet_p5", "cv_fit_glmnet_0",
                "cv_fit_unit_glmnet_1", "cv_fit_unit_glmnet_p5",
                "cv_fit_unit_glmnet_0", "cv_xgboost", "cv_rf", "cv_nnets",
                "overall_w", "user1_w", "user2_w", "user3_w"),
       file = trained_models_image)
}

get_user_from_filename <- function(filename) {
  as.integer(gsub(".*_(\\d+)_(\\d+).mat", "\\1", filename))
}

get_segment_from_filename <- function(filename) {
  as.integer(gsub(".*_(\\d+)_(\\d+).mat", "\\2", filename))
} 

check_file_format <- function(filename) {
  is_correct_format <- grepl(".*_\\d+_\\d+.mat", filename)
  if (!all(is_correct_format)) {
    stop("File format regex is '.*_\\d+_\\d+.mat'")
  }
  return(0)
}

pred_combined_user_ensemble <- function(test_df, w1, w2, w3) {

  global_pred <- rep(0, nrow(test_df))
  w_list = list(w1, w2, w3)
  for (user_id in 1:3) {
    is_user <- test_df$user == user_id
    w_i <- w_list[[user_id]]
    global_pred[is_user] <- get_ensemble_pred(w_i)[is_user]
  }
  return(global_pred)
}

get_user_ensemble_pred <- function(w, global_w, user_id) {
  global_pred <- get_ensemble_pred(global_w)
  is_user <- val_df$user == user_id
  global_pred[is_user] <- get_ensemble_pred(w)[is_user]
  return(global_pred)
}

get_ensemble_pred <- function(w) {
  lin_combo <-
    w[1] * pred_glmnet_1  +
    w[2] * pred_glmnet_p5 +
    w[3] * pred_glmnet_0  +
    w[4] * pred_unit_glmnet_1 +
    w[5] * pred_unit_glmnet_p5 +
    w[6] * pred_unit_glmnet_0 +
    w[7] * pred_xgboost +
    w[8] * pred_nnets +
    w[9] * pred_rf
  return(lin_combo / sum(w))
}

get_ensemble_auc <- function(w) {
  auc(val_df$preictal, get_ensemble_pred(w))
}

get_user1_auc <- function(w) {
  # overall_w is a global
  auc(val_df$preictal, get_user_ensemble_pred(w, overall_w, 1))
}

get_user2_auc <- function(w) {
  # overall_w is a global
  auc(val_df$preictal, get_user_ensemble_pred(w, overall_w, 2))
}

get_user3_auc <- function(w) {
  # overall_w is a global
  auc(val_df$preictal, get_user_ensemble_pred(w, overall_w, 3))
}

gen_candidate <- function(w) {
  cand <- w + rnorm(9, mean = 0, sd = .1)
  cand[cand < 0] <- 0
  return(cand)
}

train_nnets <- function(train_df, m = 10, size = 20) {
  models <- list()
  for (i in 1:m) {
    sample_df <- sample_balanced(train_df, interictal_mult = 1)
    k <- ncol(sample_df) - 2 # Not using subject index
    X <- as.matrix(sample_df[, 4:k])
    X <- scale(X)
    y <- sample_df$preictal

    models[[i]] <- nnet(X, y, size = size, MaxNWts = 1000000)
  }
  return(models) 
}

predict_nnets <- function(test_df, fit, contest_test_df = NULL) {

  # Contest scores were scaled relative to data/test_6.csv 
  k <- ncol(test_df) - 2 # Not using subject index
  X <- as.matrix(test_df[, 4:k])

  if (!is.null(contest_test_df)) {
    scaled_contest_test_df <- scale(contest_test_df[, colnames(X)])
    centers <- attr(scaled_contest_test_df, "scaled:center")
    scales <- attr(scaled_contest_test_df, "scaled:scale")
 
    X <- scale(X, center = centers, scale = scales)
  } else {
    X <- scale(X)
  }
  
  pred_list <- list()
  m <- length(fit)
  for (i in 1:m) {
    pred_list[[i]] <- predict(fit[[i]], newdata = X)
  }
  pred_nnet <- Reduce("+", pred_list) / m
  return(as.numeric(pred_nnet))
}

train_glmnet <- function(train_df, alpha = 1) {
  X <- as.matrix(train_df[, 4:ncol(train_df)])
  y <- train_df$preictal
  fit <- cv.glmnet(X, y, family = "binomial",
                   type.measure = "auc", alpha = alpha)
  return(fit)
}

predict_glmnet <- function(test_df, fit) {
  X_test <- as.matrix(test_df[, 4:ncol(test_df)])
  as.numeric(predict(fit, newx = X_test, s = "lambda.min", type = "response"))
}

train_unit_glmnet <- function(train_df, alpha = 1) {

  k <- ncol(train_df) - 2 # Not using subject index
  
  X1 <- as.matrix(train_df[train_df$user == 1, 4:k])
  y1 <- train_df[train_df$user == 1, "preictal"]
  
  X2 <- as.matrix(train_df[train_df$user == 2, 4:k])
  y2 <- train_df[train_df$user == 2, "preictal"]
  
  X3 <- as.matrix(train_df[train_df$user == 3, 4:k])
  y3 <- train_df[train_df$user == 3, "preictal"]
  
  lasso_user1 <- cv.glmnet(X1, y1, family = "binomial", type.measure = "auc",
                           alpha = alpha)
  lasso_user2 <- cv.glmnet(X2, y2, family = "binomial", type.measure = "auc",
                           alpha = alpha)
  lasso_user3 <- cv.glmnet(X3, y3, family = "binomial", type.measure = "auc",
                           alpha = alpha)
  
  return(list(user1 = lasso_user1, user2 = lasso_user2, user3 = lasso_user3))
}

predict_unit_glmnet <- function(test_df, fit) {

  k <- ncol(test_df) - 2 # Not using subject index

  X_test <- as.matrix(test_df[, 4:k])
  X1_test <- X_test[test_df$user == 1, ]
  X2_test <- X_test[test_df$user == 2, ]
  X3_test <- X_test[test_df$user == 3, ]

  pred_user_glmnet <- rep(0, nrow(X_test))
  if (nrow(X1_test) > 0) {
    pred_user_glmnet[test_df$user == 1] <- as.numeric(predict(fit[["user1"]],
      newx = X1_test, s = "lambda.min", type = "response"))
  }
  if (nrow(X2_test) > 0) {
    pred_user_glmnet[test_df$user == 2] <- as.numeric(predict(fit[["user2"]],
      newx = X2_test, s = "lambda.min", type = "response"))
  }
  if (nrow(X3_test) > 0) {
    pred_user_glmnet[test_df$user == 3] <- as.numeric(predict(fit[["user3"]],
      newx = X3_test, s = "lambda.min", type = "response"))
  }

return(pred_user_glmnet)
}

sample_balanced <- function(train_df, interictal_mult = 2) {
  # interical_mult is the relative amount of non positive cases sampled
  t <- table(train_df$user, train_df$preictal)
  n_p_1 <- t[1, 2]
  n_i_1 <- round(interictal_mult * n_p_1)
  n_p_2 <- t[2, 2]
  n_i_2 <- round(interictal_mult * n_p_2)
  n_p_3 <- t[3, 2]
  n_i_3 <- round(interictal_mult * n_p_3)
  train_df <- train_df[order(train_df$user, train_df$preictal), ] 
  my_samples <- strata(train_df, stratanames = c("user", "preictal"),
                       size = c(n_i_1, n_p_1, n_i_2, n_p_2, n_i_3, n_p_3),
                       method = "srswor")
  #table(my_samples$user, my_samples$preictal)

  sampled_df <- train_df[my_samples$ID_unit, ]
  return(sampled_df)
}

create_train_val_split <- function(whole_df, n = 1000) {
  
  # match test set proportions
  n_user1 <- round(.11 * n)
  n_user2 <- round(.53 * n)
  n_user3 <- round(.36 * n)
  
  my_samples <- strata(whole_df, stratanames = c("user"),
                       size = c(n_user1, n_user2, n_user3),
                       method = "srswor")
  sampled_units <- whole_df[my_samples$ID_unit,
                            c("user", "segment", "preictal")]
  sampled_units$in_sample <- 1
  train_df <- merge(sampled_units, whole_df,
                    by = c("user", "segment", "preictal"),
                    all.y = TRUE)
  train_df <- train_df[is.na(train_df$in_sample), ]
  train_df$in_sample <- NULL
  val_df <- whole_df[my_samples$ID_unit, ]
  val_df$in_sample <- NULL
  return(list(train = train_df, val = val_df))
}

train_xgboost <- function(train_df, nrounds = 400, eta = .01,
                          rowpct = .8, colpct = .8) {

  sampled_df <- sample_balanced(train_df) 

  X <- as.matrix(sampled_df[, 4:ncol(sampled_df)])
  y <- sampled_df$preictal

  xgboost_fit <- xgboost(data = X,label = y, nrounds = nrounds,
                      objective = "binary:logistic", eta = eta,
                      subsample = rowpct, colsample_bytree = colpct)
  return(xgboost_fit)
}

predict_xgboost <- function(test_df, fit) {

  X_test <- as.matrix(test_df[, 4:ncol(test_df)])
  xgboost_pred <- predict(fit, newdata = X_test)
  return(xgboost_pred)
}


train_rf <- function(train_df) {

  sampled_df <- sample_balanced(train_df) 

  X <- as.matrix(sampled_df[, 4:ncol(sampled_df)])
  y <- factor(sampled_df$preictal, levels = c("0", "1"))

  rf_fit <- randomForest(X, y)
  return(rf_fit)
}

predict_rf <- function(test_df, fit) {

  X_test <- as.matrix(test_df[, 4:ncol(test_df)])
  rf_pred <- predict(fit, newdata = X_test, type = "prob")[, "1"]
  names(rf_pred) <- NULL
  return(rf_pred)
}


get_missing_entries <- function(scorable_df, scoring_df, default_class) {

  scorable_df$user <- get_user_from_filename(scorable_df$File)
  scorable_df$segment <- get_segment_from_filename(scorable_df$File)
  scorable_df <- scorable_df[scorable_df$user %in% unique(scoring_df$user), ]

  scorable_df[!(as.character(scorable_df$File) %in% as.character(scoring_df$File)), ]
}
  #scorable_files <- as.character(scorable_df$File)
  #
  #is_correct_format <- grepl(".*_\\d+_\\d+.mat", scorable_files)
  #if (!all(is_correct_format)) {
  #  stop("File format regex is '.*_\\d+_\\d+.mat'")
  #}
  #
  #scorable_users <- as.integer(gsub(".*_(\\d+)_(\\d+).mat", "\\1",
  #                                  scorable_files))
  #unscored_files <- scorable_files[!(scorable_files %in% scoring_df$File) &
  #                                 scorable_users %in% user_list]
  #
  #unscored_df <- data.frame(File = unscored_files,
  #                          Class = rep(default_class, length(unscored_files)))



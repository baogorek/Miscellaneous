#!/usr/bin/env Rscript 

library(glmnet) # Version 2.0-5 during training
library(xgboost)  # Version 0.4-4 during training
library(nnet) # from R version 3.3.2
library(randomForest) # Version 4.6-12 during training

source('fns.R')
load("data/trained_models.RData")

args <- commandArgs(trailingOnly = TRUE)
input_filename <- args[1]
output_filename <- args[2]

scoring_df <- read.csv(input_filename, stringsAsFactors = FALSE)
check_file_format(scoring_df$File) == 0

user <- get_user_from_filename(scoring_df$File)
segment <- get_segment_from_filename(scoring_df$File)

# Setting up scoring df to have energy values in columns 4 and beyond
scoring_df <- cbind(data.frame(user = user, segment = segment), scoring_df)
scoring_df$is_subj1 <- as.numeric(scoring_df$user == 1)
scoring_df$is_subj2 <- as.numeric(scoring_df$user == 2)

print("User segment counts")
table(user)

# Note: These pred_.* variable names are hard-coded into fns.R
pred_glmnet_1 <- predict_glmnet(scoring_df, cv_fit_glmnet_1)
pred_glmnet_p5 <- predict_glmnet(scoring_df, cv_fit_glmnet_p5)
pred_glmnet_0 <- predict_glmnet(scoring_df, cv_fit_glmnet_0)

pred_unit_glmnet_1 <- predict_unit_glmnet(scoring_df, cv_fit_unit_glmnet_1)
pred_unit_glmnet_p5 <- predict_unit_glmnet(scoring_df, cv_fit_unit_glmnet_p5)
pred_unit_glmnet_0 <- predict_unit_glmnet(scoring_df, cv_fit_unit_glmnet_0)

pred_xgboost <- predict_xgboost(scoring_df, cv_xgboost)

contest_scoring_df <- read.csv('data/test_6.csv') # b/c scale() in predict_nnets
pred_nnets <- predict_nnets(scoring_df, cv_nnets, contest_scoring_df)

pred_rf <- predict_rf(scoring_df, cv_rf)

# overall_pred <- get_ensemble_pred(overall_w)
user_adj_pred <- pred_combined_user_ensemble(scoring_df, user1_w, user2_w,
                                             user3_w)
scoring_df$Class <- format(user_adj_pred, scientific = FALSE)

scorable_df <- read.csv('input_filenames.csv')
missing_entries <- get_missing_entries(scorable_df, scoring_df,
                                       default_class = 0)

aug_scoring_df <- rbind(scoring_df[, c("File", "Class", "user", "segment")],
                        missing_entries)

aug_scoring_df <- aug_scoring_df[order(aug_scoring_df$user,
                                       aug_scoring_df$segment), ]

write.csv(aug_scoring_df[, c('File', 'Class')],
          output_filename, row.names = F, quote = F)


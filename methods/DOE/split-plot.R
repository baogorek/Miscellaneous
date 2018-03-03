# Examples from "How To Analyze A Split-Plot Experiment,"
# by Kevin J. Potcner and Scott M. Kowalski
# URL: http://www.minitab.com/uploadedFiles/Content/News/Published_Articles/analyze_split_plot_experiment.pdf

library(lme4)
library(pbkrtest)
library(dplyr)

wood_df <- data.frame(
  pretreat = c(rep(2, 8), rep(1, 8), rep(2, 4), rep(1, 4)),
  stain = c(c(2, 4, 1, 3), c(4, 1, 3, 2), c(3, 1, 2, 4), c(2, 4, 3, 1),
            c(1, 4, 2, 3), c(1, 3, 4, 2)),
  whole_plot_error = c(rep(4, 4), rep(5, 4), rep(1, 4), rep(2, 4),
                       rep(6, 4), rep(3, 4)) ,
  resistance = c(53.5, 32.5, 46.6, 35.4, 44.6, 52.2, 45.9, 48.3, 40.8, 43.0,
                 51.8, 45.5, 60.9, 55.3, 51.1, 57.4, 32.1, 30.1, 34.4, 32.2,
                 52.8, 51.7, 55.3, 59.2)

)

# Step 1. Treating the Whole plot part as its own CRD
# Can only do this because of a completely balanced situation with the
#   subplot treatments!
whole_tbl <- wood_df %>% group_by(whole_plot_error, pretreat) %>%
  summarise(res_mean = mean(resistance))

my_lm <- lm(res_mean ~ pretreat, data = whole_tbl)
anova(my_lm) # Textbook correct answer for pretreat

wrong_lm <- lm(resistance ~ pretreat, data = wood_df)
anova(wrong_lm) # Psuedo-replication: too many error dfs!


# Step 2: moving onto the Split-Plot analysis

## Using aov for balanced data

# The whole plot "error" is aliased with the interaction between whole plot
#  and the whole plot treatment. The following is absolutely necessary:
wood_df$wp_by_trt <- interaction(wood_df$whole_plot_error, wood_df$pretreat)

# Don't forget to treat factor variables as factors!
exact_aov <- aov(resistance ~ pretreat + Error(wp_by_trt) + 
                 factor(stain) + factor(pretreat)*factor(stain), data = wood_df)
summary(exact_aov) # textbook


# Step 3: Using the LRT (poor small sample properties)
# Remember to set REML to FALSE
# Testing below for the interaction of pretreat and stain
lmer_red <- lmer(resistance ~ (1 | whole_plot_error) + factor(stain) +
                 factor(pretreat), data = wood_df, REML = FALSE)

lmer_full <- lmer(resistance ~ (1 | whole_plot_error) + factor(pretreat) +
                  factor(stain) + factor(pretreat):factor(stain),
                  data = wood_df, REML = FALSE)

summary(lmer_full)
anova(lmer_full, lmer_red)

# The p-value for the test of interaction is samller than before, though
# technically not smaller than a .1 threshold. 
# To stay within a nested set of models, we could drop terms one by one
# If we want to test the whole plot treatment's main effect only, we could
# do something like below:

# Step 4: Whole Plots treatment main effect only
lmer_red_wp <- lmer(resistance ~ (1 | whole_plot_error), data = wood_df,
                    REML = FALSE)

lmer_full_wp <- lmer(resistance ~ (1 | whole_plot_error) + factor(pretreat),
                     data = wood_df, REML = FALSE)

anova(lmer_full_wp, lmer_red_wp)

# The p-value for the whole plot treatment effect is smaller (.041 vs .115)
# and it appears the the LRT may be somewhat liberal in this case
# Getting other options from the bootstrap option offered in pbkrtest

# Step 5: LRT bootstrap in pbkrtest
library(parallel)
nc <- detectCores()
clus <- makeCluster(rep("localhost", nc))

pb_wp_test <- PBmodcomp(lmer_full_wp, lmer_red_wp, cl = clus)
summary(pb_wp_test)

# The bootstrap methods tend to agree that the LRT tests above were too 
# liberal, bringing the p-values back above .10


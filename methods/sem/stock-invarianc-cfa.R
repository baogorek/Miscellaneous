# Based on: https://scholarworks.umass.edu/cgi/viewcontent.cgi?article=1319&context=pare

library(lavaan)
library(semTools)
library(dplyr)

options(width=2000)

# mental ability test scores of children
data(HolzingerSwineford1939) 
head(HolzingerSwineford1939)

mean(HolzingerSwineford1939$x1)  # It's not centered

# 3 factor CFA. Covariances between latent variables are on by default
model <- ' visual =~ x1 + x2 + x3;
textual =~ x4 + x5 + x6; speed =~ x7
+ x8 + x9 '

fit <- cfa(model, data=HolzingerSwineford1939)


# If standardized=TRUE, the standardized solution is also printed.
# Note that SEs and tests are still based on unstandardized estimates. 
summary(fit, standardized=TRUE, fit.measures=TRUE)

semTools::moreFitIndices(fit)
# I get a note warning me against using AICs for multivariate models

# This response makes it sound ok:
# https://stats.stackexchange.com/questions/229629/why-the-formula-for-aic-is-the-same-for-uni-and-multivariate-distributions-mode


# Tutorial says that model has weak fit, so testing configural invariance is out
## Chi-square ("Test Statistic") is 85, so strongly reject null that model-implied
## covariance is the same as the empirical covariance.
## But comparitive fit index looks ok

# On "theta" parameterization
# Millsap & Tein (2004) recommended parameterization = "theta" and identified an item’s residual variance in
# all but the first group (or occasion; Liu et al., 2017) by constraining its intercept to zero and one of
# its thresholds to equality.

HolzingerSwineford1939$school %>% table()  # 2 schools
config <- cfa(model,
              data=HolzingerSwineford1939,
              group="school") 

summary(config)

weak <- cfa(model,
            data=HolzingerSwineford1939,
            group="school",
            group.equal="loadings") 

strong <- cfa(model,
             data=HolzingerSwineford1939,
             group="school", group.equal =
               c("loadings", "intercepts"))

strict<- cfa(model,
             data=HolzingerSwineford1939,
             group="school", group.equal =
             c("loadings", "intercepts",
               "residuals"))

anova(config, weak, strong, strict) 

semTools::measurementInvariance(model=model,
                                data=HolzingerSwineford1939,
                                group="school") 

# This is deprecated. Use measEq.syntax instead

# Ok, I'm not sure what's going on here:


# ID.fac : method for identying common-factor variances.
#  Need meanstructure=TRUE, i.e., intercepts are fit
# "std.lv" method is default, Kloessner & Klopp (2019) for details


fit2 <- cfa(model, data=HolzingerSwineford1939, std.lv=TRUE, meanstructure=TRUE)

# measurement equivalence/invariance across any combination of multiple groups and/or repeated
syntax_configural <- measEq.syntax(
  configural.model=fit2,
  data=HolzingerSwineford1939,
  group="school"
)

# Configural model: no constraints across groups
summary(syntax_configural)  # Not a lot to see here
cat(as.character(syntax_configural))

# Recommended practice: fit one invariance model at a time

## The recommended sequence is to (1) generate and save each syntax object,
## (2) print it to the screen to verify you are fitting the model you expect
## to (and potentially learn which identification constraints should be
## released when equality constraints are imposed), and (3) fit that model
## to the data, as you would if you had written the syntax yourself.


# Trying to replicate the configural invariance model with the model string
cat(model, '\n')
syntax_configural2 <- measEq.syntax(
  configural.model=model,
  data=HolzingerSwineford1939,
  group="school",
  meanstructure=TRUE,
  parameterization="theta"
)
summary(syntax_configural2)  # Not a lot to see here
cat(as.character(syntax_configural2))  # Looks the same
methods(class="measEq.syntax")  # Not a lot I can do

# OK, I'm getting that measEq.syntax is a function for generating model syntax
mod_configural <- as.character(syntax_configural2)
mod_configural <- cfa(mod_configural, data=HolzingerSwineford1939,
                      group="school",
                      parameterization="theta")


# Still trying to understand threshold invariance

syntax_threshold <- measEq.syntax(
  configural.model=model,
  data=HolzingerSwineford1939,
  group="school",
  group.equal="thresholds",
  meanstructure=TRUE,
  parameterization="theta"
)
summary(syntax_threshold)


mod_threshold <- as.character(syntax_threshold)
mod_threshold <- cfa(mod_threshold, data=HolzingerSwineford1939,
                      group="school",
                      parameterization="theta")

anova(mod_threshold, mod_configural)  # 0 degrees of freedom




syntax_weak <- measEq.syntax(
  configural.model=model,
  data=HolzingerSwineford1939,
  group="school",
  group.equal=c("thresholds", "loadings"),  # same w and w/o "thresholds"
  meanstructure=TRUE,
  parameterization="theta"
)

summary(syntax_loadings)
mod_weak <- as.character(syntax_weak)
mod_weak <- cfa(mod_weak, data=HolzingerSwineford1939,
                  group="school",
                  parameterization="theta")

anova(mod_weak, mod_configural)
# Ok, I can replicate the Weak invariance Chi-Square difference by
# subtracting the Chi-Squares: 8.19

syntax_strong <- measEq.syntax(
  configural.model=model,
  data=HolzingerSwineford1939,
  group="school",
  group.equal=c("loadings", "intercepts"),
  meanstructure=TRUE,
  parameterization="theta"
)

summary(syntax_strong)

mod_strong <- as.character(syntax_strong)
mod_strong <- cfa(mod_strong, data=HolzingerSwineford1939,
                  group="school",
                  parameterization="theta")
anova(mod_strong, mod_weak)
# Hey I got 40.059! So strong evidence against strong invariance

# Moving on to the ordinal indicators


download.file("http://openpsychoweaks.org/_rawdata/SCS.zip", "SCS.zip")
unzip("SCS.zip")
tmp <- read.csv("SCS/data.csv")
scs <- subset(tmp, gender == "1" | gender == "2")
head(scs)

# One-factor model using ordinal indicators
scs_model <- '
scs =~ Q1 + Q2 + Q3 + Q4 + Q5 + Q6 + Q7 + Q8 + Q9 + Q10
' 

scs_model_fit<- cfa(
  scs_model,
  ordered = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6",
              "Q7", "Q8", "Q9", "Q10"),
  data=scs
)

summary(scs_model_fit, standardized = TRUE, fit.measures = TRUE) 

scs_configuration_model <- cfa(scs_model,
  data=scs,
  ordered = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6",
              "Q7", "Q8", "Q9", "Q10"),
  group="gender"
)

scs_weak_model <- cfa(scs_model,
  data=scs,
  ordered = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6",
              "Q7", "Q8", "Q9", "Q10"),
  group="gender",
  group.equal = "loadings"
)

# So apparently difftest went away
semTools:::difftest(scs_configuration_model, scs_model_weak)

# Try it with measEq.syntax
cat(scs_model, '\n')

syntax_configural_scs <- measEq.syntax(
  configural.model=scs_model,
  data=scs,
  ordered = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6",
              "Q7", "Q8", "Q9", "Q10"),
  group="gender",
  parameterization="delta"  # Not sure what I'm doing
)

summary(syntax_configural_scs)

# OK, I'm getting that measEq.syntax is a function for generating model syntax
mod_configural_scs <- as.character(syntax_configural_scs)
mod_configural <- cfa(
  mod_configural_scs,
  data=scs,
  ordered = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6",
              "Q7", "Q8", "Q9", "Q10"),
  group="gender",
  parameterization="delta")


syntax_weak_scs <- measEq.syntax(
  configural.model=scs_model,
  data=scs,
  ordered = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6",
              "Q7", "Q8", "Q9", "Q10"),
  group="gender",
  parameterization="delta",
  group.equal = "loadings"
)

summary(syntax_weak_scs)

mod_weak_scs <- as.character(syntax_weak_scs)
mod_weak <- cfa(
  mod_weak_scs,
  data=scs,
  ordered = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6",
              "Q7", "Q8", "Q9", "Q10"),
  group="gender",
  group.equal = "loadings",  # I'm lost now on whether I need to repeat this
  parameterization="delta")

# Issue: residuals are not parameters in the delta-parameterization lavaan
# uses (see section 1.2 above), the function will produce
# meaningless output

# ??
anova(mod_weak, mod_configural)

# Simulating Figure 7a of Causal Fusion and Econometrics
# https://arxiv.org/pdf/1912.09104.pdf
# Also dicussed in Udemy course Causal Data Science with Directed Acylic Graphs
library(dplyr)
library(scatterplot3d)
library(igraph)
library(causaleffect)

#devtools::install_github("tpospisi/RFCDE", subdir="r")
library(RFCDE)

# Selection diagram
dag <- igraph::graph.formula(w -+ s, w -+ x, w -+ z,
                             z -+ x,
                             x -+ y,
                             z -+ y, y -+ z, # bidirected edge,
                             simplify = FALSE)

# Attributed is needed for the bidirected edge
edges(dag) # Count the positions of the edges, look for z->y and y->z
dag <- set.edge.attribute(dag, "description", 6:7, "U")

# Declare node s as selection node
V(dag)  # look for s
dag <- set.vertex.attribute(dag, "description", 2, "S")

# Apply algorithm by Bareinboim and Tiam (2015)
ce_recover <- causaleffect::recover(y = "y", x = "x", G = dag)
ce_recover


# Structural Equations ---------------------------------------------------
get_subject_affinity <- function(N) {
  # u ~ Uniform(1, 20)
  runif(N, min=1, max=20)
}

get_time_resources <- function(N) {
  # w ~ Gamma(3, 1)
  rgamma(N, shape=3, scale=4)
}

get_enrolled <- function(time_resources) {
  # Pr(s = 1) = .1 if w <=5, .2 if w > 5, w <= 20, .3 if w > 20 
  pr_enrolled <- rep(.1, length(time_resources))  # base probability
  pr_enrolled <- ifelse(time_resources > 5, .2, pr_enrolled)
  pr_enrolled <- ifelse(time_resources > 20, .3, pr_enrolled)
  runif(N) < pr_enrolled
}
 
get_enjoyment <- function(time_resources, subject_affinity) {
  # z = 1 if w <= 1.3, z = u ^ 2 log(1 + w) if w > 1.3
  ifelse(time_resources <= 1.3, 1,
         subject_affinity ^ 2 * log(1 + time_resources))
}

get_hours <- function(enjoyment, time_resources) {
  
  E_hours_unconstrained <- -1.5 + 1.5 * enjoyment ^ (.35)
  e_star <- rnorm(length(enjoyment), 0, 3)
  hours_unconstrained <- E_hours_unconstrained + e_star
  hours_unconstrained <- ifelse(hours_unconstrained < 0, 0, hours_unconstrained)

  #hist(hours_unconstrained)
  #plot(hours_unconstrained ~ enjoyment)
  ifelse(hours_unconstrained > time_resources, time_resources,
         hours_unconstrained)
}

get_grade <- function(subject_affinity, hours) {
    40 + subject_affinity * log(1 + hours) + rnorm(N, 0, 4)
}

# Bidirectional arc: two variables share ancestors not pictured
# https://ftp.cs.ucla.edu/pub/stat_ser/r332.pdf
# Other pathways outside of hourse invested, enjoyment found in studying to grade

simulate_observational <- function(N) {
  # Simulate using structural equations
  # Exogenous
  time_resources <- get_time_resources(N)
  subject_affinity <- get_subject_affinity(N)

  # Endogenous
  enrolled <- get_enrolled(time_resources)
  enjoyment <- get_enjoyment(time_resources, subject_affinity) 
  hours <- get_hours(enjoyment, time_resources)
  grade <- get_grade(subject_affinity, hours)
 
  df <- data.frame(enrolled, time_resources, enjoyment, hours, grade)

  #df <- df %>% filter(enrolled)  # Selection mechanism
  df
} 

simulate_interventional <- function(N, hours) {
  # Simulating from the simpler interventional graph
  subject_affinity <- get_subject_affinity(N) # just a covariate
  grades <- get_grade(subject_affinity, hours)
  grades
}

# Create data
N <- 100000
grade_do_1 <- simulate_interventional(N, 1)
grade_do_5 <- simulate_interventional(N, 5)
grade_do_10 <- simulate_interventional(N, 10)
grade_do_20 <- simulate_interventional(N, 20)

mean(grade_do_1)
mean(grade_do_20)
mean(grade_do_20) - mean(grade_do_10)

# Creating the observed data set
df <- simulate_observational(N)

# TODO: create a density approximator just from df, filter, and density

# When it comes to what value to choose for w, keep in mind that the
# range of hours observed might be very different. Here, for example,
# there's just not the range of hours to successfully estimate the
# conditional distribution
df %>% filter(time_resources > .8, time_resources < 1.2)
df %>% filter(time_resources > 20, time_resources < 21) %>% pull(hours) %>% hist()

# keep selection bias off

# Est 2
do_hours <- 20
fixed_time_resources=30
left_slide = .9
right_slide = 1.1

# Idea: choose w such that z spans the range for your do_hours

# P(z|w, s=1)
df_w <- df %>% filter(time_resources > left_slide * fixed_time_resources,
                      time_resources < right_slide * fixed_time_resources)

df_wx <- df_w %>% filter(hours > do_hours * left_slide,
                         hours < do_hours * right_slide)

plot(density(df$enjoyment))
lines(density(df_wx$enjoyment), col='green')

hist(df_w$enjoyment)
f_z <- density(df_w$enjoyment, bw=30)
plot(f_z)

f_z2 <- approxfun(f_z$x, f_z$y)
integrate(f_z2, -60, 1290)

plot(df_w$hours ~ df_w$enjoyment)
plot(df$hours ~ df$enjoyment)


z_cuts <- cut(df_w$enjoyment, 10)
labs <- levels(z_cuts)
df_prz <- data.frame(
  lower = as.numeric( sub("\\((.+),.*", "\\1", labs) ),
  upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs) ),
  n_z = table(z_cuts),
  pr_between = NA
)
for (i in 1:nrow(df_prz)) {
  integral <- integrate(f_z2,  df_prz[i, "lower"], df_prz[i, "upper"])
  df_prz[i, "pr_between"] <- integral$value
}

# Ensure probs sum to 1
int_fz <- sum(df_prz$pr_between)
df_prz$pr_between <- df_prz$pr_between / int_fz

# Adjst lower and upper splits to cover 0 to infinity
df_prz[1, "lower"] <- 0
df_prz[nrow(df_prz), "upper"] <- 1E6 

# Notes for paper
# Is the do calculus a theoretical skeleton where I should wait for
# researchers to add flesh, or should I jump in?

do_hours <- 20
left_slide <- .8
right_slide <- 1.2

# TODO: try an aggregator. Grouping by x doesn't seem to work
min_n_for_density <- 2
x_grid <- seq(-5, 110, .1)
par(mfrow=c(4, 3))
results <- data.frame()
fy_x <- rep(0, length(x_grid))

for (i in 1:nrow(df_prz)) {
  df_wxz <- df_w %>% filter(hours > do_hours * left_slide,
                            hours < do_hours * right_slide,
                            enjoyment > df_prz[i, "lower"],
                            enjoyment < df_prz[i, "upper"])
  # Problem, when hours is 20, there are barely any lower enjoyment ratings
  # Lower than about 200, when in the ordinary sample, near 0 is the mode
  # I have to ensure that I see enjoyment across the whole range, even
  # When hours is 20, unless I skip 20 and use that as an example.
  # The mean is 7 for gods sake
  title <- paste0("Z in [", df_prz[i, "lower"],
                  ", ", z_upper=df_prz[i, "upper"], "], n=",
                  nrow(df_wxz))
  enough_for_density <- nrow(df_wxz) >= min_n_for_density
  if (enough_for_density) {
      d_y <- density(df_wxz$grade)
      f_y <- approxfun(x=d_y$x, y=d_y$y)
      y <- f_y(x_grid)
      y[is.na(y)] <- 0
      fy_weighted <- df_prz[i, "pr_between"] * y
      fy_x <- fy_x + fy_weighted
      plot(fy_weighted ~ x_grid, main=title)
      res <- data.frame(z_lower=df_prz[i, "lower"], z_upper=df_prz[i, "upper"],
                        x=x_grid, y=fy_weighted)
      results <- rbind(results, res)
  } else {
    plot(1 ~ 1, type="n", main=title)
    text(1, 1, "not enough\n points for\n density")
  }
}

# Something is not right
res_agg <- (
  results %>% group_by(x) %>% dplyr::summarise(fy_x=sum(fy_weighted)) %>% as.data.frame()
  )
summary(res_agg)
#f_y_x <- approxfun(x=res_agg$x, y=res_agg$y)
plot(res_agg$fy_x ~ as.numeric(res_agg$x))
par(mfrow=c(1, 1))

f_yx_fn <- approxfun(x=x_grid, y=fy_x)
int_fyx <- integrate(f_yx_fn, -5, 110)   # Why exactly 2?
int_fyx
f_yx_fn <- approxfun(x=x_grid, y=fy_x / int_fyx$value)

par(mfrow=c(1, 1))
plot(f_yx_fn(x_grid) ~ x_grid)
lines(density(simulate_interventional(N, do_hours)), col='blue')


df_wxz <- df_w %>% filter(hours > 9, hours < 11, enjoyment > 150, enjoyment < 250)
hist(df_wxz$grade)


est_interventional_distribution <- function(
  df,
  do_hours,
  fixed_time_resources=30,  # Choose so that there is variation in hours
  grade_delta=.1,
  enjoyment_delta=1,
  min_obs_per_leaf=35,
  n_trees=1000,
  flambda=10
  ) {
  # Estimate interventional distribution via expression arrived at via do-calc
  # Pr(y | do(X)) = int_z P(Y | x, z, w, S = 1) P(Z | w, S = 1) dz

  # Where z is enjoyment and W is time resources
  # Set w (time_resources) to something, anything I want.
  p_grade_conditional <- RFCDE(df[, c("hours", "enjoyment", "time_resources")],
                               df["grade"], node_size=min_obs_per_leaf, n_trees=n_trees,
                               mtry=2, flambda=flambda
  )
  p_enjoyment_cond <- RFCDE(df[, c("time_resources")], df["enjoyment"],
                            node_size=min_obs_per_leaf, n_trees=n_trees,
                            mtry=1, flambda=flambda)

  # Create a grid for which to predict conditional densities over
  grade_grid <- seq(-10, round(max(df$grade) * 1.1), grade_delta)
  enjoyment_grid <- seq(-10, round(max(df$enjoyment) * 1.1), enjoyment_delta)

  # Make sure grid_df is in the same order as inputs to RFCDE!
  grid_df <- data.frame(
      hours=do_hours, # This is the only thing that takes do_hours
      enjoyment=enjoyment_grid,
      time_resources=fixed_time_resources
  )
  p_grade_cond_hat <- predict(p_grade_conditional, newdata = as.matrix(grid_df),
                              response = "CDE",
                              z_grid = grade_grid)

  # See enjoyment going up!
  #plot(p_grade_cond_hat[11, ] ~ grade_grid)
  #lines(p_grade_cond_hat[200, ] ~ grade_grid)
  #lines(p_grade_cond_hat[800, ] ~ grade_grid)
  #lines(p_grade_cond_hat[1000, ] ~ grade_grid)
  #lines(p_grade_cond_hat[2000, ] ~ grade_grid)
  #p_enjoyment_cond <- RFCDE(df[, c("time_resources")], df["enjoyment"],
  #                          node_size=min_obs_per_leaf, n_trees=n_trees,
  #                          #flambda=10, mtry=1
  #)
  p_enjoyment_cond_hat <- predict(p_enjoyment_cond,
                                  newdata = as.matrix(fixed_time_resources),
                                  response = "CDE",
                                  z_grid = enjoyment_grid)

  #plot(p_enjoyment_cond_hat[1, ] ~ enjoyment_grid)
  #df_star <- df %>% filter(time_resources > 1.1, time_resources < 1.3)
  #plot(density(df_star$enjoyment))
  #hist(df_star$enjoyment)
  #plot(p_grade_cond_hat[150, ] ~ grade_grid)  
  # OK, p_grade_cond_hat is P(y |do(x), z_i, w, S=1), J rows for z
  # p_enjoyment_cond_hat is P(z_i | do(x), w, S=1), J columns for z
  
  V <- diag(as.numeric(p_enjoyment_cond_hat)) %*% p_grade_cond_hat
  interventional_dist <- colSums(V)

  total_area <- sum(interventional_dist * grade_delta)
  E_interventional <- sum(grade_grid * interventional_dist * grade_delta)
  first_i_to_80pct <- which.max(cumsum(interventional_dist * grade_delta) > .8)
  q80_interventional <- grade_grid[first_i_to_80pct]


  dist_df <- data.frame(interventional_dist, grade_grid, do_hours,
                        E_interventional, q80_interventional, total_area)

  plot(interventional_dist ~ grade_grid, data = dist_df)
  lines(density(simulate_interventional(N, do_hours)))
  #df_star <- df %>% filter(do_hours > 2.8, hours < 3.2, enjoyment > .9, enjoyment < 1.1)
  #lines(density(df_star$grade), col = 'orange')

  return(dist_df)
}

f_grade_do_hours_eq_10 <- est_interventional_distribution(df, 10, fixed_time_resources=20,
                                                          min_obs_per_leaf=15, flambda=1)
f_grade_do_hours_eq_20 <- est_interventional_distribution(df, 20, fixed_time_resources=20)


plot(density(simulate_interventional(N, do_hours), bw=1.5))
grade_delta <- .1
grade_grid <- seq(-10, 110, delta)

plot(f_grade_do_hours_eq_10 ~ grade_grid, col='purple')
points(density(grade_do_10), col='grey')


points(f_grade_do_hours_eq_20 ~ grade_grid, col='green')
points(density(grade_do_20), col='orange')




# DEMO area:

dfh10 <- df %>% filter(hours >8, hours < 12)
dfh20 <- df %>% filter(hours >18, hours < 22)

p_grade_conditional <- RFCDE(df[, c("hours")], df["grade"],
                             basis_system="cosine",
                             node_size=100,
                             n_trees = 100, fit_oob=FALSE)
p_grade_cond_hat <- predict(p_grade_conditional, newdata = as.matrix(c(5, 10, 20)),
                            response = "CDE",
                            z_grid = grade_grid)

plot(p_grade_cond_hat[2, ] ~ grade_grid)
#lines(p_grade_cond_hat[1, ] ~ grade_grid, col='blue')
lines(p_grade_cond_hat[3, ] ~ grade_grid, col='blue')
points(density(dfh10$grade), col='orange')
points(density(dfh20$grade), col='red')

# Showing how you get this narrow distribution
dfh10enj <- df %>% filter(hours >8, hours < 12, enjoyment > 10, enjoyment < 20)
plot(density(dfh10enj$grade), col = 'red')

## Trying out RFCDE
my_rfcde <- RFCDE(df2, grade)
delta <- .01
grade_grid <- seq(-10, 110, delta)  # It puts some mass on impossible values
cde_hat <- predict(my_rfcde, newdata = as.matrix(df2), response = "CDE",
                   z_grid = grade_grid)
# Each row is the conditional density over the z (response) grid
plot(cde_hat[1, ] ~ grade_grid)
sum(head(cde_hat[1, ], dim(cde_hat)[2] - 1) * delta) # Leave the right endpoint off




# NOTE: curving is an interesting mechanism because the ACE will always be zero!
# First I thought this was a mistake, but it could be just the kind of interesting
# counterexample
# This is a difficult test, so it will be curved (zero-sum dynamics)
#standardize_grade <- function(raw_grade) {
#  grade <- ifelse(raw_grade < 0, 0, raw_grade)
#  grade <- ifelse(grade > 100, 100, grade)
#  #if (mean(grade) < 70) {
#  #  curve_amount <- 70 - mean(grade)
#  #  grade <- ifelse(grade + curve_amount > 100, 100, grade + curve_amount)
#  #}
#  return(grade)
#}
#grade <- standardize_grade(raw_grade)

# Question: can I really deal with this situation, or did I just turn this
# Into a massive multivariate problem by introducing the curve?
# Has to be Pr(G1, G2, ..., GN | do(hours)), doesn't it?
# I could just work with raw grade for simplicity, and introduce the question
# in the discussion section.
# the transformation is a function f(G1, G2, ..., GN) that returns (C1, C2, ..., CN)
# So perhaps it's its own node with a direct arrow pointing towards it. No error.
# It's also a violation of the non-interference assumption. There's a game-theoretic
# Aspect to it.



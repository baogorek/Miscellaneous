# A fixed effects ANOVA simulation with some states experiencing interactions
# over time. The idea was that different exploratory techniques would be
# employed to detect these interactions.

#Note: not completely sure that this file is worth saving.

set.seed(54345)

# get state list
data(USArrests)

state <- row.names(USArrests)
state.effects <- .25 * rnorm(50)

time <- c("0_before", "1_after")
time.effects <- c(0, 0) # only interactions will show you this

channel <- c("EC", "Direct")
channel.effects <- c(.5, -.5)

EFT <- c("Yes", "No")
EFT.effects <- c(.6, -.6)

tenure <- c("<1yr", "1-3yrs", "4-10yrs", ">10yrs")
tenure.effects <- c(-.7, -.2, .2, .7)

cat("Number of combinations: ", 4*2*2*2*50)

st.effects <- as.list(rep(0, 50))

st.effects[[7]] = .7  # Connecticut
st.effects[[17]] = .7 # Kentucky
st.effects[[21]] = .7 # Massachusetts
st.effects[[30]] = .7 # New Jersey
st.effects[[32]] = .7 # New York
st.effects[[33]] = .7 # North Carolina
st.effects[[36]] = .7 # Oklahoma
st.effects[[38]] = .7 #  Pennsylvania
st.effects[[42]] = .7 # Tennessee
st.effects[[48]] = .7 # West Virginia
st.effects[[49]] = .7 # Wisconsin

mock.df <- data.frame()

for (i in 1:50){
  print(i)
  for(j in 1:2) {
    for(k in 1:2) { 
      for(l in 1:2) {
        for(m in 1:4) {
          for(rep in 1:10) {
  mock.df <- rbind(mock.df,
    data.frame(state = state[i], time = time[j], channel = channel[k], 
               eft = EFT[l], tenure = tenure[m], hh = rep, 
               logit = 2 + state.effects[i] + time.effects[j] +
                 channel.effects[k] + EFT.effects[l] + tenure.effects[m] + 
                 (j == 2) * st.effects[[i]]))
          }
        }
      }
    }
  }
}

u <- runif(nrow(mock.df))
mock.df$y = as.numeric(plogis(mock.df$logit) > u)
mock.df$z = mock.df$logit + rnorm(nrow(mock.df))

mock.df$u <- NULL

### Manual inspections ###
aggregate(y ~ state, FUN = mean, data = mock.df)
aggregate(y ~ eft, FUN = mean, data = mock.df)
aggregate(y ~ channel, FUN = mean, data = mock.df)
aggregate(y ~ tenure, FUN = mean, data = mock.df)
aggregate(y ~ time, FUN = mean, data = mock.df)

#interactions
aggregate(y ~ time + state, FUN = mean, data = mock.df)
aggregate(y ~ time + eft, FUN = mean, data = mock.df)
aggregate(y ~ time + tenure, FUN = mean, data = mock.df)

### I. Continuous Variable ###

## a.  What's driving the variation - Sequential ANOVA on continuous variables ###
my.lm <- lm(z ~ (state + time + channel + eft + tenure)^2, data = mock.df)
my.aov <- aov(my.lm)
summary(my.aov)

## b. What states are really different from each other?
pairwise.t <- pairwise.t.test(mock.df$z, mock.df$state, p.adj = "bonf")

### c. What states experienced a positive effect over time? ###

p.values <- c()
for(i in 1:50) {
  next.state = state[i]
  y.0 <- subset(mock.df, time == "0_before" &  state == next.state)$z
  y.1 <- subset(mock.df, time == "1_after" &  state == next.state)$z

  cat(mean(y.0), mean(y.1),length(y.0), "\n")
  my.t.test <- t.test(y.0,y.1)
  p.values <- c(p.values, my.t.test$p.value)
}

data.frame(st = state,
           original = round(p.values, 3), 
           adjusted = round(p.adjust(p.values, method = "fdr"), 3) 
)


### II. The Binary Variable

##a. Problem of regular ANOVA

my.lm <- lm(y ~ (state + time + channel + eft + tenure)^2, data = mock.df)
my.aov <- aov(my.lm)
summary(my.aov)

##b What's going on? for binary variables, a sequential analysis
my.glm <- glm(y ~ 1, data = mock.df, family = binomial)

add1(my.glm, scope ~ state + time + channel + eft + tenure, test = "LRT")

my.glm <- glm(y ~ state + time + channel + eft + tenure, data = mock.df,
              family = binomial)

add1(my.glm, scope = ~ (state + time + channel + eft + tenure) ^ 2,
     test = "LRT")

### c. What states experienced a positive effect over time? ###
p.values <- c()
for(i in 1:50){
  next.state = state[i]
  
  y.0 <- subset(mock.df, time == "0_before" &  state == next.state, c('time', 'y'))
    
  y.1 <- subset(mock.df, time == "1_after" &  state == next.state, c('time', 'y'))
  
  my.df <- rbind(y.0, y.1)
  t <- table(my.df)
  m <- as.matrix(t)
  
  my.test <- fisher.test(m)
  
  p.values <- c(p.values, my.test$p.value)
  
}

data.frame(st = state,
           original = round(p.values, 3), 
           adjusted = round(p.adjust(p.values, method = "fdr"), 3) 
)

# Start with a Monte Carlo simulation

T <- 10

dt <- .01
mu <- .1
sigma <- .25 

S0 <- 50  # Initial Stock Price
strike <- 55  # Option strike price
premium <- 0  # Option premium paid
## Simulating a stock price as geometric brownian motion


simulate_stock_price <- function(S0, mu, sigma, dt, T) {
  # Simulates a stock price using approximate geometric Brownian motion
  # Args:
  #   S0: Initial Stock Price
  #   mu: Drift term in Geometric Brownian Motion model of stock price
  #   sigma: Multiplier of Brownian motion in geometric model of stock price
  t_seq <- seq(0, T, dt)
  for (t in t_seq) {
    if (t == 0) {
      S <- c(S0)
    } else {
      S_prev <- tail(S, 1)
      dB <- rnorm(mean=0, sd=sqrt(dt), n=1)  # Change in Brownian Motion over dt
      dS <- S_prev * (mu * dt + sigma * dB)  # Change in Stock price over dt 
  
      new_price <- S_prev + dS
      S <- c(S, new_price)
    }
  }
  return(data.frame(t = t_seq, S = S))
}

## Plot an example stock trajectory
price_df <- simulate_stock_price(S0=50, mu=.01, sigma=.1, dt=.01, T=5)
plot(S ~ t, data = price_df, type = "l")


# Option analysis via simulation -----------------------------------
seller_call_option_profit <- function(T, S_T, V_0, strike, size, r) {
  # Gain from the POV of the seller of a European option (no early exercise)
  # T: time in periods to option maturity
  # S_T: Stock price at time of Maturity 
  # V_0: Option Premium at time 0
  # strike: strike price,
  # size: contract size
  # r: risk-free rate (per period)
  profit <- V_0 * exp(T * r)
  if (S_T > strike) {
    profit <- profit - (S_T - strike) * size
  }
  return(profit)
}

periods <- 5
premium <- 310  # Toggle by hand until expected profit is zero
mc_reps <- 50000

seller_profits <- c()
for (mc_rep in 1:mc_reps) {
  if (mc_rep %% 1000 == 0) cat("Monte Carlo Rep:", mc_rep, "\n")
  price_df <- simulate_stock_price(S0=50, mu=.01, sigma=.1, dt=.01, T=periods)
  
  seller_profit <- seller_call_option_profit(T=periods,
                                             S_T=tail(price_df, 1)$S,
                                             V_0=premium,
                                             strike=55,
                                             size=100,
                                             r=.04)
  seller_profits <- c(seller_profits, seller_profit)
}

hist(seller_profits)
mean(seller_profits)
lower <- mean(seller_profits) + 1.96 * sd(seller_profits) / sqrt(mc_reps)
upper <- mean(seller_profits) - 1.96 * sd(seller_profits) / sqrt(mc_reps)
cat("95% CI of Call Option Seller's profit: [", lower, ", ", upper, "]\n", sep = "")


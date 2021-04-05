library(dplyr)
library(depmixS4)
library(data.table)
library(ggplot2)
library(moments)

# Feature Engingeering --------------------------------------------------------

coins_data <- read.csv('./data/coins_data.csv')

# calculate simple pct change and log returns
calculate_return <- function(coins){
  ndf <- nrow(coins)
  # btc
  c <- 2
  coins$btc_ret <- c(NA, (coins[2:ndf, c])/coins[1:(ndf-1), c])
  coins$btc_logret <- log(coins$btc_ret)
  
  # eth
  c <- 3
  coins$eth_ret <- c(NA, (coins[2:ndf, c])/coins[1:(ndf-1), c])
  coins$eth_logret <- log(coins$eth_ret)
  
  # xrp
  c <- 4
  coins$xrp_ret <- c(NA, (coins[2:ndf, c])/coins[1:(ndf-1), c])
  coins$xrp_logret <- log(coins$xrp_ret)
  
  # ltc
  c <- 5
  coins$ltc_ret <- c(NA, (coins[2:ndf, c])/coins[1:(ndf-1), c])
  coins$ltc_logret <- log(coins$ltc_ret)
  
  # link
  c <- 6
  coins$link_ret <- c(NA, (coins[2:ndf, c])/coins[1:(ndf-1), c])
  coins$link_logret <- log(coins$link_ret)
  
  # drop first row with NA value
  coins <- na.omit(coins) 
  return(coins)
}

# aggregate price data on 1-day 3-day and 7-days (weekly)
n <- nrow(coins_data)
# 1d
agg <- 1
coins1d <- coins_data
coins1d$group <- rep(1:(n%/%agg), each=agg)
coins1d <- calculate_return(coins1d)

# 3d
agg <- 3
coins3d <- coins_data
coins3d$group <- rep(1:(n%/%agg), each=agg)
coins3d <- coins3d %>% group_by(group) %>% slice_tail()
coins3d <- calculate_return(as.data.frame(coins3d))

# 7d
agg <- 7
coins7d <- coins_data
coins7d$group <- c(rep(1:(n%/%agg), each=agg), rep(n%/%agg+1, each=n%%agg))
coins7d <- coins3d %>% group_by(group) %>% slice_tail()
coins7d <- calculate_return(as.data.frame(coins7d))


# Descriptive Statistics ------------------------------------------------------

ggplot(coins1d, aes(x=as.Date(date))) + 
  geom_line(aes(y = btc_price)) + 
  labs(x="Date", y="BTC Log-Price") + 
  scale_y_log10()

ggplot(coins1d, aes(x=as.Date(date))) + 
  geom_line(aes(y = eth_price)) + 
  labs(x="Date", y="ETH Log-Price") + 
  scale_y_log10()

ggplot(coins1d, aes(x=as.Date(date))) + 
  geom_line(aes(y = xrp_price)) + 
  labs(x="Date", y="XRP Log-Price") + 
  scale_y_log10()

stats <- function(col){
  col <- na.omit(col)
  stat <- data.frame(mean(col), sd(col), skewness(col), kurtosis(col))
  names(stat) <- c('Mean','SD','Skewness','Kurtosis')
  return(stat)
}

des <- rbind(stats(coins1d$btc_logret),
             stats(coins1d$eth_logret),
             stats(coins1d$xrp_logret),
             stats(coins1d$ltc_logret),
             stats(coins1d$link_logret))

row.names(des) <- c('BTC','ETH','XRP','LTC','LINK')
des

# Multivariate HMM 3D aggregation----------------------------------------------
set.seed(2020)

# hmm3d3c4s: 3 Days, Top 3 Coins, 4 states model
hmm<- depmix(list(btc_logret~1, 
                   eth_logret~1, 
                   xrp_logret~1), 
              nstates=4, 
              family=list(gaussian(),
                          gaussian(),
                          gaussian()), 
              data=coins3d)

hmmfit <- fit(hmm, verbose=FALSE)
# em.control=em.control(maxit=500, tol=1e-8,random.start=TRUE)
post_probs <- posterior(hmmfit)
summary(hmmfit)
hmmfit

# plotting regime switch
par(mfrow = c(2, 1))
plot(post_probs$state,
     main='States',
     pch=16,
     cex=0.2)
matplot(post_probs[,-1], 
        type='l', 
        main='Posterior Probabilities', 
        ylab='Probability')
legend(x='topright', 
       c('State 1','State 2','State 3','State 4'),
       fill=1:4,
       bty='n')

coins3d$state <- post_probs$state


# Multivariate HMM 1D aggregation 1 coin---------------------------------------
set.seed(2020)

# hmm3d3c4s: 3 Days, Top 3 Coins, 4 states model
hmm<- depmix(list(btc_logret~1,
                  eth_logret~1), 
             nstates=2, 
             family=list(gaussian(),
                         gaussian()),
             data=coins1d)

hmmfit <- fit(hmm, verbose=FALSE)
# em.control=em.control(maxit=500, tol=1e-8,random.start=TRUE)
post_probs <- posterior(hmmfit)
summary(hmmfit)
hmmfit

# plotting regime switch
par(mfrow = c(2, 1))
plot(post_probs$state,
     main='States',
     pch=16,
     cex=0.2)
matplot(post_probs[,-1], 
        type='l', 
        main='Posterior Probabilities', 
        ylab='Probability')
legend(x='topright', 
       c('State 1','State 2','State 3','State 4'),
       fill=1:4,
       bty='n')

coins3d$state <- post_probs$state
# coins1d$state <- post_probs$state

# write.csv(coin, 'stat.csv', row.names=FALSE)


# BACKTESTING -----------------------------------------------------------------

N <- 3

# decision is a vector with length <#state> for long short position takes 1 or -1

# hmmfit@response[[<state>]][[<1=btc/2=eth/3=xrp>]]@parameters$coefficients
# hmmfit@response[[<state>]][[<1=btc/2=eth/3=xrp>]]@parameters$sd

# find portfolio weighting
btc_means <- c(
  hmmfit@response[[1]][[1]]@parameters$coefficients,
  hmmfit@response[[2]][[1]]@parameters$coefficients,
  hmmfit@response[[3]][[1]]@parameters$coefficients,
  hmmfit@response[[4]][[1]]@parameters$coefficients
)

eth_means <- c(
  hmmfit@response[[1]][[2]]@parameters$coefficients,
  hmmfit@response[[2]][[2]]@parameters$coefficients,
  hmmfit@response[[3]][[2]]@parameters$coefficients,
  hmmfit@response[[4]][[2]]@parameters$coefficients
)

xrp_means <- c(
  hmmfit@response[[1]][[3]]@parameters$coefficients,
  hmmfit@response[[2]][[3]]@parameters$coefficients,
  hmmfit@response[[3]][[3]]@parameters$coefficients,
  hmmfit@response[[4]][[3]]@parameters$coefficients
)

s1p <- c(
  hmmfit@response[[1]][[1]]@parameters$coefficients,
  hmmfit@response[[1]][[2]]@parameters$coefficients,
  hmmfit@response[[1]][[3]]@parameters$coefficients
)
s1p <- s1p / sum(s1p)

s2p <- c(
  hmmfit@response[[2]][[1]]@parameters$coefficients,
  hmmfit@response[[2]][[2]]@parameters$coefficients,
  hmmfit@response[[2]][[3]]@parameters$coefficients
)
s2p <- s2p / sum(s2p)

s3p <- c(
  hmmfit@response[[3]][[1]]@parameters$coefficients,
  hmmfit@response[[3]][[2]]@parameters$coefficients,
  hmmfit@response[[3]][[3]]@parameters$coefficients
)
s3p <- s3p / sum(s3p)

s4p <- c(
  hmmfit@response[[4]][[1]]@parameters$coefficients,
  hmmfit@response[[4]][[2]]@parameters$coefficients,
  hmmfit@response[[4]][[3]]@parameters$coefficients
)
s4p <- s4p / sum(s4p)

# btc_decision <- function(s){
#   decision <- c(s1p[1],-s1p[2],s1p[3],s1p[4])
#   result <- abs(decision[s])
#   return(result)
# }
# 
# eth_decision <- function(s){
#   decision <- c(s2p[1],-s2p[2],s2p[3],s2p[4])
#   result <- abs(decision[s])
#   return(result)
# }
# 
# xrp_decision <- function(s){
#   decision <- c(s3p[1],-s3p[2],s3p[3],s3p[4])
#   result <- abs(decision[s])
#   return(result)
# }

btc_decision <- function(s){
  decision <- c(1,-1,1,1)
  result <- (decision[s])
  return(result)
}

eth_decision <- function(s){
  decision <- c(1,-1,1,1)
  result <- (decision[s])
  return(result)
}

xrp_decision <- function(s){
  decision <- c(1,-1,1,1)
  result <- (decision[s])
  return(result)
}


portfolio_calculation <- function(coins3d){
  coins3d$equalval_ret <- (coins3d$btc_ret+coins3d$eth_ret+coins3d$xrp_ret)/N
  coins3d$equalval_logret <- log(coins3d$equalval_ret)
  coins3d$btc_decision <- btc_decision(coins3d$state)/N
  coins3d$eth_decision <- eth_decision(coins3d$state)/N
  coins3d$xrp_decision <- xrp_decision(coins3d$state)/N
  
  coins3d$btc_decision_lag1 <- shift(coins3d$btc_decision, n=1)
  coins3d$eth_decision_lag1 <- shift(coins3d$eth_decision, n=1)
  coins3d$xrp_decision_lag1 <- shift(coins3d$xrp_decision, n=1)
  
  coins3d$btc_payoff = (coins3d$btc_ret - 1) * (coins3d$btc_decision_lag1)
  coins3d$eth_payoff = (coins3d$eth_ret - 1) * (coins3d$eth_decision_lag1)
  coins3d$xrp_payoff = (coins3d$xrp_ret - 1) * (coins3d$xrp_decision_lag1)
  
  coins3d$port_ret = coins3d$btc_payoff+coins3d$eth_payoff+coins3d$xrp_payoff+1
  coins3d$port_logret = log(coins3d$port_ret)
  
  coins3d <- na.omit(coins3d)
  coins3d$date <- as.Date(coins3d$date)
  
  return(coins3d)
}

coins3d <- portfolio_calculation(coins3d)

ggplot(coins3d, aes(x=date)) + 
  geom_line(aes(y = cumsum(port_logret))) + 
  geom_line(aes(y = cumsum(equalval_logret)), linetype="dotted") + 
  labs(x="Date", y="Log-return") + 
  scale_fill_manual(labels = c('Strategy','Equal-weighted Benchmark'))


port_logret <- sum(coins3d$port_logret)
equalval_logret <- sum(coins3d$equalval_logret)

port_vol <- sd(coins3d$port_logret)# / (length(coins3d)**0.5) * (365**0.5)
equalval_vol <- sd(coins3d$equalval_logret)# / (length(coins3d)**0.5) * (365**0.5)

Rf = 0.12/100
port_sharpe <- (port_logret - Rf)/port_vol
equalval_sharpe <- (equalval_logret - Rf)/equalval_vol

port_metrics <- cbind(port_logret, port_vol, port_sharpe)
equalval_metrics <- cbind(equalval_logret, equalval_vol, equalval_sharpe)
metrics <- data.frame(rbind(port_metrics, equalval_metrics))
names(metrics) <- c('Log-return', 'Volatility', 'Sharpe')
row.names(metrics) <- c('Portfolio','Equal-weighted Benchmark')
metrics

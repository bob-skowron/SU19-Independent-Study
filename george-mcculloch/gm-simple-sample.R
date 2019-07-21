suppressWarnings(library(optparse))
suppressMessages(suppressWarnings(library(futile.logger)))
suppressMessages(suppressWarnings(library(dplyr)))

source("george_mcculloch.R")

# Step 1: Data, Y index values, X stock (asset) values, ar(1) model of stocks
create_sample_data.orig <- function(n,p,beta){
  stock.vals <- matrix(0, n, p)
  u <- runif(p, .2,1)
  phi <- runif(p,.5,.8)
  for(i in 1:n){
    for(j in 1:p){
      phi_j_power <- (rep(phi[j],1)^(1:1))
      stock.vals[i,j] <- u[j] + ifelse(i>1,sum(stock.vals[1:min(i,1),j]*phi_j_power[1:min(i,1)]),0) + rnorm(1,0,.2)
    }
  }
  
  X <- stock.vals[2:n,]
  Y <- X%*%beta+rnorm(n-1,0,1)
  summary(lm(Y~X-1))
  
  return(list(X = X,Y = Y))
}

create_sample_data <- function(n, p, rho, beta) {
  stock.vals <- matrix(0, n, p)
  u <- runif(p, 1, 150) # stock starting values
  sig <- runif(p, .08, .10) # stock stddev (1%-10% annual)
  
  rho.mat <- matrix(rho, nc=p, nr=p)
  diag(rho.mat) <- 1
  sigma.mat <- diag(sig/sqrt(252))%*%rho.mat%*%diag(sig/sqrt(252)) # cov matrix
  
  # set initial values
  stock.vals[1,] <- u
  
  # create a bunch of (possibly correlated) stock returns
  stock.returns <- MASS::mvrnorm(n-1, rep(0,p), Sigma = sigma.mat)
  stock.cumret <- apply(stock.returns, 2, cumsum) # assume log returns
  
  stock.vals[2:n,] <- matrix(rep(u,n-1),ncol = 10, byrow = TRUE) * exp(stock.cumret) # turn into prices
  
  # create return series, since that's what we'll regress on
  X <- stock.vals
  Y <- stock.vals%*%beta
  X.ret <- stock.returns
  Y.ret <- diff(stock.vals%*%beta)/(stock.vals[1:n-1,]%*%beta) # calculate the "index"
  
  return(list(X = X,Y = Y, X.ret = X.ret, Y.ret = Y.ret))
}

post_params <- list(v0 = 9.925*(10^(-6)), v1 = .0062034, nu = 25, lambda = .007^2, calcType = "fast")

sample.data.orig <- create_sample_data.orig(n = 101, p=10, c(0,0,0,0,0,0,0,0,1,1)) # only last two stocks are important
results <- run_MCMC(sample.data.orig$X, sample.data.orig$Y, n_burn = 1000, n_samples = 10000, post_param = post_params)

colMeans(results$Samples)
summary(lm(sample.data.orig$Y~sample.data.orig$X-1))
#> colMeans(samples) seems to work fairly well, last two are highest by a lot
# [1] 0.0460 0.0042 0.0106 0.6877 0.2717 0.0076 0.3385 0.5105 0.8991 0.9181

# uncorrelated stocks
sample.data.uncorr <- create_sample_data(n = 101, p=10, rho=0, c(0,0,0,0,0,0,0,0,1,1)) # only last two stocks are important
results <- run_MCMC(sample.data.uncorr$X, sample.data.uncorr$Y, n_burn = 1000, n_samples = 10000, post_param = post_params)
colMeans(results$Samples)
summary(lm(sample.data.uncorr$Y~sample.data.uncorr$X-1))

# note that multiplying by 100 or 1000 gives better results....
results <- run_MCMC(100*sample.data.uncorr$X.ret, 100*sample.data.uncorr$Y.ret, n_burn = 1000, n_samples = 10000, post_param = post_params)
colMeans(results$Samples)
summary(lm(sample.data.uncorr$Y.ret~sample.data.uncorr$X.ret-1))

# correlated stock samples
sample.data.corr <- create_sample_data(n = 101, p = 10, rho=.5, c(0,0,0,0,0,0,0,0,1,1)) # only last two stocks are important
results <- run_MCMC(sample.data.corr$X, sample.data.corr$Y, n_burn = 1000, n_samples = 10000, post_param = post_params)
colMeans(results$Samples)
summary(lm(sample.data.corr$Y~sample.data.corr$X-1))

results <- run_MCMC(10000*sample.data.corr$X.ret, 10000*sample.data.corr$Y.ret, n_burn = 1000, n_samples = 10000, post_param = post_params)
colMeans(results$Samples)
summary(lm(sample.data.corr$Y.ret~sample.data.corr$X.ret-1))

library(tidyverse)

# load data
X <- read.csv("securities.csv", header = T)
Y <- read.csv("index.csv")

n <- dim(X)[1] # number of records
p <- dim(X)[2] # number of covariates

# mle estimate
fit.mle <- lm(Y ~ X)
Beta.mle <- fit.mle$coefficients
sigma.mle <- sqrt(var(Y - X %*% Beta.mle))

# initial params
nu_0 <- 5
sigma2_0 <- sigma.mle #
beta <- matrix(0, nrow = 0, ncol = 1)
sigma2 <- 1 / rgamma(1, nu_0 / 2, nu_0 * sigma2_0 / 2) # IG(nu/2,nu*lambda/2); lambda assumed to come from LS
z <- matrix(nrow = n, ncol = 1)

beta <- Beta.mle   #initial values
beta0 <- rep(0, ncol(X))	 #prior mean of beta
Pbeta0 <- diag(1, ncol(X)) #prior precision for beta (shrinkage prior)

T <- 10000     #number of samples of Gibbs sampler
p0 <- rep(0.95, p)        # prior probability of excluding a predictor
b0 <- rep(0, p)          # prior mean for normal component if predictor included
cj <- rep(2, p)          # standard deviation for normal component

#Gibbs Sampling
Beta <- matrix(nrow = T, ncol = p)
Sigma.z <- matrix(nrow = T, ncol = 1)
Gamma <- matrix(nrow = T, ncol = p)

for (t in 1:T)
{
  eta <- X %*% beta
  
  # conditional posterior of beta
  
  
  
  # conditional posterior of sigma2
  sigma2[t] <- rgamma(1, (n + nu_0) / 2, (t(y - eta) %*% (y - eta) + nu_0 * sigma2_0) / 2)
  phi <- 1 / sigma2[t]
  
  # conditional posterior of gamma
  for (j in 1:p) {
    # conditional posterior variance of beta_j under normal prior
    
    Vj <- 1 / (t(X[, j]) %*% X[, j] * phi + 1 / cj[j] ^ 2)
    Ej <- Vj * phi * X[, j] %*% (y - X[, -j] %*% beta[-j])
    
    # conditional probability of including jth predictor
    phat <-
      1 - p0[j] / (p0[j] + (1 - p0[j]) * dnorm(0, b0[j], cj[j]) / dnorm(0, Ej, sqrt(Vj)))
    
    m <- rbinom(1, 1, phat)
    beta[j] <- m * rnorm(1, Ej, sqrt(Vj))
  }
  
  Beta[t, ] <- t(beta)
  Sigma.z[t] <- sqrt(var(X %*% (Beta[t, ]) - y))
  
}

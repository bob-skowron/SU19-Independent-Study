###########################################################################################
# Author: Nick Syring
# Contact: nasyrin@gmail.com, https://github.com/nasyring
# Title: R codes for Bayesian dirichlet/gaussian regression, posterior sampling via RJ-MCMC
###########################################################################################

# Directions for use:
#				
#		1. Install and load the packages below: MCMCpack.
#		2. Run all the codes. Output from a previous run (seed set at 54321) is at the bottom.
#


# Necessary packages 
library(MCMCpack)

set.seed(54321)


# True regression function

mean.fun <- function(X) 0.2*X[,1] + 0.4*X[,3] + 0.4*X[,5] 
  
# The design matrix is a set of fake returns
  
x.sample <- function(n, size=6){

	return(matrix(rnorm(n*size,0.08,.02),n,size))

}

  
  
# Likelihood ratio, used in MCMC sampling
  
  lr_func <- function(X,Y,betas1,betas2){
    l.old = sum(dnorm(Y,X%*%matrix(betas1,length(betas1),1),sigma, log = TRUE)) 
    l.new = sum(dnorm(Y,X%*%matrix(betas2,length(betas2),1),sigma, log = TRUE)) 
    lr = exp(l.new-l.old)
    return(lr)
  }
  
  
  # Metropolis Hastings steps for beta, given selection
  
  mh_beta <- function( dprop, rprop, X,Y,betas) {
    beta_prop <- betas
    where.zeroes <- which(betas==0)
    len.zero <- length(where.zeroes)
    if(len.zero>0){
      betas.nonzero <-betas[-where.zeroes]
    }else {
      betas.nonzero <-betas
    }
    u <- rprop(betas.nonzero)
    if(len.zero>0){
      beta_prop[-where.zeroes]<-u
    }else {
      beta_prop<-u
    }
    r <- log(lr_func(X,Y,betas,beta_prop)) 
    if(len.zero>0){
      r <-r+log(dprop(beta_prop[-where.zeroes] , u))-log(dprop(u, beta_prop[-where.zeroes]))
    }else {
      r <-r+log(dprop(beta_prop , u))-log(dprop(u, beta_prop))
    }
    
    R <- min(exp(r), 1)
    if(runif(1) <= R & min(beta_prop)>.001) {
      betas <- beta_prop
    } 
    return(betas)
  }
  
  # MH proposal distribution
  dprop <- function(x,theta){
    theta<-ifelse(theta<.01,.01,theta)
    return(ddirichlet(x, 1*theta))
  }
  
  rprop <- function(theta){
    theta<-ifelse(theta<.01,.01,theta)
    return(rdirichlet(1, 1*theta))
  }
  
  
  #Looping rx Sweeps through the RJ-MCMC sampler
  crsp.data <- read.csv("../data/1985-1991/spx-combined-data.csv", header = TRUE)
  Y <- crsp.data[,1]
  X <- as.matrix(crsp.data[,2:25])
  colnames(X) <- NULL
  incCash <- FALSE
  
  # response samples
  cashAsset <- matrix(0, nrow(X),1)
  if(incCash){
    X <- cbind(X, cashAsset)
  }
  
  
  n.MCMC <- 30000
  n.burn <- 5000
  n<-nrow(X)
  
  #size <- ncol(X)
  
  
  size<-ncol(X)
  sigma <- .01
  x.samples <- X*100 #x.sample(n,size)
  #  x.samples <- X*100
  y.sample <- Y*100 #mean.fun(x.samples) + rnorm(n,0,sigma)
  
  rx = n.MCMC + n.burn
  betas_results = matrix(0,rx,size) 
  
  # Construction of an RJ-MCMC Sweep
  
  # Initial parameter values
  
  I = rep(TRUE,size)# initial selection
  betas = rdirichlet(1,rep(30,size)) # initial beta
  bir = 0
  dea = 0
  rel = 0	
  
  
  
  for(kx in 1:(n.burn+n.MCMC)){
    
    betas<-round(betas,5)
    
    # Metropolis-within Gibbs updates to Beta
    
    betas <-  mh_beta( dprop, rprop, x.samples,y.sample,betas)
    
    
    
    # Birth, Death, and Relocate moves
    mu <- 3 #if small then penalty on inclusion
    b <- (1/3)*ifelse(sum(I)<size,1,0)*dpois(sum(I)+1,mu)/dpois(sum(I),mu)
    d <- (1/3)*ifelse(sum(I)>1,1,0)*dpois(sum(I)-1,mu)/dpois(sum(I),mu)
    r <- max(1-b-d,0)
    move <- rmultinom(1,1,c(b,d,r))
    
    # Birth
    if(move[1] == 1){
      
      #determine new covariate and reset beta
      
      
      where.zero <- which(betas==0)
      betas.new<-betas
      if(length(where.zero)>0){
        betas.nonzero <- betas[-where.zero]
        u <- runif(1, 0, min(1,sum(I)*min(betas.nonzero)))
        new.loc <- ifelse(sum(!I)==1,which(!I==TRUE),sample((1:size)[!I],1))
        jump.prob <- b*(1/(size-sum(I)))*(1/(min(1,sum(I)*min(betas.nonzero))))*lr_func(x.samples,y.sample,betas,betas.new)
        betas.new[-where.zero] <- betas[-where.zero]-u/sum(I)
      }else {
        u <- runif(1, 0, min(1,sum(I)*min(betas)))
        new.loc <- ifelse(sum(!I)==1,which(!I==TRUE),sample((1:size)[!I],1))
        jump.prob <- b*(1/(size-sum(I)))*(1/(min(1,sum(I)*min(betas))))*lr_func(x.samples,y.sample,betas,betas.new)
        betas.new <- betas-u/sum(I)
      }
      betas.new[new.loc]<-u
      
      
      accept <- runif(1)<jump.prob
      
      if(accept & min(betas>.001)){
        betas <- betas.new
        I[new.loc]<-TRUE
        bir<-bir+1
      }
      
    }
    # Death
    if(move[2] == 1){
      
      #determine location of dying covariate and reset beta
      death.loc <- ifelse(sum(I)==1,which(I==TRUE),sample((1:size)[I],1))
      betas.new <- betas
      where.zero <- which(betas==0)
      if(length(where.zero)>0){
        betas.new[-where.zero] <- betas[-where.zero]+betas[death.loc]*(1/(sum(I)-1))
      }else {
        betas.new <- betas+betas[death.loc]*(1/(sum(I)-1))
      }
      betas.new[death.loc]<-0
      jump.prob <- min(d*(1/(size-sum(I)+1))*(1/(min(1,(sum(I)-1)*min(betas.new[-death.loc])))),1)*lr_func(x.samples,y.sample,betas,betas.new)
      
      accept <- runif(1)<jump.prob
      
      if(accept & min(betas>.001)){
        betas<-betas.new
        I[death.loc]<-FALSE		
        dea<-dea+1
      }
      
    }
    
    # Relocate
    if(move[3] == 1){
      #determine locations to swap
      swap <- sample((1:size),2)
      betas.temp <- betas
      betas.temp[swap[1]]<-betas.temp[swap[2]]
      betas.temp[swap[2]]<-betas[swap[1]]
      betas.new <- betas.temp
      jump.prob <- r*(1/size)*(1/(size-1))*2*lr_func(x.samples,y.sample,betas,betas.new)
      
      accept <- runif(1)<jump.prob
      
      if(accept){
        betas <- betas.new
        I.temp <- I
        I.temp[swap[1]]<-I.temp[swap[2]]
        I.temp[swap[2]]<-I[swap[1]]
        I <- I.temp		
        rel<-rel+1
      }
      
      
      
      
    }
    
    betas_results[kx,] = betas
    
    print(c(kx, dea, bir, rel, round(betas,3)))
    #kx=kx+1
  }
  
  # Posterior results
  colMeans(betas_results[(n.burn+1):(n.burn+n.MCMC),])
  [1] 0.190360000 0.002166285 0.279306968 0.132237146 0.391993032 0.003946569  # note the average should nearly sum to 1
  
  # basic regression
  summary(lm(y.sample~x.samples-1))
  
  Call:
    lm(formula = y.sample ~ x.samples - 1)
  
  Residuals:
    Min         1Q     Median         3Q        Max 
  -0.0253110 -0.0064324  0.0000671  0.0059664  0.0220677 
  
  Coefficients:
    Estimate Std. Error t value Pr(>|t|)    
  x.samples1  0.18046    0.04349   4.149 7.32e-05 ***
    x.samples2 -0.03733    0.05587  -0.668    0.506    
  x.samples3  0.44219    0.04367  10.125  < 2e-16 ***
    x.samples4  0.02360    0.04671   0.505    0.615    
  x.samples5  0.41385    0.04491   9.216 8.56e-15 ***
    x.samples6 -0.03813    0.04868  -0.783    0.435    
  ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
  Residual standard error: 0.009886 on 94 degrees of freedom
  Multiple R-squared:  0.9851,    Adjusted R-squared:  0.9842 
  F-statistic:  1038 on 6 and 94 DF,  p-value: < 2.2e-16
  
  
  # need to think about how to compare these a bit more...
  
  
  
  
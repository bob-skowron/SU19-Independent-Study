
suppressWarnings(library(optparse))


option_list <- list(
  make_option(c("-f", "--file"), action="store_true", help="File to be parsed"),
  make_option(c("-n", "--nstocks"), action="store_true", type="integer", help="Number of stocks to include", default = -1),
  make_option(c("-b", "--nburn"), action="store_true", type="integer", help="Number of burn-in"),
  make_option(c("-s", "--nsamples"), action="store_true", type="integer", help="Number of samples"),
  make_option(c("-o", "--output"), action="store_true", help="Output file")
)
args <- parse_args(OptionParser(option_list=option_list))


######################
####
####	Rough Codes for conjugate sampler for marginal posterior 
####	of inclusion/exclusion parameter gamma, see section 4.2 eqs (24) 25)
####
####  
######################

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


# Step 2:  
log_likelihood <- function(params,X,Y){
	beta <- params[1:p]
	sigma <- params[p+1]
	gam <- params[(p+2):(p+2+p)]
	return( sum( dnorm(Y, mean = X%*%(beta*gam), sd = sigma, log = TRUE) ) ) 
}

# Step 3: priors
# (4)
log_pi_gam <- function(gam, omega=0.05){
	return(sum(gam*log(omega)+(1-gam)*log(1-omega)))
}

# (3)
log_pi_sigma <- function(sigma, nu, lambda){
	return(dgamma(1/sigma, shape=nu/2, rate = nu*lambda/2, log = TRUE))
}

# (7)
log_pi_beta <- function(beta,gam, v0, v1){
	z1<-rnorm(p,0, v0)
	z2 <-rnorm(p,0, v1)
	return(sum(log((1-gam)*z1+gam*z2)))
}

# Step 4: Marginal posterior of gam

post.gam <- function(gam, X, Y, post_param){
	n <- length(Y)
	p <- ncol(X)
	
	v0 <- post_param$v0
	v1 <- post_param$v1
	nu <- post_param$nu
	lambda <- post_param$lambda
	calcType <- post_param$calcType
	
	d0<- rep(v0, p)
	d1<- rep(v1, p)
	ind<-ifelse(gam==1,1,0)
	D <- diag(d1*gam+(1-gam)*d0) # (16)
	R <- diag(1, p)
	D.minusonehalf <- diag((1/sqrt(d1))*gam+(1-gam)*(1/sqrt(d0))) # (23)
	
	X_tilde <- rbind(X,D.minusonehalf) # (23)
	Y<-as.matrix(Y)
	Y_tilde <- as.matrix(c(Y,rep(0,p))) # (23)
	
	S2 <- t(Y_tilde)%*%Y_tilde-t(Y_tilde)%*%X_tilde%*%solve(t(X_tilde)%*%X_tilde)%*%t(X_tilde)%*%Y_tilde # (25)
	
	if(calcType == "fast"){
	  TT <- chol(t(X_tilde)%*%X_tilde)
	  W <- solve(t(TT))%*%t(X_tilde)%*%Y_tilde
	  #g.gam <- (prod(diag(TT)^2*diag(D))^(-.5))*(nu*lambda+t(Y)%*%Y-t(W)%*%W)^(-(n+nu)/2) #(31)
	  log.g.gam <- (-.5)*sum(log(diag(TT)^2*diag(D))) + (-(n+nu)/2)*log(nu*lambda+t(Y)%*%Y-t(W)%*%W) #(31) with logs
	  #log.g.gam <- g.gam
	}
	else if(calcType == "nonconj"){
	  
	}
	else if(calcType == "eig"){
	  # product of eigs = det, so attempt to use thats
	  e.val.XtilTXtil <- eigen(t(X_tilde)%*%X_tilde)$values
	  log.det.X <- sum(log(e.val.XtilTXtil))
	  e.val.DTD <- eigen(t(D)%*%D)$values
	  log.det.D <- sum(log(e.val.DTD))	
	  log.g.gam <- -0.5*log.det.X-0.5*log.det.D-((n+nu)/2)*log(nu*lambda+S2)
	}
	else { #normal
	  g.gam <- (det(t(X_tilde)%*%X_tilde)^(-0.5))*(det(t(D)%*%D)^(-0.5))*((nu*lambda+S2)^(-(n+nu)/2)) # (24)
	  log.g.gam <- log(g.gam)
	}

	return(log.g.gam+log_pi_gam(gam))
}

# Step 5: MCMC for gam
run_MCMC <- function(X, Y, n_burn, n_samples, post_param) {
  numacc <- 0
  p <- ncol(X)
  samples<-matrix(NA,n_samples,p)
  gam <-rep(0,p)
  
  for(i in 1:(n_burn+n_samples)){
    # choose an index and flip it
  	prop.index <- sample.int(p,1)
  	gam.new.ind <- 1-gam[prop.index] #ifelse(gam[prop.index]==0,1,0)
  	gam.new <- gam
  	gam.new[prop.index] <- gam.new.ind
  	#print(sum(gam.new))
  	
  	diff <- post.gam(gam.new,X,Y, post_param)-post.gam(gam,X,Y, post_param)
  	r.exp <- min(exp(diff),1)
  	coin_flip <- rbinom(1,1,r.exp)
  	
  	if(coin_flip==1){
  		gam <- gam.new
  		if(i>n_burn) numacc <- numacc + 1 # keep track of how many are accepted
  	}
  	
  	if(i>n_burn){
  		samples[i-n_burn,] <- gam
  	}
  	if(i%%1000==0) print(i)
  }
  return(list(
    Samples = samples,
    AcceptanceRatio = numacc / n_samples
  ))
}


# running out the simulation
# running out the simulation
post_params <- list(v0 = 9.925*(10^(-6)), v1 = .0062034, nu = 5, lambda = .007^2, calcType = "fast")


# sample.data.orig <- create_sample_data.orig(n = 101, p=10, c(0,0,0,0,0,0,0,0,1,1)) # only last two stocks are important
# results <- run_MCMC(sample.data.orig$X, sample.data.orig$Y, n_burn = 1000, n_samples = 10000, post_param = post_params)
# 
# colMeans(results$Samples)
# summary(lm(sample.data.orig$Y~sample.data.orig$X-1))
# #> colMeans(samples) seems to work fairly well, last two are highest by a lot
# # [1] 0.0460 0.0042 0.0106 0.6877 0.2717 0.0076 0.3385 0.5105 0.8991 0.9181
# 
# # uncorrelated stocks
# sample.data.uncorr <- create_sample_data(n = 101, p=10, rho=0, c(0,0,0,0,0,0,0,0,1,1)) # only last two stocks are important
# results <- run_MCMC(sample.data.uncorr$X, sample.data.uncorr$Y, n_burn = 1000, n_samples = 10000, post_param = post_params)
# colMeans(results$Samples)
# summary(lm(sample.data.uncorr$Y~sample.data.uncorr$X-1))
# 
# # note that multiplying by 100 or 1000 gives better results....
# results <- run_MCMC(100*sample.data.uncorr$X.ret, 100*sample.data.uncorr$Y.ret, n_burn = 1000, n_samples = 10000, post_param = post_params)
# colMeans(results$Samples)
# summary(lm(sample.data.uncorr$Y.ret~sample.data.uncorr$X.ret-1))
# 
# # correlated stock samples
# sample.data.corr <- create_sample_data(n = 101, p = 10, rho=.5, c(0,0,0,0,0,0,0,0,1,1)) # only last two stocks are important
# results <- run_MCMC(sample.data.corr$X, sample.data.corr$Y, n_burn = 1000, n_samples = 10000, post_param = post_params)
# colMeans(results$Samples)
# summary(lm(sample.data.corr$Y~sample.data.corr$X-1))
# 
# results <- run_MCMC(10000*sample.data.corr$X.ret, 10000*sample.data.corr$Y.ret, n_burn = 1000, n_samples = 10000, post_param = post_params)
# colMeans(results$Samples)
# summary(lm(sample.data.corr$Y.ret~sample.data.corr$X.ret-1))


# load the sample data from CRSP. Should be close to part 6
crsp.data <- read.csv(args$file, header = TRUE)
Y <- crsp.data[,1]

if(args$nstocks == -1){
  samplesize <- ncol(crsp.data) - 1
}else{
  samplesize <- args$nstocks
}
inc.stocks <- sample(seq(2,ncol(crsp.data)), size = samplesize, replace = FALSE)
X <- as.matrix(crsp.data[,inc.stocks])
secs <- colnames(crsp.data)
colnames(X) <- NULL

# run the MCMC
ptm <- proc.time()
crsp.results <- run_MCMC(X*100, Y*100, n_burn = args$nburn, n_samples = args$nsample, post_param = post_params)
elapsed <- (proc.time() - ptm)[3]

colMeans(crsp.results$Samples)
plot(sort(colMeans(crsp.results$Samples), decreasing =TRUE))

sortidx <- sort(colMeans(crsp.results$Samples), decreasing =TRUE, index.return=TRUE)$ix
secs[sortidx+1]
if(!is.na(args$output)){
  save(crsp.results, file = paste0(args$ouput, ".RData"))
}
# run regressions against data to get betas
# compare R^2s to "choose" how many assets to select

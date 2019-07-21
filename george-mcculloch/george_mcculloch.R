
suppressMessages(suppressWarnings(library(futile.logger)))
suppressMessages(suppressWarnings(library(dplyr)))
######################
####
####	Rough Codes for conjugate sampler for marginal posterior 
####	of inclusion/exclusion parameter gamma, see section 4.2 eqs (24) 25)
####
####  
######################

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
    AcceptanceRatio = numacc / n_samples*100
  ))
}

######################
####
####	Rough Codes for conjugate sampler for marginal posterior 
####	of inclusion/exclusion parameter gamma, see section 4.2 eqs (24) 25)
####
####  
######################


# Step 1: Data, Y index values, X stock (asset) values, ar(1) model of stocks

n <- 101
p <- 10
stock.vals <- matrix(0, n, p)
u <- runif(p, .2,1)
phi <- runif(p,.5,.8)
for(i in 1:n){
for(j in 1:p){
	phi_j_power <- (rep(phi[j],1)^(1:1))
	stock.vals[i,j] <- u[j] + ifelse(i>1,sum(stock.vals[1:min(i,1),j]*phi_j_power[1:min(i,1)]),0) + rnorm(1,0,.2)
}
}

beta <- c(0,0,0,0,0,0,0,0,1,1) # only last two stocks are important
X <- stock.vals[2:n,]
Y <- X%*%beta+rnorm(n-1,0,1)
summary(lm(Y~X-1))


# Step 2:  

log_likelihood <- function(params,X,Y){
	beta <- params[1:p]
	sigma <- params[p+1]
	gam <- params[(p+2):(p+2+p)]
	return( sum( dnorm(Y, mean = X%*%(beta*gam), sd = sigma, log = TRUE) ) ) 
}

# Step 3: priors

log_pi_gam <- function(gam, omega=0.05){
	return(sum(gam*log(omega)+(1-gam)*log(1-omega)))
}

log_pi_sigma <- function(sigma){
	return(dgamma(1/sigma, shape=5/2, rate = 5*.007*.007/2, log = TRUE))
}

log_pi_beta <- function(beta,gam){
	z1<-rnorm(p,0, 9.925*(10^(-6)))
	z2 <-rnorm(p,0, .0062034 )
	return(sum(log((1-gam)*z1+gam*z2)))
}

# Step 4: Marginal posterior of gam

post.gam <- function(gam, X,Y){
	n <- length(Y)
	p <- ncol(X)
	d0<- rep(9.925*(10^(-6)),p)
	d1<- rep(.0062034 ,p)
	ind<-ifelse(gam==1,1,0)
	D <- diag(d1*gam+(1-gam)*d0)
	D.minusonehalf <- diag((1/sqrt(d1))*gam+(1-gam)*(1/sqrt(d0)))
	X_tilde <- rbind(X,D.minusonehalf)
	Y<-as.matrix(Y)
	Y_tilde <- as.matrix(c(Y,rep(0,p)))
	S2 <- t(Y_tilde)%*%Y_tilde-t(Y_tilde)%*%X_tilde%*%solve(t(X_tilde)%*%X_tilde)%*%t(X_tilde)%*%Y_tilde
	g.gam <- (det(t(X_tilde)%*%X_tilde)^(-0.5))*(det(t(D)%*%D)^(-0.5))*((5*(.007*.007)+S2)^(-(n+5)/2))
	return(log(g.gam)+log_pi_gam(gam))
}

# Step 5: MCMC for gam

n_burn<-1000
n_samples<-10000
samples<-matrix(NA,n_samples,p)
gam <-rep(1,p)

for(i in 1:(n_burn+n_samples)){
	prop.index <- sample.int(p,1)
	gam.new.ind <- ifelse(gam[prop.index]==0,1,0)
	gam.new<-gam
	gam.new[prop.index]<-gam.new.ind
	diff <- post.gam(gam.new,X,Y)-post.gam(gam,X,Y)
	r.exp <- min(exp(diff),1)
	coin_flip <- rbinom(1,1,r.exp)
	if(coin_flip==1){
		gam <- gam.new
	}
	if(i>n_burn){
		samples[i-n_burn,] <- gam
	}
	if(i%%1000==0) print(i)
}

colMeans(samples)
#> colMeans(samples) seems to work fairly well, last two are highest by a lot
# [1] 0.0460 0.0042 0.0106 0.6877 0.2717 0.0076 0.3385 0.5105 0.8991 0.9181

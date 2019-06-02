library(mombf)

# To run from command line... $ Rscript Simulate_NonLocal_Rossell_small.r seed

args = commandArgs(TRUE)
seed = args[1]
p = '9'
set.seed(as.integer(seed))


Data = read.table(paste0('Data_',p,'p_',seed,'.csv'), header=FALSE, sep=',')
y = Data[1:30,1]
X = as.matrix(Data[1:30,-1])
y_test = Data[31:60,1]
X_test = as.matrix(Data[31:60,-1])



#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
Output = modelSelection( y, X, niter=5000, verbose=TRUE)

# MAP model.
m = as.logical(Output$postMode)

# Covariates in the MAP model.
covariates = which(m == TRUE)

# Posterior probability of the MAP model.
postProb = postProb(Output)['pp'][1,]

# Root mean squared error of the MAP model on the test set.
beta_hat = solve(t(X[,m]) %*% X[,m]) %*% t(X[,m]) %*% y
rmse = mean( (y_test - X_test[,m] %*% beta_hat)^2 )^.5

items = list( MAP_covariates=covariates, postProb=postProb, rmse=rmse)

save( items, file=paste0('nonLocalPrior_',p,'p_',seed,'.rda'))
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
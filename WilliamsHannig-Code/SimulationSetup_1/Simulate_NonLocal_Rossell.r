library(mombf)

# To run from command line... $ Rscript Simulate_NonLocal_Rossell.r seed p corr 

args = commandArgs(TRUE)
seed = args[1]
p = args[2]
corr = ifelse( as.logical(args[3])==TRUE, 'corr', '')
set.seed(as.integer(seed))


Data = read.table(paste0('generated-data/Data_',p,'p_',corr,seed,'.csv'), header=FALSE, sep=',')
y = Data[1:100,1]
X = as.matrix(Data[1:100,-1])
y_test = Data[101:200,1]
X_test = as.matrix(Data[101:200,-1])



#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
Output = modelSelection( y, X, niter=5000, verbose=TRUE)

# MAP model.
m = as.logical(Output$postMode)

# Covariates in the MAP model.
covariates = which(m == TRUE)

# Posterior probability of the true model.
postProb = 0
for(k in 1:dim(postProb(Output))[1]){
	temp = as.numeric(unlist(strsplit(as.character(postProb(Output)['modelid'][k,]),',')))
	if( identical( temp, as.double(1:8)) ) postProb = postProb(Output)['pp'][k,]
}

# Root mean squared error of the MAP model on the test set.
beta_hat = solve(t(X[,m]) %*% X[,m]) %*% t(X[,m]) %*% y
rmse = mean( (y_test - X_test[,m] %*% beta_hat)^2 )^.5

items = list( MAP_covariates=covariates, postProb=postProb, rmse=rmse)

save( items, file=paste0('output_nonlocal/nonLocalPrior_',p,'p_',corr,seed,'.rda'))
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
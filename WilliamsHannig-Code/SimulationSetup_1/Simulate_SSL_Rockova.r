library(SSL)

# To run from command line... $ Rscript Simulate_SpikeSlab_Rockova.r seed p corr 

args = commandArgs(TRUE)
seed = args[1]
p = args[2]
corr = ifelse( as.logical(args[3])==TRUE, 'corr', '')
set.seed(as.integer(seed))


Data = read.table(paste0('Data_',p,'p_',corr,seed,'.csv'), header=FALSE, sep=',')
y = Data[1:100,1]
X = as.matrix(Data[1:100,-1])
y_test = Data[101:200,1]
X_test = as.matrix(Data[101:200,-1])



#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
lambda1 = 1
Output = SSL( X, y, penalty="adaptive", lambda0=seq(lambda1,50,length=20), lambda1=lambda1, max.iter=100000)


# Covariates in the MAP model.
covariates = Output$model

# Root mean squared error of the MAP model on the test set.
beta_hat = Output$beta[,'50']
rmse = mean( (y_test - X_test %*% beta_hat)^2 )^.5

postProb = -99

items = list( MAP_covariates=covariates, postProb=postProb, rmse=rmse)

save( items, file=paste0('SSL_Rockova_',p,'p_',corr,seed,'.rda'))
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
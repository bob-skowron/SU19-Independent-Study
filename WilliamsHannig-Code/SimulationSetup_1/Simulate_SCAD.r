library(ncvreg)

# To run from command line... $ Rscript Simulate_SCAD.r seed p corr 

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
Output = cv.ncvreg(X, y, family='gaussian', penalty='SCAD')

# Root mean squared error of the MAP model on the test set.
beta_hat = Output$fit$beta[,Output$min]
names(beta_hat) = NULL
rmse = mean( (y_test - cbind( rep(1,100), X_test) %*% beta_hat)^2 )^.5

# Covariates in the MAP model.
covariates = which(abs(beta_hat[-1]) > 0)

postProb = -99

items = list( MAP_covariates=covariates, postProb=postProb, rmse=rmse)
save( items, file=paste0('output_SCAD/SCAD_',p,'p_',corr,seed,'.rda'))
#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
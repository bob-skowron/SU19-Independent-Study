
DataSets = 1000

postProbs = rep( 0, DataSets)
modelSize = rep( 0, DataSets)
rmse = rep( 0, DataSets)

for(k in 1:DataSets){
	load(paste0('output_nonlocal/nonLocalPrior_9p_',toString(k),'.rda'))
	postProbs[k] = items$postProb
	modelSize[k] = length(items$MAP_covariates)
	rmse[k] = items$rmse
}

cat('postProbs = ',mean(postProbs),'\n')
cat('modelSize = ',mean(modelSize),'\n')
cat('rmse = ',mean(rmse),'\n')

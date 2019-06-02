
args = commandArgs(TRUE)
Dir = args[1] # 'output_nonlocal/' or 'output_SSL_Rockova/' or 'output_SCAD/'
method = args[2] # 'nonLocalPrior_' or 'SSL_Rockova_' or 'SCAD_'
DataSets = 1000


rmse = matrix( 0, 5, DataSets)
trueMod_prop = matrix( 0, 5, DataSets)
postProbs = matrix( 0, 5, DataSets)
count = rep(0,5)
for(r in 1:5){
	for(k in 1:DataSets){
		if(file.exists(paste0(Dir,method,toString(seq(100,500,by=100)[r]),'p_',toString(k),'.rda'))){
			load(paste0(Dir,method,toString(seq(100,500,by=100)[r]),'p_',toString(k),'.rda'))
			postProbs[r,k] = items$postProb
			rmse[r,k] = items$rmse
			
			if(identical( items$MAP_covariates, 1:8)) trueMod_prop[r,k] = 1
				
			count[r] = count[r] + 1
		} 
	}
}
rmse = rowMeans(rmse)
trueMod_prop = rowMeans(trueMod_prop)
postProbs = rowMeans(postProbs) 

rmse_corr = matrix( 0, 5, DataSets)
trueMod_prop_corr = matrix( 0, 5, DataSets)
postProbs_corr = matrix( 0, 5, DataSets)
count_corr = rep(0,5)
for(r in 1:5){
	for(k in 1:DataSets){
		if(file.exists(paste0(Dir,method,toString(seq(100,500,by=100)[r]),'p_corr',toString(k),'.rda'))){
			load(paste0(Dir,method,toString(seq(100,500,by=100)[r]),'p_corr',toString(k),'.rda'))
			rmse_corr[r,k] = items$rmse
			postProbs_corr[r,k] = items$postProb
			
			if(identical( items$MAP_covariates, 1:8)) trueMod_prop_corr[r,k] = 1
				
			count_corr[r] = count_corr[r] + 1
		} 
	}
}
rmse_corr = rowMeans(rmse_corr) 
trueMod_prop_corr = rowMeans(trueMod_prop_corr)
postProbs_corr = rowMeans(postProbs_corr) 

print(count)
print(count_corr)

SimStats = cbind( postProbs, postProbs_corr, trueMod_prop, trueMod_prop_corr, rmse, rmse_corr)
write.table( as.matrix(SimStats), file=paste0(Dir,'SimStats_',method,'.csv'), 
             row.names=F, col.names=F, sep=',', eol='\n')
import numpy as np
import pandas as pd
import os
import sys

Dir = sys.argv[1] # 'output_GFI/' or 'output_eNet/'
method = sys.argv[2] # 'GFI_' or 'elasticNet_'
DataSets = 1000


rmse = np.zeros((5,DataSets))
trueMod_prop = np.zeros((5,DataSets))
postProbs = np.zeros((5,DataSets))
count = np.zeros(5)
for r, p in enumerate([ 100, 200, 300, 400, 500]):
	for k in range(DataSets):
		if os.path.isfile(Dir+method+str(p)+'p_'+str(k+1)+'.pickle') == True:
			items = pd.read_pickle(Dir+method+str(p)+'p_'+str(k+1)+'.pickle')
			postProbs[r,k] = items['postProb']
			rmse[r,k] = items['rmse']
			
			if items['MAP_covariates'] == [1,2,3,4,5,6,7,8]:
				trueMod_prop[r,k] = 1
		
			count[r] = count[r] +1


rmse = np.mean(rmse, axis=1).reshape((5,1))
trueMod_prop = np.mean(trueMod_prop, axis=1).reshape((5,1))
postProbs = np.mean(postProbs, axis=1).reshape((5,1))

rmse_corr = np.zeros((5,DataSets))
trueMod_prop_corr = np.zeros((5,DataSets))
postProbs_corr = np.zeros((5,DataSets))
count_corr = np.zeros(5)
for r, p in enumerate([ 100, 200, 300, 400, 500]):
	for k in range(DataSets): 
		if os.path.isfile(Dir+method+str(p)+'p_'+'corr'+str(k+1)+'.pickle') == True:
			items = pd.read_pickle(Dir+method+str(p)+'p_'+'corr'+str(k+1)+'.pickle')
			postProbs_corr[r,k] = items['postProb']
			rmse_corr[r,k] = items['rmse']
			
			if items['MAP_covariates'] == [1,2,3,4,5,6,7,8]:
				trueMod_prop_corr[r,k] = 1
			
			count_corr[r] = count_corr[r] +1


rmse_corr = np.mean(rmse_corr, axis=1).reshape((5,1))
trueMod_prop_corr = np.mean(trueMod_prop_corr, axis=1).reshape((5,1))
postProbs_corr = np.mean(postProbs_corr, axis=1).reshape((5,1))

print(count)
print(count_corr)

SimStats = np.concatenate(( postProbs, postProbs_corr, trueMod_prop, trueMod_prop_corr, rmse, rmse_corr), axis=1)
np.savetxt( Dir+'SimStats_'+method+'.csv', SimStats, delimiter=',', newline='\n')
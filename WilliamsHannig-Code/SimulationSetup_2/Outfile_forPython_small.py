import numpy as np
import pandas as pd

DataSets = 1000

postProbs_GFI = np.zeros(DataSets)
modelSize_GFI = np.zeros(DataSets)
rmse_GFI = np.zeros(DataSets)

for k in range(DataSets):

	items = pd.read_pickle('output_GFI/GFI_9p_'+str(k+1)+'.pickle')
	postProbs_GFI[k] = items['postProb']
	rmse_GFI[k] = items['rmse']
	modelSize_GFI[k] = len(items['MAP_covariates'])

print('postProbs_GFI = ',np.mean(postProbs_GFI))
print('modelSize_GFI = ',np.mean(modelSize_GFI))
print('rmse_GFI = ',np.mean(rmse_GFI))

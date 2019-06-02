import numpy as np
import pandas as pd
import scipy.linalg as spyla
from sklearn.linear_model import ElasticNetCV
import sys

# To run from command line... $ python Simulate_lasso_eNet.py seed p corr

seed = sys.argv[1]
p = sys.argv[2]
corr = ( 'corr' if sys.argv[3]=='TRUE' else '' )
np.random.seed(int(seed))

Data = np.loadtxt('Data_'+p+'p_'+corr+seed+'.csv', delimiter=',')
y = Data[:,0][0:100]
X = Data[:,1:][0:100,:]
y_test = Data[:,0][100:200]
X_test = Data[:,1:][100:200,:]


#----------------------------------------------------------------------------------------------------------------
# Elastic net with cross-validation over lambda and alpha.
#----------------------------------------------------------------------------------------------------------------
Output = ElasticNetCV(l1_ratio=[.1,.5,.7,.9,.95,.99,1], cv=10, max_iter=100000)
Output.fit( X, y)

# MAP model.
m = ( abs(Output.coef_) > 0 )

# Covariates in the MAP model.
covariates = np.where(m == True)[0]

# Root mean squared error of the MAP model on the test set.
rmse = np.mean( (y_test - X_test @ Output.coef_)**2 )**.5

postProb = -99
			   
items = pd.Series([ covariates, postProb, rmse], index=[ 'MAP_covariates', 'postProb', 'rmse'])
items.to_pickle('elasticNet_'+p+'p_'+corr+seed+'.pickle')
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------



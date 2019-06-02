from ModSelFun import *
import sys

# To run from command line... $ python Simulate_GFI_small.py seed

seed = sys.argv[1]
p = '9'
np.random.seed(int(seed))

Data = np.loadtxt('Data_'+p+'p_'+seed+'.csv', delimiter=',')
y = Data[:,0][0:30]
X = Data[:,1:][0:30,:]
y_test = Data[:,0][30:60]
X_test = Data[:,1:][30:60,:]


#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
po = admissible_subsets_cv(y,X)
Output = admissible_subsets( y, X, N=100, steps=15000, burnin=5000, po=po)

		
# Posterior probability of the MAP model.
postProb = max(Output['postProbs'])

# MAP model.
m = Output['postSample'][ np.where(Output['postProbs'] == postProb)[0][0] ].astype(bool)

# Covariates in the MAP model.
covariates = list(np.where(m == True)[0]+1)

# Root mean squared error of the MAP model on the test set.
beta_hat = np.linalg.inv( np.transpose(X[:,m]) @ X[:,m]) @ np.transpose(X[:,m]) @ y
rmse = np.mean( (y_test - X_test[:,m] @ beta_hat)**2 )**.5
			   
items = pd.Series([ covariates, postProb, rmse], index=[ 'MAP_covariates', 'postProb', 'rmse'])

items.to_pickle('GFI_'+p+'p_'+seed+'.pickle')
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
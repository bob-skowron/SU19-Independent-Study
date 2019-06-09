from ModSelFun import *
import sys

# To run from command line... $ python Simulate_GFI.py seed p corr

seed = sys.argv[1]
p = sys.argv[2]
corr = ( 'corr' if sys.argv[3]=='TRUE' else '' )
np.random.seed(int(seed))

Data = np.loadtxt('Data_'+p+'p_'+corr+seed+'.csv', delimiter=',')
y = Data[:,0][0:100]
X = Data[:,1:][0:100,:]
y_test = Data[:,0][100:200]
X_test = Data[:,1:][100:200,:]


#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
#po = admissible_subsets_cv(y,X)
po = 7
Output = admissible_subsets( y, X, N=100, steps=15000, burnin=5000, po=po)


# Posterior probability of the true model.
m_true = np.zeros(int(p),dtype=bool)*1
m_true[np.arange(8)] = 1
postProb = 0
for k in range(Output['postSample'].shape[0]):
	if np.array_equal( m_true, Output['postSample'][k,:]) == True:
		postProb = Output['postProbs'][k]
		break

# MAP model.
m = Output['postSample'][ np.where(Output['postProbs'] == max(Output['postProbs']))[0][0] ].astype(bool)

# Covariates in the MAP model.
covariates = list(np.where(m == True)[0]+1)

# Root mean squared error of the MAP model on the test set.
beta_hat = np.linalg.inv( np.transpose(X[:,m]) @ X[:,m]) @ np.transpose(X[:,m]) @ y
rmse = np.mean( (y_test - X_test[:,m] @ beta_hat)**2 )**.5
			   
items = pd.Series([ covariates, postProb, rmse], index=[ 'MAP_covariates', 'postProb', 'rmse'])
items.to_pickle('GFI_'+p+'p_'+corr+seed+'.pickle')
#-----------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------
# This script file simply generates data all at once, and stores it in a directory. 

# To run from command line... $ python GenerateData.py seed p

import numpy as np
import sys

s = sys.argv[1]
np.random.seed(int(s))

sigma = 1
p = int(sys.argv[2])
n = 200

beta = np.concatenate(( np.array([-1.5,-1,-.8,-.6,.6,.8,1,1.5]), np.zeros(p-8)))
		
for corr in ['','corr']:
	if corr == '':
		X = np.random.multivariate_normal( np.zeros(p), np.diag(np.ones(p)), size=n)

	else:
		XCov = np.ones((p,p))*0.25 + np.diag(np.ones(p))*.75
		X = np.random.multivariate_normal( np.zeros(p), XCov, size=n)

	y = X @ beta + np.random.normal(0,sigma,n) 
	y = y.reshape((n,1))
	Data = np.concatenate((y,X), axis=1) 

	np.savetxt( 'Data_'+str(p)+'p_'+corr+s+'.csv', Data, delimiter=',', newline='\n')

















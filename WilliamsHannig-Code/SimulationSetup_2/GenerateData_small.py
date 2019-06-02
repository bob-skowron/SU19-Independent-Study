# This script file simply generates data all at once, and stores it in a directory. 

# To run from command line... $ python GenerateData.py seed

import numpy as np
import sys

s = sys.argv[1]
np.random.seed(int(s))

sigma = 1
sigma_x = .1 
p = 9
n = 30*2

beta = np.array([1,1,1,1,1,1,1,1,1])
		

X1_3 = np.random.multivariate_normal( np.zeros(3), np.diag(np.ones(3)), size=n)

X4 = ( .25*X1_3[:,0]                                + np.random.normal(scale=sigma_x,size=n) ).reshape((n,1))
X5 = (                 .5*X1_3[:,1]                 + np.random.normal(scale=sigma_x,size=n) ).reshape((n,1))
X6 = (                              - .75*X1_3[:,2] + np.random.normal(scale=sigma_x,size=n) ).reshape((n,1))
X7 = (     X1_3[:,0] +                    X1_3[:,2] + np.random.normal(scale=sigma_x,size=n) ).reshape((n,1))
X8 = (                    X1_3[:,1] -     X1_3[:,2] + np.random.normal(scale=sigma_x,size=n) ).reshape((n,1))
X9 = (     X1_3[:,0] +    X1_3[:,1] +     X1_3[:,2] + np.random.normal(scale=sigma_x,size=n) ).reshape((n,1))

X = np.concatenate((X1_3,X4,X5,X6,X7,X8,X9), axis=1)

y = X @ beta + np.random.normal(0,sigma,n) 
y = y.reshape((n,1))
Data = np.concatenate((y,X), axis=1) 

np.savetxt( 'Data_'+str(p)+'p_'+s+'.csv', Data, delimiter=',', newline='\n')
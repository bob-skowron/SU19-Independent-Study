import numpy as np
import matplotlib.pyplot as pyplot


Dir = ''

SimStats_GFI = np.loadtxt( 'output_GFI/SimStats_GFI_.csv', delimiter=',')
SimStats_nonlocal = np.loadtxt( 'output_nonlocal/SimStats_nonLocalPrior_.csv', delimiter=',')
SimStats_eNet = np.loadtxt( 'output_eNet/SimStats_elasticNet_.csv', delimiter=',')
SimStats_SSL = np.loadtxt( 'output_SSL_Rockova/SimStats_SSL_Rockova_.csv', delimiter=',')
SimStats_SCAD = np.loadtxt( 'output_SCAD/SimStats_SCAD_.csv', delimiter=',')

xaxis = np.array([ 100, 200, 300, 400, 500])

ylabels = [r'$r(M_{o}|y)$'+'  or  '+r'$P(M_{o}|y)$', r'$r(M_{o}|y)$'+'  or  '+r'$P(M_{o}|y)$',
		   'Proportion of correct model selections', 'Proportion of correct model selections',
	   	   'RMSE', 'RMSE']
filename = ['postProbs.pdf', 'postProbs_corr.pdf', 
			'trueModelProp.pdf', 'trueModelProp_corr.pdf',
			'rmse.pdf', 'rmse_corr.pdf']

#------------------------------------------------------
for p in range(6):
	pyplot.plot( xaxis, SimStats_GFI[:,p], color='black',linestyle='solid',
	             linewidth=3,label=r'$\varepsilon$'+'-admissible subsets')
	pyplot.plot( xaxis, SimStats_nonlocal[:,p],color='black',linestyle=(0,(1,1)),linewidth=3,label='nonlocal prior')
	
	if p > 1:
		pyplot.plot( xaxis, SimStats_SSL[:,p], color='black', linestyle=(0,(5,1)), linewidth=3, label='SSL')
		pyplot.plot( xaxis, SimStats_eNet[:,p], color='black', marker='^', markersize='10', label='elastic net')
		pyplot.plot( xaxis, SimStats_SCAD[:,p], color='black', marker='o', markersize='10', label='SCAD')

	pyplot.yticks( np.arange(1.1, step=.1) if p < 4 else np.arange(1,1.5, step=.1), fontsize=12)
	pyplot.xticks(xaxis, fontsize=12)
	pyplot.xlabel('p', fontsize=16)
	pyplot.ylabel(ylabels[p], fontsize=16)
	pyplot.title( r'$\rho = 0$' if p==0 else ( r'$\rho = .25$' if p==1 else '' ), fontsize=20)
	if p == 2:
		pyplot.legend(loc='center left', fontsize=16)
	pyplot.savefig(Dir+filename[p])
	pyplot.clf()
#------------------------------------------------------


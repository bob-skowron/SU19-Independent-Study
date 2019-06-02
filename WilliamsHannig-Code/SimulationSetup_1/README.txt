The files in this directory produce the simulation results in Section 4.1.  

Reproducing the simulation results for the $\varepsilon$-$admissible$ and the nonlocal 
prior procedures will require use ofa computing cluster to parallelize over random 
generator seeds.

Please contact Jonathan Williams at jpwill@live.unc.edu for any help or questions.


####################

'GenerateData.py':

This file creates the synthetic data sets, and is called in each of the Simulate_* files.

####################

'Simulate_GFI.py':

For a given random generator seed, this file generates a synthetic data set, and then 
implements the $\varepsilon$-$admissible$ procedure for variable selection on the data set.  
The full simulation consisted of executing this file for seeds 1-1000.  Note that this file 
depends on the file 'ModSelFun.py', which is located in the super-directory to 
'SimulationSetup_1/'.  Please '$ mkdir output_GFI/' to store the output.

####################

'Simulate_NonLocal_Rossell.r':

For a given random generator seed, this file generates a synthetic data set, and then 
implements the nonlocal prior procedure for variable selection on the data set.  The full 
simulation consisted of executing this file for seeds 1-1000.  Please 
'$ mkdir output_nonlocal/' to store the output.

####################

'Simulate_SCAD.r':

For a given random generator seed, this file generates a synthetic data set, and then 
implements the SCAD procedure for variable selection on the data set.  The full simulation 
consisted of executing this file for seeds 1-1000.  Please '$ mkdir output_SCAD/' to store 
the output.

####################

'Simulate_SSL_Rockova.r':

For a given random generator seed, this file generates a synthetic data set, and then 
implements the spike and slab lasso procedure for variable selection on the data set.  The 
full simulation consisted of executing this file for seeds 1-1000.  Please 
'$ mkdir output_SSL_Rockova/' to store the output.

####################

'Simulate_lasso_eNet.py':

For a given random generator seed, this file generates a synthetic data set, and then 
implements the elastic net procedure for variable selection on the data set.  The full 
simulation consisted of executing this file for seeds 1-1000.  Please 
'$ mkdir output_eNet/' to store the output.

####################

'Outfile_forPython.py' and 'Outfile_forR.r':

These files aggregate the simulation results of each method and creates a text file 
containing a matrix of statistics which are plotted in the manuscript.

####################

'Outfile_forPython.py' and 'Outfile_forR.r':

These files aggregate the simulation results of each method and creates a text file 
containing a matrix of statistics which are plotted in the manuscript.

####################

'Outfile.py':

This file calls the output text files from 'Outfile_forPython.py' and 'Outfile_forR.r' to
produce the plot in the manuscript.




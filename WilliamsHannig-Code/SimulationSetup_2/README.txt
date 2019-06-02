The files in this directory produce the simulation results in Section 4.2.  

Please contact Jonathan Williams at jpwill@live.unc.edu for any help or questions.


####################

'GenerateData_small.py':

This file creates the synthetic data sets, and is called in each of the Simulate_* files.

####################

'Simulate_GFI_small.py':

For a given random generator seed, this file generates a synthetic data set, and then 
implements the $\varepsilon$-$admissible$ procedure for variable selection on the data set.  
The full simulation consisted of executing this file for seeds 1-1000.  Note that this file 
depends on the file 'ModSelFun.py', which is located in the super-directory to 
'SimulationSetup_2/'.  Please '$ mkdir output_GFI/' to store the output.

####################

'Simulate_NonLocal_Rossell_small.r':

For a given random generator seed, this file generates a synthetic data set, and then 
implements the nonlocal prior procedure for variable selection on the data set.  The full 
simulation consisted of executing this file for seeds 1-1000.  Please 
'$ mkdir output_nonlocal/' to store the output.

####################

'Outfile_forPython_small.py' and 'Outfile_forR_small.r':

These files aggregate the simulation results of each method and print the statistics used
in the table in Section 4.2.
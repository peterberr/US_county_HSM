# US_county_HSM
A housing stock model for US counties

This repository contains data and scripts used to develop a housing stock projection model for US counties, with detail on housing stocks by type (single-family, multifamily, manfactured housing), cohort, and vacancy status. 

The details and results of this model will be published open-source in 'Buildings and Cities' (awaiting doi). The accepted manuscript and supporting information can be found in the 'Publication Files' folder.

Instructions:

1. First run AHS_HSM and housing_stock scripts. These create the intermediate data inputs required for the stock model to run, and outputs of these scripts are stored in the Intermediate_results directory.

2. The run_sm_cty script prepares the data frames for the housing stock model, and then runs the housing stock model over all stock scenarios for each U.S. county. This script takes a long time to run (1-2 days if run on a single machine).
This script stores outputs in a directory named 'HSM_results' (which needs to be created). The results directory is not tracked by Git or included in pushes to the online git repository, because of the large file sizes.

3. The sm_results_vis produces visualizations of the Housing Stock Model for selected counties, and for the national aggregate level. Some summary results are saved in a tracked directory 'Summary_results',and visualizations are saved to a 'Figures' directory (not tracked).

## Git repository for ALSPAC G0 RSBB and diet analysis code (B3893)

This repository contains three Stata .do files; one for G0 mothers analysis
in pregnancy, another for G0 mothers analysis at age 4, and another for
G0 partners/fathers analysis at age 4.

Each file reads in the raw data, cleans the variables for analysis, and then
conducts three sets of analyses:
 - 1) Associations between RSBB and broad dietary PCAs (inc. multiple imputation)
 - 2) Associations between RSBB and nutrient intake (inc. multiple imputation)
 - 3) Associations between RSBB and following recommended nutrient intake
guidelines (inc. multiple imputation)

In addition to a log file, these scripts also save out most of the results used in the
tables of the paper and creates plots of the results.

Note that ALSPAC data access is through a system of managed open access. Information
about access to ALSPAC data is given on the ALSPAC website 
(http://www.bristol.ac.uk/alspac/researchers/access/). These datasets used in these
scripts are linked to ALSPAC project number B3893; if you are interested in accessing
these datasets, please quote this number during your application.

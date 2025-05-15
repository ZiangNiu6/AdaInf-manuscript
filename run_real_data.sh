# activate renv
Rscript -e 'renv::restore()'

# 1. preprocess the data
Rscript realdata-code/preprocess-data.R

# 2. Conduct the analysis for power 
Rscript realdata-code/semi-synthetic-data.R

# 3. Conduct the analysis for Type-I error control
Rscript realdata-code/calibration_IPW.R

# 4. Plot the results
Rscript realdata-code/plot.R
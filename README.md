# PHP1560: Week 12 Lab

Data:
* The sample bike usage data is found in `sample_bike.csv`

Scripts:
* The `estimation.R` script estimates arrival rates from sample bike usage data
* The `simulation.R` script simulates customer arrival times and bike trips using the estimated arrival rates
* The `optimization.R` script optimizes bike placement usiing the simulated data
* The `main.R` script run the full pipeline, from estimation, simulation, to optimization
* The `test.R` script has test the functionality of each script and main function using toy data

Results:
* The results folder contains csv files with optimization results for 50, 100, and 200 bikes based on the data from `sample_bike.csv`.
* `placement50_10.csv` contains the optimized bike placement for 50 bikes, using 10 days of simulated data. The csv files are named similarly for 100 and 200 bikes (also using 10 days of imulated data). `placement50_20.csv` contains the optimized bike placement for 50 bikes, but with 20 days of simulated data.
* The csv files starting with `results` contain additional results from optimization, with the percentage of successful trips for each of the simulated days, the average percentage across those days, and the percentage of successful trips when we simulate another day using the optimized placement. The files are named using the same convention as above.
  

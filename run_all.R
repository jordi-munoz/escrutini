# Runs the whole analysis from the project folder:
#   Rscript run_all.R
# 1. R/01_simulate.R  -> output/sim_results.rds   (simulated counts, ~15 min)
# 2. R/02_figures.R   -> output/figures/*.png, output/time_to_reliable.csv
source("R/01_simulate.R")
source("R/02_figures.R")

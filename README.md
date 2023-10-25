# Code to reproduce analyses in "Considerations for subgroup analyses in cluster-randomized trials based on aggregated individual-level predictors"

This repository contains code to reproduce the analyses in ["Considerations for subgroup analyses in cluster-randomized trials based on aggregated individual-level predictors"](https://doi.org/10.1007/s11121-023-01606-1) by Williamson, Coley, Hsu, McCracken, and Cook (2023, _Prevention Science_). All analyses were implemented in the freely available R programming language; specifically, version 4.2.1.

This README file provides an overview of the code available in the repository.

## Code directory

# Sims

The code necessary to replicate the analysis is located in this directory.
* `gen_data.R`: functions necessary to generate a dataset based on the chosen scenario
* `get_true_values.R`: compute the true regression parameters based on a chosen scenario (useful for computing bias)
* `run_simulation_once.R`: run a single replicate of the simulation based on the chosen scenario
* `run_simulation.R`: run (possibly) many replications of the simulation based on chosen arguments
* `utils.R`: generally-useful functions
* `summarize_simulation_results.R`: takes in all simulation results and creates plots

All analyses were performed on a Windows virtual machine. The `.bat` file `submit_sim.bat` runs all of the simulations, and is called from the command line.

-----

## Issues

If you encounter any bugs or have any specific questions about the analysis, please
[file an issue](https://github.com/bdwilliamson/cluster_subgroup_analyses_supplementary/issues).

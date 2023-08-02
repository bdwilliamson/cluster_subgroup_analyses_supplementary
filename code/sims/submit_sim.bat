:: run the code from CMD
:: "C:\Program Files\R\R-4.2.1\bin\Rscript.exe"
@echo off
set randomization_type=constrained

set outfile=H:\Papers\cluster_subgroup_analyses\code\sims\rout

if not exist %outfile% mkdir %outfile%

setlocal enabledelayedexpansion
:: U is vary urban, R is vary race, B is generate baseline outcomes, 
:: K is number of clusters, S is the inner scenario (1-9)
for %%U in (0 1) do (
  for %%R in (0 1) do (
    for %%B in (0 1) do ( 
      for %%S in (1 2 3 4 5 6 7 8 9) do (
        for %%K in (30 40 80) do (
          set this_outfile=!outfile!\output_k_%%K_rand_!randomization_type!_urban_%%U_race_%%R_base_%%B_scenario_%%S.out
          set this_analysis=Num clusters = %%K, urban = %%U, race = %%R, baseline outcomes = %%B, scenario = %%S
          echo !this_analysis!
          "C:\Program Files\R\R-4.2.1\bin\Rscript.exe" sims\run_simulation.R --k %%K --randomization-type !randomization_type! --vary-urban %%U --vary-race %%R --baseline-outcomes %%B --inner-scenario %%S --nreps-total 2000 --nreps-per-job 2000 1>!this_outfile! 2>&1
        )
      )
    )
  )
)

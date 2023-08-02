# Run through the simulation many times for a specified set of arguments

# load required packages
library("dplyr")
library("tibble")
library("cvcrand")
library("gee")
library("here") # works interactively with an R project located in ~/<path to folder>/cluster_subgroup_analyses/code
library("optparse")
library("parallel")

source(here::here("sims", "gen_data.R"))
source(here::here("sims", "run_simulation_once.R"))

# set up args ------------------------------------------------------------------
parser <- OptionParser()
parser <- add_option(parser, "--k", type = "integer", default = 30,
                     help = "The number of clusters")
parser <- add_option(parser, "--randomization-type", default = "constrained",
                     help = "Randomization strategy (simple or constrained)")
parser <- add_option(parser, "--vary-urban", type = "integer", default = 0,
                     help = "Should we vary urban status distribution across sites?")
parser <- add_option(parser, "--vary-race", type = "integer", default = 0,
                     help = "Should we vary race/ethnicity distribution across sites?")
parser <- add_option(parser, "--baseline-outcomes", type = "integer", default = 0,
                     help = "should we generate baseline outcomes?")
parser <- add_option(parser, "--inner-scenario", type = "integer", default = 1,
                     help = "Specifies the true regression parameters")
parser <- add_option(parser, "--nreps-total", type = "integer", default = 2000,
                     help = "The total number of replications")
parser <- add_option(parser, "--nreps-per-job", type = "integer", default = 2000,
                     help = "The number of replications per job")
args <- parse_args(parser, convert_hyphens_to_underscores = TRUE)

# set up the simulation --------------------------------------------------------
output_dir <- here::here("..", "results", "sims")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
# true regression coefficients:
# corresponds to intercept, region, urban, race, a, region:a, urban:a, race:a
beta_01 <- c(0, 0.03, 0, 0, 0.14, 0, 0, 0)
beta_02 <- c(0, 0.03, 0.05, 0, 0.14, 0, 0, 0)
beta_03 <- c(0, 0.03, 0, 0.01, 0.14, 0, 0, 0)
beta_04 <- c(0, 0.03, 0.05, 0.01, 0.14, 0, 0, 0)
beta_05 <- c(0, 0.03, 0.05, 0, 0.14, 0, 0.1, 0)
beta_06 <- c(0, 0.03, 0, 0.01, 0.14, 0, 0, 0.15)
beta_07 <- c(0, 0.03, 0.05, 0.01, 0.14, 0, 0.1, 0)
beta_08 <- c(0, 0.03, 0.05, 0.01, 0.14, 0, 0, 0.15)
beta_09 <- c(0, 0.03, 0.05, 0.01, 0.14, 0, 0.1, 0.15)
all_betas <- do.call(rbind, list(beta_01, beta_02, beta_03, beta_04,
                                 beta_05, beta_06, beta_07, beta_08,
                                 beta_09))
beta_0 <- all_betas[args$inner_scenario, ]
# obtain the list of all possible scenarios and which scenario this is based on args
all_scenarios <- expand.grid(inner_scenario = 1:9, baseline_outcomes = c(0, 1), 
                             heterogeneity_race = c(0, 1), heterogeneity_urban = c(0, 1))
this_scenario <- which(args$baseline_outcomes == all_scenarios$baseline_outcomes &
                         args$vary_urban == all_scenarios$heterogeneity_urban &
                         args$vary_race == all_scenarios$heterogeneity_race & 
                         args$inner_scenario == all_scenarios$inner_scenario)
job_id <- this_scenario
this_analysis <- all_scenarios[job_id, ]
# static args:
# number of staff members per community
n_k <- 100
# random effects
mu_b <- 0.2
sigma_b <- 0.05
# specifying the distribution of urban vs rural, race/ethnicity (and coarsening)
high_threshold <- 0.5
race_min <- 0.4
race_max <- 0.8
urban_min <- 0.4
urban_max <- 0.6

# run the simulation -----------------------------------------------------------
# set up parallelization
num_cores <- parallel::detectCores()
# set up random number stream
seed <- job_id + args$k * 1000
parallel <- is.na(as.numeric(Sys.getenv("RSTUDIO", unset = NA)))
cat("Running analysis\n")
if (parallel) {
  cl <- invisible(parallel::makePSOCKcluster(num_cores))
  invisible(parallel::clusterEvalQ(cl, library("dplyr")))
  parallel::clusterExport(cl, varlist = c("run_simulation_once", "gen_data", 
                                          "create_cluster_level_data",
                                          "beta_0", "mu_b", "sigma_b", "n_k",
                                          "this_analysis", "args", "high_threshold",
                                          paste0("race_", c("min", "max")),
                                          paste0("urban_", c("min", "max"))))
  clusterSetRNGStream(cl = cl, iseed = seed)
  start <- Sys.time()
  output_list <- parLapply(
    cl = cl,
    X = as.list(seq_len(args$nreps_per_job)),
    fun = function(i) {
      run_simulation_once(
        mc_id = i, K = args$k, nk = n_k, 
        randomization_type = args$randomization_type,
        vary_urban = as.logical(this_analysis$heterogeneity_urban),
        vary_race = as.logical(this_analysis$heterogeneity_race), 
        generate_baseline_outcomes = as.logical(this_analysis$baseline_outcomes),
        beta_0 = beta_0, mu_b = mu_b, sigma_b = sigma_b, threshold = high_threshold,
        inner_scenario = args$inner_scenario, urban_min = urban_min,
        urban_max = urban_max, race_min = race_min, race_max = race_max
      )
    }
  )
  end <- Sys.time() 
  parallel::stopCluster(cl)
} else {
  set.seed(seed)
  start <- Sys.time()
  output_list <- vector("list", length = args$nreps_per_job)
  for (i in seq_len(args$nreps_per_job)) {
    output_list[[i]] <- run_simulation_once(
      mc_id = i, K = args$k, nk = n_k, 
      randomization_type = args$randomization_type,
      vary_urban = as.logical(this_analysis$heterogeneity_urban),
      vary_race = as.logical(this_analysis$heterogeneity_race), 
      generate_baseline_outcomes = as.logical(this_analysis$baseline_outcomes),
      beta_0 = beta_0, mu_b = mu_b, sigma_b = sigma_b, threshold = high_threshold,
      inner_scenario = args$inner_scenario, urban_min = urban_min,
      urban_max = urban_max, race_min = race_min, race_max = race_max
    )
  }
  end <- Sys.time()
}
cat("Elapsed time: ", format(end - start), "\n")
output <- data.table::rbindlist(output_list)
saveRDS(output, file = paste0(output_dir, "/output_rand_",
                              args$randomization_type,
                              "_nk_100_k_", args$k, "_id_", job_id, ".rds"))
cat("Simulation complete!\n")
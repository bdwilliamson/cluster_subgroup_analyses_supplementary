# obtain true values under each scenario

library("dplyr")
library("tibble")
library("cvcrand")
library("gee")
library("here") # works interactively with an R project located in ~/<path to folder>/cluster_subgroup_analyses/code
library("optparse")
library("parallel")

source(here::here("sims", "gen_data.R"))
source(here::here("sims", "run_simulation_once.R"))

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
# obtain the list of all possible scenarios and which scenario this is based on args
all_scenarios <- expand.grid(inner_scenario = 1:9, baseline_outcomes = c(0, 1), 
                             heterogeneity_race = c(0, 1), heterogeneity_urban = c(0, 1))
all_scenarios <- all_scenarios %>% 
  mutate(correct_model = case_when(
    inner_scenario == 1 ~ "1a",
    inner_scenario == 2 ~ "1a",
    inner_scenario == 3 ~ "1c",
    inner_scenario == 4 ~ "1c",
    inner_scenario == 5 ~ "1b",
    inner_scenario == 6 ~ "1e",
    inner_scenario == 7 ~ "1d",
    inner_scenario == 8 ~ "1e",
    inner_scenario == 9 ~ "1f"
  ))
# static args:
# number of communities (large, for asymptotics)
K <- 5000
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
# all possible individual-level regression models
shared_formula_txt <- "y ~ a + region + urban"
individual_level_formulas <- paste0(
  shared_formula_txt, c("", " * a", " + race_1", " * a + race_1", 
                        " + race_1 * a", " * a + race_1 * a")
)
individual_level_model_nms <- paste0("1", c("a", "b", "c", "d", "e", "f"))

true_values_list <- vector("list", length = nrow(all_scenarios))
set.seed(4747)
seeds <- round(runif(n = nrow(all_scenarios), 1e4, 1e5))
for (i in seq_len(nrow(all_scenarios))) {
  this_scenario <- all_scenarios[i, ]
  beta_0 <- all_betas[this_scenario$inner_scenario, ]
  # generate dataset
  set.seed(seeds[i])
  long_dataset <- gen_data(
    K = K, nk = n_k, randomization_type = "constrained",
    vary_urban = this_scenario$heterogeneity_urban, 
    vary_race = this_scenario$heterogeneity_race, 
    generate_baseline_outcomes = this_scenario$baseline_outcomes,
    beta_0 = beta_0, mu_b = mu_b, sigma_b = sigma_b,
    urban_min = urban_min, urban_max = urban_max, 
    race_min = race_min, race_max = race_max
  )
  # fit correctly-specified regression model
  # this_formula <- individual_level_formulas[individual_level_model_nms == this_scenario$correct_model]
  this_formula <- individual_level_formulas[individual_level_model_nms == "1f"]
  this_model <- gee::gee(as.formula(this_formula),
                         id = community, data = subset(long_dataset, long_dataset$y0 == 0),
                         family = "gaussian", corstr = "independence", 
                         silent = TRUE)
  # save regression parameters
  true_values_list[[i]] <- data.table::data.table(
    "scenario" = this_scenario$inner_scenario, 
    "gen_base_outcomes" = this_scenario$baseline_outcomes,
    "vary_urban" = this_scenario$heterogeneity_urban, 
    "vary_race" = this_scenario$heterogeneity_race,
    "coefficient_name" = names(this_model$coefficients),
    "truth" = round(this_model$coefficients, 2)
  )
}
true_values <- tibble::as_tibble(data.table::rbindlist(true_values_list))
saveRDS(true_values, file = paste0("../results/sims/true_values.rds"))

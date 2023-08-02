# run through the simulation a single time
# @param mc_id the monte-carlo id
# @param K the number of clusters
# @param nk the number of observations within a cluster (if a single number, will be repeated across all clusters;
#         otherwise, will sample randomly from the values provided)
# @param randomization_type "simple" or "constrained"
# @param vary_urban logical; should we have an equal probability of urban/suburban vs rural, 
#         or let it vary based on region?
# @param vary_race logical; should we have an equal probability of self-reported race/ethnicity 1,
#         or let it vary based on urban/suburban vs rural status?
# @param generate_baseline_outcomes logical; should we generate baseline outcomes or not?
# @param beta_0 the vector of regression coefficients for the outcome of interest
# @param mu_b the mean parameter for outcome random effect
# @param sigma_b the standard deviation parameter for outcome random effect
# @param threshold the threshold for specifying "high" vs "low" proportion of race 1
# @param inner_scenario which beta_0 vector we're using (1-9)
# @param urban_min, urban_max minimum and maximum for urban probability
# @param race_min, race_max parameters for race probabilities
# @return 
run_simulation_once <- function(mc_id = 1, K = 40, nk = 100, 
                                randomization_type = "simple",
                                vary_urban = FALSE, vary_race = FALSE,
                                generate_baseline_outcomes = FALSE, 
                                beta_0 = c(0, 0.03, 0.05, 0.01, 0.14, 0.02, 0.02, 0.05), 
                                mu_b = 0.2, sigma_b = 0.05, threshold = 0.5,
                                inner_scenario = 1,
                                urban_min = 0.4, urban_max = 0.6,
                                race_min = 0.3, race_max = 0.7) {
  # generate data at individual level
  long_dataset <- gen_data(
    K = K, nk = nk, randomization_type = randomization_type,
    vary_urban = vary_urban, vary_race = vary_race, 
    generate_baseline_outcomes = generate_baseline_outcomes,
    beta_0 = beta_0, mu_b = mu_b, sigma_b = sigma_b,
    urban_min = urban_min, urban_max = urban_max, 
    race_min = race_min, race_max = race_max
  )
  # coarsen
  coarsened_data <- create_cluster_level_data(long_dataset = long_dataset, 
                                              threshold = threshold)
  # center proportion 
  coarsened_data$prop_race1 <- coarsened_data$prop_race1 - mean(coarsened_data$prop_race1)
  # fit all of the models
  shared_formula_txt <- "y ~ a + region + urban"
  individual_level_formulas <- paste0(
    shared_formula_txt, c("", " * a", " + race_1", " * a + race_1", 
                          " + race_1 * a", " * a + race_1 * a")
  )
  individual_level_model_nms <- paste0("1", c("a", "b", "c", "d", "e", "f"))
  cluster_level_formulas <- paste0(
    shared_formula_txt, 
    c("", " * a", " + prop_race1", " + high_prop_race1", 
      " * a + prop_race1", " * a + high_prop_race1",
      " + prop_race1 * a", " + high_prop_race1 * a",
      " * a + prop_race1 * a", " * a + high_prop_race1 * a"),
      ifelse(generate_baseline_outcomes, " + y0", "")
  )
  cluster_level_model_nms <- paste0("2", c(gsub("1", "", individual_level_model_nms), 
                                           "g", "h", "i", "j"))
  # get robust standard errors, corrected for small number of clusters;
  # denominator is K - p, where p is the number of variables used in constrained randomization
  dof_correction <- K / (K - ifelse(generate_baseline_outcomes, 3, 2))
  sink(tempfile()) # remove printing
  # fit individual-level data regression
  individual_level_summaries <- vector("list", length = length(individual_level_formulas))
  for (i in 1:length(individual_level_formulas)) {
    this_model <- gee::gee(as.formula(individual_level_formulas[i]),
                           id = community, data = subset(long_dataset, long_dataset$y0 == 0),
                           family = "gaussian", corstr = "independence", 
                           silent = TRUE)
    this_robust_cov <- dof_correction * this_model$robust.variance
    this_summary <- summary(this_model)$coefficients
    individual_level_summaries[[i]] <- tibble::tibble(
      model = individual_level_model_nms[i], coefficient_name = rownames(this_summary),
      est = this_summary[, 1], se = sqrt(diag(this_robust_cov))) %>% 
      mutate(cil = est - qnorm(0.975) * se, ciu = est + qnorm(0.975) * se,
             pval = 2 * pnorm(abs(est / se), lower.tail = FALSE),
             reject = as.numeric(pval < 0.05))
    
  }
  # fit cluster-level data regression
  cluster_level_summaries <- vector("list", length = length(cluster_level_formulas))
  for (i in 1:length(cluster_level_formulas)) {
    this_model <- tryCatch(gee::gee(as.formula(cluster_level_formulas[i]),
                                    id = community, data = coarsened_data,
                                    family = "gaussian", corstr = "independence", 
                                    silent = TRUE),
                           error = function(e) NA)
    if (any(is.na(this_model))) {
      model_mat <- model.matrix(as.formula(cluster_level_formulas[i]), 
                                data = coarsened_data)
      this_robust_cov <- matrix(NA, ncol = ncol(model_mat), nrow = ncol(model_mat))
      this_summary <- matrix(NA, nrow = ncol(model_mat))
      rownames(this_summary) <- colnames(model_mat)
    } else {
      this_robust_cov <- dof_correction * this_model$robust.variance
      this_summary <- summary(this_model)$coefficients
    }
    cluster_level_summaries[[i]] <- tibble::tibble(
      model = cluster_level_model_nms[i], coefficient_name = rownames(this_summary),
      est = this_summary[, 1], se = sqrt(diag(this_robust_cov))) %>% 
      mutate(cil = est - qnorm(0.975) * se, ciu = est + qnorm(0.975) * se,
             pval = 2 * pnorm(abs(est / se), lower.tail = FALSE),
             reject = as.numeric(pval < 0.05))
  }
  sink()
  all_summaries <- data.table::rbindlist(c(individual_level_summaries,
                                           cluster_level_summaries))
  full_summary <- dplyr::bind_cols(
    tibble::tibble(mc_id = mc_id, K = K, nk = nk, rand = randomization_type,
                   vary_urban = vary_urban, vary_race = vary_race, 
                   gen_base_outcomes = generate_baseline_outcomes,
                   inner_scenario = inner_scenario), 
    all_summaries
  )
  return(full_summary)
}
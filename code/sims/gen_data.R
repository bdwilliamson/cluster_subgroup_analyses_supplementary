# generate data for the cluster-trial subgroup analysis paper

# create an individual-level dataset in long format ----------------------------
gen_data_simple <- function(K = 40, nk = 100, beta_0 = c(0, 0.03, 0.05, 0.01, 0.14, 0, 0.1, 0.15), 
                            mu_b = 0.2, sigma_b = 0.05) {
  # region
  R <- rbinom(n = K, size = 1, prob = 0.5)
  # urban
  U <- rbinom(n = K, size = 1, prob = 0.5)
  # race/ethnicity
  # sample cluster sizes, if needed
  if (length(nk) == 1) {
    n_k <- rep(nk, K)
  } else {
    n_k <- sample(nk, size = K, replace = TRUE, prob = 1 / length(nk))
  }
  long_dataset <- do.call(rbind.data.frame, lapply(
    as.list(seq_len(K)), function(k) {
      data.frame(community = rep(k, n_k[k]), region = rep(R[k], n_k[k]),
                 urban = rep(U[k], n_k[k]), race_1 = NA, a = NA, y = NA)
    }
  ))
  p_x <- rep(0.5, K)
  for (k in seq_len(K)) {
    this_community <- long_dataset$community == k
    long_dataset$race_1[this_community] <- rbinom(n = n_k[k], size = 1, prob = p_x[k])
  }
  # randomize
  A <- rbinom(n = K, size = 1, prob = 0.5)
  long_dataset$a <- unlist(lapply(as.list(seq_len(K)), function(k) rep(A[k], n_k[k])))
  # generate outcomes
  b_init <- rnorm(n = K, mean = mu_b, sd = sigma_b)
  b <- unlist(lapply(as.list(seq_len(K)), function(k) rep(b_init[k], n_k[k])))
  model_mat <- model.matrix(~ (region + urban + race_1) * a, data = long_dataset)
  mu <- model_mat %*% beta_0 + b
  long_dataset$y <- rbinom(nrow(long_dataset), size = 1, prob = mu)
  return(long_dataset)
}
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
# @param urban_min, urban_max minimum and maximum for urban probability
# @param race_min, race_max parameters for race probabilities
# @return a long dataset (one row per participant within community)
gen_data <- function(K = 40, nk = 100, randomization_type = "simple",
                     vary_urban = FALSE, vary_race = FALSE,
                     generate_baseline_outcomes = FALSE, 
                     beta_0 = c(0, 0.03, 0.05, 0.01, 0.14, 0.02, 0.02, 0.05), 
                     mu_b = 0.2, sigma_b = 0.05,
                     urban_min = 0.4, urban_max = 0.6,
                     race_min = 0.3, race_max = 0.7) {
  # region
  R <- rbinom(n = K, size = 1, prob = 0.5)
  # urban/suburban vs rural
  if (vary_urban) {
    U <- rbinom(n = K, size = 1, prob = urban_min * (1 - R) + urban_max * R)
  } else {
    U <- rbinom(n = K, size = 1, prob = 0.5)
  }
  
  # sample cluster sizes, if needed
  if (length(nk) == 1) {
    n_k <- rep(nk, K)
  } else {
    n_k <- sample(nk, size = K, replace = TRUE, prob = 1 / length(nk))
  }
  # generate data within clusters
  long_dataset <- do.call(rbind.data.frame, lapply(as.list(seq_len(K)), function(k) {
    data.frame(community = rep(k, n_k[k]), region = rep(R[k], n_k[k]),
               urban = rep(U[k], n_k[k]), race_1 = NA, y0 = 0, 
               a = NA, y = NA)
  }))
  # random effects
  b_init <- rnorm(n = K, mean = mu_b, sd = sigma_b)
  b <- unlist(lapply(as.list(seq_len(K)), function(k) rep(b_init[k], n_k[k])))
  # set up probability of each self-reported race/ethnicity
  if (vary_race) {
    p_x_clust <- runif(n = K, min = race_min - 0.15 * (1 - R), max = race_max - 0.15 * (1 - R))
  } else {
    p_x_clust <- rep(0.5, K)
  }
  p_x <- unlist(lapply(as.list(seq_len(K)), function(k) rep(p_x_clust[k], n_k[k])))
  # generate baseline data
  long_dataset$race_1 <- rbinom(n = nrow(long_dataset), size = 1, prob = p_x)
  model_matrix_baseline <- model.matrix(~ region + urban + race_1, 
                                        data = long_dataset[, c(2, 3, 4)])
  mu_baseline <- model_matrix_baseline %*% beta_0[c(1, 2, 3, 4)] + b
  if (generate_baseline_outcomes) {
    long_dataset$y0 <- rbinom(n = nrow(long_dataset), size = 1, prob = pmin(1, pmax(mu_baseline, 0)))
  }
  # for (k in seq_len(K)) {
  #   this_community <- long_dataset$community == k
  #   # generate self-reported race/ethnicity data
  #   long_dataset[this_community, ]$race_1 <- rbinom(
  #     n = n_k[k], size = 1, prob = p_x[k]
  #   )
  #   # generate baseline outcomes, if requested
  #   if (generate_baseline_outcomes) {
  #     model_matrix <- model.matrix(~ region + urban + race_1, data = long_dataset[this_community, c(2, 3, 4)])
  #     mu <- model_matrix %*% beta_0[c(1, 2, 3, 4)] + b[k]
  #     long_dataset$y0[this_community == k] <- rbinom(n = n_k[k], size = 1, prob = mu)
  #   }
  # }
  # randomize
  if (randomization_type == "simple") {
    A <- rbinom(n = K, size = 1, prob = 0.5)  
  } else {
    # constrain on region, urban/rural status
    constrained_dataset <- data.frame(R, U)
    categorical_nms <- c("R", "U")
    if (generate_baseline_outcomes) {
      categorical_nms <- c(categorical_nms, "y0")
      region_mdns <- long_dataset %>% 
        group_by(region, community) %>% 
        summarize(booster_pct = mean(y0), .groups = "drop") %>% 
        group_by(region) %>% 
        summarize(mdn_booster = median(booster_pct))
      constrained_dataset$y0 <- long_dataset %>% 
        group_by(region, community) %>%
        summarize(booster_pct = mean(y0), .groups = "drop") %>% 
        left_join(region_mdns, by = "region") %>% 
        mutate(booster = as.numeric(booster_pct > mdn_booster)) %>% 
        arrange(community) %>% 
        pull(booster)
    } 
    randomization_object <- cvcrand::cvrall(
      x = constrained_dataset, ntotal_cluster = K,
      clustername = 1:K, ntrt_cluster = K / 2,
      cutoff = 0.05, size = 2e4, categorical = categorical_nms,
      balancemetric = "l2", stratify = "R", bhist = FALSE, seed = round(runif(1, 1e3, 1e4))
    )
    A <- randomization_object$allocation[, 2]
  }
  long_dataset$a <- unlist(lapply(as.list(seq_len(K)), function(k) rep(A[k], n_k[k])))
  # generate timepoint 1 outcomes
  model_mat <- model.matrix(~ (region + urban + race_1) * a, data = long_dataset)
  mu <- model_mat %*% beta_0 + b
  long_dataset$y <- ifelse(long_dataset$y0 == 0, 
                           rbinom(nrow(long_dataset), size = 1, prob = pmin(1, pmax(0, mu))), 
                           long_dataset$y0)
  # for (k in seq_len(K)) {
  #   this_community <- long_dataset$community == k
  #   # generate timepoint 1 outcomes among those without baseline booster
  #   model_matrix <- model.matrix(
  #     ~ (region + urban + race_1) * a, data = long_dataset[this_community, -1]
  #   )
  #   mu <- model_matrix %*% beta_0 + b[k]
  #   non_boosted_baseline <- this_community & long_dataset$y0 == 0
  #   long_dataset$y[this_community] <- long_dataset$y0[this_community]
  #   long_dataset$y[non_boosted_baseline] <- rbinom(
  #     n = sum(non_boosted_baseline), size = 1, prob = mu[long_dataset$y0[this_community] == 0]
  #   )
  # }
  # return the dataset
  return(tibble::as_tibble(long_dataset))
}

# create a cluster-level dataset given a long, individual-level dataset --------
# @param long_dataset an individual-level dataset, resulting from a call to gen_data
# @param threshold the threshold for "high" vs "low", used to dichotomize the proportion 
#         reporting race 1
# @return a coarsened dataset: one row per cluster, with self-reported race/ethnicity
#          coarsened according to the proportion within each cluster and high vs low
create_cluster_level_data <- function(long_dataset = NULL,
                                      threshold = 0.7) {
  coarsened_data <- dplyr::select(dplyr::summarize(
    dplyr::group_by(.data = long_dataset, community, region, urban, a),
    prop_race1 = mean(race_1), high_prop_race1 = as.numeric(prop_race1 > threshold),
    y0 = mean(y0), y = mean(y), .groups = "drop"
  ), community, region, urban, prop_race1, high_prop_race1, y0, a, y)
  return(coarsened_data)
}
# load simulation results, create plots and tables

library("dplyr")
library("tibble")
library("ggplot2")
library("cowplot")
theme_set(theme_cowplot())
library("here") # works interactively with an R project located in ~/<path to folder>/cluster_subgroup_analyses/code

plots_dir <- here::here("..", "plots")
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
}

# load the results -------------------------------------------------------------
files_to_load <- list.files(here::here("..", "results", "sims"), pattern = "output_rand_constrained")
output_tib <- tibble::as_tibble(
  data.table::rbindlist(
    lapply(as.list(paste0(here::here("..", "results", "sims"), "/", files_to_load)), function(x) readRDS(x))
  )
)
true_values <- readRDS(paste0(here::here("..", "results", "sims"), "/true_values.rds")) %>% 
  rename(inner_scenario = scenario) %>% 
  mutate(vary_urban = as.logical(vary_urban),
         vary_race = as.logical(vary_race),
         gen_base_outcomes = as.logical(gen_base_outcomes))
full_true_values <- true_values %>% 
  bind_rows(
    true_values %>% 
      filter(grepl("race", coefficient_name)) %>% 
      mutate(coefficient_name = gsub("race_1", "prop_race1", coefficient_name)),
    true_values %>% 
      filter(grepl("race", coefficient_name)) %>% 
      mutate(coefficient_name = gsub("race_1", "high_prop_race1", coefficient_name))
  )
output_final <- output_tib %>% 
  left_join(full_true_values, 
            by = c("inner_scenario", "gen_base_outcomes", "vary_urban", 
                   "vary_race", "coefficient_name")) %>% 
  mutate(k_fct = factor(K),
         `Baseline boosters` = factor(gen_base_outcomes, levels = c(FALSE, TRUE), labels = c("None", "Some")),
         `Inc. var. urban` = factor(vary_urban, levels = c(FALSE, TRUE), labels = c("No", "Yes")),
         `Inc. var. race` = factor(vary_race, levels = c(FALSE, TRUE), labels = c("No", "Yes")),
         `Scenario` = inner_scenario,
         bias = est - truth)
# save off
saveRDS(output_final, here::here("..", "results", "sims", "compiled_output.rds"))
# read in compiled output
output_final <- readRDS(here::here("..", "results", "sims", "compiled_output.rds"))
all_outer_scenarios <- expand.grid(baseline_outcomes = c(0, 1), 
                                   heterogeneity_race = c(0, 1), 
                                   heterogeneity_urban = c(0, 1))
# plots and tables with results ------------------------------------------------
# plot with power/type I error
power_tib <- output_final %>% 
  group_by(k_fct, nk, rand, `Baseline boosters`, `Inc. var. urban`, `Inc. var. race`, 
           `Scenario`, model, coefficient_name) %>% 
  summarize(reject_prop = mean(reject), .groups = "drop") %>% 
  rename(`Num. communities` = k_fct) %>% 
  mutate(model_interaction = factor(ifelse(model %in% c("1a", "1c", "2a", "2c", "2d"), "No", "Yes")),
         interaction_vars = factor(case_when(
           model %in% c("1a", "1c", "2a", "2c", "2d") ~ 1,
           model %in% c("1b", "1d", "2b", "2e", "2f") ~ 2,
           model %in% c("1e", "2g", "2h") ~ 3,
           model %in% c("1f", "2i", "2j") ~ 4
         ), levels = 1:4, labels = c("None", "Urban status", "Self-reported race", "Self-reported race and urban status")))
bias_tib <- output_final %>% 
  group_by(k_fct, nk, rand, `Baseline boosters`, `Inc. var. urban`, `Inc. var. race`, 
           `Scenario`, model, coefficient_name) %>% 
  summarize(mn_bias = mean(bias), mdn_bias = median(bias), .groups = "drop") %>% 
  rename(`Num. communities` = k_fct) %>% 
  mutate(model_interaction = factor(ifelse(model %in% c("1a", "1c", "2a", "2c", "2d"), "No", "Yes")),
         interaction_vars = factor(case_when(
           model %in% c("1a", "1c", "2a", "2c", "2d") ~ 1,
           model %in% c("1b", "1d", "2b", "2e", "2f") ~ 2,
           model %in% c("1e", "2g", "2h") ~ 3,
           model %in% c("1f", "2i", "2j") ~ 4
         ), levels = 1:4, labels = c("None", "Urban status", "Self-reported race", "Self-reported race and urban status")))
# get the scenarios corresponding to type I error / power for each coefficient
main_urban_alpha_scenarios <- c(1, 3, 6)
main_urban_power_scenarios <- (1:9)[-main_urban_alpha_scenarios]
main_race_alpha_scenarios <- c(1, 2, 5)
main_race_power_scenarios <- (1:9)[-main_race_alpha_scenarios]
em_urban_alpha_scenarios <- c(1, 2, 3, 4, 6, 8)
em_urban_power_scenarios <- (1:9)[-em_urban_alpha_scenarios]
em_race_alpha_scenarios <- c(1, 2, 3, 4, 5, 7)
em_race_power_scenarios <- (1:9)[-em_race_alpha_scenarios]

# create power / type I error plots (depending on scenario/coefficient), bias plots
ylim_type_1_error <- c(0, 0.2)
ylim_power <- c(0, 1)
plot_types <- c("png", "eps")
for (i in seq_len(nrow(all_outer_scenarios))) {
  this_bb <- all_outer_scenarios$baseline_outcomes[i]
  this_vu <- all_outer_scenarios$heterogeneity_urban[i]
  this_vr <- all_outer_scenarios$heterogeneity_race[i]
  this_tib <- power_tib %>% 
    filter(`Baseline boosters` == c("None", "Some")[this_bb + 1],
           `Inc. var. urban` == c("No", "Yes")[this_vu + 1],
           `Inc. var. race` == c("No", "Yes")[this_vr + 1])
  this_bias_summ_tib <- bias_tib %>% 
    filter(`Baseline boosters` == c("None", "Some")[this_bb + 1],
           `Inc. var. urban` == c("No", "Yes")[this_vu + 1],
           `Inc. var. race` == c("No", "Yes")[this_vr + 1])
  this_bias_tib <- output_final %>% 
    filter(`Baseline boosters` == c("None", "Some")[this_bb + 1],
           `Inc. var. urban` == c("No", "Yes")[this_vu + 1],
           `Inc. var. race` == c("No", "Yes")[this_vr + 1]) %>% 
    rename(`Num. communities` = k_fct) %>%
    mutate(model_interaction = factor(ifelse(model %in% c("1a", "1c", "2a", "2c", "2d"), "No", "Yes")),
           interaction_vars = factor(case_when(
             model %in% c("1a", "1c", "2a", "2c", "2d") ~ 1,
             model %in% c("1b", "1d", "2b", "2e", "2f") ~ 2,
             model %in% c("1e", "2g", "2h") ~ 3,
             model %in% c("1f", "2i", "2j") ~ 4
           ), levels = 1:4, c("None", "Urban status", "Self-reported race", "Self-reported race and urban status")))
  # difference in booster prob. by treatment arm
  main_effect_tx_power_plot <- this_tib %>% 
    filter(coefficient_name == "a") %>% 
    ggplot(aes(x = factor(model), y = reject_prop,
           color = model_interaction, shape = interaction_vars)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    ylim(ylim_power) +
    labs(title = "Power for difference in booster prob. by treatment arm",
         y = "Empirical rejection proportion", x = "Model",
         color = "Interaction modelled", shape = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.5) + 
    scale_shape_discrete() +
    guides(color = guide_legend(direction = "horizontal", nrow = 2),
           shape = guide_legend(direction = "horizontal", nrow = 2)) + 
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  for (plot_type in plot_types) {
    ggsave(filename = paste0(plots_dir, "/power_main_effect_tx_bb_", this_bb,
                             "_vu_", this_vu, "_vr_", this_vr, ".", plot_type),
           main_effect_tx_power_plot, width = 15, height = 8, units = "in")  
  }
  main_effect_tx_bias_summary_plot <- this_bias_summ_tib %>% 
    filter(coefficient_name == "a") %>% 
    ggplot(aes(x = factor(model), y = mdn_bias,
               color = model_interaction, shape = interaction_vars)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    labs(title = "Bias for estimating difference in booster prob. by treatment arm",
         y = "Empirical bias", x = "Model",
         color = "Interaction modelled", shape = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.5) + 
    scale_shape_discrete() +
    guides(color = guide_legend(direction = "horizontal", nrow = 2),
           shape = guide_legend(direction = "horizontal", nrow = 2)) + 
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  for (plot_type in plot_types) {
    ggsave(filename = paste0(plots_dir, "/bias_main_effect_tx_bb_", this_bb,
                             "_vu_", this_vu, "_vr_", this_vr, ".", plot_type),
           main_effect_tx_bias_summary_plot, width = 15, height = 8, units = "in")
  }
  main_effect_tx_bias_variability_plot <- this_bias_tib %>% 
    filter(coefficient_name == "a") %>% 
    ggplot(aes(x = factor(model), y = bias, color = interaction_vars)) +
    geom_violin() +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Distribution of bias for estimating difference in booster prob. by treatment arm",
         y = "Empirical bias", x = "Model", color = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.75) + 
    guides(color = guide_legend(direction = "horizontal", nrow = 2)) +
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  for (plot_type in plot_types) {
    ggsave(filename = paste0(plots_dir, "/bias_dist_main_effect_tx_bb_", this_bb,
                             "_vu_", this_vu, "_vr_", this_vr, ".", plot_type),
           main_effect_tx_bias_variability_plot, width = 15, height = 8, units = "in")
  }
  # difference in booster prob. by urban vs rural status
  main_effect_urban_alpha_plot <- this_tib %>% 
    filter(grepl("urban", coefficient_name) & !grepl(":", coefficient_name, fixed = TRUE), 
           `Scenario` %in% main_urban_alpha_scenarios) %>%
    ggplot(aes(x = factor(model), y = reject_prop,
               color = model_interaction, shape = interaction_vars)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    ylim(ylim_type_1_error) +
    geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
    labs(title = "Type I error for difference in booster prob. by urban vs rural status",
         y = "Empirical rejection proportion", x = "Model",
         color = "Interaction modelled", shape = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.5) + 
    scale_shape_discrete() +
    guides(color = guide_legend(direction = "horizontal", nrow = 2),
           shape = guide_legend(direction = "horizontal", nrow = 2)) + 
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  main_effect_urban_power_plot <- this_tib %>% 
    filter(grepl("urban", coefficient_name) & !grepl(":", coefficient_name, fixed = TRUE), 
           `Scenario` %in% main_urban_power_scenarios) %>%
    ggplot(aes(x = factor(model), y = reject_prop,
               color = model_interaction, shape = interaction_vars)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    ylim(ylim_power) +
    labs(title = "Power for difference in booster prob. by urban vs rural status",
         y = "Empirical rejection proportion", x = "Model",
         color = "Interaction modelled", shape = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.5) + 
    scale_shape_discrete() +
    guides(color = guide_legend(direction = "horizontal", nrow = 2),
           shape = guide_legend(direction = "horizontal", nrow = 2)) + 
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  urban_rejection_combined_plot <- plot_grid(
    plot_grid(main_effect_urban_alpha_plot + theme(legend.position = "none"), 
              main_effect_urban_power_plot + theme(legend.position = "none"),
              ncol = 2),
    get_legend(main_effect_urban_alpha_plot), nrow = 2, rel_heights = c(1, .1) 
  )
  for (plot_type in plot_types) {
    ggsave(filename = paste0(plots_dir, "/power_main_effect_urban_bb_", this_bb,
                             "_vu_", this_vu, "_vr_", this_vr, ".", plot_type),
           urban_rejection_combined_plot, 
           width = 20, height = 8, units = "in")
  }
  main_effect_urban_bias_summary_plot <- this_bias_summ_tib %>% 
    filter(grepl("urban", coefficient_name) & !grepl(":", coefficient_name, fixed = TRUE)) %>% 
    ggplot(aes(x = factor(model), y = mdn_bias,
               color = model_interaction, shape = interaction_vars)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    labs(title = "Bias for estimating difference in booster prob. by urban vs rural status",
         y = "Empirical bias", x = "Model",
         color = "Interaction modelled", shape = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.5) + 
    scale_shape_discrete() +
    guides(color = guide_legend(direction = "horizontal", nrow = 2),
           shape = guide_legend(direction = "horizontal", nrow = 2)) + 
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  for (plot_type in plot_types) {
    ggsave(filename = paste0(plots_dir, "/bias_main_effect_urban_bb_", this_bb,
                             "_vu_", this_vu, "_vr_", this_vr, ".", plot_type),
           main_effect_urban_bias_summary_plot, width = 15, height = 8, units = "in")
  }
  main_effect_urban_bias_variability_plot <- this_bias_tib %>% 
    filter(grepl("urban", coefficient_name) & !grepl(":", coefficient_name, fixed = TRUE)) %>% 
    ggplot(aes(x = factor(model), y = bias, color = interaction_vars)) +
    geom_violin() +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Distribution of bias for estimating difference in booster prob. by urban vs rural status",
         y = "Empirical bias", x = "Model", color = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.75) + 
    guides(color = guide_legend(direction = "horizontal", nrow = 2)) +
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  for (plot_type in plot_types) {
    ggsave(filename = paste0(plots_dir, "/bias_dist_main_effect_urban_bb_", this_bb,
                             "_vu_", this_vu, "_vr_", this_vr, ".", plot_type),
           main_effect_urban_bias_variability_plot, width = 15, height = 8, units = "in")
  }
  # difference in booster prob. by self-reported race
  main_effect_race_alpha_plot <- this_tib %>% 
    filter(grepl("race", coefficient_name) & !grepl(":", coefficient_name, fixed = TRUE), 
           `Scenario` %in% main_race_alpha_scenarios) %>%
    ggplot(aes(x = factor(model), y = reject_prop,
               color = model_interaction, shape = interaction_vars)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    ylim(ylim_type_1_error) +
    geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
    labs(title = "Type I error for difference in booster prob. by self-reported race",
         y = "Empirical rejection proportion", x = "Model",
         color = "Interaction modelled", shape = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.5) + 
    scale_shape_discrete() +
    guides(color = guide_legend(direction = "horizontal", nrow = 2),
           shape = guide_legend(direction = "horizontal", nrow = 2)) + 
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  main_effect_race_power_plot <- this_tib %>% 
    filter(grepl("race", coefficient_name) & !grepl(":", coefficient_name, fixed = TRUE), 
           `Scenario` %in% main_race_power_scenarios) %>%
    ggplot(aes(x = factor(model), y = reject_prop,
               color = model_interaction, shape = interaction_vars)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    ylim(ylim_power) +
    labs(title = "Power for difference in booster prob. by self-reported race",
         y = "Empirical rejection proportion", x = "Model",
         color = "Interaction modelled", shape = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.5) + 
    scale_shape_discrete() +
    guides(color = guide_legend(direction = "horizontal", nrow = 2),
           shape = guide_legend(direction = "horizontal", nrow = 2)) + 
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  race_rejection_combined_plot <- plot_grid(
    plot_grid(main_effect_race_alpha_plot + theme(legend.position = "none"), 
              main_effect_race_power_plot + theme(legend.position = "none"),
              ncol = 2),
    get_legend(main_effect_race_alpha_plot), nrow = 2, rel_heights = c(1, .1) 
  )
  for (plot_type in plot_types) {
    ggsave(filename = paste0(plots_dir, "/power_main_effect_race_bb_", this_bb,
                             "_vu_", this_vu, "_vr_", this_vr, ".", plot_type),
           race_rejection_combined_plot, 
           width = 20, height = 8, units = "in")
  }
  main_effect_race_bias_summary_plot <- this_bias_summ_tib %>% 
    filter(grepl("race", coefficient_name) & !grepl(":", coefficient_name, fixed = TRUE)) %>% 
    ggplot(aes(x = factor(model), y = mdn_bias,
               color = model_interaction, shape = interaction_vars)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    labs(title = "Bias for estimating difference in booster prob. by self-reported race",
         y = "Empirical bias", x = "Model",
         color = "Interaction modelled", shape = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.5) + 
    scale_shape_discrete() +
    guides(color = guide_legend(direction = "horizontal", nrow = 2),
           shape = guide_legend(direction = "horizontal", nrow = 2)) + 
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  for (plot_type in plot_types) {
    ggsave(filename = paste0(plots_dir, "/bias_main_effect_race_bb_", this_bb,
                             "_vu_", this_vu, "_vr_", this_vr, ".", plot_type),
           main_effect_race_bias_summary_plot, width = 15, height = 8, units = "in")
  }
  main_effect_race_bias_variability_plot <- this_bias_tib %>% 
    filter(grepl("race", coefficient_name) & !grepl(":", coefficient_name, fixed = TRUE)) %>% 
    ggplot(aes(x = factor(model), y = bias, color = interaction_vars)) +
    geom_violin() +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Distribution of bias for estimating difference in booster prob. by self-reported race",
         y = "Empirical bias", x = "Model", color = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.75) + 
    guides(color = guide_legend(direction = "horizontal", nrow = 2)) +
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  for (plot_type in plot_types) {
    ggsave(filename = paste0(plots_dir, "/bias_dist_main_effect_race_bb_", this_bb,
                             "_vu_", this_vu, "_vr_", this_vr, ".", plot_type),
           main_effect_race_bias_variability_plot, width = 15, height = 8, units = "in")
  }
  # HTE by urban/suburban vs rural status
  em_urban_alpha_plot <- this_tib %>% 
    filter(grepl("urban", coefficient_name) & grepl(":", coefficient_name, fixed = TRUE), 
           `Scenario` %in% em_urban_alpha_scenarios) %>%
    ggplot(aes(x = factor(model), y = reject_prop,
              shape = interaction_vars)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    ylim(ylim_type_1_error) +
    geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
    labs(title = "Type I error for HTE by urban/suburban vs rural status",
         y = "Empirical rejection proportion", x = "Model",
         shape = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.5) + 
    scale_shape_discrete() +
    guides(color = guide_legend(direction = "horizontal", nrow = 2),
           shape = guide_legend(direction = "horizontal", nrow = 2)) + 
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  em_urban_power_plot <- this_tib %>% 
    filter(grepl("urban", coefficient_name) & grepl(":", coefficient_name, fixed = TRUE), 
           `Scenario` %in% em_urban_power_scenarios) %>%
    ggplot(aes(x = factor(model), y = reject_prop,
               shape = interaction_vars)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    ylim(ylim_power) +
    labs(title = "Power for HTE by urban/suburban vs rural status",
         y = "Empirical rejection proportion", x = "Model",
         shape = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.5) + 
    scale_shape_discrete() +
    guides(color = guide_legend(direction = "horizontal", nrow = 2),
           shape = guide_legend(direction = "horizontal", nrow = 2)) + 
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  urban_em_rejection_combined_plot <- plot_grid(
    plot_grid(em_urban_alpha_plot + theme(legend.position = "none"), 
              em_urban_power_plot + theme(legend.position = "none"),
              ncol = 2),
    get_legend(em_urban_alpha_plot), nrow = 2, rel_heights = c(1, .1) 
  )
  for (plot_type in plot_types) {
    ggsave(filename = paste0(plots_dir, "/power_em_urban_bb_", this_bb,
                             "_vu_", this_vu, "_vr_", this_vr, ".", plot_type),
           urban_em_rejection_combined_plot, 
           width = 15, height = 8, units = "in")
  }
  em_urban_bias_summary_plot <- this_bias_summ_tib %>% 
    filter(grepl("urban", coefficient_name) & grepl(":", coefficient_name, fixed = TRUE)) %>% 
    ggplot(aes(x = factor(model), y = mdn_bias,
               shape = interaction_vars)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    labs(title = "Bias for estimating HTE by urban/suburban vs rural status",
         y = "Empirical bias", x = "Model",
         shape = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.5) + 
    scale_shape_discrete() +
    guides(color = guide_legend(direction = "horizontal", nrow = 2),
           shape = guide_legend(direction = "horizontal", nrow = 2)) + 
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  for (plot_type in plot_types) {
    ggsave(filename = paste0(plots_dir, "/bias_em_urban_bb_", this_bb,
                             "_vu_", this_vu, "_vr_", this_vr, ".", plot_type),
           em_urban_bias_summary_plot, width = 15, height = 8, units = "in")
  }
  em_urban_bias_variability_plot <- this_bias_tib %>% 
    filter(grepl("urban", coefficient_name) & grepl(":", coefficient_name, fixed = TRUE)) %>% 
    ggplot(aes(x = factor(model), y = bias, color = interaction_vars)) +
    geom_violin() +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Distribution of bias for estimating HTE by urban/suburban vs rural status",
         y = "Empirical bias", x = "Model", color = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.75) + 
    guides(color = guide_legend(direction = "horizontal", nrow = 2)) +
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  for (plot_type in plot_types) {
    ggsave(filename = paste0(plots_dir, "/bias_dist_em_urban_bb_", this_bb,
                             "_vu_", this_vu, "_vr_", this_vr, ".", plot_type),
           em_urban_bias_variability_plot, width = 15, height = 8, units = "in")
  }
  # HTE by self-reported race
  em_race_alpha_plot <- this_tib %>% 
    filter(grepl("race", coefficient_name) & grepl(":", coefficient_name, fixed = TRUE), 
           `Scenario` %in% em_race_alpha_scenarios) %>%
    ggplot(aes(x = factor(model), y = reject_prop,
               shape = interaction_vars)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    ylim(ylim_type_1_error) +
    geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
    labs(title = "Type I error for HTE by self-reported race",
         y = "Empirical rejection proportion", x = "Model",
         shape = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.5) + 
    scale_shape_discrete() +
    guides(color = guide_legend(direction = "horizontal", nrow = 2),
           shape = guide_legend(direction = "horizontal", nrow = 2)) + 
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  em_race_power_plot <- this_tib %>% 
    filter(grepl("race", coefficient_name) & grepl(":", coefficient_name, fixed = TRUE), 
           `Scenario` %in% em_race_power_scenarios) %>%
    ggplot(aes(x = factor(model), y = reject_prop,
               shape = interaction_vars)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    ylim(ylim_power) +
    labs(title = "Power for HTE by self-reported race",
         y = "Empirical rejection proportion", x = "Model",
         shape = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.5) + 
    scale_shape_discrete() +
    guides(color = guide_legend(direction = "horizontal", nrow = 2),
           shape = guide_legend(direction = "horizontal", nrow = 2)) + 
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  race_em_rejection_combined_plot <- plot_grid(
    plot_grid(em_race_alpha_plot + theme(legend.position = "none"), 
              em_race_power_plot + theme(legend.position = "none"),
              ncol = 2),
    get_legend(em_race_alpha_plot), nrow = 2, rel_heights = c(1, .1) 
  )
  for (plot_type in plot_types) {
    ggsave(filename = paste0(plots_dir, "/power_em_race_bb_", this_bb,
                             "_vu_", this_vu, "_vr_", this_vr, ".", plot_type),
           race_em_rejection_combined_plot, 
           width = 15, height = 8, units = "in")
  }
  em_race_bias_summary_plot <- this_bias_summ_tib %>% 
    filter(grepl("race", coefficient_name) & grepl(":", coefficient_name, fixed = TRUE)) %>% 
    ggplot(aes(x = factor(model), y = mdn_bias,
               shape = interaction_vars)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    labs(title = "Bias for estimating HTE by self-reported race",
         y = "Empirical bias", x = "Model",
         shape = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.5) + 
    scale_shape_discrete() +
    guides(color = guide_legend(direction = "horizontal", nrow = 2),
           shape = guide_legend(direction = "horizontal", nrow = 2)) + 
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  for (plot_type in plot_types) {
    ggsave(filename = paste0(plots_dir, "/bias_em_race_bb_", this_bb,
                             "_vu_", this_vu, "_vr_", this_vr, ".", plot_type),
           em_race_bias_summary_plot, width = 15, height = 8, units = "in")
  }
  em_race_bias_variability_plot <- this_bias_tib %>% 
    filter(grepl("race", coefficient_name) & grepl(":", coefficient_name, fixed = TRUE)) %>% 
    ggplot(aes(x = factor(model), y = bias, color = interaction_vars)) +
    geom_violin() +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Distribution of bias for estimating HTE by self-reported race",
         y = "Empirical bias", x = "Model", color = "Interaction variable(s)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    scale_color_viridis_d(begin = 0, end = 0.75) + 
    guides(color = guide_legend(direction = "horizontal", nrow = 2)) +
    facet_grid(cols = vars(`Scenario`), rows = vars(`Num. communities`), 
               labeller = label_both, scales = "free") +
    theme(panel.grid.major.x = element_line(color = "grey85"),
          panel.grid.major.y = element_line(color = "grey85"),
          legend.position = "bottom")
  for (plot_type in plot_types) {
    ggsave(filename = paste0(plots_dir, "/bias_dist_em_race_bb_", this_bb,
                             "_vu_", this_vu, "_vr_", this_vr, ".", plot_type),
           em_race_bias_variability_plot, width = 15, height = 8, units = "in")
  }
} 

# make a table with rejection proportion for HTE by self-reported race, all outer scenarios
# include only inner scenarios 1 (no HTE) and 6 (HTE by self-reported race)
overall_race_hte_type_1_error <- power_tib %>% 
  filter(grepl("race", coefficient_name) & grepl(":", coefficient_name, fixed = TRUE),
         Scenario == 1, `Num. communities` == 80) %>% 
  ggplot(aes(x = factor(model), y = reject_prop, shape = interaction_vars)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  ylim(ylim_type_1_error) +
  labs(title = "Type I error for HTE by self-reported race",
       y = "Empirical rejection proportion", x = "Model",
       shape = "Interaction variable(s)") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_color_viridis_d(begin = 0, end = 0.5) + 
  scale_shape_discrete() +
  guides(color = guide_legend(direction = "horizontal", nrow = 2),
         shape = guide_legend(direction = "horizontal", nrow = 2)) + 
  # facet_grid(cols = vars(`Baseline boosters`) + vars(`Inc. var. urban`) + vars(`Inc. var. race`), 
  #            rows = vars(`Scenario`), 
  facet_grid(`Baseline boosters` + `Inc. var. urban` + `Inc. var. race` ~ `Scenario`,
             labeller = label_both, scales = "free") +
  theme(panel.grid.major.x = element_line(color = "grey85"),
        panel.grid.major.y = element_line(color = "grey85"),
        legend.position = "bottom")

overall_race_hte_power <- power_tib %>% 
  filter(grepl("race", coefficient_name) & grepl(":", coefficient_name, fixed = TRUE),
         Scenario == 6, `Num. communities` == 80) %>% 
  ggplot(aes(x = factor(model), y = reject_prop, shape = interaction_vars)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  ylim(ylim_power) +
  labs(title = "Power for HTE by self-reported race",
       y = "Empirical rejection proportion", x = "Model",
       shape = "Interaction variable(s)") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_color_viridis_d(begin = 0, end = 0.5) + 
  scale_shape_discrete() +
  guides(color = guide_legend(direction = "horizontal", nrow = 2),
         shape = guide_legend(direction = "horizontal", nrow = 2)) + 
  facet_grid(`Baseline boosters` + `Inc. var. urban` + `Inc. var. race` ~ `Scenario`,
             labeller = label_both, scales = "free") +
  theme(panel.grid.major.x = element_line(color = "grey85"),
        panel.grid.major.y = element_line(color = "grey85"),
        legend.position = "bottom")

overall_race_hte_combined_plot <- plot_grid(
  plot_grid(overall_race_hte_type_1_error + theme(legend.position = "none"), 
            overall_race_hte_power + theme(legend.position = "none"),
            ncol = 2),
  get_legend(overall_race_hte_power), nrow = 2, rel_heights = c(1, .1) 
)

for (plot_type in plot_types) {
  ggsave(filename = paste0(plots_dir, "/overall_race_hte.", plot_type),
         overall_race_hte_combined_plot, width = 15, height = 20, units = "in")
}

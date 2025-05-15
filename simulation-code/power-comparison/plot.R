# This is a Rscript plotting the simulation results
library(ggplot2)
library(gridExtra)
library(cowplot)
library(grid)
library(dplyr)
library(reshape2)
library(katlabutils)
library(tidyr)
library(tibble)
library(latex2exp)

# specify parameter list
distribution_list <- c("gaussian", "Bernoulli", "Poisson", "mix_normal", "Student")
signal_list <- seq(0, 0.04, by = 0.01)
alpha <- 0.05

# create figure directory
figures_dir <- "manuscript/figures-and-tables/simulation"
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir)
  cat("Directory created:", figures_dir, "\n")
} else {
  cat("Directory already exists:", figures_dir, "\n")
}

# define the theme for plotting
my_theme <-   theme_bw() + 
  theme(
    legend.position = "none",
    axis.line = element_line(color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(
      color = "black"
    ),
    plot.title = element_blank(),
    legend.title = element_blank()
  )

############################ summarize results #################################
results_dir <- "simulation-code/power-comparison"

# specify the empty df
full_results_power <- data.frame(NULL)
full_results_pvalue <- data.frame(NULL)

# loop over distribution and then summariz the results
for (distribution in distribution_list) {
  
  # create empty dataframe
  final_result <- data.frame(NULL)
  
  # loop over signal
  for (signal in signal_list) {
    
    # load the results
    result_rds <- readRDS(sprintf("%s/%s/%s_%.2f_power_comparison.rds", 
                                  results_dir, distribution, distribution, signal))
    
    # combined two results
    result_rds <-  result_rds$results |> 
      mutate(signal_label = sprintf("theta == %.2f", signal),
             signal = signal)
    final_result <- rbind(final_result, result_rds |>
                            unnest(output) |>
                            mutate(
                              p_value = sapply(output, function(x) unlist(x)),
                              sideness = names(p_value)
                            ) |>
                            dplyr::select(-output))
  }
  
  
  # rearrange the results to get pvalue
  pvalue_df <- final_result |>
    dplyr::mutate(
      stat_form = case_when(
        method %in% c("Normalized_adaptive", "Normalized_dim") ~ "normalized",
        TRUE ~ "unnormalized"
      ),
      weighting = if_else(method %in% c("Normalized_adaptive", "Unnormalized_adaptive"),
                          "m = 0.5", "m = 1"))
  
  # extract the power
  power_df <- pvalue_df |>
    group_by(stat_form, weighting, signal_label, sideness, signal) |>
    summarise(
      rejection = mean(p_value <= alpha)
    ) |>
    ungroup()
  
  
  # rbind the results
  full_results_power <- rbind(full_results_power, 
                              power_df |> mutate(distribution = distribution))
  full_results_pvalue <- rbind(full_results_pvalue, 
                               pvalue_df |> mutate(distribution = distribution))
}

########################### plotting code ######################################

# Custom labels
custom_labels <- c("gaussian" = "Gaussian", 
                   "Bernoulli" = "Bernoulli", 
                   "Poisson" = "Poisson", 
                   "Student" = "Student")

########################### rejection plot ###################################

# plot the other distributions
rejection_plot <- full_results_power |>
  filter(sideness == "both" & distribution %in% c("gaussian", "Bernoulli", "Poisson", "Student")) |>
  ggplot(aes(x = signal, y = rejection, color = stat_form, linetype = weighting)) +
  scale_x_continuous() +
  scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  facet_wrap(~distribution, ncol = 4, labeller = as_labeller(custom_labels)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = alpha), linetype = "dashed", color = "red") +
  my_theme +
  labs(y = "Rejection rate", x = "Signal strength", title = "") +
  theme(strip.text.x = element_text(margin = margin(0.04, 0, 0.04, 0, "cm"),
                                    size = 14),
        strip.text.y = element_text(margin = margin(0.04, 0, 0.04, 0, "cm"),
                                    size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.key = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


# save the plot
ggsave(sprintf("%s/rejection_plot_power_comparison.pdf", figures_dir),
       rejection_plot, height = 3, width = 8)


######################### dot plot for p-values ################################

# dot plot
dot_plot <- full_results_pvalue |>
  dplyr::select(run_id, p_value, method, signal_label, sideness, distribution) |>
  filter(sideness == "both" & distribution %in% c("gaussian", "Bernoulli", "Poisson", "Student")) |>
  pivot_wider(id_cols = c("distribution", "sideness", "run_id", "signal_label"),
              names_from = "method",
              values_from = "p_value") |>
  ggplot(aes(x = Unnormalized_dim, y = Normalized_adaptive)) +
  scale_x_continuous(trans = revlog_trans(10),
                     breaks = c(1e-1, 1e-3),
                     labels = c(expression(10^{-1}), expression(10^{-3}))) +
  scale_y_continuous(trans = revlog_trans(10),
                     breaks = c(1e-1, 1e-3),
                     labels = c(expression(10^{-1}), expression(10^{-3}))) +
  facet_grid(signal_label~distribution, 
             labeller = labeller(
               distribution = custom_labels,
               signal_label = label_parsed)) +
  geom_point(size = .3) +
  geom_abline() +
  my_theme +
  labs(x = "Weighting with m = 1 (unnormalized)", y = "Weighting with m = 0.5 (normalized)", title = "") +
  theme(strip.text.x = element_text(margin = margin(0.04, 0, 0.04, 0, "cm"),
                                    size = 14),
        strip.text.y = element_text(margin = margin(0.04, 0, 0.04, 0, "cm"),
                                    size = 14),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))


# save the plot
ggsave(sprintf("%s/p_value_dot_plot.pdf", figures_dir), dot_plot, height = 6, width = 6)

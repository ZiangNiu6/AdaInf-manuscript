library(AdaReg)
library(ggplot2)
library(reshape2)
library(dplyr)
# sample plugin unnormalized adaptive weighting bootstrap
set.seed(1)
mean_estimate <- c(0, 0)
variance_estimate <- c(1, 9)
eps <- 0.05
fs_p <- 0.5
B = 1e5
h_list <- seq(-15, 0, length.out = 4)
normalization = "unnormalized"
weighting = "aw"
sample_matrix <- matrix(NA, nrow = B, ncol = length(h_list),
                        dimnames = list(
                          reps = 1:B,
                          h = h_list
                        ))

for (h in h_list) {
  sample_matrix[, as.character(h)] <- plugin_bootstrap(mean_estimate = mean_estimate,
                                                       variance_estimate = variance_estimate,
                                                       eps = eps,
                                                       fs_p = fs_p,
                                                       B = B, h = h,
                                                       normalization = normalization,
                                                       weighting = weighting)
}

# plot the histogram

sample_df <- melt(sample_matrix) |>
  mutate(signal = factor(h, levels = rev(unique(h)))) |>
  dplyr::select(-h)

# define the name list 
custom_labels <- c("c = -15", "c = -10", "c = -5", "c = 0")

# Plot histograms for each column
asy_plot <- ggplot(sample_df, aes(x = value)) +
  geom_histogram(binwidth = 0.5, fill = "purple") +
  facet_wrap(~ signal, scales = "free_x", ncol = 4, labeller = labeller(signal = setNames(custom_labels, unique(sample_df$signal)))) +
  scale_x_continuous(breaks = c(-10, 0, 10, 20)) +
  theme_bw() +
  theme(
    strip.text = element_text(margin = margin(0.04, 0, 0.04, 0, "cm"),
                              size = 10),
    axis.text.x = element_text(size = 10),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),   # Remove y-axis text
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
    panel.grid.minor.y = element_blank()   # Remove minor y-axis grid lines
  ) +
  labs(x = NULL, y = NULL, title = NULL)

# save the plot
figures_dir <- "manuscript/figures-and-tables/auxiliary"
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir)
  cat("Directory created:", figures_dir, "\n")
} else {
  cat("Directory already exists:", figures_dir, "\n")
}
ggsave(sprintf("%s/asymptotic_distribution.pdf", figures_dir), plot = asy_plot, height = 2, width = 6)

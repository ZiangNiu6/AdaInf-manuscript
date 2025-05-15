# This is a Rscript plotting the QQ-plot and rejection plot for data analysis
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

# set significance level
alpha <- 0.05

# create figure directory
figures_dir <- "manuscript/figures-and-tables/realdata"
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir)
  cat("Directory created:", figures_dir, "\n")
} else {
  cat("Directory already exists:", figures_dir, "\n")
}

# load null and alternative p-values
null_pvalue <- readRDS("realdata-code/results/null_pvalue.rds")
alternative_pvalue <- readRDS("realdata-code/results/alternative_pvalue.rds")

########################### rejection plot #####################################

# rearrange the results to get pvalue
power_df <- alternative_pvalue |>
  mutate(    
    stat_form = case_when(
      method %in% c("UA", "UC") ~ "unnormalized",
      method %in% c("NA", "NC") ~ "normalized",
      TRUE ~ "sample-splitting"
    ),
    weighting = if_else(method %in% c("UA", "NA"), "adaptive weighting", 
                        "constant weighting")) |>
  group_by(stat_form, weighting, signal, eps) |>
  summarise(
    rejection = mean(p_value <= alpha)
  ) |>
  ungroup() |>
  mutate(
    eps = sprintf("epsilon == %.2f", eps)
  )

# Modify individual plot generation code (removing axis titles)
signal_list <- seq(0, 0.06, length.out = 5)
power_df <- power_df |> dplyr::filter(signal %in% signal_list)
rejection_plot <- power_df |>
  ggplot(aes(x = signal, y = rejection, color = stat_form, linetype = weighting)) +
  scale_x_continuous(breaks = signal_list) +
  scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  ggh4x::facet_grid2(. ~ eps, scales = "fixed", labeller = label_parsed) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_hline(aes(yintercept = alpha), linetype = "dashed", color = "red") +
  theme_bw() +
  labs(title = "") +
  theme(strip.text.x = element_text(margin = margin(0.04, 0, 0.04, 0, "cm"),
                                    size = 14),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),   # Remove individual axis titles
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        panel.spacing = unit(1.2, "lines"),
        plot.margin = margin(r = 11))

# Extract the legends from the plots without background
legend1 <- get_plot_component(
  ggplot(power_df, 
         aes(x = signal, y = rejection, linetype = weighting)) +
    geom_point(size = 1.5) +
    geom_line(size = 1) +
    theme(legend.position = "bottom",
          legend.key = element_blank(),  # Removes the background of legend keys
          legend.text = element_text(size = 14),   # Increase legend text font size
          legend.title = element_text(size = 16)  # Increase legend title font size
    ),'guide-box', return_all = TRUE)

legend2 <- get_plot_component(
  ggplot(power_df, 
         aes(x = signal, y = rejection, color = stat_form)) +
    geom_point(size = 1.5) +
    geom_line(size = 1) +
    labs(color = "statistic format") +
    theme(legend.position = "bottom",
          legend.key = element_blank(),  # Removes the background of legend keys
          legend.text = element_text(size = 14),   # Increase legend text font size
          legend.title = element_text(size = 16)  # Increase legend title font size
    ),'guide-box', return_all = TRUE)

# Combine the two legends into one
combined_legend <- plot_grid(legend1[[3]], legend2[[3]], ncol = 1)

# Combine the two plots in two rows
combined_plots <- plot_grid(rejection_plot, ncol = 1)

# Create a shared Y-axis title
y_title <- textGrob("Rejection rate", rot = 90, gp = gpar(fontsize = 14))

# Create a shared X-axis title
x_title <- textGrob("Signal strength", gp = gpar(fontsize = 14))

# Combine everything
final_plot_with_labels <- grid.arrange(
  arrangeGrob(combined_plots, left = y_title, bottom = x_title)
)

# Combine with the legend at the bottom
final_plot <- plot_grid(final_plot_with_labels, combined_legend, ncol = 1, rel_heights = c(2.5, 0.7))

# save the plot
ggsave(sprintf("%s/rejection_plot.pdf", figures_dir),
       final_plot, height = 4, width = 8)

################################## QQ plot #####################################


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

# plot the p-value
qq_plot <- as_tibble(as.table(null_pvalue)) |>
  mutate(    
    stat_form = case_when(
      method %in% c("UA", "UC") ~ "unnormalized",
      method %in% c("NA", "NC") ~ "normalized",
      TRUE ~ "sample-splitting"
    ),
    weighting = if_else(method %in% c("UA", "NA"), "adaptive weighting", 
                        "constant weighting")) |>
  mutate(p_value = n,
         eps = sprintf("epsilon == %.2f", as.numeric(eps))) |>
  ggplot(mapping = aes(y = p_value, color = stat_form, shape = weighting)) +
  facet_grid(.~eps, scales = "fixed", labeller = label_parsed) +
  stat_qq_points(ymin = 1e-6, size = 0.8) +
  stat_qq_band() +
  geom_abline(col = "black") +
  scale_x_continuous(trans = revlog_trans(10),
                     breaks = c(1, 1e-1, 1e-2, 1e-3),
                     labels = c(expression(10^{0}), expression(10^{-1}), expression(10^{-2}), expression(10^{-3}))) +
  scale_y_continuous(trans = revlog_trans(10),
                     breaks = c(1, 1e-2, 1e-4, 1e-6),
                     labels = c(expression(10^{0}), expression(10^{-2}), expression(10^{-4}), expression(10^{-6}))) +
  guides(color = guide_legend(override.aes = list(size = 1))) +
  labs(y = "Observed p-value",
       x = "Expected p-value") +
  my_theme +
  theme(strip.text.x = element_text(margin = margin(0.04, 0, 0.04, 0, "cm"),
                                    size = 14),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),   # Remove individual axis titles
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14))

# Extract the legends from the plots without background
legend1_update <- get_plot_component(
  ggplot(power_df, 
         aes(x = signal, y = rejection, shape = weighting)) +
    geom_point(size = 1.5) +
    geom_line(size = 1) +
    theme(legend.position = "bottom",
          legend.key = element_blank(),  # Removes the background of legend keys
          legend.text = element_text(size = 14),   # Increase legend text font size
          legend.title = element_text(size = 16)  # Increase legend title font size
    ),'guide-box', return_all = TRUE)


# Combine the two legends into one
combined_legend <- plot_grid(legend1_update[[3]], legend2[[3]], ncol = 1)

# Combine everything
final_plot_with_labels <- grid.arrange(
  arrangeGrob(plot_grid(qq_plot, ncol = 1))
)

# Combine with the legend at the bottom
final_plot <- plot_grid(final_plot_with_labels, combined_legend, ncol = 1, rel_heights = c(2.5, 0.7))

# save plot
ggsave(sprintf("%s/qq_plot.pdf", figures_dir),
       final_plot, height = 4, width = 8)

# This is a file demonstrating the usage of the functions in AdaReg package
library(AdaReg)
library(ggplot2)
library(dplyr)
library(reshape2)
library(latex2exp)
set.seed(1)

# number of replication
B_rep <- 5000

# parameter in epsilon-greedy algorithm
eps <- 0.05

# distribution family
dist_fam <- c("gaussian")

# number of sample
n <- 1000

# initial sampling probability
initial_prob <- c(0.5, 0.5)

# signal level
signal_list <- seq(0, 15, length.out = 4)

# create empty array
result_mat <- matrix(
  data = NA,
  nrow = B_rep, ncol = length(signal_list),
  dimnames =  list(realization = 1:B_rep,
                   signal = signal_list)
  
)

# loop over signal 
for (signal in signal_list){
  
  # true mean vector
  dist_params <- data.frame(
    mu = c(0, signal / sqrt(n)),
    sigma = c(1, 3)
  )
  p <- 0
  
  # loop over B_rep replications
  cumulative_realization <- 0
  while (cumulative_realization < B_rep) {
    
    # generate data
    data <- data_generate_online(eps = eps, dist_fam = dist_fam, n_1 = n * 0.5,
                                 n_2 = n * 0.5, p = p, deciding_scheme = "IPW",
                                 dist_params = dist_params, initial_prob = initial_prob, reg_type = reg_type,
                                 hyperparams = hyperparams)
    
    # drop the replication if there is no observation for the inferior arm 1
    num_arm_second_batch <- length(unique(data$arm[which(data$batch_id == 2)]))
    if(num_arm_second_batch == 1){
      next
    }
    
    ################################################################################
    # adaptive weighting
    IPW_pt <- weighted_IPW(data)
    point_estimate <- IPW_pt["point", 1] - IPW_pt["point", 2]

    ################################################################################
    # store the test statistic
    result_mat[cumulative_realization + 1, as.character(signal)] <- point_estimate * sqrt(n) + signal
    
    # add the loop iteration
    cumulative_realization <- cumulative_realization + 1
  }
}

# rearrange the dataframe 
data_to_plot <- melt(result_mat) |>
  mutate(signal_adjusted = sprintf("c[N] == %d", -as.integer(signal))) |>
  mutate(signal_adjusted = factor(signal_adjusted, levels = c("c[N] == 0", "c[N] == -5", "c[N] == -10", "c[N] == -15")))

# Plot histograms for each column
asy_plot <- ggplot(data_to_plot, aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "purple") +
  facet_wrap(~ signal_adjusted, scales = "free_x", ncol = 4, 
             labeller = labeller(signal_adjusted = label_parsed)) +
  theme_bw() +
  scale_x_continuous(breaks = c(-10, 0, 10, 20)) +
  theme(
    strip.text = element_text(margin = margin(0.04, 0, 0.04, 0, "cm"),
                              size = 10),
    axis.text.x = element_text(size = 10),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),   # Remove y-axis text
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    panel.grid.major.y = element_blank(),  # Remove y-axis grid lines
    panel.grid.minor.y = element_blank(),  # Remove minor y-axis grid lines
    plot.title = element_blank()  # Margin below title to separate it from plot
  ) +
  labs(x = NULL, y = NULL, title = NULL)

# save the plot
# create figure directory
figures_dir <- "manuscript/figures-and-tables/auxiliary"
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir)
  cat("Directory created:", figures_dir, "\n")
} else {
  cat("Directory already exists:", figures_dir, "\n")
}
ggsave(sprintf("%s/sampling_distribution.pdf", figures_dir), plot = asy_plot, height = 2, width = 6)

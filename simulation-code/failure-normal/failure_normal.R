# This is a file demonstrating the usage of the functions in AdaReg package
library(AdaReg)
library(ggplot2)
library(dplyr)
library(patchwork)
library(katlabutils)

set.seed(1)

# number of replication
B_rep <- 2000

# parameter in epsilon-greedy algorithm
eps <- 0.05

# distribution family
dist_fam <- c("gaussian")

# number of sample
n <- 500

# initial sampling probability
initial_prob <- c(0.5, 0.5)

# create empty array
result_mat <- matrix(
  data = NA,
  nrow = B_rep, ncol = 1,
  dimnames =  list(realization = 1:B_rep,
                   signal = 0)
  
)

# true mean vector
dist_params <- data.frame(
  mu = c(2, 2),
  sigma = c(1, 3)
)
p <- 0
hyperparams <- list(family = "gaussian")

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
  AIPW_pt <- weighted_AIPW(data, hyperparams, reg_type = "MLE")
  point_estimate <- AIPW_pt["point", 1] - AIPW_pt["point", 2]
  sd_estimate <- sqrt(AIPW_pt["variance", 1] + AIPW_pt["variance", 2])
  
  ################################################################################
  # store the test statistic
  pvalue <- pnorm(point_estimate / sd_estimate)
  result_mat[cumulative_realization + 1, 1] <- pvalue
  
  # add the loop iteration
  cumulative_realization <- cumulative_realization + 1
}

# define the theme for plotting
my_theme <-   theme_bw() + 
  theme(
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

# check p_value
p_value <- result_mat[, 1]
qq_plot <- as_tibble(p_value) |>
  ggplot(mapping = aes(y = value)) +
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
       x = "Expected p-value",
       title = "") +
  my_theme +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12))

# Type-I error
Type_I_error <- data.frame(mean = rep(0, 2),
                           se = rep(0, 2),
                           method = c("0.05", "0.1"))
Type_I_error$mean <- c(mean(p_value <= 0.05), mean(p_value <= 0.1))
Type_I_error$se <- Type_I_error$mean * (1 - Type_I_error$mean) / sqrt(B_rep)


type_I_err_2 <- Type_I_error |>
  ggplot(aes(x = method,
             y = mean,
             ymin = mean - 2*se,
             ymax = mean + 2*se)) +
  geom_point() +
  geom_errorbar(width = 0.5) +
  geom_hline(yintercept=0.05, linetype=2, color = "red") +
  geom_hline(yintercept=0.1, linetype=2, color = "red") +
  labs(x = "Significance levels",
       y = "Type I Error") +
  my_theme + 
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12))

p <- qq_plot + type_I_err_2 

# create figure directory
figures_dir <- "manuscript/figures-and-tables/auxiliary"
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir)
  cat("Directory created:", figures_dir, "\n")
} else {
  cat("Directory already exists:", figures_dir, "\n")
}

# save the results
ggsave(filename = sprintf("%s/failure_Hadad.pdf", figures_dir), 
       plot = p, 
       width = 6, 
       height = 3)

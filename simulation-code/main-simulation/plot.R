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
distribution_list <- c("gaussian", "Bernoulli")
sideness <- c("left", "right")
eps_list <- c(0.1, 0.2, 0.4)
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
results_dir <- "simulation-code/main-simulation"

# specify the empty df
full_results_power <- data.frame(NULL)
full_results_pvalue <- data.frame(NULL)

# loop over distribution and then summariz the results
for (distribution in distribution_list) {
  
  # create parameter grid
  parameter_grid <- data.frame(
    signal = seq(-0.2, 0.2, length.out = 9)
  ) |> mutate(grid_id = 1:n())
  
  # create empty dataframe
  final_power <- data.frame(NULL)
  final_pvalue <- data.frame(NULL)
  
  # loop over epsilon
  for (eps in eps_list) {
    
    # load the results
    thompson_result_rds <- readRDS(sprintf("%s/%s/thompson_eps_%d_results.rds", results_dir, distribution, eps * 100))
    eps_result_rds <- readRDS(sprintf("%s/%s/eps_greedy_eps_%d_results.rds", results_dir, distribution, eps * 100))
    
    # combined two results
    thompson_result_rds <-  thompson_result_rds$results |> mutate(sampling = "thompson")
    eps_result_rds <- eps_result_rds$results |> mutate(sampling = "eps_greedy")
    result_rds <- rbind(thompson_result_rds, eps_result_rds)
    
    # rearrange the results to get pvalue
    pvalue_df <- result_rds |>
      unnest(output) |>
      mutate(
        p_value = sapply(output, function(x) unlist(x)),
        sideness = names(p_value)
      ) |>
      dplyr::select(-output) |>
      left_join(parameter_grid, by = "grid_id") |>
      dplyr::mutate(
        stat_form = case_when(
          method %in% c("Normalized_adaptive", "Normalized_constant") ~ "normalized",
          method %in% c("Unnormalized_adaptive", "Unnormalized_constant") ~ "unnormalized",
          TRUE ~ "sample-splitting"
        ),
        weighting = if_else(method %in% c("Normalized_adaptive", "Unnormalized_adaptive"),
                            "adaptive weighting", "constant weighting")) |>
      mutate(eps = eps)
    
    # extract the power
    power_df <- pvalue_df |>
      group_by(stat_form, weighting, signal, sideness, sampling) |>
      summarise(
        rejection = mean(p_value <= alpha)
      ) |>
      ungroup() |>
      mutate(
        eps = sprintf("epsilon == %.2f", eps)
      )
    
    # rbind the current power_df with final_result
    final_power <- rbind(final_power, power_df)
    final_pvalue <- rbind(final_pvalue, pvalue_df |> mutate(
      eps = sprintf("epsilon == %.2f", eps)
    ))
    
  }
  
  # rbind the results
  final_power <- final_power |> mutate(distribution = distribution)
  final_pvalue <- final_pvalue |> mutate(distribution = distribution)
  full_results_power <- rbind(full_results_power, final_power)
  full_results_pvalue <- rbind(full_results_pvalue, final_pvalue)
}

########################### plotting code ######################################

# loop over thompson versus eps_greedy
for (algorithm in c("thompson", "eps_greedy")){
  
  # create the lists
  rejection_plots_list <- list(
    left = NULL,
    right = NULL
  )
  qq_plots_list <- list(
    left = NULL,
    right = NULL
  )
  
  # loop over sideness
  for (side in sideness) {
    
    # signal list extraction
    signal_list <- unique(full_results_power$signal)
    if(side == "left"){
      cur_signal <- signal_list[signal_list <= 0]
    }else{
      cur_signal <- signal_list[signal_list >= 0]
    }
    
    # Custom labels
    custom_labels <- c("gaussian" = "Gaussian", "Bernoulli" = "Bernoulli")
    
########################### rejection plot ###################################
    
    # plot the rejection
    rejection_plot <- full_results_power |>
      filter(sampling == algorithm & sideness == side & signal %in% cur_signal) |>
      ggplot(aes(x = signal, y = rejection, color = stat_form, linetype = weighting)) +
      scale_y_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
      facet_grid(distribution~eps, scales = "fixed", 
                 labeller = labeller(
                   distribution = as_labeller(custom_labels), # Custom labels for 'distribution'
                   eps = label_parsed  # Parse 'eps' as Greek letters or mathematical expressions
                 )) +
      geom_point(size = 2) +
      geom_line(size = 1) +
      geom_hline(aes(yintercept = alpha), linetype = "dashed", color = "red") +
      theme_bw() +
      labs(y = sprintf("%s-sided test", 
                       if_else(side == "left", "Left", "Right")), 
           x = "", title = "") +
      theme(strip.text.x = element_text(margin = margin(0.04, 0, 0.04, 0, "cm"),
                                        size = 14),
            strip.text.y = element_text(margin = margin(0.04, 0, 0.04, 0, "cm"),
                                        size = 14),
            legend.position = "none",
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(size = 14),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            panel.spacing = unit(1.1, "lines"))
    
    # Extract the legends from the plots without background
    legend1 <- get_plot_component(
      ggplot(final_power |>
               filter(sampling == "thompson" & sideness == side & signal %in% cur_signal),
             aes(x = signal, y = rejection, linetype = weighting)) +
        geom_point(size = 1.5) +
        geom_line(size = 1) +
        theme(legend.position = "bottom",
              legend.key = element_blank(),  # Removes the background of legend keys
              legend.text = element_text(size = 14),   # Increase legend text font size
              legend.title = element_text(size = 16)  # Increase legend title font size
        ),'guide-box', return_all = TRUE)
    
    legend2 <- get_plot_component(
      ggplot(final_power |>
               filter(sampling == "thompson" & sideness == side & signal %in% cur_signal),
             aes(x = signal, y = rejection, color = stat_form)) +
        geom_point(size = 1.5) +
        geom_line(size = 1) +
        labs(color = "statistic format") +
        theme(legend.position = "bottom",
              legend.key = element_blank(),  # Removes the background of legend keys
              legend.text = element_text(size = 14),   # Increase legend text font size
              legend.title = element_text(size = 16)  # Increase legend title font size
        ),'guide-box', return_all = TRUE)
    
    # Combine everything
    rejection_plots_list[[side]] <- plot_grid(rejection_plot, ncol = 1, align = "v")
    
########################### QQ plot ############################################
    
    # extract null p-values
    pvalue_null <- full_results_pvalue |> filter(signal == 0)
    
    # check p_value
    qq_plot <- pvalue_null |>
      filter(sampling == algorithm & sideness == side) |>
      ggplot(mapping = aes(y = p_value, color = stat_form, shape = weighting)) +
      facet_grid(distribution~eps, scales = "fixed", 
                 labeller = labeller(
                   distribution = as_labeller(custom_labels), # Custom labels for 'distribution'
                   eps = label_parsed  # Parse 'eps' as Greek letters or mathematical expressions
                 )) +
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
      labs(y = sprintf("%s-sided test", 
                       if_else(side == "left", "Left", "Right")), 
           x = "", title = "") +
      my_theme +
      theme(strip.text.x = element_text(margin = margin(0.04, 0, 0.04, 0, "cm"),
                                        size = 14),
            strip.text.y = element_text(margin = margin(0.04, 0, 0.04, 0, "cm"),
                                        size = 14),
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(size = 14),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            panel.spacing = unit(1.1, "lines"))
    
    # Extract the legends from the plots without background
    legend1_update <- get_plot_component(
      ggplot(final_power |>
               filter(sampling == "thompson" & sideness == side & signal %in% cur_signal),
             aes(x = signal, y = rejection, shape = weighting)) +
        geom_point(size = 1.5) +
        geom_line(size = 1) +
        theme(legend.position = "bottom",
              legend.key = element_blank(),  # Removes the background of legend keys
              legend.text = element_text(size = 14),   # Increase legend text font size
              legend.title = element_text(size = 16)  # Increase legend title font size
        ),'guide-box', return_all = TRUE)
    
    # Combine everything
    qq_plots_list[[side]] <- plot_grid(qq_plot, ncol = 1, align = "v")
  }
  
############################# combine the rejection plots ######################
  
  # Create a shared Y-axis title
  y_title <- textGrob("Rejection rate", rot = 90, gp = gpar(fontsize = 14))
  
  # Create a shared X-axis title
  x_title <- textGrob("Signal strength", gp = gpar(fontsize = 14))
  
  # Combine the two legends into one
  combined_legend <- plot_grid(legend1[[3]], legend2[[3]], ncol = 1)
  
  # Add the X and Y axis titles to the combined plots
  plots_with_titles <- grid.arrange(
    arrangeGrob(
      plot_grid(
        rejection_plots_list[["left"]], 
        rejection_plots_list[["right"]], 
        ncol = 1, 
        rel_heights = c(2, 2) # Adjust plot proportions
      ),
      left = y_title,   # Add the Y-axis title
      bottom = x_title  # Add the X-axis title
    )
  )
  
  
  # Combine with the legend at the bottom
  final_plot <-   plot_grid(
    plots_with_titles,
    combined_legend,
    ncol = 1,
    rel_heights = c(8, 1) # Adjust as necessary
  )
  
  
  # save the plot
  ggsave(sprintf("%s/%s_rejection_plot.pdf", figures_dir, algorithm),
         final_plot, height = 9, width = 8)
  
############################# combine the QQ-plots #############################
  
  # Create a shared Y-axis title
  y_title <- textGrob("Observed p-value", rot = 90, gp = gpar(fontsize = 14))
  
  # Create a shared X-axis title
  x_title <- textGrob("Theoretical p-value", gp = gpar(fontsize = 14))
  
  # Combine the two legends into one
  combined_legend <- plot_grid(legend1_update[[3]], legend2[[3]], ncol = 1)
  
  # Add the X and Y axis titles to the combined plots
  plots_with_titles <- grid.arrange(
    arrangeGrob(
      plot_grid(
        qq_plots_list[["left"]], 
        qq_plots_list[["right"]], 
        ncol = 1, 
        rel_heights = c(2, 2) # Adjust plot proportions
      ),
      left = y_title,   # Add the Y-axis title
      bottom = x_title  # Add the X-axis title
    )
  )
  
  
  # Combine with the legend at the bottom
  final_plot <-   plot_grid(
    plots_with_titles,
    combined_legend,
    ncol = 1,
    rel_heights = c(8, 1) # Adjust as necessary
  )
  
  # save the plot
  ggsave(sprintf("%s/%s_qq_plot.pdf", figures_dir, algorithm),
         final_plot, height = 9, width = 8)
}

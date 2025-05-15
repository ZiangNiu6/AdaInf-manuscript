# load packages
library(AdaReg)
library(dplyr)
library(ggplot2)
library(zoo)
library(katlabutils)
set.seed(1)

# load preprocessed data
intermediate_data_dir <- "realdata-code/intermediate-data"
data_to_analyze <- readRDS(sprintf("%s/preprocessed_data.rds", intermediate_data_dir))

################################ calibration check #############################
B <- 5000
n_1 <- 1000
alpha <- 0.05
n_2 <- n_1
method_list <- c("SS", "UA", "UC", "NA", "NC")
eps_list <- c(0.1, 0.2, 0.4)
p_value_mat <- array(NA, dim = c(B, length(method_list), length(eps_list)),
                     dimnames = list(
                       reps = 1:B,
                       method = method_list,
                       eps = eps_list
                     ))
# loop over different permutations group
for (b in 1:B) {
  
  # work with whole group after permutation
  permuted_data <- data_to_analyze |> 
    mutate(outcome = sample(outcome)) 
  
  # loop over epsilon
  for (eps in eps_list) {
    
    # use first n_1 datapoints 
    prob_batch_1 <- c(0.5, 0.5)
    subset_data <- permuted_data[1:n_1, ] 
    remaining_data <- permuted_data[-(1:n_1), ]
    sample_batch_1 <- list(
      reward = subset_data$outcome,
      arm = subset_data$treatment,
      batch_id = rep(1, n_1),
      sampling_prob = rep(prob_batch_1, each = n_1 / 2)
    )
    
    # sample n_2 second batch data
    estimate_first_batch <- AdaReg::IPW(sample_batch_1, one_batch = TRUE, batch_output = 1)
    
    # use the sampling_funtion to decide the next batch sampling
    sampling_result <- sampling_function(eps = eps, reward = estimate_first_batch["point", ], 
                                         n = n_2, dist_fam = NULL, dist_params = NULL, 
                                         data_generation = FALSE,
                                         sample_size = NULL, type = "eps_greedy")
    
    # extract the outcome from the remaining data
    sample_id <- sampling_result$sample_id
    num_ctl <- length(which(sample_id == 2))
    num_trt <- n_2 - num_ctl
    trt_prob <- sampling_result$prob_estimate[1]
    ctl_prob <- sampling_result$prob_estimate[2]
    remaining_trt <- remaining_data |> filter(treatment == 2) |> dplyr::select(outcome) |> pull()
    remaining_ctl <- remaining_data |> filter(treatment == 1) |> dplyr::select(outcome) |> pull()
    
    # extract the data and merge it with first batch
    final_data <- list(
      reward = c(sample_batch_1$reward, remaining_trt[1:num_trt], remaining_ctl[1:num_ctl]),
      arm = c(sample_batch_1$arm, rep(2, num_trt), rep(1, num_ctl)),
      batch_id = c(rep(1, n_1), rep(2, n_2)),
      sampling_prob = c(sample_batch_1$sampling_prob, rep(trt_prob, num_trt), rep(ctl_prob, num_ctl))
    )
    
    # data as a list
    data <- list(
      data_to_analyze = final_data,
      eps = eps,
      initial_prob = prob_batch_1,
      type = "eps_greedy"
    )
    
    # consider three methods: sample-splitting, UA and UC
    ## sampling splitting 
    sample_spliting_result <- AdaReg::second_batch_only(data)
    p_value_mat[b, "SS", as.character(eps)] <- sample_spliting_result$right

    ## UA
    UA_result <- AdaReg::UA_test(data)
    p_value_mat[b, "UA", as.character(eps)] <- UA_result$right
    
    ## UC
    UC_result <- AdaReg::UC_test(data)
    p_value_mat[b, "UC", as.character(eps)] <- UC_result$right
    
    ## NA
    NA_result <- AdaReg::NA_test(data)
    p_value_mat[b, "NA", as.character(eps)] <- NA_result$right
    
    ## NC
    NC_result <- AdaReg::NC_test(data)
    p_value_mat[b, "NC", as.character(eps)] <- NC_result$right
    
  }
  
  # print the replication
  print(apply(p_value_mat[,,as.character(0.1)], 2, function(x) mean(x <= alpha, na.rm = TRUE)))
  
}

# create results directory
results_dir <- "realdata-code/results"
if (!dir.exists(results_dir)) {
  dir.create(results_dir)
  cat("Directory created:", results_dir, "\n")
} else {
  cat("Directory already exists:", results_dir, "\n")
}

# save the resutlt
saveRDS(p_value_mat, sprintf("%s/null_pvalue.rds", results_dir))

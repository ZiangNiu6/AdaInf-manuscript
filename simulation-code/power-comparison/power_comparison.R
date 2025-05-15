suppressMessages(suppressWarnings(library(simulatr)))
suppressMessages(suppressWarnings(library(AdaReg)))
suppressMessages(suppressWarnings(library(dplyr)))

# Read the command-line arguments
args <- commandArgs(trailingOnly = TRUE)

# Assign the arguments to variables in R
distribution <- args[1]
signal <- as.numeric(args[2])
output_dir <- args[3]

#####################
# 1. Preparing parameter
#####################

# specify the parameter grid
get_ground_truth <- function(n){
  return(NULL)
}

parameter_grid <- expand.grid(
  signal = signal
) |> add_ground_truth(get_ground_truth) |>
  mutate(grid_id = 1:n())

#######################
# 2. Fixed parameters
#######################
fixed_parameters <- list(
  n = 2e4,                                              # fixed sample size
  B = 1e4,                                              # number of replication
  seed = 1,                                             # seed to set prior to generating data and running methods
  initial_prob = c(0.5, 0.5),                           # First stage sampling probability
  eps = 0.2,                                            # epsilon parameter
  dist_fam = distribution                               # distribution family
)

###################
# 3. Generate data
###################

# define data-generating model assuming only one enhancer is involved and balanced experimental setup
generate_data_nb <- function(n, initial_prob, eps, dist_fam, signal){
  
  # switch between different distributions
  switch (distribution,
    Bernoulli = {
      
      # true mean vector
      center <- 0.5
      true_mean <- center + c(signal, 0)
      dist_params <- data.frame(mean = true_mean)
    },
    mix_normal = {
      
      # true mean vector
      center <- 0
      dist_params <- list(
        mu = cbind(signal + c(-1, 1), c(-1, 1)),
        sd = cbind(c(0.5, .5), c(0.5, .5)),
        prob = cbind(c(0.5, .5), c(0.5, .5))
      )
    },
    gaussian = {
      
      # true mean vector
      center <- 0
      true_mean <- c(signal, 0)
      dist_params <- data.frame(
        mu = true_mean,
        sd = c(1, .5)
      )
    },
    Student = {
      
      # true mean vector
      center <- 0
      true_mean <- c(signal, 0)
      dist_params <- data.frame(
        mu = true_mean,
        df = c(4, 10)
      )
    },
    Poisson = {
      
      # true mean vector
      center <- 1
      true_mean <- center + c(signal, 0)
      dist_params <- data.frame(lambda = true_mean)
    }
  )

  
  # generate data
  data <- data_generate_online(eps = eps, dist_fam = dist_fam, n_1 = n / 2, 
                               n_2 = n / 2, p = 0, deciding_scheme = "IPW",
                               dist_params = dist_params, 
                               initial_prob = initial_prob, type = "thompson")
  
  # generate centered data 
  data$reward <- data$reward - center
  
  # return the output
  return(list(
    data_to_analyze = data,
    eps = eps,
    initial_prob = initial_prob,
    type = "thompson"
  ))
}


generate_data_function <- simulatr_function(
  f = generate_data_nb,
  arg_names = formalArgs(generate_data_nb),
  loop = TRUE
)

######################
# 4. Method functions
######################


# UA test
UA_test_spec_f <- simulatr_function(f = function(data){
  AdaReg::UA_test(data)
}, arg_names = character(0), loop = T)

# NA test
NA_test_spec_f <- simulatr_function(f = function(data){
  AdaReg::NA_test(data)
}, arg_names = character(0), loop = T)

# unnormalized DM test
UDM_test_spec_f <- simulatr_function(f = function(data){
  AdaReg::UDM_test(data)
}, arg_names = character(0), loop = T)

# normalized DM test
NDM_test_spec_f <- simulatr_function(f = function(data){
  AdaReg::NDM_test(data)
}, arg_names = character(0), loop = T)

# aggregate all the methods functions
run_method_functions <- list(Unnormalized_adaptive = UA_test_spec_f,
                             Normalized_adaptive = NA_test_spec_f,
                             Unnormalized_dim = UDM_test_spec_f,
                             Normalized_dim = NDM_test_spec_f)

# ##########################
# # 5. Check simulation
# ##########################

simulatr_spec <- simulatr_specifier(
  parameter_grid,
  fixed_parameters,
  generate_data_function,
  run_method_functions
)

B_in = 2000
results <- check_simulatr_specifier_object(simulatr_spec,
                                           B_in = B_in, return_data = FALSE,
                                           parallel = FALSE)

# save the results
saveRDS(results, sprintf("%s/%s_%.2f_power_comparison.rds", output_dir, distribution, signal))


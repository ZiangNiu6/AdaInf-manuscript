suppressMessages(suppressWarnings(library(simulatr)))
suppressMessages(suppressWarnings(library(AdaReg)))
suppressMessages(suppressWarnings(library(dplyr)))

# Read the command-line arguments
args <- commandArgs(trailingOnly = TRUE)

# Assign the arguments to variables in R
eps <- as.numeric(args[1])
sampling <- args[2]
output_dir <- args[3]

#####################
# 1. Preparing parameter
#####################

# specify the parameter grid
get_ground_truth <- function(n){
  return(NULL)
}

parameter_grid <- data.frame(
  signal = seq(-0.2, 0.2, length.out = 9)
) |> add_ground_truth(get_ground_truth) |>
  mutate(grid_id = 1:n())

#######################
# 2. Fixed parameters
#######################
fixed_parameters <- list(
  sampling = sampling,                                  # sampling function choice
  B = 1e4,                                              # number of replication
  seed = 1,                                             # seed to set prior to generating data and running methods
  n = 1000,                                             # number of total sample in both batches
  initial_prob = c(0.5, 0.5),                           # First stage sampling probability
  eps = if_else(sampling == "thompson", eps / 2, eps),  # epsilon parameter
  dist_fam = c("Bernoulli")                             # distribution family
)

###################
# 3. Generate data
###################

# define data-generating model assuming only one enhancer is involved and balanced experimental setup
generate_data_nb <- function(n, initial_prob, eps, dist_fam,
                             signal, sampling){
  
  # true mean vector
  true_mean <- 0.5 + c(signal, 0)
  dist_params <- data.frame(
    mu = true_mean
  )
  
  # generate data
  data <- data_generate_online(eps = eps, dist_fam = dist_fam, n_1 = n / 2, 
                               n_2 = n / 2, p = 0, deciding_scheme = "IPW",
                               dist_params = dist_params, 
                               initial_prob = initial_prob, type = sampling)
  
  # return the output
  return(list(
    data_to_analyze = data,
    eps = eps,
    initial_prob = initial_prob,
    type = sampling
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

# IPW test based on sample splitting
sample_splitting_spec_f <- simulatr_function(f = function(data){
  AdaReg::second_batch_only(data)
}, arg_names = character(0), loop = T)

# IPW test with normal limit
IPW_normal_spec_f <- simulatr_function(f = function(data){
  AdaReg::normal_test(data)
}, arg_names = character(0), loop = T)

# NC test
NC_test_spec_f <- simulatr_function(f = function(data){
  AdaReg::NC_test(data)
}, arg_names = character(0), loop = T)

# NA test
NA_test_spec_f <- simulatr_function(f = function(data){
  AdaReg::NA_test(data)
}, arg_names = character(0), loop = T)

# UC test
UC_test_spec_f <- simulatr_function(f = function(data){
  AdaReg::UC_test(data)
}, arg_names = character(0), loop = T)

# UA test
UA_test_spec_f <- simulatr_function(f = function(data){
  AdaReg::UA_test(data)
}, arg_names = character(0), loop = T)

run_method_functions <- list(sample_splitting = sample_splitting_spec_f,
                             Normalized_constant = NC_test_spec_f,
                             Normalized_adaptive = NA_test_spec_f,
                             Unnormalized_constant = UC_test_spec_f,
                             Unnormalized_adaptive = UA_test_spec_f)

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
saveRDS(results, sprintf("%s/%s_eps_%d_results.rds", output_dir, sampling, eps * 100))

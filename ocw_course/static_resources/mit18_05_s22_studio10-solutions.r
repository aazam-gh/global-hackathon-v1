#---------------------------------------------------------
# File:   mit18_05_s22_studio10-solutions.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 10 solutions ----

# 1. Be sure to read the instructions file!

# 2. The test answers file gives sample output and the functions we ran to get it. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# **** matrixStats package *****
# You should used the Package tab in R Studio to install matrixStats.
library('matrixStats')

#-------------------------#
# Utility functions ----
# Rather than copying and pasting code, we make use of some utility functions

is_type1_error = function(trueval, ci) {
  if (trueval < ci[1] || trueval > ci[2]) {
    return(1)
  }
  else {
    return(0)
  }
}

#----------------#
# Rather than write the same code twice, we make a function that we can run for both problems. In fact we write two different solutions

# First solution: Use the package matrixstats for speed
run_bootstrap_matrixStats = function(x, n_boot, confidence) {
  # Arguments:
  # x = random data
  # n_boot = number of bootstrap samples to use
  # confidence = desired bootstrap confidence

  # Return value: list of 12 values indicating 6 confidence intervals
  # c(mean_percentile_ci, median_percentile_ci, sd_percentile_ci,
  #   mean_basic_ci, median_basic_ci, sd_basic_ci)
  # Each of the ci consists of two values: c(lower_bound, upper_bound)

  n_data = length(x)
  alpha = (1.0 - confidence)

  # Compute the desired sample statistics
  mean_hat = mean(x)
  median_hat = median(x)
  sd_hat = sd(x)

  # Resample n_boot times and compute resampled stats
  # In order to use the matrixStats functions, we generate all the
  # resamples at once and store in a matrix.
  xstar = sample(x, n_data*n_boot, replace=TRUE)
  xstar = matrix(xstar, nrow=n_data, ncol=n_boot)

  # Compute the bootstrap stats
  mean_star = colMeans2(xstar)
  median_star = colMedians(xstar)
  sd_star = colSds(xstar)

  # Find percentile CIs
  p = c(alpha/2, 1-alpha/2)

  mean_percentile_ci = quantile(mean_star, p)
  median_percentile_ci = quantile(median_star, p)
  sd_percentile_ci = quantile(sd_star, p)

  # Basic confidence intervals
  p = c(1-alpha/2, alpha/2) # reverse order for pivot

  mean_delta_star = mean_star - mean_hat
  mean_basic_ci = mean_hat - quantile(mean_delta_star, p)
  median_delta_star = median_star - median_hat
  median_basic_ci = median_hat - quantile(median_delta_star, p)
  sd_delta_star = sd_star - sd_hat
  sd_basic_ci = sd_hat - quantile(sd_delta_star, p)

  return(c(mean_percentile_ci, median_percentile_ci, sd_percentile_ci, mean_basic_ci, median_basic_ci, sd_basic_ci))
}

# Second solution: Use a for loop
run_bootstrap_loop = function(x, n_boot, confidence) {

  # Arguments:
  # x = random data
  # n_boot = number of bootstrap samples to use
  # confidence = desired bootstrap confidence

  # Return value: list of 12 values indicating 6 confidence intervals
  # c(mean_percentile_ci, median_percentile_ci, sd_percentile_ci,
  #   mean_basic_ci, median_basic_ci, sd_basic_ci)
  # Each of the ci consists of two values: c(lower_bound, upper_bound)

  n_data = length(x)
  alpha = (1.0 - confidence)

  # Compute the desired sample statistics
  mean_hat = mean(x)
  median_hat = median(x)
  sd_hat = sd(x)

  # Resample nboot times and compute resampled stats
  mean_star = rep(0,n_boot)
  median_star = rep(0,n_boot)
  sd_star = rep(0,n_boot)

  for (j in 1:n_boot) {
    xstar = sample(x, n_data, replace=TRUE)

    # Compute the bootstarp stats
    mean_star[j] = mean(xstar)
    median_star[j] = median(xstar)
    sd_star[j] = sd(xstar)
  }

  # Find percentile CIs
  p = c(alpha/2, 1-alpha/2)

  mean_percentile_ci = quantile(mean_star, p)
  median_percentile_ci = quantile(median_star, p)
  sd_percentile_ci = quantile(sd_star, p)

  # Basic confidence intervals
  p = c(1-alpha/2, alpha/2) # reverse order for pivot

  mean_delta_star = mean_star - mean_hat
  mean_basic_ci = mean_hat - quantile(mean_delta_star, p)
  median_delta_star = median_star - median_hat
  median_basic_ci = median_hat - quantile(median_delta_star, p)
  sd_delta_star = sd_star - sd_hat
  sd_basic_ci = sd_hat - quantile(sd_delta_star, p)

  return(c(mean_percentile_ci, median_percentile_ci, sd_percentile_ci, mean_basic_ci, median_basic_ci, sd_basic_ci))
}

### Here we pick one of the run_bootstrap functions. In trials on my machine, matrixStats runs about twice as fast.
run_bootstrap = run_bootstrap_matrixStats
print_progress = TRUE

#--------------------------------------
# Problem 1: Simulated type 1 bootstap CI error rate for normal data
# See instructions for this studio

studio10_problem_1 = function(true_mean, true_sd, n_data, n_boot, n_trials, confidence) {
  cat("\n----------------------------------\n")
  cat("Problem 1: Simulated type 1 empirical bootstrap CI error rate: normal distribution\n")

  # Arguments:
  # true_mean = the mean of the normal distribution used to generate the data
  # true_sd = the standard deviation of the normal distribution used to generate the data
  # n_data = number of values in one sample. (Original generated using a normal distribution.)
  # n_boot = number of bootstrap samples to use in each trial
  # n_trials = number of trials to run in simulation
  # confidence = the bootstrap confidence level


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  true_median = true_mean # For normal, median = mean.

  # counts of type1 errors
  mean_percentile_type1 = 0
  median_percentile_type1 = 0
  sd_percentile_type1 = 0
  mean_basic_type1 = 0
  median_basic_type1 = 0
  sd_basic_type1 = 0

  for (i in 1:n_trials) {
    if (print_progress && i %% 50 == 0) {
      cat(i,'')
    }

    #Generate data
    data = rnorm(n_data, true_mean, true_sd)
    cis = run_bootstrap(data, n_boot, confidence)

    # FUTURE: The code below could be packed into a function so it doesn't have to be duplicated in problem 2
    mean_percentile_ci = cis[1:2]
    median_percentile_ci = cis[3:4]
    sd_percentile_ci = cis[5:6]
    mean_basic_ci = cis[7:8]
    median_basic_ci = cis[9:10]
    sd_basic_ci = cis[11:12]

    err = is_type1_error(true_mean, mean_percentile_ci)
    mean_percentile_type1 = mean_percentile_type1 + err

    err = is_type1_error(true_median, median_percentile_ci)
    median_percentile_type1 = median_percentile_type1 + err

    err = is_type1_error(true_sd, sd_percentile_ci)
    sd_percentile_type1 = sd_percentile_type1 + err

    err = is_type1_error(true_mean, mean_basic_ci)
    mean_basic_type1 = mean_basic_type1 + err

    err = is_type1_error(true_median, median_basic_ci)
    median_basic_type1 = median_basic_type1 + err

    err = is_type1_error(true_sd, sd_basic_ci)
    sd_basic_type1 = sd_basic_type1 + err
  }

  if (print_progress) {
    cat('\n')
  }

  cat('Normal:', 'true mean =', true_mean, 'true_median =', true_median ,'true_sd =', true_sd, 'n_data =',n_data, 'n_boot =', n_boot, 'n_trials =', n_trials, 'confidence =', confidence, '\n')

  cat('Nominal confidence:', confidence, '\n')
  cat('Type 1 error rates (percentile, basic):\n')
  cat('  mean:', mean_percentile_type1/n_trials, mean_basic_type1/n_trials, '\n')
  cat('  median:', median_percentile_type1/n_trials, median_basic_type1/n_trials, '\n')
  cat('  sd:', sd_percentile_type1/n_trials, sd_basic_type1/n_trials, '\n')
}

#--------------------------------------
# Problem 2: Simulated type 1 bootstap CI error rate for log normal data
# See instructions for this studio

studio10_problem_2a = function(meanlog, sdlog) {
  cat("\n----------------------------------\n")
  cat("Problem 2a: Print true mean and true std of the log normal distribution\n")

  # Arguments:
  # meanlog = value of meanlog parameter in rlnorm
  # sdlog = value of sdlog parameter in rlnorm

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  true_mean = exp(meanlog + sdlog^2/2)
  true_median = exp(meanlog)
  true_sd = sqrt((exp(sdlog^2)-1)*exp(2*meanlog+sdlog^2))

  s = sprintf('LogNormal: meanlog=%.1f, sdlog=%.1f', meanlog, sdlog)
  cat(s,'\n')
  s = sprintf('Distribution mean=%.2f, median=%.2f, std dev=%.2f', true_mean, true_median, true_sd)
  cat(s,'\n')
}

studio10_problem_2b = function(meanlog, sdlog, n_data, n_boot, n_trials, confidence) {
  cat("-----\n")
  cat("Problem 2b: Simulated type 1 empirical bootstrap CI error rate: log normal distribution\n")

  # Arguments:
  # meanlog = value of meanlog parameter in rlnorm
  # sdlog = value of sdlog parameter in rlnorm
  # n_data = number of values in one sample. (Original generated using a log normal distribution.)
  # n_boot = number of bootstrap samples to use in each trial
  # n_trials = number of trials to run in simulation
  # confidence = the bootstrap confidence level


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  # The code here is identical to problem 1, except for the use of rlnorm instead of rnorm

  # Copied from 2a
  true_mean = exp(meanlog + sdlog^2/2)
  true_median = exp(meanlog)
  true_sd = sqrt((exp(sdlog^2)-1)*exp(2*meanlog+sdlog^2))

  # counts of type1 errors
  mean_percentile_type1 = 0
  median_percentile_type1 = 0
  sd_percentile_type1 = 0
  mean_basic_type1 = 0
  median_basic_type1 = 0
  sd_basic_type1 = 0

  for (i in 1:n_trials) {
    if (print_progress && i %% 50 == 0) {
      cat(i,'')
    }

    #Generate data
    data = rlnorm(n_data, meanlog=meanlog, sdlog=sdlog)
    cis = run_bootstrap(data, n_boot, confidence)

    # FUTURE: The code below could be packed into a function so it doesn't have to be duplicated in problem 2
    mean_percentile_ci = cis[1:2]
    median_percentile_ci = cis[3:4]
    sd_percentile_ci = cis[5:6]
    mean_basic_ci = cis[7:8]
    median_basic_ci = cis[9:10]
    sd_basic_ci = cis[11:12]

    err = is_type1_error(true_mean, mean_percentile_ci)
    mean_percentile_type1 = mean_percentile_type1 + err

    err = is_type1_error(true_median, median_percentile_ci)
    median_percentile_type1 = median_percentile_type1 + err

    err = is_type1_error(true_sd, sd_percentile_ci)
    sd_percentile_type1 = sd_percentile_type1 + err

    err = is_type1_error(true_mean, mean_basic_ci)
    mean_basic_type1 = mean_basic_type1 + err

    err = is_type1_error(true_median, median_basic_ci)
    median_basic_type1 = median_basic_type1 + err

    err = is_type1_error(true_sd, sd_basic_ci)
    sd_basic_type1 = sd_basic_type1 + err
  }

  if (print_progress) {
    cat('\n')
  }

  s = sprintf('LogNormal: meanlog=%.1f, sdlog=%.1f', meanlog, sdlog)
  cat(s,'\n')
  s = sprintf('Distribution mean=%.2f, median=%.2f, std dev=%.2f', true_mean, true_median, true_sd)
  cat(s,'\n')

  cat('n_data =',n_data, 'n_boot =', n_boot, 'n_trials =', n_trials,'\n')

  cat('Nominal confidence:', confidence, '\n')
  cat('Type 1 error rates (percentile, basic):\n')
  cat('  mean:', mean_percentile_type1/n_trials, mean_basic_type1/n_trials, '\n')
  cat('  median:', median_percentile_type1/n_trials, median_basic_type1/n_trials, '\n')
  cat('  sd:', sd_percentile_type1/n_trials, sd_basic_type1/n_trials, '\n')
}

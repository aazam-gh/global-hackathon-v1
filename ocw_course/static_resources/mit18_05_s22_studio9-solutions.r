#---------------------------------------------------------
# File:   mit18_05_s22_studio9-solutions.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 9 solutions ----

# 1. Be sure to read the instructions file!

# 2. The test answers file gives sample output and the functions we ran to get it. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# Problem 1: Simulated confidence intervals for normal data
# See instructions for this studio

# Problem 1a: Simulated type 1 CI error rate for z-confidence intervals
studio9_problem_1a = function(theta_vals, theta_prior, sigma, n_data, confidence, n_trials) { 
  cat("\n----------------------------------\n")
  cat("Problem 1a: Simulated type 1 CI error rate for z-confidence intervals \n")

  # Arguments:
  #  theta_vals = possible values for the mean of the normal distribution
  #  theta_prior = probabilities for choosing a theta from theta_vals
  #  sigma = standard deviation of the normal distribution
  #  n_data = the number of data values in each trial
  #  confidence = the confidence level, e.g. 0.95, 0.9 etc
  #  n_trials = number of trials in the simulation


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  alpha = 1 - confidence

  # Compute z margin of error
  z_alpha2 = qnorm(1-alpha/2,0,1)
  margin_of_error = z_alpha2*sigma/sqrt(n_data)

  # Run ntrials trials
  n_type1_ci_errors = 0
  for (j in 1:n_trials) {
    # Choose theta using the given prior
    theta = sample(theta_vals,1, prob=theta_prior)

    # Generate data
    x_data = rnorm(n_data, theta, sigma)

    # Compute mean
    xbar = mean(x_data)

    # Check for type 1 error: is xbar outside the margin of error from the true theta.
    if (abs(xbar - theta) > margin_of_error) {
      n_type1_ci_errors = n_type1_ci_errors + 1
    }
  }
  
  last_ci = c(xbar-margin_of_error, xbar + margin_of_error)
  
  # Print the last confidence interval and the type 1 ci error rate
  cat('Last confidence interval: [', last_ci[1], ',', last_ci[2], ']\n')

  cat('Type 1 CI-error rate:', n_type1_ci_errors/n_trials, '\n')
}


# Problem 1b: Simulated type 1 CI error rate for t-confidence intervals
studio9_problem_1b = function(theta_vals, theta_prior, sigma, n_data, confidence, n_trials) { 
  cat("\n----------------------------------\n")
  cat("Problem 1b: Simulated type 1 CI-error rate for t-confidence intervals \n")

  # Arguments:
  #  theta_vals = possible values for the mean of the normal distribution
  #  theta_prior = probabilities for choosing a theta from theta_vals
  #  sigma = standard deviation of the normal distribution
  #  n_data = the number of data values in each trial
  #  confidence = the confidence level, e.g. 0.95, 0.9 etc
  #  n_trials = number of trials in the simulation

  # Remember: in this problem, you use sigma to generate the data, but not in computing the confidence interval.
  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  alpha = 1 - confidence

  # Compute t critical value
  t_alpha2 = qt(1-alpha/2, n_data-1)

  # Run ntrials trials
  n_type1_ci_errors = 0
  for (j in 1:n_trials) {
    # Choose theta using the given prior
      theta = sample(theta_vals,1, prob=theta_prior)

    # Generate data
    x_data = rnorm(n_data, theta, sigma)

    # Compute sample mean and standard error  
    xbar = mean(x_data)
    s = sqrt(var(x_data))

    # The margin of error depends on s, so is freshly computed in each trial
    margin_of_error = t_alpha2*s/sqrt(n_data)

    # Check for a type 1 error
    if (abs(xbar - theta) > margin_of_error) {
      n_type1_ci_errors = n_type1_ci_errors + 1
    }
  }
  last_ci = c(xbar-margin_of_error, xbar + margin_of_error)
  
  # Print the last confidence interval and the type 1 ci error rate
  cat('Last confidence interval: [', last_ci[1], ',', last_ci[2], ']\n')
  
  cat('Type 1 CI-error rate:', n_type1_ci_errors/n_trials, '\n')
}


# Problem 1c: Bayesian updating and probability of hypotheses
studio9_problem_1c = function(theta_vals, theta_prior, sigma, n_data, confidence, xbar) { 
  cat("\n----------------------------------\n")
  cat("Problem 1c: Bayesian updating and probability of hypotheses\n")

  # Arguments:
  #  theta_vals = possible values for the mean of the normal distribution
  #  theta_prior = probabilities for choosing a theta from theta_vals
  #  sigma = standard deviation of the normal distribution
  #  n_data = the number of data values in each trial
  #  confidence = the confidence level, e.g. 0.95, 0.9 etc
  #  xbar = the mean of the data

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  alpha = 1 - confidence
  
  # Find z critical value and margin of error
  z_alpha2 = qnorm(1-alpha/2, 0, 1)
  margin_of_error = z_alpha2*sigma/sqrt(n_data)

  # (i) Bayesian updating
  prior = theta_prior
  likelihood = dnorm(xbar, theta_vals, sigma/sqrt(n_data))
  bayes_numerator = prior*likelihood
  theta_posterior = bayes_numerator/sum(bayes_numerator)
  cat('1c(i) theta_prior', theta_prior, '\n')
  cat('1c(i) theta_posterior', theta_posterior, '\n')

  # (ii) Compute and print the confidence interval
  z_conf_interval = c(xbar-margin_of_error, xbar+margin_of_error)
  cat('1c(ii)', confidence ,'z confidence interval:', '[', z_conf_interval[1], ',', z_conf_interval[2], ']','\n')


  # (iii) Find the probability theta is in the given confidence interval
  a = abs(xbar - theta_vals) < margin_of_error
  prior_prob = sum(theta_prior[a])
  post_prob = sum(theta_posterior[a])
  cat('1c(iii) prior prob. theta is in the CI:', prior_prob, '\n')
  cat('1c(iii) posterior prob. theta is in the CI:', post_prob, '\n')
}


#---------------------------
# OPTIONAL Problem 2: Simulated polling confidence intervals
# See instructions for this studio

studio9_problem_2 = function(true_theta, n) { 
  cat("\n----------------------------------\n")
  cat("Problem 2: Simulated polling confidence interval\n")

  # Arguments
  # true_theta = true proportion who prefer Lincoln. Use this to generate your simulated polling data
  # n = size of the sample

  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
    
  data = rbinom(n, 1, true_theta)
  estimated_theta = mean(data)
  margin_of_error = 1/sqrt(n)
  
  # Print the confidence interval
  cat('Confidence interval: ', estimated_theta, 'plus or minus', margin_of_error, '\n')
}

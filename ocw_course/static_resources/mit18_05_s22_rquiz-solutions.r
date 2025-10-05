#---------------------------------------------------------
# File:   mit18_05_s22_RQuiz-solutions.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# R Quiz Solutions

# READ THE INSTRUCTIONS PDF!
# * Read the problems carefully
# * Save your work frequently
# * You may use any resources except another person
#    (This includes, paper, books, code on your computer,
#    code on the internet ...)
# * If code is given with a question it is meant to be used. Don't just ignore it.
# * Remember to use print or cat statements to print the values asked for
# * Before uploading the code: clear your environment and source the entire file (choose source from the code menu)
#   -- Make sure that it runs without error and outputs just the answers asked for in the questions.

#-------------------------------
# 0. Run this to clean the environment

rm(list=ls())  #clear environment
cat('\014') #clear RStudio console

#--------------------------------
# Problem 1 (20: 5, 5, 10 points)
# See the instructions pdf

rquiz_problem_1a = function(mu, sigma, w_shape, w_scale, a, b) {
  cat("----------------------------------\n")
  cat("Problem 1a (5 points): Graphs\n")

  # Arguments:
  # mu = mean of the normal pdf to plot
  # sigma = standard deviation of the normal pdf to plot
  # w_shape = shape parameter for Weibull pdf
  # w_scale = scale parameter for Weibull pdf
  # a, b = endpoints of the range of x for the plot


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  lwd = 2
  x = seq(a, b, 0.001)
  y1 = dnorm(x, mu, sigma)
  y2 = dweibull(x, shape=w_shape, scale=w_scale)
  m = max(y1,y2)
  plot(c(a,b), c(0,m), type='n', xlab='x', ylab='')
  lines(x, y1, col='orange', lwd=lwd)
  lines(x, y2, col='blue', lwd=lwd)
  title('Problem 1a: various graphs')

  cat('See plots\n')
}

rquiz_problem_1b = function(n, k, m) {
  cat("-----\n")
  cat("Problem 1b (5 points): Combinations and factorials\n")

  # Arguments:
  # n = see instructions below
  # k = see instructions below
  # m = see instructions below
  
  # The problem asks you to print out n choose k, m factorial, log of n choose k.

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
  
  x1 = choose(n, k)
  cat('choose(', n, ', ', k, ') = ', x1, '\n', sep='')

  x2 = factorial(m)
  cat(m, '! = ', x2, '\n', sep='')
  
  x3 = lchoose(n, k)
  cat('lchoose(', n, ', ', k, ') = ', x3, '\n', sep='')
}

rquiz_problem_1c = function(theta_values, num_patients, num_cured) {
  cat("-----\n")
  cat("Problem 1c (10 points): Bayesian coins \n")

  # Arguments:
  # theta_values = List of possible values of theta.
  # num_patients = The number of patients in the trial.
  # num_cured = The number of successes in the trial.
  

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  # problem 1c(i)
  likelihoods = dbinom(num_cured, num_patients, theta_values)
  max_ind = which.max(likelihoods)
  mle = theta_values[max_ind]
  cat('1c(i) The MLE is theta =', mle, '\n')


  # problem 1c(ii) ----
  prior = rep(1,length(theta_values))/length(theta_values)

  # likelihoods = prob of 1 success in 1 patient for various theta
  likelihoods = dbinom(1, 1, theta_values)
  total_prob = sum(prior*likelihoods)
  cat('1c(ii) Prior predictive probability of success =', total_prob,'\n')

  # problem 1c(iii) ----
  likelihoods_data = dbinom(num_cured, num_patients, theta_values)
  bayse_numerator = likelihoods_data*prior
  posterior = bayse_numerator/sum(bayse_numerator)
  tbl = cbind(theta_values, posterior) #so it prints nicely
  cat('1c(iii) Posterior probabilities for theta\n')
  print(tbl, digits=7)
}

#--------------------------------
# Problem 2 (20 10:10 points) Histograms
# See the instructions pdf

rquiz_problem_2a = function(n_draws, k, bin_width) {
  cat("----------------------------------\n")
  cat("Problem 2a (10 points): Density histograms\n")

  # Arguments:
  # n_draws = Number of sample points in the histogram
  # k = Number of degrees of freedom for the chi-square distribution
  # bin_width = Bin width for histogram
  

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  mylightorange = rgb(1, 0.85, 0.41) # Just because

  sim_data = rchisq(n_draws, k)
  breaks = seq(0, max(sim_data)+bin_width, bin_width)
  hist(sim_data, breaks=breaks, freq=FALSE, col=mylightorange, main='')
  x = seq(0, max(sim_data), 0.01)
  y = dchisq(x, k)
  lines(x, y, col='blue', lwd=2.2)
  title('Problem 2a: chi square data')
  
  cat('See plots\n')
}

rquiz_problem_2b = function(n_trials, n_draws, k, bin_width) {
  cat("-----\n")
  cat("Problem 2b (10 points): Density histograms\n")

  # Arguments:
  # n_trials = Number of trials
  # n_draws = Number of sample points in each trial
  # k = Number of degrees of freedom for the chi-square distribution
  # bin_width = Bin width for histogram
  

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  mylightorange = rgb(1, 0.85, 0.41) # Just because
  
  sample_means = rep(NA, n_trials)
  for (j in 1:n_trials) {
    data = rchisq(n_draws, k)
    sample_means[j] = mean(data)
  }
  mu = k
  sigma = sqrt(2*k)
  standardized_means = (sample_means - mu)/(sigma/sqrt(n_draws))
  minm = min(standardized_means)
  maxm = max(standardized_means)
  breaks = seq(minm, maxm+bin_width, bin_width)
  hist(standardized_means, breaks=breaks, freq=FALSE, col=mylightorange, main='')
  z = seq(minm, maxm, 0.01)
  lines(z, dnorm(z), col='blue', lwd=2.2)
  title('Problem 2b: standardized data')

  cat('See plots\n')
}

#--------------------------------
# Problem 3 (10  points)
# See the instructions pdf

rquiz_problem_3 = function(our_data, alpha) {
  cat("----------------------------------\n")
  cat("Problem 3 (10 points): \n")

  # Arguments:
  # our_data = data from some experiment
  # alpha = significance level for the Shapiro-Wilk test

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********


  # The key is to figure out that we need shapiro.test()
  res = shapiro.test(our_data)
  p_value = res$p.value
  cat('The null hypothesis H0 is that the data is from a normal distribution.', '\n')
  cat('p_value =', p_value, '\n')
  if (p_value > alpha) {
    cat('Do not reject H0. ','\n')
  } else {
    cat('Reject H0 in favor of HA that the data is not normal','\n')
  }
}

#--------------------------------
# Problem 4 (Extra credit 5 points)
# See the instructions pdf

rquiz_problem_4 = function(data_file_name, alpha) {
  cat("----------------------------------\n")
  cat("Problem 4 (Extra credit: 5 points): \n")


  # Arguments:
  # data_file_name} = data file
  # alpha = Significance level for t-test

  # Read the data from the file
  data = read.table(data_file_name)
  x = data$x
  y = data$y

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

    res = t.test(x, y, alternative='two.sided', conf.level=1-alpha, var.equal=FALSE)
    p = res$p.value
    cat('p value =', p,'\n')
    if (p > alpha) {
        cat('Do not reject H0 that the means are the same','\n')
    } else {
        cat('Reject H0 in favor of HA that the means are different','\n')
    }
}

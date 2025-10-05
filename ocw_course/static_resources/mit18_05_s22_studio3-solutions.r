#---------------------------------------------------------
# File:   mit18_05_s22_studio3-solutions.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 3 solutions ----

# 1. Be sure to read the instructions file!

# 2. The instruction file gives instructions for testing your code. You can see the output of the tests in studio3-test-answers.html. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# Problem 1: Histograms. ----
# See the problem 1 instructions for this studio.
#
# Summary: You will make frequency and density histograms.
# 
# 1a. Frequency histogram 
# See the problem 1a instructions for this studio.
# Summary: Make a frequency histogram of simulated exponential data
# See the test answers file for the output of test calls to this function
studio3_problem_1a = function(rate, nsamples) {
  cat("\n----------------------------------\n")
  cat("1a. Frequency histogram\n")
  
  # Arguments:
  #   rate = rate parameter for an exponential distribution.
  #   nsamples = the size of the sample to use for the histogram

  # We will give you the bin_width to use in the histogram
  bin_width = 0.5

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

  data = rexp(nsamples, rate)
  bins = seq(min(data), max(data)+bin_width, bin_width)
  hist(data, breaks=bins, col='yellow', freq=TRUE)
  
  cat('See plot\n')
}

# Problem 1b: Density histogram ----
# See the problem 1b instructions for this studio.
# Summary: Make a density histogram of simulated exponential data
#          Add a graph of the pdf of exp(rate) to your plot
# See the test answers file for the output of test calls to this function
studio3_problem_1b = function(rate, nsamples) {
  cat("-----\nProblem 1b: Density histogram\n")
  
  # Arguments:
  #   rate = rate parameter for an exponential distribution.
  #   nsamples = the size of the sample to use for the histogram

  # We will give you the bin_width to use in the histogram
  bin_width = 0.5
  
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

  data = rexp(nsamples, rate)
  bins = seq(min(data), max(data)+bin_width, bin_width)
  hist(data, breaks=bins, col='yellow', freq=FALSE)
  
  x_max = max(data)
  x = seq(0, x_max, 0.01)
  y = dexp(x, rate)
  lines(x, y, col='blue', lwd=2)

  cat('See plot\n')
}
  
#-------------------------
# Problem 2: Density histogram. ----
# See the problem 2 instructions for this studio.
# Summary: Make a density histogram of average of independent exponential data

# 2a. See the problem 2a instructions for this studio.
# Summary: Average samples from two independent exponential variables
# See the test answers file for the output of test calls to this function
studio3_problem_2a = function(rate, nsamples) {
  cat("\n----------------------------------\n")
  cat("Problem 2a: Density histogram\n")

  # Arguments:
  #   rate = rate parameter for an exponential distribution.
  #   nsamples = the size of the exponential sample

  # We will give you the bin_width to use in the histogram
  bin_width = 0.4

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

  data1 = rexp(nsamples, rate)
  data2 = rexp(nsamples, rate)
  ave_data = (data1+data2)/2
  bins = seq(min(ave_data), max(ave_data)+bin_width, bin_width)
  hist(ave_data, breaks=bins, col='yellow', freq=FALSE)
  
  cat('See plot\n')
}

# 2b. See the problem 2b instructions for this studio.
# Summary: Average samples from n independent exponential variables
# See the test answers file for the output of test calls to this function
studio3_problem_2b = function(rate, nsamples, n_to_average, bin_width) {
  cat("\n----------------------------------\n")
  cat("-----Problem 2b: Density histogram of average\n")
  
  # Arguments:
  #   rate = rate parameter for an exponential distribution.
  #   nsamples = the size of each exponential sample
  #   n_to_average = the number exponential samples to average
  #   bin_width = the width of the bins to use in the histogram  

  # mean and stdandard deviation of the average
  # of n exponential random variables
  mean_of_average = 1/rate  
  std_dev_of_average = (1/rate)/sqrt(n_to_average)
  
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

  # Method (i) rexp, matrix, colMeans
  x = rexp(nsamples*n_to_average, rate)
  data = matrix(x, nrow=n_to_average, ncol=nsamples)
  ave_data = colMeans(data)
  bins = seq(min(ave_data), max(ave_data)+bin_width, bin_width)
  xlab = paste('average data', n_to_average)
  hist(ave_data, breaks=bins, col='yellow', xlab=xlab, freq=FALSE)

  # Now add the normal pdf  
  mu = mean_of_average
  sigma = std_dev_of_average
  x = mu + seq(-4*sigma, 4*sigma, 0.01)
  y = dnorm(x, mu, sigma)
  lines(x, y, col='blue', lwd=3)

  cat('See plot\n')
}

# 2b. Alternative solution
# In problem 2b, we could have used a loop to generate the data
# Method (ii) rexp and a for loop
studio3_problem_2b_alt = function(rate, nsamples, n_to_average, bin_width) {
  
  mean_of_average = 1/rate  
  std_dev_of_average = (1/rate)/sqrt(n_to_average)
  
  total_data = rep(0,nsamples)  # intialize the total to all zeros
  for (j in 1:n_to_average) {
    total_data = total_data + rexp(nsamples, rate)
  }
  ave_data = total_data/n_to_average
  bins = seq(min(ave_data), max(ave_data)+bin_width, bin_width)
  hist(ave_data, breaks=bins, col='orange', freq=FALSE)

  # Now add the normal pdf  
  mu = mean_of_average
  sigma = std_dev_of_average
  x = mu + seq(-4*sigma, 4*sigma, 0.01)
  y = dnorm(x, mu, sigma)
  lines(x, y, col='blue', lwd=3)
  
  cat('See plot\n')
}

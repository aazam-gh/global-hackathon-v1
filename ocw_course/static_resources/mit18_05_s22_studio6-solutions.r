#---------------------------------------------------------
# File:   mit18_05_s22_studio6-solutions.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 6 solutions ----

# 1. Be sure to read the instructions file!

# 2. The test answers file gives sample output and the functions we ran to get it. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# Problem 0: Averaging normal distributions.
# See instructions for this studio
studio6_problem_0 = function() { 
  cat("\n----------------------------------\n")
  cat("Problem 0: Averaging normal distributions \n")

  # This will draw the pair of histograms needed for this problem
  source('mit18_05_s22_studio6_problem0_draw_plot.r')
  studio6_problem0_draw_plot(10, 6, 9, 10000)

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat('0. Both histograms are centered near the true mean mu. The histogram of the averaged data is much narrower, i.e. less spread out from this mean.\n')
}

#---------------------------
# Problem 1: Cauchy distribution

# 1a. Formula for Cauchy pdf
studio6_problem_1a = function() {
  cat("\n----------------------------------\n")
  cat("Problem 1a. Formula for Cauchy pdf\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("1a. f(x | theta) = 1/(pi*(1+(x-theta)^2))\n")
}


# Problem 1b: Plot pdfs
studio6_problem_1b = function() {
  cat("-----\n")
  cat("1b. Plot pdf of Cauchy and standard normal\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  # Set the parameters
  loc = 0
  scale = 1
  range_min = -5
  range_max = 5

  # Compute values of the Cauchy and normal densities
  x = seq(range_min, range_max, 0.01)
  cauchy_pdf = dcauchy(x, location=loc, scale=scale)
  std_norm_pdf = dnorm(x,0,1)

  # Plot the pdfs
  plot_title = "1b. Cauchy (blue) and standard normal (orange) pdfs"
  plot(x, std_norm_pdf, type='l', col='orange', xlab='x',ylab='pdf', main=plot_title)
  lines(x, cauchy_pdf, col='blue')
  abline(h=0)

  cat("1b. See plots\n")
}

# Problem 1c: Explain fat tails
studio6_problem_1c = function() {
  cat("-----\n")
  cat("1c. Explain fat tails\n")

  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("1c. Answer: The tails are the left and right ends of the graph. In its tails the Cauchy graph is much higher than the normal, i.e. the tails of the Cauchy are much fatter than those of the normal.\n")
}

# Problem 1d: Average of Cauchy
studio6_problem_1d = function() {
  cat("-----\n")
  cat("1d. Average of Cauchy distributions\n")

  # Be sure the working directory is set to source file location
  # No code to write. Just run this code
  source('mit18_05_s22_studio6_problem_1d.r')

 
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("Notice that averaging the Cauchy distribution does not change the spread of the histogram. Averaging does not help us estimate the location of the pdf!\n")
}

#--------------------------------------
# Problem 2: Cauchy's lighthouse ----

# 2a: Load and plot data
studio6_problem_2a = function(data_frame_csv) {
  cat("\n----------------------------------\n")
  cat("Problem 2a. Load and plot data.\n")

  # Be sure the working directory is set to source file location

  # Arguments:
  #  data_frame_csv = name of the data file. (The grader will use a different data file than the test scripts.)

  # Load the data
  studio6_data_frame = read.csv(data_frame_csv)
  print(head(studio6_data_frame))
  position_data = studio6_data_frame[,'position']

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

  plot_title = "2a. Position data vs. index"
  plot(position_data, type='p', pch=19, col='blue', main=plot_title)
  cat("2a. See plot\n")
}

# Problem 2b:. Discretized Bayesian updates----
studio6_problem_2b = function(data_frame_csv) {
  cat("-----\n")
  cat("2b. Discretized Bayesian updates and MAP estimates.\n")

  # Arguments:
  #  data_frame_csv = name of the data file. (The grader will use a different data file than the test scripts.)

  # We give the unknown parameter theta a prior that follows a Uniform(theta_min, theta_max) distribution.
  # Use these values
  theta_min = -10
  theta_max = 10
  dtheta = 0.02 # Stepsize for discretizing the prior.

  # Load the data
  studio6_data_frame = read.csv(data_frame_csv)
  position_data = studio6_data_frame[,'position']

  # Do not change the above code.
  # ********* YOUR CODE FOR HERE***********

  # 2b(i)
  # Discretize the uniform(theta_min, theta_max) prior. (This is a distribution of theta).
  
  # The first array is 'unnormalized' because it doesn't sum to 1
  theta_range = seq(theta_min, theta_max, dtheta)
  unnormalized_uniform_prior = rep(1,length(theta_range))
  # To normalize we want sum(prior) = 1
  discrete_prior = unnormalized_uniform_prior/(sum(unnormalized_uniform_prior))

  ndata = length(position_data)
  cauchy_scale = 1.0  # This is given to us.

  # Initialize matrix whose jth column will store
  # the posterior distribution after the jth update
  # We will use this to make plots at the end
  posterior_mat = matrix(NA, nrow=length(theta_range), ncol=ndata)

  
  # 2b(ii) Update by looping over the data
  prior = discrete_prior
  for (j in 1:ndata) {
    x = position_data[j]
    likelihood = dcauchy(x, location=theta_range, scale=cauchy_scale)
    bayes_numerator = likelihood*prior
    posterior = bayes_numerator/(sum(bayes_numerator))
    posterior_mat[,j] = posterior

    # Make the posterior the next prior
    prior = posterior
  }

  
  # 2b(iii) Plot graphs
  
  # First we set some sizes so all the graphs fit on the plot.
  ymin = 0
  ymax = max(posterior_mat, discrete_prior)
  # Create an empty plot with the correct ranges
  plot_title = "2b(iii). Posteriors (and prior) (Cauchy location)"
  plot(c(theta_min,theta_max), c(ymin,ymax), type='n', xlab="theta", ylab="pdf", main = plot_title)

  # Plot the original prior in red
  lines(theta_range, discrete_prior, col='red')

  # Plot each of the posteriors in various colors
  for (j in 1:ndata) {
    #R lets you cycle through its list of colors by using col=j
    lines(theta_range, posterior_mat[,j], col=j)
  }

  
  # 2b(iv) Find the maximum in each of the posteriors
  MAP_estimates = rep(0, ndata)
  for (j in 1:ndata) {
    k = which.max(posterior_mat[,j])  # Index of maximum probability
    MAP_estimates[j] = theta_range[k] # Pick theta with that max. prob
  }

  # Plot the MAPEstimates
  plot_title = "2b(iv). MAP estimate vs. index"
  plot(MAP_estimates, type='p', col='blue', pch=19, xlab="index", main=plot_title)

  final_map_estimate = MAP_estimates[ndata]
  print(paste("2b(iv) Final map estimate: ", final_map_estimate))


  # 2b(v)
  # Make a separate plot of the final posterior pdf and MAP estimate
  plot_title = paste("2b(v). Final posterior. MAP = ", MAP_estimates[ndata])
  plot(theta_range, posterior_mat[,ndata], type='l',col='orange', lwd=2, xlab='theta', ylab='posterior', main=plot_title)
  abline(v=MAP_estimates[ndata], col='blue', lty='dashed')

  # 2b(vi) Where to look
  cat("2b(vi). The posterior has most of its probability for theta between -1 and 1. This is significantly less distance to cover than the initial -10 to 10. I would go to the MAP estimate of", final_map_estimate, "and search near there. I would also keep collecting data and updating my posterior.\n")
}

# Problem 2c:. OPTIONAL Explanation ----
studio6_problem_2c = function() {
  cat("-----\n")
  cat("Problem 2c (OPTIONAL). Explanation\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE***********

  # The following draws an explanatory diagram
  fake_theta = 3
  x = 7
  plot_title = "2c. Explanatory diagram"
  plot(c(0,10), c(0,1.5), xlab="theta", ylab="", axes=F, type='n',
       main=plot_title)
  points(c(fake_theta), c(1.0), pch='*', col='blue', cex=4)
  numberline = c(-10, -8, -6, -4, -2, 0, 2, fake_theta, 7, 8, 10)
  labs = c(-10, -8, -6, -4, -2, 0, 2, expression(theta), "x", 8, 10)
  axis(1, at=numberline, labels=labs, pos=0)
  axis(2, at=c(0,1), pos=0, las=1)
  lines(c(fake_theta, fake_theta, x), c(0, 1, 0))
  text(5, 0, pos=3, expression(x-theta), cex=1.5)
  text(3.5, 0.75, expression(alpha), cex=1.5)
  text(fake_theta, 0.5, 1, cex=1.5, pos=2)
  text(5, 1/2, expression(sqrt(1+(x-theta)^2)), cex=1.5, pos=4)

  # Here is the explanation.
  cat("2c (OPTIONAL). Here is the explanation.\n\n")

  cat("Look at the explanatory diagram.\n")
  cat("The angle alpha is random and uniform between -pi/2 and pi/2.\n")
  cat("The figure shows that x-theta = tan(alpha)\n")
  cat("This is a transformation from alpha to x, so we can use it to find the pdf for x.\n")
  cat("As usual, we work with the cdf and take a derivative at the end\n")
  cat("\n")
  cat("Let X be the random variable whose values x are the random (in time) position meausrements. Let A be the random variable whose values alpha are the random (in time) angles.\n")
  cat("Then, F_X(x) is the cdf of X and F_A(alpha) is the cdf of A\n")
  cat("The figure shows that X-theta = tan(alpha)\n")
  cat("So, F(x|theta) = P(X < x | theta)\n")
  cat("       = P(tan(alpha) < x-theta | theta)\n")
  cat("       = P(alpha < arctan(x-theta) | theta)\n")
  cat("       = (1/pi)*arctan(x-theta)\n")
  cat("The last equality is because A is uniform on [-pi/2,pi/2]\n")
  cat("Now all we have to do is remember how to differentiate artctan in order to find f(x|theta):\n")
  cat("f(x|theta) = (1/pi)*1/(1+(x-theta)^2))  QED\n")
}

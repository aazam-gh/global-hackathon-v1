#---------------------------------------------------------
# File:   MIT18_05S22_ps8_sol.r
# Author: Jeremy Orloff
# Calculations for Pset 8
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------

# Flags ------------------
do_problem_1a = T
do_problem_1b = T
do_problem_1c = T
do_problem_1d = T
do_problem_1e = T
do_problem_1f = T
do_problem_2 = T
do_problem_3 = T

#-------------------------#
#problem 2 ----
if (do_problem_1a) {
  cat('1a-----------------\n')
  n = 250
  x = 140
  theta_0 = 0.5
  p1 = 1 - pbinom(x-1, n, theta_0)
  s = sprintf("1a. one-sided p value: %.5f", p1)
  cat(s, '\n')
  s = sprintf("1a. two-sided p value: %.5f", 2*p1)
  cat(s, '\n')
}

if (do_problem_1b) {
  cat('1b-----------------\n')
  n = 250
  alpha1 = 0.05
  theta_0 = 0.5
  #cv stands for critical value
  cv1_left = qbinom(alpha1/2, n, theta_0) - 1
  cv1_right = qbinom(1-alpha1/2, n, theta_0) + 1
  cat('1b. alpha=0.05, left and right critical values:',
      cv1_left, cv1_right, '\n')
  #Check
  sum(dbinom(0:cv1_left, n, theta_0))
  sum(dbinom(cv1_right:n, n, theta_0))
  # widen by 1 and see the probability > 0.005 in each side
  sum(dbinom(0:(cv1_left+1), n, theta_0))
  sum(dbinom((cv1_right-1):n, n, theta_0))
  
  alpha2 = 0.1
  #cv stands for critical value
  cv2_left = qbinom(alpha2/2, n, theta_0) - 1
  cv2_right = qbinom(1-alpha2/2, n, theta_0) + 1
  cat('1b. alpha=0.1, left and right critical values:',
      cv2_left, cv2_right, '\n')
  #Check
  sum(dbinom(0:cv2_left, n, theta_0))
  sum(dbinom(cv2_right:n, n, theta_0))
  # widen by 1 and see the probability > 0.005 in each side
  sum(dbinom(0:(cv2_left+1), n, theta_0))
  sum(dbinom((cv2_right-1):n, n, theta_0))
  
  # make a plot # NEEDS n and theta_0 already defined
  x = 80:180
  prob_0 = dbinom(x, n, theta_0)
  par('mar'=c(4.1, 4.1, 1.1, 1.1))
  plot(x, prob_0, type='p', pch=19, col='blue',ylab="prob",ylim=c(-0.01,0.06), cex=0.8, cex.lab=1.2)
  col1 = rgb(1, 0.65, 0.30)
  col2 = rgb(0, 0.64, 0.84)
  lines(c(80,cv1_left), c(-0.005,-0.005), col=col1, lwd=8)
  lines(c(cv1_right,180), c(-0.005,-0.005), col=col1, lwd=8)
  lines(c(80,cv2_left), c(-0.008,-0.008), col=col2, lwd=8)
  lines(c(cv2_right,180), c(-0.008,-0.008), col=col2, lwd=8)
  text(150,0.05,  bquote(theta == .(theta_0)), cex=1.6)
  cat('1b. See plot\n')
}

if (do_problem_1c) {
  cat('1c-----------------\n')
  n = 250
  alpha = 0.01
  theta_0 = 0.5
  critical_value_left = qbinom(alpha/2, n, theta_0) - 1
  critical_value_right = qbinom(1-alpha/2, n, theta_0) + 1
  cat('1c. left and right critical values:', 
      critical_value_left, critical_value_right, '\n')
  #Check
  sum(dbinom(0:critical_value_left, n, theta_0))
  sum(dbinom(critical_value_right:n, n, theta_0))
  # widen by 1 and see the probability > 0.005 in each side
  sum(dbinom(0:(critical_value_left+1), n, theta_0))
  sum(dbinom((critical_value_right-1):n, n, theta_0))
}

if (do_problem_1d) {
  cat('1d-----------------\n')
  n = 250
  alpha = 0.05
  theta_0 = 0.5
  critical_value_left = qbinom(alpha/2, n, theta_0) - 1
  critical_value_right = qbinom(1-alpha/2, n, theta_0) + 1
  cat('1di. left and right critical values:', 
      critical_value_left, critical_value_right, '\n')
  #Check
  sum(dbinom(0:critical_value_left, n, theta_0))
  sum(dbinom(critical_value_right:n, n, theta_0))
  # widen by 1 and see the probability > 0.005 in each side
  sum(dbinom(0:(critical_value_left+1), n, theta_0))
  sum(dbinom((critical_value_right-1):n, n, theta_0))
  # Compute power
  rejectRegion = c(0:critical_value_left, critical_value_right:n)
  theta_55 = 0.55
  power_55 = sum(dbinom(rejectRegion, n, theta_55))
  cat('1di. power for theta = .55:', power_55, '\n')
  theta_6 = 0.6
  power_6 = sum(dbinom(rejectRegion, n, theta_6))
  cat('1di. power for theta = .6:', power_6, '\n')
  
  x = 80:180
  n = 250
  theta_0 = 0.5
  prob_0 = dbinom(x, n, theta_0)
  
  theta = 0.55
  col1 = rgb(1, 0.65, 0.30)
  col2 = rgb(0, 0.64, 0.84)
  prob_A = dbinom(x, n, theta)
  par('mar'=c(4.1, 4.1, 1.1, 1.1))
  plot(x, prob_0, type='p', pch=19, col='blue', ylab="prob",
       ylim=c(-0.01,0.06), cex=0.8, cex.lab=1.2)
  points(x, prob_A, type='p', pch=19, col=col1, ylab="prob", cex=0.8)
  lines(c(80,109),c(-0.005,-0.005), col=col2, lwd=8)
  lines(c(141,180),c(-0.005,-0.005), col=col2, lwd=8)
  text(170,0.05,  bquote(theta == .(theta)), cex=1.6)
  
  theta = 0.6
  prob_A = dbinom(x, n, theta)
  par('mar'=c(4.1, 4.1, 1.1, 1.1))
  plot(x, prob_0, type='p', pch=19, col='blue', ylab="prob",
       ylim=c(-0.01,0.06), cex=0.8, cex.lab=1.2)
  points(x, prob_A, type='p', pch=19, col=col1, ylab="prob", cex=0.8)
  lines(c(80,109), c(-0.005,-0.005), col=col2, lwd=8)
  lines(c(141,180), c(-0.005,-0.005), col=col2, lwd=8)
  text(170, 0.05,  bquote(theta == .(theta)), cex=1.6)

  cat('1dii: See plots\n')
}

if (do_problem_1e) {
  cat('1e-----------------\n')
  theta_A = 0.55
  theta_0 = 0.5
  # Here's code to do this for one value of n --see below for loop
  n = 1055
  critical_value_left = qbinom(0.025, n, theta_0) - 1;
  # We add one, so critical_value_right is the first point
  # IN the rejection region
  critical_value_right = qbinom(0.975, n, theta_0) + 1;
  cat('1e. left and right critical values:', 
      critical_value_left, '  ', critical_value_right, '\n')
  rejection_region = c(0:critical_value_left, critical_value_right:n)
  # We use dbinom to avoid endpoint confusions from using pbinom
  power = sum(dbinom(rejection_region, n, theta_A))
  s = sprintf('1e. power for n=%d: %f', n, power)
  cat(s, '\n')
  
  # Here we search through a range of values of n until 
  # we get power of 0.9 We do a naive search because 
  # the power is not strictly monotonic in n. This is 
  # because, for a discrete distribution the exact 
  # significance is always a small amount less than 
  # the nominal significance of 0.05. The amount less 
  # can fluctuate with n. Likewise the power can fluctuate
  # a little.
  cat('1e. Power for a range of n', '\n')
  theta_A = 0.55
  for (n in 1050:1080) {
    #find critical points for rejection region (based on theta=0.5)
    critical_value_left = qbinom(0.025, n, theta_0) - 1;
    critical_value_right = qbinom(0.975, n, theta_0) + 1;
    rejection_region = c(0:critical_value_left, critical_value_right:n)
    power = sum(dbinom(rejection_region, n, theta_A))
    cat('  ', n,'  ',power,'\n')
  }
  
  # Plot for 1e
  x = 470:630
  n = 1055
  theta_0 = 0.5
  alpha = 0.05
  prob_0 = dbinom(x, n, theta_0)
  
  theta = 0.55
  prob_A = dbinom(x, n, theta)
  par('mar'=c(4.1, 4.1, 1.1, 1.1))
  plot(x, prob_0, type='p', pch=19, col='blue', ylab="prob",
       ylim=c(-0.005,0.03), cex=0.6, cex.lab=1.2)
  col1 = rgb(1, 0.65, 0.30)
  col2 = rgb(0, 0.64, 0.84)
  points(x, prob_A, type='p', pch=19, col=col1, ylab="prob", cex=0.6)
  lines(c(470,critical_value_left), c(-0.005,-0.005), col=col2, lwd=8)
  lines(c(critical_value_right,630), c(-0.005,-0.005), col=col2, lwd=8)
  text(500,0.028,  bquote(theta == .(theta)), cex=1.6)
  text(560,0.028,  bquote(n == .(n)), cex=1.6)
  cat('1e. See plot\n')
  
  # Normal approximation for 1e ----
  cat('1e. normal approximation ------------\n')
  theta_0 = 0.5
  theta_A = 0.55
  
  z025 = qnorm(0.975, 0, 1)
  z975 = qnorm(0.025, 0, 1)
  zp9 = qnorm(0.1, 0, 1)
  t02 = sqrt(theta_0*(1-theta_0))
  tA2 = sqrt(theta_A*(1-theta_A))
  dt = theta_A - theta_0
  n2 = t02*z025/dt - zp9*tA2/dt
  cat('1e. normal approximation:', n2^2, '\n')
}
  
if (do_problem_1f) {
  cat('1f-----------------\n')
  hyp = c(0.5, 0.55)
  prior = c(0.5, 0.5)
  n = 250
  data = 140
  lik = dbinom(data, n, hyp)
  cat('1f. likelihood:', lik, '\n')
  bayes_numerator = prior*lik
  posterior = bayes_numerator/sum(bayes_numerator)
  cat('1f. posterior:', posterior, '\n')
}
 
if (do_problem_2) {
  cat('2a-----------------\n')
  p_typeI = 9/140
  p_typeII = 15/140
  cat('2a. Probability of type I error:', p_typeI, '\n')
  cat('2a. Probability of type II error:', p_typeII, '\n')
}
  
if (do_problem_3) {
  cat('3b-----------------\n')
  thresh = qnorm(0.96, 40, 5/sqrt(3))
  cat('3b. thresh:', thresh, '\n')

  cat('3c-----------------\n')
  n = 3
  mu = 40
  sigma = 5/sqrt(n)
  alpha = 0.04
  x_crit = qnorm(1-alpha, mu, sigma)
  power = 1 - pnorm(x_crit, 45, sigma)
  cat('3c. power:', power, '\n')

  # Compute power for various n
  for (n in 4:16) {
    sigma = 5/sqrt(n)
    alpha = 0.04
    x_crit = qnorm(1-alpha, mu, sigma)
    power = 1 - pnorm(x_crit, 45, sigma)
    s = sprintf("n=%d: power=%f, critical value=%f", n, power, x_crit)
    print(s)
  }
}

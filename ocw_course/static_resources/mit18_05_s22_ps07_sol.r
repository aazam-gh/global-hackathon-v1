#---------------------------------------------------------
# File:   MIT18_05S22_ps7_sol.r 
# Author: Jeremy Orloff
# Calculations for Pset 7
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------

#----------------------------
do_prob6 = TRUE

#----------------------------
if (do_prob6) {
  cat('\nProblem 6 ------------\n')
  cat('Problem 6a ------------\n')
  n = 1
  xbar = 5
  mu_prior = 6
  sig2_prior = 9
  sig2 = 4
  
  a = 1/sig2_prior 
  b = n/sig2
  mu_post = (a*mu_prior + b*xbar)/(a+b)
  sig2_post = 1/(a+b)
  cat('mu_post:', mu_post, '\n')
  cat('sig2_post', sig2_post, 'sig_post', sqrt(sig2_post),'\n')

  cat('\nProblem 6b ------------\n')
  n = 8
  xbar = 5
  mu_prior = 6
  sig2_prior = 9
  sig2 = 4
    
  a = 1/sig2_prior 
  b = n/sig2
  mu_post = (a*mu_prior + b*xbar)/(a+b)
  sig2_post = 1/(a+b)
  cat('mu_post:', mu_post, '\n')
  cat('sig2_post', sig2_post, '\n')
  
  # Draw plots
  s_prior = sqrt(sig2_prior)
  s_post = sqrt(sig2_post)
  theta_min = mu_prior - 3*s_prior
  theta_max = mu_prior + 3*s_prior
  theta = seq(theta_min, theta_max, 0.01)
  y_prior = dnorm(theta, mu_prior, s_prior)
  y_post = dnorm(theta, mu_post, s_post)
  
  # We hard wire the height of the plot. It makes the code a little fragile, but it's not a big deal in this case.
  plot(c(theta_min-0.5*s_prior,theta_max+0.5*s_prior),c(0,0.6), type='n',xlab=expression(theta), ylab='')
  
  #Draw legend first to avoid covering curves

  s1 = sprintf('Prior N(%d,%d)', mu_prior,sig2_prior)
  s2 = sprintf('Posterior N(%.2f,%.2f)', mu_post, sig2_post)
  legend(-5.5,0.6,c(s1,s2),col=c('orange','blue'), lty=1,lwd=3)
  
  col = 'orange';
  lines(theta, y_prior, col=col, lwd=1)
  
  col = 'blue';
  lines(theta, y_post, col=col, lwd=1)
  cat('\nSee plot\n')
}

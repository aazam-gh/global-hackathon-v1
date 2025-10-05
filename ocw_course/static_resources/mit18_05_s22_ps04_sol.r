#---------------------------------------------------------
# File:   MIT18_05S22_ps4_sol.r 
# Author: Jeremy Orloff
# Calculations for Pset 4
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------

do_problem_2 = TRUE
do_problem_3 = TRUE
do_problem_4 = TRUE
do_problem_6 = TRUE

#----------------------------
if (do_problem_2) {
  N = 400
  
  # 2a
  mu = N*0.5
  sigma = sqrt(N*0.25)
  x = 0.525*N
  p = 1 - pnorm(x, mu, sigma)
  cat('2(a) Computing P(S >', x, ') =', p, '\n')
  
  # 2b
  mu = 0.3
  sigma = sqrt(mu*(1-mu))
  x = 0.31
  z = (x-mu)/(sigma/sqrt(N))
  p = pnorm(z)
  cat('2(b) Computing P(Y_bar <', x, ') = P(Z <',z, ') =', p, '\n')
}

if (do_problem_3) {
  p = 2*pnorm(-100/sqrt(2000))
  cat('3. P(|total error| > 100) =', p, '\n')
  
  # Optional simulation
  n_trials = 10000
  n_orders = 1000
  threshold = 100
  cnt_above_threshold = 0
  for (j in 1:n_trials) {
    # The rounding error is 0, -1, -2, 2, 1 depending on if the price modulo 5 is 0, 1, 2, 3, 4
    # Generate 1000 random orders, just keep the rounding error
    x = sample(c(0,-1,-2,2,1), n_orders, replace=TRUE)
    total_error = sum(x)
    if (abs(total_error) > threshold) {
      cnt_above_threshold = cnt_above_threshold + 1
    }
  }
  prob_above_thresh = cnt_above_threshold/n_trials
  
  cat('3. Simulated probability that abs(total_rounding_error) >', threshold, '=', prob_above_thresh, '(', n_trials, 'trials )', '\n')
}

if (do_problem_4) {
  t = seq(0, 10, 0.01)
  x = exp(-t)
  y = exp(-t/3)/3

  par(mar=c(4,1,1,1))  
  plot(c(0,11), c(0,1.1), type='n', axes=FALSE, ylab='', xlab='')

  abline(h=0,v=0)
  axis(1, pos=0)
  axis(2, pos=0)
  
  lines(t, x, col='black')
  lines(t, y, col='blue')
  
  text(4, 0.16, bquote(f[Y]*"(t)"), cex=1.7, col='blue')
  text(0.9, 0.8, bquote(f[X]*"(t)"), cex=1.7, col='black')
  mtext('t', 1, cex=2, at=11.3)
}

if (do_problem_6) {
  # 6a
  prob_IQ_160 = 1 - pnorm(160, 100, 15)
  cat('6(a) P(IQ > 160) =', prob_IQ_160, '\n')
  
  df = 3 # degrees of freedon
  sig = sqrt(3) # standard deviation
  
  # 6c
  p4 = 1 - pt(4*sig, df)
  cat('6(c) P(I > 4*sig) =', p4, '\n')
  
  # 6d
  for (x in c(20,40,200)) {
    p_t = 1 - pt(x, df)
    p_n = 1 - pnorm(x, 0, sig)
    cat('6(d)', 'P(T >', x, ') =', p_t, ', P(N >', x, ') =', p_n, '\n')
  }

  # Plot center of t and normal distributions
  par(mar=c(4,1,1,1)) 
  
  x = seq(-3*sig, 3*sig, 0.01)
  u = dt(x, df)
  v = dnorm(x, 0, sig)
  plot(x, u, type='l', col='blue', xlab='', ylab='')
  lines(x, v, col='black')
  text(1.8, 0.3, 't-distribution')
  text(2.6, 0.14, 'Normal')
  mtext('x', 1, cex=1.5, line=2.2)
  
  # plot tails
  x = seq(3*sig, 6*sig, 0.01)
  u = dt(x, df)
  v = dnorm(x, 0, sig)
  plot(c(3*sig, 6*sig), c(0, max(u)), type='n', xlab='', ylab='')
  lines(x, u, col='blue')
  lines(x, v, col='black')
  text(6.4, 0.003, 't-distribution')
  text(6.4, 0.001, 'normal')
  mtext('x', 1, cex=1.5, line=2.2)
  
  cat("Export figures from R-studio")
}

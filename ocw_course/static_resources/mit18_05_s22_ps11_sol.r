#---------------------------------------------------------
# File:   MIT18_05S22_ps11_sol.r
# Author: Jeremy Orloff
# Calculations for Pset 11
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------

# Flags ----
do_problem_1 = T
do_problem_2 = T
do_problem_3 = T

#-----------
print_div_line = function(msg) {
  cat("\n-----------------------\n", msg, '\n')
}

#-----------
if (do_problem_1) {
  cat('----- Problem 1 ----\n')
  cat('-- 1(a)--\n')
  n = 20
  xbar = 69.55
  s2 = 14.26
  conf = 0.90
  
  s = sqrt(s2)
  alpha = 1 - conf
  df = n-1
  t_alpha_2 = qt(1-alpha/2, df)
  rad = t_alpha_2*s/sqrt(n)
  critical_value_left = xbar - rad
  critical_value_right = xbar + rad
  
  cat("1(a):", conf, "Conf. int = ", xbar, "plus/minus", rad, "= [", critical_value_left, ",", critical_value_right, "]", '\n')
  cat("t_alpha/2 =", t_alpha_2, '\n')

  cat('-- 1(b)--\n')
  n = 20
  xbar = 69.55
  sigma = 3.77
  conf = 0.90
  
  alpha = 1 - conf
  z_alpha_2 = qnorm(1-alpha/2, 0, 1)
  rad = z_alpha_2*sigma/sqrt(n)
  critical_value_left = xbar - rad
  critical_value_right = xbar + rad
  
  cat("1(b):", conf, "Conf. int = ", xbar, "plus/minus", rad, "= [", critical_value_left, ",", critical_value_right, "]", '\n')
  cat("z_alpha/2 =", z_alpha_2, '\n')

  cat('-- 1(c)--\n')
  n = (2*z_alpha_2*sigma)^2
  cat("1(c): n to make width 1 (rad=1/2) is", n, '\n')
  
}

#-----------
if (do_problem_2) {
  cat('\n----- Problem 2 ----\n')
  n = 9
  y2 = 1.5
  conf = 0.95
  
  alpha = 1-conf
  c_alpha_2 = qbeta(1-alpha/2, 2, n-1)
  c_one_minus_alpha_2 = qbeta(alpha/2, 2, n-1)
  CI = c(y2/c_alpha_2, y2/c_one_minus_alpha_2)
  cat('n =', n, ', y_2 =', y2, '\n')
  cat('c_alpha_2 =', c_alpha_2, ', c_one_minus_alpha_2 =', c_one_minus_alpha_2, '\n')
  cat(conf, "Conf. int = [", CI[1], ",", CI[2], "]", '\n')
}  

#--------
if (do_problem_3) {
  # 3a
  plot_pmf_as_histogram = FALSE
  print_div_line('Pset 11 problem 3a -- see figures')

  iii = c('i','ii','iii')
  lwd = 2
  
  theta_vals = c(0.5, 0.3, 0.1)
  lower_bnds = c(80, 40, 0)  # we don't plot the entire range
  upper_bnds = c(170, 120, 55)
  legend_left = c(132,83,30)
  for (j in 1:3) {
    # move x-axis label closer to axis
    opar = par(mar=c(3,2.5,1.5,1), mgp=c(2,1,0)) 
    theta = theta_vals[j]
    n = 250
    m = 250*theta #mean
    v = 250*theta*(1-theta) #variance
    # (i) plot binomial
    x1 = lower_bnds[j]: upper_bnds[j]
    y1 = dbinom(x1, n, theta)
    
    if (plot_pmf_as_histogram) {
      # Use the polygon function to plot the pmf as a histogram with bin
      # width 1 and breaks on the 1/2 integers. So heights = values of dbinom
      polyx = 1:(4*length(x1)) # values don't matter, just establishing length
      polyy = 0*polyx
      for (k in 1:length(x1)) {
        # set the values so the polygon function draws a histogram with width 1, breaks on the 1/2 integers and heights at the integres given by dbinom
        polyx[4*k-3] = x1[k]-1/2
        polyx[4*k-2] = x1[k]-1/2
        polyx[4*k-1] = x1[k]+1/2
        polyx[4*k] =   x1[k]+1/2
        polyy[4*k-3] = 0
        polyy[4*k-2] = y1[k]
        polyy[4*k-1] = y1[k]
        polyy[4*k] = 0
      }
    }
    plot(x1,y1, pch=19, cex=.5, col='blue', ylab='', xlab='x')
    if (plot_pmf_as_histogram) {
      polygon(polyx, polyy)
    }
    title(bquote(theta == .(theta)))
    # (ii) plot normal with same mean and variance
    x2 = seq(lower_bnds[j], upper_bnds[j], 0.1)
    y2 = dnorm(x2, m, sqrt(v))
    lines(x2, y2, col='green', lwd=lwd)
    # (iii) plot normal with same mean and variance n/4
    y3 = dnorm(x2, m, sqrt(n/4))
    lines(x2, y3, col='orange', lwd=lwd)
    leg = c('True variance', 'Conservative variance', 'Binomial pmf')
    legend(legend_left[j], max(y1), leg, col=c('green','orange', 'blue'), lty=c(1,1,NA), pch=c(NA,NA,19), lwd=4,cex=.7)
  }
  par(opar)
  
  #3b
  print_div_line('Problem 3b')
  n = 250
  x = 140
  xbar = x/n
  sigma = 1/2
  conf = 0.8
  
  alpha = 1-conf
  crit_vals = qnorm(c(alpha/2, 1-alpha/2), 0, 1)
  ci = xbar + crit_vals*sigma/sqrt(n)
  cat(conf, "confidence interval", ci, "\n")
  
  crit_vals = c(-2, 2) # rule-of-thumb 95% interval
  ci = xbar + crit_vals*sigma/sqrt(n)
  
  cat("0.95 rule of thumb confidence interval",ci, "\n")
  crit_vals = qnorm(c(0.025, 0.975), 0, 1) # 95% crit values
  ci = xbar + crit_vals*sigma/sqrt(n)
  crit_vals = qnorm(c(0.025, 0.975), 0, 1) # 95% crit values
  cat("0.95 computed CI",ci, "\n")
  
  #3c Posterior for theta is beta(141,111)
  print_div_line('Problem 3c')
  a = 141
  b = 111
  prob_int = qbeta(c(0.1, 0.9), a, b)
  cat("prob. interval", prob_int, '\n')
}


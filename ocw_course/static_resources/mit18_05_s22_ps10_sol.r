#---------------------------------------------------------
# File:   MIT18_05S22_ps10_sol.r
# Author: Jeremy Orloff
# Calculations for Pset 10
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
do_problem_4 = T
do_problem_5 = T

#-----------
if (do_problem_1) {
  cat('----- Problem 1 ----\n')
  sigma = 9
  n = 30
  xbar = 15
  
  alpha = 0.05
  z_alpha_2 = qnorm(1-alpha/2,0,1)
  rad = z_alpha_2*sigma/sqrt(n)
  critical_value_left = xbar - rad
  critical_value_right = xbar + rad
  
  cat("Problem 1:", 1-alpha, "Conf. int = ", xbar, "plus/minus", rad, "= [", critical_value_left, ",", critical_value_right, "]", '\n')

  alpha = 0.3
  z_alpha_2 = qnorm(1-alpha/2,0,1)
  rad = z_alpha_2*sigma/sqrt(n)
  critical_value_left = xbar - rad
  critical_value_right = xbar + rad
  
  cat("Problem 1:", 1-alpha, "Conf. int = ", xbar, "plus/minus", rad, "= [", critical_value_left, ",", critical_value_right, "]", '\n')
}

#-----------
if (do_problem_2) {
  cat('\n----- Problem 2 ----\n')
  data = c(352, 351, 361, 353, 352, 358, 360, 358, 359)
  xbar = mean(data)
  n = length(data)
  
  cat('-- 2(a) --\n')
  sigma = 3
  conf = 0.95
  alpha = 1-conf
  z_alpha_2 = qnorm(1-alpha/2)
  rad = sigma*z_alpha_2/sqrt(n)
  CI = c(xbar-rad, xbar+rad)
  cat('xbar =', xbar, ', z_alpha_2 =', z_alpha_2, ', rad =', rad, '\n')
  cat(conf, 'CI = ', xbar, 'plus/minus', rad, ' = [', CI[1], ',', CI[2], ']','\n')

  cat('-- 2(b) --\n')
  sigma = 3
  conf = 0.98
  alpha = 1-conf
  z_alpha_2 = qnorm(1-alpha/2)
  rad = sigma*z_alpha_2/sqrt(n)
  CI = c(xbar-rad, xbar+rad)
  cat('xbar =', xbar, ', z_alpha_2 =', z_alpha_2, ', rad =', rad, '\n')
  cat(conf, 'CI = ', xbar, 'plus/minus', rad, ' = [', CI[1], ',', CI[2], ']','\n')
  
  cat('-- 2(c) --\n')
  s = sd(data)
  df = n-1
  
  conf = 0.95
  alpha = 1-conf
  t_alpha_2 = qt(1-alpha/2,df)
  rad = s*t_alpha_2/sqrt(n)
  CI = c(xbar-rad, xbar+rad)
  cat('xbar =', xbar, ', s =', s, ', t_alpha_2 =', t_alpha_2, ', rad =', rad, '\n')
  cat(conf, 't-CI = ', xbar, 'plus/minus', rad, ' = [', CI[1], ',', CI[2], ']','\n')

  conf = 0.98
  alpha = 1-conf
  t_alpha_2 = qt(1-alpha/2,df)
  rad = s*t_alpha_2/sqrt(n)
  CI = c(xbar-rad, xbar+rad)
  cat('xbar =', xbar, ', t_alpha_2 =', t_alpha_2, ', rad =', rad, '\n')
  cat(conf, 't-CI = ', xbar, 'plus/minus', rad, ' = [', CI[1], ',', CI[2], ']','\n')
}  

#--------
if (do_problem_3) {
  cat('\n----- Problem 3 ----\n')
  data = c(6.0, 6.4, 7.0, 5.8, 6.0, 5.8, 5.9, 6.7, 6.1, 6.5, 6.3, 5.8)
  s2 = var(data) # sample variance
  n = length(data)
  
  conf = 0.95
  alpha = 1-conf
  df = n - 1 # degrees of freedom
  c_alpha_2 = qchisq(1-alpha/2, df) #critical value
  c_1_minus_alpha_2 = qchisq(alpha/2, df) # critical value
  CI = c((n-1)*s2/c_alpha_2, (n-1)*s2/c_1_minus_alpha_2)
  cat('s2 =', s2, '\n')
  cat('c_alpha_2 =', c_alpha_2, ', c_1_minus_alpha_2 =', c_1_minus_alpha_2, '\n')
  cat(conf, 'CI = [', CI[1], ',', CI[2], ']','\n')
}

#--------
if (do_problem_4) {
  cat('\n----- Problem 4 ----\n')
  s2 = 4.2 # sample variance
  n = 10
  
  conf = 0.95
  alpha = 1-conf
  df = n - 1 # degrees of freedom
  c_alpha_2 = qchisq(1-alpha/2, df) #critical value
  c_1_minus_alpha_2 = qchisq(alpha/2, df) # critical value
  CI_variance = c((n-1)*s2/c_alpha_2, (n-1)*s2/c_1_minus_alpha_2)
  CI_sd = sqrt(CI_variance)
  cat('s2 =', s2, '\n')
  cat('c_alpha_2 =', c_alpha_2, ', c_1_minus_alpha_2 =', c_1_minus_alpha_2, '\n')
  cat(conf, 'CI = [', CI_sd[1], ',', CI_sd[2], ']','\n')
}

#--------
if (do_problem_5) {
  cat('\n----- Problem 5 ----\n')
 
  n = 64
  sigma = 5
  xbar = 1.6
  
  cat('-- 5(b)i --\n') 
  conf = 0.95
  alpha = 1-conf
  z_alpha_2 = qnorm(1-alpha/2)
  rad = z_alpha_2*sigma/sqrt(n)
  CI = xbar + c(-rad, rad)
  cat('xbar =', xbar, ', sigma =', sigma, '\n')
  cat('z_alpha_2 =', z_alpha_2, ', rad =', rad, '\n')
  cat(conf, 'CI =', xbar, 'plus/minus', rad, '= [', CI[1], ',', CI[2], ']', '\n\n')
  
  cat('-- 5(b)ii --\n') 
  mu_vals = c(0,1,2,3)
  prior = c(0.94, 0.02, 0.02, 0.02)
  likelihood = dnorm(xbar, mu_vals, sigma/sqrt(n))
  bayes_numer = prior*likelihood
  total_prob = sum(bayes_numer)
  posterior = bayes_numer/total_prob
  
  bayes_table = data.frame(
    mu_vals=mu_vals,
    prior=prior,
    likelihood=likelihood,
    bayes_numer=bayes_numer,
    posterior=posterior
  )
  cat('Total prob =', total_prob, '\n')
  title = paste("Bayes table")
  print(title)
  print(bayes_table)
  
  # Compute prior prob theta in CI
  in_CI = (mu_vals >= CI[1]) & (mu_vals <= CI[2])
  prior_prob_in_CI = sum(prior[in_CI])
  posterior_prob_in_CI = sum(posterior[in_CI])
  cat('prior prob in CI =', prior_prob_in_CI, '\n')
  cat('posterior prob in CI =', posterior_prob_in_CI, '\n')
}

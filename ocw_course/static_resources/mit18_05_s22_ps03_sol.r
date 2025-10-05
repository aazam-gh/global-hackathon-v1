#---------------------------------------------------------
# File:   MIT18_05S22_ps3_sol.r 
# Author: Jeremy Orloff
# Calculations for Pset 3
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------

#----------------------------
do_problem_2 = TRUE
do_problem_4 = TRUE
do_problem_5 = TRUE
do_problem_6 = TRUE

#----------------------------
if (do_problem_2) {
  source('MIT18_05S22_ps3prob2-data.r')
  years = get_prob2_data()
  m2a = mean(years)
  v2a = var(years)
  sd2a = sqrt(var(years))

  cat('2()a. mean =', m2a, ', var = ', v2a, ', std. dev =', sd2a, '\n' )

  # 2(b): Make histogram. (Export image from R Studio to save)
  hist(years, freq=T, breaks=seq(0,5,.1), main= "Years of Survival", col="yellow")
}

#----------------------------
if (do_problem_4) {
  # Problem 4bc. 
  
  # PDF plot
  box_height = 30
  nraisins = 750
  h = seq(0, box_height, 0.01)
  pdf_g = (40-h)/nraisins
  
  # type='n' means no plot. We just use it to establish dimensions
  plot_width = c(0, box_height)
  plot_height = c(0, 5/75) # Hack to make the plot a reasonable height
  plot(plot_width, plot_height, type='n', main="PDF: g(h) vs. h", xlab='h', ylab='g(h)', 
       axes=F)
  
  # Draw the axes. pos=0 means place where x=0 or y = 0
  axis(1,pos=0,col='black')
  axis(2,pos=0,col='black')
  
  # Add the plot of the pdf
  lines(h, pdf_g, lwd=2, col='blue')
  #Export plot from RStudio
  
  # CDF plot
  plot_width = c(0, box_height)
  plot_height = c(0, 1)
  cdf_g = 40*h/nraisins - h^2/(2*nraisins)
  plot(plot_width, plot_height, type='n', main="CDF: G(h) vs h",xlab='h', ylab='G(h)', 
       axes=F)
  axis(1, pos=0, col='black')
  axis(2, pos=0, col='black')
  
  lines(h, cdf_g, lwd=2, col='blue')
  #Export plot from RStudio
}

#-----------------------------
if (do_problem_5) {
  #5a
  #5a(ii)
  p = 1 - pnorm(1.5)
  cat('(5a(ii): ', p, '\n')

  #5a(iii)
  p = pnorm(1.5) - pnorm(-1.5)
  cat('(5a(iii): ', p, '\n')

  #5b
  mu = 2
  sigma = 3

  #5b(i)
  p = pnorm(mu, mu, sigma)
  cat('(5b(i): ', p, '\n')

  #5b(ii)
  p = 1 - pnorm(mu + 1.5*sigma, mu, sigma)
  cat('(5b(ii): ', p, '\n')

  #5b(iii)
  p = pnorm(mu + 1.5*sigma, mu, sigma) - pnorm(mu - 1.5*sigma, mu, sigma)
  cat('(5b(iii): ', p, '\n')

  #5c 
  p_ans = 1 - exp(-1)

  # Check with a specific lambda
  lambda = 4.5 #Arbitrary choice
  p_check = pexp(1/lambda, lambda)

  cat('5c Theoretical prob.: p =', p_ans, '\n')
  cat('5c Check with lambda = 4.5: p =', p_check, '\n')
}

#------------------------ 
if (do_problem_6) {
  # Problem 6a
  # Plot Norm(280, 8.5^2)
  # We'll plot 3 standard deviations
  m = 280
  s = 8.5
  x = seq(m-3*s, m+3*s, 0.01)
  pdf_f = dnorm(x, m, s)
  
  # type='n' means no plot. We just use it to establish dimensions
  plot_width = c(min(x)-10, max(x)+10)
  plot_height = c(0, 1.1*max(pdf_f))
  plot(plot_width, plot_height, type='n', main="PDF: f(x) vs. x", xlab='', ylab='', axes=F)
  # Draw the axes. pos=0 means place where x=0 or y = 0
  axis(1, pos=0, col='black')
  axis(2, pos=250, col='black')
  
  #Plot the pdf
  lines(x, pdf_f, lwd=2, col='blue')
  #Export plot from RStudio

  # CDF plot
  cdf_F = pnorm(x, m, s)
  # plot_width same as above
  plot_height = c(0,1)
  plot(plot_width, plot_height, type='n', main="CDF: F(x) vs x", xlab='', ylab='',axes=F)
  axis(1, pos=0, col='black')
  axis(2, pos=250, col='black')

  # Plot the cdf
  lines(x, cdf_F, lwd=2, col='blue')
  #Export plot from RStudio
  
  # 6b
  p_before_final = pnorm(-7, 0, 8.5)
  cat('6b: Prob. of birth before final =', p_before_final, '\n')
  
  #6c
  p_after_final = pnorm(6, 0, 8.5) - pnorm(-6, 0, 8.5)
  cat('6c: Prob. of birth after final =', p_after_final, '\n')
  
  #6d
  ndays_to_move = qnorm(0.05, 0, 8.5)
  cat('6d: Number of days to move up the final =', ndays_to_move, '\n')
}

#---------------------------------------------------------
# File:   MIT18_05S22_ps6_sol.r 
# Author: Jeremy Orloff
# Calculations for Pset 6
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

#----------------------------
if (do_problem_2) {

  # 2d,2e
  x = c(1, 3, 5)
  y = c(8, 2, 1)
 
  n = length(x)
  Sx = sum(x)
  xbar = Sx/n
  Sy = sum(y)
  ybar = Sy/n
  Sxx = sum(x^2)
  Sxy = sum(x*y)
  a = (Sxy/n-xbar*ybar)/(Sxx/n - xbar^2)
  b = ybar - a*xbar

  cat('2(b) MLE for a and b: a =', a, ', b =', b, '\n')

  plot(x, y, pch=19, col="blue")
  #Perversely, in abline a is the intercept and b is the slope.\\
  abline(a=b, b=a, col="orange")

  cat('2(c): See plot.\n')
}



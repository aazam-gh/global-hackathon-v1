#---------------------------------------------------------
# File:   MIT18_05S22_ps5_sol.r 
# Author: Jeremy Orloff
# Calculations for Pset 5
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------

#----------------------------
do_problem_1 = TRUE
do_problem_4 = TRUE

#----------------------------
if (do_problem_1) {

  #1c  
  EX =  5/7  # E[X]
  EX2 = 39/70 # E[X^2]
  var_X = EX2 - EX^2
 
  cat('1(c) E[X] = ', EX, 'E[X^2] =', EX2, 'Var(X) =', var_X, '\n')
  
  #1d
  EY = 4/7    # E[Y]
  EY2 = 17/42 # E[Y^2]
  var_Y = EY2 - EY^2
  EXY = 17/42 
  cov_XY = EXY - EX*EY
  cor_XY = cov_XY/sqrt(var_X*var_Y)

  cat('1(d) E[Y] = ', EY, 'E[Y^2] =', EY2, 'Var(Y) =', var_Y, '\n')
  cat('1(d) E[XY] = ', EXY, 'cov(X,Y) =', cov_XY, 'cor(X,Y) =', cor_XY, '\n')
}

if (do_problem_4) {
  # 4d
  p = (1/3600)*(3600 - 55^2/2)
  cat('4(d) P(A < B+5) =', p, '\n')
}

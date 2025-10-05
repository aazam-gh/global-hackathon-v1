#---------------------------------------------------------
# File:   MIT18_05S22_ps1_sol.r 
# Author: Jeremy Orloff
# Calculations for Pset 1
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------

do_problem_3e = T
do_problem_3g = T

#----------------------------
if (do_problem_3e) {
    
# colMatches is an 18.05 function. Its source file MIT18_05S22_colMatches.r needs to be in the R working directory
# You can find the file MIT18_05S22_colMatches.r on our class website.
source('MIT18_05S22_colMatches.r')

  cat('------ Problem 3e ------\n')
# So we can run it multiple times we put the simulation in a function
  simulate_birthday_match2 = function(npeople, ntrials) {
    # Set up the parameters (if wanted, these could also be arguments to the function)
    ndays = 365
    size_match = 2
    year = 1:ndays
    # Run ntrials -one per column- using sample() and matrix()
    y = sample(year, npeople*ntrials, replace=TRUE)
    trials = matrix(y, nrow=npeople, ncol=ntrials)
    w = colMatches(trials, size_match)
    prob_B = mean(w)
  
    cat('npeople =', npeople, ', size_match =', size_match, 'ntrials =', ntrials, ', probability of match =', prob_B, '\n')
  }
  
  simulate_birthday_match2(20, 10000)
  simulate_birthday_match2(30, 10000)
  simulate_birthday_match2(40, 10000)
  simulate_birthday_match2(40, 10000)
  simulate_birthday_match2(40, 10000)
  simulate_birthday_match2(40, 10000)
  simulate_birthday_match2(41, 10000)
  simulate_birthday_match2(41, 10000)
  simulate_birthday_match2(41, 10000)
  simulate_birthday_match2(41, 10000)
}

if (do_problem_3g) {
  
  cat('\n------ Problem 3g ------\n')
  
  # This is the same code as for do_problem_3, except size_match is set to 3
  source('MIT18_05S22_colMatches.r')
  
  # So we can run it multiple times we put the simulation in a function
  simulate_birthday_match3 = function(npeople, ntrials) {
    # Set up the parameters (if wanted, these could also be arguments to the function)
    ndays = 365
    size_match = 3
    year = 1:ndays
    # Run ntrials -one per column- using sample() and matrix()
    y = sample(year, npeople*ntrials, replace=TRUE)
    trials = matrix(y, nrow=npeople, ncol=ntrials)
    w = colMatches(trials, size_match)
    prob_B = mean(w)
    
    cat('npeople =', npeople, ', size_match =', size_match, 'ntrials =', ntrials, ', probability of match =', prob_B, '\n')
  }
  
  simulate_birthday_match3(30, 10000)
  simulate_birthday_match3(40, 10000)
  simulate_birthday_match3(60, 10000)
  simulate_birthday_match3(80, 10000)
  simulate_birthday_match3(90, 10000)
  simulate_birthday_match3(85, 10000)
  simulate_birthday_match3(86, 10000)
  simulate_birthday_match3(87, 10000)
  simulate_birthday_match3(87, 10000)
  simulate_birthday_match3(87, 10000)
  simulate_birthday_match3(87, 10000)
  simulate_birthday_match3(88, 10000)
  simulate_birthday_match3(88, 10000)
  simulate_birthday_match3(88, 10000)
  simulate_birthday_match3(88, 10000)
}

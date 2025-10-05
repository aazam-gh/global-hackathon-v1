#---------------------------------------------------------
# File:   MIT18_05S22_ps2_sol.r 
# Author: Jeremy Orloff
# Calculations for Pset 2
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------

do_problem_6c = T
do_problem_6d = T

#----------------------------
if (do_problem_6c) {
  cat('\n----- Problem 6c\n')
  # We write a function that we can run several times
  ave_max_run = function(nflips, ntrials) {
    total = 0 # We'll keep a running total of all the trials'longest runs
    for (j in 1:ntrials) {
      # One trial consists of nflips flips
      trial = rbinom(nflips, 1, 0.5) # binomial(1,0.5) = bernoulli(0.5)
      # rle() finds the lengths of all the runs in trials. We add the max to total
      total = total + max(rle(trial)$lengths)
    }
    # The average maximum run is the total/ntrials
    ave_max = total/ntrials 
    cat('6(c) nflips =', nflips, ', ntrials =', ntrials, ', Average max run =', ave_max, '\n')
  }
  
  # Run the simulation 3 times
  ave_max_run(50, 10000)
  ave_max_run(50, 10000)
  ave_max_run(50, 10000)
}

if (do_problem_6d) {
  compute_exact_6d = FALSE
  cat('------------\nProblem 6d\n')
  # We write a function that we can run several times
  ave_max_run = function(p, min_run, nflips, ntrials) {
    # Computes the simulated probability of a run of length min_run or greater.
    # We'll keep a running count of all the trials with a run of 9 or more
    count = 0
    for (j in 1:ntrials) {
      trial = rbinom(nflips, 1, p) #binomial(1, p) = bernoulli(p)
      count = count + (max(rle(trial)$lengths) >= min_run)
    }
    # The probability of a run of min_run or more is count/ntrials
    prob = count/ntrials
    cat('6(d) p =', p, ', min_run =', min_run, ', nflips =', nflips, ', ntrials =', ntrials, ', P(min_run) =', prob, '\n')
  }
  ave_max_run(0.5, 8, 50, 10000)
  ave_max_run(0.5, 8, 50, 10000)
  ave_max_run(0.5, 8, 50, 10000)
  
  # We have code to compute this exactly without simulation
  if (compute_exact_6d ) {
    # The following source file needs to be in the working directory
    source('MIT18_05S22_ps2_exact_prob_of_runs.r')
    p = comp_prob_run_of_heads_or_tails(0.5, 50, 8)
    cat('\nWe ran code to get the exact probability:\n')
    cat('Exact prob of runs(0.5, 50, 8)', p, '\n')
  }
  else {
    cat('\nWe have code to compute the exact probability in 6(d). You have to set compute_exact_6d to TRUE and make sure the working directory is set correctly.')
  }
}
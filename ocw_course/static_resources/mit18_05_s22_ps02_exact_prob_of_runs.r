#---------------------------------------------------------
# File:   MIT18_05S22_ps2_exact_prob_of_runs.r 
# Author: Jeremy Orloff
# Bonus code for ps2. Computes the exact probability of a run of a given length.
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------
# Compute exact probabilities of runs in flips of a coin

#-----------------------
# Random variable definitions
#  X_k = max run of heads in k flips of a coin with P(H) = p. 
#  Y_k = max run of either heads or tails in k flips of a coin
#-----------------------
# Compute:  prob_run_heads[k+1] = P(X_k >= min_run) for k=0 to N
# Index k+1 because R has 1 based arrays, so prob_run_heads[1] = X_0
comp_prob_run_of_heads = function(p, nflips, min_run, return_array=FALSE) {
  q = 1-p   # prob of tails

  # Avoid indexing issues:
  if (min_run <= 0 || nflips < min_run) {
    stop('comp_prob_run_of_heads: must have 0 < min_run <= nflips.')
  }
  
  #Initialize an array to hold all the prob.
  prob_run_heads = rep(0.0, nflips+1)  
  # The zeros in prob_run_heads are correct for k=1 ... min_run
  
    # prob_run_heads[k+1] can be computed from prob_run_heads[j] for j=1 to k + a little thought
    for (k in min_run:nflips) {
      x = 0
      for (j in 0:(min_run-1)) {
        # First T at toss j+1
        x = x + p^(j)*q*prob_run_heads[k-j]  
      }
      x = x + p^min_run  # First T at toss min_run + 1 --> have min_run of H
      prob_run_heads[k+1] = x
    }
    if (return_array)
      return(prob_run_heads)
    else 
      return(prob_run_heads[nflips+1])
 }

#---------------------
# Compute prob_run_start_T[k] = P(Y_k >= min_run | first flip is tails)
# Compute prob_run_start_H[k] = P(Y_k >= min_run | first flip is heads)
#   (Remember Y_k = run of min_run heads or tails in k flips) 
#   (Note indexing for prob_run_start_T and H)
# prob_run[k+1] = P(Y_k >= min_run)

comp_prob_run_of_heads_or_tails = function(p, nflips, min_run, return_array=FALSE) {
  q = 1-p  #prob. of tails
  
  # Avoid indexing issues:
  if (min_run < 1 || nflips < min_run) {
    stop('comp_prob_run_of_heads: must have 1 <= min_run <= nflips.')
  }
  
  # min_run = 1 is a special case
  if (min_run == 1) {
    prob_run_start_H = rep(1.0, nflips) # See definition above
    prob_run_start_T = rep(1.0, nflips) # See definition above
    prob_run = rep(1.0, nflips)
    
    if (return_array) {
      return(prob_run)
    }
    else {
      return(prob_run[nflips])
    }
  }
  
  # -- min_run > 1
  # Initialize arrays
  prob_run_start_H = rep(0.0, nflips) # See definition above
  prob_run_start_T = rep(0.0, nflips) # See definition above
  prob_run = rep(0.0, nflips) 
  # The zeros in prob_run_start_H and prob_run_start_T are correct for 1 ... min_run-1
  # The zeros in prob_run are correct for 1 ... min_run-1
  
  # Computation:
  # prob_run_start_H[k] = P(Y_k >= min_run | first toss = H)
  #  = P(first tail on toss 2 | 1st toss=H)*P(Y_(k-1) >= min_run | 1st toss T)
  #   + P(first tail on toss 3 | 1st toss=H)*P(Y_(k-2) >= min_run | 1st toss T)
  #   + P(first tail on toss 4 | 1st toss=H)*P(Y_(k-3) >= min_run | 1st toss T)
  #   ...
  #   + P(first tail on toss min_run | 1st toss=H)*P(Y_(k-min_run+1) >= min_run | 1st toss T)
  #   + P(first min_run tosss are heads | 1st toss=H)
  #  =  q*prob_run_start_T[k-1]     #Seq is HT*, run of length min_run in T* (first H given)
  #   + p*q*prob_run_start_T[k-1]   #Seq is HHT*, run of length min_run in T*    "
  #   + p^2*q*prob_run_start_T[k-2] #Seq is HHHT*, run of length min_run in T*   "
  #   + p^3*q*prob_run_start_T[k-2] #Seq is HHHHT*, run of length min_run in T*    "
  #   ...
  #   + p^(min_run-2)*q*prob_run_start_T[k-min_run+1] # Seq is H....HT* (min_run-1 H's) ...
  #   + p^(min_run-1)   # Seq is H...H* (min_run H's)
  # Likewise for prob_run_start_T[k]
  # prob_run[k] = P(Y_k >= min_run) 
  #             = P(Y_k >= min_run | 1st toss=H)*P(1st toss H)
  #              +P(Y_k >= min_run | 1st toss=T)*P(1st toss T)
  #             = prob_run_start_H[k]*p + prob_run_start_T[k]*q
  
  # This computation can be done using array arithmetic
  mrj = 0:(min_run-2)  #list of indices
  p_pow = q*(p^mrj)
  q_pow = p*(q^mrj)
  
  for (k in min_run:nflips) {
    ind = (k-1) -mrj
    xh = sum(p_pow*prob_run_start_T[ind]) + p^(min_run-1)
    xt = sum(q_pow*prob_run_start_H[ind]) + q^(min_run-1)
    prob_run_start_H[k] = xh
    prob_run_start_T[k] = xt
    prob_run[k] = p*xh + q*xt
  }
  if (return_array) {
    return(prob_run)
  }
  else {
    return(prob_run[nflips])
  }
}

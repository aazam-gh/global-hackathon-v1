#---------------------------------------------------------
# File:   mit18_05_s22_studio7-solutions.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------
# Studio 7 solutions ----

# 1. Be sure to read the instructions file!

# 2. The test answers file gives sample output and the functions we ran to get it. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# Problem 1: Significance and power/type 1 and type 2 errors
# See instructions for this studio

studio7_problem_1 = function(theta_HA, alpha, n_tosses) {
  cat("----------------------------------\n")
  cat("Problem 1: Rejection region, actual significance, power\n")

  # **** Be careful to avoid an 'off by 1 error' here.

  # Arguments:
  #   theta_HA = the probability of heads in the alternate hypothesis
  #   alpha = significance level for our significance test
  #   n_tosess = the number of tosses in one trial

  theta_H0 = 0.5 # probability of heads in the null hypothesis

  # Enforce theta_HA > theta_H0
  if (theta_HA < theta_H0) {
    warning("Problem 1: We require theta_HA > theta_H0", immediate.=TRUE)
    return()
  }

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  # This has a right-sided rejection region
  # The +1 below is because qbinom(v,n_tosses,theta) gives the smallest value so that the left tail prob is >= v
  crit_right = qbinom(1-alpha, n_tosses, theta_H0) + 1
  reject_region_right = crit_right:n_tosses
  cat("  Rejection region: ", reject_region_right,"\n")

  # For discrete distributions I find it easier to use dbinom and sum, so I don't make off by one errors with pbinom.
  actual_significance_right = sum(dbinom(reject_region_right, n_tosses,theta_H0))
  experiment_power = sum(dbinom(reject_region_right, n_tosses, theta_HA))
  cat("  True significance=", actual_significance_right,'\n')
  cat("  Power=",experiment_power,'\n')
}


#--------------------
# Problem 2: Simulation with a known prior.
# See instructions for this studio

studio7_problem_2 = function(theta_HA, alpha, n_tosses, n_trials, secret_prior) {
  cat("----------------------------------\n")
  cat("Problem 2: Simulation with a mixture of coins\n")

  # Arguments:
  #   theta_HA = the probability of heads in the alternate hypothesis
  #   alpha = significance level for our significance test
  #   n_tosses = the number of tosses in one trial
  #   n_trials = the number of trials in the simulation
  #   secret_prior = the secret prior used to pick the type of coin for each trial = c(prob. of H0, prob of HA)

  theta_H0 = 0.5

  # Enforce theta_HA > theta_H0
  if (theta_HA < theta_H0) {
    warning("Problem 1: We require theta_HA > theta_H0", immediate.=TRUE)
    return()
  }

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  # The following values will be useful
  theta_values = c(theta_H0, theta_HA)

  # We copy the code from problem 1 to get the rejection region
  crit_right = qbinom(1-alpha, n_tosses, theta_H0) + 1
  reject_region_right = crit_right:n_tosses

  # Initialize our counters
  # We count everything
  # Note: we could just count n_reject, n_H0_and_reject, n_HA_and_reject. Combined with n_trials we can deduce all of the other numbers.
  n_H0_and_reject = 0     # type 1 error
  n_H0_and_nonreject = 0  # correct decision
  n_HA_and_reject = 0     # correct decision
  n_HA_and_nonreject = 0  # type 2 error
  for (j in 1:n_trials) {
    # Choose a coin -- H0, HA coin with given prior
    theta = sample(theta_values, 1, prob=secret_prior)

    # toss the coin n_tosses times = 1 sample from binom(n_tosses, theta)
    x = rbinom(1, n_tosses, theta)
    # check if we reject
    if (x %in% reject_region_right) {
      if (theta == theta_H0) { n_H0_and_reject = n_H0_and_reject + 1 }
      else { n_HA_and_reject = n_HA_and_reject + 1 }
    }
    else { # nonreject
      if (theta == theta_H0) { n_H0_and_nonreject = n_H0_and_nonreject + 1 }
      else { n_HA_and_nonreject = n_HA_and_nonreject + 1 }
    }
  }
  n_H0 = n_H0_and_nonreject + n_H0_and_reject
  n_HA = n_HA_and_nonreject + n_HA_and_reject
  n_reject = n_H0_and_reject + n_HA_and_reject
  prob_reject_given_H0 = n_H0_and_reject/n_H0
  prob_H0_given_reject = n_H0_and_reject/n_reject
  prob_reject_given_HA = n_HA_and_reject/n_HA
  prob_HA_given_reject = n_HA_and_reject/n_reject
  prob_reject = n_reject/n_trials

  cat("  theta_HA=", theta_HA, ", alpha=", alpha, '\n', sep='')
  cat("  n_tosses=", n_tosses, ", n_trials=", n_trials, '\n', sep='')
  cat("  secret_prior=", secret_prior, '\n')
  cat("  Number of rejections: ", n_reject, '\n')
  cat("  Number of type 1: ", n_H0_and_reject, '\n')
  cat("  Number of type 2: ", n_HA_and_nonreject, '\n')
  cat("  P(rejection | H0): ", prob_reject_given_H0, '\n')
  cat("  P(H0 | rejection): ", prob_H0_given_reject, '\n')
  cat("  P(rejection | HA): ", prob_reject_given_HA, '\n')
  cat("  P(HA | rejection): ", prob_HA_given_reject, '\n')
  cat("  P(rejection): ", prob_reject, '\n')
}

#--------------------
# Problem 3: Simulation with only fair coins
# See instructions for this studio

studio7_problem_3a = function(theta_HA, alpha, n_tosses, n_trials) {
  cat("----------------------------------\n")
  cat("3a: Simulation with all fair coins: explain results\n")

  secret_prior = c(1.0, 0) # Prob of H0, prob of HA
  cat("----\n")
  cat("# Problem 2 called from problem 3a\n")
  studio7_problem_2(theta_HA, alpha, n_tosses, n_trials, secret_prior)
  cat("----\n")

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("  P(rejection | H0) = significance and we saw the simulated probability was approximately the true significance given in problem 1.", "\n\n")
  cat("  P(H0 | rejection) = 1 because we (being omniscient) know the coin is fair, so all rejections are incorrect, i.e. type 1 errors.", "\n\n")
  cat("  HA is never true, so   P(HA | rejection) = 0   and   P(rejection | HA) is undefined.", "\n")
}

studio7_problem_3b = function(theta_HA, alpha, n_tosses, n_trials) {
  cat("-----\n")
  cat("3b: Simulation with all unfair coins: explain results\n")

  secret_prior = c(0.0, 1.0) # Prob of H0, prob of HA
  cat("----\n")
  cat("# Problem 2 called from problem 3b\n")
  studio7_problem_2(theta_HA, alpha, n_tosses, n_trials, secret_prior)
  cat("----\n")

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("  H0 is never true, so P(H0 | rejection) = 0   and P(rejection | H0) is undefined.", "\n\n")
  cat("  P(rejection | HA) = power and we saw the simulated probability was approximately the true power given in 1b.", "\n\n")
  cat("  P(HA | rejection) = 1 because we (being omniscient) know the coin is always HA, so all rejections are correct.", "\n")
}

studio7_problem_3c = function() {
  cat("-----\n")
  cat("3c: Explain difference between P(H0 | rejection) and significance\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("  Significance is P(rejection | H0), which does not depend on the prior probabilities of H0 and HA. We know that this is not the same as P(H0 | rejection), which does depend on the prior probabilites. Frequentists (unless they are omniscient) don't have such priors. Problems 3a and 3b show that P(H0 | rejection) can be anywhere between 0 and 1.", '\n')
}

studio7_problem_3d = function() {
  cat("-----\n")
  cat("3d: Shout it out!\n")

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  for (j in 1:5) {
    cat("  THE SIGNIFICANCE IS NOT THE PROBABILITY OF AN ERROR GIVEN REJECTION!",'\n')
  }
  cat("  Frequentists don't compute P(Error | rejection).\n  Significance is P(rejection | H0). ", '\n')
}


#--------------------
# OPTIONAL Problem 4: Use Bayes theorem
# See instructions for this studio

studio7_problem_4 = function(theta_HA, alpha, n_tosses, prior) {
  cat("----------------------------------\n")
  cat("OPTIONAL 4: Use Bayes theorem\n")

  theta_H0 = 0.5

  # Enforce theta_HA > theta_H0
  if (theta_HA < theta_H0) {
    warning("Problem 1: We require theta_HA > theta_H0", immediate.=TRUE)
    return()
  }

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  # Bayes theorem says
  #   P(H0|reject) = P(reject|H0)*P(H0)/P(reject)
  #                = significance*P(H0)/P(reject)
  #   P(HA|reject) = P(reject|HA)*P(HA)/P(reject)
  #                = power*P(HA)/P(reject)
  # Law of total probability says
  #  P(reject) = P(reject|H0)*P(H0) + P(reject|HA)*P(HA)
  #            = significance*P(H0) + power*P(HA)

  #--------------
  #We copied and pasted the code from prolbem 1 to compute the actual significance and the power:

  # This has a left-sided rejection region
  # The +1 below is because qbinom(v,n_tosses,theta) gives the smallest value so that the left tail prob is >= v
  crit_right = qbinom(1-alpha, n_tosses, theta_H0) + 1
  reject_region_right = crit_right:n_tosses

  # For discrete distributions I find it easier to use dbinom and sum, so I don't make off by one errors with pbinom.
  actual_significance_right = sum(dbinom(reject_region_right, n_tosses,theta_H0))
  experiment_power = sum(dbinom(reject_region_right, n_tosses, theta_HA))

  #------------
  # Our variable names are too long. We alias them here to shorter names
  sig = actual_significance_right
  pw = experiment_power

  prior_H0 = prior[1]
  prior_HA = prior[2]
  total_prob_reject = sig*prior_H0 + pw*prior_HA
  true_prob_H0_given_reject = sig*prior_H0/total_prob_reject
  true_prob_HA_given_reject = pw*prior_HA/total_prob_reject
  cat("  true P(H0 | rejection): ", true_prob_H0_given_reject, '\n')
  cat("  true P(HA | rejection): ", true_prob_HA_given_reject, '\n')
}

#-----------------------------

#---------------------------------------------------------
# File:   mit18_05_s22_studio4-solutions.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 4 solutions ----

# 1. Be sure to read the instructions file!

# 2. The instruction file gives instructions for testing your code. You can see the output of the tests in studio4-test-answers.html. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#---------------------------------
# Problem 1: Barto and Axel simulated covariance and correlation. ----
# See the instructions for this studio.

# 1a: Simulated covariance and correlation.
studio4_problem_1a = function(n_together, n_Barto_alone, ntrials) {
  cat("\n----------------------------------\n")
  cat("Problem 1a. Barto and Axel simulated covariance and correlation.\n")

  # Arguments:
  #   n_together = number of games Axel and Barto play together
  #   n_Barto_alone = number of games Barto plays after playing with Axel
  #   ntrials = number of trials to run in one simulation.

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

  # There are many ways to organize this. We generate a matrix for the first n_together bets they make together and another for the bets Barto makes by himself.
  one_bet_values = c(1, -1)
  one_bet_probs = c(18/38, 20/38)

  x = sample(one_bet_values, ntrials*n_together,
             replace=TRUE, prob=one_bet_probs)
  together_data = matrix(x, ncol=ntrials)

  x = sample(one_bet_values, ntrials*n_Barto_alone,
             replace=TRUE, prob=one_bet_probs)
  alone_data = matrix(x, ncol=ntrials)

  # We use colSums to total Axel and Barto's winnings for each trial
  Axel_winnings = colSums(together_data)
  Barto_winnings = Axel_winnings + colSums(alone_data)

  # Now we use mean(), var(), cov(), cor() to
  # compute the sample statistics asked for.
  Axel_sample_mean = mean(Axel_winnings)
  Barto_sample_mean = mean(Barto_winnings)
  Axel_sample_var = var(Axel_winnings)
  Barto_sample_var = var(Barto_winnings)
  AB_sample_cov = cov(Axel_winnings, Barto_winnings)
  AB_sample_cor = cor(Axel_winnings, Barto_winnings)

  #------
  # Use the cat statements below to print your results. You will need to insert your variables names where indicated.
  cat("For n_together =", n_together, " and n_Barto_alone =", n_Barto_alone, "we have\n")
  cat("Axel's sample mean is", Axel_sample_mean, '\n')
  cat("Barto's sample mean is", Barto_sample_mean, '\n')
  cat("Axel's sample variance is", Axel_sample_var, '\n')
  cat("Barto's sample variance is", Barto_sample_var, '\n')
  cat("Their sample covariance is", AB_sample_cov, '\n')
  cat("Their sample correlation is", AB_sample_cor, '\n')
}


# 1b. Print your description of what happens as n_Barto_alone increases
studio4_problem_1b = function() {
  cat("-----\n")
  cat("1b. Describe behavior as n_Barto_alone increases\n")

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

  cat('As n_Barto_alone increases, the covariance stays roughly the same. (It only measures the n_together overlapping games.)\n')
  cat('As n_Barto_alone increases, the correlation decreases (gets closer to 0). (It takes into account the fraction of the total games played that overlap.)\n')
}

#-------------------------------------
# Problem 2: Simulated central limit theorem. ----
# This is cool and shouldn't take a lot of code.
studio4_problem_2 = function(n_bets_per_trial, ntrials) {
  cat("\n----------------------------------\n")
  cat("Problem 2. Simulated central limit theorem.\n")

  # Arguments:
  #   n_bets_per_trial = the number of bets in each trial
  #   n_trials = number of trials

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  one_bet_values = c(1, -1)
  one_bet_probs = c(18/38, 20/38)

  # As usual there are many ways to do this.
  # We'll do it with a for loop.

  #Initialize the trials array that will hold the result of each trial
  trials = rep(0, ntrials)

  for (j in 1:ntrials) {
    x = sample(one_bet_values, n_bets_per_trial,
               replace=TRUE, prob=one_bet_probs)
    trials[j] = sum(x)
  }
  # Let R choose the number of bins
  hist(trials, col='orange', freq=FALSE, main="2. Histogram of trials")

  # Add a graph of the correct normal pdf

  # We compute the theoretical mean and variance of one bet
  one_bet_mean = sum(one_bet_probs*one_bet_values)
  one_bet_var = sum(one_bet_probs*(one_bet_values - one_bet_mean)^2)

  # Computing the theoretical mean and variance of a trial is easy
  trial_mean = n_bets_per_trial*one_bet_mean
  trial_var = n_bets_per_trial*one_bet_var
  trial_sigma = sqrt(trial_var)

  # Now plot the normal curve
  x = seq(min(trials), max(trials), 0.01)
  y = dnorm(x, trial_mean, trial_sigma)
  lines(x, y, col='blue', lwd=2)

  #------
  cat('See plot\n')
}

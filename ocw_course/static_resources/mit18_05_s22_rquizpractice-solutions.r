#---------------------------------------------------------
# File:   mit18_05_s22_RQuizPractice-solutions.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# R Practice Quiz Solutions

# This is much much longer than the actual quiz.

#---------------------------------
# 0.a. Clean your space.

# The following will clean the environment
rm(list=ls())  #clear environment
cat("\014")    #clear RStudio console 


#--------------------------------------
# Problem 1:  Basics
# See instructions.

rquiz_practice_problem_1a = function(n_samples, mu, sigma) {
  cat("----------------------------------\n")
  cat("Problem 1a: sampling from a normal distribution\n")

  # Arguments:
  # n_samples = size of the sample to generate
  # mu = mean of the underlying normal distribution
  # sigma = standard deviation of the underlying normal distribution

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  x = rnorm(n_samples, mu, sigma)
  cat(x, '\n')
}

rquiz_practice_problem_1b = function(n_samples, size, theta) {
  cat("-----\n")
  cat("1b: sampling from a binomial distribution\n")

  # Arguments:
  # n_samples = size of the sample to generate
  # size =  number of Bernoulli trials
  # theta = probability of success in each Bernoulli trial

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  x = rbinom(n_samples, size, theta)
  cat(x, '\n')
}

rquiz_practice_problem_1c = function(sample_space, n_samples) {
  cat("-----\n")
  cat("1c: sampling from a list with replacement\n")

  # Arguments:
  # sample_space = the list of outcomes to sample from
  # n_samples = number of the sample to generate

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  x = sample(sample_space, n_samples, replace=TRUE)
  cat(x, '\n')
}

rquiz_practice_problem_1d = function(n) {
  cat("-----\n")
  cat("1d: Permutations\n")

  # Arguments:
  # n = the problem asks you to generate a permutation of the numbers 1:n

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  x = sample(1:n, n, replace=FALSE)
  cat(x, '\n')
}

rquiz_practice_problem_1e = function(w, x_1) {
  cat("-----\n")
  cat("1e: Plotting\n")

  # Arguments:
  # w = angular frequency (for sin(w*x) and cos(w*x))
  # x_1: plot from 0 to x_1
  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  x = seq(0, x_1, 0.01)  # If x_1 and w are reasonably sized 0.01 should be fine.
  y = sin(w*x)
  plot(x, y, type='l', col='blue', lwd=1.5)
  y = cos(w*x)
  lines(x, y, col='orange', lwd=1.5)
  abline(v=0)
  abline(h=0)
  
  title("Plot for problem 1e")
  cat('See plot: Plot for problem 1e','\n')
}

rquiz_practice_problem_1f = function(n_samples, a, b) {
  cat("-----\n")
  cat("1f: Sampling and basic statistics\n")

  # Arguments:
  # n_samples = number of samples to generate from a Uniform(a,b) distribution.
  # a = left endpoint for the Uniform(a,b) distribution.
  # b = right endpoint for the Uniform(a,b) distribution.

  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  x = runif(n_samples, a, b)
  m = mean(x)
  med = median(x)
  v = var(x)
  std_dev = sqrt(v)
  q = quantile(x, c(0.25, 0.75))
  cat("mean:", m, '\n')
  cat("median:", med, '\n')
  cat("var:", v, '\n')
  cat("sd:", std_dev, '\n')
  cat("0.25 and 0.75 quantiles for the data:", q, '\n' )
}

rquiz_practice_problem_1g = function(n_samples, a, b) {
  cat("-----\n")
  cat("1g: Sampling and basic statistics\n")

  # Arguments:
  # n_samples = number of samples to generate from a Uniform(a,b) distribution.
  # a = left endpoint for the Uniform(a,b) distribution.
  # b = right endpoint for the Uniform(a,b) distribution.
  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  x = runif(n_samples, a, b)
  y = runif(n_samples, a, b)
  z = (x+y)/2
  cv = cov(x, z)
  cr = cor(x, z)
  cat('Covariance of x an z:', cv, '\n')
  cat('Correlation of x an z:', cr, '\n')
  plot(x, y, col='blue', pch=19, cex=0.5)
  s= "Scatterplot for problem 1g"
  title(s)

  cat("See plot:",s, '\n')
}

rquiz_practice_problem_1h = function(n_samples, mu1, sigma1, mu2, sigma2, alpha, mu0, sigma0) {
  cat("-----\n")
  cat("1h: Sampling and basic statistics\n")

  # Arguments:
  # n_samples = number of samples to generate from each normal distribution.
  # mu1 = mean for the first normal distribution
  # sigm1 = standard deviation for the first normal distribution.
  # mu2 = mean for the second normal distribution
  # sigm2 = standard deviation for the second normal distribution.
  # alpha = significance level for tests
  # mu0, sigma0 are the mean and standard deviation for H0. (sigma0 is not used in the t-test comparing means)

  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

    cat('1h: n_samples=',n_samples, ', alpha=', alpha, ', mu0=', mu0, ', sigma0=', sigma0, '\n', sep='')
  cat('mu1=', mu1, ', sigma1=', sigma1, ', mu2=', mu2, ', sigma2=', sigma2, '\n', sep='')

  # These will be used several times
  rejH0 = 'Reject H0 in favor of HA:'
  no_rejH0 = 'The data does not support rejecting H0:'
  
  x1 = rnorm(n_samples, mu1, sigma1)
  x2 = rnorm(n_samples, mu2, sigma2)

  # Problem 1h(i)
  cat("---Problem 1h(i)",'\n')

  xbar = mean(x1)
  z_stat = (xbar - mu0)/(sigma0/sqrt(n_samples))
  p_value = 2*(1 - pnorm(abs(z_stat),0,1))
  cat('z_stat =', z_stat, '\n')
  cat('p_value =', p_value, '\n')
  if (p_value < alpha) {
    cat(rejH0, 'The mean is not equal to', mu0, '(assuming sigma1=', sigma0, ')\n')
  } else {
    cat(no_rejH0, 'mu1 = ', mu0, '(assuming sigma1=', sigma0, ')\n',sep='')
  }

  # Problem 1h(ii)
  cat("---Problem 1h(ii)",'\n')
  # Use t.test -- it's also easy to implement from scratch
  result = t.test(x1, mu=mu0, conf.level=1-alpha)
  #print(result)
  t_stat = result$statistic
  p_value = result$p.value  # This is how you get at this component of the result
  cat('t_stat =', t_stat, '\n')
  cat('p_value =', p_value, '\n')
  if (p_value < alpha) {
    cat(rejH0, 'The mean is not equal to', mu0, '(assuming sigma1=', sigma0, ')\n')
  } else {
    cat(no_rejH0, 'mu1 =', mu0,'\n')
  }

  # Problem 1h(iii)
  cat("---Problem 1h(iii)",'\n')
  # Use t.test -- it's also easy to implement from scratch
  result = t.test(x1, x2, var.equal=TRUE, conf.level=1-alpha)
  #print(result)
  t_stat = result$statistic
  p_value = result$p.value  # This is how you get at this component of the result
  cat('t_stat =', t_stat, '\n')
  cat('p_value =', p_value, '\n')
  if (p_value < alpha) {
    cat('Reject H0 in favor of HA: mu1 != mu2\n')
  } else {
    cat('The data does not support rejecting H0 that: mu1 = mu2\n')
  }
  
  # Problem 1h(iv)
  cat("---Problem 1h(iv)",'\n')
  
  # Use var.text -- it's also easy to implement from scratch
  result = var.test(x1, x2)
  #print(result)
  test_stat = result$statistic
  p_value = result$p.value  # This is how you get at this component of the result
  cat('test_stat =', test_stat, '\n')
  cat('p_value =', p_value, '\n')
  if (p_value < alpha) {
    cat(rejH0, 'var(x1) != var(x2)', '\n')
  } else {
    cat(no_rejH0, 'var(x1) == var(x2)', '\n')
  }
}

#--------------------------------------
# Problem 2: Bayesian update simulation.
# See instructions for this studio

rquiz_practice_problem_2 = function(prior, next_roll) {
  cat("----------------------------------\n")
  cat("Problem 2: Simulation with a mixture of dice\n")

  # Arguments:
  #   prior = prior probability for the dice choice (5 numbers, usual order)
  #   next_roll = the problem asks for the predictive probability of next_roll on the next roll.

  # Here's the code to load the likelihood table.
  rownames = c('sides = 4', 'sides = 6', 'sides = 8', 'sides = 12', 'sides = 20')
  colnames = c('x=1','x=2','x=3','x=4','x=5','x=6','x=7','x=8','x=9','x=10', 'x=11','x=12','x=13','x=14','x=15','x=16','x=17','x=18','x=19','x=20')
  standard_likelihood_table = matrix(0,nrow=5, ncol=20, dimnames=list(rownames,colnames))
  standard_likelihood_table[1,1:4] = 1/4    #4 sided die
  standard_likelihood_table[2,1:6] = 1/6    #6 sided die
  standard_likelihood_table[3,1:8] = 1/8    #8 sided die
  standard_likelihood_table[4,1:12] = 1/12  #12 sided die
  standard_likelihood_table[5,1:20] = 1/20  #20 sided die
  # print(standardLikelihoodTable)


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  dice = c(4,6,8,12,20)
  chosen_die = sample(dice, 1, prob=prior)
  roll = sample(1:chosen_die, 1)
  likelihood = standard_likelihood_table[,roll]
  bayes_numerator = prior*likelihood
  posterior = bayes_numerator/sum(bayes_numerator)

  likelihood_next = standard_likelihood_table[,next_roll]
  post_predictive_prob = sum(posterior*likelihood_next)

  cat('prior =', prior,'\n')
  cat('chosen die =', chosen_die, '\n')
  cat('roll =', roll, '\n')
  cat('posterior =', posterior, '\n')
  cat("Posterior prediction: P(", next_roll, "| data) =", post_predictive_prob, '\n')
}


#--------------------------------------
# Problem 3:  Counting
# See instructions.

rquiz_practice_problem_3a = function(n_rats) {
  cat("----------------------------------\n")
  cat("Problem 3a: counting Brass rats\n")

  # Arguments:
  # n_rats = number of Brass Rats Tim has
  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  n = choose(n_rats, 2)*2
  cat(n_rats, 'rings can be worn', n, 'different ways\n')
}

rquiz_practice_problem_3b = function(n_ways) {
  cat("----------------------------------\n")
  cat("Problem 3b: more counting Brass Rats\n")

  # Arguments:
  # n_ways = minimum number of ways Tim wants to be able to wear their Brass Rats.

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********
  
  n = 2
  while(TRUE) {
    ans = choose(n,2)*2
    if (ans >= n_ways) {
      break
    }
    n = n+1
  }
  cat('They would need at least', n, 'Brass rats.\n')
}

#--------------------------------------
# Problem 4: Polling
# See instructions.

rquiz_practice_problem_4a = function(n_students, theta_tim) {
  cat("----------------------------------\n")
  cat("Problem 4a: Plot binomial\n")

  # Arguments:
  # n_students = the number of students polled
  # theta_tim = true fraction of the student population that supports Tim

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  x = 0:n_students
  y = dbinom(x, n_students, theta_tim)
  plot(x, y, type='p', pch=20, cex=0.6, col='blue')
  s = "Plot for 4a"
  title(s)
  cat('See plot:',s,'\n')
}

rquiz_practice_problem_4b = function(n_students, n_support_tim, theta_H0, alpha) {
  cat("-----\n")
  cat("Problem 4b: NHST\n")

  # Arguments:
  # n_students = the number of students polled
  # n_support_tim = the number of who support Tim in the poll
  # theta_HO = For H0, the fraction of the student population that supports Tim
  # alpha = significance level for NHST

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  # Remember for discrete distributions we need to add or subtract 1 from the quantile.
  left_critical_point = qbinom(alpha, n_students, theta_H0) - 1
  cat("H0: Support for Tim is", theta_H0, '\n')
  cat("HA: support for Tim is less than", theta_H0, '\n')
  cat('rejection region: (left tail): x <=',left_critical_point, '\n')

  p_value = pbinom(n_support_tim, n_students, theta_H0)
  cat("p =", p_value,'\n')
  if (p_value < alpha) {
    cat('Reject H0','\n')
  } else {
    cat('Do not reject H0','\n')
  }
}


#--------------------------------------
# Problem 5: Simulation of exponential data
# See instructions.

rquiz_practice_problem_5a = function(lambda) {
  cat("----------------------------------\n")
  cat("Problem 5a: Plot exponential\n")

  # Arguments:
  # lambda = rate parameter for the exponential distribution

  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  x = seq(0, 4/lambda, 0.001)
  pdf_Y = dexp(x, rate = lambda)
  plot(x,pdf_Y,type = 'l', col='blue', lwd=1.5)
  s = "Plot for problem 5a"
  title(s)
  cat('See plot:', s, '\n')
}

rquiz_practice_problem_5b = function(lambda, n_bus_trips) {
  cat("-----\n")
  cat("Problem 5b: Simulation\n")

  # Arguments:
  # lambda = rate parameter for exponential distribution
  # n_bus_trips = the number of Hack's bus trips to simulate

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  data = rexp(n_bus_trips, lambda)
  bin_width = 1.0
  bins = seq(min(data), max(data)+bin_width, bin_width)
  
  # Problem 5b(i)
  hist(data, breaks=bins, col='yellow', freq=TRUE, main="")
  s = "Problem 5b(i): frequency histogram of data"
    title(s)
    cat('See plot:', s, '\n')

  # Problem 5b(ii)
  hist(data, breaks=bins, col='yellow', freq=FALSE, main="")
  x = seq(0, max(bins), 0.01)
  y = dexp(x, lambda)
  lines(x, y, col='blue', lwd=2)
  s = "Problem 5b(ii): density histogram of data"
  title(s)
  cat('See plot:', s, '\n')
}


#--------------------------------------
# Problem 6: Gambling: theory and simulation
# See instructions.

rquiz_practice_problem_6a = function(n_bets_per_day, n_days) {
  cat("----------------------------------\n")
  cat("Problem 6a: Theoretical mean and variance\n")

  # Arguments:
  # n_bets_per_day = number of bets Punt makes each day
  # n_days = number of days Punt plays
  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  one_bet_values = c(1,-1)
  one_bet_prob = c(18/38,20/38)
  one_bet_mu = sum(one_bet_values*one_bet_prob)
  one_bet_var = sum((one_bet_values-one_bet_mu)^2*one_bet_prob)
  cat('one_bet_mu =', one_bet_mu,'\n')
  cat('one_bet_var =', one_bet_var, '\n')

  
  one_day_mu = n_bets_per_day*one_bet_mu
  one_day_var = n_bets_per_day*one_bet_var
  cat("Expected winnings in one day =",one_day_mu, '\n')
  cat("Expected variance of one day's winning =",one_day_var, '\n')

  n_days_mu = n_days*one_day_mu
  n_days_var = n_days*one_day_var
  cat("Expected winnings in", n_days, "days =", n_days_mu, '\n')
  cat("Expected variance of", n_days, "winnings in =", n_days_var, '\n')
}

rquiz_practice_problem_6b = function(n_bets_per_day, n_days, n_trials) {
  cat("-----\n")
  cat("Problem 6b: Simulation\n")

  # Arguments:
  # n_bets_per_day = number of bets Punt makes each day
  # n_days = number of days Punt plays
  # n_trials = number of simulations of n_days of betting to run
  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  # 6b(i)
  one_bet_values = c(1,-1)
  one_bet_prob = c(18/38,20/38)
  n_bets_per_trial = n_bets_per_day * n_days

  # Generate enough bets for n_trials of n_days worth of bets
  x = sample(one_bet_values, n_bets_per_trial*n_trials,
             prob=one_bet_prob, replace=TRUE)
  # Put the data in a martrix each column is one trial of sevenDays.bets
  trials = matrix(x, nrow=n_bets_per_trial, ncol=n_trials)
  # Total winnings over n_days is given by colSums
  n_days_winnings = colSums(trials)
  
  breaks = n_bets_per_day
  hist(n_days_winnings, freq=FALSE, col='orange', breaks=breaks, main="")
  s = paste("Problem 6b(i): histogram of", n_days) 
  s = paste(s, "days winnings")
  title(s)
  
  # To draw a normal pdf we need the mean and variance
  # Here are the theoretical means and variance of one trial
  one_bet_mu = sum(one_bet_values*one_bet_prob)
  one_bet_var = sum((one_bet_values-one_bet_mu)^2*one_bet_prob)
  mu_sum = n_bets_per_trial*one_bet_mu
  # Graph a range of - 4*sigma to + 4*sigma
  sigma_sum = sqrt(n_bets_per_trial*one_bet_var)
  x = seq(mu_sum-4*sigma_sum, mu_sum+4*sigma_sum, 0.01)
  y = dnorm(x, mu_sum, sigma_sum)
  lines(x, y, col='blue', lwd=2)
  cat("See plot:", s, '\n')

  # Problem 6b(ii)

  cat("Explanation: Each experimental trial gives the sum of n_days_bets independent bets.\nOne bet has mean one_bet_mu and variance one_bet_var.\n")
  cat("So, the Central Limit Theorem, one trial is approximately normal with mean\n")
  cat("     n_bets_per_trial*one_bet_mu\n")
  cat("and variance\n")
  cat("     n_bets_per_trial*one_bet_var",'\n')
}

#--------------------------------------
# Problem 7: Using Google
# See instructions.

rquiz_practice_problem_7 = function(mit_times, harvard_times, alpha) {
  cat("----------------------------------\n")
  cat("Problem 7: Using google\n")

  # Arguments:
  # mit_times = list of times for the MIT students
  # harvard_times = list of times for the Harvard students
  # alpha = significance level for test
  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  # We googled
  #   'R Wilcoxon rank sum test'  --showed how to run the test in R
  #   'Wilcoxon rank sum test'  -- found Wikipedia article on the Mann Whitney test, which said that Mann Whitney is another name for the Wilcoxon rank sum test. This article explained the test and gave us a way to formulate the results.

  result = wilcox.test(mit_times, harvard_times, alternative="two.sided")
  p_value = result$p.value
  stat = result$statistic

  mit_median = median(mit_times)
  harvard_median = median(harvard_times)
  cat('H0: the median speeds of the two schools is the same.\n')
  cat('HA: the median speeds of the two schools are different.\n')
  cat('The median times for MIT and Harvard were', mit_median, 'and', harvard_median, 'respectively.\n')
  cat('test stat =', stat,'\n')
  cat('p_value =', p_value, '\n')
  if (p_value < alpha) {
    cat('Reject H0: the median speeds of the two schools differed at significance level', alpha, '\n')
    if (mit_median < harvard_median) {
      cat('Since the median MIT time is less than that of Harvard, it appears that MIT is the faster school.\n')
    } else {
      cat('Since the median MIT time is greater than that of Harvard, it appears that MIT is the slower school.\n')
    }
  }else {
    cat('Do not reject H0. The test is not able to distinguish between the median speeds of the two schools.','\n')
  }
}

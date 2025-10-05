#---------------------------------------------------------
# File:   mit18_05_s22_studio8-solutions.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 8 solutions ----

# 1. Be sure to read the instructions file!

# 2. The test answers file gives sample output and the functions we ran to get it. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# Summary of questions
# 1. Simulation of the f-statistic: draw histogram and graph
# 2. z-test. Code by hand a z-test
# 3. Chi-square test for independence
# 4. ANOVA. using aov()

#--------------------------------------
# Problem 1: Simulate F statistic
# See instructions for this studio

studio8_problem_1 = function(n, m, mu, sigma, n_trials) {
  cat("----------------------------------\n")
  cat("Problem 1: Simulate F statistic\n")

  # Arguments
  # n = number of groups
  # m = number of data points in each group (same for all groups)
  # mu = common mean: same for each group
  # sigma = common standard deviation (same for each group)
  # n_trials = number of trials to run in the simulation
  

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

    # Compute f statistics
  fstats = rep(0, n_trials)
  for (k in 1:n_trials) {
    data = matrix(rnorm(m*n, mu, sigma), nrow=m, ncol=n)
    group_means = colMeans(data)
    # Because there is no colVars function we use a loop to compute the group vars
    # Note: var() computes the sample variance
    group_vars = rep(0, n)
    for (j in 1:n) {
      group_data = data[,j]
      group_vars[j] = var(group_data)
    }
    MSB = m*var(group_means)
    MSW = mean(group_vars)
    fstats[k] = MSB/MSW
  }
  
  # Plot density histogram and pdf
  x = seq(0, 8, 0.01)
  y = df(x, n-1, n*(m-1))
  ymax = max(y)
  binwidth = 0.2
  breaks = seq(0,8,binwidth)
  hist(fstats, breaks=breaks, freq=F, ylim=c(0,1.1*ymax))
  lines(x, y, col='orange', lwd=2)

  cat("See plots\n")
}

#--------------------------------------
# Problem 2: Code z-test by hand
# See instructions for this studio

studio8_problem_2 = function(data, mu0, known_sigma, alpha) {
  cat("----------------------------------\n")
  cat("Problem 2: Code z-test by hand\n")

  # Arguments
  # data = list of sample values
  # mu0 = hypothesized mean in the null hypothesis
  # known_sigma = known value of the standard deviation
  # alpha = significance for the test

  
  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  n = length(data)
  xbar = mean(data)

  # Compute standardized mean for test statistic
  z = (xbar-mu0)/(known_sigma/sqrt(n))
  # Test is two-sided, so we compute a two-sided p-value
  p_value = 2*(1-pnorm(abs(z)))
  cat('mean of data =', xbar,'\n')
  cat('z =', z, '\n')
  cat('p =', p_value, '\n')

  if (p_value <= alpha) {
    cat('Because the p-value', p_value, '<', alpha, ', the data supports rejecting the null hypothesis in favor of the alternative.\n')
  }
  else {
    cat('Because the p-value', p_value, '>', alpha, ', the data does not support rejecting the null hypothesis in favor of the alternative.\n')
  }
}

#--------------------------------------
# Problem 3: Chi-square test for independence
# See instructions for this studio

studio8_problem_3 = function(data_table_file, alpha) {
  cat("----------------------------------\n")
  cat("Problem 3: Chi-square test for independence\n")

  # Arguments
  # data_table_file = data file containing a two-by-two contingency table
  # alpha = significance for the test
  
  contingency_tbl = read.table(data_table_file)
  print(contingency_tbl)

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("---Using chisq.test\n")
  # correct=FALSE turns off the 'continuity correction'. We do this so chisq.test will give the same answer as our calculation by hand.
  results = chisq.test(contingency_tbl, correct=FALSE)
  print(results)
  p_value = results$p.value

  if (p_value <= alpha) {
    cat('The p-value ', p_value, ' is less than or equal to ', alpha, ', so at significance level ', alpha, ' we should reject H0 in favor of the alternative that the levels of marriage and education are not independent.\n')
  } else {
    cat('The p-value ', p_value, ' is greater than ', alpha, ', so at significance level ', alpha, ' the data does not support rejecting H0, that the levels of marriage and education are  independent.\n')
  }

  cat("----------\n")
  cat("Same calculation by hand.\n")
  n_married_once = sum(contingency_tbl[,1])
  n_married_more = sum(contingency_tbl[,2])
  n_total = sum(contingency_tbl)
  col_probs = colSums(contingency_tbl)/n_total
  row_probs = rowSums(contingency_tbl)/n_total
  expected = data.frame(row_probs*col_probs[1]*n_total, row_probs*col_probs[2]*n_total, row.names=rownames(contingency_tbl))
  colnames(expected) = colnames(contingency_tbl)
  x2 = sum((contingency_tbl -expected)^2/expected)
  df = 1 # Given the marginal counts as soon as one cell is filled the others are determined
  p_value = 1-pchisq(x2,df)
  cat('Test stat X^2 =', x2, '\n')
  cat('p-value =', p_value, '\n')
}

#--------------------------------------
# Problem 4: ANOVA using aov()
# See instructions for this studio

studio8_problem_4 = function(T1, T2, T3, alpha) {
  cat("----------------------------------\n")
  cat("Problem 4: ANOVA using aov()\n")

  # Arguments
  # T1, T2, T3 = 3 sets of data. All the same length
  # alpha = significance of the test
  
  # Put the data into a data.frame.
  # Data frames are tables. We make columns for procedure and pain
  procedure = c(rep('T1',length(T1)),rep('T2',length(T2)),rep('T3',length(T3)))
  pain = c(T1,T2,T3)
  pain_data = data.frame(procedure, pain)

  #Look at the print out. Notice how the frame has columns procedure and pain. The ANOVA test will be to see if there is a difference between the average pain from each procedure.
  print(pain_data)

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  # Do the analysis of variance
  # pain~procedure is a 'linear formula'
  # pain is the 'response' variable --this goes on the left
  # procedure is the 'explanatory' variable this goes on the right.
  # aov() tests for whether some of the differences in the pain can be attributed to the procedure. (Our Null hypothesis is that all the tests are equal, so none of the difference in pain is explained by the proocedure.
  aov_results = aov(pain~procedure, data=pain_data)

  cat("\nSummary:\n") #show summary
  print(summary(aov_results))

  # To actually get the p-value is a bit of an ordeal. The sample code shows two ways to do it. We'll use one of them.
  s = summary(aov_results)
  a = s[[1]]$'Pr(>F)'  # This is potentially a list
  p_value = a[1]
  cat('\np-value:', p_value,'\n')

  if (p_value <= alpha) {
    cat('The p-value ', p_value, ' is less than or equal to ', alpha, '. So, at significance level ', alpha, ' we should reject H0 in favor of the alternative that the levels of pain in the different treatments are not all the same.\n')
  }
  else {
    cat('The p-value ', p_value, ' is greater than ', alpha, '. So, at significance level ', alpha, ' the data does not support rejecting H0, that the levels of pain in the different treatments are all the same.\n')
  }
}

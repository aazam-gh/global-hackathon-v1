#---------------------------------------------------------
# File:   MIT18_05S22_class19.r 
# Author: Jeremy Orloff
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------
# Class 19 examples of computing p-values using R
#
#----------------------------
# Flags
do_ztest_of_mean = TRUE
do_one_sample_ttest = TRUE
do_two_sample_ttest = TRUE
do_paired_two_sample_ttest = TRUE
do_oneway_anova_test_using_aov = TRUE
do_oneway_anova_test_using_oneway.test = TRUE
do_oneway_anova_test_by_hand = TRUE
do_simple_chisquare_test = TRUE
do_chisquare_test = TRUE

div_line = "\n-------------------\n"

if (do_ztest_of_mean) {
  # z-test of the mean
  cat(div_line, "z-test of the mean\n")
  cat("Assumptions:\n")
  cat("  data: one sample x_1, ..., x_n\n")
  cat("  x_i are independently drawn from N(mu, sigma^2)\n")
  cat("  mu is unknown, sigma^2 is KNOWN\n")
  cat("H0 is mu = mu0\n")
  cat("HA can be left, right or two-sided.\n")
  cat("Test stat z = standardized sample mean\n")
  cat("(Standardization uses known variance.)\n")
  cat("Run example (see code for details):\n")
  x = c(12, 4, 8, 6, 11, 8, 10, 9, 5, 7, 11, 3)  # Data
  var_x = 8.5    # Known variance of distribution   generating the data
  mu0 = 5   # Null hypothesis for mean
  
  n = length(x)
  m = mean(x)
  z = (m - mu0)/(sqrt(var_x/n))
  p.right = 1 - pnorm(z, 0, 1)  # rejection region in right tail
  p.left = pnorm(z, 0, 1)     # rejection region in left tail
  p.twosided = 2*min(p.right, p.left)
  # Alternatively: p.twosided = 2*(1 - pnorm(abs(z), 0, 1))
  cat('right-sided p =', p.right, '\nleft-sided p =', p.left, '\ntwo-sided p =', p.twosided,'\n')
}

if (do_one_sample_ttest) {
  # One sample t-test of the mean
  cat(div_line, "One sample t-test of the mean.\n")
  cat("Assumptions:\n")
  cat("  data: one sample x_1, ..., x_n\n")
  cat("  x_i are independently drawn from N(mu, sigma^2)\n")
  cat("  mean mu and variance sigma^2 are unknown\n")
  cat("H0 is mu = mu0\n")
  cat("HA can be left, right or two-sided.\n")
  cat("Test stat t = studentized sample mean\n")
  cat("(Studentization uses estimated variance variance.)\n")
  cat("Run example using the function t.test().\n")
  cat("We run three versions: two-sided, right-sided, left-sided\n")
  cat("(see code for details):\n")
  
  x = c(12, 4, 8, 6, 11, 8, 10, 9, 5, 7, 11, 3)  # Data
  mu0 = 0   # Null hypothesis for mean
  # t.test returns p values and some other things
  result = t.test(x, mu=mu0, alternative='two.sided')  # two-sided
  print(result)
  result = t.test(x, mu=mu0, alternative='greater') # rej. region = right tail
  print(result)
  result = t.test(x, mu=mu0, alternative='less') # rej. region = left tail
  print(result)
  cat("Same two sided test computed by hand\n")
  n = length(x)
  df = n-1
  m = mean(x) #sample mean
  s = sd(x) #sample std dev
  t = (m - mu0)/(s/sqrt(n))
  if (t > 0) {
    p = 2*(1 - pt(t, df))
  }
  else {
    p = 2*pt(t, df)
  }
  cat("t = ", t, "df = ", df, "p-value =", p,"\n")
  cat("alternative hypothesis: true mean is not equal to", mu0,"\n")
}

if (do_two_sample_ttest) {
  # Two sample t-test of the mean
  cat(div_line, 'Two sample t-test of the mean\n')
  cat("Assumptions:\n")
  cat("  two samples x_1, ..., x_n and y_1, ..., y_m\n")
  cat("  x_i drawn from N(mu1, sigma1^2\n")
  cat("  y_j drawn from N(mu2, sigma2^2\n")
  cat("  all x_i, y_j are independently drawn\n")
  cat("There is a version of the test for when the (unknown) variances are assumed equal and one for when they might be different.\n")
  cat("H0 is mu1 = mu2\n")
  cat("HA can be left (mu1 < mu2), right (mu1 > mu2) or two-sided (mu1 != mu2).\n")
  cat("Test stat t = studentized difference of sample means\n")
  cat("(Studentization uses estimated pooled variance.)\n")
  cat("Note. The test allows for H0 to be mu1-mu2 = mu0. But most often mu0 = 0.")
  cat("Run example using the function t.test().\n")
  cat("We run two versions: two-tailed with equal variances, two-tailed with unequal variances.\n")
  cat("(see code for details):\n")
  
  x = c(5, 6, 8, 5, 6, 2)
  y = c(12, 4, 8, 6, 11, 8, 10, 9, 5, 7, 11, 3)
  delta_mu = 5.2  # test if the means of the underlying distributions are equal
# Equal variances
  cat('---- With equal variances')
  result = t.test(x, y, alternative="two.sided", mu=delta_mu, paired=FALSE, var.equal=TRUE)
  print(result)

  # Unequal variances
  cat('---- With unequal variances')
  result = t.test(x, y, alternative="two.sided", mu=delta_mu,  paired=FALSE, var.equal=FALSE)
  print(result)
}

if (do_paired_two_sample_ttest) {
  # Paired two sample t-test --must have x and y the same length
  cat(div_line, 'Paired two-sample t-test: cigarettes and platelets')
  cat("Assumptions:\n")
  cat("  paired samples x_1, ..., x_n and y_1, ..., y_n\n")
  cat("  (note the number of draws is the same in each sample\n")
  cat("  x_i independently drawn from N(mu1, sigma1^2\n")
  cat("  y_j independently drawn from N(mu2, sigma2^2\n")
  cat("  if i != j then x_i and y_j are independently drawn\n")
  cat("  we do not assume x_i and y_i are independent\n")
  cat("H0 is mu1 = mu2\n")
  cat("HA can be left (mu1 < mu2), right (mu1 > mu2) or two-sided (mu1 != mu2).\n")
  cat("Let w_i = x_i-y_i\n")
  cat("Run a one-sample t-test on w_i\n")
  cat("Note. The test allows for H0 to be mu1-mu2 = mu0. But most often mu0 = 0.")
  cat("Test stat: t = studentized mean of the w\n")
  cat("(Studentization uses estimated variance of w.\n")
  cat("Run example using the function t.test().\n")
  cat("We run a two-sided example.\n")
  cat("(see code for details):\n")

  # Example on cigarette smoking and platelet aggregation
  before_cig = c(25, 25, 27, 44, 30, 67, 53, 53, 52, 60, 28)
  after_cig = c(27, 29, 37, 56, 46, 82, 57, 80, 61, 59, 43)
  mu0 = 0
  result = t.test(after_cig, before_cig, alternative="two.sided", mu=mu0, paired=TRUE)
print(result)

  cat("***\nIn this case, we get identical results with a one-sample t-test of the differences.\n***")
  result = t.test(after_cig-before_cig, alternative="two.sided", mu=mu0)
  print(result)
}

if (do_oneway_anova_test_using_aov) {
  # One way ANOVA test for equal means using aov()
  cat(div_line, 'One way ANOVA for equal means')
  cat("Assumptions:\n")
  cat("  n samples of size m\n")
  cat("    x_1,2, ..., x_1,m\n")
  cat("    x_2,2, ..., x_2,m\n")
  cat("         .....\n")
  cat("    x_n,2, ..., x_n,m\n")
  cat("  x_i,j drawn from N(mu_i, sigma^2\n")
  cat("  all x_i,j are independently drawn\n")
  cat("  sigma is the same in all samples\n")
  cat("H0 is all mu_i are the same\n")
  cat("HA is not all mu_i are the same\n")
  cat("Test stat f. Computed so that, assuming H0 it follows an F distribution.\n")
  cat("Run example using the function aov().\n")
  cat("(see code for details):\n")
  #DATA
  T1 = c(2, 4, 1, 5, 3)
  T2 = c(3, 4, 6, 1, 4)
  T3 = c(2, 1, 3, 3, 5)

  procedure = c(rep('T1',length(T1)), rep('T2',length(T2)),
  	        rep('T3',length(T3)))
  pain = c(T1, T2, T3)
  data.pain = data.frame(procedure,pain)
  print(data.pain)
  aov.data = aov(pain~procedure, data=data.pain)  #do the analysis of variance
  s = summary(aov.data)
  print(s) #show the summary table
  # Here's how you get the pvalue out of the summary.
  a = s[[1]]$'Pr(>F)'   # This is potentially a list
  pvalue = a[1]
  cat('pvalue = ', pvalue, '\n')
  # Can also do s2 = anova(aov.data), pvalue = s2$'Pr(>F)'[1]

  #report the means and the number of subjects/cell
  print(model.tables(aov.data, "means"), digits=3)        
  
  boxplot(pain~procedure, data=data.pain)#graphical summarydatafilename
  cat("See box plot\n")
}

if (do_oneway_anova_test_using_oneway.test) {
  cat('\nRepeat previous example using oneway.test\n')
  #DATA
  T1 = c(2, 4, 1, 5, 3)
  T2 = c(3, 4, 6, 1, 4)
  T3 = c(2, 1, 3, 3, 5)

  procedure = c(rep('T1',length(T1)),rep('T2',length(T2)),rep('T3',length(T3)))
  pain = c(T1, T2, T3)
  data.pain = data.frame(procedure,pain)
  print(data.pain)
  result = oneway.test(pain~procedure,data=data.pain, var.equal=TRUE)  
  print(result)
  cat("p-value = ", result$p.value, '\n')
}

if (do_oneway_anova_test_by_hand) {
  cat('\nRepeat previous example, computing by hand\n')
  #DATA
  T1 = c(2, 4, 1, 5, 3)
  T2 = c(3, 4, 6, 1, 4)
  T3 = c(2, 1, 3, 3, 5)

  # Make sure all the groups are the same size
  if (length(T1) != length(T2) || length(T2) != length(T3)) {
    stop("lengths are not equal", call. = F)
  }
  m = length(T1)
  n = 3
  group.means = c(mean(T1), mean(T2), mean(T3))
  grand_mean = mean(group.means)
  group.vars = c(var(T1), var(T2), var(T3))
  between_group_var = m*var(group.means)
  ave_within_group_var = mean(group.vars)
  fstat = between_group_var/ave_within_group_var
  p = 1 - pf(fstat, n-1,n*(m-1))
  cat('group.means', group.means, '\n') #Should match previous answers
  cat('fstat =', fstat, 'p =', p, '\n')    #Should match previous answers
}

if (do_simple_chisquare_test) {
  # First chi-square example
  cat(div_line, 'chi-square: compute by hand\n')
  observed = c(3, 10, 15, 13, 7, 3)
  null_probs= c(dbinom(0:4, 8, 0.5), sum(dbinom(5:8, 8, 0.5)))
  n = sum(observed)
  expected = n*null_probs
  df = length(observed) - 1
  X2 = sum((observed-expected)^2/expected)
  G = 2*sum(observed*log(observed/expected))
  p.X2 = 1 - pchisq(X2,df)
  p.G = 1 - pchisq(G,df)
  cat('null probs:', null_probs,'\n')
  cat('expected:', expected,'\n')
  cat('n, X2, G, p.X2, p.G:', n, X2, G, p.X2, p.G, '\n')
}

if (do_chisquare_test) {
  # Chi-square for Mendel data
  cat(div_line, "Chi-square test on Mendel data\n")
  prob = c(9, 3, 3, 1)/16
  observed = c(315, 108, 102, 31)
  cat("First: using R function chisq.test")
  result = chisq.test(observed, p=prob)
  print(result)

  cat("Second: computing by hand using both the likelihood ratio statistic and the Pearson chi square statistics.\n")
  n = sum(observed)
  df = length(observed) -1
  expected = n*prob
  cat('Observed =', observed, '\nexpected', expected,'\n')
  like_ratio_stat = 2*sum(observed*log(observed/expected))
  p.like_ratio = 1 - pchisq(like_ratio_stat,df)
  cat('likelihood ratio stat =', like_ratio_stat, '\nlikelihood ratio p-value =', p.like_ratio,'\n')

  X2.pearson = sum((observed-expected)^2/expected)
  p.pearson = 1- pchisq(X2.pearson,df)
  cat('X2 stat =', X2.pearson, '\nX2 p-value =', p.pearson,'\n')
}

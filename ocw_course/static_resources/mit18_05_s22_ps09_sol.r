#---------------------------------------------------------
# File:   MIT18_05S22_ps9_sol.r
# Author: Jeremy Orloff
# Calculations for Pset 9
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms}.
#
#---------------------------------------------------------

# Flags ----
do_problem_2a = 1 # 0 to skip, 1 to do, 2 to save figure 
do_problem_2b = 1 # 0 to skip, 1 to do, 2 to save figure 
do_problem_3 = T
do_problem_4 = T
do_problem_5 = T
do_problem_6 = T

if (do_problem_2a > 0) {
  cat('----- Problem 2a ----\n')
  alpha = 0.05
  theta0 = 0.5
  n_tosses = 12
  n_heads = 3
  critical_value_left = qbinom(alpha, n_tosses, theta0) - 1
  cat('Critical value = ',critical_value_left, '\n')
  true_alpha = pbinom(critical_value_left, n_tosses, theta0)
  cat('True significance = ', true_alpha, '\n')
  
  # Make the figure
  make_pdf = F
  # This path must be valid from the working directory
  fname = '../../pdftex/img/ps9sol-2a-ocw2022.pdf'
  if (do_problem_2a == 2) {
    make_pdf = T
    wd = 4.5
    ht = 3
    scale = .8
    pdf(fname, width=wd*scale,height= ht*scale)
    cat('--- 2a. Saving figure ---\n')
  }
  opar = par(mar=c(3.5,2.5,.4,.3))
  x = 0:n_tosses
  y = dbinom(x, n_tosses, theta0)
  plot(x,y, pch=19, cex=.8, col='blue', ylab='', mgp=c(1.7,1,0))
  #axis(1,pos=0,at=0:20)
  lines(c(0, critical_value_left),c(-.004,-.004), lwd=4, col='orange')
  if (make_pdf) {
    make_pdf = FALSE
    dev.off()
  }
  par(opar)
}

if (do_problem_2b > 0) {
  cat('--- Problem 2b ---\n')
  # The following computes probabilities by hand 
  # x = #tails before the 5th head, prob of heads = .5
  alpha = 0.05
  theta0 = 0.5
  n_heads = 3
  x = 0
  
  critical_value_right = qnbinom(1-alpha, n_heads, theta0)  + 1
  true_sig = 1 - pnbinom(critical_value_right-0.5, n_heads, theta0)
  cat('Critical value = ',critical_value_right, '\n')
  cat('True significance = ', true_sig, '\n')
  
  # Just for kicks, we'll test that these values are right by computing the probabilities by hand
  
  tot = 0
  done = FALSE
  while (!done) {
    tot = tot + choose(x+2,2)*theta0^n_heads*(1-theta0)^x
    if (tot > 1-alpha) {
      test_CP = x+1
      done = TRUE
    }
    x = x+1
  }
  test_true_sig = 1-tot
  if (test_CP != critical_value_right) {
   stop('Discrepancy in the values of the critical values') 
  }
  if (test_true_sig != true_sig) {
    stop('Discrepancy in the values of true significance') 
  }

  # 1b Make the figure ----
  make_pdf = F
  # This path must be valid from the working directory
  fname = '../../pdftex/img/ps9sol-2b-ocw2022.pdf'
  if (do_problem_2b == 2) {
    make_pdf = T
    wd = 4.5
    ht = 3
    scale = .8
    pdf(fname, width=wd*scale, height=ht*scale)
    cat('--- 2b. Saving figure ---\n')
  }
  opar = par(mar=c(3.5,2.5,.4,.3))
  x = 0:20
  y = dnbinom(x, n_heads,theta0)
  plot(x, y, pch=19, cex=.8, col='blue', ylab='', mgp=c(1.7,1,0))
  #axis(1,pos=0,at=0:20)
  lines(c(critical_value_right,22), c(-.004,-.004), lwd=4, col='orange')
  if (make_pdf) {
    make_pdf = FALSE
    dev.off()
  }
  par(opar)
}

if (do_problem_3) {
  cat('\n----- Problem 3 ----\n')
  x = c(1.76, -2.28, -0.56, 1.46, 0.59, 1.26, -1.94, -0.79, -0.86, -1.41,2.07, 1.30)
  n = length(x)
  sigma2 = 1
  s2 = var(x)
  X2 = (n-1)*s2/sigma2
  p = 1 - pchisq(X2, n-1)
  cat('s2 =', s2, 'X2 =', X2, ', p =', p, '\n')
}

if (do_problem_4) {
  cat('\n----- Problem 4 ----\n')
  benford = c(.301,.176,.125,.097,.079,.067,.058,.051,.046)
  n = 100
  # Generate data from a modified benford
  do_generate_new_data = FALSE
  if (do_generate_new_data) {
    a = 0.1
    probs = (benford + a)/sum(benford + a)
    x = sample(1:9, n, replace=T, prob=probs)
    observed = table(x)
  } else { 
    observed = c(7,13,12,9,9,13,11,10,16)
    #observed = c(23,16,3,12,13,6,10,9,8)
  }
  cat('observed:',observed,'\n')
  # Run a chi square test for goodness of fit
  df = 8
  n = sum(observed)
  expected = benford*n
  cat('expected:',expected,'\n')
  G = 2*sum(observed*log(observed/expected))
  X2 = sum((observed-expected)^2/expected)
  pG = 1-pchisq(G,df)
  pX2 = 1-pchisq(X2,df)
  cat('G =',G, 'p =', pG, '\n')
  cat('X2 =',X2, 'p =', pX2, '\n')

  # Redo calculation using chisq.test
  # To get the exact same numbers we have to set correct=FALSE. (correct=TRUE does a continuity correction.)
  res = chisq.test(observed, p=benford, correct=FALSE)
  print(res)
}

if (do_problem_5) {
  cat('----- Problem 5 ----\n')
  cat('-- Problem 5a: F-test using var.test --\n')
  #4a ----
  x = c(-0.802, 0.457, 0.972, 0.044, 0.318, -1.380, 0.111, 
    -0.023, -0.700, -1.977, -0.497, 1.471, -1.314, -0.078,
    -0.505,  0.583,  1.363, -1.863,-2.105, 0.488)
  
  y = c(9.019, 9.852, 7.947, 9.465, 10.060, 10.508, 9.506,
        9.540,10.218, 9.407, 11.455,11.422, 7.698,  9.972,
        10.928, 11.577, 10.376, 8.605, 9.347, 10.715)
  
  # The following was from spring 2022
  #x = c(0.031, -0.453, -1.210,  1.210, 0.376,  0.524, 0.599,
  #      + -0.069,  -0.813,  -0.631, -0.735,  2.665, 1.167, -1.293, 
  #      +  0.029, -0.573,  -2.498, -0.464,  0.635,  1.416)

  #y = c(8.973, 9.817,  9.387,  10.734, 8.288, 10.029, 9.840,
  #      + 9.525, 8.995, 10.296, 9.131,9.891, 10.157,  9.943,
  #      + 9.773, 10.441, 10.598,9.777, 11.525,  9.943)
  vt = var.test(x, y, alternative='two.sided')
  print(vt)
  
  cat('-- Problem 5b --\n')
  #5b Repeat 5a doing calulation by hand ----
  n = length(x)
  m = length(y)
  s2x = var(x)
  s2y = var(y)
  fstat = s2x/s2y
  
  p = 2*min( pf(fstat,n-1,m-1), 1-pf(fstat,n-1,m-1) )
  cat('var(x)', s2x, ', var(y)', s2y,'\n')
  cat('fstat =', fstat, ', p =', p, '\n')
}

if (do_problem_6) {
  cat('----- Problem 6 ----\n')
  mns = c(1.32, 1.26, 1.53, 1.39)
  v = c(0.56, 0.80, 0.93, 0.82)
  m = 351  # number of samples per group
  n = length(mns) # number of groups
  msb = m*var(mns)  # between group variance
  msw = mean(v) # within group variance
  fstat = msb/msw
  df1 = n-1
  df2 =  n*(m-1);
  p = 1 - pf(fstat, df1,df2)

  cat('fstat =', fstat, '\n')
  cat('p =', p, '\n')

  #Check using aov ----
  #fake the data
  m = 351
  fd = rnorm(m,0,1)
  fake_data = (fd-mean(fd))/sqrt(var(fd)) #mean 0 variance 1 data
  x = c()
  for (j in 1:4) {
    x0 = c(rep(mns[j],m)+sqrt(v[j])*fake_data)
    x = c(x, x0)
  }
  fact = c(rep('clean',m), rep('5-day',m), rep('10-day',m), rep('full',m))
  d = data.frame(fact, x)
  res = aov(x~fact, data=d)
  print(summary(res))

  # 10-day vs others by hand
  m = 351
  basej = 3
  for (j in 1:4) {
    ind = c(j, basej)
    n = length(ind) #two groups at a time  
    msb = m*var(mns[ind])  # between group variance
    msw = mean(v[ind]) # within group variance
    fstat = msb/msw
    df1 = n-1
    df2 =  n*(m-1);
    p_fstat = 1 - pf(fstat, df1,df2)
   
    #tstat
    sxy = sqrt((v[j]+v[basej])/m)
    tstat = (mns[basej]-mns[j])/sxy
    df = 2*m-2
    p_t_one_sided = 1-pt(abs(tstat),df)
    p_t_two_sided = 2*p_t_one_sided
    s = sprintf("%.5f, %.7f, %.5f, %.5f, %.5f, %.5f",
                 tstat, p_t_one_sided, fstat, p_fstat, tstat^2, p_t_two_sided)
    print(s)
  }
}


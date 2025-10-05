#---------------------------------------------------------
# File:   mit18_05_s22_studio5-solutions.r 
# Authors: Jeremy Orloff and Jennifer French
#
# MIT OpenCourseWare: https://ocw.mit.edu
# 18.05 Introduction to Probability and Statistics
# Spring 2022
# For information about citing these materials or our Terms of Use, visit:
# https://ocw.mit.edu/terms.
#
#---------------------------------------------------------
# Studio 5 solutions ----

# 1. Be sure to read the instructions file!

# 2. The instruction file gives instructions for testing your code. You can see the output of the tests in studio5-test-answers.html. You should use this to verify your code is giving correct output in the format asked for.

# 3. The handouts section (right side) of our MITx course page has a link for uploading you work.

#--------------------------------------
# Setup ----
# We have five types of dice: 4, 6, 8, 12, 20 sided.
# There is a prior distribution of the quantity of each die.
# One die is chosen at random and rolled repeatedly.
# Our job is to figure out which type of die was chosen.

DICE = c(4, 6, 8, 12, 20) # Fixed for the entire studio
DICE_TYPES = c('D4', 'D6', 'D8', 'D12', 'D20')

#-------------------------------
# Problem 0: List hypotheses and outcomes. ----
# See the instructions for this studio.

# 0a: List hypotheses.
studio5_problem_0a = function() {
  cat("\n----------------------------------\n")
  cat("Problem 0a: List hypotheses.\n")

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

  print("The hypotheses are that the die has 4, 6, 8, 12, or 20 sides. Let's call these H4, H6 etc.")
}


# 0b: List outcomes.
studio5_problem_0b = function() {
  cat("-----\n")
  cat("0b. List outcomes.\n")

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

  cat("The possible outcomes are the numbers 1 to 20.\n")
}


# 0c: Print likelihood table for one roll. ----
studio5_problem_0c = function() {
  cat("-----\n")
  cat("0c. Print likelihood table for one roll.\n")

  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

  # Build likelihood table: 5 dice x 20 possible outcomes
  # First create the table with all 0s
  likelihood_table = matrix(0, nrow=5, ncol=20)
  rownames(likelihood_table) = DICE_TYPES
  colnames(likelihood_table) = 1:20

  # Make the probabilities (rows) for each die separately.
  # We could have done this in a loop
  likelihood_table[1,1:4] = 1/4    #4 sided die
  likelihood_table[2,1:6] = 1/6    #6 sided die
  likelihood_table[3,1:8] = 1/8    #8 sided die
  likelihood_table[4,1:12] = 1/12  #12 sided die
  likelihood_table[5,1:20] = 1/20  #20 sided die

  cat("The rows in the table are hypotheses; the columns are outcomes. The table is printed out below.\n")
  print(likelihood_table, digits=3)
}

#---------------------------
# Problem 1: Updating ----

# 1a: Run studio5_sample_code_example_2.
studio5_problem_1a = function() {
  cat("\n----------------------------------\n")
  cat("Problem 1a. Run example 2.\n")

  # Nothing more to do.
  cat("Read the code and looked at the plots.\n")
}


# Problem 1b: Updates, bar plots, stacked bar plot.
studio5_problem_1b = function(prior,
                              nrolls,
                              plot_individual_posteriors=FALSE) {
  cat("-----\n")
  cat("1b. Updates, bar plots, stacked bar plot.\n")

  # Arguments:
  #  prior = prior probilities for the type of die use to generate data
  #  nrolls = number of rolls to simulate
  #  plot_individual_posteriors = whether or not to make individual bar charts

  # For this problem force the chosen die to be 8-sided
  random_die = 8
  data_rolls = sample(1:random_die, size=nrolls, replace=TRUE)

  cat('The initial prior is\n')
  print(prior, digits=4)
  cat('nrolls =', nrolls, '\n')
  
  # Do not change the above code.
  # ********* YOUR CODE BELOW HERE ***********

  # Build likelihood table: 5 dice x 20 possible outcomes
  # First create the table with all 0s
  likelihood_table = matrix(0, nrow=5, ncol=20)
  rownames(likelihood_table) = DICE_TYPES
  colnames(likelihood_table) = 1:20

  # Make the probabilities (rows) for each die separately.
  # We could have done this in a loop
  likelihood_table[1,1:4] = 1/4    #4 sided die
  likelihood_table[2,1:6] = 1/6    #6 sided die
  likelihood_table[3,1:8] = 1/8    #8 sided die
  likelihood_table[4,1:12] = 1/12  #12 sided die
  likelihood_table[5,1:20] = 1/20  #20 sided die

  # Initialize matrix whose jth column will store the posterior distribution after updating the jth roll. We will use this to make a nifty stacked bar plot at the end
  posterior_mat<-matrix(NA, nrow=length(DICE), ncol=nrolls )

  #set the first prior
  prior.jroll = prior

  # Go throught the updata process for each roll
  for (jroll in 1:nrolls) {
    x.jroll = data_rolls[jroll]

    likelihood_column = x.jroll
    likelihood.jroll = likelihood_table[,likelihood_column]
    bayes_numerator.jroll = prior.jroll * likelihood.jroll
    posterior.jroll = bayes_numerator.jroll/sum(bayes_numerator.jroll)

    # store the posterior
    posterior_mat[,jroll] = posterior.jroll

    if (plot_individual_posteriors) {
      # Make the bar plot
      title = paste("1b. Posterior probabilities after roll ", jroll, ": roll =", x.jroll)
      barplot(posterior.jroll, col='orange', width=rep(0.1, 5), xlim=c(0, 5), space=3, names=DICE, main=title)
    }

    # Set the prior for the next roll
    prior.jroll = posterior.jroll
  }

  # stacked barplot of the prior/posterior distributions
  # as a function of the number of rolls
  # (cbind --column bind is a easy way to add columns to a matrix)
  all_probs = cbind(prior, posterior_mat)

  barplot(all_probs, names.arg=c("0(Prior)", c(1:nrolls)),
          col=rainbow(length(DICE)), border=NA, space=0)

  title(xlab="Number of Rolls")
  title("1b. Stacked Barplot of Posterior Probabilities")

  # Add a legend to the plot
  legend(0,1.1, legend=paste("D", DICE, sep=""),
         col=rainbow(length(DICE)), pch=15, horiz=TRUE, cex=0.8,
         bg='white', xpd=TRUE)

  final_posterior = matrix(posterior_mat[,nrolls], nrow=1)
  colnames(final_posterior) = DICE_TYPES
  cat('The final posterior is\n')
  print(final_posterior, digits=4)
  cat('The true type of the chosen die is', random_die, 'sided\n')
  cat('See plots\n')
}

# Problem 1c: Compare and contrast. ----
studio5_problem_1c = function() {
  cat("-----\n")
  cat("1c. Compare and contrast.\n")

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("We know the data is from the 8-sided die. The second prior is so biased towards the 20-sided die that it takes more rolls (more data) before the probability of the 20-sided die is essentially 0 and we believe we are rolling the 8-sided die.\n")
}

# Problem 1d: Too certain a prior. ----
studio5_problem_1d = function() {
  cat("-----\n")
  cat("1d. Too certain a prior.\n")

  # Prior probability of D8 = 0
  prior = c(0.25, 0.25, 0, 0.25, 0.25)
  cat('Running studio5_problem_1b\n')
  studio5_problem_1b(prior, 20, FALSE)

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  cat("With a prior probability of 0, no amount of data will convince us that the chosen die has 8 sides. It chooses the next most probable die (D12) given the data\n")
}


#---------------------------
# Problem 2 (OPTIONAL): Censored data. ----

# Problem 2a (OPTIONAL): List hypotheses, make likelihood table
studio5_problem_2a = function() {
  cat("\n----------------------------------\n")
  cat("OPTIONAL 2a. List hypotheses, outcomes, make likelihood table.\n")


  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  # Build censored likelihood table: 5 dice x 2 possible outcomes
  # column 1 is for censored data = 0 (not a 1), column 2 is for censored data = 1
  # (Note: To select a column we will have to add 1 to the data value
  #  e.g if the data is 0 we want to use likelihood column 1.)
  censored_likelihood_table = matrix(0, nrow=5, ncol=2)
  rownames(censored_likelihood_table) = DICE_TYPES
  colnames(censored_likelihood_table) = c('0', '1')

  censored_likelihood_table[1,] = c(3/4, 1/4)  #4 sided die
  censored_likelihood_table[2,] = c(5/6, 1/6)  #6 sided die
  censored_likelihood_table[3,] = c(7/8, 1/8)  #8 sided die
  censored_likelihood_table[4,] = c(11/12, 1/12)  #12 sided die
  censored_likelihood_table[5,] = c(19/20, 1/20)  #20 sided die

  cat("The hypotheses are the same as before: we chose a 4, 6, 8, 12, or 20-sided die\n")

  cat("The possible outcomes of each roll are 0 or 1\n")

  cat("The likelihood table is 5 rows for the hypotheses and 2 columns for the possible outcomes of 1 and 0.\n")
  cat("Censored likelihood table\n")
  print(censored_likelihood_table, digits=3)
}


# Problem 2b (OPTIONAL): Censored data. ----
studio5_problem_2b = function(prior, nrolls) {
  cat("-----\n")
  cat("OPTIONAL 2b: Censored data.\n")

  # Arguments:
  #  prior = prior probilities for the type of die use to generate data
  #  nrolls = number of rolls to simulate

  # Do not change the above code.
  # ********* YOUR CODE HERE ***********

  # Pick a die and simulate nrolls
  random_die = sample(DICE, 1, prob=prior)
  data_rolls = sample(1:random_die, size=nrolls, replace=TRUE)

  # Censored data
  censored_data_rolls = (data_rolls == 1)  #Booleans are 0 or 1

  # Build the censored likelihood table: The only outcomes are 0 or 1
  censored_likelihood_table = matrix(0, nrow=5, ncol=2)
  rownames(censored_likelihood_table) = DICE_TYPES
  colnames(censored_likelihood_table) = c('0', '1')

  censored_likelihood_table[1,] = c(3/4, 1/4)  #4 sided die
  censored_likelihood_table[2,] = c(5/6, 1/6)  #6 sided die
  censored_likelihood_table[3,] = c(7/8, 1/8)  #8 sided die
  censored_likelihood_table[4,] = c(11/12, 1/12)  #12 sided die
  censored_likelihood_table[5,] = c(19/20, 1/20)  #20 sided die

  # Initialize matrix whose jth column will store the posterior distribution after updating the jth roll. We will use this to make a nifty stacked bar plot at the end
  posterior_mat<-matrix(NA, nrow=length(DICE), ncol=nrolls )

  #set the first prior
  prior.jroll = prior

  # Go throught the updata process for each roll
  for (jroll in 1:nrolls) {
    x.jroll = censored_data_rolls[jroll]

    # column 1 <-> data = 0, col 2 <-> data = 1
    likelihood_column = x.jroll + 1
    likelihood.jroll = censored_likelihood_table[,likelihood_column]
    bayes_numerator.jroll = prior.jroll * likelihood.jroll
    posterior.jroll = bayes_numerator.jroll/sum(bayes_numerator.jroll)

    # store the posterior
    posterior_mat[,jroll] = posterior.jroll

    # SET THE PRIOR for the next roll
    prior.jroll = posterior.jroll
  }

  # stacked barplot of the prior/posterior distributions
  # as a function of the number of rolls
  # (cbind --column bind is a easy way to add columns to a matrix)
  all_probs = cbind(prior, posterior_mat)

  barplot(all_probs, names.arg=c("0(Prior)",c(1:nrolls)),
          col=rainbow(length(DICE)), border=NA, space=0)

  title(xlab="Number of Rolls")
  title("2b. Stacked Barplot of Posterior Probabilities")

  # Add a legend to the plot
  legend(0,1.1, legend=paste("D", DICE, sep=""),
         col=rainbow(length(DICE)), pch=15, horiz=TRUE, cex=0.8,
         bg='white', xpd=TRUE)

  final_posterior = matrix(posterior_mat[,nrolls], nrow=1)
  colnames(final_posterior) = DICE_TYPES
  cat('The final posterior is\n')
  print(final_posterior, digits=4)
  cat('The true type of the chosen die is', random_die, 'sided\n')
  cat('See plots\n')
}

#---------------------------------------------------------
# File:   MIT18_05S22_ps1.r
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

do_problem_1 = TRUE
do_problem_2 = TRUE

if (do_problem_1) {
  # Simulate some 6 card poker hands
  cat('Problem 1\n')
  ntrials = 100000
  size_hand = 6
  nranks = 13
  nsuits = 4
  deck = c(1:(nranks*nsuits))

  is_two_pair = function(hand) {
    hand = hand %% nranks # remove suit information
    ranks_in_hand = hand[!duplicated(hand)]
    ret = FALSE
    npair = 0
    for (r in ranks_in_hand) {
      u = sum(hand==r)
      if (u == 2) {
        npair = npair + 1
      }
      else if (u > 2) { # 3 or more of a kind
        npair = 0
        break
      }
    }
    if (npair == 2) {
      ret = TRUE
    }
    return(ret)
  }

  is_three_of_a_kind = function(hand) {
    hand = hand %% nranks # remove suit information
    ranks_in_hand = hand[!duplicated(hand)]
    ret = FALSE
    ntriples = 0
    for (r in ranks_in_hand) {
      u = sum(hand==r)
      if (u == 3) {
        ntriples = ntriples + 1
      }
      else if (u == 2 || u > 3) { # Found pair or more than 3 of a kind
        ntriples = 0
        break
      }
    }
    if (ntriples == 1) {
      ret = TRUE
    }
    return(ret)
  }
  
  # Simulate 2 pair
  cnt_2_pair = 0
  cnt_3_of_kind = 0
  for (j in 1:ntrials) {
    h = sample(deck, size_hand)
    if (is_two_pair(h)) {
      cnt_2_pair = cnt_2_pair + 1
    }
    if (is_three_of_a_kind(h)) {
      cnt_3_of_kind = cnt_3_of_kind + 1
    }
  }
  cat('Simulated prob of 2 pair =', cnt_2_pair/ntrials, '\n')
  cat('Simulated prob of 3 of a kind =', cnt_3_of_kind/ntrials, '\n')
  
  # Exact calculations for 6 card hands out of 52 card decks
  # Two pair
  numerator = choose(13,2)*choose(4,2)*choose(4,2)*choose(11,2)*choose(4,1)*choose(4,1)
  denominator = choose(52,6)
  cat('Exact two pair: numerator =', numerator, ', denominator =', denominator, ', prob =', numerator/denominator, '\n')
  
  # Three of a kind
  numerator = choose(13,1)*choose(4,1)*choose(12,3)*choose(4,1)*choose(4,1)*choose(4,1)
  denominator = choose(52,6)
  cat('Exact three of a kind: numerator =', numerator, ', denominator =', denominator, ', prob =', numerator/denominator, '\n')
}

if (do_problem_2) {
  # Simulated the nontransitive dice. This code is fun because it can compare two or three different dice over any number of rolls
  cat('Problem 2\n')
  
  db = c(3,3,3,3,3,6) # blue
  do = c(1,4,4,4,4,4) # orange
  dw = c(2,2,2,5,5,5) # white

  playpair = function(die1, die2, nroll, ntrials) {
    wins1 = 0
    ties = 0
    wins2 = 0
    for (j in 1:ntrials) {
      a = sum(sample(die1, nroll, replace=T))
      b = sum(sample(die2, nroll, replace=T))
      if (a > b) {
        wins1 = wins1 + 1
      }
      else if (a == b) {
        ties = ties + 1
      }
      else {
        wins2 = wins2 + 1
      }
    }
    ret = c(wins1, wins2, ties)/ntrials
    return(ret)
  }

  playthree = function(die1, die2, die3, nroll, ntrials) {
    wins1 = 0
    wins2 = 0
    wins3 = 0
    draws = 0
    for (j in 1:ntrials) {
      a1 = sum(sample(die1, nroll, replace=T))
      a2 = sum(sample(die2, nroll, replace=T))
      a3 = sum(sample(die3, nroll, replace=T))
      if (a1 > a2 && a1 > a3) {
        wins1 = wins1 + 1
      }
      else if (a2 > a1 && a2 > a3) {
        wins2 = wins2 + 1
      }
      else if (a3 > a1 && a3 > a2) {
        wins3 = wins3 + 1
      }
      else {
        draws = draws + 1
      }
    }
    ret = c(wins1, wins2, wins3, draws)/ntrials
    return(ret)
  }
  cat('You can use the functions playpair or playthree to simulate various games with the nontransitive dice\n')
}

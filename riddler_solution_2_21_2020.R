#############################################################
# Thomas Bassine
# Date: February 23, 2020
# Purpose: Solve the 538 classic riddler for Feb 21
# https://fivethirtyeight.com/features/can-you-flip-your-way-to-victory/
#############################################################

# Initialize matrices to store probabilities of winning and the 
# correct move at each game state:

a <- matrix(0, nrow = 100, ncol = 600)
# 'a' stores the probability of winning at each game state.
# Row 1 = 100 flips left, row 2 = 99, ..., row 100 = 1 flip left.
# Column 1 = Current score of -299, col 2 = current score of -298, ...
# col 300 = current score of 0,..., col 600 = current score o +300.

# 'b' will store the correct move at each state. That is, whether to 
# flip the 1 point or 2 point coin.
b <- a

# Fill in known values:

# 1 flip left(FL), current score(CS) = 0. There is a 0.5 probability 
# of winning.
a[100, 300] = .5
# 1 FL, CS = 1
a[100, 301] = .5
# 1 FL, CS = -1
a[100, 299] = .5
# 2 FL
a[99, 297] = .25
a[99, 298] = .25
a[99, 299] = .25
a[99, 300] = .5
a[99, 301] = .75
a[99, 302] = .75

# Fill in the values we already know:
for(i in 1:nrow(a)){
  for(j in 1:ncol(a)){
    cs = -300 + j #current score
    fl = 101 - i #flips left
    if(cs > fl){a[i,j] = 1} #automatic win if score > flips left
    if(2*fl + cs < 1){a[i,j] = 0} #automatic loss if 2*Flips left + CS < 1
  }
}

# Determine probability of winning at a given state by computing the 
# the probability of winning if we choose the 1 point coin, then the 
# 2 point coin, and then taking the maximum. We can work backwards
# using the 2 flips left probabilities to determine the optimal move 
# and probabilities with 3 flips left, 3 flips left solutions to determine 
# 4 flips left, etc...

for(i in 98:1){
  for(j in 100:500){
    cs = -300 + j #current score
    fl = 101 - i #flips left
    
    # Only find a solution for the states that are not already solved...
    if((cs <= fl) & (2*fl + cs >= 1)){
      # My probability of winning if I choose the 2 point coin
      two_point_prob = .5 * a[(i+1), (j-2)] + .5 * a[(i+1), (j+2)]
      
      # My probability of winning if I choose the 1 point coin
      one_point_prob = .5 * a[(i+1), (j-1)] + .5 * a[(i+1), (j+1)]
      
      # The probability of winning is the maximum of the two strategies
      a[i,j] = max(c(one_point_prob, two_point_prob))
    
      # Use 'b' to store the optimal strategy (1 point coin or 2 point 
      # coin) at each state. Return 1.5 if we get the same win prob 
      # both ways.
      if(two_point_prob > one_point_prob){b[i,j] = 2}
      if(two_point_prob < one_point_prob){b[i,j] = 1}
      if(two_point_prob == one_point_prob){b[i,j] = 1.5}
    }
    
    
    
  }
}

#This is the probability of winning with 100 flips left and score of 0.
a[1,300] # roughly 0.64
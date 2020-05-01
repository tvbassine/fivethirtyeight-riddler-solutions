###########################################################
# Riddler classic for May 1, 2020
# https://fivethirtyeight.com/features/can-you-flip-your-way-to-freedom/

# escape_prob spits out the probability of all heads 
# based on each of the 4 people independently having a j%
# chance of flipping their coin.
escape_prob <- function(j){
  # j is the probability of an individual flipping the coin
  
  out <- .5 * dbinom(x = 1, size = 4, prob = j) +
         .25 * dbinom(x = 2, size = 4, prob = j) +
         .125 * dbinom(x = 3, size = 4, prob = j) +
         .0625 * dbinom(x = 4, size = 4, prob = j) 
  
  return(out)
  
}

# z will store probability of escape for different 'j's
z <- data.frame(j = seq(0,1,.01),
                out = 0)

for(i in 1:nrow(z)){
  z$out[i] <- escape_prob(z$j[i])
  print(z$j[i])
}

# I guess ~34% likelihood of any given individual flipping their coin
# maximizes escape prob. If you solve it mathematically I bet you get 
# an exact answer.

# Around a 28.5% chance of escape is the max I found.

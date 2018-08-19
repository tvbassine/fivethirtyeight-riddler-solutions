#####################################################
# Author: Thomas Bassine
# Date: 8/19/18
# Purpose: Solve the 8/17/18 'Riddler Classic' puzzle 
# from fivethirtyeight.
# https://fivethirtyeight.com/features/step-1-game-theory-step-2-step-3-profit/
######################################################
# 1) Required packages and user-defined functions.

# Need this for 'permuteGeneral'
library("RcppAlgos")

# This function computes the payoffs for the game based
# on the series of moves.
# picks = the sequence of moves. 
# (Example: picks = c(4,7,5) means player 1 selected '4',
# player 2 selected '7', etc...)
# nums = All possible stacks available (so 1:10).
get.payouts <- function(picks, nums){
  out <- rep(0, length(picks))
  for(i in nums){
    dist <- abs(picks - i)
    ind <- which(dist == min(dist))
    if(length(ind) == 2){
      out[ind] <- out[ind] + i/2
    } else{ out[ind] <- out[ind] + i}
  }
  
  return(out)
}

# Gets the best possible play for the nth player
# based on the moves of the n-1 players previous players.
# mat = matrix of moves and payoffs.
# prev.moves = vector of players who acted first.
# (So prev.moves = 1:3 means players 1,2,and 3 have
# already acted.)
# payoff.col = column in mat which gives payoff for 
# nth player.
# (In three player game, 6th column is payoff for
# player 3, 5th for player 2, and 4th for player 1.)
get.best <- function(mat, prev.moves, payoff.col){
  keep <- rep(0, nrow(mat))
  if(length(prev.moves) > 1){
    mat$game.seq <- apply( mat[ ,prev.moves] , 1 , paste , collapse = "-" )
  } else{
    mat$game.seq <- mat[,prev.moves]
  }
  for(i in unique(mat$game.seq)){
    rows <- which(mat$game.seq == i)
    best.row <- which(mat[rows, payoff.col] == max(mat[rows, payoff.col]) )
    keep[rows[best.row]] <- 1
    
    # tie.ind is T if there are multiple moves with equal payoff
    # for the current player.
    if(length(best.row) > 1){
      mat$tie.ind[rows[best.row]] <- T
    }
  }
  
  mat <- mat[keep == 1,]
  return(mat)
}

######################################################
# 2) Write function which solves the game for a given 
# number of players.

# num.players = number of people playing (3 in the 
# original problem).
get.solution <- function(num.players){

# Generate all possible sequences of moves.
perms <- permuteGeneral(1:10, num.players)

# mat will store a game sequence and corresponding
# payoffs in each row.
# Example: In the three player game, the
# row of mat which equals c(1,2,8,1,11.5,42.5) 
# corresponds to the game in which player 1 selects 1, 
# player 2 selects 2, and player 3 selects 8.
# Their payoffs are $1, $11.5, and $42.5, respectively.
mat <- data.frame(matrix(0, nrow = nrow(perms), 
                         ncol = 2*num.players))
mat[,1:num.players] <- perms

# Get payoffs for each sequence.
mat[, (num.players + 1):(2*num.players)] <- t(apply(mat[,1:num.players], 1, get.payouts, nums = 1:10))

# Initialize tie indicator.
# This column will keep track of situations where a player
# is indifferent between multiple possible moves.
mat$tie.ind <- F

# Start with the last player acting and get their best 
# play based on previous plays. Eliminate options which don't
# maximize payoff. Continue with the second to last player
# and keep going until we get the best option for the
# player going first.
for(i in num.players:2){
  prev.moves <- 1:(i-1)
  payoff.col <- i + num.players
  mat <- get.best(mat, prev.moves, payoff.col)
  print(i)
}

# Get the best move for the player going first.
best.first <- mat[which.max(mat[,num.players + 1]),]

# Save the resulting 'ideal' games based on player
# 1's move, and also save the game which maximizes 
# player 1's profits.
return(list(mat = mat,
            best.first = best.first)
)

# END get.solution
}

######################################################
# 3) Solve the game each possible number of players
# from 3 to 10. Also, record the time it takes to 
# run the code.

# Fast
system.time( solve.3 <- get.solution(3) )
system.time( solve.4 <- get.solution(4) )
system.time( solve.5 <- get.solution(5) )

# takes about 2 minutes
system.time( solve.6 <- get.solution(6) )






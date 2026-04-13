##########################################
#                EXCERCISE               #
##########################################
# ###########################################################################
# The following matrix is the generator matrix of a three-state continuous  #
# time Markov Chain                                                         #
#############################################################################

library(markovchain)
# We must remember that in a generator matrix, the row sums must be zero.
# In this way, we find the values of a, b, and c, respectively
a <- -3
b <- 0.5
c <- 0
c(a,b,c)
G <- matrix(c( a ,2 , 1,
              2.5,-3, b,
              0  ,c , 0),
            nrow = 3,byrow = T, dimnames = list(c("A","B","C"),c("A","B","C")))
#ctmc-class Continuous time Markov Chains class
# review the PDF file
G_CTMC <- new("ctmc",
              states = c("A","B","C"),
              generator = G
              )
###########################################################################
#Derive the probability that the chain leave state 2 sometimes before 0.5 #
#time units, gives that the chain is in state 2 at time 0.                #
###########################################################################
# Now we need to find the corresponding lambda in our generator matrix—specifically
# state 2.

lambda2<- -G[2,2]
prob_leaving <- pexp(0.5,rate = lambda2)
prob_leaving
##########################################################################
# Derive the transition matrix of the corresponding Markov jump Chain    #
##########################################################################

# To obtain the jump matrix, we must remember that it is calculated as follows:
# (P_(ij) = -g_(ij) / g_(ii))

P_jump <- G

for (i in 1:nrow(P_jump)) {
  qi <- -G[i, i]
  if (qi > 0) {
    P_jump[i, ] <- G[i, ] / qi
    P_jump[i, i] <- 0  
  } else {
    P_jump[i, i] <- 1  
  }
}

jump_chain <- new("markovchain", 
                  states = G_CTMC@states, 
                  transitionMatrix = P_jump,
                  name = "Jump Chain")
jump_chain













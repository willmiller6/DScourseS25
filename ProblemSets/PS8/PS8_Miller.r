#DSforE PS8 William Miller

# Load necessary libraries
library(tidyverse)
library(dplyr)
library(nloptr)
library(modelsummary)

#generate data!!

#set the seed for reproducibility
set.seed(100)

#generate X according to the problem set
X <- matrix(rnorm(1000000), nrow = 100000, ncol = 10)
X[,1] <- 1
head(X) #this looks right


#generate eps according to the problem set
eps <- rnorm(100000, mean = 0, sd = .25)


#generate beta according to the problem set
betaTrue <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

#now generate y according to the equation y = X*beta + eps
y <- X %*% betaTrue + eps
nrow(y)
head(y) #this looks right

#Compute OLS manually

betaLinAlg <- solve(t(X) %*% X) %*% t(X) %*% y
head(betaLinAlg, 10)
#These betas are extremely close to the betas we specified above,
#which is what we would expect given that the matrix operations
#we performed are the closed form solution to the OLS estimator


#manual gradient descent

# set up a stepsize
alpha <- 0.0000003

# set up a number of iterations
maxiter <- 100000

#define the gradient of our objective function
gradient <- function(beta, y, X) {
  return( as.vector(-2 * t(X) %*% (y - X %*% beta)))
}

#initialize 10 betas with random values
betaGD <- runif(dim(X)[2])
betaGD

#create beta.All vector - from Dr. Ransom's github
beta.All <- matrix(0, nrow = ncol(X), ncol = maxiter)

iter  <- 1
beta0 <- rep(0, ncol(X))
betaGD <- runif(ncol(X))

while(norm(as.matrix(beta0) - as.matrix(betaGD)) > 1e-8 && iter <= maxiter) {
    beta0 <- betaGD
    betaGD <- beta0 - alpha * gradient(beta0, y, X)
    beta.All[, iter] <- betaGD  # Store correct variable
    
    if (iter %% 10000 == 0) {
        print(betaGD)
    }
    iter <- iter + 1
}
print(iter-1)
print(betaGD)

#This method is much faster than I remembered it being in class. 
#It's possible I made an error here but this is actually pretty 
#computationally efficient. The betas are also very close to the
#groud-truth betas we specified above.

print(dim(beta.All))  # Should be (number of betas) x (maxiter)
print(length(betaGD)) # Should match number of rows in beta.All
print(length(beta.All[, iter])) 


#L-BFGS to estimate the betas
#first, we need to define the objective function
objective_function <- function(beta, X, y) {
  return (sum((y - X %*% beta)^2))
}

#gradient function already defined above

#now we need to set the initial values for beta
beta0 <- runif(dim(X)[2])

#now we need to set the options for the L-BFGS algorithm
options <- list("algorithm" = "NLOPT_LD_LBFGS", "xtol_rel" = 1e-8, "maxeval" = 10000)

#now we need to run the L-BFGS algorithm
betaLBFGS <- nloptr(x0 = beta0, eval_f = objective_function, 
                 eval_grad_f = gradient, opts = options, 
                 X = X, y = y)
print(betaLBFGS) #this is the estimated beta
#this is also very close to the betas we specified above but ever so
#slightly closer to the ground truth betas than those calculated using the
#closed form solution. I imagine this is a slight quirck of the algorithm
#because I cannot imagine that the algorithm is more efficient than the 
#closed form solution.


#nelder-mead time!
#objective function and gradient function already defined above

optionsNM <- list("algorithm" = "NLOPT_LN_NELDERMEAD", "xtol_rel" = 1e-8, "maxeval" = 100000)

betaNM <- nloptr(x0 = beta0, eval_f = objective_function,
                 opts = optionsNM, X = X, y = y)
print(betaNM)
#these also look close! Very similar to L-BFGS but not identical


#MLE with L-BFGS
#gradient vector of this problem from the class notes
objectiveML  <- function(theta, y, X) {
beta    <- theta[1:(length(theta) - 1)]
sig     <- theta[length(theta)]
loglike <- -sum( -.5 * (log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
return (loglike)
}

#gradient of the objective function from the problem set
gradientML <- function (theta,y,X) {
  grad <- as.vector(rep(0,length(theta)))
  beta <- theta [1:(length(theta)-1)]
  sig <- theta [length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(y - X%*%beta)/(sig^2)
  grad[length(theta)] <- dim(X)[1]/sig - crossprod(y - X%*%beta)/(sig^3)
return(grad)
}

theta0 <- runif(dim(X)[2]+1)
optionsML <- list("algorithm" = "NLOPT_LD_LBFGS", "xtol_rel" = 1e-8, "maxeval" = 1000)

betaML <- nloptr(x0 = theta0, eval_f = objectiveML, eval_grad_f = gradientML, 
                 opts = optionsML, X = X, y = y)
print(betaML)

#this estimate is off. It is pretty goot but much further from the true betas
#than the estimates we got from the closed form solution and the other gradient descent algorithms


#OLS the easy way!
betaEasy <- lm(y ~ X - 1)
betaEasy$coefficients
#as far as I can tell, these estimates are identical to the ones we found
#using the closed form solution. This is what I would expect given that
#the closed form solution we estimated manually is what is used under
#the hood in the lm function.

betaEasy %>%
    modelsummary(output = "latex", fmt = 2, stars = TRUE, gof_map = "AIC|BIC|R2|adj.R2|nobs")
### HOMEWORK 01 - FOUNDATIONS OF STATISTICAL LEARNIG - JEBALI 7078487 ## 

## GLOBAL SETTING ## 
n <- 100 # sampling size
beta0 <- 5
beta1 <- 1
beta2 <- 0


## SCENARIO 1 SETTING ## 

# In this scenario the RVs X1 and X2 are dependents and positive correlated #
sigma1 <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2)
mu1 <- c(0,0)


# SIMULATION SETTING # 
nsim <- 1000 # number of simulations 

beta00.est <- numeric(nsim) # vector of intercept estimations of mod0
beta01.est <- numeric(nsim) # vector of beta1 estimations of mod0

beta10.est <- numeric(nsim) # vector of intercept estimations of mod1
beta11.est <- numeric(nsim) # vector of beta1 estimations of mod1
beta12.est <- numeric(nsim) # vector of beta2 estimations of mod1

for (i in 1:nsim){
  set.seed(123 + i)
  X <- mvtnorm::rmvnorm(n, mean = mu1, sigma =sigma1)
  e <- rnorm(n,0,1)
  
  y <- beta0 + beta1*X[,1] + beta2*X[,2] + e
  
  mod0 <- lm (y ~ X[,1]) # reduced model
  mod1 <- lm (y ~ X[,1] + X[,2]) # complete model
  
  # Parameters estimations from the reduced model 
  beta00.est[[i]] <- mod0$coeff[1] # intercept estimations from mod0
  beta01.est[[i]] <- as.vector(mod0$coeff[2]) # beta1 estimations from mod0 
  
  # Parameters estimations from the complete model
  beta10.est[[i]] <- mod1$coeff[1] # intercept estimations from mod1
  beta11.est[[i]] <- mod1$coeff[2] # beta1 estimations from mod1
  beta12.est[[i]] <- mod1$coeff[3] # berta2 estimations from mod1
  
}

# Mean of the estimated values of beta1 from the reduced model
mean(beta01.est)

# Mean of the estimated values of beta1 from the complete model
mean(beta11.est)

# Plot of the estimated values of beta1 from the reduced model
hist(beta01.est, col="#ff6600", prob=TRUE)
lines(density(beta01.est), lty="longdash", col="#6666ff")

# Plot of the estimaed values of beta1 from the complete model
hist(beta11.est, col="#33cc33", prob=TRUE)
lines(density(beta11.est), lty="longdash", col="#ff9900")




####################################################################################################################




## SCENARIO 2 SETTING ## 

# In this scenario the RVs X1 and X2 are independent #
sigma2 <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
mu2 <- c(0,0)


# SIMULATION SETTING # 
nsim <- 1000 # number of simulations 

ind_beta00.est <- numeric(nsim) # vector of intercept estimations of ind_mod0
ind_beta01.est <- numeric(nsim) # vector of beta1 estimations of ind_mod0

ind_beta10.est <- numeric(nsim) # vector of intercept estimations of ind_mod1
ind_beta11.est <- numeric(nsim) # vector of beta1 estimations of ind_mod1
ind_beta12.est <- numeric(nsim) # vector of beta2 estimations of ind_mod1

for (i in 1:nsim){
  set.seed(123 + i)
  ind_X<- mvtnorm::rmvnorm(n, mean = mu2, sigma = sigma2)
  e <- rnorm(n,0,1)
  
  ind_y <- beta0 + beta1*ind_X[,1] + beta2*ind_X[,2] + e
  
  ind_mod0 <- lm (ind_y ~ ind_X[,1]) # reduced model
  ind_mod1 <- lm (ind_y ~ ind_X[,1] + ind_X[,2]) # complete model
  
  # Parameters estimations from the reduced model
  ind_beta00.est[[i]] <- ind_mod0$coeff[1] # intercept estimations from ind_mod0
  ind_beta01.est[[i]] <- ind_mod0$coeff[2] # beta1 estimations from ind_mod0
  
  # Parameters estimations from the complete model
  ind_beta10.est[[i]] <- ind_mod1$coeff[1] # intercept estimations from ind_mod1
  ind_beta11.est[[i]] <- ind_mod1$coeff[2] # beta1 estimations from ind_mod1
  ind_beta12.est[[i]] <- ind_mod1$coeff[3] # beta2 estimations from ind_mod1
}


# Mean of the estimated values of beta1 from the reduced model
mean(ind_beta01.est)

# Mean of the estimated values of beta1 from the complete model
mean(ind_beta11.est)

# Plot of the estimated values of beta1 from the reduced model
hist(ind_beta01.est, col="#ff6600", prob=TRUE)
lines(density(ind_beta01.est), lty="longdash", col="#6666ff")

# Plot of the estimaed values of beta1 from the complete model
hist(ind_beta11.est, col="#33cc33", prob=TRUE)
lines(density(ind_beta11.est), lty="longdash", col="#ff9900")



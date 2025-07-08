## Power Analysis for Parameter Estimation in Structural Equation Modeling: ##
########################## A Discussion and Tutorial #########################

# Simulation Study Code
# Written by Y. Andre Wang and Mijke Rhemtulla
# Updated on November 21, 2019


# Note: The code for the Main Simulation Study, Supplemental Simulation Set 1,
# Supplemental Simulation Set 2, and Single Predictor Simulation can be run
# independently of each other.


# Load package lavaan (and install it if not available)
if (!"lavaan" %in% installed.packages()) install.packages("lavaan")
library(lavaan)


# Create a data frame of simulation conditions ----------------------------

ct <- data.frame(
  ksim = 1000, # number of simulations is set to 1000
  sampleN = rep(rev(seq(from = 50, to = 1000, by = 50)), each = 20), # sample N
  lambda = rep(c(.3, .5, .7, .9), each = 5), # factor loading strength
  beta = c(.1, .2, .3, .4, .5)) # population effect size of X on Y



# Main Simulation Study ---------------------------------------------------


# *- Simulation function for p/f = 3 --------------------------------------

ParaPower3 <- function(ksim, sampleN, lambda, beta, seed = 42) {
  
  # Specify model
  mod <- "
  X =~ x1 + x2 + x3
  W =~ w1 + w2 + w3
  Z =~ z1 + z2 + z3
  Y =~ y1 + y2 + y3
  
  Y ~ X + W + Z
  W ~~ X + Z
  X ~~ Z"
  
  # Calculate explained variance of Y
  ModVarY <- beta^2 + .1^2 + .2^2 + 2*beta*.1*.3 + 2*beta*.2*.3 + 2*.1*.2*.3
  
  # Assign population values
  PopMod.t <- lavaanify(mod)
  PopMod.t$ustart <- c(rep(lambda, 12), beta, .1, .2, rep(.3, 3),
                       rep(1 - lambda^2, 12), rep(1, 3), 1 - ModVarY)
  
  # Define results objects for SEM and regression analyses
  lv_results <- NULL
  reg_results <- NULL
  
  # Simulate and fit data
  set.seed(seed)
  
  # Loop by iteration
  for (i in 1:ksim) {
    
    # Simulate and store data based on sample size input
    data <- as.data.frame(simulateData(PopMod.t, sample.nobs = sampleN))
    
    # Create composite scores
    data$X <- apply(data[, 1:3], 1, sum)
    data$W <- apply(data[, 4:6], 1, sum)
    data$Z <- apply(data[, 7:9], 1, sum)
    data$Y <- apply(data[, 10:12], 1, sum)
    
    # Fit regression model
    fitreg <- lm(Y ~ X + W + Z, data)
    
    # Fit structural equation model
    fit <- sem(model = mod, data = data, std.lv = TRUE)
    
    # Store estimates of the effect of X on Y from SEM
    lv_results <- rbind(lv_results, parameterEstimates(fit)[13, -c(6, 8:13)])
    
    # Store estimates of the effect of X on Y from regression
    reg_results <- rbind(reg_results,
                         summary(fitreg)$coefficients["X", c(1:2, 4)])
  }
  
  # Reformat results from SEM
  results <- as.data.frame(cbind(lv_results, reg = "Y ~ X", reg_results), 
                           row.names = 1:ksim)
  
  # Convergence rate
  conv <- (ksim - sum(is.na(lv_results$pvalue)))/ksim
  
  # Power from SEM (denominator = # of converged cases)
  power <- length(which(lv_results$pvalue < 0.05))/(conv*ksim)
  
  # Power from SEM (denominator = # all iterations)
  powerksim <- length(which(lv_results$pvalue < 0.05))/ksim
  
  # Power from regression
  powerreg <- length(which(reg_results[, 3] < 0.05))/ksim
  
  # Create table of power analysis results
  output <- data.frame(power, powerksim, conv, powerreg)
  return(list(output, results))
}

# CAUTION: Running the next function (currently as comment)
# will write two files for each simulation condition and take many hours.

# for(i in 1:nrow(ct)) {
#  dname <- paste("MS_p3", "_", ct[i, 2], "_", substr(ct[i, 3], 2, 4), "_", 
#                 substr(ct[i, 4], 2, 4), sep = "")
#  test <- ParaPower3(ct[i, 1], ct[i, 2], ct[i, 3], ct[i, 4])
#  write.table(test[1], sep = ",", row.names = F, 
#              paste("results_", dname, ".csv", sep = "")) # power estimates
#  write.table(test[2], sep = ",", row.names = F, 
#              paste("data_", dname, ".csv", sep = "")) # parameter estimates
# }


# *- Simulation function for p/f = 5 --------------------------------------

ParaPower5 <- function(ksim, sampleN, lambda, beta, seed = 42) {
  
  # Specify model
  mod <- "
  X =~ x1 + x2 + x3 + x4 + x5
  W =~ w1 + w2 + w3 + w4 + w5
  Z =~ z1 + z2 + z3 + z4 + z5
  Y =~ y1 + y2 + y3 + y4 + y5
  
  Y ~ X + W + Z
  W ~~ X + Z
  X ~~ Z"
  
  # Calculate explained variance of Y
  ModVarY <- beta^2 + .1^2 + .2^2 + 2*beta*.1*.3 + 2*beta*.2*.3 + 2*.1*.2*.3
  
  # Assign population values
  PopMod.t <- lavaanify(mod)
  PopMod.t$ustart <- c(rep(lambda, 20), beta, .1, .2, rep(.3, 3),
                       rep(1 - lambda^2, 20), rep(1, 3), 1 - ModVarY)
  
  # Define results objects for SEM and regression analyses
  lv_results <- NULL
  reg_results <- NULL
  
  # Simulate and fit data
  set.seed(seed)
  for (i in 1:ksim) {
    
    # Simulate data from the population model
    data <- as.data.frame(simulateData(PopMod.t, sample.nobs = sampleN))
    
    # Create composite scores
    data$X <- apply(data[, 1:5], 1, sum)
    data$W <- apply(data[, 6:10], 1, sum)
    data$Z <- apply(data[, 11:15], 1, sum)
    data$Y <- apply(data[, 16:20], 1, sum)
    
    # Fit regression model
    fitreg <- lm(Y ~ X + W + Z, data)
    
    # Fit structural equation model
    fit <- sem(model = mod, data = data, std.lv = TRUE)
    
    # Save estimates of the effect of X on Y from SEM
    lv_results <- rbind(lv_results, parameterEstimates(fit)[21, -c(6, 8:13)])
    
    # Save estimates of the effect of X on Y from regression
    reg_results <- rbind(reg_results,
                         summary(fitreg)$coefficients["X", c(1:2, 4)])
  }
  
  # Reformat results from SEM
  results <- as.data.frame(cbind(lv_results, reg = "Y ~ X", reg_results), 
                           row.names = 1:ksim)
  
  # Convergence rate
  conv <- (ksim - sum(is.na(lv_results$pvalue)))/ksim
  
  # Power from SEM (denominator = # of converged cases)
  power <- length(which(lv_results$pvalue < 0.05))/(conv*ksim)
  
  # Power from SEM (denominator = # all iterations)
  powerksim <- length(which(lv_results$pvalue < 0.05))/ksim
  
  # Power from regression
  powerreg <- length(which(reg_results[, 3] < 0.05))/ksim
  
  # Create table of power analysis results
  output <- data.frame(power, powerksim, conv, powerreg)
  return(list(output, results))
}

# CAUTION: Running the next function (currently as comment)
# will write two files for each simulation condition and take many hours.

# for(i in 1:nrow(ct)) {
#   dname <- paste("MS_p5", "_", ct[i, 2], "_", substr(ct[i, 3], 2, 4), "_", 
#                  substr(ct[i, 4], 2, 4), sep = "")
#   test <- ParaPower5(ct[i, 1], ct[i, 2], ct[i, 3], ct[i, 4])
#   write.table(test[1], sep = ",", row.names = F, 
#               paste("results_", dname, ".csv", sep = "")) # power estimate
#   write.table(test[2], sep = ",", row.names = F, 
#               paste("data_", dname, ".csv", sep = "")) # parameter estimates
# }


# *- Simulation function for p/f = 10 -------------------------------------

ParaPower10 <- function(ksim, sampleN, lambda, beta, seed = 42) {
  
  # Specify model
  mod <- "
  X =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10
  W =~ w1 + w2 + w3 + w4 + w5 + w6 + w7 + w8 + w9 + w10
  Z =~ z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z9 + z10
  Y =~ y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10
  
  Y ~ X + W + Z
  W ~~ X + Z
  X ~~ Z"
  
  # Calculate explained variance of Y
  ModVarY <- beta^2 + .1^2 + .2^2 + 2*beta*.1*.3 + 2*beta*.2*.3 + 2*.1*.2*.3
  
  # Assign population values
  PopMod.t <- lavaanify(mod)
  PopMod.t$ustart <- c(rep(lambda, 40), beta, .1, .2, rep(.3, 3),
                       rep(1 - lambda^2, 40), rep(1, 3), 1 - ModVarY)
  
  # Define results objects for SEM and regression analyses
  lv_results <- NULL
  reg_results <- NULL
  
  # Simulate and fit data
  set.seed(seed)
  
  # Loop by iteration
  for (i in 1:ksim) {
    
    # Simulate and store data based on sample size input
    data <- as.data.frame(simulateData(PopMod.t, sample.nobs = sampleN))
    
    # Create composite scores
    data$X <- apply(data[, 1:10], 1, sum)
    data$W <- apply(data[, 11:20], 1, sum)
    data$Z <- apply(data[, 21:30], 1, sum)
    data$Y <- apply(data[, 31:40], 1, sum)
    
    # Fit regression model
    fitreg <- lm(Y ~ X + W + Z, data)
    
    # Fit structural equation model
    fit <- sem(model = mod, data = data, std.lv = TRUE)
    
    # Store estimates of the effect of X on Y from SEM
    lv_results <- rbind(lv_results, parameterEstimates(fit)[41, -c(6, 8:13)])
    
    # Store estimates of the effect of X on Y from regression
    reg_results <- rbind(reg_results,
                         summary(fitreg)$coefficients["X", c(1:2, 4)])
  }
  
  # Reformat results from SEM
  results <- as.data.frame(cbind(lv_results, reg = "Y ~ X", reg_results), 
                           row.names = 1:ksim)
  
  # Convergence rate
  conv <- (ksim - sum(is.na(lv_results$pvalue)))/ksim
  
  # Power from SEM (denominator = # of converged cases)
  power <- length(which(lv_results$pvalue < 0.05))/(conv*ksim)
  
  # Power from SEM (denominator = # all iterations)
  powerksim <- length(which(lv_results$pvalue < 0.05))/ksim
  
  # Power from regression
  powerreg <- length(which(reg_results[, 3] < 0.05))/ksim
  
  # Create table of power analysis results
  output <- data.frame(power, powerksim, conv, powerreg)
  return(list(output, results))
}

# CAUTION: Running the next function (currently as comment)
# will write two files for each simulation condition and take many hours.

# for(i in 1:nrow(ct)) {
#   dname <- paste("MS_p10", "_", ct[i, 2], "_", substr(ct[i, 3], 2, 4), "_", 
#                  substr(ct[i, 4], 2, 4), sep = "")
#   test <- ParaPower10(ct[i, 1], ct[i, 2], ct[i, 3], ct[i, 4])
#   write.table(test[1], sep = ",", row.names = F, 
#               paste("results_", dname, ".csv", sep = "")) # power estimates
#   write.table(test[2], sep = ",", row.names = F, 
#               paste("data_", dname, ".csv", sep = "")) # parameter estimates
# }



# Supplemental Simulation Set 1 -------------------------------------------


# *- Simulation function for p/f = 3 --------------------------------------

ParaPower3 <- function(ksim, sampleN, lambda, beta, seed = 42) {
  
  # Specify model
  mod <- "
  X =~ x1 + x2 + x3
  W =~ w1 + w2 + w3
  Z =~ z1 + z2 + z3
  Y =~ y1 + y2 + y3
  
  Y ~ X + W + Z
  W ~~ X + Z
  X ~~ Z"
  
  # Calculate explained variance of Y
  ModVarY <- beta^2 + .1^2 + .2^2 + 2*beta*.1*.5 + 2*beta*.2*.5 + 2*.1*.2*.5
  
  # Assign population values
  PopMod.t <- lavaanify(mod)
  PopMod.t$ustart <- c(rep(lambda, 12), beta, .1, .2, rep(.5, 3),
                       rep(1 - lambda^2, 12), rep(1, 3), 1 - ModVarY)
  
  # Define results objects for SEM and regression analyses
  lv_results <- NULL
  reg_results <- NULL
  
  # Simulate and fit data
  set.seed(seed)
  
  # Loop by iteration
  for (i in 1:ksim) {
    
    # Simulate and store data based on sample size input
    data <- as.data.frame(simulateData(PopMod.t, sample.nobs = sampleN))
    
    # Create composite scores
    data$X <- apply(data[, 1:3], 1, sum)
    data$W <- apply(data[, 4:6], 1, sum)
    data$Z <- apply(data[, 7:9], 1, sum)
    data$Y <- apply(data[, 10:12], 1, sum)
    
    # Fit regression model
    fitreg <- lm(Y ~ X + W + Z, data)
    
    # Fit structural equation model
    fit <- sem(model = mod, data = data, std.lv = TRUE)
    
    # Store estimates of the effect of X on Y from SEM
    lv_results <- rbind(lv_results, parameterEstimates(fit)[13, -c(6, 8:13)])
    
    # Store estimates of the effect of X on Y from regression
    reg_results <- rbind(reg_results,
                         summary(fitreg)$coefficients["X", c(1:2, 4)])
  }
  
  # Reformat results from SEM
  results <- as.data.frame(cbind(lv_results, reg = "Y ~ X", reg_results), 
                           row.names = 1:ksim)
  
  # Convergence rate
  conv <- (ksim - sum(is.na(lv_results$pvalue)))/ksim
  
  # Power from SEM (denominator = # of converged cases)
  power <- length(which(lv_results$pvalue < 0.05))/(conv*ksim)
  
  # Power from SEM (denominator = # all iterations)
  powerksim <- length(which(lv_results$pvalue < 0.05))/ksim
  
  # Power from regression
  powerreg <- length(which(reg_results[, 3] < 0.05))/ksim
  
  # Create table of power analysis results
  output <- data.frame(power, powerksim, conv, powerreg)
  return(list(output, results))
}

# CAUTION: Running the next function (currently as comment)
# will write two files for each simulation condition and take many hours.

# for(i in 1:nrow(ct)) {
#  dname <- paste("SS1_p3", "_", ct[i, 2], "_", substr(ct[i, 3], 2, 4), "_", 
#                 substr(ct[i, 4], 2, 4), sep = "")
#  test <- ParaPower3(ct[i, 1], ct[i, 2], ct[i, 3], ct[i, 4])
#  write.table(test[1], sep = ",", row.names = F, 
#              paste("results_", dname, ".csv", sep = "")) # power estimates
#  write.table(test[2], sep = ",", row.names = F, 
#              paste("data_", dname, ".csv", sep = "")) # parameter estimates
# }


# *- Simulation function for p/f = 5 --------------------------------------

ParaPower5 <- function(ksim, sampleN, lambda, beta, seed = 42) {
  
  # Specify model
  mod <- "
  X =~ x1 + x2 + x3 + x4 + x5
  W =~ w1 + w2 + w3 + w4 + w5
  Z =~ z1 + z2 + z3 + z4 + z5
  Y =~ y1 + y2 + y3 + y4 + y5
  
  Y ~ X + W + Z
  W ~~ X + Z
  X ~~ Z"
  
  # Calculate explained variance of Y
  ModVarY <- beta^2 + .1^2 + .2^2 + 2*beta*.1*.5 + 2*beta*.2*.5 + 2*.1*.2*.5
  
  # Assign population values
  PopMod.t <- lavaanify(mod)
  PopMod.t$ustart <- c(rep(lambda, 20), beta, .1, .2, rep(.5, 3),
                       rep(1 - lambda^2, 20), rep(1, 3), 1 - ModVarY)
  
  # Define results objects for SEM and regression analyses
  lv_results <- NULL
  reg_results <- NULL
  
  # Simulate and fit data
  set.seed(seed)
  for (i in 1:ksim) {
    
    # Simulate data from the population model
    data <- as.data.frame(simulateData(PopMod.t, sample.nobs = sampleN))
    
    # Create composite scores
    data$X <- apply(data[, 1:5], 1, sum)
    data$W <- apply(data[, 6:10], 1, sum)
    data$Z <- apply(data[, 11:15], 1, sum)
    data$Y <- apply(data[, 16:20], 1, sum)
    
    # Fit regression model
    fitreg <- lm(Y ~ X + W + Z, data)
    
    # Fit structural equation model
    fit <- sem(model = mod, data = data, std.lv = TRUE)
    
    # Save estimates of the effect of X on Y from SEM
    lv_results <- rbind(lv_results, parameterEstimates(fit)[21, -c(6, 8:13)])
    
    # Save estimates of the effect of X on Y from regression
    reg_results <- rbind(reg_results,
                         summary(fitreg)$coefficients["X", c(1:2, 4)])
  }
  
  # Reformat results from SEM
  results <- as.data.frame(cbind(lv_results, reg = "Y ~ X", reg_results), 
                           row.names = 1:ksim)
  
  # Convergence rate
  conv <- (ksim - sum(is.na(lv_results$pvalue)))/ksim
  
  # Power from SEM (denominator = # of converged cases)
  power <- length(which(lv_results$pvalue < 0.05))/(conv*ksim)
  
  # Power from SEM (denominator = # all iterations)
  powerksim <- length(which(lv_results$pvalue < 0.05))/ksim
  
  # Power from regression
  powerreg <- length(which(reg_results[, 3] < 0.05))/ksim
  
  # Create table of power analysis results
  output <- data.frame(power, powerksim, conv, powerreg)
  return(list(output, results))
}

# CAUTION: Running the next function (currently as comment)
# will write two files for each simulation condition and take many hours.

# for(i in 1:nrow(ct)) {
#   dname <- paste("SS1_p5", "_", ct[i, 2], "_", substr(ct[i, 3], 2, 4), "_", 
#                  substr(ct[i, 4], 2, 4), sep = "")
#   test <- ParaPower5(ct[i, 1], ct[i, 2], ct[i, 3], ct[i, 4])
#   write.table(test[1], sep = ",", row.names = F, 
#               paste("results_", dname, ".csv", sep = "")) # power estimate
#   write.table(test[2], sep = ",", row.names = F, 
#               paste("data_", dname, ".csv", sep = "")) # parameter estimates
# }


# *- Simulation function for p/f = 10 -------------------------------------

ParaPower10 <- function(ksim, sampleN, lambda, beta, seed = 42) {
  
  # Specify model
  mod <- "
  X =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10
  W =~ w1 + w2 + w3 + w4 + w5 + w6 + w7 + w8 + w9 + w10
  Z =~ z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z9 + z10
  Y =~ y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10
  
  Y ~ X + W + Z
  W ~~ X + Z
  X ~~ Z"
  
  # Calculate explained variance of Y
  ModVarY <- beta^2 + .1^2 + .2^2 + 2*beta*.1*.5 + 2*beta*.2*.5 + 2*.1*.2*.5
  
  # Assign population values
  PopMod.t <- lavaanify(mod)
  PopMod.t$ustart <- c(rep(lambda, 40), beta, .1, .2, rep(.5, 3),
                       rep(1 - lambda^2, 40), rep(1, 3), 1 - ModVarY)
  
  # Define results objects for SEM and regression analyses
  lv_results <- NULL
  reg_results <- NULL
  
  # Simulate and fit data
  set.seed(seed)
  
  # Loop by iteration
  for (i in 1:ksim) {
    
    # Simulate and store data based on sample size input
    data <- as.data.frame(simulateData(PopMod.t, sample.nobs = sampleN))
    
    # Create composite scores
    data$X <- apply(data[, 1:10], 1, sum)
    data$W <- apply(data[, 11:20], 1, sum)
    data$Z <- apply(data[, 21:30], 1, sum)
    data$Y <- apply(data[, 31:40], 1, sum)
    
    # Fit regression model
    fitreg <- lm(Y ~ X + W + Z, data)
    
    # Fit structural equation model
    fit <- sem(model = mod, data = data, std.lv = TRUE)
    
    # Store estimates of the effect of X on Y from SEM
    lv_results <- rbind(lv_results, parameterEstimates(fit)[41, -c(6, 8:13)])
    
    # Store estimates of the effect of X on Y from regression
    reg_results <- rbind(reg_results,
                         summary(fitreg)$coefficients["X", c(1:2, 4)])
  }
  
  # Reformat results from SEM
  results <- as.data.frame(cbind(lv_results, reg = "Y ~ X", reg_results), 
                           row.names = 1:ksim)
  
  # Convergence rate
  conv <- (ksim - sum(is.na(lv_results$pvalue)))/ksim
  
  # Power from SEM (denominator = # of converged cases)
  power <- length(which(lv_results$pvalue < 0.05))/(conv*ksim)
  
  # Power from SEM (denominator = # all iterations)
  powerksim <- length(which(lv_results$pvalue < 0.05))/ksim
  
  # Power from regression
  powerreg <- length(which(reg_results[, 3] < 0.05))/ksim
  
  # Create table of power analysis results
  output <- data.frame(power, powerksim, conv, powerreg)
  return(list(output, results))
}

# CAUTION: Running the next function (currently as comment)
# will write two files for each simulation condition and take many hours.

# for(i in 1:nrow(ct)) {
#   dname <- paste("SS1_p10", "_", ct[i, 2], "_", substr(ct[i, 3], 2, 4), "_", 
#                  substr(ct[i, 4], 2, 4), sep = "")
#   test <- ParaPower10(ct[i, 1], ct[i, 2], ct[i, 3], ct[i, 4])
#   write.table(test[1], sep = ",", row.names = F, 
#               paste("results_", dname, ".csv", sep = "")) # power estimates
#   write.table(test[2], sep = ",", row.names = F, 
#               paste("data_", dname, ".csv", sep = "")) # parameter estimates
# }



# Supplemental Simulation Set 2 -------------------------------------------


# *- Simulation function for p/f = 3 --------------------------------------

ParaPower3 <- function(ksim, sampleN, lambda, beta, seed = 42) {
  
  # Specify model
  mod <- "
  X =~ x1 + x2 + x3
  W =~ w1 + w2 + w3
  Z =~ z1 + z2 + z3
  Y =~ y1 + y2 + y3
  
  Y ~ X + W + Z
  W ~~ X + Z
  X ~~ Z"
  
  # Calculate explained variance of Y
  ModVarY <- beta^2 + .3^2 + .3^2 + 2*beta*.3*.3 + 2*beta*.3*.3 + 2*.3*.3*.3
  
  # Assign population values
  PopMod.t <- lavaanify(mod)
  PopMod.t$ustart <- c(rep(lambda, 12), beta, .3, .3, rep(.3, 3),
                       rep(1 - lambda^2, 12), rep(1, 3), 1 - ModVarY)
  
  # Define results objects for SEM and regression analyses
  lv_results <- NULL
  reg_results <- NULL
  
  # Simulate and fit data
  set.seed(seed)
  
  # Loop by iteration
  for (i in 1:ksim) {
    
    # Simulate and store data based on sample size input
    data <- as.data.frame(simulateData(PopMod.t, sample.nobs = sampleN))
    
    # Create composite scores
    data$X <- apply(data[, 1:3], 1, sum)
    data$W <- apply(data[, 4:6], 1, sum)
    data$Z <- apply(data[, 7:9], 1, sum)
    data$Y <- apply(data[, 10:12], 1, sum)
    
    # Fit regression model
    fitreg <- lm(Y ~ X + W + Z, data)
    
    # Fit structural equation model
    fit <- sem(model = mod, data = data, std.lv = TRUE)
    
    # Store estimates of the effect of X on Y from SEM
    lv_results <- rbind(lv_results, parameterEstimates(fit)[13, -c(6, 8:13)])
    
    # Store estimates of the effect of X on Y from regression
    reg_results <- rbind(reg_results,
                         summary(fitreg)$coefficients["X", c(1:2, 4)])
  }
  
  # Reformat results from SEM
  results <- as.data.frame(cbind(lv_results, reg = "Y ~ X", reg_results), 
                           row.names = 1:ksim)
  
  # Convergence rate
  conv <- (ksim - sum(is.na(lv_results$pvalue)))/ksim
  
  # Power from SEM (denominator = # of converged cases)
  power <- length(which(lv_results$pvalue < 0.05))/(conv*ksim)
  
  # Power from SEM (denominator = # all iterations)
  powerksim <- length(which(lv_results$pvalue < 0.05))/ksim
  
  # Power from regression
  powerreg <- length(which(reg_results[, 3] < 0.05))/ksim
  
  # Create table of power analysis results
  output <- data.frame(power, powerksim, conv, powerreg)
  return(list(output, results))
}

# CAUTION: Running the next function (currently as comment)
# will write two files for each simulation condition and take many hours.

# for(i in 1:nrow(ct)) {
#  dname <- paste("SS2_p3", "_", ct[i, 2], "_", substr(ct[i, 3], 2, 4), "_", 
#                 substr(ct[i, 4], 2, 4), sep = "")
#  test <- ParaPower3(ct[i, 1], ct[i, 2], ct[i, 3], ct[i, 4])
#  write.table(test[1], sep = ",", row.names = F, 
#              paste("results_", dname, ".csv", sep = "")) # power estimates
#  write.table(test[2], sep = ",", row.names = F, 
#              paste("data_", dname, ".csv", sep = "")) # parameter estimates
# }


# *- Simulation function for p/f = 5 --------------------------------------

ParaPower5 <- function(ksim, sampleN, lambda, beta, seed = 42) {
  
  # Specify model
  mod <- "
  X =~ x1 + x2 + x3 + x4 + x5
  W =~ w1 + w2 + w3 + w4 + w5
  Z =~ z1 + z2 + z3 + z4 + z5
  Y =~ y1 + y2 + y3 + y4 + y5
  
  Y ~ X + W + Z
  W ~~ X + Z
  X ~~ Z"
  
  # Calculate explained variance of Y
  ModVarY <- beta^2 + .3^2 + .3^2 + 2*beta*.3*.3 + 2*beta*.3*.3 + 2*.3*.3*.3
  
  # Assign population values
  PopMod.t <- lavaanify(mod)
  PopMod.t$ustart <- c(rep(lambda, 20), beta, .3, .3, rep(.3, 3),
                       rep(1 - lambda^2, 20), rep(1, 3), 1 - ModVarY)
  
  # Define results objects for SEM and regression analyses
  lv_results <- NULL
  reg_results <- NULL
  
  # Simulate and fit data
  set.seed(seed)
  for (i in 1:ksim) {
    
    # Simulate data from the population model
    data <- as.data.frame(simulateData(PopMod.t, sample.nobs = sampleN))
    
    # Create composite scores
    data$X <- apply(data[, 1:5], 1, sum)
    data$W <- apply(data[, 6:10], 1, sum)
    data$Z <- apply(data[, 11:15], 1, sum)
    data$Y <- apply(data[, 16:20], 1, sum)
    
    # Fit regression model
    fitreg <- lm(Y ~ X + W + Z, data)
    
    # Fit structural equation model
    fit <- sem(model = mod, data = data, std.lv = TRUE)
    
    # Save estimates of the effect of X on Y from SEM
    lv_results <- rbind(lv_results, parameterEstimates(fit)[21, -c(6, 8:13)])
    
    # Save estimates of the effect of X on Y from regression
    reg_results <- rbind(reg_results,
                         summary(fitreg)$coefficients["X", c(1:2, 4)])
  }
  
  # Reformat results from SEM
  results <- as.data.frame(cbind(lv_results, reg = "Y ~ X", reg_results), 
                           row.names = 1:ksim)
  
  # Convergence rate
  conv <- (ksim - sum(is.na(lv_results$pvalue)))/ksim
  
  # Power from SEM (denominator = # of converged cases)
  power <- length(which(lv_results$pvalue < 0.05))/(conv*ksim)
  
  # Power from SEM (denominator = # all iterations)
  powerksim <- length(which(lv_results$pvalue < 0.05))/ksim
  
  # Power from regression
  powerreg <- length(which(reg_results[, 3] < 0.05))/ksim
  
  # Create table of power analysis results
  output <- data.frame(power, powerksim, conv, powerreg)
  return(list(output, results))
}

# CAUTION: Running the next function (currently as comment)
# will write two files for each simulation condition and take many hours.

# for(i in 1:nrow(ct)) {
#   dname <- paste("SS2_p5", "_", ct[i, 2], "_", substr(ct[i, 3], 2, 4), "_", 
#                  substr(ct[i, 4], 2, 4), sep = "")
#   test <- ParaPower5(ct[i, 1], ct[i, 2], ct[i, 3], ct[i, 4])
#   write.table(test[1], sep = ",", row.names = F, 
#               paste("results_", dname, ".csv", sep = "")) # power estimate
#   write.table(test[2], sep = ",", row.names = F, 
#               paste("data_", dname, ".csv", sep = "")) # parameter estimates
# }


# *- Simulation function for p/f = 10 -------------------------------------

ParaPower10 <- function(ksim, sampleN, lambda, beta, seed = 42) {
  
  # Specify model
  mod <- "
  X =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10
  W =~ w1 + w2 + w3 + w4 + w5 + w6 + w7 + w8 + w9 + w10
  Z =~ z1 + z2 + z3 + z4 + z5 + z6 + z7 + z8 + z9 + z10
  Y =~ y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10
  
  Y ~ X + W + Z
  W ~~ X + Z
  X ~~ Z"
  
  # Calculate explained variance of Y
  ModVarY <- beta^2 + .3^2 + .3^2 + 2*beta*.3*.3 + 2*beta*.3*.3 + 2*.3*.3*.3
  
  # Assign population values
  PopMod.t <- lavaanify(mod)
  PopMod.t$ustart <- c(rep(lambda, 40), beta, .3, .3, rep(.3, 3),
                       rep(1 - lambda^2, 40), rep(1, 3), 1 - ModVarY)
  
  # Define results objects for SEM and regression analyses
  lv_results <- NULL
  reg_results <- NULL
  
  # Simulate and fit data
  set.seed(seed)
  
  # Loop by iteration
  for (i in 1:ksim) {
    
    # Simulate and store data based on sample size input
    data <- as.data.frame(simulateData(PopMod.t, sample.nobs = sampleN))
    
    # Create composite scores
    data$X <- apply(data[, 1:10], 1, sum)
    data$W <- apply(data[, 11:20], 1, sum)
    data$Z <- apply(data[, 21:30], 1, sum)
    data$Y <- apply(data[, 31:40], 1, sum)
    
    # Fit regression model
    fitreg <- lm(Y ~ X + W + Z, data)
    
    # Fit structural equation model
    fit <- sem(model = mod, data = data, std.lv = TRUE)
    
    # Store estimates of the effect of X on Y from SEM
    lv_results <- rbind(lv_results, parameterEstimates(fit)[41, -c(6, 8:13)])
    
    # Store estimates of the effect of X on Y from regression
    reg_results <- rbind(reg_results,
                         summary(fitreg)$coefficients["X", c(1:2, 4)])
  }
  
  # Reformat results from SEM
  results <- as.data.frame(cbind(lv_results, reg = "Y ~ X", reg_results), 
                           row.names = 1:ksim)
  
  # Convergence rate
  conv <- (ksim - sum(is.na(lv_results$pvalue)))/ksim
  
  # Power from SEM (denominator = # of converged cases)
  power <- length(which(lv_results$pvalue < 0.05))/(conv*ksim)
  
  # Power from SEM (denominator = # all iterations)
  powerksim <- length(which(lv_results$pvalue < 0.05))/ksim
  
  # Power from regression
  powerreg <- length(which(reg_results[, 3] < 0.05))/ksim
  
  # Create table of power analysis results
  output <- data.frame(power, powerksim, conv, powerreg)
  return(list(output, results))
}

# CAUTION: Running the next function (currently as comment)
# will write two files for each simulation condition and take many hours.

# for(i in 1:nrow(ct)) {
#   dname <- paste("SS2_p10", "_", ct[i, 2], "_", substr(ct[i, 3], 2, 4), "_", 
#                  substr(ct[i, 4], 2, 4), sep = "")
#   test <- ParaPower10(ct[i, 1], ct[i, 2], ct[i, 3], ct[i, 4])
#   write.table(test[1], sep = ",", row.names = F, 
#               paste("results_", dname, ".csv", sep = "")) # power estimates
#   write.table(test[2], sep = ",", row.names = F, 
#               paste("data_", dname, ".csv", sep = "")) # parameter estimates
# }



# Single Predictor Simulation Set -----------------------------------------


# *- Simulation function for p/f = 3 --------------------------------------

ParaPower3 <- function(ksim, sampleN, lambda, beta, seed = 42) {
  
  # Specify model
  mod <- "
  X =~ x1 + x2 + x3
  Y =~ y1 + y2 + y3
  
  Y ~ X"
  
  # Calculate explained variance of Y
  ModVarY <- beta^2
  
  # Assign population values
  PopMod.t <- lavaanify(mod)
  PopMod.t$ustart <- c(rep(lambda, 6), beta, rep(1 - lambda^2, 6), 1,
                       1 - ModVarY)
  
  # Define results objects for SEM and regression analyses
  lv_results <- NULL
  reg_results <- NULL
  
  # Simulate and fit data
  set.seed(seed)
  
  # Loop by iteration
  for (i in 1:ksim) {
    
    # Simulate and store data based on sample size input
    data <- as.data.frame(simulateData(PopMod.t, sample.nobs = sampleN))
    
    # Create composite scores
    data$X <- apply(data[, 1:3], 1, sum)
    data$Y <- apply(data[, 4:6], 1, sum)
    
    # Fit regression model
    fitreg <- lm(Y ~ X, data)
    
    # Fit structural equation model
    fit <- sem(model = mod, data = data, std.lv = TRUE)
    
    # Store estimates of the effect of X on Y from SEM
    lv_results <- rbind(lv_results, parameterEstimates(fit)[7, -c(6, 8:13)])
    
    # Store estimates of the effect of X on Y from regression
    reg_results <- rbind(reg_results,
                         summary(fitreg)$coefficients["X", c(1:2, 4)])
  }
  
  # Reformat results from SEM
  results <- as.data.frame(cbind(lv_results, reg = "Y ~ X", reg_results), 
                           row.names = 1:ksim)
  
  # Convergence rate
  conv <- (ksim - sum(is.na(lv_results$pvalue)))/ksim
  
  # Power from SEM (denominator = # of converged cases)
  power <- length(which(lv_results$pvalue < 0.05))/(conv*ksim)
  
  # Power from SEM (denominator = # all iterations)
  powerksim <- length(which(lv_results$pvalue < 0.05))/ksim
  
  # Power from regression
  powerreg <- length(which(reg_results[, 3] < 0.05))/ksim
  
  # Create table of power analysis results
  output <- data.frame(power, powerksim, conv, powerreg)
  return(list(output, results))
}

# CAUTION: Running the next function (currently as comment)
# will write two files for each simulation condition and take many hours.

# for(i in 1:nrow(ct)) {
#  dname <- paste("SP_p3", "_", ct[i, 2], "_", substr(ct[i, 3], 2, 4), "_", 
#                 substr(ct[i, 4], 2, 4), sep = "")
#  test <- ParaPower3(ct[i, 1], ct[i, 2], ct[i, 3], ct[i, 4])
#  write.table(test[1], sep = ",", row.names = F, 
#              paste("results_", dname, ".csv", sep = "")) # power estimates
#  write.table(test[2], sep = ",", row.names = F, 
#              paste("data_", dname, ".csv", sep = "")) # parameter estimates
# }



# *- Simulation function for p/f = 5 --------------------------------------

ParaPower5 <- function(ksim, sampleN, lambda, beta, seed = 42) {
  
  # Specify model
  mod <- "
  X =~ x1 + x2 + x3 + x4 + x5
  Y =~ y1 + y2 + y3 + y4 + y5
  
  Y ~ X"
  
  # Calculate explained variance of Y
  ModVarY <- beta^2
  
  # Assign population values
  PopMod.t <- lavaanify(mod)
  PopMod.t$ustart <- c(rep(lambda, 10), beta, rep(1 - lambda^2, 10), 1, 
                       1 - ModVarY)
  
  # Define results objects for SEM and regression analyses
  lv_results <- NULL
  reg_results <- NULL
  
  # Simulate and fit data
  set.seed(seed)
  for (i in 1:ksim) {
    
    # Simulate data from the population model
    data <- as.data.frame(simulateData(PopMod.t, sample.nobs = sampleN))
    
    # Create composite scores
    data$X <- apply(data[, 1:5], 1, sum)
    data$Y <- apply(data[, 6:10], 1, sum)
    
    # Fit regression model
    fitreg <- lm(Y ~ X, data)
    
    # Fit structural equation model
    fit <- sem(model = mod, data = data, std.lv = TRUE)
    
    # Save estimates of the effect of X on Y from SEM
    lv_results <- rbind(lv_results, parameterEstimates(fit)[11, -c(6, 8:13)])
    
    # Save estimates of the effect of X on Y from regression
    reg_results <- rbind(reg_results,
                         summary(fitreg)$coefficients["X", c(1:2, 4)])
  }
  
  # Reformat results from SEM
  results <- as.data.frame(cbind(lv_results, reg = "Y ~ X", reg_results), 
                           row.names = 1:ksim)
  
  # Convergence rate
  conv <- (ksim - sum(is.na(lv_results$pvalue)))/ksim
  
  # Power from SEM (denominator = # of converged cases)
  power <- length(which(lv_results$pvalue < 0.05))/(conv*ksim)
  
  # Power from SEM (denominator = # all iterations)
  powerksim <- length(which(lv_results$pvalue < 0.05))/ksim
  
  # Power from regression
  powerreg <- length(which(reg_results[, 3] < 0.05))/ksim
  
  # Create table of power analysis results
  output <- data.frame(power, powerksim, conv, powerreg)
  return(list(output, results))
}

# CAUTION: Running the next function (currently as comment)
# will write two files for each simulation condition and take many hours.

# for(i in 1:nrow(ct)) {
#   dname <- paste("SP_p5", "_", ct[i, 2], "_", substr(ct[i, 3], 2, 4), "_", 
#                  substr(ct[i, 4], 2, 4), sep = "")
#   test <- ParaPower5(ct[i, 1], ct[i, 2], ct[i, 3], ct[i, 4])
#   write.table(test[1], sep = ",", row.names = F, 
#               paste("results_", dname, ".csv", sep = "")) # power estimate
#   write.table(test[2], sep = ",", row.names = F, 
#               paste("data_", dname, ".csv", sep = "")) # parameter estimates
# }



# *- Simulation function for p/f = 10 -------------------------------------

ParaPower10 <- function(ksim, sampleN, lambda, beta, seed = 42) {
  
  # Specify model
  mod <- "
  X =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10
  Y =~ y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10
  
  Y ~ X"
  
  # Calculate explained variance of Y
  ModVarY <- beta^2
  
  # Assign population values
  PopMod.t <- lavaanify(mod)
  PopMod.t$ustart <- c(rep(lambda, 20), beta, rep(1 - lambda^2, 20), 1, 
                       1 - ModVarY)
  
  # Define results objects for SEM and regression analyses
  lv_results <- NULL
  reg_results <- NULL
  
  # Simulate and fit data
  set.seed(seed)
  
  # Loop by iteration
  for (i in 1:ksim) {
    
    # Simulate and store data based on sample size input
    data <- as.data.frame(simulateData(PopMod.t, sample.nobs = sampleN))
    
    # Create composite scores
    data$X <- apply(data[, 1:10], 1, sum)
    data$Y <- apply(data[, 11:20], 1, sum)
    
    # Fit regression model
    fitreg <- lm(Y ~ X, data)
    
    # Fit structural equation model
    fit <- sem(model = mod, data = data, std.lv = TRUE)
    
    # Store estimates of the effect of X on Y from SEM
    lv_results <- rbind(lv_results, parameterEstimates(fit)[21, -c(6, 8:13)])
    
    # Store estimates of the effect of X on Y from regression
    reg_results <- rbind(reg_results,
                         summary(fitreg)$coefficients["X", c(1:2, 4)])
  }
  
  # Reformat results from SEM
  results <- as.data.frame(cbind(lv_results, reg = "Y ~ X", reg_results), 
                           row.names = 1:ksim)
  
  # Convergence rate
  conv <- (ksim - sum(is.na(lv_results$pvalue)))/ksim
  
  # Power from SEM (denominator = # of converged cases)
  power <- length(which(lv_results$pvalue < 0.05))/(conv*ksim)
  
  # Power from SEM (denominator = # all iterations)
  powerksim <- length(which(lv_results$pvalue < 0.05))/ksim
  
  # Power from regression
  powerreg <- length(which(reg_results[, 3] < 0.05))/ksim
  
  # Create table of power analysis results
  output <- data.frame(power, powerksim, conv, powerreg)
  return(list(output, results))
}

# CAUTION: Running the next function (currently as comment)
# will write two files for each simulation condition and take many hours.

# for(i in 1:nrow(ct)) {
#   dname <- paste("SP_p10", "_", ct[i, 2], "_", substr(ct[i, 3], 2, 4), "_", 
#                  substr(ct[i, 4], 2, 4), sep = "")
#   test <- ParaPower10(ct[i, 1], ct[i, 2], ct[i, 3], ct[i, 4])
#   write.table(test[1], sep = ",", row.names = F, 
#               paste("results_", dname, ".csv", sep = "")) # power estimates
#   write.table(test[2], sep = ",", row.names = F, 
#               paste("data_", dname, ".csv", sep = "")) # parameter estimates
# }

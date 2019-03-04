setwd("/Users/hkromer/02_PhD/08.ETH_courses/01.CompStat/02.ETH_course.CompStat/")
getwd()

############################################################################################################
# Problem 1
############################################################################################################

# a)

# data points
data.x <- c(1, 1, 4.5, 4.5)
data.y <- c(1, 2.5, 2.5, 4)

# fit the regression line for x being independent, y being dependent variable
fit.XY <- lm(data.y~data.x)
coeff <- coefficients(fit.XY)

# fit for regression line for y being independent, x being dependent variable
fit.YX <- lm(data.x~data.y)
coeff2 <- coefficients(fit.YX)


# plots
plot(data.x, data.y)
abline(a=coeff[1], b=coeff[2])
abline(a=-coeff2[1]/coeff2[2], b=1/coeff[1], col="red")
legend(1, 3.5, legend=c("data.y~data.x", "data.x~data.y"), col=c("black", "red"), lty=1:1, cex=0.8)


############################################################################################################
# Problem 2
############################################################################################################


# a)
b0 <- 2.1783
b1 <- 1.8232
res <- b0+b1*log(4)
res


# c)

b0 <- 1.12022 
b1 <- 0.95966 
res.log <- b0+b1*(3)
res <- exp(res.log)
res

############################################################################################################
# Problem 3
############################################################################################################



## a)

# simulation
set.seed(21)                          # initializes the random number generator
x <- rnorm(40, 20, 3)                 # generates x-values
nsim <- 100
hatbeta0 <- rep(NA, nsim)             # vector to store estimated beta_0 values
hatbeta1 <- rep(NA, nsim)             # vector to store estimated beta_0 values

for(i in 1:nsim){
  y <- 1 + 2 * x + 5 * rnorm(length(x)) # y-values = linear function(x) + error
  fit <- lm(y~x)              # fit linear regression
  hatbeta0[i] <- fit$coef[1]  # store estimated intercept
  hatbeta1[i] <- fit$coef[2]  # store estimated slope
}

par(mfrow=c(1,1))     # plot 3x2
errors <- 5 * rnorm(length(x)) # y-values = linear function(x) + error
hist(errors)
mean(errors)
var(errors)


## b)
par(mfrow=c(3,2))     # plot 3x2
set.seed(21)   # same seed
for(i in 1:3){
  y <- 1 + 2 * x + 5 * rnorm(length(x)) # y-values = linear function(x) + error
  fit.new <- lm(y~x)              # fit linear regression
  plot(y~x)
  abline(fit.new)
  plot(fit.new, which=1)   # Tukey-Anscombe
  
}

## c)

mean(hatbeta1)
sd(hatbeta1)


## d)

X = cbind(1,x)                          # design matrix
XtX.inv <- solve(t(X) %*% X)            # (X^T X)^{-1}
betahat.theo.var <- 5^2 * XtX.inv[2,2]
betahat.theo.var

## e)

par(mfrow=c(1,1))     # plot 1x1
hist(hatbeta1, breaks=10, freq=FALSE)
betahat.theo.sd <- sqrt(betahat.theo.var)
betahat.theo <- 2  # from the model definition 1 + 2 * x + 5 * rnorm(length(x)) 
lines(seq(1.3, 2.6, by = 0.01), col="red", dnorm(seq(1.3, 2.6, by = 0.01), mean=betahat.theo , sd=betahat.theo.sd))
legend(1.4, 1.55, legend=c("estimation (slope)", "theoretical"), col=c("black", "red"), lty=1:1, cex=0.8)


############################################################################################################
# Problem 4
############################################################################################################



## a)

# simulation
set.seed(21)                          # initializes the random number generator
x <- rnorm(40, 20, 3)                 # generates x-values
nsim <- 100
hatbeta0 <- rep(NA, nsim)             # vector to store estimated beta_0 values
hatbeta1 <- rep(NA, nsim)             # vector to store estimated beta_0 values

for(i in 1:nsim){
  y <- 1 + 2 * x + 5 * (1 - rchisq(length(x), df = 1)) / sqrt(2)
  fit <- lm(y~x)              # fit linear regression
  hatbeta0[i] <- fit$coef[1]  # store estimated intercept
  hatbeta1[i] <- fit$coef[2]  # store estimated slope
}

par(mfrow=c(1,1))     # plot 3x2
errors <- 5 * (1 - rchisq(40, df = 1)) / sqrt(2)
hist(errors)
mean(errors)
var(errors)

par(mfrow=c(3,2))     # plot 3x2
set.seed(21)   # same seed
for(i in 1:3){
  y <- 1 + 2 * x + 5 * (1 - rchisq(length(x), df = 1)) / sqrt(2)
  fit.new <- lm(y~x)              # fit linear regression
  plot(y~x)
  abline(fit.new)
  plot(fit.new, which=1)   # Tukey-Anscombe
  
}


mean(hatbeta1)
sd(hatbeta1)

X = cbind(1,x)                          # design matrix
XtX.inv <- solve(t(X) %*% X)            # (X^T X)^{-1}
betahat.theo.var <- 5^2 * XtX.inv[2,2]
betahat.theo.var

par(mfrow=c(1,1))     # plot 1x1
hist(hatbeta1, breaks=10, freq=FALSE)
betahat.theo.sd <- sqrt(betahat.theo.var)
betahat.theo <- 2  # from the model definition 1 + 2 * x + 5 * rnorm(length(x)) 
lines(seq(1.3, 2.6, by = 0.01), col="red", dnorm(seq(1.3, 2.6, by = 0.01), mean=betahat.theo , sd=betahat.theo.sd))
legend(1.4, 1.55, legend=c("estimation (slope)", "theoretical"), col=c("black", "red"), lty=1:1, cex=0.8)







## b)

# simulation
set.seed(21)                          # initializes the random number generator
x <- rnorm(40, 20, 3)                 # generates x-values
nsim <- 100
hatbeta0 <- rep(NA, nsim)             # vector to store estimated beta_0 values
hatbeta1 <- rep(NA, nsim)             # vector to store estimated beta_0 values

for(i in 1:nsim){
  y <- 1 + 2 * x + 5 * rnorm(length(x), mean = x^2 / 5 - 1, sd = 1)
  fit <- lm(y~x)              # fit linear regression
  hatbeta0[i] <- fit$coef[1]  # store estimated intercept
  hatbeta1[i] <- fit$coef[2]  # store estimated slope
}

par(mfrow=c(1,1))     # plot 3x2
errors <- 5 * rnorm(length(x), mean = x^2 / 5 - 1, sd = 1)
hist(errors)
mean(errors)
var(errors)

par(mfrow=c(3,2))     # plot 3x2
set.seed(21)   # same seed
for(i in 1:3){
  y <- 1 + 2 * x + 5 * rnorm(length(x), mean = x^2 / 5 - 1, sd = 1)
  fit.new <- lm(y~x)              # fit linear regression
  plot(y~x)
  abline(fit.new)
  plot(fit.new, which=1)   # Tukey-Anscombe
  
}


mean(hatbeta1)
sd(hatbeta1)

X = cbind(1,x)                          # design matrix
XtX.inv <- solve(t(X) %*% X)            # (X^T X)^{-1}
betahat.theo.var <- 5^2 * XtX.inv[2,2]
betahat.theo.var

par(mfrow=c(1,1))     # plot 1x1
hist(hatbeta1, breaks=10, freq=FALSE)
betahat.theo.sd <- sqrt(betahat.theo.var)
betahat.theo <- 2  # from the model definition 1 + 2 * x + 5 * rnorm(length(x)) 
lines(seq(1.3, 2.6, by = 0.01), col="red", dnorm(seq(1.3, 2.6, by = 0.01), mean=betahat.theo , sd=betahat.theo.sd))
legend(1.4, 1.55, legend=c("estimation (slope)", "theoretical"), col=c("black", "red"), lty=1:1, cex=0.8)








## c)

# simulation
set.seed(21)                          # initializes the random number generator
x <- rnorm(40, 20, 3)                 # generates x-values
nsim <- 100
hatbeta0 <- rep(NA, nsim)             # vector to store estimated beta_0 values
hatbeta1 <- rep(NA, nsim)             # vector to store estimated beta_0 values

for(i in 1:nsim){
  require(MASS)
  Sigma <- matrix(0.7,40,40)
  diag(Sigma) <- 1
  y   <- 1 + 2 * x + 5 * mvrnorm(n = 1, mu = rep(0, length(x)), Sigma = Sigma)
  fit <- lm(y~x)              # fit linear regression
  hatbeta0[i] <- fit$coef[1]  # store estimated intercept
  hatbeta1[i] <- fit$coef[2]  # store estimated slope
}

par(mfrow=c(1,1))     # plot 3x2
errors <- 5 * mvrnorm(n = 1, mu = rep(0, length(x)), Sigma = Sigma)
hist(errors)
mean(errors)
var(errors)

par(mfrow=c(3,2))     # plot 3x2
set.seed(21)   # same seed
for(i in 1:3){
  require(MASS)
  Sigma <- matrix(0.7,40,40)
  diag(Sigma) <- 1
  y   <- 1 + 2 * x + 5 * mvrnorm(n = 1, mu = rep(0, length(x)), Sigma = Sigma)
  fit.new <- lm(y~x)              # fit linear regression
  plot(y~x)
  abline(fit.new)
  plot(fit.new, which=1)   # Tukey-Anscombe
  
}


mean(hatbeta1)
sd(hatbeta1)

X = cbind(1,x)                          # design matrix
XtX.inv <- solve(t(X) %*% X)            # (X^T X)^{-1}
betahat.theo.var <- 5^2 * XtX.inv[2,2]
betahat.theo.var

par(mfrow=c(1,1))     # plot 1x1
hist(hatbeta1, breaks=10, freq=FALSE)
betahat.theo.sd <- sqrt(betahat.theo.var)
betahat.theo <- 2  # from the model definition 1 + 2 * x + 5 * rnorm(length(x)) 
lines(seq(1.3, 2.6, by = 0.01), col="red", dnorm(seq(1.3, 2.6, by = 0.01), mean=betahat.theo , sd=betahat.theo.sd))
legend(1.6, 2.55, legend=c("estimation (slope)", "theoretical"), col=c("black", "red"), lty=1:1, cex=0.8)










## d)

# simulation
set.seed(21)                          # initializes the random number generator
x <- rnorm(40, 20, 3)                 # generates x-values
nsim <- 100
hatbeta0 <- rep(NA, nsim)             # vector to store estimated beta_0 values
hatbeta1 <- rep(NA, nsim)             # vector to store estimated beta_0 values

for(i in 1:nsim){
  y <- 1 + 2 * x + 5 * rnorm(length(x), mean = 0, sd = (x-15)^2 / 30)
  fit <- lm(y~x)              # fit linear regression
  hatbeta0[i] <- fit$coef[1]  # store estimated intercept
  hatbeta1[i] <- fit$coef[2]  # store estimated slope
}

par(mfrow=c(1,1))     # plot 3x2
errors <-  5 * rnorm(length(x), mean = 0, sd = (x-15)^2 / 30)
hist(errors)
mean(errors)
var(errors)

par(mfrow=c(3,2))     # plot 3x2
set.seed(21)   # same seed
for(i in 1:3){
  y <- 1 + 2 * x + 5 * rnorm(length(x), mean = 0, sd = (x-15)^2 / 30)
  fit.new <- lm(y~x)              # fit linear regression
  plot(y~x)
  abline(fit.new)
  plot(fit.new, which=1)   # Tukey-Anscombe
  
}


mean(hatbeta1)
sd(hatbeta1)

X = cbind(1,x)                          # design matrix
XtX.inv <- solve(t(X) %*% X)            # (X^T X)^{-1}
betahat.theo.var <- 5^2 * XtX.inv[2,2]
betahat.theo.var

par(mfrow=c(1,1))     # plot 1x1
hist(hatbeta1, breaks=10, freq=FALSE)
betahat.theo.sd <- sqrt(betahat.theo.var)
betahat.theo <- 2  # from the model definition 1 + 2 * x + 5 * rnorm(length(x)) 
lines(seq(1.3, 2.6, by = 0.01), col="red", dnorm(seq(1.3, 2.6, by = 0.01), mean=betahat.theo , sd=betahat.theo.sd))
legend(1.6, 2.55, legend=c("estimation (slope)", "theoretical"), col=c("black", "red"), lty=1:1, cex=0.8)


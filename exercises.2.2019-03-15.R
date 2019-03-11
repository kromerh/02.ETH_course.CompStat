setwd("/Users/hkromer/02_PhD/08.ETH_courses/01.CompStat/02.ETH_course.CompStat/")
getwd()

############################################################################################################
# Problem 1
############################################################################################################
# 1. The following R-code genereates an artificial dataset with predictors x1, x2 and response y.


set.seed(0)
n<-100
z1<-rnorm(n)
z2<-rnorm(n)
M=matrix(c(1,1,0.1,-0.1),2,2)
X=t(M%*%rbind(z1,z2))
beta<-c(0.5,-1.0)
x1=X[,1]
x2=X[,2]
y=5+beta[1]*x1+beta[2]*x2 +rnorm(n)



# a) Create a plot of the observations of the two predictor variables x1 and x2.
plot(x1,x2)



# b) Fit a linear model fit1<-lm(y~x1+x2) and print the summary using summary(fit1).
fit1 <- lm(y~x1+x2)
s1 <- summary(fit1)
s1


# c) Recompute the t-value corresponding to βˆ1 by hand using the estimate βˆ1 and
# its estimated standard error se(βˆ1).

# t value by hand
beta1.hat <- s1$coefficients["x1", "Estimate"]
se.beta1.hat <- s1$coefficients["x1", "Std. Error"]
tval.beta1 <- beta1.hat / se.beta1.hat

# t value from R
tval.beta1.fromR <- s1$coefficients["x1", "t value"]

# check if the values are different
abs(tval.beta1 -  tval.beta1.fromR)



# d) Give the definition of a p-value. Then compute the p-value corresponding 
# to βˆ1 using the t-value from part c) and the quantile function of the t-distribution pt().
# Note: You need to provide the correct number of degrees of freedom.



# GIVE DEFINITION of a p-value!
# The p-value is the probability of observing any value equal to |t| or larger,
# where t = (beta1hat - 0)/(SE(beta1hat)) under the null hypothesis which tests beta1 = 0 (there is no 
# relationship between X and Y) versus beta1 != 0 (there is a relationship between X and Y).



# n are the number of observations, already defined
# p is # intercept and two variables x1 and x2
p <- 2
pval.beta1 <- 2*pt(abs(tval.beta1), df=n-p, lower=FALSE)
pval.beta1
pval.beta1.fromR <- s1$coefficients["x1", "Pr(>|t|)"]
pval.beta1.fromR

# check difference
abs(tval.beta1 -  tval.beta1.fromR)





# e) Report the p-value of the overall F-test and reproduce it using anova().
options(digits=10)
s1
pOverall.s1 <- 0.002868773
# overall p-value is 0.002869

# We can also reproduce the p-value by comparing two models:
fit1.small <- lm(y~1)
# and conducting a partial F-test:
s1.anova <- anova(fit1.small, fit1)   
(pOverall.s1.anova <- s1.anova$`Pr(>F)`[2])


abs(pOverall.s1.anova-pOverall.s1)




# f) The overall F-test is significant. However, the p-values for x1 and x2 are not significant. Explain
# how this can be true.

# The overall F-test compares two models: the constant model and a model with both predictors x1 and x2 
# present. The p-values in the table compare a model with one of the predictors versus a model without
# the predictor (i.e. model one is y~x1, model two is y~x1+x2). Only one predictor is enough to make a 
# significant prediction. This is not very surprising since we saw that the x1 and x2 are highly correlated,
# i.e. low x1 correspond to low values of x2 and high values of x1 correspond to high values of x2.




# g) Report the residual standard error, interpret it, and recompute it based on residuals(fit1).
options(digits=5)
res.fromR <- residuals(fit1)

# Compute estimates by hand:
X <- as.matrix( cbind(1, x1, x2) )  # design matrix with [1 x1 x2]. 1 is for the intercept
XtX.inv <- solve(t(X) %*% X)           # (X^T X)^{-1}
(hatbeta <- XtX.inv %*% t(X) %*% y)      # hatbeta = (X^T X)^{-1} X^T y 
# Compare to summary(fit1) which is s1
# s1

# Compute fitted values by hand:
y.hat <- X %*% hatbeta                 # yhat = X hatbeta
# Compare to fitted.values(fit.all):
max( abs(y.hat - fitted.values(fit1)) )

# Re-compute residuals:
res <- y - y.hat 
# Compare to given residuals:
max(abs(res-residuals(fit1)))

# USE R OUTPUT!!!
# Re-compute summary statistics of residuals
summary(res.fromR, digits=3) 
# Compare to summary(fit1)
summary(fit1)

# Re-compute residual standard error (RSE):
p <- 3                                # intercept and two variables
sum(res.fromR^2)/(n-p)                      # sigmahat^2 = RSS/(n-p)
(RSE <- sqrt( sum(res.fromR^2)/(n-p) ))     # RSE = sqrt(RSS/(n-p))
# sigmahat^2 is a measure of goodness of fit.
# It is an estimate of sigma^2, the variance of the statistical errors
# The smaller the number, the better the fit (points closer to the line) 
# The RSE is measured in the same units as the dependent variable.

# Plot
plot(residuals(fit1), ylim=c(-6,6), main="Residuals")
# In case of normally distributed errors, we expect:
#   About 66% of the points are within +/- hat.sigma 
#     from the regression plane (blue dotted lines)
#   Aboute 95% of the points are within +/- 2*hat.sigma 
#     from the regression plane (orange dotted lines)
abline(h=0, lty=2)
abline(h=RSE, lty=3, col="blue", lwd=2)
abline(h=-RSE, lty=3, col="blue", lwd=2)
abline(h=2*RSE, lty=3, col="orange", lwd=2)
abline(h=-2*RSE, lty=3, col="orange", lwd=2)




# h) Report the R2 value, interpret it, and recompute it using residuals(fit1).
# Re-compute R^2:
RSS <- sum( residuals(fit1)^2 )  # residuals = y - yhat
TSS <- sum( (y-mean(y))^2 )
(Rsquared <- 1 - RSS/TSS) 
# Interpretation: proportion of variance explained by regression model
# R**2 is the proportion of the variance in y that is explained by the model
# R**2 = 0, then the model is useless
# R**2 = 1, model explains everything (errors are the smallest)

s1

# Re-compute adjusted R^2:
(Rsquared.adj <- 1 - (RSS/(n-p))/(TSS/(n-1)))
# Adjusted for number of variables in the model




# i) Assume now that we only observed the values for x1 and y whereas x2 is a hidden predictor 
# that we do not observe. Fit the model fit3<-lm(y~x1) and print the summary summary(fit3). 
# Compare the estimated coefficient corresponding to x1 to the one in part b). Interpret the 
# coefficient of x1 in both models.
fit3<-lm(y~x1)
(s3 <- summary(fit3))
s1
# Note: The sign and value of beta1 flipped between the 1st and 3rd model. 
# This shows that the interpretation of each beta_k depends on 
#   the other variables in the model! 
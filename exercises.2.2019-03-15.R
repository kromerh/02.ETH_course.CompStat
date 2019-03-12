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

















############################################################################################################
# Problem 2
############################################################################################################
# 2. In this exercise, we will code a categorical variable by hand. 
# The dataset Carseats contains the number of child car seat sales and several predictors in 
# 400 locations. We will only use the quantitative predictor advertising (local advertising budget 
# for company at each location in thousands of dollars) and the qualitative predictor shelveloc (a factor
# with levels ‘Bad’, ‘Good’ and ‘Medium’ indicating the quality of the shelving location for the car seats 
# at each site). Consider the following R code:
install.packages('ISLR') 

# prepare data
library(ISLR)
data(Carseats) #use ?Carseats for an explaination of the dataset
?Carseats 

shelveloc=Carseats$ShelveLoc
# A factor with levels Bad, Good and Medium indicating the quality of the shelving location 
# for the car seats at each site

sales=Carseats$Sales
# Unit sales (in thousands) at each location

advertising=Carseats$Advertising
# Local advertising budget for company at each location (in thousands of dollars)

# fit using automatic coding
fit<-lm(sales~shelveloc+advertising)
(s <- summary(fit))


# plot results:
coeff <- coefficients(fit)
plot(advertising, sales, col=shelveloc, pch=20)
legend("topleft", c("bad", "medium", "good"), col=c(1,3,2), pch=20)
abline(a=coeff[1], b=coeff[4], col=1, lwd=2)
abline(a=coeff[1]+coeff[3], b=coeff[4], col=3, lwd=2)
abline(a=coeff[1]+coeff[2], b=coeff[4], col=2, lwd=2)



# a) Encode the factor variable shelveloc in the same way as done automatically by R by constructing 
# appropriate predictors a1 and a2. a1 shall be 1 when the level of shelveloc is medium and a2 shall be 1 
# if its level is good. The so-called contrast coding in this case can be seen in Table 1. Fit the model 
# fit_a<-lm(sales~a1+a2+advertising). Verify that fit and fit_a are indeed equal and give an interpretation
# of the coefficients corresponding to a1 and a2.

# boolean vectors for easy construction of a1, a2
bad <- levels(shelveloc)[1]==shelveloc
medium <- levels(shelveloc)[3]==shelveloc
good <- levels(shelveloc)[2]==shelveloc
a1 <- medium*1
a2 <- good*1

fit_a<-lm(sales~a1+a2+advertising)
summary(fit_a)



# slopes of the model are the same, but the intercepts are different according to a1 and a2. 
# a1 corresponds to the intercept for shelvelocMedium added to the baseline intercept (4.89662) of the 
# shelvelocBad data. a2 corresponds to the intercept for shelvelocGood added to the baseline intercept of the 
# shelvelocBad data.




# b) Construct predictor variables b1 and b2 according to the contrast coding in Table 1 and fit the 
# model fit_b<-lm(sales~b1+b2+advertising). Give an interpretation of the coefficients of b1 and b2.
# boolean vectors for easy construction of b1, b2
bad <- levels(shelveloc)[1]==shelveloc
medium <- levels(shelveloc)[3]==shelveloc
good <- levels(shelveloc)[2]==shelveloc
b1 <- bad*1
b2 <- good*1

fit_b<-lm(sales~b1+b2+advertising)
summary(fit_b)

# slopes of the model are the same, but the intercepts are different according to b1 and b2. 
# b1 corresponds to the intercept for shelvelocBad added (subtracted because it is negative) 
# to the baseline intercept (6.64805) of the shelvelocMedium data. b2 corresponds to the intercept 
# for shelvelocGood added to the baseline intercept of the shelvelocMedium data.


# c) Construct predictor variables c1, c2 and c3 according to Table 1.
# Then fit the model fit_c<-lm(sales~c1+c2+c3+advertising). This causes a problem. Why?

bad <- levels(shelveloc)[1]==shelveloc
medium <- levels(shelveloc)[3]==shelveloc
good <- levels(shelveloc)[2]==shelveloc
c1 <- bad*1
c2 <- medium*1
c3 <- good*1

fit_c<-lm(sales~c1+c2+c3+advertising)
summary(fit_c)

# The problem is the dummy encoding, write equations...




# d) Remove the intercept by using fit_c<-lm(-1+...). Interpret the coefficients corresponding 
# to c1, c2 and c3.

fit_c<-lm(sales~-1+c1+c2+c3+advertising)
summary(fit_c)

#  write equations... quite clear then




# e) Show that the fitted values are the same for fit_a, fit_b and fit_c.
# Note: Due to rounding errors the values are not exactly the same. Show that they are very close. 
# R-hint: max(abs(fitted(fit_a)-fitted(fit_b)))

max(abs(fitted(fit_a)-fitted(fit_b)))
max(abs(fitted(fit_b)-fitted(fit_c)))
max(abs(fitted(fit_a)-fitted(fit_c)))





# f) We now want to know if distinguishing between all three categories is significantly better than 
# distinguishing only between “bad” (level bad) and “not bad” (level medium or good) each time also 
# accounting for advertising. In which of the summaries of the fits fit_a, fit_b, fit_c can we see this 
# directly? Explain.
summary(fit)
summary(fit_a)
summary(fit_b)
summary(fit_c)

# We can see this directly in fit_c. Check the equations

# Question: Why are the R2 different between fit and fit_a and fit_b versus fit_c???


# g) Suppose we used the coding from fit_a. Conduct a partial F-test to check if we need to distinguish 
# between medium and good by fitting a model fit_d with a new dummy variable.

bad <- levels(shelveloc)[1]==shelveloc
medium <- levels(shelveloc)[3]==shelveloc
good <- levels(shelveloc)[2]==shelveloc
a1 <- medium*1
a2 <- good*1

# large model: distinguishes between medium and good
fit_a<-lm(sales~a1+a2+advertising)

# small model: does not distinguish between medium and good
d1 <- bad*1
d2 <- bad*0   # not bad --> medium or good
fit_d <- lm(sales~d1+d2+advertising)

# and conducting a partial F-test:
(fit.anova <- anova(fit_d, fit_a) )


# this significant very small p-value means that we can reject the hypothesis of the smaller model, so we
# need to distinguish between the medium and good as done in model fit_a.












############################################################################################################
# Problem 3
############################################################################################################
# 3. The dataset airline contains the monthly number of flight passengers in the USA in the years 1949-1960 
# ranging from January 1949 to December 1960. Read the data with the command: 

airline <- scan("http://stat.ethz.ch/Teaching/Datasets/airline.dat")



# a) Plot the data against time and describe what you observe.
plot(airline, xlab="Index Jan 1949-Dec1960", ylab="Monthly number of flight passengers")

# The monthly number of flight passengers in the USA increases from Jan 1949 to Dec 1960. At the same time
# the spread between one timestamp and the next increases, meaning that for larger x values the spread in
# the y values increases.



# b) Compute the logarithm of the data and plot against time. Comment on the difference.
log.airline <- log(airline)
plot(log.airline , xlab="Index Jan 1949-Dec1960", ylab="Monthly number of flight passengers (log)")

# The monthly number of flight passengers in the USA increases from Jan 1949 to Dec 1960. However in this
# representation the spread in y is not increasing with increasing x.




# c) Define a linear model of the form ...

x1<-rep(c(1,rep(0,11)),12) 
x2<-rep(c(0,1,rep(0,10)),12) 
x3<-rep(c(0,0,1,rep(0,9)),12) 
x4<-rep(c(0,0,0,1,rep(0,8)),12) 
x5<-rep(c(0,0,0,0,1,rep(0,7)),12) 
x6<-rep(c(0,0,0,0,0,1,rep(0,6)),12) 
x7<-rep(c(rep(0,6),1,rep(0,5)),12) 
x8<-rep(c(rep(0,7),1,rep(0,4)),12) 
x9<-rep(c(rep(0,8),1,rep(0,3)),12) 
x10<-rep(c(rep(0,9),1,rep(0,2)),12) 
x11<-rep(c(rep(0,10),1,0),12) 
x12<-rep(c(rep(0,11),1),12) 
t<-1:144
fit <- lm(log.airline~-1+t+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12)
(s <- summary(fit))


# d) Plot the fitted values and residuals against time. Do you think that the model assumptions hold?

# fitted values
yhat <- fitted.values(fit)
# residuals
et <-residuals(fit)
plot(log.airline , xlab="Index Jan 1949-Dec1960", ylab="Monthly number of flight passengers (log)")
lines(t, yhat, col=c(2))
legend("topleft", c("data", "fitted values"), col=c(1,2), pch=20)

plot(t, et, main="Residuals vs time")

# I do not think that the model assumptions hold. For low and high values of t the residuals are small 
# (negative) and for medium values of t the residuals are large. This means that E(eps) is not 0.




# e) Give an interpretation of the parameter beta in the above model if we consider the original scale.
# see notes

# Conduct a partial F-test to check whether we can use four predictors indicating the seasons 
# s1 , · · · , s4 (s1 for spring (month 3,4,5),. . . , s4 for winter (month 12,1,2)) instead of 
#twelve indicators x1, · · · , x12 encoding the month.
s1 <- rep(c(rep(0,2),rep(1,3),rep(0,7)),12) # spring
s2 <- rep(c(rep(0,5),rep(1,3),rep(0,4)),12) # summer
s3 <- rep(c(rep(0,8),rep(1,3),0),12) # herbst
s4 <- rep(c(1,1,rep(0,9),1),12) # summer
fit.seasons <- lm(log.airline~-1+t+s1+s2+s3+s4)
(s.seasons <- summary(fit.seasons))
# fitted values
yhat <- fitted.values(fit.seasons)
# residuals
et <-residuals(fit.seasons)
plot(log.airline , xlab="Index Jan 1949-Dec1960", ylab="Monthly number of flight passengers (log)")
lines(t, yhat, col=c(4))

plot(t, et, main="Residuals vs time")
# and conducting a partial F-test:
(fit.anova <- anova(fit.seasons, fit) )


# the partial F test has a very low p value indicating that we cannot use the four predictors of the seasons 
# but should stick with the 12 indicators encoding each month.

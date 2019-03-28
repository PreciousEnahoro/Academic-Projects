#Question 1

install.packages('Matching')
install.packages('foreign')
library(Matching)
library(foreign)

dj <- read.dta('nsw_dw.dta')
dj.treat <- subset(dj, treat == 1)
dj.control <- subset(dj, treat == 0)

#Obtain the point estimate of the treatment effect by running a simple difference in means.
observed.diffmeans <- mean(dj.treat$re78) - mean(dj.control$re78)

#Obtain the confidence interval by running a univariate linear regression (with just the treat variable)
regression_1 <- lm(re78 ~ treat, data = dj)
summary(regression_1)

SE <- summary(regression_1)[4][["coefficients"]][2,2]
z <- qnorm(0.975)
Estimatedmeaneffect <- coef(regression_1)[2]

confidenceinterval <- c(Estimatedmeaneffect- z*SE, Estimatedmeaneffect+ z*SE)
names(confidenceinterval) <- c('Lower bound', 'Upper Bound')
confidenceinterval


#Question 2
b <- read.dta('cps_controls.dta')
cps <- rbind(dj.treat,b)
cps.treat <- subset(cps, treat == 1)
cps.control <- subset(cps, treat == 0)

#Obtain the point estimate of the treatment effect by running a simple difference in means.
observed.diffmeans <- mean(cps.treat$re78) - mean(cps.control$re78)

#Obtain the confidence interval by running a univariate linear regression (with just the treat variable)
regression_2 <- lm(re78 ~ treat, data = cps)
summary(regression_2)

SE_2 <- summary(regression_2)[4][["coefficients"]][2,2]
z <- qnorm(0.975)
Estimatedmeaneffect_2 <- coef(regression_2)[2]

confidenceinterval <- c(Estimatedmeaneffect_2- z*SE_2, Estimatedmeaneffect_2+ z*SE_2)
names(confidenceinterval) <- c('Lower bound', 'Upper Bound')
confidenceinterval


#Question 3

# Estimate the propensity model
glm1  <- glm(treat~age + I(age^2) + education + I(education^2) + black +
               hispanic + married + nodegree + re74  + I(re74^2) + re75 + I(re75^2)
             , family=binomial, data=cps)

#save data objects
X  <- glm1$fitted
Y  <- cps$re78
Tr  <- cps$treat

# one-to-one matching with replacement (the "M=1" option).
# Estimating the treatment effect on the treated (the "estimand" option defaults to ATT).
rr  <- Match(Y=Y, Tr=Tr, X=X, M=1);
summary(rr)

SE_3 <- rr$se
z <- 1.96
Estimatedmeaneffect_3 <- rr$est

Estimatedmeaneffect_3

confidenceinterval <- c(Estimatedmeaneffect_3- z*SE_3, Estimatedmeaneffect_3+ z*SE_3)
names(confidenceinterval) <- c('Lower bound', 'Upper Bound')
confidenceinterval

# covariate balance check
mb  <- MatchBalance(treat~age + I(age^2) + education + I(education^2) + black +
                      hispanic + married + nodegree + re74  + I(re74^2) + re75 + I(re75^2)
                    , data=cps, match.out=rr, nboots=500)

#Question 4

# Estimate the propensity model
glm1  <- glm(treat~age + I(age^2) + education + I(education^2) + black +
               hispanic + married + nodegree + re74  + I(re74^2) + re75 + I(re75^2)
             , family=binomial, data=cps)

#save data objects
X  <- glm1$fitted #propensity scores
Y  <- cps$re78
Tr  <- cps$treat
Z <- cbind(cps$age, cps$education, cps$black, cps$hispanic, cps$married, cps$nodegree, cps$re74, cps$re75)

# one-to-one matching with replacement (the "M=1" option).
# Estimating the treatment effect on the treated (the "estimand" option defaults to ATT).
rr  <- Match(Y=Y, Tr=Tr, X=cbind(X, Z), M=1);
summary(rr)

SE_4 <- rr$se
z <- 1.96
Estimatedmeaneffect_4 <- rr$est

Estimatedmeaneffect_4

confidenceinterval <- c(Estimatedmeaneffect_4- z*SE_4, Estimatedmeaneffect_4+ z*SE_4)
names(confidenceinterval) <- c('Lower bound', 'Upper Bound')
confidenceinterval

# covariate balance check
mb  <- MatchBalance(treat~age + I(age^2) + education + I(education^2) + black +
                      hispanic + married + nodegree + re74  + I(re74^2) + re75 + I(re75^2)
                    , data=cps, match.out=rr, nboots=500)

#Here, we add the other covariates to the matching, and we get a better p-value after matching
#because we are using the other covariates, so there is more freedom, and a way to better match the 
#units.


#Question 5

## Number 3
attach(cps)

#The covariates we want to obtain balance on
BalanceMat <- cbind(cps$age, cps$education, cps$black, cps$hispanic, cps$married, cps$nodegree, cps$re75, cps$re74,
                    I(cps$re74*cps$re75))

#GenMatch() to find the optimal weight to give each covariate in 'X' 

#install.packages('rgenoud')
genout <- GenMatch(Tr=Tr, X=X, BalanceMatrix=BalanceMat, estimand="ATT", M=1,
                   pop.size=100, max.generations=10, wait.generations=1)

#The outcome variable
Y=re78/1000

# Estimate causal effect of interest using GenMatch() weights
mout <- Match(Y=Y, Tr=Tr, X=X, estimand="ATT", Weight.matrix=genout)
summary(mout)

SE_5 <- (mout$se)*1000
z <- 1.96
Estimatedmeaneffect_5 <- (mout$est)*1000

Estimatedmeaneffect_5

confidenceinterval <- c(Estimatedmeaneffect_5- z*SE_5, Estimatedmeaneffect_5+ z*SE_5)
names(confidenceinterval) <- c('Lower bound', 'Upper Bound')
confidenceinterval

#covariate balance check
mb <- MatchBalance(treat~age +education+black+ hispanic+ married+ nodegree+
                     re75+ re74+ I(re74*re75),
                   data = cps,match.out=mout, nboots=500)

##Question 4

attach(cps)

#The covariates we want to match on
#X <- cbind(cps$age, cps$education, cps$black, cps$hispanic, cps$married, cps$nodegree, cps$re74, cps$re75)

#The covariates we want to obtain balance on
BalanceMat <- cbind(cps$age, cps$education, cps$black, cps$hispanic, cps$married, cps$nodegree, cps$re75, cps$re74,
                    I(cps$re74*cps$re75))

#GenMatch() to find the optimal weight to give each covariate in 'X' 

#install.packages('rgenoud')

genout <- GenMatch(Tr=Tr, X=cbind(X, Z), BalanceMatrix=BalanceMat, estimand="ATT", M=1,
                   pop.size=100, max.generations=10, wait.generations=1)

#The outcome variable
Y=re78/1000


# Estimate causal effect of interest using GenMatch() weights
mout <- Match(Y=Y, Tr=Tr, X=cbind(X, Z), estimand="ATT", Weight.matrix=genout)
summary(mout)

SE_5 <- (mout$se)*1000
z <- 1.96
Estimatedmeaneffect_5 <- (mout$est)*1000

Estimatedmeaneffect_5

confidenceinterval <- c(Estimatedmeaneffect_5- z*SE_5, Estimatedmeaneffect_5+ z*SE_5)
names(confidenceinterval) <- c('Lower bound', 'Upper Bound')
confidenceinterval

#covariate balance check
mb <- MatchBalance(treat~age +education+black+ hispanic+ married+ nodegree+
                     re75+ re74+ I(re74*re75),
                   data = cps,match.out=mout, nboots=500)

#Extra credit

#Question 3
#increase M

attach(cps)

#The covariates we want to obtain balance on
BalanceMat <- cbind(cps$age, cps$education, cps$black, cps$hispanic, cps$married, cps$nodegree, cps$re75, cps$re74,
                    I(cps$re74*cps$re75))

#GenMatch() to find the optimal weight to give each covariate in 'X' 

#install.packages('rgenoud')
genout <- GenMatch(Tr=Tr, X=X, BalanceMatrix=BalanceMat, estimand="ATT", M=3,
                   pop.size=100, max.generations=10, wait.generations=1)

#The outcome variable
Y=re78/1000

# Estimate causal effect of interest using GenMatch() weights
mout <- Match(Y=Y, Tr=Tr, X=X, estimand="ATT", Weight.matrix=genout)
summary(mout)

#covariate balance check
mb <- MatchBalance(treat~age +education+black+ hispanic+ married+ nodegree+
                     re75+ re74+ I(re74*re75),
                   data = cps,match.out=mout, nboots=500)

#increase pop size
attach(cps)

#The covariates we want to obtain balance on
BalanceMat <- cbind(cps$age, cps$education, cps$black, cps$hispanic, cps$married, cps$nodegree, cps$re75, cps$re74,
                    I(cps$re74*cps$re75))

#GenMatch() to find the optimal weight to give each covariate in 'X' 

#install.packages('rgenoud')
genout <- GenMatch(Tr=Tr, X=X, BalanceMatrix=BalanceMat, estimand="ATT", M=1,
                   pop.size=500, max.generations=10, wait.generations=1)

#The outcome variable
Y=re78/1000

# Estimate causal effect of interest using GenMatch() weights
mout <- Match(Y=Y, Tr=Tr, X=X, estimand="ATT", Weight.matrix=genout)
summary(mout)

#covariate balance check
mb <- MatchBalance(treat~age +education+black+ hispanic+ married+ nodegree+
                     re75+ re74+ I(re74*re75),
                   data = cps,match.out=mout, nboots=500)


#with caliper - Question 3
attach(cps)

#The covariates we want to obtain balance on
BalanceMat <- cbind(cps$age, cps$education, cps$black, cps$hispanic, cps$married, cps$nodegree, cps$re75, cps$re74,
                    I(cps$re74*cps$re75))

#GenMatch() to find the optimal weight to give each covariate in 'X' 

#install.packages('rgenoud')
genout <- GenMatch(Tr=Tr, X=X, BalanceMatrix=BalanceMat, estimand="ATT", M=1,
                   pop.size=100, max.generations=10, wait.generations=1, caliper= 0.0001)

#The outcome variable
Y=re78/1000

# Estimate causal effect of interest using GenMatch() weights
mout <- Match(Y=Y, Tr=Tr, X=X, estimand="ATT", Weight.matrix=genout, caliper= 0.0001)
summary(mout)

#covariate balance check
mb <- MatchBalance(treat~age +education+black+ hispanic+ married+ nodegree+
                     re75+ re74+ I(re74*re75),
                   data = cps,match.out=mout, nboots=500)


#with caliper - Question 4
attach(cps)

#The covariates we want to match on
#X <- cbind(cps$age, cps$education, cps$black, cps$hispanic, cps$married, cps$nodegree, cps$re74, cps$re75)

#The covariates we want to obtain balance on
BalanceMat <- cbind(cps$age, cps$education, cps$black, cps$hispanic, cps$married, cps$nodegree, cps$re75, cps$re74,
                    I(cps$re74*cps$re75))

#GenMatch() to find the optimal weight to give each covariate in 'X' 

#install.packages('rgenoud')

genout <- GenMatch(Tr=Tr, X=cbind(X, Z), BalanceMatrix=BalanceMat, estimand="ATT", M=1,
                   pop.size=100, max.generations=10, wait.generations=1, caliper = 0.5)

#The outcome variable
Y=re78/1000


# Estimate causal effect of interest using GenMatch() weights
mout <- Match(Y=Y, Tr=Tr, X=cbind(X, Z), estimand="ATT", Weight.matrix=genout, caliper = 0.5)
summary(mout)

#covariate balance check
mb <- MatchBalance(treat~age +education+black+ hispanic+ married+ nodegree+
                     re75+ re74+ I(re74*re75),
                   data = cps,match.out=mout, nboots=500)

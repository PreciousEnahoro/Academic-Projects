install.packages('rbounds')
library(rbounds)
install.packages('Matching')
library(Matching)
demo(GerberGreenImai)


#GenMatch

data(lalonde)
attach(lalonde)

#The covariates we want to match on
X = cbind(age, educ, black, hisp, married, nodegr, u74, u75, re75, re74)

#The covariates we want to obtain balance on
BalanceMat <- cbind(age, educ, black, hisp, married, nodegr, u74, u75, re75, re74,
                    I(re74*re75))

#
#Let's call GenMatch() to find the optimal weight to give each
#covariate in 'X' so as we have achieved balance on the covariates in
#'BalanceMat'. This is only an example so we want GenMatch to be quick
#so the population size has been set to be only 16 via the 'pop.size'
#option. This is *WAY* too small for actual problems.
#For details see http://sekhon.berkeley.edu/papers/MatchingJSS.pdf.
#
install.packages('rgenoud')

genout <- GenMatch(Tr=treat, X=X, BalanceMatrix=BalanceMat, estimand="ATE", M=1,
                   pop.size=16, max.generations=10, wait.generations=1)

#The outcome variable
Y=re78/1000

#
# Now that GenMatch() has found the optimal weights, let's estimate
# our causal effect of interest using those weights
#
mout <- Match(Y=Y, Tr=treat, X=X, estimand="ATE", Weight.matrix=genout)
summary(mout)

#                        
#Let's determine if balance has actually been obtained on the variables of interest
#                        
mb <- MatchBalance(treat~age +educ+black+ hisp+ married+ nodegr+ u74+ u75+
                     re75+ re74+ I(re74*re75),
                   match.out=mout, nboots=500)

mout$index.treated
install.packages("quantreg")
library(quantreg)
??quantreg
quantile(mout$index.treated)
quantile(mout$index.control)

install.packages('Hmisc')
library('Hmisc')
t <- wtd.quantile(x =lalonde$re78[mout$index.treated], weights = mout$weights)
c <- wtd.quantile(x =lalonde$re78[mout$index.control], weights = mout$weights)
t-c

??rq
rq(formula =lalonde$re78 ~ lalonde$treat, tau = 0.5)


?tau

plot(rq(formula =lalonde$re78 ~ lalonde$treat, tau = 0.5))

https://rstudio-pubs-static.s3.amazonaws.com/152505_49d1881e3fe64f0bad072282c36a6ca5.html

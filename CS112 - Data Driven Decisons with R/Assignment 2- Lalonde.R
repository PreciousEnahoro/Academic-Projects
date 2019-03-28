#Question 1
install.packages("Matching")
library(Matching)
data('lalonde')

#Regresion with all predictors
regression_1 <- lm(re78 ~., data = lalonde)
summary(regression_1)

nohighschooldegree <- subset(lalonde, nodegr == 1)
havehighschooldegree <- subset(lalonde, nodegr == 0)

#Regression with all predictors for people with degree
regression_2 <- lm(re78 ~ .-nodegr, data = havehighschooldegree)
summary(regression_2) 

#Regression with all predictors for people without degree
regression_3 <- lm(re78 ~ .-nodegr, data = nohighschooldegree)
summary(regression_3)

#CI for regression 1
SEfortreat <- summary(regression_1)[4][["coefficients"]][12,2]
z <- qnorm(0.975)
Estimatedmeaneffect <- coef(regression_1)[12]

confidenceinterval <- c(Estimatedmeaneffect- z*SEfortreat, Estimatedmeaneffect+ z*SEfortreat)
names(confidenceinterval) <- c('Lower bound', 'Upper Bound')
confidenceinterval

#CI for regression 2 
SEfortreat_2<- summary(regression_2)[4][["coefficients"]][11,2]
z_2 <- qnorm(0.975)
Estimatedmeaneffect_2 <- coef(regression_2)[11]

confidenceinterval_2 <- c(Estimatedmeaneffect_2- z_2*SEfortreat_2, Estimatedmeaneffect_2+ z_2*SEfortreat_2)
names(confidenceinterval_2) <- c('Lower bound', 'Upper Bound')
confidenceinterval_2

# CI for regression3
SEfortreat_3 <- summary(regression_3)[4][["coefficients"]][11,2]
z_3 <- qnorm(0.975)
Estimatedmeaneffect_3 <- coef(regression_3)[11]

confidenceinterval_3 <- c(Estimatedmeaneffect_3- z_3*SEfortreat_3, Estimatedmeaneffect_3+ z_3*SEfortreat_3)
names(confidenceinterval_3) <- c('Lower bound', 'Upper Bound')
confidenceinterval_3


#Question 2
install.packages('randomForest')
library(randomForest)
set.seed (13)
forestwdegree =randomForest(re78 ~.-nodegr, data = havehighschooldegree,mtry= 3,importance =TRUE)
forestwdegree
importance(forestwdegree)
varImpPlot(forestwdegree) #explained in text book

forestwnodegree =randomForest(re78 ~.-nodegr, data = nohighschooldegree,mtry= 3,importance =TRUE)
forestwnodegree
importance(forestwnodegree)
varImpPlot(forestwnodegree) 

#for prediction, change the treat parts to 0, for both degree and non-degree
#subset for treat

newdegree <- subset(havehighschooldegree, treat == 1)
newdegree$treat <- rep(0,54) 
newnodegree <- subset(nohighschooldegree, treat == 1)
newnodegree$treat <- rep(0,131) 

#prediction for counterfactuals of people with degree
predictnewdegree <- predict(forestwdegree, newdegree)

#prediction for counterfactuals of people without degree
predictnewnodegree <- predict(forestwnodegree, newnodegree)

#differential averages
#FOR DEGREE
originaldegreetreat <- subset(havehighschooldegree, treat == 1)
mean(originaldegreetreat$re78) - mean(predictnewdegree)

#FOR NO DEGREE
originalnodegreetreat <- subset(nohighschooldegree, treat == 1)
mean(originalnodegreetreat$re78) - mean(predictnewnodegree)


#Question 3

treat <- subset(lalonde, treat == 1)
control <- subset(lalonde, treat == 0)

observed.diffmeans <- mean(treat$re78) - mean(control$re78)
#observed treatment effect

print(observed.diffmeans)

#Assignment Mechanism

experiment <- function() {
storage.vector <- NA
  k = 1
  for (i in 1:445) {
    if(
      sample(x = c(1,0), size = 1, prob = c(185/445, 
          260/445)) == 1) {
      storage.vector[k] <- i
      k = k + 1
    }
  }
  return(list(treated.units = storage.vector, 
              control.units = c(1:445)[-storage.vector]))
}

#Calculating the treatment effect through difference in means between treatment and control groups 
difference_in_means_storer <- NA
for (i in 1:10000){
    experiment_results <-experiment()
    treatment_group <- unlist(experiment_results[1])
    control_group <- unlist(experiment_results[2])
    difference_in_means_storer[i] <- mean(lalonde[treatment_group,]$re78) - mean(lalonde[control_group,]$re78)
  }

#Calculating p_value
p_value <- length(difference_in_means_storer[difference_in_means_storer >= observed.diffmeans]) / length(difference_in_means_storer)
p_value

density.plot <- plot(density(difference_in_means_storer))
abline(v= observed.diffmeans, lwd = 2, col = 'blue')
abline(v=quantile(difference_in_means_storer, prob= 0.95), lwd=2, col="red") #shows the alpha value

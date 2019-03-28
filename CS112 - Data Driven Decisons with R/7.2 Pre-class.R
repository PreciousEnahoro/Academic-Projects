storage.vector <- NA
vector.people <- c(0.68,0.42,0.73,0.79,0.63,0.4,0.38,0.44,0.41)

# Function that assigns treatment/control depending on 
# propensity scores (assignment probabilities)
experiment <- function(vector.of.probabilities = NULL) {
  k = 1
  for (i in 1:length(vector.of.probabilities)) {
    if(
      sample(x = c(1,0), size = 1, prob = c(vector.of.probabilities[i], 
                                            1 - vector.of.probabilities[i])) == 1) {
      storage.vector[k] <- i
      k = k + 1
    }
  }
  return(list(treated.units = storage.vector, 
              control.units = (1:(length(vector.of.probabilities)))[-storage.vector]))
}
#prob is showing the probability of treatment on the left side, and probability of control on the other side.
#Since they are not evenly matched, it differs, so higher on left means higher chance of treatment.
#higher weight of being sampled as 1

set.seed(12345)
men.income <- round(abs(exp(rnorm(1000, 5, 1))))
women.children <- round(sqrt(abs((rnorm(1000, 12, 100)))))

min(men.income)
max(men.income)
hist(men.income)

min(women.children)
max(women.children)
hist(women.children)

experiment(vector.of.probabilities = vector.people)
?sample


# 7.2 Class
set.seed(123)
nokids.income <- round(abs(exp(rnorm(1000, 5, 1))))
kids.hhsize <- round(sqrt(abs(rnorm(1000, 12, 100))) + .3)
max(nokids.income)
max(kids.hhsize)
probs.wnokids <- 0.5*((((max(nokids.income) + 100) -
                          nokids.income)/(max(nokids.income) + 100))^4)
#people with less income are assigned to treatment

hist(probs.wnokids)
summary(probs.wnokids)

probs.wnokids
probs.wyeskids <- kids.hhsize/(max(kids.hhsize) + 1)
#people with more children are assigned to treatment

probs.wyeskids
hist(probs.wyeskids)
summary(probs.wyeskids)



# Install the Matching package if you have not done so already
install.packages("Matching")

# Load the Matching library
library(Matching)

# Load lalonde data set, and name it "foo"
foo <- data(lalonde)

# Recall that this "lalonde" data set identifies treatment & control groups (the "treat" variable)
?Match
# Open the help file associated with the "Match" function. (How?) (Do capitalize the word "Match".)

# Scroll down the help file page to the "Examples" section.

# Follow along until you get to the "glm1" line -- run it (actually 3 lines b/c it's long)
# Then, type the following:
glm1$fitted.values

# What does "glm1$fitted.values" show you? - probability of treatment assignment (propensity score)

# Plot the histogram of these values (figure out how).

# Then, define the values SEPARATELY for treated units and control units. How?
# Plot them SEPARATELY, if you can, and compare them.


##Extra credit: Bootstrap confidence intervals for the random forest results.
#I decided to do a bootstrap that would give confidence intervals for the mean of the counterfactual re78.

library(boot)

#for mean of counterfactuals - degree
counterfactuals <- predictnewdegree
sampledrawn <- sample(counterfactuals, 15, replace = FALSE)
boot.mean <- function(data, index){
  return(mean(data[index]))
}

result <- boot(sampledrawn, boot.mean, 10000)
boot.ci(result, type="bca")
summary(result)

sample.mean = mean(sampledrawn) 
sample.se = sd(sampledrawn)/sqrt(length(sampledrawn))
sampleCI = c(sample.mean - 2.15 *sample.se, sample.mean + 2.15*sample.se)  
sampleCI

#for mean of counterfactuals - no degree
counterfactuals <- predictnewnodegree
sampledrawn <- sample(counterfactuals, 15, replace = FALSE)
boot.mean <- function(data, index){
  return(mean(data[index]))
}

result <- boot(sampledrawn, boot.mean, 10000)
boot.ci(result, type="bca")
summary(result)

sample.mean = mean(sampledrawn) 
sample.se = sd(sampledrawn)/sqrt(length(sampledrawn))
sampleCI = c(sample.mean - 2.15*sample.se, sample.mean + 2.15*sample.se) 
sampleCI
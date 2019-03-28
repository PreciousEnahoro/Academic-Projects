library(boot)
pop <- rnorm(10000, 0, 5)
obs <- sample(pop, 15, replace = FALSE)
boot.mean <- function(data, index){
  return(mean(data[index]))
}

result <- boot(obs, boot.mean, 10000)
boot.ci(result, type="bca")
summary(result)
qt(a, df = 14)

obs.mean = mean(obs) 
obs.se = sd(obs)/sqrt(length(obs))
samp.ci = c(obs.mean - 2.15*obs.se, obs.mean + 2.15*obs.se)
samp.ci

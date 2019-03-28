install.packages("Synth")
library(Synth)
data(basque)

#Placebo in Space Code

dataprep.out <-
  dataprep(
    foo = basque
    ,predictors= c("school.illit",
                   "school.prim",
                   "school.med",
                   "school.high",
                   "school.post.high"
                   ,"invest"
    )
    ,predictors.op = c("mean")
    ,dependent     = c("gdpcap")
    ,unit.variable = c("regionno")
    ,time.variable = c("year")
    ,special.predictors = list(
      list("gdpcap",1960:1969,c("mean")),                            
      list("sec.agriculture",seq(1961,1969,2),c("mean")),
      list("sec.energy",seq(1961,1969,2),c("mean")),
      list("sec.industry",seq(1961,1969,2),c("mean")),
      list("sec.construction",seq(1961,1969,2),c("mean")),
      list("sec.services.venta",seq(1961,1969,2),c("mean")),
      list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
      list("popdens",1969,c("mean")))
    ,treatment.identifier  = 10
    ,controls.identifier   = c(2:9,11:16,18)
    ,time.predictors.prior = c(1964:1969) 
    ,time.optimize.ssr     = c(1960:1969)
    ,unit.names.variable   = c("regionname")
    ,time.plot            = c(1955:1997) 
  )

# run synth
synth.out <- synth(data.prep.obj = dataprep.out, method = "BFGS")

# plot results:
# path
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = c("real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(0,13), 
          Legend = c("Catalonia country","synthetic Catalonia country"),
          
) 


#Placebo in Time Code

#basque

dataprep.out <-
  dataprep(
    foo = basque
    ,predictors= c("school.illit",
                   "school.prim",
                   "school.med",
                   "school.high",
                   "school.post.high"
                   ,"invest"
    )
    ,predictors.op = c("mean")
    ,dependent     = c("gdpcap")
    ,unit.variable = c("regionno")
    ,time.variable = c("year")
    ,special.predictors = list(
      list("gdpcap",1960:1969,c("mean")),                            
      list("sec.agriculture",seq(1961,1969,2),c("mean")),
      list("sec.energy",seq(1961,1969,2),c("mean")),
      list("sec.industry",seq(1961,1969,2),c("mean")),
      list("sec.construction",seq(1961,1969,2),c("mean")),
      list("sec.services.venta",seq(1961,1969,2),c("mean")),
      list("sec.services.nonventa",seq(1961,1969,2),c("mean")),
      list("popdens",1969,c("mean")))
    ,treatment.identifier  = 17
    ,controls.identifier   = c(2:16,18)
    ,time.predictors.prior = c(1964:1969) 
    ,time.optimize.ssr     = c(1960:1972)
    ,unit.names.variable   = c("regionname")
    ,time.plot            = c(1955:1997) 
  )

# run synth
synth.out <- synth(data.prep.obj = dataprep.out, method = "BFGS")

# plot results:
# path
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = c("real per-capita GDP (1986 USD, thousand)"),
          Xlab = c("year"), 
          Ylim = c(0,13), 
          Legend = c("Basque country","synthetic Basque country"),
          abline(v=c(1972, 1975),lty="dashed"),
          
) 

gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)

#[21] is year 1975, when terrorism became widespread according to readings

print(p_value <- length(which(gaps>gaps[21]))/length(gaps))
install.packages('rdrobust')
library(rdrobust)
data("rdrobust_RDsenate")
vote <- rdrobust_RDsenate$vote
margin <- rdrobust_RDsenate$margin
summary(vote)
summary(margin)
c<- (rdplot(y = vote, x = margin, title = "RD Plot - Senate Elections Data",
        y.label = "Vote Share in Election at time t+1",
        x.label = "Vote Share in Election at time t"))
a <- (rdplot(y = vote, x = margin, binselect = "es",
         title = "RD Plot - Senate Elections Data",
        y.label = "Vote Share in Election at time t+1",
         x.label = "Vote Share in Election at time t"))
b <- (rdplot(y = vote, x = margin, binselect = "es", scale = 5,
         title = "RD Plot - Senate Elections Data",
         y.label = "Vote Share in Election at time t+1",
        x.label = "Vote Share in Election at time t"))

a$coef
b$coef
c$coef
a$ci
b$ci
d <-rdrobust(y = vote, x = margin)
e <-rdrobust(y = vote, x = margin, all = TRUE)
d$coef
e$coef
rdbwselect(y = vote, x = margin, all = TRUE)
#rdbwselect(y = vote, x = margin, bwselect = "CV",
        #cvgrid_min = 10, cvgrid_max = 80, cvplot = TRUE)

#mine
grades <- read.csv('grades.csv')
initial <- grades$Initial
after <- grades$After
summary(initial)
summary(after)
(rdplot(y = after, x = initial, c = 6, title = "RD Plot - Maths Grades",
        y.label = "Grades after coaching",
        x.label = "Grades before coaching"))
(rdplot(y = after, x = initial, c = 6, binselect = "es", scale = 5,
        title = "RD Plot - Maths Grades",
        y.label = "Grades after coaching",
        x.label = "Grades before coaching"))
rdrobust(y = after, x = initial, c= 6)

treat <- grades$Scores.for.treatment.group
control <-grades$Scores.for.control.group
(rdplot(y = control, x = treat, c = 10, title = "RD Plot - Maths Grades",
        y.label = "Grades after coaching",
        x.label = "Grades before coaching"))

#Eynat
wins <- read.csv('wins3.csv')
x <- wins$x
observable <- wins$observable
(rdplot(y = observable, x = x, c = 30, title = "RD Plot - Wins",
        y.label = "Observable",
        x.label = "Initial"))
a <- rdrobust(y = observable, x = x, c= 30)
a$coef
a$ci
?rdrobust

TrueTreatmentEffect <- 31.62289 - 39.77417826
TrueTreatmentEffect

#treatment effect is only seen as positive if treatment is on the right
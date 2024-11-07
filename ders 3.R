data('gpa1')
library(wooldridge)

####sat: combined SAT score
#tothrs: total hours through fall semest
#colgpa: GPA after fall semester
#athlete: =1 if athlete
#verbmath: verbal/math SAT score
#hsize: size grad. class, 100s
#hsrank: rank in grad. class
#hsperc: high school percentile, from top
#female: =1 if female
#white: =1 if white
#black: =1 if black
#hsizesq: hsize^2
###
data('gpa2')
sat = gpa2$sat
tothrs = gpa2$tothrs
colgpa = gpa2$colgpa
female = gpa2$female
hsrank = gpa2$hsrank


model = lm(sat ~ tothrs + colgpa + female + hsrank )
summary(model)
confint(model, level = 0.90)


install.packages("car")
library(car)
# Test if the coefficient of tothrs (b1) is equal to 0.5
linearHypothesis(model, "tothrs = 0.5")

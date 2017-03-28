

# Q1 ----------------------------------------------------------------------

dat = data.frame(interview=c("no", "yes"), cases=c(195,46), controls=c(979,370))
dat$interview

fm = glm(cbind(cases,controls) ~ interview, family=binomial, data=dat)
summary(fm) # interview coef is significant
anova(fm, test="Chisq") # interview coef is significant, there is a relationship between participation and medical aid
exp(coef(fm)[2]) # odds of having medical aid from interviewed mothers is 0.624 times the odds of not interviewed

# calculate CI of log odds, then transform back
lor_CI = coef(fm)[2] + c(-1,1)*qnorm(0.975)*sqrt(diag(vcov(fm))[2])
exp(lor_CI) # CI does not include 1, participation is a risk factor for medical aid involvment

plot(cases/(cases+controls) ~ interview, data=dat)
plot(predict(fm))

dat_white = data.frame(interview=c("no", "yes"), cases=c(104,10), controls=c(22,2))
dat_black = data.frame(interview=c("no", "yes"), cases=c(91,36), controls=c(957,368))

(fm_white = glm(cbind(cases,controls) ~ interview, family=binomial, data=dat_white))
(fm_black = glm(cbind(cases,controls) ~ interview, family=binomial, data=dat_black))
# both shows that participation doesnt affect the medical aid status
anova(fm_white, test="Chisq")
anova(fm_black, test="Chisq")

# med aid may have higher dependence on black / white race instead on participation
dat_race = data.frame(race=c("black", "yes"), cases=c(127, 114), controls=c(1325, 24))
(fm_race = glm(cbind(cases,controls) ~ race, family=binomial, data=dat_race))
anova(fm_race, test="Chisq") # zero residual deviance


# Q2 ----------------------------------------------------------------------



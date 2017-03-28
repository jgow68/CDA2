

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

data_set2 = read.csv("task2.csv", header=T)
str(data_set2)





# Q2a ---------------------------------------------------------------------

# smoking and family as response, rest as explanatory variables
# min model is (family, smoking, race:sex:age)

fm = glm(Count ~ (.)^5, data_set2, family=poisson())
summary(fm)

drop1(fm, test="Chisq")
fm = update(fm, .~. -Family:Race:Sex:Age:Smoking_I) # drop 5 way interaction (p-value 0.05644)
 
drop1(fm, test="Chisq") # drop Family:Race:Sex:Age, p-value=0.7923
fm = update(fm, .~. -Family:Race:Sex:Age)

drop1(fm, test="Chisq") # drop Family:Race:Sex:Smoking_I, p-value 0.5966
fm = update(fm, .~. -Family:Race:Sex:Smoking_I)

drop1(fm, test="Chisq") # drop Family:Race:Sex, p-value 0.7459
fm = update(fm, .~. -Family:Race:Sex)

drop1(fm, test="Chisq") # drop ace:Sex:Age:Smoking_I, p-value 0.61798
fm = update(fm, .~. -Race:Sex:Age:Smoking_I)

drop1(fm, test="Chisq") # can't drop Race:Sex:Age, drop Race:Sex:Smoking_I, pva=0.57454
fm = update(fm, .~. -Race:Sex:Smoking_I)

drop1(fm, test="Chisq") # can't drop Race:Sex:Age, drop Family:Sex:Age:Smoking_I, pva=0.34809
fm = update(fm, .~. -Family:Sex:Age:Smoking_I)

drop1(fm, test="Chisq") # can't drop Race:Sex:Age, drop Sex:Age:Smoking_I , pva=0.81672
fm = update(fm, .~. -Sex:Age:Smoking_I)

drop1(fm, test="Chisq") # can't drop Race:Sex:Age, drop Family:Sex:Age  , pva=0.29404
fm = update(fm, .~. -Family:Sex:Age)

drop1(fm, test="Chisq") # can't drop Race:Sex:Age, drop Family:Sex:Smoking_I , pva=0.12656
fm = update(fm, .~. -Family:Sex:Smoking_I)

drop1(fm, test="Chisq") # can't drop Race:Sex:Age, drop Family:Sex , pva=0.46289
fm = update(fm, .~. -Family:Sex)

drop1(fm, test="Chisq") # can't drop Race:Sex:Age, drop Sex:Smoking_I, pva=0.1887
fm = update(fm, .~. -Sex:Smoking_I)

drop1(fm, test="Chisq") # can't drop Race:Sex:Age, drop Family:Race:Age:Smoking_I, pva=0.0847
fm = update(fm, .~. -Family:Race:Age:Smoking_I)

drop1(fm, test="Chisq") # can't drop Race:Sex:Age, drop Family:Age:Smoking_I, pva=0.95417
fm = update(fm, .~. -Family:Age:Smoking_I)

drop1(fm, test="Chisq") # can't drop Race:Sex:Age, drop Family:Race:Age , pva=0.8473
fm = update(fm, .~. -Family:Race:Age )

drop1(fm, test="Chisq") # can't drop Race:Sex:Age, drop Race:Age:Smoking_I , pva=0.2882
fm = update(fm, .~. -Race:Age:Smoking_I )

drop1(fm, test="Chisq") # can't drop Race:Sex:Age, drop Family:Age   , pva=0.26258
fm = update(fm, .~. -Family:Age)

drop1(fm, test="Chisq") # can't drop Race:Sex:Age, drop Family:Race:Smoking_I, pva=0.05113
fm = update(fm, .~. -Family:Race:Smoking_I)

drop1(fm, test="Chisq") # can't drop Race:Sex:Age, drop Race:Smoking_I, pva=0.450285
fm = update(fm, .~. -Race:Smoking_I)

drop1(fm, test="Chisq") # can't drop Race:Sex:Age, cant drop any more
# Final model:  Count ~ Family + Race + Sex + Age + Smoking_I + Family:Race + 
# Family:Smoking_I + Race:Sex + Race:Age + Sex:Age + Age:Smoking_I + Race:Sex:Age

pchisq(fm$deviance, fm$df.residual, lower.tail=F) # pval 0.334 reject H0, model is adequate

# state the conditional independence structure in the selected model

# Q2b ---------------------------------------------------------------------

# smoking as response, rest as explanatory variables
# min model is (smoking, family:race:sex:age)

fm = glm(Count ~ (.)^5, data_set2, family=poisson())
summary(fm)

drop1(fm, test="Chisq")
fm = update(fm, .~. -Family:Race:Sex:Age:Smoking_I) # drop 5 way interaction (p-value 0.05644)

drop1(fm, test="Chisq") # cannot drop Family:Race:Sex:Age , drop Family:Race:Sex:Smoking_I, pval 0.6502
fm = update(fm, .~. -Family:Race:Sex:Smoking_I)

drop1(fm, test="Chisq") # cannot drop Family:Race:Sex:Age , drop Race:Sex:Age:Smoking_I, pval 0.64504
fm = update(fm, .~. -Race:Sex:Age:Smoking_I)

drop1(fm, test="Chisq") # cannot drop Family:Race:Sex:Age , drop Race:Sex:Smoking_I , pval 0.53462
fm = update(fm, .~. -Race:Sex:Smoking_I )

drop1(fm, test="Chisq") # cannot drop Family:Race:Sex:Age , drop Family:Sex:Age:Smoking_I , pval 0.36547
fm = update(fm, .~. -Family:Sex:Age:Smoking_I)

drop1(fm, test="Chisq") # cannot drop Family:Race:Sex:Age , drop Sex:Age:Smoking_I  , pval 0.79750
fm = update(fm, .~. -Sex:Age:Smoking_I)

drop1(fm, test="Chisq") # cannot drop Family:Race:Sex:Age , drop Family:Sex:Smoking_I, pval 0.10118
fm = update(fm, .~. -Family:Sex:Smoking_I)

drop1(fm, test="Chisq") # cannot drop Family:Race:Sex:Age , drop Sex:Smoking_I , pval 0.1994
fm = update(fm, .~. -Sex:Smoking_I )

drop1(fm, test="Chisq") # cannot drop Family:Race:Sex:Age , drop Family:Race:Age:Smoking_I, pval 0.0847
fm = update(fm, .~. -Family:Race:Age:Smoking_I)

drop1(fm, test="Chisq") # cannot drop Family:Race:Sex:Age , drop Family:Age:Smoking_I, pval 0.95417
fm = update(fm, .~. -Family:Age:Smoking_I)

drop1(fm, test="Chisq") # cannot drop Family:Race:Sex:Age , drop Race:Age:Smoking_I , pval 0.2845
fm = update(fm, .~. -Race:Age:Smoking_I)

drop1(fm, test="Chisq") # cannot drop Family:Race:Sex:Age , drop Family:Race:Smoking_I, pval 0.05025
fm = update(fm, .~. -Family:Race:Smoking_I)

drop1(fm, test="Chisq") # cannot drop Family:Race:Sex:Age , drop Race:Smoking_I , pval 0.46187
fm = update(fm, .~. -Race:Smoking_I )

drop1(fm, test="Chisq") # cannot drop Family:Race:Sex:Age , drop Age:Smoking_I , pval 0.05157
fm = update(fm, .~. -Age:Smoking_I)

drop1(fm, test="Chisq") # cannot drop Family:Race:Sex:Age , cant drop any other predictors
summary(fm)

pchisq(fm$deviance, fm$df.residual, lower.tail=F) # pval 0.079 reject H0, model is adequate

# residuals test
library(boot)
fm.diag = glm.diag(fm)
round(ftable(xtabs(fm.diag$rp ~ Family + Race + Sex + Age + Smoking_I, data=data_set2)),2)
# overest smokers from mother family, black, female, age<13
# underest smokers from mother family, black, female, age>13

#  Count ~ Family + Race + Sex + Age + Smoking_I + Family:Race + 
#  Family:Sex + Family:Age + Family:Smoking_I + Race:Sex + Race:Age + Sex:Age + Family:Race:Sex + 
#  Family:Race:Age + Family:Sex:Age + Race:Sex:Age + Family:Race:Sex:Age

tmp = xtabs(Count ~ Family + Race + Sex + Age + Smoking_I, data=data_set2)
tmp
ftable(tmp)

Race_Smoking = apply(tmp, c("Race", "Smoking_I"), sum)
Family_Smoking = apply(tmp, c("Family", "Smoking_I"), sum)
Sex_Smoking = apply(tmp, c("Sex", "Smoking_I"), sum)
Age_Smoking = apply(tmp, c("Age", "Smoking_I"), sum)


library(vcd)

or_race_smoking = oddsratio(Race_Smoking, log=F) # odds of white kids smoking is 0.9894 times the odds of black kids
or_family_smoking = oddsratio(Family_Smoking, log=F) # odds of children from family with mother only to smoke is 1.7467 times the odds of children from family with both parents
or_sex_smoking = oddsratio(Sex_Smoking, log=F) # odds of Male smoking is 0.8322 times the odds of Females?? SURPRISING!!
or_age_smoking = oddsratio(Age_Smoking, log=F) # odds of children >=13 smoking is 1.496 times the odds of childre <12
# sample size dominated by both parents, white

confint(or_race_smoking) # CI incl. 1
confint(or_family_smoking) # CI do not incl. 1 
confint(or_sex_smoking) # CI incl. 1
confint(or_age_smoking) # CI do not incl. 1
# family and age significantly affects smoking patterns at 95% confidence level

# state the logit model equivalent to the selected loglinear model
# prepare the logit data set
dat.logit = cbind(expand.grid(F=levels(data_set2$Family), R=levels(data_set2$Race), S=levels(data_set2$Sex), A=levels(data_set2$Age)),
            SN=data_set2$Count[data_set2$Smoking_I=="none"], SS=data_set2$Count[data_set2$Smoking_I=="some"])

fm.logit = glm(cbind(SN, SS) ~ ., dat.logit, family=binomial)
fm.logit$deviance; fm$deviance

summary(fm.logit) # much simpler model than log linear
summary(fm)

update(fm.logit, .~. -A, dat.logit ) # can't fit to match deviance


# zero cell in the contingency table? only zero in the tmp is the male, <12 yrs, some smoking, mother family, black
fit_check = cbind(c(dat.logit$SN, dat.logit$SS), fitted(fm))
(fit_check[,1] - fit_check[,2])^2

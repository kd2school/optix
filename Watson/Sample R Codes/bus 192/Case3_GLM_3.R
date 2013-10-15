german <- read.csv("~/Dropbox/SJSU ALLT/Kurser SJSU/Data mining SJSU/Shared Bus 192/Case3/german.excel2.csv", header=T)


german <- read.csv("~/Dropbox//Bus 192/Shared Bus 192/Case3/german.excel2.csv", header=T)

german <- read.csv("C:/Dropbox/Bus 192/Shared Bus 192/Case3/german.excel2.csv", header=T)


#http://cran.r-project.org/doc/contrib/Sharma-CreditScoring.pdf
#http://www.r-bloggers.com/modelling-with-r-part-1/
#http://programming-r-pro-bro.blogspot.com/2011/10/modelling-with-r-part-2.html

#View(german)
head(german)
names(german) <- c("chk_acct", "duration", "history", "purpose", "credit_amount", "sav_acct", "employment", "install_rate", "pstatus", "other_debtor", "time_resid", "property", "age", "other_install", "housing", "other_credits", "job", "num_depend", "telephone", "foreign", "response")
#Change the response from 1 and 2 to 0 and 1
german$response = german$response - 1



head(german)




#I think 3, history, which is qualitative - should be factorized - 
german$history<-as.factor(german$history)

#4, qualitative, purpose, should also be qualitative - factorized
german$purpose<-as.factor(german$purpose)


#9, personal status, should be factorized
german$pstatus<-as.factor(german$pstatus)



#Maybe for 10...  No.  Guarantor is better than co-applicant which is better than none.
#Attribute 10: (qualitative)
#Other debtors / guarantors
#A101 : none
#A102 : co-applicant
#A103 : guarantor
#10 is qualitative...
#11 permanent resident since is okay


#12 property = confusing.  I would say factorize it if anything  (1 real estate, 2 just life insurance or savings 3 just a car or not, 4 unknown)
#Best to worst logically inherent

#13 Age in years

#14 Qualitative - factorize - 1 bank, 2 store, 3 none
german$other_install<-as.factor(german$other_install)


#15 factorize = housing: rent or own or for free 
german$housing<-as.factor(german$housing)



#16 - number of existing credits at the bank - more is worse?

#17 - qualitative but higher is better

#18 - more is worse - numeric

#19 - qualitative - telephone - none or yes

#20 - foreign worker - yes or no.


#of V21, 1 is good credit, 2 is bad credit - changed now
german$response<-as.factor(german$response)

#This way you always get the same random sample each time!
str(german)


set.seed(8675)

#Set data lenth to 80% of data set:
#n <- length(german[[1]])*.8
#     german_train <- german[sample(nrow(german), size = n), ]     

subset<- sample(nrow(german), nrow(german)*.8)
german_train <- german[subset,]
german_test <- german[-subset,]

str(german_train)
str(german_test)
head(german_test)

# normalize data
#for (i in 1:(ncol(german_train) - 1)) {
# german_train[, i] = scale(german_train[, i])
#}

#install.packages(fBasics)
#install.packages(ellipse)
#install.librarys(ellipse)



# New libraries:
library(MASS)
library(aplpack)
library(tkrplot)
library(ggplot2)
library(fBasics)


# Correlation
# Data analysis

head(german_train)

#names(german) <- c("chk_acct", "duration", "history", "purpose", "credit_amount", "sav_acct", "employment", "install_rate", "pstatus", "other_debtor", "time_resid", "property", "age", "other_install", "housing", "other_credits", "job", "num_depend", "telephone", "foreign", "response")


credit.glm<-glm(response~ ., family=binomial,data=german_train)
summary(credit.glm)

#REPEAT!!!
credit.glm1<-glm(response~ chk_acct + duration + history + purpose+ credit_amount+ sav_acct+ employment+ install_rate+ pstatus+ other_debtor+ time_resid+ property + age+ other_install+ housing+ other_credits+ job+ num_depend+ telephone + foreign
                   , family=binomial,data=german_train)
summary(credit.glm1)


#http://www.ats.ucla.edu/stat/r/dae/probit.htm
head(german_train)
credit.probita <- glm(response ~ ., family = binomial(link = "probit"), data = german_train)

credit.probitb<-glm(response~ chk_acct + duration + history + purpose+ credit_amount+ sav_acct+ employment+ install_rate+ pstatus+ other_debtor+ time_resid+ property + age+ other_install+ housing+ other_credits+ job+ num_depend+ telephone + foreign
                 , family=binomial(link = "probit"),data=german_train)

#same!
summary(credit.probita)
summary(credit.probitb)


#http://stat.ethz.ch/R-manual/R-patched/library/stats/html/family.html
credit.loglink<-glm(response~ chk_acct + duration + history + purpose+ credit_amount+ sav_acct+ employment+ install_rate+ pstatus+ other_debtor+ time_resid+ property + age+ other_install+ housing+ other_credits+ job+ num_depend+ telephone + foreign
                   , family=binomial(link = "cloglog"),data=german_train)



summary(credit.loglink)

#http://stats.stackexchange.com/questions/20523/difference-between-logit-and-probit-models
#Logit, probit, and cloglog tend to be the same given the same inputs.


#myprobit <- glm(admit ~ gre + gpa + rank, family = binomial(link = "probit"), 
 #               data = mydata)


credit.glm0<-glm(response~ ., family=binomial,data=german_train)
summary(credit.glm0)

#FULL!!
credit.glm.stepAIC <- step(credit.glm0)
#AIC selection
#lowest AIC is 789.89 with the following model:


credit.glm1 <- glm(response ~ chk_acct + duration + history + purpose + credit_amount + 
                     sav_acct + install_rate + pstatus + other_debtor + property + 
                     age + other_install + foreign, family = binomial, data = german_train)

summary(credit.glm1)



credit.glm.stepBIC <- step(credit.glm0, k = log(nrow(german_train)))

credit.glm2 <- glm(response ~ chk_acct + duration + sav_acct + other_debtor + other_install, 
                   family = binomial, data = german_train)
summary(credit.glm2)
#AIC 821.13
#Both should be the full model
AIC(credit.glm)
AIC(credit.glm0)

#det by AIC
AIC(credit.glm1)

#det by BIC
AIC(credit.glm2)

BIC(credit.glm)
BIC(credit.glm0)

BIC(credit.glm1)
BIC(credit.glm2)


#


#The old way of doing things...
#With holding all else the same, the log odds increases by x amount.

#pi <- predict(object = credit.glm1, newdata = german_test)

#mean((pi - german_test$response)^2)



hist(predict(credit.glm1))

hist(predict(credit.glm1, type = "response"))

table(predict(credit.glm1, type = "response") > .5)

table(predict(credit.glm1, type = "response") > .2)

table(predict(credit.glm1, type = "response") > .001)

##############################################################
#insample error rate

prob.glm1.insample <- predict(credit.glm1, type = "response")
predicted.glm1.insample <- prob.glm1.insample > 0.2
predicted.glm1.insample <- as.numeric(predicted.glm1.insample)

table(german_train$response, predicted.glm1.insample, dnn = c("Truth", "Predicted"))
mean(ifelse(german_train$response != predicted.glm1.insample, 2, 1))
mean(ifelse(german_train$response != predicted.glm1.insample, 1, 0))

#####
prob.glm1.insample <- predict(credit.glm1, type = "response")
predicted.glm1.insample <- prob.glm1.insample > 0.4
predicted.glm1.insample <- as.numeric(predicted.glm1.insample)

table(german_train$response, predicted.glm1.insample, dnn = c("Truth", "Predicted"))


#There are many ways to calculate the error rate. The following is one way. The ifelse function returns a vector, the elements are 1 if actual != predicted, 0 otherwise. mean gives you the percentage of 1s in the vector.

mean(ifelse(german_train$response != predicted.glm1.insample, 2, 1))
mean(ifelse(german_train$response != predicted.glm1.insample, 1, 0))


plot(german$response)

plot(factor(german$response))



#AIC Model
summary(credit.glm1)
#BIC Model
summary(credit.glm2)

###################################
#Out of sample
prob.glm1.outsample <- predict(credit.glm1, german_test, type = "response")
predicted.glm1.outsample <- prob.glm1.outsample > .2
predicted.glm1.outsample <- as.numeric(predicted.glm1.outsample)
table(german_test$response, predicted.glm1.outsample, dnn = c("Truth", "Predicted"))

mean(ifelse(german_test$response != predicted.glm1.outsample, 1, 0))

#############################################
prob.glm2.outsample <- predict(credit.glm2, german_test, type = "response")
predicted.glm2.outsample <- prob.glm2.outsample > .2
predicted.glm2.outsample <- as.numeric(predicted.glm2.outsample)
table(german_test$response, predicted.glm2.outsample, dnn = c("Truth", "Predicted"))

mean(ifelse(german_test$response != predicted.glm2.outsample, 1, 0))


##########################################################
#install.packages("verification")

library("verification")

mod1 <- verify(german_test$response == "1", prob.glm1.outsample, bins = FALSE)

mod2 <- verify(german_test$response == "1", prob.glm2.outsample)
roc.plot(mod1, plot.thres = NULL)
lines.roc(mod2, col = "blue", lwd = 2, plot.thres = NULL, lty = 5)


roc.plot(mod2, plot.thres = NULL)
lines.roc(mod1, col = "blue", lwd = 2, plot.thres = NULL)






roc.plot(german_test$response == "1", prob.glm1.outsample)

lines.roc(mod2, col = 2, lwd = 2)


roc.plot(german_test$response == "1", prob.glm2.outsample, add = TRUE)

lines(german_test$response == "1", prob.glm2.outsample, add = TRUE)

#From ?roc.plot
#mod24 <- verify(d$obs_norain, d$p24_norain, bins = FALSE)
#mod48 <- verify(d$obs_norain, d$p48_norain, bins = FALSE)
#roc.plot(mod24, plot.thres = NULL)
#lines.roc(mod48, col = 2, lwd = 2)
#leg.txt <- c("24 hour forecast", "48 hour forecast")
#legend( 0.6, 0.4, leg.txt, col = c(1,2), lwd = 2)


roc.plot(german_test$response == "1", prob.glm1.outsample)$roc.vol


prob.glm0.outsample <- predict(credit.glm0, german_test, type = "response")
roc.plot(x = german_test$response == "1", pred = cbind (prob.glm0.outsample, prob.glm1.outsample),
         legend = TRUE, leg.text = c("Full Model", "X_3, X_8, and X_11_2"))$roc.vol



prob.glm2.outsample <- predict(credit.glm2, german_test, type = "response")
roc.plot(x = german_test$response == "1", pred = cbind (prob.glm2.outsample, prob.glm1.outsample),
         legend = TRUE, leg.text = c("Full Model", "X_3, X_8, and X_11_2"))$roc.vol




#######
#install.packages("ROCR")
library(ROCR)

pred <- prediction(prob.glm1.outsample, german_test$response)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)
############

pcut <- .2
# Symmetric cost
cost1 <- function(r, pi) {
  mean(((r == 0) & (pi > pcut)) | ((r == 1) & (pi < pcut)))
}
# Asymmetric cost
cost2 <- function(r, pi) {
  weight1 = 2
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}


######################
library(boot)
chk_acct + duration + history + purpose+ credit_amount+ sav_acct+ employment+ install_rate+ pstatus+ other_debtor+ time_resid+ property + age+ other_install+ housing+ other_credits+ job+ num_depend+ telephone + foreign


credit.glm3 <- glm(response ~ chk_acct + duration + history + purpose+ credit_amount+ sav_acct+ employment+ install_rate+ pstatus+ other_debtor+ time_resid+ property + age+ other_install+ housing+ other_credits+ job+ num_depend+ telephone + foreign
, family = binomial, german)
cv.result = cv.glm(german, credit.glm3, cost1, 10) 
cv.result$delta



# define the searc grid from 0.01 to 0.99
searchgrid = seq(0.01, 0.99, 0.01)
# result is a 99x2 matrix, the 1st col stores the cut-off p, the 2nd
# column stores the cost
result = cbind(searchgrid, NA)
# in the cost function, both r and pi are vectors, r=truth, pi=predicted
# probability
cost1 <- function(r, pi) {
  weight1 = 10
  weight0 = 1
  c1 = (r == 1) & (pi < pcut)  #logical vector - true if
  actual 1 but predict 0
  c0 = (r == 0) & (pi > pcut)  #logical vecotr - true if
  actual 0 but predict 1
  return(mean(weight1 * c1 + weight0 * c0))
}
credit.glm3 <- glm(response ~ chk_acct + duration + history + purpose+ credit_amount+ sav_acct+ employment+ install_rate+ pstatus+ other_debtor+ time_resid+ property + age+ other_install+ housing+ other_credits+ job+ num_depend+ telephone + foreign
                   , family = binomial, german)
for (i in 1:length(searchgrid)) {
  pcut <- result[i, 1]
  # assign the cost to the 2nd col
  result[i, 2] <- cost1(german_train$response, predict
                        (credit.glm3, type = "response"))
}
plot(result, ylab = "Cost in Training Set")

####################################
save.par <- par(ask = interactive())

# Plot the correlation matrix for the mtcars data full model fit 
data(german_train)
plotcorr(summary(fit, correlation = TRUE)$correlation)

# Plot a second figure with numbers in place of the
# ellipses
plotcorr(summary(fit, correlation = TRUE)$correlation, numbers = TRUE)

# Colour the ellipses to emphasize the differences.  The color range
# is based on RColorBrewer's Reds and Blues (suggested by Gregor Gorjanc)

corr.german <- cor(german)
ordnung <- order(corr.german[1,])
xc <- corr.german[ordnung, ordnung]
colors <- c("#A50F15","#DE2D26","#FB6A4A","#FCAE91","#FEE5D9","white",
            "#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C")   
plotcorr(xc, col=colors[5*xc + 6])

plotcorr(xc, col=colors[5*xc + 6], type = "upper")
plotcorr(xc, col=colors[5*xc + 6], type = "lower", diag = TRUE)
par(save.par)


save.par <- par(ask = interactive())

# Plot the correlation matrix for the mtcars data full model fit 
data(mtcars)
fit <- lm(V25 ~ ., german)
plotcorr(summary(fit, correlation = TRUE)$correlation)

# Plot a second figure with numbers in place of the
# ellipses
plotcorr(summary(fit, correlation = TRUE)$correlation, numbers = TRUE)

# Colour the ellipses to emphasize the differences.  The color range
# is based on RColorBrewer's Reds and Blues (suggested by Gregor Gorjanc)

corr.german <- cor(german)
ord <- order(corr.german[1,])
ord
xc <- corr.german[ord, ord]
colors <- c("#A50F15","#DE2D26","#FB6A4A","#FCAE91","#FEE5D9","white",
            "#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C")   
plotcorr(xc, col=colors[5*xc + 6])

plotcorr(xc, col=colors[5*xc + 6], type = "upper")
plotcorr(xc, col=colors[5*xc + 6], type = "lower", diag = TRUE)
par(save.par)

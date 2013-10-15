german <- read.csv("~/Dropbox/SJSU ALLT/Kurser SJSU/Data mining SJSU/Shared Bus 192/Case3/german.excel2.csv", header=T)


german <- read.csv("C:/Dropbox/Bus 192/Shared Bus 192/Case3/german.excel2.csv", header=T)
german <- read.csv("~/Dropbox//Bus 192/Shared Bus 192/Case3/german.excel2.csv", header=T)

#http://cran.r-project.org/doc/contrib/Sharma-CreditScoring.pdf
#http://www.r-bloggers.com/modelling-with-r-part-1/
#http://programming-r-pro-bro.blogspot.com/2011/10/modelling-with-r-part-2.html

#View(german)

names(german) <- c("chk_acct", "duration", "history", "purpose", "credit_amount", "sav_acct", "employment", "install_rate", "pstatus", "other_debtor", "time_resid", "property", "age", "other_install", "housing", "other_credits", "job", "num_depend", "telephone", "foreign", "response")
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


#of V21, 1 is good credit, 2 is bad credit
german$response<-as.factor(german$response)

#This way you always get the same random sample each time!
str(german)


set.seed(8675)

#Set data lenth to 80% of data set:
n <- length(german[[1]])*.8
     german_train <- german[sample(nrow(german), size = n), ]     


str(german_train)


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


credit.glm0<-glm(response~ ., family=binomial,data=german_train)
summary(credit.glm0)

credit.glm1<-glm(response~ chk_acct + duration + history + purpose+ credit_amount+ sav_acct+ employment+ install_rate+ pstatus+ other_debtor+ time_resid+ property + age+ other_install+ housing+ other_credits+ job+ num_depend+ telephone + foreign
                   , family=binomial,data=german_train)
summary(credit.glm1)


#http://www.ats.ucla.edu/stat/r/dae/probit.htm
head(german_train)
credit.probit <- response ~ ., family = binomial(link = "probit"), data = german_train)

credit.probit<-glm(response~ chk_acct + duration + history + purpose+ credit_amount+ sav_acct+ employment+ install_rate+ pstatus+ other_debtor+ time_resid+ property + age+ other_install+ housing+ other_credits+ job+ num_depend+ telephone + foreign
                 , family=binomial(link = "probit"),data=german_train)

summary(credit.probit)
confint(credit.probit)

#http://stat.ethz.ch/R-manual/R-patched/library/stats/html/family.html
credit.loglink<-glm(response~ chk_acct + duration + history + purpose+ credit_amount+ sav_acct+ employment+ install_rate+ pstatus+ other_debtor+ time_resid+ property + age+ other_install+ housing+ other_credits+ job+ num_depend+ telephone + foreign
                   , family=binomial(link = "cloglog"),data=german_train)

summary(credit.loglink)

#http://stats.stackexchange.com/questions/20523/difference-between-logit-and-probit-models
#Logit, probit, and cloglog tend to be the same given the same inputs.


#myprobit <- glm(admit ~ gre + gpa + rank, family = binomial(link = "probit"), 
 #               data = mydata)



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

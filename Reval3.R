#tgmctrain <- read.csv("C:/Watson/tgmctrain.csv", header=F)

#train <- read.csv("/Watson/tgmctrain.csv", header=F)
eval <- read.csv("/Watson/tgmcevaluation.csv", header=F)

#rm(tgmctrain)
  
train <- read.csv("C:/Users/Marc/Desktop/CMPE239/Watson/tgmctrain.csv", header=F)
eval <- read.csv("C:/Users/Marc/Desktop/CMPE239/Watson/tgmcevaluation.csv", header=F)
#236.53
#eval <- read.csv("/Watson/tgmcevaluation.csv", header=F)


summary(train)
ncol(train)
train<-train[,2:321]

k<-nrow(train)
train.subset <-train[1:(k/100),]
nrow(train.subset)



#321 variables
#239944 objects
ncol(train)
summary(train)

train$V321[1] == "false"

num<-numeric()

ptm <- proc.time()

for(i in 1:nrow(train.subset)){
  if(train.subset$V321[i] == "true")
  {
    num[i]<- 1
  }
  else
    num[i]<- 0
  
}
proc.time() - ptm

ptm <- proc.time()
reg <- lm(factor(V321) ~ ., data = train)
#fullmodel = lm(medv ~ ., data = NFL)
proc.time() - ptm

summary(reg)

sample()

reg2 <- lm()


#Make a numeric for how many there are in the second column for that particular row

end<-numeric()
ptm <- proc.time()

for(i in 1:nrow(train)){
  if(train$V321[i] == "true")
  {
    end[i]<- 1
  }
  else
    end[i]<- 0
  
}
proc.time() - ptm
#   user  system elapsed 
#61.234  72.784 133.981 
head(train$V321)
train$V321<-end

summary(train$V321)
sum(train$V321)
nrow(train$V321)

#write.table(tgmc)

proc.time() - ptm

ptm <- proc.time()
reg <- lm(V321 ~ ., data = train)
#fullmodel = lm(medv ~ ., data = NFL)
proc.time() - ptm

summary(tgmctrain)
#V5
#V6
#(take all zero columns and ignore those variables!!!)
#V33 is zero
#V36 is zero
#V39 is zero
#V41 is zero
#V42, V43 are zero
#V61 is zero
#V64
#V65
#V66
#V67
#V80 is zero

mean(tgmctrain$V314)
max(tgmctrain$V314)
#if a column is all zeros, we kill it from our algorithm.  period.
#training versus 
#What do we do with zero columns?
#What does it mean when a variable is zero?
#Super sparse data

summary(eval)
summary(train)

train[order(train$V321),]
#hopefully sorts data by V321

train[1,]
train[239941,]
train[239944,]

names(x.df)
[1] "V1" "V2" "V3" "V4" "V5" "y"
Subsetting rows using the subset function
The subset function with a logical statement will let you subset the data frame by observations. In the following example the x.sub data frame contains only the observations for which the values of the variable y is greater than 2.

train.sub0 <- subset(train, V321 == 0)
train.sub1 <- subset(train, V321 > 0)

summary(train.sub0)
summary(train.sub1)

install.packages("fBasics")
library(fBasics)
train1.sum <- basicStats(train.sub0) # Common use of library
train2.sum<- basicStats(train.sub1)
train1.sum
train2.sum

summary(train.sub0)
summary(train.sub1)
write.table(train.sub0, file = "train.sub0.csv", sep = ",")
write.table(train.sub1, file = "train.sub1.csv", sep = ",")
 
getwd()

###################
#For Cross Validation
subset<- sample(nrow(train), nrow(train)*.8)
subset_train <- german[subset,]
subset_test <- german[-subset,]
###########################

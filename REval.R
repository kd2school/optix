#tgmctrain <- read.csv("C:/Watson/tgmctrain.csv", header=F)

#tgmctrain <- read.csv("/Watson/tgmctrain.csv", header=F)
#rm(tgmctrain)
train <- read.csv("/Watson/tgmctrain.csv", header=F)

eval <- read.csv("/Watson/tgmcevaluation.csv", header=F)


summary(train)
ncol(train)
tgmctrain<-train[,2:321]

k<-nrow(tgmctrain)
train.subset <-tgmctrain[1:(k/100),]
nrow(subset)



#321 variables
#239944 objects
ncol(tgmctrain)
summary(tgmctrain)

tgmctrain$V321[1] == "false"

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
reg <- lm(factor(V321) ~ ., data = tgmctrain)
#fullmodel = lm(medv ~ ., data = NFL)
proc.time() - ptm

summary(reg)

sample()

reg2 <- lm()


#Make a numeric for how many there are in the second column for that particular row

end<-numeric()
ptm <- proc.time()

for(i in 1:nrow(tgmctrain)){
  if(tgmctrain$V321[i] == "true")
  {
    end[i]<- 1
  }
  else
    end[i]<- 0
  
}
proc.time() - ptm
#   user  system elapsed 
#61.234  72.784 133.981 
head(tgmctrain$V321)
tgmctrain$V321<-end

summary(tgmctrain$V321)
sum(tgmctrain$V321)
nrow(tgmctrain$V321)

write.table(tgmc)

proc.time() - ptm

ptm <- proc.time()
reg <- lm(V321 ~ ., data = tgmctrain)
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

summary
summary(eval)
summary(train)


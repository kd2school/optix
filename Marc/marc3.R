train <- read.csv("/Watson/tgmctrain.csv", header=F)
eval <- read.csv("/Watson/tgmcevaluation.csv", header=F)
trainM <- as.matrix(train);
evalM <- as.matrix(eval);

ptm <- proc.time()
#Import training/evaluation set and create a matrix for both of them.
train <- read.csv("C:/Users/Marc/Desktop/CMPE239/Watson/tgmctrain.csv", header=F);
trainM <- as.matrix(train);
trainStats <- summary(train);
eval <- read.csv("C:/Users/Marc/Desktop/CMPE239/Watson/tgmcevaluation.csv", header=F);
evalM <- as.matrix(eval);
evalStats <- summary(eval);
proc.time() - ptm




#ptm <- proc.time()
#trainM <- as.matrix(train);
#proc.time() - ptm
ptm <- proc.time()
trainM[,321][trainM[,321] == "true"] <- 1;
trainM[,321][trainM[,321] == "false"] <- 0;
#print("Empty columns: ");
deleted <- numeric();
i <- 1;
j <- 1;
ncolumn = ncol(trainM);
while (i <= ncolumn){
  if(max(trainM[,i]) == min(trainM[,i]))
  {
    colDeleted = i + j - 1;
    #print(colDeleted);
    deleted[j] <- colDeleted;
    trainM <- trainM[,-i];
    i <- i - 1;
    ncolumn <- ncolumn - 1;
    j <- j + 1;
  }
  i <- i + 1;
}

#This segment of code may be off - Vivek...
#This code finds empty columns in the Eval set and then stores it to evalZ
#print(deleted);
evalZ <- numeric();
i <- 1;
j <- 1;
ncolumn = ncol(evalM);
while (i <= ncolumn){
  if(max(evalM[,i]) == min(evalM[,i]))
  {
    evalZ[j] <- i;
    j <- j + 1;
  }
  i <- i + 1;
}
proc.time() - ptm


deleted
evalZ
#which(deleted!= evalZ)

length(deleted)
length(evalZ)
length(unique(deleted, evalZ))

#print(deleted);
#print(evalZ);

ptm <- proc.time()
library(Hmisc);
corrTable <- rcorr(trainM, type="pearson");
proc.time() - ptm



#########################


nrow(trainM)
ncol(trainM)
ptm <- proc.time()
first<- glm(trainM[,249] ~ trainM[,1:248], family = binomial)
proc.time() - ptm
summary(first)
#Error in `[[<-.data.frame`(`*tmp*`, i, value = c(2L, 3L, 4L, 5L, 6L, 7L,  : 
#replacement has 59506112 rows, data has 239944
#error - didn't work
#user  system elapsed 
#859.795   7.047 934.419 

#prob.glm1.insample <- predict(credit.glm1, type = "response")
#predicted.glm1.insample <- prob.glm1.insample > 0.2
#predicted.glm1.insample <- as.numeric(predicted.glm1.insample)
#table(german_train$response, predicted.glm1.insample, dnn = c("Truth", "Predicted"))


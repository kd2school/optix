ptm <- proc.time()
#Import training/evaluation set and create a matrix for both of them.

train <- read.csv("C:/Users/Marc/Desktop/CMPE239/Watson/tgmctrain.csv", header=F);
train <- read.csv("C:/WatsonR/tgmctrain.csv", header=F);

str(train)
trainM <- as.matrix(train);
trainStats <- summary(train);
eval <- read.csv("C:/Users/Marc/Desktop/CMPE239/Watson/tgmcevaluation.csv", header=F);
eval <- read.csv("C:/WatsonR/tgmcevaluation.csv", header=F);

evalM <- as.matrix(eval);
evalStats <- summary(eval);
proc.time() - ptm

#ptm <- proc.time()
#trainM <- as.matrix(train);
#proc.time() - ptm
ptm <- proc.time()
trainM[,321][trainM[,321] == "true"] <- 1;
trainM[,321][trainM[,321] == "false"] <- 0;
print("Empty columns: ");
deleted <- numeric();
i <- 1;
j <- 1;
ncolumn = ncol(trainM);
while (i <= ncolumn){
  if(max(trainM[,i]) == min(trainM[,i]))
  {
    colDeleted = i + j - 1;
    print(colDeleted);
    deleted[j] <- colDeleted;
    trainM <- trainM[,-i];
    i <- i - 1;
    ncolumn <- ncolumn - 1;
    j <- j + 1;
  }
  i <- i + 1;
}
print(deleted);
proc.time() - ptm

credit.glm<-glm(response~ ., family=binomial,data=german_train)
summary(credit.glm)




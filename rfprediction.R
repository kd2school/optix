firstRFPrediction <- read.table("/Volumes/Windows/optix/1stRFPrediction.csv", header=T)
secondRFPrediction <- read.table("/Volumes/Windows/optix/2ndRFP.csv", header=T)


RFP <- firstRFPrediction
RFP <- secondRFPrediction

str(RFP)
nrow(RFP)
y$V3 <- RFP$X320
k<-numeric()
j<-1
  for(i in 1:length(y$V3))
{
  if(y$V3[i]>pcut)
  {
    k[j]<-i
    j <- j+1
  }
}

j<-1
num<-nrow(tgmcevaluation)
finals<- numeric()
for(i in 1:num){
  if(i == k[j])
  {
    finals[j] <- tgmcevaluation$V1[i]
    j<- j + 1
  }
}

length(finals)
#write.table(finals, file = "watsonsubmission5.txt", row.names = FALSE, col.names = FALSE)
#Algorithm = Random Forest
#response_variable
#320
#ntree
#50
mtry
-1
mtry_nodes
[15, 15]
out_of_bag_error_estimate
true
no_confusion_matrix
false
refresh_threshold_cm
25
Confusion matrix - OOB error estimate
classification error
1.217 %
used / skipped rows
239944 / 0 (0.0 %)
trees used
50

#length = 207
#score = 97


write.table(finals, file = "watsonsubmission6.txt", row.names = FALSE, col.names = FALSE)
#same thing, 100 trees
#length = 221
#score = 89


read.

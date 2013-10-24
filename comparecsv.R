library(plyr)
wat5 <- read.table("/Volumes/Windows/optix/submission/watsonsubmission5.txt", quote="\"")
str(watsonsubmission5)
#This was random forest

#wat3 <- read.table("/Volumes/Windows/optix/submission/watsonsubmission3.txt", quote="\"")

wat12 <- read.table("/Volumes/Windows/optix/submission/watsonsubmission12.txt", quote="\"")

#http://hosho.ees.hokudai.ac.jp/~kubo/Rdoc/library/plyr/html/join.html
watfull<-join(wat4, wat12, type = "full")
watleft<-join(wat4, wat12, type = "left")
watright<-join(wat4, wat12, type = "right")
watinner<-join(wat4, wat12, type = "inner")

#watfull - a combo of RF and Logistic Reg
write.table(watfull, file = "watsonsubmission13.txt", row.names = FALSE, col.names = FALSE)
nrow(watfull)
#length = 275
#Score = 127

#watleft - a combo of RF and Logistic Reg - sub 5 and sub 12
write.table(watleft, file = "watsonsubmission14.txt", row.names = FALSE, col.names = FALSE)
nrow(watleft)
#length = 210
#Score = 108

#watright - a combo of RF and Logistic Reg - sub 5 and sub 12
write.table(watright, file = "watsonsubmission15.txt", row.names = FALSE, col.names = FALSE)
nrow(watright)
#length = 272
#Score = 128

nrow(watinner)

#watinner - a combo of RF and Logistic Reg - sub 5 and sub 12
write.table(watinner, file = "watsonsubmission16.txt", row.names = FALSE, col.names = FALSE)
nrow(watinner)
#length = 207
#Score = 109


str(wat)
wat
#139 observations
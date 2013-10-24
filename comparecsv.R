wat5 <- read.table("/Volumes/Windows/optix/submission/watsonsubmission5.txt", quote="\"")
str(watsonsubmission5)

wat3 <- read.table("/Volumes/Windows/optix/submission/watsonsubmission3.txt", quote="\"")

wat4 <- read.table("/Volumes/Windows/optix/submission/watsonsubmission4.txt", quote="\"")


nrow(wat5)
nrow(wat4)
wat<- merge(wat5, wat4)
#http://hosho.ees.hokudai.ac.jp/~kubo/Rdoc/library/plyr/html/join.html
join(wat4, wat5, type = "")

str(wat)
wat
#139 observations
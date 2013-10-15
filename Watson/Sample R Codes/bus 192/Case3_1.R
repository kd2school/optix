german <- read.csv("~/Dropbox/SJSU ALLT/Kurser SJSU/Data mining SJSU/Shared Bus 192/Case3/german.excel2.csv", header=T)

german <- read.csv("~/Dropbox//Bus 192/Shared Bus 192/Case3/german.excel2.csv", header=T)

View(german)
     
#This way you always get the same random sample each time!
set.seed(8675)

#Set data lenth to 80% of data set:
n <- length(german[[1]])*.8
     german_train <- german[sample(nrow(german), size = n), ]     


# normalize data
#for (i in 1:(ncol(german_train) - 1)) {
# german_train[, i] = scale(german_train[, i])
#}

# New libraries:
library(MASS)
library(aplpack)
library(tkrplot)
library(ggplot2)
library(fBasics)

# -------------------------------------
# (i): Outliers for all variables
# -------------------------------------
# CSV_table <- write.csv(basicStats(german_train[]), file = "csvsummary.csv") # Common use of library

  names <- names(german)
colorcode <- rainbow(26)
cols <- ncol(german)
cols
layout(matrix(c(1:12),4,3))
       
for(iterations in 1:cols){
  hist(german_train[,iterations], main=names(german[iterations]),breaks = 10,col = colorcode[iterations], xlab = "Normalized value")
}

layout(matrix(c(1:cols),7,3))
for(iterations in 13:25){
  # Density plot
  d<- density(german_train[,iterations])
  plot(d, main=names(german[iterations]),col = colors(iterations), xlab = "Value")
  polygon(d,col = colorcode[iterations])  
}

layout(matrix(c(1:12),4,3))
for(iterations in 13:25){
hist(german_train[,iterations], main=names(german[iterations]),breaks = 2,col = colorcode[iterations], xlab = "Normalized value")
}

library(ellipse)


####Exploratory data analysis
# summary
# Histograms, density plots
# Outlier analysis
# Correlation
# Data analysis


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

#Aneeta Uppal
#Lab 11
#4/12/17

myT <- read.table("/Users/aneetauppal/colorectal_adenomas_family.txt", row.names=1, sep= '\t',header = TRUE)

rowSums <- apply( myT, 1, sum)

avgPerSample <- mean(rowSums)

for( i in 1:nrow(myT))
{
  rowSum = sum(myT[i,])
  myT[i,] = avgPerSample *  myT[i,] / rowSum
}

myT <- round(myT)

caseControl <- ifelse( grepl("case", row.names(myT)), "case", "control")
bug <- vector()
x<- vector()

index = 1
for(i in 1:ncol(myT)){
  if(sum(myT[,i]==0) > 1){
    bug <- log(myT[,i] + 1)
    myLm <- lm(bug ~ caseControl)
    x[index] <-anova(myLm)$"Pr(>F)"[1]
    index = index + 1
  }
}

hist(x)
install.packages("lmtest")
library("lmtest")
install.packages("pscl")
library("pscl")

#2B & 2C
y <- vector()
index <- 1 
someCol <- 1
pdf("results.pdf")

for(i in 1:ncol(myT)){
  
  if(sum(myT[,i]==0) > 1){
    bug <- (myT[,i])
    myFrame<-data.frame(bug, caseControl)
    zeroin <- zeroinfl(bug ~ caseControl |caseControl, data= myFrame, dist="negbin", link ="logit")
    y[index] <-lrtest(zeroin)$"Pr(>Chisq)"[2]
    caseControl <- ifelse( grepl("case", row.names(myT)), "case", "control")
    boxplot( bug ~ caseControl,main=paste(names(myT)[index],"p-values", signif(y[index], digits = 3)))
    myFrame <- data.frame(bug, caseControl)
    stripchart(bug~ caseControl, data = myFrame,vertical = TRUE, pch = 21, add=TRUE )
    index = index + 1
  }
}

dev.off()
hist(y)
plot(log(y),log(x))


adjusted_p1 <- sum(p.adjust(y, method="BH") <.1)
adjusted_p2 <- sum(p.adjust(x, method="BH") <.1)



AIC(myLm)
AIC(zeroin)


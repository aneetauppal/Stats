#Aneeta Uppal
#Lab5
#2/15/17

myT <- read.table("/Users/aneetauppal/Graduate_PhD/Advanced_Stats/longitdunalRNASeqData/nc101_scaff_dataCounts.txt",sep="\t",header=TRUE,row.names=1)

numCols <- ncol(myT)

myColClasses <- c("character", rep("numeric", numCols))
myTAsNum <-read.table("/Users/aneetauppal/Graduate_PhD/Advanced_Stats/longitdunalRNASeqData/nc101_scaff_dataCounts.txt",sep="\t",header=TRUE,colClasses=myColClasses)

myTNorm <- myTAsNum

for ( i in 2:ncol(myTAsNum))
{
  colSum = sum(myTNorm[,i])
  myTNorm[,i] = myTNorm[,i]/colSum
}

pValue1 <- vector()
pValue2 <- vector()
pValue3 <- vector()
allpvalues <- vector()
for(i in 1:nrow(myTNorm) )
  {
p.values1 <- t.test( myTNorm[ i, 2:4], myTNorm[i, 5:7] )$p.value
p.values2 <- t.test( myTNorm[ i, 5:7], myTNorm[i, 8:12] )$p.value
p.values3 <- t.test( myTNorm[ i, 2:4], myTNorm[i, 8:12] )$p.value

allp.values <- t.test(myTNorm[i,2:12])$p.value

pValue1[i] <- p.values1
pValue2[i] <- p.values2
pValue3[i] <- p.values3
allpvalues[i] <- allp.values
}

hist(pValue1, col = "red", ylim = c(0,2000))
hist(pValue2, col = "blue", add = TRUE)
hist(pValue3, col = "green", add = TRUE)


pVal1adj <- p.adjust( pValue1[!is.nan(pValue1)], method="BH", n=length(pValue1))
pVal2adj <- p.adjust(pValue2[!is.nan(pValue2)], method="BH", n=length(pValue2))
pVal3adj <- p.adjust( pValue3[!is.nan(pValue3)], method="BH", n=length(pValue3))

sum(pVal1adj < 0.1)
#0 signficiant genes
sum(pVal2adj < 0.1)
#37 signficiant genes
sum(pVal3adj < 0.1)
#625 signficant genes


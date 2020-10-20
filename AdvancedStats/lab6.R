#Aneeta Uppal
#Lab 6
#2/28/1993

source("http://bioconductor.org/biocLite.R")
biocLite("DESeq")
library("DESeq")

#2
numRows = 3000
numCols = 10
for( i in 1:numCols)
  
  myFrame <- data.frame(1:numRows)

names(myFrame)[1] <- "tempColumn"

for( i in 1: numCols)
{
  vals <- vector(length=numRows)
  
  for( j in 1:numRows)
  {
    aMean = j /10
    aMean = max( aMean,5)
    aVar = aMean+ 5* aMean 
    aVal = round( max( rnorm(1,mean=aMean,sd=sqrt(aVar)), 1))
    vals[j] = aVal
  }
  
  colName <- paste( "sample" , i ,sep="")
  
  myFrame[[colName]] = vals
}

myFrame["tempColumn"] <- NULL
row.names(myFrame) <- paste("Gene_",1:numRows,sep="")


#3
myconditions <- c(rep("a",5),rep("b", 5))
cds <- newCountDataSet(myFrame,myconditions)
cds <- estimateSizeFactors(cds)
cds <- estimateDispersions(cds)
bintest <- nbinomTest(cds,"a","b")

means <- apply(counts(cds,normalized=TRUE), 1,mean)
myInfo <- fitInfo(cds)

plot(means, means * means* myInfo$perGeneDispEsts, main = "DeSeq's estimated mean and variance for each gene")
lines(means, means*means* myInfo$dispFunc(means),col="red")
points(means, means * means* fData(cds)[,1], col="yellow")

 
vals <- vector()
for(i in 1:length(myFrame[,1]))
{
  a <- myFrame[i,1:5]
  b <- myFrame[i,6:10]
  pval <- t.test(a,b,alternative = "two.sided")$p.value
  vals[i] <- pval
}

hist(bintest$pval)


#Genes are significantly different at a 10% False Discovery Rate?
sum(bintest$pval<0.1)


#4
newcds <- newCountDataSet(myFrame,myconditions)
newcds <- estimateSizeFactors(newcds)
newcds <- estimateDispersions(newcds, sharingMode="gene-est-only")
nbintest <- nbinomTest(newcds,"a","b")

meansNew <- apply(counts(newcds,normalized=TRUE), 1,mean)
newInfo <- fitInfo( newcds )

plot(meansNew, meansNew * meansNew* newInfo$perGeneDispEsts)
lines(meansNew, meansNew*meansNew* newInfo$dispFunc(means),col="red")
points(meansNew, meansNew * meansNew* fData(newcds)[,1], col="YELLOW")



#Do the p-values become more or less closer to uniform using this option?  
hist(nbintest$pval)

#How many genes are significantly different at a 10% False Discovery Rate.  
sum(nbintest$pval < 0.1)


#5 

test <- vector()
for(i in 1:length(myFrame))
{
  x <- t.test( myFrame[i,1:5], myFrame[i,6:10] )$p.value  
  test[i] <- x
}

hist(test)

hist(nbintest$pval)
par(new=TRUE)
hist(test, col="red")

hist(bintest$pval)
par(new=TRUE)
hist(test, col="blue")


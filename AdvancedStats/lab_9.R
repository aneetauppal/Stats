#Aneeta Uppal
#3/29/17
#Lab_09
rm(list=ls())
library("nlme")

myTable <- read.table("/Users/aneetauppal/Graduate_PhD/Advanced_Stats/prePostPhylum.txt", sep= '\t',header = TRUE)
myT <- myTable[myTable$time %in% c("POST"),]

#Plot cage versus phyla
par(mfrow=c(3,2)) 
plot(myT$cage, myT$Tenericutes, 
     main="Rel. Abundance of Tenericutes Across Cages",
     xlab="Cages",
     ylab="Tenericutes Abundance")

plot(myT$cage, myT$Verrucomicrobia, 
     main="Rel. Abundance of Verrucomicrobia Across Cages",
     xlab="Cages",
     ylab="Verrucomicrobia Abundance",
     ylim=c(0,5.1))

plot(myT$cage, myT$Bacteroidetes, 
     main="Rel. Abundance of Bacteroidetes Across Cages",
     xlab="Cages",
     ylab="Bacteroidetes Abundance",
     ylim=c(4,5.5))

plot(myT$cage, myT$Actinobacteria, 
     main="Rel. Abundance of Actinobacteria Across Cages",
     xlab="Cages",
     ylab="Actinobacteria Abundance",
     ylim=c(2,3.33))

plot(myT$cage, myT$Firmicutes, 
     main="Rel. Abundance of Firmicutes Across Cages",
     xlab="Cages",
     ylab="Firmicutes Abundance",
     ylim=c(5.2,5.7))

plot(myT$cage, myT$Proteobacteria, 
     main="Rel. Abundance of Proteobacteria Across Cages",
     xlab="Cages",
     ylab="Proteobacteria Abundance",
     ylim=c(0.5,3.5))


par(mfrow=c(3,2)) 
j=1
rhoValue <- vector()
pValues <- vector()
for(i in 5:10)
{
  bugs <- myT[,i]
  cages <- myT$cage
  genotypes <- myT$genotype
  myF <- data.frame(bugs,cages,genotypes)
  
  plot(myF$bugs ~ myF$cages)
  stripchart(bugs ~ cages, data = myF,vertical = TRUE, pch = 21, add=TRUE)
  
  M.gls <- gls(bugs ~ genotypes, method = "REML", correlation = corCompSymm( form = ~ 1 | cages),data=myF)
  myLm <- gls(bugs ~ genotypes, method = "REML",data=myF)
  nullLogLike = unclass(logLik(myLm))[1] 
  alterLogLike = unclass(logLik(M.gls))[1]
  values <- -2 * nullLogLike + 2 * alterLogLike
  answer <- 1-pchisq(values,1)
  pValues[j] <- answer
  
  #test for false discovery rate:
  if (sum(answer < 0.1))
    print ("False")
  else
    print ("True")
  
  #intraclass corr coef: 
  rhoV <- coef(M.gls$modelStruct[1]$corStruct,unconstrained=FALSE)[[1]]
  rhoValue[j] <- rhoV
  i = i + 1
  j = j + 1
}
           


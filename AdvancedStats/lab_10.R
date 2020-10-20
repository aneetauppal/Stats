#Aneeta Uppal
#4/5/17
#lab 10

myTable <- read.table("/Users/aneetauppal/Graduate_PhD/Advanced_Stats/prePostPhylum.txt", sep= '\t',header = TRUE)

myPCA<-princomp(myTable[5:10])

var <- myPCA$sdev^2
#PC1 & PC2 7.741278078 1.075276818

percentVar <- sum(var[1:2])/sum(var)
#[1] 0.8599567

PC1 <- myPCA$scores[,1]
PC2 <- myPCA$scores[,2]

plot(PC1, PC2, main = "PC1 & PC2", col = c("red", "blue"))
legend("bottomright", c("PC1", "PC2"), col =c("red", "blue"), pch = 16)


colors <- vector()
for(i in 1:length(myTable$time))
{
  if(myTable$time[i] == "POST")
    colors[i] = "PURPLE"
  if(myTable$time[i] == "PRE")
    colors[i] = "PINK"
}

plot(PC1, PC2, col=colors, main="Pre vs. Post Data", pch=16)
legend("bottomright", c("Pre","Post"), col=c("purple", "pink"), pch=16)


colors1 <- vector()
for(i in 1:length(myTable$genotype))
{
  if(myTable$genotype[i] == "WT")
    colors1[i] = "DEEPPINK"
  if(myTable$genotype[i] == "10-/-")
    colors1[i] = "TURQUOISE3"
}

plot(PC1, PC2, col=colors1, main="WT vs. IL10-/-", pch=16)
legend("bottomright", c("WT","IL10-/-"), col=c("DEEPPINK", "TURQUOISE"), pch=16)

myLm1 <- lm(PC1 ~ myTable$genotype + myTable$cage)
myLm2 <- lm(PC2 ~ myTable$genotype + myTable$cage)
anova(myLm1)
anova(myLm2)

library("nlme")

cage <- myTable$cage
genotype <- myTable$genotype
time <- myTable$time

#Time and point w/ cage PC1
M.glsPC1 <- gls(PC1 ~ genotype + time, method = "REML", correlation = corCompSymm( form = ~ 1 | cage),data=myTable)
#PC1 with no cage
M.glsPC1_wno_cage <- gls(PC1 ~ genotype + time, method = "REML",data=myTable)
x<- anova(M.glsPC1_wno_cage, M.glsPC1)
summary(x)
#Time and point w/ cage
M.glsPC2 <- gls(PC2 ~ genotype + time, method = "REML", correlation = corCompSymm( form = ~ 1 | cage),data=myTable)
#PC2 with no cage 
M.glsPC2_wno_cage<-gls(PC2 ~ genotype + time, method = "REML",data=myTable)
y <- anova(M.glsPC2_wno_cage, M.glsPC2)
summary(y)


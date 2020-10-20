#Aneeta Uppal
#Lab 4

file <-read.table("/Users/aneetauppal/Downloads/data_rnaseq.txt",header=TRUE,row.names=1)

plot(log10(file$D2_01), (log10(file$D2_02)))

plot(log10(file$D2_01), col = "blue")
points(log10(file$D2_02))

#3
sam1 <- file [1,1]
sam2 <- file[1,2]

col1 <- file[,1]
col2 <- file[,2]

total1 <- sum(col1) - sam1
total2 <- sum(col2) - sam2    

row1 <- (c(sam1, total1))
row2 <-(c(sam2, total2))

table<- data.frame(row1, row2)
colnames(table) <- c("sequences in D2_01", "sequences in D2_02")
rownames(table) <- c("Assigned to NC101_00003", "Not assigned to NC101_00003")
table

pValueA<- fisher.test(table)$p.value




#4
pVal1 <- vector()
for(i in 1:length(col1))
{
  matrix1 <- matrix(c(col1[i], col2[i], 4630, 4630), nrow =2, byrow = TRUE)  
  pValues1 <- fisher.test(matrix1, alternative = "two.sided")$p.value
  pVal1[i] <- pValues1
}

hist(pVal1, main= "Histogram of Pvalues", col = "blue")

#remove low abudance
newmatrix1 <- myT[(myT$D2_01 + myT$D2_02 >50),]
newCol1 <- newmatrix1[,1]
newCol2 <- newmatrix1[,2]
pVal2 <- vector()

for(i in 1:length(newCol1))
{
  matrix2 <- matrix(c(newCol1[i], newCol2[i], 4630, 4630), nrow =2, byrow = TRUE)  
  pValues2 <- fisher.test(matrix2, alternative = "two.sided")$p.value
  pVal2[i] <- pValues2
}
hist(pVal2, main = "Pvalues after removing low abun. genes", col = "red")

#5
pseudocount <- myT + 1
expfreq <- pseudocount[1,1]/length(pseudocount[,1])
pvalue <- poisson.test(pseudocount[1,2], length(psuedocount[,1]), r =expfreq)$p.value


#6 repeat for every gene

psuedopvalues <- vector()
for(i in 1:length(pseudocount[,1]))
{
  expfre1 <- psuedocount[i,1]/length(psuedocount[,1])
  pvalues3 <- poisson.test(psuedocount[i,2], length(psuedocount[,1]), r=expfre1)$p.value
  psuedopvalues[i] <- pvalues3 
}
hist(psuedopvalues, main="Poisson-Psuedo Pvalues", col = "pink")


#Aneeta Uppal
#Lab1
list1 <- c(2.3,  4.3 ,1.2 , 3.4, 8.3, 12.2)
sd(list1)
mean(list1)

lists = (seq(4, 680, by =1))
lists
source("http://bioconductor.org/biocLite.R") #this only needs to be done once
biocLite("ShortRead") #this only has to be done once
library("ShortRead") 
file1 <- ("/Users/aneetauppal/Downloads/Hamp_Fodor/hamp_fodor_090810.fna")
reads <- readFasta(file1)
myreads <- sread(reads)
myreads
hist(myreads@ranges@width, breaks=50)
mean(myreads@ranges@width)
sd(myreads@ranges@width)
hist(letterFrequency(myreads, letters = "GC"))


#Lab2 Loadeddie
loaded.die <- sample(1:6, 10000, replace = TRUE, prob=c(.1, .1, .1, .1, .1, .5))
hist(loaded.die)
mean(loaded.die)
sd(loaded.die)

diceroll <- function(x){
  loaded.die <- sample(1:6, x, replace = TRUE, prob=c(.1, .1, .1, .1, .1, .5))
  means <- mean(loaded.die)
  results = list(loaded.die = loaded.die, means = means)
  return (results)
}

diceroll(10000)
hist(diceroll(10000))

#Aneeta Uppal 
#Lab2 Loadeddie

#1
meandie <- (.1*1 + .1*2 + .1*3 + .1*4 + .1*5 +.5*6)/6
meandie
variancedie <- (.1*(1-meandie)^2) + (.1*(2-meandie)^2) + (.1*(3-meandie)^2) + (.1*(4-meandie)^2) + (.1*(5-meandie)^2) + (.5*(6-meandie)^2)
variancedie  

#2
diceroll <- function(x){
  loadeddie <- sample(1:6, x, replace = TRUE, prob=c(.1, .1, .1, .1, .1, .5))
  means <- mean(loadeddie)
  variance <-var(loadeddie)
  results = list(loadeddie = loadeddie, means = means, variance = variance)
  return (results)
}

#3
dice <- diceroll(10000)
hist(dice$loadeddie)
dice
#No the histagram does not follow uniform distribution since it is loaded. 

#4
##Note - I did not use your numbers exactly for trial sizes - kinda made up some of my own *** 
trialSizes<- c(5,10,15,20,30,40,50,100,200,300,500,900, 1000, 5000, 10000, 20000, 30000, 100000)
means1 <- vector(mode="double", length = length(trialSizes))
variances1 <- vector(mode="double", length=length(trialSizes))



for( i in 1:length(trialSizes) )
{
  rolls <- vector(length=trialSizes[i], mode = "double")
  for(j in 1:trialSizes[i])
  {
    rolls[j] <- sample(1:6, i, replace = TRUE, prob=c(.1, .1, .1, .1, .1, .5))
  }
  means1[i]<-mean(rolls)
  variances1[i]<- var(rolls)
  
}

plot(log10(trialSizes), means1)
lines(log10(trialSizes), rep(3.5, length(trialSizes)))

plot(log10(trialSizes), variances1)
lines(log10(trialSizes), rep(2.916, length(trialSizes)))


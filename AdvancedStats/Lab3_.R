#Aneeta Uppal
#Lab 3
#2/1/17


#A uniform prior ( for example dbeta(1,1)).
#A prior of 5 heads and tails ( dbeta(6,6)).
#(1A) superimpose these two priors (using different colors for each prior).
x <- seq(0,1, 0.01)
a <- dbeta(x, 6, 6)
b <- dbeta(x, 1, 1)

plot(a, col = 'blue')
  points(b, col = 'pink')

#0ne with 1 heads and 1 tail  
y <- seq(0,1, 0.01)
d <- dbeta(y, 2, 2)
e <- dbeta(y, 7, 7)

plot(y, d, col = 'purple', ylim = c(0, 3))
  points(y, e, col = 'pink')
  
#One with 400 heads and 400 tails 
f <- dbeta(y, 401, 401)
g <- dbeta(y, 406, 406)

plot(y, f, col = 'red', ylim = c(0, 30))  
points(y, g, col = 'green')

#Why are the two poesterior plots involving the 800 coin flips so similar?
#Because there is so much data the prior probability is being washed out 
#by such a large sample size. 
  

#Why are the two posterior plots  involving the two coin flips so different?
#The smaller sample size will stay closer to the prior probability giving the 
#expected probability of what you think will happen when the experiment is 
#carried out. The beta distribution will cause the graph to follow the 
#Bernoulli distrbution as the parameters entered get closer to 0.
 
#2A 
z <- seq(0,1, 0.01) 
plot((dexp(z, rate =5) / 0.9932621), col = 'pink')

#2B
numIterations <- 100000
posteiorDist <- vector()
piOld <- (.5)

for( i in 1:numIterations )
{
  pOld <- (dexp(piOld, rate =5) / 0.9932621) * dbinom( 14, 24, piOld)
  
  piNew <- piOld + rnorm(1, 0, sd =0.01);
  
  if( piNew > 1) 
    piNew = 1;
  
  if( piNew < 0 ) 
    piNew =0;
  
  pNew <- (dexp(piNew, rate =5) / 0.9932621) * dbinom( 14, 24, piNew )
  
  ratio <- pNew / pOld
  
  if( ratio > 1 || ratio >= runif(1) ) 
    piOld = piNew;
  
  posteiorDist[i] = piOld;	
  if( i %% 100 == 0 )
  {	
    myHist <- hist(posteiorDist,breaks=200,plot=FALSE)
    plot( myHist$mids, myHist$counts/i, main = paste("iteration", i), ylim = c(0, 0.05)) 
    dbetasum = sum(dbeta(myHist$mids, 14+40, 10+40))
    points( myHist$mids, dbeta(myHist$mids, 14+40, 10+40)/dbetasum,col="red")
    Sys.sleep(.1)
  }
}


#grid approach to beta prior using a naive (brute force) numerical 
#integration algorithm using prior beta(40,40)

numBreaks=50000;
posteriorDistri <- vector()
xVals <- seq(0,1,1/numBreaks);

i <- 1;
sum <- 0;
for( x in xVals )
{
  posteriorDistri[i] <- (dexp(x, rate =5) / 0.9932621) * dbinom( 14, 24, x)
  sum = sum + posteriorDistri[i];
  i <- i + 1;	
}
par(new=TRUE)
plot(posteriorDistri/sum, axes = FALSE, col = "green")

#2C
numIterations <- 50000
posteiorDist <- vector()
piOld <- (.5)

for( i in 1:numIterations )
{
  pOld <- (dexp(piOld, rate =5) / 0.9932621) * dbinom( 583, 1000, piOld)
  
  piNew <- piOld + rnorm(1, 0, sd =0.01);
  
  if( piNew > 1) 
    piNew = 1;
  
  if( piNew < 0 ) 
    piNew =0;
  
  pNew <- (dexp(piNew, rate =5) / 0.9932621) * dbinom( 583, 1000, piNew )
  
  ratio <- pNew / pOld
  
  if( ratio > 1 || ratio >= runif(1) ) 
    piOld = piNew;
  
  posteiorDist[i] = piOld;	
  if( i %% 100 == 0 )
  {	
    myHist <- hist(posteiorDist,breaks=200,plot=FALSE)
    plot( myHist$mids, myHist$counts/i, main = paste("iteration", i), ylim = c(0, 0.05)) 
    dbetasum = sum(dbeta(myHist$mids, 583+40, 417+40))
    lines( myHist$mids, dbeta(myHist$mids, 583+40, 417+40)/dbetasum,col="red") 	
    Sys.sleep(.1)
  }
}

#Grid Approx for 2C
numBreaks=50000;
posteriorDistri <- vector()
xVals <- seq(0,1,1/numBreaks);

i <- 1;
sum <- 0;
for( x in xVals )
{
  posteriorDistri[i] <- (dexp(x, rate =5) / 0.9932621) * dbinom( 583,1000, x)
  sum = sum + posteriorDistri[i];
  i <- i + 1;	
}
par(new=TRUE)
plot(posteriorDistri/sum, axes=FALSE, col = "green")

#plot(dbinom(583,1000, .5))
#plot(dbinom(14,24, .5), col = "red")

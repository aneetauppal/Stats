# 2. visualize the raw data
plot(x,y, 
     asp=1, 
     pch=16, cex=1,
     col="blue",
     main="raw data and PC axis",
     xlab="x", ylab="y") # plot raw data


setwd('/Users/aneetauppal/Downloads/')
in_file_name <- "dataset_2.csv"
dataIn <- read.csv(file=in_file_name,
                   header=T)

dataIn <-read.table(file="/Users/aneetauppal/Downloads/dataset_2.csv", header=T, sep=",")
x <- dataIn[1]
y<- dataIn[2]
z <- dataIn[3]


# 3. do PCA
s <- cbind(x, y)

re <- princomp(dataIn, cor=T)
str(re)
var(dataIn)
var(x)
var(y)
var(z)
totalvar = var(x) + var(y) + var(z)
totalvar2 = re$sdev[1]^2 + re$sdev[2]^2  + re$sdev[3]^2 
totalvar/3
totalvar2/3
#variance of PC1 
re$sdev[2]^2/sum(re$sdev^2)
#variance of PC2
re$sdev[2]^2 
#variance of PC3
re$sdev[3]^2 
re

scaleddata <- scale(dataIn)
scaleddata
var(dataIn)
scaledx <- scale(x)
scaley <- scale(y)
scalez <- scale(z)
cor(x,z)
# 4. PCA looks at the original data in a different coordinate system
# the PC1-PC2 coordinate system
points(c(0, re$loadings[1,1]), c(0, re$loadings[2,1]), 
       type="l", col="green", lwd=4) # add PC1 axis 
points(c(0, re$loadings[1,2]), c(0, re$loadings[2,2]), 
       type="l", col="green", lwd=4) # add PC2 axis






# 5. percentage of variance preserved after discarding PC2
re$sdev[1]^2 / sum(re$sdev^2)
re$sdev[2]^2 / sum(re$sdev^2)
re$sdev[3]^2 / sum(re$sdev^2)





# 6. PCA on completely random data
y <- runif(n=N)


plot(dataIn, 
     asp=1, 
     pch=16, cex=1,
     col="blue",
     main="raw data and PC axis", 
     xlab="x", ylab="y") # plot raw data


s <- cbind(x, y)
re <- princomp(s, cor=T)


points(c(0, re$loadings[1,1]), c(0, re$loadings[2,1]), 
       type="l", col="green", lwd=4) # add PC1 axis 
points(c(0, re$loadings[1,2]), c(0, re$loadings[2,2]), 
       type="l", col="green", lwd=4) # add PC2 axis



re$sdev[1]^2 / sum(re$sdev^2)



####################################################

# 2. PCA by R
re <- princomp(dataIn, cor=T)





# 3. visualize the raw data and the principal components
plot(x, z, asp=1, 
     pch=16, cex=1, col="blue",
     main="raw data and PC axis", 
     xlab="x", ylab="y") # plot raw data


# the PC1-PC2 coordinate system
points(c(0, re$loadings[1,1]), c(0, re$loadings[2,1]), 
       type="l", col="green", lwd=4) # add PC1 axis 
points(c(0, re$loadings[1,2]), c(0, re$loadings[2,2]), 
       type="l", col="green", lwd=4) # add PC2 axis

# The PC1-PC2 axis form a new orthogonal coordinate system, a rotated version of the original x-y coordinate system
sum(re$loadings[,1] * re$loadings[,2]) # show PC1 is orthogonal to PC2


# why do the PC1-PC2 axis are not aligned with the data?

# compute the z-score of x and y
x_zscore <- (x - mean(x))/sd(x)
y_zscore <- (y - mean(y))/sd(y)

plot(x_zscore, y_zscore, asp=1, 
     pch=16, cex=1, col="blue",
     main="z_score transformed data and PC axis", 
     xlab="x", ylab="y") # plot raw data

# the PC1-PC2 coordinate system
points(c(0, re$loadings[1,1]), c(0, re$loadings[2,1]), 
       type="l", col="green", lwd=4) # add PC1 axis 
points(c(0, re$loadings[1,2]), c(0, re$loadings[2,2]), 
       type="l", col="green", lwd=4) # add PC2 axis





# 4. scores
plot(re$scores[,1], re$scores[,2], asp=1, 
     pch=16, cex=1, col="blue",
     main="scores", xlab="PC1", ylab="PC2") # scores plot


# scores are linear combinations of the z_score transformed data, weights are the loadings
my_scores <- cbind(x_zscore, y_zscore) %*% re$loadings

# scores are uncorrelated
cor(x_zscore, y_zscore) # correlation between the original x and y
cor(re$scores[,1], re$scores[,2]) # correlation between PC1 and PC2

#dotproduct
x&y
re$scores[,1] %*% re$scores[,2]
x&z
re$scores[,1] %*% re$scores[,3]
y&z
re$scores[,2] %*% re$scores[,3]

# 5. variance
# total variance in the data remain the same before and after PCA
total_variance_in_zscore_transformed_data <- var(x_zscore) + var(y_zscore)
total_variance_in_scores <- var(re$scores[,1]) + var(re$scores[,2])

# scree plot -- variance vs PC number
plot(re$sdev^2, 
     pch=16, cex=1, col="blue",
     main="scree plot", 
     xlab="PC number", ylab="eigenvalues")





# 6. loadings
plot(re$loadings[1,1], re$loadings[1,2], 
     pch=16, cex=1, col="blue",
     xlim=c(min(re$loadings[,1]), max(re$loadings[,1])), 
     ylim=c(min(re$loadings[,2]), max(re$loadings[,2]))) # loadings plot
points(re$loadings[2,1], re$loadings[2,2], 
       pch=16, cex=1, col="red")


# the loadings matrix is an orthogonal matrix
all(round(t(re$loadings), digits=2) == round(solve(re$loadings), digits=2))

temp <- re$scores %*% t(re$loadings)
x_reconstructed <- temp[,1] * sd(x) + mean(x)
y_reconstructed <- temp[,2] * sd(y) + mean(y)
plot(x, y,
     pch=16, cex=1)

points(x_reconstructed, y_reconstructed, 
       pch=16, cex=1,
       col="red")
re 
x <- dataIn[1]
y <- dataIn[2]
cor(x,y)
p_reduced <- re$loadings[,1]
s_reduced <- re$scores[,1] %*% t(p_reduced)

x_reconstructed_reduced <- s_reduced[,2] * sd(x) + mean(x)
y_reconstructed_reduced <- s_reduced[,3] * sd(y) + mean(y)
points(x_reconstructed_reduced, y_reconstructed_reduced,
       pch=16, cex=1,
       col="blue")

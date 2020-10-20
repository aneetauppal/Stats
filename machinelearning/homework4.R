#AneetaUppal
#homework4
rm(list=ls())
graphics.off()





MLTB_dir <- "/Users/aneetauppal/Graduate_MS/Fall_Semester_2016/Machine_learning/MLTB_export"

# 5.1 apply LDA function in MASS to the iris data
packageName <- "MASS"
install.packages(packageName)
library(packageName, character.only = T)



re.LDA.MASS <- lda(x=iris[,1:4], grouping=iris$Species, tol=1.0e-4, method="mle", CV=F)
plot(re.LDA.MASS)
re.LDA.MASS$means
re.LDA.MASS$scaling # the transformation matrix
re.LDA.MASS


# get the projections
Y <- t(re.LDA.MASS$scaling) %*% t(iris[, 1:4])
Y <- t(Y)
Y
for(i in nrow(Y))


# plot the projections
II_setosa <- which(iris$Species=="setosa")
II_versicolor <- which(iris$Species=="versicolor")
II_virginica <- which(iris$Species=="virginica")

plot(Y[II_setosa,1], Y[II_setosa,2],
     pch=16, cex=1,
     xlim=c(min(Y[,1]), max(Y[, 1])),
     ylim=c(min(Y[,2]), max(Y[,2])),
     col="red",
     xlab="LD1", ylab="LD2",
     main="LDA: iris projections onto the first two linear discrimants")

points(Y[II_versicolor, 1], Y[II_versicolor, 2],
       pch=16, cex=1,
       col="green")

points(Y[II_virginica, 1], Y[II_virginica, 2],
       pch=16, cex=1,
       col="blue")

legend("topright", legend=c("setosa", "versicolor", "virginica"), 
       pch=c(16, 16, 16), 
       col=c("red", "green", "blue"))

#results are in Y matrix

z <- cbind(Y, iris[,5])
for(i in 1:nrow(z)){
  if(z[i,3] == 1){
    z[i,3] <- "setosa"
  }
  else if(z[i,3] == 2){
    z[i,3] <- "versicolor"
  }
  else{
    z[i,3] <- "virginica"
  }
  i = i + 1
}
z <- data.frame(z)


# 2. cross-validation
# 2.1 leave-one-out CV
re.knn <- vector(mode="list", length=nrow(z))
i =1
for (i in 1:nrow(z)){
  
  if (i==1) {
    iris.test <- z[1, 1:2]
    iris.test.labels <- z[1, 3]
    
    iris.training <- z[2:nrow(z), 1:2]
    iris.training.labels <- z[2:nrow(z), 3]
    
  } else if (i==nrow(z)) {
    iris.test <- z[i, 1:2]
    iris.test.labels <- z[i, 3]
    
    iris.training <- z[1:(nrow(z)-1), 1:2]
    iris.training.labels <- z[1:(nrow(z)-1), 3]
  } else {
    iris.test <- z[i, 1:2]
    iris.test.labels <- z[i, 3]
    
    iris.training <- rbind(z[1:(i-1), 1:2],
                           z[(i+1):nrow(z), 1:2])
    
    iris.training.labels <- factor( c(levels(z[1:(i-1), 3])[z[1:(i-1), 3]] ,
                                      levels(z[(i+1):nrow(z), 3])[z[(i+1):nrow(z), 3]]) )


}

  
  re.knn[[i]]$knn <- knn(train=iris.training,
                         test=iris.test,
                         cl=iris.training.labels,
                         k=3)
  
  compare_results <- identical(iris.test.labels, re.knn[[i]]$knn )
  
  
  error_rate <- length(which(compare_results==F)) / length(compare_results)
  re.knn[[i]]$number_of_false_classification <- length(which(compare_results==F))
  re.knn[[i]]$error_rate <- error_rate
  i = i+1 
  
  }

error_rate_vector <- vector(mode="numeric", length=nrow(z))

for (i in 1:nrow(z)) {
  error_rate_vector[i] <- re.knn[[i]]$error_rate
}
overall_error_rate <- mean(error_rate_vector)
overall_error_rate










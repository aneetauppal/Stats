#Aneeta Uppal
#Machine learning
#Homework 2


PCA_func=function(data_path){
  
  datafile=read.csv(data_path)
  datafile=datafile[ ,1:2]
  data_centered=scale(datafile, center=T, scale=T)
  covariance_data= round(cov(data_centered), 60)
  eigen_val_vec <- eigen(covariance_data, symmetric = TRUE)
  eigenv <- eigen_val_vec$values
  sdeviation<- sqrt(eigenv)
  loadings = eigen_vectors * sqrt(eigenvalues)
  projectdata=cbind(eigen_vectors[,1:2]) 
  variance = var(projectdata)
  scores = data_centered %*% projectdata
  final <- list(eigenv = eigenv, sdeviation = sdeviation, variance=variance, scores = scores)
  return(final)
}

x <- PCA_func("/Users/aneetauppal/Downloads/assignment_2_MR/Homework_2_dataset_prob3.csv")
x
file <- read.csv("/Users/aneetauppal/Downloads/assignment_2_MR/Homework_2_dataset_prob3.csv")
x
str(x)

#to check
x <- princomp(file, cor = T)
summary(x)
str(x)


PCA_func2=function(data_path){
  
  datafile=read.csv(data_path)
  file = t(datafile)
  data_centered=scale(file, center=T, scale=F)
  covariance_data= cov(data_centered)
  eigen_val_vec <- eigen(covariance_data, symmetric = TRUE)
  eigenv <- eigen_val_vec$values
  sdeviation<- sqrt(eigenv)
  loadings = eigen_vectors * sqrt(eigenvalues)
  projectdata=cbind(eigen_vectors[,1:2])
  variance = var(projectdata)
  scores = data_centered %*% projectdata
  final <- list(eigenv = eigenv, sdeviation = sdeviation, variance=variance, scores = scores)
  return(final)
}

re <- PCA_func2("/Users/aneetauppal/Downloads/assignment_2_MR/Homework_2_dataset_prob4.csv")
re


#plots
plot(re$sdeviation, pch=16, cex=1, col="red",
     main="scree plot", 
     xlab="PC number", ylab="variance")


plot(re$scores[1:20,1], re$scores[1:20, 2],
     pch=16, cex=1,
     xlim=c(min(scores[,1]), max(scores[,1])),
     ylim=c(min(scores[,2]), max(scores[,2])),
     main="scores plot",
     xlab="PC1", ylab="PC2")

plot(re$loadings[,1], re$loadings[,2], 
     pch=16, cex=1,
     xlab="PC1", ylab="PC2")


#Aneeta Uppal
#homework 3
#functions for Kmeans and heirarchical clustering 

#installations needed for dist2 function calculator
install.packages("flexclust")
library(flexclust)

K_means=function(data_path, k, iter){
  datafile=(data_path)
  maxiter = iter
  distance <- NULL 
  
  index = 0
  coordinates<-list()

  
  for(i in 1:k){
    tmp <- (datafile[i,])
    coordinates[[index]]<- tmp
  }

  matrix1 <- matrix(unlist(coordinates), ncol=2, byrow=TRUE)
  z<- (unlist(coordinates, recursive = TRUE))
  matrix2 <- data_in[-(1:k),]
  
 
   for(i in 1:nrow(matrix2)) {
      for(i in 1:nrow(matrix1)){
        distance[i] <- sqrt(sum(matrix1[i,] - matrix2[i,])^2)
      } 
   }
    
 # for (i in 1:ncol(data_in)){
    
#  }
distan<-dist2(matrix1, matrix2, method = "euclidean")
  
  centers = rowMeans(datafile)
    
  final <- list(centers = centers, coordinates = coordinates, matrix1 = matrix1, matrix2 = matrix2, distance = distance, z= z, distan = distan)
  return(final)
  
}

data_in <- matrix(c(1, 2, 2, 1, 4, 3, 5, 5), ncol=2, byrow=T)
x<-K_means(data_in, 2, 0)

x


stats:::kmeans
stats:::hclust


matrix1 <- matrix(unlist(coordinates), ncol=2, byrow=TRUE)
matrix2 <- data_in[-(1:k),]

#calculate the euclidean distance of two matrices
distan<-dist2(matrix1, matrix2, method = "euclidean")
for(i in distances[i])
  if((distan[1,i]) > (distan[2,i])){
    bucket1<-(matrix2[i,])
    bucket1list[[i]]<-bucket1
  } else{
    bucket2<-(matrix2[i,])
    bucket2list[[i]]<-bucket2
  }

centers = rowMeans(datafile)
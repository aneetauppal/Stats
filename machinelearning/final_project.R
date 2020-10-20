#Aneeta Uppal
#Final Project

rm(list=ls())
graphics.off()
install.packages("ggplot2")
library(ggplot2)


#############################
#Parse File/Pretreatment
#############################


datafile = read.csv("/Users/aneetauppal/Downloads/final_project/ST000003_peaks_table.csv")
#remove rows with nulls
withoutnulls <- datafile[complete.cases(datafile),]
data <- as.matrix(withoutnulls)

#remove columns we don't need
data_proc <- data[,-1:-7]
data_proc <- data_proc[,-19]
data_proc <- t(data_proc)
data_proc
#find row labels 
all_row_names <- rownames(data_proc)

#replace . with spaces in row names
all_row_names <- gsub(pattern="\\.", replacement=" ", x=all_row_names, perl=T)
rownames(data_proc) <- all_row_names
all_row_names

#normalize data
data_norm<- scale(data_proc, center=T, scale =T)
data_norm


##################################
#Principal Components Analysis
##################################
re.pca<-prcomp(data_proc, cor = T)
summary(re)

#Make a Scree Plot
plot(re.pca$sdev^2,
     pch=16, cex=1, col="blue",
     main="Scree plot",
     xlab="PCA index", ylab="variance")

#find the variance
variance_all <- re.pca$sdev^2
#find variance in pc1&pc2
princomp_var <- sum(variance_all[1:2]) / sum(variance_all)
princomp_var
princomp_3_var <-sum(variance_all[1:3]) / sum(variance_all)
princomp_3_var

#Loadings plot

plot(re.pca$rotation[,1], re.pca$rotation[,2], 
     pch=16, cex=1,
     xlab="PC1", ylab="PC2", 
     main="Loadings Plot")

text(re.pca$rotation[,1], 0.005+re.pca$rotation[,2], 
     labels=as.character(c(1:nrow(re.pca$rotation))), 
     pos=3, offset=0.05, cex=1, col="purple")

#plot scores
plot(re.pca$x[1:6,1], re.pca$x[1:6, 2],
     pch=16, cex=1, xlim=c(min(re.pca$x[,1]), max(re.pca$x[,1])), 
     ylim=c(min(re.pca$x[,2]), max(re.pca$x[,2])),
     col="blue",
     xlab="PC1", ylab="PC2",
     main="PCA scores plot")

points(re.pca$x[7:12, 1], re.pca$x[7:12, 2], pch=16, cex=1, col="purple")

points(re.pca$x[13:18, 1], re.pca$x[13:18, 2], pch=16, cex=1, col="pink")

legend_text <- c("isPSC", "m15", "mESC")
legend("topright", legend=legend_text,
       col=c("blue", "purple", "pink"),
       pch=16, cex=1)


###################################
#K-means clustering
###################################


datap<-data_proc
re.kmeans <- kmeans(x=datap, centers=5, algorithm="Lloyd")

#number of clusters
wss <- (nrow(datap)-1)*sum(apply(datap,2,var))

for (i in 2:15){
  wss[i] <- sum(kmeans(datap, centers=i )$withinss)
}

plot(1:15, wss, type="o", pch=18, 
     col="magenta",xlab="Number of Clusters", 
     ylab="Within groups of SS", main="Cluster Sizes")

#plot k-means
plot(datap[,c(106,127)], col=re.kmeans$cluster, 
     pch=16, xlab= "Metabolite-106", ylab= "Metabolite-127", 
     main= "Clustering with kmeans")

#show centers
points(re.kmeans$centers[,c(106,127)], 
       col=1:5, pch=8, cex=2)

text(datap[,c(106,127)], labels=rownames(datap), 
     pos=3, offset=0.3, cex=0.5, col="deeppink1")

###########################################
#LDA
###########################################
#[,1] is column labeles

MLTB_dir <- "/Users/aneetauppal/Graduate_MS/Fall_Semester_2016/Machine_learning/MLTB_export"
packageName <- "MASS"
install.packages(packageName)
library(packageName, character.only = T)

newdata<-read.csv("/Users/aneetauppal/Graduate_MS/Fall_Semester_2016/Machine_learning/MyData.csv")
newdata <- data.frame(newdata)
newdata2 <-data.frame(t(newdata))

re.LDA.MASS <- lda(x=newdata[,1:144], grouping=newdata$X145,tol=1.0e-4, method="mle", CV=F)


plot(re.LDA.MASS)
re.LDA.MASS$means
re.LDA.MASS$scaling # the transformation matrix
re.LDA.MASS


# get the projections
Y <- t(re.LDA.MASS$scaling) %*% t(newdata[, 1:144])
Y <- t(Y)
Y

  
  
  # plot the projections
I_iPSC<- which(newdata$X145=="iPSC")
II_m15 <- which(newdata$X145=="m15")
III_mESC <- which(newdata$X145=="mESC")

plot(Y[I_iPSC,1], Y[I_iPSC,2],
     pch=16, cex=1,
     xlim=c(min(Y[,1]), max(Y[, 1])),
     ylim=c(min(Y[,2]), max(Y[,2])),
     col="red",
     xlab="LD1", ylab="LD2",
     main="LDA: Cell line projections on first two Linear Discr.")

points(Y[II_m15, 1], Y[II_m15, 2],
       pch=16, cex=1,
       col="green")

points(Y[III_mESC, 1], Y[III_mESC, 2],
       pch=16, cex=1,
       col="blue")

legend("topright", legend=c("iPSC", "m15", "mESC"), 
       pch=c(16, 16, 16), 
       col=c("red", "green", "blue"))


###################################
#USING ALL THE DATA
###################################




datafile = read.csv("/Users/aneetauppal/Graduate_MS/Fall_Semester_2016/Machine_learning/mydata3.csv")

data<- datafile
#remove columns we don't need
data_proc <- data[,-1:-7]
data_proc <- data_proc[,-19]
data_proc <- t(datafile)
#replace nulls with 0's
data_proc[is.na(data_proc)] <- 0
data_proc

#find row labels 
all_row_names <- rownames(data_proc)

#replace . with spaces in row names
all_row_names <- gsub(pattern="\\.", replacement=" ", x=all_row_names, perl=T)
rownames(data_proc) <- all_row_names
all_row_names

#normalize data
data_norm<- scale(data_proc, center=T, scale =T)
data_norm


##################################
#Principal Components Analysis
##################################
re.pca<-prcomp(data_proc, cor = T)
summary(re.pca)

#Make a Scree Plot
plot(re.pca$sdev^2,
     pch=16, cex=1, col="blue",
     main="Scree plot",
     xlab="PCA index", ylab="variance")

#find the variance
variance_all <- re.pca$sdev^2
#find variance in pc1&pc2
princomp_var <- sum(variance_all[1:2]) / sum(variance_all)
princomp_var
princomp_3_var <-sum(variance_all[1:3]) / sum(variance_all)
princomp_3_var

#Loadings plot

plot(re.pca$rotation[,1], re.pca$rotation[,2], 
     pch=16, cex=1,
     xlab="PC1", ylab="PC2", 
     main="Loadings Plot")

text(re.pca$rotation[,1], 0.005+re.pca$rotation[,2], 
     labels=as.character(c(1:nrow(re.pca$rotation))), 
     pos=3, offset=0.05, cex=1, col="purple")

#plot scores
plot(re.pca$x[1:6,1], re.pca$x[1:6, 2],
     pch=16, cex=1, xlim=c(min(re.pca$x[,1]), max(re.pca$x[,1])), 
     ylim=c(min(re.pca$x[,2]), max(re.pca$x[,2])),
     col="blue",
     xlab="PC1", ylab="PC2",
     main="PCA scores plot")

points(re.pca$x[7:12, 1], re.pca$x[7:12, 2], pch=16, cex=1, col="purple")

points(re.pca$x[13:18, 1], re.pca$x[13:18, 2], pch=16, cex=1, col="pink")

legend_text <- c("isPSC", "m15", "mESC")
legend("topright", legend=legend_text,
       col=c("blue", "purple", "pink"),
       pch=16, cex=1)


###################################
#K-means clustering
###################################


datap<-data_proc
re.kmeans <- kmeans(x=datap, centers=4, algorithm="Lloyd")

#number of clusters
wss <- (nrow(datap)-1)*sum(apply(datap,2,var))

for (i in 2:15){
  wss[i] <- sum(kmeans(datap, centers=i )$withinss)
}

plot(1:15, wss, type="o", pch=18, 
     col="magenta",xlab="Number of Clusters", 
     ylab="Within groups of SS", main="Cluster Sizes")

#plot k-means
plot(datap[,c(106,197)], col=re.kmeans$cluster, 
     pch=16, xlab= "Metabolite-106", ylab= "Metabolite-127", 
     main= "Clustering with kmeans")

#show centers
points(re.kmeans$centers[,c(106,127)], 
       col=1:5, pch=8, cex=2)

text(datap[,c(106,127)], labels=rownames(datap), 
     pos=3, offset=0.3, cex=0.5, col="deeppink1")

###########################################
#LDA
###########################################
#[,1] is column labeles

MLTB_dir <- "/Users/aneetauppal/Graduate_MS/Fall_Semester_2016/Machine_learning/MLTB_export"
packageName <- "MASS"
install.packages(packageName)
library(packageName, character.only = T)

newdata<-read.csv("/Users/aneetauppal/Graduate_MS/Fall_Semester_2016/Machine_learning/MyData.csv")
newdata <- data.frame(newdata)
newdata2 <-data.frame(t(newdata))
newdata<-(t(data_proc))
re.LDA.MASS <- lda(x=newdata[,1:555], grouping=newdata$X145,tol=1.0e-4, method="mle", CV=F)
newdata

plot(re.LDA.MASS)
re.LDA.MASS$means
re.LDA.MASS$scaling # the transformation matrix
re.LDA.MASS


# get the projections
Y <- t(re.LDA.MASS$scaling) %*% t(newdata[, 1:144])
Y <- t(Y)
Y



# plot the projections
I_iPSC<- which(newdata$X145=="iPSC")
II_m15 <- which(newdata$X145=="m15")
III_mESC <- which(newdata$X145=="mESC")

plot(Y[I_iPSC,1], Y[I_iPSC,2],
     pch=16, cex=1,
     xlim=c(min(Y[,1]), max(Y[, 1])),
     ylim=c(min(Y[,2]), max(Y[,2])),
     col="red",
     xlab="LD1", ylab="LD2",
     main="LDA: Cell line projections on first two Linear Discr.")

points(Y[II_m15, 1], Y[II_m15, 2],
       pch=16, cex=1,
       col="green")

points(Y[III_mESC, 1], Y[III_mESC, 2],
       pch=16, cex=1,
       col="blue")

legend("topright", legend=c("iPSC", "m15", "mESC"), 
       pch=c(16, 16, 16), 
       col=c("red", "green", "blue"))


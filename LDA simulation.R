library(datasets)
data(iris)

falsecount=replicate(n=100,{
  # divide data into train and test
  train=iris[sample(nrow(iris),50,replace=FALSE,prob=NULL),]
  test=iris[sample(nrow(iris),100,replace=FALSE,prob=NULL),]
  #Sample size in training set
  n_setosa=sum(ifelse(train$Species=="setosa",1,0))
  n_versicolor=sum(ifelse(train$Species=="versicolor",1,0))
  n_virginica=sum(ifelse(train$Species=="virginica",1,0))
  
  ############## Choose Prior ##############
  #Prior=relative sample size in train data
  p_setosa=n_setosa/50
  p_versicolor=n_versicolor/50
  p_virginica=n_virginica/50

  ############## Calculate sample mean vectors ##############
  #subsetting by each variable
  setosa=train[which(train$Species=="setosa"),]
  Mean_setosa=colMeans(setosa[,1:4])
  versicolor=train[which(train$Species=="versicolor"),]
  Mean_versicolor=colMeans(train[,1:4])
  virginica=train[which(train$Species=="virginica"),]
  Mean_virginica=colMeans(train[,1:4])
  
  
  
  ############## Calculate pooled variance-covariance matrix ##############
  #Sample variance-covariance matrix for each specie
  S_setosa=cov(setosa[,1:4])
  S_versicolor=cov(versicolor[,1:4])
  S_virginica=cov(virginica[,1:4])
  
  #Complete fomula
  S_pooled= ((n_setosa-1)*S_setosa+(n_versicolor-1)*S_versicolor+(n_virginica-1)*S_virginica)/(n_setosa+n_versicolor+n_virginica-3)
  
  S_inv=solve(S_pooled)
  
  #Simple way
  #S_pooled=(S_setosa+S_versicolor+S_virginica)/3
  
  ############## Calculate alpha_i ##############
  
  alpha_setosa= -0.5* t(Mean_setosa) %*% S_inv %*% Mean_setosa
  alpha_versicolor= -0.5* t(Mean_versicolor) %*% S_inv %*% Mean_versicolor
  alpha_virginica= -0.5* t(Mean_virginica) %*% S_inv %*% Mean_virginica
  
  
  ############## Calculate beta_i ##############
  
  beta_setosa=S_inv %*% Mean_setosa
  beta_versicolor=S_inv %*% Mean_versicolor
  beta_virginica=S_inv %*% Mean_virginica
  
  
  ############## Classification ##############
  prediction=c()
  d_setosa_vec=c()
  d_versicolor_vec=c()
  d_virginica_vec=c()
  label=c("setosa", "versicolor", "virginica")
  
  for(i in 1:nrow(test)){
    #Read an ovservation in test data
    x=t(test[i,1:4])
    
    #Calcualte linear discriminant functions for each speice
    d_setosa=alpha_setosa+ t(beta_setosa) %*% x
    d_versicolor=alpha_versicolor+ t(beta_versicolor) %*% x
    d_virginica=alpha_virginica+ t(beta_virginica) %*% x
    
    #Calssify the observation to the speice with highest function value
    d_vec=c(d_setosa, d_versicolor, d_virginica)
    prediction=append(prediction, label[which.max( d_vec )])
    
    d_setosa_vec=append(d_setosa_vec, d_setosa)
    d_versicolor_vec=append(d_versicolor_vec, d_versicolor)
    d_virginica_vec=append(d_virginica_vec, d_virginica)
    
  }
  
  #Combine the predicted resutls to the test dataset.
  test$prediction=prediction
  falsenum=sum(ifelse(test$prediction!=test$Species,1,0))
  
})
hist(falsecount)



#euclidean and average
dist_L2=dist(iris, method="euclidean")
den2=as.dendrogram(hclust (dist_L2, method ="average"))
plot(den2)
cutree=cutree(den2,k=3)
irisbind=cbind(iris,cutree)
true=c(rep(1,50),rep(2,50),rep(3,50))
iris_compare=cbind(iris,cutree,true)
Eucledian_count = sum(iris_compare$cutree==iris_compare$true)/nrow(iris_compare)

#manhattan and average
dist_L2=dist(iris, method="manhattan")
den2=as.dendrogram(hclust (dist_L2, method ="average"))
plot(den2)
cutree=cutree(den2,k=3)
irisbind=cbind(iris,cutree)
true=c(rep(1,50),rep(2,50),rep(3,50))
iris_compare=cbind(iris,cutree,true)
manhattan_count = sum(iris_compare$cutree==iris_compare$true)/nrow(iris_compare)

#euclidean and single
dist_L2=dist(iris, method="euclidean")
den2=as.dendrogram(hclust (dist_L2, method ="single"))
plot(den2)
cutree=cutree(den2,k=3)
irisbind=cbind(iris,cutree)
true=c(rep(1,50),rep(2,50),rep(3,50))
iris_compare=cbind(iris,cutree,true)
single_count = sum(iris_compare$cutree==iris_compare$true)/nrow(iris_compare)

#euclidean and average
species=iris[,5]
irisdf=iris[,-5]
irisdf
irisstandard=scale(irisdf,center=TRUE,scale=TRUE)
irisstandard
standard=cbind(irisstandard,iris[,5])
head(standard)
dist_L2=dist(standard, method="euclidean")
den2=as.dendrogram(hclust (dist_L2, method ="average"))
plot(den2)
cutree=cutree(den2,k=3)
irisbind=cbind(standard,cutree)
irisbind
true=c(rep(1,50),rep(2,50),rep(3,50))
iris_compare=cbind(standard,cutree,true)
iris_compare=data.frame(iris_compare)
standard_count = sum(iris_compare$cutree==iris_compare$true)/nrow(iris_compare)
print(standard_count)






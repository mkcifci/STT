###################################################################################
###### STAT 557 (Group Project 1); Muhammed Cifci, Caner Simsek, Chase Bloch ######
###################################################################################

rm(list=ls()) ## To clear your environment

## Read the data
xTrain=read.csv("ecoli_xTrain.csv",header = FALSE)
yTrain=read.csv("ecoli_yTrain.csv",header = FALSE)
xTest=read.csv("ecoli_xTest.csv",header = FALSE)
yTest=read.csv("ecoli_yTest.csv",header = FALSE)

new_xTrain=read.csv("ecoli_new.xTrain.csv",header = FALSE)
new_yTrain=read.csv("ecoli_new.yTrain.csv",header = FALSE)
new_xTest=read.csv("ecoli_new.xTest.csv",header = FALSE)
new_yTest=read.csv("ecoli_new.yTest.csv",header = FALSE)

#### Part 1 ####
xi<-c(1,3,5,7,9)

#X is a vector of numbers in logspace
logProd <- function(x){
  sum(x)
}

logSum <- function(x){
  max(x) + log(sum(exp(x-max(x))))
}
logSum(xi)

#### Part 2 ####
#Function for probability of class priors
prior <- function(yTrain){
  y1 = sum(yTrain$V1 == 1)/length(yTrain[,1])
  y2 = sum(yTrain$V1 == 2)/length(yTrain[,1])
  y3 = sum(yTrain$V1 == 3)/length(yTrain[,1])
  y4 = sum(yTrain$V1 == 4)/length(yTrain[,1])
  y5 = sum(yTrain$V1 == 5)/length(yTrain[,1])
  
  p = c(y1,y2,y3,y4,y5)
  return(p)
}


#Test implementation to ensure that it works
p <- prior(yTrain)

#likelihood function
likelihood <- function(xTrain, yTrain){
  nfeat = length(xTrain[1,])
  nclass = length(unique(yTrain[,1]))
  data <- xTrain
  data$Y <- data.matrix(yTrain)

  
  M<-matrix(nrow = nclass,ncol = nfeat)
  for(k in 1:nclass){
    M[,k]<-1/length(subset(data,Y == k)[,1])*colSums(subset(data,Y == k)[1:nfeat])
  }
  
  V<-matrix(nrow = nclass,ncol = nfeat)
  for(k in 1:nclass){
    V[,k]<-1/length(subset(data,Y == k)[,1])*colSums((subset(data,Y==k)[1:nfeat] - M[,k])^2)
  }
  return(list(M,V))
}

#Specify conditional matrices
M <- matrix(unlist(likelihood(xTrain,yTrain)[1]),ncol = 5,nrow = 5)
V <- matrix(unlist(likelihood(xTrain,yTrain)[2]),ncol=5,nrow=5)
####

#Naive Bayes classification function
naiveBayesClassify <- function(xTest, M, V, p){
  output<-vector(length = length(xTest[,1]))
  for(i in 1:length(xTest[,1])){
    output[i]<-which.max(p*apply(dnorm(as.numeric(xTest[i,]),M,sqrt(V)),2,prod))
  }
  return(output)
}

#Predictions
output<-naiveBayesClassify(xTest,M,V,p)

#Test with outside Naive Bayes package
#install.packages("e1071")
library(e1071)
data<-xTrain
data$Y<-data.matrix(yTrain)
NB = naiveBayes(as.factor(Y) ~ .,data = data)
pred <- as.numeric(predict(NB, xTest))
table(pred,yTest$V1)

#Compare accuracies
acc <- sum(yTest == output)/length(yTest[,1])
packacc <- sum(yTest == pred)/length(yTest[,1])

#Specify confusion matrix
cm <- table(output,yTest$V1)

#Calculate precisions and recalls
prec1 <- cm[1]/(cm[1]+cm[11])
reca1 <- cm[1]/(cm[1]+cm[2])
prec5 <- cm[25]/cm[25]
reca5 <- cm[25]/(cm[25]+cm[23])

#### Part 3 ####
# y is (0,1). w is weights. Function returns a value p=p(y|x)
sigmoidProb <- function(y,x,w){
  if(y==0){
    p <- 1/(1 + exp(sum(w*x)))}
  else{
    p <- (exp(sum(w*x))/(1+exp(sum(w*x))))
  }
  return(p)
}

w0 = c(1,1,1,1,1,1)

logisticRegressionWeights<- function(new_xTrain, new_yTrain, w0, nIter){
  wn <- w0
  p0 <- rep(0,length(new_xTrain[,1]))
  for(k in 1:nIter){
      for(l in 1:length(new_xTrain[,1])){
        p0[l] <- sigmoidProb(0,new_xTrain[l,],wn)
      }
  val <- rep(0,length(new_xTrain[,]))
  for(i in 1:length(new_xTrain[,])){
    val[i] <- 0.1*colSums(new_xTrain[i]*(new_yTrain-p0))
  }
  wn<-wn-val
  }
  return(wn)
}

w<-logisticRegressionWeights(new_xTrain, new_yTrain, w0, 500)


logisticRegressionClassify <- function(new_xTest, w){
  prob <- 1/(1+exp(sum(new_xTest*w)))
  #return(prob)
  if(prob > .5){return(1)}
  else{return(0)}
}

outcome_2 <- rep(0,length(new_xTest[,1]))
for(i in 1:length(new_xTest[,1])){
  outcome_2[i] <- logisticRegressionClassify(new_xTest[i,],w)
}

# Validate results with 'glm' function
new_yTrain2=read.csv("ecoli_new.yTrain.csv", header = FALSE)
new_yTest2=read.csv("ecoli_new.yTest.csv", header = FALSE)
new_xTrain2=read.csv("ecoli_new.xTrain.csv", header = FALSE)
new_xTest2=read.csv("ecoli_new.xTest.csv", header = FALSE)

new_xTest2$Y <- as.factor(new_yTest2$V1)
new_xTrain2$Y <- as.factor(new_yTrain2$V1)

logit_mod <- glm(Y ~ ., data=new_xTrain2 ,family = binomial)
pred2 <- predict(logit_mod, new_xTest2, type="response")
pred2 <- ifelse(pred2 > 0.5, 1, 0)

#Compare accuracies
acc_2 <- sum(new_yTest == outcome_2)/length(new_yTest[,1]) # 0.9633
packacc_2 <- sum(new_yTest2 == pred2)/length(new_yTest2[,1]) # 0.9541

#Specify confusion matrix
cm2 <- table(outcome_2,new_yTest$V1)

#Calculate precisions and recalls
prec2 <- cm2[1]/(cm2[1]+cm2[3]) # [1] 0.9577465
reca2 <- cm2[1]/(cm2[1]+cm2[2]) # [1] 0.9855072



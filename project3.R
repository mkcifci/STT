###################################################################################
###### STAT 557 (Group Project 3); Muhammed Cifci, Caner Simsek, Chase Bloch ######
###################################################################################

options(scipen=999)

library(readxl)
RNGkind(sample.kind = "Rounding")
set.seed(1908)

data <- read_excel("data.xlsx")

data$partychoice<-"Center Populists"
data$partychoice[data$rightpop==1]<-"Rightwing Populist"
data$partychoice[data$rightmaparty==1]<-"Mainstream Right"
data$partychoice[data$leftmaparty==1]<-"Mainstream Left"
data$partychoice[data$leftpop==1]<-"Leftwing Populist"
data$partychoice[data$farleft==1]<-" Far-Left"
data <- data[!(data$farright==1),]
data$partychoice<-as.factor(data$partychoice)

data_plot <- table(data$partychoice)
barplot(data_plot, ylim=c(0, 14000))

data$partychoice<-as.numeric(data$partychoice)

table(data$partychoice)

#pca
pca <- prcomp(data[c(4:30)], center = TRUE, scale = TRUE) #1:3 are categorical variables
summary(pca)

pca2<-princomp(data[c(4:30)], cor = FALSE, scores = TRUE)
summary(pca2)

#prcomp uses spectral decomposition wheres princomp uses singular value decomposition
#putting all the variables together is not very informative. using theory, I come up,
#following group of variables:
efficacy<-data.frame(data$stfgov,data$stfdem,data$psppsgva,data$cptppola,data$trstplt,data$psppipla)
cult.grvs<-data.frame(data$imptrad,data$ipfrule,data$imwbcnt,data$freehms)
econ.grvs<-data.frame(data$stfeco,data$gincdif,data$hincfel)

pca3<-princomp(efficacy,scores=T)
summary(pca3) 
data$efficacy<-pca3$scores[,1]

pca4<-princomp(cult.grvs,scores=T)
summary(pca4) 
data$cultural<-pca4$scores[,1]

pca5<-princomp(econ.grvs,scores=T)
summary(pca5)
data$economic<-pca5$scores[,1] #first components explain much of the variance,
#thus i only use them.

#visualizing pca

library(factoextra)

fviz_eig(pca3, addlabels = TRUE, ylim = c(0, 70), main="Efficacy")
fviz_eig(pca4, addlabels = TRUE, ylim = c(0, 60), main="Cultural grievances")
fviz_eig(pca5, addlabels = TRUE, ylim = c(0, 80), main="Economic grievances")

fviz_pca_var(pca3, col.var = "black")

# Contributions of variables to PC1
fviz_contrib(pca3, choice = "var", axes = 1, top = 10)
fviz_contrib(pca4, choice = "var", axes = 1, top = 10)

# 1. Apply K-Means to your dataset. Use adjusted rand index to evaluate your results.
# Run K-Means for multiple rounds with different random initializations. 
# Do the results vary for different initializations?
library(dplyr)
data2 <- data %>% 
    select(efficacy, economic , cultural , eduyrs , agea , gndr , hinctnta)

label <- data$partychoice

# 2. Evaluate how results change with respect to parameter K (using adjusted rand index). 
# Since random initialization impacts the results, 
# run K-means multiple times and show the average result.

#K-means varies with random initialization
set.seed(1234)
library(mclust)
nstarts_2 = 20
km.result_2 = rep(0,nstarts_2)
for(i in 1:nstarts_2){
    km.out_2 = kmeans(data2, 6, nstart=5)
    km.result_2[i] <- adjustedRandIndex(km.out_2$cluster, label)
}

#K-means converges when increasing number of random sets
nstarts = 20
km.result = rep(0,nstarts)
for(i in 1:nstarts){
    km.out = kmeans(data2, 6, nstart=i)
    km.result[i] <- adjustedRandIndex(km.out$cluster, label)
}

#Evaluating K-means as K increases
nstarts_3 = 20
km.result_3 = rep(0,nstarts_3)
ave_iter = 5
result_matrix = matrix(, nrow=ave_iter,ncol=(nstarts_3))
for(j in 1:ave_iter){
    for(i in 1:(nstarts_3)){
        km.out_3 = kmeans(data2, i, nstart=5)
        km.result_3[i] <- adjustedRandIndex(km.out_3$cluster, label)
        
    }
    result_matrix[j,] <- km.result_3
}
ari_means <- colMeans(result_matrix)

par(mfrow=c(1,2))
plot((1:nstarts),km.result,col="blue", xlim=range(c(0,20)), xlab="Nstart iteration",ylab="Adjusted Rand Index")
lines((1:nstarts),km.result,col="blue" )

plot((1:nstarts),ari_means, col="red", ylim=range(c(-0.0004862436,0.005438634)), xlim=range(c(0,20)), xlab="K",ylab="Adjusted Rand Index")
lines((1:nstarts),ari_means, col="red")

# 3. Apply Gaussian mixture model and evaluate the results (using adjusted rand index). 
# Try several different K.

gaus2 = Mclust(data2, G=2)
gaus3 = Mclust(data2, G=3)
gaus4 = Mclust(data2, G=4)
gaus5 = Mclust(data2, G=5)
gaus6 = Mclust(data2, G=6)
gaus7 = Mclust(data2, G=7)
gaus8 = Mclust(data2, G=8)
gaus9 = Mclust(data2, G=9)
gaus10 = Mclust(data2, G=10)
gaus11 = Mclust(data2, G=11)

a1 <- adjustedRandIndex(label, gaus2$classification)
a2 <- adjustedRandIndex(label, gaus3$classification)
a3 <- adjustedRandIndex(label, gaus4$classification)
a4 <- adjustedRandIndex(label, gaus5$classification)
a5 <- adjustedRandIndex(label, gaus6$classification)
a6 <- adjustedRandIndex(label, gaus7$classification)
a7 <- adjustedRandIndex(label, gaus8$classification)
a8 <- adjustedRandIndex(label, gaus9$classification)
a9 <- adjustedRandIndex(label, gaus10$classification)
a10 <- adjustedRandIndex(label, gaus11$classification)

gaus_mat <- matrix(c(1,2,3,4,5,6,7,8,9,10, a1,a2,a3,a4,a5,a6,a7,a8,a9,a10),nrow=10,ncol=2, byrow = FALSE)

plot(gaus_mat, type="p", ylim=range(c(-0.0003992302,0.005599785)), ylab = "Adjusted Rand Index", xlab="K", xaxt='n')
axis(1, at=1:10,labels=c("2","3","4","5","6","7","8","9","10","11"))
lines(gaus_mat,col="blue" )

# fviz_cluster(gaus6, data = data2)

# 4. Use PCA to reduce the feature space to 2 dimensions. Apply K-Means and Gaussian 
# mixture to 2- dimensional data. Compare the results using scatter plot. 
# Try several different numbers of clusters.

data_2_pca <- princomp(data2,scores=T)
data2_pca <- data_2_pca$scores[,1:2]

gaus2_1 = Mclust(data2_pca, G=2)
gaus3_1 = Mclust(data2_pca, G=3)
gaus4_1 = Mclust(data2_pca, G=4)
gaus5_1 = Mclust(data2_pca, G=5)
gaus6_1 = Mclust(data2_pca, G=6)
gaus7_1 = Mclust(data2_pca, G=7)
gaus8_1 = Mclust(data2_pca, G=8)
gaus9_1 = Mclust(data2_pca, G=9)
gaus10_1 = Mclust(data2_pca, G=10)
gaus11_1 = Mclust(data2_pca, G=11)

a1_1 <- adjustedRandIndex(label, gaus2_1$classification)
a2_1 <- adjustedRandIndex(label, gaus3_1$classification)
a3_1 <- adjustedRandIndex(label, gaus4_1$classification)
a4_1 <- adjustedRandIndex(label, gaus5_1$classification)
a5_1 <- adjustedRandIndex(label, gaus6_1$classification)
a6_1 <- adjustedRandIndex(label, gaus7_1$classification)
a7_1 <- adjustedRandIndex(label, gaus8_1$classification)
a8_1 <- adjustedRandIndex(label, gaus9_1$classification)
a9_1 <- adjustedRandIndex(label, gaus10_1$classification)
a10_1 <- adjustedRandIndex(label, gaus11_1$classification)

gaus_mat2 <- matrix(c(1,2,3,4,5,6,7,8,9,10, a1_1,a2_1,a3_1,a4_1,a5_1,a6_1,a7_1,a8_1,a9_1,a10_1),nrow=10,ncol=2, byrow = FALSE)

par(mfrow=c(1,1)) 
plot(gaus_mat, type="p", ylab = "Adjusted Rand Index", xlab="K", ylim=range(c(-0.0004291757,0.01485755)), xaxt='n', main="Gaussian Mixture Model")
axis(1, at=1:10,labels=c("2","3","4","5","6","7","8","9","10","11")) 
lines(gaus_mat,col="red" )
par(new=TRUE)
plot(gaus_mat2, type="p", ylab = "Adjusted Rand Index", xlab="K", ylim=range(c(-0.0004291757,0.01485755)), xaxt='n')
axis(1, at=1:10,labels=c("2","3","4","5","6","7","8","9","10","11"))
lines(gaus_mat2,col="blue" )
legend(1,.015,legend=c("With PCA", "Without PCA"), col=c("red","blue"), lty=1:1)


max(gaus_mat[,2],gaus_mat2[,2]) 

##Evaluating K-means as K increases with two dimensional data
nstarts_4 = 20
km.result_4 = rep(0,nstarts_4)
ave_iter = 5
result_matrix = matrix(, nrow=ave_iter,ncol=(nstarts_4))
for(j in 1:ave_iter){
    for(i in 1:(nstarts_4)){
        km.out_4 = kmeans(data2_pca, i, nstart=20)
        km.result_4[i] <- adjustedRandIndex(km.out_4$cluster, label)
        
    }
    result_matrix[j,] <- km.result_4
}
ari_means_2 <- colMeans(result_matrix)

plot((1:nstarts),ari_means, col="red", ylim=range(c(-0.0004862436,0.005438634)), xlim=range(c(0,20)), xlab="K",ylab="Adjusted Rand Index")
lines((1:nstarts),ari_means, col="red")

par(mfrow=c(1,1)) 
plot(ari_means, type="p", ylab = "Adjusted Rand Index", xlab="K", ylim=range(c( -0.0007287559,0.005438634)), xlim=range(c(1:20)), main="K-Means")
axis(1, at=seq(0, 20, 1))
lines(ari_means,col="red" )
par(new=TRUE)
plot(ari_means_2, type="p", ylab = "Adjusted Rand Index", xlab="K", ylim=range(c( -0.0007287559,0.005438634)), xlim=range(c(1:20)))
lines(ari_means_2,col="blue" )
legend(16,.005,legend=c("With PCA", "Without PCA"), col=c("red","blue"), lty=1:1)

min(ari_means,ari_means_2)

# description of the data, objectives, methods, parameter choices and justification, results, 
# interpretation and conclusion. 

library(factoextra)
fviz_cluster(gaus2_1 , data = , palette = c(""),  geom = "point", ellipse.type = "convex", ggtheme = theme_bw())
             
# fviz_cluster(km.out, data = data2)
# plot(data2, col=(km.out$cluster+1))


load("~/Google Drive/MSC/AARMS 2018 (UPEI)/High dim data/prostate.rda")

#dataplot
quartz()
plot(prostate.x)


#SIS
#generate response (binary)
library(SIS)
model=SIS(prostate.x, prostate.y, family='binomial', tune='bic', varISIS='aggr', seed=21)


#glmnet
#binomial
library(glmnet)
quartz()
fit=glmnet(prostate.x,prostate.y,family='binomial')
plot(fit, xvar = "dev", label = TRUE)
predict(fit, newx = prostate.x[1:5,], type = "class", s = c(0.05, 0.01))
cvfit = cv.glmnet(prostate.x, prostate.y, family = "binomial", type.measure = "class")
quartz()
plot(cvfit)
cvfit$lambda.min
cvfit$lambda.1se


#linear regression
library(ncvreg)
quartz()
fit2 <- ncvreg(prostate.x,prostate.y,penalty="SCAD")
plot(fit2, main = expression(paste("a =",3.7)))
quartz()
fit3 <- ncvreg(prostate.x,prostate.y,gamma=10,penalty="SCAD")
plot(fit3, main = expression(paste("a =",10)))
quartz()
fit4 <- ncvreg(prostate.x,prostate.y,gamma=5,penalty="SCAD")
plot(fit4, main = expression(paste("a =",5)))
quartz()
fit5 <- ncvreg(prostate.x,prostate.y)
plot(fit5, main = expression(paste("MCP, ", gamma,"=",3)))
quartz()
cvfitn <- cv.ncvreg(prostate.x, prostate.y)
plot(cvfitn)
cvfitn$lambda.min


quartz()
fit6 <- ncvreg(prostate.x,prostate.y,penalty="SCAD")
plot(fit6, main = expression(paste(alpha,"=",1)))
quartz()
fit7 <- ncvreg(prostate.x,prostate.y,alpha=0.9,penalty="SCAD")
plot(fit7, main = expression(paste(alpha,"=",0.9)))
quartz()
fit8 <- ncvreg(prostate.x,prostate.y,alpha=0.5,penalty="SCAD")
plot(fit8, main = expression(paste(alpha,"=",0.5)))
quartz()
fit9 <- ncvreg(prostate.x,prostate.y,alpha=0.1,penalty="SCAD")
plot(fit9, main = expression(paste(alpha,"=",0.1)))


#Logistic regression
library(ncvreg)
quartz()
fit10 <- ncvreg(prostate.x,prostate.y,family="binomial",penalty="SCAD")
plot(fit10, main = expression(paste("a =",3.7)))
quartz()
fit11 <- ncvreg(prostate.x,prostate.y,gamma=10,family="binomial",penalty="SCAD")
plot(fit11, main = expression(paste("a =",10)))
quartz()
fit12 <- ncvreg(prostate.x,prostate.y,gamma=5,family="binomial",penalty="SCAD")
plot(fit12, main = expression(paste("a =",5)))
quartz()
fit13 <- ncvreg(prostate.x,prostate.y,family="binomial")
plot(fit13, main = expression(paste("MCP, ", gamma,"=",3)))

quartz()
fit14 <- ncvreg(prostate.x,prostate.y,family="binomial",penalty="SCAD")
plot(fit14, main = expression(paste(alpha,"=",1)))
quartz()
fit15 <- ncvreg(prostate.x,prostate.y,family="binomial",alpha=0.9,penalty="SCAD")
plot(fit15, main = expression(paste(alpha,"=",0.9)))
quartz()
fit16 <- ncvreg(prostate.x,prostate.y,family="binomial",alpha=0.5,penalty="SCAD")
plot(fit16, main = expression(paste(alpha,"=",0.5)))
quartz()
fit17 <- ncvreg(prostate.x,prostate.y,family="binomial",alpha=0.1,penalty="SCAD")
plot(fit17, main = expression(paste(alpha,"=",0.1)))


#K-means clustering
pkgs <- c("factoextra",  "NbClust")
library(factoextra)
library(NbClust)
# Elbow method
quartz()
fviz_nbclust(prostate.x, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# Silhouette method
quartz()
fviz_nbclust(prostate.x, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
quartz()
fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

library(cluster)
library(shape)
set.seed(150)
quartz()
kmean<-kmeans(prostate.x[,c(2,1)],5,iter.max=300,nstart=10)
clusplot(prostate.x[,c(2,1)],kmean$cluster,line=0,shade=F,color=TRUE,labels=4,plotchar=TRUE,span=TRUE,main=paste('Clusters'))


#hclust
library(haven)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
# Compute with agnes
hc2 <- agnes(prostate.x, method = "complete")
hc3 <- agnes(prostate.x, method = "ward")
hc4 <- agnes(prostate.x, method = "single")
hc5 <- agnes(prostate.x, method = "average")
# Agglomerative coefficient
hc5$ac #0.5930282
hc4$ac #0.4575989
hc3$ac #0.9126836
hc2$ac #coefficient is 0.6992576
quartz()
hc6 <- agnes(prostate.x, method = "single")
pltree(hc6, cex = 0.6, hang = -1, main = "Dendrogram of agnes")


#SVM

library(e1071)

#Fit a model. The function syntax is very similar to lm function

model_svm <- svm(prostate.y ~ prostate.x)

#Use the predictions on the data

pred <- predict(model_svm)

#For svm, we have to manually calculate the difference between actual values with our predictions (pred)

error_2 <- prostate.y - pred

svm_error <- sqrt(mean(error_2^2)) #error is 0.1425545
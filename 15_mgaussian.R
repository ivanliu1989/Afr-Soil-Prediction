#Create a dataset
set.seed(888)
library(glmnet)
x=matrix(rnorm(100*20),100,20)
cf <- sample(0:1, 20, replace=TRUE) #Select some columns
response1 <- x %*% cf*runif(20) #Apply random coefficients
response2 <- x %*% cf*runif(20)
y=cbind(response1, response2)

#Fit a single lasso model
#0 for ridge
#1 for lasso
#>0 & <1 for the elastic net (mix of ridge and lasoo)
fit1m=glmnet(x,y,family="mgaussian",alpha=1)
plot(fit1m,type.coef="2norm")

#Select lambda through cross validation
fit1m.cv <- cv.glmnet(x,y,family="mgaussian",alpha=1) 
plot(fit1m.cv)
coef(fit1m.cv) #Show coefficients at the selected value of lambda


y <- as.matrix(y)
x<-as.matrix(x)

summary(x)

pred_P <- predict(fit1m, test)
test <- as.matrix(test)

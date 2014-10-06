setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/datasets_all_01Oct2014.RData')
ID <- as.data.frame(test[,1])
names(ID)<-'PIDN'
index_P <- createDataPartition(train_P$P, p=0.75, list = F)
train_P_1 <- train_P[index_P,]
train_P_2 <- train_P[-index_P,]

y <- train_Ca$Ca;
x <- train_Ca[,-c(1)];
# x$Depth <- as.numeric(x$Depth)
x <- as.matrix(x)
y<-as.matrix(y)

library('e1071')
fit_P_svmL <- svm(x,y,scale=T,kernel='linear',cost=100, cross=10, cachesize=1000, tolerance=.001)
fit_P_svmL$SV
fit_P_svmL$coefs
fit_P_svmL_tune <- tune.svm(Ca~., data=train_Ca, gamma=2^(-1:1),cost=2^(2:6))

tc <- tune.control(nrepeat=1,repeat.aggregate=min, sampling='cross',sampling.aggregate=min,
                   sampling.dispersion=sd,cross=10,best.model=T,
                   performances=T,error.fun=NULL)

fit_P_svmL_tune <- tune(svm, Ca~., data=train_Ca, ranges=list(gamma=2^(-1:1),cost=2^(2:8)),
                        tunecontrol = tc)
best.tune()

summary(fit_P_svmL_tune)
plot(fit_P_svmL_tune)

P <- predict(fit_P_svmL, train_P)
rmse(as.vector(P), train_P$P)

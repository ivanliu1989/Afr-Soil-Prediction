setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/datasets_all_01Oct2014.RData')
ID <- as.data.frame(test[,1])
names(ID)<-'PIDN'
index_pH <- createDataPartition(train_pH$pH, p=0.75, list = F)
train_pH_1 <- train_pH[index_pH,]
train_pH_2 <- train_pH[-index_pH,]

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

tc <- tune.control(nrepeat=5,repeat.aggregate=min, sampling='cross',sampling.aggregate=min,
                   sampling.dispersion=sd,cross=10,best.model=T,
                   performances=T,error.fun=NULL)
gamma = 1/length(names(train_pH_1[,-1]))
fit_pH_svmL_tune <- tune(svm, pH~., data=train_pH_1, ranges=list(gamma=2^(-3:1),cost=2^(2:10)),
                        tunecontrol = tc)
best.tune()

summary(fit_P_svmL_tune)
png('svm_tune_Ca.png')
plot(fit_P_svmL_tune)
dev.off()

Ca <- predict(fit_P_svmL_tune$best.model, train_Ca)
rmse(as.vector(Ca), train_Ca$Ca)

fit_P_svmL_tune$best.parameters
fit_P_svmL_tune$best.performance
fit_P_svmL_tune$performances
fit_P_svmL_tune$train.ind
fit_P_svmL_tune$best.model


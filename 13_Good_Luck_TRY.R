setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/datasets_all_01Oct2014.RData')
ID <- as.data.frame(test[,1])
names(ID)<-'PIDN'
index_P <- createDataPartition(train_P$P, p=0.75, list = F)
train_P_1 <- train_P[index_P,]
train_P_2 <- train_P[-index_P,]

### Model preProcess ###
set.seed(888)
# Grid <- expand.grid(C=c(8,16,32,64,128),sigma=c(0.0118)) 
fitControl <- trainControl(method="adaptive_cv",number=10,
                           repeats=5, summaryFunction = defaultSummary,
                           returnResamp = "all",
                           adaptive=list(min=10,alpha=.05,method='BT',complete=T))
y <- train_P_1$P;
x <- train_P_1[,-c(1,3580)];
# x$Depth <- as.numeric(x$Depth)
# enet - elasticnet (fraction, lambda)
fit_P_enet_2 <- train(x,y, method='enet',trControl = fitControl,
                   preProc = c('center', 'scale'),tuneLength=12,# tuneGrid = Grid,
                   verbose=T,metric='RMSE')
predict.glmnet()
#############################################################################################
### log transformation ###
range(train_P[,-3580])
plot(train_P$P, train_P[,3000])

### Model evaluation ### 
trellis.par.set(caretTheme())
plot(fit_Ca_svm)
P <- predict(fit_P_svm, train_P_1) # Prediction
P <- 10^P - 0.5
rmse(P, train_P_1$P)
svmImp_P <- varImp(fit_P_svm, scale = FALSE) # varImp
svmImp_P; plot(svmImp_P)

### svm ###
require(e1071)
fit <- svm(train_P_1[,-1],train_P_1$P, scale=F,cost=10000)
P <- predict(fit, train_P_2[,-1])
rmse(P, train_P_2$P)

### transformation ### 
range(train_P$P)
shapiro.test(log10(train_P$P+0.5))[1]
apply(train_P[,-3580], 2, shapiro.test)
plot(density(log10(train_P$P+0.5)))
yeo.johnson(y, lambda, derivative = 0, 
            epsilon = sqrt(.Machine$double.eps), inverse = FALSE)
y_n <- yeo.johnson(y, lambda=1, inverse = T)
# Box-Cox/Yeo-Johnson transformation, centering, scaling,
# range, imputation, PCA, ICA then spatial sign.
install.packages('VGAM')
require(VGAM)
y <- train_P$P
ltry <- c(0, 0.5, 1, 1.5, 2)  # Try these values of lambda
lltry <- length(ltry)
psi <- matrix(as.numeric(NA), length(y), lltry)
for (ii in 1:lltry)
    psi[, ii] <- yeo.johnson(y, lambda = ltry[ii])
## Not run:
png('yeo_johnson_P.png')
matplot(y, psi, type = "l", lwd = 2, lty = 1:lltry,
        ylab = "Yeo-Johnson transformation", col = 1:lltry, las = 1,
        main = "Yeo-Johnson transformation with some values of lambda")
abline(v = 0, h = 0)
legend(x = 1, y = -0.5, lty = 1:lltry, legend = as.character(ltry),
       lwd = 2, col = 1:lltry) ## End(Not run)
dev.off()


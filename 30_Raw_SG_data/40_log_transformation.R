# P <- c(-1.1,.0002,-.000003,.74,5.23,-1.214,-.1235,-12,-5,12,-2)
# log_P <- sign(P)*log10(abs(P) + 1 )
# P2 <- sign(log_P)*(10^(abs(log_P))-1)
# P;log_P;P2
# identical(P,P2)
# sum(P-P2)
# plot(log_P)

#################
### Functions ###
#################
log_trans <- function(P) {
    sign(P)*log10(abs(P) + 1 )    
}
raw_trans <- function(log_P){
    sign(log_P)*(10^(abs(log_P))-1)    
}

############
### Test ###
############
setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction')
load('data/Savitzky-Golay-Data.RData')
# load('data/88_data.RData')
require(caret); require(hydroGOF); require(parcor); require(prospectr)

### P ###
test_P <- test_SG
test_P$Depth <- ifelse(test_P$Depth == 'Topsoil',1,0)
train_P <- train_SG[,-c(1,2,5,4)] #,3559:3574
train_P$Depth <- ifelse(train_P$Depth == 'Topsoil',1,0)
### P_transformation ###
    train_P$P_log <- log_trans(train_P$P)
    raw_P <- raw_trans(train_P$P_log)
    plot(density(train_P$P_log))
    sum(train_P$P-raw_P)
index_P <- createDataPartition(train_P$P_log, p=.9, list=F)
train_P2 <- train_P[index_P,]
test_P2 <- train_P[-index_P,]

### set.seeds ###
set.seed(888)
seeds <- vector(mode = "list", length = 121)
for(i in 1:120) seeds[[i]] <- sample.int(1000, 21)
seeds[[121]] <- sample.int(1000, 1)
### Model prepare ###
fitControl <- trainControl(method="adaptive_cv", number=12, repeats=10,
                           summaryFunction = defaultSummary,
                           returnResamp = "all", selectionFunction = "best",
                           adaptive=list(min=12,alpha=.05,method='gls',complete=F),seeds=seeds)
### Model_P ###
fit_P <- train(P_log~.,data=train_P[,-1], method='svmRadial',trControl = fitControl,
               tuneLength=17,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
fit_P_2 <- train(P_log~.,data=train_P2[,-1], method='svmRadial',trControl = fitControl,
                 tuneLength=16,verbose=T,metric='RMSE',preProc = c('center', 'scale'))
### Pred_P ###
P_log <- predict(fit_P, test_P)
P2_log <- predict(fit_P_2, test_P2)
rmse(P2_log, test_P2$P_log)
P <- raw_trans(P_log)
rmse(P, train_P$P)

submit <- read.csv('submission_new/11OCT_2.csv', sep=',')
head(submit$P); head(P)
submit$P <- P
write.csv(submit, 'submission_new/P_Model_15_OCT.csv', row.names=F)

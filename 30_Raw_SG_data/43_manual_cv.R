setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
load('data/Savitzky-Golay-Data.RData')
load('data/Savitzky-Golay-Data-Filtered.RData')
library(caret);library(e1071);require(hydroGOF); require(parcor); require(prospectr)
set.seed(888);
trainingdata <- train_SG;
testdata <- test_SG;
soil_properties <- c("Ca", "P", "pH", "SOC", "Sand");
train = trainingdata;
test = testdata;
train$Depth <- with ( train, ifelse ( ( Depth == 'Subsoil' ), 0 , 1 ) );
test$Depth <- with ( test, ifelse ( ( Depth == 'Subsoil' ), 0 , 1 ) );
pred_cols = trainingdata[, soil_properties];

#now just change the p_var to some of the soil properties
p_var = "Ca";
folds = createFolds(pred_cols[,p_var],k=10);
fold_rmse = rep(0,10);
avg_rmse = 0;
for(i in 1:length(folds)){
    smpl = folds[[i]];
    g_train = train[-smpl,];
    g_test = train[smpl,];
    g_y = pred_cols[-smpl,p_var];
    g_y_test = pred_cols[smpl,p_var];
    m2 = svm(x=as.matrix(g_train),y=g_y,scale=T,kernel="linear",cost=50);
    m2.pred = predict(m2,newdata=g_test,type="response");
    fold_rmse[i]=rmse(m2.pred, g_y_test);
}
mean(fold_rmse);
sd(fold_rmse);

fit <- svm(x=as.matrix(train[,-c(1:5)]),y=pred_cols$Ca,scale=T,kernel="linear",cost=50)
# Ca 36 Radial noScale (0.048/0.009)
# Ca 36 Radial Scale (0.266/0.097)
# Ca 36 Linear noScale (0.0397/0.003) # Ca 16 Linear noScale (0.0395/0.004)
# Ca 36 Linear Scale (0.167/0.051) # Ca 16 Linear Scale (0.1806/0.071)

# P 36 Radial noScale (0.079/0.008)
# P 36 Radial Scale (0.594/0.251)
# P 36 Linear noScale (0.071/0.005)
# P 36 Linear Scale (0.122/0.014)

# pH 36 Radial noScale (0.052/0.004)
# pH 36 Radial Scale (0.304/0.039)
# pH 36 Linear noScale (0.043/0.003)
# pH 36 Linear Scale (0.116/0.010)

# SOC 36 Radial noScale (0.063/0.007)
# SOC 36 Radial Scale (0.372/0.188)
# SOC 36 Linear noScale (0.060/0.004)
# SOC 36 Linear Scale (0.161/0.031)

# Sand 36 Radial noScale (0./0.)
# Sand 36 Radial Scale (0.311/0.052)
# Sand 36 Linear noScale (0./0.)
# Sand 36 Linear Scale (0./0.)

Ca <- predict(m2, test[,-1])
### SAVE file ###
submit <- read.csv('submission_new/11OCT_2.csv', sep=',')
head(submit); head(Sand); head(pH); head(Ca); head(SOC); head(P)
submit$Sand <- Sand
submit$Ca <- Ca
submit$P <- P
submit$pH <- pH
submit$SOC <- SOC
write.csv(submit, 'submission_new/2014101703_Savitzky-Golay_Ca.csv', row.names=F)
save(fit_Sand,fit_pH,fit_Ca,fit_P,fit_SOC, file='models/2014101703_Savitzky-Golay.RData')
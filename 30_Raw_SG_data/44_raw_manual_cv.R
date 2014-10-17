library(caret);
library(e1071);require(hydroGOF); require(parcor); require(prospectr)
set.seed(25);
trainingdata <- read.csv("data/training.csv");
testdata <- read.csv("data/sorted_test.csv");
soil_properties <- c("Ca", "P", "pH", "SOC", "Sand");
trainingdata = trainingdata[,-c(2656:2670)];
testdata = testdata[,-c(2656:2670)];
train = trainingdata[,2:(ncol(trainingdata)-5)];
test = testdata;
train$Depth <- with ( train, ifelse ( ( Depth == 'Subsoil' ), 0 , 1 ) );
test$Depth <- with ( test, ifelse ( ( Depth == 'Subsoil' ), 0 , 1 ) );
pred_cols = trainingdata[, soil_properties];
#now just change the p_var to some of the soil properties
p_var = "Ca";
folds = createFolds(pred_cols[,p_var],k=10);
fold_rmse = rep(0,5);
avg_rmse = 0;
for(i in 1:length(folds)){
    smpl = folds[[i]];
    g_train = train[-smpl,];
    g_test = train[smpl,];
    g_y = pred_cols[-smpl,p_var];
    g_y_test = pred_cols[smpl,p_var];
    m2 = svm(x=as.matrix(g_train),y=g_y,scale=T,kernel="linear",cost=32);
    m2.pred = predict(m2,newdata=g_test,type="response");
    fold_rmse[i]=rmse(m2.pred, g_y_test);
}
mean(fold_rmse);
sd(fold_rmse);

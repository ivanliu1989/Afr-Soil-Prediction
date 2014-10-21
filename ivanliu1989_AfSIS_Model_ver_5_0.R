#######################
## Environment Setup ##
#######################
setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction')
setwd('H:\\Machine Learning\\Afr-Soil-Prediction')
setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
rm(list=ls(all=TRUE));gc(reset=TRUE);par(mfrow=c(1,1))
# require(randomForest);require(gbm);require(extraTrees);
require(data.table);require(bit64);require(foreach);require(reshape)
require(caret); require(hydroGOF); require(parcor); require(prospectr)

########################
## Load Data Function ##
########################
load_data <- function(method="SavitzkyGolay", derivative=1, windows=11, poly=3){
    
    df_train <- as.data.frame(fread("./data/training.csv"))
    df_test <- as.data.frame(fread("./data/sorted_test.csv"))
    df_test_PIDN <- df_test$PIDN
    location <- c('BSAN', 'BSAS', 'BSAV', 'CTI', 'ELEV', 'EVI', 'LSTD', 'LSTN',
                  'REF1', 'REF2', 'REF3', 'REF7', 'RELI', 'TMAP', 'TMFI', 'Depth')
    labels <- c("Ca", "P", "pH", "SOC", "Sand")
    par(mfcol=c(1,2))
    # df_train_sorted <- sort_df(df_train, location)
    # write.csv(df_train_sorted, "./data/sorted_training.csv", row.names=F)
    # df_test_sorted <- sort_df(df_test, location)
    # write.csv(df_test_sorted, "./data/sorted_testing.csv", row.names=F)
    
    # CO2_bands <- 2656:2670
    names(df_train)[2656:2670]

    # take the Savitzky Golay to smoothe out the measurement noise
    # training data
    if(method=="SavitzkyGolay"){
        MIR_DER1_order1 <- savitzkyGolay(df_train[, 2:2655], p = poly, w = windows, m = derivative)
        MIR_DER2_order1 <- savitzkyGolay(df_train[, 2671:3579], p = poly, w = windows, m = derivative)
        X_train <- cbind(MIR_DER1_order1,
                         MIR_DER2_order1,
                         df_train[, 3580:3595])
        plot(as.matrix(cbind(MIR_DER1_order1,MIR_DER2_order1))[100,], type='l',
             main = paste('SavitzkyGolay-Train-','poly:',poly,'windows:',windows,
                          'derivative:',derivative,sep=""), col='blue')
        rm(list=c("MIR_DER1_order1","MIR_DER2_order1"))
        gc(reset=TRUE)
    }else if(method=="FirstDerivatives"){
        diff_lag <- derivative
        diff_order <- derivative
        MIR_DER1_order1 <- t(diff(t(df_train[, 2:2655]), lag=diff_lag, difference=diff_order))
        MIR_DER2_order1 <- t(diff(t(df_train[, 2671:3579]), lag=diff_lag, difference=diff_order))
        X_train <- cbind(MIR_DER1_order1,
                         MIR_DER2_order1,
                         df_train[, 3580:3595])
        plot(as.matrix(cbind(MIR_DER1_order1,MIR_DER2_order1))[100,], type='l',
             main = paste(derivative,'Derivatives-Train',sep=" "), col='blue')
        rm(list=c("MIR_DER1_order1","MIR_DER2_order1"))
        gc(reset=TRUE)
    }else{
        X_train <- df_train[, c(2:2655,2671:3595)]
        plot(as.matrix(X_train)[100,1:3563], type='l',
             main = 'Raw-Data-Train', col='blue')
    }
    X_train$PIDN <- as.character(df_train$PIDN)
    X_train$Depth <- as.factor(X_train$Depth)
    Y_train <- df_train[, labels]
    
    # testing data
    if(method=="SavitzkyGolay"){
        MIR_DER1_order1 <- savitzkyGolay(df_test[, 2:2655], p = poly, w = windows, m = derivative)
        MIR_DER2_order1 <- savitzkyGolay(df_test[, 2671:3579], p = poly, w = windows, m = derivative)
        X_test <- cbind(MIR_DER1_order1,
                        MIR_DER2_order1,
                        df_test[, 3580:3595])
        plot(as.matrix(cbind(MIR_DER1_order1,MIR_DER2_order1))[100,], type='l',
             main = paste('SavitzkyGolay-Test-','poly:',poly,'windows:',windows,
                          'derivative:',derivative,sep=""), col='red')
        rm(list=c("MIR_DER1_order1","MIR_DER2_order1"))
        gc(reset=TRUE)
    }else if(method=="FirstDerivatives"){
        diff_lag <- derivative
        diff_order <- derivative
        MIR_DER1_order1 <- t(diff(t(df_test[, 2:2655]), lag=diff_lag, difference=diff_order))
        MIR_DER2_order1 <- t(diff(t(df_test[, 2671:3579]), lag=diff_lag, difference=diff_order))
        X_test <- cbind(MIR_DER1_order1,
                        MIR_DER2_order1,
                        df_test[, 3580:3595])
        plot(as.matrix(cbind(MIR_DER1_order1,MIR_DER2_order1))[100,], type='l',
             main = paste(derivative,'Derivatives-Test',sep=" "), col='red')
        rm(list=c("MIR_DER1_order1","MIR_DER2_order1"))
        gc(reset=TRUE)
    }else{
        X_test <- df_test[, c(2:2655,2671:3595)]
        plot(as.matrix(X_test)[100,1:3563], type='l',
             main = 'Raw-Data-Test', col='red')
    }
    X_test$PIDN <- as.character(df_test$PIDN)
    X_test$Depth <- as.factor(X_test$Depth)
    
    PIDN_test <- data.frame(PIDN=X_test$PIDN)
    return(list(X_train=X_train, Y_train=Y_train,
                X_test=X_test, PIDN_test=PIDN_test))
}

##############################
## Preprocess Data Function ##
##############################
preprocess_data <- function(dfTrain, dfTest, flag=TRUE){
    
    if(flag){
        location <- c('BSAN', 'BSAS', 'BSAV', 'CTI', 'ELEV', 'EVI', 'LSTD', 'LSTN',
                      'REF1', 'REF2', 'REF3', 'REF7', 'RELI', 'TMAP', 'TMFI', 'Depth')
        df <- rbind(dfTrain, dfTest) # all datasets
        #   print(table(df$PIDN)[table(df$PIDN)==2])
        Topsoil <- subset(df, Depth == 'Topsoil') # topsoil datasets
        Subsoil <- subset(df, Depth == 'Subsoil') # subsoil datasets
        
        m_predictors <- NULL
        for(p in names(df)){
            if(substr(p, 1, 1)=="m"){
                m_predictors <- c(m_predictors, p) # spectrum columns
            }
        }
        predictors_top <- paste("t_", names(Topsoil)[names(Topsoil) %in% m_predictors], sep='') # top spectrum predictors
        names(Topsoil)[names(Topsoil) %in% m_predictors] <- predictors_top
        predictors_sub <- paste("s_", names(Subsoil)[names(Subsoil) %in% m_predictors], sep='') # sub spectrum predictors
        names(Subsoil)[names(Subsoil) %in% m_predictors]<- predictors_sub
        
        names(Subsoil)[names(Subsoil)=="PIDN"] <- "PIDN2"
        Topsoil_new <- merge(x=Topsoil, y=Subsoil[,c(predictors_sub, "PIDN2", location[location!="Depth"])],
                             all.x=TRUE, all.y=FALSE, by=location[location!="Depth"])
        names(Subsoil)[names(Subsoil)=="PIDN2"] <- "PIDN"
        names(Topsoil)[names(Topsoil)=="PIDN"] <- "PIDN2"
        #   print(nrow(Topsoil))
        #   print(nrow(Topsoil_new))
        Subsoil_new <- merge(x=Subsoil, y=Topsoil[,c(predictors_top, "PIDN2", location[location!="Depth"])],
                             all.x=TRUE, all.y=FALSE, by=location[location!="Depth"])
        #   print(nrow(Subsoil))
        #   print(nrow(Subsoil_new))
        df <- rbind(Topsoil_new, Subsoil_new)
        
        X_train <- subset(df, PIDN %in% dfTrain$PIDN)
        X_test <- subset(df, PIDN %in% dfTest$PIDN)
        
        # delete duplicate
        #   # example: 144:147, 600:603
        #   dfTrain[144:147,location]
        #   dfTrain[600:603,location]
        #   
        #     dup <- names(table(X_train$PIDN)[table(X_train$PIDN)==2])
        #     print(dup)
        #     dup <- names(table(X_test$PIDN)[table(X_train$PIDN)==2])
        #     print(dup)
        
        KEEP <- list(PIDN=c("1BdbDkli","8ujcZd8d","95JESRyu","Asng1brQ",
                            "codSJtJ8","dHeWGQVn","GhzeToUr","gizKbXmr",
                            "H5tzOxul","hbr0DKIU","jd7gBtZy","jRhvsI8y",
                            "p4tTcdxM","pzdxsXO1","RP4Jx9yy","z1viX5xU"),
                     PIDN2=c("gizKbXmr","dHeWGQVn","Asng1brQ","95JESRyu",
                             "z1viX5xU","8ujcZd8d","jRhvsI8y","1BdbDkli",
                             "RP4Jx9yy","jd7gBtZy","hbr0DKIU","GhzeToUr",
                             "pzdxsXO1","p4tTcdxM","H5tzOxul","codSJtJ8"))
        del <- NULL
        for(i in seq(1,nrow(X_train))){
            if(X_train$PIDN[i] %in% KEEP$PIDN){
                PIDN2 <- KEEP$PIDN2[which(KEEP$PIDN == X_train$PIDN[i])]
                if(PIDN2 != X_train$PIDN2[i]){
                    del <- c(del, i)
                }
            }
        }
        X_train <- X_train[-del,]
        
        # convert to the original order
        PIDN_train <- data.frame(PIDN=as.character(dfTrain$PIDN))
        X_train <- merge(x=PIDN_train, y=X_train,
                         all.x=TRUE, all.y=FALSE, by="PIDN", sort=FALSE)
        PIDN_test <- data.frame(PIDN=as.character(dfTest$PIDN))
        X_test <- merge(x=PIDN_test, y=X_test,
                        all.x=TRUE, all.y=FALSE, by="PIDN", sort=FALSE)
        #   X_train$PIDN <- as.character(X_train$PIDN)
        #   X_test$PIDN <- as.character(X_test$PIDN)
        
        # delete PIDN
        X_train <- X_train[,-which(names(X_train) == "PIDN")]
        X_test <- X_test[,-which(names(X_test) == "PIDN")]
        # delete PIDN2
        X_train <- X_train[,-which(names(X_train) == "PIDN2")]
        X_test <- X_test[,-which(names(X_test) == "PIDN2")]
    }else{
        # delete PIDN
        X_train <- X_train[,-which(names(X_train) == "PIDN")]
        X_test <- X_test[,-which(names(X_test) == "PIDN")]
    }
    return(list(X_train=X_train, X_test=X_test))
}

######################
## Model Preparison ##
######################
cv_svm <- function(X_train, Y_train, X_test, log_transform=TRUE, log_const,fit_method='svmRadial', 
                   fit_metric='RMSE', cv_repeats=10, cv_numbers=10, fit_target,cv_method="row", 
                   adaptiveMin=10, tune_Length=10, plot_it=TRUE, adaptiveMethod = 'BT'){
    
    # message
    msg <- paste('[Target:',fit_target,'],',
                 '[Log_Transform:',log_transform,'],',
                 '[Log_Const:',log_const[fit_target],'],',
                 '[Fit_Method:',fit_method,'],',
                 '[CV:',cv_repeats,'/',cv_numbers,'],',
                 '[CV_Method:',cv_method,']',
                 sep='')
    print(msg)
    
    # prepare
    location <- c('BSAN', 'BSAS', 'BSAV', 'CTI', 'ELEV', 'EVI', 'LSTD', 'LSTN',
                  'REF1', 'REF2', 'REF3', 'REF7', 'RELI', 'TMAP', 'TMFI')
    LOC_train <- apply(X_train[,location], 1, function(x)paste(x,collapse="_"))
    LOC_test <- apply(X_test[,location], 1, function(x)paste(x,collapse="_"))
    LOC_train <- as.numeric(as.factor(LOC_train))
    LOC_train_num <- length(unique(LOC_train))
    
    # apply log transform
    if(log_transform){
        Y_train2 <- log(Y_train[,fit_target] + log_const[fit_target])
    }else{
        Y_train2 <- Y_train[,fit_target]
    }
    
    # training  
    if(cv_method=="row"){
        cat("Generate cross validation samples by ROW\n")
    }else if(cv_method=="location"){
        cat("Generate cross validation samples by LOCATION\n")
    }
    
    # set.seeds
    set.seed(888)
    seeds <- vector(mode = "list", length = 101)
    for(i in 1:100) seeds[[i]] <- sample.int
    
    # Cross validation
    if(cv_method=="row"){
        # sample row index
        trainInd <- createMultiFolds(Y_train[,fit_target],k=cv_numbers, times=cv_repeats);
    }else if(cv_method=="location"){
        # sample location index
        trainInd <- createMultiFolds(LOC_train,k=cv_numbers, times=cv_repeats);
        }
    
    # fit.control
    fitControl <- trainControl(method="adaptive_cv", index = trainInd, number=cv_numbers, 
                               repeats=cv_repeats, summaryFunction = defaultSummary, 
                               returnResamp = "all", selectionFunction = "best", 
                               adaptive=list(min=adaptiveMin, alpha=.05,
                                             method=adaptiveMethod,complete=T),
                               seeds=NULL, allowParallel=TRUE)
    
    # train svm  
    fit <- train(x=X_train,y=Y_train2, method=fit_method,trControl = fitControl,
                 tuneLength=tune_Length,verbose=T,metric=fit_metric,
                 preProc = c('center', 'scale'))
    
    # make prediction
    y_pred <- predict(fit, newdata = X_train)
    y_test <- predict(fit, newdata = X_test)
    
    # invert log transform
    if(log_transform){      
        y_pred <- exp(y_pred) - log_const[fit_target]
        y_test <- exp(y_test) - log_const[fit_target]
    }
    
    # plot the OOB predictions
    if(plot_it){
        y_true_oob <- Y_train[,fit_target]
        y_pred_oob <- y_pred
        RMSE_OOB <- rmse(y_true_oob, y_pred_oob)
        par(mfrow=c(1,2))
        axisRange <- extendrange(c(y_true_oob, y_pred_oob))
        plot(y_true_oob, y_pred_oob, main=fit_target,
             xlim = axisRange, ylim = axisRange,
             xlab="observed", ylab="predicted")
        abline(0, 1, col="darkgrey", lty=2)
        res <- y_true_oob - y_pred_oob
        plot(y_pred_oob, res, xlab="predicted", ylab="residuals", main=fit_target)
        abline(h=0, col="darkgrey", lty=2)
    }
    
    # return prediction
    return(list(y_pred=y_pred, y_test=y_test, RMSE_OOB=RMSE_OOB, fit=fit))
    
}

################################## I am a #####################################################
################################ break line :)#################################################

######################
## Model Preparison ##
######################
## load original (diff) data
method<-"SavitzkyGolay"; # SavitzkyGolay / FirstDerivatives / NULL
derivative<-1
windows<-11
poly<-3
data <- load_data(method, derivative, windows, poly)
X_train <- data[["X_train"]]
Y_train <- data[["Y_train"]]
X_test <- data[["X_test"]]
PIDN_test <- data[["PIDN_test"]]
soil_properties <- c("Ca", "P", "pH", "SOC", "Sand")

## generate pair data
data <- preprocess_data(X_train, X_test, FALSE)
X_train <- data[["X_train"]]
X_test <- data[["X_test"]]
X_train$Depth <- ifelse(X_train$Depth=='Topsoil', 0, 1)
X_test$Depth <- ifelse(X_test$Depth=='Topsoil', 0, 1)
predictors <- names(X_train)
rm(list=c("data"))
gc(reset=TRUE)

########################
## Log transformation ##
########################
log_const <- c(
    -min(Y_train$Ca)+1e-3,
    -min(Y_train$P)+1e-3,
    -min(Y_train$pH)+1e-2,
    -min(Y_train$SOC)+1e-3,
    -min(Y_train$Sand)+1e-2
)
# log transform only helpful for P
log_transform <- c(TRUE, FALSE, TRUE, TRUE, TRUE)
# log_transform <- rep(TRUE, length(soil_properties))
names(log_transform) <- soil_properties
names(log_const) <- soil_properties

#########################
## Model Configuration ##
#########################
# log_transform[fit_target];log_const
fit_method <- 'svmRadial'
fit_metric <- 'RMSE' 
cv_repeats <- 10
cv_numbers <- 10
cv_method <- 'row' # row, location
adaptiveMin <- 9
tune_Length <- 16
plot_it <- TRUE
adaptiveMethod <- 'gls' # BT/gls
p_train <- c(); fit_all <-list(); p_test <- c(); RMSE_OOB <- c()

####################
## Model Training ##
####################
for (P_var in soil_properties){
    # P_var <- 'P'
    fit_target <- soil_properties[P_var]
    cat("\n-----------------------------\n")
    cat("Train for target: ", P_var, "\n", sep="")      
    cat("-----------------------------\n")
    fit_svm <- cv_svm(X_train, Y_train, X_test, log_transform=log_transform[P_var], 
                      log_const=log_const, fit_method=fit_method, fit_metric=fit_metric, 
                      cv_repeats=cv_repeats, cv_numbers=cv_numbers, fit_target = P_var, 
                      cv_method=cv_method, adaptiveMin=adaptiveMin, tune_Length=tune_Length, 
                      plot_it=plot_it, adaptiveMethod=adaptiveMethod)
    fit_all[[P_var]] <- fit_svm[['fit']]
    p_train <- cbind(p_train, fit_svm[["y_pred"]])
    p_test <- cbind(p_test,fit_svm[["y_test"]])
    RMSE_OOB <- cbind(RMSE_OOB, fit_svm[["RMSE_OOB"]])
}
p_test_df <- as.data.frame(p_test)
names(p_test_df) <- soil_properties
head(p_test_df)
submit_df <- cbind(PIDN_test, p_test_df)

p_train_df <- as.data.frame(p_train)
names(p_train_df) <- soil_properties
head(p_train_df)

RMSE_OOB_df <- as.data.frame(RMSE_OOB)
names(RMSE_OOB_df) <- soil_properties
RMSE_OOB_df

submit <- read.csv('submission_new/11OCT_2.csv', sep=',')
head(submit); head(submit_df)

rmse(submit$Ca, submit_df$Ca)
rmse(submit$P, submit_df$P)
rmse(submit$pH, submit_df$pH)
rmse(submit$SOC, submit_df$SOC)
rmse(submit$Sand, submit_df$Sand)

#####################
## Save submission ##
#####################
fileName <- paste(
    "./submission_last/",Sys.Date(),
    "_[feature_engineer_", method, "]_",
    "[fit_method_", fit_method, "]_",
    "[cv_repeats_", cv_repeats, "]_",
    "[cv_numbers_", cv_numbers, "]_",
    "[cv_method_", cv_method, "]_TFTTT",
    ".csv", sep="")
modelName <- paste(
    "./submission_last/",Sys.Date(),
    "_[feature_engineer_", method, "]_",
    "[fit_method_", fit_method, "]_",
    "[cv_repeats_", cv_repeats, "]_",
    "[cv_numbers_", cv_numbers, "]_",
    "[cv_method_", cv_method, "]_TFTTT",
    ".RData", sep="")

write.csv(submit_df, fileName, row.names=FALSE)
save('fit_all', file=modelName)


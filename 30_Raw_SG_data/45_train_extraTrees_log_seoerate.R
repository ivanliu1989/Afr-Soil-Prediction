setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction')

rm(list=ls(all=TRUE))
gc(reset=TRUE)
par(mfrow=c(1,1))

require(randomForest);require(gbm);require(extraTrees);require(data.table)
require(bit64);require(foreach);require(reshape)
require(caret); require(hydroGOF); require(parcor); require(prospectr)

load_data <- function(diff=1){
    dfTrain <- as.data.frame(fread("./data/training.csv"))
    dfTest <- as.data.frame(fread("./data/sorted_test.csv"))
    PIDN <- dfTest$PIDN
    location <- c('BSAN', 'BSAS', 'BSAV', 'CTI', 'ELEV', 'EVI', 'LSTD', 'LSTN',
                  'REF1', 'REF2', 'REF3', 'REF7', 'RELI', 'TMAP', 'TMFI', 'Depth')
    soil_properties <- c("Ca", "P", "pH", "SOC", "Sand")
    
    # dfTrain_sorted <- sort_df(dfTrain, location)
    # write.csv(dfTrain_sorted, "./data/sorted_training.csv", row.names=F)
    # dfTest_sorted <- sort_df(dfTest, location)
    # write.csv(dfTest_sorted, "./data/sorted_testing.csv", row.names=F)
    
    # CO2_bands <- 2656:2670
    names(dfTrain)[2656:2670]
    
    
    # take the first derivatives to smoothe out the measurement noise
    # training data
    if(diff==1){
        diff_lag <- 1
        diff_order <- 1
        MIR_DER1_order1 <- t(diff(t(dfTrain[, 2:2655]), lag=diff_lag, difference=diff_order))
        MIR_DER2_order1 <- t(diff(t(dfTrain[, 2671:3579]), lag=diff_lag, difference=diff_order))
        X_train <- cbind(MIR_DER1_order1,
                         MIR_DER2_order1,
                         dfTrain[, 3580:3595])
        rm(list=c("MIR_DER1_order1","MIR_DER2_order1"))
        gc(reset=TRUE)
    }else{
        X_train <- dfTrain[, c(2:2655,2671:3595)]
    }
    
    X_train$PIDN <- as.character(dfTrain$PIDN)
    X_train$Depth <- as.factor(X_train$Depth)
    Y_train <- dfTrain[, soil_properties]
    
    
    # testing data
    if(diff==1){
        diff_lag <- 1
        diff_order <- 1
        MIR_DER1_order1 <- t(diff(t(dfTest[, 2:2655]), lag=diff_lag, difference=diff_order))
        MIR_DER2_order1 <- t(diff(t(dfTest[, 2671:3579]), lag=diff_lag, difference=diff_order))
        X_test <- cbind(MIR_DER1_order1,
                        MIR_DER2_order1,
                        dfTest[, 3580:3595])
        rm(list=c("MIR_DER1_order1","MIR_DER2_order1"))
        gc(reset=TRUE)
    }else{
        X_test <- dfTest[, c(2:2655,2671:3595)]
    }
    X_test$PIDN <- as.character(dfTest$PIDN)
    X_test$Depth <- as.factor(X_test$Depth)
    
    PIDN_test <- data.frame(PIDN=X_test$PIDN)
    return(list(X_train=X_train, Y_train=Y_train,
                X_test=X_test, PIDN_test=PIDN_test))
}

get_pair_data <- function(dfTrain, dfTest, flag=TRUE){
    if(flag){
        location <- c('BSAN', 'BSAS', 'BSAV', 'CTI', 'ELEV', 'EVI', 'LSTD', 'LSTN',
                      'REF1', 'REF2', 'REF3', 'REF7', 'RELI', 'TMAP', 'TMFI', 'Depth')
        df <- rbind(dfTrain, dfTest)
        #   print(table(df$PIDN)[table(df$PIDN)==2])
        Topsoil <- subset(df, Depth == 'Topsoil')
        Subsoil <- subset(df, Depth == 'Subsoil')
        
        m_predictors <- NULL
        for(p in names(df)){
            if(substr(p, 1, 1)=="m"){
                m_predictors <- c(m_predictors, p)
            }
        }
        predictors_top <- paste("t_", names(Topsoil)[names(Topsoil) %in% m_predictors], sep='')
        names(Topsoil)[names(Topsoil) %in% m_predictors] <- predictors_top
        predictors_sub <- paste("s_", names(Subsoil)[names(Subsoil) %in% m_predictors], sep='')
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

#### This function trains bagging gbm using data sampled with/without replacement
bagging_gbm <- function(X_train, Y_train, X_test,
                        model_params, which_model, log_transform, log_const,
                        bagging_iterations=10, bootstrap_method="row", bootstrap_replace=TRUE,
                        bootstrap_ratio=1.0, feat_ratio=1.0, seed=1234, plot.it=TRUE){
    
    location <- c('BSAN', 'BSAS', 'BSAV', 'CTI', 'ELEV', 'EVI', 'LSTD', 'LSTN',
                  'REF1', 'REF2', 'REF3', 'REF7', 'RELI', 'TMAP', 'TMFI')
    LOC_train <- apply(X_train[,location], 1, function(x)paste(x,collapse="_"))
    LOC_test <- apply(X_test[,location], 1, function(x)paste(x,collapse="_"))
    LOC_train <- as.numeric(as.factor(LOC_train))
    LOC_train_num <- length(unique(LOC_train))
    
    # number of training data
    numTrain <- nrow(X_train)
    numFeat <- ncol(X_train)
    Y_pred <- matrix(0, nrow(X_train), bagging_iterations)
    Y_test <- matrix(0, nrow(X_test), bagging_iterations)
    
    # apply log transform
    if(log_transform){
        Y_train2 <- log(Y_train + log_const)
    }else{
        Y_train2 <- Y_train
    }
    
    # training  
    if(bootstrap_method=="row"){
        cat("Generate bootstrap samples by ROW\n")
    }else if(bootstrap_method=="location"){
        cat("Generate bootstrap samples by LOCATION\n")
    }
    cat("OOB of RMSE:\n")
    RMSE_OOB <- rep(0, bagging_iterations)
    for(i in seq(1, bagging_iterations)){
        set.seed(seed + i*1000)
        
        # sample data with replacement
        if(bootstrap_ratio==1.0 && bootstrap_replace==FALSE){
            trainInd <- seq(1, numTrain)
        }else{
            if(bootstrap_method=="row"){
                # sample row index
                trainInd <- sample(numTrain, size=floor(numTrain*bootstrap_ratio),
                                   replace=bootstrap_replace) 
                # indices for OOB samples
                oobInd <- seq(1,numTrain)[!seq(1,numTrain) %in% trainInd]
            }else if(bootstrap_method=="location"){
                # sample location index
                trainLocInd <- sample(LOC_train_num, size=floor(LOC_train_num*bootstrap_ratio),
                                      replace=bootstrap_replace)
                # indices for OOB locations
                oobLocInd <- seq(1,LOC_train_num)[!(seq(1,LOC_train_num) %in% trainLocInd)]
                
                # get the row index corresponding to the location
                trainInd <- NULL
                for(locInd in trainLocInd){
                    trainInd <- c(trainInd, which(LOC_train == locInd))
                }
                # indices for OOB samples
                oobInd <- seq(1,numTrain)[!seq(1,numTrain) %in% trainInd]
            }      
        }
        
        # sample features as well (without replacement as in Random Forest)
        if(feat_ratio==1.0){
            featInd <- seq(1, numFeat)
        }else{
            featInd <- sample(numFeat, size=floor(numFeat*feat_ratio), replace=FALSE)
        }
        
        set.seed(seed + i*1000)
        if(which_model == "gbm"){
            # train gbm    
            reg_model <- gbm.fit(
                x = X_train[trainInd,featInd],
                y = Y_train2[trainInd],
                distribution = model_params$distribution,
                n.trees = model_params$n.trees,
                interaction.depth = model_params$interaction.depth,
                n.minobsinnode = model_params$n.minobsinnode,
                shrinkage = model_params$shrinkage,
                bag.fraction = model_params$bag.fraction,
                verbose=FALSE
            )
            # make prediction
            best.n.trees <- gbm.perf(reg_model, method='OOB', plot.it=FALSE)
            y_pred <- predict(reg_model, newdata = X_train[,featInd], 
                              n.trees = best.n.trees, type='response')
            y_test <- predict(reg_model, newdata = X_test[,featInd],
                              n.trees = best.n.trees, type='response')
        }else if(which_model == "extraTrees"){
            # train extraTrees
            reg_model <- extraTrees(
                x = X_train[trainInd,featInd],
                y = Y_train2[trainInd],
                ntree = model_params$ntree,
                numRandomCuts = model_params$numRandomCuts,
                evenCuts = model_params$evenCuts,
                quantile = model_params$quantile
            )
            # make prediction
            y_pred <- predict(reg_model, newdata = X_train[,featInd])
            y_test <- predict(reg_model, newdata = X_test[,featInd])
        }
        
        # invert log transform
        if(log_transform){      
            y_pred <- exp(y_pred) - log_const
            y_test <- exp(y_test) - log_const
        }
        Y_pred[,i] <- y_pred
        Y_test[,i] <- y_test
        
        # plot the OOB predictions
        if(!(bootstrap_ratio==1.0 && bootstrap_replace==FALSE)){
            y_true_oob <- Y_train[oobInd]
            y_pred_oob <- y_pred[oobInd]
            RMSE_OOB[i] <- RMSE(y_true_oob, y_pred_oob)
            if(bootstrap_method=="row"){
                cat(i, ": ", round(RMSE_OOB[i],5),
                    " (#sample: ", length(oobInd), "/", numTrain, ")\n", sep="")
            }else if(bootstrap_method=="location"){
                cat(i, ": ", round(RMSE_OOB[i],5),
                    " (#location: ", length(oobLocInd), "/", LOC_train_num, 
                    ", #sample: ", length(oobInd), "/", numTrain, ")\n", sep="")
            }      
            
            if(plot.it){
                par(mfrow=c(1,2))
                axisRange <- extendrange(c(y_true_oob, y_pred_oob))
                plot(y_true_oob, y_pred_oob,
                     xlim = axisRange, ylim = axisRange,
                     xlab="observed", ylab="predicted")
                abline(0, 1, col="darkgrey", lty=2)
                res <- y_true_oob - y_pred_oob
                plot(y_pred_oob, res,
                     xlab="predicted", ylab="residuals")
                abline(h=0, col="darkgrey", lty=2)
            }
        }
    }
    # average prediction
    Y_pred <- rowMeans(Y_pred)
    Y_test <- rowMeans(Y_test)
    if(!(bootstrap_ratio==1.0 && bootstrap_replace==FALSE)){
        cat("Overall OOB of RMSE:\nMean = ", round(mean(RMSE_OOB),5),
            " Std = ", round(sd(RMSE_OOB),5), "\n", sep="")
    }
    return(list(Y_pred=Y_pred, Y_test=Y_test, RMSE_OOB=RMSE_OOB))
}


###############
## Load data ##
###############
## load original (diff) data
diff <- 1
data <- load_data(diff)
X_train <- data[["X_train"]]
Y_train <- data[["Y_train"]]
X_test <- data[["X_test"]]
PIDN_test <- data[["PIDN_test"]]
soil_properties <- c("Ca", "P", "pH", "SOC", "Sand")

## generate pair data
data <- get_pair_data(X_train, X_test, FALSE)
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
log_transform <- c(FALSE, TRUE, FALSE, FALSE, FALSE)
# log_transform <- rep(TRUE, length(soil_properties))
names(log_transform) <- soil_properties
names(log_const) <- soil_properties


#########################
## Model configuration ##
#########################
## general params
bagging_iterations <- 50
bootstrap_method <- "location"
bootstrap_replace <- TRUE
bootstrap_ratio <- 1.0
feat_ratio <- 1.0

which_model <- "gbm"
# which_model <- "extraTrees"
seperate <- FALSE

ntrees <- c(500)
cuts <- c(1)
for(ntree in ntrees){
    for(cut in cuts){
        ## model params
        model_params <- list(
            # params for gbm
            gbm=list(
                distribution = "gaussian",
                n.trees = ntrees,
                interaction.depth = 20,
                n.minobsinnode = 10,
                shrinkage = 0.01,
                bag.fraction = 0.7),
            # params for extraTrees
            extraTrees=list(
                ntree = ntree,
                numRandomCuts = cut,
                evenCuts = FALSE,
                quantile = FALSE)
        )
        
        #############
        ## Traning ##
        #############
        sub <- PIDN_test
        RMSE_OOB <- matrix(0, bagging_iterations, length(soil_properties))
        RMSE_OOB <- as.data.frame(RMSE_OOB)
        names(RMSE_OOB) <- soil_properties
        for(soil_property in soil_properties){
            
            cat("\n-----------------------------\n")
            cat("Train for target: ", soil_property, "\n", sep="")      
            cat("-----------------------------\n")
            if(seperate){
                p_train <- rep(0, nrow(X_train))
                p_test <- rep(0, nrow(X_test))
                for(Depth in c("Topsoil", "Subsoil")){
                    indTrain <- which(X_train$Depth == Depth)
                    indTest <- which(X_test$Depth == Depth)
                    #
                    bagging.pred <- bagging_gbm(
                        X_train=X_train[indTrain, predictors],
                        Y_train=Y_train[indTrain, soil_property],
                        X_test=X_test[indTest,],
                        log_transform=log_transform[soil_property],
                        log_const=log_const[soil_property],
                        model_params=model_params[[which_model]],
                        which_model=which_model,
                        bagging_iterations=bagging_iterations,
                        bootstrap_method=bootstrap_method,
                        bootstrap_replace=bootstrap_replace,
                        bootstrap_ratio=bootstrap_ratio,
                        feat_ratio=feat_ratio)
                    p_train[indTrain] <- bagging.pred[["Y_pred"]]
                    p_test[indTest] <- bagging.pred[["Y_test"]]
                }
            }else{
                bagging.pred <- bagging_gbm(
                    X_train=X_train[, predictors],
                    Y_train=Y_train[, soil_property],
                    X_test=X_test,
                    log_transform=log_transform[soil_property],
                    log_const=log_const[soil_property],
                    model_params=model_params[[which_model]],
                    which_model=which_model,
                    bagging_iterations=bagging_iterations,
                    bootstrap_method=bootstrap_method,
                    bootstrap_replace=bootstrap_replace,
                    bootstrap_ratio=bootstrap_ratio,
                    feat_ratio=feat_ratio)
                p_train <- bagging.pred[["Y_pred"]]
                p_test <- bagging.pred[["Y_test"]]
                RMSE_OOB[,soil_property] <- bagging.pred[["RMSE_OOB"]]
            }
            #
            sub[,soil_property] <- p_test
        }
        
        MCRMSE <- apply(RMSE_OOB, 1, mean)
        cat("MCRMSE: Mean = ", round(mean(MCRMSE),5),
            " Std = ", round(sd(MCRMSE),5), sep="")
        
        
        #####################
        ## Save submission ##
        #####################
        if(which_model == "gbm"){
            fileName <- paste(
                "./Submission/gbm/gbm_",
                "[Diff", diff, "]_",
                "[Bootstrap_", bootstrap_method, "]_",
                "[Bagging", bagging_iterations, "]_",
                "[Ntree", model_params[[which_model]]$n.trees, "]_",
                "[Depth", model_params[[which_model]]$interaction.depth, "]_",
                "[lr", model_params[[which_model]]$shrinkage, "]_",
                "[Bag",  model_params[[which_model]]$bag.fraction, "]_",
                "[MCRMSE", round(mean(MCRMSE),5), "]_",
                "[Sd", round(sd(MCRMSE),5), "]",
                ".csv", sep="")
        }else if(which_model == "extraTrees"){
            fileName <- paste(
                "./Submission/extraTrees/extraTrees_",
                "[Diff", diff, "]_",
                "[Bootstrap_", bootstrap_method, "]_",
                "[Bagging", bagging_iterations, "]_",
                "[Ntree", model_params[[which_model]]$ntree, "]_",
                "[Cut", model_params[[which_model]]$numRandomCuts, "]_",        
                "[MCRMSE", round(mean(MCRMSE),5), "]_",
                "[Sd", round(sd(MCRMSE),5), "]",
                ".csv", sep="")
        }
        write.csv(sub, fileName, row.names=FALSE)
    }
}
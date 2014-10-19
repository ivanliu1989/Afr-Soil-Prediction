#######################
## Environment Setup ##
#######################
setwd('C:\\Users\\Ivan.Liuyanfeng\\Desktop\\Data_Mining_Work_Space\\Afr-Soil-Prediction')
setwd('H:\\Machine Learning\\Afr-Soil-Prediction')
rm(list=ls(all=TRUE));gc(reset=TRUE);par(mfrow=c(1,1))
# require(randomForest);require(gbm);require(extraTrees);
require(data.table);require(bit64);require(foreach);require(reshape)
require(caret); require(hydroGOF); require(parcor); require(prospectr)

########################
## Load Data Function ##
########################
load_data <- function(SavitzkyGolay=TRUE, derivative=1, windows=11, poly=3){
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
    if(SavitzkyGolay==TRUE){
        MIR_DER1_order1 <- savitzkyGolay(df_train[, 2:2655], p = poly, w = windows, m = derivative)
        MIR_DER2_order1 <- savitzkyGolay(df_train[, 2671:3579], p = poly, w = windows, m = derivative)
        X_train <- cbind(MIR_DER1_order1,
                         MIR_DER2_order1,
                         df_train[, 3580:3595])
        plot(as.matrix(cbind(MIR_DER1_order1,MIR_DER2_order1))[100,], type='l',
             main = 'SavitzkyGolay-Train', col='blue')
        rm(list=c("MIR_DER1_order1","MIR_DER2_order1"))
        gc(reset=TRUE)
    }else{
        X_train <- df_train[, c(2:2655,2671:3595)]
    }
    X_train$PIDN <- as.character(df_train$PIDN)
    X_train$Depth <- ifelse(X_train$Depth == 'Topsoil',1,0)
    Y_train <- df_train[, labels]
    
    # testing data
    if(SavitzkyGolay==TRUE){
        MIR_DER1_order1 <- savitzkyGolay(df_test[, 2:2655], p = poly, w = windows, m = derivative)
        MIR_DER2_order1 <- savitzkyGolay(df_test[, 2671:3579], p = poly, w = windows, m = derivative)
        X_test <- cbind(MIR_DER1_order1,
                        MIR_DER2_order1,
                        df_test[, 3580:3595])
        plot(as.matrix(cbind(MIR_DER1_order1,MIR_DER2_order1))[100,], type='l',
             main = 'SavitzkyGolay-Test', col='red')
        rm(list=c("MIR_DER1_order1","MIR_DER2_order1"))
        gc(reset=TRUE)
    }else{
        X_test <- df_test[, c(2:2655,2671:3595)]
    }
    X_test$PIDN <- as.character(df_test$PIDN)
    X_test$Depth <- ifelse(X_test$Depth == 'Topsoil',1,0)
    
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
        df <- rbind(df_train, df_test) # all datasets
        #   print(table(df$PIDN)[table(df$PIDN)==2])
        Topsoil <- subset(df, Depth == 'Topsoil') # topsoil datasets
        Subsoil <- subset(df, Depth == 'Subsoil') # subsoil datasets
        
        m_predictors <- NULL
        for(p in names(df)){
            if(substr(p, 1, 1)=="m"){
                m_predictors <- c(m_predictors, p) # reflectance columns
            }
        }
        predictors_top <- paste("t_", names(Topsoil)[names(Topsoil) %in% m_predictors], sep='') # top reflectance predictors
        names(Topsoil)[names(Topsoil) %in% m_predictors] <- predictors_top
        predictors_sub <- paste("s_", names(Subsoil)[names(Subsoil) %in% m_predictors], sep='') # sub reflectance predictors
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
# bagging_gbm <- function(X_train, Y_train, X_test, log_transform, log_const,
#                         bagging_iterations=10, bootstrap_method="row", bootstrap_replace=TRUE,
#                         bootstrap_ratio=1.0, feat_ratio=1.0, seed=1234, plot.it=TRUE){}








######################
## Model Preparison ##
######################
## load original (diff) data
SavitzkyGolay<-TRUE; derivative<-1; windows<-11; poly<-3
data <- load_data(SavitzkyGolay, derivative, windows, poly)
X_train <- data[["X_train"]]
Y_train <- data[["Y_train"]]
X_test <- data[["X_test"]]
PIDN_test <- data[["PIDN_test"]]
soil_properties <- c("Ca", "P", "pH", "SOC", "Sand")

## generate pair data
data <- preprocess_data(X_train, X_test, FALSE)
X_train <- data[["X_train"]]
X_test <- data[["X_test"]]
# X_train$Depth <- ifelse(X_train$Depth=='Topsoil', 0, 1)
# X_test$Depth <- ifelse(X_test$Depth=='Topsoil', 0, 1)
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
## Model Configuration ##
#########################

setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret); require(hydroGOF); require(parcor); require(prospectr)
load('data/datasets_all_01Oct2014.RData')

x <- train_Ca[,-1]
y <- train_Ca$Ca

glmnetFuncs <- caretFuncs
glmnetFuncs$summary <-  defaultSummary
glmnetFuncs$rank <- function (object, x, y) {
    vimp <- sort(object$finalModel$beta[, 1])
    vimp <- as.data.frame(vimp)
    vimp$var <- row.names(vimp)
    vimp$'Overall' <- seq(nrow(vimp),1)
    vimp
}

MyRFEcontrol <- rfeControl(
    functions = glmnetFuncs,
    method = "boot",
    number = 25,
    rerank = FALSE,
    returnResamp = "final",
    saveDetails = FALSE,
    verbose = TRUE)

MyTrainControl=trainControl(
    method = "boot",
    number=25,
    returnResamp = "all",
    classProbs = TRUE,
    summaryFunction=defaultSummary
)

RFE <- rfe(x,y,sizes = seq(1000,3000,by=100),
           metric = "RMSE",maximize=TRUE,rfeControl = MyRFEcontrol,
           method='glmnet',
           tuneGrid = expand.grid(.alpha=0,.lambda=c(0.01,0.02)),
           trControl = MyTrainControl)

NewVars <- RFE$optVariables
RFE
plot(RFE)

FL <- as.formula(paste("Ca ~ ", paste(NewVars, collapse= "+"))) #RFE
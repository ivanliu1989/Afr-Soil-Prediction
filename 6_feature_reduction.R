####################
## pre Processing ##
####################
setwd('/Users/ivan/Work_directory/Afr-Soil-Prediction-master')
require(caret);
load('data/datasets_all_30Sep2014.RData')
load('data/SVMs_Models_30Sep2014.RData')

varImp_Ca <- varImp(fit_Ca_svm, scale = FALSE)
png('varImp_Ca.png'); plot(varImp_Ca, top=100); dev.off();
varImp_P <- varImp(fit_P_svm, scale = FALSE)
png('varImp_P.png'); plot(varImp_P, top=100); dev.off();
varImp_SOC <- varImp(fit_SOC_svm, scale = FALSE)
png('varImp_SOC.png'); plot(varImp_SOC, top=100); dev.off();
varImp_pH <- varImp(fit_pH_svm, scale = FALSE)
png('varImp_pH.png'); plot(varImp_pH, top=100); dev.off();
varImp_Sand <- varImp(fit_Sand_svm, scale = FALSE)
png('varImp_Sand.png'); plot(varImp_Sand, top=100); dev.off();

###################################
## Principal Components Analysis ##
###################################


#####################
## Factor Analysis ##
#####################


##################################
## Singular value decomposition ##
##################################
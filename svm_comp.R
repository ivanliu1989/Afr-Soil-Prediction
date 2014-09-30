#Use a support vector machine to predict iris species
library(caret)
library(caTools)

#Choose x and y
x <- iris[,c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")]
y <- iris$Species

#Pre-Compute CV folds so we can use the same ones for all models
CV_Folds <- createMultiFolds(y, k = 10, times = 5)

#Fit a Linear SVM
L_model <- train(x,y,method="svmLinear",tuneLength=5,
    trControl=trainControl(method='repeatedCV',index=CV_Folds))

#Fit a Poly SVM
P_model <- train(x,y,method="svmPoly",tuneLength=5,
    trControl=trainControl(method='repeatedCV',index=CV_Folds))

#Fit a Radial SVM
R_model <- train(x,y,method="svmRadial",tuneLength=5,
    trControl=trainControl(method='repeatedCV',index=CV_Folds))

#Compare 3 models:
resamps <- resamples(list(Linear = L_model, Poly = P_model, Radial = R_model))
summary(resamps)
bwplot(resamps, metric = "Accuracy")
densityplot(resamps, metric = "Accuracy")

#Test a model's predictive accuracy Using Area under the ROC curve
#Ideally, this should be done with a SEPERATE test set
pSpecies <- predict(L_model,x,type='prob')
colAUC(pSpecies,y,plot=TRUE)
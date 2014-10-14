# -*- coding: utf-8 -*-
"""
Created on Tue Oct 14 09:08:46 2014

@author: ivan
"""

import pandas as pd
import numpy as np
from sklearn import svm, cross_validation, preprocessing
from sklearn.svm import SVR

train = pd.read_csv('/Users/ivan/Work_directory/Afr-Soil-Prediction-master/data/train_py.csv')
test = pd.read_csv('/Users/ivan/Work_directory/Afr-Soil-Prediction-master/data/test_py.csv')
labels = train[['Ca','P','pH','SOC','Sand']].values
PIDN = test[['PIDN']].values

train.drop(['Ca', 'P', 'pH', 'SOC', 'Sand'], axis=1, inplace=True)
test.drop('PIDN', axis=1, inplace=True)

xtrain, xtest = np.array(train)[:,:3569], np.array(test)[:,:3569]
xtrain_scaled = preprocessing.scale(xtrain)
xtest_scaled = preprocessing.scale(xtest)

svr_lin = SVR(kernel='linear', C=1e4, verbose = 2)

preds = np.zeros((xtest.shape[0], 5))
for i in range(5):
    svr_lin.fit(xtrain_scaled, labels[:,i])
    preds[:,i] = svr_lin.predict(xtest_scaled).astype(float)

sample = pd.read_csv('/Users/ivan/Work_directory/Afr-Soil-Prediction-master/submission_new/2.csv')
sample['Ca'] = preds[:,0]
sample['P'] = preds[:,1]
sample['pH'] = preds[:,2]
sample['SOC'] = preds[:,3]
sample['Sand'] = preds[:,4]

sample.to_csv('/Users/ivan/Work_directory/Afr-Soil-Prediction-master/submission_new/python_14OCT2014.csv', index = False)



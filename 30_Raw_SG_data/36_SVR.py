# -*- coding: utf-8 -*-
"""
Created on Tue Oct 14 09:08:46 2014

@author: ivan
"""

import pandas as pd
import numpy as np
from sklearn import svm, cross_validation

train = pd.read_csv('/Users/ivan/Work_directory/Afr-Soil-Prediction-master/data/train_py.csv')
test = pd.read_csv('/Users/ivan/Work_directory/Afr-Soil-Prediction-master/data/test_py.csv')
labels = train[['Ca','P','pH','SOC','Sand']].values

train.drop(['Ca', 'P', 'pH', 'SOC', 'Sand'], axis=1, inplace=True)
test.drop('PIDN', axis=1, inplace=True)

xtrain, xtest = np.array(train)[:,:3568], np.array(test)[:,:3568]


sup_vec = svm.SVR(C=10000.0, verbose = 2)

preds = np.zeros((xtest.shape[0], 5))
for i in range(5):
    sup_vec.fit(xtrain, labels[:,i])
    preds[:,i] = sup_vec.predict(xtest).astype(float)

sample = pd.read_csv('/Users/ivan/Work_directory/Afr-Soil-Prediction-master/submission_new/2.csv')
sample['Ca'] = preds[:,0]
sample['P'] = preds[:,1]
sample['pH'] = preds[:,2]
sample['SOC'] = preds[:,3]
sample['Sand'] = preds[:,4]

sample.to_csv('/Users/ivan/Work_directory/Afr-Soil-Prediction-master/submission_new/python_try_27Sep2014.csv', index = False)



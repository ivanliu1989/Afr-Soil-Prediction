import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

from sklearn.svm import SVR
from sklearn.preprocessing import StandardScaler
from sklearn.datasets import load_iris
from sklearn.cross_validation import StratifiedKFold
from sklearn.grid_search import GridSearchCV

##############################################################################
# Load and prepare data set
#
# dataset for grid search
train = pd.read_csv('/Users/ivan/Work_directory/Afr-Soil-Prediction-master/data/train_py.csv')
test = pd.read_csv('/Users/ivan/Work_directory/Afr-Soil-Prediction-master/data/test_py.csv')
labels = train[['Ca','P','pH','SOC','Sand']].values
PIDN = test[['PIDN']].values

train.drop(['Ca', 'P', 'pH', 'SOC', 'Sand'], axis=1, inplace=True)
test.drop('PIDN', axis=1, inplace=True)

xtrain, xtest = np.array(train)[:,:3569], np.array(test)[:,:3569]
scaler = StandardScaler()
xtrain_scaled = scaler.fit_transform(xtrain)
xtest_scaled = scaler.fit_transform(xtest)

##############################################################################
# Train classifier
#
# For an initial search, a logarithmic grid with basis
# 10 is often helpful. Using a basis of 2, a finer
# tuning can be achieved but at a much higher cost.

C_range = 10.0 ** np.arange(-2, 4)
gamma_range = 10.0 ** np.arange(-8, 1)
kernel_opt = ('linear', 'rbf')
param_grid = dict(gamma=gamma_range, C=C_range, kernel=kernel_opt)
cv = StratifiedKFold(y=labels[:,4], n_folds=5)
grid = GridSearchCV(SVR(), param_grid=param_grid, cv=cv)
grid.fit(xtrain_scaled, labels[:,4])

print("The best classifier is: ", grid.best_estimator_)



# We extract just the scores
scores = [x[1] for x in score_dict]
scores = np.array(scores).reshape(len(C_range), len(gamma_range))

# draw heatmap of accuracy as a function of gamma and C
plt.figure(figsize=(8, 6))
plt.subplots_adjust(left=0.05, right=0.95, bottom=0.15, top=0.95)
plt.imshow(scores, interpolation='nearest', cmap=plt.cm.spectral)
plt.xlabel('gamma')
plt.ylabel('C')
plt.colorbar()
plt.xticks(np.arange(len(gamma_range)), gamma_range, rotation=45)
plt.yticks(np.arange(len(C_range)), C_range)

plt.show()
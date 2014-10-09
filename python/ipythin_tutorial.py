# -*- coding: utf-8 -*-
"""
Created on Thu Oct  9 13:05:20 2014

@author: ivan
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
%matplotlib inline

url = "http://donnees.ville.montreal.qc.ca/storage/f/2014-01-20T20%3A48%3A50.296Z/2013.csv"
df = pd.read_csv(url, index_col='Date', parse_dates=True, dayfirst=True)
df.head(2)
df.describe()
df[['Berri1', 'PierDup']].plot(figsize=(8,4), style=['-', '--']);
df.index.weekday
days = np.array(['Monday', 'Tuesday', 'Wednesday', 
                 'Thursday', 'Friday', 'Saturday', 
                 'Sunday'])
df['Weekday'] = days[df.index.weekday]
df_week = df.groupby('Weekday').sum()
df_week
df_week.ix[days].plot(lw=3, figsize=(6,4));
plt.ylim(0);

from IPython.html.widgets import interact
@interact
def plot(n=(1, 30)):
    plt.figure(figsize=(8,4));
    pd.rolling_mean(df['Berri1'], n).dropna().plot();
    plt.ylim(0, 8000);
    plt.show();


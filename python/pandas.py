import numpy as np
import scipy.stats as st
import scipy.special as sp

n = 100  # number of coin flips
h = 61  # number of heads
q = .5  # null-hypothesis of fair coin

xbar = float(h)/n
z = (xbar - q) * np.sqrt(n / (q*(1-q))); z

pval = 2 * (1 - st.norm.cdf(z)); pval

import matplotlib.pyplot as plt
%matplotlib inline

posterior = lambda n, h, q: (n+1) * st.binom(n, q).pmf(h)

n = 100
h = 61
q = np.linspace(0., 1., 1000)
d = posterior(n, h, q)

plt.figure(figsize=(5,3));
plt.plot(q, d, '-k');
plt.xlabel('q parameter');
plt.ylabel('Posterior distribution');
plt.ylim(0, d.max()+1);

import numpy as np
import sklearn as sk
import sklearn.datasets as skd
import sklearn.ensemble as ske
import matplotlib.pyplot as plt
import matplotlib as mpl
%matplotlib inline
mpl.rcParams['figure.dpi'] = mpl.rcParams['savefig.dpi'] = 300

data = skd.load_boston()
data['DESCR']
reg = ske.RandomForestRegressor()
X = data['data']
y = data['target']
reg.fit(X, y);

fet_ind = np.argsort(reg.feature_importances_)[::-1]
fet_imp = reg.feature_importances_[fet_ind]

fig = plt.figure(figsize=(2,1));
ax = plt.subplot(111);
plt.bar(np.arange(len(fet_imp)), fet_imp, width=1, lw=2);
plt.grid(False);
ax.set_xticks(np.arange(len(fet_imp))+.5);
ax.set_xticklabels(data['feature_names'][fet_ind]);
plt.xlim(0, len(fet_imp));
plt.scatter(X[:,-1], y);
plt.xlabel('LSTAT indicator');
plt.ylabel('Value of houses (k$)');
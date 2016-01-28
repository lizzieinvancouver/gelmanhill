import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import pystan as pyst
import seaborn as sns
sns.set(context='talk', style='ticks', font_scale=1.2, rc={'figure.figsize': (6.5, 5.5), 'xtick.direction': 'in', 'ytick.direction': 'in'})

shorebirdTraits = pd.read_csv('~/Documents/CSU/iDiv/BayesPGLS/shorebirdData.csv')
shorebirdVCV = np.array(pd.read_csv('~/Documents/CSU/iDiv/BayesPGLS/shorebirdVCV.csv'))
Lmat = np.ones(shorebirdVCV.shape)
Lmat[np.diag_indices(shorebirdVCV.shape[0])] = 0

stanmodel = """
data{
	int N;
	int K;
	vector[N] y;
	matrix[N, N] V;
	matrix[N, N] Lmat;
	vector[N] X;
}
transformed data{
	real Ndiv;
	matrix[N, N] Ident;

	Ndiv <- N;
	Ident <- diag_matrix(rep_vector(1, N));
}
parameters{
	real<lower=0> sigma;
	real<lower=0, upper=1> lambda;
	vector[2] B;
}
transformed parameters{
}
model{
	matrix[N,N] Vlambda;
	matrix[N,N] Vsigma;
	vector[N] yhat;
	real detV;
	real cdf;
	real logLike_PGLS;

	Vlambda <- (lambda*Lmat + Ident) .* V;
	Vsigma <- sigma^2*Vlambda;

	yhat <- B[1] + B[2]*X;


	//detV <- log_determinant(Vsigma);
	//cdf <- ((y-yhat)'*inverse(Vsigma)*(y-yhat));
	//logLike_PGLS <-  -0.5*(detV + cdf);
	//increment_log_prob(logLike_PGLS);
	
	y ~ multi_normal(yhat, Vsigma);
	
	B ~ normal(0, 1);
	sigma ~ cauchy(0, 2.5);
	lambda ~ beta(1, 1);
}
"""
compMod = pyst.StanModel(model_code=stanmodel, model_name='TEST')

stdX = (np.log(shorebirdTraits['M.Mass']) - np.log(shorebirdTraits['M.Mass']).mean()) / np.log(shorebirdTraits['M.Mass']).std()
stdY = (np.log(shorebirdTraits['Egg.Mass']) - np.log(shorebirdTraits['Egg.Mass']).mean()) / np.log(shorebirdTraits['Egg.Mass']).std()

X = np.vstack((np.ones(len(shorebirdTraits)), stdX))
# y = np.log(shorebirdTraits['Egg.Mass'])

data = {'N': len(shorebirdTraits), 'X': stdX,'K': 1, 'V': shorebirdVCV, 'Lmat': Lmat, 'y': stdY}
fit = compMod.sampling(data, iter=500)
fit.plot(['B', 'sigma', 'lambda'])
plt.show()



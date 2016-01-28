# Working on Bayes PGLS, following along here #
# https://github.com/Auerilas/BayesPGLS/blob/master/PGLS.py #
# Same as Will Pearse's code I think ... ##

library(phytools)

setwd("~/Documents/git/teaching/stan/phylogeny")
shorebirdTraits <- read.csv('input/shorebirdData.csv')

## START here: Need to update below .... ##

shorebirdVCV <- np.array(pd.read_csv('~/Documents/CSU/iDiv/BayesPGLS/shorebirdVCV.csv'))
Lmat = np.ones(shorebirdVCV.shape)
Lmat[np.diag_indices(shorebirdVCV.shape[0])] = 0

compMod = pyst.StanModel(model_code=stanmodel, model_name='TEST')

stdX = (np.log(shorebirdTraits['M.Mass']) - np.log(shorebirdTraits['M.Mass']).mean()) / np.log(shorebirdTraits['M.Mass']).std()
stdY = (np.log(shorebirdTraits['Egg.Mass']) - np.log(shorebirdTraits['Egg.Mass']).mean()) / np.log(shorebirdTraits['Egg.Mass']).std()

X = np.vstack((np.ones(len(shorebirdTraits)), stdX))
# y = np.log(shorebirdTraits['Egg.Mass'])

data = {'N': len(shorebirdTraits), 'X': 

data {
  int<lower=0> N;//number of visits? Not necessary to make lower bound for N
  int<lower=0> J;//patients (=id)
  vector[N] y;
  real<lower=0,upper=2> x[N]; // define as vector could be better

// matrix[N,K] X;
// or
// real X[N,K];
//
// vector[N] X[K];

  int id[N];
}
parameters {
  real e_a[J];
  real b;   //effect of time (=x)                       
  real mu_a;
  real<lower=0> sigma_y;
  real<lower=0> sigma_a;
}
model { // “the log posterior block”
// a ~ normal_ncp(mu_a, sigma_a);
  vector[N] ypred;
  e_a ~ normal(0,1);
  for (n in 1:N) 
    ypred[n] <- mu_a + sigma_a*e_a[id[n]] + b*x[n];
  y ~ normal(ypred, sigma_y);
}

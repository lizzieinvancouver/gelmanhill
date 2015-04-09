data {
  int<lower=0> N;//number of visits?
  int<lower=0> J;//patients (=id)
  vector[N] y;
  real<lower=0,upper=2> x[N];
  int id[N];
}
parameters {
  real a[J];
  real b;   //effect of time (=x)                       
  real mu_a;
  real<lower=0> sigma_y;
  real<lower=0> sigma_a;
}
model {
  a ~ normal(mu_a, sigma_a);            
  for (n in 1:N)
    y[n] ~ normal(a[id[n]] + b * x[n], sigma_y);
}

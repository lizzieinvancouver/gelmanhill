data {
  int<lower=1> N; # number of kids (assumed numbered 1 to N)
  int<lower=1> D; # number of data points
  
  int<lower=0,upper=100> y[D];  # CD4 percentage
  real<lower=0> time[D];        # timing of visits
  int<lower=1,upper=N> kid[D];  # ID of kids
}

parameters {
  real a[N];              # intercept, varies by kid
  real b[N];              # slope over time, varies by kid
  real<lower=0> err;      # error of the regression
  real mu_a;              # mean of kids' intercepts
  real mu_b;              # mean of kids' slopes
  real<lower=0> sigma_a;  # stdev of kids' intercepts
  real<lower=0> sigma_b;  # stdev of kids' slopes
}

transformed parameters {
  real y_hat[D];
  for (i in 1:D) {
    y_hat[i] <- a[kid[i]] + b[kid[i]] * time[i];
  }
}

model {
  # the hyper-model
  for (j in 1:N) {
    a[j] ~ normal(mu_a,sigma_a);
    b[j] ~ normal(mu_b,sigma_b);
  }

  # the base level of the model
  for (i in 1:D) {
    y[i] ~ normal(y_hat[i],err);
  }
}
  
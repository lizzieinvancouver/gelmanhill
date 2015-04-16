data {
  int<lower=0> N;//number of people in survey
  int J;//states
  vector[N] y;
  int<lower=0> x[N];
  int st[N];
}
parameters {
  real<lower=0> sigma;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  vector[J] a;
  vector[J] b;
  real mu_a;
  real mu_b;
}
transformed parameters {
  vector[N] y_hat;//i got this from the code for radon but i’m not sure why we are estimating N?

  for (i in 1:N)
    y_hat[i] <- a[st[i]] + b[st[i]] * x[i];
}
model {
  mu_a ~ normal(0, 100);//i don’t think this should be normal but what should it be?
  mu_b ~ normal(0, 100);

  a ~ normal(mu_a, sigma_a);
  b ~ normal(mu_b, sigma_b);
  y ~ normal(y_hat, sigma);
}

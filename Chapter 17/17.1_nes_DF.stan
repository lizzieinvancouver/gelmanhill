data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  int<lower=0> ideo[N];
  int state[N];
  int female[N];
  int race[N];
  int edu[N];
  int income[N];
  int age[N];

}
parameters {
  real<lower=0> sigma;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;

  vector[J] a;
  vector[J] b;
  vector[5] beta;
  real mu_a;
  real mu_b;
}
transformed parameters {
  vector[N] y_hat;

  for (i in 1:N) 
    y_hat[i] <- a[state[i]] + b[state[i]]* ideo[i] +  beta[1]*female[i] + beta[2]*race[i] + beta[3]*edu[i] + beta[4]*income[i] + beta[5]*age[i];
}

model {
  mu_a ~ normal(0, 100);
  mu_b ~ normal(0, 100);

  a ~ normal(mu_a, sigma_a);
  b ~ normal(mu_b, sigma_b);

  y ~ normal(y_hat, sigma);
}

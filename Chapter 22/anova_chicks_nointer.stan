data {
  int<lower=0> J;
  int<lower=0> N;
  int<lower=1,upper=J> n[N];
  vector[N] y;
  vector[N] freq;
  vector[N] treat;
}
parameters {
  vector[J] eta;
  real mu_a;
  real<lower=0,upper=100> sigma_a;
  real<lower=0,upper=100> sigma_y;
}

transformed parameters {
  vector[J] a; // group level predictor size n
  vector[2] beta; // individual level predictors
  vector[N] y_hat;

  a <- mu_a + sigma_a * eta;

  for (i in 1:N)
    y_hat[i] <- a[n[i]] + beta[1] * freq[i] + beta[2] * treat[i];
}
model {
  mu_a ~ normal(0, 1);

  eta ~ normal(0, 1);
  y ~ normal(y_hat, sigma_y);
}
generated quantities {
  vector[N] e_y;
  real<lower=0> s_a;
  real<lower=0> s_y;
  real<lower=0> s_b1;
  real<lower=0> s_b2;

  e_y <- y - y_hat;
  s_a <- sd(a);
  s_y <- sd(e_y);
//  s_b1 <- sd(beta[1]);
//  s_b2 <- sd(beta[2]);
  
}
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
  real<lower=0> sigma_b1;
  real<lower=0> sigma_b2;
  real<lower=0> sigma_b3;
  real<lower=0> sigma_b4;
  real<lower=0> sigma_b5;

  vector[J] a;
  vector[J] b;
  vector[J] b1;
  vector[J] b2;
  vector[J] b3;
  vector[J] b4;
  vector[J] b5;
  real mu_a;
  real mu_b;
  real mu_b1;
  real mu_b2;
  real mu_b3;
  real mu_b4;
  real mu_b5;
}
transformed parameters {
  vector[N] y_hat;

  for (i in 1:N)
    y_hat[i] <- a[state[i]] + b[state[i]] * ideo[i] + b1*female + b2*race + b3*edu + b4*income + b5*age;
}

model {
  mu_a ~ normal(0, 100);
  mu_b ~ normal(0, 100);
  mu_b1 ~ normal(0, 100);
  mu_b2 ~ normal(0, 100);
  mu_b3 ~ normal(0, 100);
  mu_b4 ~ normal(0, 100);
  mu_b5 ~ normal(0, 100);

  a ~ normal(mu_a, sigma_a);
  b ~ normal(mu_b, sigma_b);
  b1 ~ normal(mu_b1, sigma_b1);
  b2 ~ normal(mu_b2, sigma_b2);
  b3 ~ normal(mu_b3, sigma_b3);
  b4 ~ normal(mu_b4, sigma_b4);
  b5 ~ normal(mu_b5, sigma_b5);


  y ~ normal(y_hat, sigma);
}

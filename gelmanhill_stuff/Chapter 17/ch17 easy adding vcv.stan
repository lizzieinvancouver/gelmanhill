data {
  int<lower=0> N;  // # data points
  int<lower=0> J;  // # groups
  int K;           // # coefs
  vector[N] y;
  vector[N] ideo;
  int<lower=1> state[N];
}
transformed data {
  vector[N] scaled_ideo;
  vector[N] scaled_y;
  matrix[N,K] X;
  vector[J] zero;
  zero <- rep_vector (0., J);
  scaled_ideo <- (ideo - 4.)/4.;
    // Lib is -.5, Cons is +.5 (comparing ideo=2 to ideo=6)
  scaled_y <- (y - mean(y))/(2*sd(y));
  for (n in 1:N){
    X[n,1] <- 1;
    X[n,2] <- scaled_ideo[n];
  }
}
parameters {
  real<lower=0> sigma;
  vector[K] sigma_a;
  corr_matrix[K] Omega_a;
  real<lower=0> sigma_b;
  vector[K] a[J];      // a is an array of J vectors, each of length K
  vector[K] mu_a;
}
transformed parameters {
  matrix[K,K] Sigma_a; // 2 x 2 matrix
  vector[N] scaled_y_hat;
  matrix[K,2] coef; 
  for (n in 1:N)
    scaled_y_hat[n] <- X[n]*a[state[n]];
  Sigma_a <- quad_form_diag(Omega_a, sigma_a);
}
model {
  mu_a ~ normal(0, 10);
  sigma_a ~ normal(0, 10);
  Omega_a ~ lkj_corr(2);  // 2 is the df of the lkj model
  a ~ multi_normal(zero, Sigma_a);
  scaled_y ~ normal(scaled_y_hat, sigma);
}
data {
  int<lower=0> N;
  int<lower=0,upper=1> switc[N];
  vector[N] dist;
  vector[N] arsenic;
  vector[N] village;
}
transformed data {			// * are these necessary to put in 'transformed data' vs 'data' block?
  vector[N] dist100;         // rescaling
  vector[N] inter;           // interaction
  dist100 <- dist / 100.0;
  inter   <- dist100 .* arsenic;
}
parameters {
  vector[4] beta;
}
model {

for (i in 1:N) {

  switc[i] ~ bernoulli_logit(beta[1][village[i]] + beta[2][village[i]] * dist100[i] 
                             + beta[3][village[i]] * arsenic[i] + beta[4][village[i]] * inter[i]);
}
}

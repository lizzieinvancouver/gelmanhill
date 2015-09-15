data {
  int<lower=0> N;
  int<lower=0,upper=1> switc[N];
  int<lower=0> J; // added for number of villages
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
  // vector[4] beta; // doesn't work with village-level indexing?
	real sigma;
	vector[J] b1; // trying, change from real
	vector[J] b2;
	vector[J] b3;  
	vector[J] b4;
}
transformed parameters { // changed from 'model' block
	vector[N] y_hat;
	
	for (i in 1:N) {
	  y_hat[i] <- bernoulli_logit(b1[village[i]] + b2[village[i]] * dist100[i] 
                             + b3[village[i]] * arsenic[i] + b4[village[i]] * inter[i]);
	}
}

model {
	y ~ normal(y_hat, sigma)
	}
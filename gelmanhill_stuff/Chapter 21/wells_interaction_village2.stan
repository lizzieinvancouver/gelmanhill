data {
  int<lower=0> N;
  int<lower=0,upper=1> switc[N];
  int<lower=0> n_vill; // added for number of villages
  vector[N] dist;
  vector[N] arsenic;
  vector[N] village; // should we define this as a vector of integers?
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
	vector[n_vill] b_0; 
	vector[n_vill] b_dist;
	vector[n_vill] b_as;  
	vector[n_vill] b_int;
}
model { // changed from 'model' block
	vector[N] y_hat;
	
	for (i in 1:N) {
	  y_hat[i] ~ bernoulli_logit(b_0[village[i]] + b_dist[village[i]] * dist100[i] 
                             + b_as[village[i]] * arsenic[i] + b_int[village[i]] * inter[i]);
	}
}


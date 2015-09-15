data {
  int<lower=0> N;
  int<lower=0,upper=1> switc[N];
  int<lower=0> n_vill; // added for number of villages, "J"
  vector[N] dist;
  vector[N] arsenic;
  int village[N]; // should we define this as a vector of integers? This has to be int, not vector[N]
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
model { // tried as 'transformed parameters' block, what is the difference between these blocks?
	int y_hat[N];
	
	for (i in 1:N) {
	  y_hat[i] ~ bernoulli_logit(b_0[village[i]] + b_dist[village[i]]*dist100[i] );
	}

	
}


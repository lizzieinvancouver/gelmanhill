
// Stan model with no sp_site or interactions.
// Leafout day as a function of species and site of origin as modeled group level factors, and temperature, photoperiod, and chilling as unmodeled factors (experimental manipulation)

data {
  int<lower=0> N;
  int<lower=0> n_site;
  int<lower=0> n_sp;
  int<lower=1, upper=n_site> site[N];
  int<lower=1, upper=n_sp> sp[N];
  vector[N] lday;
  vector[N] warm;
  vector[N] photo;
  vector[N] chill;
}

parameters {
  vector[n_site] a;
  vector[n_sp] b_warm;
  vector[n_sp] b_photo;
  vector[n_sp] b_chill;
  
  // real mu_a;
  real mu_b_warm; // vectors of length n_sp instead of real?
  real mu_b_chill;
  real mu_b_photo;

  // real<lower=0> sigma_a;
  real<lower=0> sigma_b_warm;
  real<lower=0> sigma_b_photo;
  real<lower=0> sigma_b_chill;
  
  real<lower=0> sigma_y; 
  }


transformed parameters {
	vector[N] y_hat;
		
	for(i in 1:N){
		// different intercepts for sites. Different slopes for species, of warming, photo, and chill. No interactions here
		
		y_hat[i] <- a[site[i]] + b_warm[sp[i]] * warm[i] + b_photo[sp[i]] * photo[i] + b_chill[sp[i]] * chill[i];
		
		}

	
}

model {
	// Priors. Make them flat
	// mu_a ~ normal(0, 100);
	mu_b_warm ~ normal(0, 100);
	mu_b_photo ~ normal(0, 100);
	mu_b_chill ~ normal(0, 100);
	
	// a ~ normal(mu_a, sigma_a);	
	b_warm ~ normal(mu_b_warm, sigma_b_warm);
	b_photo ~ normal(mu_b_photo, sigma_b_photo);
	b_chill ~ normal(mu_b_chill, sigma_b_chill);

	lday ~ normal(y_hat, sigma_y);
}
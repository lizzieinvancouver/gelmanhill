// Experimental phenology analyses

// Started by aettinger@fas.harvard.edu, c

// multilevel level model for soil moisture as a function of precip treatment and temperature treatment

// Level: site, dog, year, only on INTERCEPTS

// Includes: Interactions 

data {

	int<lower=1> N;

	int<lower=1> n_sites;

	int<lower=1, upper=n_sites> sites[N];

	vector[N] y; 		// response(soil moisture)

	vector[N] temp; 	// predictor

	vector[N] precip; 	// predictor

}


transformed data { 			// 1 two-way interaction term 
	
	vector[N] inter_tp; 	// temp x precip                    
	
	inter_fp   <- temp .* precip;  

}




parameters {

  real a_0; // overall intercept, same as "beta_0" in classroom example model
  real b_temp_0; // overall temp treatment effect
  real b_precip_0; // overall precip treatment effect  
  real b_inter_tp_0; // overall temp x precip effect  
  real mu_a_sites[n_sites];
  real<lower=0> sigma_a_sites; 
  real<lower=0> sigma_y; 
  real a_sites[n_sites]; // intercept for sites
}

model {

   real yhat[N];

   for(i in 1:N){

       yhat[i] = a_sites[sites[i]] + // indexed with site
		temp[i] * precip[i] 
			}
	a_sites ~ normal(0, 12); //
	b_temp_0 ~ normal(0, 30);
	b_precip_0 ~ normal(0, 30);
	b_inter_tp_0 ~ normal(0, 30);
	mu_a_sites ~ normal(0, sigma_a_sites); // 
	sigma_a_sites ~ normal(0, 20); 
	y ~ normal(yhat, sigma_y);
}


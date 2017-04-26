

data{
    int N;
    int mois[N];
    real temp_cent[N];
    real precip_cent[N];
    int site_index[N];
    int year_index[N];
    int doy_index[N];

}

parameters{
    real Intercept;
    real beta_temp_cent;
    real beta_precip_cent;
    real INTERACTION;
    real vary_site_index[N_site_index];
    real vary_year_index[N_year_index];
    real vary_doy_index[N_doy_index];
    real<lower=0> sigma_herd_index;
}



data{ 
  int n;
  vector[n] flipper_length_cm;
}

parameters{
  real log_mu; // likelihood mean in log space
  real<lower=0> theta; // likelihood scale
}

model{
  // Priors
  log_mu ~ normal( log(50) , 0.4 );
  theta ~ exponential( 1 );
      
  // Link function
  real mu = exp( log_mu );

  // Gamma likelihood
  flipper_length_cm ~ gamma( mu / theta , 1 / theta );
}
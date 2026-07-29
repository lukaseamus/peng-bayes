data{ 
  int n;
  vector[n] flipper_length_cm;
  array[n] int species;
  int n_species;
}

parameters{
  vector[n_species] log_mu; // likelihood mean in log space
  real<lower=0> theta; // likelihood scale
}

model{
  // Priors
  log_mu ~ normal( log(20) , 0.4 );
  theta ~ exponential( 1 );
      
  // Linear model with link function
  vector[n] mu = exp( log_mu[species] );

  // Gamma likelihood
  flipper_length_cm ~ gamma( mu / theta , 1 / theta );
}
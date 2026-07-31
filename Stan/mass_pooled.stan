data{
  int n;
  vector[n] flipper_length_cm;
  vector[n] body_mass_g;
  array[n] int species;
  int n_species;
}

parameters{
  // Hyperparameters
  real log_alpha_mu;
  real<lower=0> log_alpha_sigma;
  
  // Parameters
  vector[n_species] log_alpha_z; // coefficient z-score
  real<lower=0> sigma; // likelihood sd
}

transformed parameters{
  // Convert z-scores
  vector[n_species] log_alpha = log_alpha_z * log_alpha_sigma + log_alpha_mu;
}

model{
  // Hyperpriors
  log_alpha_mu ~ normal( log(1.5) , 0.4 );
  log_alpha_sigma ~ normal( 0 , 0.1 )T[0,];
  
  // Priors
  log_alpha_z ~ normal( 0 , 1 );
  sigma ~ exponential( 1 ); 

  // Model
  vector[n] alpha = exp( log_alpha[species] );
  vector[n] mu = alpha .* flipper_length_cm^3;

  // Likelihood
  body_mass_g ~ normal( mu , sigma );
}
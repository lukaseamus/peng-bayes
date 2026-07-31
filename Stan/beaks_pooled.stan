data{ 
  int n;
  vector[n] beak_length;
  vector[n] beak_height;
  array[n] int species;
  int n_species;
}

parameters{
  // Hyperparameters
  real alpha_mu; // intercept
  real<lower=0> alpha_sigma;
  real beta_mu; // slope
  real<lower=0> beta_sigma;
  
  // Parameters
  vector[n_species] alpha; // intercept
  vector[n_species] beta; // slope
  real<lower=0> sigma; // likelihood sd
}

model{
  // Hyperpriors
  alpha_mu ~ normal( 2 , 1 );
  alpha_sigma ~ normal( 0 , 1 )T[0,]; // half-normal
  
  beta_mu ~ normal( 0 , 1 );
  beta_sigma ~ normal( 0 , 1 )T[0,];
  
  // Priors
  alpha ~ normal( alpha_mu , alpha_sigma );
  beta ~ normal( beta_mu , beta_sigma );
  sigma ~ exponential( 1 );
  
  // Linear model
  vector[n] mu = alpha[species] + beta[species] .* beak_length;

  // Normal likelihood
  beak_height ~ normal( mu , sigma );
}
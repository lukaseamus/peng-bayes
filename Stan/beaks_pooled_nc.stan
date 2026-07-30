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
  vector[n_species] alpha_z; // intercept z-score
  vector[n_species] beta_z; // slope z-score
  real<lower=0> sigma; // likelihood sd
}

transformed parameters{
  // Convert z-scores
  vector[n_species] alpha = alpha_z * alpha_sigma + alpha_mu;
  vector[n_species] beta = beta_z * beta_sigma + beta_mu;
}

model{
  // Hyperpriors
  alpha_mu ~ normal( 2 , 1 );
  alpha_sigma ~ normal( 0 , 1 )T[0,]; // half-normal
  
  beta_mu ~ normal( 0 , 1 );
  beta_sigma ~ normal( 0 , 1 )T[0,];
  
  // Priors
  alpha_z ~ normal( 0 , 1 ); // standard normal for z-scores
  beta_z ~ normal( 0 , 1 );
  sigma ~ exponential( 1 );
      
  // Linear model
  vector[n] mu = alpha[species] + beta[species] .* beak_length;

  // Normal likelihood
  beak_height ~ normal( mu , sigma );
}
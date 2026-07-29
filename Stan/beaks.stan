data{ 
  int n;
  vector[n] beak_length;
  vector[n] beak_height;
  array[n] int species;
  int n_species;
}

parameters{
  vector[n_species] alpha; // intercept
  vector[n_species] beta; // slope
  real<lower=0> sigma; // likelihood sd
}

model{
  // Priors
  alpha ~ normal( 0 , 1 );
  beta ~ normal( 0 , 1 );
  sigma ~ exponential( 1 );
      
  // Linear model
  vector[n] mu = alpha[species] + beta[species] .* beak_length;

  // Normal likelihood
  beak_height ~ normal( mu , sigma );
}
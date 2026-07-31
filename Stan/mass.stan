data{
  int n;
  vector[n] flipper_length_cm;
  vector[n] body_mass_g;
  array[n] int species;
  int n_species;
}

parameters{
  vector<lower=0>[n_species] alpha; // constant
  vector<lower=1>[n_species] beta; // exponent
  real<lower=0> sigma; // likelihood sd
}

model{
  // Priors
  alpha ~ gamma( square(1.5) / square(0.5) , 1.5 / square(0.5) );
  beta ~ normal( 3 , 0.5 )T[1,];
  sigma ~ exponential( 1 ); 

  // Model
  vector[n] mu = alpha[species] .* flipper_length_cm ^ beta[species];

  // Likelihood
  body_mass_g ~ normal( mu , sigma );
}
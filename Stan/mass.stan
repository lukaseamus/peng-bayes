data{
  int N;
  vector[N] flipper_length_cm;
  vector[N] body_mass_g;
  array[N] int species;
  int N_species;
}
parameters{
  // Species parameters
  vector<lower=0>[N_species] k;
  vector<lower=1>[N_species] n;

  // Likelihood uncertainty
  real<lower=0> sigma;
}

model{
  // Species priors
  k ~ gamma( 2^2 / 1^2 , 2 / 1^2 );
  n ~ normal( 3 , 0.5 ) T[1, ];

  // Likelihood uncertainty prior
  // standard exponential priors are the default for uncertainties
  sigma ~ exponential( 1 ); 

  // Model
  vector[N] mu;
  for ( i in 1:N ) {
    mu[i] = k[species[i]] * flipper_length_cm[i] ^ n[species[i]];
  }

  // Likelihood
  body_mass_g ~ normal( mu , sigma );
}
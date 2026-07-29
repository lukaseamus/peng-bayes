# This script depends on the data being loaded and wrangled in peng-bayes.R.
prior_linear_stan <- "
parameters{
  real alpha;
  real beta;
}

model{
  alpha ~ normal( 4e3, 1e3 );
  beta ~ normal( 0 , 100 );
}
"
require(tidyverse)
require(cmdstanr)
prior_linear_mod <- prior_linear_stan %>%
  write_stan_file() %>%
  cmdstan_model()

prior_linear_samples <- prior_linear_mod$sample(
  data = list(),
  seed = 100,
  chains = 8,
  parallel_chains = parallel::detectCores(),
  iter_warmup = 1e4,
  iter_sampling = 1e4)

require(magrittr)
prior_linear_samples$draws(format = "df") %>%
  as_tibble() %>%
  slice_sample(n = 1e3) %>%
  expand_grid(flipper_length_cm = seq(10, 30, length.out = 100)) %>%
  mutate(body_mass_g = alpha + beta * flipper_length_cm) %>%
  ggplot(aes(flipper_length_cm, body_mass_g, group = .draw)) +
    geom_line(alpha = 0.05) +
    annotate("rect", xmin = penguins_cc %$% min(flipper_length_cm),
             xmax = penguins_cc %$% max(flipper_length_cm),
             ymin = penguins_cc %$% min(body_mass_g),
             ymax = penguins_cc %$% max(body_mass_g),
             colour = "black", fill = NA) +
    theme_minimal() +
    theme(panel.grid = element_blank())

penguins_linear_stan <- "
data{
  int N;
  vector[N] flipper_length_cm;
  vector[N] body_mass_g;
  array[N] int species;
  int N_species;
}
parameters{
  // Species parameters
  vector[N_species] alpha;
  vector[N_species] beta;

  // Likelihood uncertainty
  real<lower=0> sigma;
}

model{
  // Species priors
  alpha ~ normal( 4e3 , 1e3 );
  beta ~ normal( 0 , 100 );
  
  // Likelihood uncertainty prior
  sigma ~ exponential( 1 );

  // Model
  vector[N] mu;
  for ( i in 1:N ) {
    mu[i] = alpha[species[i]] + beta[species[i]] * flipper_length_cm[i];
  }

  // Likelihood
  body_mass_g ~ normal( mu , sigma );
}
"

penguins_linear_mod <- penguins_linear_stan %>%
  write_stan_file() %>%
  cmdstan_model()

require(tidybayes)
penguins_linear_samples <- penguins_linear_mod$sample(
  data = penguins_cc %>%
    select(flipper_length_cm, body_mass_g, species) %>%
    compose_data(.n_name = n_prefix("N")),
  seed = 100,
  chains = 8,
  parallel_chains = parallel::detectCores(),
  iter_warmup = 1e4,
  iter_sampling = 1e4)

require(bayesplot)
penguins_linear_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "beta[1]"))
penguins_linear_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "beta[2]")) %T>% # save plot for other script
  ggsave(filename = "penguins_linear.tiff", width = 10, height = 10, units = "cm")
penguins_linear_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[3]", "beta[3]"))
# Correlation between pairs of parameters.
# -> Try centred approach

penguins_cc %<>%
  mutate(flipper_length_cm_c = flipper_length_cm - mean(flipper_length_cm))

prior_linear_samples$draws(format = "df") %>%
  as_tibble() %>%
  slice_sample(n = 1e3) %>%
  expand_grid(flipper_length_cm_c = seq(-10, 10, length.out = 100)) %>%
  mutate(body_mass_g = alpha + beta * flipper_length_cm_c) %>%
  ggplot(aes(flipper_length_cm_c, body_mass_g, group = .draw)) +
    geom_line(alpha = 0.05) +
    annotate("rect", xmin = penguins_cc %$% min(flipper_length_cm_c),
             xmax = penguins_cc %$% max(flipper_length_cm_c),
             ymin = penguins_cc %$% min(body_mass_g),
             ymax = penguins_cc %$% max(body_mass_g),
             colour = "black", fill = NA) +
    theme_minimal() +
    theme(panel.grid = element_blank())
# We can already tell by looking at the prior simulation that centring is better given
# identical prior distributions for alpha and beta.

penguins_linear_c_stan <- "
data{
  int N;
  vector[N] flipper_length_cm_c;
  vector[N] body_mass_g;
  array[N] int species;
  int N_species;
}
parameters{
  // Species parameters
  vector[N_species] alpha;
  vector[N_species] beta;

  // Likelihood uncertainty
  real<lower=0> sigma;
}

model{
  // Species priors
  alpha ~ normal( 4e3 , 1e3 );
  beta ~ normal( 0 , 100 );
  
  // Likelihood uncertainty prior
  sigma ~ exponential( 1 );

  // Model
  vector[N] mu;
  for ( i in 1:N ) {
    mu[i] = alpha[species[i]] + beta[species[i]] * flipper_length_cm_c[i];
  }

  // Likelihood
  body_mass_g ~ normal( mu , sigma );
}
"

penguins_linear_c_mod <- penguins_linear_c_stan %>%
  write_stan_file() %>%
  cmdstan_model()

penguins_linear_c_samples <- penguins_linear_c_mod$sample(
  data = penguins_cc %>%
    select(flipper_length_cm_c, body_mass_g, species) %>%
    compose_data(.n_name = n_prefix("N")),
  seed = 100,
  chains = 8,
  parallel_chains = parallel::detectCores(),
  iter_warmup = 1e4,
  iter_sampling = 1e4)
# Model also runs faster.

penguins_linear_c_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "beta[1]"))
penguins_linear_c_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "beta[2]")) %T>% # save plot for other script
  ggsave(filename = "penguins_linear_c.tiff", width = 10, height = 10, units = "cm")
penguins_linear_c_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[3]", "beta[3]"))
# Less correlation between pairs of parameters.
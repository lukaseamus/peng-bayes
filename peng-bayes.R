# 1. Load data ####
require(palmerpenguins) # data on penguins at Palmer Station
penguins

# see also
require(lterdatasampler) # data on various cute plants and animals
knz_bison # example data
pie_crab
nwt_pikas
hbr_maples

require(tidyverse) # how could we possibly live without the tiduverse?!
penguins_cc <- penguins %>% filter(!is.na(flipper_length_mm | body_mass_g))
penguins_cc

# 2. Visualise data ####
require(cowplot) # allows plotting of images
(penguins_cc %>%
  ggplot(aes(flipper_length_mm, body_mass_g, colour = species)) +
    geom_point(shape = 16, size = 2) +
    scale_colour_manual(values = c("darkorange","purple","cyan4"),
                        guide = "none") +
    theme_minimal() +
    theme(panel.grid = element_blank())) %>%
    ggdraw() +
    draw_image("penguins.png", x = 0, y = 0.65, width = 0.5, height = 0.2983333)

# 3. Causal model ####
# We can do better than fitting a linear model. Flipper length cannot be linearly related to body mass.
# For one, both variable cannot be negative. Luckily there are no negative flippers! A linear model will 
# eventually make the mistake of predicting negative values. A basic rule of allometry is that as the 
# one-dimensional size of an organism changes, the mass changes as a power function of that dimension.
# To conceptualise this, imagine penguins as spheres (chubby penguins!). The mass or volume of a sphere
# is calculated from its height as 4/3 * pi * (h/2)^3. See that exponent. That's not linear!
# The standard two-parameter power function is f(x) = k * x ^ n. We don't need an intercept because
# penguins that have no flipper length also weigh nothing. So let's use that function.

# 4. Prior simulation ####
# We know up front that k cannot be negative because this would lead to a negative predictions.
# We also know that n has to be greater than 1, since n < 1 leads to a decreasing slope with 
# increasing predictor and n = 1 to a linear relationship. We expect it to be around 3, due to
# the cubic relationship between height and volume. Beyond this we don't know any constraints 
# without looking at penguin literature. But we can always simulate. Let's visualise!

# 4.1 With R ####
# Let's first plot a naive set of priors, to see the detrimental effect.
tibble(iteration = 1:1e3,
       k = rnorm(n = 1e3, mean = 0, sd = 1), # these are standard normal distributions 
       n = rnorm(n = 1e3, mean = 0, sd = 1)) %>% # which allow negative values
  expand_grid(flipper_length_mm = seq(100, 300, length.out = 100)) %>%
  mutate(body_mass_g = k * flipper_length_mm ^ n) %>%
  ggplot(aes(flipper_length_mm, body_mass_g, group = iteration)) +
    geom_line(alpha = 0.5) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# Now let's do an informed prior simulation.
require(truncnorm) # allows us to truncate the normal distribution
require(magrittr) # expands the pipe operator
tibble(iteration = 1:1e3,
       k = rgamma(n = 1e3, shape = 5e-4^2 / 1e-4^2, rate = 5e-4 / 1e-4^2), # reparameterised with mean and sd
       n = rtruncnorm(n = 1e3, mean = 3, sd = 0.1, a = 1, b = Inf)) %>% # a is lower bound, b is upper bound
  expand_grid(flipper_length_mm = seq(100, 300, length.out = 100)) %>%
  mutate(body_mass_g = k * flipper_length_mm ^ n) %>%
  ggplot(aes(flipper_length_mm, body_mass_g, group = iteration)) +
    geom_vline(xintercept = penguins_cc %$% c(min(flipper_length_mm), max(flipper_length_mm))) +
    geom_hline(yintercept = penguins_cc %$% c(min(body_mass_g), max(body_mass_g))) +
    geom_line(alpha = 0.05) +
    coord_cartesian(xlim = penguins_cc %$% c(min(flipper_length_mm), max(flipper_length_mm)),
                    ylim = penguins_cc %$% c(min(body_mass_g), max(body_mass_g))) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 4.2 With Stan ####
prior_stan <- "
parameters{
  real<lower=0> k;
  real<lower=1> n;
}

model{
  k ~ gamma( 5e-4^2 / 1e-4^2 , 5e-4 / 1e-4^2 );
  n ~ normal( 3 , 0.1 ) T[1, ];
}
"
require(cmdstanr)
prior_mod <- prior_stan %>% 
  write_stan_file() %>% 
  cmdstan_model()

prior_samples <- prior_mod$sample(data = list(), # no data to condition on
                                  seed = 100,
                                  chains = 8,
                                  parallel_chains = parallel::detectCores(),
                                  iter_warmup = 1e4,
                                  iter_sampling = 1e4)

prior_samples$draws(format = "df") %>%
  as_tibble() %>%
  expand_grid(flipper_length_mm = seq(100, 300, length.out = 100)) %>%
  mutate(body_mass_g = k * flipper_length_mm ^ n) %>%
  ggplot(aes(flipper_length_mm, body_mass_g, group = .draw)) +
    geom_vline(xintercept = penguins_cc %$% c(min(flipper_length_mm), max(flipper_length_mm))) +
    geom_hline(yintercept = penguins_cc %$% c(min(body_mass_g), max(body_mass_g))) +
    geom_line(alpha = 0.05) +
    coord_cartesian(xlim = penguins_cc %$% c(min(flipper_length_mm), max(flipper_length_mm)),
                    ylim = penguins_cc %$% c(min(body_mass_g), max(body_mass_g))) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 4. Stan model ####
# 4.1 Write model ####
# So we've decided on the parameterisation and priors but we haven't considered the rest
# of the model. We'll use a normal likelihood function, but I want to make predictions
# for penguins in general, so we need a multilevel model that estimates variability across
# penguins as well as within penguins. This is achieved by using hierarchical priors.
# The priors we settled on become our hyperpriors and the species priors are centred on 
# them with some additional interspecific variability. We'll put a suffix "mu" on the 
# hyperpriors to indicate that they represent the mean parameter space across species.
# Our Stan code will now have a data block to condition the model on.

penguins_stan <- "
data{
  int N;
  vector[N] flipper_length_mm;
  vector[N] body_mass_g;
  array[N] int species;
  int N_species;
}
parameters{
  // Hyperparameters
  real<lower=0> k_mu;
  real<lower=1> n_mu;
  
  // Interspecific variability
  real<lower=0> k_sigma;
  real<lower=0> n_sigma;
  
  // Species parameters
  vector<lower=0>[N_species] k;
  vector<lower=1>[N_species] n;
  
  // Likelihood uncertainty
  real<lower=0> sigma;
}

model{
  // Hyperpriors
  k_mu ~ gamma( 5e-4^2 / 1e-4^2 , 5e-4 / 1e-4^2 );
  n_mu ~ normal( 3 , 0.1 ) T[1, ];
  
  // Interspecific variability priors
  k_sigma ~ exponential( 1 );
  n_sigma ~ exponential( 1 );
  
  // Species priors
  k ~ gamma( k_mu^2 / k_sigma^2 , k_mu / k_sigma^2 );
  n ~ normal( n_mu , n_sigma ) T[1, ];
  
  // Likelihood uncertainty prior
  sigma ~ exponential( 1 );
  
  // Model
  vector[N] mu;
  for ( i in 1:N ) {
    mu[i] = k[species[i]] * flipper_length_mm[i] ^ n[species[i]];
  }
  
  // Likelihood
  body_mass_g ~ normal( mu , sigma );
}
"

# 4.2 Run model ####
penguins_mod <- penguins_stan %>% 
  write_stan_file() %>% 
  cmdstan_model()

require(tidybayes)
penguins_samples <- penguins_mod$sample(data = penguins_cc %>% 
                                          select(flipper_length_mm, body_mass_g, species) %>% 
                                          compose_data(.n_name = n_prefix("N")),
                                        seed = 100,
                                        chains = 8,
                                        parallel_chains = parallel::detectCores(),
                                        iter_warmup = 1e4,
                                        iter_sampling = 1e4,
                                        max_treedepth = 12)

# Phew, now you know what churning numbers really looks like!

# 4.3 Model checks ####
penguins_summary <- penguins_samples$summary()
penguins_summary

penguins_summary %>%
  filter(rhat > 1.001)
# no Rhat above 1.001

penguins_draws <- penguins_samples$draws(format = "df")

require(bayesplot)
penguins_draws %>% mcmc_trace()
penguins_draws %>% mcmc_rank_overlay() 
# chains look healthy

penguins_draws %>% mcmc_pairs(pars = c("k[1]", "n[1]"))
penguins_draws %>% mcmc_pairs(pars = c("k[2]", "n[2]"))
# pairs plots reveal correlation between parameters

# 4.4 Prior-posterior comparison ####
penguins_posterior <- penguins_samples %>%
  recover_types(penguins_cc %>% select(species)) %>%
  gather_draws(k[species], n[species], k_mu, n_mu) %>%
  ungroup() %>%
  mutate(level = if_else(str_ends(.variable, "mu"),
                         "general", "specific") %>% fct(),
         .variable = case_when(
           .variable %in% c("k", "k_mu") ~ "k",
           .variable %in% c("n", "n_mu") ~ "n"
           ) %>% fct()
         )
penguins_posterior

penguins_prior_posterior <- penguins_posterior %>%
  mutate(distribution = "posterior") %>%
  bind_rows(prior_samples %>%
              gather_draws(k, n) %>%
              ungroup() %>%
              slice(rep(1:n(), penguins_posterior %$% 
                          ( nlevels(species) * nlevels(level) ))
                    ) %>%
              mutate(level = penguins_posterior %$% 
                       rep(levels(level), 
                           each = max(.draw) * nlevels(.variable) * nlevels(species)), 
                     species = penguins_posterior %$% 
                       rep(levels(species), 
                           each = max(.draw) * nlevels(.variable) * nlevels(level)),
                     distribution = "prior"))


# 5. Predictions ####
# 5.1 mu ####

# 5.1 Observations ####

# 6. Visualise predictions ####






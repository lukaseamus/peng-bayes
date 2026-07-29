####____________________________________####
#### Bayesian penguins: introduction to ####
#### Bayesian statistics in R and Stan, ####
#### Part 1: flippers                   ####
#### Luka Seamus Wright, 30 July 2026   ####
####____________________________________####

# 1. Flippers ####
# 1.1 Prior simulation ####
require(tidyverse) # how could we possibly live without the tidyverse?!
require(magrittr) # oui oui

tibble(
  mu = rnorm( 1e4 , 50 , 20 ), # likelihood mean
  sigma = rexp( 1e4 , 1 ) # likelihood sd
) %>%
  mutate(
    length = rnorm( n() , mu , sigma )
  ) %>%
  pivot_longer(
    cols = c(length, mu, sigma),
    names_to = "Parameter"
  ) %>%
  ggplot() +
    geom_density(aes(value)) +
    facet_wrap(~ Parameter, scale = "free", nrow = 1) +
    theme_classic()

# we can do better because there are no negative flippers!
# enter the gamma distribution

tibble(
  log_mu = rnorm( 1e4 , log(50) , 0.4 ), # log likelihood mean
  theta = rexp( 1e4 , 1 ) # likelihood scale
) %>%
  mutate(
    mu = exp(log_mu),
    length = rgamma( n() , mu / theta , 1 / theta )
  ) %>%
  pivot_longer(
    cols = c(length, log_mu, theta),
    names_to = "Parameter"
  ) %>%
  ggplot() +
    geom_density(aes(value)) +
    facet_wrap(~ Parameter, scale = "free", nrow = 1) +
    theme_classic()

# 1.2 Load data ####
require(palmerpenguins) # data on penguins at Palmer Station
penguins <- penguins %>% 
  as_tibble() %T>%
  print()

# prep for Stan is easy peasy!
require(tidybayes)
penguins %>% compose_data()

# but we want flipper length in centimetres and
# Stan doesn't like superfluous variables or NAs
penguins %>% 
  mutate(flipper_length_cm = flipper_length_mm / 10) %>%
  select(flipper_length_cm) %>%
  drop_na() %>%
  compose_data()

# 1.3 Stan model ####
require(cmdstanr)
require(here)

flippers_model <- here("Stan", "flippers.stan") %>%
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model() %T>%
  print()

flippers_samples <- flippers_model$sample(
  data = penguins %>% 
    mutate(flipper_length_cm = flipper_length_mm / 10) %>%
    select(flipper_length_cm) %>%
    drop_na() %>%
    compose_data(),
  chains = 8,
  parallel_chains = parallel::detectCores(),
  iter_warmup = 1e3,
  iter_sampling = 1e3,
) %T>%
  print()

# 1.4 Model checks ####

# R-hat
flippers_samples$summary() %>%
  summarise(rhat_1.01 = mean( rhat > 1.01 ),
            rhat_mean = mean( rhat ),
            rhat_sd = sd( rhat ))

# Chains
require(bayesplot)
flippers_samples$draws(format = "df") %>%
  mcmc_trace()

flippers_samples$draws(format = "df") %>%
  mcmc_trace_highlight()

flippers_samples$draws(format = "df") %>%
  mcmc_rank_overlay()

flippers_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Flippers model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), ylim = c(0, 100),
                  expand = FALSE, clip = "off") +
  theme_classic() +
  theme(
    legend.position = "top", 
    legend.justification = 0,
    panel.spacing = unit(1, "cm")
  )

# Pairs
flippers_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("log_mu", "theta"),
    grid_args = list(top = "Flippers model")
  )

# 1.5 Prior-posterior comparison ####

# Prior
flippers_prior <- tibble(
  log_mu = rnorm( 8e3 , log(50) , 0.4 ), # log likelihood mean
  theta = rexp( 8e3 , 1 ) # likelihood scale
) %T>%
  print()

# Posterior
flippers_posterior <- flippers_samples %>%
  spread_draws(log_mu, theta) %T>%
  print()

# Join
flippers_prior_posterior <- bind_rows(
    Prior = flippers_prior,
    Posterior = flippers_posterior %>% select(log_mu, theta),
    .id = "Distribution"
  ) %T>%
  print()

# Visualise
flippers_prior_posterior %>%
  pivot_longer(
    cols = c(log_mu, theta),
    names_to = "Parameter"
  ) %>%
  ggplot() +
    geom_density(
      aes(value, alpha = Distribution),
      colour = NA, fill = "black"
    ) +
    scale_alpha_manual(values = c("Prior" = 0.2, "Posterior" = 0.6)) +
    facet_wrap(~ Parameter, scales = "free") +
    theme_classic()

# 1.6 Prediction ####

flippers_prediction <- flippers_prior_posterior %>%
  mutate(
    mu = exp(log_mu),
    length = rgamma( n() , mu / theta , 1 / theta )
  ) %T>%
  print()


# 1.7 Summary ####

flippers_summary <- flippers_prediction %>%
  select(-c(log_mu, theta)) %>%
  pivot_longer(
    cols = -Distribution,
    names_to = "Parameter"
  ) %>%
  summarise(
    across(
      value,
      list(
        Mean = mean,
        SD = sd,
        Median = median
      ),
      .names = "{.fn}"
    ),
    P = mean( value > 21 ),
    N = n(),
    .by = c(Distribution, Parameter)
  ) %T>%
  print()
  
# 1.8 Visualisation ####

flippers_plot <- flippers_prediction %>%
  pivot_longer(
    cols = c(mu, length),
    names_to = "Parameter"
  ) %>%
  filter(Distribution == "Posterior") %>%
  ggplot() +
    geom_density(
      aes(value, alpha = Parameter),
      colour = NA, fill = "black"
    ) +
    geom_jitter(
      data = penguins %>% 
        mutate(flipper_length_cm = flipper_length_mm / 10) %>%
        select(flipper_length_cm) %>%
        drop_na(),
      aes(flipper_length_cm, -1), 
      alpha = 0.3, shape = 16, size = 2, height = 0.8
    ) +
    scale_alpha_manual(values = c("length" = 0.2, "mu" = 0.6)) +
    labs(x = "Flipper length (cm)") +
    coord_cartesian(xlim = c(16, 24), expand = c(T, F)) +
    theme_classic() +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.position = c(0.1, 0.8)
    )

flippers_plot

flippers_plot %>%
  ggsave(
    filename = "flippers.pdf", 
    path = "Plots",
    device = cairo_pdf,
    height = 6, width = 10, units = "cm"
  )

# 2. Flippers by species ####
# 2.1 Prior simulation ####

tibble(
  log_mu = rnorm( 1e4 , log(20) , 0.4 ), # log likelihood mean
  theta = rexp( 1e4 , 1 ) # likelihood scale
) %>%
  mutate(
    mu = exp(log_mu),
    length = rgamma( n() , mu / theta , 1 / theta )
  ) %>%
  pivot_longer(
    cols = c(length, log_mu, theta),
    names_to = "Parameter"
  ) %>%
  ggplot() +
    geom_density(aes(value)) +
    facet_wrap(~ Parameter, scale = "free", nrow = 1) +
    theme_classic()

# 2.2 Load data ####

penguins %>% 
  mutate(flipper_length_cm = flipper_length_mm / 10) %>%
  select(species, flipper_length_cm) %>%
  drop_na() %>%
  compose_data()

# 2.3 Stan model ####

flippers_species_model <- here("Stan", "flippers_species.stan") %>%
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model() %T>%
  print()

flippers_species_samples <- flippers_species_model$sample(
  data = penguins %>% 
    mutate(flipper_length_cm = flipper_length_mm / 10) %>%
    select(species, flipper_length_cm) %>%
    drop_na() %>%
    compose_data(),
  chains = 8,
  parallel_chains = parallel::detectCores(),
  iter_warmup = 1e3,
  iter_sampling = 1e3,
) %T>%
  print()

# 2.4 Model checks ####

# R-hat
flippers_species_samples$summary() %>%
  summarise(rhat_1.01 = mean( rhat > 1.01 ),
            rhat_mean = mean( rhat ),
            rhat_sd = sd( rhat ))

# Chains
flippers_species_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Flippers species model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), ylim = c(0, 100),
                  expand = FALSE, clip = "off") +
  theme_classic() +
  theme(
    legend.position = "top", 
    legend.justification = 0,
    panel.spacing = unit(1, "cm")
  )

# Pairs
flippers_species_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("log_mu[1]", "log_mu[2]", "log_mu[3]", "theta"),
    grid_args = list(top = "Flippers species model")
  )

# 2.5 Prior-posterior comparison ####

# Prior
flippers_species_prior <- tibble(
  log_mu = rnorm( 8e3 , log(20) , 0.4 ), # log likelihood mean
  theta = rexp( 8e3 , 1 ) # likelihood scale
) %>%
  expand_grid(
    species = penguins %$% 
                levels(species) %>%
                fct()
  ) %T>%
  print()

# Posterior
flippers_species_posterior <- flippers_species_samples %>%
  recover_types(penguins) %>%
  spread_draws(log_mu[species], theta) %T>%
  print()

# Join
flippers_species_prior_posterior <- bind_rows(
    Prior = flippers_species_prior,
    Posterior = flippers_species_posterior %>% 
      select(species, log_mu, theta),
    .id = "Distribution"
  ) %T>%
  print()

# Visualise
flippers_species_prior_posterior %>%
  pivot_longer(
    cols = c(log_mu, theta),
    names_to = "Parameter"
  ) %>%
  ggplot() +
    geom_density(
      aes(value, alpha = Distribution),
      colour = NA, fill = "black"
    ) +
    scale_alpha_manual(values = c("Prior" = 0.2, "Posterior" = 0.6)) +
    facet_wrap(~ Parameter + species, scales = "free") +
    theme_classic()

# 2.6 Prediction ####

flippers_species_prediction <- flippers_species_prior_posterior %>%
  filter(
    Distribution == "Prior" & species == "Adelie" |
      Distribution == "Posterior"
  ) %>%
  mutate(
    species = if_else(
      Distribution == "Prior",
      "Prior" %>% fct(), species
    ),
    mu = exp(log_mu),
    length = rgamma( n() , mu / theta , 1 / theta )
  ) %>%
  select(-Distribution) %T>%
  print()

# 2.7 Summary ####

flippers_species_summary <- flippers_species_prediction %>%
  select(-c(log_mu, theta)) %>%
  pivot_longer(
    cols = -species,
    names_to = "Parameter"
  ) %>%
  summarise(
    across(
      value,
      list(
        Mean = mean,
        SD = sd,
        Median = median
      ),
      .names = "{.fn}"
    ),
    P = mean( value > 21 ),
    N = n(),
    .by = c(species, Parameter)
  ) %T>%
  print()

# 2.8 Contrasts ####

flippers_species_contrasts <- flippers_species_prediction %>%
  filter(species != "Prior") %>%
  droplevels() %>%
  select(-c(log_mu, theta)) %>%
  pivot_longer(
    cols = -species,
    names_to = "Parameter"
  ) %>%
  mutate(n = 1:n(), .by = species) %>% # needed to identify rows
  pivot_wider(
    names_from = species,
    values_from = value
  ) %>%
  mutate(
    AC_Difference = Adelie - Chinstrap,
    AC_Proportion = Adelie / Chinstrap,
    AG_Difference = Adelie - Gentoo,
    AG_Proportion = Adelie / Gentoo,
    CG_Difference = Chinstrap - Gentoo,
    CG_Proportion = Chinstrap / Gentoo
  ) %>%
  select(Parameter, ends_with("Difference"), ends_with("Proportion")) %>%
  pivot_longer(
    cols = -Parameter,
    names_to = c("Contrast", "Statistic"),
    names_sep = "_"
  ) %T>%
  print()


flippers_species_contrasts_summary <- flippers_species_contrasts %>%
  summarise(
    across(
      value,
      list(
        Mean = mean,
        SD = sd,
        Median = median
      ),
      .names = "{.fn}"
    ),
    P = max( mean( value > 0 ) , mean( value < 0 ) ),
    N = n(),
    .by = c(Parameter, Contrast, Statistic)
  ) %T>%
  print()


# 2.9 Visualisation ####
require(cowplot)

flippers_species_plot <- ( 
  flippers_species_prediction %>%
  pivot_longer(
    cols = c(mu, length),
    names_to = "Parameter"
  ) %>%
  filter(species != "Prior") %>%
  droplevels() %>%
  ggplot() +
    geom_density(
      aes(value, alpha = Parameter, fill = species),
      colour = NA
    ) +
    geom_jitter(
      data = penguins %>%
        mutate(flipper_length_cm = flipper_length_mm / 10) %>%
        select(species, flipper_length_cm) %>%
        drop_na(),
      aes(flipper_length_cm, -1, colour = species),
      alpha = 0.3, shape = 16, size = 2, height = 0.8
    ) +
    scale_fill_manual(
      values = c(
        "Adelie" = "darkorange",
        "Chinstrap" = "purple",
        "Gentoo" = "cyan4"
      ),
      guide = "none"
    ) +
    scale_colour_manual(
      values = c(
        "Adelie" = "darkorange",
        "Chinstrap" = "purple",
        "Gentoo" = "cyan4"
      ),
      guide = "none"
    ) +
    scale_alpha_manual(values = c("length" = 0.2, "mu" = 0.6),
                       guide = "none") +
    labs(x = "Flipper length (cm)") +
    coord_cartesian(xlim = c(16, 24), expand = c(T, F)) +
    theme_classic() +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank()
    ) 
  ) %>%
  ggdraw() +
  draw_image(
    here("Images", "penguins.png"), 
    x = -0.12, y = 0.6, 
    width = 0.6, height = 0.6*1074/1800
  )

flippers_species_plot

flippers_species_plot %>%
  ggsave(
    filename = "flippers_species.pdf", 
    path = "Plots",
    device = cairo_pdf,
    height = 6, width = 10, units = "cm"
  )

flippers_species_contrasts_plot <- flippers_species_contrasts %>%
  filter(Statistic == "Difference") %>%
  ggplot() +
    stat_slab(
      aes(value, Contrast, alpha = Parameter),
      colour = NA, fill = "black"
    ) +
    geom_vline(xintercept = 0) +
    scale_alpha_manual(values = c("length" = 0.2, "mu" = 0.6),
                       guide = "none") +
    labs(x = "Flipper length difference (cm)") +
    coord_cartesian(xlim = c(-6, 2), expand = c(T, F)) +
    theme_classic() +
    theme(
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank()
    )

flippers_species_contrasts_plot

flippers_species_contrasts_plot %>%
  ggsave(
    filename = "flippers_species_contrasts.pdf", 
    path = "Plots",
    device = cairo_pdf,
    height = 6, width = 10, units = "cm"
  )


################################


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
# For one, both variables cannot be negative. Luckily there are no negative flippers! A linear model will 
# eventually make the mistake of predicting negative values. A basic rule of allometry is that as the 
# one-dimensional size of an organism changes, the mass changes as a power function of that dimension.
# To conceptualise this, imagine penguins as spheres (chubby penguins!). 
ggdraw() + draw_image("meme.jpg")

# The mass or volume of a sphere is calculated from its height as 4/3 * pi * (h/2)^3. See that exponent. 
# That's not linear! Simplifying the function results in the standard two-parameter power function
# f(x) = k * x ^ n. n is the exponent that describes the relationship between a one-dimensional metric,
# such as flipper length, and volume. k is a constant that converts volume to mass, i.e. density. 
# We don't need an intercept because penguin embryos have no flippers and practically no mass. 
# So let's use that function. Here's an illustrative example where I used it on snails:
ggdraw() + draw_image("snails.tiff") # Fig. S2 in Wright (2023) https://doi.org/10.1007/s10750-023-05143-4

# n is pretty close to 3, huh? This means snails basically scale like spheres. But you see how small 
# that k is? That's because back when I wrote the snail paper I was naive scientist and didn't attempt 
# to make sense of the parameter. It essentially represents g/mm^3, so is bound to be tiny because snails
# don't happen to be denser than gold (0.0193 g/mm^3)! It is much easier to conceptualise k when the
# predictor variable is rescaled to cm. I also generally find it easier to imagine flipper length in cm.
require(magrittr) # expands the pipe operator
penguins_cc %<>%
  mutate(flipper_length_cm = flipper_length_mm / 10)

# 4. Prior simulation ####
# We know up front that k cannot be negative because this would imply negative density and lead to
# negative predictions of mass. We also know that n has to be greater than 1, since n < 1 leads to 
# a decreasing slope with increasing predictor and n = 1 to a linear relationship. We expect it to 
# be around 3, due to the cubic relationship between height and volume. Beyond this we don't know 
# any constraints without looking at penguin literature. But we can always simulate. Let's visualise!

# 4.1 With R ####
# Let's first plot a naive set of priors, to see the detrimental effect.
tibble(iteration = 1:1e3,
       k = rnorm(n = 1e3, mean = 0, sd = 1), # these are standard normal distributions 
       n = rnorm(n = 1e3, mean = 0, sd = 1)) %>% # which allow negative values
  expand_grid(flipper_length_cm = seq(10, 30, length.out = 100)) %>%
  mutate(body_mass_g = k * flipper_length_cm ^ n) %>%
  ggplot(aes(flipper_length_cm, body_mass_g, group = iteration)) +
    geom_line(alpha = 0.5) +
    annotate("rect", xmin = penguins_cc %$% min(flipper_length_cm), 
             xmax = penguins_cc %$% max(flipper_length_cm), 
             ymin = penguins_cc %$% min(body_mass_g), 
             ymax = penguins_cc %$% max(body_mass_g), 
             colour = "black", fill = NA) +
    theme_minimal() +
    theme(panel.grid = element_blank())
# Clearly, negative body masses are impossible!

# Now let's do an informed prior simulation.
require(truncnorm) # allows us to truncate the normal distribution
tibble(iteration = 1:1e3,
       k = rgamma(n = 1e3, shape = 2^2 / 1^2, rate = 2 / 1^2), # reparameterised with mean and sd
       n = rtruncnorm(n = 1e3, mean = 3, sd = 0.5, a = 1, b = Inf)) %>% # a is lower bound, b is upper bound
  expand_grid(flipper_length_cm = seq(10, 30, length.out = 100)) %>%
  mutate(body_mass_g = k * flipper_length_cm ^ n) %>%
  ggplot(aes(flipper_length_cm, body_mass_g, group = iteration)) +
    geom_line(alpha = 0.05) +
    annotate("rect", xmin = penguins_cc %$% min(flipper_length_cm), 
             xmax = penguins_cc %$% max(flipper_length_cm), 
             ymin = penguins_cc %$% min(body_mass_g), 
             ymax = penguins_cc %$% max(body_mass_g), 
             colour = "black", fill = NA) +
    coord_cartesian(ylim = c(0, 9e3)) +
    # this is an alternative coordinate system focussing on the data range
    # coord_cartesian(xlim = penguins_cc %$% c(min(flipper_length_cm), max(flipper_length_cm)),
    #                 ylim = penguins_cc %$% c(min(body_mass_g), max(body_mass_g))) +
    theme_minimal() +
    theme(panel.grid = element_blank())
# This has some generous variability and could certainly be constrained further because there are many 
# completely improbable trajectories, but we'll leave it at this. Sometimes it also helps to visualise 
# individual distributions to realise impossibilities.
ggplot() +
  geom_density(aes(rgamma(n = 1e5, shape = 2^2 / 1^2, rate = 2 / 1^2)),
               fill = "black", alpha = 0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank())

ggplot() +
  geom_density(aes(rtruncnorm(n = 1e5, mean = 3, sd = 0.5, a = 1, b = Inf)),
               fill = "black", alpha = 0.2) +
  theme_minimal() +
  theme(panel.grid = element_blank())

# 4.2 With Stan ####
prior_stan <- "
parameters{
  real<lower=0> k;
  real<lower=1> n;
}

model{
  k ~ gamma( 2^2 / 1^2 , 2 / 1^2 );
  n ~ normal( 3 , 0.5 ) T[1, ];
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
  slice_sample(n = 1e3) %>% # take 1e3 random samples to match the above
  expand_grid(flipper_length_cm = seq(10, 30, length.out = 100)) %>%
  mutate(body_mass_g = k * flipper_length_cm ^ n) %>%
  ggplot(aes(flipper_length_cm, body_mass_g, group = .draw)) +
    annotate("rect", xmin = penguins_cc %$% min(flipper_length_cm), 
             xmax = penguins_cc %$% max(flipper_length_cm), 
             ymin = penguins_cc %$% min(body_mass_g), 
             ymax = penguins_cc %$% max(body_mass_g), 
             colour = "black", fill = NA) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(0, 10e3)) + 
    theme_minimal() +
    theme(panel.grid = element_blank())
# Tada! Similar to above.

# 4. Stan model ####
# 4.1 Write model ####
# So we've decided on the parameterisation and priors but we haven't considered the rest
# of the model. We'll use a normal likelihood function, because non-negativity is already
# ensured by our priors. Our Stan code will now have a data block to condition the model 
# on and include model and likelihood functions.

penguins_stan <- "
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
"

# 4.2 Run model ####
penguins_mod <- penguins_stan %>%
  write_stan_file() %>%
  cmdstan_model()

require(tidybayes)
penguins_samples <- penguins_mod$sample(data = penguins_cc %>%
                                          select(flipper_length_cm, body_mass_g, species) %>%
                                          compose_data(.n_name = n_prefix("N")),
                                        seed = 100,
                                        chains = 8,
                                        parallel_chains = parallel::detectCores(),
                                        iter_warmup = 1e4,
                                        iter_sampling = 1e4)
# Phew, now you know what churning numbers really looks like!
# You'll get a warning if there are divergent transitions (i.e. where the marble flew out the bowl),
# but there aren't any here so that's a first great sign the model ran smoothly.

# 4.3 Model checks ####
penguins_summary <- penguins_samples$summary()
penguins_summary

penguins_summary %>%
  filter(rhat > 1.001)
# no Rhat above 1.001

penguins_draws <- penguins_samples$draws(format = "df")

require(bayesplot)
penguins_draws %>% 
  mcmc_trace() # fuzzy caterpillars are good!
penguins_draws %>% 
  mcmc_rank_overlay() # mixing is good
# chains look healthy

penguins_draws %>% 
  mcmc_pairs(pars = c("k[1]", "n[1]"))
penguins_draws %>% 
  mcmc_pairs(pars = c("k[2]", "n[2]"))
penguins_draws %>%
  mcmc_pairs(pars = c("k[3]", "n[3]"))
# Pair plots reveal correlation between parameters, meaning the data do not inform independent
# estimation of k and n (i.e. k and n are not identifiable). This is normal for this type of model.
# But insights like these allow us to think up better models.

# To demonstrate what these probability biplots (bowls) should look like in a healthy linear model, 
# here are some images because we don't have time to run additional models (see linear.R for code).
# This is what a naive linear model looks like.
ggdraw() + draw_image("penguins_linear.tiff")
# Clearly there is strong correlation between the intercept (alpha) and slope (beta). If you think
# about it long enough, this is only logical if the prediction range is positive. Luckily, there's
# a simple trick called centring, where the mean of the predictor variable is subtracted from each
# observation in that variable. This causes the intercept to be equivalent to the response at the 
# predictor's mean rather than when it equals 0. Here's the same model with a centred predictor.
ggdraw() + draw_image("penguins_linear_c.tiff")
# This is close to an ideal bowl for the sampler!

# 4.4 Prior-posterior comparison ####
penguins_posterior <- penguins_samples %>%
  recover_types(penguins_cc %>% select(species)) %>%
  gather_draws(k[species], n[species], sigma) %>%
  ungroup() %>%
  mutate(.variable = .variable %>% fct())
penguins_posterior

penguins_prior_posterior <- penguins_posterior %>%
  filter(.variable != "sigma") %>%
  mutate(.variable = fct_drop(.variable),
         distribution = "posterior") %>%
  bind_rows(prior_samples %>%
              gather_draws(k, n) %>%
              ungroup() %>%
              slice(rep(1:n(), penguins_posterior %$% nlevels(species) )) %>%
              mutate(species = penguins_posterior %>% 
                       filter(.variable != "sigma") %>%
                       mutate(.variable = fct_drop(.variable)) %$% 
                       rep(levels(species), 
                           each = nlevels(.variable) * max(.draw)),
                     distribution = "prior"))
penguins_prior_posterior

require(ggh4x) # allows hacking ggplot2
penguins_prior_posterior %>%
  ggplot(aes(.value, fill = distribution)) +
    geom_density(colour = NA) +
    scale_fill_manual(values = c(alpha("black", 0.6), alpha("black", 0.2))) +
    facet_nested(~ .variable + species, scales = "free",
                 nest_line = TRUE) +
    theme_minimal() +
    theme(panel.grid = element_blank())
# As you can see, priors are not restrictive at all, but still informative.
# The non-identifiability of k and n are again clear because the prior and
# posterior for k are very similar, indicating that the model couldn't learn
# much from the data to update the prior.

# Interestingly the exponent is closer to 2, indicating a squared rather than
# a cubic relationship between flipper length and penguin volume. So our penguins 
# don't look like this one after all!
ggdraw() + draw_image("meme.jpg")

# 5. Predictions ####
# 5.1 Parameter estimates ####
penguins_posterior %>%
  group_by(species, .variable) %>%
  summarise(mean = mean(.value),
            sd = sd(.value),
            n = length(.value)) %>%
  mutate(rounded = paste(mean %>% signif(digits = 2), "±", 
                         sd %>% signif(digits = 2)))

# 5.2 Pairwise contrasts ####
penguins_cc %$% ( nlevels(species) * ( nlevels(species) - 1 ) / 2 )
# There are 3 pairwise contrasts for 3 species. Calculate differences for both variables.
penguins_delta <- penguins_posterior %>%
  pivot_wider(names_from = c(.variable, species), values_from = .value) %>%
  mutate(delta_k_A_C = k_Adelie - k_Chinstrap,
         delta_k_A_G = k_Adelie - k_Gentoo,
         delta_k_C_G = k_Chinstrap - k_Gentoo,
         delta_n_A_C = n_Adelie - n_Chinstrap,
         delta_n_A_G = n_Adelie - n_Gentoo,
         delta_n_C_G = n_Chinstrap - n_Gentoo) %>%
  select(starts_with(c(".", "delta"))) %>%
  pivot_longer(cols = starts_with("delta"),
               names_to = ".variable", values_to = ".value", names_prefix = "delta_") %>%
  mutate(contrast = paste(str_extract(.variable, "(?<=_)[^_]+"), "−", 
                          str_extract(.variable, "[^_]+$")),
         .variable = str_extract(.variable, "^."))

# Since the differences are also distributions, we can calculate the probability that any 
# pair of penguins are different simply by calculating probability mass above or below 0
penguins_delta %>%
  group_by(.variable, contrast) %>%
  summarise(mean = mean(.value),
            sd = sd(.value),
            P_more = mean(.value > 0),
            P_less = mean(.value < 0),
            n = length(.value)) %>%
  mutate(rounded = paste(mean %>% abs() %>% signif(digits = 2), "±", 
                         sd %>% signif(digits = 2)),
         P = pmax(P_less, P_more) %>% signif(digits = 2))

# 5.3 mu and observations ####
penguins_prediction <- penguins_posterior %>%
  filter(.variable != "sigma") %>%
  mutate(.variable = fct_drop(.variable)) %>%
  pivot_wider(names_from = .variable, values_from = .value) %>%
  left_join(penguins_posterior %>%
              filter(.variable == "sigma") %>%
              select(-c(.variable, species)) %>% 
              rename(sigma = .value),
            by = c(".chain", ".iteration", ".draw")) %>%
  left_join(penguins_cc %>%
              group_by(species) %>%
              summarise(min = min(flipper_length_cm),
                        max = max(flipper_length_cm)),
            by = "species") %>%
  rowwise() %>%
  mutate(flipper_length_cm = list( seq(min, max, length.out = 100) )) %>%
  unnest(flipper_length_cm) %>%
  mutate(mu = k * flipper_length_cm ^ n,
         obs = rnorm(n = n(), mean = mu, sd = sigma))
penguins_prediction

penguins_prediction_summary <- penguins_prediction %>%
  group_by(species, flipper_length_cm) %>%
  reframe(mu = mu %>% mean_qi(.width = c(.5, .8, .9)),
          obs = obs %>% mean_qi(.width = c(.5, .8, .9))) %>%
  unnest_wider(c(mu, obs), names_sep = "_")
penguins_prediction_summary

# 6. Visualise predictions ####
# We can visualise predictions for mu on top of the data and show a probability interval
# for new observations. It is too messy to plot the entire distribution for new observations 
# on top of that of mu.
(penguins_cc %>%
   ggplot() +
    geom_point(aes(flipper_length_cm, body_mass_g, colour = species),
               shape = 16, size = 2) +
    geom_line(data = penguins_prediction_summary,
              aes(flipper_length_cm, mu_y, colour = species)) +
    geom_ribbon(data = penguins_prediction_summary,
                aes(flipper_length_cm, ymin = mu_ymin, ymax = mu_ymax,
                    fill = species, alpha = factor(mu_.width)), colour = NA) +
    geom_ribbon(data = penguins_prediction_summary %>%
                  filter(obs_.width == 0.9),
                aes(flipper_length_cm, ymin = obs_ymin, ymax = obs_ymax,
                    colour = species), fill = NA) +
    scale_colour_manual(values = c("darkorange","purple","cyan4"),
                        guide = "none") +
    scale_fill_manual(values = c("darkorange","purple","cyan4"),
                      guide = "none") +
    scale_alpha_manual(values = c(0.4, 0.3, 0.2), guide = "none") +
    labs(x = "Flipper length (cm)", y = "Body mass (g)") +
    theme_minimal() +
    theme(panel.grid = element_blank())) %>%
    ggdraw() +
    draw_image("penguins.png", x = 0, y = 0.65, width = 0.5, height = 0.2983333)

# Alternatively we can just plot predictions for new observations.
(penguins_cc %>%
   ggplot() +
    geom_point(aes(flipper_length_cm, body_mass_g, colour = species),
               shape = 16, size = 2) +
    geom_line(data = penguins_prediction_summary,
              aes(flipper_length_cm, obs_y, colour = species)) +
    geom_ribbon(data = penguins_prediction_summary,
                aes(flipper_length_cm, ymin = obs_ymin, ymax = obs_ymax,
                    fill = species, alpha = factor(obs_.width)), colour = NA) +
    scale_colour_manual(values = c("darkorange","purple","cyan4"),
                        guide = "none") +
    scale_fill_manual(values = c("darkorange","purple","cyan4"),
                      guide = "none") +
    scale_alpha_manual(values = c(0.4, 0.3, 0.2), guide = "none") +
    labs(x = "Flipper length (cm)", y = "Body mass (g)") +
    theme_minimal() +
    theme(panel.grid = element_blank())) %>%
    ggdraw() +
    draw_image("penguins.png", x = 0, y = 0.65, width = 0.5, height = 0.2983333)

# That's it! Hope you learned something!
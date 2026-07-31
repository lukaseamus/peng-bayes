####____________________________________####
#### Bayesian penguins: introduction to ####
#### Bayesian statistics in R and Stan, ####
#### Part 3: spherical penguins         ####
#### Luka Seamus Wright, 30 July 2026   ####
####____________________________________####

# 1. Mass ####
# 1.1 Load data ####
require(tidyverse)
require(magrittr)
require(here)
require(palmerpenguins)
require(cowplot)

penguins <- penguins %>% 
  mutate(
    flipper_length_cm = flipper_length_mm / 10
  ) %T>%
  print()

penguins %>% 
  drop_na(species, flipper_length_cm, body_mass_g) %>%
  ggplot() +
    geom_point(
      aes(flipper_length_cm, body_mass_g, colour = species),
      alpha = 0.5, shape = 16, size = 2
    ) +
    scale_colour_manual(
      values = c(
        "Adelie" = "darkorange",
        "Chinstrap" = "purple",
        "Gentoo" = "cyan4"
      ),
      guide = "none"
    ) +
    theme_classic()

# 1.2 Prior simulation ####
# We can do better than fitting a linear model. Flipper length cannot be linearly related to body mass.
# For one, both variables cannot be negative. Luckily there are no negative flippers! A linear model will 
# eventually make the mistake of predicting negative values. A basic rule of allometry is that as the 
# one-dimensional size of an organism changes, the mass changes as a power function of that dimension.
# To conceptualise this, imagine penguins as spheres (chubby penguins!). 
ggdraw() + draw_image(here("Images", "meme.jpg"))

# The mass or volume of a sphere is calculated from its height as 4/3 * pi * (h/2)^3. See that exponent. 
# That's not linear! Simplifying the function results in the standard two-parameter power function
# f(x) = alpha * x ^ beta. beta is the exponent that describes the relationship between a one-dimensional metric,
# such as flipper length, and volume. alpha is a constant that converts volume to mass, i.e. density. 
# We don't need an intercept because penguin embryos have no flippers and practically no mass. 
# So let's use that function.

# We know up front that alpha cannot be negative because this would imply negative density and lead to
# negative predictions of mass. We also know that beta has to be greater than 1, since beta < 1 leads to 
# a decreasing slope with increasing predictor and beta = 1 to a linear relationship. We expect it to 
# be around 3, due to the cubic relationship between height and volume. Beyond this we don't know 
# any constraints without looking at penguin literature. But we can always simulate. Let's visualise!

# Let's first plot a naive set of priors, to see the detrimental effect.
tibble(
  n = 1:1e3,
  alpha = rnorm( 1e3 , 0 , 1 ), # constant
  beta = rnorm( 1e3 , 0 , 1 ), # exponent
  sigma = rexp( 1e3 , 1 ) # likelihood sd
) %>%
  expand_grid(
    flipper_length_cm = penguins %>% drop_na(flipper_length_cm) %$% 
      seq( min(flipper_length_cm) , max(flipper_length_cm) , length.out = 50 )
  ) %>%
  mutate(
    mu = alpha * flipper_length_cm ^ beta,
    body_mass_g = rnorm( n() , mu , sigma )
  ) %>%
  pivot_longer(
    cols = c(body_mass_g, mu),
    names_to = "Parameter"
  ) %>%
  ggplot() +
    geom_line(
      aes(flipper_length_cm, value, group = n),
      alpha = 0.05
    ) +
    facet_wrap(~ Parameter, scale = "free", nrow = 1) +
    theme_classic()

# Now let's do an informed prior simulation.
require(extraDistr) # allows us to truncate the normal distribution

tibble(
  n = 1:1e3,
  alpha = rgamma( 1e3 , 1.5^2 / 0.5^2 , 1.5 / 0.5^2 ), # constant
  beta = rtnorm( 1e3 , 3 , 0.5 , 1 ), # exponent
  sigma = rexp( 1e3 , 1 ) # likelihood sd
) %>%
  expand_grid(
    flipper_length_cm = penguins %>% drop_na(flipper_length_cm) %$% 
      seq( min(flipper_length_cm) , max(flipper_length_cm) , length.out = 50 )
  ) %>%
  mutate(
    mu = alpha * flipper_length_cm ^ beta,
    body_mass_g = rnorm( n() , mu , sigma )
  ) %>%
  pivot_longer(
    cols = c(body_mass_g, mu),
    names_to = "Parameter"
  ) %>%
  ggplot() +
    geom_line(
      aes(flipper_length_cm, value, group = n),
      alpha = 0.05
    ) +
    facet_wrap(~ Parameter, scale = "free", nrow = 1) +
    theme_classic()

# 1.3 Stan model ####
require(cmdstanr)
require(tidybayes)

mass_model <- here("Stan", "mass.stan") %>%
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model() %T>%
  print()

mass_samples <- mass_model$sample(
  data = penguins %>%
    select(species, flipper_length_cm, body_mass_g) %>%
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
mass_samples$summary() %>%
  summarise(rhat_1.01 = mean( rhat > 1.01 ),
            rhat_mean = mean( rhat ),
            rhat_sd = sd( rhat ))

# Chains
require(bayesplot)

mass_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Mass model",
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
mass_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c(
      "alpha[1]", "beta[1]", 
      "alpha[2]", "beta[2]",
      "alpha[3]", "beta[3]", 
      "sigma"
    ),
    grid_args = list(top = "Mass model")
  )
# strong correlation between alpha and beta

# 1.5 Prior-posterior comparison ####

# Prior
mass_prior <- tibble(
  alpha = rgamma( 1e3 , 1.5^2 / 0.5^2 , 1.5 / 0.5^2 ), # constant
  beta = rtnorm( 1e3 , 3 , 0.5 , 1 ), # exponent
  sigma = rexp( 1e3 , 1 ) # likelihood sd
) %>%
  expand_grid(
    species = penguins %$% 
      levels(species) %>%
      fct()
  ) %T>%
  print()
  
# Posterior
mass_posterior <- mass_samples %>%
  recover_types(penguins) %>%
  spread_draws(alpha[species], beta[species], sigma) %T>%
  print()

# Join
mass_prior_posterior <- bind_rows(
    Prior = mass_prior,
    Posterior = mass_posterior %>% select(-starts_with(".")),
    .id = "Distribution"
  ) %T>%
  print()

# Visualise
mass_prior_posterior %>%
  pivot_longer(
    cols = c(alpha, beta, sigma),
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

# 1.7 Summary ####

mass_parameters <- mass_prior_posterior %>%
  filter(
    Distribution == "Prior" & species == "Adelie" |
      Distribution == "Posterior"
  ) %>%
  mutate(
    species = if_else(
      Distribution == "Prior",
      "Prior" %>% fct(), species
    )
  ) %>%
  select(-Distribution) %T>%
  print()
  
mass_summary <- mass_parameters %>%
  select(-sigma) %>%
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
    P = mean( value > 0 ),
    N = n(),
    .by = c(species, Parameter)
  ) %T>%
  print()

# 1.8 Contrasts ####

mass_contrasts <- mass_parameters %>%
  filter(species != "Prior") %>%
  droplevels() %>%
  select(-sigma) %>%
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


mass_contrasts_summary <- mass_contrasts %>%
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

# 1.6 Prediction ####

mass_prediction <- mass_prior_posterior %>%
  filter(Distribution == "Posterior") %>%
  select(-Distribution) %>%
  full_join(
    penguins %>%
      drop_na(species, flipper_length_cm, body_mass_g) %>%
      group_by(species) %>%
      summarise(
        min = min(flipper_length_cm),
        max = max(flipper_length_cm)
      ),
    by = "species"
  ) %>%
  rowwise() %>%
  mutate(flipper_length_cm = list( seq(min, max, length.out = 50) )) %>%
  select(-c(min, max)) %>%
  unnest(flipper_length_cm) %>%
  mutate(
    mu = alpha * flipper_length_cm ^ beta,
    body_mass_g = rnorm( n() , mu , sigma )
  ) %T>%
  print()

mass_prediction_summary <- mass_prediction %>%
  group_by(species, flipper_length_cm) %>%
  mean_qi(mu, body_mass_g, .width = c(.5, .8, .9)) %T>%
  print()


# 1.9 Visualisation ####

mass_plot <- (
  mass_prediction_summary %>%
  ggplot() +
    geom_point(
      data = penguins %>% 
        drop_na(species, flipper_length_cm, body_mass_g),
      aes(flipper_length_cm, body_mass_g, colour = species),
      alpha = 0.5, shape = 16, size = 2
    ) +
    geom_line(
      aes(flipper_length_cm, mu, colour = species)
    ) +
    geom_ribbon(
      aes(flipper_length_cm, ymin = mu.lower, ymax = mu.upper,
          fill = species, alpha = as_factor(.width))
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
    scale_alpha_manual(
      values = c("0.5" = 0.5, "0.8" = 0.4, "0.9" = 0.3),
      guide = "none"
    ) +
    labs(x = "Flipper length (cm)", y = "Body mass (g)") +
    coord_cartesian(xlim = c(16, 24), ylim = c(2e3, 7e3), 
                    expand = F) +
    theme_classic()
  ) %>%
  ggdraw() +
  draw_image(
    here("Images", "penguins.png"), 
    x = 0.04, y = 0.65, 
    width = 0.6, height = 0.6*1074/1800
  )

mass_plot

mass_plot %>%
  ggsave(
    filename = "mass.pdf", 
    path = "Plots",
    device = cairo_pdf,
    height = 6, width = 10, units = "cm"
  )

# 2. Pooled mass ####
# 2.1 Prior simulation ####

tibble(
  n = 1:1e3,
  log_alpha_mu = rnorm( 1e3 , log(1.5) , 0.2 ), # model in log space
  log_alpha_sd = rtnorm( 1e3 , 0 , 0.2 , 0 ),
  log_beta_mu = rnorm( 1e3 , log(3) , 0.2 ),
  log_beta_sd = rtnorm( 1e3 , 0 , 0.2 , 0 ),
  sigma = rexp( 1e3 , 1 ) # likelihood sd
) %>%
  mutate(
    log_alpha = rnorm( n() , log_alpha_mu , log_alpha_sd ),
    log_beta = rnorm( n() , log_beta_mu , log_beta_sd )
  ) %>%
  expand_grid(
    flipper_length_cm = penguins %>% drop_na(flipper_length_cm) %$% 
      seq( min(flipper_length_cm) , max(flipper_length_cm) , length.out = 50 )
  ) %>%
  mutate(
    alpha = exp(log_alpha),
    beta = exp(log_beta),
    mu = alpha * flipper_length_cm ^ beta,
    body_mass_g = rnorm( n() , mu , sigma )
  ) %>%
  pivot_longer(
    cols = c(body_mass_g, mu),
    names_to = "Parameter"
  ) %>%
  ggplot() +
    geom_line(
      aes(flipper_length_cm, value, group = n),
      alpha = 0.05
    ) +
    facet_wrap(~ Parameter, scale = "free", nrow = 1) +
    theme_classic()

# 1.3 Stan model ####
require(cmdstanr)
require(tidybayes)

mass_model <- here("Stan", "mass.stan") %>%
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model() %T>%
  print()

mass_samples <- mass_model$sample(
  data = penguins %>%
    select(species, flipper_length_cm, body_mass_g) %>%
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
mass_samples$summary() %>%
  summarise(rhat_1.01 = mean( rhat > 1.01 ),
            rhat_mean = mean( rhat ),
            rhat_sd = sd( rhat ))

# Chains
require(bayesplot)

mass_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Mass model",
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
mass_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c(
      "alpha[1]", "beta[1]", 
      "alpha[2]", "beta[2]",
      "alpha[3]", "beta[3]", 
      "sigma"
    ),
    grid_args = list(top = "Mass model")
  )
# strong correlation between alpha and beta

# 1.5 Prior-posterior comparison ####

# Prior
mass_prior <- tibble(
  alpha = rgamma( 1e3 , 1.5^2 / 0.5^2 , 1.5 / 0.5^2 ), # constant
  beta = rtnorm( 1e3 , 3 , 0.5 , 1 ), # exponent
  sigma = rexp( 1e3 , 1 ) # likelihood sd
) %>%
  expand_grid(
    species = penguins %$% 
      levels(species) %>%
      fct()
  ) %T>%
  print()
  
# Posterior
mass_posterior <- mass_samples %>%
  recover_types(penguins) %>%
  spread_draws(alpha[species], beta[species], sigma) %T>%
  print()

# Join
mass_prior_posterior <- bind_rows(
    Prior = mass_prior,
    Posterior = mass_posterior %>% select(-starts_with(".")),
    .id = "Distribution"
  ) %T>%
  print()

# Visualise
mass_prior_posterior %>%
  pivot_longer(
    cols = c(alpha, beta, sigma),
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

# 1.7 Summary ####

mass_parameters <- mass_prior_posterior %>%
  filter(
    Distribution == "Prior" & species == "Adelie" |
      Distribution == "Posterior"
  ) %>%
  mutate(
    species = if_else(
      Distribution == "Prior",
      "Prior" %>% fct(), species
    )
  ) %>%
  select(-Distribution) %T>%
  print()
  
mass_summary <- mass_parameters %>%
  select(-sigma) %>%
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
    P = mean( value > 0 ),
    N = n(),
    .by = c(species, Parameter)
  ) %T>%
  print()

# 1.8 Contrasts ####

mass_contrasts <- mass_parameters %>%
  filter(species != "Prior") %>%
  droplevels() %>%
  select(-sigma) %>%
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


mass_contrasts_summary <- mass_contrasts %>%
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

# 1.6 Prediction ####

mass_prediction <- mass_prior_posterior %>%
  filter(Distribution == "Posterior") %>%
  select(-Distribution) %>%
  full_join(
    penguins %>%
      drop_na(species, flipper_length_cm, body_mass_g) %>%
      group_by(species) %>%
      summarise(
        min = min(flipper_length_cm),
        max = max(flipper_length_cm)
      ),
    by = "species"
  ) %>%
  rowwise() %>%
  mutate(flipper_length_cm = list( seq(min, max, length.out = 50) )) %>%
  select(-c(min, max)) %>%
  unnest(flipper_length_cm) %>%
  mutate(
    mu = alpha * flipper_length_cm ^ beta,
    body_mass_g = rnorm( n() , mu , sigma )
  ) %T>%
  print()

mass_prediction_summary <- mass_prediction %>%
  group_by(species, flipper_length_cm) %>%
  mean_qi(mu, body_mass_g, .width = c(.5, .8, .9)) %T>%
  print()


# 1.9 Visualisation ####

mass_plot <- (
  mass_prediction_summary %>%
  ggplot() +
    geom_point(
      data = penguins %>% 
        drop_na(species, flipper_length_cm, body_mass_g),
      aes(flipper_length_cm, body_mass_g, colour = species),
      alpha = 0.5, shape = 16, size = 2
    ) +
    geom_line(
      aes(flipper_length_cm, mu, colour = species)
    ) +
    geom_ribbon(
      aes(flipper_length_cm, ymin = body_mass_g.lower, ymax = body_mass_g.upper,
          fill = species, alpha = as_factor(.width))
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
    scale_alpha_manual(
      values = c("0.5" = 0.5, "0.8" = 0.4, "0.9" = 0.3),
      guide = "none"
    ) +
    labs(x = "Flipper length (cm)", y = "Body mass (g)") +
    coord_cartesian(xlim = c(16, 24), ylim = c(2e3, 7e3), 
                    expand = F) +
    theme_classic()
  ) %>%
  ggdraw() +
  draw_image(
    here("Images", "penguins.png"), 
    x = 0.04, y = 0.65, 
    width = 0.6, height = 0.6*1074/1800
  )

mass_plot

mass_plot %>%
  ggsave(
    filename = "mass.pdf", 
    path = "Plots",
    device = cairo_pdf,
    height = 6, width = 10, units = "cm"
  )


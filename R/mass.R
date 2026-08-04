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
  alpha = rnorm( 1e3 , 0 , 1 ), # coefficient
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
  alpha = rgamma( 1e3 , 1.5^2 / 0.5^2 , 1.5 / 0.5^2 ), # coefficient
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
  alpha = rgamma( 1e3 , 1.5^2 / 0.5^2 , 1.5 / 0.5^2 ), # coefficient
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

# 1.6 Summary ####

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

# 1.7 Contrasts ####

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

# 1.8 Prediction ####

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

mass_alpha_plot <- mass_parameters %>%
  # filter(species != "Prior") %>%
  # droplevels() %>%
  ggplot() +
    stat_slab(
      aes(alpha, species, fill = species),
      colour = NA
    ) +
    scale_fill_manual(
      values = c(
        "Prior" = "darkgrey",
        "Adelie" = "darkorange",
        "Chinstrap" = "purple",
        "Gentoo" = "cyan4"
      ),
      guide = "none"
    ) +
    labs(x = expression("Density coefficient (g per cm"^beta*")")) +
    coord_cartesian(xlim = c(0, 5), expand = c(T, F)) +
    theme_classic() +
    theme(
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank()
    )

mass_alpha_plot

mass_alpha_plot %>%
  ggsave(
    filename = "mass_alpha.pdf", 
    path = "Plots",
    device = cairo_pdf,
    height = 6, width = 10, units = "cm"
  )


mass_beta_plot <- mass_parameters %>%
  # filter(species != "Prior") %>%
  # droplevels() %>%
  ggplot() +
    stat_slab(
      aes(beta, species, fill = species),
      colour = NA
    ) +
    scale_fill_manual(
      values = c(
        "Prior" = "darkgrey",
        "Adelie" = "darkorange",
        "Chinstrap" = "purple",
        "Gentoo" = "cyan4"
      ),
      guide = "none"
    ) +
    labs(x = "Scaling exponent") +
    coord_cartesian(xlim = c(1, 5), expand = c(T, F)) +
    theme_classic() +
    theme(
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank()
    )

mass_beta_plot

mass_beta_plot %>%
  ggsave(
    filename = "mass_beta.pdf", 
    path = "Plots",
    device = cairo_pdf,
    height = 6, width = 10, units = "cm"
  )

# 2. Pooled mass ####
# 2.1 Prior simulation ####

# given that there is non-identifiability in alpha and beta,
# we will make beta a constant. beta = 3 is the natural choice
# since mass is equivalent to volume and volume is the cube of
# height or length. See https://wikipedia.org/wiki/Allometry.

tibble(
  n = 1:1e3,
  log_alpha_mu = rnorm( 1e3 , log(1.5) , 0.4 ), # model in log space
  log_alpha_sigma = rtnorm( 1e3 , 0 , 0.1 , 0 ),
  sigma = rexp( 1e3 , 1 ) # likelihood sd
) %>%
  mutate(
    log_alpha = rnorm( n() , log_alpha_mu , log_alpha_sigma )
  ) %>%
  expand_grid(
    flipper_length_cm = penguins %>% drop_na(flipper_length_cm) %$% 
      seq( min(flipper_length_cm) , max(flipper_length_cm) , length.out = 50 )
  ) %>%
  mutate(
    alpha = exp(log_alpha),
    mu = alpha * flipper_length_cm^3,
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

# 2.2 Stan model ####

mass_pooled_model <- here("Stan", "mass_pooled.stan") %>%
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model() %T>%
  print()

mass_pooled_samples <- mass_pooled_model$sample(
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

# 2.3 Model checks ####

# R-hat
mass_pooled_samples$summary() %>%
  summarise(rhat_1.01 = mean( rhat > 1.01 ),
            rhat_mean = mean( rhat ),
            rhat_sd = sd( rhat ))

# Chains

mass_pooled_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Mass pooled model",
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
mass_pooled_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c(
      "log_alpha_mu", "log_alpha_sigma",
      "log_alpha[1]", "log_alpha[2]", 
      "log_alpha[3]", "sigma"
    ),
    grid_args = list(top = "Mass pooled model")
  )

# 2.4 Prior-posterior comparison ####

## Global
# Prior
mass_pooled_prior_global <- tibble(
  # Hyperpriors
  log_alpha_mu = rnorm( 8e3 , log(1.5) , 0.4 ), # model in log space
  log_alpha_sigma = rtnorm( 8e3 , 0 , 0.1 , 0 ),
  # Priors
  sigma = rexp( 8e3 , 1 ) # likelihood sd
) %>%
  mutate(
    # Priors depending on hyperpriors
    alpha = rnorm( n() , log_alpha_mu , log_alpha_sigma ) %>% exp()
  ) %T>%
  print()
  
# Posterior
mass_pooled_posterior_global <- mass_pooled_samples %>%
  spread_draws(log_alpha_mu, log_alpha_sigma, sigma) %>%
  mutate(
    # Predict for unobserved penguin species
    alpha = rnorm( n() , log_alpha_mu , log_alpha_sigma ) %>% exp()
  ) %T>%
  print()

# Join
mass_pooled_prior_posterior_global <- bind_rows(
    Prior = mass_pooled_prior_global,
    Posterior = mass_pooled_posterior_global %>% 
      select(-starts_with(".")),
    .id = "Distribution"
  ) %T>%
  print()

# Visualise
mass_pooled_prior_posterior_global %>%
  pivot_longer(
    cols = -Distribution,
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

## Species
# Prior
mass_pooled_prior_species <- mass_pooled_prior_global %>%
  select(alpha, sigma) %>%
  expand_grid(
    species = penguins %$% 
      levels(species) %>%
      fct()
  ) %T>%
  print()

# Posterior
mass_pooled_posterior_species <- mass_pooled_samples %>%
  recover_types(penguins) %>%
  spread_draws(log_alpha[species], sigma) %>%
  mutate(alpha = exp(log_alpha)) %>%
  select(-log_alpha) %T>%
  print()

# Join
mass_pooled_prior_posterior_species <- bind_rows(
    Prior = mass_pooled_prior_species,
    Posterior = mass_pooled_posterior_species %>% 
      select(-starts_with(".")),
    .id = "Distribution"
  ) %T>%
  print()

# Visualise
mass_pooled_prior_posterior_species %>%
  pivot_longer(
    cols = -c(Distribution, species),
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

# 2.5 Summary ####

mass_pooled_parameters <- mass_pooled_prior_posterior_global %>%
  mutate(
    species = if_else(
      Distribution == "Prior", "Prior", "Global"
    ) %>% fct()
  ) %>%
  select(species, alpha, sigma) %>%
  bind_rows(
    mass_pooled_posterior_species %>% # species priors are already captured
      select(-starts_with("."))
  ) %T>%
  print()
  
mass_pooled_summary <- mass_pooled_parameters %>%
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
    N = n(),
    .by = c(species, Parameter)
  ) %T>%
  print()

# 2.6 Contrasts ####

mass_pooled_contrasts <- mass_pooled_parameters %>%
  filter(!species %in% c("Prior", "Global")) %>%
  droplevels() %>%
  select(-sigma) %>%
  mutate(n = 1:n(), .by = species) %>% # needed to identify rows
  pivot_wider(
    names_from = species,
    values_from = alpha
  ) %>%
  mutate(
    AC_Difference = Adelie - Chinstrap,
    AC_Proportion = Adelie / Chinstrap,
    AG_Difference = Adelie - Gentoo,
    AG_Proportion = Adelie / Gentoo,
    CG_Difference = Chinstrap - Gentoo,
    CG_Proportion = Chinstrap / Gentoo
  ) %>%
  select(ends_with("Difference"), ends_with("Proportion")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Contrast", "Statistic"),
    names_sep = "_"
  ) %T>%
  print()


mass_pooled_contrasts_summary <- mass_pooled_contrasts %>%
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
    .by = c(Contrast, Statistic)
  ) %T>%
  print()

# 2.7 Prediction ####

mass_pooled_prediction <- mass_pooled_parameters %>%
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
  mutate(
    min = if_else(
      is.na(min),
      penguins %$% min(flipper_length_cm, na.rm = T),
      min
    ),
    max = if_else(
      is.na(max),
      penguins %$% max(flipper_length_cm, na.rm = T),
      max
    )
  ) %>%
  rowwise() %>%
  mutate(flipper_length_cm = list( seq(min, max, length.out = 50) )) %>%
  select(-c(min, max)) %>%
  unnest(flipper_length_cm) %>%
  mutate(
    mu = alpha * flipper_length_cm^3,
    body_mass_g = rnorm( n() , mu , sigma )
  ) %T>%
  print()

mass_pooled_prediction_summary <- mass_pooled_prediction %>%
  group_by(species, flipper_length_cm) %>%
  mean_qi(mu, body_mass_g, .width = c(.5, .8, .9)) %T>%
  print()


# 2.8 Visualisation ####

mass_pooled_plot <- (
  mass_pooled_prediction_summary %>%
  filter(species != "Prior") %>%
  droplevels() %>%
  ggplot() +
    geom_line(
      aes(flipper_length_cm, mu, colour = species)
    ) +
    geom_ribbon(
      aes(flipper_length_cm, ymin = mu.lower, ymax = mu.upper,
          fill = species, alpha = as_factor(.width))
    ) +
    geom_point(
      data = penguins %>% 
        drop_na(species, flipper_length_cm, body_mass_g),
      aes(flipper_length_cm, body_mass_g, colour = species),
      alpha = 0.5, shape = 16, size = 2
    ) +
    scale_fill_manual(
      values = c(
        "Global" = "darkgrey",
        "Adelie" = "darkorange",
        "Chinstrap" = "purple",
        "Gentoo" = "cyan4"
      ),
      guide = "none"
    ) +
    scale_colour_manual(
      values = c(
        "Global" = "darkgrey",
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

mass_pooled_plot

mass_pooled_plot %>%
  ggsave(
    filename = "mass_pooled.pdf", 
    path = "Plots",
    device = cairo_pdf,
    height = 6, width = 10, units = "cm"
  )

mass_pooled_alpha_plot <- mass_pooled_parameters %>%
  filter(species != "Prior") %>%
  droplevels() %>%
  ggplot() +
    stat_slab(
      aes(alpha, species, fill = species),
      colour = NA
    ) +
    scale_fill_manual(
      values = c(
        "Global" = "darkgrey",
        "Adelie" = "darkorange",
        "Chinstrap" = "purple",
        "Gentoo" = "cyan4"
      ),
      guide = "none"
    ) +
    scale_x_continuous(breaks = seq(0.3, 0.7, 0.1)) +
    labs(x = expression("Density coefficient (g per cm"^3*")")) +
    coord_cartesian(xlim = c(0.3, 0.7), expand = c(T, F)) +
    theme_classic() +
    theme(
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank()
    )

mass_pooled_alpha_plot

mass_pooled_alpha_plot %>%
  ggsave(
    filename = "mass_pooled_alpha.pdf", 
    path = "Plots",
    device = cairo_pdf,
    height = 6, width = 10, units = "cm"
  )

mass_pooled_contrasts_plot <- mass_pooled_contrasts %>%
  filter(Statistic == "Difference") %>%
  ggplot() +
    stat_slab(
      aes(value, Contrast),
      colour = NA, fill = "black", alpha = 0.5
    ) +
    geom_vline(xintercept = 0) +
    labs(x = expression("Density coefficient difference (g per cm"^3*")")) +
    coord_cartesian(xlim = c(-0.02, 0.06), expand = c(T, F)) +
    theme_classic() +
    theme(
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank()
    )

mass_pooled_contrasts_plot

mass_pooled_contrasts_plot %>%
  ggsave(
    filename = "mass_pooled_contrasts.pdf", 
    path = "Plots",
    device = cairo_pdf,
    height = 6, width = 10, units = "cm"
  )

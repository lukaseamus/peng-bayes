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
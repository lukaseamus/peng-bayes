####____________________________________####
#### Bayesian penguins: introduction to ####
#### Bayesian statistics in R and Stan, ####
#### Part 2: beaks                      ####
#### Luka Seamus Wright, 30 July 2026   ####
####____________________________________####

# 1. Beaks ####
# 1.1 Load data ####
require(tidyverse)
require(magrittr)
require(here)
require(palmerpenguins)
require(cowplot)

ggdraw() + draw_image(here("Images", "beak.png"))

penguins <- penguins %>% 
  mutate(
    beak_length = bill_length_mm / 10,
    beak_height = bill_depth_mm / 10
  ) %T>%
  print()

penguins %>% 
  drop_na(species, beak_length, beak_height) %>%
  ggplot() +
    geom_point(
      aes(beak_length, beak_height, colour = species),
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

tibble(
  n = 1:1e3,
  alpha = rnorm( 1e3 , 0 , 1 ), # intercept
  beta = rnorm( 1e3 , 0 , 1 ), # slope
  sigma = rexp( 1e3 , 1 ) # likelihood sd
) %>%
  expand_grid(
    beak_length = penguins %>% drop_na(beak_length) %$% 
      seq( min(beak_length) , max(beak_length) , length.out = 50 )
  ) %>%
  mutate(
    mu = alpha + beta * beak_length,
    beak_height = rnorm( n() , mu , sigma )
  ) %>%
  pivot_longer(
    cols = c(beak_height, mu),
    names_to = "Parameter"
  ) %>%
  ggplot() +
    geom_line(
      aes(beak_length, value, group = n),
      alpha = 0.05
    ) +
    facet_wrap(~ Parameter, scale = "free", nrow = 1) +
    theme_classic()

# 1.3 Stan model ####
require(cmdstanr)
require(tidybayes)

beaks_model <- here("Stan", "beaks.stan") %>%
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model() %T>%
  print()

beaks_samples <- beaks_model$sample(
  data = penguins %>%
    select(species, beak_length, beak_height) %>%
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
beaks_samples$summary() %>%
  summarise(rhat_1.01 = mean( rhat > 1.01 ),
            rhat_mean = mean( rhat ),
            rhat_sd = sd( rhat ))

# Chains
require(bayesplot)

beaks_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Beaks model",
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
beaks_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c(
      "alpha[1]", "beta[1]", 
      "alpha[2]", "beta[2]",
      "alpha[3]", "beta[3]", 
      "sigma"
    ),
    grid_args = list(top = "Beaks model")
  )
# strong correlation between alpha and beta

# Re-run prior simulation
tibble(
  n = 1:1e3,
  alpha = rnorm( 1e3 , 0 , 1 ), # intercept
  beta = rnorm( 1e3 , 0 , 1 ), # slope
  sigma = rexp( 1e3 , 1 ) # likelihood sd
) %>%
  expand_grid(
    beak_length = penguins %>% drop_na(beak_length) %>%
      mutate(beak_length = beak_length - mean(beak_length)) %$% 
      seq( min(beak_length) , max(beak_length) , length.out = 50 )
  ) %>%
  mutate(
    mu = alpha + beta * beak_length,
    beak_height = rnorm( n() , mu , sigma )
  ) %>%
  pivot_longer(
    cols = c(beak_height, mu),
    names_to = "Parameter"
  ) %>%
  ggplot() +
    geom_line(
      aes(beak_length, value, group = n),
      alpha = 0.05
    ) +
    facet_wrap(~ Parameter, scale = "free", nrow = 1) +
    theme_classic()

# Re-run model
beaks_samples <- beaks_model$sample(
  data = penguins %>%
    select(species, beak_length, beak_height) %>%
    drop_na() %>%
    mutate(beak_length = beak_length - mean(beak_length)) %>%
    compose_data(),
  chains = 8,
  parallel_chains = parallel::detectCores(),
  iter_warmup = 1e3,
  iter_sampling = 1e3,
) %T>%
  print()

# Pairs
beaks_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c(
      "alpha[1]", "beta[1]", 
      "alpha[2]", "beta[2]",
      "alpha[3]", "beta[3]", 
      "sigma"
    ),
    grid_args = list(top = "Beaks model")
  )
# weak correlation

# 1.5 Prior-posterior comparison ####

# Prior
beaks_prior <- tibble(
  alpha = rnorm( 8e3 , 0 , 1 ), # intercept
  beta = rnorm( 8e3 , 0 , 1 ), # slope
  sigma = rexp( 8e3 , 1 ) # likelihood sd
) %>%
  expand_grid(
    species = penguins %$% 
      levels(species) %>%
      fct()
  ) %T>%
  print()
  
# Posterior
beaks_posterior <- beaks_samples %>%
  recover_types(penguins) %>%
  spread_draws(alpha[species], beta[species], sigma) %T>%
  print()

# Join
beaks_prior_posterior <- bind_rows(
    Prior = beaks_prior,
    Posterior = beaks_posterior %>% select(-starts_with(".")),
    .id = "Distribution"
  ) %T>%
  print()

# Visualise
beaks_prior_posterior %>%
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

beaks_parameters <- beaks_prior_posterior %>%
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
  
beaks_summary <- beaks_parameters %>%
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

beaks_contrasts <- beaks_parameters %>%
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


beaks_contrasts_summary <- beaks_contrasts %>%
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

beaks_prediction <- beaks_prior_posterior %>%
  filter(Distribution == "Posterior") %>%
  select(-Distribution) %>%
  full_join(
    penguins %>%
      drop_na(beak_length) %>%
      mutate(beak_length = beak_length - mean(beak_length)) %>%
      group_by(species) %>%
      summarise(
        min = min(beak_length),
        max = max(beak_length)
      ),
    by = "species"
  ) %>%
  rowwise() %>%
  mutate(beak_length = list( seq(min, max, length.out = 50) )) %>%
  select(-c(min, max)) %>%
  unnest(beak_length) %>%
  mutate(
    mu = alpha + beta * beak_length,
    beak_height = rnorm( n() , mu , sigma )
  ) %T>%
  print()

beaks_prediction_summary <- beaks_prediction %>%
  group_by(species, beak_length) %>%
  mean_qi(mu, beak_height, .width = c(.5, .8, .9)) %T>%
  print()


# 1.9 Visualisation ####

beaks_plot <- (
  beaks_prediction_summary %>%
  mutate( # undo centring
    beak_length = beak_length + penguins %$% mean(beak_length, na.rm = T)
  ) %>%
  ggplot() +
    geom_point(
      data = penguins %>% 
        drop_na(species, beak_length, beak_height),
      aes(beak_length, beak_height, colour = species),
      alpha = 0.5, shape = 16, size = 2
    ) +
    geom_line(
      aes(beak_length, mu, colour = species)
    ) +
    geom_ribbon(
      aes(beak_length, ymin = mu.lower, ymax = mu.upper,
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
    labs(x = "Beak length (cm)", y = "Beak height (cm)") +
    coord_cartesian(xlim = c(3, 6), ylim = c(1, 2.5), expand = F) +
    theme_classic()
  ) %>%
  ggdraw() +
  draw_image(
    here("Images", "penguins.png"), 
    x = 0, y = 0.65, 
    width = 0.6, height = 0.6*1074/1800
  )

beaks_plot

beaks_plot %>%
  ggsave(
    filename = "beaks.pdf", 
    path = "Plots",
    device = cairo_pdf,
    height = 6, width = 10, units = "cm"
  )

beaks_contrasts_plot <- beaks_contrasts %>%
  filter(Statistic == "Difference",
         Parameter == "beta") %>%
  ggplot() +
    stat_slab(
      aes(value, Contrast),
      colour = NA, fill = "black", alpha = 0.5
    ) +
    geom_vline(xintercept = 0) +
    labs(x = "Height–length slope difference (cm per cm)") +
    coord_cartesian(xlim = c(-0.2, 0.2), expand = c(T, F)) +
    theme_classic() +
    theme(
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank()
    )

beaks_contrasts_plot

beaks_contrasts_plot %>%
  ggsave(
    filename = "beaks_contrasts.pdf", 
    path = "Plots",
    device = cairo_pdf,
    height = 6, width = 10, units = "cm"
  )

# 2. Pooled beaks ####

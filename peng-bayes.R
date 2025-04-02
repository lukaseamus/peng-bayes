####____________________________________####
#### Bayesian penguins: introduction to ####
#### Bayesian statistics in R and Stan  ####
#### Luka Seamus Wright, 3rd April 2025 ####
####____________________________________####

# See chapter 16.1 "Geometric people" in McElreath (2019) Statistical Rethinking^2 
# for an example on the relationship between human height and weight which inspired 
# this tutorial.

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
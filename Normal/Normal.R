#####     Load Packages     #####

library(rstan)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)

#####     Create Data     #####

mu_param = 10
sigma_param = 2
lambda_param = 3
N_param = 1000

ded_risk = 1000
lim_risk = 12000
ded_agg = 1500
lim_agg = 45000
sims = 1000

stan_data =
  list(
    N        = N_param,
    lambda   = lambda_param,
    y        = rlnorm(N_param, mu_param, sigma_param),
    sims     = sims,
    ded_risk = ded_risk,
    lim_risk = lim_risk,
    ded_agg  = ded_agg,
    lim_agg  = lim_agg
  )

#####     Stan Models     #####

pricing_model =
  stan_model(
    "Pricing_Model/Pricing_Model.stan"
  )

#####     Stan Sampling     #####

pricing_samples =
  sampling(
    pricing_model,
    data = stan_data,
    iter = 2000,
    warmup = 500,
    chains = 4,
    refresh = 100
  )

#####    Stan Output    #####

output = rstan::extract(pricing_samples)

technical_price = mean(output$loss)


#####     Load Packages     #####

library(rstan)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)

#####     Create Data     #####

mu_param = 1000
sigma_param = 200
N_param = 1000

stan_data =
  list(
    N = N_param,
    y = rnorm(N_param, mu_param, sigma_param)
  )

#####     Stan Models     #####

normal_model =
  stan_model(
    "Normal/Normal.stan"
  )

#####     Stan Sampling     #####

normal_samples =
  sampling(
    normal_model,
    data = stan_data,
    iter = 1000,
    chains = 4,
    refresh = 10
  )


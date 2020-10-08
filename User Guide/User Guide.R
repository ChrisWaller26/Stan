#####     Load Packages     #####

library(rstan)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)

options(mc.cores = 6)

#####     Create Data     #####

n_samples = 1e4

#####     Stan Models     #####

#### 1.1 Linear Regression ####

x_1_1 = cbind(runif(n_samples), runif(n_samples))
alpha_1_1 = 0.3
beta_1_1 = c(1.2, 1.6)
sigma_1_1 = 0.2
y_1_1 = alpha_1_1 + c(x_1_1 %*% beta_1_1) + rnorm(n_samples, 0, sigma_1_1)

linear_reg_data =
  list(
    N = n_samples,
    K = length(beta_1_1),
    y = y_1_1,
    x = x_1_1
  )

linear_reg_model =
  stan_model(
    "User Guide/1.1 Linear Regression.stan"
  )

linear_reg =
  sampling(
    linear_reg_model,
    data = linear_reg_data,
    iter = 2000,
    chains = 3,
    refresh = 50,
    warmup = 500
  )

linear_reg_out = rstan::extract(linear_reg)

linear_reg_avg = lapply(linear_reg_out, function(x) colMeans(as.matrix(x)))

linear_reg_ggdata =
  data.frame(
    beta1 = linear_reg_out$beta[,1],
    beta2 = linear_reg_out$beta[,2],
    alpha = linear_reg_out$alpha,
    sigma = linear_reg_out$sigma
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "param",
    values_to = "value"
  )

linear_reg_act =
  data.frame(
    param = c("beta1", "beta2", "alpha", "sigma"),
    value = c(beta_1_1, alpha_1_1, sigma_1_1)
  )

ggplot() + 
  geom_boxplot(
    data = linear_reg_ggdata,
    aes(
      x = factor(param),
      y = value,
      color = param
      )
    ) + 
  geom_point(
    data = linear_reg_act,
    aes(
      x = factor(param),
      y = value
    )
  ) + 
  theme(
    legend.position = "none"
  ) + 
  labs(
    title = "1.1 Linear Regression",
    y = "Value",
    x = "Parameter"
  ) +
  facet_wrap(
    ~ param,
    scales = "free"
  )

#### 1.2 The QR Reparameterization ####


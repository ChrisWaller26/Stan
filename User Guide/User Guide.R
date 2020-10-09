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

linear_reg_then = Sys.time()

linear_reg =
  sampling(
    linear_reg_model,
    data = linear_reg_data,
    iter = 2000,
    chains = 3,
    refresh = 50,
    warmup = 500
  )

linear_reg_time = difftime(Sys.time(), linear_reg_then)

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

x_1_2 = cbind(runif(n_samples), runif(n_samples))
alpha_1_2 = 0.3
beta_1_2 = c(1.2, 1.6)
sigma_1_2 = 0.2
y_1_2 = alpha_1_2 + c(x_1_2 %*% beta_1_2) + rnorm(n_samples, 0, sigma_1_2)

qr_reparam_data =
  list(
    N = n_samples,
    K = length(beta_1_2),
    y = y_1_2,
    x = x_1_2
  )

qr_reparam_model =
  stan_model(
    "User Guide/1.2 The QR Reparameterization.stan"
  )

qr_reparam_then = Sys.time()

qr_reparam =
  sampling(
    qr_reparam_model,
    data = qr_reparam_data,
    iter = 2000,
    chains = 3,
    refresh = 50,
    warmup = 500
  )

qr_reparam_time = difftime(Sys.time(), qr_reparam_then)

qr_reparam_out = rstan::extract(qr_reparam)

qr_reparam_avg = lapply(qr_reparam_out, function(x) colMeans(as.matrix(x)))

qr_reparam_ggdata =
  data.frame(
    beta1 = qr_reparam_out$beta[,1],
    beta2 = qr_reparam_out$beta[,2],
    alpha = qr_reparam_out$alpha,
    sigma = qr_reparam_out$sigma
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "param",
    values_to = "value"
  )

qr_reparam_act =
  data.frame(
    param = c("beta1", "beta2", "alpha", "sigma"),
    value = c(beta_1_2, alpha_1_2, sigma_1_2)
  )

ggplot() + 
  geom_boxplot(
    data = qr_reparam_ggdata,
    aes(
      x = factor(param),
      y = value,
      color = param
    )
  ) + 
  geom_point(
    data = qr_reparam_act,
    aes(
      x = factor(param),
      y = value
    )
  ) + 
  theme(
    legend.position = "none"
  ) + 
  labs(
    title = "1.2 QR Reparameterization",
    y = "Value",
    x = "Parameter"
  ) +
  facet_wrap(
    ~ param,
    scales = "free"
  )

#### 1.4 Robust Noise Models ####

x_1_4 = cbind(runif(n_samples), runif(n_samples))
alpha_1_4 = 2
beta_1_4 = c(7, 9)
sigma_1_4 = 1.5
nu_degf = 5 # Degrees of freedom of t-distribution
y_1_4 = alpha_1_4 + c(x_1_4 %*% beta_1_4) + rt(n_samples, nu_degf) * sigma_1_4

robust_noise_data =
  list(
    N  = n_samples,
    K  = length(beta_1_4),
    y  = y_1_4,
    x  = x_1_4,
    nu = nu_degf
  )

robust_noise_model =
  stan_model(
    "User Guide/1.4 Robust Noise Models.stan"
  )

robust_noise_then = Sys.time()

robust_noise =
  sampling(
    robust_noise_model,
    data = robust_noise_data,
    iter = 2000,
    chains = 3,
    refresh = 50,
    warmup = 500
  )

robust_noise_time = difftime(Sys.time(), robust_noise_then)

robust_noise_out = rstan::extract(robust_noise)

robust_noise_avg = lapply(robust_noise_out, function(x) colMeans(as.matrix(x)))

robust_noise_ggdata =
  data.frame(
    beta1 = robust_noise_out$beta[,1],
    beta2 = robust_noise_out$beta[,2],
    alpha = robust_noise_out$alpha,
    sigma = robust_noise_out$sigma
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "param",
    values_to = "value"
  )

robust_noise_act =
  data.frame(
    param = c("beta1", "beta2", "alpha", "sigma"),
    value = c(beta_1_4, alpha_1_4, sigma_1_4)
  )

ggplot() + 
  geom_boxplot(
    data = robust_noise_ggdata,
    aes(
      x = factor(param),
      y = value,
      color = param
    )
  ) + 
  geom_point(
    data = robust_noise_act,
    aes(
      x = factor(param),
      y = value
    )
  ) + 
  theme(
    legend.position = "none"
  ) + 
  labs(
    title = "1.4 Robust Noise Models",
    y = "Value",
    x = "Parameter"
  ) +
  facet_wrap(
    ~ param,
    scales = "free"
  )

#### 1.5 Logistic and Probit Regression ####

x_1_5 = runif(n_samples)
alpha_1_5 = 2
logit_inv_1_5 = 1/(1 + exp(-alpha_1_5))
y_1_5 = x_1_5 < logit_inv_1_5

log_and_pro_data =
  list(
    N  = n_samples,
    y  = y_1_5
  )

log_and_pro_model =
  stan_model(
    "User Guide/1.5 Logistic and Probit Regression.stan"
  )

log_and_pro_then = Sys.time()

log_and_pro =
  sampling(
    log_and_pro_model,
    data = log_and_pro_data,
    iter = 2000,
    chains = 3,
    refresh = 50,
    warmup = 500
  )

log_and_pro_time = difftime(Sys.time(), log_and_pro_then)

log_and_pro_out = rstan::extract(log_and_pro)

log_and_pro_avg = lapply(log_and_pro_out, function(x) colMeans(as.matrix(x)))

log_and_pro_ggdata =
  data.frame(
    alpha = log_and_pro_out$alpha
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "param",
    values_to = "value"
  )

log_and_pro_act =
  data.frame(
    param = c("alpha"),
    value = c(alpha_1_5)
  )

ggplot() + 
  geom_boxplot(
    data = log_and_pro_ggdata,
    aes(
      x = factor(param),
      y = value,
      color = param
    )
  ) + 
  geom_point(
    data = log_and_pro_act,
    aes(
      x = factor(param),
      y = value
    )
  ) + 
  theme(
    legend.position = "none"
  ) + 
  labs(
    title = "1.5 Logistic and Probit Regression",
    y = "Value",
    x = "Parameter"
  ) +
  facet_wrap(
    ~ param,
    scales = "free"
  )



data {
  int<lower=0> N;
  int<lower=0,upper=1> y[N];
}
parameters {
  real alpha;
}
model {
  y ~ bernoulli_logit(alpha);
}
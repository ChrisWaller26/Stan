
data {
  int<lower=0> N;
  int<lower=0> sims;
  vector[N] y;
  real<lower=0> lambda;
  real<lower=0> ded_risk;
  real<lower=0> lim_risk;
  real<lower=0> ded_agg;
  real<lower=0> lim_agg;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  y ~ lognormal(mu, sigma);
}

generated quantities {
  
  vector[sims] loss;
  real avg_loss;

  for(i in 1:sims){
    
    int loss_count = 3;
    vector[loss_count] claim;
    vector[loss_count] net_claim;
    real net_claim_tot;
    
    for(j in 1:loss_count){
      
      claim[j] = lognormal_rng(mu, sigma);
      net_claim[j] = min([max([claim[j] - ded_risk, 0]), lim_risk]);
      
    }
    
    net_claim_tot = sum(net_claim);
    
    loss[i] = min([max([net_claim_tot - ded_agg, 0]), lim_agg]);
    
  }
  
  avg_loss = mean(loss);
  
}

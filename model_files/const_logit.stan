data {
  int<lower=1> N_poll; // number of polls
  int<lower=1> N_election; // number of elections
  int<lower=1> N_days; // number of days
  vector<lower=0,upper=1>[N_election] v; // election results
  vector<lower=0,upper=1>[N_poll] y; // poll results
  
  int<lower=1,upper=N_election> r[N_poll]; // index specifying the election for each poll
  vector<lower=1>[N_poll] n; // number of respondents in each poll
}

transformed data {
  vector<lower=-2,upper=2>[N_election] logit_v;
  for (i in 1:N_election){
    logit_v[i] = logit(v[i]);
  }
}

parameters {
  real bias_a;  // mean of alpha
  real<lower=0> sigma_a; // standard deviation of alpha

  real<lower=0> sigma_tao; // standard deviation of excess variance
  
  vector[N_election] alpha; // bias component independent of time
  vector<lower=0>[N_election] tao_sqr; // poll excess variance
}

transformed parameters{
}

model {
  vector[N_poll] p;
  vector[N_poll] logit_p;
  
  // hierarchical model
  sigma_a ~ normal(0, 0.2);
  sigma_tao ~ normal(0, 0.05);
  
  bias_a ~ normal(0, 0.2);

  alpha ~ normal(bias_a, sigma_a);
  tao_sqr ~ normal(0, sigma_tao);

  
  for (i in 1:N_poll){
    logit_p[i] = logit_v[r[i]] + alpha[r[i]];
    p[i] = inv_logit(logit_p[i]);
    
    y[i] ~ normal(p[i], sqrt( p[i]*(1-p[i])/n[i] + tao_sqr[r[i]] ));
  }
}

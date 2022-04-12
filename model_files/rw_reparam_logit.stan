data {
  int<lower=1> N_poll; // number of polls
  int<lower=1> N_election; // number of elections
  int<lower=1> N_days; // number of days
  int<lower=1> N_year; // number of years
  
  vector<lower=0,upper=1>[N_election] v; // election results
  vector<lower=0,upper=1>[N_poll] y; // poll results
  int<lower=1,upper=N_election> r[N_poll]; // index specifying the election for each poll
  int<lower=1,upper=N_year> year[N_election]; // year of each election
  int<lower=1,upper=100> t_int[N_poll]; // (number of days before the election each poll was conducted)
  vector<lower=1>[N_poll] n; // number of respondents in each poll
}

transformed data{
  vector[N_days] ones;
  vector<lower=-2,upper=2>[N_election] logit_v;
  
  for (i in 1:N_election){
    logit_v[i] = logit(v[i]);
  }
  for(i in 1:N_days) ones[i] = 1;
}

parameters{
  matrix[N_election,N_days] raw_z;
  vector[N_election] raw_alpha;
  vector<lower=0>[N_election] tau_sqr;
  vector<lower=0>[N_election] gamma;
  
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_tau;
  real<lower=0> sigma_gamma;
  
  real mu_alpha;
}

transformed parameters{
  matrix[N_election,N_days] z;
  vector[N_election] alpha;
 
  for(j in 1:N_election) alpha[j] = raw_alpha[j]*sigma_alpha + mu_alpha;
  
  z[:,1] = gamma .* raw_z[:,1] + logit_v + alpha;
  for(i in 2:N_days) z[:,i] = gamma .* raw_z[:,i] + z[:,i-1];
  
}



model{
  vector[N_poll] p;
  vector[N_poll] logit_p;
  
  mu_alpha ~ normal(0, 0.2);
  sigma_alpha ~ normal(0, 0.2);
  sigma_tau ~ normal(0, 0.05);
  sigma_gamma ~ normal(0,0.05); //~\pm 2pp, expit(0 + rnorm(100,0,abs(rnorm(100,0,0.05)))) %>% quantile(probs = c(.025,.975))
  
  tau_sqr ~ normal(0, sigma_tau);
  gamma ~ normal(0,sigma_gamma);
  
  raw_alpha ~ std_normal();
  to_vector(raw_z) ~ std_normal();
  
  
  for(i in 1:N_poll){
    logit_p[i] = z[r[i],t_int[i]];
    p[i] = inv_logit( logit_p[i] );
    
    y[i] ~ normal(p[i],sqrt(p[i]*(1-p[i])/n[i] + tau_sqr[r[i]]));
  }
}

generated quantities{
  matrix[N_election,N_days] theta;

  theta = z - alpha * transpose(ones);
}



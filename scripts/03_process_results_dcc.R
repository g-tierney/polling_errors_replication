library(tidyverse)
library(rstan)
rm(list=ls())

funs <- c("mean"=mean,
          "q025"=function(x) quantile(x,probs=0.025),
          "q05"=function(x) quantile(x,probs=0.05),
          "q95"=function(x) quantile(x,probs=0.95),
          "q975"=function(x) quantile(x,probs=0.975))

logit <- function(x) log(x/(1-x))
ilogit <- function(x) 1/(1+exp(-x))

elec <- 'Presidential'
data_file <- sprintf("temp/stan_data_%s_auxiliary_2020Update.RData", tolower(elec))
load(data_file)

base_tibble <- tibble(r = 1:actual_polls_data$N_election,
                      election_identifiers = actual_polls_data$election_identifiers,
                      year = (2004 + (0:4)*4)[actual_polls_data$year],
                      state = actual_polls_data$state,
                      v = actual_polls_data$v)
results <- tibble()

results_tau <- tibble()

models <- c("rw_reparam_lpm","rw_reparam_logit","jasa_final_model","const_logit","const_lpm")

for (m in  models){
  stan_results <- list.files(paste0("stan_results/",m))

  for(f in stan_results){
    print(f)
    model_name <- m 
    load(paste0("stan_results/",model_name,"/",f))
    
    t <- str_extract(f,"_t([0-9]+)_") %>% str_remove_all("_|t") %>% as.integer()
    
    samples <- extract(fit)$alpha
    
    #for logistic models, day of error is ilogit(logit(v) + alpha) - v
    if(str_detect(model_name,"jasa|logit")){
      v_mat <- matrix(actual_polls_data_t$v,
                      nrow=dim(samples)[1],ncol=actual_polls_data_t$N_election,
                      byrow = T)

      samples <- ilogit(logit(v_mat) + samples) - v_mat
    }
    
    summaries <- sapply(funs, function(f) apply(samples,2,f))

    summaries <- as_tibble(summaries) %>%
      mutate(r = 1:actual_polls_data_t$N_election,
             election_identifiers = actual_polls_data_t$election_identifiers,
             state = actual_polls_data_t$state,
             year = (2004 + (0:4)*4)[actual_polls_data_t$year],
             t=t,
             model = model_name,
             file = f)

    results <- results %>%
      bind_rows(summaries)
    
    
    ## now excess variance parameters
    if(str_detect(model_name,"jasa|const")){
      samples <- sqrt(extract(fit)$tao_sqr)
    }else{
      samples <- sqrt(extract(fit)$tau_sqr)
    }
    
    summaries <- sapply(funs, function(f) apply(samples,2,f))
    
    summaries <- as_tibble(summaries) %>% 
      mutate(r = 1:actual_polls_data_t$N_election,
             election_identifiers = actual_polls_data_t$election_identifiers,
             state = actual_polls_data_t$state,
             year = (2004 + (0:4)*4)[actual_polls_data_t$year],
             t=t,
             model = model_name,
             file = f)
    
    results_tau <- results_tau %>% 
      bind_rows(summaries)
    
    rm("fit","actual_polls_data_t")
    gc()
  }
}

write_csv(results,"output/results_bias.csv")
write_csv(results_tau,"output/results_tau.csv")



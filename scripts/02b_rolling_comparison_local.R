library(tidyverse)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

rm(list=ls())

load("temp/stan_data_presidential_auxiliary_2020Update.RData")

logit <- function(x) log(x/(1-x))

subset_leq_t <- function(t,data=actual_polls_data){
  polls_to_keep <- which(data$t_int <= t)
  
  for(val in c("y","t","t_int","n","r")){
    data[[val]] <- data[[val]][polls_to_keep]
  }
  
  remaining_rs <- unique(data$r)
  
  #reset r if any elections now have zero polls
  data$r <- match(data$election_identifiers[data$r],data$election_identifiers[remaining_rs])
  
  #drop extra elections
  for(val in c("v","election_identifiers","year","state","logit_v")){
    data[[val]] <- data[[val]][remaining_rs]
  }
  
  #reset global variables
  data$N_poll <- length(polls_to_keep)
  data$N_election <- length(remaining_rs)
  
  data$N_poll <- length(polls_to_keep)
  data$N_days <- t
  data
}

models <- c("rw_reparam_lpm","rw_reparam_logit","jasa_final_model","const_logit","const_lpm")

# to run a specific iteration set these, then run the inner loop below
model_name <- models[1]
t <- 10


# ensure subfolders all exist
if(!file.exists("stan_results")) dir.create("stan_results")
for (m in models){
  if(!file.exists(paste0("stan_results/",m))) dir.create(paste0("stan_results/",m))
}

# execute models
for (model_name in models){
  
  print(paste0("Model: ",model_name))
  
  for (t in seq(10,10,10)) {
    #skip if you've already ran the result
    result_name <- paste0("stan_results/",model_name,"/",model_name,"_t",t,"_local.RData")
    if(file.exists(result_name)) next
    
    print("")
    print(str_c("Starting window t=",t))
    print("")
    
    # set sample frame
    actual_polls_data_t <- subset_leq_t(t,actual_polls_data)
    
    actual_polls_data_t$logit_v <- logit(actual_polls_data_t$v)
    # Fit the model to data
    
    # more iterations for smaller t
    if(t <= 30 | model_name %in% c("const_logit","const_lpm")){
      iter_bump <- 10000
    } else{
      iter_bump <- 0
    }
    
    start <- Sys.time()
    fit <- stan(file = paste0("model_files/",model_name,".stan"),
                data = actual_polls_data_t, iter = 10000 + iter_bump,
                chains = 8, cores = 8, seed = 1,
                pars = c("raw_beta","raw_alpha","z","raw_z"),include = FALSE,
                control = list(adapt_delta=0.99,max_treedepth=13)
    )
    print(sprintf("Warnings t=%s:",t))
    print(warnings())
    
    end <- Sys.time()
    print("")
    print(paste0("t=",t," took ",difftime(end,start,units = "min") %>% round(2)," min"))
    print("")
    
    #smallest ESS and Rhat
    print(summary(fit)$summary[order(summary(fit)$summary[,"n_eff"]),][1:10,])
    print(summary(fit)$summary[order(-summary(fit)$summary[,"Rhat"]),c("n_eff","Rhat")][1:10,])
    
    # Store the fit
    save(actual_polls_data_t,fit,
         file = result_name)
    rm("fit","actual_polls_data_t")
    gc(verbose = T)
  }
}


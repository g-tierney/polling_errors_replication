



library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = FALSE)

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

#get slurm id to assign t
if(Sys.getenv('SLURM_ARRAY_TASK_ID') != ""){
  slurm_id <- as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))
} else slurm_id <- 1


# create matrix of parameter values
models <- c("rw_reparam_lpm","rw_reparam_logit","jasa_final_model","const_logit","const_lpm")
ts <- c(seq(1,50,1),seq(55,100,5))
elecs <- c("senatorial","presidential")

param_matrix <- expand.grid(models=models,elecs=elecs,ts=ts) 
nrow(param_matrix)

# select this run's paramter values from the slurm_id
model_name <- param_matrix$models[slurm_id]
stan_model <- paste0("model_files/",model_name,".stan")
t <- param_matrix$ts[slurm_id]
elec <- param_matrix$elec[slurm_id]

if(elec == "senatorial"){
  data_file <- sprintf("temp/stan_data_%s_cnn_Polls.RData", tolower(elec))
} else{
  data_file <- sprintf("temp/stan_data_%s_auxiliary_2020Update.RData", tolower(elec))
}

load(data_file)

print(paste0("Starting window t=",t,", model: ",model_name))

# reset sample frame
actual_polls_data_t <- subset_leq_t(t)
actual_polls_data_t$logit_v <- logit(actual_polls_data_t$v)

# more iterations for smaller t
if(t <= 30 | model_name %in% c("const_logit","const_lpm")){
  iter_bump <- 10000
} else{
  iter_bump <- 0
}


# Fit the model to data
start <- Sys.time()
fit <- stan(file = stan_model, 
            data = actual_polls_data_t, iter = 10000 + iter_bump,
            chains = 8, cores = 8, seed = 1,
            pars = c("raw_beta","raw_alpha","z","raw_z"),include = FALSE,
            control = list(adapt_delta=0.99,max_treedepth=13)
)
end <- Sys.time()

print("")
print(paste0(" for ",model_name," t=",t," took ",round(difftime(end,start,units = "min"),2)," min."))
print(sprintf("Warnings=%s:",t))
print(warnings())

fit

#smallest ESS
summary(fit)$summary[order(summary(fit)$summary[,"n_eff"]),c("n_eff","Rhat")][1:10,]

#save
if(elec == 'presidential'){
  result_name <- paste0("/work/gt83/polling_errors/stan_results/",model_name,"/",model_name,"_t",t,"_results_pminmax.RData")
} else {
  result_name <- paste0("/work/gt83/polling_errors/stan_results/",model_name,"/",model_name,"_t",t,"_",elec,"_results_pminmax.RData")
}

print(result_name)
save(fit,actual_polls_data_t, file = result_name)



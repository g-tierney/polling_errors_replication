rm(list = ls())
library(data.table)
set.seed(2)

# Pre-process polls and remove corrupted entries
preprocess_polls_f <- function(input_polls_data) {
  input_polls_data <- input_polls_data[, y_i:= republican/(democratic+republican)]
  input_polls_data <- input_polls_data[, v_r := finalTwoPartyVSRepublican/100]
  input_polls_data <- input_polls_data[, twoparty_voteshare_error := y_i - v_r]
  input_polls_data <- input_polls_data[, days_to_election := as.integer(as.Date(electionDate)-as.Date(endDate)) ]
  input_polls_data <- input_polls_data[, n_i := round(numberOfRespondents * (democratic + republican) / 100)]
  input_polls_data <- input_polls_data[, state_year_concat := paste(as.character(state), as.character(year), sep = "_")]
  
  # Remove corrupted data entries
  #GT Comment: input_polls_data <- input_polls_data[!is.na(n_i) & !is.na(twoparty_voteshare_error) & !is.na(state_year_concat) & !is.na(days_to_election)]
  input_polls_data <- input_polls_data[!is.na(n_i) & !is.na(state_year_concat) & !is.na(days_to_election)]
  input_polls_data <- input_polls_data[days_to_election >= 1]
  # Don't consider polls too far from the election, national polls, or house election polls
  input_polls_data <- input_polls_data[(days_to_election <= 100) & (state != "USA") & (election != "House")]
  
  input_polls_data$election_type = ""
  input_polls_data[election == "Sen"]$election_type <- "Senatorial"
  input_polls_data[election == "Pres"]$election_type <- "Presidential"
  input_polls_data[election == "Gov"]$election_type <- "Gubernatorial"
  input_polls_data$election_type <- factor(
    input_polls_data$election_type,
    levels=c('Senatorial','Gubernatorial','Presidential')
  )
  input_polls_data$election <- NULL
  
  return(input_polls_data)
}


# Load 2020 poll data and results data
polls_main <- preprocess_polls_f( data.table( read.csv("data/polls_auxiliary_dataset_2020Update.csv") ) )


# Write data for each election type into a separate file
for (elec in c('Presidential','Senatorial')) {
  
  if(elec == 'Presidential') polls_main <- preprocess_polls_f( data.table( read.csv("data/polls_auxiliary_dataset_2020Update.csv") ) )
  if(elec == 'Senatorial') polls_main <- preprocess_polls_f( data.table( read.csv("data/polls_cnn_dataset.csv") ) )
  
  election_polls <- polls_main[election_type == elec]
  
  # Generate correponding vector of r[i]'s for input to Stan model
  election_polls[, election_identifier := paste(as.character(state), as.character(year), as.character(election_type), sep = "_")]
  election_agg_info <- election_polls[, list(
    num_of_polls_in_election = .N,
    v_r = head(v_r, 1)                
  ), by = election_identifier]
  election_polls[, election_index := match(election_identifier, election_agg_info$election_identifier)]
  
  # aggregate and return all the data necessary to fit the model in stan
  actual_polls_data <- list(
    N_poll = nrow(election_polls),
    N_election = nrow(election_agg_info),
    N_days = max(election_polls$days_to_election),
    v = election_agg_info$v_r,
    y = election_polls$y_i,
    r = election_polls$election_index,
    t = election_polls$days_to_election/30,
    t_int = election_polls$days_to_election,
    n = election_polls$n_i,
    election_identifiers = election_agg_info$election_identifier
  )
  
  actual_polls_data$year <-  stringr::str_sub(actual_polls_data$election_identifiers,4,7) |> as.factor() |> as.integer()
  actual_polls_data$N_year <- max(actual_polls_data$year)
  actual_polls_data$state <- stringr::str_sub(actual_polls_data$election_identifiers,1,2) 
  actual_polls_data$logit_v <- log(actual_polls_data$v/(1-actual_polls_data$v))
  
  if(elec == 'Presidential') file_name <- sprintf("temp/stan_data_%s_auxiliary_2020Update.RData", tolower(elec))
  if(elec == 'Senatorial') file_name <- sprintf("temp/stan_data_%s_cnn_Polls.RData", tolower(elec))
  
  save(actual_polls_data, file = file_name)
}

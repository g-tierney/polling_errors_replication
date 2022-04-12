########################################################################
#' 
#' Excecute project to reproduce results. 
#' 
#' Note that 02a will only run when executed within a SLURM job. The file is included
#' because that is how the paper results were created, but so that others can replicate 
#' results, file 02b runs the same code just implemented in a for loop. Running 
#' the stan code will take a while (>3 days on the Duke Computing Cluster), so the 
#' estimation results used in the paper are also included here (output/frozen_*)
#' and files 02* and 03 are not run by defult. 
#' 
########################################################################


source("scripts/01_model_prepare_input.R")
if(FALSE){
  source("scripts/02b_rolling_comparison_local.R")
  source("scripts/03_process_results_dcc.R")
}
source("scripts/04_visualize_results.R")
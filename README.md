# Introduction

Replication hub for Bias and Excess Variance in Election Polling: A Not-So-Hidden Markov Model. 

This repository contains data and code to replicate the reported results as well as a frozen copy of the results reported in the paper. The full estimation took several days on the Duke Computing Cluster, so use of the frozen results is recommended. 

Raw polling data are in `data`, which are processed into the relevant RData file by `scripts/01_model_prepare_input.R`. The models are estimated with either script `02a_rolling_comparison_cluster.R` or `02b_rolling_comparison_local.R`, 02a is the actual script used to produce the results but can only run when executed within a multicore SLURM implementation. 02b uses a simple for loop but can be run locally on any machine without a computing cluster. Both files save the resulting stanfit objects into the relevant subfolder in `stan_results`. `03_process_results.R` processes the fitted models to produce more user-friendly summaries of the posterior distributions. `04_visualize_results.R` uses those summaries to produce the figures and results reported in the manuscript.  

The file `run_project.R` will run all of the files to reproduce the figures, but by default skips the Stan estimation. Code structure was based on the implementation of [Shirani-Mehr et al. (2018)](https://5harad.com/papers/polling-errors.pdf) in their replication package here: https://github.com/stanford-policylab/polling-errors. 
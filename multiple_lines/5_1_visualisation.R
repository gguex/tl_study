#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# COMPUTE TOY EXAMPLE
# Visualisation outputs
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#--------------------------------
# Head
#--------------------------------

# Set working directory path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries
#--------------------------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)

# Load functions
source("local_functions.R")

# The folder containing pre-processed data
data_folder = "multilines_data/preprocessed_data/loop_toy_example"

# Output folder for results
out_folder = "results/loop_toy_example"

#--------------------------------
# Network parameters
#--------------------------------
# Choose number of lines
nb_lines = 3
# Choose number of stops
nb_stops = nb_lines + 1

#--------------------------------
# Algorithm parameters
#--------------------------------

# Conv threshold for iterative fitting
conv_thres_if = 0.0001
# Conv threshold
conv_thres_algo = 0.0001
# epsilon
epsilon = 1e-40
# max iteration
max_it = 1000
# print iterations
display_it = F
# number of iterations
n_test = 10
# prop limit
hyper_par = c(0.01, 0.1, 0.3, 0.5, 0.7, 0.9, 0.99)
# number of passengers
n_passengers = 2000

#--------------------------------
# Output and visualizations
#--------------------------------

# Network properties output
#--------------------------
network_prop = network_prop(nb_lines, nb_stops)
# Unlist and save variables
list2env(network_prop, .GlobalEnv)

# Alogorithm output
#------------------
res_mat = compute_toy(hyper_par, n_test, paths, n_passengers, edge_ref, sp_ref,
                      sp_edge_link, conv_thres_algo, conv_thres_if, max_it,
                      display_it)

# Results according to the prop limit parameter
mean_test = apply(res_mat, 2, mean)
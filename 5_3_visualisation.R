#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# COMPUTE TOY EXAMPLE
# Visualisation output
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# - Histogram of mean error, according to the number of lines into the network
#   (only 1 hyper parameter)

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
# below

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
n_test = 5
# prop limit
hyper_par = 0.3
# number of passengers
n_passengers = 2000
# number of cores
mc.cores = detectCores() - 2

#--------------------------------
# Output and visualizations
#--------------------------------

# Create a data frame with different number of line into the network
res_mat_line = c()
seq_lines = 2:8

for (i in seq_lines) {
  nb_lines = i
  nb_stops = i + 1
  # Network properties output
  network_prop_res = network_prop(nb_lines, nb_stops, mc.cores)
  # Unlist and save variables
  list2env(network_prop_res, .GlobalEnv)
  
  res_mat = compute_toy(hyper_par, n_test, paths, n_passengers, edge_ref, sp_ref,
                        sp_edge_link, conv_thres_algo, conv_thres_if, max_it,
                        display_it)
  
  colnames(res_mat)[1] <- paste("Lines:", i)
  res_mat_line = cbind(res_mat_line, res_mat)
}

# Add the mean and the standard deviation
mean_line = as.data.frame(colMeans(res_mat_line))
sd_line = apply(res_mat_line, 2, sd)
sd_line = sd_line/sqrt(n_test)
mean_line = cbind(seq_lines, mean_line, sd_line)
colnames(mean_line) <- c("Lines","mean_error", "sd_error")


### Best graph according to the number of passengers into the network
ggplot() +
  geom_line(data = mean_line, aes(x = Lines, y = mean_error), color = "red") +
  labs(title = paste(n_test, "iterations, parameter:", hyper_par), x = "Nb of lines into the network", y = "Mean error")
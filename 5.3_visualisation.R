#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# COMPUTE TOY EXAMPLE
# Visualisation output
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# - Histogram of mean error, according to the number of passengers travelling
#   into the network (only 1 hyper parameter)

#--------------------------------
# Head
#--------------------------------

# Set working directory path
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
nb_lines = 2:8
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
hyper_par = 0.1

#--------------------------------
# Output and visualizations
#--------------------------------

# Network properties output
#--------------------------
# network_prop_list = network_prop(nb_lines, nb_stops)
# Unlist and save variables
# list2env(network_prop_list, .GlobalEnv)

# Create a data frame with all different number of passengers into the network

res_mean = c()
seq_passengers = seq(from = 200, to = 4000, by = 200)

for (j in 1:length(nb_lines)) {
  
  res_mat = c()
  res_mat_passengers = c()
  
  network_prop_list = network_prop(nb_lines[j], nb_lines[j]+1)
  # Unlist and save variables
  list2env(network_prop_list, .GlobalEnv)
  
  for (i in seq_passengers) {
    res_mat = compute_toy(hyper_par, n_test, paths, i, edge_ref, sp_ref,
                          sp_edge_link, conv_thres_algo, conv_thres_if, max_it,
                          display_it)
    res_mat_passengers = cbind(res_mat_passengers, res_mat)
  }
  
  mean_pass = c()
  # Add the mean and the standard deviation
  mean_pass = as.data.frame(colMeans(res_mat_passengers))
  sd_pass = apply(res_mat_passengers, 2, sd)
  sd_pass = sd_pass/sqrt(n_test)
  mean_pass = cbind(seq_passengers, mean_pass, sd_pass)
  colnames(mean_pass) = c("passengers","mean_error", "sd_error")
  
  mean_pass$line[1:length(seq_passengers)] = nb_lines[j]
  
  res_mean = rbind(res_mean, mean_pass)
}

# write.csv(res_mean, "results/5_toy_ex_outputs/res_mean_passengers.csv", row.names = F)

### Best graph according to the number of passengers into the network
# only 1/3 error bar
# ggplot() +
#   geom_line(data = mean_pass, aes(x = Passengers, y = mean_error), color = "red") +
#   # geom_point(data = mean_pass[seq(1, nrow(mean_pass), by = 3),],
#   #            aes(x = Passengers, y = mean_error)) +
#   # geom_line(data = mean_pass2, aes(x = Passengers, y = mean_error)) +
#   geom_errorbar(data=mean_pass[seq(1, nrow(mean_pass), by = 3),],
#                 aes(x=Passengers, ymin=mean_error-sd_error, ymax=mean_error+sd_error), width=100) +
#   labs(title = paste0(n_test, " iterations, ", expression(theta), ": ", hyper_par), x = "Passengers into the network", y = "Mean error")

# Plot the results 2
ggplot(data=res_mean) +
  geom_ribbon(aes(x=passengers, ymin=mean_error-sd_error/sqrt(n_test), 
                  ymax=mean_error+sd_error/sqrt(n_test), group=line, 
                  fill=factor(line)), alpha=0.2) +
  geom_line(aes(x=passengers, y=mean_error, group=line, color=factor(line))) +
  labs(x="Nb passengers", y="MTE") +
  # geom_point(data=min_max,aes(x=passengers, y=mean_error, color=factor(line)))+
  labs(color = "Nb tours", fill = "Nb tours")

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
nb_lines = 2:3
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
n_test = 3
# prop limit
hyper_par = 0.1
# number of passengers
n_passengers = 1000

#--------------------------------
# Output and visualizations
#--------------------------------

# Network properties output
#--------------------------
network_prop = network_prop(nb_lines, nb_stops)
# Unlist and save variables
list2env(network_prop, .GlobalEnv)

# Create a data frame with all different number of passengers into the network
res_mat_passengers = c()
seq_passengers = seq(from = 200, to = 4000, by = 200)

for (i in seq_passengers) {
  res_mat = compute_toy(hyper_par, n_test, paths, i, edge_ref, sp_ref,
                        sp_edge_link, conv_thres_algo, conv_thres_if, max_it,
                        display_it)
  colnames(res_mat)[1] <- paste("Nb:", i)
  res_mat_passengers = cbind(res_mat_passengers, res_mat)
}


# Add the mean and the standard deviation
mean_pass = as.data.frame(colMeans(res_mat_passengers))
sd_pass = apply(res_mat_passengers, 2, sd)
sd_pass = sd_pass/sqrt(n_test)
mean_pass = cbind(seq_passengers, mean_pass, sd_pass)
colnames(mean_pass) <- c("Passengers","mean_error", "sd_error")














# Create a data frame with all different number of passengers into the network
# A continuer...
res_mat_passengers = c()
seq_passengers = seq(from = 200, to = 600, by = 200)
seq_lines = 2:3

for (j in seq_lines) {
  for (i in seq_passengers) {
    res_mat = compute_toy(hyper_par, n_test, paths, i, edge_ref, sp_ref,
                          sp_edge_link, conv_thres_algo, conv_thres_if, max_it,
                          display_it)
    colnames(res_mat)[1] = paste("Nb:", i)
    res_mat = cbind(res_mat, j)
    colnames(res_mat)[2] = paste("par", i)
    res_mat_passengers = cbind(res_mat_passengers, res_mat)
  }
}
library(tidyr)
df_transformed <- pivot_wider(res_mat_passengers, names_from =, values_from = A)

  
  # Add the mean and the standard deviation
  mean_pass = as.data.frame(colMeans(res_mat_passengers))
  sd_pass = apply(res_mat_passengers, 2, sd)
  sd_pass = sd_pass/sqrt(n_test)
  mean_pass = cbind(seq_passengers, mean_pass, sd_pass)
  colnames(mean_pass) <- c("Passengers","mean_error", "sd_error")


  as.data.frame(res_mat_passengers)
  df_new <- as.data.frame(res_mat_passengers) %>% 
    gather(key = "key", value = "value") %>% 
    separate(key, into = c("letter", "number")) %>% 
    spread(key = letter, value = value)
  
  colnames(df_new)[1] <- "letter"
  df_new <- df_new[,c(1,3,4,5,6)]
#...





















### Best graph according to the number of passengers into the network
# only 1/3 error bar
ggplot() +
  geom_line(data = mean_pass, aes(x = Passengers, y = mean_error), color = "red") +
  # geom_point(data = mean_pass[seq(1, nrow(mean_pass), by = 3),],
  #            aes(x = Passengers, y = mean_error)) +
  # geom_line(data = mean_pass2, aes(x = Passengers, y = mean_error)) +
  geom_errorbar(data=mean_pass[seq(1, nrow(mean_pass), by = 3),],
                aes(x=Passengers, ymin=mean_error-sd_error, ymax=mean_error+sd_error), width=100) +
  labs(title = paste0(n_test, " iterations, ", expression(theta), ": ", hyper_par), x = "Passengers into the network", y = "Mean error")


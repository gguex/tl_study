#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# COMPUTE TOY EXAMPLE
# Visualisation output
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# - Histogram of mean error, according to the number of lines into the network
#   (possible to have more than 1 hyper parameter)

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
n_test = 50
# prop limit
# hyper_par = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1,
#               0.12, 0.14, 0.16, 0.18, 0.2,
#               0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
hyper_par = seq(0.001,0.009, 0.001)

# number of passengers
# n_passengers = 2000
nb_pass_stop = 5
# number of cores
mc.cores = detectCores() - 2
# lines
seq_lines = 2:8

#--------------------------------
# Output and visualizations
#--------------------------------

# Create a data frame with different number of line into the network
res_mat_line = c()

for (j in 1:length(hyper_par)) {
  for (i in seq_lines) {
    nb_lines = i
    nb_stops = i + 1
    # Network properties output
    network_prop_res = network_prop(nb_lines, nb_stops)
    # Unlist and save variables
    list2env(network_prop_res, .GlobalEnv)
    
    # Passengers depend on the number of stops
    n_passengers = sum(paths)*nb_pass_stop
    
    res_mat = compute_toy(hyper_par[j], n_test, paths, n_passengers, edge_ref,
                          sp_ref, sp_edge_link, conv_thres_algo, conv_thres_if, 
                          max_it, display_it)
    colnames(res_mat) = paste0("L",i,"P",hyper_par[j])
    
    res_mat_line = cbind(res_mat_line, res_mat)
  }
  
  # Add the mean and the standard deviation
  mean_line = as.data.frame(colMeans(res_mat_line))
  sd_line = apply(res_mat_line, 2, sd)
  sd_line = sd_line/sqrt(n_test)
  mean_line = cbind(seq_lines, mean_line, sd_line)
  mean_line$param = row.names(mean_line)
  colnames(mean_line) <- c("Lines", "mean_error", "sd_error","param")
  mean_line$param = substr(mean_line$param, 4, 7)
}
# write.csv(res_mat_line, "res_line_param_small.csv", row.names = F)
# write.csv(mean_line, "mean_line_param_small.csv", row.names = F)

# Read saved data
mean_line = read.csv("results/5_toy_ex_outputs/mean_line_param.csv")
# Delete parm. 0
mean_line = mean_line[-(1:7),]

# Extract min/max
min_max = as.data.frame(mean_line %>% group_by(Lines) %>% top_n(-1, mean_error))


# Plot the results
ggplot() +
  geom_line(data=mean_line,
            aes(x=Lines, y=mean_error, group=param, color=param)) +
  # labs(title=paste(n_test, "iterations, parameter:", paste(hyper_par,collapse = ', ')), x = "Nb of lines into the network", y = "Mean error")
  labs(title=paste(n_test, "tests,", n_passengers, "passengers"), 
       x = "Nb of lines into the network", y = "Mean error") +
  geom_errorbar(data=mean_line,
                aes(x=Lines, ymin=mean_error-sd_error, ymax=mean_error+sd_error), width=0.1) +
  scale_x_continuous(limits = c(min(mean_line$Lines)-1, max(mean_line$Lines))+0.5,
                     breaks = pretty(mean_line$Lines)) +
  labs(color=expression(theta)) 

mean_line_tot_order$param = as.numeric(mean_line_tot_order$param)


# Plot the results 2
ggplot(data=mean_line) +
  geom_ribbon(aes(x=param, ymin=mean_error-sd_error/sqrt(n_test), 
                  ymax=mean_error+sd_error/sqrt(n_test), group=Lines, 
                  fill=factor(Lines)), alpha=0.2) +
  geom_line(aes(x=param, y=mean_error, group=Lines, color=factor(Lines))) +
  labs(x=expression(theta), y="MTE") +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  geom_point(data=min_max,aes(x=param, y=mean_error, color=factor(Lines)))+
  labs(color = "Nb tours", fill = "Nb tours")

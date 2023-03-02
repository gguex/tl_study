#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# COMPUTE TOY EXAMPLE
# Visualisation output
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# - Line plot of the mean error, according to the theta hyper parameter and the
#   number of passengers depends on number of line tours

#--------------------------------
# Head
#--------------------------------

# Libraries
#--------------------------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(reshape2)

# Load functions
source("local_functions.R")

#--------------------------------
# Network parameters
#--------------------------------
# Choose tours
seq_lines = 2:8
# Choose prop limit theta
hyper_par = c(seq(0.001,0.009, 0.001), seq(0.01,0.1, 0.01), seq(0.12,0.2, 0.02),
              0.25, 0.3, 0.35, seq(0.4,1, 0.1))
# number of passengers per node (n_passengers = sum(paths)*nb_pass_stop)
nb_pass_stop = 5

#--------------------------------
# Algorithm parameters
#--------------------------------

# Conv threshold for iterative fitting
conv_thres_if = 1e-5
# Conv threshold
conv_thres_algo = 1e-8
# epsilon
epsilon = 1e-40
# max iteration
max_it = 1000
# print iterations
display_it = F
# number of iterations
n_test = 10
# multicors
mc.cores = detectCores() - 2

#--------------------------------
# Output and visualizations
#--------------------------------

# Create a data frame with different number of line into the network
res_mat_line = c()

for (j in 1:length(hyper_par)) {
  print(paste("hyperpar", j))
  for (i in seq_lines) {
    nb_lines = i
    nb_stops = i + 1
    # Network properties output
    network_prop_res = network_prop(nb_lines, nb_stops, mc.cores )
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
  mean_line$param = substring(mean_line$param, 4)
}
# write.csv(res_mat_line, "results/toys_plots/res_line_param.csv", row.names = F)
# write.csv(mean_line, "results/toys_plots/mean_line_param.csv", row.names = F)
# Read saved data
# mean_line = read.csv("results/toys_plots/mean_line_param.csv")

mean_line$param = as.numeric(mean_line$param)

# Extract min/max
min_max = as.data.frame(mean_line %>% group_by(Lines) %>% top_n(-1, mean_error))

# Plot the results
ggplot(data=mean_line) +
  geom_ribbon(aes(x=param, ymin=mean_error-sd_error/sqrt(n_test), 
                  ymax=mean_error+sd_error/sqrt(n_test), group=Lines, 
                  fill=factor(Lines)), alpha=0.2) +
  geom_line(aes(x=param, y=mean_error, group=Lines, color=factor(Lines))) +
  labs(x=expression(theta), y="MTE") +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  geom_point(data=min_max,aes(x=param, y=mean_error, color=factor(Lines)))+
  labs(color = "p", fill = "p")


### Old visu
# Plot the results
# ggplot() +
#   geom_line(data=mean_line,
#             aes(x=Lines, y=mean_error, group=param, color=param)) +
#   # labs(title=paste(n_test, "iterations, parameter:", paste(hyper_par,collapse = ', ')), x = "Nb of lines into the network", y = "Mean error")
#   labs(title=paste(n_test, "tests,", n_passengers, "passengers"), 
#        x = "Nb of lines into the network", y = "Mean error") +
#   geom_errorbar(data=mean_line,
#                 aes(x=Lines, ymin=mean_error-sd_error, ymax=mean_error+sd_error), width=0.1) +
#   scale_x_continuous(limits = c(min(mean_line$Lines)-1, max(mean_line$Lines))+0.5,
#                      breaks = pretty(mean_line$Lines)) +
#   labs(color=expression(theta)) 
# 
# mean_line_tot_order$param = as.numeric(mean_line_tot_order$param)

#-------------------------------------------------------------------------------
#
# Loop the algorithm from toy example data
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# COMPUTE TOY EXAMPLE
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
# Choose number of crossing stops
cross_stop = 2
# Choose number of passengers in the network (with normal distribution)
# nb_passengers = 6000
# Total number of stops *2 (back and forth)
nb_stops_tot = nb_lines*nb_stops*2

# Intermediate outputs
########## 1. ##########
# Stops' list
stops = stops_list(nb_lines, nb_stops)
########## 2. ##########
name_stops = name_stops_function(stops)
########## 3. ##########
# adj = adj_function(stops, name_stops)
adj_list = adj_function2(stops, name_stops)
adj = adj_list$adj_w + adj_list$adj_b
########## 3. ##########
# paths = paths_function(nb_stops_tot, name_stops, cross_stop)
line_mbrshps = interaction(name_stops$V1, name_stops$V2)
tour_mbrshps = name_stops$V1
sp_data = build_sp_data(line_mbrshps, tour_mbrshps, adj_list$adj_w, 
                        adj_list$adj_b)
edge_ref = sp_data$edge_ref
sp_edge_link = sp_data$sp_edge_link
sp_ref = sp_data$sp_ref
paths = 1*as.matrix(sparseMatrix(i=sp_ref[, 1], j=sp_ref[, 2], 
                                 dims=c(nb_stops_tot, nb_stops_tot)))
########## 4. ##########
# go_in_out = get_passengers(nb_passengers)[[1]]
# paths_passengers = get_passengers(nb_passengers)[[2]]
# n_poisson(nb_stops_tot, name_stops, cross_stop, lambda)
########## 5. ##########



#--------------------------------
# Algorithm parameters
#--------------------------------

# Conv threshold for iterative fitting
conv_thres_if = 0.0001
# Conv threshold
conv_thres_algo = 0.0001
# epsilon
epsilon = 1e-50
# max iteration
max_it = 1000
# print iterations
display_it = F
# number of iterations
n_test = 50
# --- prop_limit
hyper_par = c(0.05, 0.1, 0.3, 0.5, 0.7, 0.9, 0.99)
# hyper_par = c(1:15)/20
# hyper_par = 0.3
# lambda
lambda = 4
# n passengers
n_passengers = 2000

#--------------------------------
# Process
#--------------------------------
# initialization
# res_chi2 = matrix(0, nrow = n_test, ncol = length(hyper_par))

compute_toy = function(hyper_par, n_test, paths, n_passengers, edge_ref, sp_ref,
                       sp_edge_link, conv_thres_algo, conv_thres_if, max_it,
                       display_it){
  res_mat = matrix(0, nrow = n_test, ncol = length(hyper_par))
  mean_test = c()
    
  for (i in 1:n_test) {
    set.seed(i)
    # passengers_rho = get_passengers(nb_passengers)
    # paths_passengers = n_poisson(paths, lambda)
    paths_passengers = n_multin(paths, n_passengers, epsilon=epsilon)
    paths_passengers = paths_passengers / sum(paths_passengers)
    x_btw = compute_x_from_n(paths_passengers, edge_ref, sp_ref, 
                             sp_edge_link)$x_btw
    flow_l_in = rowSums(paths_passengers) + colSums(x_btw)
    flow_l_out = colSums(paths_passengers) + rowSums(x_btw)
    
    for (j in 1:length(hyper_par)) {
  
      # --- Run the algorithm
      n_mat = compute_origin_destination(flow_l_in,
                                         flow_l_out,
                                         edge_ref,
                                         sp_ref, 
                                         sp_edge_link,
                                         min_p_ntwk=hyper_par[j],
                                         conv_thres_algo=conv_thres_algo,
                                         conv_thres_if=conv_thres_if,
                                         epsilon=epsilon,
                                         max_it=max_it, 
                                         display_it=display_it)
      stat_out = abs(n_mat - paths_passengers)
      stat_out = stat_out[!is.infinite(stat_out)]
      stat_out = sum(stat_out)
  
      res_mat[i,j] = stat_out
      
      cat("Iteration n°:", i, "Parameter n°:", j, "done.\n")
    }
  }
  return(res_mat)
}
#--------------------------------
# Outputs and visualizations
#--------------------------------
mean_test = apply(res_mat, 2, mean)
# res_chi2 = res_mat

mean_test = as.data.frame(mean_test)
rownames(mean_test) = as.character(hyper_par)

# Best hyperparameter
paste0("Best parameter: ",rownames(mean_test)[which.min(mean_test$mean_test)])


# Histograms
df = as.data.frame(res_mat)
rownames(df) = as.character(paste("it",1:n_test))
colnames(df) = as.character(paste("par",hyper_par))
dfcolnames = colnames(df)

# Histograms
out<-list()
for (i in 1:length(hyper_par)){
  x = df[,i]
  if (i == which.min(mean_test$mean_test)) {
    out[[i]] <- ggplot(data.frame(x), aes(x)) + geom_histogram(bins = 20, color="black", fill="#addd8e") + xlim(min(df), max(df)) + labs(x = paste0("Best: ", dfcolnames[i], " Mean = ", round(mean_test$mean_test[i]*100)/100))
  }
  else {
    out[[i]] <- ggplot(data.frame(x), aes(x)) + geom_histogram(bins = 20, color="black") + xlim(min(df), max(df)) + labs(x = paste0(dfcolnames[i], " Mean = ", round(mean_test$mean_test[i]*100)/100))
  }
}
do.call(grid.arrange, out)

# Plots
mdf = df
mdf$par = row.names(mdf)
mdf = melt(mdf, id=c("par"))

ggplot(mdf, aes(x=variable, y=value, color=par)) +
  # geom_smooth(show.legend = FALSE, fullrange=TRUE) +
  geom_point(show.legend = FALSE) +
  labs(x ="Parameter", y = "% of error") +
  stat_summary(aes(y = value,group=1), fun=mean, colour="blue", geom="line",group=1)

 # Save data
# write.csv(df, "100_iterations_lambda_12.csv")

# Network drawing
set.seed(1)
paths_passengers = n_multin(paths, n_passengers, epsilon=1e-2)
plot_flow_graph(adj, paths_passengers)

### Graph based on decreasing passengers
# Fixed parameter
res_mat = compute_toy(hyper_par, n_test, paths, n_passengers, edge_ref, sp_ref,
                      sp_edge_link, conv_thres_algo, conv_thres_if, max_it,
                      display_it)

# Create a data frame with all different number of passengers into the network
res_mat_passengers = c()
hyper_par = 0.3
for (i in seq(from = 200, to = 4000, by = 200)) {
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
mean_pass = cbind(seq(from = 200, to = 4000, by = 200), mean_pass, sd_pass)
colnames(mean_pass) <- c("Passengers","mean_error", "sd_error")


ggplot(mean_pass, aes(x = Passengers, y = mean_error)) + 
  geom_point()

ggplot(mean_pass, aes(x = Passengers, y = mean_error)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = mean_error - 2*sd_error,
                    ymax = mean_error + 2*sd_error), width = 0.1)



# 1 of 3
mean_pass[seq(1, nrow(mean_pass), by = 3),]

### Best graph according to the number of passengers into the network
# only 1/3 errorbar
ggplot() +
  geom_line(data = mean_pass, aes(x = Passengers, y = mean_error), color = "red") +
  # geom_point(data = mean_pass[seq(1, nrow(mean_pass), by = 3),],
  #            aes(x = Passengers, y = mean_error)) +
  # geom_line(data = mean_pass2, aes(x = Passengers, y = mean_error)) +
  
  geom_errorbar(data=mean_pass[seq(1, nrow(mean_pass), by = 3),],
                aes(x=Passengers, ymin=mean_error-sd_error, ymax=mean_error+sd_error), width=0.1) +
  labs(title = paste(n_test, "iterations, parameter:", hyper_par), x = "Passengers into the network", y = "Mean error")





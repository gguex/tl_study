#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# COMPUTE TOY EXAMPLE
# Visualisation outputs (3)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# - Histogram according to several prop limit parameter
# - Scatter plot according to several prop limit parameter
# - Network drawing

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
n_test = 50
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
mean_test = as.data.frame(mean_test)
rownames(mean_test) = as.character(hyper_par)

# Best hyperparameter
paste0("Best parameter: ",rownames(mean_test)[which.min(mean_test$mean_test)])


# Histograms
df = as.data.frame(res_mat)
rownames(df) = as.character(paste("it",1:n_test))
colnames(df) = as.character(paste("par",hyper_par))
dfcolnames = colnames(df)

out = list()
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

# Scatter plot
mdf = df
mdf$par = row.names(mdf)
mdf = melt(mdf, id=c("par"))

ggplot(mdf, aes(x=variable, y=value, color=par)) +
  # geom_smooth(show.legend = FALSE, fullrange=TRUE) +
  geom_point(show.legend = FALSE) +
  labs(title = paste(nb_lines, "lines network", n_test, "iterations"), x ="Parameter", y = "% of error") +
  stat_summary(aes(y = value,group=1), fun=mean, colour="blue", geom="line",group=1)

# Network drawing
set.seed(1)
paths_passengers = n_multin(paths, n_passengers, epsilon=1e-2)
plot_flow_graph(adj, paths_passengers)

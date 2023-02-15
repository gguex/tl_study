#-------------------------------------------------------------------------------
#
# Preprocess data and save it
#
#-------------------------------------------------------------------------------

#--------------------------------
# Head
#--------------------------------

#------ Set working directory path
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("local_functions.R")

#--------------------------------
# Parameters
#--------------------------------

# Result folder
result_folder = "results/iteration_plots"

# Choose number of lines
n_lines_vec = c(2, 3, 4)

#--------------------------------
# Code
#--------------------------------

# --- loop

n_lines = n_lines_vec[1]
n_stops = n_lines + 1

# --- Network creation

# Create the network
network_data = network_prop(n_lines, n_stops)
adj = network_data$adj

# Stop names 
if(nb_lines == 2){
  stop_names = c("A1", "A2", "A3", 
                 "B1", "B2", "B3", 
                 "C1", "C2", "C3", 
                 "D1", "D2", "D3")
} else {
  stop_names = NULL
}
colnames(adj) = stop_names
rownames(adj) = stop_names

g = graph_from_adjacency_matrix(adj)
plot(g)


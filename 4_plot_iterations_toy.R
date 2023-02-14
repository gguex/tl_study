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

# Choose number of lines
nb_lines = 2
# Choose number of stops
nb_stops = nb_lines + 1
# Choose number of passengers in the network (with normal distribution)
nb_passengers = 1000
# Total number of stops *2 (back and forth)
nb_stops_tot = nb_lines*nb_stops*2

# Conv threshold for iterative fitting
conv_thres_if = 1e-5
# Conv threshold
conv_thres_algo = 1e-8
# proportional limit 
min_p_ntwk = 0.1
# epsilon
epsilon = 1e-40
# max iterations
max_it = 15
# max iterations for iterative fitting
max_it_if = 400

# Seed
set.seed(35)

#--------------------------------
# Code
#--------------------------------

# --- Network creation

# Stop names 
stop_names = c("A1", "A2", "A3",
               "B1", "B2", "B3",
               "C1", "C2", "C3",
               "D1", "D2", "D3")

stop_names = rep(1:4, each=3)

# Create the network
network_prop = network_prop(nb_lines, nb_stops)
edge_ref = network_prop$edge_ref
sp_ref = network_prop$sp_ref
sp_edge_link = network_prop$sp_edge_link
paths = network_prop$paths
adj = network_prop$adj
colnames(adj) = stops
rownames(adj) = stops

# --- Flow creation

n_real = n_multin(paths, nb_passengers)
n_real = n_real / sum(n_real)
x_btw = compute_x_from_n(n_real, edge_ref, sp_ref, sp_edge_link)$x_btw
flow_l_in = rowSums(n_real) + colSums(x_btw)
flow_l_out = colSums(n_real) + rowSums(x_btw)

# --- Run the algo 

n_mat_list = compute_origin_destination(flow_l_in,
                                        flow_l_out,
                                        edge_ref,
                                        sp_ref, 
                                        sp_edge_link, 
                                        min_p_ntwk=min_p_ntwk,
                                        conv_thres_algo=conv_thres_algo,
                                        conv_thres_if=conv_thres_if,
                                        epsilon=epsilon,
                                        max_it=max_it,
                                        max_it_if=max_it_if,
                                        return_it=T)

# --- Graphs 

# Layout
df_line = igraph::as_data_frame(graph_from_adjacency_matrix(adj, 
                                                            weighted = TRUE))
layout = layout_nicely(graph_from_data_frame(df_line))

# 
plot_flow_graph(adj, n_real, layout)

n_mat = n_mat_list[[1]]
graph_mat = as.matrix(n_mat)
rownames(graph_mat) = rownames(adj)
colnames(graph_mat) = colnames(adj)
plot_flow_graph(adj, graph_mat, layout, v_names=stop_names, main="LABGP", 
                e_line_size=1, e_flow_size=6, v_label_size=2)

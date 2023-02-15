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
nb_lines = 2
# Choose number of stops
nb_stops = nb_lines + 1
# Choose number of passengers in the network (with normal distribution)
nb_passengers = 100
# Total number of stops *2 (back and forth)
nb_stops_tot = nb_lines*nb_stops*2

# Conv threshold for iterative fitting
conv_thres_if = 1e-6
# Conv threshold
conv_thres_algo = 1e-8
# proportional limit 
min_p_ntwk = 0.1
# epsilon
epsilon = 1e-40
# max iterations
max_it = 16
# max iterations for iterative fitting
max_it_if = 1000

# Seed
set.seed(10)

# It list
it_list = c(2, 4, 7, 11, 16)

#--------------------------------
# Code
#--------------------------------

# --- Network creation

# Stop names 
stop_names = c("A1", "A2", "A3", 
               "B1", "B2", "B3", 
               "C1", "C2", "C3", 
               "D1", "D2", "D3")

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
rownames(adj) = NULL
colnames(adj) = NULL
df_line = igraph::as_data_frame(graph_from_adjacency_matrix(adj, 
                                                            weighted = TRUE))

#layout = layout_nicely(graph_from_data_frame(df_line))

lay_g = graph_from_data_frame(df_line)
v_order = as.numeric(V(lay_g)$name)
layout = matrix(c(1, 2,
                  2, 2,
                  2, 1,
                  3, 1,
                  3, 2,
                  4, 2,
                  4, 3,
                  3, 3,
                  3, 4,
                  2, 4,
                  2, 3,
                  1, 3), 12, 2, byrow = T)
layout = layout[v_order, ]

e_line_size = 0.2
e_flow_size = 3
v_label_size = 1

pdf(paste0(result_folder, "/iterations.pdf"))
par(mfrow=c(2,3))
plot_flow_graph(adj, n_real, layout, v_names=stop_names, main="Real", 
                e_line_size=e_line_size, e_flow_size=e_flow_size, 
                v_label_size=v_label_size)

for(it in it_list){
  n_mat = n_mat_list[[it]]
  x_r_btw = compute_x_from_n(n_mat, edge_ref, sp_ref, sp_edge_link)$x_btw
  flow_r_in = rowSums(n_mat) + colSums(x_r_btw)
  flow_r_out = colSums(n_mat) + rowSums(x_r_btw)
  error = sum(abs(n_mat - n_real)) / sum(n_real)
  error_in = sum(abs(flow_l_in - flow_r_in)) / sum(flow_l_in)
  error_out = sum(abs(flow_l_out - flow_r_out)) / sum(flow_l_out)
  error_m = mean(c(error_in, error_out))
  plot_flow_graph(adj, n_mat, layout, v_names=stop_names, 
                  main=paste0("\n\nIt=", it - 1, "\nError=", 
                              format(error, digits=3),"\nMargins error=", 
                              round(error_m, digits=3)), 
                  e_line_size=e_line_size, e_flow_size=e_flow_size, 
                  v_label_size=v_label_size)
}
dev.off()



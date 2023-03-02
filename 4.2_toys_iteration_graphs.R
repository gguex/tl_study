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
result_folder = "results/toys_graphs"

# Choose number of lines
nb_lines = 2
# Choose number of stops
nb_stops = nb_lines + 1
# Choose number of passengers in the network (with normal distribution)
nb_passengers = 50
# Total number of stops *2 (back and forth)
nb_stops_tot = nb_lines*nb_stops*2

# Conv threshold for iterative fitting
conv_thres_if = 1e-6
# Conv threshold
conv_thres_algo = 1e-10
# proportional limit 
min_p_ntwk = 0.001
# epsilon
epsilon = 1e-40
# max iterations
max_it = 15
# max iterations for iterative fitting
max_it_if = 1000

# Seed
set.seed(30)

# It list
it_list = c(1, 2, 4, 7, 15)

#--------------------------------
# Code
#--------------------------------

# --- Network creation

# Stop names 
if(nb_lines == 2){
  stop_names = c("A1", "A2", "A3", 
                 "B1", "B2", "B3", 
                 "C1", "C2", "C3", 
                 "D1", "D2", "D3")
} else {
  stop_names = NULL
}

# Create the network
network_data = network_prop(nb_lines, nb_stops)
edge_ref = network_data$edge_ref
sp_ref = network_data$sp_ref
sp_edge_link = network_data$sp_edge_link
paths = network_data$paths
adj = network_data$adj

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

layout = layout_nicely(graph_from_data_frame(df_line))

if(nb_lines == 2){
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
}


e_line_size = 0.02
e_flow_size = 3
v_size = 20
v_label_size = 0.00001

pdf(paste0(result_folder, "/iterations.pdf"))
par(mfrow=c(2,3))
plot_flow_graph(adj, n_real, layout, v_names=stop_names, main="Real", 
                e_line_size=e_line_size, e_flow_size=e_flow_size, 
                v_label_size=v_label_size, v_size=v_size)

it_list[it_list > length(n_mat_list)] = length(n_mat_list)
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
                  main=paste0("\n\nIt=", it, "\nMTE=", 
                              format(error, digits=3),"\nMME=", 
                              round(error_m, digits=3)), 
                  e_line_size=e_line_size, e_flow_size=e_flow_size, 
                  v_label_size=v_label_size, v_size=v_size)
}
dev.off()



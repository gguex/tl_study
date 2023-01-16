#------ Testing the 2 lines

library(igraph)
source("local_functions_v2.R")

nb_lines = 2
nb_stops = 3
cross_stop = 2
nb_stops_tot = nb_lines*nb_stops*2

stops = stops_list(nb_lines, nb_stops)
name_stops = name_stops_function(stops)
adj = adj_function(stops, name_stops)
paths = paths_function(nb_stops_tot, name_stops, cross_stop)
edge_ref = edge_ref_p_mat_sp_ref(adj)[[1]]
p_mat = edge_ref_p_mat_sp_ref(adj)[[2]]
sp_ref = edge_ref_p_mat_sp_ref(adj)[[3]]


# Testing with a case

set.seed(3)
n_real = n_multin(paths, 100)
n_real = n_real / sum(n_real)
x_btw = compute_x_from_n(n_real, edge_ref, sp_ref, p_mat)$x_btw
rho_in = rowSums(n_real) + colSums(x_btw)
rho_out = colSums(n_real) + rowSums(x_btw)

# 0) param

epsilon = 1e-10
conv_thres_if = 0.00001
prop_limit = 0.3

# INIT

s_it_mat = paths
sigma_in = rho_in
sigma_out = rho_out

# ------- ALGO (to iterate)

n_it = 30

# Graphics 

df_line = igraph::as_data_frame(graph_from_adjacency_matrix(adj, weighted = TRUE))
layout = layout_nicely(graph_from_data_frame(df_line))

par(mfrow=c(3,3))
plot_flow_graph(adj, n_real, layout, main="Real")
disp_it = ceiling(n_it / 8)

for(it in 1:n_it){
  
  prev_sigma_in = sigma_in
  prev_sigma_out = sigma_out
  
  # 1) Computing if
  
  b = rep(1, ncol(s_it_mat))
  converge_if = F
  while(!converge_if){
    b_old = b
    a = (sigma_in + epsilon) / colSums(t(s_it_mat + epsilon) * b)
    b = (sigma_out + epsilon) / colSums((s_it_mat + epsilon) * a)
    if(sum(abs(b_old - b)) < conv_thres_if){
      converge_if = T
    }
  }
  n_mat = t(b * t(a * (s_it_mat + epsilon)))
  
  # 2) Compute the flow
  
  n = length(rho_in)
  sp_order_ref = mapply(function(i, j) i+n*(j-1), sp_ref[, 1], sp_ref[, 2])
  where_edge_btw = as.logical(edge_ref[, 3])
  p_btw_mat = p_mat[, where_edge_btw]
  edge_btw_ref = edge_ref[where_edge_btw, 1:2]
  
  sp_flow_vec = as.vector(n_mat)[sp_order_ref]
  # Compute the flow on edges
  btw_edge_flow = as.vector(t(p_btw_mat) %*% sp_flow_vec)
  # Compute the in-out between flow on nodes 
  x_btw = sparseMatrix(i=edge_btw_ref[, 1], j=edge_btw_ref[, 2], 
                       x=btw_edge_flow, dims=c(n, n))
  
  # 3) flow on nodes
  
  node_in_btw = colSums(x_btw)
  node_out_btw = rowSums(x_btw)
  
  # 4) limits on nodes
  
  # Copy the vector
  allowed_in_btw = node_in_btw
  allowed_out_btw = node_out_btw
  # Replace the overflowing value with the limit
  allowed_in_btw[allowed_in_btw > rho_in*(1 - prop_limit)] =
    rho_in[allowed_in_btw > rho_in*(1 - prop_limit)]*(1 - prop_limit)
  allowed_out_btw[allowed_out_btw > rho_out*(1 - prop_limit)] =
    rho_out[allowed_out_btw > rho_out*(1 - prop_limit)]*(1 - prop_limit)
  
  sum((allowed_in_btw - node_in_btw)^2)
  sum((allowed_out_btw - node_out_btw)^2)
  
  # 5) limits on edges
  
  # Compute the proportion of allowed flow
  p_allowed_in = allowed_in_btw / (node_in_btw + epsilon)
  p_allowed_out = allowed_out_btw / (node_out_btw + epsilon)
  p_allowed_in[p_allowed_in == 0] = 1
  p_allowed_out[p_allowed_out == 0] = 1
  # Compute the allowed flow on edges
  p_allowed_min = mapply(
    function(i, j) min(p_allowed_out[i], p_allowed_in[j]), 
    edge_btw_ref[ ,1], edge_btw_ref[,2])
  allowed_edge_flow = p_allowed_min * btw_edge_flow
  
  # 6) update the flow entering and leaving the network
  
  # Set the allowed flow in a matrix
  allowed_flow_mat = sparseMatrix(i=edge_btw_ref[, 1], j=edge_btw_ref[, 2],
                                  x=allowed_edge_flow, dims=c(n, n))
  # Update with margins
  sigma_in = rho_in - colSums(allowed_flow_mat)
  sigma_out = rho_out - rowSums(allowed_flow_mat)
  
  # 7) compute the reducing factor
  
  # Compute the excess proportion of flow
  p_to_red = (btw_edge_flow - allowed_edge_flow) / (btw_edge_flow + epsilon)
  # Compute the maximum reduction needed on the shortest-path list
  max_p_to_red = apply(t(p_btw_mat) * p_to_red, 2, max)
  # Convert it to a reduction factor and transform it in a s,t matrix
  red_mat = sparseMatrix(i=sp_ref[, 1], j=sp_ref[, 2], 
                         x=(1-max_p_to_red), dims=c(n, n))
  
  
  # 8) compute the updated version of origin-destination affinity matrix
  
  s_it_mat = as.matrix(s_it_mat * red_mat)
  
  # --- Compute indices and display graph
  
  perc_diff = abs(n_mat  - n_real) / n_real
  mean_perc_diff = mean(perc_diff[!is.infinite(perc_diff)])
  
  err_in = abs(rho_in - (prev_sigma_in + node_in_btw)) / rho_in
  mean_err_in = mean(err_in[!is.na(err_in)])
  
  err_out = abs(rho_out - (prev_sigma_out + node_out_btw)) / rho_out
  mean_err_out = mean(err_out[!is.na(err_out)])

  if(it %% disp_it == 0 || it == n_it){
    plot_flow_graph(adj, n_mat, layout, 
                    main=paste0("it=", it, 
                               " | err_in_out=", round(mean_err_in, 3),
                               "/", round(mean_err_out, 3),
                               " | diff=", round(mean_perc_diff, 3)))
  }
}

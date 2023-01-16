#------ Set working directory path

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#------ Testing the 2 lines

library(igraph)
source("local_functions.R")

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
prop_limit = 0.285

# INIT

s_mat = paths

# ------- ALGO (to iterate)

n_it = 30

# Graphics 

df_line = igraph::as_data_frame(graph_from_adjacency_matrix(adj, weighted = TRUE))
layout = layout_nicely(graph_from_data_frame(df_line))

par(mfrow=c(3,3))
plot_flow_graph(adj, n_real, layout, main="Real")
disp_it = ceiling(n_it / 8)

# --- ALGO

# --- Get the network structure 

# The number of stops 
n = length(rho_in)
# The reference of transfer edges
where_edge_btw = as.logical(edge_ref[, 3])
# The list of in-out nodes for transfer edges 
edge_btw_ref = edge_ref[where_edge_btw, 1:2]
# The shortest-path - edge matrix retrained on transfer edges
p_btw_mat = p_mat[, where_edge_btw]
# The shortest-path order reference
sp_order_ref = mapply(function(i, j) i+n*(j-1), sp_ref[, 1], sp_ref[, 2])

# If s_mat is not given, build it
if(is.null(s_mat)){
  # The matrix of admissible sp 
  s_mat = 1*as.matrix(sparseMatrix(i=sp_ref[, 1], j=sp_ref[, 2], 
                                   dims=c(n, n)))
}
# A stop without transfer edges
trans_edge = edge_ref[edge_ref[,3] == 1, ]
node_ref = setdiff(1:n, unique(c(trans_edge[,1], trans_edge[,2])))[1]

# The st-distribution reference matrix
g_ref = s_mat / sum(s_mat)
# The evolving in and out distribution
sigma_in = rho_in / sum(rho_in)
sigma_out = rho_out / sum(rho_out)

for(it in 1:n_it){
  
  prev_sigma_in = sigma_in
  prev_sigma_out = sigma_out
  
  # 1 --- Iterative fitting 
  
  psi = rep(1, ncol(g_ref))
  converge_if = F
  while(!converge_if){
    # Saving old b results
    psi_old = psi
    # Compute new a and b
    phi = (sigma_in + epsilon) / colSums(t(g_ref + epsilon) * psi)
    psi = (sigma_out + epsilon) / colSums((g_ref + epsilon) * phi)
    # Checking for convergence
    if(sum(abs(psi_old - psi)) < conv_thres_if){
      converge_if = T
    }
  }
  # Building f_mat
  f_mat = t(psi * t(phi * (g_ref + epsilon)))
  # Building the n_mat
  n_mat = f_mat * rho_in[node_ref] / sigma_in[node_ref]
  
  # 2 --- Update sigma_in, sigma_out
  
  # Get the flow as vector in the admissible shortest-path order
  sp_flow_vec = as.vector(n_mat)[sp_order_ref]
  # Compute the flow on edges
  btw_edge_flow = as.vector(t(p_btw_mat) %*% sp_flow_vec)
  
  # Compute the in-out between flow on nodes 
  x_btw = sparseMatrix(i=edge_btw_ref[, 1], j=edge_btw_ref[, 2], 
                       x=btw_edge_flow, dims=c(n, n))
  node_in_btw = colSums(x_btw)
  node_out_btw = rowSums(x_btw)
  
  # Compute unscaled sigmas
  unscaled_sigma_in = rho_in - node_in_btw
  unscaled_sigma_out = rho_out - node_out_btw
  unscaled_sigma_in[unscaled_sigma_in < prop_limit*rho_in] = 
    prop_limit*rho_in[unscaled_sigma_in < prop_limit*rho_in]
  unscaled_sigma_out[unscaled_sigma_out < prop_limit*rho_out] = 
    prop_limit*rho_out[unscaled_sigma_out < prop_limit*rho_out]
  
  # Scale them
  sigma_in = unscaled_sigma_in / sum(unscaled_sigma_in)
  sigma_out = unscaled_sigma_out / sum(unscaled_sigma_out)
  
  # 3 --- Update g_ref
  
  # Compute the ratio of flow
  ratio_in = (node_in_btw / ((1-prop_limit)*rho_in + epsilon))
  ratio_out = (node_out_btw / ((1-prop_limit)*rho_out + epsilon))
  # Compute the ratio of flow on edges
  ratio_edge_btw = mapply(
    function(i, j) max(ratio_in[i], ratio_out[j]), 
    edge_btw_ref[ ,1], edge_btw_ref[,2])
  # Compute the path max ratio
  path_max_ratio = apply(t(p_btw_mat) * ratio_edge_btw, 2, max)
  path_max_ratio[path_max_ratio < 1] = 1
  # Compute the phi, psi scaling factor
  scaling_phi_psi = mapply(function(i, j) phi[i]*psi[j], 
                           sp_ref[,1], sp_ref[,2])
  # Compute the updated g_vec
  g_ref_vec = sp_flow_vec / path_max_ratio / scaling_phi_psi
  g_ref_vec = g_ref_vec / sum(g_ref_vec)
  # Back to g_ref
  g_ref = as.matrix(sparseMatrix(i=sp_ref[, 1], j=sp_ref[, 2], 
                                 x=g_ref_vec, dims=c(n, n)))
  
  # --- Compute indices and display graph
  
  perc_diff = abs(n_mat  - n_real) / n_real
  mean_perc_diff = mean(perc_diff[!is.infinite(perc_diff)])
  
  err_in = abs(rho_in - (prev_sigma_in*rho_in[node_ref]/prev_sigma_in[node_ref] + node_in_btw)) / rho_in
  mean_err_in = mean(err_in[!is.na(err_in)])
  
  err_out = abs(rho_out - (prev_sigma_out*rho_in[node_ref]/prev_sigma_in[node_ref] + node_out_btw)) / rho_out
  mean_err_out = mean(err_out[!is.na(err_out)])

  if(it %% disp_it == 0 || it == n_it){
    graph_mat = as.matrix(n_mat)
    rownames(graph_mat) = rownames(adj)
    colnames(graph_mat) = colnames(adj)
    plot_flow_graph(adj, graph_mat, layout, 
                    main=paste0("it=", it, 
                               " | err_in_out=", round(mean_err_in, 3),
                               "/", round(mean_err_out, 3),
                               " | diff=", round(mean_perc_diff, 3)))
  }
}

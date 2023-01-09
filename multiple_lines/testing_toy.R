#------ Testing the 2 lines

library(igraph)
source("local_functions_v2.R")

nb_lines = 2
nb_stops = 3
cross_stop = 2

stops = stops_list(nb_lines, nb_stops)
name_stops = name_stops_function(stops)
adj = adj_function(stops, name_stops)
paths = paths_function(nb_stops_tot, name_stops, cross_stop)
edge_ref = edge_ref_p_mat_sp_ref(adj)[[1]]
p_mat = edge_ref_p_mat_sp_ref(adj)[[2]]
sp_ref = edge_ref_p_mat_sp_ref(adj)[[3]]


# Testing with a case

set.seed(1)
n_real = n_poisson(s_init, 1)
n_real = n_real / sum(n_real)
x_btw = compute_x_from_n(n_real, edge_ref, sp_ref, p_mat)$x_btw
rho_in = rowSums(n_real) + colSums(x_btw)
rho_out = colSums(n_real) + rowSums(x_btw)

# 0) param

epsilon = 1e-10
conv_thres_if = 0.00001
prop_limit = 0.1

# 1) Computing if

s_it_mat = paths
sigma_in = rho_in
sigma_out = rho_out

b = rep(1, ncol(n_mat))
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

# Display the diff
round(n_mat, 4) / n_real

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
node_in_btw = colSums(x_btw)
node_out_btw = rowSums(x_btw)

# 3) limits

# Copy the vector
allowed_in_btw = node_in_btw
allowed_out_btw = node_out_btw
# Replace the overflowing value with the limit
allowed_in_btw[allowed_in_btw > rho_in*(1 - prop_limit)] =
  rho_in[allowed_in_btw > rho_in*(1 - prop_limit)]*(1 - prop_limit)
allowed_out_btw[allowed_out_btw > rho_out*(1 - prop_limit)] =
  rho_out[allowed_out_btw > rho_out*(1 - prop_limit)]*(1 - prop_limit)


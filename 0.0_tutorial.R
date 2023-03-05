# -- Load all functions

source("local_functions.R")

# -- Build the network data

# The line memberships of nodes
line_mbrshps = c(1, 1, 1, 2, 2, 2)

# The adjacency matrix within lines
adj_w = matrix(c(0, 1, 0, 0, 0, 0,
                 0, 0, 1, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 
                 0, 0, 0, 0, 1, 0,
                 0, 0, 0, 0, 0, 1,
                 0, 0, 0, 0, 0, 0), 6, 6, byrow=T)

# The adjacency matrix between lines
adj_b = matrix(c(0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 1, 0,
                 0, 0, 0, 0, 0, 0, 
                 0, 0, 0, 0, 0, 0,
                 0, 1, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0), 6, 6, byrow=T)

# Prepare the shortest path data
sp_data = build_sp_data(line_mbrshps, 
                        tour_mbrshps=line_mbrshps, 
                        adj_w, 
                        adj_b)
sp_data

# Matrix of permitted paths
permitted_paths = as.matrix(sparseMatrix(sp_data$sp_ref[, 1], 
                                         sp_data$sp_ref[, 2], 
                                         dims=c(6, 6)))
permitted_paths

# -- Draw a random flow 

# A random vector with the size of possible paths
random_vec = round(runif(sum(permitted_paths), 1, 1000))

# Fill possible paths
n_real = permitted_paths
n_real[n_real] = random_vec
n_real

# -- Compute the embarkment and disembarkment counts

# Compute the flow on transfer edges
x_btw = compute_x_from_n(n_real, 
                         sp_data$edge_ref, 
                         sp_data$sp_ref, 
                         sp_data$sp_edge_link)$x_btw

# Compute the embarkment counts
flow_l_in = rowSums(n_real) + colSums(x_btw)
flow_l_in
# Compute the disembarkment counts
flow_l_out = colSums(n_real) + rowSums(x_btw)
flow_l_out

# -- Estimation of the flow

n_alg = compute_origin_destination(flow_l_in,
                                   flow_l_out,
                                   sp_data$edge_ref,
                                   sp_data$sp_ref, 
                                   sp_data$sp_edge_link, 
                                   min_p_ntwk=0.001, 
                                   display_it=F)

# Compute the MTE
mean_transport_error = sum(abs(n_alg - n_real)) / sum(n_real)
# Compute the MME
x_btw_alg = compute_x_from_n(n_alg, 
                             sp_data$edge_ref, 
                             sp_data$sp_ref, 
                             sp_data$sp_edge_link)$x_btw
mean_margin_error = sum(abs(flow_l_in - rowSums(n_alg) - colSums(x_btw_alg))) + 
  sum(abs(flow_l_out - colSums(n_alg) - rowSums(x_btw_alg))) / 
  (2 * sum(flow_l_in))

n_alg
n_real
mean_transport_error
mean_margin_error

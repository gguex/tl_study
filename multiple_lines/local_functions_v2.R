#-------------------------------------------------------------------------------
#
# Functions for transportation lines algorithms 
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Needed libraries
#-------------------------------------------------------------------------------

library(igraph)
library(Matrix)



#-------------------------------------------------------------------------------
# Function: build_network_structure()
#
# Description:  This function compute the network structure of a multiple line
#               transportation network. It uses a matrix of pedestrian time to 
#               build transfer edges, and make sure lines from same tour are not 
#               connected.
#
# In:
# - line_mbr: A n-length vector containing line memberships of stops.
# - tour_mbr: A n-length vector containing tour memberships of stops.
# - dist_mat: A (n x n) pedestrian time matrix between stops.
# - dist_thres: A scalar threshold on the pedestrian time for considering stops 
#             linked.
# Out:
# - adj_w:    A (n x n) adjacency matrix within transportation lines.
# - adj_b:    A (n x n) adjacency matrix between transportation lines.
#-------------------------------------------------------------------------------

build_network_structure = function(line_mbr, tour_mbr, dist_mat, dist_thres){
  
  # --- Get number of stops and make levels for line_mbr and tour_mbr
  
  # Get the number of stops 
  n = length(line_mbr)
  # Get levels of lines
  lines_lvl = as.factor(line_mbr)
  # Get levels of tour
  tour_lvl = as.factor(tour_mbr)
  
  # --- Build A_W
  
  # Make links to the next stop
  adj_w = matrix(0, n, n)
  for(i in 1:(n-1)){
    if(lines_lvl[i] == lines_lvl[i + 1]) adj_w[i, i + 1] = 1
  }
  
  # --- Build A_B
  
  # With distance threshold
  adj_b = 1*(dist_mat < dist_thres)
  # Removing diagonal
  diag(adj_b) = 0
  # Removing same tour links
  for(t_lvl in levels(tour_lvl)){
    adj_b[tour_lvl == t_lvl, tour_lvl == t_lvl] = 0
  }
  
  # --- Return the results
  
  return(list(adj_w=adj_w, adj_b=adj_b))
  
}


#-------------------------------------------------------------------------------
# Function: build_sp_data()
#
# Description:  This function computes all the shortest-path data for a 
#               transportation network. It make sure the saved shortest-path 
#               gives a better traveling time compared to pedestrian time, and 
#               that these shortest-paths are not "incoherent", regarding the 
#               network structure.
#
# In:  
# - line_mbr: A n-length vector containing line memberships of stops.
# - tour_mbr  A n-length vector containing tour memberships of stops.
# - travel_t: A n-length vector giving time needed to reach next stop.
# - wait_t:   A n-length vector giving time needed to enter a line at each stop.
# - dist_mat: A (n x n) pedestrian time matrix between stops.
# - adj_w:    A (n x n) adjacency matrix within transportation lines.
# - adj_b:    A (n x n) adjacency matrix between transportation lines. 
# Out:
# - edge_ref: A (m x 3) matrix, giving the edge starting node index, ending 
#             node index, and a boolean indicating if this is an transfer edge.
# - sp_ref:   A (n_sp x 2) matrix, giving the source-target node pair for each 
#             admissible shortest-path.
# - p_mat:    A (n_sp x m) sparse shortest-path - edges matrix, 
#             with s_{ij} = TRUE iff edge j is in shortest-path i.
#-------------------------------------------------------------------------------

build_sp_data = function(line_mbr, tour_mbr, travel_t, wait_t, dist_mat, 
                         adj_w, adj_b){
  
  # --- Get number of stops and make levels for line_mbr and tour_mbr
  
  # Get the number of stops 
  n = length(line_mbr)
  # Get levels of lines
  lines_lvl = as.factor(line_mbr)
  # Get levels of tour
  tour_lvl = as.factor(tour_mbr)
  
  # --- Compute the traveling time with the line network 
  
  # Replace potential NA with large values
  travel_t[is.na(travel_t)] = 1e10
  # Adjacency matrix for traveling time
  adj_travel_time = adj_w * travel_t + t(t(adj_b) * wait_t)
  # The graph for traveling
  travel_time_g = graph_from_adjacency_matrix(adj_travel_time, 
                                              mode="directed", 
                                              weighted=T)
  # Travel time between stops
  travel_time_mat = distances(travel_time_g, mode="out")
  
  # --- Compute the edge reference matrix
  
  # The edge reference 
  edge_ref = which((adj_w + adj_b) == 1, arr.ind=T)
  is_transfer_edge = as.logical(mapply(function(i, j) adj_b[i, j], 
                                       edge_ref[,1], edge_ref[,2]))
  edge_ref = cbind(edge_ref, is_transfer_edge)
  n_edges = dim(edge_ref)[1]
  
  # --- Compute shortest-path data
  
  # Create the graph 
  full_g = graph_from_adjacency_matrix(adj_w + adj_b, mode="directed")
  
  # Make the containers for results
  sp_ref = c()
  
  ##--- MOD
  #p_mat = c()
  i_loc = c()
  j_loc = c()
  n_admissible_path = 1
  ##--- MOD
  
  # Loop on pairs of nodes
  for(i in 1:n){
    cat("Node: ", i, "; ")
    for(j in 1:n){
      
      # Check if the path is shorter with transport line (except line travel), 
      # and if i not j
      if( ((travel_time_mat[i, j] < dist_mat[i, j]) | (adj_w[i, j] == 1)) & 
          (i!=j) ){
        
        # Get the shortest path
        sp = get.shortest.paths(full_g, i, j)$vpath[[1]]
        
        # If it exists 
        if(length(sp) > 1){
          # And if it does not start or end with a transfer edge
          if( !(adj_b[sp[length(sp) - 1], sp[length(sp)]] | 
                adj_b[sp[1], sp[2]]) ){
            
            # Get the index of edge in sp
            index_sp_edge = mapply(
              function(a, b) which((edge_ref[,1] == a) & (edge_ref[,2] == b)), 
              sp[-length(sp)], sp[-1])
            
            # Check if there are no consecutive transfer edge
            if(!any(head(is_transfer_edge[index_sp_edge],-1) & 
                    tail(is_transfer_edge[index_sp_edge], -1))){
              
              # Save the shortest path
              sp_ref = rbind(sp_ref, c(i, j))
              
              ##--- MOD
              #sp_edge_vec = rep(0, n_edges)
              #sp_edge_vec[index_sp_edge] = 1
              #p_mat = rbind(p_mat, sp_edge_vec)
              i_loc = c(i_loc, rep(n_admissible_path, length(index_sp_edge)))
              j_loc = c(j_loc, index_sp_edge)
              n_admissible_path = n_admissible_path + 1
              ##--- MOD
              
            }
            
          }
        }
        
      }
      
    }
  }
  
  ##--- MOD
  p_mat = sparseMatrix(i_loc, j_loc)
  ##--- MOD
  
  # --- Return the results
  
  return(list(edge_ref=edge_ref, sp_ref=sp_ref, p_mat=p_mat))
  
}


#-------------------------------------------------------------------------------
# Function: build_in_out_flow 
#
# Description:  This function compute a corrected version of ingoing and 
#               outgoing flow inside the different lines, in order that the 
#               balance is correct.
#
# In:
# - line_mbr: A n-length vector containing line memberships for stops.
# - flow_in:  A n-length vector containing measured flow entering lines.
# - flow_out: A n-length vector containing measured flow leaving lines.
# Out:
# - rho_in:   A n-length vector of corrected flow entering lines.
# - rho_out:  A n-length vector of corrected flow leaving lines.
#-------------------------------------------------------------------------------

build_in_out_flow = function(line_mbr, flow_in, flow_out){
  
  # --- Get number of stops and make levels for line_mbr
  
  # Get the number of stops 
  n = length(line_mbr)
  # Get levels of lines
  lines_lvl = as.factor(line_mbr)
  
  # --- Correct the flow on every line
  
  # Vectors to store the corrected flow values 
  rho_in = rep(0, n)
  rho_out = rep(0, n)
  # Loop on lines
  for(l_lvl in levels(lines_lvl)){
  
    # Compute the flow balance and the cumulative sum
    line_flow_in = flow_in[lines_lvl == l_lvl]
    line_flow_out = flow_out[lines_lvl == l_lvl]
    line_balance = line_flow_in - line_flow_out
    line_flow = cumsum(line_balance)
    
    # The quantity to correct
    correction = line_flow[length(line_flow)]
    # We reduce (or raise, if negative correction) the flow in
    line_rho_in = line_flow_in
    line_rho_in[-length(line_rho_in)] = line_rho_in[-length(line_rho_in)] - 
      correction / (2*(length(line_rho_in) - 1))
    # We raise (or reduce, if negative correction) the flow out
    line_rho_out = line_flow_out 
    line_rho_out[-1] = line_rho_out[-1] + 
      correction / (2*(length(line_rho_out) - 1))
    
    # Check if there is any negative value, and correct it if it's the case
    if(any(line_rho_in < 0)){
      line_rho_out[line_rho_in < 0] = line_rho_out[line_rho_in < 0] - 
        line_rho_in[line_rho_in < 0]
      line_rho_in[line_rho_in < 0] = 0
    }
    if(any(line_rho_out < 0)){
      line_rho_in[line_rho_out < 0] = line_rho_in[line_rho_out < 0] - 
        line_rho_out[line_rho_out < 0]
      line_rho_out[line_rho_out < 0] = 0
    }
    
    # Add the corrected flow for line to the vector of global corrected flow 
    rho_in[lines_lvl == l_lvl] = line_rho_in
    rho_out[lines_lvl == l_lvl] = line_rho_out
  }
  
  # --- Return the results
  
  return(list(rho_in=rho_in, rho_out=rho_out))
  
}


#-------------------------------------------------------------------------------
# Function: compute_origin_destination
#
# Description:  This function compute the origin-destination matrix from a 
#               graph with multiple lines structure, an affinity matrix between 
#               stops, and the flow going in and out at each line stops.
#
# In:
# - rho_in:   A n-length vector of flow entering lines.
# - rho_out:  A n-length vector of flow leaving lines.
# - edge_ref: A (m x 3) matrix, giving the edge starting node index, ending 
#             node index, and a boolean indicating if this is an transfer edge.
# - sp_ref:   A (n_sp x 2) matrix, giving the source-target node pair for each 
#             admissible shortest-path.
# - p_mat:    A (n_sp x m) shortest-path - edges matrix, with p_{ij} = 1 iff
#             edge j is in shortest-path i, 0 otherwise.
# - s_mat:    A (n x n) optional affinity matrix between stops. If None is given
#             this matrix will set to 1 for each pair having an admissible 
#             shorest-path (default = NULL).
# - smooth_limit:     An boolean indicating if the algorithm should use the 
#                     smooth limit for between-line flow (default = F).
# - exp_lambda:       The exponential law parameter for the smooth limit
#                     (default = 10).
# - prop_limit:       The minimum percentage of flow newly entering in network, 
#                     used only if smooth_limit=F (default = 0.1).
# - conv_thres_if:    A threshold for the convergence of the iterative fitting 
#                     algorithm (default = 1e-5).
# - conv_thres_algo:  A threshold for the convergence of the whole algorithm 
#                     (default = 1e-5).
# - epsilon:          A small number, added in iterative fitting to make sure 
#                     the components are not null (default = 1e-40).
# - max_it:           The maximum number of iterations.
# Out:
# - n_mat:            A (n x n) matrix containing the origin-destination flow
#                     when using the multiple lines network.
#-------------------------------------------------------------------------------

compute_origin_destination = function(rho_in, rho_out, edge_ref, sp_ref, p_mat, 
                                      s_mat=NULL, smooth_limit=F, exp_lambda=10,
                                      prop_limit=0.1, conv_thres_if=1e-5,
                                      conv_thres_algo=1e-5, epsilon=1e-40,
                                      max_it=200){
  
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
  
  # --- Algorithm 
  
  # The evolving affinity matrix
  s_it_mat = s_mat
  # The initial value for n_mat (has no effect, only to create variable)
  n_mat = matrix(1e5, n, n)
  # The evolving in and out distribution
  sigma_in = rho_in
  sigma_out = rho_out
  # A boolean for convergence
  converge_algo = F
  # Iteration counter
  it_algo = 1
  
  while(!converge_algo & it_algo <= max_it){
    
    # --- Save old values
    
    n_mat_old = n_mat
    s_it_mat_old = s_it_mat
    sigma_in_old = sigma_in
    sigma_out_old = sigma_out
    
    # --- Iterative fitting 
    
    # n_mat = s_it_mat + epsilon
    # converge_if = F
    # while(!converge_if){
    #   # Saving old results
    #   n_mat_if_old = n_mat 
    #   # Normalizing by row
    #   n_mat = n_mat * sigma_in / rowSums(n_mat + epsilon)
    #   # Normalizing by columns 
    #   n_mat = t(t(n_mat) * sigma_out / colSums(n_mat + epsilon))
    #   # Checking for convergence
    #   if(sum(abs(n_mat_if_old - n_mat)) < conv_thres_if){
    #     converge_if = T
    #   }
    # }
    
    # -- NEW VERSION 
    
    b = rep(1, ncol(n_mat))
    converge_if = F
    while(!converge_if){
      # Saving old b results
      b_old = b
      # Compute new a and b
      a = (sigma_in + epsilon) / colSums(t(s_it_mat + epsilon) * b)
      b = (sigma_out + epsilon) / colSums((s_it_mat + epsilon) * a)
      # Checking for convergence
      if(sum(abs(b_old - b)) < conv_thres_if){
        converge_if = T
      }
    }
    # Building n_mat
    n_mat = t(b * t(a * (s_it_mat + epsilon)))
    
    # --- Find between flow for edges and nodes
    
    # Get the flow as vector in the admissible shortest-path order
    sp_flow_vec = as.vector(n_mat)[sp_order_ref]
    # Compute the flow on edges
    btw_edge_flow = as.vector(t(p_btw_mat) %*% sp_flow_vec)
    
    # Compute the in-out between flow on nodes 
    x_btw = sparseMatrix(i=edge_btw_ref[, 1], j=edge_btw_ref[, 2], 
                         x=btw_edge_flow, dims=c(n, n))
    node_in_btw = colSums(x_btw)
    node_out_btw = rowSums(x_btw)
    
    # --- Compute the allowed flow on nodes
    
    # If smooth limit
    if(smooth_limit){
      # Compute the exponential law
      allowed_in_btw = rho_in * 
        (1 - exp(-exp_lambda * node_in_btw / (rho_in + epsilon)))
      allowed_out_btw = rho_out * 
        (1 - exp(-exp_lambda * node_out_btw / (rho_out + epsilon)))
      # If this is more than identity, set it to indentity  
      allowed_in_btw[allowed_in_btw > node_in_btw] = 
        node_in_btw[allowed_in_btw > node_in_btw]
      allowed_out_btw[allowed_out_btw > node_out_btw] = 
        node_out_btw[allowed_out_btw > node_out_btw]
      # Else, set the limit to a crisp limit 
    } else {
      # Copy the vector
      allowed_in_btw = node_in_btw
      allowed_out_btw = node_out_btw
      # Replace the overflowing value with the limit
      allowed_in_btw[allowed_in_btw > rho_in*(1 - prop_limit)] =
        rho_in[allowed_in_btw > rho_in*(1 - prop_limit)]*(1 - prop_limit)
      allowed_out_btw[allowed_out_btw > rho_out*(1 - prop_limit)] =
        rho_out[allowed_out_btw > rho_out*(1 - prop_limit)]*(1 - prop_limit)
    }
    
    # --- Compute the allowed flow on edge
    
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
    
    # --- Update the flow entering and leaving the network
    
    # Set the allowed flow in a matrix
    allowed_flow_mat = sparseMatrix(i=edge_btw_ref[, 1], j=edge_btw_ref[, 2],
                                    x=allowed_edge_flow, dims=c(n, n))
    # Update with margins
    sigma_in = rho_in - colSums(allowed_flow_mat)
    sigma_out = rho_out - rowSums(allowed_flow_mat)
    
    # --- Compute the reducing factor and update the affinity matrix
    
    # Compute the excess proportion of flow
    p_to_red = (btw_edge_flow - allowed_edge_flow) / (btw_edge_flow + epsilon)
    # Compute the maximum reduction needed on the shortest-path list
    max_p_to_red = apply(t(p_btw_mat) * p_to_red, 2, max)
    # Convert it to a reduction factor and transform it in a s,t matrix
    red_mat = sparseMatrix(i=sp_ref[, 1], j=sp_ref[, 2], 
                           x=(1-max_p_to_red), dims=c(n, n))
    # Compute the updated version of origin-destination affinity matrix
    s_it_mat = as.matrix(s_it_mat * red_mat)
    
    # --- Check for convergence and iterate
    
    # Compute iteration statistics
    diff = sum(abs(n_mat_old - n_mat))
    diff_s_it = sum(abs(s_it_mat_old - s_it_mat))
    diff_sigma_in = sum(abs(sigma_in_old - sigma_in))
    diff_sigma_out = sum(abs(sigma_out_old - sigma_out))
    couple = which(red_mat == (1 - max(max_p_to_red)), arr.ind=T)[1,]
    
    # Print iteration statistics
    if (display_it) {
      cat("It", it_algo, ": diff =", diff, ", diff_si =", diff_s_it, 
          ", diff_in =", diff_sigma_in, ", diff_out =", diff_sigma_out, 
          ", to_red_max =", max(max_p_to_red), ", which =", couple,"\n")
    }
    
    
    # Check for convergence
    if(diff < conv_thres_algo){
      converge_algo = T
    }
    
    # Iteration
    it_algo = it_algo + 1
  }
  
  # --- Return results
  
  return(n_mat)
  
}

#-----------------
#--- NEW VERSION
#-----------------

compute_origin_destination2 = function(rho_in, rho_out, edge_ref, sp_ref, p_mat, 
                                       s_mat=NULL, prop_limit=0.1, conv_thres_if=1e-5,
                                       conv_thres_algo=1e-5, epsilon=1e-40,
                                       max_it=200, display_it=T){
  
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
  
  # --- Algorithm 
  
  # The st-distribution reference matrix
  g_ref = s_mat / sum(s_mat)
  # The initial value for n_mat (has no effect, only to create variable)
  f_mat = matrix(1e5, n, n)
  # The evolving in and out distribution
  sigma_in = rho_in / sum(rho_in)
  sigma_out = rho_out / sum(rho_out)
  # A boolean for convergence
  converge_algo = F
  # Iteration counter
  it_algo = 1
  
  while(!converge_algo & it_algo <= max_it){
    
    # --- Save old values
    
    f_mat_old = f_mat
    g_ref_old = g_ref
    sigma_in_old = sigma_in
    sigma_out_old = sigma_out
    
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
    ratio_in = node_in_btw / (prop_limit*rho_in + epsilon)
    ratio_out = node_out_btw / (prop_limit*rho_out + epsilon)
    # Compute the ratio of flow on edges
    ratio_edge_btw = mapply(
      function(i, j) min(ratio_in[i], ratio_out[j]), 
      edge_btw_ref[ ,1], edge_btw_ref[,2])
    
    # Compute the path max ratio
    path_max_ratio = apply(t(p_btw_mat) * ratio_edge_btw, 2, max)
    path_max_ratio[path_max_ratio < 1] = 1
    # Convert it to a st-matrix
    st_ratio = sparseMatrix(i=sp_ref[, 1], j=sp_ref[, 2], 
                            x=path_max_ratio, dims=c(n, n))
    # Compute the updated n_mat
    updated_n_mat = n_mat / st_ratio
    updated_n_mat[is.infinite(updated_n_mat)] = 0
    # Update g_ref
    unscaled_g_ref = t(t(updated_n_mat / (phi + epsilon)) / (psi + epsilon))
    g_ref = unscaled_g_ref / sum(unscaled_g_ref)
    
    # --- Check for convergence and iterate
    
    # Compute iteration statistics
    diff_f = sum(abs(f_mat_old - f_mat))
    diff_g = sum(abs(g_ref_old - g_ref))
    diff_in = sum(abs(sigma_in_old - sigma_in))
    diff_out = sum(abs(sigma_out_old - sigma_out))
    couple = which(st_ratio == max(st_ratio), arr.ind=T)[1,]
    
    # Print iteration statistics
    if (display_it) {
      cat("It", it_algo, ": diff_f =", diff_f, ", diff_g =", diff_g, 
          ", diff_in =", diff_in, ", diff_out =", diff_out, 
          ", to_red_max =", max(st_ratio), ", where =", couple,"\n")
    }
    
    
    # Check for convergence
    if(diff_f < conv_thres_algo){
      converge_algo = T
    }
    
    # Iteration
    it_algo = it_algo + 1
  }
  
  # --- Return results
  
  return(n_mat)
  
}

#-------------------------------------------------------------------------------
# Function: compute_x_from_n 
#
# Description:  This function compute the flow matrix on edges from the 
#               origin-destination matrix and the network structure.
#
# In:
# - n_mat:    A (n x n) matrix containing the origin-destination flow.
# - edge_ref: A (m x 3) matrix, giving the edge starting node index, ending 
#             node index, and a boolean indicating if this is an transfer edge.
# - sp_ref:   A (n_sp x 2) matrix, giving the source-target node pair for each 
#             admissible shortest-path.
# - p_mat:    A (n_sp x m) shortest-path - edges matrix, with p_{ij} = 1 iff
#             edge j is in shortest-path i, 0 otherwise.
# Out:
# - x_mat:    A (n x n) matrix of flow on each edge, with x_mat = x_wit + x_btw.
# - x_wit:    A (n x n) matrix of flow on each edge inside lines.
# - x_btw:    A (n x n) matrix of flow on each transfer edge between lines.
#-------------------------------------------------------------------------------

compute_x_from_n = function(n_mat, edge_ref, sp_ref, p_mat){
  
  # --- Get the network structure
  
  # Number of stops
  n = dim(n_mat)[1]
  # The reference of transfer edges
  where_edge_btw = as.logical(edge_ref[, 3])
  # The shortest-path order reference
  sp_order_ref = mapply(function(i, j) i+n*(j-1), sp_ref[, 1], sp_ref[, 2])
  
  # --- Compute results 
  
  # Get the flow as vector in the admissible shortest-path order
  sp_flow_vec = as.vector(n_mat)[sp_order_ref]
  # Compute the flow on edges
  edge_flow = as.vector(t(p_mat) %*% sp_flow_vec)
  # Compute the matrix of edge flow
  x_mat = as.matrix(sparseMatrix(i=edge_ref[, 1], j=edge_ref[, 2], 
                                 x=edge_flow, dims=c(n, n)))
  # Compute the matrix of transfer edge flow
  x_btw = as.matrix(sparseMatrix(i=edge_ref[where_edge_btw, 1], 
                                 j=edge_ref[where_edge_btw, 2], 
                                 x=edge_flow[where_edge_btw], dims=c(n, n)))
  # Compute the matrix of within line edge flow
  x_wit = x_mat - x_btw 
  
  # --- Return results
  
  return(list(x_mat=x_mat, x_wit=x_wit, x_btw=x_btw))
  
}

#-------------------------------------------------------------------------------
# Function: plot_flow_graph 
#
# Description:  Plot a graph with lines and flow
#
# In:
# - adj: the adjacency matrix of the line network (inter and intra)
# - n_mat: the flow matrix between stops
# - layout: a given layout for the graph, if NULL, automatic
# Out:
# - None, only plotting
#-------------------------------------------------------------------------------

plot_flow_graph = function(adj, n_mat, layout=NULL, main=NULL){
  
  igraph_options(annotate.plot=TRUE)
  
  df_line = as_data_frame(graph_from_adjacency_matrix(adj, weighted = TRUE))
  df_line["type"] = "line"
  df_flow = as_data_frame(graph_from_adjacency_matrix(n_mat, weighted = TRUE))
  df_flow["type"] = "flow"
  df_tot = rbind(df_line, df_flow)
  
  if(is.null(layout)){
    layout = layout_nicely(graph_from_data_frame(df_line))
  }
  
  g_tot = graph_from_data_frame(df_tot)
  
  n_vertices = length(V(g_tot))
  n_edges = length(E(g_tot))
  
  std_weights = E(g_tot)$weight[E(g_tot)$type == "flow"]
  std_weights = std_weights / max(std_weights)
  
  color_green_fn = colorRamp(c("white", "forestgreen"))
  color_red_fn = colorRamp(c("white", "red"))
  color_blue_fn = colorRamp(c("white", "blue"))
  
  edge_sizes = (E(g_tot)$type == "line") * 0.5
  edge_sizes[E(g_tot)$type == "flow"] = std_weights * 2
  edge_curves = (E(g_tot)$type == "line") * 0.2
  edge_colors = rep("black", n_edges)
  new_colors = apply(cbind(color_green_fn(std_weights), std_weights), 1, 
                     function(row) rgb(row[1], row[2], row[3], row[4]*255, 
                                       maxColorValue=255))
  edge_colors[E(g_tot)$type == "flow"] = new_colors
  
  pie_prop = list()
  in_out_colors = list()
  in_prop = rowSums(n_mat)
  in_prop = in_prop / max(in_prop)
  out_prop = colSums(n_mat)
  out_prop = out_prop / max(out_prop)
  for (i in 1:n_vertices){
    pie_prop[[i]] = c(0.5, 0.5)
    loc = which(V(g_tot)$name == names(in_prop)[i])
    in_color = color_red_fn(in_prop[i])
    out_color = color_blue_fn(out_prop[i])
    in_out_colors[[loc]] = c(rgb(in_color, maxColorValue=255), 
                             rgb(out_color, maxColorValue=255))
  }
  in_out_colors = setNames(in_out_colors, names(in_prop))
  
  plot(g_tot, layout=layout, edge.color=edge_colors, edge.width=edge_sizes,
       edge.curved=edge_curves, edge.arrow.size=0.1, vertex.shape="pie", 
       vertex.pie=pie_prop, vertex.pie.color=in_out_colors, main=main)
}

#-------------------------------------------------------------------------------
# Network building
#-------------------------------------------------------------------------------
#--------------------------------
# Network function
#--------------------------------
stops_list = function(nb_lines, nb_stops){
  k <- 1
  j <- 1
  stops_list = list()
  while (j <= nb_stops) {
    while (k <= nb_lines) {
      stops_list = append(stops_list, paste0("S",k,"A",j))
      stops_list = append(stops_list, paste0("S",k,"R",j))
      k <- k + 1
    }
    k <- 1
    j <- j + 1
  }
  j <- 1
  stops_list = as.character(stops_list)
  stops_list = stops_list[order(stops_list, na.last = NA)]
  
  return(stops_list)
}


#--------------------------------
# Create name_stops
#--------------------------------

name_stops_function = function(stops){
  name_stops = strsplit(gsub("([A-Z]*)([0-9]*)([A-Z]*)", "\\1 \\2 \\3", stops), " ")
  name_stops = as.data.frame(name_stops)
  name_stops = t(name_stops)
  rownames(name_stops) = stops
  name_stops = name_stops[,2:4]
  name_stops <- as.data.frame(name_stops)
  name_stops[,1] = as.integer(name_stops[,1])
  name_stops[,3] = as.integer(name_stops[,3])
  
  name_stops$code <- 0
  for (i in 1:dim(name_stops)[1]) {
    name_stops$list[i] = i
  }
  return(name_stops)
}

#--------------------------------
# Create Adjency matrix
#--------------------------------
adj_function = function(stops, name_stops){
  adj = matrix(0, nrow = nb_stops_tot, ncol = nb_stops_tot)
  colnames(adj) = rownames(adj) = stops
  
  for (i in 1:(dim(name_stops)[1])) {
    for (j in 1:dim(name_stops)[1]) {
      # next stop
      if ((name_stops[i,1] == name_stops[j,1]) & (name_stops[i,2] == name_stops[j,2]) & (name_stops[i,3]+1 == name_stops[j,3])) {
        adj[i,j] = 1
      }
      # Crossing stops
      if ((name_stops[i,1] != name_stops[j,1]) & (name_stops[i,3] == cross_stop) & (name_stops[j,3] == cross_stop)) {
        adj[i,j] = 1
      }
    }
  }
  return(adj)
}

#--------------------------------
# Create Paths matrix
#--------------------------------

paths_function = function(nb_stops_tot, name_stops, cross_stop){
  paths = matrix(0, nrow = nb_stops_tot, ncol = nb_stops_tot)
  colnames(paths) = rownames(paths) = stops
  
  for (i in 1:(dim(name_stops)[1])) {
    for (j in 1:dim(name_stops)[1]) {
      # Conditions to stay on the same line
      if ((name_stops[i,1] == name_stops[j,1]) & (name_stops[i,2] == name_stops[j,2]) & (name_stops[i,3] < name_stops[j,3])) {
        paths[i,j] = 1
      }
      
      if ((name_stops[i,1] != name_stops[j,1]) & (name_stops[i,3] < name_stops[j,3]) & (name_stops[j,3] != cross_stop) & (name_stops[i,3] != cross_stop)) {
        paths[i,j] = 1
      }
    }
  }
  return(paths)
}

#-------------------------------------------------------------------------------
# Passengers into the network
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Function: get_passengers, normal distribution (NOT USED)
#
# Description:            Gives the distribution of the passengers going in and 
#                         of at each stop. 
#
# In:
# - nb_passengers:        A number of passengers into the network.
#
# Out:
# - passengers_in_out:    Distibution of the passengers into the network.
#-------------------------------------------------------------------------------

# get_passengers = function(nb_passengers){
#   # function to get uniform passengers
#   rand_vect <- function(N, M, sd = 50, pos.only = TRUE) {
#     vec <- rnorm(N, M/N, sd)
#     if (abs(sum(vec)) < 0.01) vec <- vec + 1
#     vec <- round(vec / sum(vec) * M)
#     deviation <- M - sum(vec)
#     for (. in seq_len(abs(deviation))) {
#       vec[i] <- vec[i <- sample(N, 1)] + sign(deviation)
#     }
#     if (pos.only) while (any(vec < 0)) {
#       negs <- vec < 0
#       pos  <- vec > 0
#       vec[negs][i] <- vec[negs][i <- sample(sum(negs), 1)] + 1
#       vec[pos][i]  <- vec[pos ][i <- sample(sum(pos ), 1)] - 1
#     }
#     vec
#   }
#   
#   # Create a list with the stops
#   random_passengers = c()
#   # Distribution of the passengers per stop
#   random_passengers = rand_vect(sum(paths), nb_passengers)
#   
#   paths_passengers = paths
#   
#   k = 1
#   for (i in 1:(dim(name_stops)[1])) {
#     for (j in 1:dim(name_stops)[1]) {
#       if (paths_passengers[i,j] == 1) {
#         paths_passengers[i,j] = random_passengers[k]
#         k = k + 1
#       }
#     }
#   }
#   
#   #--------------------------------
#   # In- and outgoing
#   #--------------------------------
#   
#   rho_in = rowSums(paths_passengers)
#   rho_out = colSums(paths_passengers)
#   go_in_out = cbind(rho_in, rho_out)
#   output = list(go_in_out,paths_passengers)
#   return(output)
# }

#-------------------------------------------------------------------------------
# Passengers into the network, Poisson distribution version
#-------------------------------------------------------------------------------

n_poisson = function(paths, lambda){
  nb_stops_tot = dim(paths)[1]
  n_drawn = matrix(0, dim(paths)[1], dim(paths)[2])
  n_drawn[paths == 1] = rpois(sum(paths), lambda = lambda) + 1
  return(n_drawn)
}

#-------------------------------------------------------------------------------
# Draw n_passagers passengers to non-null positions with a multinomial distrib
#-------------------------------------------------------------------------------

n_multin = function(paths, n_passagers, p_pos = rep(1/sum(paths), sum(paths))){
  drawn = as.vector(rmultinom(1, n_passagers, p_pos))
  n_drawn = paths
  n_drawn[n_drawn == 1] = drawn
  return(n_drawn)
}

#-------------------------------------------------------------------------------
# - edge_ref
# - p_mat
# - sp_ref
#-------------------------------------------------------------------------------

edge_ref_p_mat_sp_ref = function(adj){
  g = graph_from_adjacency_matrix(
    adj,
    mode = c("directed")
  )
  
  # set edge_ref from g
  edge_ref = as.data.frame(as_edgelist(g))
  edge_ref$B = 0
  n_edges = dim(edge_ref)[1]
  
  # find edge transfer
  for (i in 1:n_edges) {
    if (substr(edge_ref[i,1], 1, 2) != substr(edge_ref[i,2], 1, 2)) {
      edge_ref$B[i] = 1
    }
  }
  
  between_line = edge_ref$B
  
  V(g)$name = 1:vcount(g)
  edge_ref = as.data.frame(as_edgelist(g))
  edge_ref$B = between_line
  
  # make the containers for results
  p_mat = c()
  sp_ref = c()
  
  # compute p_mat
  for (i in 1:dim(paths)[1]) {
    for (j in 1:dim(paths)[1]) {
      if (paths[i,j] == 1) {
        sp = get.shortest.paths(g,i,j)$vpath[[1]]
        sp_ref = rbind(sp_ref, c(i, j))
        
        # Get the index of edge in sp
        index_sp_edge = mapply(
          function(a, b) which((edge_ref[,1] == a) & (edge_ref[,2] == b)), 
          sp[-length(sp)], sp[-1])
        
        sp_edge_vec = rep(0, n_edges)
        sp_edge_vec[index_sp_edge] = 1
        p_mat = rbind(p_mat, sp_edge_vec)
      }
    }
  }
  # edge_ref format
  colnames(edge_ref) = c("row", "col", "is_transfer_edge")
  edge_ref$row = as.integer(edge_ref$row)
  edge_ref$col = as.integer(edge_ref$col)
  edge_ref$is_transfer_edge = as.integer(edge_ref$is_transfer_edge)
  
  # p_mat sparse format
  p_mat <- as(p_mat, "ngTMatrix")
  
  # list output
  output = list(edge_ref,p_mat,sp_ref)
  return(output)
}

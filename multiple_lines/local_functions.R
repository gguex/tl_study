#-------------------------------------------------------------------------------
#
# Functions for transportation lines algorithms 
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Needed libraries gskln
#-------------------------------------------------------------------------------

library(igraph)
library(Matrix)
library(parallel)

#-------------------------------------------------------------------------------
# Function: build_network_structure()
#
# Description:  This function compute the network structure of a multiple line
#               transportation network. It uses a matrix of pedestrian time to 
#               build transfer edges, and make sure lines from same tour aren't 
#               connected.
#
# In:
# - line_mbrshps: A n-length vector containing line memberships of stops.
# - tour_mbrshps: A n-length vector containing tour memberships of stops.
# - ped_t_mat: A (n x n) pedestrian time matrix between stops.
# - ped_t_thres: A scalar threshold on the pedestrian time for considering 
#                stops linked.
# Out:
# - adj_w:    A (n x n) adjacency matrix within transportation lines.
# - adj_b:    A (n x n) adjacency matrix between transportation lines.
#-------------------------------------------------------------------------------

build_network_structure = 
  function(line_mbrshps, tour_mbrshps, ped_t_mat, ped_t_thres){
  
  # --- Get number of stops and make levels for line_mbr and tour_mbr
  
  # Get the number of stops 
  n = length(line_mbrshps)
  # Get levels of lines
  lines_lvl = as.factor(line_mbrshps)
  # Get levels of tour
  tour_lvl = as.factor(tour_mbrshps)
  
  # --- Build A_W
  
  # Make links to the next stop
  adj_w = matrix(0, n, n)
  for(i in 1:(n-1)){
    if(lines_lvl[i] == lines_lvl[i + 1]) adj_w[i, i + 1] = 1
  }
  
  # --- Build A_B
  
  # With distance threshold
  adj_b = 1*(ped_t_mat < ped_t_thres)
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
# - line_mbrshps: A n-length vector containing line memberships of stops.
# - tour_mbrshps: A n-length vector containing tour memberships of stops.
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
# - sp_edge_link:    A (n_sp x m) sparse shortest-path - edges matrix, 
#             with s_{ij} = TRUE iff edge j is in shortest-path i.
#-------------------------------------------------------------------------------

build_sp_data = function(line_mbrshps, tour_mbrshps, travel_t, wait_t, dist_mat, 
                         adj_w, adj_b){
  
  # --- Get number of stops and make levels for line_mbr and tour_mbr
  
  # Get the number of stops 
  n = length(line_mbrshps)
  # Get levels of lines
  lines_lvl = as.factor(line_mbrshps)
  # Get levels of tour
  tour_lvl = as.factor(tour_mbrshps)
  
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
  
  # To store location and the number of admissible paths
  i_loc = c()
  j_loc = c()
  n_admissible_path = 1
  
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
            if(!any(head(is_transfer_edge[index_sp_edge], -1) & 
                    tail(is_transfer_edge[index_sp_edge], -1))){
              
              # Save the shortest path
              sp_ref = rbind(sp_ref, c(i, j))
              
              # save location and update the number of admissible paths
              i_loc = c(i_loc, rep(n_admissible_path, length(index_sp_edge)))
              j_loc = c(j_loc, index_sp_edge)
              n_admissible_path = n_admissible_path + 1
            }
            
          }
        }
        
      }
      
    }
  }
  
  # Sparse matrix
  sp_edge_link = sparseMatrix(i_loc, j_loc)
  
  # --- Return the results
  
  return(list(edge_ref=edge_ref, sp_ref=sp_ref, sp_edge_link=sp_edge_link))
  
}

#---------------------------------------------------------------------------
# MULTICORE VERSION
#---------------------------------------------------------------------------

compute_sp_edges = function(s, t, adj_w, adj_b, ped_t_mat, travel_t_mat, 
                            full_g, edge_ref){
  
  # Test if i, j are not valid
  if((s == t) | 
     ((adj_w[s, t] == 0) & (travel_t_mat[s, t] > ped_t_mat[s, t]))){
    return(NULL)
  }
  
  # Get the shortest path
  sp = get.shortest.paths(full_g, s, t)$vpath[[1]]
  
  # Test if sp does not exit or if if does start or end with a transfer edge
  if((length(sp) < 1) | 
     (adj_b[sp[length(sp) - 1], sp[length(sp)]] | adj_b[sp[1], sp[2]])){
    return(NULL)
  }
  
  # Get the index of edges in sp
  index_sp_edge = mapply(
    function(i, j) which((edge_ref[,1] == i) & (edge_ref[,2] == j)), 
    sp[-length(sp)], sp[-1])
  
  # Test if there are consecutive transfer edges
  if(any(head(edge_ref[index_sp_edge, 3], -1) & 
         tail(edge_ref[index_sp_edge, 3], -1))){
    return(NULL)
  }
  
  return(index_sp_edge)
}

build_sp_data_mc = function(line_mbrshps, tour_mbrshps, travel_t_vec, 
                            wait_t_vec, ped_t_mat, adj_w, adj_b, mc.cores=1){
  
  # --- Get number of stops and make levels for line_mbr and tour_mbr
  
  # Get the number of stops 
  n = length(line_mbrshps)
  # Get levels of lines
  lines_lvl = as.factor(line_mbrshps)
  # Get levels of tour
  tour_lvl = as.factor(tour_mbrshps)
  
  # --- Compute the traveling time with the line network 
  
  # Replace potential NA with large values
  travel_t_vec[is.na(travel_t_vec)] = 1e10
  # Adjacency matrix for traveling time
  adj_travel_t = adj_w * travel_t_vec + t(t(adj_b) * wait_t_vec)
  # The graph for traveling
  travel_t_g = graph_from_adjacency_matrix(adj_travel_t, mode="directed", 
                                           weighted=T)
  # Travel time between stops
  travel_t_mat = distances(travel_t_g, mode="out")
  
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
  
  # s and t vec 
  s_vec = rep(1:n, n)
  t_vec = rep(1:n, each=n)
  
  # Compute shortest_path edges with multiple cores
  sp_edges = mcmapply(function(s, t) 
    compute_sp_edges(s, t, adj_w, adj_b, ped_t_mat, travel_t_mat, 
                     full_g, edge_ref), s_vec, t_vec, mc.cores=mc.cores)
  
  # Admissible sp indices
  admissible_sp_ind = which(!sapply(sp_edges, is.null))
  
  # Build sp_ref 
  sp_ref = cbind(s_vec[admissible_sp_ind], t_vec[admissible_sp_ind])
  
  # Build sp_edge_link
  i_loc = c()
  j_loc = c()
  n_paths = 1
  for(sp_ind in admissible_sp_ind){
    index_sp_edge = sp_edges[[sp_ind]]
    i_loc = c(i_loc, rep(n_paths, length(index_sp_edge)))
    j_loc = c(j_loc, index_sp_edge)
    n_paths = n_paths + 1
  }
  sp_edge_link = sparseMatrix(i_loc, j_loc)
  
  # --- Return the results
  
  return(list(edge_ref=edge_ref, sp_ref=sp_ref, sp_edge_link=sp_edge_link))
  
}

#-------------------------------------------------------------------------------
# Function: balance_flow_l_inout 
#
# Description:  This function compute a corrected version of ingoing and 
#               outgoing flow inside the different lines, in order that the 
#               balance is correct.
#
# In:
# - line_mbrshps: A n-length vector containing line memberships for stops.
# - unblced_flow_l_in:  A n-length vector with unbalanced flow in lines.
# - unblced_flow_l_out: A n-length vector with unbalanced flow out lines.
# Out:
# - flow_l_in:  A n-length vector of corrected flow entering lines.
# - flow_l_out:  A n-length vector of corrected flow leaving lines.
#-------------------------------------------------------------------------------

balance_flow_l_inout = 
  function(line_mbrshps, unblced_flow_l_in, unblced_flow_l_out){
  
  # Get the number of stops 
  n = length(line_mbrshps)
  # Get levels of lines
  lines_lvl = as.factor(line_mbrshps)
  
  # Vectors to store the corrected flow values 
  flow_l_in = rep(0, n)
  flow_l_out = rep(0, n)
  
  # Loop on lines
  for(l_lvl in levels(lines_lvl)){
  
    # Compute the flow balance and the cumulative sum
    unblced_line_flow_in = unblced_flow_l_in[lines_lvl == l_lvl]
    unblced_line_flow_out = unblced_flow_l_out[lines_lvl == l_lvl]
    line_balance = unblced_line_flow_in - unblced_line_flow_out
    line_flow = cumsum(line_balance)
    
    # The quantity to correct
    correction = line_flow[length(line_flow)]
    # We reduce (or raise, if negative correction) the flow in
    line_flow_in = unblced_line_flow_in
    line_flow_in[-length(line_flow_in)] = line_flow_in[-length(line_flow_in)] - 
      correction / (2*(length(line_flow_in) - 1))
    # We raise (or reduce, if negative correction) the flow out
    line_flow_out = unblced_line_flow_out 
    line_flow_out[-1] = line_flow_out[-1] + 
      correction / (2*(length(line_flow_out) - 1))
    
    # Check if there is any negative value, and correct it if it's the case
    if(any(line_flow_in < 0)){
      line_flow_out[line_flow_in < 0] = line_flow_out[line_flow_in < 0] - 
        line_flow_in[line_flow_in < 0]
      line_flow_in[line_flow_in < 0] = 0
    }
    if(any(line_flow_out < 0)){
      line_flow_in[line_flow_out < 0] = line_flow_in[line_flow_out < 0] - 
        line_flow_out[line_flow_out < 0]
      line_flow_out[line_flow_out < 0] = 0
    }
    
    # Add the corrected flow for line to the vector of global corrected flow 
    flow_l_in[lines_lvl == l_lvl] = line_flow_in
    flow_l_out[lines_lvl == l_lvl] = line_flow_out
  }
  
  # --- Return the results
  
  return(list(flow_l_in=flow_l_in, flow_l_out=flow_l_out))
  
}


#-------------------------------------------------------------------------------
# Function: compute_origin_destination
#
# Description:  This function compute the source-destination matrix from a 
#               graph with multiple lines structure, an affinity matrix between 
#               stops, and the flow going in and out at each line stops.
#
# In:
# - flow_l_in:   A n-length vector of flow entering lines.
# - flow_l_out:  A n-length vector of flow leaving lines.
# - edge_ref: A (m x 3) matrix, giving the edge starting node index, ending 
#             node index, and a boolean indicating if this is an transfer edge.
# - sp_ref:   A (n_sp x 2) matrix, giving the source-target node pair for each 
#             admissible shortest-path.
# - sp_edge_link:    A (n_sp x m) shortest-path - edges matrix, with p_{ij} = 1 iff
#             edge j is in shortest-path i, 0 otherwise.
# - s_mat:    A (n x n) optional affinity matrix between stops. If None is given
#             this matrix will set to 1 for each pair having an admissible 
#             shorest-path (default = NULL).
# - min_p_ntwk:  The min percentage of the in/out line flow entering/leaving
#                the network (default = 0.1).
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

compute_origin_destination = 
  function(flow_l_in, flow_l_out, edge_ref, sp_ref, sp_edge_link, 
           s_mat=NULL, min_p_ntwk=0.1, conv_thres_if=1e-5,
           conv_thres_algo=1e-5, epsilon=1e-40,
           max_it=200, display_it=T){
  
  # --- Get the network structure 
  
  # The number of stops 
  n = length(flow_l_in)
  # The reference of transfer edges
  where_edge_btw = which(as.logical(edge_ref[, 3]))
  # The list of in-out nodes for transfer edges 
  edge_btw_ref = edge_ref[where_edge_btw, 1:2]
  # The shortest-path - edge matrix retrained on transfer edges
  sp_edge_btw_link = sp_edge_link[, where_edge_btw]
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
  # The initial value for f_mat (has no effect, only to create variable)
  f_mat = matrix(1e5, n, n)
  # The evolving in and out distribution
  sigma_in = flow_l_in / sum(flow_l_in)
  sigma_out = flow_l_out / sum(flow_l_out)
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
    
    # --- Get the distribution to flow constant
    
    distrib2flow_const = flow_l_in[node_ref] / sigma_in[node_ref]
    
    # --- STEP 1: Iterative fitting 
    
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
    n_mat = distrib2flow_const * f_mat
    
    # --- STEP 2: Update sigma_in, sigma_out
    
    # Get the flow as vector in the admissible shortest-path order
    sp_flow_vec = as.vector(n_mat)[sp_order_ref]
    # Compute the flow on edges
    btw_edge_flow = colSums(sp_edge_btw_link * sp_flow_vec)
    # Compute the in-out between flow on nodes 
    node_in_btw = sapply(1:n, function(i) 
      sum(btw_edge_flow[edge_btw_ref[, 2] == i]))
    node_out_btw = sapply(1:n, function(i) 
      sum(btw_edge_flow[edge_btw_ref[, 1] == i]))
  
    # Compute unscaled sigmas
    unscaled_sigma_in = flow_l_in - node_in_btw
    unscaled_sigma_out = flow_l_out - node_out_btw
    unscaled_sigma_in[unscaled_sigma_in < min_p_ntwk*flow_l_in] = 
      min_p_ntwk*flow_l_in[unscaled_sigma_in < min_p_ntwk*flow_l_in]
    unscaled_sigma_out[unscaled_sigma_out < min_p_ntwk*flow_l_out] = 
      min_p_ntwk*flow_l_out[unscaled_sigma_out < min_p_ntwk*flow_l_out]
    
    # Scale them
    sigma_in = unscaled_sigma_in / sum(unscaled_sigma_in)
    sigma_out = unscaled_sigma_out / sum(unscaled_sigma_out)
    
    # --- STEP 3: Update g_ref
    
    # Compute the ratio of flow
    ratio_in = (node_in_btw / ((1-min_p_ntwk)*flow_l_in + epsilon))
    ratio_out = (node_out_btw / ((1-min_p_ntwk)*flow_l_out + epsilon))
    # Compute the ratio of flow on edges
    ratio_edge_btw = mapply(
      function(i, j) max(ratio_in[j], ratio_out[i]), 
      edge_btw_ref[ ,1], edge_btw_ref[,2])
    # Compute the path max ratio
    path_max_ratio = apply(t(sp_edge_btw_link) * ratio_edge_btw, 2, max)
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
    
    # --- Display and check for convergence
    
    # Compute iteration statistics
    diff_f = sum(abs(f_mat_old - f_mat))
    diff_g = sum(abs(g_ref_old - g_ref))
    diff_in = sum(abs(sigma_in_old - sigma_in))
    diff_out = sum(abs(sigma_out_old - sigma_out))
    couple = sp_ref[which(path_max_ratio == max(path_max_ratio)), ]
    if(!is.null(dim(couple))){
      couple = couple[1, ]
    }
    # Print iteration statistics
    if (display_it) {
      cat("It", it_algo, ": diff_f =", diff_f, ", diff_g =", diff_g, 
          ", diff_in =", diff_in, ", diff_out =", diff_out, 
          ", to_red_max =", max(path_max_ratio), ", where =", couple,"\n")
    }
    
    
    # Check for convergence
    if(diff_f < conv_thres_algo){
      converge_algo = T
    }
    
    # Iteration
    it_algo = it_algo + 1
  }
  
  # Return results
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
# - sp_edge_link:    A (n_sp x m) shortest-path - edges matrix, with p_{ij} = 1 iff
#             edge j is in shortest-path i, 0 otherwise.
# Out:
# - x_mat:    A (n x n) matrix of flow on each edge, with x_mat = x_wit + x_btw.
# - x_wit:    A (n x n) matrix of flow on each edge inside lines.
# - x_btw:    A (n x n) matrix of flow on each transfer edge between lines.
#-------------------------------------------------------------------------------

compute_x_from_n = function(n_mat, edge_ref, sp_ref, sp_edge_link){
  
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
  edge_flow = as.vector(t(sp_edge_link) %*% sp_flow_vec)
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
# - None, gives a plot
#-------------------------------------------------------------------------------

plot_flow_graph = function(adj, n_mat, layout=NULL, main=NULL){
  
  igraph_options(annotate.plot=TRUE)
  
  df_line = igraph::as_data_frame(graph_from_adjacency_matrix(adj, 
                                                              weighted = TRUE))
  df_line["type"] = "line"
  df_flow = igraph::as_data_frame(graph_from_adjacency_matrix(n_mat, 
                                                              weighted = TRUE))
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
# Crossing all on one stop
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

# Crossing on different stops
adj_function2 = function(stops, name_stops){
  nb_stops_tot = length(stops)
  adj = matrix(0, nrow = nb_stops_tot, ncol = nb_stops_tot)
  colnames(adj) = rownames(adj) = stops
  a = 0
  s = 0
  l = 1
  m = nb_stops*2
  for (k in 1:nb_lines) {
    for (i in l:m) {
      for (j in 1:dim(name_stops)[1]) {
        # next stop on the same line
        if ((name_stops[i,1] == name_stops[j,1])
            & (name_stops[i,2] == name_stops[j,2])
            & (name_stops[i,3]+1 == name_stops[j,3])) {
          adj[i,j] = 1
        }
        # Crossing stops "A"
        if ((name_stops[i,1] + 1 + s == name_stops[j,1])
            & (name_stops[i,3] + a == name_stops[j,3])
            & (name_stops[i,1] < name_stops[j,3])
            & (name_stops[i,1] < name_stops[j,1])
        ) {
          adj[i,j] = 1
          
          # Symmetry, change i or j
          adj[i,j + 2*(nb_stops - j %% nb_stops) + 1] = 1
          adj[i + 2*(nb_stops - i %% nb_stops) + 1,j] = 1
          # Symmetry, change i and j
          adj[i + 2*(nb_stops - i %% nb_stops) + 1,j + 2*(nb_stops - j %% nb_stops) + 1] = 1
          
          a = a - 1
          s = s + 1
        }
      }
    }
    l = k*nb_stops*2+1
    m = m+nb_stops*2
    a = 0
    s = 0
  }
  # Back an fort on the same line
  # upper_tri <- t(adj)
  # lower_tri <- lower.tri(adj)
  # adj[lower_tri] <- upper_tri[lower_tri]
  
  ### comment to keep way back
  # # Find indices of row and columns including "R" 
  # row_indices_to_remove <- grep("R", rownames(adj))
  # col_indices_to_remove <- grep("R", colnames(adj))
  # # Delete row and lines
  # adj <- adj[-row_indices_to_remove, -col_indices_to_remove]
  ###
  
  # Decompose in adj_b and adj_w
  is_w = matrix(mapply(function(i, j) i + 1 == j, 
                       rep(1:nb_stops_tot, nb_stops_tot),
                       rep(1:nb_stops_tot, each=nb_stops_tot)),
                nb_stops_tot, nb_stops_tot)
  adj_w = adj
  adj_w[!is_w] = 0
  adj_b = adj
  adj_b[is_w] = 0
  
  return(list(adj_w=adj_w, adj_b=adj_b))
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
      if ((name_stops[i,1] == name_stops[j,1])
          & (name_stops[i,2] == name_stops[j,2])
          & (name_stops[i,3] < name_stops[j,3])) {
        paths[i,j] = 1
      }
      if ((name_stops[i,1] != name_stops[j,1])
          & (name_stops[i,3] < name_stops[j,3])
          & (name_stops[j,3] != name_stops[i,3])) {
        paths[i,j] = 1
      }
    }
  }
  return(paths)
}

paths_function2 = function(nb_stops_tot, name_stops){
  
  ### Comment for back and forth
  # Find indices of row including "R"
  row_indices_to_remove <- grep("R", rownames(name_stops))
  # Delete rows
  name_stops <- name_stops[-row_indices_to_remove,]
  nb_stops_tot <- nb_stops_tot/2
  stops_to_remove <- grep("R", stops)
  stops <- stops[-stops_to_remove]
  ###
  
  paths = matrix(0, nrow = nb_stops_tot, ncol = nb_stops_tot)
  colnames(paths) = rownames(paths) = stops
  
  for (i in 1:(dim(name_stops)[1])) {
    for (j in 1:dim(name_stops)[1]) {
      # Conditions to stay on the same line
      if ((name_stops[i,1] == name_stops[j,1]) & (name_stops[i,2] == name_stops[j,2]) & (name_stops[i,3] < name_stops[j,3])) {
        paths[i,j] = 1
      }
      
      if ((name_stops[i,1] != name_stops[j,1])
          & (name_stops[i,3] + 1 < name_stops[j,3]) 
          & (name_stops[i,1] + 1 < name_stops[j,3]) 
          & (name_stops[i,3] < name_stops[j,1])) {
        paths[i,j] = 1
      }
      # lower matrix
      if (i > j
          & name_stops[i,1] > name_stops[j,1]
          & name_stops[i,1] < name_stops[j,3]
          & name_stops[i,3] <= name_stops[j,1]) {
        paths[i,j] = 1
      }
    }
  }
  # complete with new informations
  for (k in 1:(nb_lines-2)) {
    for (i in 2:(dim(name_stops)[1])) {
      for (j in 1:(dim(name_stops)[1]-1)) {
        if (paths[(i-1),j] == 1
            & paths[i,j+1] == 1
            & name_stops[i,1] != name_stops[j,1]) {
          paths[i,j] = 1
        }
      }
    }
  }
  return(paths)
}

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

n_multin = function(paths, n_passagers, p_pos = rep(1/sum(paths), sum(paths)),
                    epsilon=1e-2){
  drawn = as.vector(rmultinom(1, n_passagers, p_pos)) + epsilon
  n_drawn = paths
  n_drawn[n_drawn == 1] = drawn
  return(n_drawn)
}

#-------------------------------------------------------------------------------
# - edge_ref
# - sp_edge_link
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
  sp_edge_link = c()
  sp_ref = c()
  
  # compute sp_edge_link
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
        sp_edge_link = rbind(sp_edge_link, sp_edge_vec)
      }
    }
  }
  # edge_ref format
  colnames(edge_ref) = c("row", "col", "is_transfer_edge")
  edge_ref$row = as.integer(edge_ref$row)
  edge_ref$col = as.integer(edge_ref$col)
  edge_ref$is_transfer_edge = as.integer(edge_ref$is_transfer_edge)
  
  # sp_edge_link sparse format
  sp_edge_link <- as(sp_edge_link, "TsparseMatrix")
  
  # list output
  output = list(edge_ref,sp_edge_link,sp_ref)
  return(output)
}

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
#               build transfer edges, and make sure line from same tour are not 
#               connected.
#
# In:
# - line_mbr: A n-length vector containing line memberships of stops.
# - tour_mbr: A n-length vector containing tour memberships of stops.
# - D:        A (n x n) pedestrian time matrix between stops.
# - thres_D:  A scalar threshold on the pedestrian time for considering stops 
#             linked.
# Out:
# - A_W:      A (n x n) adjacency matrix within transportation lines.
# - A_B:      A (n x n) adjacency matrix between transportation lines.
#-------------------------------------------------------------------------------

build_network_structure = function(line_mbr, tour_mbr, D, thres_D){
  
  # --- Get number of stops and make levels for line_mbr and tour_mbr
  
  # Get the number of stops 
  n = length(line_mbr)
  # Get levels of lines
  lines_lvl = as.factor(line_mbr)
  # Get levels of tour
  tour_lvl = as.factor(tour_mbr)
  
  # --- Build A_W
  
  # Make links to the next stop
  A_W = matrix(0, n, n)
  for(i in 1:(n-1)){
    if(lines_lvl[i] == lines_lvl[i + 1]) A_W[i, i + 1] = 1
  }
  
  # --- Build A_B
  
  # With distance threshold
  A_B = 1*(D < thres_D)
  # Removing diagonal
  diag(A_B) = 0
  # Removing same tour links
  for(t_lvl in levels(tour_lvl)){
    A_B[tour_lvl == t_lvl, tour_lvl == t_lvl] = 0
  }
  
  # --- Return the results
  
  return(list(A_W=A_W, A_B=A_B))
  
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
# - D:        A (n x n) pedestrian time matrix between stops.
# - A_W:      A (n x n) adjacency matrix within transportation lines.
# - A_B:      A (n x n) adjacency matrix between transportation lines. 
# Out:
# - edge_ref: A (m x 3) matrix, giving the edge starting node index, ending 
#             node index, and a boolean indicating if this is an transfer edge.
# - sp_ref:   A (n_sp x 2) matrix, giving the source-target node pair for each 
#             admissible shortest-path.
# - P:        A (n_sp x m) shortest-path - edges matrix, with s_{ij} = 1 iff
#             edge j is in shortest-path i.
#-------------------------------------------------------------------------------

build_sp_data = function(line_mbr, tour_mbr, travel_t, wait_t, D, A_W, A_B){
  
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
  A_travel_time = A_W * travel_t + t(t(A_B) * wait_t)
  # The graph for traveling
  travel_time_g = graph_from_adjacency_matrix(A_travel_time, 
                                              mode="directed", 
                                              weighted=T)
  # Travel time between stops
  travel_time_mat = distances(travel_time_g, mode="out")
  
  # --- Compute the edge reference matrix
  
  # The edge reference 
  edge_ref = which((A_W + A_B) == 1, arr.ind=T)
  is_transfer_edge = as.logical(mapply(function(i, j) A_B[i, j], 
                                       edge_ref[,1], edge_ref[,2]))
  edge_ref = cbind(edge_ref, is_transfer_edge)
  n_edges = dim(edge_ref)[1]
  
  # --- Compute shortest-path data
  
  # Create the graph 
  full_g = graph_from_adjacency_matrix(A_W + A_B, mode="directed")
  
  # Make the containers for results
  sp_ref = c()
  P = c()
  
  # Loop on pairs of nodes
  for(i in 1:n){
    cat("Node: ", i, "; ")
    for(j in 1:n){
      
      # Check if the path is shorter with transport line (except line travel), 
      # and if i not j
      if(((travel_time_mat[i, j] < D[i, j]) | (A_W[i, j] == 1)) & (i!=j)){
        
        # Get the shortest path
        sp = get.shortest.paths(full_g, i, j)$vpath[[1]]
        
        # If it exists 
        if(length(sp) > 1){
          # And if it does not start or end with a transfer edge
          if( !(A_B[sp[length(sp) - 1], sp[length(sp)]] | A_B[sp[1], sp[2]]) ){
            
            # Get the index of edge in sp
            index_sp_edge = mapply(
              function(a, b) which((edge_ref[,1] == a) & (edge_ref[,2] == b)), 
              sp[-length(sp)], sp[-1])
            
            # Check if there are no consecutive transfer edge
            if(!any(head(is_transfer_edge[index_sp_edge],-1) & 
                    tail(is_transfer_edge[index_sp_edge], -1))){
              
              # Save the shortest path
              sp_ref = rbind(sp_ref, c(i, j))
              sp_edge_vec = rep(0, n_edges)
              sp_edge_vec[index_sp_edge] = 1
              P = rbind(P, sp_edge_vec)
              
            }
            
          }
        }
        
      }
      
    }
  }
  
  # --- Return the results
  
  return(list(edge_ref=edge_ref, sp_ref=sp_ref, P=P))
  
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
  
  # Loop on lines
  for(l_lvl in levels(lines_lvl)){
  
    # Compute the flow balance
    line_flow_in = flow_in[lines_lvl == l_lvl]
    line_flow_out = flow_out[lines_lvl == l_lvl]
    line_balance = line_flow_in - line_flow_out
    line_flow = cumsum(line_balance)
    
    # If there is positive or negative flow at the end of the line, 
    # we correct the values
    correction = line_flow[length(line_flow)]
    inout_cor[selected_ind[-1], ]$descentes = inout_data[selected_ind[-1], ]$descentes + correction / length(selected_ind[-1])
    new_line_data = inout_cor[selected_ind, ]
    new_line_balance = as.vector(t(new_line_data["montees"] - new_line_data["descentes"]))
    new_line_flow = round(cumsum(new_line_balance), 2)
    
    # TO COMPLETE 
    
  }
}
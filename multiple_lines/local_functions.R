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
# Function: build_network_structure
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
# Function: build_sp_data
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
# - T:        A (n x n) matrix of admissible shortest-paths in the network, 
#             containing (n_sp) ones and (n^2 - n_sp) zeroes.
# - S:        A (n_sp x m) shortest-path - edges matrix, with s_{ij} = 1 iff
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
  travel_time_mat = distances(full_time_g, mode="out")
  
}

#-------------------------------------------------------------------------------
# Function: build_in_out_flow 
#
# In:
# - line_mbr: A n-length vector containing line memberships for stops.
# - meas_in:  A n-length vector containing measured flow entering lines.
# - meas_out: A n-length vector containing measured flow leaving lines.
# Out:
# - rho_in:   A n-length vector of corrected flow entering lines.
# - rho_out:  A n-length vector of corrected flow leaving lines.
#-------------------------------------------------------------------------------

build_in_out_flow = function(line_mbr, meas_in, meas_out){
  
}
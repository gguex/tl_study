#-------------------------------------------------------------------------------
#
# Functions for transportation lines algorithms 
#
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Function: build_network_structure
#
# In:
# - line_mbr: A n-length vector containing line memberships of stops.
# - D:        A (n x n) pedestrian time matrix between stops.
# - D_thres:  A scalar threshold on the pedestrian time for considering stops 
#             linked.
# Out:
# - A_W:      A (n x n) adjacency matrix within transportation lines.
# - A_B:      A (n x n) adjacency matrix between transportation lines.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Function: build_sp_data
#
# In:  
# - line_mbr: A n-length vector containing line memberships of stops.
# - tour_mbr  A n-length vector containing tour memberships of stops.
# - D:        A (n x n) pedestrian time matrix between stops.
# - A_W:      A (n x n) adjacency matrix within transportation lines.
# - A_B:      A (n x n) adjacency matrix between transportation lines. 
# - travel_t: A n-length vector giving time needed to reach next stop.
# - wait_t:   A n-length vector giving time needed to enter a line at each stop.
# Out:
# - T:        A (n x n) matrix of admissible shortest-paths in the network, 
#             containing (n_sp) ones and (n^2 - n_sp) zeroes.
# - S:        A (n_sp x m) shortest-path - edges matrix, with s_{ij} = 1 iff
#             edge j is in shortest-path i.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Function: build_in_out_flow 
#
# In:
# - line_mbr: A n-length vector containing line memberships for stops.
# - mes_in:   A n-length vector containing measured flow entering lines.
# - mes_out:  A n-length vector containing measured flow leaving lines.
# Out:
# - rho_in:   A n-length vector of corrected flow entering lines.
# - rho_out:  A n-length vector of corrected flow leaving lines.
#-------------------------------------------------------------------------------


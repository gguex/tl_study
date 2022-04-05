#-------------------------------------------------------------------------------
#
# Functions for transportation lines algorithms 
#
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Function: build_transport_structure
#
# Data_in:
# Data out:
# - A_W:  The (n x n) adjacency matrix within transportation lines.
# - A_B:  The (n x n) adjacency matrix between transportation lines.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Function: build_sp_data
#
# Data_in:
# Data out:
# - T:  The (n x n) matrix of admissible shortest-paths in the network, 
#       containing (n_sp) ones and (n^2 - n_sp) zeroes.
# - S:  The (n_sp x m) shortest-path - edges matrix, with s_{ij} = 1 iff edge j
#       is in shortest-path i.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Function: build_in_out_flow 
#
# Data_in:
# Data out:
# - sigma_in: The n-length vector of flow entering lines.
# - sigma_out: The n-length vector of flow leaving lines.
#-------------------------------------------------------------------------------


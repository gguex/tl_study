library(igraph)
library(parallel)


test_sp_validity = function(i, j, adj_w, dist_mat, travel_time_mat){
  return(((travel_time_mat[i, j] < dist_mat[i, j]) | (adj_w[i, j] == 1)) & 
           (i!=j))
}

validity_vec = mcmapply(function(i, j) 
  test_sp_validity(i, j, adj_w, dist_mat, travel_time_mat), 
  rep(1:n, n), rep(1:n, each=n), mc.cores=8)

sp_compute = function(i, j, edge_ref, full_g, adj_b, validity_vec){
  
  # Get the shortest path
  sp = get.shortest.paths(full_g, i, j)$vpath[[1]]
  
  # If it exists 
  if( (length(sp) > 1) & 
      !(adj_b[sp[length(sp) - 1], sp[length(sp)]] | adj_b[sp[1], sp[2]]) ) {
    
    
  }
}



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
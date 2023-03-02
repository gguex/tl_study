# An old version of the algorithm

compute_origin_destination_old = 
  function(flow_l_in, flow_l_out, edge_ref, sp_ref, sp_edge_link, 
           s_mat=NULL, min_p_ntwk=0.1, conv_thres_if=1e-5,
           conv_thres_algo=1e-5, epsilon=1e-40,
           max_it=200, max_it_if=400, display_it=T, return_it=F, 
           return_details=F){
    
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
    # The initial value for f_mat (has no effect, only to create old variable)
    f_mat = matrix(1e5, n, n)
    # The sum in and out
    sum_flow_l_in = sum(flow_l_in)
    sum_flow_l_out = sum(flow_l_out)
    # The evolving in and out distribution
    # sigma_in = flow_l_in / sum_flow_l_in
    # sigma_out = flow_l_out / sum_flow_l_out
    sigma_in = rowSums(g_ref)
    sigma_out = colSums(g_ref)
    # A boolean for convergence
    converge_algo = F
    # Iteration counter
    it_algo = 1
    
    # --- For it return
    
    if(return_it){
      n_mat_list = list()
    }
    
    # --- For details return
    if(return_details){
      iterations_df = data.frame(it_algo=NULL,
                                 diff_f=NULL,
                                 diff_g=NULL,
                                 error_in=NULL,
                                 error_out=NULL,
                                 max_diff_flow=NULL,
                                 couple_1=NULL,
                                 couple_2=NULL)
    }
    
    
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
      it_if = 1
      while(!converge_if & it_if <= max_it_if){
        # Saving old b results
        psi_old = psi
        # Compute new a and b
        phi = sigma_in / (colSums(t(g_ref) * psi) + epsilon)
        psi = sigma_out / (colSums(g_ref * phi) + epsilon)
        # Checking for convergence
        if(sum(abs(psi_old - psi)) < conv_thres_if){
          converge_if = T
        }
        it_if = it_if + 1
      }
      
      # Building f_mat
      f_mat = t(psi * t(phi * g_ref))
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
                               sp_ref[, 1], sp_ref[, 2])
      # Compute the updated g_vec
      reduced_flow = sp_flow_vec / (path_max_ratio)
      g_ref_vec = reduced_flow / (scaling_phi_psi + epsilon)
      g_ref_vec = g_ref_vec / sum(g_ref_vec)
      # Back to g_ref
      g_ref = as.matrix(sparseMatrix(i=sp_ref[, 1], j=sp_ref[, 2], 
                                     x=g_ref_vec, dims=c(n, n)))
      
      # --- Display and check for convergence
      
      # Compute iteration statistics
      diff_f = sum(abs(f_mat_old - f_mat))
      diff_g = sum(abs(g_ref_old - g_ref))
      d2f_new = flow_l_in[node_ref] / sigma_in[node_ref]
      error_in = sum(abs(flow_l_in - sigma_in*d2f_new - node_in_btw)) / 
        sum_flow_l_in
      error_out = sum(abs(flow_l_out - sigma_out*d2f_new - node_out_btw)) / 
        sum_flow_l_out
      diff_flow = sp_flow_vec - reduced_flow
      max_diff_flow = max(diff_flow)
      couple = sp_ref[which(diff_flow ==max_diff_flow), ]
      if(!is.null(dim(couple))){
        couple = couple[1, ]
      }
      # Print iteration statistics
      if (display_it) {
        cat("It", it_algo, ": diff_f =", diff_f, ", diff_g =", diff_g, 
            ", error_in =", error_in, ", error_out =", error_out, 
            ", max_diff_flow =", max_diff_flow, ", where =", couple,"\n")
      }
      # Check for convergence
      if(diff_f < conv_thres_algo){
        converge_algo = T
      }
      # If return for each it
      if(return_it){
        n_mat_list[[it_algo]] = n_mat
      }
      # If iteration details
      if(return_details){
        row_df = data.frame(it_algo=it_algo,
                            diff_f=diff_f,
                            diff_g=diff_g,
                            error_in=error_in,
                            error_out=error_out,
                            max_diff_flow=max_diff_flow,
                            couple_1=couple[1],
                            couple_2=couple[2])
        iterations_df = rbind(iterations_df, row_df)
      }
      # Iteration
      it_algo = it_algo + 1
    }
    
    # Return results
    results = NULL
    if(!return_it & !return_details){
      results = n_mat
    } else if (!return_it & return_details){
      results = list(n_mat=n_mat, iterations_df=iterations_df)
    } else if (return_it & !return_details){
      results = n_mat_list
    } else {
      results = list(n_mat_list=n_mat_list, iterations_df=iterations_df)
    }
    return(results)
  }

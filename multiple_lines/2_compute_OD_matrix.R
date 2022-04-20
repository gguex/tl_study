  #-------------------------------------------------------------------------------
  #
  # Run the algorithm from preprocessed data
  #
  #-------------------------------------------------------------------------------
  
  #--------------------------------
  # Head
  #--------------------------------
  
  # Set working directory path
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  # Load functions
  source("local_functions.R")
  
  #--------------------------------
  # Parameters
  #--------------------------------
  
  # The folder containing preprocessed data
  data_folder = "multilines_data/preprocessed_data/test_6789"
  # Output folder for results
  out_folder = "results/test_6789"
  
  # Conv threshold for iterative fitting
  conv_thres_if = 10
  # Conv threshold
  conv_thres_algo = 0.1
  # Smooth or strict limit
  smooth_limit = T
  # lambda for exponential law
  exp_lambda = 1.5
  # prop limit 
  prop_limit = 0.2
  # epsilon
  epsilon = 1e-40
  # max iteration
  max_it = 200
  
  #--------------------------------
  # Process
  #--------------------------------
  
  # --- Load the data
  
  # Load the line dataframe
  line_df = read.csv(paste0(data_folder, "/line_df.csv"))
  rho_in = line_df$rho_in
  rho_out = line_df$rho_out
  # Load edge_ref
  edge_ref = as.matrix(read.csv(paste0(data_folder, "/edge_ref.csv")))
  # Load sp_ref
  sp_ref = as.matrix(read.csv(paste0(data_folder, "/sp_ref.csv")))
  # Load p_mat
  p_mat = as.matrix(read.csv(paste0(data_folder, "/p_mat.csv"), header=F))
  
  # --- Run the algorithm 
  
  n_mat = compute_origin_destination(rho_in,
                                     rho_out,
                                     edge_ref,
                                     sp_ref, 
                                     p_mat, 
                                     smooth_limit=smooth_limit,
                                     prop_limit=prop_limit,
                                     exp_lambda=exp_lambda,
                                     conv_thres_algo=conv_thres_algo,
                                     conv_thres_if=conv_thres_if,
                                     epsilon=epsilon,
                                     max_it=max_it)
  
  # --- See the result
  
  # Compute the between lines flow on node 
  x_res = compute_x_from_n(n_mat, edge_ref, sp_ref, p_mat)
  node_in_btw = colSums(x_res$x_btw)
  node_out_btw = rowSums(x_res$x_btw)
  
  # Save data 
  write.table(x_res$x_mat, paste0(out_folder, "/x_mat.csv"), sep=",",
              row.names=F, col.names=F)
  write.table(n_mat, paste0(out_folder, "/n_mat.csv"), sep=",",
              row.names=F, col.names=F)

  # Update dataframe and View it
  line_res = line_df
  line_res["montees_initiales"] = round(rowSums(n_mat), 3)
  line_res["transferts_in"] = round(node_in_btw, 3)
  line_res["transferts_in%"] = round(line_res["transferts_in"] / line_res["rho_in"] * 100, 3)
  line_res["diff_in"] = round(line_res["montees_initiales"] + line_res["transferts_in"] - line_res["rho_in"], 3)
  line_res["err_in%"] = round(line_res["diff_in"] / line_res["rho_in"] * 100, 3)
  line_res["descentes_finales"] = round(colSums(n_mat), 3)
  line_res["transferts_out"] = round(node_out_btw, 3)
  line_res["transferts_out%"] = round(line_res["transferts_out"] / line_res["rho_out"] * 100, 3)
  line_res["diff_out"] = round(line_res["descentes_finales"] + line_res["transferts_out"] - line_res["rho_out"], 3)
  line_res["err_out%"] = round(line_res["diff_out"] / line_res["rho_out"] * 100, 3)
  View(line_res)
  
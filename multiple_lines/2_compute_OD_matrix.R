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

# Conv threshold for iterative fitting
conv_thres_if = 100
# Conv threshold
conv_thres_algo = 1e-5
# Smooth or strict limit
smooth_limit = F
# lambda for exponential law
exp_lambda = 100
# prop limit 
prop_limit = 0.2
# epsilon
epsilon = 1e-40
# max iteration
max_it = 30

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
                                   smooth_limit = smooth_limit,
                                   prop_limit = prop_limit,
                                   exp_lambda = exp_lambda,
                                   conv_thres_algo=conv_thres_algo,
                                   conv_thres_if=conv_thres_if,
                                   epsilon=epsilon,
                                   max_it=max_it)

# --- See the result

# The reference of transfer edges
where_edge_btw = as.logical(edge_ref[, 3])
# The list of in-out nodes for transfer edges 
edge_btw_ref = edge_ref[where_edge_btw, 1:2]
# The shortest-path - edge matrix retrained on transfer edges
p_btw_mat = p_mat[, where_edge_btw]
# Get the flow as vector in shortest-path order
sp_flow_vec = mapply(function(i, j) n_mat[i, j], sp_ref[,1], sp_ref[,2])
# Compute the flow on edges
btw_edge_flow = as.vector(t(p_btw_mat) %*% sp_flow_vec)
# Compute the in-out between flow on nodes 
x_btw = sparseMatrix(i=edge_btw_ref[, 1], j=edge_btw_ref[, 2], 
                     x=btw_edge_flow, dims=c(length(rho_in), length(rho_in)))
node_in_btw = colSums(x_btw)
node_out_btw = rowSums(x_btw)

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
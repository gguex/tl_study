#-------------------------------------------------------------------------------
#
# Run the algorithm from pre-processed data
#
#-------------------------------------------------------------------------------

#--------------------------------
# Head
#--------------------------------

# Set working directory path
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load functions
source("local_functions.R")

#--------------------------------
# Parameters
#--------------------------------

# --- Test 6789

# # The folder containing pre-processed data
# data_folder = "multilines_data/preprocessed_data/test_mc_6789"
# # Output folder for results
# out_folder = "results/test_mc_6789"

# --- all lines

# The folder containing pre-processed data
data_folder = "multilines_data/preprocessed_data/all_lines"
# Output folder for results
out_folder = "results/all_lines"

# Conv threshold for iterative fitting
conv_thres_if = 1e-5
# Conv threshold
conv_thres_algo = 1e-5
# proportional limit 
min_p_ntwk = 0.1
# epsilon
epsilon = 1e-40
# max iteration
max_it = 400

#--------------------------------
# Process
#--------------------------------

# --- Load the data

# Load the line dataframe
line_df = read.csv(paste0(data_folder, "/line_df.csv"))
flow_l_in = line_df$flow_l_in
flow_l_out = line_df$flow_l_out
# Load edge_ref
edge_ref = as.matrix(read.csv(paste0(data_folder, "/edge_ref.csv")))
# Load sp_ref
sp_ref = as.matrix(read.csv(paste0(data_folder, "/sp_ref.csv")))
# Load sp_edge_link
sp_edge_link = readMM(paste0(data_folder, "/sp_edge_link.mtx"))

# --- Run the algorithm 

n_mat = compute_origin_destination(flow_l_in,
                                   flow_l_out,
                                   edge_ref,
                                   sp_ref, 
                                   sp_edge_link, 
                                   min_p_ntwk=min_p_ntwk,
                                   conv_thres_algo=conv_thres_algo,
                                   conv_thres_if=conv_thres_if,
                                   epsilon=epsilon,
                                   max_it=max_it)

# --- Save the results

# Compute the between lines flow on node 
x_res = compute_x_from_n(n_mat, edge_ref, sp_ref, sp_edge_link)
node_in_btw = colSums(x_res$x_btw)
node_out_btw = rowSums(x_res$x_btw)

# Updated dataframe
line_res = line_df
line_res["flow_n_in"] = round(rowSums(n_mat), 3)
line_res["transfers_in"] = round(node_in_btw, 3)
line_res["transfers_in%"] = round(line_res["transfers_in"] / 
                                     line_res["flow_l_in"] * 100, 3)
line_res["diff_in"] = round(line_res["flow_n_in"] + 
                              line_res["transfers_in"] - 
                              line_res["flow_l_in"], 3)
line_res["err_in%"] = round(line_res["diff_in"] / 
                              line_res["flow_l_in"] * 100, 3)
line_res["flow_n_out"] = round(colSums(n_mat), 3)
line_res["transfers_out"] = round(node_out_btw, 3)
line_res["transfers_out%"] = round(line_res["transfers_out"] / 
                                      line_res["flow_l_out"] * 100, 3)
line_res["diff_out"] = round(line_res["flow_n_out"] + 
                               line_res["transfers_out"] - 
                               line_res["flow_l_out"], 3)
line_res["err_out%"] = round(line_res["diff_out"] / 
                               line_res["flow_l_out"] * 100, 3)

# Create dir if it do not exist
if(!dir.exists(out_folder)){
  dir.create(out_folder)
}

write.table(line_res, paste0(out_folder, "/line_res.csv"), sep=",",
            row.names=F)
write.table(n_mat, paste0(out_folder, "/n_mat.csv"), sep=",",
            row.names=F, col.names=F)
write.table(x_res$x_mat, paste0(out_folder, "/x_mat.csv"), sep=",",
            row.names=F, col.names=F)
write.table(x_res$x_btw, paste0(out_folder, "/x_btw.csv"), sep=",",
            row.names=F, col.names=F)

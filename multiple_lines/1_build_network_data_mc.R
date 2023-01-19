#-------------------------------------------------------------------------------
#
# Preprocess data and save it
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

# --- TEST 6789

# # The file containing the dataset
line_data_file = "multilines_data/formatted_data/test_6789/bus_df.csv"
# # The file containing the pedestrian distance between stops
d_ped_file = "multilines_data/formatted_data/test_6789/ped_time.csv"
# # Out folder
out_folder = "multilines_data/preprocessed_data/test_mc_6789"

# --- ALL LINES

# Pedestrian threshold for connecting stops
ped_t_thres = 120
# The mean waiting time to enter a bus line
waiting_time = 120

#--------------------------------
# Process
#--------------------------------

# Load the line file
line_df = read.csv(line_data_file)
# Load the distance file
ped_t_mat = as.matrix(read.csv(d_file, header=F))
# Save n
n = dim(line_df)[1]
# Line memberships 
line_mbrshps = interaction(line_df$line_nbr, line_df$direction)
# Tour memberships
tour_mbrshps = line_df$line_nbr

# Build the network structure 
res_net_list = build_network_structure(line_mbrshps, 
                                       tour_mbrshps,
                                       ped_t_mat,
                                       ped_t_thres)

# Travel time vec
travel_t_vec = line_df$travel_time
# Waiting time vec
waiting_t_vec = rep(waiting_time, n)
# Adjacency within
adj_w = res_net_list$adj_w
# Adjacency between
adj_b = res_net_list$adj_b
# Number of cores
mc.cores = detectCores() - 2



# Build the shortest path data
res_sp_list = build_sp_data_mc(line_mbrshps, 
                               tour_mbrshps,
                               travel_t_vec,
                               waiting_t_vec,
                               ped_t_mat, 
                               adj_w,
                               adj_b, 
                               mc.cores=mc.cores)

# Build the corrected balance 
res_inout_list = balance_flow_l_inout(line_mbrshps,
                                      line_df$passengers_in, 
                                      line_df$passengers_out)

# Add corrected in/out data to dataframe
line_df["flow_l_in"] = res_inout_list$flow_l_in
line_df["flow_l_out"] = res_inout_list$flow_l_out

# Save data 
write.table(res_net_list$adj_w, paste0(out_folder, "/adj_w.csv"), sep=",",
            row.names=F, col.names=F)
write.table(res_net_list$adj_b, paste0(out_folder, "/adj_b.csv"), sep=",",
            row.names=F, col.names=F)
write.table(res_sp_list$edge_ref, paste0(out_folder, "/edge_ref.csv"), sep=",",
            row.names=F)
write.table(res_sp_list$sp_ref, paste0(out_folder, "/sp_ref.csv"), sep=",",
            row.names=F)
write.table(as.matrix(res_sp_list$sp_edge_link), 
            paste0(out_folder, "/sp_edge_link.csv"), sep=",", 
            row.names=F, col.names=F)
write.table(line_df, paste0(out_folder, "/line_df.csv"), sep=",",
            row.names=F)




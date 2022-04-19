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

# The file containing the dataset
line_data_file = "multilines_data/bus_df.csv"
# The file containing the pedestrian distance between stops
d_file = "multilines_data/ped_time.csv"
# Out folder
out_folder = "network_data"

# Pedestrian threshold for connecting stops
d_ped_threshold = 120
# The mean waiting time to enter a bus line
waiting_time = 120

#--------------------------------
# Process
#--------------------------------

# Load the line file
line_df = read.csv(line_data_file)
# Load the distance file
d_ped = as.matrix(read.csv(d_file, header=F))
# Save n
n = dim(line_df)[1]

# Build the network structure 
res_net_list = build_network_structure(interaction(line_df$line_nbr, 
                                                   line_df$direction), 
                                       line_df$line_nbr,
                                       d_ped,
                                       d_ped_threshold)

# Build the shortest path data
res_sp_list = build_sp_data(interaction(line_df$line_nbr, 
                                        line_df$direction), 
                            line_df$line_nbr,
                            line_df$travel_time,
                            rep(waiting_time, n),
                            d_ped, 
                            res_net_list$A_W,
                            res_net_list$A_B)

# Build the corrected balance 
res_inout_list = build_in_out_flow(interaction(line_df$line_nbr, 
                                               line_df$direction), 
                                   line_df$passengers_in, 
                                   line_df$passengers_out)




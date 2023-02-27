#-------------------------------------------------------------------------------
#
# Plot the graphic for MME error vs iterations
#
#-------------------------------------------------------------------------------

#--------------------------------
# Head
#--------------------------------

# Load packages 
library("tidyverse")

#--------------------------------
# Parameters
#--------------------------------

# The folder containing results
data_folder = "multilines_data/preprocessed_data/all_lines"
results_folder = "results/all_lines2"

#--------------------------------
# Process
#--------------------------------

# Load network data
edge_ref = as.matrix(read.csv(paste0(data_folder, "/edge_ref.csv")))
sp_ref = as.matrix(read.csv(paste0(data_folder, "/sp_ref.csv")))
sp_edge_link = readMM(paste0(data_folder, "/sp_edge_link.mtx"))

# Load results
line_res = read.csv(paste0(results_folder, "/line_res.csv")) 
n_mat = as.matrix(read.csv(paste0(results_folder, "/n_mat.csv"), header=F))
stop_names = line_res$stop_names

# Compute transfers
x_res = compute_x_from_n(n_mat, edge_ref, sp_ref, sp_edge_link)
x_btw = x_res$x_btw

# Get the most transfer
x_to_empty = x_btw
transfer_df = data.frame(id_i=NULL, tag_i=NULL, stop_i=NULL, 
                         line_i=NULL, direction_i=NULL,
                         id_j=NULL, tag_j=NULL, stop_j=NULL, 
                         line_j=NULL, direction_j=NULL,
                         number_of_transfers=NULL)
while(sum(x_to_empty) != 0){
  n_of_trfr = max(x_to_empty)
  indices_stop = which(x_to_empty==n_of_trfr, arr.ind=T)
  row_df = data.frame(id_i=indices_stop[1],
                      tag_i=line_res$stop_names[indices_stop[1]],
                      stop_i=line_res$label[indices_stop[1]],
                      line_i=line_res$line_nbr[indices_stop[1]],
                      direction_i=line_res$direction[indices_stop[1]],
                      id_j=indices_stop[2],
                      tag_j=line_res$stop_names[indices_stop[2]],
                      stop_j=line_res$label[indices_stop[2]],
                      line_j=line_res$line_nbr[indices_stop[2]],
                      direction_j=line_res$direction[indices_stop[2]],
                      number_of_transfers=n_of_trfr)
  transfer_df = rbind(transfer_df, row_df)
  x_to_empty[indices_stop[1], indices_stop[2]] = 0
}

write.table(transfer_df, paste0(results_folder, "/transfer_df.csv"), sep=",",
            row.names=F)




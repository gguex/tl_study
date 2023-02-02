###############################################################
# CODE R : multi-line model
###############################################################

############################################################### 
####### LIBRARIES

library(igraph)

############################################################### 
####### Setting paths

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

############################################################### 
####### PARAMETERS

compute_sp_data = F
epsilon = 1e-30
conv_thres_if = 1e-15
conv_thres_algo = 1e-15
local_lambda = 1
flow_error = 1e-20

############################################################### 
####### LOADING AND PREPROCESSING MATRICES


# --- Loading adjacency matrices 

# Loading adjacency matrix without loop
adj_line = read.csv("data_multiligne/adj_next.csv", sep=",", row.names = 1)
adj_line = as.matrix(adj_line)
diag(adj_line) = 0

# Loading adjacency matrix with loops on the end of the line
adj_line_wloop = read.csv("data_multiligne/adj_next_bind.csv", sep=",", row.names = 1)
adj_line_wloop = as.matrix(adj_line_wloop)
diag(adj_line_wloop) = 0

# Component for line
g_adj_wloop = graph_from_adjacency_matrix(adj_line_wloop, mode = "directed")
line_comp = components(g_adj_wloop)$membership

# Loading proximity matrix
adj_prox = read.csv("data_multiligne/adj_prox.csv", sep=",", row.names = 1)
adj_prox = as.matrix(adj_prox)
diag(adj_prox) = 0

# Erease same line join
for(id_line in 1:max(line_comp)){
  adj_prox[line_comp == id_line, line_comp == id_line] = 0
}

# Saving stop names and renaming columns
stop_names = trimws(paste0("S", rownames(adj_line)))
colnames(adj_line) = stop_names
rownames(adj_line) = stop_names
colnames(adj_line_wloop) = stop_names
rownames(adj_line_wloop) = stop_names
colnames(adj_prox) = stop_names
rownames(adj_prox) = stop_names

# Number of stops
n = length(stop_names)

# Correction for the 7th line 
adj_line[which(stop_names == "S7_R_SF_O"), which(stop_names == "S7_A_SF_O")] = 1

# Suppressing prox when connected by bus line
adj_prox[as.logical(adj_line)] = 0

# Full adjacency (setting names to NULL for graph constructions)
adj_full = adj_line + adj_prox
colnames(adj_full) = NULL
rownames(adj_full) = NULL


# --- Time matrices and vectors 

# Pedestrian time between stops
ped_time_mat = read.csv("data_multiligne/time_mat_s_6789_names.csv", sep=",")
ped_time_mat = as.matrix(ped_time_mat[, -1])
colnames(ped_time_mat) = stop_names
rownames(ped_time_mat) = stop_names

# Mean stop time at bus stops
stop_time_df = read.csv("data_multiligne/waiting_time_s_tp.csv", sep=",")
# Get the name of stop
stop_time_rownames = trimws(paste(paste0("S", stop_time_df$code_ligne_theo), 
                           stop_time_df$direction_voy_theo, 
                           stop_time_df$code_arret_theo, sep="_"))

# Get the mean stopping time 
stop_time_vec = c()
for (i in 1:length(stop_names)){
  stop_time = stop_time_df[which(stop_time_rownames == stop_names[i]),]$mean_waiting_time
  stop_time_vec = c(stop_time_vec, stop_time)
}
names(stop_time_vec) = stop_names
# Limit the time
stop_time_vec[stop_time_vec > 100] = 100
 
# Bus time between stops
mean_time_df = read.csv("data_multiligne/mean_time_i-j_s_tp.csv", sep=",")
# Get the name of starting stop
mt_rownames = trimws(paste(paste0("S", mean_time_df$code_ligne_theo), 
                           mean_time_df$direction_voy_theo, 
                           mean_time_df$code_arret_prec_theo, sep="_"))
# Compute the bus time mat
bus_time_mat = adj_line
for (i in 1:length(stop_names)){
  travel_time = mean_time_df[which(mt_rownames == stop_names[i]),]$mean_time + stop_time_vec[i]
  if (length(travel_time) == 0) travel_time = 1
  bus_time_mat[i, ] = adj_line[i, ] * travel_time
}

# Define the changing time
change_time = 200

# The full time matrix
time_full = bus_time_mat + (adj_prox * change_time)

############################################################### 
####### CREATING EDGE DATA

# Make the full graph
full_g = graph_from_adjacency_matrix(adj_full, mode = "directed")
edge_mat = as_edgelist(full_g)
n_edges = dim(edge_mat)[1]

# Vector giving index of free edges  
free_edge_vec = rep(F, dim(edge_mat)[1])
for(i in 1:length(free_edge_vec)){
  free_edge_vec[i] = as.logical(adj_prox[edge_mat[i, 1], edge_mat[i, 2]])
}

############################################################### 
####### CREATING SHORTEST-PATHS DATA

# --- --- COMPUTING (if wanted, slow)

if(compute_sp_data){
  
  # --- Compute admissible sp in term of travel time
  
  # Graphs with weights
  full_time_g = graph_from_adjacency_matrix(time_full, mode="directed", weighted=T)
  
  # Bus time between stops
  bus_time_mat = distances(full_time_g, mode="out")
  
  # Matrix of forbidden paths between stops
  admissible_sp_mat = (bus_time_mat < ped_time_mat)
  admissible_sp_mat[adj_line > 1] = 1
  
  # Create the file for the matrix 
  file.create("aux_data/sp_edge_mat.csv")
  
  # The shortest-path x edge matrix
  sp_edge_mat = c()
  # The shortest-path x (source, target) matrix
  sp_mat = c()
  # Loop on every node pair
  for(i in 1:n){
    cat("Node: ", i, "; ")
    for(j in 1:n){
      # Create the row for sp_edge_mat
      sp_vec = rep(0, n_edges)
      # If it's already admissible
      if(admissible_sp_mat[i, j] & (i!=j)){
        # Get the shortest path
        sp = get.shortest.paths(full_g, i, j)$vpath[[1]]
        # If it does not begin or finish with an adj_prox edge
        if(length(sp) > 1){
          if( !(adj_prox[sp[length(sp) - 1], sp[length(sp)]] | adj_prox[sp[1], sp[2]]) ){
            # Put 1 on every edges
            for(node_id in 1:(length(sp) - 1)){
              edge_id = which(edge_mat[, 1] == sp[node_id] & edge_mat[, 2] == sp[node_id + 1])
              sp_vec[edge_id] = 1
            }
            # If it does finish with an adj_prox edge, remove it from admissible sp
          } else {
            admissible_sp_mat[i, j] = 0
          }
        } else {
          admissible_sp_mat[i, j] = 0
        }
      }
      # Add the row on sp_edge_mat
      write.table(t(sp_vec), "aux_data/sp_edge_mat.csv", append=T, row.names=F, col.names=F, sep=",")
    }
  }
  write.table(1*admissible_sp_mat, "aux_data/admissible_sp_mat.csv", row.names=F, col.names=F, sep=",")
}

# --- --- LOADING

# Loading files
admissible_sp_mat = as.matrix(read.csv("aux_data/admissible_sp_mat.csv", header=F))
sp_edge_mat = as.matrix(read.csv("aux_data/sp_edge_mat.csv", header=F))

# Setting col and row names of admissible paths
rownames(admissible_sp_mat) = stop_names
colnames(admissible_sp_mat) = stop_names

############################################################### 
####### COMPUTING FREE NODE DATA

free_node_vec = rep(T, n)
free_node_vec[rowSums(admissible_sp_mat) == 0] = F
free_node_vec[colSums(admissible_sp_mat) == 0] = F
free_node_vec[rowSums(adj_prox) == 0] = F
free_node_vec[colSums(adj_prox) == 0] = F

############################################################### 
####### LOADING AND PREPROCESSING IN, OUT AND TRAVEL DATA

# Loading in/out data
inout_data = read.csv("data_multiligne/stops_frequentation_6789.csv", sep=",",
                      row.names = 1)
inout_data["stop_names"] = stop_names

# The table for corrected data
inout_cor = inout_data
inout_cor$outflow = 0

# the ID of line 
id_line_vec = unique(inout_data$code_ligne_theo)
# The direction
direction_vec = c("A", "R")

# Creating the flow from data (with correction)
for(id_line in id_line_vec){
  for(direction in direction_vec){
    
    # Line 7 is particular
    if(id_line == 7){
      selected_ind = which(inout_data$code_ligne_theo == id_line & inout_data$direction_voy_theo == "R")
      selected_plus = which(inout_data$code_ligne_theo == id_line & inout_data$direction_voy_theo == "A")
      selected_ind = c(selected_ind, selected_plus)
    }
    else{
      selected_ind = which(inout_data$code_ligne_theo == id_line & inout_data$direction_voy_theo == direction)
    }
    
    # We compute the flow
    line_data = inout_data[selected_ind, ]
    line_balance = as.vector(t(line_data["montees"] - line_data["descentes"]))
    line_flow = cumsum(line_balance)
    
    # If some people are left in the end of the line, we correct the values
    correction = line_flow[length(line_flow)]
    inout_cor[selected_ind[-1], ]$descentes = inout_data[selected_ind[-1], ]$descentes + correction / length(selected_ind[-1])
    new_line_data = inout_cor[selected_ind, ]
    new_line_balance = as.vector(t(new_line_data["montees"] - new_line_data["descentes"]))
    new_line_flow = round(cumsum(new_line_balance), 2)
    
    # Saving the corrected results in the new dataset
    inout_cor[selected_ind, "outflow"] = new_line_flow
  }
}

# Fixed distributions of in and out
pi_in = inout_cor$montees + epsilon
pi_in = pi_in / sum(pi_in)
pi_out = inout_cor$descentes + epsilon
pi_out = pi_out / sum(pi_out)

############################################################### 
####### Algorithm 

# Init
sigma_in = pi_in
sigma_out = pi_out
p_sigma_in = sigma_in
p_sigma_out = sigma_out
converge_algo = F
epoch_algo = 1

while(!converge_algo){
  
  # Save old distrib values
  p_sigma_in_old = p_sigma_in
  p_sigma_out_old = p_sigma_out
  
  # Shuffle free node
  free_node_shuffled = sample(which(free_node_vec))
  # Reset id 
  id_sample = 1
  # edge_cycle_done
  node_cycle_done = F
  # Has mod
  modification_occured = T
  
  while(!node_cycle_done){
    
    # --- --- Iterative fitting 
    
    if(modification_occured){
      
      converge_if = F
      results_mat = admissible_sp_mat + epsilon
      while(!converge_if){
        # Saving old results
        results_mat_old = results_mat 
        # Normalizing by row
        results_mat = results_mat * sigma_in / rowSums(results_mat)
        # Normalizing by columns 
        results_mat = t(t(results_mat) * sigma_out / rowSums(t(results_mat)))
        # Checking for convergence
        if(sum(abs(results_mat_old - results_mat)) < conv_thres_if){
          converge_if = T
        }
      }
      
      # Find flow on edges 
      total_edge_flow = as.vector(t(sp_edge_mat) %*% c(results_mat))
      
      # Modification occured off
      modification_occured = F
    }
    
    # --- --- Flow correction on node
    
    # Take the node
    id_node = free_node_shuffled[id_sample]
    
    # Free out/in-going flow from node in and out
    id_free_edge_from = intersect(which(edge_mat[,1] == id_node), which(free_edge_vec))
    id_free_edge_to = intersect(which(edge_mat[,2] == id_node), which(free_edge_vec))
    
    # Outgoing and ingoing flow
    free_outgoing_flow = sum(total_edge_flow[id_free_edge_from])
    free_ingoing_flow = sum(total_edge_flow[id_free_edge_to])
    
    # Transfer flow 
    trans_flow_out = pi_out[id_node] - sigma_out[id_node] 
    trans_flow_in = pi_in[id_node] - sigma_in[id_node] 
    
    # Outgoing flow management
    if(trans_flow_out - free_outgoing_flow < -flow_error){
      
      # Print Red-out
      cat("Red-out;")
      
      # Modification occured on
      modification_occured = T
      
      # Quantity to reduce
      to_reduce_outgoing = local_lambda*(free_outgoing_flow - trans_flow_out)
      
      # Trying to see if possible to reduce locally
      if(sigma_out[id_node] < to_reduce_outgoing) to_reduce_outgoing = sigma_out[id_node]
      
      # Reduce the outgoing flow 
      sigma_out[id_node] = sigma_out[id_node] - to_reduce_outgoing + epsilon
      
    } else if(trans_flow_out - free_outgoing_flow > flow_error){
      
      # Print Inc-out
      cat("Inc-out;")
      
      # Modification occured on
      modification_occured = T
      
      # Increase the outgoing flow
      sigma_out[id_node] = sigma_out[id_node] + local_lambda*(trans_flow_out - free_outgoing_flow) + epsilon
      
    }
    
    # Ingoing flow management
    if(trans_flow_in - free_ingoing_flow < -flow_error){
      
      # Print Red-in
      cat("Red-in;")
      
      # Modification occured on
      modification_occured = T
      
      # Quantity to reduce
      to_reduce_ingoing = local_lambda*(free_ingoing_flow - trans_flow_in)
      
      # Trying to see if possible to reduce locally
      if(sigma_in[id_node] < to_reduce_ingoing) to_reduce_ingoing = sigma_in[id_node]
      
      # Reduce the ingoing flow 
      sigma_in[id_node] = sigma_in[id_node] - to_reduce_ingoing + epsilon
      
    } else if(trans_flow_in - free_ingoing_flow > flow_error){
      
      # Print Inc-in
      cat("Inc-in;")
      
      # Modification occured on
      modification_occured = T
      
      # Increase the outgoing flow
      sigma_in[id_node] = sigma_in[id_node] + local_lambda*(trans_flow_in - free_ingoing_flow) + epsilon
      
    }
  
    # Iterate on node 
    
    # Print node id
    if(modification_occured){
      cat("|", id_node, "|\n")
    }
    
    id_sample = id_sample + 1
    if(id_sample > length(free_node_shuffled)){
      node_cycle_done = T
    }
    
  }
  
  # --- --- Compute in-out balance 
  
  # IF
  converge_if = F
  results_mat = admissible_sp_mat + epsilon
  while(!converge_if){
    # Saving old results
    results_mat_old = results_mat 
    # Normalizing by row
    results_mat = results_mat * sigma_in / rowSums(results_mat)
    # Normalizing by columns 
    results_mat = t(t(results_mat) * sigma_out / rowSums(t(results_mat)))
    # Checking for convergence
    if(sum(abs(results_mat_old - results_mat)) < conv_thres_if){
      converge_if = T
    }
  }
  
  # Find flow on edges 
  total_edge_flow = as.vector(t(sp_edge_mat) %*% c(results_mat))
  
  # Find flow on free edges
  in_error_vec = c()
  out_error_vec = c()
  for(id_node in 1:n){
    
    # Free out/in-going flow from node in and out
    id_free_edge_from = intersect(which(edge_mat[,1] == id_node), which(free_edge_vec))
    id_free_edge_to = intersect(which(edge_mat[,2] == id_node), which(free_edge_vec))
    
    # Outgoing and ingoing flow
    free_outgoing_flow = sum(total_edge_flow[id_free_edge_from])
    free_ingoing_flow = sum(total_edge_flow[id_free_edge_to])
    
    # Errors vec
    in_error_vec = c(in_error_vec, pi_in[id_node] - (sigma_in[id_node] + free_ingoing_flow))
    out_error_vec = c(out_error_vec, pi_out[id_node] - (sigma_out[id_node] + free_outgoing_flow))
  }
  
  # Total error in and out
  out_error = sum(abs(out_error_vec))
  in_error = sum(abs(in_error_vec))
  
  # --- --- Check for convergence and iterate
  p_sigma_in = sigma_in / sum(sigma_in)
  p_sigma_out = sigma_out / sum(sigma_out)
  
  diff = sum(abs(p_sigma_in - p_sigma_in_old)) + sum(abs(p_sigma_out - p_sigma_out_old))
    
  cat("Epoch", epoch_algo, "ends, diff =", diff, ", out_error = ", out_error, "in_error = ", in_error,"\n")
  if(out_error + in_error < conv_thres_algo){
    converge_algo = T
  }
  
  #Epoch iteration
  epoch_algo = epoch_algo + 1
}
  
# END 

# Normalizing
sigma_in = sigma_in / sum(sigma_in)
sigma_out = sigma_out / sum(sigma_out)
# Last Iterative fitting
converge_if = F
results_mat = admissible_sp_mat + epsilon
while(!converge_if){
  # Saving old results
  results_mat_old = results_mat 
  # Normalizing by row
  results_mat = results_mat * sigma_in / rowSums(results_mat)
  # Normalizing by columns 
  results_mat = t(t(results_mat) * sigma_out / rowSums(t(results_mat)))
  # Checking for convergence
  if(sum(abs(results_mat_old - results_mat)) < conv_thres_if){
    converge_if = T
  }
}

# Computing flow on edges
total_edge_flow = as.vector(t(sp_edge_mat) %*% c(results_mat))

# To num number
to_num = inout_cor$montees[1] / rowSums(results_mat_old)[1]

# See link
non_null_edge = edge_mat[(round(total_edge_flow, 15) > 0) & free_edge_vec, ]
in_node_nn = stop_names[non_null_edge[,1]]
out_node_nn = stop_names[non_null_edge[,2]]
df_free_edge = data.frame(from=in_node_nn, to=out_node_nn, 
                          quant=total_edge_flow[(round(total_edge_flow, 10) > 0) & free_edge_vec] * to_num)

# Computing in-network/def, out-network/def
transfer_out = c()
transfer_in = c()
for(id_node in 1:n){
  
  # Free out/in-going flow from node in and out
  id_free_edge_from = intersect(which(edge_mat[,1] == id_node), which(free_edge_vec))
  id_free_edge_to = intersect(which(edge_mat[,2] == id_node), which(free_edge_vec))
  
  # Outgoing and ingoing flow
  free_outgoing_flow = sum(total_edge_flow[id_free_edge_from])
  free_ingoing_flow = sum(total_edge_flow[id_free_edge_to])
  
  # flow transfer 
  transfer_out = c(transfer_out, free_outgoing_flow)
  transfer_in = c(transfer_in, free_ingoing_flow)
}
  
full_df = inout_cor
full_df["montees_initiales"] = round(sigma_in * to_num)
full_df["transferts_in"] = round(transfer_in * to_num)
full_df["diff_in"] = full_df["montees_initiales"] + full_df["transferts_in"] - full_df["montees"]
full_df["descentes_finales"] = round(sigma_out * to_num)
full_df["transferts_out"] = round(transfer_out * to_num)
full_df["diff_out"] = full_df["descentes_finales"] + full_df["transferts_out"] - full_df["descentes"]

View(full_df)
View(df_free_edge)

# View
id_arret = 30
arret_prob = rev(results_mat_old[id_arret, ])
barplot(arret_prob, las=2, horiz=T, cex.names=0.5, main=stop_names[id_arret])

res_num = round(results_mat / rowSums(results_mat_old)[1] * inout_cor$montees[1])

# TESTs (to eventually earse)

which_edge = c(39, 51)
edge_id =  which(edge_mat[, 1] == which_edge[1] & edge_mat[, 2] == which_edge[2])

sp_mat_from_edge = matrix(sp_edge_mat[, edge_id], n, n, byrow=T)
sp_node_mat = which(sp_mat_from_edge > 0, arr.ind = T)

nomb_t = c()
for(i in 1:dim(sp_node_mat)[1]){
  nomb_t = c(nomb_t, res_num[sp_node_mat[i,1], sp_node_mat[i,2]])
}
df_sp = data.frame(stop_names[sp_node_mat[,1]], stop_names[sp_node_mat[,2]], nomb_t)

###############################################################
# CODE R : multi-line model
###############################################################

############################################################### 
####### LIBRARIES

library(igraph)
library(Matrix)

############################################################### 
####### Setting paths

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

############################################################### 
####### PARAMETERS

compute_sp_data = F
epsilon = 1e-40
conv_thres_if = 1e-5
conv_thres_algo = 1e-5
min_sigma = 0.0001

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

# Erase same line join
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
nodefrom_edge_mat = matrix(0, n, n_edges)
nodeto_edge_mat = matrix(0, n, n_edges) 
for(i in 1:n_edges){
  nodefrom_edge_mat[edge_mat[i, 1], i] = 1
  nodeto_edge_mat[edge_mat[i, 2], i] = 1
}

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
rho_in = inout_cor$montees + epsilon
rho_in = rho_in / sum(rho_in)
rho_out = inout_cor$descentes + epsilon
rho_out = rho_out / sum(rho_out)




############################################################### 
####### Algorithm 

# Aux
edge_to_sp = t(sp_edge_mat[, free_edge_vec])

# Init
sigma_in = rho_in
sigma_out = rho_out
min_sigma_in = rho_in * min_sigma
min_sigma_out = rho_out * min_sigma
converge_algo = F
it_algo = 1
X_b = matrix(1, n, n)
relationship_ref_mat = admissible_sp_mat
total_rel = sum(admissible_sp_mat)
max_to_red = 1

while(!converge_algo){
  
  # --- --- Save old X_b
  
  X_b_old = X_b 
  max_to_red_old = max_to_red
  
  # --- --- Iterative fitting 
  
  converge_if = F
  results_mat = relationship_ref_mat + epsilon
  while(!converge_if){
    # Saving old results
    results_mat_old = results_mat 
    # Normalizing by row
    results_mat = results_mat * sigma_in / rowSums(results_mat + epsilon)
    # Normalizing by columns 
    results_mat = t(t(results_mat) * sigma_out / colSums(results_mat + epsilon))
    # Checking for convergence
    if(sum(abs(results_mat_old - results_mat)) < conv_thres_if){
      converge_if = T
    }
  }

  # Find flow on edges and free edges
  total_edge_flow = as.vector(t(sp_edge_mat) %*% c(t(results_mat)))
  free_edge_flow = total_edge_flow[free_edge_vec]
    
  # --- --- In and out-flow correction on nodes
  
  # The flow on free edges
  free_edge_loc = edge_mat[free_edge_vec, ]
  X_b = sparseMatrix(i=free_edge_loc[, 1], 
                     j=free_edge_loc[, 2], 
                     x=free_edge_flow, dims=c(n, n))
  from_free_flow = colSums(X_b)
  to_free_flow = rowSums(X_b)
  
  ## FOR DEBUG
  # df_edge = data.frame(free_edge_loc[, 1],free_edge_loc[, 2],free_edge_flow)
  # df_edge = df_edge[df_edge$free_edge_flow > 0, ]
  # print(df_edge)
  ## FOR DEBUG
  
  # Update sigma_in and sigma_out
  sigma_in = rho_in - from_free_flow
  sigma_out = rho_out - to_free_flow
  
  # Still to reduce in/out
  to_red_in = min_sigma_in - sigma_in
  to_red_out = min_sigma_out - sigma_out
  to_red_in[to_red_in < 0] = 0
  to_red_out[to_red_out < 0] = 0
  p_to_red_from = to_red_in / (from_free_flow + epsilon)
  p_to_red_to = to_red_out / (to_free_flow + epsilon)
  
  # Cap sigma_in and sigma_out
  sigma_in[sigma_in < min_sigma_in] = min_sigma_in[sigma_in < min_sigma_in]
  sigma_out[sigma_out < min_sigma_out] = min_sigma_out[sigma_out < min_sigma_out]
  
  # --- --- Correction on dependence
  
  # Revert back the flow
  to_red_in_edge = as.vector(t(nodeto_edge_mat[,free_edge_vec]) %*% p_to_red_from)
  to_red_out_edge = as.vector(t(nodefrom_edge_mat[,free_edge_vec]) %*% p_to_red_to)
  to_red_max_edge = pmax(to_red_in_edge, to_red_out_edge)
  to_red = apply(edge_to_sp * to_red_max_edge, 2, max)
  to_red_mat = matrix(to_red, n, n, byrow=T)
  max_to_red = max(to_red)
  
  ## FOR DEBUG
  # df_it_p = data.frame(which(to_red_mat==max_to_red, arr.ind=T))
  # ref_vec = c()
  # res_vec = c()
  # for(row_id in 1:dim(df_it_p)[1]){
  #   ref_vec = c(ref_vec, relationship_ref_mat[df_it_p$row[row_id], df_it_p$col[row_id]])
  #   res_vec = c(res_vec, results_mat[df_it_p$row[row_id], df_it_p$col[row_id]])
  # }
  # df_it_p["reference"] = ref_vec
  # df_it_p["flow_res"] = res_vec
  # print(df_it_p)
  ## FOR DEBUG
  
  # Reduce the dep
  relationship_ref_mat = (relationship_ref_mat * (1 - to_red_mat)) * admissible_sp_mat
  
  # --- --- Check for convergence and iterate
  
  diff = sum(abs(X_b - X_b_old))
    
  cat("It", it_algo, ": diff =", diff, ", to_red_max =", max_to_red,"\n")
  if(diff < conv_thres_algo | max_to_red < conv_thres_algo){
    converge_algo = T
  }
  
  # Iteration
  it_algo = it_algo + 1
}
  
# END 

# Normalizing
sigma_in = sigma_in / sum(sigma_in)
sigma_out = sigma_out / sum(sigma_out)
# Last Iterative fitting
converge_if = F
results_mat = relationship_ref_mat + epsilon
while(!converge_if){
  # Saving old results
  results_mat_old = results_mat 
  # Normalizing by row
  results_mat = results_mat * sigma_in / rowSums(results_mat + epsilon)
  # Normalizing by columns 
  results_mat = t(t(results_mat) * sigma_out / colSums(results_mat + epsilon))
  # Checking for convergence
  if(sum(abs(results_mat_old - results_mat)) < conv_thres_if){
    converge_if = T
  }
}

# Computing flow on edges
total_edge_flow = as.vector(t(sp_edge_mat) %*% c(t(results_mat)))

# To num number
to_num = inout_cor$montees[1] / rowSums(results_mat_old)[1]

# See link
non_null_edge = edge_mat[(round(total_edge_flow, 10) > 0) & free_edge_vec, ]
in_node_nn = stop_names[non_null_edge[,1]]
out_node_nn = stop_names[non_null_edge[,2]]
df_free_edge = data.frame(from=in_node_nn, to=out_node_nn, 
                          quant=round(total_edge_flow[(round(total_edge_flow, 10) > 0) & free_edge_vec] * to_num))

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
full_df["montees_initiales"] = round(sigma_in * to_num, 3)
full_df["transferts_in"] = round(transfer_in * to_num, 3)
full_df["diff_in"] = full_df["montees_initiales"] + full_df["transferts_in"] - full_df["montees"]
full_df["err_in%"] = round(full_df["diff_in"] / full_df["montees"] * 100, 3)
full_df["descentes_finales"] = round(sigma_out * to_num, 3)
full_df["transferts_out"] = round(transfer_out * to_num, 3)
full_df["diff_out"] = full_df["descentes_finales"] + full_df["transferts_out"] - full_df["descentes"]
full_df["err_out%"] = round(full_df["diff_out"] / full_df["descentes"] * 100, 3)

View(full_df)
View(df_free_edge)

# View
id_arret = 30
arret_prob = rev(results_mat_old[id_arret, ])
barplot(arret_prob, las=2, horiz=T, cex.names=0.5, main=stop_names[id_arret])

res_num = round(results_mat / rowSums(results_mat_old)[1] * inout_cor$montees[1])

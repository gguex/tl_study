#-------------------------------------------------------------------------------
#
# Convert old format into new format
#
#-------------------------------------------------------------------------------

#---------------------------
# Set working directory path
#---------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#---------------------------
# Loading the datasets 
#---------------------------

# --- Passengers in and out df

# Loading in/out data
inout_data = read.csv("stops_frequentation_6789.csv", sep=",", row.names = 1)
stop_names = trimws(paste(paste0("S", inout_data$code_ligne_theo), 
                          inout_data$direction_voy_theo, 
                          inout_data$code_arret_theo, sep="_"))

# --- Mean stopping time df

# Mean stop time at bus stops
stop_time_df = read.csv("waiting_time_s_tp.csv", sep=",")
# Get the name of stop
stop_time_rownames = trimws(paste(paste0("S", stop_time_df$code_ligne_theo), 
                                  stop_time_df$direction_voy_theo, 
                                  stop_time_df$code_arret_theo, sep="_"))
# Get the mean stopping time 
stop_time_vec = c()
for (i in 1:length(stop_names)){
  stop_time = stop_time_df[which(stop_time_rownames == stop_names[i]),
                           ]$mean_waiting_time
  stop_time_vec = c(stop_time_vec, stop_time)
}

# --- Mean travelling time df

# Bus time between stops
travel_time_df = read.csv("mean_time_i-j_s_tp.csv", sep=",")
# Get the name of starting stop
travel_time_rownames = trimws(paste(paste0("S", travel_time_df$code_ligne_theo), 
                                    travel_time_df$direction_voy_theo, 
                                    travel_time_df$code_arret_prec_theo, sep="_"))
# Get the mean travel time 
travel_time_vec = c()
for (i in 1:length(stop_names)){
  travel_time = travel_time_df[which(travel_time_rownames == stop_names[i])[1],
                               ]$mean_time
  if(length(travel_time) == 0) travel_time = NA
  travel_time_vec = c(travel_time_vec, travel_time)
}

# --- Pedestrian time matrix

# Pedestrian time martix 
ped_time_mat = read.csv("time_mat_s_6789_names.csv", sep=",")
ped_time_mat = as.matrix(ped_time_mat[, -1])
colnames(ped_time_mat) = stop_names

#---------------------------
# Building new datasets  
#---------------------------

# Building the complete dataframe
bus_df = data.frame(stop_names=stop_names,
                    label=inout_data$libelle_arret_theo2,
                    line_nbr=inout_data$code_ligne_theo,
                    direction=inout_data$direction_voy_theo,
                    passengers_in=inout_data$montees,
                    passengers_out=inout_data$descentes,
                    travel_time=travel_time_vec,
                    stopping_time=stop_time_vec)

# Changing the line 7 order
new_index = 50:72
old_index = c(63:72, 50:62)
new_bus_df = bus_df
new_bus_df[new_index, ] = bus_df[old_index, ]
new_ped_time_mat = ped_time_mat
new_ped_time_mat[new_index, ] = ped_time_mat[old_index, ]
new_ped_time_mat[, new_index] = new_ped_time_mat[, old_index]

# Fusion of SF for line 7
ind_to_merge = c(59, 60)
new_bus_df$passengers_in[ind_to_merge[2]] = new_bus_df$passengers_in[ind_to_merge[1]] + new_bus_df$passengers_in[ind_to_merge[2]]
new_bus_df$passengers_out[ind_to_merge[2]] = new_bus_df$passengers_out[ind_to_merge[1]] + new_bus_df$passengers_out[ind_to_merge[2]]
new_bus_df = new_bus_df[-ind_to_merge[1], ]
new_bus_df$direction[50:58] = "A"
new_ped_time_mat = new_ped_time_mat[-ind_to_merge[1], -ind_to_merge[1]]

write.table(new_bus_df, "../bus_df.csv", sep=",", row.names = F)
write.table(new_ped_time_mat, "../ped_time.csv", sep=",", row.names=F, col.names=F)

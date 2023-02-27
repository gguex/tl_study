#-------------------------------------------------------------------------------
#
# Convert old format into new format
#
#-------------------------------------------------------------------------------

library(stringr)

#---------------------------
# Set working directory path
#---------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#---------------------------
# Loading the datasets 
#---------------------------

# --- Passengers in and out df

# Loading line data
line_data = read.csv("stop_travel_waiting_time_all.csv", sep=",")
stop_names = trimws(paste(paste0("S", line_data$code_ligne_theo), 
                          line_data$direction_voy_theo, 
                          line_data$code_arret_theo, sep="_"))

# --- Pedestrian time matrix

# Pedestrian time martix 
ped_time_mat = read.csv("ped_time_tot.csv", sep=",", header=F)
# ped_time_names = names(ped_time_mat)
# formatted_names = c()
# for(name in ped_time_names){
#   f_name = str_replace_all(name, "[\\.]+$", "")
#   f_name = str_replace_all(f_name, "\\.", "-")
#   f_name = paste0("S", str_sub(f_name, 2))
#   formatted_names = c(formatted_names, f_name)
# }

#---------------------------
# Building new datasets  
#---------------------------

# Building the complete dataframe
bus_df = data.frame(stop_names=stop_names,
                    label=line_data$libelle_arret_theo2,
                    line_nbr=line_data$code_ligne_theo,
                    direction=line_data$direction_voy_theo,
                    passengers_in=line_data$montees,
                    passengers_out=line_data$descentes,
                    travel_time=line_data$mean_travel_time,
                    stopping_time=line_data$mean_waiting_time)

# Changing the line 7 order
new_index = 211:233
old_index = c(224:233, 211:223)
new_bus_df = bus_df
new_bus_df[new_index, ] = bus_df[old_index, ]
new_ped_time_mat = ped_time_mat
new_ped_time_mat[new_index, ] = ped_time_mat[old_index, ]
new_ped_time_mat[, new_index] = new_ped_time_mat[, old_index]

# Fusion of SF for line 7
ind_to_merge = c(220, 221)
new_bus_df$passengers_in[ind_to_merge[2]] = new_bus_df$passengers_in[ind_to_merge[1]] + new_bus_df$passengers_in[ind_to_merge[2]]
new_bus_df$passengers_out[ind_to_merge[2]] = new_bus_df$passengers_out[ind_to_merge[1]] + new_bus_df$passengers_out[ind_to_merge[2]]
new_bus_df = new_bus_df[-ind_to_merge[1], ]
new_bus_df$direction[211:219] = "A"
new_ped_time_mat = new_ped_time_mat[-ind_to_merge[1], -ind_to_merge[1]]

write.table(new_bus_df, "../../formatted_data/all_lines_old/bus_df.csv", 
            sep=",", row.names = F)
write.table(new_ped_time_mat, "../../formatted_data/all_lines_old/ped_time.csv", 
            sep=",", row.names=F, col.names=F)

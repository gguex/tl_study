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

# --- Pedestrian time matrix

# Pedestrian time martix 
ped_time_mat = read.csv("ped_time_tot.csv", sep=",")
ped_time_names = names(ped_time_mat)
formatted_names = c()
for(name in ped_time_names){
  f_name = str_replace_all(name, "[\\.]+$", "")
  f_name = str_replace_all(f_name, "\\.", "-")
  f_name = paste0("S", str_sub(f_name, 2))
  formatted_names = c(fromatted_names, f_name)
}

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

write.table(new_bus_df, "../../formatted_data/all_lines/bus_df.csv", 
            sep=",", row.names = F)
write.table(new_ped_time_mat, "../../formatted_data/all_lines/ped_time.csv", 
            sep=",", row.names=F, col.names=F)

# Remove problematic lines

# Set working directory path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data
bus_df = read.csv("bus_df.csv")
ped_time_mat = read.csv("ped_time.csv", header=F)

# Line numbers to remove
lines_to_remove = c(23, 29, 36, 41, 48, 60, 69, 81, 82)

# Indices to remove
to_remove_ids = which(bus_df$line_nbr %in% lines_to_remove)

# Remove lines
new_bus_df = bus_df[-to_remove_ids, ]
#new_ped_time_mat = ped_time_mat[-to_remove_ids, -to_remove_ids]
new_ped_time_mat = ped_time_mat

# Write new results
write.table(new_bus_df, "../all_lines/bus_df.csv", 
            sep=",", row.names = F)
write.table(new_ped_time_mat, "../all_lines/ped_time.csv", 
            sep=",", row.names=F, col.names=F)
# Remove the temporary lines 81 and 82

# Set working directory path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Indices to remove
to_remove_ids = 1338:1360

# Load data
bus_df = read.csv("bus_df.csv")
ped_time_mat = read.csv("ped_time.csv")

# Remove idx
new_bus_df = bus_df[-to_remove_ids, ]
new_ped_time_mat = ped_time[-to_remove_ids, -to_remove_ids]

# Write new results
write.table(new_bus_df, "../all_lines/bus_df.csv", 
            sep=",", row.names = F)
write.table(new_ped_time_mat, "../all_lines/ped_time.csv", 
            sep=",", row.names=F, col.names=F)
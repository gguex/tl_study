#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Transfert visualisation
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# - Map important nodes

#--------------------------------
# Head
#--------------------------------


# Libraries
#--------------------------------
# library(dplyr)
# library(ggplot2)
# library(gridExtra)
# library(reshape2)
# library(FactoMineR)
# library(sf)
# library(RColorBrewer)
# library(leaflet)

# # Load functions
# source("local_functions.R")

# # The folder containing pre-processed data
# data_folder = "multilines_data/preprocessed_data/all_lines"

# The folder containing results
results_folder = "results/all_lines2"

#--------------------------------
# Process
#--------------------------------

# --- Reading files

# Reading results data 
line_res = read.csv(paste0(results_folder, "/line_res.csv")) 
n_mat = as.matrix(read.csv(paste0(results_folder, "/n_mat.csv"), header=F))
ped_time = read.csv(as.matrix("multilines_data/formatted_data/all_lines/ped_time.csv"), header=F)
stop_names = line_res$stop_names
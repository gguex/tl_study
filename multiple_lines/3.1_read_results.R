#-------------------------------------------------------------------------------
#
# Read the results in different tables and graphics
#
#-------------------------------------------------------------------------------

#--------------------------------
# Head
#--------------------------------

# Set working directory path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load packages 
library("sf")
library("dplyr")
library("leaflet")

# Load functions
source("local_functions.R")

#--------------------------------
# Parameters
#--------------------------------

# The folder containing pre-processed data
data_folder = "multilines_data/preprocessed_data/test_6789"
# The folder containing results
results_folder = "results/test_6789"

#--------------------------------
# Process
#--------------------------------

# ---- SAVED FROM OLD FILE (OPEN)

# Compute transfer edges 
agr_passengers = aggregate(line_df$passengers_in, list(line_df$line_nbr), 
                           FUN=sum)
agr_passengers_stop = aggregate(line_df$passengers_out, list(line_df$line_nbr), 
                                FUN=sum)

top = sort(x_res$x_btw[x_res$x_btw > 0], decreasin=T)
for(i in 1:10){
  ind_top = which(x_res$x_btw == top[i], arr.ind=T)
  cat(line_df["stop_names"][ind_top[1],], "to", line_df["stop_names"][ind_top[2],], "with", top[i], "Tot stop:", line_df["passengers_out"][ind_top[1],],
      "Prop.:", top[i]/line_df["passengers_out"][ind_top[1],]*100 ,"\n"
  )
  
  #   cat(line_df["stop_names"][ind_top[1],], "to", line_df["stop_names"][ind_top[2],], "with", top[i], "Tot stop:", line_df["passengers_out"][ind_top[1],],
  #     "Prop.:", top[i]/sum(n_mat)*1000 ,"\n"
  # )
}

line_df["passengers_in"][ind_top[1],]

agr_passengers[which(agr_passengers == line_df["line_nbr"][ind_top[1],]),][2]
line_df["line_nbr"][ind_top[1],]

# ---- SAVED FROM OLD FILE (CLOSE)
